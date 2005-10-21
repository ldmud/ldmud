/*---------------------------------------------------------------------------
 * Array handling functions.
 *
 *---------------------------------------------------------------------------
 * TODO: Rewrite the low-level functions (like allocate_array()) to return
 * TODO:: failure codes (errno like) instead of throwing errors. In addition,
 * TODO:: provide wrapper functions which do throw error()s, so that every
 * TODO:: caller can handle the errors himself (like the swapper).
 * The structure of an array ("vector") is defined in datatypes.h as this:
 *
 *   vector_t_s {
 *       p_int size; 
 *       p_int ref;
 *       p_int extra_ref;          (ifdef DEBUG)
 *       wiz_list_t *user;
 *       svalue_t item[1...];
 *   };
 *
 * .size is the number of elements in the vector.
 *
 * .ref is the number of references to the vector. If this number
 * reaches 0, the vector can (and should) be deallocated. This scheme
 * breaks down with circular references, but those are caught by
 * the garbage collector.
 *
 * .extra_ref exists when the driver is compiled for DEBUGging, and
 * is used to countercheck the the .ref count.
 *
 * .user records which wizard's object created the vector, and is used
 * to keep the wizlist statistics (array usage) up to date.
 *
 * .item[] is the array of elements in indexing order. The structure
 * itself declares just an array of one element, it is task of the user
 * to allocated a big enough memory block.
 *
 *
 * Some macros help with the use of vector variables:
 *
 *   ALLOC_VECTOR(size,file,line): Allocate dynamically the memory for
 *      a vector of <size> elements.
 *
 *   VEC_SIZE(v): Return the number of elements in v.
 *
 *   VEC_HEAD(size): Expand to the initializers of a vector with
 *       <size> elements and 1 ref. This does not include the
 *       element initialisers.
 *
 *   LOCAL_VEC1(name, type1)
 *   LOCAL_VEC2(name, type1, type2)
 *       Construct a local vector instance named <name> with 1(2)
 *       elements of type <type1> (and <type2>). Both elements are
 *       initialised to 0, and the actual vector can be accessed
 *       as '<name>.v'.
 *
 * This module contains both low-level and efun-level functions.
 * The latter are collected in the lower half of the source.
 *---------------------------------------------------------------------------
 * One special application of arrays are alists: associative lists.
 * Alists allow the association of data (single values or tuples) with
 * a key value, which is then used to locate the data in the alist structure.
 *
 * Nowadays the same functionality is offered by mappings in a much more
 * efficient manner, so this usage of alists is deprecated. However, for
 * reasons explained below, alists can be used as an efficient way to
 * construct lookup arrays.
 *
 * It might be historically interesting to know that the very first
 * implementations of mappings were mere syntactic sugar for alists.
 * Furthermore, the LPMud variant of alists offers only a part of the
 * functionality of 'real' alists.
 *
 * Alists are implemented by a vector of vectors. A typical alist
 * for (key:data1,...,dataN) tuples looks like this:
 *
 *   alist = ({ ({ key values })
 *            , ({ data1 values })
 *            , ...
 *            , ({ dataN values })
 *           })
 *
 * All subarrays are of the same length, and all the values for one tuple
 * is found at the same index. For example, if the key for a tuple
 * is found in alist[0][3], the data values are found in alist[1..N][3].
 *
 * The key value array is sorted to allow fast lookups, the sorting order
 * uses the internal representation of the key values (which usually has
 * nothing in common with the meaning of the key values). Three things
 * however can be guaranteed:
 *
 *   - integer key values appear in rising order in the key array, though
 *     not necessarily consecutive.
 *   - removing one or more keys does not break the order of the
 *     other keys.
 *   - all strings used as key values are made shared strings.
 *
 * TODO: order_alist() should be generalized into a sort_array() function
 * TODO:: since it is used for more than just alists (similar assoc() into
 * TODO:: a lookup function). Alists themselves are pretty outdated by now.
 *---------------------------------------------------------------------------
 */

#include "driver.h"
#include "typedefs.h"

#include "my-alloca.h"
#include <stddef.h>

#include "array.h"
#include "backend.h"
#include "instrs.h"     /* F_INHERIT_LIST */
#include "interpret.h"  /* for the efuns */
#include "main.h"
#include "mapping.h"
#include "mempools.h"
#include "mstrings.h"
#include "object.h"
#include "regexp.h"
#include "rxcache.h"
#include "stdstrings.h"
#include "simulate.h"
#include "svalue.h"
#include "swap.h"
#include "wiz_list.h"
#include "xalloc.h"

/*-------------------------------------------------------------------------*/

int num_arrays;
  /* Total number of allocated arrays */

vector_t null_vector = { VEC_HEAD(0), { { T_INVALID } } };
  /* The global empty array ({}).
   * Reusing it is cheaper than repeated allocations/deallocations.
   */

void (*allocate_array_error_handler) (char *, ...)
  = error; /* from simulate.c */
  /* This handler is called if an allocation fails.
   * Usually it points to simulate::error(), but the swapper
   * replaces it temporarily with its own dummy handler when
   * swapping in an object.
   */

/*-------------------------------------------------------------------------*/
#ifndef allocate_array

vector_t *
allocate_array (mp_int n)

#else

vector_t *
_allocate_array(mp_int n, char * file, int line)

#endif

/* Allocate an array for <n> elements (but not more than the current
 * maximum) and return the pointer.
 * The elements are initialised to the svalue 0.
 *
 * If the allocations fails (and error() does return), a 0 pointer
 * may be returned. This is usually only possible when arrays
 * are allocated from the swapper.
 *
 * Allocating an array of size 0 will return a reference to the
 * globally shared empty array.
 *
 * If possible, annotate the allocations with <file> and <line>
 */

{
    mp_int i;
    vector_t *p;
    svalue_t *svp;

    if (n < 0 || (max_array_size && (size_t)n > max_array_size))
        error("Illegal array size: %ld.\n", n);

    if (n == 0) {
        p = ref_array(&null_vector);
        return p;
    }

    num_arrays++;

    p = ALLOC_VECTOR(n, file, line);
    if (!p) {
#ifndef allocate_array
        (*allocate_array_error_handler)("Out of memory: array[%ld]\n", n);
#else
        (*allocate_array_error_handler)
            ("(%s:%d) Out of memory: array[%ld]\n", file, line, n);
#endif
        return 0;
    }

    p->ref = 1;
    p->size = n;
    (p->user = current_object->user)->size_array += n;

    svp = p->item;
    for (i = n; --i >= 0; )
        *svp++ = const0;

    return p;
}

/*-------------------------------------------------------------------------*/
#ifndef allocate_array_unlimited

vector_t *
allocate_array_unlimited (mp_int n)

#else

vector_t *
_allocate_array_unlimited(mp_int n, char * file, int line)

#endif

/* Allocate an array for <n> elements and return the pointer.
 * The elements are initialised to the svalue 0.
 *
 * If the allocations fails (and error() does return), a 0 pointer
 * may be returned. This is usually only possible when arrays
 * are allocated from the swapper.
 *
 * Allocating an array of size 0 will return a reference to the
 * globally shared empty array.
 *
 * If possible, annotate the allocations with <file> and <line>
 */

{
    mp_int i;
    vector_t *p;
    svalue_t *svp;

    if (n < 0)
        error("Illegal array size: %ld.\n", n);

    if (n == 0) {
        p = ref_array(&null_vector);
        return p;
    }

    num_arrays++;

    p = ALLOC_VECTOR(n, file, line);
    if (!p) {
#ifndef allocate_array_unlimited
        (*allocate_array_error_handler)
            ("Out of memory: unlimited array[%ld]\n", n);
#else
        (*allocate_array_error_handler)
            ("(%s:%d) Out of memory: unlimited array[%ld]\n", file, line, n);
#endif
        return 0;
    }

    p->ref = 1;
    p->size = n;
    (p->user = current_object->user)->size_array += n;

    svp = p->item;
    for (i = n; --i >= 0; )
        *svp++ = const0;

    return p;
}

/*-------------------------------------------------------------------------*/
#ifndef allocate_uninit_array

vector_t *
allocate_uninit_array (mp_int n)

#else

vector_t *
_allocate_uninit_array (mp_int n, char *file, int line)

#endif

/* Allocate an array for <n> elements (but no more than the current
 * maximum) and return the pointer.
 * The elements are not initialised.
 * If the allocations fails (and error() does return), a 0 pointer
 * may be returned.
 *
 * Allocating an array of size 0 will return a reference to the
 * globally shared empty array.
 *
 * If possible, annotate the allocations with <file> and <line>
 */

{
    vector_t *p;

    if (n < 0 || (max_array_size && (size_t)n > max_array_size))
        error("Illegal array size: %ld.\n", n);

    if (n == 0) {
        p = ref_array(&null_vector);
        return p;
    }

    num_arrays++;

    p = ALLOC_VECTOR(n, file, line);
    if (!p) {
#ifndef allocate_uninit_array
        (*allocate_array_error_handler)
            ("Out of memory: uninited array[%ld]\n", n);
#else
        (*allocate_array_error_handler)
            ("(%s:%d) Out of memory: uninited array[%ld]\n", file, line, n);
#endif
        return 0;
    }

    p->ref = 1;
    p->size = n;
    (p->user = current_object->user)->size_array += n;

    return p;
}

/*-------------------------------------------------------------------------*/
void
_free_vector (vector_t *p)

/* Deallocate the vector <p>, properly freeing the contained elements.
 * The refcount is supposed to be zero at the time of call.
 */

{
    mp_uint i;
    svalue_t *svp;

#ifdef DEBUG
    if (p->ref > 0)
        fatal("Vector with %ld refs passed to _free_vector()\n", p->ref);
    if (p == &null_vector)
        fatal("Tried to free the zero-size shared vector.\n");
#endif

    i = VEC_SIZE(p);

    num_arrays--;
    p->user->size_array -= i;

    svp = p->item;
    do {
        free_svalue(svp++);
    } while (--i);

    xfree(p);
} /* _free_vector() */

/*-------------------------------------------------------------------------*/
void
free_empty_vector (vector_t *p)

/* Deallocate the vector <p> without regard of refcount or contained
 * elements. Just the statistics are cared for.
 */

{
    mp_uint i;

    i = VEC_SIZE(p);
    p->user->size_array -= i;
    num_arrays--;
    xfree((char *)p);
}

/*-------------------------------------------------------------------------*/
static vector_t *
shrink_array (vector_t *p, mp_int n)

/* Create and return a new array containing just the first <n> elements
 * of <p>. <p> itself is freed (and thus possibly deallocated).
 */

{
    vector_t *res;

    if (p->ref == 1 && VEC_SIZE(p) == n)
        return p;
        /* This case seems to happen often enough to justify
         * the shortcut
         */

    if (n)
    {
        res = slice_array(p, 0, n-1);
    }
    else
    {
        res = ref_array(&null_vector);
    }
    free_array(p);
    return res;
}

/*-------------------------------------------------------------------------*/
void
set_vector_user (vector_t *p, object_t *owner)

/* Wizlist statistics: take vector <p> from its former owner and account it
 * under its new <owner>.
 */

{
    svalue_t *svp;
    mp_int i;

    i = (mp_int)VEC_SIZE(p);
    if (p->user)
        p->user->size_array -= i;
    if ( NULL != (p->user = owner->user) )
        p->user->size_array += i;
    svp = p->item;
    for (; --i >= 0; svp++) {
        set_svalue_user(svp, owner);
    }
}

/*-------------------------------------------------------------------------*/
void
check_for_destr (vector_t *v)

/* Check the vector <v> for destructed objects and closures on destructed
 * objects and replace them with svalue 0s. Subvectors are not checked, though.
 *
 * This function is used by certain efuns (parse_command(), unique_array(),
 * map_array()) to make sure that the data passed to the efuns is valid,
 * avoiding game crashes (though this won't happen on simple operations
 * like assign_svalue).
 * TODO: The better way is to make the affected efuns resistant against
 * TODO:: destructed objects, and keeping this only as a safeguard and
 * TODO:: to save memory.
 */

{
    mp_int i;
    svalue_t *p;

    for (p = v->item, i = (mp_int)VEC_SIZE(v); --i >= 0 ; p++ )
    {
        if (destructed_object_ref(p))
            assign_svalue(p, &const0);
    }
} /* check_for_destr() */

/*-------------------------------------------------------------------------*/
long
total_array_size (void)

/* Statistics for the command 'status [tables]'.
 * Return the total memory used for all vectors in the game.
 */

{
    wiz_list_t *wl;
    long total;

    total = default_wizlist_entry.size_array;
    for (wl = all_wiz; wl; wl = wl->next)
        total += wl->size_array;
    total *= sizeof(svalue_t);
    total += num_arrays * (sizeof(vector_t) - sizeof(svalue_t));
    return total;
}

/*-------------------------------------------------------------------------*/
vector_t *
explode_string (string_t *str, string_t *del)

/* Explode the string <str> by delimiter string <del> and return an array
 * of the (unshared) strings found between the delimiters.
 * They are unshared because they are most likely short-lived.
 *
 * TODO: At some later point in the execution thread, all the longlived
 *   unshared strings should maybe be converted into shared strings.
 *
 * This is the new, logical behaviour: nothing is assumed.
 * The relation implode(explode(x,y),y) == x holds.
 *
 *   explode("xyz", "")         -> { "x", "y", "z" }
 *   explode("###", "##")       -> { "", "#" }
 *   explode(" the  fox ", " ") -> { "", "the", "", "", "fox", ""}
 */

{
    char *p, *beg;
    long num;
    long len, left;
    vector_t *ret;
    string_t *buff;

    len = (long)mstrsize(del);

    /* --- Special case: Delimiter is an empty or one-char string --- */
    if (len <= 1) {

        /* Delimiter is empty: return an array which holds all characters as
         *   single-character strings.
         */
        if (len < 1) {
            svalue_t *svp;

            len = (long)mstrsize(str);
            ret = allocate_array(len);
            for ( svp = ret->item, p = get_txt(str)
                ; --len >= 0
                ; svp++, p++ ) {
                buff = new_n_mstring(p, 1);
                if (!buff) {
                    free_array(ret);
                    error("(explode_string) Out of memory (1 byte string).\n");
                }
                put_string(svp, buff);
            }
            return ret;

        }

        /* Delimiter is one-char string: speedy implementation which uses
         *   direct character comparisons instead of calls to memcmp().
         */
        else {
            char c;
            char * txt;
            svalue_t *svp;

            txt = get_txt(str);
            c = get_txt(del)[0];

            /* TODO: Remember positions here */
            /* Determine the number of delimiters in the string. */
            for (num = 1, p = txt
                ; p < txt + len && NULL != (p = memchr(p, c, len - (p - txt)))
                ; p++, num++) NOOP;

            ret = allocate_array(num);
            for ( svp = ret->item, left = len
                ; NULL != (p = memchr(txt, c, left))
                ; left -= (p + 1 - txt), txt = p + 1, svp++)
            {
                len = p - txt;
                buff = new_n_mstring(txt, (size_t)len);
                if (!buff) {
                    free_array(ret);
                    error("(explode_string) Out of memory (%ld byte string)\n"
                         , len);
                }
                put_string(svp, buff);
            }

            /* txt now points to the (possibly empty) remains after
             * the last delimiter.
             */
            len = get_txt(str) + mstrsize(str) - txt;
            buff = new_n_mstring(txt, (size_t)len);
            if (!buff) {
                free_array(ret);
                error("(explode_string) Out of memory (%ld byte string)\n"
                     , len);
            }
            put_string(svp, buff);

            return ret;
        }

        /* NOTREACHED */
    } /* --- End of special case --- */

    /* Find the number of occurences of the delimiter 'del' by doing
     * a first scan of the string.
     *
     * The number of array items is then one more than the number of
     * delimiters, hence the 'num=1'.
     * TODO: Implement a strncmp() which returns the number of matching
     *   characters in case of a mismatch.
     * TODO: Remember the found positions so that we don't have to
     *   do the comparisons again.
     */
    for (p = get_txt(str), left = mstrsize(str), num=1 ; left > 0; )
    {
        if (left >= len && memcmp(p, get_txt(del), (size_t)len) == 0) {
            p += len;
            left -= len;
            num++;
        }
        else
        {
            p += 1;
            left -= 1;
        }
    }

    ret = allocate_array(num);

    /* Extract the <num> strings into the result array <ret>.
     *   <buff> serves as temporary buffer for the copying.
     */
    for (p = get_txt(str), beg = get_txt(str), num = 0, left = mstrsize(str)
        ; left > 0; )
    {
        if (left >= len && memcmp(p, get_txt(del), (size_t)len) == 0)
        {
            ptrdiff_t bufflen;

            bufflen = p - beg;
            buff = new_n_mstring(beg, (size_t)bufflen);
            if (!buff) {
                free_array(ret);
                error("(explode_string) Out of memory (%ld bytes) for buffer\n"
                     , bufflen);
            }

            put_string(ret->item+num, buff);

            num++;
            beg = p + len;
            p = beg;
            left -= len;

        } else {
            p += 1;
            left -= 1;
        }
    }

    /* Copy the last occurence (may be empty). */
    len = get_txt(str) + mstrsize(str) - beg;
    buff = new_n_mstring(beg, (size_t)len);
    if (!buff) {
        free_array(ret);
        error("(explode_string) Out of memory (%ld bytes) for last fragment\n"
             , len);
    }
    put_string(ret->item + num, buff);

    return ret;
} /* explode_string() */

/*-------------------------------------------------------------------------*/
vector_t *
old_explode_string (string_t *str, string_t *del)

/* Explode the string <str> by delimiter string <del> and return an array
 * of the (unshared) strings found between the delimiters.
 *
 * This is the old behaviour: leading and trailing occurences of <del>
 * in <str> are ignored.
 *
 *   explode("xyz", "")         -> { "xyz" }
 *   explode("###", "##")       -> { "", "#" }
 *   explode(" the  fox ", " ") -> { "the", "", "fox" }
 *
 * This function used to implement the explode() efun. Now the parse_command
 * parser and the efun process_string() are the only parts still using it.
 */

{
    char *p, *beg, *strtxt, *deltxt;
    size_t num, len, txtlen, left;
    vector_t *ret;

    len = mstrsize(del);
    deltxt = get_txt(del);

    /* Take care of the case where the delimiter is an
     * empty string. Then, return an array with only one element,
     * which is the original string.
     */
    if (len == 0)
    {
        ret = allocate_array(1);
        put_ref_string(ret->item, str);
        return ret;
    }

    /* Skip leading 'del' strings, if any.
     */
    strtxt = get_txt(str);
    txtlen = mstrsize(str);
    while (txtlen >= len && memcmp(strtxt, deltxt, (size_t)len) == 0)
    {
        strtxt += len;
        txtlen -= len;
        if (txtlen <= 0)
            return allocate_array(0);
    }

    /* Find number of occurences of the delimiter 'del' by doing a first
     * scan of the string.
     *
     * The found number + 1 is then the number of needed array elements.
     * Remember that explode("###","##") -> { "","#" }.
     * TODO: Implement a strncmp() which returns the number of matching
     *   characters in case of a mismatch.
     * TODO: Remember the found positions so that we don't have to
     *   do the comparisons again.
     */
    for (p = strtxt, num = 1, left = txtlen; left > 0;)
    {
        if (left >= len && memcmp(p, deltxt, len) == 0)
        {
            p += len;
            left -= len;
            if (*p)
                num++;
        }
        else
        {
            p += 1;
            left -= 1;
        }
    }

    ret = allocate_array(num);

    /* Extract the <num> strings into the result array <ret>.
     */

    for (p = strtxt, beg = strtxt, num = 0, left = txtlen; left > 0; )
    {
        if (left >= len && memcmp(p, deltxt, len) == 0)
        {
            ptrdiff_t bufflen;
            string_t *buff;

            bufflen = p - beg;
            buff = new_n_mstring(beg, (size_t)bufflen);
            if (!buff)
            {
                free_array(ret);
                error("(old_explode_string) Out of memory (%ld bytes) for buffer\n"
                     , bufflen);
            }

            put_string(ret->item + num, buff);

            num++;
            left -= p - beg + len;
            beg = p + len;
            p = beg;
        }
        else
        {
            p += 1;
            left -= 1;
        }
    }

    /* Copy last occurence, if there was not a 'del' at the end.
     */
    if (beg - strtxt < txtlen)
    {
        ptrdiff_t bufflen;
        string_t *buff;

        if (num >= VEC_SIZE(ret))
            fatal("Index out of bounds in old explode(): estimated %ld, got %ld.\n", (long)num, VEC_SIZE(ret));

        bufflen = p - beg;
        buff = new_n_mstring(beg, (size_t)bufflen);
        if (!buff)
        {
            free_array(ret);
            error("(old_explode_string) Out of memory (%ld bytes) for buffer\n"
                 , bufflen);
        }

        put_string(ret->item + num, buff);
    }

    return ret;
} /* old_explode_string() */

/*-------------------------------------------------------------------------*/
string_t *
arr_implode_string (vector_t *arr, string_t *del MTRACE_DECL)

/* Implode the string vector <arr> by <del>, i.e. all strings from <arr>
 * with <del> interspersed are contatenated into one string. The
 * resulting string is returned. The function will return at least
 * the empty string "".
 *
 * Non-string elements are ignore; elements referencing destructed
 * objects are replaced by the svalue number 0.
 *
 *   implode({"The", "fox", ""}, " ") -> "The fox "
 *
 * If possible, annotate the allocations with <file> and <line>
 */

{
    mp_int size, i, arr_size;
    size_t del_len;
    char *deltxt;
    char *p;
    string_t *result;
    svalue_t *svp;

    del_len = mstrsize(del);
    deltxt = get_txt(del);

    /* Compute the <size> of the final string
     */
    size = -(mp_int)del_len;
    for (i = (arr_size = (mp_int)VEC_SIZE(arr)), svp = arr->item; --i >= 0; svp++)
    {
        if (svp->type == T_STRING) {
            size += (mp_int)del_len + mstrsize(svp->u.str);
        }
        else if (destructed_object_ref(svp))
        {
            /* While we're here anyway... */
            assign_svalue(svp, &const0);
        }
    }

    /* Allocate the string; cop out if there's nothing to implode.
     */
    if (size <= 0)
        return ref_mstring(STR_EMPTY);

    result = mstring_alloc_string(size MTRACE_PASS);
    if (!result)
    {
        /* caller raises the error() */
        return NULL;
    }
    p = get_txt(result);

    /* Concatenate the result string.
     *
     * <i>   is the number of elements left to check,
     * <svp> is the next element to check,
     * <p>   points to the current end of the result string.
     */

    svp = arr->item;

    /* Look for the first element to add (there is at least one!) */
    for (i = arr_size; svp->type != T_STRING; )
    {
        --i;
        svp++;
    }

    memcpy(p, get_txt(svp->u.str), mstrsize(svp->u.str));
    p += mstrsize(svp->u.str);

    /* Copy the others if any */
    while (--i > 0)
    {
        svp++;
        if (svp->type == T_STRING)
        {
            memcpy(p, deltxt, del_len);
            p += del_len;
            memcpy(p, get_txt(svp->u.str), mstrsize(svp->u.str));
            p += mstrsize(svp->u.str);
        }
    }

    return result;
} /* implode_array() */

/*-------------------------------------------------------------------------*/
vector_t *
slice_array (vector_t *p, mp_int from, mp_int to)

/* Create a vector slice from vector <p>, range <from> to <to> inclusive,
 * and return it.
 *
 * <to> is guaranteed to not exceed the size of <p>.
 * If <from> is greater than <to>, the empty array is returned.
 */

{
    vector_t *d;
    int cnt;

    if (from < 0)
            from = 0;

    if (to < from)
        return allocate_array(0);

    d = allocate_array(to-from+1);
    for (cnt = from; cnt <= to; cnt++)
        assign_svalue_no_free(&d->item[cnt-from], &p->item[cnt]);

    return d;
}

/*-------------------------------------------------------------------------*/
vector_t *
add_array (vector_t *p, vector_t *q)

/* Concatenate the vectors <p> and <q> and return the resulting vector.
 * <p> and <q> are not modified.
 */

{
    mp_int cnt;
    svalue_t *s, *d;
    mp_int q_size;

    s = p->item;
    p = allocate_array((cnt = (mp_int)VEC_SIZE(p)) + (q_size = (mp_int)VEC_SIZE(q)));
    d = p->item;
    for ( ; --cnt >= 0; ) {
        assign_svalue_no_free (d++, s++);
    }
    s = q->item;
    for (cnt = q_size; --cnt >= 0; ) {
        assign_svalue_no_free (d++, s++);
    }
    return p;
}

/*-------------------------------------------------------------------------*/
static int
compare_single (svalue_t *svp, vector_t *v)

/* Compare *svp and v->item[0], return 0 if equal, and -1 if not.
 *
 * The function is used by subtract_array() and must match the signature
 * of assoc().
 */

{
    svalue_t *p2 = &v->item[0];

    if (svp->type != p2->type)
        return -1;

    if (svp->type == T_STRING)
    {
        return mstreq(svp->u.str, p2->u.str) ? 0 : -1;
    }

    /* All other types have to be equal by address, visible in u.number */
    /* TODO: This comparison is not valid according to ISO C */
    if (svp->u.number != p2->u.number)
        return -1;

    switch (svp->type)
    {
    case T_FLOAT:
    case T_CLOSURE:
    case T_SYMBOL:
    case T_QUOTED_ARRAY:
        return svp->x.generic != p2->x.generic ? -1 : 0;
    default:
        return 0;
    }

    /* NOTREACHED */
    return 0;
}

/*-------------------------------------------------------------------------*/
vector_t *
subtract_array (vector_t *minuend, vector_t *subtrahend)

/* Subtract all elements in <subtrahend> from the vector <minuend>
 * and return the resulting difference vector.
 * <subtrahend> and <minuend> are freed.
 *
 * The function uses order_alist()/assoc()/compare_single() on
 * <subtrahend> for faster operation, and recognizes subtrahends with
 * only one element and/or one reference.
 */

{
    svalue_t ltmp = { T_POINTER };
      /* Temporary svalue to pass vectors to order_alist(). */

    vector_t *difference;    /* Resulting difference vector,
                                with extra zeroes at the end */
    vector_t *vtmpp;         /* {( Ordered <subtrahend> }) */
    svalue_t *source, *dest; /* Pointers into minuend
                                and difference vector */
    mp_int i;
    mp_int minuend_size    = (mp_int)VEC_SIZE(minuend);
    mp_int subtrahend_size = (mp_int)VEC_SIZE(subtrahend);

    int (*assoc_function)(svalue_t *, vector_t *);
      /* Function to find an svalue in a sorted vector.
       * Use of this indirection allows to replace assoc() with
       * faster functions for special cases.
       */

    /* Handle empty vectors quickly */

    if (minuend_size == 0)
    {
        free_array(subtrahend);
        return minuend;
    }
    if (subtrahend_size == 0)
    {
        free_array(subtrahend);
        return shrink_array(minuend, minuend_size);
    }

    /* Order the subtrahend */
    if (subtrahend_size == 1)
    {
        if (destructed_object_ref(&subtrahend->item[0]))
        {
            assign_svalue(&subtrahend->item[0], &const0);
        }
        assoc_function = &compare_single;
        vtmpp = subtrahend;
    }
    else
    {
        ltmp.u.vec = subtrahend;
        vtmpp = order_alist(&ltmp, 1, 1);
        free_array(ltmp.u.vec);
        assoc_function = &assoc;
        subtrahend = vtmpp->item[0].u.vec;
    }

    /* Scan minuend and look up every element in the ordered subtrahend.
     * If it's not there, add the element to the difference vector.
     * If minuend is referenced only once, reuse its memory.
     */

    if (minuend->ref == 1)
    {
        for (source = minuend->item, i = minuend_size ; i-- ; source++)
        {
            if (destructed_object_ref(source))
                assign_svalue(source, &const0);
            if ( (*assoc_function)(source, subtrahend) >-1 ) break;
        }
        for (dest = source++; i-- > 0 ; source++)
        {
            if (destructed_object_ref(source))
                assign_svalue(source, &const0);
            if ( (*assoc_function)(source, subtrahend) < 0 )
                assign_svalue(dest++, source);
        }
        free_array(vtmpp);
        return shrink_array(minuend, dest - minuend->item);
    }

    /* The difference can be equal to minuend in the worst case */
    difference = allocate_array(minuend_size);

    for (source = minuend->item, dest = difference->item, i = minuend_size
        ; i--
        ; source++) {
        if (destructed_object_ref(source))
            assign_svalue(source, &const0);
        if ( (*assoc_function)(source, subtrahend) < 0 )
            assign_svalue_no_free(dest++, source);
    }

    free_array(vtmpp);
    free_array(minuend);

    /* Shrink the difference vector to the needed size and return it. */
    return shrink_array(difference, dest-difference->item);
}

/*-------------------------------------------------------------------------*/
static INLINE int
alist_cmp (svalue_t *p1, svalue_t *p2)

/* Alist comparison function.
 *
 * Compare the svalues <p1> and <p2> and return an integer with the
 * following meaning:
 *
 *   > 0: <p1> 'is greater than' <p2>
 *   = 0: <p1> 'is equal to' <p2>
 *   < 0: <p1> 'is less than' <p2>
 *
 * The relation need not make sense with the actual interpretation
 * of <p1>/<p2>, as long as it defines a deterministic order relation.
 * Especially, it works for strings because the caller makes sure
 * that only directly tabled strings are used.
 *
 * TODO: Is the assumption '.number is big enough to hold everything
 * TODO:: in the svalue' true for future hardware?
 * TODO: Reinterpreting the pointers as 'integer' may not be portable
 * TODO:: enough.
 */

{
    register int d;

    /* Avoid a numeric overflow by first comparing the values halfed. */
    if ( 0 != (d = (p1->u.number >> 1) - (p2->u.number >> 1)) ) return d;
    if ( 0 != (d = p1->u.number - p2->u.number) ) return d;
    if ( 0 != (d = p1->type - p2->type) ) return d;
    switch (p1->type) {
      case T_FLOAT:
      case T_CLOSURE:
      case T_SYMBOL:
      case T_QUOTED_ARRAY:
        if ( 0 != (d = p1->x.generic - p2->x.generic) ) return d;
    }
    return 0;
}

/*-------------------------------------------------------------------------*/
vector_t *
order_alist (svalue_t *inlists, int listnum, Bool reuse)

/* Order the alist <inlists> and return a new vector with it. The sorting
 * order is the internal order defined by alist_cmp().
 *
 * <inlists> is a vector of <listnum> vectors:
 *   <inlists> = ({ ({ keys }), ({ data1 }), ..., ({ data<listnum-1> }) })
 *
 * If <reuse> is true, the vectors of <inlists> are reused for the
 * vectors of the result when possible, and their entries in <inlists> are
 * set to T_INVALID.
 *
 * This function and assoc() are used in several places for internal
 * lookup functions (e.g. in sort_array()).
 *
 * As a side effect, strings in the key vector are made shared, and
 * destructed objects in key and data vectors are replaced by svalue 0s.
 */

{
    vector_t *outlist;   /* The result vector of vectors */
    vector_t *v;         /* Aux vector pointer */
    svalue_t *outlists;  /* Next element in outlist to fill in */
    ptrdiff_t * sorted;
      /* The vector elements in sorted order, given as the offsets of the array
       * element in question to the start of the vector. This way,
       * sorted[] needs only to be <keynum> elements long.
       * sorted[] is created from root[] after sorting.
       */

    svalue_t **root;
      /* Auxiliary array with the sorted keys as svalue* into inlists[0].vec.
       * This way the sorting is given by the order of the pointers, while
       * the original position is given by (pointer-inlists[0].vec->item).
       * The very first element is a dummy (heapsort uses array indexing
       * starting with index 1), the next <keynum> elements are scratch
       * area, the final <keynum> elements hold the sorted keys in reverse
       * order.
       */
    svalue_t **root2;   /* Aux pointer into *root. */
    svalue_t *inpnt;    /* Pointer to the value to copy into the result */
    mp_int keynum;      /* Number of keys */
    int i, j;

    keynum = (mp_int)VEC_SIZE(inlists[0].u.vec);

    /* Allocate the auxiliary array. */
    root = (svalue_t **)alloca(keynum * sizeof(svalue_t *[2])
                                           + sizeof(svalue_t)
                                   );
    sorted = alloca(keynum * sizeof(ptrdiff_t) + sizeof(ptrdiff_t));
    /* TODO: keynum may be 0, so the c-alloca() would return NULL without
     * the extra sizeof(ptrdiff_t) :-(
     */

    if (!root || !sorted)
    {
        error("Stack overflow in order_alist()");
        /* NOTREACHED */
        return NULL;
    }

    /*
     * Heapsort inlists[0].vec into *root.
     * TODO: For small arrays a simpler sort like linear insertion or
     * TODO:: even bubblesort might be faster (less overhead). Best solution
     * TODO:: would be to offer both algorithms and determine the threshhold
     * TODO:: at startup.
     */

    /* Heapify the keys into the first half of root */
    for ( j = 1, inpnt = inlists->u.vec->item
        ; j <= keynum
        ; j++, inpnt++)
    {
        int curix, parix;

        /* make sure that strings can be compared by their pointer */
        if (inpnt->type == T_STRING)
        {
            if (!mstr_d_tabled(inpnt->u.str))
            {
                inpnt->u.str = make_tabled(inpnt->u.str);
            }
        }
        else if (destructed_object_ref(inpnt))
        {
            free_svalue(inpnt);
            put_number(inpnt, 0);
        }

        /* propagate the new element up in the heap as much as necessary */
        for (curix = j; 0 != (parix = curix>>1); ) {
            if ( alist_cmp(root[parix], inpnt) > 0 ) {
                root[curix] = root[parix];
                curix = parix;
            } else {
                break;
            }
        }
        root[curix] = inpnt;
    }

    root++; /* Adjust root to ignore the heapsort-dummy element */

    /* Sort the heaped keys from the first into the second half of root. */
    root2 = &root[keynum];
    for(j = keynum; --j >= 0; ) {
        int curix;

        *root2++ = *root;
        for (curix=0; ; ) {
            int child, child2;

            child = curix+curix+1;
            child2 = child+1;
            if (child2 >= keynum) {
                if (child2 == keynum && root[child]) {
                    root[curix] = root[child];
                    curix = child;
                }
                break;
            }
            if (root[child2]) {
                if (!root[child] || alist_cmp(root[child], root[child2]) > 0)
                {
                    root[curix] = root[child2];
                    curix = child2;
                    continue;
                }
            } else if (!root[child]) {
                break;
            }
            root[curix] = root[child];
            curix = child;
        }
        root[curix] = 0;
    }

    /* Compute the sorted offsets from root[] into sorted[].
     * Note that root[] is in reverse order.
     */
    for (root = &root[keynum], j = 0; j < keynum; j++)
        sorted[j] = root[keynum-j-1] - inlists[0].u.vec->item;

    /*
     * Generate the result vectors from the sorted keys in root.
     */

    outlist = allocate_array(listnum);
    outlists = outlist->item;

    /* Copy the elements from all inlist vectors into the outlist
     * vectors.
     *
     * At the beginning of every loop v points to the vector to
     * use as the next 'out' vector. It may be a re-used 'in' vector
     * from the previous run.
     */
    v = allocate_array(keynum);
    for (i = listnum; --i >= 0; ) {

        svalue_t *outpnt; /* Next result value element to fill in */

        /* Set the new array v as the next 'out' vector, and init outpnt
         * and offs.
         */
        put_array(outlists + i, v);
        outpnt = v->item;

        v = inlists[i].u.vec; /* Next vector to fill if reusable */

        /* Copy the elements.
         * For a reusable 'in' vector, a simple memory copy is sufficient.
         * For a new vector, a full assignment is due to keep the refcounters
         * happy.
         */
        if (reuse && inlists[i].u.vec->ref == 1) {

            if (i)/* not the last iteration */
                inlists[i].type = T_INVALID;

            for (j = keynum; --j >= 0; ) {
                inpnt = inlists[i].u.vec->item + sorted[j];
                if (destructed_object_ref(inpnt))
                {
                    free_svalue(inpnt);
                    put_number(outpnt, 0);
                    outpnt++;
                } else {
                    *outpnt++ = *inpnt;
                }
                inpnt->type = T_INVALID;
            }

        } else {

            if (i) /* not the last iteration */
                v = allocate_array(keynum);

            for (j = keynum; --j >= 0; ) {
                inpnt = inlists[i].u.vec->item + sorted[j];
                if (destructed_object_ref(inpnt))
                {
                    put_number(outpnt, 0);
                    outpnt++;
                } else {
                    assign_svalue_no_free(outpnt++, inpnt);
                }
            }
        } /* if (reuse) */
    } /* for (listnum) */

    return outlist;
}

/*-------------------------------------------------------------------------*/
Bool
is_alist (vector_t *v)

/* Determine if <v> satisfies the conditions for being an alist key vector.
 * Return true if yes, false if not.
 *
 * The conditions are:
 *   - every string is shared
 *   - all elements are sorted according to alist_cmp().
 *
 * Note that an ordinary array can do this by chance.
 *
 * This predicate is currently used just by the swapper to avoid swapping
 * out alist values. This is because the internal order is based on
 * pointer values and thus unreproducible.
 */

{
    svalue_t *svp;
    mp_int i;

    for (svp = v->item, i = (mp_int)VEC_SIZE(v); --i > 0; svp++) {
        if (svp->type == T_STRING && !mstr_d_tabled(svp->u.str))
            return MY_FALSE;
        if (alist_cmp(svp, svp+1) > 0)
            return MY_FALSE;
    }
    if (svp->type == T_STRING && !mstr_d_tabled(svp->u.str))
        return MY_FALSE;

    return MY_TRUE;
}

/*=========================================================================*/

/*                            EFUNS                                        */

/*-------------------------------------------------------------------------*/
svalue_t *
f_allocate (svalue_t *sp)

/* EFUN allocate()
 *
 *     mixed *allocate(int size)
 *
 * Allocate an empty array of <size> elements.
 */

{
    vector_t *v;

    v = allocate_array(sp->u.number); /* Will have ref count == 1 */
    put_array(sp, v);

    return sp;
} /* f_allocate() */

/*-------------------------------------------------------------------------*/
svalue_t *
x_filter_array (svalue_t *sp, int num_arg)

/* EFUN: filter() for arrays.
 *
 *   mixed *filter(mixed *arr, string fun)
 *   mixed *filter(mixed *arr, string fun, string|object obj, mixed extra, ...)
 *   mixed *filter(mixed *arr, closure cl, mixed extra, ...)
 *   mixed *filter(mixed *arr, mapping map)
 *
 * Filter the elements of <arr> through a filter defined by the other
 * arguments, and return an array of those elements, for which the
 * filter yields non-zero.
 *
 * The filter can be a function call:
 *
 *    <obj>-><fun>(elem, <extra>...)
 *
 * or a mapping query:
 *
 *    <map>[elem]
 *
 * <obj> can both be an object reference or a filename. If omitted,
 * this_object() is used (this also works if the third argument is
 * neither a string nor an object).
 *
 * As a bonus, all references to destructed objects in <arr> are replaced
 * by proper 0es.
 *
 * TODO: Autodoc-Feature to create doc/efun/filter_array automatically.
 */

{
    svalue_t *arg;    /* First argument the vm stack */
    vector_t *p;      /* The filtered vector */
    mp_int    p_size; /* sizeof(*p) */
    vector_t *vec;
    svalue_t *v, *w;
    char     *flags;  /* Flag array, one flag for each element of <p> */
    int       res;    /* Number of surviving elements */
    int       cnt;

    res = 0;

    /* Locate the args on the stack, extract the vector to filter
     * and allocate the flags vector.
     */
    arg = sp - num_arg + 1;

    p = arg->u.vec;
    p_size = (mp_int)VEC_SIZE(p);

    flags = alloca((size_t)p_size+1);
    if (!flags)
    {
        error("Stack overflow in filter()");
        /* NOTREACHED */
        return sp;
    }

    /* Every element in flags is associated by index number with an
     * element in the vector to filter. The filter function is evaluated
     * for every vector element, and the associated flag is set to 0
     * or 1 according to the result.
     * At the end, all 1-flagged elements are gathered and copied
     * into the result vector.
     */

    if (arg[1].type == T_MAPPING) {

        /* --- Filter by mapping query --- */
        mapping_t *m;

        if (num_arg > 2) {
            inter_sp = sp;
            error("Too many arguments to filter_array()\n");
        }
        m = arg[1].u.map;

        for (w = p->item, cnt = p_size; --cnt >= 0; )
        {
            if (destructed_object_ref(w))
                assign_svalue(w, &const0);
            if (get_map_value(m, w++) == &const0) {
                flags[cnt] = 0;
                continue;
            }
            flags[cnt] = 1;
            res++;
        }

        free_svalue(arg+1);
        sp = arg;

    } else {

        /* --- Filter by function call --- */

        int         error_index;
        callback_t  cb;
        
        assign_eval_cost();
        inter_sp = sp;

        error_index = setup_efun_callback(&cb, arg+1, num_arg-1);

        if (error_index >= 0)
        {
            vefun_bad_arg(error_index+2, arg);
            /* NOTREACHED */
            return arg;
        }
        inter_sp = sp = arg+1;
        put_callback(sp, &cb);

        /* Loop over all elements in p and call the filter.
         * w is the current element filtered.
         */
        for (w = p->item, cnt = p_size; --cnt >= 0; )
        {
            flags[cnt] = 0;

            if (current_object->flags & O_DESTRUCTED)
                continue;
                /* Don't call the filter anymore, but fill the
                 * flags array with 0es.
                 */

            if (destructed_object_ref(w))
                assign_svalue(w, &const0);

            if (!callback_object(&cb))
            {
                inter_sp = sp;
                error("object used by filter_array destructed");
            }

            push_svalue(w++);
            
            v = apply_callback(&cb, 1);
            if (!v || (v->type == T_NUMBER && !v->u.number) )
                continue;

            flags[cnt] = 1;
            res++;
        }

        free_callback(&cb);
    }

    /* flags[] holds the filter results, res is the number of
     * elements to keep. Now create the result vector.
     */
    vec = allocate_array(res);
    if (res) {
        for(v = p->item, w = vec->item, flags = &flags[p_size]; ; v++) {
            if (*--flags) {
                assign_svalue_no_free (w++, v);
                if (--res <= 0) break;
            }
        }
    }

    /* Cleanup (everything but the array has been removed already) */
    free_array(p);
    arg->u.vec = vec;

    return arg;
} /* x_filter_array() */

/*-------------------------------------------------------------------------*/
svalue_t *
x_map_array (svalue_t *sp, int num_arg)

/* EFUN map() on arrays
 *
 *   mixed * map(mixed *arg, string func, string|object ob, mixed extra...)
 *   mixed * map(mixed *arg, closure cl, mixed extra...)
 *   mixed * map(mixed *arr, mapping map)
 *
 * Map the elements of <arr> through a filter defined by the other
 * arguments, and return an array of the elements returned by the filter.
 *
 * The filter can be a function call:
 *
 *    <obj>-><fun>(elem, <extra>...)
 *
 * or a mapping query:
 *
 *    <map>[elem]
 *
 * In the mapping case, if <map>[elem] does not exist, the original
 * value is returned in the result.
 *
 * <obj> can both be an object reference or a filename. If <ob> is
 * omitted, or neither an object nor a string, then this_object() is used.
 *
 * As a bonus, all references to destructed objects in <arr> are replaced
 * by proper 0es.
 */

{
    vector_t   *arr;
    vector_t   *res;
    svalue_t   *arg;
    svalue_t   *v, *w, *x;
    mp_int      cnt;

    arg = sp - num_arg + 1;

    arr = arg->u.vec;
    cnt = (mp_int)VEC_SIZE(arr);

    if (arg[1].type == T_MAPPING)
    {
        /* --- Map through mapping --- */

        mapping_t *m;

        if (num_arg > 2) {
            inter_sp = sp;
            error("Too many arguments to map_array()\n");
        }
        m = arg[1].u.map;

        res = allocate_array(cnt);
        if (!res)
            error("(map_array) Out of memory: array[%ld] for result\n", cnt);
        push_array(inter_sp, res); /* In case of errors */

        for (w = arr->item, x = res->item; --cnt >= 0; w++, x++)
        {
            if (destructed_object_ref(w))
                assign_svalue(w, &const0);

            v = get_map_value(m, w);
            if (v == &const0)
                assign_svalue_no_free(x, w);
            else
                assign_svalue_no_free(x, v);
        }

        free_svalue(arg+1); /* the mapping */
        sp = arg;
    }
    else
    {
        /* --- Map through function call --- */

        callback_t  cb;
        int         error_index;

        error_index = setup_efun_callback(&cb, arg+1, num_arg-1);
        if (error_index >= 0)
        {
            vefun_bad_arg(error_index+2, arg);
            /* NOTREACHED */
            return arg;
        }
        inter_sp = sp = arg+1;
        put_callback(sp, &cb);
        num_arg = 2;

        res = allocate_array(cnt);
        if (!res)
            error("(map_array) Out of memory: array[%ld] for result\n", cnt);
        push_array(inter_sp, res); /* In case of errors */

        /* Loop through arr and res, mapping the values from arr */
        for (w = arr->item, x = res->item; --cnt >= 0; w++, x++)
        {
            if (current_object->flags & O_DESTRUCTED)
                continue;

            if (destructed_object_ref(w))
                assign_svalue(w, &const0);

            if (!callback_object(&cb))
                error("object used by map_array destructed");

            push_svalue(w);

            v = apply_callback(&cb, 1);
            if (v)
            {
                transfer_svalue_no_free(x, v);
                v->type = T_INVALID;
            }
        }

        free_callback(&cb);
    }
    
    /* The arguments have been removed already, now just replace
     * the arr on the stack with the result.
     */
    free_array(arr);
    arg->u.vec = res;

    return arg;
} /* x_map_array () */

/*-------------------------------------------------------------------------*/
svalue_t *
f_sort_array (svalue_t * sp, int num_arg)

/* EFUN sort_array()
 *
 *   mixed *sort_array(mixed *arr, string wrong_order
 *                               , object|string ob, mixed extra...)
 *   mixed *sort_array(mixed *arr, closure cl, mixed extra...)
 *
 * Create a shallow copy of array <arr> and sort that copy by the ordering
 * function ob->wrong_order(a, b), or by the closure expression 'cl'.
 * The sorted copy is returned as result.
 *
 * If the 'arr' argument equals 0, the result is also 0.
 * 'ob' is the object in which the ordering function is called
 * and may be given as object or by its filename.
 * If <ob> is omitted, or neither an object nor a string, then
 * this_object() is used.
 *
 * The elements from the array to be sorted are passed in pairs to
 * the function 'wrong_order' as arguments, followed by any <extra>
 * arguments.
 *
 * The function should return a positive number if the elements
 * are in the wrong order. It should return 0 or a negative
 * number if the elements are in the correct order.
 *
 * The sorting is implemented using Mergesort, which gives us a O(N*logN)
 * worst case behaviour and provides a stable sort.
 */

{
    vector_t   *data;
    svalue_t   *arg;
    callback_t  cb;
    int         error_index;
    mp_int      step, halfstep, size;
    int         i, j, index1, index2, end1, end2;
    svalue_t   *source, *dest, *temp;

    arg = sp - num_arg + 1;

    error_index = setup_efun_callback(&cb, arg+1, num_arg-1);
    if (error_index >= 0)
    {
        vefun_bad_arg(error_index+2, arg);
        /* NOTREACHED */
        return arg;
    }
    inter_sp = sp = arg+1;
    put_callback(sp, &cb);
    num_arg = 2;

    /* Get the array. Since the sort sorts in-place, we have
     * to make a shallow copy of arrays with more than one
     * ref.
     */
    data = arg->u.vec;
    check_for_destr(data);

    if (data->ref != 1)
    {
        vector_t *vcopy;

        vcopy = slice_array(data, 0, VEC_SIZE(data)-1);
        free_array(data);
        data = vcopy;
        arg->u.vec = data;
    }

    size = (mp_int)VEC_SIZE(data);

    /* Easiest case: nothing to sort */
    if (size <= 1)
    {
        free_callback(&cb);
        return arg;
    }

    /* In order to provide clean error recovery, data must always hold
     * exactly one copy of each original content svalue when an error is
     * possible. Thus, it would be not a good idea to use it as scrap
     * space.
     */

    temp = data->item;

    source = alloca(size*sizeof(svalue_t));
    dest = alloca(size*sizeof(svalue_t));
    if (!source || !dest)
    {
        free_callback(&cb);
        error("Stack overflow in sort_array()");
        /* NOTREACHED */
        return arg;
    }

    for (i = 0; i < size; i++)
        source[i] = temp[i];

    step = 2;
    halfstep = 1;
    while (halfstep<size)
    {
        for (i = j = 0; i < size; i += step)
        {
            index1 = i;
            index2 = i + halfstep;
            end1 = index2;
            if (end1 > size)
                end1 = size;
            end2 = i + step;
            if (end2 > size)
                end2 = size;

            while (index1 < end1 && index2 < end2)
            {
                svalue_t *d;

                if (!callback_object(&cb))
                    error("object used by sort_array destructed");

                push_svalue(source+index1);
                push_svalue(source+index2);
                d = apply_callback(&cb, 2);

                if (d && (d->type != T_NUMBER || d->u.number > 0))
                    dest[j++] = source[index2++];
                else
                    dest[j++] = source[index1++];
            }

            if (index1 == end1)
            {
                while (index2 < end2)
                    dest[j++] = source[index2++];
            }
            else
            {
                while (index1 < end1)
                    dest[j++] = source[index1++];
            }
        }
        halfstep = step;
        step += step;
        temp = source;
        source = dest;
        dest = temp;
    }

    temp = data->item;
    for (i = size; --i >= 0; )
      temp[i] = source[i];

    free_callback(&cb);
    return arg;
} /* f_sort_array() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_filter_objects (svalue_t *sp, int num_arg)

/* EFUN filter_objects()
 *
 *   object *filter_objects (object *arr, string fun, mixed extra, ...)
 *
 * Filter the objects in <arr> by calling the lfun obj-><fun>(<extra>...)
 * and return an array of those objects for which the lfun call yields
 * non-zero.
 *
 * The objects can be true objects or filenames. In the latter case, the
 * function tries to load the object before calling the lfun. Any non-object
 * element in <arr> is ignored and thus not included in the result.
 *
 * As a bonus, all references to destructed objects in <arr> are replaced
 * by proper 0es.
 */

{
    vector_t *p;          /* The <arr> argument */
    string_t *func;       /* The <fun> argument */
    svalue_t *arguments;  /* Beginning of 'extra' arguments on vm stack */
    vector_t *w;          /* Result vector */
    CBool *flags = NULL;  /* Flag array, one flag for each element of <p> */
    int res;              /* Count of objects to return */
    object_t *ob;         /* Object to call */
    mp_int p_size;        /* Size of <p> */
    int cnt = 0;
    svalue_t *v;

    assign_eval_cost();
    inter_sp = sp; /* needed for errors in allocate_array(), apply() */

    /* Locate the arguments on the stack and extract them */
    arguments = sp-num_arg+3;

    p = arguments[-2].u.vec;
    func = arguments[-1].u.str;
    num_arg -= 2;

    p_size = (mp_int)VEC_SIZE(p);

    /* Call <func> in every object, recording the result in flags.
     *
     * Every element in flags is associated by index number with an
     * element in the vector to filter. The filter function is evaluated
     * for every vector element, and the associated flag is set to 0
     * or 1 according to the result.
     * At the end, all 1-flagged elements are gathered and copied
     * into the result vector.
     *
     * Checking if <func> exists as shared string takes advantage of
     * the fact that every existing lfun name is stored as shared string.
     * If it's not shared, no object implements it and we can skip
     * the whole function call loop.
     */

    res = 0;

    func = find_tabled(func);
    if (NULL != func)
    {
        flags = alloca((p_size+1)*sizeof(*flags));
        if (!flags)
        {
            error("Stack overflow in filter_objects()");
            /* NOTREACHED */
            return NULL;
        }

        for (cnt = 0; cnt < p_size; cnt++) {
            flags[cnt] = MY_FALSE;
            v = &p->item[cnt];

            /* Coerce <v> into a (non-destructed) object ob (if necessary
             * by loading it). If that doesn't work, simply continue
             * with the next element.
             */
            if (v->type != T_OBJECT)
            {
                if (v->type != T_STRING)
                    continue;
                if ( !(ob = get_object(v->u.str)) )
                    continue;
            } else {
                ob = v->u.ob;
                if (ob->flags & O_DESTRUCTED)
                {
                    assign_svalue(v, &const0);
                    continue;
                }
            }

            /* Abort the efun if this_object is destructed (slightly
             * strange place to check for it).
             */
            if (current_object->flags & O_DESTRUCTED)
                continue;

            /* Call the filter lfun and record the result. */
            push_svalue_block(num_arg, arguments);
            v = sapply (func, ob, num_arg);
            if ((v) && (v->type!=T_NUMBER || v->u.number) ) {
                flags[cnt] = MY_TRUE;
                res++;
            }
        } /* for() */
    } /* if() */

    /* Now: cnt == p_size, res == number of 'true' flags */

    /* Create the result vector and fill it with all objects for which
     * true flag was recorded.
     */

    w = allocate_array(res); /* might be a 0-elements array */

    if (res) {

        /* Walk through flags/w->item from the end, copying all
         * positively flagged elements from p.
         */

        v = &w->item[res];
        for (;;) {
            if (flags[--cnt])
            {
                svalue_t sv;

                /* Copy the element and update the ref-count */

                *--v = sv = p->item[cnt];
                if (sv.type == T_STRING)
                {
                    (void)ref_mstring(sv.u.str);
                }
                else
                {
                    (void)ref_object(sv.u.ob, "filter");
                }

                /* Loop termination check moved in here to save cycles */
                if (v == w->item)
                    break;
            }
        } /* for () */
    } /* if (res) */

    /* Cleanup and return */
    free_array(p);

    do {
        free_svalue(sp--);
    } while(--num_arg >= 0);

    /* sp now points at the former <arr> argument, so the .type is
     * already correct.
     */
    sp->u.vec = w;
    return sp;
}

/*-------------------------------------------------------------------------*/
svalue_t *
f_map_objects (svalue_t *sp, int num_arg)

/* EFUN map_objects()
 *
 *   mixed *map_objects (object *arr, string fun, mixed extra, ...)
 *
 * Map the objects in <arr> by calling the lfun obj-><fun>(<extra>...)
 * and return an array of the function call results.
 *
 * The objects can be true objects or filenames. In the latter case, the
 * function tries to load the object before calling the lfun. Any non-object
 * element in <arr> is ignored and a 0 is returned in its place.
 *
 * As a bonus, all references to destructed objects in <arr> are replaced
 * by proper 0es.
 */

{
    vector_t *p;          /* The <arr> argument */
    string_t *func;       /* The <fun> argument */
    svalue_t *arguments;  /* Beginning of 'extra' arguments on vm stack */
    vector_t *r;          /* Result vector */
    object_t *ob;         /* Object to call */
    mp_int size;          /* Size of <p> */
    int cnt;
    svalue_t *w, *v, *x;

    assign_eval_cost();
    inter_sp = sp;  /* In case of errors leave a clean stack behind */

    arguments = sp-num_arg+3;

    p = arguments[-2].u.vec;
    func = arguments[-1].u.str;
    num_arg -= 2;

    r = allocate_array(size = (mp_int)VEC_SIZE(p));
    arguments[-2].u.vec = r;

    push_array(inter_sp, p); /* Ref it from the stack in case of errors */

    /* Call <func> in every object, storing the result in r.
     *
     * Checking if <func> exists as shared string takes advantage of
     * the fact that every existing lfun name is stored as shared string.
     * If it's not shared, no object implements it and we can skip
     * the whole function call loop.
     */

    func = find_tabled(func);
    if (NULL != func)
    {
        for (cnt = size, v = p->item, x = r->item; --cnt >= 0; v++, x++) {

            /* Coerce <v> into a (non-destructed) object ob (if necessary
             * by loading it). If that doesn't work, simply continue
             * with the next element.
             */
            if (v->type != T_OBJECT) {
                if (v->type != T_STRING)
                    continue;
                if ( !(ob = get_object(v->u.str)) )
                    continue;
            } else {
                ob = v->u.ob;
                if (ob->flags & O_DESTRUCTED) {
                    assign_svalue(v, &const0);
                    continue;
                }
            }

            /* Abort the efun if this_object is destructed (slightly
             * strange place to check for it).
             */
            if (current_object->flags & O_DESTRUCTED)
                continue;

            /* Call the lfun and record the result */
            push_svalue_block(num_arg, arguments);
            w = sapply (func, ob, num_arg);
            if (w)
            {
                *x = *w;
                w->type = T_INVALID;
            }
        } /* for() */
    } /* if() */

    /* Clean up and return */
    do {
        free_svalue(sp--);
    } while(--num_arg >= 0);
    free_array(p);

    return sp;
} /* f_map_objects() */

/*-------------------------------------------------------------------------*/
static int
search_alist (svalue_t *key, vector_t *keylist)

/* Helper for insert_alist() and assoc().
 *
 * Search for <key> in the alist key vector <keylist> and return its position.
 * If <key> is not found, return the position at which the <key> would
 * have to be inserted (this might be sizeof(<keylist>), ie. the element
 * beyond the current end).
 *
 * The key vector must be sorted according to alist_cmd(), else the
 * binary search will return surely interesting but useless results.
 */

{
    mp_int i, o, d, keynum;

    if ( !(keynum = (mp_int)VEC_SIZE(keylist)) )
        return 0;

    /* Simple binary search */

    i = keynum >> 1;
    o = (i+2) >> 1;
    for (;;) {
        d = alist_cmp(key, &keylist->item[i]);
        if (d<0) {
            i -= o;
            if (i<0) {
                i = 0;
            }
        } else if (d>0) {
            i += o;
            if (i >= keynum) {
                i = keynum-1;
            }
        } else {
            return i;
        }
        if (o<=1) {
            if (alist_cmp(key, &keylist->item[i]) > 0) return i+1;
            return i;
        }
        o = (o+1) >> 1;
    }

    return 0;
}


/*-------------------------------------------------------------------------*/
#ifdef F_INSERT_ALIST

static svalue_t *
insert_alist (svalue_t *key, svalue_t * /* TODO: bool */ key_data, vector_t *list)

/* Implementation of efun insert_alist()
 *
 * The function can be used in two ways:
 *
 * 1. Insert/replace a (new) <key>:<keydata> tuple into the alist <list>.
 *    <key> and <key_data> have to point to an array of svalues. The first
 *    element is the key value, the following values the associated
 *    data values. The function will read as many elements from the
 *    array as necessary to fill the alist <list>.
 *    Result is a fresh copy of the modified alist.
 *
 * 2. Lookup a <key> in the alist <list> and return its index+1. The
 *    result is 0 if the key is not found.
 *    <key_data> must be NULL, <key> points to the svalue to be looked
 *    up, and <list> points to an alist with at least the key vector.
 *
 * If <list> is no alist, the result can be wrong (case 2.) or not
 * an alist either (case 1.).
 *
 * If the <key> is a string, it is made shared.
 *
 * TODO: Make the hidden flag 'key_data' a real flag.
 */

{
    static svalue_t stmp; /* Result value */
    mp_int i,j,ix;
    mp_int keynum, list_size;  /* Number of keys, number of alist vectors */
    int new_member;            /* Flag if a new tuple is given */

    /* If key is a string, make it shared */
    if (key->type == T_STRING && !mstr_d_tabled(key->u.str))
    {
        key->u.str = make_tabled(key->u.str);
    }

    keynum = (mp_int)VEC_SIZE(list->item[0].u.vec);

    /* Locate the key */
    ix = search_alist(key, list->item[0].u.vec);

    /* If its just a lookup: return the result.
     */
    if (key_data == NULL) {
         put_number(&stmp, ix);
         return &stmp;
    }

    /* Prepare the result alist vector */
    put_array(&stmp, allocate_array(list_size = (mp_int)VEC_SIZE(list)));

    new_member = ix == keynum || alist_cmp(key, &list->item[0].u.vec->item[ix]);

    /* Loop over all key/data vectors in <list>, insert/replace the
     * new value and put the new vector into <stmp>.
     */
    for (i = 0; i < list_size; i++) {
        vector_t *vtmp;

        if (new_member) {

            svalue_t *pstmp = list->item[i].u.vec->item;

            vtmp = allocate_array(keynum+1);
            for (j=0; j < ix; j++) {
               assign_svalue_no_free(&vtmp->item[j], pstmp++);
            }
            assign_svalue_no_free(&vtmp->item[ix], i ? &key_data[i] : key );
            for (j = ix+1; j <= keynum; j++) {
               assign_svalue_no_free(&vtmp->item[j], pstmp++);
            }

        } else {

            vtmp = slice_array(list->item[i].u.vec, 0, keynum-1);
            if (i)
                assign_svalue(&vtmp->item[ix], &key_data[i]);
                /* No need to assign the key value: it's already there. */

        }

        stmp.u.vec->item[i].type=T_POINTER;
        stmp.u.vec->item[i].u.vec=vtmp;
    }

    /* Done */
    return &stmp;
} /* insert_alist() */

#endif

/*-------------------------------------------------------------------------*/
#ifdef F_INSERT_ALIST

svalue_t *
f_insert_alist (svalue_t *sp, int num_arg)

/* EFUN insert_alist()
 *
 *   mixed* insert_alist (mixed key, mixed data..., mixed * alist)
 *   int    insert_alist (mixed key, mixed * keys)
 *
 * 1. Form: Alist Insertion
 *
 *   The <key> and all following <data> values are inserted
 *   into the <alist>. If an entry for <key> already exists
 *   in the list, just the data values are replaced. The number
 *   of <data> values must match the number of data arrays
 *   in the alist, naturally.
 *
 *   Result is the updated <alist>.
 *
 * 2. Form: Key Insertion
 *
 *   Insert the <key> into the (ordered) array of <keys>, so that
 *   subsequent assoc()s can perform quick lookups. Result is the
 *   index at which <key> was inserted (or already found).
 *
 *   CAVEAT: when working with string keys, the index might no longer
 *     be valid after the next call to insert_alist().
 */
/* When the key list of an alist contains destructed objects
   it is better not to free them till the next reordering by
   order_alist to retain the alist property.
 */

{
    int i;
    vector_t *list;
    long listsize;
    size_t keynum;
    svalue_t *key,*key_data,*ret;
    static LOCAL_VEC1(insert_alist_vec, T_NUMBER);
      /* Mock-alist for the insert_alist() key-insertion form.
       */

    if (sp->type != T_POINTER)
        vefun_arg_error(num_arg, T_POINTER, sp->type, sp);

    /* Make up an alist if only a key-insertion is required */
    if ( !(listsize = (long)VEC_SIZE(sp->u.vec))
     ||  sp->u.vec->item[0].type != T_POINTER )
    {
        list = &insert_alist_vec.v;
        *list->item = *sp;
        listsize = 1;
    }
    else
        list = sp->u.vec;

    /* Check the validity of the alist */
    keynum = VEC_SIZE(list->item[0].u.vec);
    for (i = 1; i < listsize; i++)
    {
        if (list->item[i].type != T_POINTER
         || VEC_SIZE(list->item[i].u.vec) != keynum)
        {
            error("Type or size mismatch of the data arrays.\n");
            /* NOTREACHED */
            return sp;
        }
    }

    /* Get and test the data to insert */
    if (num_arg == 2)
    {
        if (sp[-1].type != T_POINTER)
        {
            key_data = NULL;
            key = sp-1;
        }
        else
        {
            if (VEC_SIZE(sp[-1].u.vec) != (size_t)listsize)
            {
                error("Size mismatch of the data arrays.\n");
                /* NOTREACHED */
                return sp;
            }
            key_data = key = sp[-1].u.vec->item;
        }
    }
    else
    {
        if (num_arg - 1 != listsize)
        {
            error("Not enough data given.\n");
            /* NOTREACHED */
            return sp;
        }
        key_data = key = sp-num_arg+1;
    }

    /* Do the insertion */
    ret = insert_alist(key,key_data,list);
    sp = pop_n_elems(num_arg, sp);
    sp++;
    *sp = *ret;

    return sp;
} /* f_insert_alist() */

#endif /* F_INSERT_ALIST */

/*-------------------------------------------------------------------------*/
int
assoc (svalue_t *key, vector_t *list)

/* EFUN assoc(), also used for internal vector lookups.
 *
 * Lookup <key> in the alist key vector <list> and return its position.
 * If it is not found, return -1.
 *
 * The key vector must be sorted according to alist_cmd(), else the
 * result will be interesting, but useless.
 */

{
    int i;
    svalue_t shared_string_key; 
      /* The svalue used to pass the shared search key to search_alist().
       * It does not count as reference!
       */

    /* If key is a non-shared string, lookup and use the shared copy.
     */
    if (key->type == T_STRING && !mstr_d_tabled(key->u.str)) {

        shared_string_key.type = T_STRING;
        if ( !(shared_string_key.u.str = find_tabled(key->u.str)) )
            return -1;
        key = &shared_string_key;
    }

    i = search_alist(key, list);
    if (i == (int)VEC_SIZE(list) || alist_cmp(key, &list->item[i]))
        i = -1;

    return i;
} /* assoc() */

/*-------------------------------------------------------------------------*/
#ifdef F_ASSOC

svalue_t *
f_assoc (svalue_t *sp, int num_arg)

/* EFUN assoc()
 *
 *     int   assoc (mixed key, mixed *keys)
 *     mixed assoc (mixed key, mixed *alist [, mixed fail] )
 *     mixed assoc (mixed key, mixed *keys, mixed *data [, mixed fail])
 *
 * Search for <key> in the <alist> resp. in the <keys>.
 *
 * When the key list of an alist contains destructed objects
 * it is better not to free them till the next reordering by
 * order_alist to retain the alist property.
 */

{
    svalue_t *args;
    vector_t *keys,*data;
    svalue_t *fail_val;
    int ix;

    args = sp -num_arg +1;

    /* Analyse the arguments */
    if ( !VEC_SIZE(args[1].u.vec)
     ||  args[1].u.vec->item[0].type != T_POINTER )
    {
        keys = args[1].u.vec;
        if (num_arg == 2)
        {
            data = NULL;
        }
        else
        {
            if (args[2].type != T_POINTER
             || VEC_SIZE(args[2].u.vec) != VEC_SIZE(keys))
            {
                error("Number of values in key and data arrays differ.\n");
                /* NOTREACHED */
                return sp;
            }
            data = args[2].u.vec;
        }
        if (num_arg == 4)
        {
            fail_val = &args[3];
        }
        else
        {
            fail_val = &const0;
        }
    }
    else
    {
        keys = args[1].u.vec->item[0].u.vec;
        if (VEC_SIZE(args[1].u.vec) > 1)
        {
            if (args[1].u.vec->item[1].type != T_POINTER
             || VEC_SIZE(args[1].u.vec->item[1].u.vec) != VEC_SIZE(keys))
            {
                error("Number of values in key and data arrays differ.\n");
                /* NOTREACHED */
                return sp;
            }
            data = args[1].u.vec->item[1].u.vec;
        }
        else
        {
            data = NULL;
        }

        if (num_arg == 3) fail_val = &args[2];
        else if (num_arg == 2) fail_val = &const0;
        else
        {
            error("too many args to efun assoc\n");
            /* NOTREACHED */
            return sp;
        }
    }

    /* Call assoc() and push the result */
    ix = assoc(&args[0],keys);
    if (data == NULL)
    {
        sp = pop_n_elems(num_arg, sp);
        push_number(sp, ix);
    }
    else
    {
        assign_svalue(args
                     , ix == -1
                       ? fail_val
                       : (destructed_object_ref(&data->item[ix])
                         ? &const0
                         : &data->item[ix])
                     );
        sp = pop_n_elems(num_arg-1, sp);
    }

    return sp;
} /* f_assoc() */

#endif

/*-------------------------------------------------------------------------*/
static vector_t *
intersect_alist (vector_t *a1, vector_t *a2)

/* Used by efun intersect_alist() and by generic array intersection.
 *
 * Perform a fast intersection of the alist key vectors <a1> and <a2>.
 * The result is a new sorted(!) vector with all elements, which are present
 * in both input vectors.
 *
 * TODO: Maybe rename the fun.
 */

{
    vector_t *a3;
    mp_int d, l, i1, i2, a1s, a2s;

    a1s = (mp_int)VEC_SIZE(a1);
    a2s = (mp_int)VEC_SIZE(a2);
    a3 = allocate_array( a1s < a2s ? a1s : a2s);
    for (i1=i2=l=0; i1 < a1s && i2 < a2s; ) {
        d = alist_cmp(&a1->item[i1], &a2->item[i2]);
        if (d<0)
            i1++;
        else if (d>0)
            i2++;
        else {
            assign_svalue_no_free(&a3->item[l++], &a2->item[(i1++,i2++)] );
        }
    }
    return shrink_array(a3, l);
}

/*-------------------------------------------------------------------------*/
#ifdef F_INTERSECT_ALIST

svalue_t *
f_intersect_alist (svalue_t *sp)

/* EFUN intersect_alist()
 *
 *   mixed * intersect_alist (mixed * list1, mixed * list2)
 *
 * Does a fast set intersection on alist key vectors (NOT on full
 * alists!).
 *
 * The result is a new sorted(!) vector with all elements, which are present
 * in both input vectors.
 *
 * The operator '&' does set intersection on arrays in
 * general.
 *
 * TODO: Maybe rename the efun.
 */

{
    vector_t *rc;

    rc = intersect_alist(sp[-1].u.vec, sp->u.vec);

    free_svalue(sp--);
    free_array(sp->u.vec);
    sp->u.vec = rc;

    return sp;
} /* f_intersect_alist() */

#endif /* F_INTERSECT_ALIST */

/*-------------------------------------------------------------------------*/
vector_t *
intersect_array (vector_t *a1, vector_t *a2)

/* OPERATOR & (array intersection)
 *
 * Perform an intersection of the two vectors <a1> and <a2>.
 * The result is a new vector with all elements which are present in both
 * input vectors.
 *
 * The result vector is also sorted according to alist_cmp(), but
 * don't rely on it.
 */

{
    vector_t *vtmpp1, *vtmpp2, *vtmpp3;
    static svalue_t ltmp = { T_POINTER };

    /* Order the two ingoing lists and then perform an alist intersection.
     */

    ltmp.u.vec = a1;
    vtmpp1 = order_alist(&ltmp, 1, 1);
    free_array(ltmp.u.vec);

    ltmp.u.vec = a2;
    vtmpp2 = order_alist(&ltmp, 1, 1);
    free_array(ltmp.u.vec);

    vtmpp3 = intersect_alist(vtmpp1->item[0].u.vec, vtmpp2->item[0].u.vec);

    free_array(vtmpp1);
    free_array(vtmpp2);

    return vtmpp3;
}

/*-------------------------------------------------------------------------*/
#ifdef F_ORDER_ALIST

svalue_t *
f_order_alist (svalue_t *sp, int num_arg)

/* EFUN order_alist()
 *
 *   mixed *order_alist(mixed *keys, mixed *|void data, ...)
 *
 * Creates an alist.
 *
 * Either takes an array containing keys, and others containing
 * the associated data, where all arrays are to be of the same
 * length, or takes a single array that contains as first member
 * the array of keys and has an arbitrary number of other members
 * containing data, each of wich has to be of the same length as
 * the key array. Returns an array holding the sorted key array
 * and the data arrays; the same permutation that is applied to
 * the key array is applied to all data arrays.
 */

{
    int i;
    svalue_t *args;
    vector_t *list;
    long listsize;
    Bool reuse;
    size_t keynum;

    args = sp-num_arg+1;

    /* Get the key array to order */
    if (num_arg == 1
      && ((list = args->u.vec), (listsize = (long)VEC_SIZE(list)))
      && list->item[0].type == T_POINTER)
    {
        args     = list->item;
        reuse = (list->ref == 1);
    }
    else
    {
        listsize = num_arg;
        reuse = MY_TRUE;
    }
    keynum = VEC_SIZE(args[0].u.vec);

    /* Get the data arrays to order */
    for (i = 0; i < listsize; i++)
    {
        if (args[i].type != T_POINTER
         || VEC_SIZE(args[i].u.vec) != keynum)
        {
            error("bad data array %d in call to order_alist\n",i);
        }
    }

    /* Create the alist */
    list = order_alist(args, listsize, reuse);
    sp = pop_n_elems(num_arg, sp);
    sp++;
    put_array(sp, list);

    return sp;
} /* f_order_alist() */

#endif /* F_ORDER_ALIST */

/*-------------------------------------------------------------------------*/
svalue_t *
f_transpose_array (svalue_t *sp)

/* EFUN transpose_array()
 *
 *   mixed *transpose_array (mixed *arr);
 *
 * transpose_array ( ({ ({1,2,3}), ({a,b,c}) }) )
 * 		  => ({ ({1,a}), ({2,b)}, ({3,c}) })
 *
 * transpose_array() applied to an alist results in an array of
 * ({ key, data }) pairs, useful if you want to use sort_array()
 * or filter_array() on the alist.
 *
 * TODO: There should be something like this for mappings.
 */

{
    vector_t *v;  /* Input vector */
    vector_t *w;  /* Result vector */
    mp_int a;     /* size of <v> */
    mp_int b;     /* size of <v>[ix] for all ix */
    mp_int i, j;
    int no_copy;
      /* 1 if <v> has only one ref, else 0. Not just a boolean, it
       * is compared with the ref counts of the subvectors of v.
       */
    svalue_t *x, *y, *z;
    int o;

    /* Get and test the arguments */
    v = sp->u.vec;

    if ( !(a = (mp_int)VEC_SIZE(v)) )
        return sp;

    /* Find the widest subarray in the main array */
    b = 0;
    for (x = v->item, i = a; i > 0; i--, x++)
    {
        mp_int c;

        if (x->type != T_POINTER)
        {
              error("Bad arg 1 to transpose_array(): not an array of arrays.\n");
              /* NOTREACHED */
              return sp;
        }
        c = (mp_int)VEC_SIZE(x->u.vec);
        if (c > b)
            b = c;
    }

    /* If all subarrays are empty, just return an empty array */
    if (!b)
    {
        sp->u.vec = ref_array(v->item->u.vec);
        free_array(v);
        return sp;
    }

    no_copy = (v->ref == 1) ? 1 : 0;

    /* Allocate and initialize the result vector */
    w = allocate_uninit_array(b);
    for (j = b, x = w->item; --j >= 0; x++)
    {
        put_array(x, allocate_array(a));
    }

    o = offsetof(vector_t, item);

    for (i = a, y = v->item; --i >= 0; o += sizeof(svalue_t), y++)
    {
        mp_int c;

        x = w->item;
        if (y->type != T_POINTER)
            break;

        z = y->u.vec->item;

        c = b;
        if (VEC_SIZE(y->u.vec) < (size_t)b
         && !(c = (mp_int)VEC_SIZE(y->u.vec)) )
                continue;

        if (y->u.vec->ref == no_copy)
        {
            /* Move the values to the result vector */

            j = c;
            do {
                transfer_svalue_no_free(
                  (svalue_t *)((char*)x->u.vec+o),
                  z
                );
                x++;
                z++;
            } while (--j > 0);
            free_empty_vector(y->u.vec);
            y->type = T_INVALID;
        }
        else
        {
            /* Assign the values to the result vector */

            j = c;
            do {
                assign_svalue_no_free(
                  (svalue_t *)((char*)x->u.vec+o),
                  z
                );
                x++;
                z++;
            } while (--j > 0);
        }
    }

    /* Clean up and return the result */

    free_array(sp->u.vec);
    sp->u.vec = w;
    return sp;
} /* f_transpose_array() */

/*=========================================================================*/

/* EFUN unique_array()
 *
 *   mixed *unique_array (object *obarr, string seperator, mixed skip = 0)
 *
 * Group all those objects from <obarr> together for which the
 * <separator> function (which is called in every object) returns the
 * same value. Objects for which the function returns the <skip> value
 * and all non-object elements are omitted fully from the result.
 *
 * The returned array is an array of arrays of objects in the form:
 *
 *       ({ ({ Same1:1, Same1:2, ... Same1:N }),
 *          ({ Same2:1, Same2:2, ... Same2:N }),
 *             ....
 *          ({ SameM:1, SameM:2, ... SameM:N })
 *       })
 *
 * The result of <separator>() (the 'marker value') must be a number,
 * a string, an object or an array.
 *
 * Basic purpose of this efun is to speed up the preparation of an
 * inventory description - e.g. it allows to to fold all objects with
 * identical descriptions into one textline.
 *
 * Other applications are possible, for example:
 *
 *   mixed *arr;
 *   arr=unique_array(users(), "_query_level", -1);
 *
 * This will return an array of arrays holding all user objects
 * grouped together by their user levels. Wizards have a user
 * level of -1 so they will not appear in the the returned array.
 *
 * TODO: Expand unique_array(), e.g. by taking a closure as function
 * TODO:: or provide a simulation.
 * TODO: Allow unique_array() to tag the returned groups with the
 * TODO:: value returned by the separator().
 * TODO: unique_array() is almost big enough for a file on its own.
 */

/*-------------------------------------------------------------------------*/

/* The function builds a comb of unique structures: every tooth lists
 * all objects with the same marker value, with the first structure
 * of every tooth linked together to form the spine:
 *
 *   -> Marker1:1 -> Marker1:2 -> ...
 *         |
 *         V
 *      Marker2:1 -> Marker2:2 -> ...
 *         |
 *         V
 *        ...
 */

struct unique
{
    int count;            /* Number of structures in this tooth */
    svalue_t *val;        /* The object itself */
    svalue_t mark;        /* The marker value for this object */
    struct unique *same;  /* Next structure in this tooth */
    struct unique *next;  /* Next tooth head */
};

/*-------------------------------------------------------------------------*/
static int
sameval (svalue_t *arg1, svalue_t *arg2)

/* Return true if <arg1> is identical to <arg2>.
 * For arrays, this function only compares if <arg1> and <arg2> refer
 * to the same array, not the values.
 */

{
    if (!arg1 || !arg2) return 0;
    if (arg1->type == T_NUMBER && arg2->type == T_NUMBER) {
        return arg1->u.number == arg2->u.number;
    } else if (arg1->type == T_POINTER && arg2->type == T_POINTER) {
        return arg1->u.vec == arg2->u.vec;
    } else if (arg1->type == T_STRING && arg2->type == T_STRING) {
        return mstreq(arg1->u.str, arg2->u.str);
    } else if (arg1->type == T_OBJECT && arg2->type == T_OBJECT) {
        return arg1->u.ob == arg2->u.ob;
    } else
        return 0;
}


/*-------------------------------------------------------------------------*/
static int
put_in (Mempool pool, struct unique **ulist
       , svalue_t *marker, svalue_t *elem)

/* Insert the object <elem> according to its <marker> value into the comb
 * of unique structures. <ulist> points to the root pointer of this comb.
 * Return the (new) number of distinct markers.
 */

{
    struct unique *llink, *slink, *tlink;
    int cnt;                      /* Number of distinct markers */
    Bool fixed;                   /* True: <elem> was inserted */

    llink = *ulist;
    cnt = 0;
    fixed = 0;

    /* Loop through the comb's top level, counting the distinct marker
     * and searching for the right teeth to insert <elem> into.
     */
    while (llink) {
        if (!fixed && sameval(marker, &(llink->mark))) {

            /* Insert the new <elem> here
             */
            for (tlink = llink; tlink->same; tlink = tlink->same) tlink->count++;
            tlink->count++;
            /* TODO: Is the above really necessary?
             *   slink = new unique; llink->same = slink; llink->count++;
             * should be sufficient.
             */

            slink = mempool_alloc(pool, sizeof(struct unique));
            if (!slink)
            {
                error("(unique_array) Out of memory (%lu bytes pooled) "
                      "for comb.\n", (unsigned long)sizeof(struct unique));
                /* NOTREACHED */
                return 0;
            }
            slink->count = 1;
            assign_svalue_no_free(&slink->mark,marker);
            slink->val = elem;
            slink->same = NULL;
            slink->next = NULL;
            tlink->same = slink;

            fixed = 1; /* ...just continue to count now */
            /* TODO: Do not recount the comb size all the time! */
        }

        llink=llink->next;
        cnt++;
    }
    if (fixed)
        return cnt;

    /* It's a really new marker -> start a new tooth in the comb.
     */
    llink = mempool_alloc(pool, sizeof(struct unique));
    if (!llink)
    {
        error("(unique_array) Out of memory (%lu bytes pooled) "
              "for comb.\n", (unsigned long)sizeof(struct unique));
        /* NOTREACHED */
        return 0;
    }
    llink->count = 1;
    assign_svalue_no_free(&llink->mark,marker);
    llink->val = elem;
    llink->same = NULL;

    llink->next = *ulist;
    *ulist = llink;

    return cnt+1;
}


/*-------------------------------------------------------------------------*/
static vector_t *
make_unique (vector_t *arr, string_t *func, svalue_t *skipnum)

/* The actual implementation of efun unique_array();
 *
 * The caller made sure that <arr> contains no destructed objects.
 */

{
    Mempool    pool;      /* Pool for the unique structures */
    svalue_t *v;
    vector_t *ret;        /* Result vector */
    vector_t *res;        /* Current sub vector in ret */
    struct unique *head;  /* Head of the unique comb */
    struct unique *nxt;
    mp_int arr_size;      /* Size of the incoming <arr>ay */
    mp_int ant;           /* Number of distinct markers */
    mp_int cnt, cnt2;

    head = NULL;

    arr_size = (mp_int)VEC_SIZE(arr);

    /* Special case: unifying an empty array */
    if (!arr_size)
        return allocate_array(0);
    
    /* Get the memory for the arr_size unique-structures we're going
     * to need. 
     * TODO: Implement an automatic memory-cleanup in case of errors,
     * TODO:: e.g. by adding a dedicated structure on the runtime stack.
     */
    pool = new_mempool(arr_size * sizeof(*head));
    if (!pool)
        error("(unique_array) Out of memory: (%lu bytes) for mempool\n"
             , arr_size * sizeof(*head));

    ref_array(arr);  /* Prevent apply from freeing this */


    /* Build the comb structure.
     */
    ant = 0;
    for (cnt = 0; cnt < arr_size; cnt++)
        if (arr->item[cnt].type == T_OBJECT) {
            v = apply(func,arr->item[cnt].u.ob, 0);
            if (v && !sameval(v, skipnum))
                ant = put_in(pool, &head, v, &(arr->item[cnt]));
        }

    deref_array(arr); /* Undo the protection from above */

    ret = allocate_array(ant);

    /* Copy the objects from the comb structure into the result vector,
     * deallocating the structure by this.
     * The elements are stored in reverse to compensate put_in(),
     * but TODO: does someone really care?
     */

    for (cnt = ant-1; cnt >= 0; cnt--) {
        res = allocate_array(head->count);
        put_array(ret->item+cnt, res);

        nxt = head;
        head = head->next;

        cnt2 = 0;
        while (nxt) {
            assign_svalue_no_free (&res->item[cnt2++], nxt->val);
            free_svalue(&nxt->mark);
            nxt = nxt->same;
        }

        if (!head)
            break; /* It shouldn't but, to avoid skydive just in case */
    }

    mempool_delete(pool);
    
    return ret;
} /* make_unique() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_unique_array (svalue_t *sp)

/* EFUN unique_array()
 *
 *   mixed unique_array(object *obarr, string seperator)
 *   mixed unique_array(object *obarr, string seperator, mixed skip)
 *
 * Groups objects together for which the separator function
 * returns the same value. obarr should be an array of objects,
 * other types are ignored. The separator function is called only
 * once in each object in obarr. If no separator function is
 * given, 0 is used instead of a return value.
 * If a 3rd argument is given and this argument matches the
 * return value of the separator function this object will not be
 * included in the returned array.
 */

{
    vector_t *res;

    check_for_destr((sp-2)->u.vec);
    res = make_unique((sp-2)->u.vec, (sp-1)->u.str, sp);

    /* Clean up the stack and push the result */
    free_svalue(sp--);
    free_svalue(sp--);
    free_svalue(sp);

    if (res)
        put_array(sp, res);
    else
        put_number(sp, 0);

    return sp;
} /* f_unique_array() */

/***************************************************************************/

