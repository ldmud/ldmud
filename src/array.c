/*---------------------------------------------------------------------------
 * Array handling functions.
 *
 *---------------------------------------------------------------------------
 * TODO: Rewrite the low-level functions (like allocate_array()) to return
 * TODO:: failure codes (errno like) instead of throwing errors. In addition,
 * TODO:: provide wrapper functions which do throw errorf()s, so that every
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
 */

#include "driver.h"
#include "typedefs.h"

#include "my-alloca.h"
#include <stddef.h>

#include "array.h"
#include "backend.h"
#include "closure.h"    /* closure_cmp(), closure_eq() */
#include "interpret.h"
#include "main.h"
#include "mapping.h"
#include "mempools.h"
#include "mstrings.h"
#include "object.h"
#include "stdstrings.h"
#include "simulate.h"
#include "svalue.h"
#include "swap.h"
#include "wiz_list.h"
#include "xalloc.h"

#include "i-svalue_cmp.h"

/*-------------------------------------------------------------------------*/

#define ALLOC_VECTOR(nelem) \
      ((size_t)nelem >= (SSIZE_MAX - sizeof(vector_t)) / sizeof(svalue_t)) \
      ? NULL \
      : (vector_t *)xalloc_pass(sizeof(vector_t) + \
                                sizeof(svalue_t) * (nelem - 1))

/* ALLOC_VECTOR(size,file,line): Allocate dynamically the memory for
 *    a vector of <size> elements.
 * TODO: Use SIZET_MAX instead of SSIZE_MAX, see port.h
 */

/*-------------------------------------------------------------------------*/

int num_arrays;
  /* Total number of allocated arrays */

vector_t null_vector = { VEC_HEAD(0), { { T_INVALID } } };
  /* The global empty array ({}).
   * Reusing it is cheaper than repeated allocations/deallocations.
   */

void (*allocate_array_error_handler) (const char *, ...)
  = errorf; /* from simulate.c */
  /* This handler is called if an allocation fails.
   * Usually it points to simulate::errorf(), but the swapper
   * replaces it temporarily with its own dummy handler when
   * swapping in an object.
   */

/*-------------------------------------------------------------------------*/
vector_t *
_allocate_array(mp_int n MTRACE_DECL)

/* Allocate an array for <n> elements (but not more than the current
 * maximum) and return the pointer.
 * The elements are initialised to the svalue 0.
 *
 * If the allocations fails (and errorf() does return), a 0 pointer
 * may be returned. This is usually only possible when arrays
 * are allocated from the swapper.
 *
 * Allocating an array of size 0 will return a reference to the
 * globally shared empty array.
 *
 * If possible, annotate the allocations with <malloc_trace> and <...line>
 */

{
    mp_int i;
    vector_t *p;
    svalue_t *svp;

    if (n < 0 || (max_array_size && (size_t)n > max_array_size))
        errorf("Illegal array size: %"PRIdMPINT".\n", n);

    if (n == 0) {
        p = ref_array(&null_vector);
        return p;
    }

    num_arrays++;

    p = ALLOC_VECTOR(n);
    if (!p) {
#ifndef MALLOC_TRACE
        (*allocate_array_error_handler)
            ("Out of memory: array[%"PRIdMPINT"]\n", n);
#else
        (*allocate_array_error_handler)
            ("(%s:%d) Out of memory: array[%"PRIdMPINT"]\n"
             MTRACE_PASS, n);
#endif
        return 0;
    }

    p->ref = 1;
    p->size = n;
    if (current_object)
        (p->user = current_object->user)->size_array += n;
    else
        (p->user = &default_wizlist_entry)->size_array += n;

    svp = p->item;
    for (i = n; --i >= 0; )
        *svp++ = const0;

    return p;
}

/*-------------------------------------------------------------------------*/
vector_t *
_allocate_array_unlimited(mp_int n MTRACE_DECL)

/* Allocate an array for <n> elements and return the pointer.
 * The elements are initialised to the svalue 0.
 *
 * If the allocations fails (and errorf() does return), a 0 pointer
 * may be returned. This is usually only possible when arrays
 * are allocated from the swapper.
 *
 * Allocating an array of size 0 will return a reference to the
 * globally shared empty array.
 *
 * If possible, annotate the allocations with <malloc_trace_file> and <...line>
 */

{
    mp_int i;
    vector_t *p;
    svalue_t *svp;

    if (n < 0)
        errorf("Illegal array size: %"PRIdMPINT".\n", n);

    if (n == 0) {
        p = ref_array(&null_vector);
        return p;
    }

    num_arrays++;

    p = ALLOC_VECTOR(n);
    if (!p) {
#ifndef MALLOC_TRACE
        (*allocate_array_error_handler)
            ("Out of memory: unlimited array[%"PRIdMPINT"]\n", n);
#else
        (*allocate_array_error_handler)
            ("(%s:%d) Out of memory: unlimited array[%"PRIdMPINT"]\n"
            MTRACE_PASS, n);
#endif
        return 0;
    }

    p->ref = 1;
    p->size = n;
    if (current_object)
        (p->user = current_object->user)->size_array += n;
    else
        (p->user = &default_wizlist_entry)->size_array += n;

    svp = p->item;
    for (i = n; --i >= 0; )
        *svp++ = const0;

    return p;
}

/*-------------------------------------------------------------------------*/
vector_t *
_allocate_uninit_array (mp_int n MTRACE_DECL)

/* Allocate an array for <n> elements (but no more than the current
 * maximum) and return the pointer.
 * The elements are not initialised.
 * If the allocations fails (and errorf() does return), a 0 pointer
 * may be returned.
 *
 * Allocating an array of size 0 will return a reference to the
 * globally shared empty array.
 *
 * If possible, annotate the allocations with <malloc_trace_file> and <...line>
 */

{
    vector_t *p;

    if (n < 0 || (max_array_size && (size_t)n > max_array_size))
        errorf("Illegal array size: %"PRIdMPINT".\n", n);

    if (n == 0) {
        p = ref_array(&null_vector);
        return p;
    }

    num_arrays++;

    p = ALLOC_VECTOR(n);
    if (!p) {
#ifndef MALLOC_TRACE
        (*allocate_array_error_handler)
            ("Out of memory: uninited array[%"PRIdMPINT"]\n", n);
#else
        (*allocate_array_error_handler)
            ("(%s:%d) Out of memory: uninited array[%"PRIdMPINT"]\n"
            MTRACE_PASS, n);
#endif
        return 0;
    }

    p->ref = 1;
    p->size = n;
    if (current_object)
        (p->user = current_object->user)->size_array += n;
    else
        (p->user = &default_wizlist_entry)->size_array += n;

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
        fatal("Vector with %"PRIdPINT" refs passed to _free_vector()\n",
              p->ref);
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
#ifdef USE_ALISTS
static INLINE vector_t *
i_shrink_array (vector_t *p, mp_int n)

/* Create and return a new array containing just the first <n> elements
 * of <p>. <p> itself is freed (and thus possibly deallocated).
 * This function is only needed if alists are used.
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

vector_t * shrink_array (vector_t *p, mp_int n) { return i_shrink_array(p, n); }

#define shrink_array(p,n) i_shrink_array(p,n)
#endif

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
 * objects and replace them with svalue 0s. Subvectors are not checked,
 * though.
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
#if defined(GC_SUPPORT)

void
clear_array_size (void)

/* Clear the statistics about the number and memory usage of all vectors
 * in the game.
 */

{
    wiz_list_t *wl;

    num_arrays = 0;
    default_wizlist_entry.size_array = 0;
    for (wl = all_wiz; wl; wl = wl->next)
        wl->size_array = 0;
} /* clear_array_size(void) */


/*-------------------------------------------------------------------------*/
void
count_array_size (vector_t *vec)

/* Add the vector <vec> to the statistics.
 */

{
    num_arrays++;
    vec->user->size_array += VEC_SIZE(vec);
} /* count_array_size(void) */

#endif /* GC_SUPPORT */

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
 *   explode("", whatever)      -> { "" }
 */

{
    char *p, *beg;
    long num;
    long len, left;
    vector_t *ret;
    string_t *buff;

    /* Special case: str is an empty string. */
    if (mstrsize(str) == 0)
    {
        ret = allocate_array(1);
        buff = new_n_mstring("", 0);
        put_string(ret->item, buff);
        return ret;
    }
    
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
                    outofmem(1, "explode() on a string");
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
            len = (long)mstrsize(str);
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
                    outofmem(len, "explode() on a string");
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
                outofmem(len, "explode() on a string");
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
                outofmem(bufflen, "buffer for explode()");
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
        outofmem(len, "last fragment in explode()");
    }
    put_string(ret->item + num, buff);

    return ret;
} /* explode_string() */

/*-------------------------------------------------------------------------*/
string_t *
arr_implode_string (vector_t *arr, string_t *del MTRACE_DECL)

/* Implode the string vector <arr> by <del>, i.e. all strings from <arr>
 * with <del> interspersed are concatenated into one string. The
 * resulting string is returned. The function will return at least
 * the empty string "".
 *
 * Non-string elements are ignored; elements referencing destructed
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
        /* caller raises the errorf() */
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
} /* add_array() */

/*-------------------------------------------------------------------------*/
static INLINE void
sanitize_array (vector_t * vec)

/* In the given array, make all strings tabled, and replace destructed
 * object references by svalue 0s.
 * Used for example in preparation for ordering the array.
 */

{
    size_t     j, keynum;
    svalue_t * inpnt;

    keynum = VEC_SIZE(vec);
    for ( j = 0, inpnt = vec->item; j < keynum; j++, inpnt++)
    {
        if (inpnt->type == T_STRING)
        {
            if (!mstr_tabled(inpnt->u.str))
            {
                inpnt->u.str = make_tabled(inpnt->u.str);
            }
        }
        else if (destructed_object_ref(inpnt))
        {
            free_svalue(inpnt);
            put_number(inpnt, 0);
        }
    }
} /* sanitize_array() */

/*-------------------------------------------------------------------------*/
ptrdiff_t *
get_array_order (vector_t * vec )

/* Determine the order of the elements in vector <vec> and return the
 * sorted indices (actually svalue_t* pointer diffs). The order is
 * determined by svalue_cmp() (which happens to be high-to-low).
 *
 * As a side effect, strings in the vector are made shared, and
 * destructed objects in the vector are replaced by svalue 0s.
 */

{
    ptrdiff_t * sorted;
      /* The vector elements in sorted order, given as the offsets of the array
       * element in question to the start of the vector. This way,
       * sorted[] needs only to be <keynum> elements long.
       * sorted[] is created from root[] after sorting.
       */

    svalue_t **root;
      /* Auxiliary array with the sorted keys as svalue* into vec.
       * This way the sorting is given by the order of the pointers, while
       * the original position is given by (pointer - vec->item).
       * The very first element is a dummy (heapsort uses array indexing
       * starting with index 1), the next <keynum> elements are scratch
       * area, the final <keynum> elements hold the sorted keys in reverse
       * order.
       */
    svalue_t **root2;   /* Aux pointer into *root. */
    svalue_t *inpnt;    /* Pointer to the value to copy into the result */
    mp_int keynum;      /* Number of keys */
    int j;

    keynum = (mp_int)VEC_SIZE(vec);

    xallocate(sorted, keynum * sizeof(ptrdiff_t) + sizeof(ptrdiff_t)
             , "sorted index array");
      /* The extra sizeof(ptrdiff_t) is just to have something in
       * case keynum is 0.
       */

    sanitize_array(vec);

    /* For small arrays, use something else but Heapsort - trading
     * less overhead for worse complexity.
     * TODO: The limit of '6' is arbitrary (it was the transition point
     * TODO:: on my machine) - a better way would be to test the system
     * TODO:: speed at startup.
     */
    if (keynum <= 6)
    {
        switch (keynum)
        {
        case 0:
            /* Do nothing */
            break;

        case 1:
            sorted[0] = 0;
            break;

        case 2:
            if (svalue_cmp(vec->item, vec->item + 1) > 0)
            {
                sorted[0] = 0;
                sorted[1] = 1;
            }
            else
            {
                sorted[0] = 1;
                sorted[1] = 0;
            }
            break;

        case 3:
          {
            int d;

            sorted[0] = 0;
            sorted[1] = 1;
            sorted[2] = 2;
            d = svalue_cmp(vec->item, vec->item + 1);
            if (d < 0)
            {
                sorted[1] = 0;
                sorted[0] = 1;
            }
            d = svalue_cmp(vec->item + sorted[0], vec->item + 2);
            if (d < 0)
            {
                ptrdiff_t tmp = sorted[2];
                sorted[2] = sorted[0];
                sorted[0] = tmp;
            }
            d = svalue_cmp(vec->item + sorted[1], vec->item + sorted[2]);
            if (d < 0)
            {
                ptrdiff_t tmp = sorted[2];
                sorted[2] = sorted[1];
                sorted[1] = tmp;
            }
            break;
          } /* case 3 */

        default:
          {
            size_t  start;  /* Index of the next position to set */

            /* Initialise the sorted[] array */
            for (start = 0; (mp_int)start < keynum; start++)
                sorted[start] = (ptrdiff_t)start;

            /* Outer loop: walk start through the array, being the position
             * where the next highest element has to go.
             */
            for (start = 0; (mp_int)start < keynum-1; start++)
            {
                size_t    max_idx;  /* Index (in sorted[]) of the current max */
                svalue_t *max;      /* Pointer to the current max svalue */
                size_t    test_idx; /* Index of element to test */

                /* Find the highest element in the remaining vector */
                max_idx = start;
                max = vec->item + sorted[start];

                for (test_idx = start+1; (mp_int)test_idx < keynum; test_idx++)
                {
                    svalue_t *test = vec->item + sorted[test_idx];

                    if (svalue_cmp(max, test) < 0)
                    {
                        max_idx = test_idx;
                        max = test;
                    }
                }

                /* Put the found maximum at position start */
                if (max_idx != start)
                {
                    ptrdiff_t tmp = sorted[max_idx];
                    sorted[max_idx] = sorted[start];
                    sorted[start] = tmp;
                }
            }
            break;
          } /* case default */
        } /* switch(keynum) */

        return sorted;
    }

    /* Allocate the auxiliary array. */
    root = (svalue_t **)alloca(keynum * sizeof(svalue_t *[2])
                                           + sizeof(svalue_t)
                              );
    if (!root)
    {
        errorf("Stack overflow in get_array_order()");
        /* NOTREACHED */
        return NULL;
    }

    /* Heapsort vec into *root.
     */

    /* Heapify the keys into the first half of root */
    for ( j = 1, inpnt = vec->item
        ; j <= keynum
        ; j++, inpnt++)
    {
        int curix, parix;

        /* propagate the new element up in the heap as much as necessary */
        for (curix = j; 0 != (parix = curix>>1); ) {
            if ( svalue_cmp(root[parix], inpnt) > 0 ) {
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
                if (!root[child] || svalue_cmp(root[child], root[child2]) > 0)
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
        sorted[j] = root[keynum-j-1] - vec->item;

    return sorted;
} /* get_array_order() */

/*-------------------------------------------------------------------------*/
vector_t *
order_array (vector_t *vec)

/* Order the array <vec> and return a new vector with the sorted data.
 * The sorting order is the internal order defined by svalue_cmp() (which
 * happens to be high-to-low).
 *
 * This function and lookup_key() are used in several places for internal
 * lookup functions (e.g. in say()).
 *
 * As a side effect, strings in the vector are made shared, and
 * destructed objects in the vector are replaced by svalue 0s.
 */

{
    vector_t  * out;     /* The result vector of vectors */
    svalue_t  * outpnt;  /* Next result value element to fill in */
    ptrdiff_t * sorted;  /* The vector elements in sorted order */
    long        keynum;  /* Number of keys */
    long j;

    keynum = (long)VEC_SIZE(vec);

    sorted = get_array_order(vec);

    /* Copy the elements from the in-vector to the result vector.
     */
    out = allocate_array(VEC_SIZE(vec));
    outpnt = out->item;
    for (j = keynum; --j >= 0; )
    {
         assign_svalue_no_free(outpnt++, vec->item + sorted[j]);
    }

    xfree(sorted);

    return out;
} /* order_array() */

/*-------------------------------------------------------------------------*/
long
lookup_key (svalue_t *key, vector_t *vec)

/* Lookup up value <key> in ordered vector <vec> and return it's position.
 * If not found, return as negative number the position at which the
 * key would have to be inserted, incremented by 1. That is:
 *   -1          -> key should be at position 0,
 *   -2          -> key should be at position 1,
 *   -len(vec)-1 -> key should be appended to the vector.
 *
 * <vec> must be sorted according to svalue_cmp(), else the result will be
 * interesting, but useless.
 *
 * The function is used by object.c and pkg-alists.c .
 */

{
    mp_int i, o, d, keynum;
    svalue_t shared_string_key;
      /* The svalue used to shared search key during the search.
       * It does not count as reference!
       */

    /* If key is a non-shared string, lookup and use the shared copy.
     */
    if (key->type == T_STRING && !mstr_tabled(key->u.str))
    {
        shared_string_key.type = T_STRING;
        if ( !(shared_string_key.u.str = find_tabled(key->u.str)) )
        {
            return -1;
        }
        key = &shared_string_key;
    }

    if ( !(keynum = (mp_int)VEC_SIZE(vec)) )
        return -1;

    /* Simple binary search */

    i = keynum >> 1;
    o = (i+2) >> 1;
    for (;;) {
        d = svalue_cmp(key, &vec->item[i]);
        if (d < 0)
        {
            i -= o;
            if (i < 0)
            {
                i = 0;
            }
        }
        else if (d > 0)
        {
            i += o;
            if (i >= keynum)
            {
                i = keynum-1;
            }
        }
        else
        {
            /* Found! */
            return i;
        }

        if (o <= 1)
        {
            /* Last element to try */
            d = svalue_cmp(key, &vec->item[i]);
            if (d == 0) return i;
            if (d > 0) return -(i+1)-1;
            return -i-1;
        }
        o = (o+1) >> 1;
    }

    /* NOTREACHED */
    return -1;
} /* lookup_key() */

/*-------------------------------------------------------------------------*/
static Bool *
match_arrays (vector_t *vec1, vector_t *vec2)

/* Compare the contents of the two (unordered) vectors <vec1> and
 * <vec2> and return a boolean vector describing for each vector
 * which elements are in both.
 *
 * The resulting bool vector has len(vec1)+len(vec2) flags (but
 * at least 1); the first describing the elements of vec1, the last
 * describing those of vec2. Each flag is FALSE if the vector entry
 * is unique, and TRUE if the same value appears in the other vector.
 *
 * When out of memory, an errorf() is thrown.
 */

{
    size_t  len1, len2, len; /* Length of vec1, vec2, and both summed */
    Bool   *flags;           /* The resulting flag vector */

    len1 = VEC_SIZE(vec1);
    len2 = VEC_SIZE(vec2);

    /* Get the flag vector, default it to 'non matching'. */
    len = len1 + len2; if (!len) len = 1;
    xallocate(flags, len * sizeof(Bool), "flag vector");
    memset(flags, 0, len * sizeof(Bool));

    /* Test some special cases */

    /* Special case: if one of the vectors is empty, no elements match */
    if (len1 == 0 || len2 == 0)
        return flags;

    /* Special case: if one of the vectors has only one element,
     * a simple linear comparison is sufficient.
     */
    if (len1 == 1 || len2 == 1)
    {
        svalue_t * rover;  /* Pointer to the long vector elements */
        size_t     rlen;   /* Length (remaining) in the long vector */
        svalue_t * elem;   /* Pointer to the single-elem vector elements */
        Bool     * rflag;  /* Pointer to the long vector flags */
        Bool     * eflag;  /* Pointer to the single-elem vector flag */

        sanitize_array(vec1);
        sanitize_array(vec2);

        /* Sort out which vector is which */
        if  (len1 == 1)
        {
            /* Even more special case: both vectors have just one elem */
            if (len2 == 1)
            {
                if (!svalue_eq(vec1->item, vec2->item))
                {
                    flags[0] = flags[1] = MY_TRUE;
                }
                return flags;
            }

            /* vec1 is the short one */
            rover = vec2->item;
            rlen = len2;
            rflag = flags + len1;
            elem = vec1->item;
            eflag = flags;
        }
        else /* len2 == 1 */
        {
            /* vec2 is the short one */
            rover = vec1->item;
            rlen = len1;
            rflag = flags;
            elem = vec2->item;
            eflag = flags + len1;
        }

        /* Now loop over all elements in the long vector and compare
         * them to the one in the short vector.
         */
        for ( ; rlen != 0; rlen--, rover++, rflag++)
        {
            if (!svalue_eq(rover, elem))
                *rflag = *eflag = MY_TRUE;
        }

        /* Done */
        return flags;
    } /* if (one vector has only one element */

    /* The generic matching routine: first both arrays are ordered,
     * then compared side by side.
     */
    {
        ptrdiff_t *sorted1, *sorted2; /* Ordered indices to the vectors */
        ptrdiff_t *index1, *index2;   /* Current elements to compare */
        Bool      *flag1, *flag2;     /* flags base pointers */

        sorted1 = get_array_order(vec1);
        sorted2 = get_array_order(vec2);

        /* Set up the comparison */
        index1 = sorted1;
        index2 = sorted2;
        flag1 = flags;
        flag2 = flags + len1;

        /* Compare side by side. Any element left uncompared at
         * the end is automatically non-matching.
         */
        while (len1 != 0 && len2 != 0)
        {
            int d;

            d = svalue_cmp(vec1->item + *index1, vec2->item + *index2);
            if (d == 0)
            {
                /* Elements match */
                svalue_t *test_val = vec1->item+*index1;

                /* Important here is to remember that there might
                 * be several elements of the same value in a row.
                 * The side-by-side comparison itself is not able
                 * to handle it, so we have to check here manually
                 * for it.
                 * The loops will leave index1/index2 point to the
                 * first element after the sequence of matching ones.
                 */
                do {
                    flag1[*index1] = MY_TRUE;
                    index1++;
                    len1--;
                    if (len1 != 0)
                        d = svalue_eq(test_val, vec1->item + *index1);
                }
                while (len1 != 0 && d == 0);

                do {
                    flag2[*index2] = MY_TRUE;
                    index2++;
                    len2--;
                    if (len2 != 0)
                        d = svalue_eq(test_val, vec2->item + *index2);
                }
                while (len2 != 0 && d == 0);

                continue; /* Next iteration of the main loop */
            }

            /* Else advance in array(s) */
            if (d > 0)
            {
                index1++;
                len1--;
            }

            if (d < 0)
            {
                index2++;
                len2--;
            }
        } /* while (in both vectors) */

        /* Cleanup */
        xfree(sorted1);
        xfree(sorted2);

        /* Done */
        return flags;
    }

    /* NOTREACHED */
    return flags;

} /* match_array() */

/*-------------------------------------------------------------------------*/
vector_t *
subtract_array (vector_t *minuend, vector_t *subtrahend)

/* Subtract all elements in <subtrahend> from the vector <minuend>
 * and return the resulting difference vector.
 * <subtrahend> and <minuend> are freed.
 */

{
    Bool     *flags;       /* The result from match_arrays() */
    size_t    result_size; /* Size of the result array */
    vector_t *result;      /* Result array */
    svalue_t *dest;        /* Pointer for storing the result elements */
    size_t i;

    size_t minuend_size    = VEC_SIZE(minuend);
    size_t subtrahend_size = VEC_SIZE(subtrahend);

    /* Handle empty vectors quickly */

    if (minuend_size == 0 || subtrahend_size == 0)
    {
        free_array(subtrahend);
        return minuend;
    }

    /* Non-trivial arrays: match them up */
    flags = match_arrays(minuend, subtrahend);

    /* Count how many elements would be left in minuend
     * and allocate the result array.
     */
    for (i = result_size = 0; i < minuend_size; i++)
    {
        if (!flags[i])
            result_size++;
    }

    if (result_size == minuend_size)
    {
        /* No elements to remove */
        xfree(flags);
        free_array(subtrahend);
        return minuend;
    }

    if (max_array_size && result_size > max_array_size)
    {
        xfree(flags);
        free_array(minuend);
        free_array(subtrahend);
        errorf("Illegal array size: %lu.\n", (unsigned long)result_size);
    }

    result = allocate_array(result_size);

    /* Copy the elements to keep from minuend into result.
     * We count down result_size to be able to stop as early
     * as possible.
     */
    for ( dest = result->item, i = 0
        ; i < minuend_size && result_size != 0
        ; i++
        )
    {
        if (!flags[i])
        {
            assign_svalue_no_free(dest, minuend->item+i);
            dest++;
            result_size--;
        }
    }

    /* Cleanup and return */
    xfree(flags);
    free_array(minuend);
    free_array(subtrahend);

    return result;
} /* subtract_array() */

/*-------------------------------------------------------------------------*/
vector_t *
intersect_array (vector_t *vec1, vector_t *vec2)

/* OPERATOR & (array intersection)
 *
 * Perform an intersection of the two vectors <vec1> and <vec2>.
 * The result is a new vector with all elements which are present in both
 * input vectors.
 *
 * Both <vec1> and <vec2> are freed.
 */

{
    Bool     *flags;       /* The result from match_arrays() */
    size_t    result_size; /* Size of the result array */
    vector_t *result;      /* Result array */
    svalue_t *dest;        /* Pointer for storing the result elements */
    size_t i;

    size_t vec1_size = VEC_SIZE(vec1);
    size_t vec2_size = VEC_SIZE(vec2);

    /* Handle empty arrays quickly */

    if (vec1_size == 0 || vec2_size == 0)
    {
        free_array(vec2);
        free_array(vec1);
        return ref_array(&null_vector);
    }

    /* Non-trivial arrays: match them up */
    flags = match_arrays(vec1, vec2);

    /* Count how many elements have to be copied from vec1
     * and allocate the result array.
     */
    for (i = result_size = 0; i < vec1_size; i++)
    {
        if (flags[i])
            result_size++;
    }

    if (result_size == vec1_size)
    {
        /* No elements to remove */
        xfree(flags);
        free_array(vec2);
        return vec1;
    }

    if (max_array_size && result_size > max_array_size)
    {
        xfree(flags);
        free_array(vec1);
        free_array(vec2);
        errorf("Illegal array size: %lu.\n", (unsigned long)result_size);
    }

    result = allocate_array(result_size);

    /* Copy the elements to keep from vec1 into result.
     * We count down result_size to be able to stop as early
     * as possible.
     */
    for ( dest = result->item, i = 0
        ; i < vec1_size && result_size != 0
        ; i++
        )
    {
        if (flags[i])
        {
            assign_svalue_no_free(dest, vec1->item+i);
            dest++;
            result_size--;
        }
    }

    /* Cleanup and return */
    xfree(flags);
    free_array(vec1);
    free_array(vec2);

    return result;
} /* intersect_array() */

/*-------------------------------------------------------------------------*/
vector_t *
join_array (vector_t *vec1, vector_t *vec2)

/* OPERATOR | (array union)
 *
 * Perform a join of the two vectors <vec1> and <vec2>.
 * The result is a new vector with all elements <vec1> and those elements
 * from <vec2> which are not present in <vec1>.
 *
 * Both <vec1> and <vec2> are freed.
 */

{
    Bool     *flags;       /* The result from match_arrays() */
    size_t    result_size; /* Size of the result array */
    vector_t *result;      /* Result array */
    svalue_t *src;         /* Pointer for getting the result elements */
    svalue_t *dest;        /* Pointer for storing the result elements */
    size_t i;

    size_t vec1_size = VEC_SIZE(vec1);
    size_t vec2_size = VEC_SIZE(vec2);
    size_t sum_size = vec1_size + vec2_size;

    /* Handle empty arrays quickly */

    if (vec1_size == 0)
    {
        free_array(vec1);
        return vec2;
    }

    if (vec2_size == 0)
    {
        free_array(vec2);
        return vec1;
    }

    /* Non-trivial arrays: match them up */
    flags = match_arrays(vec1, vec2);

    /* Count how many elements have to be copied from vec2
     * (we have to get all from vec1 anyway) and allocate the result array.
     */
    result_size = 0;
    for (i = vec1_size; i < sum_size; i++)
    {
        if (!flags[i])
            result_size++;
    }

    if (result_size == 0)
    {
        /* No elements to copy */
        xfree(flags);
        free_array(vec2);
        return vec1;
    }

    if (max_array_size && result_size+vec1_size > max_array_size)
    {
        xfree(flags);
        errorf("Illegal array size: %lu.\n", (unsigned long)(result_size+vec1_size));
    }

    result = allocate_array(vec1_size+result_size);

    /* Copy the elements to keep from vec1 into result.
     */
    for (dest = result->item, i = 0 ; i < vec1_size ; i++)
    {
        assign_svalue_no_free(dest, vec1->item+i);
        dest++;
    }

    /* Copy the elements to keep from vec1 into result.
     * We count down result_size to be able to stop as early
     * as possible.
     */
    for ( src = vec2->item, dest = result->item + vec1_size, i = vec1_size
        ; i < sum_size && result_size != 0
        ; i++, src++
        )
    {
        if (!flags[i])
        {
            assign_svalue_no_free(dest, src);
            dest++;
            result_size--;
        }
    }

    /* Cleanup and return */
    xfree(flags);
    free_array(vec1);
    free_array(vec2);

    return result;
} /* join_array() */

/*-------------------------------------------------------------------------*/
vector_t *
symmetric_diff_array (vector_t *vec1, vector_t *vec2)

/* OPERATOR ^ (symmetric array difference)
 *
 * Compute the symmetric difference of the two vectors <vec1> and <vec2>.
 * The result is a new vector with all elements which are present in only
 * one of the input vectors.
 *
 * Both <vec1> and <vec2> are freed.
 */

{
    Bool     *flags;       /* The result from match_arrays() */
    size_t    result_size; /* Size of the result array */
    vector_t *result;      /* Result array */
    svalue_t *src;         /* Pointer for getting the result elements */
    svalue_t *dest;        /* Pointer for storing the result elements */
    size_t i;

    size_t vec1_size = VEC_SIZE(vec1);
    size_t vec2_size = VEC_SIZE(vec2);
    size_t sum_size = vec1_size + vec2_size;

    /* Handle empty arrays quickly */

    if (vec1_size == 0)
    {
        free_array(vec1);
        return vec2;
    }

    if (vec2_size == 0)
    {
        free_array(vec2);
        return vec1;
    }

    /* Non-trivial arrays: match them up */
    flags = match_arrays(vec1, vec2);

    /* Count how many elements have to be copied
     * and allocate the result array.
     */
    for (i = result_size = 0; i < sum_size; i++)
    {
        if (!flags[i])
            result_size++;
    }

    if (max_array_size && result_size > max_array_size)
    {
        xfree(flags);
        errorf("Illegal array size: %lu.\n", (unsigned long)result_size);
    }

    result = allocate_array(result_size);

    /* Copy the elements to keep from vec1 into result.
     * We count down result_size to be able to stop as early
     * as possible.
     */
    dest = result->item;
    for ( src = vec1->item, i = 0
        ; i < vec1_size && result_size != 0
        ; i++, src++
        )
    {
        if (!flags[i])
        {
            assign_svalue_no_free(dest, src);
            dest++;
            result_size--;
        }
    }

    /* Copy the elements to keep from vec2 into result, starting
     * at the current position <dest>.
     * We count down result_size to be able to stop as early
     * as possible.
     */
    for ( src = vec2->item, i = vec1_size
        ; i < sum_size && result_size != 0
        ; i++, src++
        )
    {
        if (!flags[i])
        {
            assign_svalue_no_free(dest, src);
            dest++;
            result_size--;
        }
    }

    /* Cleanup and return */
    xfree(flags);
    free_array(vec1);
    free_array(vec2);

    return result;
} /* symmetric_diff_array() */

/*-------------------------------------------------------------------------*/
Bool
is_ordered (vector_t *v)

/* Determine if <v> satisfies the conditions for being an ordered vector.
 * Return true if yes, false if not.
 *
 * The conditions are:
 *   - every string is shared
 *   - all elements are sorted according to svalue_cmp().
 *
 * This predicate is currently used just by the swapper, historically
 * to avoid swapping out alist values. This is because the internal order
 * is based on pointer values and thus unreproducible.
 */

{
    svalue_t *svp;
    mp_int i;

    for (svp = v->item, i = (mp_int)VEC_SIZE(v); --i > 0; svp++) {
        if (svp->type == T_STRING && !mstr_tabled(svp->u.str))
            return MY_FALSE;
        if (svalue_cmp(svp, svp+1) > 0)
            return MY_FALSE;
    }
    if (svp->type == T_STRING && !mstr_tabled(svp->u.str))
        return MY_FALSE;

    return MY_TRUE;
} /* is_ordered() */

/*=========================================================================*/

/*                            EFUNS                                        */

/*-------------------------------------------------------------------------*/
svalue_t *
v_allocate (svalue_t *sp, int num_arg)

/* EFUN allocate()
 *
 *     mixed *allocate(int|int* size)
 *     mixed *allocate(int|int* size, mixed init_value)
 *
 * Allocate an array of <size> elements (if <size> is an array, the result
 * will be a multidimensional array), either empty or all
 * elements initialized with <init_value>. If <init_value> is a
 * mapping or array, allocate will create shallow copies of them.
 */

{
    vector_t *v;
    svalue_t *argp;
    size_t new_size;

    argp = sp - num_arg + 1;

    if (argp->type == T_NUMBER)
    {
        new_size = (size_t)argp->u.number;

        if (num_arg == 1 || (sp->type == T_NUMBER && !sp->u.number))
            v = allocate_array(new_size);
        else
        {
            size_t i;
            svalue_t *svp;

            /* If the initialisation value is a mapping, remove all
             * destructed elements so that we can use copy_mapping()
             * later on.
             */
            if (sp->type == T_MAPPING)
                check_map_for_destr(sp->u.map);

            v = allocate_uninit_array(new_size);
            for (svp = v->item, i = 0; i < new_size; i++, svp++)
                copy_svalue_no_free(svp, sp);
        }
    }
    else if (argp->type == T_POINTER
          && (    VEC_SIZE(argp->u.vec) == 0
               || (   VEC_SIZE(argp->u.vec) == 1
                   && argp->u.vec->item->type == T_NUMBER
                   && argp->u.vec->item->u.number == 0)
             )
            )
    {
        /* Special case: result is the empty array.
         * The condition catches ( ({}) ) as well as ( ({0}) )
         * (the generic code below can't handle either of them).
         */
        v = allocate_array(0);
    }
    else if (argp->type == T_POINTER)
    {
        svalue_t *svp;
        size_t dim, num_dim;
        size_t count;
        Bool hasInitValue = MY_FALSE;
        size_t * curpos = alloca(VEC_SIZE(argp->u.vec) * sizeof(*curpos));
        size_t * sizes = alloca(VEC_SIZE(argp->u.vec) * sizeof(*sizes));
        vector_t ** curvec = alloca(VEC_SIZE(argp->u.vec) * sizeof(*curvec));

        num_dim = VEC_SIZE(argp->u.vec);

        if (!curpos || !curvec || !sizes)
        {
            errorf("Out of stack memory.\n");
            /* NOTREACHED */
        }

        if (num_arg == 2 && (sp->type != T_NUMBER || sp->u.number != 0))
        {
            hasInitValue = MY_TRUE;

            /* If the initialisation value is a mapping, remove all
             * destructed elements so that we can use copy_mapping()
             * later on.
             */
            if (sp->type == T_MAPPING)
                check_map_for_destr(sp->u.map);
        }

        /* Check the size array for consistency, and also count how many
         * elements we're going to allocate.
         */
        for ( dim = 0, count = 0, svp = argp->u.vec->item
            ; dim < num_dim
            ; dim++, svp++
            )
        {
            p_int size;

            if (svp->type != T_NUMBER)
            {
                errorf("Bad argument to allocate(): size[%d] is a '%s', "
                      "expected 'int'.\n"
                     , (int)dim, typename(svp->type));
                /* NOTREACHED */
            }

            size = svp->u.number;

            if (size < 0 || (max_array_size && (size_t)size > max_array_size))
                errorf("Illegal array size: %"PRIdPINT"\n", size);

            if (size == 0 && dim < num_dim-1)
                errorf("Only the last dimension can have empty arrays.\n");

            count *= (size_t)size;
            if (max_array_size && count > max_array_size)
                errorf("Illegal total array size: %lu\n", (unsigned long)count);

            sizes[dim] = (size_t)size;
            curvec[dim] = NULL;
        }

        /* Now loop over the dimensions, creating the array structure */
        dim = 0;
        curpos[0] = 0;
        while (dim > 0 || curpos[0] < sizes[0])
        {
            if (!curvec[dim])
            {
                /* We just entered this dimension.
                 * Create a new array and initialise the loop.
                 */
                if (hasInitValue || dim+1 < num_dim)
                {
                    curvec[dim] = allocate_uninit_array(sizes[dim]);
                }
                else
                {
                    curvec[dim] = allocate_array(sizes[dim]);
                    /* This is the last dimension, and there is nothing
                     * to initialize: return immediately to the higher level
                     */
                    curpos[dim] = sizes[dim]; /* In case dim == 0 */
                    if (dim > 0)
                        dim--;
                    continue;
                }
                curpos[dim] = 0;
            }

            /* curvec[dim] is valid, and we have to put the next
             * element in at index curpos[dim].
             */
            if (dim == num_dim-1)
            {
                /* Last dimension: assign the init value */
                if (hasInitValue && curpos[dim] < sizes[dim])
                    copy_svalue_no_free(curvec[dim]->item+curpos[dim], sp);
            }
            else if (!curvec[dim+1])
            {
                /* We need a vector from a lower dimension, but it doesn't
                 * exist yet: setup the loop parameters to go into
                 * that lower level.
                 */
                dim++;
                continue;
            }
            else if (curpos[dim] < sizes[dim])
            {
                /* We got a vector from a lower lever */
                put_array(curvec[dim]->item+curpos[dim], curvec[dim+1]);
                curvec[dim+1] = NULL;
            }

            /* Continue to the next element. If we are at the end
             * of this dimension, return to the next higher one.
             */
            curpos[dim]++;
            if (curpos[dim] >= sizes[dim] && dim > 0)
            {
                dim--;
            }
        } /* while() */

        /* The final vector is now in curvec[0] */
        v = curvec[0];
    }
    else
    {
        /* The type checker should prevent this case */
        fatal("Illegal arg 1 to allocate(): got '%s', expected 'int|int*'.\n"
             , typename(argp->type));
    } /* if (argp->type) */

    if (num_arg == 2)
        free_svalue(sp--);

    free_svalue(sp);
    put_array(sp, v);

    return sp;
} /* v_allocate() */

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
    char     *flags;  /* Flag array, one flag for each element of <p>
                       * (in reverse order) */
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
        errorf("Stack overflow in filter()");
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
            errorf("Too many arguments to filter(array)\n");
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
                errorf("object used by filter(array) destructed");
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
 *   mixed * map(mixed * arg, string func, string|object ob, mixed extra...)
 *   mixed * map(mixed * arg, closure cl, mixed extra...)
 *   mixed * map(mixed * arr, mapping map [, int col])
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
 *    <map>[elem[,idx]]
 *
 * In the mapping case, if <map>[elem[,idx]] does not exist, the original
 * value is returned in the result.
 * [Note: argument type and range checking for idx is done in v_map()]
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

    inter_sp = sp;

    arg = sp - num_arg + 1;

    arr = arg->u.vec;
    cnt = (mp_int)VEC_SIZE(arr);

    if (arg[1].type == T_MAPPING)
    {
        /* --- Map through mapping --- */

        mapping_t *m;
        p_int column = 0; /* mapping column to use */

        m = arg[1].u.map;

        if (num_arg > 2)
            column = arg[2].u.number;

        res = allocate_array(cnt);
        if (!res)
            errorf("(map_array) Out of memory: array[%"PRIdMPINT
                "] for result\n", cnt);
        push_array(inter_sp, res); /* In case of errors */

        for (w = arr->item, x = res->item; --cnt >= 0; w++, x++)
        {
            if (destructed_object_ref(w))
                assign_svalue(w, &const0);

            v = get_map_value(m, w);
            if (v == &const0)
                assign_svalue_no_free(x, w);
            else
                assign_svalue_no_free(x, v + column);
        }

        if (num_arg > 2)
            free_svalue(arg+2);
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
            errorf("(map_array) Out of memory: array[%"PRIdMPINT
                "] for result\n", cnt);
        push_array(inter_sp, res); /* In case of errors */

        /* Loop through arr and res, mapping the values from arr */
        for (w = arr->item, x = res->item; --cnt >= 0; w++, x++)
        {
            if (current_object->flags & O_DESTRUCTED)
                continue;

            if (destructed_object_ref(w))
                assign_svalue(w, &const0);

            if (!callback_object(&cb))
                errorf("object used by map(array) destructed");

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
    arg->u.vec = res; /* Keep svalue type: T_POINTER */

    return arg;
} /* x_map_array () */

/*-------------------------------------------------------------------------*/
svalue_t *
v_sort_array (svalue_t * sp, int num_arg)

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
    Bool        inplace = MY_FALSE;
    
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

    /* If the argument is passed in by reference, make sure that it is
     * an array, place the argument vector directly into the stack and set
     * inplace.
     */
    if (arg->type == T_LVALUE)
    {
        svalue_t * svp = arg;
        vector_t * vec = NULL;
        
        do {
            svp = svp->u.lvalue;
        } while (svp->type == T_LVALUE || svp->type == T_PROTECTED_LVALUE);
        
        if (svp->type != T_POINTER)
        {
            inter_sp = sp;
            errorf("Bad arg 1 to sort_array(): got '%s &', "
                   "expected 'mixed * / mixed *&'.\n"
                   , typename(svp->type));
            // NOTREACHED
            return sp;
        }

        inplace = MY_TRUE;
        
        vec = ref_array(svp->u.vec);
        free_svalue(arg);
        put_array(arg, vec);
    }
        
    
    /* Get the array. Since the sort sorts in-place, we have
     * to make a shallow copy of arrays with more than one
     * ref. Exception is, if the array is given as reference/lvalue, then we
     * always sort in-place.
     */
    data = arg->u.vec;
    check_for_destr(data);

    if (!inplace && data->ref != 1)
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
        errorf("Stack overflow in sort_array()");
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
                    errorf("object used by sort_array destructed");

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
} /* v_sort_array() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_filter_objects (svalue_t *sp, int num_arg)

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
            errorf("Stack overflow in filter_objects()");
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

    put_array(sp, w);
    return sp;
} /* v_filter_objects() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_map_objects (svalue_t *sp, int num_arg)

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
} /* v_map_objects() */

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
              errorf("Bad arg 1 to transpose_array(): not an array of arrays.\n");
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
        if (VEC_SIZE(y->u.vec) < b
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
} /* sameval() */


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
                errorf("(unique_array) Out of memory (%lu bytes pooled) "
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
        errorf("(unique_array) Out of memory (%lu bytes pooled) "
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
} /* put_in() */


/*-------------------------------------------------------------------------*/
/* To facilitate automatic cleanup of the temporary structures in case
 * of an error, the following structure will be pushed onto the VM stack
 * as T_ERROR_HANDLER.
 */

struct unique_cleanup_s {
    error_handler_t head;  /* The link to the error handler function */
    Mempool         pool;  /* Pool for the unique structures */
    vector_t      * arr;   /* Protective reference to the array */
};

static void
make_unique_cleanup (error_handler_t * arg)
{
    struct unique_cleanup_s * data = (struct unique_cleanup_s *)arg;

    if (data->pool)
        mempool_delete(data->pool);
    if (data->arr)
        deref_array(data->arr);
    xfree(arg);
} /* make_unique_cleanup() */

/*-------------------------------------------------------------------------*/
static vector_t *
make_unique (vector_t *arr, callback_t *cb, svalue_t *skipnum)

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
    struct unique_cleanup_s * ucp;

    head = NULL;

    arr_size = (mp_int)VEC_SIZE(arr);

    /* Special case: unifying an empty array */
    if (!arr_size)
        return allocate_array(0);

    /* Get the memory for the arr_size unique-structures we're going
     * to need.
     */
    pool = new_mempool(size_mempool(sizeof(*head)));
    if (!pool)
        errorf("(unique_array) Out of memory: (%lu bytes) for mempool\n"
             , (unsigned long)arr_size * sizeof(*head));

    /* Create the automatic cleanup structure */
    ucp = xalloc(sizeof(*ucp));
    if (!ucp)
    {
        mempool_delete(pool);
        errorf("(unique_array) Out of memory: (%lu bytes) for cleanup structure\n"
             , (unsigned long)sizeof(*ucp));
    }

    ucp->pool = pool;
    ucp->arr = ref_array(arr);  /* Prevent apply from freeing this */

    push_error_handler(make_unique_cleanup, &(ucp->head));

    /* Build the comb structure.
     */
    ant = 0;
    for (cnt = 0; cnt < arr_size; cnt++)
    {
        if (current_object->flags & O_DESTRUCTED)
            break;
            /* Don't call the filters anymore */

        if (arr->item[cnt].type == T_OBJECT
         && !destructed_object_ref(&(arr->item[cnt]))
           )
        {
            /* It's usually done the other way around, but not here: if
             * it's a closure, we pass the object analyzed; otherwise we
             * change the object the callback is bound to to call the
             * discriminator function in it.
             */
            if (!cb->is_lambda)
                callback_change_object(cb, arr->item[cnt].u.ob);
            else
                push_ref_object(inter_sp, arr->item[cnt].u.ob, "unique_array");

            v = apply_callback(cb, cb->is_lambda ? 1 : 0);
            if (v && !sameval(v, skipnum))
                ant = put_in(pool, &head, v, &(arr->item[cnt]));
        }
    }

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

    /* Cleanup using the cleanup structure */
    free_svalue(inter_sp--);

    return ret;
} /* make_unique() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_unique_array (svalue_t *sp, int num_arg)

/* EFUN unique_array()
 *
 *   mixed unique_array(object *obarr, string|closure fun)
 *   mixed unique_array(object *obarr, string|closure fun, mixed skip)
 *   mixed unique_array(object *obarr, string|closure fun, mixed extra..., mixed skip)
 *
 * Groups objects together for which the separator function
 * returns the same value. obarr should be an array of objects,
 * other types are ignored.
 *
 * If the separator function is defined by name, it is searched and called
 * in the objects from <obarr>. If <extra> arguments are given, they are
 * passed to the function as arguments.
 *
 * If the separator function is defined as a closure, it will be passed
 * the objects from <obarr> as first argument, with the <extra> arguments
 * (if any) passed following.
 *
 * If the <skip> argument is given (it is required when <extra> arguments
 * are to be used), and the return value from the separator function call
 * matches this value, the object in question will _not_ be included in the
 * returned array. Default value for <skip> is the number 0.
 */

{
    vector_t *res;
    svalue_t *argp = sp - num_arg + 1;
    callback_t  cb; /* must persist until the end of the function */

    check_for_destr(argp->u.vec);

    /* Sort out the arguments */
    if (num_arg == 2)
    {
        /* Just the callback function name on the stack: add the default
         * 'skip' value
         */
        sp++;
        put_number(sp, 0);
    }

    {
        /* Extract the callback information from the stack */
        int         error_index;

        assign_eval_cost();
        inter_sp = sp;

        error_index = setup_efun_callback_noobj(&cb, argp+1, num_arg-2);

        if (error_index >= 0)
        {
            /* The callback values have already been removed, now
             * make sure that the 'skip' value isn't left out either
             */
            transfer_svalue_no_free(argp+1, sp);
            inter_sp = sp = argp+1;
            vefun_bad_arg(error_index+2, argp+1);
            /* NOTREACHED */
            return argp;
        }

        /* Callback creation successful, now setup the stack */
        put_callback(argp+1, &cb);
        transfer_svalue_no_free(argp+2, sp);

        inter_sp = sp = argp+2;
    }

    /* At this point:       argp[0]: the vector
     *                      argp[1]: the callback structure
     *                sp -> argp[2]: the skip value
     */
    res = make_unique(argp->u.vec, argp[1].u.cb, argp+2);

    /* Clean up the stack and push the result */
    free_svalue(sp--);
    free_svalue(sp--);
    free_svalue(sp);

    if (res)
        put_array(sp, res);
    else
        put_number(sp, 0);

    return sp;
} /* v_unique_array() */

/***************************************************************************/

