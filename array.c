/*---------------------------------------------------------------------------
 * Array handling functions.
 *
 *---------------------------------------------------------------------------
 * The structure of an array ("vector") is defined in datatypes.h as this:
 *
 *   struct vector {
 *       p_int size;               (ifndef MALLOC_smalloc)
 *       p_int ref;
 *       p_int extra_ref;          (ifdef DEBUG)
 *       struct wiz_list *user;
 *       struct svalue item[1...];
 *   };
 *
 * .size is the number of elements in the vector. If smalloc is used,
 * this number can be deduced from the memory block size, the entry
 * itself is therefore omitted.
 *
 * .ref is the number of references to the vector. If this number
 * reaches 0, the vector can (and should) be deallocated. This scheme
 * breaks down with circular references, but those are caught by
 * the garbage collector.
 *
 * .extra_ref exists when the driver is compiled for DEBUGging, and
 * is used to countercheck the the .ref count.
 * TODO: How _is_ .extra_ref checked?
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
 *   INIT_VEC_TYPE: Type to construct local vector instances. Must
 *       be used in a structure, e.g.
 *          struct { INIT_VEC_TYPE; } tmpvec
 *       for a single value vector, or
 *          struct { INIT_VEC_TYPE; struct svalue item[2] } tmpvec;
 *       for a three value vector.
 *       The vector can then be accessed as tmpvec.v .
 *
 *       Reason is that the smalloc overhead must be simulated for
 *       VEC_SIZE() to work even though this particular array is
 *       not allocated by smalloc.
 *
 *   VEC_INIT(size, ref, type): Initialize the head of a local vector
 *       as defined by INIT_VEC_TYPE. The vector shall contain <size>
 *       elements, have an initial refcount of <ref>, and the svalue.type
 *       of the first element is to be <type>.
 *       Proper initialisations of the above examples would be:
 *            { VEC_INIT(1,1,T_INVALID) }
 *       and  { VEC_INIT(3,1,T_NUMBER), { { T_OBJECT, { 0 } }
 *                                    ,   { T_OBJECT, { 0 } } }
 *            }
 *
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
 * TODO: The useful lookup/forcetoshare-bits of the alists should go into dedicated
 * TODO:: (e)functions so that the alists themselves can be made optional.
 *---------------------------------------------------------------------------
 */

#include "driver.h"

#include "my-alloca.h"
#include <stddef.h>

#include "array.h"
#include "backend.h"
#include "datatypes.h"
#include "exec.h"       /* for the efuns */
#include "instrs.h"     /* F_INHERIT_LIST */
#include "interpret.h"  /* for the efuns */
#include "main.h"
#include "mapping.h"
#include "object.h"
#include "prolang.h"
#include "regexp.h"
#include "rxcache.h"
#include "simulate.h"
#include "stralloc.h"
#include "swap.h"
#include "wiz_list.h"

/*-------------------------------------------------------------------------*/

int num_arrays;
  /* Total number of allocated arrays */

struct null_vector_aggregate_struct null_vector_aggregate
  = { VEC_INIT(0 /* size */, 1 /* ref */, T_INVALID) };
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

struct vector *subtract_array_tmp_vec;  /* TODO: Remove me */
  /* Ordered version of the last subtrahend passed to subtract_array().
   * At the moment, this value is not used and could as well be
   * freed immediately in subtract_array().
   */

char *last_insert_alist_shared_string = 0; /* TODO: Remove me */
  /* The last key string inserted into an alist.
   * gcollect needs to know this.
   * At the moment this value is not used and could as well be
   * avoided immediately in insert_alist().
   */

struct svalue assoc_shared_string_key; /* TODO: Remove me */
  /* The svalue assoc() uses to pass the shared search key to
   * search_alist(). It is initialised by main() on startup,
   * probably in order to save a few cycles (assoc() was once
   * heavily used). This should be done on every call (static
   * initialisation is not possible as it would confuse the
   * garbage collector).
   */

/*-------------------------------------------------------------------------*/
#ifndef allocate_array

struct vector *
allocate_array (mp_int n)

#else

struct vector *
_allocate_array(mp_int n, char * file, int line)

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
 * MALLOC_smalloc: annotate the allocations with <file> and <line>
 */

{
    mp_int i;
    struct vector *p;
    struct svalue *svp;

    if (n < 0 || n > MAX_ARRAY_SIZE)
        error("Illegal array size: %ld.\n", n);

    if (n == 0) {
        p = &null_vector;
        p->ref++;
        return p;
    }

    num_arrays++;

    p = ALLOC_VECTOR(n, file, line);
    if (!p) {
        (*allocate_array_error_handler)("Out of memory\n");
        return 0;
    }

    p->ref = 1;
#ifndef MALLOC_smalloc
    p->size = n;
#endif
    (p->user = current_object->user)->size_array += n;

    svp = p->item;
    for (i = n; --i >= 0; )
        *svp++ = const0;

    return p;
}

/*-------------------------------------------------------------------------*/
#ifndef allocate_uninit_array

struct vector *
allocate_uninit_array (mp_int n)

#else

struct vector *
_allocate_uninit_array (mp_int n, char *file, int line)

#endif

/* Allocate an array for <n> elements and return the pointer.
 * The elements are not initialised.
 * If the allocations fails (and error() does return), a 0 pointer
 * may be returned.
 *
 * Allocating an array of size 0 will return a reference to the
 * globally shared empty array.
 *
 * MALLOC_smalloc: annotate the allocations with <file> and <line>
 */

{
    struct vector *p;

    if (n < 0 || n > MAX_ARRAY_SIZE)
        error("Illegal array size: %ld.\n", n);

    if (n == 0) {
        p = &null_vector;
        p->ref++;
        return p;
    }

    num_arrays++;

    p = ALLOC_VECTOR(n, file, line);
    if (!p) {
        (*allocate_array_error_handler)("Out of memory\n");
        return 0;
    }

    p->ref = 1;
#ifndef MALLOC_smalloc
    p->size = n;
#endif
    (p->user = current_object->user)->size_array += n;

    return p;
}

/*-------------------------------------------------------------------------*/
void
free_vector (struct vector *p)

/* Decrement the ref-count of vector <p> and deallocate the vector
 * if possible (the contained elements are properly freed in this
 * case).
 */

{
    mp_int i;
    struct svalue *svp;

    p->ref--;
    if (p->ref > 0)
        return;

#ifdef DEBUG
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

    xfree((char *)p);
}

/*-------------------------------------------------------------------------*/
void
free_empty_vector (struct vector *p)

/* Deallocate the vector <p> without regard of refcount or contained
 * elements. Just the statistics are cared for.
 */

{
    mp_int i;

    i = VEC_SIZE(p);
    p->user->size_array -= i;
    num_arrays--;
    xfree((char *)p);
}

/*-------------------------------------------------------------------------*/
static struct vector *
shrink_array (struct vector *p, int n)

/* Create and return a new array containing just the first <n> elements
 * of <p>. <p> itself is freed (and thus possibly deallocated).
 */

{
    struct vector *res;

    res = slice_array(p, 0, n-1);
    free_vector(p);
    return res;
}

/*-------------------------------------------------------------------------*/
void
set_vector_user (struct vector *p, struct object *owner)

/* Wizlist statistics: take vector <p> from its former owner and account it
 * under its new <owner>.
 */

{
    struct svalue *svp;
    mp_int i;

    i = VEC_SIZE(p);
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
long
total_array_size (void)

/* Statistics for the command 'status [tables]'.
 * Return the total memory used for all vectors in the game.
 */

{
    struct wiz_list *wl;
    long total;

    total = default_wizlist_entry.size_array;
    for (wl = all_wiz; wl; wl = wl->next)
        total += wl->size_array;
    total *= sizeof(struct svalue);
    total += num_arrays * (sizeof(struct vector) - sizeof(struct svalue));
    return total;
}

/*-------------------------------------------------------------------------*/
struct vector *
explode_string (char *str, char *del)

/* Explode the string <str> by delimiter string <del> and return an array
 * of the (unshared) strings found between the delimiters.
 *
 * This is the old behaviour: leading and trailing occurences of <del>
 * in <str> are ignored.
 *
 *   explode("xyz", "")         -> { "xyz" }
 *   explode("###", "##")       -> { "", "#" }
 *   explode(" the  fox ", " ") -> { "the", "", "fox" }
 */

{
    char *p, *beg;
    size_t num, len;
    struct vector *ret;
    char *buff;

    len = strlen(del);

    /* Take care of the case where the delimiter is an
     * empty string. Then, return an array with only one element,
     * which is the original string.
     */
    if (len == 0) {
        ret = allocate_array(1);
        ret->item[0].type = T_STRING;
        ret->item[0].x.string_type = STRING_MALLOC;
        ret->item[0].u.string = string_copy(str);
        return ret;
    }

    /* Skip leading 'del' strings, if any.
     */
    while(strncmp(str, del, len) == 0) {
        str += len;
        if (str[0] == '\0')
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
    for (p=str, num=1; *p;) {
        if (strncmp(p, del, len) == 0) {
            p += len;
            if (*p)
                num++;
        } else
            p += 1;
    }

    ret = allocate_array(num);

    /* Extract the <num> strings into the result array <ret>.
     *   <buff> serves as temporary buffer for the copying.
     */
    buff = xalloc(strlen(str) + 1);
    if (!buff)
    {
        free_vector(ret);
        error("Out of memory.\n");
        /* NOTREACHED */
        return NULL;
    }

    for (p=str, beg = str, num=0; *p; ) {
        if (strncmp(p, del, len) == 0) {
            strncpy(buff, beg, p - beg);
            buff[p-beg] = '\0';
            ret->item[num].type = T_STRING;
            ret->item[num].x.string_type = STRING_MALLOC;
            ret->item[num].u.string = string_copy(buff);
            /* TODO: implement a string_copy_n(beg, n) */
            num++;
            beg = p + len;
            p = beg;
        } else {
            p += 1;
        }
    }

    /* Copy last occurence, if there was not a 'del' at the end.
     */
    if (*beg != '\0') {
#if defined(DEBUG) || 1
        if (num >= VEC_SIZE(ret))
            fatal("Index out of bounds in old explode(): estimated %d, got %ld.\n", num, VEC_SIZE(ret));
#endif
        ret->item[num].type = T_STRING;
        ret->item[num].x.string_type = STRING_MALLOC;
        ret->item[num].u.string = string_copy(beg);
    }

    xfree(buff);

    return ret;
}

/*-------------------------------------------------------------------------*/
struct vector *
new_explode_string (char *str, char *del)

/* Explode the string <str> by delimiter string <del> and return an array
 * of the (unshared) strings found between the delimiters.
 * They are unshared because they are most likely short-lived.
 *
 * TODO: At some later point in the execution thread, all the longlived
 *   unshared strings should maybe be converted into shared strings.
 *
 * This is the new, logical behaviour: nothing is occured.
 * The relation implode(explode(x,y),y) == x holds.
 *
 *   explode("xyz", "")         -> { "x", "y", "z" }
 *   explode("###", "##")       -> { "", "#" }
 *   explode(" the  fox ", " ") -> { "", "the", "", "", "fox", ""}
 */

{
    char *p, *beg;
    size_t num;
    int len;
    struct vector *ret;
    char *buff;

    len = strlen(del);

    /* --- Special case: Delimiter is an empty or one-char string --- */
    if (len <= 1) {

        /* Delimiter is empty: return an array which holds all characters as
         *   single-character strings.
         */
        if (len < 1) {
            struct svalue *svp;

            len = strlen(str);
            ret = allocate_array(len);
            for( svp = ret->item; --len >= 0; svp++, str++ ) {
                buff = xalloc(2);
                if (!buff) {
                    free_vector(ret);
                    error("Out of memory\n");
                }
                buff[0] = *str;
                buff[1] = '\0';
                svp->type = T_STRING;
                svp->x.string_type = STRING_MALLOC;
                svp->u.string = buff;
            }
            return ret;

        }

        /* Delimiter is one-char string: speedy implementation which uses
         *   direct character comparisons instead of calls to strncmp().
         */
        else {
            char c;
            struct svalue *svp;

            c = *del;
            /* TODO: Remember positions here */
            /* Determine the number of delimiters in the string. */
            for (num = 1, p = str; NULL != (p = strchr(p, c)); p++, num++) NOOP;

            ret = allocate_array(num);
            for (svp = ret->item; NULL != (p = strchr(str, c)); str = p + 1, svp++) {
                len = p - str;
                buff = xalloc(len + 1);
                if (!buff) {
                    free_vector(ret);
                    error("Out of memory\n");
                }
                memcpy(buff, str, len);
                buff[len] = '\0';
                svp->type = T_STRING;
                svp->x.string_type = STRING_MALLOC;
                svp->u.string = buff;
            }

            /* str now points to the (possibly empty) remains after
             * the last delimiter.
             */
            svp->type = T_STRING;
            svp->x.string_type = STRING_MALLOC;
            if ( !(svp->u.string = string_copy(str)) ) {
                free_vector(ret);
                error("Out of memory\n");
            }

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
    for (p=str, num=1; *p;) {
        if (strncmp(p, del, len) == 0) {
            p += len;
            num++;
        } else
            p += 1;
    }

    ret = allocate_array(num);

    /* Extract the <num> strings into the result array <ret>.
     *   <buff> serves as temporary buffer for the copying.
     */
    for (p=str, beg = str, num=0; *p; ) {
        if (strncmp(p, del, len) == 0) {
            int bufflen;

            bufflen = p - beg;
            buff = xalloc(bufflen + 1);
            if (!buff) {
                free_vector(ret);
                error("Out of memory\n");
            }
            memcpy(buff, beg, bufflen);
            buff[bufflen] = '\0';

            ret->item[num].type = T_STRING;
            ret->item[num].x.string_type = STRING_MALLOC;
            ret->item[num].u.string = buff;

            num++;
            beg = p + len;
            p = beg;

        } else {
            p += 1;
        }
    }

    /* Copy the last occurence (may be empty). */
    if ( !(ret->item[num].u.string = string_copy(beg)) ) {
        free_vector(ret);
        error("Out of memory\n");
    }
    ret->item[num].type = T_STRING;
    ret->item[num].x.string_type = STRING_MALLOC;

    return ret;
}

/*-------------------------------------------------------------------------*/
#ifndef implode_string

char *
implode_string (struct vector *arr, char *del)

#else

char *
_implode_string (struct vector *arr, char *del, char *file, int line)

#endif

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
 * MALLOC_smalloc: annotate the allocations with <file> and <line>
 */

{
    mp_int size, i, del_len, arr_size;
    char *p, *q;
    struct svalue *svp;

    del_len = strlen(del);

    /* Compute the <size> of the final string
     */
    size = -del_len;
    for (i = (arr_size = VEC_SIZE(arr)), svp = arr->item; --i >= 0; svp++)
    {
        if (svp->type == T_STRING) {
            size += del_len + strlen(svp->u.string);
        } else if (svp->type == T_OBJECT && svp->u.ob->flags & O_DESTRUCTED) {
            zero_object_svalue(svp);
        }
    }

    /* Allocate the string; cop out if there's nothing to implode.
     */
#ifndef implode_string
    if (size <= 0)
        return string_copy("");
    p = xalloc(size + 1);
#else
    if (size <= 0)
        return _string_copy("", file, line);
    p = smalloc(size + 1, file, line);
#endif
    if (!p) {
        /* caller raises the error() */
        return NULL;
    }
    q = p; /* Remember the start of the allocated string */

    /* Concatenate the result string.
     *
     * <i>   is the number of elements left to check,
     * <svp> is the next element to check,
     * <p>   points to the current end of the result string.
     */

    svp = arr->item;

    /* Look for the first element to add (there is at least one!) */
    for (i = arr_size; svp->type != T_STRING; ) {
        --i;
        svp++;
    }

    strcpy(p, svp->u.string);
    p += strlen(svp->u.string);

    /* Copy the others if any */
    while (--i > 0) {
        svp++;
        if (svp->type == T_STRING) {
            strcpy(p, del);
            p += del_len;
            strcpy(p, svp->u.string);
            p += strlen(svp->u.string);
        }
    }

    return q;
}

/*-------------------------------------------------------------------------*/
struct vector *
slice_array (struct vector *p, int from, int to)

/* Create a vector slice from vector <p>, range <from> to <to> inclusive,
 * and return it.
 *
 * <to> is guaranteed to not exceed the size of <p>.
 * If <from> is greater than <to>, the empty array is returned.
 */

{
    struct vector *d;
    int cnt;

    if (from < 0)
            from = 0;

    if (to < from)
        return allocate_array(0);

    d = allocate_array(to-from+1);
    for (cnt = from; cnt <= to; cnt++)
        assign_svalue_no_free (&d->item[cnt-from], &p->item[cnt]);

    return d;
}

/*-------------------------------------------------------------------------*/
struct vector *
add_array (struct vector *p, struct vector *q)

/* Concatenate the vectors <p> and <q> and return the resulting vector.
 * <p> and <q> are not modified.
 */

{
    mp_int cnt;
    struct svalue *s, *d;
    mp_int q_size;

    s = p->item;
    p = allocate_array((cnt = VEC_SIZE(p)) + (q_size = VEC_SIZE(q)));
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
struct vector *
subtract_array (struct vector *minuend, struct vector *subtrahend)

/* Subtract all elements in <subtrahend> from the vector <minuend>
 * and return the resulting difference vector.
 * <subtrahend> and <minuend> are not modified.
 *
 * The function uses order_alist()/assoc() on <subtrahend> for
 * faster operation.
 *
 * The ordered version of <subtrahend> is assigned to the global
 * variable subtract_array_tmp_vec.
 */

{
static struct svalue ltmp = { T_POINTER };
  /* Temporary svalue to pass vectors to order_alist().
   * The static initialisation saves a few cycles.
   */

    struct vector *difference;    /* Resulting difference vector,
                                     with extra zeroes at the end */
    struct vector *vtmpp;         /* {( Ordered <subtrahend> }) */
    struct svalue *source, *dest; /* Pointers into minuend
                                     and difference vector */
    mp_int i, minuend_size;

    /* Order the subtrahend */
    ltmp.u.vec = subtrahend;
    vtmpp = order_alist(&ltmp, 1, 1);
    free_vector(subtrahend);
    subtrahend = vtmpp->item[0].u.vec;

    /* The difference can be equal to minuend in the worst case */
    difference = allocate_array(minuend_size = VEC_SIZE(minuend));

    /* Scan minuend and look up every element in the ordered subtrahend.
     * If it's not there, add the element to the difference vector.
     */
    for (source = minuend->item, dest = difference->item, i = minuend_size
        ; i--
        ; source++) {
        if (source->type == T_OBJECT && source->u.ob->flags & O_DESTRUCTED)
            assign_svalue(source, &const0);
        if ( assoc(source, subtrahend) < 0 )
            assign_svalue_no_free(dest++, source);
    }
    subtract_array_tmp_vec = vtmpp;

    /* Shrink the difference vector to the needed size and return it. */
    return shrink_array(difference, dest-difference->item);
}

/*-------------------------------------------------------------------------*/
/* Returns an array of all objects contained in 'ob'
 */
struct vector *
all_inventory (struct object *ob)

/* Return a vector with all objects contained in <ob>.
 * TODO: Make this a proper f_all_inventory(sp, num_arg) efun?
 */

{
    struct vector *d;    /* The result vector */
    struct object *cur;  /* Current inventory object */
    int cnt, res;

    /* Count how many inventory objects there are. */
    cnt=0;
    for (cur=ob->contains; cur; cur = cur->next_inv)
        cnt++;

    if (!cnt)
        return allocate_array(0);

    d = allocate_array(cnt);

    /* Copy the object references */
    cur=ob->contains;
    for (res=0; res < cnt; res++) {
        d->item[res].type=T_OBJECT;
        d->item[res].u.ob = cur;
        add_ref(cur,"all_inventory");
        cur=cur->next_inv;
    }

    return d;
}


/*-------------------------------------------------------------------------*/
void
map_array ( struct vector *arr
          , char *func
          , struct object *ob
          , int num_extra
          , struct svalue *extra)

/* Map all elements from <arr> through the
 * function <ob>-><func>(elem, <extra>...) and create a vector of
 * the result values in the order of calling.
 *
 * If <ob> is 0, <func> is in fact a struct svalue* pointing to
 * a closure.
 * TODO: UGLY UGLY UGLY! Make this a proper f_map_array(sp, numarg) efun.
 *
 * The resulting vector is pushed onto the VM stack.
 */

{
    struct vector *r;
    struct svalue *v, *w, *x;
    mp_int cnt;

    r = allocate_array(cnt = VEC_SIZE(arr));
    if (!r)
        error("Out of memory\n");
    push_referenced_vector(r);

    /* Loop through arr and d, mapping the values from arr */
    for (w = arr->item, x = r->item; --cnt >= 0; w++, x++) {
        if (current_object->flags & O_DESTRUCTED)
            continue;

        push_svalue(w);
        push_svalue_block(num_extra, extra);
        if (ob) {
            if (ob->flags & O_DESTRUCTED)
                error("object used by map_array destructed");
            v = sapply (func, ob, 1 + num_extra);
            if (v) {
                transfer_svalue_no_free (x, v);
                v->type = T_INVALID;
            }
        } else {
            call_lambda((struct svalue *)func, 1 + num_extra);
            transfer_svalue_no_free (x, inter_sp--);
        }
    }
}

/*-------------------------------------------------------------------------*/
static INLINE int
sort_array_cmp (char *func, struct object *ob, struct svalue *p1, struct svalue *p2)

/* Auxiliary function to sort_array() to compare two svalues by lfun call.
 *
 * The function <ob>-><func>(<p1>, <p2>) is called and has to return 'true'
 * if <p1> is "less than" <p2>. Any positive number and non-numeric value
 * is regarded as 'true'; 0 and negative numbers are 'false'.
 *
 * sort_array_cmp() itself returns true or false as the function call
 * dictates.
 */

{
    struct svalue *d;

    if (ob->flags & O_DESTRUCTED)
        error("object used by sort_array destructed");
    push_svalue(p1);
    push_svalue(p2);
    d = sapply(func, ob, 2);
    if (!d) return 0;
    if (d->type != T_NUMBER) {
        /* value will be freed at next call of apply() */
        return 1;
    }
    return d->u.number > 0;
}

/*-------------------------------------------------------------------------*/
static INLINE int
sort_array_lambda_cmp (struct svalue *func, struct svalue *p1, struct svalue *p2)

/* Auxiliary function to sort_array() to compare two svalues by closure call.
 *
 * The closure <func>(<p1>, <p2>) is called and has to return 'true'
 * if <p1> is "less than" <p2>. Any positive number and non-numeric value
 * is regarded as 'true'; 0 and negative numbers are 'false'.
 *
 * sort_array_lambda_cmp() itself returns true or false as the function call
 * dictates.
 */

{
    struct svalue *d;

    push_svalue(p1);
    push_svalue(p2);
    call_lambda(func, 2);
    d = inter_sp--;
    if (d->type != T_NUMBER) {
        free_svalue(d);
        return 1;
    }
    return d->u.number > 0;
}

/*-------------------------------------------------------------------------*/
struct vector *
sort_array (struct vector *data, char *func, struct object *ob)

/* Sort the vector <data> according to the given comparison function.
 * The function <ob>-><func>(elem1, elem2) is repeatedly called for two
 * selected data elements of <data> and has to return 'true' if <p1>
 * is "less than" <p2>. Any positive number and non-numeric value
 * is regarded as 'true'; 0 and negative numbers are 'false'.
 *
 * If <ob> is 0, <func> is in fact a struct svalue* pointing to
 * a closure.
 * TODO: UGLY UGLY UGLY! Make this a proper f_sort_array(sp, numarg) efun.
 *
 * The sorting is implemented using Mergesort, which gives us a O(N*logN)
 * worst case behaviour and provides a stable sort.
 */

{
  int step, halfstep, size;
  int i, j, index1, index2, end1, end2;
  struct svalue *source, *dest, *temp;

  size = VEC_SIZE(data);
  temp = data->item;

  /* Squash all destructed objects in data. */
  for (i=0; i < size; i++)
  {
    if (temp[i].type == T_OBJECT && temp[i].u.ob->flags & O_DESTRUCTED)
      assign_svalue(&temp[i],&const0);
  }

  /* Easiest case: nothing to sort */
  if (size <= 1)
    return data;

  /* In order to provide clean error recovery, data must always hold
   * exactly one copy of each original content svalue when an error is
   * possible. Thus, it would be not a good idea to use it as scrap
   * space.
   */

  source = (struct svalue *)alloca(size*sizeof(struct svalue));
  dest = (struct svalue *)alloca(size*sizeof(struct svalue));
  if (!source || !dest)
  {
      fatal("Stack overflow in sort_array()");
      /* NOTREACHED */
      return data;
  }

  push_referenced_vector(data);
  for (i=0;i<size;i++)
    source[i]=temp[i];
  step = 2;
  halfstep = 1;
  while (halfstep<size)
  {
    for (i=j=0; i < size; i += step)
    {
      index1 = i;
      index2 = i + halfstep;
      end1 = index2;
      if (end1 > size)
        end1 = size;
      end2 = i + step;
      if (end2 > size)
        end2 = size;
      if (ob) {
        while (index1<end1 && index2<end2)
        {
          if (sort_array_cmp(func,ob,source+index1,source+index2))
            dest[j++]=source[index2++];
          else
            dest[j++]=source[index1++];
        }
      } else {
        while (index1<end1 && index2<end2)
        {
          if (sort_array_lambda_cmp((struct svalue *)func,source+index1,source+index2))
            dest[j++]=source[index2++];
          else
            dest[j++]=source[index1++];
        }
      }
      if (index1==end1)
      {
        while (index2<end2)
          dest[j++]=source[index2++];
      }
      else
      {
        while (index1<end1)
          dest[j++]=source[index1++];
      }
    }
    halfstep = step;
    step += step;
    temp = source;
    source = dest;
    dest = temp;
  }

  temp = data->item;
  for (i=size; --i >= 0; )
    temp[i]=source[i];
  drop_stack();
  return data;
}

/*-------------------------------------------------------------------------*/
static int
deep_inventory_size (struct object *ob)

/* Helper function for deep_inventory()
 *
 * Count the size of <ob>'s inventory by counting the contained objects,
 * invoking this function for every object and then returning the sum
 * of all numbers.
 */

{
    int n;

    n = 0;
    do {
        if (ob->contains)
            n += deep_inventory_size(ob->contains);
        n++;
    } while ( NULL != (ob = ob->next_inv) );

    return n;
}

/*-------------------------------------------------------------------------*/
static struct svalue *
write_deep_inventory (struct object *first, struct svalue *svp)

/* Helper function for deep_inventory()
 *
 * Copy into <svp> and following a reference to all objects in the
 * inventory chain starting with <first>; then invoke this function
 * for every inventory chain in the found objects.
 *
 * <svp> has to point into a suitably big area of svalue elements, like
 * a vector.
 *
 * Result is the updated <svp>, pointing to the next free svalue element
 * in the storage area.
 */

{
    struct object *ob;

    ob = first;
    do {
        svp->type = T_OBJECT;
        add_ref( (svp->u.ob = ob), "deep_inventory");
        svp++;
    } while ( NULL != (ob = ob->next_inv) );

    ob = first;
    do {
        if (ob->contains)
            svp = write_deep_inventory(ob->contains, svp);
    } while ( NULL != (ob = ob->next_inv) );

    return svp;
}

/*-------------------------------------------------------------------------*/
struct vector *
deep_inventory (struct object *ob, int /* TODO: bool */ take_top)

/* Return a vector with the full inventory of <ob>, i.e. all objects contained
 * by <ob> and all deep inventories of those objects, too. The resulting
 * vector is created by a recursive breadth search.
 *
 * If <take_top> is true, <ob> itself is included as first element in the
 * result vector.
 *
 * The function is used for the efuns deep_inventory() and parse_command().
 */

{
    struct vector *dinv;  /* The resulting inventory vector */
    struct svalue *svp;   /* Next element to fill in dinv */
    int n;                /* Number of elements in dinv */

    /* Count the contained objects */
    n = take_top ? 1 : 0;
    if (ob->contains) {
        n += deep_inventory_size(ob->contains);
    }

    /* Get the array */
    dinv = allocate_array(n);
    svp = dinv->item;

    /* Fill in <ob> if desired */
    if (take_top) {
        svp->type = T_OBJECT;
        add_ref( (svp->u.ob = ob), "deep_inventory");
        svp++;
    }

    /* Fill in the deep inventory */
    if (ob->contains) {
        write_deep_inventory(ob->contains, svp);
    }

    return dinv;
}

/*-------------------------------------------------------------------------*/
static INLINE int
alist_cmp (struct svalue *p1, struct svalue *p2)

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
struct vector *
order_alist (struct svalue *inlists, int listnum, int /* TODO: bool */ reuse)

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
    struct vector *outlist;   /* The result vector of vectors */
    struct vector *v;         /* Aux vector pointer */
    struct svalue *outlists;  /* Next element in outlist to fill in */
    ptrdiff_t * sorted;
      /* The vector elements in sorted order, given as the offsets of the array
       * element in question to the start of the vector. This way,
       * sorted[] needs only to be <keynum> elements long.
       * sorted[] is created from root[] after sorting.
       */

    struct svalue **root;
      /* Auxiliary array with the sorted keys as svalue* into inlists[0].vec.
       * This way the sorting is given by the order of the pointers, while
       * the original position is given by (pointer-inlists[0].vec->item).
       * The very first element is a dummy (heapsort uses array indexing
       * starting with index 1), the next <keynum> elements are scratch
       * area, the final <keynum> elements hold the sorted keys in reverse
       * order.
       */
    struct svalue **root2;   /* Aux pointer into *root. */
    struct svalue *inpnt;    /* Pointer to the value to copy into the result */
    int keynum;              /* Number of keys */
    int i, j;

    keynum = VEC_SIZE(inlists[0].u.vec);

    /* Allocate the auxiliary array. */
    root = (struct svalue **)alloca(keynum * sizeof(struct svalue *[2])
                                           + sizeof(struct svalue)
                                   );
    sorted = alloca(keynum * sizeof(ptrdiff_t) + sizeof(ptrdiff_t));
    /* TODO: keynum may be 0, so the c-alloca() would return NULL without
     * the extra sizeof(ptrdiff_t) :-(
     */

    if (!root || !sorted)
    {
        fatal("Stack overflow in order_alist()");
        /* NOTREACHED */
        return NULL;
    }

    /*
     * Heapsort inlists[0].vec into *root.
     */

    /* Heapify the keys into the first half of root */
    for ( j = 1, inpnt = inlists->u.vec->item
        ; j <= keynum
        ; j++, inpnt++)
    {
        int curix, parix;

        /* make sure that strings can be compared by their pointer */
        if (inpnt->type == T_STRING) {
            if (inpnt->x.string_type != STRING_SHARED) {
                char *str = make_shared_string(inpnt->u.string);
                free_string_svalue(inpnt);
                inpnt->x.string_type = STRING_SHARED;
                inpnt->u.string = str;
            }
        } else if (inpnt->type == T_OBJECT) {
            if (inpnt->u.ob->flags & O_DESTRUCTED) {
                free_object_svalue(inpnt);
                inpnt->type = T_NUMBER;
                inpnt->u.number = 0;
            }
        }
        /* propagate the new element up in the heap as much as necessary */
        for(curix = j; 0 != (parix = curix>>1); ) {
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

        struct svalue *outpnt; /* Next result value element to fill in */

        /* Set the new array v as the next 'out' vector, and init outpnt
         * and offs.
         */
        outlists[i].type  = T_POINTER;
        outlists[i].u.vec = v;
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
                if (inpnt->type == T_OBJECT &&
                    inpnt->u.ob->flags & O_DESTRUCTED)
                {
                    free_object_svalue(inpnt);
                    outpnt->type = T_NUMBER;
                    outpnt->u.number = 0;
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
                if (inpnt->type == T_OBJECT &&
                    inpnt->u.ob->flags & O_DESTRUCTED)
                {
                    outpnt->type = T_NUMBER;
                    outpnt->u.number = 0;
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
int /* TODO: bool */
is_alist (struct vector *v)

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
    struct svalue *svp;
    mp_int i;

    for (svp = v->item, i = VEC_SIZE(v); --i > 0; svp++) {
        if (svp->type == T_STRING && svp->x.string_type != STRING_SHARED)
            return 0;
        if (alist_cmp(svp, svp+1) > 0)
            return 0;
    }
    if (svp->type == T_STRING && svp->x.string_type != STRING_SHARED)
        return 0;

    return 1;
}

/*=========================================================================*/

/*                            EFUNS                                        */

/*-------------------------------------------------------------------------*/
struct svalue *
f_filter_array (struct svalue *sp, int num_arg)

/* EFUN: filter_array()
 *
 *   mixed *filter_array(mixed *arr, string fun)
 *   mixed *filter_array(mixed *arr, string fun, string|object obj, mixed extra, ...)
 *   mixed *filter_array(mixed *arr, closure cl, mixed extra, ...)
 *   mixed *filter_array(mixed *arr, mapping map)
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
 * this_object() is used.
 *
 * As a bonus, all references to destructed objects in <arr> are replaced
 * by proper 0es.
 *
 * TODO: Autodoc-Feature to create doc/efun/filter_array automatically.
 */

{
    struct svalue *arg;   /* First argument the vm stack */
    struct vector *p;     /* The filtered vector */
    mp_int p_size;        /* sizeof(*p) */
    char *func;           /* Functionname and object of the filter */
    struct object *ob;
    int num_extra;        /* Number of extra args to pass */
    struct svalue *extra = NULL; /* Pointer to the <extra> arg(s) on vm stack */
    struct vector *vec;
    struct svalue *v, *w;
    char *flags;          /* Flag array, one flag for each element of <p> */
    int res;              /* Number of surviving elements */
    int cnt;

    res = 0;

    /* Locate the args on the stack, extract the vector to filter
     * and allocate the flags vector.
     */
    arg = sp - num_arg + 1;
    if (arg->type != T_POINTER)
        bad_efun_vararg(1, sp);

    p = arg->u.vec;
    p_size = VEC_SIZE(p);

    flags = alloca(p_size+1);
    if (!flags)
    {
        fatal("Stack overflow in filter_array()");
        /* NOTREACHED */
        return NULL;
    }

    /* Every element in flags is associated by index number with an
     * element in the vector to filter. The filter function is evaluated
     * for every vector element, and the associated flag is set to 0
     * or 1 according to the result.
     * At the end, all 1-flagged elements are gathered and copied
     * into the result vector.
     */

#ifdef MAPPINGS
    if (arg[1].type == T_MAPPING) {

        /* --- Filter by mapping query --- */
        struct mapping *m;

        if (num_arg > 2) {
            inter_sp = sp;
            error("Too many arguments to filter_array()\n");
        }
        m = arg[1].u.map;

        for (w = p->item, cnt = p_size; --cnt >= 0; ) {
            if (w->type == T_OBJECT && w->u.ob->flags & O_DESTRUCTED)
                zero_object_svalue(w);
            if (get_map_lvalue(m, w++, 0) == &const0) {
                flags[cnt] = 0;
                continue;
            }
            flags[cnt] = 1;
            res++;
        }
    } else
#endif
    {
        /* --- Filter by function call --- */

        assign_eval_cost();
        inter_sp = sp;

        /* Gather the filter function parameters */
        if (arg[1].type == T_CLOSURE) {
            ob = 0;
            func = (char *)(arg+1);
            num_extra = num_arg - 2;
            extra = arg + 2;

        } else if (arg[1].type == T_STRING) {
            if (num_arg > 2) {
                num_extra = num_arg - 3;
                if (arg[2].type == T_OBJECT)
                    ob = arg[2].u.ob;
                else if (arg[2].type == T_STRING &&
                    NULL != ( ob = get_object(arg[2].u.string) )) NOOP;
                else bad_efun_vararg(3, sp);
                extra = arg + 3;
            } else {
                ob = current_object;
                num_extra = 0; /* thus extra needs no initialization */
            }
            func = arg[1].u.string;

        } else {
            bad_efun_vararg(2, sp);
        }

        /* Loop over all elements in p and call the filter.
         * w is the current element filtered.
         */
        for (w = p->item, cnt = p_size; --cnt >= 0; ) {
            flags[cnt] = 0;

            if (current_object->flags & O_DESTRUCTED)
                continue;
                /* Don't call the filter anymore, but fill the
                 * flags array with 0es.
                 */

            if (w->type == T_OBJECT && w->u.ob->flags & O_DESTRUCTED)
                zero_object_svalue(w);

            push_svalue(w++);
            push_svalue_block(num_extra, extra);
            if (ob) {
                if (ob->flags & O_DESTRUCTED)
                    error("object used by filter_array destructed");
                v = sapply (func, ob, 1 + num_extra);
                if (!v || (v->type == T_NUMBER && !v->u.number) )
                    continue;
            } else {
                call_lambda((struct svalue *)func, 1 + num_extra);
                v = inter_sp--;
                if (v->type == T_NUMBER) {
                    if (!v->u.number)
                        continue;
                } else {
                    free_svalue(v);
                }
            }

            flags[cnt]=1;
            res++;
        }
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

    /* Cleanup */
    free_vector(p);
    arg->u.vec = vec;
    while(--num_arg)
        free_svalue(sp--);

    return sp;
}

/*-------------------------------------------------------------------------*/
struct svalue *
f_filter_objects (struct svalue *sp, int num_arg)

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
    struct vector *p;          /* The <arr> argument */
    char *func;                /* The <fun> argument */
    struct svalue *arguments;  /* Beginning of 'extra' arguments on vm stack */
    struct vector *w;          /* Result vector */
    /* TODO: bool */ char *flags = NULL;        /* Flag array, one flag for each element of <p> */
    int res;                   /* Count of objects to return */
    struct object *ob;         /* Object to call */
    mp_int p_size;             /* Size of <p> */
    int cnt = 0;
    struct svalue *v;

    assign_eval_cost();
    inter_sp = sp; /* needed for errors in allocate_array(), apply() */

    /* Locate the arguments on the stack and extract them */
    arguments = sp-num_arg+3;
    if (arguments[-2].type != T_POINTER)
        bad_xefun_vararg(1, sp);
    if (arguments[-1].type != T_STRING)
        bad_xefun_vararg(2, sp);

    p = arguments[-2].u.vec;
    func = arguments[-1].u.string;
    num_arg -= 2;

    p_size = VEC_SIZE(p);

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
    switch(arguments[-1].x.string_type) {

      default:
        if ( !(func = findstring(func)) )
            break;
        /* FALLTHROUGH */

      case STRING_SHARED:

        flags = alloca(p_size+1);
        if (!flags)
        {
            fatal("Stack overflow in filter_objects()");
            /* NOTREACHED */
            return NULL;
        }

        for (cnt = 0; cnt < p_size; cnt++) {
            flags[cnt]=0;
            v = &p->item[cnt];

            /* Coerce <v> into a (non-destructed) object ob (if necessary
             * by loading it). If that doesn't work, simply continue
             * with the next element.
             */
            if (v->type != T_OBJECT) {
                if (v->type != T_STRING)
                    continue;
                if ( !(ob = get_object(v->u.string)) )
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

            /* Call the filter lfun and record the result. */
            push_svalue_block(num_arg, arguments);
            v = sapply (func, ob, num_arg);
            if ((v) && (v->type!=T_NUMBER || v->u.number) ) {
                flags[cnt]=1;
                res++;
            }
        } /* for() */
    } /* switch() */

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
            if (flags[--cnt]) {
                struct svalue sv;

                /* Copy the element and update the ref-count */

                *--v = sv = p->item[cnt];
                if (sv.type == T_STRING) {
                    if (sv.x.string_type == STRING_MALLOC) {
                        if ( !(v->u.string = string_copy(sv.u.string)) ) {
                            v->type = T_INVALID;
                            free_vector(w);
                            error("Out of memory\n");
                        }
                    } else {
                        increment_string_ref(sv.u.string);
                    }
                } else {
                    add_ref(sv.u.ob, "filter");
                }

                /* Loop termination check moved in here to save cycles */
                if (v == w->item)
                    break;
            }
        } /* for () */
    } /* if (res) */

    /* Cleanup and return */
    free_vector(p);

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
struct svalue *
f_map_objects (struct svalue *sp, int num_arg)

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
    struct vector *p;          /* The <arr> argument */
    char *func;                /* The <fun> argument */
    struct svalue *arguments;  /* Beginning of 'extra' arguments on vm stack */
    struct vector *r;          /* Result vector */
    struct object *ob;         /* Object to call */
    int size;                  /* Size of <p> */
    int cnt;
    struct svalue *w, *v, *x;

    assign_eval_cost();
    inter_sp = sp;  /* In case of errors leave a clean stack behind */

    arguments = sp-num_arg+3;
    if (arguments[-2].type != T_POINTER)
        bad_xefun_vararg(1, sp);
    if (arguments[-1].type != T_STRING)
        bad_xefun_vararg(2, sp);

    p = arguments[-2].u.vec;
    func = arguments[-1].u.string;
    num_arg -= 2;

    r = allocate_array(size = VEC_SIZE(p));
    arguments[-2].u.vec = r;

    push_referenced_vector(p); /* Ref it from the stack in case of errors */

    /* Call <func> in every object, storing the result in r.
     *
     * Checking if <func> exists as shared string takes advantage of
     * the fact that every existing lfun name is stored as shared string.
     * If it's not shared, no object implements it and we can skip
     * the whole function call loop.
     */

    switch(arguments[-1].x.string_type) {

      default:
        if ( !(func = findstring(func)) )
            break;
        /* FALLTHROUGH */

      case STRING_SHARED:
        for (cnt = size, v = p->item, x = r->item; --cnt >= 0; v++, x++) {

            /* Coerce <v> into a (non-destructed) object ob (if necessary
             * by loading it). If that doesn't work, simply continue
             * with the next element.
             */
            if (v->type != T_OBJECT) {
                if (v->type != T_STRING)
                    continue;
                if ( !(ob = get_object(v->u.string)) )
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
            if (w) {
                *x = *w;
                w->type = T_INVALID;
            }
        } /* for() */
    } /* switch() */

    /* Clean up and return */
    do {
        free_svalue(sp--);
    } while(--num_arg >= 0);
    free_vector(p);

    return sp;
}

/*-------------------------------------------------------------------------*/
static int
search_alist (struct svalue *key, struct vector *keylist)

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

    if ( !(keynum = VEC_SIZE(keylist)) )
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
struct svalue *
insert_alist (struct svalue *key, struct svalue * /* TODO: bool */ key_data, struct vector *list)

/* EFUN insert_alist()
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
    static struct svalue stmp; /* Result value */
    mp_int i,j,ix;
    mp_int keynum, list_size;  /* Number of keys, number of alist vectors */
    int new_member;            /* Flag if a new tuple is given */

    /* If key is a string, make it shared */
    if (key->type == T_STRING && key->x.string_type != STRING_SHARED) {
        char *tmpstr;

        if (last_insert_alist_shared_string)
            free_string(last_insert_alist_shared_string);
        tmpstr = make_shared_string(key->u.string);
        if (key->x.string_type == STRING_MALLOC)
            xfree(key->u.string);
        key->x.string_type = STRING_SHARED;
        key->u.string = tmpstr;
        increment_string_ref(tmpstr);
        last_insert_alist_shared_string = tmpstr;
    }

    keynum = VEC_SIZE(list->item[0].u.vec);

    /* Locate the key */
    ix = search_alist(key, list->item[0].u.vec);

    /* If its just a lookup: return the result.
     */
    if (key_data == 0) {
         stmp.type = T_NUMBER;
         stmp.u.number = ix;
         return &stmp;
    }

    /* Prepare the result alist vector */
    stmp.type = T_POINTER;
    stmp.u.vec = allocate_array(list_size = VEC_SIZE(list));

    new_member = ix == keynum || alist_cmp(key, &list->item[0].u.vec->item[ix]);

    /* Loop over all key/data vectors in <list>, insert/replace the
     * new value and put the new vector into <stmp>.
     */
    for (i = 0; i < list_size; i++) {
        struct vector *vtmp;

        if (new_member) {

            struct svalue *pstmp = list->item[i].u.vec->item;

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
}


/*-------------------------------------------------------------------------*/
int
assoc (struct svalue *key, struct vector *list)

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

    /* If key is a non-shared string, lookup and use the shared copy.
     */
    if (key->type == T_STRING && key->x.string_type != STRING_SHARED) {

        if ( !(assoc_shared_string_key.u.string = findstring(key->u.string)) )
            return -1;
        key = &assoc_shared_string_key;
    }

    i = search_alist(key, list);
    if (i == (int)VEC_SIZE(list) || alist_cmp(key, &list->item[i]))
        i = -1;

    return i;
}

/*-------------------------------------------------------------------------*/
struct vector *
intersect_alist (struct vector *a1, struct vector *a2)

/* EFUN intersect_alist(), also used by generic array intersection.
 *
 * Perform a fast intersection of the alist key vectors <a1> and <a2>.
 * The result is a new sorted(!) vector with all elements, which are present
 * in both input vectors.
 *
 * TODO: Maybe rename the efun.
 */

{
    struct vector *a3;
    int d, l, i1, i2, a1s, a2s;

    a1s = VEC_SIZE(a1);
    a2s = VEC_SIZE(a2);
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
struct vector *
intersect_array (struct vector *a1, struct vector *a2)

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
    struct vector *vtmpp1, *vtmpp2, *vtmpp3;
    static struct svalue ltmp = { T_POINTER };

    /* Order the two ingoing lists and then perform an alist intersection.
     */

    ltmp.u.vec = a1;
    vtmpp1 = order_alist(&ltmp, 1, 1);
    free_vector(ltmp.u.vec);

    ltmp.u.vec = a2;
    vtmpp2 = order_alist(&ltmp, 1, 1);
    free_vector(ltmp.u.vec);

    vtmpp3 = intersect_alist(vtmpp1->item[0].u.vec, vtmpp2->item[0].u.vec);

    free_vector(vtmpp1);
    free_vector(vtmpp2);

    return vtmpp3;
}

/*-------------------------------------------------------------------------*/
struct vector *
match_regexp (struct vector *v, char *pattern)

/* EFUN regexp()
 *
 * Match the content of <v> against the regexp <pattern>
 * Return a new vector of all strings in <v> which match the pattern.
 * Evalcost is sizeof(<v>).
 */

{
    struct regexp *reg;        /* compiled regexp */
    /* TODO: bool */ char *res;           /* res[i] true -> v[i] matches */
    mp_int num_match, v_size;  /* Number of matches, size of <v> */
    struct vector *ret;        /* The result vector */
    mp_int i;

    /* Simple case: empty input yields empty output */
    if ((v_size = VEC_SIZE(v)) == 0)
        return allocate_array(0);

    /* Compile the regexp (or take it from the cache) */
    reg = REGCOMP(pattern, 0);
    if (reg == NULL)
        return NULL;

    /* Check every string in <v> if it matches and set res[]
     * accordingly.
     */
    res = (char *)alloca(v_size);
    if (!res)
    {
        fatal("Stack overflow in regexp()");
        /* NOTREACHED */
        return NULL;
    }

    for (num_match = i = 0; i < v_size; i++) {
        char *line;

        res[i] = 0;

        if (v->item[i].type != T_STRING)
            continue;

        eval_cost++;
        line = v->item[i].u.string;
        if (regexec(reg, line, line) == 0)
            continue;

        res[i] = 1;
        num_match++;
    }

    /* Create the result vector and copy the matching lines */
    ret = allocate_array(num_match);
    for (num_match=i=0; i < v_size; i++) {
        if (res[i] == 0)
            continue;
        assign_svalue_no_free(&ret->item[num_match], &v->item[i]);
        num_match++;
    }

    REGFREE(reg);

    return ret;
}

/*-------------------------------------------------------------------------*/
struct svalue *
f_regexplode (struct svalue *sp)

/* EFUN regexplode()
 *
 *   string *regexplode (string text, string pattern)
 *
 * Explode the <text> by the delimiter <pattern>, returning a vector
 * of the exploded text. Every second element in the result vector
 * is the text that matched the delimiter.
 * Evalcost: number of matches.
 */

{
    /* The found delimiter matches are kept in a list of these
     * structures which are allocated on the stack.
     */
    struct regexplode_match {
        char *start, *end;              /* Start and end of the match in text */
        struct regexplode_match *next;  /* Next list element */
    };

    char *text;                        /* Input text from the vm stack */
    char *pattern;                     /* Delimiter pattern from the vm stack */
    struct regexp *reg;                /* Compiled pattern */
    struct regexplode_match *matches;  /* List of matches */
    struct regexplode_match **matchp;  /* Pointer to previous_match.next */
    struct regexplode_match *match;    /* Current match structure */
    struct vector *ret;                /* Result vector */
    struct svalue *svp;                /* Next element in ret to fill in */
    int num_match;                     /* Number of matches */
    char *str;

    /* Get the efun arguments */
    if (sp[-1].type != T_STRING)
        bad_xefun_arg(1, sp);
    if (sp->type != T_STRING)
        bad_xefun_arg(2, sp);

    text = sp[-1].u.string;
    pattern = sp->u.string;

    reg = REGCOMP(pattern, 0);
    if (reg == 0) {
        inter_sp = sp;
        error("Unrecognized search pattern");
        /* NOTREACHED */
        return NULL;
    }

    /* Loop over <text>, repeatedly matching it against the pattern,
     * until all matches have been found and recorded.
     */
    str = text;        /* Remaining <text> to analyse */
    num_match = 0;
    matchp = &matches;
    while (regexec(reg, str, text)) {
        eval_cost++;
        match = (struct regexplode_match *)alloca(sizeof *match);
        if (!match)
        {
            fatal("Stack overflow in regexplode()");
            /* NOTREACHED */
            return NULL;
        }
        match->start = reg->startp[0];
        match->end = str = reg->endp[0];
        *matchp = match;
        matchp = &match->next;
        num_match++;
        if (!*str || (match->start == str && !*++str) )
            break;
    }
    *matchp = 0; /* Terminate list properly */

    /* Prepare the result vector */
    if (num_match > ((MAX_ARRAY_SIZE-1) >> 1) ) {
        REGFREE(reg);
        inter_sp = sp;
        error("Illegal array size");
        /* NOTREACHED */
        return NULL;
    }
    ret = allocate_array((num_match << 1) + 1);

    /* Walk down the list of matches, extracting the
     * text parts and matched delimiters, copying them
     * into ret.
     */
    svp = ret->item;
    for (match = matches; match; match = match->next) {
        mp_int len;

        /* Copy the text leading up to the current delimiter match. */
        len = match->start - text;
        str = xalloc(len + 1);
        if (!str)
        {
            error("Out of memory.\n");
            /* NOTREACHED */
            return NULL;
        }
        strncpy(str, text, len);
        str[len] = 0;
        text += len;
        svp->type = T_STRING;
        svp->x.string_type = STRING_MALLOC;
        svp->u.string = str;
        svp++;

        /* Copy the matched delimiter */
        len = match->end - text;
        str = xalloc(len + 1);
        if (!str)
        {
            error("Out of memory.\n");
            /* NOTREACHED */
            return NULL;
        }
        strncpy(str, text, len);
        str[len] = 0;
        text += len;
        svp->type = T_STRING;
        svp->x.string_type = STRING_MALLOC;
        svp->u.string = str;
        svp++;
    }

    /* Copy the remaining text (maybe the empty string) */
    svp->type = T_STRING;
    svp->x.string_type = STRING_MALLOC;
    svp->u.string = string_copy(text);

    /* Cleanup */
    REGFREE(reg);
    free_string_svalue(sp);
    sp--;
    free_string_svalue(sp);

    /* Return the result */
    sp->type = T_POINTER;
    sp->u.vec = ret;
    return sp;
}

/*-------------------------------------------------------------------------*/
#ifdef F_INHERIT_LIST

struct svalue *
f_inherit_list (struct svalue *sp)

/* EFUN inherit_list()
 *
 *   string* inherit_list (object ob = this_object())
 *
 * Return a list with the filenames of all programs inherited by <ob>, include
 * <ob>'s program itself.
 * TODO: Must be fixed so that any number of files can be returned, not just 256.
 */

{
    struct object *ob;           /* Analyzed object */
    struct vector *vec;          /* Result vector */
    struct svalue *svp;          /* Pointer to next vec entry to fill in */
    struct program *pr;          /* Next program to count */
    struct program **prp;        /* Pointer to pr->inherit[x].prog */
      /* Incrementing prp by sizeof(inherit) bytes walks along the
       * the vector of inherited programs.
       */
    struct program *plist[256];  /* Table of found programs */
    int next;                    /* Next free entry in plist[] */
    int cur;                     /* Current plist[] entry analyzed */

    /* Get the argument */
    if (sp->type != T_OBJECT)
        bad_xefun_arg(1, sp);
    ob = sp->u.ob;

    inter_sp = sp;
      /* three possibilities for 'out of memory' follow, so clean
       * up the stack now.
       */

    if (O_PROG_SWAPPED(ob))
        if (load_ob_from_swap(ob) < 0) {
            error("Out of memory\n");
            /* NOTREACHED */
            return NULL;
        }

    /* Perform a breadth search on ob's inherit tree and store the
     * program pointers into plist[] while counting them.
     */

    plist[0] = ob->prog;
    next = 1;
    for (cur = 0; cur < next; cur++)
    {
        int cnt;

        pr = plist[cur];
        cnt = pr->num_inherited;
        if (next + cnt > (int)(sizeof plist/sizeof *plist))
            break;

        /* Store the inherited programs in the list.
         *
         * This is an optimized version of:
         *   for (i = 0; i < cnt; i++) plist[next++] = pr->inherit[i].prog;
         */
        prp = &pr->inherit[0].prog;
        while(--cnt >= 0) {
            plist[next++] = *prp;
            prp = (struct program **)((char *)prp + sizeof(struct inherit));
        }
    }

    /* next is also the number of files found :-) */
    vec = allocate_array(next);

    /* Take the filenames of the programs, make them shared and
     * put them into the result vector.
     * TODO: What? The filenames are not shared a priori?
     */
    for (svp = vec->item, prp = plist; --next >= 0; svp++) {
        char *str;

        pr = *prp++;
        if ( !(str = make_shared_string(pr->name)) ) {
            free_vector(vec);
            error("Out of memory\n");
        }
        svp->type = T_STRING;
        svp->x.string_type = STRING_SHARED;
        svp->u.string = str;
    }

    free_object_svalue(sp);

    sp->type = T_POINTER;
    sp->u.vec = vec;
    return sp;
}

#endif /* F_INHERIT_LIST */

/*-------------------------------------------------------------------------*/
struct svalue *
f_functionlist (struct svalue *sp)

/* EFUN functionlist()
 *
 *   mixed *functionlist (object ob, int flags = RETURN_FUNCTION_NAME)
 *
 * Return an array with information about <ob>s lfunctions. For every
 * function, 1 to 4 values (depending on <flags>) are stored in
 * the result array conveying in this order:
 *   - the name of the function
 *   - the function flags (see below)
 *   - the return type (listed in mudlib/sys/lpctypes.h)
 *   - the number of accepted argumens
 *
 * <ob> may be given as true object or as a filename. In the latter
 * case, the efun tries to load the object before proceeding.
 *
 * <flags> determines both which information is returned for every
 * function, and which functions should be considered at all.
 * Its value is created by bin-or'ing together following flags from
 * mudlib/sys/functionlist.h:
 *
 *   Control of returned information:
 *     RETURN_FUNCTION_NAME    include the function name
 *     RETURN_FUNCTION_FLAGS   include the function flags
 *     RETURN_FUNCTION_TYPE    include the return type
 *     RETURN_FUNCTION_NUMARG  include the number of arguments.
 *
 *     The name RETURN_FUNCTION_ARGTYPE is defined but not implemented.
 *
 *   Control of listed functions:
 *     NAME_INHERITED      list if defined by inheritance
 *     TYPE_MOD_STATIC     list if static function
 *     TYPE_MOD_PRIVATE    list if private
 *     TYPE_MOD_PROTECTED  list if protected
 *     NAME_HIDDEN         list if not visible through inheritance
 *
 * The 'flags' information consists of the bin-or of the list control
 * flags given above, plus the following:
 *
 *     TYPE_MOD_VARARGS    function takes varargs
 *     NAME_UNDEFINED      function not defined yet, but referenced.
 *     NAME_CROSS_DEFINED  function is defined to be in a different program
 *     TYPE_MOD_NO_MASK    function is nomask
 *     TYPE_MOD_PUBLIC     function is public
 *
 * All these flags are defined in mudlib/sys/functionlist.h, which
 * should be copied into an accessible place in the mudlib. The
 * return types are defined in mudlib/sys/lpctypes.h which also
 * should be copied into the mudlib.
 *
 * TODO: All these defs are in mudlib/sys/functionlist.h and mudlib/sys/lpctypes.h
 * TODO:: as well as in exec.h and this file. This should be centralized.
 * TODO:: Maybe write the files on mud startup?
 */

{
#define RETURN_FUNCTION_NAME        0x01
#define RETURN_FUNCTION_FLAGS        0x02
#define RETURN_FUNCTION_TYPE        0x04
#define RETURN_FUNCTION_NUMARG        0x08

#define RETURN_FUNCTION_MASK    0x0f  /* union of all RETURN_FUNCTION_ defs */

#define RETURN_FUNCTION_ARGTYPE 0x10 /* not implemented */

    struct object *ob;        /* <ob> argument to list */
    mp_int mode_flags;        /* <flags> argument */
    struct program *prog;     /* <ob>'s program */
    mp_int num_functions;     /* Number of functions to list */
    char *vis_tags;
      /* Bitflag array describing the visibility of every function in prog
       * in relation to the passed <flags>: */
#define VISTAG_INVIS '\0'  /* Function should not be listed */
#define VISTAG_VIS   '\1'  /* Function matches the <flags> list criterium */
#define VISTAG_ALL   '\2'  /* Function should be listed, no list restrictions */

    struct vector *list;      /* Result vector */
    struct svalue *svp;       /* Last element in list which was filled in. */
    uint32 *fun;              /* Current function under examination */
    uint32 active_flags;      /* A functions definition status flags */
    struct program *defprog;  /* Program which actually defines *fun */
    uint32 flags;
    unsigned short *ixp;
    int i, j;

    inter_sp = sp; /* In case of errors leave a clean stack */

    /* Extract the arguments from the vm stack.
     */
    if (sp[-1].type != T_OBJECT) {
        if (sp[-1].type != T_STRING || !(ob = find_object(sp[-1].u.string)))
            bad_xefun_arg(1, sp);
    } else
        ob = sp[-1].u.ob;
    if (sp->type != T_NUMBER)
        bad_xefun_arg(2, sp);

    mode_flags = sp->u.number;

    if (O_PROG_SWAPPED(ob))
        if (load_ob_from_swap(ob) < 0)
        {
            error("Out of memory\n");
            /* NOTREACHED */
            return NULL;
        }

    prog = ob->prog;

    /* Initialize the vistag[] flag array.
     */
    num_functions = prog->num_functions;
    vis_tags = alloca(num_functions);
    if (!vis_tags)
    {
        fatal("Stack overflow in functionlist()");
        /* NOTREACHED */
        return NULL;
    }

    memset(
      vis_tags,
      mode_flags &
      (NAME_HIDDEN|TYPE_MOD_PRIVATE|TYPE_MOD_STATIC|TYPE_MOD_PROTECTED|
       NAME_INHERITED) ?
        VISTAG_INVIS :
        VISTAG_ALL  ,
      num_functions
    );

    flags = mode_flags &
        (TYPE_MOD_PRIVATE|TYPE_MOD_STATIC|TYPE_MOD_PROTECTED|NAME_INHERITED);

    /* Count how many functions need to be listed in the result.
     * Flag every function to list in vistag[].
     * TODO: Document me properly when the layout of programs and functions
     * TODO:: is clear.
     */
    fun = prog->functions;
    num_functions = 0;
    j = prog->num_function_names;
    for (ixp = prog->function_names + j; --j >= 0; ) {
        i = *--ixp;
        if ( !(fun[i] & flags) ) {
            vis_tags[i] = VISTAG_VIS;
            num_functions++;
        }
    }

    /* If <flags> accepts all functions, use the total number of functions
     * instead of the count computed above.
     */
    if ( !(mode_flags &
           (NAME_HIDDEN|TYPE_MOD_PRIVATE|TYPE_MOD_STATIC|TYPE_MOD_PROTECTED|
            NAME_INHERITED) ) )
    {
        num_functions = prog->num_functions;
    }

    /* Compute the size of the result vector to
     *  2**(number of RETURN_FUNCTION_ bits set)
     */
    for (i = mode_flags & RETURN_FUNCTION_MASK, j = 0; i; i >>= 1) {
        if (i & 1)
            j += num_functions;
    }

    /* Allocate the result vector and set svp to its end
     */
    list = allocate_array(j);
    svp = list->item + j;

    /* Loop backwards through all functions, check their flags if
     * they are to be listed and store the requested data in
     * the result vector.
     */

    for(i = prog->num_functions, fun += i; --i >= 0; ) {
        unsigned char *funstart; /* Pointer to function in the executable */

        fun--;

        if (!vis_tags[i]) continue; /* Don't list this one */

        flags = *fun;

        active_flags = (flags & ~INHERIT_MASK);
        if (vis_tags[i] & VISTAG_ALL)
            active_flags |= NAME_HIDDEN; /* TODO: Why? */

        defprog = prog;

        /* If its a cross-defined function, get the flags from
         * real definition and let j point to it.
         */
        if ( !~(flags | ~(NAME_INHERITED|NAME_CROSS_DEFINED) ) ) {
            active_flags |= NAME_CROSS_DEFINED;
            j = (flags & INHERIT_MASK) - ((INHERIT_MASK + 1) >> 1);
            flags = fun[j];
            j += i;
        } else {
            j = i;
        }

        /* If the function is inherited, find the original definition.
         */
        while (flags & NAME_INHERITED) {
            struct inherit *ip = &defprog->inherit[flags & INHERIT_MASK];

            defprog = ip->prog;
            j -= ip->function_index_offset;
            flags = defprog->functions[j];
        }

        /* defprog now points to the program which really defines
         * the function fun.
         */

        funstart = defprog->program + (flags & FUNSTART_MASK);

        /* Add the data to the result vector as <flags> determines.
         */

        if (mode_flags & RETURN_FUNCTION_NUMARG) {
            svp--;
            svp->u.number = (funstart[0] & 0x7f); /* number of arguments */
        }

        if (mode_flags & RETURN_FUNCTION_TYPE) {
            svp--;
            svp->u.number = funstart[-1]; /* return type */
        }

        if (mode_flags & RETURN_FUNCTION_FLAGS) {

            /* If the function starts with the bytecodes F_ESCAPE F_UNDEF,
             * it referenced but undefined. But you know that.
             */
            if (funstart[2] == F_ESCAPE-F_OFFSET &&
                funstart[3] == F_UNDEF-F_OFFSET-0x100)
            {
                active_flags |= NAME_UNDEFINED;
            }
            svp--;
            svp->u.number = active_flags;
        }

        if (mode_flags & RETURN_FUNCTION_NAME) {
            svp--;
            svp->type = T_STRING;
            svp->x.string_type = STRING_SHARED;
            memcpy(
              (char *)&svp->u.string,
              funstart - 1 - sizeof svp->u.string,
              sizeof svp->u.string
            );
            increment_string_ref(svp->u.string);
        }
    } /* for() */

    /* Cleanup and return */
    free_svalue(sp);
    sp--;
    free_svalue(sp);

    sp->type = T_POINTER;
    sp->u.vec = list;
    return sp;

#undef VISTAG_INVIS
#undef VISTAG_VIS
#undef VISTAG_ALL

#undef RETURN_FUNCTION_NAME
#undef RETURN_FUNCTION_FLAGS
#undef RETURN_FUNCTION_TYPE
#undef RETURN_FUNCTION_NUMARG
#undef RETURN_FUNCTION_ARGTYPE
#undef RETURN_FUNCTION_MASK
}

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
    struct svalue *val;   /* The object itself */
    struct svalue mark;   /* The marker value for this object */
    struct unique *same;  /* Next structure in this tooth */
    struct unique *next;  /* Next tooth head */
};

/*-------------------------------------------------------------------------*/
static int
sameval (struct svalue *arg1, struct svalue *arg2)

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
        return !strcmp(arg1->u.string, arg2->u.string);
    } else if (arg1->type == T_OBJECT && arg2->type == T_OBJECT) {
        return arg1->u.ob == arg2->u.ob;
    } else
        return 0;
}


/*-------------------------------------------------------------------------*/
static int
put_in (struct unique **ulist, struct svalue *marker, struct svalue *elem)

/* Insert the object <elem> according to its <marker> value into the comb
 * of unique structures. <ulist> points to the root pointer of this comb.
 * Return the (new) number of distinct markers.
 */

{
    struct unique *llink, *slink, *tlink;
    int cnt;                      /* Number of distinct markers */
    int /* TODO: bool */ fixed;   /* True: <elem> was inserted */

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

            /* TODO: regionalized allocator to reduce fragmentation */
            slink = (struct unique *) xalloc(sizeof(struct unique));
            if (!slink)
            {
                error("Out of memory.\n");
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
    llink = (struct unique *) xalloc(sizeof(struct unique));
    if (!llink)
    {
        error("Out of memory.\n");
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
struct vector *
make_unique (struct vector *arr, char *func, struct svalue *skipnum)

/* EFUN unique_array()
 *
 * See above for the commentary :-)
 *
 * The caller made sure that <arr> contains no destructed objects.
 */

{
    struct svalue *v;
    struct vector *ret;        /* Result vector */
    struct vector *res;        /* Current sub vector in ret */
    struct unique *head;       /* Head of the unique comb */
    struct unique *nxt,*nxt2;
    mp_int arr_size;           /* Size of the incoming <arr>ay */
    mp_int ant;                /* Number of distinct markers */
    mp_int cnt, cnt2;

    head = NULL;

    arr->ref++;  /* TODO: Really necessary? Looks more like a memleak
                  * in case of errors to me. */
    arr_size = VEC_SIZE(arr);

    /* Build the comb structure.
     */
    ant = 0;
    for (cnt = 0; cnt < arr_size; cnt++)
        if (arr->item[cnt].type == T_OBJECT) {
            v = apply(func,arr->item[cnt].u.ob, 0);
            if (v && !sameval(v, skipnum))
                ant = put_in(&head, v, &(arr->item[cnt]));
        }

    arr->ref--;

    ret = allocate_array(ant);

    /* Copy the objects from the comb structure into the result vector,
     * deallocating the structure by this.
     * The elements are stored in reverse to compensate put_in(),
     * but TODO: does someone really care?
     */

    for (cnt = ant-1; cnt >= 0; cnt--) {
        ret->item[cnt].type = T_POINTER;
        ret->item[cnt].u.vec = res = allocate_array(head->count);

        nxt2 = head;
        head = head->next;

        cnt2 = 0;
        while (nxt2) {
            assign_svalue_no_free (&res->item[cnt2++], nxt2->val);
            free_svalue(&nxt2->mark);
            nxt = nxt2->same;
            xfree((char *) nxt2);
            nxt2 = nxt;
        }

        if (!head)
            break; /* It shouldn't but, to avoid skydive just in case */
    }

    return ret;
}

/***************************************************************************/

