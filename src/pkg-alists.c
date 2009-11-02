/*---------------------------------------------------------------------------
 * Alist handling functions.
 *
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
 *---------------------------------------------------------------------------
 */

#include "driver.h"
#include "typedefs.h"

#ifdef USE_ALISTS

#include "pkg-alists.h"

#include "my-alloca.h"
#include <stddef.h>

#include "array.h"
#include "interpret.h" /* destructed_object_ref(), error functions */
#include "main.h"
#include "mstrings.h"
#include "simulate.h"  /* errorf() */
#include "svalue.h"
#include "xalloc.h"

#include "i-svalue_cmp.h"

/*-------------------------------------------------------------------------*/
static vector_t *
intersect_ordered_arr (vector_t *a1, vector_t *a2)

/* Compute the intersection of the two ordered arrays <a1> and <a2>.
 *
 * The result is a new sorted(!) vector with all elements, which are present
 * in both input vectors.
 * This function is called by f_intersect_alists().
 */

{
    vector_t *a3;
    mp_int d, l, i1, i2, a1s, a2s;

    a1s = (mp_int)VEC_SIZE(a1);
    a2s = (mp_int)VEC_SIZE(a2);
    a3 = allocate_array( a1s < a2s ? a1s : a2s);
    for (i1=i2=l=0; i1 < a1s && i2 < a2s; ) {
        d = svalue_cmp(&a1->item[i1], &a2->item[i2]);
        if (d<0)
            i1++;
        else if (d>0)
            i2++;
        else {
            assign_svalue_no_free(&a3->item[l++], &a2->item[(i1++,i2++)] );
        }
    }
    return shrink_array(a3, l);
} /* intersect_ordered_arr() */

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
 * As a side effect, strings in the key vector are made shared, and
 * destructed objects in key and data vectors are replaced by svalue 0s.
 *
 * This function is also called by the compiler for constant expressions.
 */

{
    vector_t *outlist;   /* The result vector of vectors */
    vector_t *v;         /* Aux vector pointer */
    svalue_t *outlists;  /* Next element in outlist to fill in */
    ptrdiff_t * sorted;  /* The vector elements in sorted order */
    svalue_t *inpnt;     /* Pointer to the value to copy into the result */
    mp_int keynum;       /* Number of keys */
    int i, j;

    keynum = (mp_int)VEC_SIZE(inlists[0].u.vec);

    /* Get the sorting order */

    sorted = get_array_order(inlists[0].u.vec);

    /* Generate the result vectors from the sorting order.
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

            if (i) /* not the last iteration */
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

            if (i) /* Not the last iteration: get new out-vector */
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

    xfree(sorted);

    return outlist;
} /* order_alist() */

/*-------------------------------------------------------------------------*/
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
 * 2. Lookup a <key> in the alist <list> and return its index. If the key
 *    is not found, return  the position at which it would be inserted.
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
    if (key->type == T_STRING && !mstr_tabled(key->u.str))
    {
        key->u.str = make_tabled(key->u.str);
    }

    keynum = (mp_int)VEC_SIZE(list->item[0].u.vec);

    /* Locate the key */
    ix = lookup_key(key, list->item[0].u.vec);

    /* If its just a lookup: return the result.
     */
    if (key_data == NULL) {
         put_number(&stmp, ix < 0 ? -ix-1 : ix);
         return &stmp;
    }

    /* Prepare the result alist vector */
    put_array(&stmp, allocate_array(list_size = (mp_int)VEC_SIZE(list)));

    new_member = ix < 0;
    if (new_member)
        ix = -ix-1;

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

/*=========================================================================*/

/*                            EFUNS                                        */

/*-------------------------------------------------------------------------*/
svalue_t *
v_insert_alist (svalue_t *sp, int num_arg)

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
         || (size_t)VEC_SIZE(list->item[i].u.vec) != keynum)
        {
            errorf("Type or size mismatch of the data arrays.\n");
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
            if (VEC_SIZE(sp[-1].u.vec) != listsize)
            {
                errorf("Size mismatch of the data arrays: "
                      "vec size %ld, list size %ld.\n"
                     , (long)VEC_SIZE(sp[-1].u.vec), (long)listsize
                     );
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
            errorf("Not enough data given: %ld arguments, %ld listsize.\n"
                 , (long)num_arg - 1, (long)listsize);
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
} /* v_insert_alist() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_assoc (svalue_t *sp, int num_arg)

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
                errorf("Number of values in key and data arrays differ.\n");
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
                errorf("Number of values in key and data arrays differ.\n");
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
            errorf("too many args to efun assoc\n");
            /* NOTREACHED */
            return sp;
        }
    }

    /* Call lookup_key() and push the result */
    ix = lookup_key(&args[0],keys);
    if (data == NULL)
    {
        sp = pop_n_elems(num_arg, sp);
        push_number(sp, ix < 0 ? -1 : ix);
    }
    else
    {
        assign_svalue(args
                     , ix < 0
                       ? fail_val
                       : (destructed_object_ref(&data->item[ix])
                         ? &const0
                         : &data->item[ix])
                     );
        sp = pop_n_elems(num_arg-1, sp);
    }

    return sp;
} /* v_assoc() */

/*-------------------------------------------------------------------------*/
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

    rc = intersect_ordered_arr(sp[-1].u.vec, sp->u.vec);

    free_svalue(sp--);
    free_array(sp->u.vec);
    sp->u.vec = rc;

    return sp;
} /* f_intersect_alist() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_order_alist (svalue_t *sp, int num_arg)

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
         || (size_t)VEC_SIZE(args[i].u.vec) != keynum)
        {
            errorf("bad data array %d in call to order_alist\n",i);
        }
    }

    /* Create the alist */
    list = order_alist(args, listsize, reuse);
    sp = pop_n_elems(num_arg, sp);
    sp++;
    put_array(sp, list);

    return sp;
} /* v_order_alist() */

#endif /* USE_ALISTS */

/***************************************************************************/

