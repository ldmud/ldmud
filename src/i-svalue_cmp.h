/*---------------------------------------------------------------------------
 * svalue comparison functions.
 *
 *---------------------------------------------------------------------------
 */

#ifndef I_SVALUE_CMP__
#define I_SVALUE_CMP__

#include "driver.h"

#include "closure.h" /* closure_eq(), closure_cmp() */
#include "interpret.h" /* get_rvalue() */
#include "svalue.h"

/*-------------------------------------------------------------------------*/
static INLINE int
svalue_cmp (svalue_t *left, svalue_t *right)

/* Order function for sorts.
 *
 * Compare the svalues <left> and <right> and return an integer with the
 * following meaning:
 *
 *   > 0: <left> 'is greater than' <right>
 *   = 0: <left> 'is equal to' <right>
 *   < 0: <left> 'is less than' <right>
 *
 * The relation need not make sense with the actual interpretation
 * of <left>/<right>, as long as it defines a deterministic order relation.
 *
 * See also svalue_eq() for a more specialized version.
 *
 * TODO: Is the assumption '.number is big enough to hold everything
 * TODO:: in the svalue' true for future hardware?
 * TODO: Reinterpreting the pointers as 'integer' may not be portable
 * TODO:: enough.
 */

{
    register p_int d;

    if ( 0 != (d = left->type - right->type) ) return d;

    if (left->type == T_CLOSURE)
        return closure_cmp(left, right);

    if (left->type == T_STRING || left->type == T_BYTES)
    {
        return mstr_order(left->u.str, right->u.str);
    }

    /* Avoid a numeric overflow by first comparing the values halfed. */
    if ( 0 != (d = (left->u.number >> 1) - (right->u.number >> 1)) ) return d;
    if ( 0 != (d = left->u.number - right->u.number) ) return d;

    switch (left->type)
    {
#ifndef FLOAT_FORMAT_2
    case T_FLOAT:
#endif
    case T_SYMBOL:
    case T_QUOTED_ARRAY:
    case T_LVALUE:
        if ( 0 != (d = left->x.generic - right->x.generic) ) return d;
        break;
    }
    return 0;
} /* svalue_cmp() */

/*-------------------------------------------------------------------------*/
static INLINE int
rvalue_cmp (svalue_t *left, svalue_t *right)

/* Order function for sorts.
 *
 * Compares just like svalue_cmp, but dereferences lvalues before.
 * So the comparison is done on the plain rvalue with the exception
 * of array ranges, which are compared as their own reference type.
 *
 * This function will normalize any given svalue (ie. shorten
 * lvalue chains, replace destructed object refs with 0).
 */

{
    svalue_t *left_rv = get_rvalue(left, NULL);
    svalue_t *right_rv = get_rvalue(right, NULL);

    if (left_rv == NULL && right_rv == NULL)
    {
        /* Both are ranges. */
        struct protected_range_lvalue *lr = left->u.protected_range_lvalue, *rr = right->u.protected_range_lvalue;
        register p_int d;

        if (lr->vec.type != rr->vec.type)
        {
            if (lr->vec.type == T_POINTER)
                return left->type - rr->vec.type;
            else if (rr->vec.type == T_POINTER)
                return lr->vec.type - right->type;
        }

        if (lr->vec.type != T_POINTER)
        {
            /* String ranges */
            if ((d = (lr->vec.type - rr->vec.type)) != 0)
                return d;

            if ((d = (lr->index2 - lr->index1) - (rr->index2 - rr->index1)) != 0)
                return d;

            return memcmp(get_txt(lr->vec.u.str) + lr->index1, get_txt(rr->vec.u.str) + rr->index1, lr->index2 - lr->index1);
        }
        else
        {
            /* Array ranges */
            return lr - rr;
        }
    }
    else if (left_rv == NULL || right_rv == NULL)
    {
        struct protected_range_lvalue *r = ((left_rv == NULL) ? left : right)->u.protected_range_lvalue;
        svalue_t *sv = (left_rv == NULL) ? right_rv : left_rv;
        size_t len;
        register p_int d;

        if (r->vec.type == T_POINTER)
            return left->type - right->type;

        if (sv->type != r->vec.type)
            return ((sv->type < r->vec.type) == (left_rv == NULL)) ? 1 : -1;

        len = mstrsize(sv->u.str);
        if ((d = (len - (r->index2 - r->index1))) != 0)
            return ((d < 0) == (left_rv == NULL)) ? 1 : -1;

        d = memcmp(get_txt(sv->u.str), get_txt(r->vec.u.str) + r->index1, len);
        if (left_rv == NULL)
            return -d;
        else
            return d;
    }

    return svalue_cmp(left_rv, right_rv);
} /* rvalue_cmp() */

/*-------------------------------------------------------------------------*/
static INLINE int
svalue_eq (svalue_t *left, svalue_t *right)

/* Compare *left and *right, return 0 if equal, and -1 if not
 * (this is to keep in line with the svalue_cmp() return values).
 *
 * See also svalue_cmp() for the general version.
 */

{
    if (left->type != right->type)
        return -1;

    if (left->type == T_STRING || left->type == T_BYTES)
    {
        return mstreq(left->u.str, right->u.str) ? 0 : -1;
    }

    if (left->type == T_CLOSURE)
    {
        return closure_eq(left, right) ? 0 : -1;
    }

    /* All other types have to be equal by address, visible in u.number */
    /* TODO: This comparison is not valid according to ISO C */
    if (left->u.number != right->u.number)
        return -1;

    switch (left->type)
    {
#ifndef FLOAT_FORMAT_2
    case T_FLOAT:
#endif
    case T_SYMBOL:
    case T_QUOTED_ARRAY:
    case T_LVALUE:
        return left->x.generic != right->x.generic ? -1 : 0;
    default:
        return 0;
    }

    /* NOTREACHED */
    return 0;
} /* svalue_eq() */

/*-------------------------------------------------------------------------*/
static INLINE int
rvalue_eq (svalue_t *left, svalue_t *right)

/* Compare both values.
 *
 * Compares just like svalue_eq, but dereferences lvalues before.
 * So the comparison is done on the plain rvalue with the exception
 * of array ranges, which are compared as their own reference type.
 *
 * This function will normalize any given svalue (ie. shorten
 * lvalue chains, replace destructed object refs with 0).
 */

{
    svalue_t *left_rv = get_rvalue(left, NULL);
    svalue_t *right_rv = get_rvalue(right, NULL);

    if (left_rv == NULL && right_rv == NULL)
    {
        /* Both are ranges. */
        struct protected_range_lvalue *lr = left->u.protected_range_lvalue, *rr = right->u.protected_range_lvalue;
        if (lr->vec.type != rr->vec.type)
            return -1;

        if (lr->vec.type != T_POINTER)
        {
            /* String ranges */
            if ((lr->index2 - lr->index1) != (rr->index2 - rr->index1))
                return -1;

            return (memcmp(get_txt(lr->vec.u.str) + lr->index1, get_txt(rr->vec.u.str) + rr->index1, lr->index2 - lr->index1) == 0) ? 0 : -1;
        }
        else
        {
            /* Array ranges */
            return (lr == rr) ? 0 : -1;
        }
    }
    else if (left_rv == NULL || right_rv == NULL)
    {
        struct protected_range_lvalue *r = ((left_rv == NULL) ? left : right)->u.protected_range_lvalue;
        svalue_t *sv = (left_rv == NULL) ? right_rv : left_rv;
        size_t len;

        if (r->vec.type == T_POINTER || sv->type == T_POINTER || r->vec.type != sv->type)
            return -1;

        len = mstrsize(sv->u.str);
        if (len != r->index2 - r->index1)
            return -1;

        return (memcmp(get_txt(sv->u.str), get_txt(r->vec.u.str) + r->index1, len) == 0) ? 0 : -1;
    }

    return svalue_eq(left_rv, right_rv);
} /* rvalue_eq() */

/***************************************************************************/

#endif /* I_SVALUE_CMP__ */
