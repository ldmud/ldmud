/*---------------------------------------------------------------------------
 * svalue comparison functions.
 *
 *---------------------------------------------------------------------------
 */

#ifndef I_SVALUE_CMP__
#define I_SVALUE_CMP__

#include "driver.h"

#include "closure.h" /* closure_eq(), closure_cmp() */
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

    if (left->type == T_STRING)
    {
        return mstr_order(left->u.str, right->u.str);
    }

    /* Avoid a numeric overflow by first comparing the values halfed. */
    if ( 0 != (d = (left->u.number >> 1) - (right->u.number >> 1)) ) return d;
    if ( 0 != (d = left->u.number - right->u.number) ) return d;

    switch (left->type)
    {
    case T_FLOAT:
    case T_SYMBOL:
    case T_QUOTED_ARRAY:
        if ( 0 != (d = left->x.generic - right->x.generic) ) return d;
        break;
    }
    return 0;
} /* svalue_cmp() */

/*-------------------------------------------------------------------------*/
static INLINE int
svalue_eq (svalue_t *left, svalue_t *right)

/* Compare *left and *right, return 0 if equal, and -1 if not (this
 * is to keep in line with the svalue_cmp() return values).
 *
 * See also svalue_cmp() for the general version.
 */

{
    if (left->type != right->type)
        return -1;

    if (left->type == T_STRING)
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
    case T_FLOAT:
    case T_SYMBOL:
    case T_QUOTED_ARRAY:
        return left->x.generic != right->x.generic ? -1 : 0;
    default:
        return 0;
    }

    /* NOTREACHED */
    return 0;
} /* svalue_eq() */

/***************************************************************************/

#endif /* I_SVALUE_CMP__ */
