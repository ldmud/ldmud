/*---------------------------------------------------------------------------
 * svalue hash function.
 *
 *---------------------------------------------------------------------------
 */

#ifndef I_SVALUE_HASH__
#define I_SVALUE_HASH__

#include <assert.h>

#include "driver.h"
#include "svalue.h"

/*-------------------------------------------------------------------------*/
static INLINE int
svalue_hash (svalue_t *svp, int bits)

/* Hash function for svalues.
 *
 * This is a relative simple hash function without any cryptographic
 * aspirations. It returns a value with its lower <bits> bits usable
 * in a hash table (<bits> < 32).
 */

{
    p_int result;

    switch (svp->type)
    {
        default:
            fatal("Illegal svalue type %d\n", svp->type);

        case T_LVALUE:
            switch (svp->x.lvalue_type)
            {
                case LVALUE_PROTECTED:
                    result = (p_int) svp->u.protected_lvalue;
                    break;

                case LVALUE_PROTECTED_CHAR:
                    result = (p_int) svp->u.protected_char_lvalue;
                    break;

                case LVALUE_PROTECTED_RANGE:
                    result = (p_int) svp->u.protected_range_lvalue;
                    break;

                case LVALUE_PROTECTED_MAPENTRY:
                    result = (p_int) svp->u.protected_mapentry_lvalue;
                    break;

                case LVALUE_PROTECTED_MAP_RANGE:
                    result = (p_int) svp->u.protected_map_range_lvalue;
                    break;

                default:
                    fatal("Illegal lvalue %p type %d\n", svp, svp->x.lvalue_type);
                    break;
            }
            break;

        case T_NUMBER:
            result = svp->u.number;
            break;

        case T_STRING:
        case T_BYTES:
        case T_SYMBOL:
            result = mstr_get_hash(svp->u.str);
            if (svp->type == T_SYMBOL)
                result ^= svp->x.quotes;
            break;

        case T_POINTER:
            result = (p_int)svp->u.vec;
            break;

        case T_OBJECT:
            result = (p_int)svp->u.ob;
            break;

        case T_MAPPING:
            result = (p_int)svp->u.map;
            break;

        case T_FLOAT:
#ifdef FLOAT_FORMAT_2
            /* We use the binary representation of the double value. */
            result = svp->u.number;
#else
            result = svp->u.mantissa ^ svp->x.exponent;
#endif
            break;

        case T_CLOSURE:
            switch (svp->x.closure_type)
            {
                case CLOSURE_LFUN:
                    result = (p_int)svp->u.lfun_closure;
                    break;

                case CLOSURE_IDENTIFIER:
                    result = (p_int)svp->u.identifier_closure;
                    break;

                case CLOSURE_BOUND_LAMBDA:
                    result = (p_int)svp->u.bound_lambda;
                    break;

                case CLOSURE_LAMBDA:
                case CLOSURE_UNBOUND_LAMBDA:
                    result = (p_int)svp->u.lambda;
                    break;

                default:
                    if (svp->x.closure_type < CLOSURE_LWO)
                        result = ((p_int)svp->u.lwob) ^ svp->x.closure_type;
                    else
                        result = ((p_int)svp->u.ob) ^ svp->x.closure_type;
                    break;
            }
            break;

        case T_QUOTED_ARRAY:
            result = ((p_int)svp->u.vec) ^ svp->x.quotes;
            break;

        case T_STRUCT:
            result = (p_int)svp->u.strct;
            break;

        case T_LWOBJECT:
            result = (p_int)svp->u.lwob;
            break;

        case T_COROUTINE:
            result = (p_int)svp->u.coroutine;
            break;
    }

#if SIZEOF_CHAR_P > 4
    result ^= result >> 32;
#endif

    /* Fibonacci Hashing by Knuth. */
    if (bits >= 32)
        return result * 265443576910;
    else
        return ((result * 265443576910) >> (32-bits)) & ((1ULL<<bits) - 1);
} /* svalue_hash() */

#endif /* I_SVALUE_CMP__ */
