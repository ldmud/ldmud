#include "/sys/lpctypes.h"

/*
 * deep equality: make a best effort to check whether arg1 and arg2
 * represent the same value.
 */
int deep_eq(mixed arg1, mixed arg2)
{
    for (;;) {
        if (typeof(arg1) != typeof(arg2))
            return 0;
        switch (typeof(arg1)) {
        case T_INVALID:
            raise_error("Don't know how to compare T_INVALID.\n");

        case T_LVALUE:
            raise_error("Don't know how to compare T_LVALUE.\n");

        case T_NUMBER:
        case T_FLOAT:
        case T_STRING:
        case T_OBJECT:
            return arg1 == arg2;

        case T_POINTER:
            if (sizeof(arg1) != sizeof(arg2))
                return 0;
            for (int i = 0; i < sizeof(arg1); i++)
                if (!deep_eq(arg1[i], arg2[i]))
                    return 0;
            return 1;

        case T_MAPPING:
            if (widthof(arg1) != widthof(arg2) ||
                sizeof(arg1) != sizeof(arg2))
                return 0;
            foreach (mixed k: arg1) {
                mixed row1 = m_entry(arg1, k);
                mixed row2 = m_entry(arg2, k);
                if (!row2)
                    return 0;
                for (int i = 0; i < sizeof(row1); i++)
                    if (!deep_eq(row1[i], row2[i]))
                        return 0;
            }
            return 1;

        case T_CLOSURE:
            if (get_type_info(arg1, 1) != get_type_info(arg2, 1) ||
                get_type_info(arg1, 2) != get_type_info(arg2, 2))
                return 0;
            int t = get_type_info(arg1, 1);
            if (CLOSURE_IS_LFUN(t))
                return to_int(arg1) == to_int(arg2);
            if (CLOSURE_IS_IDENTIFIER(t))
                return to_int(arg1) == to_int(arg2);
            if (CLOSURE_IS_BOUND_LAMBDA(t) ||
                CLOSURE_IS_LAMBDA(t) ||
                CLOSURE_IS_UNBOUND_LAMBDA(t))
                raise_error("Don't know how to compare lambda closures.\n");
            if (CLOSURE_IS_SIMUL_EFUN(t))
                return 1;
            if (CLOSURE_IS_EFUN(t))
                return 1;
            if (CLOSURE_IS_OPERATOR(t))
                return 1;
            raise_error("Unknown closure type " + t + ".\n");

        case T_SYMBOL:
        case T_QUOTED_ARRAY:
            arg1 = unquote(arg1);
            arg2 = unquote(arg2);
            break;

        default:
            raise_error("Unknown type " + typeof(arg1) + ".\n");
        }
    }
    return 0; /* not reached */
}
