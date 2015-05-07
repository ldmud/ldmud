/* This sefun is the provide the old semantics of the efun object_info().
 * Feel free to add it to your mudlibs, if you have much code relying on that.
 */

#if __EFUN_DEFINED__(driver_info) /* Newer version is there */

#include <objectinfo.h>
#include <object_info.h>

mixed object_info(object ob, int what, varargs int* index)
{
    mixed * result;

    if (sizeof(index) > 1)
        raise_error("Too many arguments to object_info\n");

    switch(what)
    {
        default:
            raise_error(sprintf("Illegal value %d for object_info().\n", what));

        case OINFO_BASIC:
        {
            result = allocate(OIB_MAX);

            result[OIB_HEART_BEAT]        = efun::object_info(ob, OC_HEART_BEAT);
            result[OIB_IS_WIZARD]         = 0;   /* Not supported anymore. */
            result[OIB_ENABLE_COMMANDS]   = efun::object_info(ob, OC_COMMANDS_ENABLED);
            result[OIB_CLONE]             = efun::clonep(ob);
            result[OIB_DESTRUCTED]        = 0;   /* Not possible anymore. */
            result[OIB_SWAPPED]           = efun::object_info(ob, OI_SWAPPED);
            result[OIB_ONCE_INTERACTIVE]  = efun::object_info(ob, OI_ONCE_INTERACTIVE);
            result[OIB_RESET_STATE]       = efun::object_info(ob, OI_RESET_STATE);
            result[OIB_WILL_CLEAN_UP]     = efun::object_info(ob, OI_WILL_CLEAN_UP);
            result[OIB_LAMBDA_REFERENCED] = efun::object_info(ob, OI_LAMBDA_REFERENCED);
            result[OIB_SHADOW]            = efun::object_info(ob, OI_SHADOW_PREV) && 1;
            result[OIB_REPLACED]          = efun::object_info(ob, OI_REPLACED);
            result[OIB_NEXT_RESET]        = efun::object_info(ob, OI_NEXT_RESET_TIME);
            result[OIB_TIME_OF_REF]       = efun::object_info(ob, OI_LAST_REF_TIME);
            result[OIB_REF]               = efun::object_info(ob, OI_OBJECT_REFS);
            result[OIB_GIGATICKS]         = efun::object_info(ob, OI_GIGATICKS);
            result[OIB_TICKS]             = efun::object_info(ob, OI_TICKS);
            result[OIB_SWAP_NUM]          = efun::object_info(ob, OI_SWAP_NUM);
            result[OIB_PROG_SWAPPED]      = efun::object_info(ob, OI_PROG_SWAPPED);
            result[OIB_VAR_SWAPPED]       = efun::object_info(ob, OI_VAR_SWAPPED);
            result[OIB_NAME]              = efun::object_name(ob);
            result[OIB_LOAD_NAME]         = efun::load_name(ob);
            result[OIB_NEXT_ALL]          = efun::object_info(ob, OI_OBJECT_NEXT);
            result[OIB_PREV_ALL]          = efun::object_info(ob, OI_OBJECT_PREV);
            result[OIB_NEXT_CLEANUP]      = efun::object_info(ob, OI_NEXT_CLEANUP_TIME);
            break;
        }

        case OINFO_POSITION:
        {
            result = allocate(OIP_MAX);

            result[OIP_NEXT] = efun::object_info(ob, OI_OBJECT_NEXT);
            result[OIP_PREV] = efun::object_info(ob, OI_OBJECT_PREV);
            result[OIP_POS]  = efun::object_info(ob, OI_OBJECT_POS);
            break;
        }

        case OINFO_MEMORY:
        {
            result = allocate(OIM_MAX);

            result[OIM_REF]                = efun::object_info(ob, OI_PROG_REFS);
            result[OIM_NAME]               = efun::program_name(ob);
            result[OIM_PROG_SIZE]          = efun::object_info(ob, OI_PROG_SIZE);
            result[OIM_NUM_FUNCTIONS]      = efun::object_info(ob, OI_NUM_FUNCTIONS);
            result[OIM_SIZE_FUNCTIONS]     = efun::object_info(ob, OI_SIZE_FUNCTIONS);
            result[OIM_NUM_VARIABLES]      = efun::object_info(ob, OI_NUM_VARIABLES);
            result[OIM_SIZE_VARIABLES]     = efun::object_info(ob, OI_SIZE_VARIABLES);
            result[OIM_NUM_STRINGS]        = efun::object_info(ob, OI_NUM_STRINGS);
            result[OIM_SIZE_STRINGS]       = efun::object_info(ob, OI_SIZE_STRINGS);
            result[OIM_SIZE_STRINGS_DATA]  = efun::object_info(ob, OI_SIZE_STRINGS_DATA);
            result[OIM_SIZE_STRINGS_TOTAL] = efun::object_info(ob, OI_SIZE_STRINGS_DATA_TOTAL);
            result[OIM_NUM_INHERITED]      = efun::object_info(ob, OI_NUM_INHERITED);
            result[OIM_SIZE_INHERITED]     = efun::object_info(ob, OI_SIZE_INHERITED);
            result[OIM_TOTAL_SIZE]         = efun::object_info(ob, OI_PROG_SIZE_TOTAL);
            result[OIM_DATA_SIZE]          = efun::object_info(ob, OI_DATA_SIZE);
            result[OIM_TOTAL_DATA_SIZE]    = efun::object_info(ob, OI_DATA_SIZE_TOTAL);
            result[OIM_NO_INHERIT]         = efun::object_info(ob, OI_NO_INHERIT);
            result[OIM_NO_CLONE]           = efun::object_info(ob, OI_NO_CLONE);
            result[OIM_NO_SHADOW]          = efun::object_info(ob, OI_NO_SHADOW);
            result[OIM_NUM_INCLUDES]       = efun::object_info(ob, OI_NUM_INCLUDED);
            result[OIM_SHARE_VARIABLES]    = efun::object_info(ob, OI_SHARE_VARIABLES);
            break;
        }
    }

    if (sizeof(index))
    {
        int idx = index[0];
        if (idx < 0 || idx >= sizeof(result))
            raise_error(sprintf("Illegal index for object_info(): %d, expected 0..%d\n",
                idx, sizeof(result)-1));

        return result[idx];
    }
    else
        return result;
}

#endif
