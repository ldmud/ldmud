/* These sefuns are to provide a replacement for the efun query_shadowing()
 * and the old semantics of the efun shadow().
 * Feel free to add it to your mudlibs, if you have much code relying on that.
 */

#if ! __EFUN_DEFINED__(query_shadowing)

#include <object_info.h>

object query_shadowing(object ob)
{
    return efun::object_info(ob, OI_SHADOW_PREV);
}

object shadow(object ob, int flag)
{
    if(flag)
    {
        object shadower = efun::previous_object();
        efun::set_this_object(shadower);

        if (efun::shadow(ob))
            return efun::object_info(shadower, OI_SHADOW_PREV);
        else
            return 0;
    }
    else
        return efun::object_info(ob, OI_SHADOW_NEXT);
}

#endif
