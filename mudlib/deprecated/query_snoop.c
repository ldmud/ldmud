/* This sefun is to provide a replacement for the efun query_snoop().
 * Feel free to add it to your mudlibs, if you have much code relying on that.
 */

#if ! __EFUN_DEFINED__(query_snoop)

#include <interactive_info.h>

object query_snoop(object ob)
{
    if(!efun::interactive(ob))
        return 0;

    object prev = efun::previous_object();
    efun::set_this_object(prev);

    return efun::interactive_info(ob, II_SNOOP_NEXT);
}

#endif
