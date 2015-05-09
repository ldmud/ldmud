/* This sefun is to provide a replacement for the efun query_once_interactive().
 * Feel free to add it to your mudlibs, if you have much code relying on that.
 */

#if ! __EFUN_DEFINED__(query_once_interactive)

#include <object_info.h>

int query_once_interactive(object ob)
{
    return efun::object_info(ob, OI_ONCE_INTERACTIVE);
}

#endif
