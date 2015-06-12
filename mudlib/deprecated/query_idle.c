/* This sefun is to provide a replacement for the efun query_idle().
 * Feel free to add it to your mudlibs, if you have much code relying on that.
 */

#if ! __EFUN_DEFINED__(query_idle)

#include <interactive_info.h>

int query_idle(object ob)
{
    return efun::interactive_info(ob, II_IDLE);
}

#endif
