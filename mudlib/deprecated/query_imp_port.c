/* This sefun is to provide a replacement for the efun query_imp_port().
 * Feel free to add it to your mudlibs, if you have much code relying on that.
 */

#if ! __EFUN_DEFINED__(query_imp_port)

#include <driver_info.h>

int query_imp_port()
{
    return efun::driver_info(DI_UDP_PORT);
}

#endif
