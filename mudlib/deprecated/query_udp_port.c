/* This sefun is to provide a replacement for the efun query_udp_port().
 * Feel free to add it to your mudlibs, if you have much code relying on that.
 */

#if ! __EFUN_DEFINED__(query_udp_port)

#include <driver_info.h>

int query_udp_port()
{
    return efun::driver_info(DI_UDP_PORT);
}

#endif
