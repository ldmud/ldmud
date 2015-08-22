/* This sefun is to provide a replacement for the efun query_load_average().
 * Feel free to add it to your mudlibs, if you have much code relying on that.
 */

#if ! __EFUN_DEFINED__(query_load_average)

#include <driver_info.h>

string query_load_average()
{
    return efun::sprintf("%.2f cmds/s, %.2f comp lines/s",
        efun::driver_info(DI_LOAD_AVERAGE_COMMANDS),
        efun::driver_info(DI_LOAD_AVERAGE_LINES));
}

#endif
