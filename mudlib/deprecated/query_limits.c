/* This sefun is to provide a replacement for the efun query_limits().
 * Feel free to add it to your mudlibs, if you have much code relying on that.
 */

#if ! __EFUN_DEFINED__(query_limits)

#include <driver_info.h>

varargs int* query_limits(int def)
{
    return efun::driver_info(def ? DC_DEFAULT_RUNTIME_LIMITS : DI_CURRENT_RUNTIME_LIMITS);
}

#endif
