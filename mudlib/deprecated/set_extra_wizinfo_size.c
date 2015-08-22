/* This sefun is to provide a replacement for the efun set_extra_wizinfo_size().
 * Feel free to add it to your mudlibs, if you have much code relying on that.
 */

#if ! __EFUN_DEFINED__(set_extra_wizinfo_size)

#include <configuration.h>

void set_extra_wizinfo_size(int size)
{
    int result;

    if(!efun::call_direct_resolved(&result, __MASTER_OBJECT__, "privilege_violation",
            "set_extra_wizinfo_size", previous_object(), size))
        result = -1;

    if(result < 0)
        raise_error("privilege violation: set_extra_wizinfo_size\n");
    else if (result > 0)
        efun::configure_driver(DC_EXTRA_WIZINFO_SIZE, size);
}

#endif
