/* This sefun is to provide a replacement for the efun seteuid().
 * Feel free to add it to your mudlibs, if you have much code relying on that.
 */

#if ! __EFUN_DEFINED__(seteuid)

#include <configuration.h>

int seteuid(string str)
{
    object ob = efun::previous_object();

    if (!str)
    {
        efun::configure_object(ob, OC_EUID, 0);
        return 1;
    }

    if (efun::call_direct(__MASTER_OBJECT__, "valid_seteuid", ob, str) != 1)
        return 0;

    efun::configure_object(ob, OC_EUID, str);
    return 1;
}

#endif
