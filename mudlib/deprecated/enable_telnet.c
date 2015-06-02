/* This sefun is to provide a replacement for the efun enable_telnet().
 * Feel free to add it to your mudlibs, if you have much code relying on that.
 */

#if ! __EFUN_DEFINED__(enable_telnet)

#include <configuration.h>

varargs int enable_telnet(int num, object ob)
{
    ob ||= efun::this_interactive();

    int oldstate = efun::interactive_info(ob, IC_TELNET_ENABLED);

    if (num >= 0)
    {
        int result;

        if(!efun::call_direct_resolved(&result, __MASTER_OBJECT__, "privilege_violation",
            "enable_telnet", previous_object(), ob, num))
                result = -1;
        if(result < 0)
            raise_error("privilege violation: enable_telnet\n");
        if(result > 0)
            efun::configure_interactive(ob, IC_TELNET_ENABLED, num);
    }

    return oldstate;
}

#endif
