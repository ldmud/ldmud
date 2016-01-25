/* This sefun is to provide a replacement for the efuns set_max_commands()
 * and get_max_commands.
 * Feel free to add it to your mudlibs, if you have much code relying on that.
 */

#if ! __EFUN_DEFINED__(set_max_commands)

#include <configuration.h>

varargs void set_max_commands(int num, object ob)
{
    ob ||= efun::this_interactive();

    int result;

    if (num < 0)
        num = -1;

    if(!efun::call_direct_resolved(&result, __MASTER_OBJECT__, "privilege_violation",
        "set_max_commands", previous_object(), ob, num))
            result = -1;

    if(result < 0)
        raise_error("privilege violation: set_max_commands\n");

    if(result > 0)
        efun::configure_interactive(ob, IC_MAX_COMMANDS, num);
}

varargs int get_max_commands(object ob)
{
    ob ||= efun::this_interactive();

    return efun::interactive_info(ob, IC_MAX_COMMANDS);
}

#endif
