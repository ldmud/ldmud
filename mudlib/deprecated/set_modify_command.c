/* This sefun is to provide a replacement for the efun set_modify_command().
 * Feel free to add it to your mudlibs, if you have much code relying on that.
 */

#if ! __EFUN_DEFINED__(set_modify_command)

#include <configuration.h>

object set_modify_command(varargs <object|string|int>* arg)
{
    object ob = efun::previous_object();
    object|string modifier;
    object|string oldmodifier = efun::interactive_info(ob, IC_MODIFY_COMMAND);

    if(!sizeof(arg))
        modifier = ob;
    else if(!intp(arg[0]) || !arg[0])
        modifier = arg[0];
    else
        return oldmodifier;

    efun::configure_interactive(ob, IC_MODIFY_COMMAND, modifier);
    return oldmodifier;
}

#endif
