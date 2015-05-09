/* These sefuns are to provide a replacement for the efuns enable_commands()
 * and disable_commands().
 * Feel free to add it to your mudlibs, if you have much code relying on that.
 */

#include <configuration.h>

#if ! __EFUN_DEFINED__(enable_commands)

void enable_commands()
{
    object ob = efun::previous_object();

    efun::configure_object(ob, OC_COMMANDS_ENABLED, 1);
    efun::set_this_player(ob);
}

#endif

#if ! __EFUN_DEFINED__(disable_commands)

void disable_commands()
{
    object ob = efun::previous_object();

    efun::configure_object(ob, OC_COMMANDS_ENABLED, 0);
    efun::set_this_player(0);
}

#endif
