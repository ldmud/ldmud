/* This sefun is to provide a replacement for the efun query_mud_port().
 * Feel free to add it to your mudlibs, if you have much code relying on that.
 */

#if ! __EFUN_DEFINED__(query_mud_port)

#include <interactive_info.h>
#include <driver_info.h>

int query_mud_port(varargs int*|object* args)
{
    if(!sizeof(args) && efun::this_player() && efun::interactive(this_player()))
        return efun::interactive_info(this_player(), II_MUD_PORT);

    if(sizeof(args) > 1)
        raise_error("Too many arguments to query_mud_port\n");

    if(sizeof(args) && efun::objectp(args[0]) && efun::interactive(args[0]))
        return efun::interactive_info(args[0], II_MUD_PORT);

    if(sizeof(args) && intp(args[0]))
    {
        int* ports = efun::driver_info(DI_MUD_PORTS);
        if(args[0] < -1 || args[0] >= sizeof(ports))
            raise_error(efun::sprintf("Bad arg 1 to query_mud_port(): value %d out of range.\n", args[0]));
        else if(args[0] == -1)
            return sizeof(ports);
        else
            return ports[args[0]];
    }

    return efun::driver_info(DI_MUD_PORTS)[0];
}

#endif
