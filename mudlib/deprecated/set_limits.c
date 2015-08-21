/* This sefun is to provide a replacement for the efun set_limits().
 * Feel free to add it to your mudlibs, if you have much code relying on that.
 */

#if ! __EFUN_DEFINED__(set_limits)

#include <driver_info.h>
#include <rtlimits.h>

void set_limits(varargs mixed* args)
{
    int * limits;
    int * def;

    if(!sizeof(args))
        raise_error("No arguments given.\n");

    if(sizeof(args) == 1 && pointerp(args[0]))
        limits = args[0];
    else if(sizeof(args) % 2)
        raise_error("set_limits(): Invalid limit specification.\n");
    else
    {
        // Extract the limits...
        limits = ({ LIMIT_KEEP }) * LIMIT_MAX;
        for(int i = 0; i < sizeof(args); i+= 2)
        {
            if(!intp(args[i]))
                raise_error("Illegal limit tag: expected a number\n");

            if(!intp(args[i+1]))
                raise_error("Illegal limit value: expected a number\n");

            if(args[i] < 0 || args[i] >= LIMIT_MAX)
                raise_error(sprintf("Unimplemented limit #%d\n", args[i]));

            limits[args[i]] = args[i+1];
        }
    }

    def = efun::driver_info(DC_DEFAULT_RUNTIME_LIMITS);
    for(int i = 0; i < sizeof(limits); i++)
    {
        if(limits[i] == LIMIT_KEEP || limits[i] == LIMIT_DEFAULT)
            limits[i] = def[i];
    }

    int result;

    if(!efun::call_direct_resolved(&result, __MASTER_OBJECT__, "privilege_violation",
            "set_limits", previous_object(), limits))
        result = -1;

    if(result < 0)
        raise_error("privilege violation: set_limits\n");
    else if (result > 0)
        efun::configure_driver(DC_DEFAULT_RUNTIME_LIMITS, limits);
}

#endif
