/* These are the special commands from the driver that are activated with
 * set_is_wizard(). These functions must be added to the player object.
 * Also set_is_wizard() must be called in the corresponding player object,
 * not as an (simul-)efun.
 */

#include <commands.h>
#include <driver_info.h>

/* is_wizard:
 *  1: has actions,
 *  0: never had actions,
 * -1: had actions once.
 */
private nosave int is_wizard;

private int driver_malloc(string str)
{
    if(is_wizard <= 0)
        return 0;

    if(!sizeof(str))
    {
        write(efun::driver_info(DI_STATUS_TEXT_MALLOC));
        return 1;
    }

    if(str == "extstats")
    {
        write(efun::driver_info(DI_STATUS_TEXT_MALLOC_EXTENDED));
        return 1;
    }

    return 0;
}

private int driver_dumpallobj(string str)
{
    if(is_wizard <= 0 || sizeof(str))
        return 0;

    write("Dumping to /OBJ_DUMP ... ");
    efun::dump_driver_info(DDI_OBJECTS);
    efun::dump_driver_info(DDI_OBJECTS_DESTRUCTED);
    write("done\n");
    return 1;
}

private int driver_opcdump(string str)
{
    if(is_wizard <= 0 || sizeof(str))
        return 0;

    efun::dump_driver_info(DDI_OPCODES);
    return 1;
}

private int driver_status(string str)
{
    int opt;
    if(is_wizard <= 0)
        return 0;

    switch(str || "")
    {
        case "":
            opt = DI_STATUS_TEXT_MEMORY;
            break;

        case "tables":
        case " tables":
            opt = DI_STATUS_TEXT_TABLES;
            break;

        case "swap":
        case " swap":
            opt = DI_STATUS_TEXT_SWAP;
            break;

        case "malloc":
        case " malloc":
            opt = DI_STATUS_TEXT_MALLOC;
            break;

        case "malloc extstats":
        case " malloc extstats":
            opt = DI_STATUS_TEXT_MALLOC_EXTENDED;
            break;

        default:
            return 0;
    }

    write(efun::driver_info(opt));
    return 1;
}

int set_is_wizard(varargs <object|int>* args)
{
    int oldval = is_wizard;

    if(!sizeof(args))
        raise_error("Too few arguments to set_is_wizard\n");
    if(sizeof(args) > 2)
        raise_error("Too many arguments to set_is_wizard\n");
    if(!objectp(args[0]))
        raise_error("Bad arg 1 to set_is_wizard()\n");
    if(args[0] != this_object())
        raise_error("Only set_is_wizard for the current object supported\n");
    if(this_player() != this_object())
        raise_error("The current object must be this_player() for set_is_wizard()\n");
    if(sizeof(args) == 2 && !intp(args[1]))
        raise_error("Bad arg 2 to set_is_wizard()\n");

    if(sizeof(args) == 2 && !args[1])
    {
        if(is_wizard > 0)
            is_wizard = -1;
    }
    else if(sizeof(args) == 2 && args[1]<0)
    {
         // Just return the old value.
    }
    else
    {
        if(!is_wizard)
        {
            add_action(#'driver_malloc, "malloc");
            add_action(#'driver_dumpallobj, "dumpallobj");
            add_action(#'driver_opcdump, "opcdump");
            add_action(#'driver_status, "status", AA_NOSPACE);
        }
        is_wizard = 1;
    }

    return oldval > 0;
}
