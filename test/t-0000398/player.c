#include "/inc/gc.inc"

#include "/sys/configuration.h"
#include "/sys/driver_info.h"

void start_client()
{
    net_connect("127.0.0.1", efun::driver_info(DI_MUD_PORTS)[0]);
}

int logon(int flag)
{
    configure_interactive(this_object(), IC_TELNET_ENABLED, 0);
    configure_interactive(this_object(), IC_PROMPT, "");

    call_out(#'start_gc, 1,
    (:
	if($1)
	    shutdown(1);
	else
	{
	    write("x\n");
	    call_out(#'start_gc, 1, #'shutdown);
	    return;
	}
    :));    
    return 1;
}
