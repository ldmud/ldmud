#include "/inc/gc.inc"

void start_client()
{
    net_connect("127.0.0.1", query_mud_port());
}

int logon(int flag)
{
    enable_telnet(0);
    set_prompt("");

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
