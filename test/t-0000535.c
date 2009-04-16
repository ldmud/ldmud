#include "/inc/base.inc"
#include "/inc/gc.inc"

/* These functions are for the clones (the player objects). */

int active = 0; /* The object, that does the input_tos. */
void start_client()
{
    net_connect("127.0.0.1", query_mud_port());
    active = 1;
}

int test_action(string str)
{
    input_to("fun3");
    remove_input_to(this_object(), "fun3");
    input_to("fun4");
	
    if(find_input_to(this_object(), "fun4")<0)
        shutdown(1);
    else
        start_gc(#'shutdown);
    
    return 1;
}

int logon(int flag)
{
    enable_telnet(0);
    enable_commands();
    set_prompt("");
    
    if(active)
    {
	input_to("fun1");

	call_out(
	    function void()
	    {
		input_to("fun2");
	
	        if(find_input_to(this_object(), "fun2")<0)
		    shutdown(1);
	        else
		    start_gc(#'shutdown);
	    }, __ALARM_TIME__);
	add_action("test_action", "test");
    }
    else
	call_out(#'write, 2*__ALARM_TIME__, "!test\n");
	    
    return 1;
}

/* These functions are for the blueprint (the real master). */
object connect()
{
    return clone_object(this_object());
}

void run_test()
{
    msg("\nRunning test for #0000535:\n"
          "--------------------------\n");

    /* Waiting until LDMud is ready for users. */
    call_out("run_test2", 0);
}

void run_test2()
{
    object dummy;
    dummy = clone_object(this_object());
    dummy->start_client();
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
