#include "/inc/base.inc"
#include "/inc/gc.inc"
#include "/inc/client.inc"

#include "/sys/configuration.h"

/* These functions are for the clones (the player objects). */

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

void run_server()
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

void run_client()
{
    call_out(#'write, 2*__ALARM_TIME__, "!test\n");
}

void run_test()
{
    msg("\nRunning test for #0000535:\n"
          "--------------------------\n");

    connect_self("run_server", "run_client");
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
