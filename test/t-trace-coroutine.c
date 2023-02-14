#include "/inc/base.inc"
#include "/inc/client.inc"

#include "/sys/trace.h"

/* We test calling coroutines while efun::trace() is active. */

void run_server()
{
    efun::trace(TRACE_CALL_OTHER|TRACE_RETURN|TRACE_ARGS);

    coroutine cr = async function void()
    {
        yield("A");
        yield("B");
        yield("C");
    };

    foreach(string str: cr)
        if (!stringp(str))
        {
            efun::shutdown(1);
            return;
        }

    efun::trace(TRACE_NOTHING);

    /* The test succeeds, if we didn't crash. */
    call_out(#'efun::shutdown,0,0);
}

void run_client()
{
    input_to("client_input");
}

void client_input(string str)
{
    input_to("client_input");
}

void run_test()
{
    msg("\nRunning test for tracing with coroutines:\n"
          "-----------------------------------------\n");

    /* For efun::trace() we need an interactive object. */
    connect_self("run_server", "run_client");
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}

int valid_trace(string what, int|string arg)
{
    return 1;
}
