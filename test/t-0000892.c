#include "/inc/base.inc"
#include "/inc/client.inc"

#include "/sys/input_to.h"

bytes b(string s)
{
    return to_bytes(s, "ISO-8859-1");
}

void run_server()
{
    binary_message(b"a\n\nb");
}

void run_client()
{
    input_to("client_input1");
    call_out("end_client", __ALARM_TIME__);
}

int step;

void client_input1(string str)
{
    step = 1;
    if (str != "a") {
        msg("FAILURE: Received %O instead of %O.\n", str, "a");
        shutdown(1);
    }
    input_to("client_input2", INPUT_CHARMODE);
}

void client_input2(string str)
{
    step = 2;
    if (str != "\n") {
        msg("FAILURE: Received %O instead of %O.\n", b(str), b("\n"));
        shutdown(1);
    } else {
        msg("Success!\n");
        shutdown(0);
    }
}

void end_client()
{
    msg("FAILURE: Received %d/2 messages.\n", step);
    shutdown(1);
}

void run_test()
{
    msg("\nRunning test for #0000892:\n"
          "--------------------------\n");

    connect_self("run_server", "run_client");
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
