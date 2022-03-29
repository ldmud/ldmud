#include "/inc/base.inc"
#include "/inc/client.inc"

#include "/sys/input_to.h"

bytes b(string s)
{
    return to_bytes(s, "ISO-8859-1");
}

void run_server()
{
    binary_message(b"\x00");
}

void run_client()
{
    input_to("client_input", INPUT_CHARMODE);
    call_out("end_client", __ALARM_TIME__);
}

void client_input(string str)
{
    if (str != "\0") {
        msg("FAILURE: Received %O instead of %O.\n", b(str), b"\x00");
        shutdown(1);
    }
    msg("Success!\n");
    shutdown(0);
}

void end_client()
{
    msg("FAILURE: Received nothing.\n");
    shutdown(1);
}

void run_test()
{
    msg("\nRunning test for #0000446:\n"
          "--------------------------\n");

    connect_self("run_server", "run_client");
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
