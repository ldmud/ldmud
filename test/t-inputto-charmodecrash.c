#include "/inc/base.inc"
#include "/inc/client.inc"

#include "/sys/configuration.h"
#include "/sys/input_to.h"

void input(string str)
{
}

int test_action(string str)
{
    input_to("input", INPUT_PROMPT|INPUT_CHARMODE, "Eingabe: ");
    return 1;
}

int quit_action(string str)
{
    efun::shutdown(0);
    return 1;
}

void run_server()
{
    configure_interactive(this_object(), IC_TELNET_ENABLED, 1);

    add_action("test_action", "test");
    add_action("quit_action", "quit");
}

void run_client()
{
    call_out(#'write, __ALARM_TIME__ + 0, "test\njaha\n");
    call_out(#'write, __ALARM_TIME__ + 1, "\nquit\n");
}

void run_test()
{
    msg("\nRunning test for crash when finishing charmode:\n"
          "-----------------------------------------------\n");

    connect_self("run_server", "run_client");
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
