// Check that we can receive a bunch of lines at once rather than having to
// wait for the next backend cycle after each line of input.

#include "/inc/base.inc"
#include "/inc/client.inc"

#include "/sys/input_to.h"

void run_server()
{
   write("H\n"*10);
}

int start;

void run_client()
{
    start = time();
    input_to("client_input", 0, 1);
}

void client_input(string str, int c)
{
    msg("ci " + c + "\n");
    if (time() - start >= 2 * __ALARM_TIME__) {
        msg("FAILURE: Took too long to receive 10 lines.\n");
        shutdown(1);
        return;
    }

    if (c == 10) {
        msg("Success!\n");
        shutdown(0);
        return;
    }

    input_to("client_input", 0, c + 1);
}

void run_test()
{
    msg("\nRunning test for receiving several lines at once:\n"
          "-------------------------------------------------\n");

    connect_self("run_server", "run_client");
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
