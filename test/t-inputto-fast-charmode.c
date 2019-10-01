#include "/inc/base.inc"
#include "/inc/client.inc"

#include "/sys/input_to.h"

/* This test checks that with INPUT_CHARMODE all
 * characters are handled in the same backend cycle
 * (within half a second).
 */

/* This is the MUD object */
void run_server()
{
    input_to("test_input_to", INPUT_CHARMODE, utime(), 0);
}

void test_input_to(string str, int* start, int num)
{
    if (num == 3)
    {
        int* now = utime();
        int diff = (now[0] - start[0]) * 10000000 + now[1] - start[1];
        msg("Time difference: %d microseconds\n", diff);
        shutdown(diff > 500000); // Half a second
    }
    else
        input_to("test_input_to", INPUT_CHARMODE, start, num+1);
}

/* This is the object simulating a player. */
void run_client()
{
    write("ABCD");
    call_out(#'shutdown, 3, 1); // If something goes wrong.
}

void run_test()
{
    msg("\nRunning test for fast handling of charmode input_to:\n"
          "----------------------------------------------------\n");

    connect_self("run_server", "run_client");
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
