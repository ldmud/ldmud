#include "/inc/base.inc"
#include "/inc/client.inc"

#include "/sys/input_to.h"

/* This is the MUD object */
void run_server()
{
    add_action("test_action", "test");
    ed("/dummy","ed_ends");
}

void ed_ends()
{
}

int test_action(string str)
{
    input_to("test_input_to", INPUT_PROMPT, "+");
    return 1;
}

void test_input_to(string str)
{
    if(str != "=")
    {
        msg("Failed: Wrong message received.\n");
        shutdown(1);
        return;
    }

    write("A\n"); // Not a number as 'ed' would show.
}

/* This is the object simulating a player. */

void receive(string str, int nr)
{
    if(!strstr(str, "\"dummy\" ") || !strstr(str, "/dummy,"))
    {
        input_to("receive", 0, nr);
        return;
    }

    if(!nr)
    {
        if(strstr(str,":"))
        {
            msg("Failed: Ed prompt expected.\n");
            shutdown(1);
            return;
        }

        str = str[1..];
    }

    // We just verify, that the prompt
    // corresponds to the answer.
    if(str != "+A" && str != ":0")
    {
        msg("Failed: Received %Q as the %d. line.\n", str, nr+1);
        shutdown(1);
        return;
    }

    if(nr)
    {
        msg("Success.\n");
        shutdown(0);
    }
    else
        input_to("receive", 0, 1);
}

void run_client()
{
    write("!test\n=\n=\n");
    call_out(#'shutdown, 1, 1); // If something goes wrong.
    input_to("receive", 0, 0);
}

void run_test()
{
    msg("\nRunning test for #0000522:\n"
          "--------------------------\n");

    connect_self("run_server", "run_client");
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
