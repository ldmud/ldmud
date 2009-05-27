#include "/inc/base.inc"

#include "/sys/input_to.h"

/* These functions are for the clone (the player object). */
int active;
void start_client()
{
    net_connect("127.0.0.1", query_mud_port());
    active = 1;
}

int logon(int flag)
{
    enable_telnet(0);
    set_prompt("");

    add_action("test_action", "test");

    if(active)
        ed("/dummy","ed_ends");
    return 1;
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

/* These functions are for the blueprint (the virtual player that
   sends us the commands). */

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

object connect()
{
    enable_telnet(0);
    set_prompt("");

    write("!test\n=\n=\n");
    call_out(#'shutdown, 1, 1); // If something goes wrong.
    input_to("receive", 0, 0);

    return clone_object(this_object()); // Just a dummy object.
}

void run_test()
{
    msg("\nRunning test for #0000522:\n"
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
