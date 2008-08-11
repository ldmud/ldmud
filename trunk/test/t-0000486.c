#include "/inc/base.inc"
#include "/inc/gc.inc"

/* These functions are for the clone (the player object). */
void start_client()
{
    net_connect("127.0.0.1", query_mud_port());
}

int logon(int flag)
{
    enable_telnet(0);
    set_prompt("");
    return 1;
}

/* These functions are for the blueprint (the virtual player that
   sends us the commands). */
object connect()
{
    enable_telnet(0);
    set_prompt("");

    catch(efun::input_to("bla",4,({1}),({2})));
    
    start_gc(#'shutdown);

    return 0;
}

void run_test()
{
    msg("\nRunning test for #0000486:\n"
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
