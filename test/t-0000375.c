#include "/inc/base.inc"

/* These functions are for the clone (the player object). */
void start_client()
{
    net_connect("127.0.0.1", query_mud_port());
    set_heart_beat(1);
}

int logon(int flag)
{
    enable_telnet(0);
    set_prompt("");

    return 1;
}

object connect()
{
   enable_telnet(0);
   set_prompt("");

   return clone_object(this_object());
}

void fun(int* oldtime)
{
    int* now = utime();

    remove_call_out(#'shutdown);

    // Is there a time difference of
    // at least 0.5 seconds?
    if (oldtime[1] < 500000)
        oldtime[1] += 500000;
    else
    {
        oldtime[1] -= 500000;
        oldtime[0] += 1;
    }

    if ((oldtime[0] == now[0])
       ?(oldtime[1] < now[1])
       :(oldtime[0] < now[0]))
    {
        msg("Success\n");
        shutdown(0);
    }
    else
    {
        msg("Failure: call_out from heart_beat within 0.5s.\n");
        shutdown(1);
    }
}

void heart_beat()
{
    call_out("fun", __ALARM_TIME__, utime());
}

void run_test()
{
    int result;

    msg("\nRunning test for #0000375:\n"
        "--------------------------\n");

    call_out("run_test2", 0);
}

void run_test2()
{
    object dummy;
    dummy = clone_object(this_object());
    dummy->start_client();

    call_out(#'shutdown, (__HEART_BEAT_INTERVAL__+__ALARM_TIME__)*2, 1); // Just to make sure.
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
