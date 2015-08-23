#include "/inc/base.inc"
#include "/inc/client.inc"

#include "/sys/configuration.h"

/* These functions are for the clone (the player object). */
void run_server()
{
    configure_object(this_object(), OC_HEART_BEAT, 1);
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

    connect_self("run_server", 0);
    call_out(#'shutdown, (__HEART_BEAT_INTERVAL__+__ALARM_TIME__)*2, 1); // Just to make sure.
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
