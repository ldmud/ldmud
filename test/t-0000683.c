#include "/inc/base.inc"

#include "/sys/configuration.h"

/* These functions are for the clone (the player object). */
int sleeping;

void start_client()
{
    net_connect("127.0.0.1", query_mud_port());
    sleeping = 1;
}

int logon(int flag)
{
    set_prompt("");
    if(!sleeping)
    {
        object me;

        configure_interactive(this_object(), IC_MAX_WRITE_BUFFER_SIZE, 0);
        set_buffer_size(0);

        /* 100 MB should be enough. */
        foreach(int i: 10240)
            write("*"*10240+"\n");

        me = clone_object(this_object());
        exec(me, this_object());
        write("Done.\n");
    }

    return 1;
}

/* Here comes the master. */

object connect()
{
    set_prompt("");
    return clone_object(this_object());
}

void run_test()
{
    msg("\nRunning test for #0000683:\n"
          "--------------------------\n");

    /* Waiting until LDMud is ready for users. */
    call_out("run_test2", 0);
}

void run_test2()
{
    object dummy;

    call_out(#'shutdown, __ALARM_TIME__ * 2, 0);

    dummy = clone_object(this_object());
    dummy->start_client();
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
