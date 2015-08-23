#include "/inc/base.inc"
#include "/inc/client.inc"

#include "/sys/configuration.h"

void run_server()
{
    object me;

    configure_interactive(this_object(), IC_MAX_WRITE_BUFFER_SIZE, 0);
    configure_interactive(this_object(), IC_SOCKET_BUFFER_SIZE, 0);

    /* 100 MB should be enough. */
    foreach(int i: 10240)
        write("*"*10240+"\n");

    me = clone_object(this_object());
    exec(me, this_object());
    write("Done.\n");
}

void run_test()
{
    msg("\nRunning test for #0000683:\n"
          "--------------------------\n");

    connect_self("run_server", 0);
    call_out(#'shutdown, __ALARM_TIME__ * 2, 0);
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
