#pragma save_types, rtt_checks
#include "/inc/base.inc"
#include "/inc/client.inc"
#include "/inc/gc.inc"

void run_ed()
{
    // Just to make sure.
    call_out(#'shutdown, __HEART_BEAT_INTERVAL__, 1);

    rm("/dummy");
    ed("/dummy","ed_ends");
}

void ed_ends()
{
     __MASTER_OBJECT__->check_test();
}

void check_test()
{
    rm("/dummy");
    start_gc(#'shutdown);
}

void send_ed_cmds()
{
    write("f\nf /dummy2\ne /dummy3\nQ\n");
}

void run_test()
{
    msg("\nRunning ed() leak test:\n"
          "-----------------------\n");

    connect_self("run_ed", "send_ed_cmds");
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
