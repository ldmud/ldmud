#pragma save_types, rtt_checks
#include "/inc/base.inc"
#include "/inc/client.inc"

void testfun(int arg)
{
}

void heart_beat() {}

void run_client()
{
    set_heart_beat(1);

    foreach(int i:  __HEART_BEAT_INTERVAL__)
        call_out(#'testfun, i+1, "abc");

    call_out(#'shutdown, __HEART_BEAT_INTERVAL__+2, 0);
}

void run_test()
{
    msg("\nRunning test for #0000715:\n"
          "--------------------------\n");

    // (S)Efun closures as call_outs left current_prog set.
    call_out(#'intp, 0, 0);
    // So this lead to a crash on an error before the function
    // is called (csp is not valid, but current_prog is set).
    call_out(#'testfun, 0, "abc");

    // Heart beats also left current_prog, so we test that, too.
    // We need a client for heart_beat to work.
    connect_self("run_client", 0);
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
