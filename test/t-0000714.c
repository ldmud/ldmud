#pragma save_types, rtt_checks
#include "/inc/base.inc"

int x; // One variable, otherwise restore_object takes a shortcut.

void run_test()
{
    msg("\nRunning test for #0000714:\n"
          "--------------------------\n");

    call_out(#'shutdown, 0, 1);
    restore_object("#1:0\na \"Hello, World!\"\nx 1\nb \"Hello, World!\"\n");

    shutdown(0);
    remove_call_out(#'shutdown);
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
