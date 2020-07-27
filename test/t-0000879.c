#pragma save_types, rtt_checks

#include "/sys/configuration.h"
#include "/inc/base.inc"

void finish()
{
      msg("Success. (No Crash)\n");
      shutdown(0);
}

void run_test()
{
    // We need to create an additional reference to the program
    // so clean_up will be called with a value != 0.
    object ob = clone_object(this_object());

    msg("\nRunning test for #0000879:\n"
          "--------------------------\n");

    set_driver_hook(H_CLEAN_UP, "clean_up");
    configure_driver(DC_CLEANUP_TIME, 1);

    call_out(#'finish, 2);
}

public void clean_up(object ob)
{
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
