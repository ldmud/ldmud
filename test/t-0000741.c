// This test must be run without any catch() in the call chain. Otherwise the
// control stack will be handled differently and not crash.

#pragma save_types,rtt_checks

#include "/inc/base.inc"
#include "/inc/gc.inc"

#include "/sys/input_to.h"
#include "/sys/rtlimits.h"

void fun(int x) { }

void run_test()
{
    object dummy = clone_object(this_object());

    msg("\nRunning test for #0000741:\n"
          "--------------------------\n");

    // simple test: evaluating a closure from a different object with a
     // wrong argument type must not crash, but raise an error.
     funcall(symbol_function("fun",dummy),"abc");
     // not reached, because the above command should raise an error (or
     // crash).
}

string *epilog(int eflag)
{
    call_out(function void () { msg("Success\n"); shutdown(0); }, 0);
    run_test();
    return 0;
}
