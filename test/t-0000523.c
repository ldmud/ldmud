#include "/inc/base.inc"
#include "/inc/gc.inc"

#include "/sys/input_to.h"
#include "/sys/rtlimits.h"

closure get_cl()
{
    return (: 42 :);
}

void run_test()
{
    object dummy = clone_object(this_object());

    msg("\nRunning test for #0000523:\n"
          "--------------------------\n");
    /* Check if the driver does handle closures
       that are bound to destructed objects. */    

    set_driver_hook(H_DEFAULT_METHOD, dummy->get_cl());
    destruct(dummy);
    if(catch((dummy=clone_object(this_object()))->i_dont_exit()))
        shutdown(1);
    else
        start_gc(#'shutdown);
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
