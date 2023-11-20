#include "/inc/base.inc"
#include "/inc/gc.inc"

#include "/sys/configuration.h"

void run_test()
{
    msg("\nRunning crasher...\n"
          "------------------\n");

    call_out("finish", 1);
    load_object("crasher");
    return 0;
}

void finish(int success)
{
    remove_call_out("finish");

    if (success)
        start_gc(#'shutdown);
    else
        shutdown(1);
}

string *epilog(int eflag)
{
    set_driver_hook(H_RESET,        "reset");
    set_driver_hook(H_CREATE_OB,    "create");
    set_driver_hook(H_CREATE_CLONE, "create");

    configure_driver(DC_MEMORY_LIMIT, ({ 1024*1024*1024, 2048*1024*1024 }));

    run_test();
    return 0;
}
