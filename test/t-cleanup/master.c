#include "/sys/configuration.h"
#include "/sys/driver_hook.h"

#include "/inc/base.inc"

mapping resets = ([:1]), cleanups = ([:2]);

async void sleep(int sec = __ALARM_TIME__)
{
    call_out(#'call_coroutine, sec, this_coroutine());

    yield();
}

async void run_test()
{
    int errors;
    object ob1;
    object clone1, clone2;

    msg("\nRunning test for reset and cleanup:\n"
          "-----------------------------------\n");

    ob1 = load_object("ob1");
    load_object("ob2");
    clone1 = clone_object("/clone");
    clone2 = clone_object("/clone");

    ob1->fun();
    clone1->fun();

    await(sleep(3*__ALARM_TIME__));

    if ("/base" in resets)
    {
        msg("H_RESET was called for /base!\n");
        errors++;
    }
    if (!("/ob1" in resets))
    {
        msg("H_RESET was not called for /ob1!\n");
        errors++;
    }
    if ("/ob2" in resets)
    {
        msg("H_RESET was called for /ob2!\n");
        errors++;
    }
    if ("/clone" in resets)
    {
        msg("H_RESET was called for /clone!\n");
        errors++;
    }
    if (!(object_name(clone1) in resets))
    {
        msg("H_RESET was not called for clone#1!\n");
        errors++;
    }
    if (object_name(clone2) in resets)
    {
        msg("H_RESET was called for clone#2!\n");
        errors++;
    }
    if (!("/base" in cleanups))
    {
        msg("H_CLEAN_UP was not called for /base!\n");
        errors++;
    }
    else if (cleanups["/base",1] != 3)
    {
        msg("H_CLEAN_UP refcount for /base (%d) != 3!\n", cleanups["/base",1]);
        errors++;
    }
    if (!("/ob1" in cleanups))
    {
        msg("H_CLEAN_UP was not called for /ob1!\n");
        errors++;
    }
    else if (cleanups["/ob1",1] != 1)
    {
        msg("H_CLEAN_UP refcount for /ob1 (%d) != 1!\n", cleanups["/ob1",1]);
        errors++;
    }
    if (!("/ob2" in cleanups))
    {
        msg("H_CLEAN_UP was not called for /ob2!\n");
        errors++;
    }
    else if (cleanups["/ob2",1] != 1)
    {
        msg("H_CLEAN_UP refcount for /ob2 (%d) != 1!\n", cleanups["/ob2",1]);
        errors++;
    }
    if (!("/clone" in cleanups))
    {
        msg("H_CLEAN_UP was not called for /clone!\n");
        errors++;
    }
    else if (cleanups["/clone",1] != 3)
    {
        msg("H_CLEAN_UP refcount for /clone (%d) != 3!\n", cleanups["/clone",1]);
        errors++;
    }
    if (!(object_name(clone1) in cleanups))
    {
        msg("H_CLEAN_UP was not called for /clone#1!\n");
        errors++;
    }
    else if (cleanups[object_name(clone1),1] != 0)
    {
        msg("H_CLEAN_UP refcount for /clone#1 (%d) != 0!\n", cleanups[object_name(clone1),1]);
        errors++;
    }
    if (!(object_name(clone2) in cleanups))
    {
        msg("H_CLEAN_UP was not called for /clone#2!\n");
        errors++;
    }
    else if (cleanups[object_name(clone2),1] != 0)
    {
        msg("H_CLEAN_UP refcount for /clone#2 (%d) != 0!\n", cleanups[object_name(clone2),1]);
        errors++;
    }

    shutdown(errors && 1);
}

string *epilog(int eflag)
{
    configure_driver(DC_RESET_TIME, 1);
    configure_driver(DC_CLEANUP_TIME, 1);

    set_driver_hook(H_RESET, unbound_lambda(0, ({#'m_add, resets, ({#'object_name}), ({#'time}) }) ));
    set_driver_hook(H_CLEAN_UP, unbound_lambda(({'refs}), ({#',, ({#'m_add, cleanups, ({#'object_name}), ({#'time}), 'refs }), 0 }) ));

    call_coroutine(run_test());
    return 0;
}
