#include "/inc/base.inc"

void run_test()
{
    object ob;

    msg("\nRunning test for #0000772:\n"
          "--------------------------\n");

    msg("Checking for __INIT... ");
    set_driver_hook(H_CREATE_OB, "create");
    ob = load_object("init");

    if(ob)
    {
        msg("Object still alive!\n");
        shutdown(1);
        return;
    }
    else
        msg("Success.\n");

    msg("Checking for process_string... ");
    ob = load_object("process_string");
    ob->do_process_string();

    if(ob)
    {
        msg("Object still alive!\n");
        shutdown(1);
        return;
    }
    else
        msg("Success.\n");

    shutdown(0);
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
