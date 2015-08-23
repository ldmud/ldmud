#include "/inc/base.inc"
#include "/inc/gc.inc"
#include "/inc/client.inc"

/* These functions are for the blueprint (the virtual player that
   sends us the commands). */
void run_server()
{
    catch(efun::input_to("bla",4,({1}),({2})));
    start_gc(#'shutdown);

    return 0;
}

void run_test()
{
    msg("\nRunning test for #0000486:\n"
          "--------------------------\n");

    connect_self("run_server", 0);
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
