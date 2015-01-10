#include "/inc/base.inc"
#include "/inc/gc.inc"

void run_test()
{
    object ob;

    msg("\nRunning test for #0000837:\n"
          "--------------------------\n");

    ob = load_object("/stest");
    destruct(ob);
    ob = load_object("/stest");
    // if this did not crash, it is a success
    msg("Success.\n");
    start_gc(#'shutdown);
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
