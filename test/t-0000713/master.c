#include "/inc/base.inc"
#include "/inc/gc.inc"

void run_test()
{
    object ob;

    msg("\nRunning test for #0000713:\n"
          "--------------------------\n");

    ob = load_object("/action");
    ob->start();
    destruct(ob);
    start_gc(#'shutdown);
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
