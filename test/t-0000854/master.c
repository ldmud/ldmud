#include "/inc/base.inc"

/* We create an object that will contain all sorts of variables.
 * Then we'll swap it out and back in and check for correctness.
 */
void run_test()
{
    object ob;

    msg("\nRunning test for #0000854:\n"
          "--------------------------\n");

#if __EFUN_DEFINED__(swap)
    ob = load_object("swap_ob");
    swap(ob);
    shutdown(!ob->check());

#else
    msg("swap() efun not available, skipping test.");
    shutdown(0);
#endif

    return 0;
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
