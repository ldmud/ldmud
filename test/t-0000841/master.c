#include "/inc/base.inc"

/* closure_crash constructs a closure with a context variable
 * in the global scope. The inline closure compiler tries to
 * inform the outer scope about possibly having to initialize
 * following local variables again (bug #537). In the global
 * scope there is no further outer scope with local variables,
 * leading to a write access beyond the scope structure.
 */
void run_test()
{
    object ob;

    msg("\nRunning test for #0000841:\n"
          "--------------------------\n");

    ob = load_object("closure_crash");
    ob->reset();
    shutdown(0);

    return 0;
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
