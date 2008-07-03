/* Testscenario 041124
 *
 * Test lookup issues with inherited function closures.
 */

#include "/inc/base.inc"
#include "/inc/gc.inc"

void run_test()
{
    msg("\nRunning test for t-041124:\n"
          "--------------------------\n");

    object a=load_object("a");
    object c=load_object("c");
    if(c->run_test())
	shutdown(1);
    else
	start_gc(#'shutdown);
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
