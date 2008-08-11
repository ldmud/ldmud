/* Testscenario 040413
 *
 * Test two lookup issues with structs.
 * The object "a" should load without an error.
 */
#include "/inc/base.inc"

void run_test()
{
    msg("\nRunning test for t-040413:\n"
          "--------------------------\n");

    shutdown(catch(load_object("/a"); publish) && 1);
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
