/* Testscenario 030925
 *
 * Shadowing a public sefun with a private lfun, which was then virtually
 * inherited twice in parallel, used to crash the driver, because        
 * the public sefun inhibited a proper cross-definition of the second       
 * lfun instance. The problem was in the either-or assignment of an         
 * identifier to either be an lfun or a (s)efun/global var.
 */

#include "/inc/base.inc"
#include "/inc/gc.inc"
#include "/inc/sefun.inc"

void run_test()
{
    msg("\nRunning test for t-030925:\n"
          "--------------------------\n");

    load_object("/d");
    start_gc(#'shutdown);
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
