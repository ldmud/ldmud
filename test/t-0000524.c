#include "/sys/regexp.h"
#include "/inc/base.inc"
#include "/inc/gc.inc"

// tries to regmatch some big expression, which causes the PCRE engine to
// recurse tens of thousands of times. If the recursion is not limited
// properly in respect to the stack size limit of the OS, this will crash the
// driver.
// test is implemented separately because it is supposed to crash the driver
// if it is not successful.
void try_crash()
{ 
    // use at least some of the available stack.
    if (caller_stack_depth() < __MAX_RECURSION__-10) {
        this_object()->try_crash();
        return;
    }

    catch(regmatch("aaa bbb ccc"*1000000,"a{1,2}(\n|.)+cc",
            RE_PCRE|RE_MULTILINE|RE_MATCH_SUBS,0);nolog);
    // actually, if the driver survives, the test is successful, no matter if
    // the driver was able to match it correctly (because it got plenty of
    // stack space from the OS) or raised an error.
    msg("Success\n");
    shutdown(0);
}

void run_test()
{
    msg("\nRunning test for #0000524...\n"
          "----------------------------\n");
    try_crash();
}

string *epilog(int eflag)
{
    call_out(#'run_test,0);
    return 0;
}

