#define OWN_PRIVILEGE_VIOLATION
#include "/inc/base.inc"
#include "/inc/sefun.inc"
#include "/inc/gc.inc"

void run_test()
{
    msg("\nRunning test for #0000548:\n"
          "--------------------------\n");

    "/unpriv"->run_test();
    
}
int privilege_violation(string what, mixed who, mixed where, mixed how)
{
    restore_value("#1:0\n0\n");
    return 1;
}

string *epilog(int eflag)
{
    run_test();
    start_gc(#'shutdown);
    return 0;
}
