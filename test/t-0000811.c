#include "/inc/base.inc"

int test()
{
    int a = __FUNCTION__ == "test";
    int b = funcall(function() { return __FUNCTION__ == "test"; });
    int c = funcall((: __FUNCTION__ == "test" :));

    return a && b && c;
}

void run_test()
{
    msg("\nRunning test for #0000811:\n"
          "--------------------------\n");
    if (test()) {
        msg("Success\n");
    } else {
        msg("Failure\n");
    }
    shutdown(0);
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
