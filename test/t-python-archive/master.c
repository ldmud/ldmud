#include "/inc/base.inc"

void run_test()
{
#ifdef __PYTHON__
    msg("\nRunning test for Python startup archive:\n"
          "----------------------------------------\n");

#if __EFUN_DEFINED__(startup_called)
    shutdown(0);
#else
    shutdown(1);
#endif
#else
    shutdown(0);
#endif
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
