#include "/inc/base.inc"
#include "/inc/gc.inc"

// this test feeds some big string of zeroes into restore_value(). This should
// a) not crash the driver and b) result in an error.
// If OSSTACKLIMIT is not set by the shell script running the tests, choose
// 130MB.
#ifndef OSSTACKLIMIT
#define OSSTACKLIMIT (1024*130)
#elif OSSTACK < 1
#define OSSTACKLIMIT (1024*130)
#endif

void run_test()
{
    msg("\nRunning test for #0000532...\n"
          "----------------------------\n"
          "Test with stack size limit "+OSSTACKLIMIT*1024+": ");
    if (catch(restore_value("0"*(OSSTACKLIMIT*1024));nolog)) {
      // still running and error occurred... excellent.
      msg("Success.\n");
      shutdown(0);
    }
    else {
      // test failure, no error occurred.
      msg("FAILURE! (No error occurred)\n");
      shutdown(1);
    }
}

string *epilog(int eflag)
{
    call_out(#'run_test,0);
    return 0;
}

