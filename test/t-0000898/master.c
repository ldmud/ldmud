/* Testscenario 0000898
 *
 * Test line number argument to log_error.
 */

#include "/inc/base.inc"
#include "/inc/gc.inc"

int log_error_line;

void log_error(string file, string err, int warn, int line) {
  log_error_line = line;
}

int get_log_error_line() { return log_error_line; }

void run_test()
{
    msg("\nRunning test for t-0000898:\n"
          "--------------------------\n");

    catch(load_object("a"));

    int error_line = __MASTER_OBJECT__.get_log_error_line();

    if(error_line == 5)
      start_gc(#'shutdown);
    else
      shutdown(1);
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
