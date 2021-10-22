/* Test scenario for #0000898:
 *
 * Test line number argument to log_error().
 */

#include "/inc/base.inc"

int log_error_line;

void log_error(string file, string err, int warn, int line)
{
    log_error_line = line;
}

int get_log_error_line() { return log_error_line; }

void run_test()
{
    msg("\nRunning test for t-0000898:\n"
          "--------------------------\n");

    catch(load_object("a"));

    if (log_error_line != 5)
    {
        msg("Wrong reported error line: %d, expected 5.\n", log_error_line);
        shutdown(1);
    }
    else
        shutdown(0);
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
