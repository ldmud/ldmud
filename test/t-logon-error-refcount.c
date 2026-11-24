#pragma save_types, rtt_checks
#include "/inc/base.inc"
#include "/inc/client.inc"

#include "/sys/configuration.h"

/* An uncaught error thrown from logon() must not leave a stale
 * reference to the player object behind.
 *
 * The object returned by master::connect() is kept in the driver's
 * static apply result until the next apply completes. When logon()
 * throws, the error recovery returns straight to the backend without
 * storing a new result, and clear_state() has to release the old one.
 * If it does not, the --check-refcounts pass reports the object with
 * one reference too many ("Bad ref count") on every backend cycle.
 */

int checks;

void run_server()
{
    raise_error("boom\n");
}

void run_client()
{
    /* Nothing to do. */
}

void check_log()
{
    string log = read_file(driver_info(DC_DEBUG_FILE));

    if (stringp(log) && strstr(log, "Bad ref count") >= 0)
    {
        msg("FAILURE: The error from logon() left a stale reference.\n");
        shutdown(1);
        return;
    }

    /* Give the refcount check some backend cycles to run. */
    if (++checks > 3)
    {
        msg("Success.\n");
        shutdown(0);
        return;
    }

    call_out("check_log", __ALARM_TIME__);
}

void run_test()
{
    msg("\nRunning test for an uncaught error in logon():\n"
          "----------------------------------------------\n");

    connect_self("run_server", "run_client");
    call_out("check_log", 2 * __ALARM_TIME__);

    /* Safety net in case the check never finishes. */
    call_out(#'shutdown, 20 * __ALARM_TIME__, 1);
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
