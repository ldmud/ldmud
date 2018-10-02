#pragma save_types, rtt_checks
#include "/inc/base.inc"
#include "/inc/client.inc"

/* This test tries to crash the driver when sending a
 * command to a player object that was just destructed.
 * When the driver actually executes that command,
 * this will result in a function call to a destructed
 * object which is not supported.
 */

void client_fun()
{
    /* The command to send. */
    write("XXXXXTest\n");
}

void server_fun()
{
    /* This is the player object, we destruct ourself.
     * And we schedule the shutdown of the driver.
     */
    __MASTER_OBJECT__->finish();
    destruct(this_object());
}

void finish()
{
    call_out(#'shutdown, 0, 0);
}

void run_test()
{
    msg("\nRunning test for destructed objects:\n"
          "------------------------------------\n");

    connect_self("server_fun", "client_fun");
}

string *epilog(int eflag)
{
    /* The H_SEND_NOTIFY_FAIL hook will result in a
     * function call to a destructed object.
     */
    set_driver_hook(H_CREATE_CLONE, "create");
    set_driver_hook(H_SEND_NOTIFY_FAIL, "fail");

    run_test();
    return 0;
}

void create()
{
    /* When we are handling a new connection, let's start a TLS connection.
     * The test works also without, but this introduces a second codepath
     * to a destructed player object.
     */
#ifdef __TLS__
    if (this_interactive() && tls_available())
        tls_init_connection(previous_object());
#endif
}
