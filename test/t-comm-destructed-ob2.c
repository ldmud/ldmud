#pragma save_types, rtt_checks
#include "/inc/base.inc"
#include "/inc/client.inc"
#include "/sys/telnet.h"

/* This test tries to crash the driver when sending a
 * command to a player object that was just destructed.
 * When the driver actually executes that command,
 * this will result in a function call to a destructed
 * object which is not supported.
 */

void client_fun()
{
    /* We send some telnet negotations that result
     * in the player object being destructed.
     */
    binary_message(({IAC,DO,TELOPT_TM,IAC,WILL,TELOPT_TM,'H','a','l','l','o','\r','\n'}));
}

void server_fun()
{
    /* This is the player object, we activate the telnet machine.
     * And we schedule the shutdown of the driver.
     */
    __MASTER_OBJECT__->finish();

    configure_interactive(this_object(), IC_TELNET_ENABLED, 1);
}

void telnet()
{
    /* This is only called in the player object,
     * because only there telnet is active.
     */
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
    set_driver_hook(H_SEND_NOTIFY_FAIL, "fail");
    set_driver_hook(H_TELNET_NEG, "telnet");

    run_test();
    return 0;
}
