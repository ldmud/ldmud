#include "/inc/base.inc"
#include "/inc/gc.inc"
#include "/inc/client.inc"

#include "/sys/input_to.h"
#include "/sys/rtlimits.h"

/* We make two steps, one without and one with catch().
 * The later might abort the thread, so we have to start
 * a callout to shutdown with an error.
 */

void run_server(int step)
{
    configure_interactive(this_object(), IC_TELNET_ENABLED, 1);

    while(get_eval_cost() > 100000);
    
    call_out(#'shutdown, 0, 1);
    if(step)
        catch(efun::input_to("bla",INPUT_NOECHO); reserve 50000);
    else
        efun::input_to("bla",INPUT_NOECHO);
    remove_call_out(#'shutdown);
    
    if(get_eval_cost() > 100000)
    {
        msg("Got my eval ticks back: FAILURE.\n");
        shutdown(1);
    }
    else if(!step)
    {
        step=1;
        __MASTER_OBJECT__->connect_self("run_server2", 0);
    }
    else
        start_gc(#'shutdown);

    return 0;
}

void run_server1() { run_server(0); }
void run_server2() { run_server(1); }

void run_test()
{
    msg("\nRunning test for #0000534:\n"
          "--------------------------\n");

    set_driver_hook(H_NOECHO,
        function void(int flag, object ob)
        {
            if(previous_object())
                raise_error("Intentional error in H_NOECHO hook.\n");
        });

    configure_driver(DC_DEFAULT_RUNTIME_LIMITS, ({LIMIT_KEEP}) * LIMIT_EVAL + ({200000}));
    connect_self("run_server1", 0);
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
