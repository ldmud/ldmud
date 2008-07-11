#include "/inc/base.inc"
#include "/inc/gc.inc"

#include "/sys/input_to.h"
#include "/sys/rtlimits.h"

/* These functions are for the clone (the player object). */
void start_client()
{
    net_connect("127.0.0.1", query_mud_port());
}

int logon(int flag)
{
    set_prompt("");
    return 1;
}

/* These functions are for the blueprint (the virtual player that
   sends us the commands). */

/* We make two steps, one without and one with catch().
 * The later might abort the thread, so we have to start
 * a callout to shutdown with an error.
 */

int step = 0;

object connect()
{
    set_prompt("");
    
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
        this_object()->run_test2();
    }
    else
        start_gc(#'shutdown);

    return 0;
}

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
    set_limits(LIMIT_EVAL, 200000);
    
    /* Waiting until LDMud is ready for users. */
    call_out("run_test2", 0);
}

void run_test2()
{
    object dummy;
    dummy = clone_object(this_object());
    dummy->start_client();
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
