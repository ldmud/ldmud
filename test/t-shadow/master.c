#include "/inc/base.inc"
#include "/inc/testarray.inc"

#include "/sys/configuration.h"

int query_allow_shadow(object victim)
{
    return 1;
}

void run_test()
{
    msg("\nRunning tests for shadows:\n"
          "--------------------------\n");

    efun::configure_object(this_object(), OC_COMMANDS_ENABLED, 1);
    efun::set_this_player(this_object());

    run_array(({
        ({ "Action from shadows with function name", 0,
            (:
                object sh = clone_object("/action_string_sh");

                // Add shadow with an action
                if (!sh.start(this_object()))
                    return 0;

                // Check that the action is there.
                if (!query_actions(this_object(), "shadowstrtest"))
                    return 0;

                // Put another shadow on top.
                clone_object("/dummy_sh").start(this_object());

                // And remove the shadow with the action.
                sh.stop();
                destruct(sh);

                // Check that the action is not there anymore.
                if (query_actions(this_object(), "shadowstrtest"))
                    return 0;

                return 1;
            :)
        }),
        ({ "Action from shadows with closures", 0,
            (:
                object sh = clone_object("/action_closure_sh");

                // Add shadow with an action
                if (!sh.start(this_object()))
                    return 0;

                // Check that the action is there.
                if (!query_actions(this_object(), "shadowcltest"))
                    return 0;

                // Put another shadow on top.
                clone_object("/dummy_sh").start(this_object());

                // And remove the shadow with the action.
                sh.stop();
                destruct(sh);

                // Check that the action is not there anymore.
                if (query_actions(this_object(), "shadowcltest"))
                    return 0;

                return 1;
            :)
        }),
    }), #'shutdown);
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
