/* Check that assignments are handled well.
 */

#pragma strong_types, save_types, rtt_checks

#include "/inc/msg.inc"
#include "/inc/testarray.inc"

<int|float> g;
nosave mixed *tests = ({
    ({ "Initialiation of a local variable with the correct type", 0,
        (:
            int i = funcall((: 10 :));
            return i;
        :),
    }),
    ({ "Initialiation of a local variable with the wrong type", TF_ERROR,
        (:
            int i = funcall((: "10" :));
            return 0;
        :),
    }),
    ({ "Assignment to a local variable with the correct type", 0,
        (:
            int i;
            i = funcall((: 10 :));
            return i;
        :),
    }),
    ({ "Assignment to a local variable with the wrong type", TF_ERROR,
        (:
            int i;
            i = funcall((: "10" :));
            return 0;
        :),
    }),
    ({ "Assignment to a global variable with the correct type", 0,
        (:
            g = funcall((: 10 :));
            return g;
        :),
    }),
    ({ "Assignment to a global variable with the wrong type", TF_ERROR,
        (:
            g = funcall((: "10" :));
            return 0;
        :),
    }),
    ({ "Checking that the last assigmnent didn't take place.", 0,
        (:
            return g == 10;
        :),
    }),
});

int run_test()
{
    msg("\nRunning RTTC assignment tests:\n"
          "------------------------------\n");
    return !run_array_without_callback(tests);
}
