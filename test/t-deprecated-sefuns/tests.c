#include "/inc/msg.inc"
#include "/inc/testarray.inc"
#include "/inc/deep_eq.inc"

mixed *tests = ({
    ({ "set_heart_beat", 0,
        (:
            if (!set_heart_beat(1))
                return 0;
            if (!set_heart_beat(0))
                return 0;
            if (set_heart_beat(0))
                return 0;
            return 1;
        :)
    }),
});

int run_test()
{
    return run_array_without_callback(tests);
}
