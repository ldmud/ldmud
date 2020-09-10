#include "/inc/deep_eq.inc"
#include "/inc/msg.inc"
#include "/inc/testarray.inc"

mixed* fun(string arg, int num)
{
    return ({arg, num});
}

void run_test()
{
    msg("\nRunning test for simul-efuns:\n"
          "-----------------------------\n");

    run_array(({
        ({ "Calling sefun0000", 0,
            (:
                return sefun0000() == 0;
            :)
        }),
        ({ "Calling sefunFFF0", 0,
            (:
                return sefunFFF0() == 0xfff0;
            :)
        }),
        ({ "Calling increment()", 0,
            (:
                return increment(100) == 102;
            :)
        }),
        ({ "Calling simul-efun call_strict", 0,
            (:
                object ob = this_object();
                return deep_eq(ob.fun(10), ({"sefun", 10}));
            :)
        }),
        ({ "Calling simul-efun call_other", 0,
            (:
                object ob = this_object();
                return deep_eq(ob->fun(20), ({"sefun", 20}));
            :)
        }),
        ({ "Calling simul-efun with optional arguments", 0,
            (:
                return opt_args_sefun(1, 3) == 531;
            :)
        }),
        ({ "Lambda: Calling simul-efun with optional arguments", 0,
            lambda(0, ({#'==, ({#'opt_args_sefun, 1, 3}), 531})),
        }),
    }), #'shutdown);

}
