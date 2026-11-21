/* Regression tests for inline closures (CLOSURE_LFUN) in driver hooks
 * and copy-on-rebind behavior in bind_lambda().
 */
#pragma save_types, rtt_checks, clone

#include "/inc/base.inc"
#include "/inc/testarray.inc"
#include "/inc/deep_eq.inc"
#include "/inc/gc.inc"
#include "/sys/driver_hook.h"

mixed *move_seen;

int get_val()
{
    return 42;
}

mixed *tests = (this_object() == blueprint()) && ({
    ({ "bind_lambda on shared lfun closure (ref > 1)", 0,
        (:
            object other = clone_object(this_object());
            closure cl1 = #'get_val;
            closure cl2 = bind_lambda(cl1, other);
            int ok = (to_object(cl1) == this_object())
                  && (to_object(cl2) == other)
                  && (cl1 != cl2)
                  && (funcall(cl1) == 42)
                  && (funcall(cl2) == 42);
            destruct(other);
            return ok;
        :)
    }),
    ({ "bind_lambda on shared inline closure with context (ref > 1)", 0,
        (:
            object other = clone_object(this_object());
            string tag = "ctx";
            int n = 7;
            closure cl1 = function mixed*() { return ({ tag, n }); };
            closure cl2 = bind_lambda(cl1, other);
            int ok = (to_object(cl1) == this_object())
                  && (to_object(cl2) == other)
                  && (cl1 != cl2)
                  && deep_eq(funcall(cl1), ({ "ctx", 7 }))
                  && deep_eq(funcall(cl2), ({ "ctx", 7 }));
            destruct(other);
            return ok;
        :)
    }),
    ({ "bind_lambda on single-ref inline closure (ref == 1)", 0,
        (:
            object other = clone_object(this_object());
            closure cl = bind_lambda(function int() { return 99; }, other);
            int ok = (to_object(cl) == other) && (funcall(cl) == 99);
            destruct(other);
            return ok;
        :)
    }),
    ({ "set_driver_hook H_MOVE_OBJECT0 with inline closure", 0,
        (:
            object item = clone_object(this_object());
            object dest = clone_object(this_object());
            string tag = "move0";
            move_seen = ({});

            set_driver_hook(H_MOVE_OBJECT1, 0);
            set_driver_hook(H_MOVE_OBJECT0,
                function void(object ob, object to)
                {
                    move_seen = ({ tag, ob, to });
                    set_environment(ob, to);
                });

            move_object(item, dest);
            set_driver_hook(H_MOVE_OBJECT0, 0);

            int ok = deep_eq(move_seen, ({ "move0", item, dest }))
                  && (environment(item) == dest);
            move_seen = 0;
            destruct(item);
            destruct(dest);
            return ok;
        :)
    }),
    ({ "set_driver_hook H_MOVE_OBJECT1 with inline closure", 0,
        (:
            object item = clone_object(this_object());
            object dest = clone_object(this_object());
            string tag = "move1";
            move_seen = ({});

            set_driver_hook(H_MOVE_OBJECT0, 0);
            set_driver_hook(H_MOVE_OBJECT1,
                function void(object ob, object to)
                {
                    move_seen = ({ tag, ob, to });
                    set_environment(ob, to);
                });

            move_object(item, dest);
            set_driver_hook(H_MOVE_OBJECT1, 0);

            int ok = deep_eq(move_seen, ({ "move1", item, dest }))
                  && (environment(item) == dest);
            move_seen = 0;
            destruct(item);
            destruct(dest);
            return ok;
        :)
    }),
});

void run_test()
{
    msg("\nRunning tests for inline closures in bind_lambda and driver hooks:\n"
          "------------------------------------------------------------------\n");

    run_array(tests,
        (:
            if ($1)
                shutdown(1);
            else
                start_gc(#'shutdown);

            return 0;
        :));
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
