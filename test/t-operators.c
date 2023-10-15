#include "/inc/base.inc"
#include "/inc/testarray.inc"
#include "/inc/gc.inc"
#include "/inc/deep_eq.inc"

mixed *tests = ({
    ({ "__INT_MIN__ % -1", 0,        (: __INT_MIN__ % -1 == 0 :) }),
    ({ "__INT_MIN__ / -1", TF_ERROR, (: __INT_MIN__ / -1 == 0 :) }),
    ({ "__INT_MIN__ * -1", TF_ERROR, (: __INT_MIN__ * -1 == 0 :) }),
});

void run_test()
{
    msg("\nRunning test suite for operators:\n"
          "---------------------------------\n");

    run_array(tests,
        (:
            if($1)
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
