/* Check that assignments are handled well.
 */

#pragma strong_types, save_types, rtt_checks

#include "/inc/deep_eq.inc"
#include "/inc/msg.inc"
#include "/inc/testarray.inc"

<int|float> g;
nosave mixed *tests = ({
    ({ "Initialization of a local variable with the correct type", 0,
        (:
            int i = funcall(10);
            return i;
        :),
    }),
    ({ "Initialization of a local variable with the wrong type", TF_ERROR,
        (:
            int i = funcall("10");
            return 0;
        :),
    }),
    ({ "Assignment to a local variable with the correct type", 0,
        (:
            int i;
            i = funcall(10);
            return i;
        :),
    }),
    ({ "Assignment to a local variable with the wrong type", TF_ERROR,
        (:
            int i;
            i = funcall("10");
            return 0;
        :),
    }),
    ({ "Assignment to a global variable with the correct type", 0,
        (:
            g = funcall(10);
            return g;
        :),
    }),
    ({ "Assignment to a global variable with the wrong type", TF_ERROR,
        (:
            g = funcall("10");
            return 0;
        :),
    }),
    ({ "Checking that the last assigmnent didn't take place.", 0,
        (:
            return g == 10;
        :),
    }),
    ({ "Addition to a local variable with the correct type 1", 0,
        (:
            int i = 2;
            i += funcall(10);
            return i == 12;
        :),
    }),
    ({ "Addition to a local variable with the correct type 2", 0,
        (:
            float i = 2.0;
            i += funcall(10);
            return i > 11.999 && i < 12.001;
        :),
    }),
    ({ "Addition to a local variable with the wrong type 1", TF_ERROR,
        (:
            int i = 2;
            i += funcall("10");
            return 0;
        :),
    }),
    ({ "Addition to a local variable with the wrong type 2", TF_ERROR,
        (:
            int* i = ({ 1 });
            i += funcall( ({ "2" }) );
            return 0;
        :),
    }),
    ({ "Addition to a global variable with the correct type", 0,
        (:
            g = 2;
            g += funcall(10);
            return g == 12;
        :),
    }),
    ({ "Addition to a global variable with the wrong type", TF_ERROR,
        (:
            g += funcall("10");
            return 0;
        :),
    }),
    ({ "Checking that the last addition didn't take place.", 0,
        (:
            return g == 12;
        :),
    }),
    ({ "Subtraction from a local variable with the correct type", 0,
        (:
            int i = 42;
            i -= funcall(10);
            return i == 32;
        :),
    }),
    ({ "Subtraction from a local variable with the wrong type", TF_ERROR,
        (:
            int i = 42;
            i -= funcall(1.5);
            return 0;
        :),
    }),
    ({ "Multiplication of a local variable with the correct type", 0,
        (:
            int i = 42;
            i *= funcall(10);
            return i == 420;
        :),
    }),
    ({ "Multiplication of a local variable with the wrong type", TF_ERROR,
        (:
            int i = 42;
            i *= funcall(1.5);
            return 0;
        :),
    }),
    ({ "Division of a local variable with the correct type", 0,
        (:
            int i = 40;
            i /= funcall(10);
            return i == 4;
        :),
    }),
    ({ "Division of a local variable with the wrong type", TF_ERROR,
        (:
            int i = 42;
            i /= funcall(1.5);
            return 0;
        :),
    }),
    ({ "Assignment with logical AND to a local variable with the correct type", 0,
        (:
            int i = 1;
            i &&= funcall(10);
            return i == 10;
        :),
    }),
    ({ "Assignment with logical AND to a local variable with the wrong type", TF_ERROR,
        (:
            int i = 1;
            i &&= funcall(1.5);
            return 0;
        :),
    }),
    ({ "Assignment with logical AND to a global variable with the correct type", 0,
        (:
            g = 1;
            g &&= funcall(10);
            return g == 10;
        :),
    }),
    ({ "Assignment with logical AND to a global variable with the wrong type", TF_ERROR,
        (:
            g = 1;
            g &&= funcall("10");
            return 0;
        :),
    }),
    ({ "Checking that the last assignment didn't take place.", 0,
        (:
            return g == 1;
        :),
    }),
    ({ "Assignment with logical OR to a local variable with the correct type", 0,
        (:
            int i = 0;
            i ||= funcall(10);
            return i == 10;
        :),
    }),
    ({ "Assignment with logical OR to a local variable with the wrong type", TF_ERROR,
        (:
            int i = 0;
            i ||= funcall(1.5);
            return 0;
        :),
    }),
    ({ "Assignment with logical OR to a global variable with the correct type", 0,
        (:
            g = 0;
            g ||= funcall(10);
            return g == 10;
        :),
    }),
    ({ "Assignment with logical OR to a global variable with the wrong type", TF_ERROR,
        (:
            g = 0;
            g ||= funcall("10");
            return 0;
        :),
    }),
    ({ "Checking that the last assignment didn't take place.", 0,
        (:
            return g == 0;
        :),
    }),
    ({ "Assignment with binary OR to a local variable with the correct type", 0,
        (:
            int* i = ({ 10 });
            i |= funcall(({ 20 }));
            return deep_eq(i, ({10,20}));
        :),
    }),
    ({ "Assignment with binary OR to a local variable with the wrong type", TF_ERROR,
        (:
            int* i = ({ 10 });
            i |= funcall(({ 1.5 }));
            return 0;
        :),
    }),
    ({ "Assignment with binary XOR to a local variable with the correct type", 0,
        (:
            int* i = ({ 10, 20 });
            i ^= funcall(({ 20, 30 }));
            return deep_eq(i, ({10, 30}));
        :),
    }),
    ({ "Assignment with binary XOR to a local variable with the wrong type", TF_ERROR,
        (:
            int* i = ({ 10, 20 });
            i ^= funcall(({ 20, 2.5 }));
            return 0;
        :),
    }),
});

int run_test()
{
    msg("\nRunning RTTC assignment tests:\n"
          "------------------------------\n");
    return !run_array_without_callback(tests);
}
