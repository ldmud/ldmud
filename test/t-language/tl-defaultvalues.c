/* Test default values for functions.
 */

inherit "/ti-defaultvalues";

#include "/inc/msg.inc"
#include "/inc/testarray.inc"

#pragma strong_types, rtt_checks

int fun2(int a, int b = 2*a, int c = 100);

int fun1(int a, int b = a, int c = 42)
{
    return a + b + c;
}

int fun2(int a, int b, int c)
{
    return a + b + c;
}

int fun3(int a, int b = 3*a, int c = 200, varargs int* d)
{
    int sum = a + b + c;
    foreach(int i: d)
        sum += i;
    return sum;
}

int fun4(int a, closure cl = function int() : int b = 2+10*a { return b;})
{
    return a + funcall(cl);
}

// Overloading a nomask prototype should be allowed.
nomask int iprototype(int a, int b = 100) { return a+b; }

int run_test()
{
    closure cl1 = function int(int a, int b = a, int c = 42) { return a + b + c; };
    closure cl2 = function int(int a, int b = 3*a, int c = 200, varargs int* d)
    {
        int sum = a + b + c;
        foreach(int i: d)
            sum += i;
        return sum;
    };
    closure cl3 = function int(int a, closure cl = function int() : int b = 2+10*a { return b;})
    {
        return a + funcall(cl);
    };

    msg("\nRunning default value tests:\n"
          "----------------------------\n");

    return !run_array_without_callback(({
        ({ "Regular function with complete arguments", 0,
            (:
                return fun1(10,21,53) == 84;
            :)
        }),
        ({ "Regular function with one missing argument", 0,
            (:
                return fun1(10,21) == 73;
            :)
        }),
        ({ "Regular function with two missing arguments", 0,
            (:
                return fun1(10) == 62;
            :)
        }),
        ({ "Regular function with prototype and complete arguments", 0,
            (:
                return fun2(10,21,53) == 84;
            :)
        }),
        ({ "Regular function with prototype and one missing argument", 0,
            (:
                return fun2(10,21) == 131;
            :)
        }),
        ({ "Regular function with prototype and two missing arguments", 0,
            (:
                return fun2(10) == 130;
            :)
        }),
        ({ "Varargs function with two additional arguments", 0,
            (:
                return fun3(1,2,5,100,200) == 308;
            :)
        }),
        ({ "Varargs function with one additional argument", 0,
            (:
                return fun3(1,2,5,100) == 108;
            :)
        }),
        ({ "Varargs function with complete arguments", 0,
            (:
                return fun3(1,2,5) == 8;
            :)
        }),
        ({ "Varargs function with one missing argument", 0,
            (:
                return fun3(1,2) == 203;
            :)
        }),
        ({ "Varargs function with two missing arguments", 0,
            (:
                return fun3(1) == 204;
            :)
        }),
        ({ "Function with inline closure", 0,
            (:
                return fun4(13) == 145;
            :)
        }),
        ({ "Inherited function with complete arguments", 0,
            (:
                return ifun(3,9,81) == 93;
            :)
        }),
        ({ "Inherited function with one missing argument", 0,
            (:
                return ifun(3,9) == 2012;
            :)
        }),
        ({ "Inherited function with two missing arguments", 0,
            (:
                return ifun(3) == 2303;
            :)
        }),

        ({ "Regular inline closure with complete arguments", 0,
            (:
                return funcall(cl1,10,21,53) == 84;
            :)
        }),
        ({ "Regular inline closure with one missing argument", 0,
            (:
                return funcall(cl1,10,21) == 73;
            :)
        }),
        ({ "Regular inline closure with two missing arguments", 0,
            (:
                return funcall(cl1,10) == 62;
            :)
        }),
        ({ "Regular inline closure with three missing arguments", 0,
            (:
                return funcall(cl1) == 42;
            :)
        }),
        ({ "Varargs inline closure with two additional arguments", 0,
            (:
                return funcall(cl2,1,2,5,100,200) == 308;
            :)
        }),
        ({ "Varargs inline closure with one additional argument", 0,
            (:
                return funcall(cl2,1,2,5,100) == 108;
            :)
        }),
        ({ "Varargs inline closure with complete arguments", 0,
            (:
                return funcall(cl2,1,2,5) == 8;
            :)
        }),
        ({ "Varargs inline closure with one missing argument", 0,
            (:
                return funcall(cl2,1,2) == 203;
            :)
        }),
        ({ "Varargs inline closure with two missing arguments", 0,
            (:
                return funcall(cl2,1) == 204;
            :)
        }),
        ({ "Varargs inline closure with three missing arguments", 0,
            (:
                return funcall(cl2) == 200;
            :)
        }),
        ({ "inline closure with inline closure", 0,
            (:
                return funcall(cl3,13) == 145;
            :)
        }),

        ({ "Lambda: Regular function with complete arguments", 0,
            lambda(0, ({#'==, ({#'fun1,10,21,53}), 84 })),
        }),
        ({ "Lambda: Regular function with one missing argument", 0,
            lambda(0, ({#'==, ({#'fun1,10,21}), 73 })),
        }),
        ({ "Lambda: Regular function with two missing arguments", 0,
            lambda(0, ({#'==, ({#'fun1,10}), 62 })),
        }),
        ({ "Lambda: Regular function with prototype and complete arguments", 0,
            lambda(0, ({#'==, ({#'fun2,10,21,53}), 84 })),
        }),
        ({ "Lambda: Regular function with prototype and one missing argument", 0,
            lambda(0, ({#'==, ({#'fun2,10,21}), 131 })),
        }),
        ({ "Lambda: Regular function with prototype and two missing arguments", 0,
            lambda(0, ({#'==, ({#'fun2,10}), 130 })),
        }),
        ({ "Lambda: Varargs function with two additional arguments", 0,
            lambda(0, ({#'==, ({#'fun3,1,2,5,100,200}), 308 })),
        }),
        ({ "Lambda: Varargs function with one additional argument", 0,
            lambda(0, ({#'==, ({#'fun3,1,2,5,100}), 108 })),
        }),
        ({ "Lambda: Varargs function with complete arguments", 0,
            lambda(0, ({#'==, ({#'fun3,1,2,5}), 8 })),
        }),
        ({ "Lambda: Varargs function with one missing argument", 0,
            lambda(0, ({#'==, ({#'fun3,1,2}), 203 })),
        }),
        ({ "Lambda: Varargs function with two missing arguments", 0,
            lambda(0, ({#'==, ({#'fun3,1}), 204 })),
        }),
        ({ "Lambda: Function with inline closure", 0,
            lambda(0, ({#'==, ({#'fun4,13}), 145 })),
        }),
        ({ "Lambda: Inherited function with complete arguments", 0,
            lambda(0, ({#'==, ({#'ifun,3,9,81}), 93 })),
        }),
        ({ "Lambda: Inherited function with one missing argument", 0,
            lambda(0, ({#'==, ({#'ifun,3,9}), 2012 })),
        }),
        ({ "Lambda: Inherited function with two missing arguments", 0,
            lambda(0, ({#'==, ({#'ifun,3}), 2303 })),
        }),
    }));
}
