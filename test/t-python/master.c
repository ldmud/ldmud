#include "/inc/base.inc"
#include "/inc/testarray.inc"
#include "/inc/gc.inc"

struct test_struct
{
    int    t_int;
    float  t_float;
    string t_string;
    object t_object;
};

void run_test()
{
    object ob;

    msg("\nRunning tests for pkg_python:\n"
          "-----------------------------\n");

    run_array(({
        ({ "passing int", 0,
            (:
                return python_return(0) == 0 &&
                       python_return(-1) == -1 &&
                       python_return(__INT_MAX__) == __INT_MAX__ &&
                       python_return(__INT_MIN__) == __INT_MIN__;
            :)
        }),
        ({ "passing float", 0,
            (:
                return python_return(0.0) == 0.0 &&
                       python_return(-1.1) == -1.1 &&
                       python_return(__FLOAT_MAX__) == __FLOAT_MAX__ &&
                       python_return(__FLOAT_MIN__) == __FLOAT_MIN__ &&
                       python_return(-__FLOAT_MAX__) == -__FLOAT_MAX__ &&
                       python_return(-__FLOAT_MIN__) == -__FLOAT_MIN__;
            :)
        }),
        ({ "passing string", 0,
            (:
                return python_return("") == "" &&
                       python_return("Hi") == "Hi" &&
                       python_return("\0") == "\0" &&
                       python_return("42\xe2\x82\xac") == "42\xe2\x82\xac"; // UTF-8 encoding allowed
            :)
        }),
        ({ "passing objects", 0,
            (:
                return python_return(this_object()) == this_object();
            :)
        }),
        ({ "passing structs", 0,
            (:
                struct test_struct s = (<test_struct> 123);
                return python_return(s) == s; // Shall preserve identity
            :)
        }),
#if 0 /* Lvalues are not supported */
        ({ "passing lvalue", 0,
            (:
                int x = 70550;
                mixed* result = ({ python_return(&x) });
                result[0] = 66606;
                return x == 66606;
            :)
        }),
#endif
        ({ "passing too many arguments", TF_ERROR,
            (:
                return python_return(1,2);
            :)
        }),
        ({ "passing less arguments", TF_ERROR,
            (:
                return python_return();
            :)
        }),
        ({ "Python test suite", 0,
            (:
                msg("\n");
                return python_test();
            :)
        }),

    }), //#'shutdown);
    (:
        if($1)
            shutdown(1);
        else
        {
            python_set((<test_struct> 705948522));
            start_gc(function void(int result)
            {
                mixed val = python_get();
                if(!structp(val) || val->t_int != 705948522)
                {
                    msg("Wrong value returned from python_get() after GC.!\n");
                    shutdown(1);
                }

                python_set(0);

                start_gc(#'shutdown);
            });
        }
        return 0;
    :));
}

int master_fun() { return 54321; }

string *epilog(int eflag)
{
    run_test();
    return 0;
}
