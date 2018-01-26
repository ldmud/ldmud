#include "/inc/base.inc"
#include "/inc/testarray.inc"
#include "/inc/gc.inc"

struct test_struct
{
    int     t_int;
    float   t_float;
    string  t_string;
    object  t_object;
    mixed*  t_array;
    mapping t_mapping;
    symbol  t_symbol;
    mixed   t_quoted_array;
};

void run_test()
{
    object ob;

#ifdef __PYTHON__

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
        ({ "passing arrays", 0,
            (:
                mixed * arr = ({1,2,3});
                return python_return(({})) == ({}) &&
                       python_return(arr) == arr;
            :)
        }),
        ({ "passing arrays", 0,
            (:
                mapping m = ([1,2,3]);
                return python_return(m) == m;
            :)
        }),
        ({ "passing structs", 0,
            (:
                struct test_struct s = (<test_struct> 123);
                return python_return(s) == s; // Shall preserve identity
            :)
        }),
        ({ "passing closures", 0,
            (:
                closure cl = lambda(0,0);
                return python_return(#'run_test) == #'run_test &&
                       python_return(#'this_object) == #'this_object &&
                       python_return(cl) == cl &&
                       python_return(#',) == #',;
            :)
        }),
        ({ "passing symbols", 0,
            (:
                return python_return(quote("Hello")) == 'Hello &&
                       python_return('''abc) == '''abc;
            :)
        }),
        ({ "passing quoted arrays", 0,
            (:
                mixed arr = '''({5,15,20});
                return python_return(arr) == arr &&
                       python_return('({})) == '({}) &&
                       python_return(''({})) != '({});
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
        ({ "getting an exception", TF_ERROR,
            (:
                return funcall(#'python_error);
            :)
        }),
        ({
            "Overriding efuns", 0,
            (:
                return funcall(symbol_function("abs"),-10) == -20;
            :)
        }),
        ({
            "Unregistering efuns", 0,
            (:
                unregister_abs();
                return funcall(symbol_function("abs"),-10) == 10;
            :)
        }),
        ({
            "Python object hook 1", 0,
            (:
                object* oblist = python_get_hook_info()[1];
                return sizeof(oblist) == 1 && oblist[0] == this_object();
            :)
        }),
        ({ "Python test suite", 0,
            (:
                msg("\n");
                return python_test();
            :)
        }),
        ({
            "Python object hook 2", 0,
            (:
                object* oblist = python_get_hook_info()[1];
                return sizeof(oblist) == 2 && oblist[0] == this_object();
            :)
        }),

    }), //#'shutdown);
    (:
        if($1)
            shutdown(1);
        else
        {
            python_set((<test_struct> 
                705948522,
                -1000000.0,
                "Garbage",
                this_object(),
                ({ 5, 3, 1}),
                ([2,3,5]),
                quote("abc"+"gc"),
                quote(({11, 13, 17}))
            ));

            start_gc(function void(int result)
            {
                mixed val = python_get();

                if (result)
                {
                    shutdown(result);
                    return;
                }

                if(!structp(val) ||
                    val->t_int != 705948522 ||
                    val->t_float != -1000000.0 ||
                    val->t_string != "Garbage" ||
                    val->t_object != this_object() ||
                    val->t_array[0] != 5 || val->t_array[1] != 3 || val->t_array[2] != 1 ||
                    sizeof(val->t_mapping) != 3 || widthof(val->t_mapping) != 0 ||
                    !member(val->t_mapping, 2) || !member(val->t_mapping, 3) || !member(val->t_mapping, 5) ||
                    unquote(val->t_symbol) != "ab" + "cgc" ||
                    sizeof(unquote(val->t_quoted_array)) != 3
                  )
                {
                    msg("Wrong value returned from python_get() after GC.!\n");
                    shutdown(1);
                    return;
                }

                python_set(0);

                if(python_get_hook_info()[0] == 0)
                {
                    msg("Heartbeat hook didn't count any heartbeats!\n");
                    shutdown(1);
                    return;
                }

                start_gc(#'shutdown);
            });
        }
        return 0;
    :));
#else
    shutdown(0);
#endif
}

int master_fun() { return 54321; }

string *epilog(int eflag)
{
    run_test();
    return 0;
}
