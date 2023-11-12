#include "/sys/driver_info.h"
#include "/inc/base.inc"
#include "/inc/deep_eq.inc"
#include "/inc/testarray.inc"
#include "/inc/gc.inc"

struct test_struct
{
    int     t_int;
    float   t_float;
    string  t_string;
    object  t_object;
    lwobject t_lwobject;
    mixed*  t_array;
    mapping t_mapping;
    symbol  t_symbol;
    mixed   t_quoted_array;
    coroutine t_coroutine;
};

struct other_struct
{
};

void run_test()
{
#ifdef __PYTHON__

    msg("\nRunning tests for pkg_python:\n"
          "-----------------------------\n");

    run_array(({
        ({ "driver_info(DI_NUM_LPC_PYTHON_REFS) at the beginning", 0,
            function int()
            {
                return driver_info(DI_NUM_LPC_PYTHON_REFS) == 0;
            }
        }),
        ({ "driver_info(DI_NUM_PYTHON_LPC_REFS) at the beginning", 0,
            function int()
            {
                clean_early_ob();
                /* There should only be one reference:
                 * The master object startup.ob_list.
                 */
                return driver_info(DI_NUM_PYTHON_LPC_REFS) == 1;
            }
        }),
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
        ({ "passing unicode string", 0,
            (:
                return python_return("") == "" &&
                       python_return("Hi") == "Hi" &&
                       python_return("\0") == "\0" &&
                       python_return("42\u20ac") == "42\u20ac";
            :)
        }),
        ({ "passing byte string", 0,
            (:
                return python_return(to_bytes(({}))) == to_bytes(({})) &&
                       python_return(to_bytes(({0,1,2,3,4}))) == to_bytes(({0,1,2,3,4})) &&
                       python_return(to_bytes("42\u20ac", "UTF-8")) == to_bytes(({0x34, 0x32, 0xe2, 0x82, 0xac}));
            :)
        }),
        ({ "passing objects", 0,
            (:
                return python_return(this_object()) == this_object();
            :)
        }),
        ({ "passing lightweight objects", 0,
            (:
                lwobject lwob = new_lwobject("/testob");
                destruct(find_object("/testob"));
                return lwobjectp(lwob) && python_return(lwob) == lwob;
            :)
        }),
        ({ "passing arrays", 0,
            (:
                mixed * arr = ({1,2,3});
                return python_return(({})) == ({}) &&
                       python_return(arr) == arr;
            :)
        }),
        ({ "passing mappings", 0,
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
        ({ "passing coroutines", 0,
            (:
                coroutine cr = "/testob"->testcoroutine();
                int result = python_return(cr) == cr;
                destruct(find_object("/testob"));

                return result;
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
        ({ "passing lvalue", 0,
            (:
                int x = 70550;
                mixed* result = ({ &(python_return(&x)) });
                result[0] = 66606;
                return x == 66606;
            :)
        }),
        ({ "passing lpctype", 0,
            (:
                return python_return([int])                == [int] &&
                       python_return([string])             == [string] &&
                       python_return([void])               == [void] &&
                       python_return([mapping])            == [mapping] &&
                       python_return([float])              == [float] &&
                       python_return([closure])            == [closure] &&
                       python_return([coroutine])          == [coroutine] &&
                       python_return([symbol])             == [symbol] &&
                       python_return([bytes])              == [bytes] &&
                       python_return([lpctype])            == [lpctype] &&
                       python_return([mixed])              == [mixed] &&
                       python_return([object])             == [object] &&
                       python_return([object "/master"])   == [object "/master"] &&
                       python_return([lwobject])           == [lwobject] &&
                       python_return([lwobject "/master"]) == [lwobject "/master"] &&
                       python_return([struct mixed])       == [struct mixed] &&
                       python_return([struct test_struct]) == [struct test_struct] &&
                       python_return([mixed*])             == [mixed*] &&
                       python_return([int*])               == [int*] &&
                       python_return([symbol*])            == [symbol*] &&
                       python_return([int|string])         == [int|string] &&
                       python_return([lpctype])            == [lpctype] &&
                       python_return([box])                == [box];
            :)
        }),
        ({ "passing too many arguments", TF_ERROR,
            (:
                return funcall(#'python_return, 1, 2);
            :)
        }),
        ({ "passing less arguments", TF_ERROR,
            (:
                return funcall(#'python_return);
            :)
        }),
        ({ "passing invalid arguments 1", TF_ERROR,
            (:
                return funcall(#'python_typecheck, 10, "");
            :)
        }),
        ({ "passing invalid arguments 2", TF_ERROR,
            (:
                return funcall(#'python_typecheck, "", "", "");
            :)
        }),
        ({ "passing valid arguments", 0,
            (:
                return funcall(#'python_typecheck, "", "", 1, 2);
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
            "using python type 1 (bigint)", 0,
            (:
                bigint val = to_bigint(1000);

                // Check that it's really a big integer.
                if (sprintf("%Q", val << 1000) != "10715086071862673209484250490600018105614048117055336074437503883703510511249361224931983788156958581275946729175531468251871452856923140435984577574698574803934567774824230985421074605062371141877954182153046474983581941267398767559165543946077062914571196477686542167660429831652624386837205668069376000")
                    return 0;

                // Check comparisons.
                if (val != 1000
                 || val > 1000
                 || val >= 1001
                 || val < 1000
                 || val <= 999
                 || val == 1
                 || 1000 != val
                 || 1000 < val
                 || 1001 <= val
                 || 1000 > val
                 || 999 >= val
                 || 1 == val)
                    return 0;

                // Check arithmetics
                if (val + 5 != 1005
                 || val - 5 != 995
                 || val * 5 != 5000
                 || val / 5 != 200
                 || val % 6 != 4
                 || val << 2 != 4000
                 || val >> 2 != 250
                 || (val & 10) != 8
                 || (val | 10) != 1002
                 || (val ^ 10) != 994
                 || 5 + val != 1005
                 || 5 - val != -995
                 || 5 * val != 5000
                 || 5000 / val != 5
                 || 5500 % val != 500
                 || 2 >> val != 0
                 || (10 & val) != 8
                 || (10 | val) != 1002
                 || (10 ^ val) != 994
                 || -val != -1000
                 || ~val != -1001)
                    return 0;

                // Check efun override
                int i = to_int(val);
                string s = to_string(val << 1000);
                if (intp(val)
                 || !intp(i)
                 || i != 1000
                 || s != "10715086071862673209484250490600018105614048117055336074437503883703510511249361224931983788156958581275946729175531468251871452856923140435984577574698574803934567774824230985421074605062371141877954182153046474983581941267398767559165543946077062914571196477686542167660429831652624386837205668069376000")
                    return 0;

                // Check __int__, __float__ und __str__ working.
                if (to_type(val, [int]) != 1000
                 || to_type(val, [string]) != "1000"
                 || to_type(val, [float]) != 1000.0)
                    return 0;

                val <<= 1000;
                bigint val2 = restore_value(save_value(val));
                if (val != val2)
                    return 0;

                return 1;
            :)
        }),
        ({
            "using python type 2 (random_generator)", 0,
            (:
                random_generator r = create_random_generator();

                if (r.randint(10,20) < 10
                 || r.randint(10,20) > 20)
                     return 0;

                if (r.uniform(20,30) < 20
                 || r.uniform(20,30) > 30)
                     return 0;

                string *elems = ({"A","B","C","D"});
                r.shuffle(elems);

                string elem = r.choice(elems);
                if (sizeof(elem) != 1 || elem[0] < 'A' || elem[0] > 'D')
                    return 0;

                /* We cannot save that, but it should not bring an error. */
                if (restore_value(save_value(r)) != 0)
                    return 0;

                return 1;
            :)
        }),
        ({
            "using python type 3 (box)", 0,
            (:
                box b = create_box(100);
                box c = copy(b);
                box d = deep_copy(b);
                box e = deep_copy(({b}))[0];

                /* Check that the copy contains the data. */
                if (b.get_value() != 100
                 || c.get_value() != 100
                 || d.get_value() != 100
                 || e.get_value() != 100)
                     return 0;

                /* Check that these are independed objects. */
                b.set_value(101);
                c.set_value(102);
                d.set_value(103);
                e.set_value(104);

                if (b.get_value() != 101
                 || c.get_value() != 102
                 || d.get_value() != 103
                 || e.get_value() != 104)
                    return 0;

                /* Check that save/restore retains the values. */
                c = restore_value(save_value(b));
                d = restore_value(save_value(({b})))[0];
                e = restore_value(save_value((["A":b])))["A"];

                if (c.get_value() != 101
                 || d.get_value() != 101
                 || e.get_value() != 101)
                    return 0;

                /* Check to_type() using __convert__(). */
                if (to_type(b, [int|string]) != 101)
                    return 0;

                /* And works with complex data structures. */
                b.set_value(({20,30}));
                c = restore_value(save_value(b));
                if (!deep_eq(c.get_value(), ({20,30})))
                    return 0;

                mapping m = ([1:({2})]);
                b.set_value(m);
                <mapping|box>* x = restore_value(save_value(({b,m})));
                if (x[0].get_value() != x[1]
                 || !deep_eq(x[1],m))
                    return 0;

                return 1;
            :)
        }),
        ({ "driver_info(DI_NUM_LPC_PYTHON_REFS) after some tests", 0,
            function int()
            {
                return driver_info(DI_NUM_LPC_PYTHON_REFS) == 0;
            }
        }),
        ({ "driver_info(DI_NUM_PYTHON_LPC_REFS) after some tests", 0,
            function int()
            {
                return driver_info(DI_NUM_PYTHON_LPC_REFS) == 1;
            }
        }),
        ({ "driver_info(DI_NUM_LPC_PYTHON_REFS) with a Python object", 0,
            function int()
            {
                box b = create_box(100);
                return driver_info(DI_NUM_LPC_PYTHON_REFS) == 1;
            }
        }),
        ({ "driver_info(DI_NUM_PYTHON_LPC_REFS) with an LPC reference", 0,
            function int()
            {
                box b = create_box(({100}));
                return driver_info(DI_NUM_PYTHON_LPC_REFS) == 2;
            }
        }),
        ({
            "Python object hook 1", 0,
            (:
                object* oblist = python_get_hook_info()[1];
                return sizeof(oblist) == 1 && oblist[0] == this_object();
            :)
        }),
        ({ "Python GC", 0,
            (:
                /* We just start it and see, that it doesn't crash. */
                python_gc();
                return 1;
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
        ({
           "passing valid arguments with ...", 0,
            (:
                return load_object("/testcompileargcheck")->check();
            :)
        }),
        ({
           "passing valid arguments with ...", TF_ERROR,
            (:
                load_object("/testcompileargstrict");
            :)
        }),
        ({
            "Compiler bug on type testing", TF_DONTCHECKERROR,
            (:
                load_object("/testcompiler");
            :)
        })

    }), //#'shutdown);
    (:
        if($1)
            shutdown(1);
        else
        {
            object tmp_ob = clone_object(this_object());

            python_set((<test_struct> 
                705948522,
                -1000000.0,
                "Garbage",
                this_object(),
                new_lwobject("/testob"),
                ({ 5, 3, 1}),
                ([2:2,3:3,5:5,tmp_ob:({2,3,5})]),
                quote("abc"+"gc"),
                quote(({11, 13, 17})),
                "/testob"->testcoroutine(),
            ));

            python_remember_testob(load_object("/testrp"));
            if (!python_check_testob())
            {
                shutdown(1);
                return 0;
            }

            /* Check whether mapping cleanup works across Python boundary. */
            destruct(tmp_ob);

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
                    !lwobjectp(val->t_lwobject) || program_name(val->t_lwobject) != "/testob.c" ||
                    val->t_array[0] != 5 || val->t_array[1] != 3 || val->t_array[2] != 1 ||
                    sizeof(val->t_mapping) != 3 || widthof(val->t_mapping) != 1 ||
                    !member(val->t_mapping, 2) || !member(val->t_mapping, 3) || !member(val->t_mapping, 5) ||
                    unquote(val->t_symbol) != "ab" + "cgc" ||
                    sizeof(unquote(val->t_quoted_array)) != 3 ||
                    !coroutinep(val->t_coroutine)
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

                if(!python_check_testob())
                {
                    shutdown(1);
                    return;
                }

                if(has_gil_log_message())
                {
                    msg("Python GIL was held during backend loop!\n");
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
    set_driver_hook(H_CREATE_OB, "create");

    run_test();
    return 0;
}
