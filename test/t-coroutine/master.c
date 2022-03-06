#pragma save_types, strong_types, rtt_checks

#include "/sys/lpctypes.h"
#include "/inc/base.inc"
#include "/inc/deep_eq.inc"
#include "/inc/gc.inc"
#include "/inc/sefun.inc"
#include "/inc/testarray.inc"

async void range(int start, int stop, int step = 1)
{
    for (int i = start; i < stop; i+= step)
        yield(i);
}

coroutine reverse(mixed *args)
{
    return async function void()
    {
        for (int i = sizeof(args); i-- > 0;)
            yield(&(args[i]));
    };
}

async void data_cr(mixed arg, mixed val = ({ 'me }))
{
}

async mixed return_value(mixed arg)
{
    return arg;
}

coroutine* global_crs;
int errors, finished;

void check_finish(int garbage)
{
    if (call_coroutine(global_crs[0]) != 2 ||
        !deep_eq(call_coroutine(global_crs[1]), ({0})))
    {
        msg("Global coroutines returned wrong result after GC!\n");
        shutdown(1);
    }
    else
        shutdown(garbage);
}

void finish()
{
    finished++;
    if (finished < 2)
        return;

    global_crs = ({
        reverse(({1,2,3})),
        async function int*() { return funcall(({0}), ({1}), ({2}), ({3}), ({4}), ({5}), ({6}), ({7}), ({8}), yield()); },
    });
    call_coroutine(global_crs[0]);
    call_coroutine(global_crs[1]);

    remove_call_out(#'shutdown);

    if(errors)
        shutdown(1);
    else
        start_gc(#'check_finish);
}

void run_test()
{
    msg("\nRunning test for coroutines:\n"
          "----------------------------\n");

    call_out(#'shutdown, 5*__ALARM_TIME__, 1);

    errors = run_array_without_callback(({
        ({ "creating a coroutine", 0,
            function int()
            {
                coroutine cr = async function void() {};
                return cr != 0;
            },
        }),
        ({ "coroutinep()", 0,
            function int()
            {
                coroutine cr = async function void() {};
                return coroutinep(cr);
            },
        }),
        ({ "call_coroutine() & destruction", 0,
            function int()
            {
                coroutine cr = async function void() {};
                call_coroutine(cr);
                return !cr;
            },
        }),
        ({ "call_coroutine(), yield & return", 0,
            function int()
            {
                coroutine cr = async function int()
                {
                    return 20 + yield(10+yield(100));
                };

                /* The first value will be ignored. */
                if (call_coroutine(cr, 2) != 100)
                    return 0;
                if (call_coroutine(cr, 3) != 13)
                    return 0;
                if (call_coroutine(cr, 5) != 25)
                    return 0;
                return !cr;
            },
        }),
        ({ "this_coroutine()", 0,
            function int()
            {
                coroutine cr = async function void()
                {
                    yield(this_coroutine());
                };
                return call_coroutine(cr) == cr;
            },
        }),
        ({ "foreach()", 0,
            function int()
            {
                int sum;
                foreach (int num: range(10, 100, 5))
                    sum += num;
                return sum == 945;
            },
        }),
        ({ "foreach() with references", 0,
            function int()
            {
                string* arr = ({ "1", "2", "3", });
                foreach (string s: &(reverse(arr)))
                {
                    s += "x";
                    if (s == "2x")
                        break;
                }
                return deep_eq(arr, ({"1", "2x", "3x" }));
            },
        }),
        ({ "foreach() in coroutine 1", 0,
            function int()
            {
                return call_coroutine(async function int()
                {
                    int sum;

                    foreach (int num: range(10, 100, 5))
                        sum += await(return_value(num));

                    return sum;
                }) == 945;
            },
        }),
        ({ "foreach() in coroutine 2", 0,
            function int()
            {
                return call_coroutine(async function int()
                {
                    int sum;

                    foreach (int num: range(10, 100, 5))
                    {
                        sum += await(return_value(num));
                        if (sum > 500)
                            return sum;
                    }

                    return sum;
                }) == 520;
            },
        }),
        ({ "foreach() in coroutine 3", 0,
            function int()
            {
                coroutine cr = async function int()
                {
                    foreach (int num: ({1,2,3}))
                        yield(num);
                };

                if (call_coroutine(cr) != 1)
                    return 0;
                if (5 + call_coroutine(cr) != 7)
                    return 0;
                if (funcall(#'+, 10, call_coroutine(cr)) != 13)
                    return 0;
                return 1;
            },
        }),
        ({ "Caught errors within coroutine 1", TF_ERROR,
            function void()
            {
                coroutine cr1 = async function void()
                {
                    raise_error("Test error.\n");
                };
                coroutine cr2 = async function void()
                {
                    yield(0, cr1);
                };
                call_coroutine(cr2);
            }
        }),
        ({ "Caught errors within coroutine 2", TF_ERROR,
            function void()
            {
                coroutine cr1 = async function void()
                {
                    raise_error("Test error.\n");
                };
                coroutine cr2 = async function void()
                {
                    await(cr1);
                };
                call_coroutine(cr2);
            }
        }),
        ({ "Caught errors within coroutine 3", 0,
            function int()
            {
                coroutine cr1 = async function void()
                {
                    raise_error("Test error.\n");
                };
                coroutine cr2 = async function void()
                {
                    await(cr1);
                };
                coroutine cr3 = async function void()
                {
                    await(cr2);
                };
                catch(call_coroutine(cr3));
                return !cr1 && !cr2 && !cr3; /* All are invalid. */
            }
        }),
        ({ "Caught errors within coroutine 4", TF_ERROR,
            function void()
            {
                coroutine cr1 = async function void()
                {
                    raise_error("Test error.\n");
                };
                coroutine cr2 = async function void()
                {
                    call_coroutine(cr1);
                };
                call_coroutine(cr2);
            }
        }),
        // The uncaught errors we do with call_out,
        // not checking whether they are happing,
        // they just shouldn't crash.
        ({ "Uncaught errors within coroutine 1", 0,
            function int()
            {
                coroutine cr1 = async function void()
                {
                    raise_error("Test error.\n");
                };
                coroutine cr2 = async function void()
                {
                    yield(0, cr1);
                };
                call_out(#'call_coroutine, 0, cr2);
                return 1;
            }
        }),
        ({ "Uncaught errors within coroutine 2", 0,
            function int()
            {
                coroutine cr1 = async function void()
                {
                    raise_error("Test error.\n");
                };
                coroutine cr2 = async function void()
                {
                    await(cr1);
                };
                call_out(#'call_coroutine, 0, cr2);
                return 1;
            }
        }),
        ({ "Uncaught errors within coroutine 3", 0,
            function int()
            {
                coroutine cr1 = async function void()
                {
                    raise_error("Test error.\n");
                };
                coroutine cr2 = async function void()
                {
                    await(cr1);
                };
                coroutine cr3 = async function void()
                {
                    await(cr2);
                };
                call_out(#'call_coroutine, 0, cr3);
                return 1;
            }
        }),
        ({ "Uncaught errors within coroutine 4", 0,
            function int()
            {
                coroutine cr1 = async function void()
                {
                    raise_error("Test error.\n");
                };
                coroutine cr2 = async function void()
                {
                    call_coroutine(cr1);
                };
                call_out(#'call_coroutine, 0, cr2);
                return 1;
            }
        }),
        ({ "await() with wrong type", TF_ERROR,
            function void()
            {
                call_coroutine(async function void()
                {
                    mixed cr = 10;
                    await(cr);
                });
            }
        }),
        ({ "yield() with wrong type", TF_ERROR,
            function void()
            {
                call_coroutine(async function void()
                {
                    mixed cr = 10;
                    yield(20, cr);
                });
            }
        }),
        ({ "Coroutine with bigger stack 1", 0,
            function int()
            {
                coroutine cr = async function string()
                {
                    return sprintf("%s:\n%s", "Result",
                            sprintf("%d %d %d %d %s %d %d %d %d %s %d", 1, 2, 3, 4, "5", 6, 7, 8, 9, yield(), 11));
                };

                call_coroutine(cr);
                return call_coroutine(cr, "x") == "Result:\n1 2 3 4 5 6 7 8 9 x 11";
            }
        }),
        ({ "Coroutine with bigger stack 2", 0,
            function int()
            {
                coroutine cr = async function string()
                {
                    string str = "";

                    str = sprintf("%d", yield(str));
                    str = sprintf("%d %d %d", 1, 2, yield(str));
                    str = sprintf("%d %d %d %d %d", 1, 2, 3, 4, yield(str));
                    str = sprintf("%d %d %d %d %d %d %d", 1, 2, 3, 4, 5, 6, yield(str));
                    str = sprintf("%d %d %d %d %d %d %d %d %d", 1, 2, 3, 4, 5, 6, 7, 8, yield(str));
                    str = sprintf("%d %d %d %d %d %d %d", 1, 2, 3, 4, 5, 6, yield(str));
                    str = sprintf("%d %d %d %d %d", 1, 2, 3, 4, yield(str));
                    str = sprintf("%d %d %d", 1, 2, yield(str));
                    str = sprintf("%d", yield(str));
                    return str;
                };

                if (call_coroutine(cr, 0) != "") return 0;
                if (call_coroutine(cr, 10) != "10") return 0;
                if (call_coroutine(cr, 11) != "1 2 11")  return 0;
                if (call_coroutine(cr, 12) != "1 2 3 4 12") return 0;
                if (call_coroutine(cr, 13) != "1 2 3 4 5 6 13") return 0;
                if (call_coroutine(cr, 14) != "1 2 3 4 5 6 7 8 14") return 0;
                if (call_coroutine(cr, 15) != "1 2 3 4 5 6 15") return 0;
                if (call_coroutine(cr, 16) != "1 2 3 4 16") return 0;
                if (call_coroutine(cr, 17) != "1 2 17") return 0;
                if (call_coroutine(cr, 18) != "18") return 0;
                return 1;
            }
        }),
        ({ "Coroutine keeping all data", 0,
            function int()
            {
                /* We just create coroutines and hope, the GC
                 * will not find any remainders of it.
                 */
                coroutine cr1 = data_cr((["Number": 'one]));
                coroutine cr2 = data_cr((["Number": 'two]));
                coroutine cr3 = async function void() : mapping data = (["Number": 'three]) {};
                coroutine cr4 = async function void() : mapping data = (["Number": 'four]) {};
                coroutine cr5 = async function string()
                {
                    return funcall(0, ({1}), ({2}), ({3}), ({4}), ({5}), ({6}), ({7}), ({8}), ({9}), yield(), ({11}));
                };

                /* Two of them we'll finish. */
                call_coroutine(cr2);
                call_coroutine(cr4);
                call_coroutine(cr5); /* cr5 will still be alive after that. */
                return 1;
            }
        }),
        ({ "Freeing await list", 0,
            function int()
            {
                mapping crs = ([]);
                coroutine cr3;
                m_add(crs, 1, async function void() { await(crs[2]); });
                m_add(crs, 2, async function void() { await(crs[3]); });
                m_add(crs, 3, async function void() { await(crs[4]); });
                m_add(crs, 4, async function void() { await(crs[5]); });
                m_add(crs, 5, async function void() { yield(22); yield(33); });

                /* Initalize the list. */
                if (call_coroutine(crs[1]) != 22)
                    return 0;

                /* Forget all of them, except the number three. */
                cr3 = crs[3];
                crs -= ({1,2,3,4,5});

                /* All calls will be forwarded to the last one. */
                if (call_coroutine(cr3) != 33)
                    return 0;

                /* The third one should still be alive and the GC later
                 * should not complain, as it will be freed just after
                 * the return statement.
                 */
                return cr3 != 0;
            }
        }),
        ({ "to_string() for coroutines of objects", 0,
           function int()
           {
                coroutine cr = "/ob".sleeping_fun(0);
                string name = to_string(cr);

                return strstr(name, "ob") >= 0 && strstr(name, "sleeping_fun") >= 0;
           }
        }),
        ({ "to_string() for coroutines of lwobjects", 0,
           function int()
           {
                lwobject lwob = new_lwobject("/lwob");
                coroutine cr = lwob.sleeping_fun(0);
                string name = to_string(cr);

                return strstr(name, "lwob") >= 0 && strstr(name, "sleeping_fun") >= 0;
           }
        }),
        ({ "get_type_info(cr,0) of objects", 0,
           function int()
           {
                return get_type_info("/ob".sleeping_fun(0), 0) == TYPE_COROUTINE;
           }
        }),
        ({ "get_type_info(cr,2) of objects", 0,
           function int()
           {
                return get_type_info("/ob".sleeping_fun(0), 2) == find_object("/ob");
           }
        }),
        ({ "get_type_info(cr,3) of objects", 0,
           function int()
           {
                return get_type_info("/ob".sleeping_fun(0), 3) == "/ob";
           }
        }),
        ({ "get_type_info(cr,4) of objects", 0,
           function int()
           {
                return get_type_info("/ob".sleeping_fun(0), 4) == "sleeping_fun";
           }
        }),
        ({ "get_type_info(cr,0) of lwobjects", 0,
           function int()
           {
                return get_type_info(new_lwobject("/lwob").sleeping_fun(0), 0) == TYPE_COROUTINE;
           }
        }),
        ({ "get_type_info(cr,2) of lwobjects", 0,
           function int()
           {
                lwobject lwob = new_lwobject("/lwob");
                return get_type_info(lwob.sleeping_fun(0), 2) == lwob;
           }
        }),
        ({ "get_type_info(cr,3) of lwobjects", 0,
           function int()
           {
                return get_type_info(new_lwobject("/lwob").sleeping_fun(0), 3) == "/lwob";
           }
        }),
        ({ "get_type_info(cr,4) of lwobjects", 0,
           function int()
           {
                return get_type_info(new_lwobject("/lwob").sleeping_fun(0), 4) == "sleeping_fun";
           }
        }),
        ({ "coroutines of destructed objects", 0,
           function int()
           {
                object ob = load_object("ob");
                coroutine cr = ob.sleeping_fun(0);

                destruct(ob);
                return cr == 0;
           }
        }),
        ({ "sleep() example for object", 0,
           function int()
           {
                call_coroutine(
                    "ob".sleeping_fun(function void(int success) : int errors = &errors
                    {
                        if (!success)
                        {
                            msg("sleep() example for object FAILED.\n");
                            errors++;
                        }

                        finish();
                    }));
                return 1;
           }
        }),
        ({ "sleep() example for lwobject", 0,
           function int()
           {
                call_coroutine(
                    new_lwobject("/lwob").sleeping_fun(function void(int success) : int errors = &errors
                    {
                        if (!success)
                        {
                            msg("sleep() example for lwobject FAILED.\n");
                            errors++;
                        }

                        finish();
                    }));
                return 1;
           }
        }),
    }));
}

string *epilog(int eflag)
{
    call_out(#'run_test, 0);
    return 0;
}
