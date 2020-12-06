#pragma save_types, strong_types, rtt_checks, no_inherit, lightweight, no_warn_lightweight

#include "../sys/driver_info.h"
#include "../sys/lpctypes.h"
#include "../sys/lwobject_info.h"

#include "../inc/deep_eq.inc"
#include "../inc/msg.inc"
#include "../inc/testarray.inc"

int var1 = 42;
string var2;
mixed var3;

void new()
{
    var2 = "new";
}

int check(int val)
{
    return var1 == val;
}

int get()
{
    return var1;
}

string string_function(string arg)
{
    return "Hussa, " + arg + "!";
}

int run_tests()
{
    return !run_array_without_callback(({
        ({ "__INIT called", 0,
            function int()
            {
                return get() == 42;
            },
        }),
        ({ "H_CREATE_LWOBJECT called", 0,
            function int()
            {
                return var2 == "new";
            },
        }),
        ({ "this_object()", 0,
            function int()
            {
                lwobject lwob = this_object();
                return lwob != 0;
            },
        }),
        ({ "lwobjectp()", 0,
            function int()
            {
                return lwobjectp(this_object())
                    && !lwobjectp(blueprint());
            }
        }),
        ({ "comparisons", 0,
            function int()
            {
                lwobject lwob = this_object();
                return lwob == this_object()
                   && !(lwob != this_object())
                   && member(({1,2,lwob,3}), this_object()) == 2
                   && rmember(({1,2,lwob,3}), this_object()) == 2;
            },
        }),
        ({ "caller_stack()", 0,
            function int()
            {
                return caller_stack()[0] == this_object();
            },
        }),
        ({ "previous_object()", 0,
            function int()
            {
                return previous_object() == this_object();
            },
        }),
        ({ "previous_object(0)", 0,
            function int()
            {
                return previous_object(0) == this_object();
            },
        }),
        ({ "blueprint()", 0,
            function int()
            {
                return blueprint() == find_object("/lwo/tests");
            }
        }),
        ({ "program_name()", 0,
            function int()
            {
                return program_name() == "/lwo/tests.c";
            }
        }),
        ({ "program_time()", 0,
            function int()
            {
                return program_time() == program_time(find_object("/lwo/tests"));
            }
        }),
        ({ "load_name()", 0,
            function int()
            {
                return load_name() == "/lwo/tests";
            }
        }),
        ({ "to_string()", 0,
            function int()
            {
                return strstr(to_string(this_object()), load_name()) >= 0;
            }
        }),
        ({ "filter_objects()", 0,
            function int()
            {
                lwobject ob2 = new_lwobject(program_name());
                var1 = 100;

                return deep_eq(({ob2}), filter_objects(({this_object(), ob2}), "check", 42));
            }
        }),
        ({ "map_objects()", 0,
            function int()
            {
                lwobject ob2 = new_lwobject(program_name());
                var1 = 100;

                return deep_eq(({100, 42}), map_objects(({this_object(), ob2}), "get"));
            }
        }),
        ({ "unique_array()", 0,
            function int()
            {
                lwobject ob2, ob3, ob4;
                closure check;

                var1 = 200;
                ob2 = copy(this_object());
                ob3 = copy(this_object());
                ob4 = new_lwobject(load_name());
                var1 = 100;

                check = function int(lwobject** result)
                {
                    if (sizeof(result) != 2)
                        return 0;
                    if (!deep_eq(mkmapping(map(result,#'sizeof)), ([1,2])))
                        return 0;
                    foreach (lwobject* group: result)
                        if (sizeof(group) == 2 && !deep_eq(mkmapping(group), ([ob2, ob3])))
                            return 0;
                        else if (sizeof(group) == 1 && !deep_eq(group, ({ob4})))
                            return 0;
                    return 1;
                };

                return funcall(check, unique_array(({this_object(), ob2, ob3, ob4}), "get", 100))
                    && funcall(check, unique_array(({this_object(), ob2, ob3, ob4}), #'call_strict, "get", 100));
            }
        }),
        ({ "lambda()", 0,
            function int()
            {
                return funcall(lambda(0, ({#'&&,                /* Operator          */
                    ({#'lwobjectp, ({#'this_object}) }),        /* Efuns             */
                    ({#'check, ({#'var1}) }) })));              /* Lfun & Identifier */
            }
        }),
        ({ "bind_lambda() 1", 0,
            function int()
            {
                return funcall(bind_lambda(#'this_object)) == this_object();
            }
        }),
        ({ "bind_lambda() 2", 0,
            function int()
            {
                return funcall(bind_lambda(unbound_lambda(0,
                    ({#'&&,
                    ({#'lwobjectp, ({#'this_object}) }),
                    ({#'check, ({#'var1}) }) }))));
            }
        }),
        ({ "bind_lambda() 3", 0,
            function int()
            {
                return funcall(bind_lambda(bind_lambda(unbound_lambda(0,
                    ({#'&&,
                    ({#'lwobjectp, ({#'this_object}) }),
                    ({#'check, ({#'var1}) }) })))));
            }
        }),
        ({ "bind_lambda() 4", 0,
            function int()
            {
                closure cl = bind_lambda(unbound_lambda(0,
                    ({#'&&,
                    ({#'lwobjectp, ({#'this_object}) }),
                    ({#'check, ({#'var1}) }) })));
                return funcall(bind_lambda(cl)) && funcall(cl);
            }
        }),
        ({ "symbol_function()", 0,
            function int()
            {
                return funcall(symbol_function("this_object")) == this_object();
            }
        }),
        ({ "lfun closure call", 0,
            function int()
            {
                return funcall(#'check, var1);
            }
        }),
        ({ "identifier closure call", 0,
            function int()
            {
                return funcall(#'var1) == var1;
            }
        }),
        ({ "closure and lwobject order", 0,
            function int()
            {
                lwobject lwob1 = this_object(), lwob2 = copy(lwob1);
                closure cl1 = #'check, cl2 = #'var1, cl3 = symbol_function("check", lwob2);

                /* If the ordering didn't work, set operations wouldn't. */
                return !sizeof(({cl1, cl2, cl3, lwob1, lwob2})^({lwob2, lwob1, cl3, cl2, cl1}))
                    && !sizeof(({cl3, cl2, cl1, lwob2, lwob1})^({lwob1, lwob2, cl1, cl2, cl3}));
            }
        }),
        ({ "last_instructions()", 0,
            function int()
            {
                /* Can't really verify it, but it shouldn't crash. */
                last_instructions(4096, 1);
                return 1;
            }
        }),
        ({ "typeof(TO)", 0,
            function int()
            {
                return typeof(this_object()) == T_LWOBJECT;
            }
        }),
        ({ "get_type_info(TO,0)", 0,
            function int()
            {
                return get_type_info(this_object(),0) == T_LWOBJECT;
            }
        }),
        ({ "get_type_info(efun_cl,0)", 0,
            function int()
            {
                return get_type_info(#'this_object,0) == T_CLOSURE;
            }
        }),
        ({ "get_type_info(efun_cl,1)", 0,
            function int()
            {
                return CLOSURE_IS_EFUN(get_type_info(#'this_object,1));
            }
        }),
        ({ "get_type_info(efun_cl,2)", 0,
            function int()
            {
                return get_type_info(#'this_object,2) == this_object();
            }
        }),
        ({ "get_type_info(lfun_cl,0)", 0,
            function int()
            {
                return get_type_info(#'check,0) == T_CLOSURE;
            }
        }),
        ({ "get_type_info(lfun_cl,1)", 0,
            function int()
            {
                return CLOSURE_IS_LFUN(get_type_info(#'check,1));
            }
        }),
        ({ "get_type_info(lfun_cl,2)", 0,
            function int()
            {
                return get_type_info(#'check,2) == this_object();
            }
        }),
        ({ "get_type_info(lfun_cl,3)", 0,
            function int()
            {
                return get_type_info(#'check,3) == load_name();
            }
        }),
        ({ "get_type_info(lfun_cl,4)", 0,
            function int()
            {
                return get_type_info(#'check,4) == "check";
            }
        }),
        ({ "get_type_info(var_cl,0)", 0,
            function int()
            {
                return get_type_info(#'var1,0) == T_CLOSURE;
            }
        }),
        ({ "get_type_info(var_cl,1)", 0,
            function int()
            {
                return CLOSURE_IS_IDENTIFIER(get_type_info(#'var1,1));
            }
        }),
        ({ "get_type_info(var_cl,2)", 0,
            function int()
            {
                return get_type_info(#'var1,2) == this_object();
            }
        }),
        ({ "save/restore_value(op_cl)", 0,
            function int()
            {
                return restore_value(save_value(#'break)) == #'break;
            }
        }),
        ({ "save/restore_value(efun_cl)", 0,
            function int()
            {
                return restore_value(save_value(#'this_object)) == #'this_object;
            }
        }),
        ({ "save/restore_value(lfun_cl)", 0,
            function int()
            {
                return restore_value(save_value(#'check)) == #'check;
            }
        }),
        ({ "save/restore_value(var_cl)", 0,
            function int()
            {
                return restore_value(save_value(#'var1)) == #'var1;
            }
        }),
        ({ "to_string(op_cl)", 0,
            function int()
            {
                return stringp(to_string(#'break)); /* Did not crash. */
            }
        }),
        ({ "to_string(efun_cl)", 0,
            function int()
            {
                return stringp(to_string(#'break));
            }
        }),
        ({ "to_string(lfun_cl)", 0,
            function int()
            {
                return strstr(to_string(#'check), load_name()) >= 0;
            }
        }),
        ({ "to_string(var_cl)", 0,
            function int()
            {
                return strstr(to_string(#'var1), load_name()) >= 0;
            }
        }),
        ({ "lwobject_info(TO, LI_LWOBJECT_REFS)", 0,
            function int()
            {
                int cur = lwobject_info(this_object(), LI_LWOBJECT_REFS);
                lwobject lwob = this_object();
                return lwobject_info(this_object(), LI_LWOBJECT_REFS) > cur;
            }
        }),
        ({ "lwobject_info(TO, LI_DATA_SIZE)", 0,
            function int()
            {
                int cur = lwobject_info(this_object(), LI_DATA_SIZE);
                var3 = ({10,20,30});
                return lwobject_info(this_object(), LI_DATA_SIZE) > cur;
            }
        }),
        ({ "lwobject_info(TO, LI_DATA_SIZE_TOTAL)", 0,
            function int()
            {
                int cur = lwobject_info(this_object(), LI_DATA_SIZE_TOTAL);
                var3 = ({10,20,30,40,50,60});
                return lwobject_info(this_object(), LI_DATA_SIZE_TOTAL) > cur;
            }
        }),
        ({ "lwobject_info(TO, LI_NO_INHERIT)", 0,
            function int()
            {
                return lwobject_info(this_object(), LI_NO_INHERIT) == 1;
            }
        }),
        ({ "lwobject_info(TO, LI_NO_CLONE)", 0,
            function int()
            {
                return lwobject_info(this_object(), LI_NO_CLONE) == 1;
            }
        }),
        ({ "lwobject_info(TO, LI_NO_LIGHTWEIGHT)", 0,
            function int()
            {
                return lwobject_info(this_object(), LI_NO_LIGHTWEIGHT) == 0;
            }
        }),
        ({ "lwobject_info(TO, LI_SHARE_VARIABLES)", 0,
            function int()
            {
                return lwobject_info(this_object(), LI_SHARE_VARIABLES) == 0;
            }
        }),
        ({ "lwobject_info(TO, LI_PROG_REFS)", 0,
            function int()
            {
                int cur = lwobject_info(this_object(), LI_PROG_REFS);
                lwobject c = copy(this_object());
                return lwobject_info(this_object(), LI_PROG_REFS) > cur;
            }
        }),
        ({ "lwobject_info(TO, LI_NUM_FUNCTIONS)", 0,
            function int()
            {
                return lwobject_info(this_object(), LI_NUM_FUNCTIONS) >= 4;
            }
        }),
        ({ "lwobject_info(TO, LI_NUM_VARIABLES)", 0,
            function int()
            {
                return lwobject_info(this_object(), LI_NUM_VARIABLES) == 3;
            }
        }),
        ({ "lwobject_info(TO, LI_NUM_STRINGS)", 0,
            function int()
            {
                return lwobject_info(this_object(), LI_NUM_STRINGS) > 10;
            }
        }),
        ({ "lwobject_info(TO, LI_NUM_INHERITED)", 0,
            function int()
            {
                return lwobject_info(this_object(), LI_NUM_INHERITED) == 0;
            }
        }),
        ({ "lwobject_info(TO, LI_NUM_INCLUDED)", 0,
            function int()
            {
                return lwobject_info(this_object(), LI_NUM_INCLUDED) >= 6;
            }
        }),
        /* For the LI_SIZE_* stuff we basically check,
         * that it doesn't crash the driver.
         */
        ({ "lwobject_info(TO, LI_SIZE_FUNCTIONS)", 0,
            function int()
            {
                return lwobject_info(this_object(), LI_SIZE_FUNCTIONS) > 10;
            }
        }),
        ({ "lwobject_info(TO, LI_SIZE_VARIABLES)", 0,
            function int()
            {
                return lwobject_info(this_object(), LI_SIZE_VARIABLES) > 3;
            }
        }),
        ({ "lwobject_info(TO, LI_SIZE_STRINGS)", 0,
            function int()
            {
                return lwobject_info(this_object(), LI_SIZE_STRINGS) > 10;
            }
        }),
        ({ "lwobject_info(TO, LI_SIZE_STRINGS_DATA)", 0,
            function int()
            {
                return lwobject_info(this_object(), LI_SIZE_STRINGS_DATA) > 10;
            }
        }),
        ({ "lwobject_info(TO, LI_SIZE_STRINGS_DATA_TOTAL)", 0,
            function int()
            {
                return lwobject_info(this_object(), LI_SIZE_STRINGS_DATA_TOTAL) > 10;
            }
        }),
        ({ "lwobject_info(TO, LI_SIZE_INHERITED)", 0,
            function int()
            {
                return lwobject_info(this_object(), LI_SIZE_INHERITED) == 0;
            }
        }),
        ({ "lwobject_info(TO, LI_SIZE_INCLUDED)", 0,
            function int()
            {
                return lwobject_info(this_object(), LI_SIZE_INCLUDED) >= 6;
            }
        }),
        ({ "lwobject_info(TO, LI_PROG_SIZE)", 0,
            function int()
            {
                return lwobject_info(this_object(), LI_PROG_SIZE) > 0;
            }
        }),
        ({ "lwobject_info(TO, LI_PROG_SIZE_TOTAL)", 0,
            function int()
            {
                return lwobject_info(this_object(), LI_PROG_SIZE_TOTAL) > 0;
            }
        }),
        ({ "driver_info(DI_TRACE_CURRENT)", 0,
            function int()
            {
                mixed* trace = driver_info(DI_TRACE_CURRENT);
                return trace[<1][TRACE_TYPE] == TRACE_TYPE_LFUN
                    && trace[<1][TRACE_PROGRAM] == __FILE__[1..]
                    && trace[<1][TRACE_OBJECT] == 0;
                return 1;
            }
        }),
        ({ "driver_info(DI_TRACE_CURRENT_AS_STRING)", 0,
            function int()
            {
                return strstr(driver_info(DI_TRACE_CURRENT_AS_STRING), __FILE__[1..]) > 0;
            }
        }),
        ({ "driver_info(DI_TRACE_LAST_ERROR)", 0,
            function int()
            {
                mixed* trace;

                catch(0/0);
                trace = driver_info(DI_TRACE_LAST_ERROR);

                return trace[<2][TRACE_TYPE] == TRACE_TYPE_LFUN
                    && trace[<2][TRACE_PROGRAM] == __FILE__[1..]
                    && trace[<2][TRACE_OBJECT] == 0;
                return 1;
            }
        }),
        ({ "driver_info(DI_TRACE_LAST_ERROR_AS_STRING)", 0,
            function int()
            {
                catch(0/0);
                return strstr(driver_info(DI_TRACE_LAST_ERROR_AS_STRING), __FILE__[1..]) > 0;
            }
        }),
        ({ "process_string()", 0,
            function int()
            {
                return process_string("@@string_function|you there@@ How are you?") == "Hussa, you there! How are you?";
            }
        }),
        ({ "add_action()", TF_ERROR,
            function void()
            {
                add_action((: 1 :), "jump");
            }
        }),
        ({ "command()", TF_ERROR,
            function void()
            {
                command("jump");
            }
        }),
        ({ "remove_action()", TF_ERROR,
            function void()
            {
                remove_action("jump", find_object(__MASTER_OBJECT__));
            }
        }),
        ({ "clones()", TF_ERROR,
            function void()
            {
                clones();
            }
        }),
        ({ "present_clone()", TF_ERROR,
            function void()
            {
                present_clone(blueprint());
            }
        }),
        ({ "present()", TF_ERROR,
            function void()
            {
                present("Who");
            }
        }),
        ({ "environment()", TF_ERROR,
            function void()
            {
                environment();
            }
        }),
        ({ "all_environment()", TF_ERROR,
            function void()
            {
                all_environment();
            }
        }),
        ({ "all_inventory()", TF_ERROR,
            function void()
            {
                all_inventory();
            }
        }),
        ({ "deep_inventory()", TF_ERROR,
            function void()
            {
                deep_inventory();
            }
        }),
        ({ "first_inventory()", TF_ERROR,
            function void()
            {
                first_inventory();
            }
        }),
        ({ "next_inventory()", TF_ERROR,
            function void()
            {
                next_inventory();
            }
        }),
        ({ "say()", TF_ERROR,
            function void()
            {
                say("Hi, there!");
            }
        }),
        ({ "to_object()", TF_ERROR,
            function void()
            {
                to_object(this_object());
            }
        }),
        ({ "save_object()", TF_ERROR,
            function void()
            {
                save_object();
            }
        }),
        ({ "restore_object()", TF_ERROR,
            function void()
            {
                restore_object("#3:2\n");
            }
        }),
        ({ "set_next_reset()", TF_ERROR,
            function void()
            {
                set_next_reset(0);
            }
        }),
        ({ "shadow()", TF_ERROR,
            function void()
            {
                shadow(blueprint());
            }
        }),
        ({ "unshadow()", TF_ERROR,
            function void()
            {
                unshadow();
            }
        }),
        ({ "shadow()", TF_ERROR,
            function void()
            {
                shadow(blueprint());
            }
        }),
        ({ "shadow()", TF_ERROR,
            function void()
            {
                shadow(blueprint());
            }
        }),
        ({ "net_connect()", TF_ERROR,
            function void()
            {
                net_connect("127.0.0.1", 1234);
            }
        }),
    }));
}
