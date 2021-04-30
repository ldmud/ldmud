#include "/inc/base.inc"
#include "/inc/testarray.inc"
#include "/inc/gc.inc"

#include "/sys/rtlimits.h"

mapping counter = ([:1]);
int testnum;
string teststr = "a_disappearing_function";

object compile_object(string filename)
{
    int nr;
    object ob;

    filename = filename[0..<3]; /* strip ending. */
    nr = ++counter[filename];

    copy_file(filename + nr + ".c", filename + ".c");
    ob = load_object(filename);
    rm(filename + ".c");

    return ob;
}

void run_test()
{
    int errors;

    msg("\nRunning test for #0000233:\n"
          "--------------------------\n");

    rm("a.c");

    for(testnum = 0; testnum < 32; testnum++)
    {
        foreach(string ob: ({"a","b","b2","c","c2","d"}))
            destruct(find_object(ob));

        counter = ([:1]);
        load_object("b");
        destruct(find_object("a"));

        for(int i=1; i <= 13; i++)
        {
            string testname = sprintf("%d%c", testnum + 1, 'a' + i - 1);
            object d;

            destruct(find_object(sprintf("d%02d", i)));
            d = load_object(sprintf("d%02d", i));

            errors += run_array_without_callback(({
                ({ "Run " + testname + ": Check for merged variables", 0,
                    (:
                        return d->count_both() == 2;
                    :),
                }),
                ({ "Run " + testname + ": Check for having the right program", 0,
                    (:
                        /* a2 is the newer program and thus should have survived. */
                        return d->calc(22) == 42;
                    :)
                }),
                ({ "Run " + testname + ": Check for 'b' calling the right program", 0,
                    (:
                        return d->b_calc(215) == 235;
                    :)
                }),
                ({ "Run " + testname + ": Check for 'c' calling the right program", 0,
                    (:
                        return d->c_calc(330) == 350;
                    :)
                }),
                ({ "Run " + testname + ": Check for missing first inherit", 0,
                    (:
                        /* Shouldn't exist anymore. */
                        mixed res;
                        return call_direct_resolved(&res, d, "fun_a1") == 0;
                    :)
                }),
                ({ "Run " + testname + ": Check for having the second inherit", 0,
                    (:
                        return d->fun_a2() == "a2";
                    :)
                }),
                ({ "Run " + testname + ": Check for locality of private function overrides", 0,
                    (:
                        /* Check doesn't work further up in the inherit chain. */
                        if (i>4)
                           return 1;
                        return d->check_locality();
                    :)
                }),
                ({ "Run " + testname + ": Check for non-virtual vars in 'b'.", 0,
                    (:
                        return d->get_b_var() == "b";
                    :)
                }),
                ({ "Run " + testname + ": Check for non-virtual variable closures in 'b'.", 0,
                    (:
                        return funcall(d->get_b_var_cl()) == "b";
                    :)
                }),
                ({ "Run " + testname + ": Check for non-virtual variable lambda closures in 'b'.", 0,
                    (:
                        return funcall(d->get_b_var_cl2()) == "b";
                    :)
                }),
                ({ "Run " + testname + ": Check for non-virtual vars in 'c'.", 0,
                    (:
                        return d->get_c_var() == "c";
                    :)
                }),
                ({ "Run " + testname + ": Check for non-virtual variable closures in 'c'.", 0,
                    (:
                        return funcall(d->get_c_var_cl()) == "c";
                    :)
                }),
                ({ "Run " + testname + ": Check for non-virtual variable lambda closures in 'c'.", 0,
                    (:
                        return funcall(d->get_c_var_cl2()) == "c";
                    :)
                }),
                ({ "Run " + testname + ": Check for virtual variable closures in 'a'.", 0,
                    (:
                        return funcall(d->get_a_var_cl()) == "a";
                    :)
                }),
                ({ "Run " + testname + ": Check for virtual variable lambda closures in 'a'.", 0,
                    (:
                        return funcall(d->get_a_var_cl2()) == "a";
                    :)
                }),
                ({ "Run " + testname + ": Check for virtual variable closure (via symbol_variable) in 'a'.", 0,
                    (:
                        return funcall(d->get_a_var_cl3()) == "a";
                    :)
                }),
                ({ "Run " + testname + ": Check for virtual variable (via symbol_variable) in a lambda closure in 'a'.", 0,
                    (:
                        return funcall(d->get_a_var_cl4()) == "a";
                    :)
                }),
                ({ "Run " + testname + ": Check for double inherit: Merged Variables", 0,
                    (:
                        /* Test only for DOUBLE_INHERIT */
                        if (!(testnum & 16))
                            return 1;
                        /* Those files don't have a "c". */
                        if (member(([3,4,7,8]), i))
                            return d->get_b2_count() == 3;
                        return d->get_b2_count() + d->get_c2_count() == 7;
                    :)
                }),
                ({ "Run " + testname + ": Check for double inherit: Locality of function overrides.", 0,
                    (:
                        /* Test only for DOUBLE_INHERIT */
                        if (!(testnum & 16))
                            return 1;
                        /* Those files don't have a "c". */
                        if (member(([3,4,7,8]), i))
                            return d->get_b2_program() == "b2";
                        return d->get_b2_program() == "b2" && d->get_c2_program() == "c2";
                    :)
                }),
                ({ "Run " + testname + ": Check for wildcard calls in 'b'.", 0,
                    (:
                        return d.b_wildcard_calc() == 1;
                    :)
                }),
                ({ "Run " + testname + ": Check for wildcard calls in 'c'.", 0,
                    (:
                        return d.c_wildcard_calc() == 1;
                    :)
                }),
            }));
        }
    }

    /* Check that the string is still valid. */
    if (!errors &&
        (teststr[0..1] != "a_" || teststr[2..] != "disappearing_function"))
    {
        msg("Checking function name string '%Q': FAILED\n", teststr);
        errors = 1;
    }

    foreach(string ob: ({"a","b","b2","c","c2","d"}))
        destruct(find_object(ob));
    for(int i=1; i <= 13; i++)
        destruct(find_object(sprintf("d%02d", i)));

    if (errors)
        shutdown(1);
    else
        start_gc(function void(int err)
        {
            /* Check the string again. */
            if (teststr[0..1] != "a_" || teststr[2..] != "disappearing_function")
            {
                msg("Checking function name string '%Q': FAILED\n", teststr);
                err = 1;
            }
            shutdown(err);
        });
    return 0;
}

string *epilog(int eflag)
{
    set_driver_hook(H_AUTO_INCLUDE,
        function string(string base_file, string current_file, int sys_include)
        {
            return "#pragma strong_types, save_types\n" +
                ((testnum & 1) ? "#define OLD_VARIABLES\n" : "") +
                ((testnum & 2) ? "#define NEW_VARIABLES\n" : "") +
                ((testnum & 4) ? "#define SUB_INHERIT\n" : "") +
                ((testnum & 8) ? "#define TEST_VIRTUAL virtual\n" : "#define TEST_VIRTUAL\n") +
                ((testnum &16) ? "#define DOUBLE_INHERIT\n" : "");
        });

    limited(#'run_test, LIMIT_EVAL, 1000000);
    return 0;
}
