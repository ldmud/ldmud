#include "/inc/base.inc"
#include "/inc/testarray.inc"

mapping counter = ([:1]);
int testnum;

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

    for(testnum = 0; testnum < 16; testnum++)
    {
        foreach(string ob: ({"a","b","c"}))
            destruct(find_object(ob));

        counter = ([:1]);
        load_object("b");
        destruct(find_object("a"));

        for(int i=1; i <= 10; i++)
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
                	asin(1.0);
                        /* Check doesn't work further up in the inherit chain. */
                        return d->get_b_var() == "b";
                    :)
                }),
                ({ "Run " + testname + ": Check for non-virtual vars in 'c'.", 0,
                    (:
                        /* Check doesn't work further up in the inherit chain. */
                        return d->get_c_var() == "c";
                    :)
                }),
            }));
        }
    }

    shutdown(errors && 1);
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
                ((testnum & 8) ? "#define TEST_VIRTUAL virtual\n" : "#define TEST_VIRTUAL\n");
        });

    run_test();
    return 0;
}
