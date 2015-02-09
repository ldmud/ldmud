#include "/inc/base.inc"
#include "/inc/testarray.inc"

mapping counter = ([:1]);

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

    msg("\nRunning test for #0000838:\n"
          "---------------------------\n");

    rm("a.c");

    run_array(({
        ({ "Check for structs containing other structs", 0,
            (:
                object comp = load_object("compound_defs");
                destruct(comp);
                // Remember, that this object will be deallocated entirely at
                // the next backend cycle. So the old struct is still known,
                // that's why reactivation works here.
                comp = load_object("compound_use");
                comp->create_struct();
                return 1;
            :)
        }),
        ({ "Check for structs arrays being considered", 0,
            (:
                object ob = load_object("array");
                destruct(ob);
                ob = load_object("array");
                ob->fun();
                return 1;
            :)
        }),
        ({ "Check for compatibility of changed structs", 0,
            (:
                object ob1, ob2;

                rm("changing_def.c");
                ob1 = load_object("changing_use1");
                destruct(find_object("changing_def"));
                ob2 = load_object("changing_use2");

                mixed s = ob1->get_data();

                msg("\n");
                return !run_array_without_callback(({
                    ({ "Check read access with compile-time lookup",  0, (: ob2->read_data_ct(s) :) }),
                    ({ "Check read access with runtime lookup",       0, (: ob2->read_data_rt(s) :) }),
                    ({ "Check write access with compile-time lookup", 0, (: ob2->write_data_ct(s) :) }),
                    ({ "Check write access with runtime lookup",      0, (: ob2->write_data_rt(s) :) }),
                }));
            :)
        }),
    }), #'shutdown);

    return 0;
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
