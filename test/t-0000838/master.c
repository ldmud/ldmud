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
                comp.create_struct1();
                comp.create_struct2();
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

                mixed s1 = ob1.get_data();
                mixed s2 = ob1.get_sub_data();

                msg("\n");
                return !run_array_without_callback(({
                    ({ "Check read access with compile-time lookup",           0, (: ob2.read_data_ct(s1) :) }),
                    ({ "Check read access with compile-time lookup on child",  0, (: ob2.read_data_ct(s2) :) }),
                    ({ "Check read access with runtime lookup",                0, (: ob2.read_data_rt(s1) :) }),
                    ({ "Check read access with runtime lookup on child",       0, (: ob2.read_data_rt(s2) :) }),
                    ({ "Check write access with compile-time lookup",          0, (: ob2.write_data_ct(s1) :) }),
                    ({ "Check write access with compile-time lookup on child", 0, (: ob2.write_data_ct(s2) :) }),
                    ({ "Check write access with runtime lookup",               0, (: ob2.write_data_rt(s1) :) }),
                    ({ "Check write access with runtime lookup on child",      0, (: ob2.write_data_rt(s2) :) }),
                    ({ "Relation between old super and new super",             0, (: ob2.baseof_super(s1) == 2 :) }),
                    ({ "Relation between old sub and new super",               0, (: ob2.baseof_super(s2) == 0 :) }),
                    ({ "Relation between old super and new sub",               0, (: ob2.baseof_sub(s1)   == 1 :) }),
                    ({ "Relation between old sub and new sub",                 0, (: ob2.baseof_sub(s2)   == 2 :) }),
                    ({ "Conversion from super to super",                       0, (: ob2.conv_data_to_super(s1) :) }),
                    ({ "Conversion from sub to super",                         0, (: ob2.conv_data_to_super(s2) :) }),
                    ({ "Conversion from super to sub",                         0, (: ob2.conv_data_to_sub(s1) :) }),
                    ({ "Conversion from sub to sub",                           0, (: ob2.conv_data_to_sub(s2) :) }),
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
