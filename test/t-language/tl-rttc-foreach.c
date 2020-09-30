/* Check that assignments are handled well.
 */

#pragma strong_types, save_types, rtt_checks

#include "/inc/msg.inc"
#include "/inc/testarray.inc"

struct mystruct
{
    mapping a;
    string b;
};

<int|float> g;
void fun() {}

nosave mixed *tests = ({
    ({ "Foreach over integers with the correct type", 0,
        (:
            mixed num = 20;
            foreach (int|float i, string j: num)
            {
                fun();
            }

            return 1;
        :),
    }),
    ({ "Foreach over integers with the wrong type", TF_ERROR,
        (:
            mixed num = 20;
            foreach (string i, int j: num)
            {
                fun();
            }
        :),
    }),
    ({ "Foreach over strings with the correct type", 0,
        (:
            mixed val = "ABC";
            foreach (int|float i, string j: val)
            {
                fun();
            }

            return 1;
        :),
    }),
    ({ "Foreach over strings with the wrong type", TF_ERROR,
        (:
            mixed val = "ABC";
            foreach (string i, int j: val)
            {
                fun();
            }
        :),
    }),
    ({ "Foreach over bytes with the correct type", 0,
        (:
            mixed val = b"ABC";
            foreach (int|float i, string j: val)
            {
                fun();
            }

            return 1;
        :),
    }),
    ({ "Foreach over bytes with the wrong type", TF_ERROR,
        (:
            mixed val = b"ABC";
            foreach (string i, int j: val)
            {
                fun();
            }
        :),
    }),
    ({ "Foreach over arrays with the correct type", 0,
        (:
            mixed val = ({ ([]), "ABC", "DEF", 0 });
            foreach (mapping|string i, int j: val)
            {
                fun();
            }

            return 1;
        :),
    }),
    ({ "Foreach over arrays with the wrong type", TF_ERROR,
        (:
            mixed val = ({ ([]), "ABC", "DEF", 0 });
            foreach (int i, string j: val)
            {
                fun();
            }
        :),
    }),
    ({ "Foreach over arrays with the correct type and a global variable", 0,
        (:
            mixed val = ({ 1, 2, 3 });
            foreach (g: val)
            {
                fun();
            }

            return 1;
        :),
    }),
    ({ "Foreach over arrays with the wrong type and a global variable", TF_ERROR,
        (:
            mixed val = ({ ([]), "ABC", "DEF", 0 });
            foreach (g: val)
            {
                fun();
            }
        :),
    }),
    ({ "Foreach over structs with the correct type", 0,
        (:
            mixed val = (<mystruct> ([]), "ABC");
            foreach (mapping|string i: val)
            {
                fun();
            }

            return 1;
        :),
    }),
    ({ "Foreach over structs with the wrong type", TF_ERROR,
        (:
            mixed val = (<mystruct> ([]), "ABC");
            foreach (int|float i: val)
            {
                fun();
            }
        :),
    }),
    ({ "Foreach over mappings with the correct type", 0,
        (:
            mixed val = ([ 10: ([]), 20: "ABC", 30: "DEF" ]);
            foreach (int i, mapping|string j, symbol k: val)
            {
                fun();
            }

            return 1;
        :),
    }),
    ({ "Foreach over mappings with the wrong key type", TF_ERROR,
        (:
            mixed val = ([ 10: ([]), 20: "ABC", 30: "DEF" ]);
            foreach (string i, mixed j: val)
            {
                fun();
            }
        :),
    }),
    ({ "Foreach over mappings with the wrong value type 1", TF_ERROR,
        (:
            mixed val = ([ 10: ([]), 20: "ABC", 30: "DEF" ]);
            foreach (int i, int j: val)
            {
                fun();
            }
        :),
    }),
    ({ "Foreach over mappings with the wrong value type 2", TF_ERROR,
        (:
            mixed val = ([ 10: ([]);'x, 20: "ABC";'y, 30: "DEF";'z ]);
            foreach (int i, mapping|string j, int k: val)
            {
                fun();
            }
        :),
    }),
    ({ "Foreach over mapping refs with the correct type", 0,
        (:
            mixed val = ([ 10: ([]), 20: "ABC", 30: "DEF" ]);
            foreach (int i, mapping|string j, symbol k: &val)
            {
                fun();
            }

            return 1;
        :),
    }),
    ({ "Foreach over mapping refs with the wrong key type", TF_ERROR,
        (:
            mixed val = ([ 10: ([]), 20: "ABC", 30: "DEF" ]);
            foreach (string i, mixed j: &val)
            {
                fun();
            }
        :),
    }),
    ({ "Foreach over mapping refs with the wrong value type 1", TF_ERROR,
        (:
            mixed val = ([ 10: ([]), 20: "ABC", 30: "DEF" ]);
            foreach (int i, int j: &val)
            {
                fun();
            }
        :),
    }),
    ({ "Foreach over mapping refs with the wrong value type 2", TF_ERROR,
        (:
            mixed val = ([ 10: ([]);'x, 20: "ABC";'y, 30: "DEF";'z ]);
            foreach (int i, mapping|string j, int k: &val)
            {
                fun();
            }
        :),
    }),

});

int run_test()
{
    msg("\nRunning RTTC foreach tests:\n"
          "---------------------------\n");
    return !run_array_without_callback(tests);
}
