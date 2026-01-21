/* Test of runtime inherit handling.
 *
 * We have the following inherit structure:
 *   main
 *   +-- regular
 *   |   +-- virtual1
 *   |   +-- virtual2
 *   +-- virtual1
 */

#include "/inc/base.inc"
#include "/inc/testarray.inc"

void run_test()
{
    object ob;
    closure cl_symbol_variable, cl_compile_string;

    msg("\nRunning test for runtime inherit handling:\n"
          "------------------------------------------\n");

    ob = load_object("main");
    cl_symbol_variable = bind_lambda(#'symbol_variable, ob);
    cl_compile_string = bind_lambda(#'compile_string, ob);

    run_array(
        ({
            ({ "Global variable access in lambda() of main", 0,
               function int()
               {
                   return funcall(ob.get_main_lambda()) == "main";
               }
             }),
            ({ "Global variable access in lambda() of regular", 0,
               function int()
               {
                   return funcall(ob.get_regular_lambda()) == "regular";
               }
             }),
            ({ "Global variable access in lambda() of virtual1", 0,
               function int()
               {
                   return funcall(ob.get_virtual1_lambda()) == "virtual1";
               }
             }),
            ({ "Global variable access in lambda() of virtual2", 0,
               function int()
               {
                   return funcall(ob.get_virtual2_lambda()) == "virtual2";
               }
             }),
            ({ "Global variable access via symbol_variable() of main", 0,
               function int()
               {
                   return funcall(funcall(cl_symbol_variable, "main_var")) == "main";
               }
             }),
            ({ "Global variable access via symbol_variable() of regular", 0,
               function int()
               {
                   return funcall(funcall(cl_symbol_variable, "regular_var")) == "regular";
               }
             }),
            ({ "Global variable access via symbol_variable() of virtual1", 0,
               function int()
               {
                   return funcall(funcall(cl_symbol_variable, "virtual1_var")) == "virtual1";
               }
             }),
            ({ "Global variable access via symbol_variable() of virtual2", 0,
               function int()
               {
                   return funcall(funcall(cl_symbol_variable, "virtual2_var")) == "virtual2";
               }
             }),
            ({ "Global variable access via compile_string() of main", 0,
               function int()
               {
                   return funcall(funcall(cl_compile_string, 0, "main_var", (<compile_string_options> use_object_variables: 1))) == "main";
               }
             }),
            ({ "Global variable access via compile_string() of regular", 0,
               function int()
               {
                   return funcall(funcall(cl_compile_string, 0, "regular_var", (<compile_string_options> use_object_variables: 1))) == "regular";
               }
             }),
            ({ "Global variable access via compile_string() of virtual1", 0,
               function int()
               {
                   return funcall(funcall(cl_compile_string, 0, "virtual1_var", (<compile_string_options> use_object_variables: 1))) == "virtual1";
               }
             }),
            ({ "Global variable access via compile_string() of virtual2", 0,
               function int()
               {
                   return funcall(funcall(cl_compile_string, 0, "virtual2_var", (<compile_string_options> use_object_variables: 1))) == "virtual2";
               }
             }),
        }), function void(int errors)
        {
            shutdown(errors);
        });
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
