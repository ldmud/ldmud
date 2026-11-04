#define OWN_PRIVILEGE_VIOLATION
#define OWN_VALID_WRITE

#include "/inc/base.inc"
#include "/inc/testarray.inc"

int log_error_count;
mixed last_privilege;

private int has_error(mixed *result)
{
    return pointerp(result) && sizeof(result) > 1 &&
           sizeof(filter(result[1], (: pointerp($1) && sizeof($1) > 2 && $1[2] == "error" :)));
}

private int has_warning(mixed *result)
{
    return pointerp(result) && sizeof(result) > 1 &&
           sizeof(filter(result[1], (: pointerp($1) && sizeof($1) > 2 && $1[2] == "warning" :)));
}

private int mentions_file(mixed *result, string file)
{
    return pointerp(result) && sizeof(result) > 1 &&
           sizeof(filter(result[1], (: pointerp($1) && sizeof($1) && $1[0] == file :)));
}

void log_error(string file, string message, int warn, int line)
{
    log_error_count++;
}

string get_wiz_name(string file)
{
    return "compile_check";
}

int privilege_violation(string op, mixed who, varargs mixed *args)
{
    if (op == "check_compile")
    {
        last_privilege = ({ op, who }) + args;
        return objectp(who) && object_name(who) == "/master";
    }
    return 1;
}

void run_test()
{
    msg("\nRunning check_compile tests:\n"
          "----------------------------\n");

    rm("/compile_check_side_effect");

    run_array(({
        ({ "recursive cache growth retains the correct entries", 0,
           (:
               string top = "";
               mixed *result;
               for (int i = 0; i < 6; i++)
               {
                   string name = sprintf("cache_growth_%d.c", i);
                   rm(name);
                   write_file(name, sprintf("int value%d;\n", i));
                   top += sprintf("inherit \"cache_growth_%d\";\n", i);
               }
               rm("cache_growth_top.c");
               write_file("cache_growth_top.c", top);
               result = check_compile("cache_growth_top");
               for (int i = 0; i < 6; i++)
                   rm(sprintf("cache_growth_%d.c", i));
               rm("cache_growth_top.c");
               return pointerp(result) && result[0] && !has_error(result)
                      && !find_object("cache_growth_5");
           :)
        }),
        ({ "simple file compiles", 0,
           (:
               mixed *result = check_compile("/ok");
               return pointerp(result) && result[0] && !has_error(result);
           :)
        }),
        ({ "path without leading slash works", 0,
           (:
               mixed *result = check_compile("ok");
               return pointerp(result) && result[0];
           :)
        }),
        ({ "path with .c suffix works", 0,
           (:
               mixed *result = check_compile("/ok.c");
               return pointerp(result) && result[0];
           :)
        }),
        ({ "missing file fails with diagnostic", 0,
           (:
               mixed *result = check_compile("/does_not_exist");
               return pointerp(result) && !result[0] && has_error(result);
           :)
        }),
        ({ "direct syntax error is returned", 0,
           (:
               mixed *result = check_compile("/syntax_error");
               return pointerp(result) && !result[0] &&
                      has_error(result) && mentions_file(result, "/syntax_error.c");
           :)
        }),
        ({ "included syntax error preserves include filename", 0,
           (:
               mixed *result = check_compile("/include_error");
               return pointerp(result) && !result[0] &&
                      has_error(result) && mentions_file(result, "/include_error.h") &&
                      !mentions_file(result, "/include_error.h.c");
           :)
        }),
        ({ "warning-only compile still succeeds", 0,
           (:
               mixed *result = check_compile("/warn_parent");
               return pointerp(result) && result[0] &&
                      has_warning(result) && !has_error(result);
           :)
        }),
        ({ "unloaded inherited file compiles without loading objects", 0,
           (:
               mixed *result = check_compile("/good_parent");
               return pointerp(result) && result[0] &&
                      !find_object("/good_parent") &&
                      !find_object("/good_child");
           :)
        }),
        ({ "unloaded inherited syntax error is reported on dependency", 0,
           (:
               mixed *result = check_compile("/bad_parent");
               return pointerp(result) && !result[0] &&
                      has_error(result) && mentions_file(result, "/bad_child.c") &&
                      !find_object("/bad_parent") &&
                      !find_object("/bad_child");
           :)
        }),
        ({ "recursive inherit fails with diagnostic", 0,
           (:
               mixed *result = check_compile("/recursive_a");
               return pointerp(result) && !result[0] && has_error(result);
           :)
        }),
        ({ "top-level create/reset side effects do not run", 0,
           (:
               mixed *result = check_compile("/side_effect_top");
               return pointerp(result) && result[0] &&
                      file_size("/compile_check_side_effect") < 0 &&
                      !find_object("/side_effect_top");
           :)
        }),
        ({ "inherited create/reset side effects do not run", 0,
           (:
               mixed *result = check_compile("/side_effect_parent");
               return pointerp(result) && result[0] &&
                      file_size("/compile_check_side_effect") < 0 &&
                      !find_object("/side_effect_parent") &&
                      !find_object("/side_effect_child");
           :)
        }),
        ({ "compile check does not call master log_error", 0,
           (:
               int before = log_error_count;
               check_compile("/syntax_error");
               return log_error_count == before;
           :)
        }),
        ({ "compile check does not update get_error_file state", 0,
           (:
               check_compile("/syntax_error");
               return !get_error_file("compile_check", 0);
           :)
        }),
        ({ "untrusted caller is denied", 0,
           (:
               last_privilege = 0;
               mixed err = "/unpriv"->run();
               return err && stringp(err) &&
                      pointerp(last_privilege) &&
                      sizeof(last_privilege) > 2 &&
                      last_privilege[0] == "check_compile" &&
                      objectp(last_privilege[1]) &&
                      object_name(last_privilege[1]) == "/unpriv" &&
                      last_privilege[2] == "/ok";
           :)
        }),
        ({ "privilege hook receives requested path", 0,
           (:
               last_privilege = 0;
               "/unpriv"->run();
               return pointerp(last_privilege) && sizeof(last_privilege) > 2 &&
                      last_privilege[0] == "check_compile" &&
                      objectp(last_privilege[1]) &&
                      object_name(last_privilege[1]) == "/unpriv" &&
                      last_privilege[2] == "/ok";
           :)
        }),
    }), function void(int errors)
    {
        rm("/compile_check_side_effect");
        shutdown(errors);
    });
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}

int valid_write(string path, string eff_user, string fun, object caller)
{
    return 1;
}
