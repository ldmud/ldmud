/* LPC language tests.
 *
 * Tests, whether some files are compiled or not.
 * Has a list tl-*.c of files that should load
 * and a list tn-*.c of files that should not load.
 * In the former run_test() is called and should
 * return a non-zero value for success.
 */

#define OWN_RUNTIME_WARNING
#include "/inc/base.inc"
#include "/inc/gc.inc"

int got_warning, got_rt_warning;

void log_error(string file, string err, int warn)
{
    if (warn)
        got_warning++;
}


void runtime_warning(string msg, string curobj, string prog, int line, int inside_catch)
{
    got_rt_warning++;
}

int warning_occured()
{
    return got_warning;
}

int rt_warning_occured()
{
    return got_rt_warning;
}

void run_test()
{
    int errors;
    
    msg("\nRunning test for t-language:\n"
          "----------------------------\n");

    foreach(string file: get_dir("/tl-*.c"))
    {
	string err;
	int res;
	
	msg("Running Test %s...", file[0..<3]);
	
	got_warning = 0;
	got_rt_warning = 0;
	if((err = catch(res = load_object(file[0..<3])->run_test();nolog)))
	{
	    errors++;
	    msg(" FAILURE! (%s)\n", err[1..<2]);
	}
        else if(!res)
	{
	    errors++;
	    msg(" FAILURE! (Wrong result)\n");
	}
	else
	{
	    msg(" Success.\n");
	}
    }

    foreach(string file: get_dir("/tl-*.c"))
    {
        object ob;
        string err;

        msg("Running String Compiler Test %s...", file[0..<3]);

        got_warning = 0;
        got_rt_warning = 0;

        ob = find_object(file[0..<3]);
        if (ob)
            destruct(ob);

        if((err = catch(ob = load_object(file[0..<3]);nolog)))
        {
            errors++;
            msg(" FAILURE! (%s)\n", err[1..<2]);
        }
        else
        {
            int res;
            string code = read_file(file);
            string* fun = explode(code, "int run_test()");
            if (sizeof(fun) != 2 || "// Not for compile_string()" in code)
            {
                msg(" Skipped.\n");
                continue;
            }

            err = catch(res = funcall(function closure()
            {
                set_this_object(ob);
                return compile_string(0,
                    implode(regexp(explode(code,"\n"), "#include \"/sys"),"\n")+"\n"+fun[1],
                    (<compile_string_options>
                        compile_block: 1,
                        use_object_functions: 1,
                        use_object_variables: 1,
                        use_object_structs: 1
                    ));
            }));

            if (err)
            {
                errors++;
                msg(" FAILURE! (%s)\n", err[1..<2]);
            }
            else if (!res)
            {
                errors++;
                msg(" FAILURE! (Wrong result)\n");
            }
            else
            {
                msg(" Success.\n");
            }
        }
    }

    foreach(string file: get_dir("/tf-*.c"))
    {
	msg("Running Test %s...\n", file[0..<3]);
	
	if(catch(load_object(file[0..<3]);nolog))
	{
	    msg("    Success.\n");
	}
        else
	{
	    errors++;
	    msg("    FAILURE! (No error occurred.)\n");
	}
    }

    if (errors)
        shutdown(1);
    else
        start_gc(#'shutdown);
}

string *epilog(int eflag)
{
    set_driver_hook(H_INCLUDE_DIRS, ({"/sys/"}) );
    set_driver_hook(H_COMMAND, "command"); /* For the applied lfun check. */
    set_driver_hook(H_CREATE_LWOBJECT, "create_lwobject");       /* Dito. */
    set_driver_hook(H_CLEAN_UP, "clean_up");                     /* Dito. */

    run_test();
    return 0;
}
