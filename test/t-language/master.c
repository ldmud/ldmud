/* LPC language tests.
 *
 * Tests, whether some files are compiled or not.
 * Has a list tl-*.c of files that should load
 * and a list tn-*.c of files that should not load.
 * In the former run_test() is called and should
 * return a non-zero value for success.
 */

#include "/inc/base.inc"

int got_warning;

void log_error(string file, string err, int warn)
{
    if (warn)
        got_warning++;
}

int warning_occured()
{
    return got_warning;
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

    shutdown(errors && 1); 
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
