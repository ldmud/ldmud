/* LPC language tests.
 *
 * Tests, whether some files are compiled or not.
 * Has a list tl-*.c of files that should load
 * and a list tn-*.c of files that should not load.
 * In the former run_test() is called and should
 * return a non-zero value for success.
 */

#include "/inc/base.inc"

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
	string err;
	
	msg("Running Test %s...\n", file[0..<3]);
	
	if((err = catch(load_object(file[0..<3]);nolog)))
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
    run_test();
    return 0;
}
