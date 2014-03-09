/* Test of the garbage collection.
 *
 * We'll load files 'before-*.c', do a garbage_collection
 * and then load the 'after-*.c' files. In the later run_test()
 * is called and should return a non-zero value for success.
 * Also the garbage_collection should not find any lost blocks.
 */

#include "/inc/base.inc"
#include "/inc/gc.inc"

void run_after(int errors)
{
    foreach(string file: get_dir("/after-*.c"))
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

    shutdown(errors && 1);
}

void run_test()
{
    int errors;
    
    msg("\nRunning test for t-gc:\n"
          "----------------------\n");

    foreach(string file: get_dir("/before-*.c"))
    {
	msg("Loading %s...\n", file[0..<3]);
	
	catch(load_object(file[0..<3])->run_test();nolog);
    }
    
    start_gc(#'run_after);
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
