/* Test of the garbage collection.
 *
 * We'll load files 'before-*.c', do a garbage_collection
 * and then load the 'after-*.c' files. In the later run_test()
 * is called and should return a non-zero value for success.
 * Also the garbage_collection should not find any lost blocks.
 *
 * Then we start the garbage-*.c files, who test the garbage
 * collection by actually creating garbage. Here the GC just
 * shouldn't crash.
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

    foreach(string file: get_dir("/garbage-*.c"))
    {
        msg("Loading %s...\n", file[0..<3]);
        catch(load_object(file[0..<3])->run_test();nolog);
    }

    if (errors)
        shutdown(1);
    else
    {
         garbage_collection(__MASTER_OBJECT__ ".gc.log");
         call_out(function void()
         {
            rm(__MASTER_OBJECT__ ".gc.log");
            shutdown(0);
         }, __ALARM_TIME__);

        start_gc(function void(int result) { shutdown(0); });
    }
}

void run_test()
{
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
