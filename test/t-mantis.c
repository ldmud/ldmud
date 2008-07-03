#include "/inc/base.inc"
#include "/inc/testarray.inc"
#include "/inc/gc.inc"

// Name, Flag, Function
mixed *tests = ({
    ({ "0000537-1", 0,
	(:
	    int a;
	    { int b = 42; }
	    int c;
	    
	    return c==0;
	:)
    }),
    ({ "0000537-2", 0,
	(:
	    mapping m = ([ "": 1]);
	    string key;
	    foreach (key, int val: &m)
	    {
		val++;
	    }
	    int tmp=-1;
	    
	    return m[""] == 2;
	:)
    }),
});

void run_test()
{
    msg("\nRunning Mantis test suite:\n"
          "--------------------------\n");

    run_array(tests,
	(:
	    if($1)
		shutdown(1);
	    else
		start_gc(#'shutdown);
	
	    return 0;
	:));
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
