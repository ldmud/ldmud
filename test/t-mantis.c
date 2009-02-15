#include "/inc/base.inc"
#include "/inc/testarray.inc"
#include "/inc/gc.inc"

// Name, Flag, Function
mixed *tests = ({
    ({ "0000587-1", TF_DONTCHECKERROR,
        (: strftime(__INT_MAX__); return 1; :)
    }),
    ({ "0000587-2", TF_DONTCHECKERROR,
        (: gmtime(__INT_MAX__); return 1; :)
    }),
    ({ "0000587-3", TF_DONTCHECKERROR,
        (: localtime(__INT_MAX__); return 1; :)
    }),
    ({ "0000587-4", TF_DONTCHECKERROR,
        (: ctime(__INT_MAX__); return 1; :)
    }),
    ({ "0000587-5", TF_DONTCHECKERROR,
        (: ctime(({__INT_MAX__,1})); return 1; :)
    }),
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
    ({ "0000537-3", 0,
	(:
            {
                int a;
                { int b = 42; }
                int c;
	    
                return c==0;
            }
	:)
    }),
    ({ "0000537-4", 0,
	(:
            {
                mapping m = ([ "": 1]);
                string key;
                foreach (key, int val: &m)
                {
                    val++;
                }
                int tmp=-1;

                return m[""] == 2;
            }
	:)
    }),
  ({ "0000528", 0,
  function int () {
      return to_int(sprintf("%d",__INT_MAX__)) == __INT_MAX__;
  } }),
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
