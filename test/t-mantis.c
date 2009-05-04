#include "/inc/base.inc"
#include "/inc/testarray.inc"
#include "/inc/gc.inc"

private float testfloat = 3.499965555966e-01;

// Name, Flag, Function
nosave mixed *tests = ({
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
       function int ()
       {
            return to_int(sprintf("%d",__INT_MAX__)) == __INT_MAX__;
       }
    }),
    ({ "0000613", 0,
        (:
            int x = 1;
            return funcall(lambda(0,({ (: x :) })));
        :)
    }),
    ({ "0000627", 0,
        function int () {
            save_object("/t-mantis");
            restore_object("/t-mantis");
            rm("/t-mantis.o");
            return testfloat == 3.499965555966e-01;
        }
    }),
    ({ "0000630-1", 0,
       (:
            /* calc __INT_MAX__ + 1 */
            string num = to_string(__INT_MAX__);
            int i = strlen(num);

            while(i && num[i-1]=='9') num[--i] = '0';
            if(!i)
               num = "1"+num;
            else
               num[--i] += 1;

            return to_int(num) == __INT_MAX__;
       :)
    }),
    ({ "0000630-2", 0,
       (:
            /* calc 2*__INT_MAX__+1 */
            string num = to_string(__INT_MAX__);
            int i = strlen(num);
            int carry = 1;

            while(i)
            {
                --i;
                if(num[i]<'5')
                {
                    num[i] += num[i] - '0' + carry;
                    carry = 0;
                }
                else
                {
                    num[i] += num[i] - '0' - 10 + carry;
                    carry = 1;
                }
            }

            if(carry)
               num = "1"+num;

            return to_int(num) == __INT_MAX__;
       :)
    }),
    ({ "0000630-3", 0,
       (:
            /* calc __INT_MIN__ - 1 */
            string num = to_string(__INT_MIN__)[1..];
            int i = strlen(num);

            while(i && num[i-1]=='9') num[--i] = '0';
            if(!i)
               num = "1"+num;
            else
               num[--i] += 1;

            return to_int("-"+num) == __INT_MIN__;
       :)
    }),
    ({ "0000630-4", 0,
       (:
            /* calc 2*__INT_MIN__-1 */
            string num = to_string(__INT_MIN__)[1..];
            int i = strlen(num);
            int carry = 1;

            while(i)
            {
                --i;
                if(num[i]<'5')
                {
                    num[i] += num[i] - '0' + carry;
                    carry = 0;
                }
                else
                {
                    num[i] += num[i] - '0' - 10 + carry;
                    carry = 1;
                }
            }

            if(carry)
               num = "1"+num;

            return to_int("-"+num) == __INT_MIN__;
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
