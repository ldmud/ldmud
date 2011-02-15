#include "/inc/base.inc"
#include "/inc/testarray.inc"
#include "/inc/gc.inc"
#include "/sys/debug_info.h"
#include "/sys/rtlimits.h"

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
    ({ "0000612", 0,
        (:
            return pointerp(funcall(#'tests, 42, (["Ignore me"])));
        :)
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
    ({ "0000631-1", 0,
        (:
            int i=1;

            return funcall(
                (:
                    if(i)
                        return funcall((: return i; :));
                :)) == 1;
        :)
    }),
    ({ "0000631-2", 0,
        (:
            int i=1;

            return funcall(
                function int() : int j = 2
                {
                    if(i)
                        return funcall((: return j; :));
                }) == 2;
        :)
    }),
    ({ "0000631-3", 0,
        (:
            int i=1;

            return funcall(
                function int()
                {
                    int j = 2;
                    return funcall((: return i; :));
                }) == 1;
        :)
    }),
    ({ "0000631-4", 0,
        (:
            int i=1;

            return funcall(
                function int ()
                {
                    return funcall(
                         function int () : int j=i
                         {
                             return j;
                         });
                });
        :)
    }),
    ({ "0000631-5", 0,
        (:
            int a = 1;

            return funcall(
                function int() : closure cl = (: a :)
                {
                    return funcall(cl);
                }) == 1;
        :)
    }),
    ({ "0000631-6", 0,
        (:
            int a=1;

            return funcall(
                function int () : int b = a+1; int c = b+1
                {
                    return c;
                }) == 3;
        :)
    }),
    ({ "0000631-7", 0, /* #631 in conjunction with #537. */
        (:
            closure c;

            {
               int a = 1;
            }

            c = function int () : int b
            {
                return b;
            };

            return funcall(c) == 0;
        :)
    }),
    ({ "0000631-8", 0, /* #631 in conjunction with #537. */
        (:
            closure c;

            c = function int () : int b = 2
            {
                return b;
            };

            {
                int d;

                return d == 0;
            }

        :)
    }),
    ({ "0000631-9", 0, /* Check, that the array is only freed once. */
        (:
            closure c = function int() : string* a = ({ "" })
            {
                return 0;
            };

            return closurep(c); /* As long as it doesn't crash... */
        :)
    }),
    ({ "0000631-10", 0,
        function int(int a, int b, int c)
        {

            return funcall(funcall(funcall(
                function :
                    mixed i = 1;
                    mixed j = function :
                                 mixed j = function { return i; }
                              { return j; };
                { return j; }
                ))) == 1;
        }
    }),
    ({ "0000631-11", 0,
        (:
            return funcall(function int() :
                    closure c = function int() : int a = 1 { return 2; };
                    int b;
                {
                    return b;
                }) == 0;
        :)
    }),
#ifdef __XML_DOM__
    ({ "0000671", 0,
        (:
            return stringp(xml_generate(({ "abc", ([ "xyz" : "cde" ]), 0 })));
        :)
    }),
    ({ "0000683", 0,
        (:
            foreach(int i: 1000)
                catch(xml_generate(({ "a", "b", "c" })));
            return 1; // It didn't crash...
        :)
    }),
#endif // __XML_DOM__
    ({ "0000689", 0,
        (:
            // Each pass will crash with approx. 40% propability.
            // So let's make that a sure thing:
            foreach(int i: 1000)
            {
                mapping xm = ([
                    clone_object(this_object()),
                    clone_object(this_object())
                ]);
                object * x = m_indices(xm);

                destruct(x[0]);
                destruct(x[1]);

                // This will crash when there is a destructed object with the
                // same hash value as '0' and would have another hash value
                // with being '0' instead of a destructed object.
                m_delete(mkmapping(x),0);
            }

            return 1;
        :)
    }),
    ({ "0000712", 0,
        (:
            // If get_type_info() looses refcounts to the function name, this
            // will cause the string "get_master_uid" to be free'd
            // erroneously - reducing the number of tabled strings. This must
            // not happen here.
            closure cl = #'get_master_uid;
            int num = debug_info(DINFO_DATA, DID_STATUS, DID_ST_TABLED);
            foreach(int i: 10000)
                if(get_type_info(cl, 4) != "get_master_uid" ||
                   num != debug_info(DINFO_DATA, DID_STATUS, DID_ST_TABLED))
                    return 0;
            return 1;
        :)
    }),
    ({ "0000302", TF_ERROR,
        (:
           // must raise an error in the 19th iteration when it exceeds
           // LIMIT_MEMORY.
           limited(function void (void) {
              string s="a";
              foreach(int i: 20) {
                s+=s;
              }
           }, LIMIT_MEMORY, 500000);
           return 1; // never reached
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
