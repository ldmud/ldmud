#include "/inc/base.inc"
#include "/inc/testarray.inc"
#include "/inc/gc.inc"
#include "/sys/rtlimits.h"
#include "/sys/driver_info.h"

private float testfloat = 3.499965555966e-01;

// Name, Flag, Function
nosave mixed *tests = ({
    ({ "0000198-1", 0, (: to_string( #'[     ) == "#'["     :) }),
    ({ "0000198-2", 0, (: to_string( #'[>..  ) == "#'[>.."  :) }),
    ({ "0000198-3", 0, (: to_string( #'[>..] ) == "#'[>..]" :) }),
    ({ "0000198-4", 0, (: to_string( #'.     ) == "#'."     :) }),
    ({ "0000198-5", 0, (: to_string( #',     ) == "#',"     :) }),
    ({ "0000198-6", 0, (: to_string( #'({    ) == "#'({"    :) }),
    ({ "0000198-7", 0, (: to_string( #'([    ) == "#'(["    :) }),
    ({ "0000198-8", 0, (: to_string( #'(<    ) == "#'(<"    :) }),
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
            int i = sizeof(num);

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
            int i = sizeof(num);
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
            int i = sizeof(num);

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
            int i = sizeof(num);
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
            int num = driver_info(DI_NUM_STRINGS_TABLED);
            foreach(int i: 10000)
                if(get_type_info(cl, 4) != "get_master_uid" ||
                   num != efun::driver_info(DI_NUM_STRINGS_TABLED))
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
    ({ "0000791-a", 0,
        (:
            // try to restore a __INT_MAX__ from a LP64 system. If there is
            // truncation, there must be a warning.
            last_rt_warning=0;
            int i = restore_value("#2:2\n9223372036854775807\n");
            if (to_string(i) != "9223372036854775807"
                && (!last_rt_warning || member(last_rt_warning,
                          "Integer value out of range in restore_svalue(). "
                          "Value was truncated!\n") == -1) )
              return 0;
            return 1;
        :)
    }),
    ({ "0000791-b", 0,
        (:
            // try to restore a too large integer (on ILP32 as well as LP64).
            // There must be a warning.
            last_rt_warning=0;
            int i = restore_value("#2:2\n922337203685477580742\n");
            if (to_string(i) != "922337203685477580742"
                && (!last_rt_warning || member(last_rt_warning,
                          "Integer value out of range in restore_svalue(). "
                          "Value was truncated!\n") == -1) )
              return 0;
            return 1;
        :)
    }),
    ({ "0000819", 0,
        (:
            // sprintf() table mode skips the last row if it's only party filled.
            return sprintf("%#6.2s\n", "A\n\nB\nC\n") == "  A  B\n     C\n";
        :)
    }),
    ({ "0000836", 0,
        (:
           // load a program that dereferences an unknown struct. Should cause
           // an error, but no crash.
           write_file(__DIR__".tmp836.c",
                      "void testfun(struct unknown a) {return a->id;}",1);
           mixed err=catch(load_object(__DIR__".tmp836.c");nolog);
           // still running? test successful...
           rm(__DIR__".tmp836.c");
           return err != 0;
        :)
    }),
    ({ "0000839", 0,
       function int ()
       {
            return sprintf("%b",1470) == "10110111110";
       }
    }),
    ({ "0000868-1", 0,
       function int ()
       {
            return funcall(lambda(0, ({#'+, 3, ({#'return, 2}) }))) == 2;
       }
    }),
    ({ "0000868-2", 0,
       function int ()
       {
            return funcall(lambda(0, ({#'=, ({ #'[, ({#'return, 2 }), 0 }), 10 }))) == 2;
       }
    }),
    ({ "0000868-3", 0,
       function int ()
       {
            return funcall(lambda(0, ({#'foreach, 'x, '({2,4,6}), ({#'return, 'x}) }))) == 2;
       }
    }),
    ({ "0000868-4", 0,
       function int ()
       {
            return funcall(lambda(0, ({#'foreach, ({'x, ({#'[, ({#'return, 2}), 0})}), ([1:'a', 2:'b', 3:'c']), 'x }))) == 2;
       }
    }),
    ({ "0000868-5", 0,
       function int ()
       {
            return funcall(lambda(0, ({#'foreach, 'x,  ({#'return, 2}), 'x }))) == 2;
       }
    }),
    ({ "0000868-6", 0,
       function int ()
       {
            return funcall(lambda(0, ({#'({, 0, 1, ({#'return, 2}), 3}))) == 2;
       }
    }),
    ({ "0000868-7", 0,
       function int ()
       {
            return funcall(lambda(0, ({#'+, 1, ({#'return, ({#'({, 0, 1, ({#'return, 2}) }), }) }))) == 2;
       }
    }),
    ({ "0000868-8", 0,
       function int ()
       {
            return funcall(lambda(0, ({#'+, ([0,1,2,3]), ({(: 0 :), 0, 1, ({#'return, 2}), 3})}))) == 2;
       }
    }),
    ({ "0000868-9", 0,
       function int ()
       {
            return funcall(bind_lambda(unbound_lambda(0, ({#'implode, '({"a","b","c"}), ({#'return, 2})})))) == 2;
       }
    }),
    ({ "0000872", 0,
       function int()
       {
            terminal_colour("X \n" + ("X"*3), ([]), 2);
            return 1;
       }
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
