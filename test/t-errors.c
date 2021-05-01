#define OWN_PRIVILEGE_VIOLATION

#include "/inc/base.inc"
#include "/inc/testarray.inc"
#include "/inc/gc.inc"

mixed globalvar;
object unpriv;

// Used later in a test:
mixed array = ({42, function void() : int destroy_that = 43 {} });

// Name, Flag, Function
mixed *tests = ({
    ({ "b-980420", 0,
	(:
	   sprintf("%.5f",
	   99999999129532841420853412051438322515563209673591926319148179520720936003174400.00000 *
	   99999999129532841420853412051438322515563209673591926319148179520720936003174400.00000)
	:)
    }),
    ({ "b-980922-0", 0,
       (:
          mixed x;
	  x=1.0;
	  return to_int(x*1)==1;
       :)
    }),
    ({ "b-980922-1", 0,
       (:
          mixed x,y,z;
	  m_contains(&x, &y, &z, ([ "abc":1;2;3]), "abc");
	  return (x==1) && (y==2) && (z==3);
       :)
    }),
    ({ "b-980925-0", 0,
       (:
          set_bit( "", 1201)
       :)
    }),
    ({ "b-981202-2", 0,
       (:
          for(int i=0;i<500;i++)
	      regreplace("acc", "a*", "yy", 1);
	  return 1;
       :)
    }),
    ({ "inspired by 3.2.6dev74", 0,
       (:
          mixed res = map(
	    ({clone_object(this_object()), clone_object(this_object())}),
	    #'destruct);
	   return !(res[0] || res[1]);
        :)
    }),
    ({ "3.2.6dev68", 0,
       (:
          mixed arr = '({1,2});
	  return copy(arr)!=arr;
       :)
    }),
    ({ "3.2.6dev68", 0,
       (:
          mixed arr = '({ ({1,2}), '({1,2})});
	  mixed arr2 = unquote(deep_copy(arr));
	  arr = unquote(arr);
	  return arr[0]!=arr2[0] && arr[1]!=arr2[1]; 
       :)
    }),
    ({ "3.2.6.dev49, b-990217-2", 0,
       (:
          object o = clone_object(this_object());
	  mapping m = ([o:1]);
	  destruct(o);
	  return !sizeof(m_indices(m));
       :)
    }),
    ({ "b-990203-5", 0,
       (:
          mixed ignore;
	  globalvar = ({ ({ }) });
	  for(int i=6000;i--;)
	  {
              ignore = globalvar[0];
              ignore+= ({i});
	      globalvar[0] = ignore;
	  }
	  for(int i=6000; i--;)
	      if(globalvar[0][i]!=5999-i)
	          return 0;
	  return 1;
       :)
    }),
    ({ "b-990204-3", 0, (: funcall(lambda(0,({#',,({#'++, 'x}),'x})))==1 :) }),
    ({ "b-990204-3", 0, (: funcall(lambda(0,({#',,({#'--, 'x}),'x})))==-1 :) }),
    ({ "b-990210", 0, (: terminal_colour("foobar bla", ([ ]), 9, 2)=="foobar\n  bla" :) }),
    ({ "sprintf-INT_MIN", 0, (: sprintf("%Q",__INT_MIN__) == to_string(__INT_MIN__) :) }),
    ({ "send_udp leak", 0, (: funcall(bind_lambda(#'send_udp, unpriv), "127.0.0.1",9999, ({ 1 })); return 1; :) }),
    ({ "present_clone string corruption", 0,
       (: string a="item#123"; present_clone(a); return a[4..]=="#123"; :)
    }),
    ({ "string range memory corruption 1", 0,
       (: string a="abcdefg"; funcall((: $1 = "y"; :), &(a[0..1])); return a == "ycdefg"; :)
    }),
    ({ "string range memory corruption 2", 0,
       (: string a="abcdefg"; funcall((: $1 = "y"; :), &(a[1..2])); return a == "aydefg"; :)
    }),
    ({ "string range memory corruption 3", 0,
       (: string a="abcdefg"; funcall((: $1 = "y"; :), &(a[1..2]),(a="abc")); return a == "abc"; :)
    }),
    ({ "Lambda error message", 0,
       (: closure c = lambda(0, ({#'/, 0, 0})); call_out(c, 0); return 1; :)
   }),
   ({ "Stack corruption due to a context variable in global context", 0,
      (:
          return array[0] == 42;
      :)
   }),
   ({ "Length miscalculation in terminal_colour() with indent.", 0,
      (:
          terminal_colour("X\n ", 0, 80, 8);
          return 1;
      :)
   }),
   ({ "Memory leak with default functions.", 0,
      (:
          // We provoke a runtime error in the default function.
          set_driver_hook(H_DEFAULT_METHOD, function int(mixed result, object ob, string fun, varargs mixed* args)
          {
             raise_error("create garbage.\n");
          });

          // Call the function
          catch(clone_object(this_object())->missing_function());

          set_driver_hook(H_DEFAULT_METHOD, 0);

          // And let the GC at the end do the check.
          return 1;
      :)
   }),
   ({ "Crash in reentrant SQLite calls.", TF_ERROR,
      (:
          unpriv.sl_check1();
      :)
   }),
});

void run_test()
{
    msg("\nRunning old error test suite:\n"
          "-----------------------------\n");

    unpriv = clone_object(this_object());

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

int privilege_violation(string op, mixed who, mixed arg, mixed arg2)
{
    switch (op)
    {
        case "sqlite_pragma":
            who.sl_check2();
            return 0;
    }
    return 1;
}

void sl_check1()
{
    sl_open(__FILE__ + ".db");
    sl_exec("PRAGMA user_version");
}

void sl_check2()
{
    /* Shouldn't crash or report Valgrind errors. */
    sl_close();
}
