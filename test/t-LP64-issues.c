#include "/inc/base.inc"
#include "/inc/testarray.inc"
#include "/inc/gc.inc"
#include "/sys/debug_info.h"

// Name, Flag, Function
nosave mixed *tests = ({
    ({ "0000770-1", 0,
       (: return (0x1000000000 & 0x1000000000) == 0x1000000000; :)
    }),
    ({ "0000770-2", 0,
       (: return (0x1000000000 | 0x0) == 0x1000000000; :)
    }),
    ({ "0000770-3", 0,
       (: return (3000000010 % 3000000000) == 10; :)
    }),
    ({ "0000770-4", 0,
       (: return (1 << 62) == 4611686018427387904; :)
    }),
    ({ "0000770-5", 0,
       (: return (4611686018427387904 >> 61) == 2; :)
    }),
    ({ "0000770-5", 0,
       (: return (4611686018427387904 >> 61) == 2; :)
    }),
    ({ "0000770-6", 0,
       (: int i = 4611686018427387904;
          i>>=61;
          return i == 2; :)
    }),
    ({ "0000770-7", 0,
       (: int i = 1;
          i<<=62;
          return i == 4611686018427387904; :)
    }),
});

void run_test()
{
    msg("\nRunning LP64 issues test suite:\n"
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
// these tests can only performed if the driver is compiled for a LP64
// platform.
#if __INT_MAX__ > 2147483647
    run_test();
#else
    shutdown(0);
#endif
    return 0;
}
