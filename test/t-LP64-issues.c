#include "/inc/base.inc"
#include "/inc/testarray.inc"
#include "/inc/gc.inc"
#include "/sys/debug_info.h"

// Name, Flag, Function
nosave mixed *tests = ({
// these tests can only performed if the driver is compiled for a LP64
// platform.
#if __INT_MAX__ > 2147483647
    ({ "0000770-1 (bitwise AND)", 0,
       (: return (0x1000000000 & 0x1000000000) == 0x1000000000; :)
    }),
    ({ "0000770-2 (bitwise AND assignment)", 0,
       (: int i = 0x1000000000;
          i &= 0x1000000000;
          return i == 0x1000000000; :)
    }),
    ({ "0000770-3 (bitwise OR)", 0,
       (: return (0x1000000000 | 0x0) == 0x1000000000; :)
    }),
    ({ "0000770-4 (bitwise OR assignment)", 0,
       (: int i = 0x1000000000;
          i |= 0x0;
          return i == 0x1000000000; :)
    }),
    ({ "0000770-5 (Modulus)", 0,
       (: return (216681099904 % 100000000000) == 16681099904; :)
    }),
    ({ "0000770-6 (Modulus assignment)", 0,
       (: int i = 216681099904;
          i %= 100000000000;
          return i == 16681099904; :)
    }),
    ({ "0000770-7 (bitwise XOR)", 0,
       (: return (0x10000000000 ^ 0x01000000000) == 0x11000000000; :)
    }),
    ({ "0000770-8 (bitwise XOR assignment)", 0,
       (: int i = 0x10000000000;
          i ^= 0x01000000000;
          return i == 0x11000000000; :)
    }),
#endif
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
