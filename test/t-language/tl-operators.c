#include "/inc/base.inc"
#include "/inc/testarray.inc"
#include "/inc/gc.inc"
#include "/sys/debug_info.h"

// Name, Flag, Function
nosave mixed *tests = ({
    ({ "bitwise AND", 0,
       (: return (0x10f0f & 0x10f0f) == 0x10f0f; :)
    }),
    ({ "bitwise AND assignment (int)", 0,
       (: int i = 0x10f0f;
          i&=0x10f0f;
          return i == 0x10f0f; :)
    }),
    ({ "bitwise AND assignment (char)", 0,
       (: string s = "0"; // 48
          s[0]&=32;
          return s[0] == 32; :)
    }),
    ({ "bitwise OR", 0,
       (: return (0x10f0f | 0x0) == 0x10f0f; :)
    }),
    ({ "bitwise OR assignment (int)", 0,
       (: int i = 0x10f0f;
          i|=0x0;
          return i == 0x10f0f; :)
    }),
    ({ "bitwise OR assignment (char)", 0,
       (: string s = "8"; // 56
          s[0]|=1;
          return s[0] == 57; :)
    }),
    ({ "bitwise XOR 1", 0,
       (: return (0x101010 ^ 0x000000) == 0x101010; :)
    }),
    ({ "bitwise XOR 2", 0,
       (: return (0x101010 ^ 0x010101) == 0x111111; :)
    }),
    ({ "bitwise XOR assignment (int)", 0,
       (: int i = 0x101011;
          i^=0x010101;
          return i == 0x111110; :)
    }),
    ({ "bitwise XOR assignment (char)", 0,
       (: string s = "1";
          s[0] ^= 0x30;
          return s[0] == 1; :)
    }),
    ({ "Modulus", 0,
       (: return (3000010 % 3000000) == 10; :)
    }),
    ({ "Modulus by zero", 1,
       (: return (3000010 % 0) == 10; :)
    }),
    ({ "Modulus assignment (int)", 0,
       (: int i = 3000010;
          i%=3000000;
          return i == 10; :)
    }),
    ({ "Modulus assignment by zero (int)", 1,
       (: int i = 3000010;
          i%=0;
          return i == 10; :)
    }),
    ({ "Modulus assignment (char)", 0,
       (: string s = "a";
          s[0] %= 90;
          return s[0] == 7; :)
    }),
    ({ "Modulus assignment by zero (char)", 1,
       (: string s = "a";
          s[0] %= 0;
          return s[0] == 7; :)
    }),
    ({ "bitwise left shift", 0,
       (: return (1 << 30) == 1073741824; :)
    }),
    ({ "bitwise left shift (>MAX_SHIFT)", 0,
       (: return (1 << 10000) == 0; :)
    }),
    ({ "bitwise left shift assignment (int)", 0,
       (: int i = 1;
          i<<=30;
          return i == 1073741824; :)
    }),
    ({ "bitwise left shift assignment (int, >MAX_SHIFT)", 0,
       (: int i = 1;
          i<<=10000;
          return i == -__INT_MAX__-1; :)
    }),
    ({ "bitwise left shift assignment (char)", 0,
       (: string s = "0"; // 48
          s[0] <<= 1;
          return s[0] == 96; :)
    }),
    ({ "bitwise left shift assignment (char, MAX_SHIFT)", 0,
       (: string s = "0"; // 48
          s[0] <<= 10000;
          return s[0] == 0; :)
    }),
    ({ "bitwise right shift", 0,
       (: return (1073741824 >> 29) == 2; :)
    }),
    ({ "bitwise right shift (>MAX_SHIFT)", 0,
       (: return (1073741824 >> 100000) == 0; :)
    }),
    ({ "bitwise right shift assignment (int)", 0,
       (: int i = 1073741824;
          i>>=29;
          return i == 2; :)
    }),
    ({ "bitwise right shift assignment (int, >MAX_SHIFT)", 0,
       (: int i = 1073741824;
          i>>=10000;
          return i == 1073741824; :)
    }),
    ({ "bitwise right shift assignment (char)", 0,
       (: string s = "0"; // 48
          s[0] >>= 2;
          return s[0] == 12; :)
    }),
    ({ "bitwise right shift assignment (char, MAX_SHIFT)", 0,
       (: string s = "0"; // 48
          s[0] >>= 10000;
          return s[0] == 0; :)
    }),
    ({ "logical right shift (positive)", 0,
       (: return (2147483647 >>> 20) == 2047; :)
    }),
    ({ "logical right shift (negative)", 0,
#if __INT_MAX__ > 2147483647
       (: printf("%d", -2048 >>> 20); return (-2048 >>> 20) == 17592186044415; :)
#else
       (: return (-2048 >>> 20) == 4095; :)
#endif
    }),
    ({ "logical right shift assignment (positive,int)", 0,
       (: int i = 2147483647;
          i>>>=20;
          return i == 2047; :)
    }),
    ({ "logical right shift assignment (negative,int)", 0,
       (: int i = -2048;
          i>>>=20;
#if __INT_MAX__ > 2147483647
          printf("%d", i);
          return i == 17592186044415; :)
#else
          return i == 4095; :)
#endif
    }),
    ({ "logical right shift assignment (positive,char)", 0,
       (: string s = "0";
          s[0]>>>=2;
          return s[0] == 12; :)
    }),
});

int run_test()
{
    msg("\nRunning basic operators test suite:\n"
          "--------------------------\n");
    return !run_array_without_callback(tests);
}
