#include "/inc/base.inc"
#include "/inc/testarray.inc"
#include "/inc/gc.inc"
#include "/inc/deep_eq.inc"

#pragma no_warn_missing_return

struct teststruct
{
    int a,b;
    mixed c;
};

mixed return_first_lvalue(mixed a, mixed b) { return &a; }
mixed global_var;

#define MAKE_TEST(VAR_NAME, OP_NAME, START_VAL, OP_VAL, RESULT) \
    ({ "Checking OP_NAME with VAR_NAME", 0,                     \
       function int() : mixed context_var = START_VAL           \
       {                                                        \
           mixed local_var = global_var = context_var;          \
           VAR_NAME OP_NAME OP_VAL;                             \
           return deep_eq(VAR_NAME, RESULT);                    \
       }                                                        \
    })

#define MAKE_OP_TEST(OP_NAME, START_VAL, OP_VAL, RESULT)        \
    MAKE_TEST(local_var, OP_NAME, START_VAL, OP_VAL, RESULT),   \
    MAKE_TEST(global_var, OP_NAME, START_VAL, OP_VAL, RESULT),  \
    MAKE_TEST(context_var, OP_NAME, START_VAL, OP_VAL, RESULT)  \

/* The RANGE argument must always correspond to [4..6] */
#define MAKE_RANGE_TEST(RANGE)                                  \
    ({ "Checking array range RANGE", 0,                         \
        function int()                                          \
        {                                                       \
            mixed arr = ({ 0, 1, 2, 3, 4, 5, 6 });              \
            if(!deep_eq(arr RANGE, ({4,5,6})))                  \
                 return 0;                                      \
            mixed result = arr RANGE = ({10});                  \
            return deep_eq(arr, ({0,1,2,3,10}))                 \
                && deep_eq(result, ({10}));                     \
        }                                                       \
    }),                                                         \
    ({ "Checking string range RANGE", 0,                        \
        function int()                                          \
        {                                                       \
            mixed str = "abcdefg";                              \
            if(str RANGE != "efg")                              \
                 return 0;                                      \
            mixed result = str RANGE = "x";                     \
            return str == "abcdx" && result == "x";             \
        }                                                       \
    })

mixed *tests = ({
    ({ "Rvalue string range",  0, (: string s = "abc"; return s[0..0] == "a"; :) }),
    ({ "Rvalue array range", 0, (: int *a = ({1,2,3}); return deep_eq(a[0..0], ({1}));:) }),

    MAKE_OP_TEST(=,1,10,10),
    MAKE_OP_TEST(+=,2,10,12),
    MAKE_OP_TEST(-=,2,10,-8),
    MAKE_OP_TEST(++,5,,6),
    MAKE_OP_TEST(--,5,,4),
    MAKE_OP_TEST(*=,({1,2,3}),2,({1,2,3,1,2,3})),
    MAKE_OP_TEST(/=,42,6,7),
    MAKE_OP_TEST(%=,42,10,2),
    MAKE_OP_TEST(&=,"ldmud","abcdef","dd"),
    MAKE_OP_TEST(&=,({1,2,3}),({3,4,5}),({3})),
    MAKE_OP_TEST(|=,({1,2,3}),({3,4,5}),({1,2,3,4,5})),
    MAKE_OP_TEST(^=,({1,2,3}),({3,4,5}),({1,2,4,5})),
    MAKE_OP_TEST(&&=,1,-1,-1),
    MAKE_OP_TEST(&&=,0,2,0),
    MAKE_OP_TEST(||=,1,2,1),
    MAKE_OP_TEST(||=,0,2,2),
    MAKE_OP_TEST(<<=,5,2,20),
    MAKE_OP_TEST(>>=,-20,2,-5),
    MAKE_OP_TEST(>>>=,-1,1,__INT_MAX__),

    MAKE_RANGE_TEST([4..6]),
    MAKE_RANGE_TEST([<3..6]),
    MAKE_RANGE_TEST([>4..6]),
    MAKE_RANGE_TEST([>-3..6]),
    MAKE_RANGE_TEST([4..<1]),
    MAKE_RANGE_TEST([<3..<1]),
    MAKE_RANGE_TEST([>4..<1]),
    MAKE_RANGE_TEST([>-3..<1]),
    MAKE_RANGE_TEST([4..>6]),
    MAKE_RANGE_TEST([<3..>6]),
    MAKE_RANGE_TEST([>4..>6]),
    MAKE_RANGE_TEST([>-3..>6]),
    MAKE_RANGE_TEST([4..>-1]),
    MAKE_RANGE_TEST([<3..>-1]),
    MAKE_RANGE_TEST([>4..>-1]),
    MAKE_RANGE_TEST([>-3..>-1]),
    MAKE_RANGE_TEST([4..]),
    MAKE_RANGE_TEST([<3..]),
    MAKE_RANGE_TEST([>4..]),
    MAKE_RANGE_TEST([>-3..]),

    ({ "Unprotected char: =", 0,  (: string str="xyz"; str[2]='x'; return str=="xyx"; :) }),
    ({ "Unprotected char: pre ++",  0,  (: string str="@@@";  int result = ++str[1];       return str == "@A@"  && result == 'A'; :) }),
    ({ "Unprotected char: pre --",  0,  (: string str="@@@";  int result = --str[1];       return str == "@?@"  && result == '?'; :) }),
    ({ "Unprotected char: post ++", 0,  (: string str="@@@";  int result = str[1]++;       return str == "@A@"  && result == '@'; :) }),
    ({ "Unprotected char: post --", 0,  (: string str="@@@";  int result = str[1]--;       return str == "@?@"  && result == '@'; :) }),
    ({ "Unprotected char: /=",      0,  (: string str="@@@";  int result = str[1] /= 2;    return str == "@ @"  && result == ' '; :) }),
    ({ "Unprotected char: &&=",     0,  (: string str="@@@";  int result = str[1] &&= 'A'; return str == "@A@"  && result == 'A'; :) }),
    ({ "Unprotected char: &&=",     0,  (: string str="@\0@"; int result = str[1] &&= 'A'; return str == "@\0@" && result == 0; :) }),
    ({ "Unprotected char: ||=",     0,  (: string str="@@@";  int result = str[1] ||= 'A'; return str == "@@@"  && result == '@'; :) }),
    ({ "Unprotected char: ||=",     0,  (: string str="@\0@"; int result = str[1] ||= 'A'; return str == "@A@"  && result == 'A'; :) }),

    ({ "Unprotected lvalue-array element [1]",   0, (: mixed a = ({0,1,2,3}); int result = a[1] = 5;            return deep_eq(a, ({0,5,2,3})) && result == 5; :) }),
    ({ "Unprotected rvalue-array element [1]",   0, (: mixed a = ({0,1,2,3}); int result = funcall(a)[1] = 5;   return deep_eq(a, ({0,5,2,3})) && result == 5; :) }),
    ({ "Unprotected last array element [1]",     0, (: int result = ({0,1,2,({3})})[1] = 5; return result == 5; :) }),
    ({ "Unprotected lvalue-array element [<2]",  0, (: mixed a = ({0,1,2,3}); int result = a[<2] = 5;           return deep_eq(a, ({0,1,5,3})) && result == 5; :) }),
    ({ "Unprotected rvalue-array element [<2]",  0, (: mixed a = ({0,1,2,3}); int result = funcall(a)[<2] = 5;  return deep_eq(a, ({0,1,5,3})) && result == 5; :) }),
    ({ "Unprotected last array element [<2]",     0, (: int result = ({0,1,2,({3})})[<2] = 5; return result == 5; :) }),
    ({ "Unprotected lvalue-array element [>1]",  0, (: mixed a = ({0,1,2,3}); int result = a[>1] = 5;           return deep_eq(a, ({0,5,2,3})) && result == 5; :) }),
    ({ "Unprotected rvalue-array element [>1]",  0, (: mixed a = ({0,1,2,3}); int result = funcall(a)[>1] = 5;  return deep_eq(a, ({0,5,2,3})) && result == 5; :) }),
    ({ "Unprotected last array element [>1]",     0, (: int result = ({0,1,2,({3})})[>1] = 5; return result == 5; :) }),
    ({ "Unprotected lvalue-array element [>-2]", 0, (: mixed a = ({0,1,2,3}); int result = a[>-2] = 5;          return deep_eq(a, ({0,1,5,3})) && result == 5; :) }),
    ({ "Unprotected rvalue-array element [>-2]", 0, (: mixed a = ({0,1,2,3}); int result = funcall(a)[>-2] = 5; return deep_eq(a, ({0,1,5,3})) && result == 5; :) }),
    ({ "Unprotected last array element [>-2]",     0, (: int result = ({0,1,2,({3})})[>-2] = 5; return result == 5; :) }),

    ({ "Unprotected lvalue-struct element",         0, (: struct teststruct s = (<teststruct> -1, -2, -3); int result = s->b = 55; return deep_eq(s, (<teststruct> -1, 55, -3)) && result == 55; :) }),
    ({ "Unprotected rvalue-struct element",         0, (: struct teststruct s = (<teststruct> -1, -2, -3); int result = funcall(s)->b = 55; return deep_eq(s, (<teststruct> -1, 55, -3)) && result == 55; :) }),
    ({ "Unprotected last rvalue-struct element",    0, (: int result = (<teststruct> -1, -2, ({-3}))->b = 55;   return result == 55; :) }),
    ({ "Unprotected lvalue-struct element by name", 0, (: mixed s = (<teststruct> -1, -2, -3); int result = s->"b" = 55; return deep_eq(s, (<teststruct> -1, 55, -3)) && result == 55; :) }),
    ({ "Unprotected rvalue-struct element by name", 0, (: mixed s = (<teststruct> -1, -2, -3); int result = funcall(s)->"b" = 55; return deep_eq(s, (<teststruct> -1, 55, -3)) && result == 55; :) }),
    ({ "Unprotected last rvalue-struct element by name", 0, (: int result = (<teststruct> -1, -2, ({-3}))->"b" = 55; return result == 55; :) }),

    ({ "Unprotected lvalue-mapping element",         0, (: mapping m = ([ "a": 'a', "b": 'b', "c": ({'c'}) ]); int result = m["b"] = 'B'; return deep_eq(m, (["a":'a',"b":'B',"c":({'c'})])) && result == 'B'; :) }),
    ({ "Unprotected rvalue-mapping element",         0, (: mapping m = ([ "a": 'a', "b": 'b', "c": ({'c'}) ]); int result = funcall(m)["b"] = 'B'; return deep_eq(m, (["a":'a',"b":'B',"c":({'c'})])) && result == 'B'; :) }),
    ({ "Unprotected last rvalue-mapping element",    0, (: int result = ([ "a": 'a', "b": 'b', "c": ({'c'}) ])["b"] = 'B'; return result == 'B'; :) }),

    ({ "Unprotected array range 1", 0, (: mixed a = ({1})*10; a[2..6]   = ({2})*5; return deep_eq(a, ({1})*2+({2})*5+({1})*3); :) }),
    ({ "Unprotected array range 2", 0, (: mixed a = ({1})*10; a[2..6] &&= ({2})*5; return deep_eq(a, ({1})*2+({2})*5+({1})*3); :) }),
    ({ "Unprotected array range 3", 0, (: mixed a = ({0,1,2,3,4}); a[0..1]= ({}); return deep_eq(a, ({2,3,4})); :) }),
    ({ "Unprotected array range 4", 0, (: mixed a = ({0,1,2,3,4}); a[2..3]= ({10,11,12,13}); return deep_eq(a, ({0,1,10,11,12,13,4})); :) }),
    ({ "Unprotected string range 1", 0, (: string a = "0123456789"; a[2..6]   = "xxxxx"; return a == "01xxxxx789"; :) }),
    ({ "Unprotected string range 2", 0, (: string a = "0123456789"; a[2..6] &&= "xxxxx"; return a == "01xxxxx789"; :) }),

    ({ "Indexing array range 1", 0, (: int* a = ({1,2,3,4,5,6}); a[1..3][1] = 7; return deep_eq(a, ({1,2,7,4,5,6})); :) }),
    ({ "Indexing array range 2", 0, (: int* a = ({1,2,3,4,5,6}); a[1..3][1..1] = ({}); return deep_eq(a, ({1,2,4,5,6})); :) }),
    ({ "Indexing string range 1", 0, (: string str = "abcdef"; str[0..3][2] = 'x'; return str == "abxdef"; :) }),
    ({ "Indexing string range 2", 0, (: string str = "abcdef"; str[0..3][2..2] = "xyz"; return str == "abxyzdef"; :) }),

    /* TODO: What should the proper results be? */
    ({ "Vanishing destinations 1a", TF_ERROR, (: mixed a = ({1}); a[0] = (a=0); :) }),
    ({ "Vanishing destinations 1b", TF_ERROR, (: mixed a = ({1}); a[a=0] = 0; :) }),
    ({ "Vanishing destinations 2a", TF_ERROR, (: mixed a = ([]); a["abc"] = (a=0); :) }),
    ({ "Vanishing destinations 2b", TF_ERROR, (: mixed a = ([]); a[a="abc"] = 0; :) }),
    ({ "Vanishing destinations 3a", TF_ERROR, (: string str = "abcd"; str[0] = (str=0); :) }),
    ({ "Vanishing destinations 3b", TF_ERROR, (: string str = "abcd"; str[str=0] = 0; :) }),
    ({ "Vanishing destinations 4", TF_DONTCHECKERROR, (: mixed a = ({1}); a[0] &&= (a=0); :) }),
    /* If the previous one didn't crash, let's try to detect its wrongdoing */
    ({ "Vanishing destinations 4b", 0,
        (:
            mixed a = ({1}),b;

            a[0] &&= ((a=0), (b=({2})), 0);
            return b[0]==2;
        :)
    }),
    ({ "Vanishing destinations 5", TF_DONTCHECKERROR, (: mixed a = ([]); a["abc"] ||= (a=0); :) }),
    ({ "Vanishing destinations 6", TF_DONTCHECKERROR, (: string str = "abcd"; str[0] &&= (str=0); :) }),
    ({ "Vanishing destinations 7", TF_DONTCHECKERROR, (: mixed a = ({1})*10; a[2..6] &&= ((a=0), ({2})*10); :) }),
    ({ "Vanishing destinations 8", 0, (: mixed a = ({1,2,3,4,5,6})[4] *= 2; return a == 10; :) }),
    ({ "Vanishing destinations 9", 0, (: mixed a = ({1,2,3,4,5,6}); mixed element = &(a[5]); a = 0; return element == 6; :) }),
    ({ "Vanishing destinations 10", 0, (: mixed a = ({1,2,3,4,5,6}); mixed range = &(a[1..4]); a = 0; range = ({7,8,9}); return deep_eq(range, ({7,8,9})); :) }),
    ({ "Vanishing destinations 11", 0, (: mixed a = "Vanishing"; mixed element = &(a[1]); a = 0; return element == 'a'; :) }),
    ({ "Vanishing destinations 12", 0, (: mixed a = "Vanishing"; mixed range = &(a[5..6]); a = 0; return range == "hi"; :) }),
    ({ "Protected locals 1", 0, (: mixed a = ({1,2,3,4,5}); int summe; map(a, (: $2+=$1 :), &summe); return summe == 15; :) }),
    ({ "Protected locals 2", 0, (: int x = 1; return &x; :) }),
    ({ "Protected elements 1", 0, (: mixed a = ({1,2,3,4,5}); funcall((: $1 = $2; $2 = $3; :), &(a[1]), &(a[2]), &(a[<2])); return deep_eq(a, ({1,3,4,4,5})); :) }),
    ({ "Protected elements 2", 0, (: mixed a = ({1,2,3,4,5}); funcall((: $1 = $2; $2 = $3; :), &(a[1]), &(a[2]), &(a[1])); return deep_eq(a, ({1,3,3,4,5})); :) }),
    ({ "Protected char 1", 0, (: string str = "xyz"; mixed c = 0 || &(str[1]); c = 'a'; return str == "xaz"; :) }),
    ({ "Protected char 2", 0, (: string str = "xyz"; mixed c1 = 0 || &(str[0]), c2 = 0 || &(str[1]); c1 = 'a'; c2 = 'b';return str == "abz"; :) }),
    ({ "Protected char 3", 0,
        (:
            string str1,str2;
            mixed c;

            str1="abcdefgh"; 
            c = (0||&(str1[3]));
            c='1';

            str2=str1;
            c='2'; /* Should only change str1. */
            return str2 != str1;
         :)
    }),
    ({ "Protected char 4", 0,
        (:
            mapping m = ([:1]);
            string str;
            mixed c;
            string *ind;

            str = "abcdefghi";
            c = (0||&(str[3]));
            c = '1';
            m[str] = "Nr. 1";
            c = '2';
            m["abc2efghi"] = "Nr. 2";

            ind = m_indices(m);
            return sizeof(m)==2 && "X"+ind[0] != "X"+ind[1];
        :)
    }),
    ({ "Protected array range 1", 0, (: mixed a = ({1,2,3,4,5}); funcall((: $1 = $2; :), &(a[1..2]), &(a[3..3])); return deep_eq(a, ({1,4,4,5})); :) }),
    ({ "Protected array range 2", 0, (: mixed a = ({1,2,3,4,5}); funcall((: $1[1] = "a"; :), &(a[1..2])); return deep_eq(a, ({1,2,"a",4,5})); :) }),
    ({ "Protected array range 3", 0, (: mixed a = ({0,1,2,3,4}); funcall((: $1 = ({}); :), &(a[0..1])); return deep_eq(a, ({2,3,4})); :) }),
    ({ "Protected array range 4", 0, (: mixed a = ({0,1,2,3,4}); funcall((: $1 = ({10,11,12,13}); :), &(a[2..3]));  return deep_eq(a, ({0,1,10,11,12,13,4})); :) }),

#if 0 /* These need linked range lvalues */
    // Some challenges:
    ({ "Protected array range 5", 0, (: mixed a = ({1,2,3,4,5}); funcall((: $1 = $3; $2 = $3; :), &(a[1..2]), &(a[3..3]), &(a[4..4])); return deep_eq(a, ({1,5,5,5})); :) }),
    ({ "Protected array range 6", 0,
       (:
           mixed a = ({1,2,3,4,5}), b = ({6,7,8,9,0});
           mixed x = &(a[1..2]), y = &(b[1..2]);

           x = &y;

           y = ({10, 11, 12});

           return deep_eq(a, ({1,10,11,12,4,5})) && deep_eq(b, ({6,10,11,12,9,0}));
       :)
    }),
#endif
    ({ "Protected string range 1", 0, (: string a = "12345"; funcall((: $1 = $2; :), &(a[1..2]), &(a[3..3])); return a == "1445"; :) }),
    ({ "Protected string range 2a", 0, (: string a = "12345"; funcall((: $1[1] = 'a'; :), &(a[1..2])); return "X" + a == "X12a45"; :) }),
    ({ "Protected string range 2b", 0, (: string a = "12345"; funcall((: $1[1] = 'a'; :), &(a[1..2])); return a == "12a45"; :) }),
#if 0 /* These need linked range lvalues */
    ({ "Protected string range 3", 0, (: string a = "12345"; funcall((: $1 = $3; $2 = $3; :), &(a[1..2]), &(a[3..3]), &(a[4..4])); return a == "1555"; :) }),
    ({ "Protected string range 4", 0,
       (:
           string a = "12345", b = "67890";
           mixed x = &(a[1..2]), y = &(b[1..2]);

           x = &y;

           y = "abc";

           return a == "1abc45" && b == "6abc90";
       :)
    }),
#endif
    ({ "Volatile protected lvalues 1", 0,
        (:
            mixed val = 10;
            mixed lv1 = 0 || &val;
            mixed lv2 = 1;

            lv2 = lv1; /* Only the value (10) should be copied. */
            lv2 = 20;

            return val == 10; /* Shouldn't have changed. */
        :)
    }),
    ({ "Volatile protected lvalues 2", 0,
        (:
            mixed val = 10;
            mixed arr1 = ({ 0 || &val});
            mixed arr2 = ({ 1 });

            arr2[0] = arr1[0]; /* Only the value (10) should be copied. */
            arr2[0] = 20;

            return val == 10; /* Shouldn't have changed. */
        :)
    }),
    ({ "Reference loops 1", 0,
       (:
           mixed a = 10;
           mixed b =  &a;
           a = &b;

           return (a==10);
       :)
    }),
    ({ "Reference loops 2", 0,
       (:
           mixed a = 10;
           mixed b = &a;
           mixed c = 20;
           mixed d = &c;
           a = &d;

           if(a!=20 || d!=20)
               return 0;

           a = 30;
           return a == 30 && b == 30 && c == 30 && d == 30;
       :)
    }),
    ({ "foreach over integer", 0,
       (:
           int a, sum;
           foreach(a: 10) { sum += a; }

           return a == 9 && sum == 45;
       :)
    }),
    ({ "foreach over singular array", 0,
       (:
           int a, sum;
           foreach(a: ({1,2,3})) { sum += a; }

           return a == 3 && sum == 6;
       :)
    }),
    ({ "foreach over array variable", 0,
       (:
           int a;
           int *arr = ({1,2,3});

           foreach(a: arr) { a *= 2; }

           return a == 6 && deep_eq(arr, ({1,2,3}));
       :)
    }),
    ({ "foreach over array reference", 0,
       (:
           int a;
           int *arr = ({1,2,3});

           foreach(a: &arr) { a *= 2; }

           return a == 6 && deep_eq(arr, ({2,4,6}));
       :)
    }),
    ({ "foreach over array range", 0,
       (:
           int a;
           int *arr = ({0,1,2,3,4,5,6});

           foreach(a: &(arr[2..4])) { a *= 2; }

           return a == 8 && deep_eq(arr, ({0,1,4,6,8,5,6}));
       :)
    }),
    ({ "foreach over singular string", 0,
       (:
           int a, i;
           foreach(a: "Hello") { i++; }

           return a == 'o' && i==5;
       :)
    }),
    ({ "foreach over string variable", 0,
       (:
           int a;
           string str = "LDMud";

           foreach(a: str) { a += 1; }
           return a == 'e' && str == "LDMud";
       :)
    }),
    ({ "foreach over string reference", 0,
       (:
           int a;
           string str = "LDMud";

           foreach(a: &str) { a += 1; }
           return a == 'e' && str == "MENve";
       :)
    }),
    ({ "foreach over string range", 0,
       (:
           int a;
           string str = "LDMud";

           foreach(a: &(str[0..2])) { a += 32; }
           return a == 'm' && str == "ldmud";
       :)
    }),
    ({ "foreach over singular struct", 0,
       (:
           int a, sum;
           foreach(a: (<teststruct> 1,2,3)) { sum += a; }

           return a == 3 && sum == 6;
       :)
    }),
    ({ "foreach over struct variable", 0,
       (:
           int a;
           struct teststruct str = (<teststruct> 1,2,3);

           foreach(a: str) { a *= 2; }

           return a == 6 && deep_eq(str, (<teststruct> 1,2,3));
       :)
    }),
    ({ "foreach over struct reference", 0,
       (:
           int a;
           struct teststruct str = (<teststruct> 1,2,3);

           foreach(a: &str) { a *= 2; }

           return a == 6 && deep_eq(str, (<teststruct> 2,4,6));
       :)
    }),
    ({ "foreach over singular mapping", 0,
       (:
           string key;
           int a, sum;
           foreach(key, a: ([ "a": 1, "b": 2, "c": 3 ])) { sum += a; }

           return member((["a","b","c"]),key) && member(([1,2,3]),a) && sum == 6;
       :)
    }),
    ({ "foreach over mapping variable", 0,
       (:
           string key; int a;
           mapping m = ([ "a": 'a', "b": 'b', "c": 'c' ]);

           foreach(key, a: m) { a -= 32; }

           return member((["a","b","c"]),key) && member("ABC",a) >= 0 && deep_eq(m, (["a":'a',"b":'b',"c":'c']));
       :)
    }),
    ({ "foreach over mapping reference", 0,
       (:
           string key; int a;
           mapping m = ([ "a": 'a', "b": 'b', "c": 'c' ]);

           foreach(key, a: &m) { a -= 32; }

           return member((["a","b","c"]),key) && member("ABC",a) >= 0 && deep_eq(m, (["a":'A',"b":'B',"c":'C']));
       :)
    }),

#if 0 /* Still needs to be done... */
    ({ "foreach with an empty body", 0,
       (:
           int a;
           foreach(a: ({2})) {}

           return a == 2;
       :)
    }),
#endif
    ({ "foreach not touching references", 0,
       (:
           int a;
           int aref = &a;
           foreach(aref: ({2})) {1+1;}
           return a == 0 && aref == 2;
       :)
    }),
    ({"foreach, why we should not touch references", 0,
       (:
           int a,b;
           foreach(int i:({&a,&b,2})) {1+1;}

           return a == 0 && b == 0;
       :)
    }),
    ({ "Special Efuns 1", 0,
       (:
           mixed a = ({ 1 });
           int nr;
           nr = sscanf("5 6","%d %d", a[0], a);
           return nr == 2 && a == 6;
       :)
    }),
    ({ "sort_array by value", 0,
       (:
           mixed arr = ({ 5,1,7,4,8,3,0 });
           mixed result = sort_array(arr, #'>);
           return deep_eq(arr, ({ 5,1,7,4,8,3,0 })) &&
                  deep_eq(result, ({ 0,1,3,4,5,7,8 }));
       :)
    }),
    ({ "sort_array by reference", 0,
       (:
           mixed arr = ({ 5,1,7,4,8,3,0 });
           mixed result = sort_array(&arr, #'>);
           return arr == result &&
                  deep_eq(result, ({ 0,1,3,4,5,7,8 }));
       :)
    }),
    ({ "sort_array by range", 0,
       (:
           mixed arr = ({ 5,1,7,4,8,3,0 });
           mixed result = sort_array(&(arr[1..5]), #'>);
           return deep_eq(arr, ({ 5,1,3,4,7,8,0 })) &&
                  deep_eq(result, ({ 1,3,4,7,8 }));
       :)
    }),
    ({ "sort_array by vanishing range", 0,
       (:
           mixed arr = ({ 5,1,7,4,8,3,0 });
           mixed result = sort_array(&(arr[1..5]), (arr=0) || #'>);
           return arr == 0 &&
                  deep_eq(result, ({ 1,3,4,7,8 }));
       :)
    }),
    ({ "reverse by array value", 0,
       (:
           mixed arr = ({ 0, 1, 2, 3, 4, 5, 6 });
           mixed result = reverse(arr);
           return deep_eq(arr, ({ 0,1,2,3,4,5,6 })) &&
                  deep_eq(result, ({ 6,5,4,3,2,1,0 }));
       :)
    }),
    ({ "reverse by string value", 0,
       (:
           mixed arr = "0123456";
           mixed result = reverse(arr);
           return arr == "0123456" && result == "6543210";
       :)
    }),
    ({ "reverse by array reference", 0,
       (:
           mixed arr = ({ 0, 1, 2, 3, 4, 5, 6 });
           mixed result = reverse(&arr);
           return arr == result && deep_eq(result, ({ 6,5,4,3,2,1,0 }));
       :)
    }),
    ({ "reverse by string reference a", 0,
       (:
           mixed arr = "0123456";
           mixed result = reverse(&arr);
           return arr == result && result+"X" == "6543210X";
       :)
    }),
    ({ "reverse by string reference b", 0,
       (:
           mixed arr = "0123456";
           mixed result = reverse(&arr);
           return arr == result && result == "6543210";
       :)
    }),
    ({ "reverse by array range", 0,
       (:
           mixed arr = ({ 0, 1, 2, 3, 4, 5, 6 });
           mixed result = reverse(&(arr[1..5]));
           return deep_eq(arr, ({ 0,5,4,3,2,1,6 })) &&
                  deep_eq(result, ({ 5,4,3,2,1 }));
       :)
    }),
    ({ "reverse by string range", 0,
       (:
           mixed arr = "0123456";
           mixed result = reverse(&(arr[1..5]));
           return arr = "0543216" && result == "54321";
       :)
    }),
    ({ "reverse by vanishing array range", 0,
       (:
           mixed arr = ({ 0, 1, 2, 3, 4, 5, 6 });
           mixed result = reverse(return_first_lvalue(&(arr[1..5]), arr=10));
           return arr == 10 &&
                  deep_eq(result, ({ 5,4,3,2,1 }));
       :)
    }),
    ({ "reverse by vanishing string range", 0,
       (:
           mixed arr = "lvalu" + to_string(({'e'}));
           mixed result = reverse(return_first_lvalue(&(arr[1..4]), arr=6));
           return arr == 6 && result == "ulav";
       :)
    }),
    ({ "referencep with a lvalue parameter", 0,
       (:
           int var;
           return funcall((: referencep(&$1) :), &var);
       :)
    }),
    ({ "referencep with a rvalue parameter", 0,
       (:
           int var;
           return !funcall((: referencep(&$1) :), var);
       :)
    }),
    ({ "referencep with a rvalue parameter, that was an lvalue", 0,
       (:
           int var;
           int ref = &var;
           return !funcall((: referencep(&$1) :), ref);
       :)
    }),
});

void run_test()
{
    msg("\nRunning test suite for lvalues:\n"
          "-------------------------------\n");

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

int f(int arg)
{
    return arg + 1;
}
