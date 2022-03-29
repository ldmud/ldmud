#include "/inc/base.inc"
#include "/inc/testarray.inc"
#include "/inc/gc.inc"
#include "/inc/deep_eq.inc"

#include "/sys/driver_info.h"

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

#define MAKE_LAMBDA_TEST(VAR_NAME, OP_NAME, START_VAL, OP_VAL, RESULT)                             \
    ({ "Lambda: Checking OP_NAME with VAR_NAME", 0,                                                \
       lambda(0,                                                                                   \
        ({#',,                                                                                     \
            ({ #'=, 'local_var, ({ #'=, #'global_var, START_VAL }) }),                             \
            ({ OP_NAME, VAR_NAME, OP_VAL }),                                                       \
            ({ #'return, ({#'deep_eq, symbolp(VAR_NAME) ? VAR_NAME : ({ VAR_NAME }), RESULT }) }), \
        })),                                                                                       \
    })

#define MAKE_LAMBDA_OP_TEST(OP_NAME, START_VAL, OP_VAL, RESULT)                                    \
    MAKE_LAMBDA_TEST('local_var, OP_NAME, START_VAL, OP_VAL, RESULT),                              \
    MAKE_LAMBDA_TEST(#'global_var, OP_NAME, START_VAL, OP_VAL, RESULT)

/* The RANGE argument must always correspond to [4..6] */
#define MAKE_LAMBDA_RANGE_TEST(OP_RANGE, INDICES)                                                  \
    ({ "Lambda: Checking array range OP_RANGE with INDICES", 0,                                    \
        lambda(0,                                                                                  \
         ({#',,                                                                                    \
            ({ #'=, 'arr, '({ 0, 1, 2, 3, 4, 5, 6 }) }),                                           \
            ({ #'?!,                                                                               \
                ({#'deep_eq, ({OP_RANGE, 'arr }) + INDICES, '({4,5,6}) }), 0,                      \
                ({ #',,                                                                            \
                    ({ #'=, 'result, ({ #'=, ({ OP_RANGE, 'arr }) + INDICES, '({10}) }) }),        \
                    ({ #'&&,                                                                       \
                        ({ #'deep_eq, 'arr, '({0,1,2,3,10}) }),                                    \
                        ({ #'deep_eq, 'result, '({10}) }),                                         \
                    })                                                                             \
                })                                                                                 \
            })                                                                                     \
        })),                                                                                       \
    }),                                                                                            \
    ({ "Lambda: Checking string range OP_RANGE with INDICES", 0,                                   \
        lambda(0,                                                                                  \
         ({#',,                                                                                    \
            ({ #'=, 'str, "abcdefg" }),                                                            \
            ({ #'?,                                                                                \
                ({#'!=, ({OP_RANGE, 'str }) + INDICES, "efg" }), 0,                                \
                ({ #',,                                                                            \
                    ({ #'=, 'result, ({ #'=, ({ OP_RANGE, 'str }) + INDICES, "x" }) }),            \
                    ({ #'&&,                                                                       \
                        ({ #'==, 'str, "abcdx" }),                                                 \
                        ({ #'==, 'result, "x" }),                                                  \
                    })                                                                             \
                })                                                                                 \
            })                                                                                     \
        })),                                                                                       \
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

    ({ "Unprotected strict lvalue-struct element",               0, (: struct teststruct s = (<teststruct> -1, -2, -3); int result = s.b  = 55; return deep_eq(s, (<teststruct> -1, 55, -3)) && result == 55; :) }),
    ({ "Unprotected relaxed lvalue-struct element",              0, (: struct teststruct s = (<teststruct> -1, -2, -3); int result = s->b = 55; return deep_eq(s, (<teststruct> -1, 55, -3)) && result == 55; :) }),
    ({ "Unprotected strict rvalue-struct element",               0, (: struct teststruct s = (<teststruct> -1, -2, -3); int result = funcall(s).b  = 55; return deep_eq(s, (<teststruct> -1, 55, -3)) && result == 55; :) }),
    ({ "Unprotected relaxed rvalue-struct element",              0, (: struct teststruct s = (<teststruct> -1, -2, -3); int result = funcall(s)->b = 55; return deep_eq(s, (<teststruct> -1, 55, -3)) && result == 55; :) }),
    ({ "Unprotected strict last rvalue-struct element",          0, (: int result = (<teststruct> -1, -2, ({-3})).b  = 55;   return result == 55; :) }),
    ({ "Unprotected relaxed last rvalue-struct element",         0, (: int result = (<teststruct> -1, -2, ({-3}))->b = 55;   return result == 55; :) }),
    ({ "Unprotected strict lvalue-struct element by name",       0, (: mixed s = (<teststruct> -1, -2, -3); int result = s."b" = 55; return deep_eq(s, (<teststruct> -1, 55, -3)) && result == 55; :) }),
    ({ "Unprotected relaxed rvalue-struct element by name",      0, (: mixed s = (<teststruct> -1, -2, -3); int result = funcall(s)->"b" = 55; return deep_eq(s, (<teststruct> -1, 55, -3)) && result == 55; :) }),
    ({ "Unprotected strict last rvalue-struct element by name",  0, (: int result = (<teststruct> -1, -2, ({-3}))."b" = 55; return result == 55; :) }),
    ({ "Unprotected relaxed last rvalue-struct element by name", 0, (: int result = (<teststruct> -1, -2, ({-3}))->"b" = 55; return result == 55; :) }),
    ({ "Unprotected strict non-existant lvalue-struct element",  TF_ERROR, (: struct teststruct s = (<teststruct> -1, -2, -3); int result = s.("d")  = 55; return deep_eq(s, (<teststruct> -1, -2, -3)) && result == 55; :) }),
    ({ "Unprotected relaxed non-existant lvalue-struct element", 0,        (: struct teststruct s = (<teststruct> -1, -2, -3); int result = s->("d") = 55; return deep_eq(s, (<teststruct> -1, -2, -3)) && result == 55; :) }),

    ({ "Unprotected lvalue-mapping element 1",      0, (: mapping m = ([ "a": 'a', "b": 'b', "c": ({'c'}) ]); int result = m["b"]    = 'B'; return deep_eq(m, (["a":'a',"b":'B',"c":({'c'})])) && result == 'B'; :) }),
    ({ "Unprotected lvalue-mapping element 2",      0, (: mapping m = ([ "a": 'a', "b": 'b', "c": ({'c'}) ]); int result = m["b", 0] = 'B'; return deep_eq(m, (["a":'a',"b":'B',"c":({'c'})])) && result == 'B'; :) }),
    ({ "Unprotected rvalue-mapping element 1",      0, (: mapping m = ([ "a": 'a', "b": 'b', "c": ({'c'}) ]); int result = funcall(m)["b"]    = 'B'; return deep_eq(m, (["a":'a',"b":'B',"c":({'c'})])) && result == 'B'; :) }),
    ({ "Unprotected rvalue-mapping element 2",      0, (: mapping m = ([ "a": 'a', "b": 'b', "c": ({'c'}) ]); int result = funcall(m)["b", 0] = 'B'; return deep_eq(m, (["a":'a',"b":'B',"c":({'c'})])) && result == 'B'; :) }),
    ({ "Unprotected last rvalue-mapping element 1", 0, (: int result = ([ "a": 'a', "b": 'b', "c": ({'c'}) ])["b"]    = 'B'; return result == 'B'; :) }),
    ({ "Unprotected last rvalue-mapping element 2", 0, (: int result = ([ "a": 'a', "b": 'b', "c": ({'c'}) ])["b", 0] = 'B'; return result == 'B'; :) }),

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

    ({ "Protected strict struct non-existant member lvalue", TF_ERROR,
        (:
            struct teststruct s = (<teststruct> -1, -2, -3);
            int result = &(s.("d"));
        :)
    }),
    ({ "Protected relaxed struct non-existant member lvalue", 0,
        (:
            struct teststruct s = (<teststruct> -1, -2, -3);
            int result = &(s->("d"));

            // Non existant member shall return 0.
            if (result != 0)
                return 0;

            result = 55;

            // Any write access shall not change our struct.
            if (!deep_eq(s, (<teststruct> -1, -2, -3)))
                return 0;

            // Any further non-existant member access
            // shall not disturb our temporary lvalue.
            s->("e") = 99;
            return result == 55;
        :)
    }),

    ({ "Protected array range 1", 0, (: mixed a = ({1,2,3,4,5}); funcall((: $1 = $2; :), &(a[1..2]), &(a[3..3])); return deep_eq(a, ({1,4,4,5})); :) }),
    ({ "Protected array range 2", 0, (: mixed a = ({1,2,3,4,5}); funcall((: $1[1] = "a"; :), &(a[1..2])); return deep_eq(a, ({1,2,"a",4,5})); :) }),
    ({ "Protected array range 3", 0, (: mixed a = ({0,1,2,3,4}); funcall((: $1 = ({}); :), &(a[0..1])); return deep_eq(a, ({2,3,4})); :) }),
    ({ "Protected array range 4", 0, (: mixed a = ({0,1,2,3,4}); funcall((: $1 = ({10,11,12,13}); :), &(a[2..3]));  return deep_eq(a, ({0,1,10,11,12,13,4})); :) }),
    ({ "Protected array range 5", 0, (: mixed a = ({1,2,3,4,5}); funcall((: $1 = $3; $2 = $3; :), &(a[2..2]), &(a[3..3]), &(a[4..4])); return deep_eq(a, ({1,2,5,5,5})); :) }),
    ({ "Protected array range 6", 0,
       (:
           mixed a = ({1,2,3,4,5}), b = ({6,7,8,9,0});
           mixed x = &(a[1..2]), y = &(b[1..2]);

           x = &y;

           y = ({10, 11}); /* Size changes would break the link. */

           return deep_eq(a, ({1,10,11,4,5})) && deep_eq(b, ({6,10,11,9,0}));
       :)
    }),
    ({ "Protected array range 7", 0,
       (:
           mixed a = ({1,2,3,4,5}), b = ({6,7,8,9,0});
           mixed x = &(a[1..2]), y = &(b[1..2]);

           x = &y;

           x[0] = 100;

           return deep_eq(a, ({1,100,8,4,5})) && deep_eq(b, ({6,100,8,9,0}));
       :)
    }),
    ({ "Protected string range 1", 0, (: string a = "12345"; funcall((: $1 = $2; :), &(a[1..2]), &(a[3..3])); return a == "1445"; :) }),
    ({ "Protected string range 2a", 0, (: string a = "12345"; funcall((: $1[1] = 'a'; :), &(a[1..2])); return "X" + a == "X12a45"; :) }),
    ({ "Protected string range 2b", 0, (: string a = "12345"; funcall((: $1[1] = 'a'; :), &(a[1..2])); return a == "12a45"; :) }),
    ({ "Protected string range 3", 0, (: string a = "12345"; funcall((: $1 = $3; $2 = $3; :), &(a[1..2]), &(a[3..3]), &(a[4..4])); return a == "1555"; :) }),
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
    ({ "Volatile return lvalues 1", 0,
        (:
            mixed val = 10;
            mixed result = return_first_lvalue(&val, 0);

            /* result should not be a reference to val. */
            result = 12;
            return val == 10;
        :)
    }),
    ({ "Volatile return lvalues 2", 0,
        (:
            mixed val = 10;
            mixed result = funcall(function mixed() : mixed ref = &val { return &ref; });

            /* result should not be a reference to val. */
            result = 12;
            return val == 10;
        :)
    }),
    ({ "Volatile array elements 1", 0,
        (:
            int  a = 1;
            int* b = ({&a});
            int* c = copy(b);
            a = 2;

            return c[0] == 1;
        :)
    }),
    ({ "Volatile array elements 2", 0,
        (:
            int  a = 1;
            int* b = ({&a});
            int* c = b + ({});
            a = 2;

            return c[0] == 1;
        :)
    }),
    ({ "Volatile array elements 3", 0,
        (:
            int  a = 1;
            int* b = ({&a});
            int* c = ({}) + b;
            a = 2;

            return c[0] == 1;
        :)
    }),
    ({ "Volatile array elements 4", 0,
        (:
            int  a = 1;
            int* b = ({&a});
            int* c = b[0..<1];
            a = 2;

            return c[0] == 1;
        :)
    }),
    ({ "Volatile array elements 5", 0,
        (:
            int  a = 1;
            int* b = ({0, &a, 2});
            int* c = b - ({ 1 });

            return deep_eq(c, ({0, 2}));
        :)
    }),
    ({ "Volatile array elements 6", 0,
        (:
            int  a = 1;
            int* b = ({0, 1, 2});
            int* c = b - ({ &a });

            return deep_eq(c, ({0, 2}));
        :)
    }),
    ({ "Volatile array elements 7", 0,
        (:
            int  a = 1;
            int* b = ({0, &a, 2});
            int* c = b - ({ 0, 2 });

            b[0] = 10;
            return a == 1;
        :)
    }),
    ({ "Dereferenced mapping keys 1", 0,
        (:
            int a = 1;
            mapping m = ([:1]);

            funcall(#'m_add, m, &a, 2);
            a = 10;

            return member(m, 1) && !member(m, 10);
        :)
    }),
    ({ "Dereferenced mapping keys 2", 0,
        (:
            int a = 1;
            mapping m = mkmapping(({&a, 2}));

            a = 10;

            return member(m, 1) && !member(m, 10);
        :)
    }),


    ({ "Protected return lvalues 1", 0,
        (:
            mixed val = 20;
            mixed result = &(return_first_lvalue(&val, 0));

            /* result should not be a reference to val. */
            result = 22;
            return val == 22;
        :)
    }),
    ({ "Protected return lvalues 2", 0,
        (:
            mixed val = 20;
            mixed result = &(funcall(function mixed() : mixed ref = &val { return &ref; }));

            /* result should not be a reference to val. */
            result = 22;
            return val == 22;
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
    ({ "Special Efuns 2", 0,
       (:
           mixed a = ({ 1 });
           int nr;
           nr = funcall(#'sscanf, "5 6","%d %d", &(a[0]), &a);
           return nr == 2 && a == 6;
       :)
    }),
    ({ "Special Efuns 3", 0,
       (:
           mixed a = ({ 1 });
           int nr;
           mixed args = ({ &(a[0]), &a });

           nr = sscanf("5 6","%d %d", args...);
           return nr == 2 && a == 6 && deep_eq(args, ({5,6}));
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
    ({
       "deep_copy with references in an array", 0,
       (:
           int a = 2017;
           mixed res = deep_copy(({&a, &a}));

           if(!deep_eq(res, ({2017, 2017})))
               return 0;
           res[0] = 2018;
           return a == 2017 && res[1] == 2018;
       :)
    }),
    ({
       "deep_copy with references in a mapping", 0,
       (:
           int a = 2017;
           mixed res = deep_copy(([1: &a, 2: &a]));

           if(!deep_eq(res, ([1: 2017, 2: 2017])))
               return 0;
           res[1] = 2018;
           return a == 2017 && res[2] == 2018;
       :)
    }),
    ({
       "deep_copy with string ranges in an array 1", 0,
       (:
           string str = "Good Luck!";
           mixed res = deep_copy(({&(str[0..3]), &(str[5..8]), &str}));

           if(!deep_eq(res, ({"Good", "Luck", "Good Luck!"})))
               return 0;
           res[0] = "Best";
           return str == "Good Luck!" && res[2] == "Best Luck!";
       :)
    }),
    ({
       "deep_copy with string ranges in an array 2", 0,
       (:
           string str = "Good Luck!";
           mixed res = deep_copy(({&str, &(str[0..3]), &(str[5..8])}));

           if(!deep_eq(res, ({"Good Luck!", "Good", "Luck"})))
               return 0;
           res[1] = "Best";
           return str == "Good Luck!" && res[0] == "Best Luck!";
       :)
    }),
    ({
       "deep_copy with char lvalues 1", 0,
       (:
           string str = "Char";
           mixed res = deep_copy(({&(str[0]), &(str[1]), &str}));

           if(!deep_eq(res, ({'C', 'h', "Char"})))
               return 0;
           res[0] = 'K';
           res[1] = 'l';
           return str == "Char" && res[2] == "Klar";
       :)
    }),
    ({
       "deep_copy with char lvalues 2", 0,
       (:
           string str = "Char";
           mixed a;
           mixed res = deep_copy(a = ({&str, &(str[0]), &(str[1])}));

           if(!deep_eq(res, ({"Char", 'C', 'h'})))
               return 0;
           res[1] = 'K';
           res[2] = 'l';
           return str == "Char" && res[0] == "Klar";
       :)
    }),
    ({
       "implode with references in the array 1", 0,
       (:
           string b = "B";
           int val = 67;
           string d = "LDMud";
           
           return implode(({"A", &b, &val, &(d[1..1])}),":") == "A:B:D";
       :)
    }),
    ({
       "implode with references in the array 2", 0,
       (:
           string s = "xyz";

           return implode(({&s, "123"}), ", ") == "xyz, 123";
       :)
    }),
    ({
       "implode with references in the array 3", 0,
       (:
           string s = "abc";

           return implode(({&(s[1..1]), "2"}), ", ") == "b, 2";
       :)
    }),
    ({
       "implode with references in the array 4", 0,
       (:
           string s = "abc";

           return implode(({&s, &(s[1..1])}), ", ") == "abc, b";
       :)
    }),
    ({
        "min with references in the array 1", 0,
        (:
            int a = 40, b = 10;
            return funcall(#'min, &a, 20, &b) == 10;
        :)
    }),
    ({
        "min with references in the array 2", 0,
        (:
            string a = "xyz", b = "abc";
            return funcall(#'min, &a, "def", &b) == "abc";
        :)
    }),
    ({
        "min with string ranges", 0,
        (:
            string a = "xabc", b = "axyz";
            return funcall(#'min, &(a[1..]), "def", &(b[1..])) == "abc";
        :)
    }),
    ({
        "max with references in the array 1", 0,
        (:
            int a = 40, b = 10;
            return funcall(#'max, &a, 20, &b) == 40;
        :)
    }),
    ({
        "max with references in the array 2", 0,
        (:
            string a = "xyz", b = "abc";
            return funcall(#'max, &a, "def", &b) == "xyz";
        :)
    }),
    ({
        "max with string ranges", 0,
        (:
            string a = "xabc", b = "axyz";
            return funcall(#'max, &(a[1..]), "def", &(b[1..])) == "xyz";
        :)
    }),
    ({
        "member with string ranges", 0,
        (:
            string str = "Hallo!";
            return member(({str, &(str[1..3]), "x"}), "all") == 1;
        :)
    }),
    ({
        "member with lvalue references 1", 0,
        (:
            mixed obj = this_object();
            return member(({&obj, obj}), this_object()) == 0;
        :)
    }),
    ({
        "member with lvalue references 2", 0,
        (:
            closure cl = (: 1 :);
            return member(({&cl, cl}), cl) == 0;
        :)
    }),
    ({
        "member with lvalue references 3", 0,
        (:
            int i = 0;
            return member(({&i, this_object()}), 0) == 0;
        :)
    }),
    ({
        "rmember with string ranges", 0,
        (:
            string str = "Hallo!";
            return rmember(({str, &(str[1..3]), "x"}), "all") == 1;
        :)
    }),
    ({
        "rmember with lvalue references 1", 0,
        (:
            mixed obj = this_object();
            return rmember(({obj, &obj}), this_object()) == 1;
        :)
    }),
    ({
        "rmember with lvalue references 2", 0,
        (:
            closure cl = (: 1 :);
            return rmember(({cl, &cl}), cl) == 1;
        :)
    }),
    ({
        "rmember with lvalue references 3", 0,
        (:
            int i = 0;
            return rmember(({this_object(), &i}), 0) == 1;
        :)
    }),
    ({
        "to_string with references in the array", 0,
        (:
            int a = 'L';
            return to_string(({&a, 'D', 'M', 'u', 'd'})) == "LDMud";
        :)
    }),
    ({
       "transpose_array with references in the array 1", 0,
       (:
           int* a1 = ({1,2,3});
           int* a2 = ({'a','b','c'});
           int** both = ({ &a1, &a2 });

           return deep_eq(transpose_array(both), ({ ({1,'a'}), ({2,'b'}), ({3,'c'}) }));
       :)
    }),
    ({
       "transpose_array with references in the array 2", 0,
       (:
           string k = "abc", v = "def";

           return deep_eq(transpose_array(({ ({ &k, "xyz" }), ({ &v, "123" }) })), ({ ({ "abc", "def" }), ({ "xyz", "123" }) }));
       :)
    }),
    ({
       "transpose_array with references in the array 3", 0,
       (:
           string *k = ({ "abc", "xyz" }), *v = ({ "def", "123" });

           return deep_eq(transpose_array(({ &k, &v })), ({ ({ "abc", "def" }), ({ "xyz", "123" }) }));
       :)
    }),
    ({
       "transpose_array with ranges in the array", 0,
       (:
           int* a = ({1,2,3,'a','b','c'});
           int** both = ({ &(a[0..2]), &(a[3..5]) });

           return deep_eq(transpose_array(both), ({ ({1,'a'}), ({2,'b'}), ({3,'c'}) }));
       :)
    }),
    ({
       "transpose_array unraveling references", 0,
       (:
           int i = 11;
           mixed * result = transpose_array(({({ &i })}));

           i = 12;
           return deep_eq(result, ({({11})}));
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
    ({ "regexp with lvalue references", 0,
       (:
           string str1 = "abcdef", str2 = "uwvxyz";

           return deep_eq(regexp(({ &str1, &str2 }), "def"), ({"abcdef"}));
       :)
    }),
    ({ "regexp with string ranges", 0,
       (:
           string str = "abcdef";

           return deep_eq(regexp(({ &(str[2..3]), &(str[1..4]) }), "^cd$"), ({"cd"}));
       :)
    }),
    ({ "save_value with lvalue references", 0,
       (:
           int var = 99;

           return explode(save_value(({&var, &var})),"\n")[1] == "({<1>=&(99,),<1>,})";
       :)
    }),
    ({ "save_value with temporary lvalue references", 0,
       (:
           int var = 98;
           int* arr = ({&var});
           &var = 97;

           /* We shouldn't see a trace of the lvalue reference. */
           return explode(save_value(arr),"\n")[1] == "({98,})";
       :)
    }),
    ({ "save_value with single lvalue references", 0,
       (:
           int var = 98;
           int* arr = ({&var});

           /* We shouldn't see a trace of the lvalue reference, either. */
           return explode(save_value(arr),"\n")[1] == "({98,})";
       :)
    }),
    ({ "save_value with lvalue references in old format", 0,
       (:
           int var = 99;
           return explode(save_value(({&var, &var}), 2),"\n")[1] == "({99,99,})";
       :)
    }),
    ({ "save_value with char lvalue references", 0,
       (:
           string str = "Hello";
           mixed c = &(str[1]);

           return explode(save_value(({&c, &c})),"\n")[1] == "({<1>=&(101,),<1>,})";
       :)
    }),
    ({ "save_value with temporary char lvalue references", 0,
       (:
           string str = "Hello";
           mixed c = &(str[2]);
           int* arr = ({&c});
           &c = 109;

           /* We shouldn't see a trace of the lvalue reference. */
           return explode(save_value(arr),"\n")[1] == "({108,})";
       :)
    }),
    ({ "save_value with single char lvalue references", 0,
       (:
           string str = "Hello";
           mixed c = &(str[2]);
           int* arr = ({&c});

           /* We shouldn't see a trace of the lvalue reference, either. */
           return explode(save_value(arr),"\n")[1] == "({108,})";
       :)
    }),
    ({ "save_value with char lvalue references in old format", 0,
       (:
           string str = "Hello";
           mixed c = &(str[1]);

           return explode(save_value(({&c, &c}), 2),"\n")[1] == "({101,101,})";
       :)
    }),
    ({ "save_value with array range lvalue references 1", 0,
       (:
           int* arr = ({0,1,2,3,4,5});
           mixed var = &(arr[1..3]);

           return explode(save_value(({arr, &arr, &var, &var})),"\n")[1]
               == "({<1>=({0,1,2,3,4,5,}),<2>=&(<1>,),<3>=&(<2>,1..3),<3>,})";
       :)
    }),
    ({ "save_value with array range lvalue references 2", 0,
       (:
           int* arr = ({0,1,2,3,4,5});
           mixed var = &(arr[1..3]);

           return explode(save_value(({&arr, &var, &var})),"\n")[1]
               == "({<1>=&(({0,1,2,3,4,5,}),),<2>=&(<1>,1..3),<2>,})";
       :)
    }),
    ({ "save_value with array range lvalue references 3", 0,
       (:
           int* arr = ({0,1,2,3,4,5});
           mixed var = &(arr[1..3]);

           return explode(save_value(({arr, &var, &var})),"\n")[1]
               == "({<1>=({0,1,2,3,4,5,}),<2>=&(<1>,1..3),<2>,})";
       :)
    }),
    ({ "save_value with array range lvalue references 4", 0,
       (:
           int* arr = ({0,1,2,3,4,5});
           mixed var = &(arr[1..3]);

           return explode(save_value(({&var, &var})),"\n")[1]
               == "({<1>=&(({1,2,3,}),),<1>,})";
       :)
    }),
    ({ "save_value with array range lvalue references 5", 0,
       (:
           int* arr = ({0,1,2,3,4,5});
           mixed var = &(arr[1..3]);

           return explode(save_value(({&var})),"\n")[1]
               == "({({1,2,3,}),})";
       :)
    }),
    ({ "save_value with array range lvalue references in old format 1", 0,
       (:
           int* arr = ({0,1,2,3,4,5});
           mixed var = &(arr[1..3]);

           return explode(save_value(({arr, &arr, &var, &var}),2),"\n")[1]
               == "({<1>=({0,1,2,3,4,5,}),<1>,<2>=({1,2,3,}),<2>,})";
       :)
    }),
    ({ "restore_value of lvalue references", 0,
       (:
           mixed* arr = restore_value("#3:2\n({<1>=&(10,),<1>,})\n");

           if(sizeof(arr) != 2 || arr[0] != 10 || arr[1] != 10)
               return 0;

           arr[0] = 12;
           return arr[1] == 12;
       :)
    }),
    ({ "restore_value of recursive lvalue references", 0,
       (:
           mixed arr = restore_value("#3:2\n<1>=&(({<1>,2,}),)\n");

           if(sizeof(arr) != 2 || sizeof(arr[0]) != 2)
               return 0;

           arr += ({3});
           if(sizeof(arr) != 3 || sizeof(arr[0]) != 3)
               return 0;

           // Break the circular reference.
           &(arr[0]) = 1;

           return 1;
       :)
    }),
    ({ "restore_value of array range", 0,
       (:
           mixed* arr = restore_value("#3:2\n({<1>=&(<2>=({10,11,12,}),1..1),<1>,<2>,})\n");

           if(sizeof(arr) != 3 || !deep_eq(arr[0],({11})) || !deep_eq(arr[1],({11})) || !deep_eq(arr[2], ({10,11,12})))
               return 0;

           arr[2][1] = 101;
           return arr[0][0] == 101 && arr[1][0] == 101;
       :)
    }),
    ({ "restore_value of a recursive array range", 0,
       (:
           mixed* arr = restore_value("#3:2\n({<1>=&(<2>=({<1>,11,12,}),1..1),<1>,<2>,})\n");

           if(sizeof(arr) != 3 || !deep_eq(arr[0],({11})) || !deep_eq(arr[1],({11})) || sizeof(arr[2]) != 3)
               return 0;

           arr[2][1] = 101;

           if(arr[0][0] != 101 || arr[1][0] != 101)
               return 0;

           // Break the circular reference.
           &(arr[2][0]) = 10;
           return deep_eq(arr[2], ({10,101,12}));
       :)
    }),
    ({ "restore_value of array range with wrong indices 1", TF_ERROR,
       (:
           restore_value("#3:2\n({<1>=&(<2>=({10,11,12,}),1..10),<1>,<2>,})\n");
       :)
    }),
    ({ "restore_value of array range with wrong indices 2", TF_ERROR,
       (:
           restore_value("#3:2\n({<1>=&(<2>=({10,11,12,}),2..0),<1>,<2>,})\n");
       :)
    }),
    ({ "restore_value of array range with wrong indices 3", TF_ERROR,
       (:
           restore_value("#3:2\n({<1>=&(<2>=({10,11,12,}),-1..0),<1>,<2>,})\n");
       :)
    }),
    ({ "dump_driver_info(DDI_OBJECTS) with lvalues", 0,
       (:
            mixed val = -1;
            global_var = ({ &val, &val });

            // And this shouldn't crash.
            dump_driver_info(DDI_OBJECTS);
            return 1;
       :)
    }),
    ({ "driver_info(DI_NUM_LVALUS)", 0,
       (:
            int old = driver_info(DI_NUM_LVALUES);
            int lv = &old;

            int new = driver_info(DI_NUM_LVALUES);
            if (new <= old)
                return 0;

            old = new;
            int* arr = ({});
            int* range = &(arr[0..-1]);

            new = driver_info(DI_NUM_LVALUES);
            if (new <= old)
                return 0;

            old = new;
            string str = "driver";
            int ch = &(str[2]);

            new = driver_info(DI_NUM_LVALUES);
            if (new <= old)
                return 0;

            return 1;
       :)
    }),
    ({
       "Array matching with lvalue references 1", 0,
       (:
           int* arr = ({1,2,3});
           string str = "Hallihallo!";
           int* arrref = &(arr[1..2]);

           return sizeof(({ &arrref, &(str[5..9]), &str, arr }) - ({ &arrref, "hallo", str, &arr })) == 0 &&
                  sizeof(({ &arrref }) - ({ arr[1..2] })) == 1;
       :)
    }),
    ({
       "Array matching with lvalue references 2", 0,
       (:
           int* arr = ({1,2,3});
           string str = "Hallihallo!";
           int* arrref = &(arr[1..2]);

           return sizeof(({ &arrref, &(str[5..9]), &str, &arr }) & ({ &arrref, "hallo", str, arr })) == 4 &&
                  sizeof(({ &arrref }) & ({ arr[1..2] })) == 0;
       :)
    }),

    ({
        "Flattening lvalue parameters with apply", 0,
        (:
            closure cl = function void(int a, int b, int c, int d) { a+=2; b+=3; c+=5; d+=7; };
            int* arr = ({0, 1, 2, 3, 4, 5});
            apply(cl, &arr);
            return deep_eq(arr, ({2,4,7,10,4,5}));
        :),
    }),
    ({
        "Flattening vanishing lvalue parameters with apply", 0,
        (:
            closure cl = function void(int a, int b, int c, int d) { a+=2; b+=3; c+=5; d+=7; };
            int* arr = ({0, 1, 2, 3, 4, 5});
            int* arr2 = ({&(arr[0]),&(arr[1]),&(arr[2]),&(arr[3]),&(arr[4]),&(arr[5])});
            apply(cl, &(arr2[0..<1][(arr2=0)..<1]));
            return deep_eq(arr, ({2,4,7,10,4,5}));
        :),
    }),
    ({
        "Flattening range lvalue parameters with apply", 0,
        (:
            closure cl = function void(int a, int b, int c, int d) { a+=2; b+=3; c+=5; d+=7; };
            int* arr = ({0, 1, 2, 3, 4, 5});
            apply(cl, &(arr[2..4]));
            return deep_eq(arr, ({0,1,4,6,9,5}));
        :),
    }),
    ({
        "Flattening lvalue parameters '...'", 0,
        (:
            closure cl = function void(int a, int b, int c, int d) { a+=2; b+=3; c+=5; d+=7; };
            int* arr = ({0, 1, 2, 3, 4, 5});
            funcall(cl, &arr...);
            return deep_eq(arr, ({2,4,7,10,4,5}));
        :),
    }),
    ({
        "Flattening vanishing lvalue parameters with '...'", 0,
        (:
            closure cl = function void(int a, int b, int c, int d) { a+=2; b+=3; c+=5; d+=7; };
            int* arr = ({0, 1, 2, 3, 4, 5});
            int* arr2 = ({&(arr[0]),&(arr[1]),&(arr[2]),&(arr[3]),&(arr[4]),&(arr[5])});
            funcall(cl, &(arr2[0..<1][(arr2=0)..<1])...);
            return deep_eq(arr, ({2,4,7,10,4,5}));
        :),
    }),
    ({
        "Flattening range lvalue parameters '...'", 0,
        (:
            closure cl = function void(int a, int b, int c, int d) { a+=2; b+=3; c+=5; d+=7; };
            int* arr = ({0, 1, 2, 3, 4, 5});
            funcall(cl, &(arr[2..4])...);
            return deep_eq(arr, ({0,1,4,6,9,5}));
        :),
    }),
    /* The same tests without any '&' should leave them alone. */
    ({
        "Flattening rvalue parameters with apply", 0,
        (:
            closure cl = function void(int a, int b, int c, int d) { a+=2; b+=3; c+=5; d+=7; };
            int* arr = ({0, 1, 2, 3, 4, 5});
            apply(cl, arr);
            return deep_eq(arr, ({0,1,2,3,4,5}));
        :),
    }),
    ({
        "Flattening vanishing rvalue parameters with apply", 0,
        (:
            closure cl = function void(int a, int b, int c, int d) { a+=2; b+=3; c+=5; d+=7; };
            int* arr = ({0, 1, 2, 3, 4, 5});
            int* arr2 = ({&(arr[0]),&(arr[1]),&(arr[2]),&(arr[3]),&(arr[4]),&(arr[5])});
            apply(cl, arr2[0..<1][(arr2=0)..<1]);
            return deep_eq(arr, ({0,1,2,3,4,5}));
        :),
    }),
    ({
        "Flattening range rvalue parameters with apply", 0,
        (:
            closure cl = function void(int a, int b, int c, int d) { a+=2; b+=3; c+=5; d+=7; };
            int* arr = ({0, 1, 2, 3, 4, 5});
            apply(cl, arr[2..4]);
            return deep_eq(arr, ({0,1,2,3,4,5}));
        :),
    }),
    ({
        "Flattening rvalue parameters '...'", 0,
        (:
            closure cl = function void(int a, int b, int c, int d) { a+=2; b+=3; c+=5; d+=7; };
            int* arr = ({0, 1, 2, 3, 4, 5});
            funcall(cl, arr...);
            return deep_eq(arr, ({0,1,2,3,4,5}));
        :),
    }),
    ({
        "Flattening vanishing rvalue parameters with '...'", 0,
        (:
            closure cl = function void(int a, int b, int c, int d) { a+=2; b+=3; c+=5; d+=7; };
            int* arr = ({0, 1, 2, 3, 4, 5});
            int* arr2 = ({&(arr[0]),&(arr[1]),&(arr[2]),&(arr[3]),&(arr[4]),&(arr[5])});
            funcall(cl, (arr2[0..<1][(arr2=0)..<1])...);
            return deep_eq(arr, ({0,1,2,3,4,5}));
        :),
    }),
    ({
        "Flattening range rvalue parameters '...'", 0,
        (:
            closure cl = function void(int a, int b, int c, int d) { a+=2; b+=3; c+=5; d+=7; };
            int* arr = ({0, 1, 2, 3, 4, 5});
            funcall(cl, (arr[2..4])...);
            return deep_eq(arr, ({0,1,2,3,4,5}));
        :),
    }),

    ({
        "Reseating of variables", 0,
        (:
            int a = 10;
            int b = 12;
            int l = &a;
            &l = &b;

            return a==10 && b = 12 && l = 12;
        :),
    }),

    ({
        "Reseating of array entry [1]", 0,
        (:
            int b = 11;
            int* arr = ({0, &b, 2, 3});

            arr[1] = 10;
            &(arr[1]) = 12;

            return deep_eq(arr, ({0, 12, 2, 3})) && b == 10;
        :),
    }),

    ({
        "Reseating of array entry [<1]", 0,
        (:
            int b = 11;
            int* arr = ({0, 1, 2, &b});

            arr[<1] = 10;
            &(arr[<1]) = 12;

            return deep_eq(arr, ({0, 1, 2, 12})) && b == 10;
        :),
    }),

    ({
        "Reseating of array entry [>1]", 0,
        (:
            int b = 11;
            int* arr = ({0, &b, 2, 3});

            arr[>1] = 10;
            &(arr[>1]) = 12;

            return deep_eq(arr, ({0, 12, 2, 3})) && b == 10;
        :),
    }),

    ({
        "Reseating of mapping entry", 0,
        (:
            int b = 11;
            mapping m = ([ "Hi": &b ]);

            m["Hi",0] = 33;
            &(m["Hi",0]) = 22;

            return deep_eq(m, (["Hi": 22])) && b == 33;
        :),
    }),

    ({
        "Reseating of struct entry", 0,
        (:
            int b = 100;
            struct teststruct s = (<teststruct> &b, 300, 500);

            s.a = 101;
            &(s.a) = 200;

            return deep_eq(s, (<teststruct> 200, 300, 500)) && b == 101;
        :),
    }),

    ({
        "Unprotected lvalues to mapping entries 1", 0,
        (:
            mapping m = ([:1]);
            int v = m["Hi"] = 22;

            return deep_eq(m, (["Hi": 22])) && v == 22;
        :)
    }),
    ({
        "Unprotected lvalues to mapping entries 2", 0,
        (:
            mapping m = ([:2]);
            int v = m["Hi",1] = 22;

            return deep_eq(m, (["Hi": 0;22])) && v == 22;
        :)
    }),
    ({
        "Unprotected lvalues to mapping entries 3", 0,
        (:
            mapping m = (["Hi": ({11}) ]);
            m["Hi"][0] = 22;

            return deep_eq(m, (["Hi": ({22}) ]));
        :)
    }),
    ({
        "Unprotected lvalues to mapping entries 4", 0,
        (:
            mapping m = (["Hi": ({1,2,3,4,5})]);
            m["Hi"][0..1] = ({22});

            return deep_eq(m, (["Hi": ({22,3,4,5}) ]));
        :)
    }),
    ({
        "Unprotected lvalues to mapping entries 5", 0,
        (:
            mapping m = ([:1]);
            catch(m["Hi"][0] = 22);

            return sizeof(m) == 0;
        :)
    }),
    ({
        "Unprotected lvalues to mapping entries 6", 0,
        (:
            mapping m = ([:1]);
            catch(m["Hi"][0..1] = ({22}));

            return sizeof(m) == 0;
        :)
    }),
    ({
        "Unprotected lvalues to mapping entries 7", 0,
        (:
            mapping m = ([:2]);
            int v = ++m["Hi",1];

            return deep_eq(m, (["Hi": 0;1])) && v == 1;
        :)
    }),
    ({
        "Unprotected lvalues to mapping entries 8", 0,
        (:
            mapping m = ([:2]);
            int v = m["Hi",1]--;

            return deep_eq(m, (["Hi": 0;-1])) && v == 0;
        :)
    }),
    ({
        "Unprotected lvalues to mapping entries 9", 0,
        (:
            mapping m = ([:1]);
            int v = m["Hi"] |= 22;

            return deep_eq(m, (["Hi": 22])) && v == 22;
        :)
    }),
    ({
        "Protected lvalues to mapping entries 1", 0,
        (:
            mapping m = ([:1]);
            int v = &(m["Hi"]);

            v = 22;
            return deep_eq(m, (["Hi": 22])) && v == 22;
        :)
    }),
    ({
        "Protected lvalues to mapping entries 2", 0,
        (:
            mapping m = ([:2]);
            int v = &(m["Hi",1]);

            v = 22;
            return deep_eq(m, (["Hi": 0;22])) && v == 22;
        :)
    }),
    ({
        "Protected lvalues to mapping entries 3", 0,
        (:
            mapping m = (["Hi": ({11})]);
            int* v = &(m["Hi"]);

            v[0] = 22;
            return deep_eq(m, (["Hi": ({22}) ])) && deep_eq(v, ({22}));
        :)
    }),
    ({
        "Protected lvalues to mapping entries 4", 0,
        (:
            mapping m = (["Hi": ({1,2,3,4,5})]);
            int* v = &(m["Hi"]);

            v[0..1] = ({22});
            return deep_eq(m, (["Hi": ({22,3,4,5}) ])) && deep_eq(v, ({22,3,4,5}));
        :)
    }),
    ({
        "Protected lvalues to mapping entries 5", 0,
        (:
            mapping m = ([:1]);
            int* v = &(m["Hi"]);

            catch(v[0] = 22);
            return sizeof(m) == 0 && v == 0;
        :)
    }),
    ({
        "Protected lvalues to mapping entries 6", 0,
        (:
            mapping m = ([:1]);
            int* v = &(m["Hi"]);

            catch(v[0..1] = ({22}));
            return sizeof(m) == 0 && v == 0;
        :)
    }),
    ({
        "Protected lvalues to mapping entries 7", 0,
        (:
            mapping m = ([:2]);
            int v = &(m["Hi",1]);

            ++v;

            return deep_eq(m, (["Hi": 0;1])) && v == 1;
        :)
    }),
    ({
        "Protected lvalues to mapping entries 8", 0,
        (:
            mapping m = ([:2]);
            int v = &(m["Hi",0]);
            int w = &(m["Hi",1]);

            v--;
            w++;

            return deep_eq(m, (["Hi": -1;1])) && v == -1 && w == 1;
        :)
    }),
    ({
        "Protected lvalues to mapping entries 9", 0,
        (:
            mapping m = ([:1]);
            int v = &(m["Hi"]);

            v |= 22;
            return deep_eq(m, (["Hi": 22])) && v == 22;
        :)
    }),
    ({
        "Protected lvalues to mapping entries 10", 0,
        (:
            mapping m = ([:1]);
            <mapping|int>* pair1 = ({ m, &(m["Hi"]), &(m["Hi"]) });
            <mapping|int>* pair2 = deep_copy(pair1);

            pair1[1] = 11;
            pair2[1] = 22;

            return deep_eq(m, (["Hi": 11])) &&
                   deep_eq(pair1, ({ (["Hi": 11]), 11, 11 })) &&
                   deep_eq(pair2, ({ (["Hi": 22]), 22, 22 }));
        :)
    }),
    ({
        "Protected lvalues to mapping entries 11", 0,
        (:
            mapping m = ([:1]);
            <mapping|int>* pair1 = ({ m, &(m["Hi"]), &(m["Hi"]) });
            <mapping|int>* pair2 = restore_value(save_value(pair1));

            pair1[1] = 11;
            pair2[1] = 22;

            return deep_eq(m, (["Hi": 11])) &&
                   deep_eq(pair1, ({ (["Hi": 11]), 11, 11 })) &&
                   deep_eq(pair2, ({ (["Hi": 22]), 22, 22 }));
        :)
    }),


    /* -------------------------------------------------------- *
     * And now the same tests again as lambda closures.         *
     * This is needed, because lambdas have their own compiler. *
     * -------------------------------------------------------- */

    ({ "Lambda: Rvalue string range",  0,
       lambda(0,
        ({#',,
            ({ #'=, 's, "abc" }),
            ({ #'return, ({#'==, ({ #'[..], 's, 0, 0 }), "a" }) }),
        })),
    }),
    ({ "Lambda: Rvalue array range", 0,
       lambda(0,
        ({#',,
            ({ #'=, 'a, '({1,2,3}) }),
            ({ #'return, ({#'deep_eq, ({ #'[..], 'a, 0, 0 }), '({1}) }) }),
        })),
    }),

    MAKE_LAMBDA_OP_TEST(#'=,1,10,10),
    MAKE_LAMBDA_OP_TEST(#'+=,2,10,12),
    MAKE_LAMBDA_OP_TEST(#'-=,2,10,-8),
    MAKE_LAMBDA_OP_TEST(#'++,5,,6),
    MAKE_LAMBDA_OP_TEST(#'--,5,,4),
    MAKE_LAMBDA_OP_TEST(#'*=,'({1,2,3}),2,'({1,2,3,1,2,3})),
    MAKE_LAMBDA_OP_TEST(#'/=,42,6,7),
    MAKE_LAMBDA_OP_TEST(#'%=,42,10,2),
    MAKE_LAMBDA_OP_TEST(#'&=,"ldmud","abcdef","dd"),
    MAKE_LAMBDA_OP_TEST(#'&=,'({1,2,3}),'({3,4,5}),'({3})),
    MAKE_LAMBDA_OP_TEST(#'|=,'({1,2,3}),'({3,4,5}),'({1,2,3,4,5})),
    MAKE_LAMBDA_OP_TEST(#'^=,'({1,2,3}),'({3,4,5}),'({1,2,4,5})),
    MAKE_LAMBDA_OP_TEST(#'&&=,1,-1,-1),
    MAKE_LAMBDA_OP_TEST(#'&&=,0,2,0),
    MAKE_LAMBDA_OP_TEST(#'||=,1,2,1),
    MAKE_LAMBDA_OP_TEST(#'||=,0,2,2),
    MAKE_LAMBDA_OP_TEST(#'<<=,5,2,20),
    MAKE_LAMBDA_OP_TEST(#'>>=,-20,2,-5),
    MAKE_LAMBDA_OP_TEST(#'>>>=,-1,1,__INT_MAX__),

    MAKE_LAMBDA_RANGE_TEST(#'[..],   ({4,6})),
    MAKE_LAMBDA_RANGE_TEST(#'[<..],  ({3,6})),
    MAKE_LAMBDA_RANGE_TEST(#'[>..],  ({4,6})),
    MAKE_LAMBDA_RANGE_TEST(#'[>..],  ({-3,6})),
    MAKE_LAMBDA_RANGE_TEST(#'[..<],  ({4,1})),
    MAKE_LAMBDA_RANGE_TEST(#'[<..<], ({3,1})),
    MAKE_LAMBDA_RANGE_TEST(#'[>..<], ({4,1})),
    MAKE_LAMBDA_RANGE_TEST(#'[>..<], ({-3,1})),
    MAKE_LAMBDA_RANGE_TEST(#'[..>],  ({4,6})),
    MAKE_LAMBDA_RANGE_TEST(#'[<..>], ({3,6})),
    MAKE_LAMBDA_RANGE_TEST(#'[>..>], ({4,6})),
    MAKE_LAMBDA_RANGE_TEST(#'[>..>], ({-3,6})),
    MAKE_LAMBDA_RANGE_TEST(#'[..>],  ({4,-1})),
    MAKE_LAMBDA_RANGE_TEST(#'[<..>], ({3,-1})),
    MAKE_LAMBDA_RANGE_TEST(#'[>..>], ({4,-1})),
    MAKE_LAMBDA_RANGE_TEST(#'[>..>], ({-3,-1})),
    MAKE_LAMBDA_RANGE_TEST(#'[..,    ({4})),
    MAKE_LAMBDA_RANGE_TEST(#'[<..,   ({3})),
    MAKE_LAMBDA_RANGE_TEST(#'[>..,   ({4})),
    MAKE_LAMBDA_RANGE_TEST(#'[>..,   ({-3})),

    ({ "Lambda: Unprotected char: =",       0,  lambda(0, ({#',, ({#'=,'str,"xyz"}),  ({#'=, ({#'[,'str,2}), 'x'}), ({#'==,'str, "xyx"}) })) }),
    ({ "Lambda: Unprotected char: post ++", 0,  lambda(0, ({#',, ({#'=,'str,"@@@"}),  ({#'=, 'result, ({#'++,  ({#'[,'str,1}) }) }),      ({#'&&, ({#'==,'str, "@A@"}),  ({#'==,'result,'@'}) }) })) }),
    ({ "Lambda: Unprotected char: post --", 0,  lambda(0, ({#',, ({#'=,'str,"@@@"}),  ({#'=, 'result, ({#'--,  ({#'[,'str,1}) }) }),      ({#'&&, ({#'==,'str, "@?@"}),  ({#'==,'result,'@'}) }) })) }),
    ({ "Lambda: Unprotected char: /=",      0,  lambda(0, ({#',, ({#'=,'str,"@@@"}),  ({#'=, 'result, ({#'/=,  ({#'[,'str,1}), 2 }) }),   ({#'&&, ({#'==,'str, "@ @"}),  ({#'==,'result,' '}) }) })) }),
    ({ "Lambda: Unprotected char: &&=",     0,  lambda(0, ({#',, ({#'=,'str,"@@@"}),  ({#'=, 'result, ({#'&&=, ({#'[,'str,1}), 'A' }) }), ({#'&&, ({#'==,'str, "@A@"}),  ({#'==,'result,'A'}) }) })) }),
    ({ "Lambda: Unprotected char: &&=",     0,  lambda(0, ({#',, ({#'=,'str,"@\0@"}), ({#'=, 'result, ({#'&&=, ({#'[,'str,1}), 'A' }) }), ({#'&&, ({#'==,'str, "@\0@"}), ({#'==,'result,0  }) }) })) }),
    ({ "Lambda: Unprotected char: ||=",     0,  lambda(0, ({#',, ({#'=,'str,"@@@"}),  ({#'=, 'result, ({#'||=, ({#'[,'str,1}), 'A' }) }), ({#'&&, ({#'==,'str, "@@@"}),  ({#'==,'result,'@'}) }) })) }),
    ({ "Lambda: Unprotected char: ||=",     0,  lambda(0, ({#',, ({#'=,'str,"@\0@"}), ({#'=, 'result, ({#'||=, ({#'[,'str,1}), 'A' }) }), ({#'&&, ({#'==,'str, "@A@"}),  ({#'==,'result,'A'}) }) })) }),

    ({ "Lambda: Unprotected lvalue-array element [1]",   0, lambda(0, ({#',, ({#'=,'a,'({0,1,2,3})}), ({#'=, 'result, ({#'=,({#'[,'a,                1}),5}) }), ({#'&&, ({#'deep_eq,'a,'({0,5,2,3})}), ({#'==,'result,5}) }) })) }),
    ({ "Lambda: Unprotected rvalue-array element [1]",   0, lambda(0, ({#',, ({#'=,'a,'({0,1,2,3})}), ({#'=, 'result, ({#'=,({#'[,({#'funcall,'a}),  1}),5}) }), ({#'&&, ({#'deep_eq,'a,'({0,5,2,3})}), ({#'==,'result,5}) }) })) }),
    ({ "Lambda: Unprotected last array element [1]",     0, lambda(0, ({#',,                          ({#'=, 'result, ({#'=,({#'[,'({0,2,3,({3})}),  1}),5}) }),                                        ({#'==,'result,5})    })) }),
    ({ "Lambda: Unprotected lvalue-array element [<2]",  0, lambda(0, ({#',, ({#'=,'a,'({0,1,2,3})}), ({#'=, 'result, ({#'=,({#'[<,'a,               2}),5}) }), ({#'&&, ({#'deep_eq,'a,'({0,1,5,3})}), ({#'==,'result,5}) }) })) }),
    ({ "Lambda: Unprotected rvalue-array element [<2]",  0, lambda(0, ({#',, ({#'=,'a,'({0,1,2,3})}), ({#'=, 'result, ({#'=,({#'[<,({#'funcall,'a}), 2}),5}) }), ({#'&&, ({#'deep_eq,'a,'({0,1,5,3})}), ({#'==,'result,5}) }) })) }),
    ({ "Lambda: Unprotected last array element [<2]",    0, lambda(0, ({#',,                          ({#'=, 'result, ({#'=,({#'[<,'({0,2,3,({3})}), 2}),5}) }),                                        ({#'==,'result,5})    })) }),
    ({ "Lambda: Unprotected lvalue-array element [>1]",  0, lambda(0, ({#',, ({#'=,'a,'({0,1,2,3})}), ({#'=, 'result, ({#'=,({#'[>,'a,               1}),5}) }), ({#'&&, ({#'deep_eq,'a,'({0,5,2,3})}), ({#'==,'result,5}) }) })) }),
    ({ "Lambda: Unprotected rvalue-array element [>1]",  0, lambda(0, ({#',, ({#'=,'a,'({0,1,2,3})}), ({#'=, 'result, ({#'=,({#'[>,({#'funcall,'a}), 1}),5}) }), ({#'&&, ({#'deep_eq,'a,'({0,5,2,3})}), ({#'==,'result,5}) }) })) }),
    ({ "Lambda: Unprotected last array element [>1]",    0, lambda(0, ({#',,                          ({#'=, 'result, ({#'=,({#'[>,'({0,2,3,({3})}), 1}),5}) }),                                        ({#'==,'result,5})    })) }),
    ({ "Lambda: Unprotected lvalue-array element [>-2]", 0, lambda(0, ({#',, ({#'=,'a,'({0,1,2,3})}), ({#'=, 'result, ({#'=,({#'[>,'a,              -2}),5}) }), ({#'&&, ({#'deep_eq,'a,'({0,1,5,3})}), ({#'==,'result,5}) }) })) }),
    ({ "Lambda: Unprotected rvalue-array element [>-2]", 0, lambda(0, ({#',, ({#'=,'a,'({0,1,2,3})}), ({#'=, 'result, ({#'=,({#'[>,({#'funcall,'a}),-2}),5}) }), ({#'&&, ({#'deep_eq,'a,'({0,1,5,3})}), ({#'==,'result,5}) }) })) }),
    ({ "Lambda: Unprotected last array element [>-2]",   0, lambda(0, ({#',,                          ({#'=, 'result, ({#'=,({#'[>,'({0,2,3,({3})}),-2}),5}) }),                                        ({#'==,'result,5})    })) }),

    ({ "Lambda: Unprotected strict lvalue-struct element by name",       0,        lambda(0, ({#',, ({#'=,'s,(<teststruct> -1,-2,-3)}), ({#'=, 'result, ({#'=,({#'., 's,                     "b"}), 55}) }), ({#'&&, ({#'deep_eq,'s,(<teststruct> -1,55,-3)}), ({#'==,'result,55}) }) })) }),
    ({ "Lambda: Unprotected relaxed lvalue-struct element by name",      0,        lambda(0, ({#',, ({#'=,'s,(<teststruct> -1,-2,-3)}), ({#'=, 'result, ({#'=,({#'->,'s,                     "b"}), 55}) }), ({#'&&, ({#'deep_eq,'s,(<teststruct> -1,55,-3)}), ({#'==,'result,55}) }) })) }),
    ({ "Lambda: Unprotected strict rvalue-struct element by name",       0,        lambda(0, ({#',, ({#'=,'s,(<teststruct> -1,-2,-3)}), ({#'=, 'result, ({#'=,({#'., ({#'funcall,'s}),       "b"}), 55}) }), ({#'&&, ({#'deep_eq,'s,(<teststruct> -1,55,-3)}), ({#'==,'result,55}) }) })) }),
    ({ "Lambda: Unprotected relaxed rvalue-struct element by name",      0,        lambda(0, ({#',, ({#'=,'s,(<teststruct> -1,-2,-3)}), ({#'=, 'result, ({#'=,({#'->,({#'funcall,'s}),       "b"}), 55}) }), ({#'&&, ({#'deep_eq,'s,(<teststruct> -1,55,-3)}), ({#'==,'result,55}) }) })) }),
    ({ "Lambda: Unprotected strict last rvalue-struct element by name",  0,        lambda(0, ({#',,                                     ({#'=, 'result, ({#'=,({#'., (<teststruct> -1,-2,-3),"b"}), 55}) }),                                                   ({#'==,'result,55})    })) }),
    ({ "Lambda: Unprotected relaxed last rvalue-struct element by name", 0,        lambda(0, ({#',,                                     ({#'=, 'result, ({#'=,({#'->,(<teststruct> -1,-2,-3),"b"}), 55}) }),                                                   ({#'==,'result,55})    })) }),
    ({ "Lambda: Unprotected strict non-existant lvalue-struct element",  TF_ERROR, lambda(0, ({#',, ({#'=,'s,(<teststruct> -1,-2,-3)}), ({#'=, 'result, ({#'=,({#'., 's,                     "d"}), 55}) }), ({#'&&, ({#'deep_eq,'s,(<teststruct> -1,-2,-3)}), ({#'==,'result,55}) }) })) }),
    ({ "Lambda: Unprotected relaxed non-existant lvalue-struct element", 0,        lambda(0, ({#',, ({#'=,'s,(<teststruct> -1,-2,-3)}), ({#'=, 'result, ({#'=,({#'->,'s,                     "d"}), 55}) }), ({#'&&, ({#'deep_eq,'s,(<teststruct> -1,-2,-3)}), ({#'==,'result,55}) }) })) }),

    // There are three mapping index operations, we test each one of them:
    // 1. ({ #'[, mapping, index })
    // 2. ({ #'[, mapping, index, subindex })
    // 3. ({ #'[,], mapping, index, subindex })
    ({ "Lambda: Unprotected lvalue-mapping element 1",      0, lambda(0, ({#',, ({#'=,'m,(["a":'a',"b":'b',"c":({'c'})])}), ({#'=, 'result, ({#'=,({#'[,   'm,                              "b"   }), 'B'}) }), ({#'&&, ({#'deep_eq,'m,(["a":'a',"b":'B',"c":({'c'})])}), ({#'==,'result,'B'}) }) })) }),
    ({ "Lambda: Unprotected lvalue-mapping element 2",      0, lambda(0, ({#',, ({#'=,'m,(["a":'a',"b":'b',"c":({'c'})])}), ({#'=, 'result, ({#'=,({#'[,   'm,                              "b", 0}), 'B'}) }), ({#'&&, ({#'deep_eq,'m,(["a":'a',"b":'B',"c":({'c'})])}), ({#'==,'result,'B'}) }) })) }),
    ({ "Lambda: Unprotected lvalue-mapping element 3",      0, lambda(0, ({#',, ({#'=,'m,(["a":'a',"b":'b',"c":({'c'})])}), ({#'=, 'result, ({#'=,({#'[,], 'm,                              "b", 0}), 'B'}) }), ({#'&&, ({#'deep_eq,'m,(["a":'a',"b":'B',"c":({'c'})])}), ({#'==,'result,'B'}) }) })) }),
    ({ "Lambda: Unprotected rvalue-mapping element 1",      0, lambda(0, ({#',, ({#'=,'m,(["a":'a',"b":'b',"c":({'c'})])}), ({#'=, 'result, ({#'=,({#'[,   ({#'funcall,'m}),                "b"   }), 'B'}) }), ({#'&&, ({#'deep_eq,'m,(["a":'a',"b":'B',"c":({'c'})])}), ({#'==,'result,'B'}) }) })) }),
    ({ "Lambda: Unprotected rvalue-mapping element 2",      0, lambda(0, ({#',, ({#'=,'m,(["a":'a',"b":'b',"c":({'c'})])}), ({#'=, 'result, ({#'=,({#'[,   ({#'funcall,'m}),                "b", 0}), 'B'}) }), ({#'&&, ({#'deep_eq,'m,(["a":'a',"b":'B',"c":({'c'})])}), ({#'==,'result,'B'}) }) })) }),
    ({ "Lambda: Unprotected rvalue-mapping element 3",      0, lambda(0, ({#',, ({#'=,'m,(["a":'a',"b":'b',"c":({'c'})])}), ({#'=, 'result, ({#'=,({#'[,], ({#'funcall,'m}),                "b", 0}), 'B'}) }), ({#'&&, ({#'deep_eq,'m,(["a":'a',"b":'B',"c":({'c'})])}), ({#'==,'result,'B'}) }) })) }),
    ({ "Lambda: Unprotected last rvalue-mapping element 1", 0, lambda(0, ({#',,                                             ({#'=, 'result, ({#'=,({#'[,   (["a":'a',"b":'b',"c":({'c'})]), "b"   }), 'B'}) }),                                                           ({#'==,'result,'B'})    })) }),
    ({ "Lambda: Unprotected last rvalue-mapping element 2", 0, lambda(0, ({#',,                                             ({#'=, 'result, ({#'=,({#'[,   (["a":'a',"b":'b',"c":({'c'})]), "b", 0}), 'B'}) }),                                                           ({#'==,'result,'B'})    })) }),
    ({ "Lambda: Unprotected last rvalue-mapping element 3", 0, lambda(0, ({#',,                                             ({#'=, 'result, ({#'=,({#'[,], (["a":'a',"b":'b',"c":({'c'})]), "b", 0}), 'B'}) }),                                                           ({#'==,'result,'B'})    })) }),

    ({ "Lambda: Unprotected array range 1", 0, lambda(0, ({#',, ({#'=,'a,quote(({1})*10)}), ({#'=,  ({#'[..],'a,2,6}),quote(({2})*5)  }), ({#'deep_eq,'a,quote(({1})*2+({2})*5+({1})*3)}) })) }),
    ({ "Lambda: Unprotected array range 2", 0, lambda(0, ({#',, ({#'=,'a,quote(({1})*10)}), ({#'&&=,({#'[..],'a,2,6}),quote(({2})*5)  }), ({#'deep_eq,'a,quote(({1})*2+({2})*5+({1})*3)}) })) }),
    ({ "Lambda: Unprotected array range 3", 0, lambda(0, ({#',, ({#'=,'a,'({0,1,2,3,4}) }), ({#'=,  ({#'[..],'a,0,1}),'({})           }), ({#'deep_eq,'a,'({2,3,4})}) })) }),
    ({ "Lambda: Unprotected array range 4", 0, lambda(0, ({#',, ({#'=,'a,'({0,1,2,3,4}) }), ({#'=,  ({#'[..],'a,2,3}),'({10,11,12,13})}), ({#'deep_eq,'a,'({0,1,10,11,12,13,4})}) })) }),
    ({ "Lambda: Unprotected string range 1", 0, lambda(0, ({#',, ({#'=,'a,"0123456789"}),   ({#'=,  ({#'[..],'a,2,6}),"xxxxx"         }), ({#'==,'a,"01xxxxx789"}) })) }),
    ({ "Lambda: Unprotected string range 2", 0, lambda(0, ({#',, ({#'=,'a,"0123456789"}),   ({#'&&=,({#'[..],'a,2,6}),"xxxxx"         }), ({#'==,'a,"01xxxxx789"}) })) }),

    ({ "Lambda: Indexing array range 1", 0, lambda(0, ({#',, ({#'=,'a,'({1,2,3,4,5,6})}), ({#'=,({#'[,({#'[..],'a,1,3}),1}),7}), ({#'deep_eq,'a,'({1,2,7,4,5,6})}) })) }),
    ({ "Lambda: Indexing array range 2", 0, lambda(0, ({#',, ({#'=,'a,'({1,2,3,4,5,6})}), ({#'=,({#'[..],({#'[..],'a,1,3}),1,1}),'({})}), ({#'deep_eq,'a,'({1,2,4,5,6})}) })) }),
    ({ "Lambda: Indexing string range 1", 0, lambda(0, ({#',, ({#'=,'str,"abcdef"}), ({#'=,({#'[,({#'[..],'str,0,3}),2}),'x'}), ({#'==,'str,"abxdef"}) })) }),
    ({ "Lambda: Indexing string range 2", 0, lambda(0, ({#',, ({#'=,'str,"abcdef"}), ({#'=,({#'[..],({#'[..],'str,0,3}),2,2}),"xyz"}), ({#'==,'str,"abxyzdef"}) })) }),

    ({ "Lambda: Vanishing destinations 1a", TF_ERROR, lambda(0, ({#',, ({#'=,'a,'({1})}),   ({#'=, ({#'[,'a,          0       }), ({#'=,'a,  0}) }) })) }),
    ({ "Lambda: Vanishing destinations 1b", TF_ERROR, lambda(0, ({#',, ({#'=,'a,'({1})}),   ({#'=, ({#'[,'a, ({#'=,'a,0})     }),            0   }) })) }),
    ({ "Lambda: Vanishing destinations 2a", TF_ERROR, lambda(0, ({#',, ({#'=,'a,([])}),     ({#'=, ({#'[,'a,          "abc"   }), ({#'=,'a,  0}) }) })) }),
    ({ "Lambda: Vanishing destinations 2b", TF_ERROR, lambda(0, ({#',, ({#'=,'a,([])}),     ({#'=, ({#'[,'a, ({#'=,'a,"abc"}) }),            0   }) })) }),
    ({ "Lambda: Vanishing destinations 3a", TF_ERROR, lambda(0, ({#',, ({#'=,'str,"abcd"}), ({#'=, ({#'[,'a,            0     }), ({#'=,'str,0}) }) })) }),
    ({ "Lambda: Vanishing destinations 3b", TF_ERROR, lambda(0, ({#',, ({#'=,'str,"abcd"}), ({#'=, ({#'[,'a, ({#'=,'str,0})   }),            0   }) })) }),
    ({ "Lambda: Vanishing destinations 4", TF_DONTCHECKERROR, lambda(0, ({#',, ({#'=,'a,'({1})}), ({#'&&=, ({#'[,'a,0}), ({#'=,'a,0}) }) })) }),
    ({ "Lambda: Vanishing destinations 4b", 0, lambda(0, ({#',, ({#'=,'a,'({1})}), ({#'&&=, ({#'[,'a,0}), ({#',, ({#'=,'a,0}), ({#'=,'b,'({2})}), 0}) }), ({#'==, ({#'[,'b, 0}), 2}) })) }),
    ({ "Lambda: Vanishing destinations 5", TF_DONTCHECKERROR, lambda(0, ({#',, ({#'=,'a,([])           }), ({#'||=, ({#'[,'a,"abc"}),         ({#'=,'a,0})                    }) })) }),
    ({ "Lambda: Vanishing destinations 6", TF_DONTCHECKERROR, lambda(0, ({#',, ({#'=,'str,"abcd"       }), ({#'&&=, ({#'[,'str,0}),           ({#'=,'a,0})                    }) })) }),
    ({ "Lambda: Vanishing destinations 7", TF_DONTCHECKERROR, lambda(0, ({#',, ({#'=,'a,quote(({1})*10)}), ({#'&&=, ({#'[..],'a,2,6}), ({#',, ({#'=,'a,0}), quote(({2})*10)}) }) })) }),
    ({ "Lambda: Vanishing destinations 8", 0, lambda(0, ({#',, ({#'=,'a, ({#'*=, ({#'[,'({1,2,3,4,5,6}),4}), 2}) }), ({#'==,'a,10}) })) }),
    ({ "Lambda: Vanishing destinations 9", 0, lambda(0, ({#',, ({#'=,'a,'({1,2,3,4,5,6})}), ({#'=,'element,({#'&,({#'[,'a,5})})}), ({#'=,'a,0}), ({#'==,'element,6}) })) }),
    ({ "Lambda: Vanishing destinations 10", 0, lambda(0, ({#',, ({#'=,'a,'({1,2,3,4,5,6})}), ({#'=,'range,({#'&,({#'[..],'a,1,4})})}), ({#'=,'a,0}), ({#'=,'range,'({7,8,9})}), ({#'deep_eq,'range,'({7,8,9})}) })) }),
    ({ "Lambda: Vanishing destinations 11", 0, lambda(0, ({#',, ({#'=,'a,"Vanishing"}), ({#'=,'element,({#'&,({#'[,'a,1})})}), ({#'=,'a,0}), ({#'==,'element,'a'}) })) }),
    ({ "Lambda: Vanishing destinations 12", 0, lambda(0, ({#',, ({#'=,'a,"Vanishing"}), ({#'=,'range,({#'&,({#'[..],'a,5,6})})}), ({#'=,'a,0}), ({#'==,'range,"hi"}) })) }),


    ({ "Lambda: Protected locals 1", 0,
       lambda(0,
        ({#',,
            ({#'=, 'a, '({1,2,3,4,5}) }),
            ({#'=, 'summe, 0}),
            ({#'map, 'a, lambda(({'entry, 'summe_ref}), ({#'+=, 'summe_ref, 'entry})), ({#'&, 'summe}) }),
            ({#'==, 'summe, 15})
        }))
    }),
    ({ "Lambda: Protected locals 2", 0,
       lambda(0,
        ({#',,
            ({#'=, 'x, 1}),
            ({#'&, 'x})
        }))
    }),
    ({ "Lambda: Protected elements 1", 0,
       lambda(0,
        ({#',,
            ({#'=, 'a, '({1,2,3,4,5}) }),
            ({#'funcall,
                lambda(({'arg1, 'arg2, 'arg3}), ({#',, ({#'=, 'arg1, 'arg2}), ({#'=, 'arg2, 'arg3}) })),
                ({#'&, ({#'[, 'a, 1}) }),
                ({#'&, ({#'[, 'a, 2}) }),
                ({#'&, ({#'[<,'a, 2}) }),
            }),
            ({#'deep_eq, 'a, '({1,3,4,4,5}) })
        }))
    }),
    ({ "Lambda: Protected elements 2", 0,
       lambda(0,
        ({#',,
            ({#'=, 'a, '({1,2,3,4,5}) }),
            ({#'funcall,
                lambda(({'arg1, 'arg2, 'arg3}), ({#',, ({#'=, 'arg1, 'arg2}), ({#'=, 'arg2, 'arg3}) })),
                ({#'&, ({#'[, 'a, 1}) }),
                ({#'&, ({#'[, 'a, 2}) }),
                ({#'&, ({#'[, 'a, 1}) }),
            }),
            ({#'deep_eq, 'a, '({1,3,3,4,5}) })
        }))
    }),
    ({ "Lambda: Protected char 1", 0,
       lambda(0,
        ({#',,
            ({#'=, 'str, "xyz" }),
            ({#'=, 'c, ({#'||, 0, ({#'&, ({#'[,'str,1}) }) }) }),
            ({#'=, 'c, 'a' }),
            ({#'==, 'str, "xaz" })
        }))
    }),
    ({ "Lambda: Protected char 2", 0,
       lambda(0,
        ({#',,
            ({#'=, 'str, "xyz" }),
            ({#'=, 'c1, ({#'||, 0, ({#'&, ({#'[,'str,0}) }) }) }),
            ({#'=, 'c2, ({#'||, 0, ({#'&, ({#'[,'str,1}) }) }) }),
            ({#'=, 'c1, 'a' }),
            ({#'=, 'c2, 'b' }),
            ({#'==, 'str, "abz" })
        }))
    }),
    ({ "Lambda: Protected char 3", 0,
       lambda(0,
        ({#',,
            ({#'=, 'str1, "abcdefgh" }),
            ({#'=, 'c, ({#'||, 0, ({#'&, ({#'[,'str1,3}) }) }) }),
            ({#'=, 'c, '1' }),
            ({#'=, 'str2, 'str1 }),
            ({#'=, 'c, '2' }),
            ({#'!=, 'str2, 'str1 })
        }))
    }),
    ({ "Lambda: Protected char 4", 0,
       lambda(0,
        ({#',,
            ({#'=, 'm, ([:1]) }),
            ({#'=, 'str, "abcdefghi" }),
            ({#'=, 'c, ({#'||, 0, ({#'&, ({#'[,'str,3}) }) }) }),
            ({#'=, 'c, '1' }),
            ({#'=, ({#'[, 'm, 'str}), "Nr. 1" }),
            ({#'=, 'c, '2' }),
            ({#'=, ({#'[, 'm, "abc2efghi"}), "Nr. 2" }),

            ({#'=, 'ind, ({#'m_indices, 'm}) }),
            ({#'&&,
                ({#'==, ({#'sizeof, 'm}), 2 }),
                ({#'!=, ({#'+, "X", ({#'[, 'ind, 0}) }), ({#'+, "X", ({#'[, 'ind, 1}) }) })
            })
        }))
    }),
    ({ "Lambda: Protected array range 1", 0,
       lambda(0,
        ({#',,
            ({#'=, 'a, '({1,2,3,4,5}) }),
            ({#'funcall,
                lambda(({'arg1, 'arg2}), ({#'=, 'arg1, 'arg2})),
                ({#'&, ({#'[..], 'a, 1, 2}) }),
                ({#'&, ({#'[..], 'a, 3, 3}) }),
            }),
            ({#'deep_eq, 'a, '({1,4,4,5}) })
        }))
    }),
    ({ "Lambda: Protected array range 2", 0,
       lambda(0,
        ({#',,
            ({#'=, 'a, '({1,2,3,4,5}) }),
            ({#'funcall,
                lambda(({'arg}), ({#'=, ({#'[, 'arg, 1}), "a" })),
                ({#'&, ({#'[..], 'a, 1, 2}) }),
            }),
            ({#'deep_eq, 'a, '({1,2,"a",4,5}) })
        }))
    }),
    ({ "Lambda: Protected array range 3", 0,
       lambda(0,
        ({#',,
            ({#'=, 'a, '({0,1,2,3,4}) }),
            ({#'funcall,
                lambda(({'arg}), ({#'=, 'arg, '({}) })),
                ({#'&, ({#'[..], 'a, 0, 1}) }),
            }),
            ({#'deep_eq, 'a, '({2,3,4}) })
        }))
    }),
    ({ "Lambda: Protected array range 4", 0,
       lambda(0,
        ({#',,
            ({#'=, 'a, '({0,1,2,3,4}) }),
            ({#'funcall,
                lambda(({'arg}), ({#'=, 'arg, '({10,11,12,13}) })),
                ({#'&, ({#'[..], 'a, 2, 3}) }),
            }),
            ({#'deep_eq, 'a, '({0,1,10,11,12,13,4}) })
        }))
    }),
    ({ "Lambda: Protected array range 5", 0,
       lambda(0,
        ({#',,
            ({#'=, 'a, '({1,2,3,4,5}) }),
            ({#'funcall,
                lambda(({'arg1, 'arg2, 'arg3}), ({#',, ({#'=, 'arg1, 'arg3}), ({#'=, 'arg2, 'arg3})})),
                ({#'&, ({#'[..], 'a, 2, 2}) }),
                ({#'&, ({#'[..], 'a, 3, 3}) }),
                ({#'&, ({#'[..], 'a, 4, 4}) }),
            }),
            ({#'deep_eq, 'a, '({1,2,5,5,5}) })
        }))
    }),
    ({ "Lambda: Protected array range 6", 0,
       lambda(0,
       ({#',,
            ({#'=, 'a, '({1,2,3,4,5}) }),
            ({#'=, 'b, '({6,7,8,9,0}) }),
            ({#'=, 'x, ({#'&, ({#'[..], 'a, 1, 2}) }) }),
            ({#'=, 'y, ({#'&, ({#'[..], 'b, 1, 2}) }) }),
            ({#'=, 'x, ({#'&, 'y}) }),
            ({#'=, 'y, '({10, 11}) }),
            ({#'&&,
                ({#'deep_eq, 'a, '({1,10,11,4,5}) }),
                ({#'deep_eq, 'b, '({6,10,11,9,0}) }),
            })
       }))
    }),
    ({ "Lambda: Protected array range 7", 0,
       lambda(0,
       ({#',,
            ({#'=, 'a, '({1,2,3,4,5}) }),
            ({#'=, 'b, '({6,7,8,9,0}) }),
            ({#'=, 'x, ({#'&, ({#'[..], 'a, 1, 2}) }) }),
            ({#'=, 'y, ({#'&, ({#'[..], 'b, 1, 2}) }) }),
            ({#'=, 'x, ({#'&, 'y}) }),
            ({#'=, ({#'[, 'x, 0}), 100 }),
            ({#'&&,
                ({#'deep_eq, 'a, '({1,100,8,4,5}) }),
                ({#'deep_eq, 'b, '({6,100,8,9,0}) }),
            })
       }))
    }),
    ({ "Lambda: Protected string range 1", 0,
       lambda(0,
        ({#',,
            ({#'=, 'a, "12345" }),
            ({#'funcall,
                lambda(({'arg1, 'arg2}), ({#'=, 'arg1, 'arg2})),
                ({#'&, ({#'[..], 'a, 1, 2}) }),
                ({#'&, ({#'[..], 'a, 3, 3}) }),
            }),
            ({#'==, 'a, "1445" })
        }))
    }),
    ({ "Lambda: Protected string range 2a", 0,
       lambda(0,
        ({#',,
            ({#'=, 'a, "12345" }),
            ({#'funcall,
                lambda(({'arg}), ({#'=, ({#'[, 'arg, 1}), 'a' })),
                ({#'&, ({#'[..], 'a, 1, 2}) }),
            }),
            ({#'==, ({#'+,"X",'a}), "X12a45" })
        }))
    }),
    ({ "Lambda: Protected string range 2b", 0,
       lambda(0,
        ({#',,
            ({#'=, 'a, "12345" }),
            ({#'funcall,
                lambda(({'arg}), ({#'=, ({#'[, 'arg, 1}), 'a' })),
                ({#'&, ({#'[..], 'a, 1, 2}) }),
            }),
            ({#'==, 'a, "12a45" })
        }))
    }),
    ({ "Lambda: Protected string range 3", 0,
       lambda(0,
        ({#',,
            ({#'=, 'a, "12345" }),
            ({#'funcall,
                lambda(({'arg1, 'arg2, 'arg3}), ({#',, ({#'=, 'arg1, 'arg3}), ({#'=, 'arg2, 'arg3})})),
                ({#'&, ({#'[..], 'a, 1, 2}) }),
                ({#'&, ({#'[..], 'a, 3, 3}) }),
                ({#'&, ({#'[..], 'a, 4, 4}) }),
            }),
            ({#'==, 'a, "1555" })
        }))
    }),

    ({ "Lambda: Volatile protected lvalues 1", 0,
       lambda(0,
        ({#',,
            ({#'=, 'val, 10 }),
            ({#'=, 'lv1, ({#'||, 0, ({#'&, 'val}) }) }),
            ({#'=, 'lv2, 1 }),

            ({#'=, 'lv2, 'lv1 }),
            ({#'=, 'lv2, 20 }),

            ({#'==, 'val, 10 })
        }))
    }),
    ({ "Lambda: Volatile protected lvalues 2", 0,
       lambda(0,
        ({#',,
            ({#'=, 'val, 10 }),
            ({#'=, 'arr1, ({#'({, ({#'||, 0, ({#'&, 'val}) }) }) }),
            ({#'=, 'arr2, '({ 1 }) }),

            ({#'=, ({#'[, 'arr2, 0}), ({#'[, 'arr1, 0}) }),
            ({#'=, ({#'[, 'arr2, 0}), 20 }),

            ({#'==, 'val, 10 })
        }))
    }),
    ({ "Lambda: Volatile return lvalues 1", 0,
       lambda(0,
        ({#',,
            ({#'=, 'val, 10 }),
            ({#'=, 'result, ({#'return_first_lvalue, ({#'&, 'val}), 0 }) }),
            ({#'=, 'result, 12 }),
            ({#'==, 'val, 10 })
        }))
    }),
    ({ "Lambda: Volatile return lvalues 2", 0,
       lambda(0,
        ({#',,
            ({#'=, 'val, 10 }),
            ({#'=, 'result,
                ({#'funcall,
                    ({ #'lambda, 0, ({#'({, #'return, ({#'&, 'val}) }) })
                })
            }),
            ({#'=, 'result, 12 }),
            ({#'==, 'val, 10 })
        }))
    }),


    ({ "Lambda: Protected return lvalues 1", 0,
       lambda(0,
        ({#',,
            ({#'=, 'val, 20 }),
            ({#'=, 'result, ({#'&, ({#'return_first_lvalue, ({#'&, 'val}), 0 }) }) }),
            ({#'=, 'result, 22 }),
            ({#'==, 'val, 22 })
        }))
    }),
    ({ "Lambda: Protected return lvalues 2", 0,
       lambda(0,
        ({#',,
            ({#'=, 'val, 20 }),
            ({#'=, 'result,
                ({#'&,
                    ({#'funcall,
                        ({ #'lambda, '({'arg}), ({#'({, #'return, '({#'&, 'arg}) }) }),
                        ({ #'&, 'val })
                    })
                })
            }),
            ({#'=, 'result, 22 }),
            ({#'==, 'val, 22 })
        }))
    }),


    ({ "Lambda: Reference loops 1", 0,
       lambda(0,
        ({#',,
            ({#'=, 'a, 10 }),
            ({#'=, 'b, ({#'&, 'a}) }),
            ({#'=, 'a, ({#'&, 'b}) }),

            ({#'==, 'a, 10})
        }))
    }),
    ({ "Lambda: Reference loops 2", 0,
       lambda(0,
        ({#',,
            ({#'=, 'a, 10 }),
            ({#'=, 'b, ({#'&, 'a}) }),
            ({#'=, 'c, 20 }),
            ({#'=, 'd, ({#'&, 'c}) }),
            ({#'=, 'a, ({#'&, 'd}) }),

            ({#'?,
                ({#'||, ({#'!=, 'a, 20}), ({#'!=, 'd, 20}) }), 0,
                ({#',,
                    ({#'=, 'a, 30}),
                    ({#'&&,
                        ({#'==, 'a, 30}),
                        ({#'==, 'b, 30}),
                        ({#'==, 'c, 30}),
                        ({#'==, 'd, 30}),
                    })
                })
            })
        }))
    }),



    ({ "Lambda: foreach over integer", 0,
       lambda(0,
        ({#',,
            ({#'=, 'sum, 0}),
            ({#'foreach, 'a, 10,
                ({#'+=, 'sum, 'a})
            }),

            ({#'&&, ({#'==, 'a, 9}), ({#'==, 'sum, 45}) })
        }))
    }),
    ({ "Lambda: foreach over singular array", 0,
       lambda(0,
        ({#',,
            ({#'=, 'sum, 0}),
            ({#'foreach, 'a, '({1,2,3}),
                ({#'+=, 'sum, 'a})
            }),

            ({#'&&, ({#'==, 'a, 3}), ({#'==, 'sum, 6}) })
        }))
    }),
    ({ "Lambda: foreach over array variable", 0,
       lambda(0,
        ({#',,
            ({#'=, 'arr, '({1,2,3}) }),
            ({#'foreach, 'a, 'arr,
                ({#'*=, 'a, 2})
            }),

            ({#'&&, ({#'==, 'a, 6}), ({#'deep_eq, 'arr, '({1,2,3}) }) })
        }))
    }),
    ({ "Lambda: foreach over array reference", 0,
       lambda(0,
        ({#',,
            ({#'=, 'arr, '({1,2,3}) }),
            ({#'foreach, 'a, ({#'&, 'arr}),
                ({#'*=, 'a, 2})
            }),

            ({#'&&, ({#'==, 'a, 6}), ({#'deep_eq, 'arr, '({2,4,6}) }) })
        }))
    }),
    ({ "Lambda: foreach over array range", 0,
       lambda(0,
        ({#',,
            ({#'=, 'arr, '({0,1,2,3,4,5,6}) }),
            ({#'foreach, 'a, ({#'&, ({#'[..], 'arr, 2, 4}) }),
                ({#'*=, 'a, 2})
            }),

            ({#'&&, ({#'==, 'a, 8}), ({#'deep_eq, 'arr, '({0,1,4,6,8,5,6}) }) })
        }))
    }),
    ({ "Lambda: foreach over singular string", 0,
       lambda(0,
        ({#',,
            ({#'=, 'i, 0}),
            ({#'foreach, 'a, "Hello",
                ({#'++, 'i})
            }),

            ({#'&&, ({#'==, 'a, 'o'}), ({#'==, 'i, 5}) }),
        }))
    }),
    ({ "Lambda: foreach over string variable", 0,
       lambda(0,
        ({#',,
            ({#'=, 'str, "LDMud"}),
            ({#'foreach, 'a, 'str,
                ({#'+=, 'a, 1})
            }),

            ({#'&&, ({#'==, 'a, 'e'}), ({#'==, 'str, "LDMud"}) }),
        }))
    }),
    ({ "Lambda: foreach over string reference", 0,
       lambda(0,
        ({#',,
            ({#'=, 'str, "LDMud"}),
            ({#'foreach, 'a, ({#'&, 'str}),
                ({#'+=, 'a, 1})
            }),

            ({#'&&, ({#'==, 'a, 'e'}), ({#'==, 'str, "MENve"}) }),
        }))
    }),
    ({ "Lambda: foreach over string range", 0,
       lambda(0,
        ({#',,
            ({#'=, 'str, "LDMud"}),
            ({#'foreach, 'a, ({#'&, ({#'[..], 'str, 0, 2}) }),
                ({#'+=, 'a, 32})
            }),

            ({#'&&, ({#'==, 'a, 'm'}), ({#'==, 'str, "ldmud"}) }),
        }))
    }),
    ({ "Lambda: foreach over singular struct", 0,
       lambda(0,
        ({#',,
            ({#'=, 'sum, 0}),
            ({#'foreach, 'a, (<teststruct> 1,2,3),
                ({#'+=, 'sum, 'a})
            }),

            ({#'&&, ({#'==, 'a, 3}), ({#'==, 'sum, 6}) }),
        }))
    }),
    ({ "Lambda: foreach over struct variable", 0,
       lambda(0,
        ({#',,
            ({#'=, 'str, (<teststruct> 1,2,3)}),
            ({#'foreach, 'a, 'str,
                ({#'*=, 'a, 2})
            }),

            ({#'&&, ({#'==, 'a, 6}), ({#'deep_eq, 'str, (<teststruct> 1,2,3)}) }),
        }))
    }),
    ({ "Lambda: foreach over struct reference", 0,
       lambda(0,
        ({#',,
            ({#'=, 'str, (<teststruct> 1,2,3)}),
            ({#'foreach, 'a, ({#'&, 'str}),
                ({#'*=, 'a, 2})
            }),

            ({#'&&, ({#'==, 'a, 6}), ({#'deep_eq, 'str, (<teststruct> 2,4,6)}) }),
        }))
    }),
    ({ "Lambda: foreach over singular mapping", 0,
       lambda(0,
        ({#',,
            ({#'=, 'sum, 0}),
            ({#'foreach, ({'key, 'a}), ([ "a": 1, "b": 2, "c": 3 ]),
                ({#'+=, 'sum, 'a})
            }),

            ({#'&&,
                ({#'member, (["a","b","c"]), 'key}),
                ({#'member, ([1,2,3]), 'a}),
                ({#'==, 'sum, 6})
            })
        }))
    }),
    ({ "Lambda: foreach over mapping variable", 0,
       lambda(0,
        ({#',,
            ({#'=, 'm, ([ "a": 'a', "b": 'b', "c": 'c' ]) }),
            ({#'foreach, ({'key, 'a}), 'm,
                ({#'-=, 'a, 32})
            }),

            ({#'&&,
                ({#'member, (["a","b","c"]), 'key}),
                ({#'>=, ({#'member, "ABC", 'a}), 0}),
                ({#'deep_eq, 'm, (["a":'a',"b":'b',"c":'c']) }),
            })
        }))
    }),
    ({ "Lambda: foreach over mapping reference", 0,
       lambda(0,
        ({#',,
            ({#'=, 'm, ([ "a": 'a', "b": 'b', "c": 'c' ]) }),
            ({#'foreach, ({'key, 'a}), ({#'&, 'm}),
                ({#'-=, 'a, 32})
            }),

            ({#'&&,
                ({#'member, (["a","b","c"]), 'key}),
                ({#'>=, ({#'member, "ABC", 'a}), 0}),
                ({#'deep_eq, 'm, (["a":'A',"b":'B',"c":'C']) }),
            })
        }))
    }),
#if 0 /* Still needs to be done... */
    ({ "foreach with an empty body", 0,
       lambda(0,
        ({#',,
            ({#'foreach, 'a, '({2}) }),
            ({#'==, 'a, 2})
        }))
    }),
#endif
    ({ "Lambda: foreach not touching references", 0,
       lambda(0,
        ({#',,
            ({#'=, 'a, 0}),
            ({#'=, 'aref, ({#'&, 'a}) }),

            ({#'foreach, 'aref, '({2}),
                ({#'+, 1, 1})
            }),

            ({#'&&, ({#'==, 'a, 0}), ({#'==, 'aref, 2}) })
        }))
    }),
    ({"Lambda: foreach, why we should not touch references", 0,
       lambda(0,
        ({#',,
            ({#'=, 'a, 0}),
            ({#'=, 'b, 0}),

            ({#'foreach, 'i,
                ({#'({, ({#'&, 'a}), ({#'&, 'b}), 2 }),
                ({#'+, 1, 1})
            }),

            ({#'&&, ({#'==, 'a, 0}), ({#'==, 'b, 0}) })
        }))
    }),



    ({ "Lambda: Special Efuns 1", 0,
       lambda(0,
        ({#',,
            ({#'=, 'a, '({1}) }),
            ({#'=, 'nr, ({#'sscanf, "5 6", "%d %d", ({#'[, 'a, 0}), 'a}) }),
            ({#'&&, ({#'==, 'nr, 2}), ({#'==, 'a, 6}) })
        }))
    }),
    ({ "Lambda: sort_array by value", 0,
       lambda(0,
        ({#',,
            ({#'=, 'arr, '({ 5,1,7,4,8,3,0 }) }),
            ({#'=, 'result, ({#'sort_array, 'arr, #'>}) }),
            ({#'&&,
                ({#'deep_eq, 'arr, '({ 5,1,7,4,8,3,0 }) }),
                ({#'deep_eq, 'result, '({ 0,1,3,4,5,7,8 }) })
            })
        }))
    }),
    ({ "Lambda: sort_array by reference", 0,
       lambda(0,
        ({#',,
            ({#'=, 'arr, '({ 5,1,7,4,8,3,0 }) }),
            ({#'=, 'result, ({#'sort_array, ({#'&,'arr}), #'>}) }),
            ({#'&&,
                ({#'==, 'arr, 'result }),
                ({#'deep_eq, 'result, '({ 0,1,3,4,5,7,8 }) })
            })
        }))
    }),
    ({ "Lambda: sort_array by range", 0,
       lambda(0,
        ({#',,
            ({#'=, 'arr, '({ 5,1,7,4,8,3,0 }) }),
            ({#'=, 'result, ({#'sort_array, ({#'&,({#'[..],'arr,1,5})}), #'>}) }),
            ({#'&&,
                ({#'deep_eq, 'arr, '({ 5,1,3,4,7,8,0 }) }),
                ({#'deep_eq, 'result, '({ 1,3,4,7,8 }) })
            })
        }))
    }),
    ({ "Lambda: sort_array by vanishing range", 0,
       lambda(0,
        ({#',,
            ({#'=, 'arr, '({ 5,1,7,4,8,3,0 }) }),
            ({#'=, 'result, ({#'sort_array, ({#'&,({#'[..],'arr,1,5})}), ({#'||, ({#'=,'arr,0}), #'>}) }) }),
            ({#'&&,
                ({#'==, 'arr, 0 }),
                ({#'deep_eq, 'result, '({ 1,3,4,7,8 }) })
            })
        }))
    }),
    ({ "Lambda: reverse by array value", 0,
       lambda(0,
        ({#',,
            ({#'=, 'arr, '({ 0, 1, 2, 3, 4, 5, 6 }) }),
            ({#'=, 'result, ({#'reverse, 'arr}) }),
            ({#'&&,
                ({#'deep_eq, 'arr, '({ 0,1,2,3,4,5,6 }) }),
                ({#'deep_eq, 'result, '({ 6,5,4,3,2,1,0 }) })
            })
        }))
    }),
    ({ "Lambda: reverse by string value", 0,
       (:
           mixed arr = "0123456";
           mixed result = reverse(arr);
           return arr == "0123456" && result == "6543210";
       :)
    }),
    ({ "Lambda: reverse by array reference", 0,
       lambda(0,
        ({#',,
            ({#'=, 'arr, '({ 0, 1, 2, 3, 4, 5, 6 }) }),
            ({#'=, 'result, ({#'reverse, ({#'&,'arr}) }) }),
            ({#'&&,
                ({#'==, 'arr, 'result}),
                ({#'deep_eq, 'result, '({ 6,5,4,3,2,1,0 }) })
            })
        }))
    }),
    ({ "Lambda: reverse by string reference a", 0,
       lambda(0,
        ({#',,
            ({#'=, 'arr, "0123456" }),
            ({#'=, 'result, ({#'reverse, ({#'&,'arr}) }) }),
            ({#'&&,
                ({#'==, 'arr, 'result}),
                ({#'==, ({#'+,'result,"X"}), "6543210X"})
            })
        }))
    }),
    ({ "Lambda: reverse by string reference b", 0,
       lambda(0,
        ({#',,
            ({#'=, 'arr, "0123456" }),
            ({#'=, 'result, ({#'reverse, ({#'&,'arr}) }) }),
            ({#'&&,
                ({#'==, 'arr, 'result}),
                ({#'==, 'result, "6543210"})
            })
        }))
    }),
    ({ "Lambda: reverse by array range", 0,
       lambda(0,
        ({#',,
            ({#'=, 'arr, '({ 0, 1, 2, 3, 4, 5, 6 }) }),
            ({#'=, 'result, ({#'reverse, ({#'&,({#'[..],'arr,1,5}) }) }) }),
            ({#'&&,
                ({#'deep_eq, 'arr, '({ 0,5,4,3,2,1,6 }) }),
                ({#'deep_eq, 'result, '({ 5,4,3,2,1 }) })
            })
        }))
    }),
    ({ "Lambda: reverse by string range", 0,
       lambda(0,
        ({#',,
            ({#'=, 'arr, "0123456" }),
            ({#'=, 'result, ({#'reverse, ({#'&,({#'[..],'arr,1,5}) }) }) }),
            ({#'&&,
                ({#'==, 'arr, "0543216" }),
                ({#'==, 'result, "54321" }),
            })
        }))
    }),
    ({ "Lambda: reverse by vanishing array range", 0,
       lambda(0,
        ({#',,
            ({#'=, 'arr, '({ 0, 1, 2, 3, 4, 5, 6 }) }),
            ({#'=, 'result, ({#'reverse, ({#'return_first_lvalue, ({#'&,({#'[..],'arr,1,5})}), ({#'=,'arr,10}) }) }) }),
            ({#'&&,
                ({#'==, 'arr, 10 }),
                ({#'deep_eq, 'result, '({ 5,4,3,2,1 }) })
            })
        }))
    }),
    ({ "Lambda: reverse by vanishing string range", 0,
       lambda(0,
        ({#',,
            ({#'=, 'arr, ({#'+, "lvalu", ({#'to_string, '({'e'}) }) }) }),
            ({#'=, 'result, ({#'reverse, ({#'return_first_lvalue, ({#'&,({#'[..],'arr,1,4})}), ({#'=,'arr,6}) }) }) }),
            ({#'&&,
                ({#'==, 'arr, 6 }),
                ({#'==, 'result, "ulav" })
            })
        }))
    }),
    ({ "Lambda: referencep with a lvalue parameter", 0,
       lambda(0,
        ({#',,
            ({#'=, 'var, 0 }),
            ({#'funcall, lambda(({'arg}), ({#'referencep, ({#'&, 'arg}) })), ({#'&, 'var}) })
        }))
    }),
    ({ "Lambda: referencep with a rvalue parameter", 0,
       lambda(0,
        ({#',,
            ({#'=, 'var, 0 }),
            ({#'!, ({#'funcall, lambda(({'arg}), ({#'referencep, ({#'&, 'arg}) })), 'var }) }),
        }))
    }),
    ({ "Lambda: referencep with a rvalue parameter, that was an lvalue", 0,
       lambda(0,
        ({#',,
            ({#'=, 'var, 0 }),
            ({#'=, 'ref, ({#'&, 'var}) }),
            ({#'!, ({#'funcall, lambda(({'arg}), ({#'referencep, ({#'&, 'arg}) })), 'ref }) }),
        }))
    }),



    ({ "Lambda: Flattening lvalue parameters with apply", 0,
       lambda(0,
        ({#',,
            ({#'=, 'arr, '({0, 1, 2, 3, 4, 5}) }),
            ({#'apply,
                lambda(({'a,'b,'c,'d}),
                ({#',,
                    ({#'+=, 'a, 2}),
                    ({#'+=, 'b, 3}),
                    ({#'+=, 'c, 5}),
                    ({#'+=, 'd, 7}),
                })),
                ({#'&, 'arr})
            }),
            ({#'deep_eq, 'arr, '({2,4,7,10,4,5}) })
        }))
    }),
    ({ "Lambda: Flattening vanishing lvalue parameters with apply", 0,
       lambda(0,
        ({#',,
            ({#'=, 'arr, '({0, 1, 2, 3, 4, 5}) }),
            ({#'=, 'arr2, ({#'({, ({#'&,({#'[,'arr,0})}), ({#'&,({#'[,'arr,1})}), ({#'&,({#'[,'arr,2})}), ({#'&,({#'[,'arr,3})}), ({#'&,({#'[,'arr,4})}), ({#'&,({#'[,'arr,5})}) }) }),
            ({#'apply,
                lambda(({'a,'b,'c,'d}),
                ({#',,
                    ({#'+=, 'a, 2}),
                    ({#'+=, 'b, 3}),
                    ({#'+=, 'c, 5}),
                    ({#'+=, 'd, 7}),
                })),
                ({#'&, ({#'[..<], ({#'[..<], 'arr2, 0, 1}), ({#'=, 'arr2, 0}), 1}) }),
            }),
            ({#'deep_eq, 'arr, '({2,4,7,10,4,5}) })
        }))
    }),
    ({ "Lambda: Flattening range lvalue parameters with apply", 0,
       lambda(0,
        ({#',,
            ({#'=, 'arr, '({0, 1, 2, 3, 4, 5}) }),
            ({#'apply,
                lambda(({'a,'b,'c,'d}),
                ({#',,
                    ({#'+=, 'a, 2}),
                    ({#'+=, 'b, 3}),
                    ({#'+=, 'c, 5}),
                    ({#'+=, 'd, 7}),
                })),
                ({#'&, ({#'[..], 'arr, 2, 4}) }),
            }),
            ({#'deep_eq, 'arr, '({0,1,4,6,9,5}) })
        }))
    }),
    ({ "Lambda: Flattening rvalue parameters with apply", 0,
       lambda(0,
        ({#',,
            ({#'=, 'arr, '({0, 1, 2, 3, 4, 5}) }),
            ({#'apply,
                lambda(({'a,'b,'c,'d}),
                ({#',,
                    ({#'+=, 'a, 2}),
                    ({#'+=, 'b, 3}),
                    ({#'+=, 'c, 5}),
                    ({#'+=, 'd, 7}),
                })),
                'arr
            }),
            ({#'deep_eq, 'arr, '({0,1,2,3,4,5}) })
        }))
    }),
    ({ "Lambda: Flattening vanishing rvalue parameters with apply", 0,
       lambda(0,
        ({#',,
            ({#'=, 'arr, '({0, 1, 2, 3, 4, 5}) }),
            ({#'=, 'arr2, ({#'({, ({#'&,({#'[,'arr,0})}), ({#'&,({#'[,'arr,1})}), ({#'&,({#'[,'arr,2})}), ({#'&,({#'[,'arr,3})}), ({#'&,({#'[,'arr,4})}), ({#'&,({#'[,'arr,5})}) }) }),
            ({#'apply,
                lambda(({'a,'b,'c,'d}),
                ({#',,
                    ({#'+=, 'a, 2}),
                    ({#'+=, 'b, 3}),
                    ({#'+=, 'c, 5}),
                    ({#'+=, 'd, 7}),
                })),
                ({#'[..<], ({#'[..<], 'arr2, 0, 1}), ({#'=, 'arr2, 0}), 1}),
            }),
            ({#'deep_eq, 'arr, '({0,1,2,3,4,5}) })
        }))
    }),
    ({
        "Lambda: Flattening range rvalue parameters with apply", 0,
       lambda(0,
        ({#',,
            ({#'=, 'arr, '({0, 1, 2, 3, 4, 5}) }),
            ({#'apply,
                lambda(({'a,'b,'c,'d}),
                ({#',,
                    ({#'+=, 'a, 2}),
                    ({#'+=, 'b, 3}),
                    ({#'+=, 'c, 5}),
                    ({#'+=, 'd, 7}),
                })),
                ({#'[..], 'arr, 2, 4}),
            }),
            ({#'deep_eq, 'arr, '({0,1,2,3,4,5}) })
        }))
    }),



    ({ "Lambda: Reseating of variables", 0,
       lambda(0,
        ({#',,
            ({#'=, 'a, 10}),
            ({#'=, 'b, 12}),
            ({#'=, 'l, ({#'&, 'a}) }),
            ({#'=, ({#'&, 'l}), ({#'&, 'b}) }),

            ({#'&&,
                ({#'==, 'a, 10}),
                ({#'==, 'b, 12}),
                ({#'==, 'l, 12}),
            })
        }))
    }),
    ({ "Lambda: Reseating of array entry [1]", 0,
       lambda(0,
        ({#',,
            ({#'=, 'b, 11 }),
            ({#'=, 'arr, ({#'({, 0, ({#'&,'b}), 2, 3}) }),

            ({#'=, ({#'[,'arr,1}), 10}),
            ({#'=, ({#'&, ({#'[,'arr,1}) }), 12}),

            ({#'&&,
                ({#'deep_eq, 'arr, '({0, 12, 2, 3}) }),
                ({#'==, 'b, 10})
            })
        }))
    }),
    ({ "Lambda: Reseating of array entry [<1]", 0,
       lambda(0,
        ({#',,
            ({#'=, 'b, 11 }),
            ({#'=, 'arr, ({#'({, 0, 1, 2, ({#'&,'b}) }) }),

            ({#'=, ({#'[<,'arr,1}), 10}),
            ({#'=, ({#'&, ({#'[<,'arr,1}) }), 12}),

            ({#'&&,
                ({#'deep_eq, 'arr, '({0, 1, 2, 12}) }),
                ({#'==, 'b, 10})
            })
        }))
    }),
    ({ "Lambda: Reseating of array entry [>1]", 0,
       lambda(0,
        ({#',,
            ({#'=, 'b, 11 }),
            ({#'=, 'arr, ({#'({, 0, ({#'&,'b}), 2, 3}) }),

            ({#'=, ({#'[>,'arr,1}), 10}),
            ({#'=, ({#'&, ({#'[>,'arr,1}) }), 12}),

            ({#'&&,
                ({#'deep_eq, 'arr, '({0, 12, 2, 3}) }),
                ({#'==, 'b, 10})
            })
        }))
    }),
    ({ "Lambda: Reseating of mapping entry 1", 0,
       lambda(0,
        ({#',,
            ({#'=, 'b, 11 }),
            ({#'=, 'm, ({#'([, ({ "Hi", ({#'&,'b}) }) }) }),

            ({#'=, ({#'[,'m,"Hi"}), 33}),
            ({#'=, ({#'&, ({#'[,'m,"Hi"}) }), 22}),

            ({#'&&,
                ({#'deep_eq, 'm, (["Hi": 22]) }),
                ({#'==, 'b, 33})
            })
        }))
    }),
    ({ "Lambda: Reseating of mapping entry 2", 0,
       lambda(0,
        ({#',,
            ({#'=, 'b, 11 }),
            ({#'=, 'm, ({#'([, ({ "Hi", ({#'&,'b}) }) }) }),

            ({#'=, ({#'[,'m,"Hi",0}), 33}),
            ({#'=, ({#'&, ({#'[,'m,"Hi",0}) }), 22}),

            ({#'&&,
                ({#'deep_eq, 'm, (["Hi": 22]) }),
                ({#'==, 'b, 33})
            })
        }))
    }),
    ({ "Lambda: Reseating of mapping entry 3", 0,
       lambda(0,
        ({#',,
            ({#'=, 'b, 11 }),
            ({#'=, 'm, ({#'([, ({ "Hi", ({#'&,'b}) }) }) }),

            ({#'=, ({#'[,],'m,"Hi",0}), 33}),
            ({#'=, ({#'&, ({#'[,],'m,"Hi",0}) }), 22}),

            ({#'&&,
                ({#'deep_eq, 'm, (["Hi": 22]) }),
                ({#'==, 'b, 33})
            })
        }))
    }),
    ({ "Lambda: Reseating of struct entry", 0,
       lambda(0,
        ({#',,
            ({#'=, 'b, 100 }),
            ({#'=, 's, ({#'(<, (<teststruct>), ({#'&,'b}), 300, 500}) }),

            ({#'=, ({#'->,'s,"a"}), 101}),
            ({#'=, ({#'&, ({#'->,'s,"a"}) }), 200}),

            ({#'&&,
                ({#'deep_eq, 's, (<teststruct> 200, 300, 500) }),
                ({#'==, 'b, 101})
            })
        }))
    }),

    ({
        "Lambda: Unprotected lvalues to mapping entries 1", 0,
        lambda(0,
        ({#',,
            ({#'=, 'm, ([:1]) }),
            ({#'=, 'v, ({#'=, ({#'[, 'm, "Hi"}), 22}) }),

            ({#'&&,
                ({#'deep_eq, 'm, (["Hi": 22]) }),
                ({#'==, 'v, 22 })
            })
        }))
    }),
    ({
        "Lambda: Unprotected lvalues to mapping entries 2", 0,
        lambda(0,
        ({#',,
            ({#'=, 'm, ([:2]) }),
            ({#'=, 'v, ({#'=, ({#'[, 'm, "Hi", 1}), 22}) }),

            ({#'&&,
                ({#'deep_eq, 'm, (["Hi": 0;22]) }),
                ({#'==, 'v, 22 })
            })
        }))
    }),
    ({
        "Lambda: Unprotected lvalues to mapping entries 3", 0,
        lambda(0,
        ({#',,
            ({#'=, 'm, (["Hi": ({11}) ]) }),
            ({#'catch, ({#'=, ({#'[, ({#'[, 'm, "Hi"}), 0}), 22}) }),

            ({#'deep_eq, 'm, (["Hi": ({22}) ]) })
        }))
    }),
    ({
        "Lambda: Unprotected lvalues to mapping entries 4", 0,
        lambda(0,
        ({#',,
            ({#'=, 'm, (["Hi": ({1,2,3,4,5}) ]) }),
            ({#'catch, ({#'=, ({#'[..], ({#'[, 'm, "Hi"}), 0, 1}), '({22}) }) }),

            ({#'deep_eq, 'm, (["Hi": ({22,3,4,5}) ]) })
        }))
    }),
    ({
        "Lambda: Unprotected lvalues to mapping entries 5", 0,
        lambda(0,
        ({#',,
            ({#'=, 'm, ([:1]) }),
            ({#'catch, ({#'=, ({#'[, ({#'[, 'm, "Hi"}), 0}), 22}) }),

            ({#'==, ({#'sizeof, 'm}), 0 })
        }))
    }),
    ({
        "Lambda: Unprotected lvalues to mapping entries 6", 0,
        lambda(0,
        ({#',,
            ({#'=, 'm, ([:1]) }),
            ({#'catch, ({#'=, ({#'[..], ({#'[, 'm, "Hi"}), 0, 1}), '({22}) }) }),

            ({#'==, ({#'sizeof, 'm}), 0 })
        }))
    }),
    ({
        "Lambda: Unprotected lvalues to mapping entries 7", 0,
        lambda(0,
        ({#',,
            ({#'=, 'm, ([:2]) }),
            ({#'=, 'v, ({#'++, ({#'[, 'm, "Hi", 1}) }) }),

            ({#'&&,
                ({#'deep_eq, 'm, (["Hi": 0;1]) }),
                ({#'==, 'v, 0 })
            })
        }))
    }),
    ({
        "Lambda: Unprotected lvalues to mapping entries 8", 0,
        lambda(0,
        ({#',,
            ({#'=, 'm, ([:2]) }),
            ({#'=, 'v, ({#'-=, ({#'[, 'm, "Hi", 1}), 1 }) }),

            ({#'&&,
                ({#'deep_eq, 'm, (["Hi": 0;-1]) }),
                ({#'==, 'v, -1 })
            })
        }))
    }),
    ({
        "Lambda: Unprotected lvalues to mapping entries 9", 0,
        lambda(0,
        ({#',,
            ({#'=, 'm, ([:1]) }),
            ({#'=, 'v, ({#'|=, ({#'[, 'm, "Hi"}), 22 }) }),

            ({#'&&,
                ({#'deep_eq, 'm, (["Hi": 22]) }),
                ({#'==, 'v, 22 })
            })
        }))
    }),
    ({
        "Lambda: Protected lvalues to mapping entries 1", 0,
        lambda(0,
        ({#',,
            ({#'=, 'm, ([:1]) }),
            ({#'=, 'v, ({#'&, ({#'[, 'm, "Hi"}) }) }),
            ({#'=, 'v, 22}),

            ({#'&&,
                ({#'deep_eq, 'm, (["Hi": 22]) }),
                ({#'==, 'v, 22 })
            })
        }))
    }),
    ({
        "Lambda: Protected lvalues to mapping entries 2", 0,
        lambda(0,
        ({#',,
            ({#'=, 'm, ([:2]) }),
            ({#'=, 'v, ({#'&, ({#'[, 'm, "Hi", 1}) }) }),
            ({#'=, 'v, 22}),

            ({#'&&,
                ({#'deep_eq, 'm, (["Hi": 0;22]) }),
                ({#'==, 'v, 22 })
            })
        }))
    }),
    ({
        "Lambda: Protected lvalues to mapping entries 3", 0,
        lambda(0,
        ({#',,
            ({#'=, 'm, (["Hi": ({11}) ]) }),
            ({#'=, 'v, ({#'[, 'm, "Hi"}) }),

            ({#'=, ({#'[, 'v, 0}), 22}),

            ({#'&&,
                ({#'deep_eq, 'm, (["Hi": ({22}) ]) }),
                ({#'deep_eq, 'v, '({22}) })
            }),
        }))
    }),
    ({
        "Lambda: Protected lvalues to mapping entries 4", 0,
        lambda(0,
        ({#',,
            ({#'=, 'm, (["Hi": ({1,2,3,4,5}) ]) }),
            ({#'=, 'v, ({#'&, ({#'[, 'm, "Hi"}) }) }),

            ({#'=, ({#'[..], 'v, 0, 1}), '({22}) }),

            ({#'&&,
                ({#'deep_eq, 'm, (["Hi": ({22,3,4,5}) ]) }),
                ({#'deep_eq, 'v, '({22,3,4,5}) })
            }),
        }))
    }),
    ({
        "Lambda: Protected lvalues to mapping entries 5", 0,
        lambda(0,
        ({#',,
            ({#'=, 'm, ([:1]) }),
            ({#'=, 'v, ({#'&, ({#'[, 'm, "Hi"}) }) }),

            ({#'catch, ({#'=, ({#'[, 'v, 0}), 22}) }),

            ({#'&&,
                ({#'==, ({#'sizeof, 'm}), 0 }),
                ({#'==, 'v, 0 })
            }),
        }))
    }),
    ({
        "Lambda: Protected lvalues to mapping entries 6", 0,
        lambda(0,
        ({#',,
            ({#'=, 'm, ([:1]) }),
            ({#'=, 'v, ({#'&, ({#'[, 'm, "Hi"}) }) }),

            ({#'catch, ({#'=, ({#'[..], 'v, 0, 1}), '({22}) }) }),

            ({#'&&,
                ({#'==, ({#'sizeof, 'm}), 0 }),
                ({#'==, 'v, 0 })
            }),
        }))
    }),
    ({
        "Lambda: Protected lvalues to mapping entries 7", 0,
        lambda(0,
        ({#',,
            ({#'=, 'm, ([:2]) }),
            ({#'=, 'v, ({#'&, ({#'[, 'm, "Hi", 1}) }) }),
            ({#'++, 'v}),

            ({#'&&,
                ({#'deep_eq, 'm, (["Hi": 0;1]) }),
                ({#'==, 'v, 1 })
            })
        }))
    }),
    ({
        "Lambda: Protected lvalues to mapping entries 8", 0,
        lambda(0,
        ({#',,
            ({#'=, 'm, ([:2]) }),
            ({#'=, 'v, ({#'&, ({#'[, 'm, "Hi", 0}) }) }),
            ({#'=, 'w, ({#'&, ({#'[, 'm, "Hi", 1}) }) }),
            ({#'-=, 'v, 1 }),
            ({#'+=, 'w, 1 }),

            ({#'&&,
                ({#'deep_eq, 'm, (["Hi": -1;1]) }),
                ({#'==, 'v, -1 }),
                ({#'==, 'w,  1 })
            })
        }))
    }),
    ({
        "Lambda: Protected lvalues to mapping entries 9", 0,
        lambda(0,
        ({#',,
            ({#'=, 'm, ([:1]) }),
            ({#'=, 'v, ({#'&, ({#'[, 'm, "Hi"}) }) }),
            ({#'|=, 'v, 22}),

            ({#'&&,
                ({#'deep_eq, 'm, (["Hi": 22]) }),
                ({#'==, 'v, 22 })
            })
        }))
    }),
    ({
        "Lambda: Protected lvalues to mapping entries 10", 0,
        lambda(0,
        ({#',,
            ({#'=, 'm, ([:1]) }),
            ({#'=, 'pair1, ({#'({, 'm, ({#'&, ({#'[, 'm, "Hi"}) }), ({#'&, ({#'[, 'm, "Hi"}) }) }) }),
            ({#'=, 'pair2, ({#'deep_copy, 'pair1}) }),

            ({#'=, ({#'[, 'pair1, 1}), 11 }),
            ({#'=, ({#'[, 'pair2, 1}), 22 }),

            ({#'&&,
                ({#'deep_eq, 'm, (["Hi": 11]) }),
                ({#'deep_eq, 'pair1, '({ (["Hi": 11]), 11, 11 }) }),
                ({#'deep_eq, 'pair2, '({ (["Hi": 22]), 22, 22 }) }),
            })
        }))
    }),
    ({
        "Lambda: Protected lvalues to mapping entries 11", 0,
        lambda(0,
        ({#',,
            ({#'=, 'm, ([:1]) }),
            ({#'=, 'pair1, ({#'({, 'm, ({#'&, ({#'[, 'm, "Hi"}) }), ({#'&, ({#'[, 'm, "Hi"}) }) }) }),
            ({#'=, 'pair2, ({#'restore_value, ({#'save_value, 'pair1}) }) }),

            ({#'=, ({#'[, 'pair1, 1}), 11 }),
            ({#'=, ({#'[, 'pair2, 1}), 22 }),

            ({#'&&,
                ({#'deep_eq, 'm, (["Hi": 11]) }),
                ({#'deep_eq, 'pair1, '({ (["Hi": 11]), 11, 11 }) }),
                ({#'deep_eq, 'pair2, '({ (["Hi": 22]), 22, 22 }) }),
            })
        }))
    }),
});

void run_test()
{
    msg("\nRunning test suite for lvalues:\n"
          "-------------------------------\n");

    run_array(tests,
        (:
            msg("At the end %d lvalues in memory.\n", driver_info(DI_NUM_LVALUES));

            if($1)
                shutdown(1);
            else
                start_gc(function void(int result)
                {
                    msg("After GC %d lvalues in memory.\n", driver_info(DI_NUM_LVALUES));
                    shutdown(result);
                });

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
