#pragma save_types, rtt_checks
#include "/inc/base.inc"
#include "/inc/deep_eq.inc"
#include "/inc/gc.inc"

#define TF_LAMBDA_ERROR 1       // lambda() should generate an error
#define TF_CALL_ERROR   2       // The call should generate an error
#define TF_SURVIVE      4       // The result doesn't matter, as long as it doesn't crash.

struct TestCase
{
    string name;                // Name of the test case.
    int flags;                  // TF_* flags
    symbol* vars;               // Variables for lambda()
    mixed expr;                 // Lambda expression
    mixed* args;                // Arguments for lambda call.
    mixed result;               // The expected result.
    closure setup;              // Closure to execute before calling the lambda.
    closure test;               // Test to evaluate the result instead of .result.
                                // (Gets the actual result and .result as arguments.)
};

mixed global_var, global_var2;

mixed get_global_var()
{
    return global_var;
}

void set_global_var(mixed value)
{
    global_var = value;
}

struct TestCase* cases = ({
    (<TestCase>
        name:           "#'|| (0 args)",
        flags:          0,
        vars:           0,
        expr:           ({#'||}),
        args:           0,
        result:         0,
    ),
    (<TestCase>
        name:           "#'|| (1 arg)",
        flags:          0,
        vars:           0,
        expr:           ({#'||, 100}),
        args:           0,
        result:         100,
    ),
    (<TestCase>
        name:           "#'|| (2 args)",
        flags:          0,
        vars:           0,
        expr:           ({#'||, 0, 200}),
        args:           0,
        result:         200,
    ),
    (<TestCase>
        name:           "#'|| (3 args)",
        flags:          0,
        vars:           0,
        expr:           ({#'||, 0, 200, 300}),
        args:           0,
        result:         200,
    ),
    (<TestCase>
        name:           "#'|| (4 args)",
        flags:          0,
        vars:           ({'x,'y, 'z}),
        expr:           ({#'||, 0, 'x, 'y, 'z}),
        args:           ({0,0,1000}),
        result:         1000,
    ),
    (<TestCase>
        name:           "#'&& (0 args)",
        flags:          0,
        vars:           0,
        expr:           ({#'&&}),
        args:           0,
        result:         1,
    ),
    (<TestCase>
        name:           "#'&& (1 arg)",
        flags:          0,
        vars:           0,
        expr:           ({#'&&, 100}),
        args:           0,
        result:         100,
    ),
    (<TestCase>
        name:           "#'&& (2 args)",
        flags:          0,
        vars:           0,
        expr:           ({#'&&, 100, 200}),
        args:           0,
        result:         200,
    ),
    (<TestCase>
        name:           "#'&& (3 args)",
        flags:          0,
        vars:           0,
        expr:           ({#'&&, 100, 200, 0}),
        args:           0,
        result:         0,
    ),
    (<TestCase>
        name:           "#'&& (4 args)",
        flags:          0,
        vars:           ({'x,'y, 'z}),
        expr:           ({#'&&, 1, 'x, 'y, 'z}),
        args:           ({2,0,3}),
        result:         0,
    ),
    (<TestCase>
        name:           "#'? (0 args)",
        flags:          0,
        vars:           0,
        expr:           ({#'?}),
        args:           0,
        result:         0,
    ),
    (<TestCase>
        name:           "#'? (1 arg)", // No condition, just a return value.
        flags:          0,
        vars:           0,
        expr:           ({#'?, 42}),
        args:           0,
        result:         42,
    ),
    (<TestCase>
        name:           "#'? (2 args)",
        flags:          0,
        vars:           0,
        expr:           ({#'?, 1, 2}),
        args:           0,
        result:         2,
    ),
    (<TestCase>
        name:           "#'? (3 args)",
        flags:          0,
        vars:           0,
        expr:           ({#'?, 1, 2, 3}),
        args:           0,
        result:         2,
    ),
    (<TestCase>
        name:           "#'? (4 args)",
        flags:          0,
        vars:           0,
        expr:           ({#'?, 0, 2, 0, 3}),
        args:           0,
        result:         0,
    ),
    (<TestCase>
        name:           "#'? (5 args)",
        flags:          0,
        vars:           0,
        expr:           ({#'?, 0, 2, 0, 3, 4}),
        args:           0,
        result:         4,
    ),
    (<TestCase>
        name:           "#'?! (0 args)",
        flags:          0,
        vars:           0,
        expr:           ({#'?!}),
        args:           0,
        result:         0,
    ),
    (<TestCase>
        name:           "#'?! (1 arg)", // No condition, just a return value.
        flags:          0,
        vars:           0,
        expr:           ({#'?!, 42}),
        args:           0,
        result:         42,
    ),
    (<TestCase>
        name:           "#'?! (2 args)",
        flags:          0,
        vars:           0,
        expr:           ({#'?!, 1, 2}),
        args:           0,
        result:         0,
    ),
    (<TestCase>
        name:           "#'?! (3 args)",
        flags:          0,
        vars:           0,
        expr:           ({#'?!, 1, 2, 3}),
        args:           0,
        result:         3,
    ),
    (<TestCase>
        name:           "#'?! (4 args)",
        flags:          0,
        vars:           0,
        expr:           ({#'?!, 0, 2, 0, 3}),
        args:           0,
        result:         2,
    ),
    (<TestCase>
        name:           "#'?! (5 args)",
        flags:          0,
        vars:           0,
        expr:           ({#'?!, 1, 2, 0, 3, 4}),
        args:           0,
        result:         3,
    ),
    (<TestCase>
        name:           "#', (0 args)",
        flags:          0,
        vars:           0,
        expr:           ({#',}),
        args:           0,
        result:         0,
    ),
    (<TestCase>
        name:           "#', (1 arg)",
        flags:          0,
        vars:           0,
        expr:           ({#',, 123}),
        args:           0,
        result:         123,
    ),
    (<TestCase>
        name:           "#', (2 args)",
        flags:          0,
        vars:           0,
        expr:           ({#',, 123, 234}),
        args:           0,
        result:         234,
    ),
    (<TestCase>
        name:           "#', (3 args)",
        flags:          0,
        vars:           0,
        expr:           ({#',, 123, 234, 345}),
        args:           0,
        result:         345,
    ),
    (<TestCase>
        name:           "#'= (0 args)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'=}),
    ),
    (<TestCase>
        name:           "#'= (1 arg)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'=, 'a}),
    ),
    (<TestCase>
        name:           "#'= (2 args, no lvalue)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'=, ({#'+,1,1}), 2}),
    ),
    (<TestCase>
        name:           "#'= (2 args, symbol)",
        flags:          0,
        vars:           0,
        expr:           ({#'=, 'a, 42}),
        args:           0,
        result:         42,
    ),
    (<TestCase>
        name:           "#'= (2 args, identifier 1)",
        flags:          0,
        vars:           0,
        expr:           ({#'=, #'global_var, 142}),
        args:           0,
        test:           function int(mixed result) { return result == 142 && global_var == 142; },
    ),
    (<TestCase>
        name:           "#'= (2 args, identifier 2)",
        flags:          0,
        vars:           0,
        expr:           ({#'=, ({#'global_var}), 242}),
        args:           0,
        test:           function int(mixed result) { return result == 242 && global_var == 242; },
    ),
    (<TestCase>
        name:           "#'= (2 args, array index)",
        flags:          0,
        vars:           0,
        expr:           ({#'=, ({#'[, ({#'global_var}), 1}), 342}),
        args:           0,
        setup:          function void() { global_var = ({1,2,3}); },
        test:           function int(mixed result) { return result == 342 && deep_eq(global_var, ({1,342,3})); },
    ),
    (<TestCase>
        name:           "#'= (2 args, array reverse index)",
        flags:          0,
        vars:           0,
        expr:           ({#'=, ({#'[<, ({#'global_var}), 1}), 442}),
        args:           0,
        setup:          function void() { global_var = ({1,2,3}); },
        test:           function int(mixed result) { return result == 442 && deep_eq(global_var, ({1,2,442})); },
    ),
    (<TestCase>
        name:           "#'= (2 args, array arithmetic index)",
        flags:          0,
        vars:           0,
        expr:           ({#'=, ({#'[>, ({#'global_var}), -1}), 542}),
        args:           0,
        setup:          function void() { global_var = ({1,2,3}); },
        test:           function int(mixed result) { return result == 542 && deep_eq(global_var, ({1,2,542})); },
    ),
    (<TestCase>
        name:           "#'= (2 args, mapping index)",
        flags:          0,
        vars:           0,
        expr:           ({#'=, ({#'[, ({#'global_var}), "X"}), 642}),
        args:           0,
        setup:          function void() { global_var = ([]); },
        test:           function int(mixed result) { return result == 642 && deep_eq(global_var, (["X":642])); },
    ),
    (<TestCase>
        name:           "#'= (2 args, mapping wide index 1)",
        flags:          0,
        vars:           0,
        expr:           ({#'=, ({#'[, ({#'global_var}), "X", 1}), 742}),
        args:           0,
        setup:          function void() { global_var = ([:2]); },
        test:           function int(mixed result) { return result == 742 && deep_eq(global_var, (["X":0;742])); },
    ),
    (<TestCase>
        name:           "#'= (2 args, mapping wide index 2)",
        flags:          0,
        vars:           0,
        expr:           ({#'=, ({#'[,], ({#'global_var}), "X", 1}), 842}),
        args:           0,
        setup:          function void() { global_var = ([:2]); },
        test:           function int(mixed result) { return result == 842 && deep_eq(global_var, (["X":0;842])); },
    ),
    (<TestCase>
        name:           "#'= (2 args, mapping reverse index)",
        flags:          0,
        vars:           0,
        expr:           ({#'=, ({#'[,<], ({#'global_var}), "X", 2}), 842}),
        args:           0,
        setup:          function void() { global_var = ([:5]); },
        test:           function int(mixed result) { return result == 842 && deep_eq(global_var, (["X":0;0;0;842;0])); },
    ),
    (<TestCase>
        name:           "#'= (2 args, mapping arithmetic index)",
        flags:          0,
        vars:           0,
        expr:           ({#'=, ({#'[,>], ({#'global_var}), "X", -4}), 842}),
        args:           0,
        setup:          function void() { global_var = ([:5]); },
        test:           function int(mixed result) { return result == 842 && deep_eq(global_var, (["X":0;842;0;0;0])); },
    ),
    (<TestCase>
        name:           "#'= (2 args, strict struct access)",
        flags:          0,
        vars:           0,
        expr:           ({#'=, ({#'., ({#'global_var}), "result"}), 942}),
        args:           0,
        setup:          function void() { global_var = (<TestCase>); },
        test:           function int(mixed result) { return result == 942 && global_var.result == 942; },
    ),
    (<TestCase>
        name:           "#'= (2 args, relaxed struct access)",
        flags:          0,
        vars:           0,
        expr:           ({#'=, ({#'->, ({#'global_var}), "result"}), 1042}),
        args:           0,
        setup:          function void() { global_var = (<TestCase>); },
        test:           function int(mixed result) { return result == 1042 && global_var.result == 1042; },
    ),
    (<TestCase>
        name:           "#'= (2 args, array[..])",
        flags:          0,
        vars:           0,
        expr:           ({#'=, ({#'[..], ({#'global_var}), 1, 2}), '({1,2,3}) }),
        args:           0,
        setup:          function void() { global_var = ({-1,-2,-3,-4,-5}); },
        test:           function int(mixed result) { return deep_eq(result, ({1,2,3})) && deep_eq(global_var, ({-1,1,2,3,-4,-5})); },
    ),
    (<TestCase>
        name:           "#'= (2 args, array[..<])",
        flags:          0,
        vars:           0,
        expr:           ({#'=, ({#'[..<], ({#'global_var}), 1, 3}), '({1,2,3}) }),
        args:           0,
        setup:          function void() { global_var = ({-1,-2,-3,-4,-5}); },
        test:           function int(mixed result) { return deep_eq(result, ({1,2,3})) && deep_eq(global_var, ({-1,1,2,3,-4,-5})); },
    ),
    (<TestCase>
        name:           "#'= (2 args, array[..>])",
        flags:          0,
        vars:           0,
        expr:           ({#'=, ({#'[..>], ({#'global_var}), 1, -3}), '({1,2,3}) }),
        args:           0,
        setup:          function void() { global_var = ({-1,-2,-3,-4,-5}); },
        test:           function int(mixed result) { return deep_eq(result, ({1,2,3})) && deep_eq(global_var, ({-1,1,2,3,-4,-5})); },
    ),
    (<TestCase>
        name:           "#'= (2 args, array[..)",
        flags:          0,
        vars:           0,
        expr:           ({#'=, ({#'[.., ({#'global_var}), 1}), '({1,2,3}) }),
        args:           0,
        setup:          function void() { global_var = ({-1,-2,-3,-4,-5}); },
        test:           function int(mixed result) { return deep_eq(result, ({1,2,3})) && deep_eq(global_var, ({-1,1,2,3})); },
    ),
    (<TestCase>
        name:           "#'= (2 args, array[<..])",
        flags:          0,
        vars:           0,
        expr:           ({#'=, ({#'[<..], ({#'global_var}), 4, 2}), '({1,2,3}) }),
        args:           0,
        setup:          function void() { global_var = ({-1,-2,-3,-4,-5}); },
        test:           function int(mixed result) { return deep_eq(result, ({1,2,3})) && deep_eq(global_var, ({-1,1,2,3,-4,-5})); },
    ),
    (<TestCase>
        name:           "#'= (2 args, array[<..<])",
        flags:          0,
        vars:           0,
        expr:           ({#'=, ({#'[<..<], ({#'global_var}), 4, 3}), '({1,2,3}) }),
        args:           0,
        setup:          function void() { global_var = ({-1,-2,-3,-4,-5}); },
        test:           function int(mixed result) { return deep_eq(result, ({1,2,3})) && deep_eq(global_var, ({-1,1,2,3,-4,-5})); },
    ),
    (<TestCase>
        name:           "#'= (2 args, array[<..>])",
        flags:          0,
        vars:           0,
        expr:           ({#'=, ({#'[<..>], ({#'global_var}), 4, -3}), '({1,2,3}) }),
        args:           0,
        setup:          function void() { global_var = ({-1,-2,-3,-4,-5}); },
        test:           function int(mixed result) { return deep_eq(result, ({1,2,3})) && deep_eq(global_var, ({-1,1,2,3,-4,-5})); },
    ),
    (<TestCase>
        name:           "#'= (2 args, array[<..)",
        flags:          0,
        vars:           0,
        expr:           ({#'=, ({#'[<.., ({#'global_var}), 4}), '({1,2,3}) }),
        args:           0,
        setup:          function void() { global_var = ({-1,-2,-3,-4,-5}); },
        test:           function int(mixed result) { return deep_eq(result, ({1,2,3})) && deep_eq(global_var, ({-1,1,2,3})); },
    ),
    (<TestCase>
        name:           "#'= (2 args, array[>..])",
        flags:          0,
        vars:           0,
        expr:           ({#'=, ({#'[>..], ({#'global_var}), -4, 2}), '({1,2,3}) }),
        args:           0,
        setup:          function void() { global_var = ({-1,-2,-3,-4,-5}); },
        test:           function int(mixed result) { return deep_eq(result, ({1,2,3})) && deep_eq(global_var, ({-1,1,2,3,-4,-5})); },
    ),
    (<TestCase>
        name:           "#'= (2 args, array[>..<])",
        flags:          0,
        vars:           0,
        expr:           ({#'=, ({#'[>..<], ({#'global_var}), -4, 3}), '({1,2,3}) }),
        args:           0,
        setup:          function void() { global_var = ({-1,-2,-3,-4,-5}); },
        test:           function int(mixed result) { return deep_eq(result, ({1,2,3})) && deep_eq(global_var, ({-1,1,2,3,-4,-5})); },
    ),
    (<TestCase>
        name:           "#'= (2 args, array[>..>])",
        flags:          0,
        vars:           0,
        expr:           ({#'=, ({#'[>..>], ({#'global_var}), -4, -3}), '({1,2,3}) }),
        args:           0,
        setup:          function void() { global_var = ({-1,-2,-3,-4,-5}); },
        test:           function int(mixed result) { return deep_eq(result, ({1,2,3})) && deep_eq(global_var, ({-1,1,2,3,-4,-5})); },
    ),
    (<TestCase>
        name:           "#'= (2 args, array[>..)",
        flags:          0,
        vars:           0,
        expr:           ({#'=, ({#'[>.., ({#'global_var}), -4}), '({1,2,3}) }),
        args:           0,
        setup:          function void() { global_var = ({-1,-2,-3,-4,-5}); },
        test:           function int(mixed result) { return deep_eq(result, ({1,2,3})) && deep_eq(global_var, ({-1,1,2,3})); },
    ),
    (<TestCase>
        name:           "#'= (2 args, mapping[,..])",
        flags:          0,
        vars:           0,
        expr:           ({#'=, ({#'[,..], ({#'global_var}), "X", 1, 2}), '({11,22}) }),
        args:           0,
        setup:          function void() { global_var = ([:5]); },
        test:           function int(mixed result) { return deep_eq(result, ({11,22})) && deep_eq(global_var, (["X":0;11;22;0;0])); },
    ),
    (<TestCase>
        name:           "#'= (2 args, mapping[,..<])",
        flags:          0,
        vars:           0,
        expr:           ({#'=, ({#'[,..<], ({#'global_var}), "X", 1, 3}), '({11,22}) }),
        args:           0,
        setup:          function void() { global_var = ([:5]); },
        test:           function int(mixed result) { return deep_eq(result, ({11,22})) && deep_eq(global_var, (["X":0;11;22;0;0])); },
    ),
    (<TestCase>
        name:           "#'= (2 args, mapping[,..>])",
        flags:          0,
        vars:           0,
        expr:           ({#'=, ({#'[,..>], ({#'global_var}), "X", 1, -3}), '({11,22}) }),
        args:           0,
        setup:          function void() { global_var = ([:5]); },
        test:           function int(mixed result) { return deep_eq(result, ({11,22})) && deep_eq(global_var, (["X":0;11;22;0;0])); },
    ),
    (<TestCase>
        name:           "#'= (2 args, mapping[,..)",
        flags:          0,
        vars:           0,
        expr:           ({#'=, ({#'[,.., ({#'global_var}), "X", 1}), '({11,22,33,44}) }),
        args:           0,
        setup:          function void() { global_var = ([:5]); },
        test:           function int(mixed result) { return deep_eq(result, ({11,22,33,44})) && deep_eq(global_var, (["X":0;11;22;33;44])); },
    ),
    (<TestCase>
        name:           "#'= (2 args, mapping[,<..])",
        flags:          0,
        vars:           0,
        expr:           ({#'=, ({#'[,<..], ({#'global_var}), "X", 4, 2}), '({11,22}) }),
        args:           0,
        setup:          function void() { global_var = ([:5]); },
        test:           function int(mixed result) { return deep_eq(result, ({11,22})) && deep_eq(global_var, (["X":0;11;22;0;0])); },
    ),
    (<TestCase>
        name:           "#'= (2 args, mapping[,<..<])",
        flags:          0,
        vars:           0,
        expr:           ({#'=, ({#'[,<..<], ({#'global_var}), "X", 4, 3}), '({11,22}) }),
        args:           0,
        setup:          function void() { global_var = ([:5]); },
        test:           function int(mixed result) { return deep_eq(result, ({11,22})) && deep_eq(global_var, (["X":0;11;22;0;0])); },
    ),
    (<TestCase>
        name:           "#'= (2 args, mapping[,<..>])",
        flags:          0,
        vars:           0,
        expr:           ({#'=, ({#'[,<..>], ({#'global_var}), "X", 4, -3}), '({11,22}) }),
        args:           0,
        setup:          function void() { global_var = ([:5]); },
        test:           function int(mixed result) { return deep_eq(result, ({11,22})) && deep_eq(global_var, (["X":0;11;22;0;0])); },
    ),
    (<TestCase>
        name:           "#'= (2 args, mapping[,<..)",
        flags:          0,
        vars:           0,
        expr:           ({#'=, ({#'[,<.., ({#'global_var}), "X", 4}), '({11,22,33,44}) }),
        args:           0,
        setup:          function void() { global_var = ([:5]); },
        test:           function int(mixed result) { return deep_eq(result, ({11,22,33,44})) && deep_eq(global_var, (["X":0;11;22;33;44])); },
    ),
    (<TestCase>
        name:           "#'= (2 args, mapping[,>..])",
        flags:          0,
        vars:           0,
        expr:           ({#'=, ({#'[,>..], ({#'global_var}), "X", -4, 2}), '({11,22}) }),
        args:           0,
        setup:          function void() { global_var = ([:5]); },
        test:           function int(mixed result) { return deep_eq(result, ({11,22})) && deep_eq(global_var, (["X":0;11;22;0;0])); },
    ),
    (<TestCase>
        name:           "#'= (2 args, mapping[,>..<])",
        flags:          0,
        vars:           0,
        expr:           ({#'=, ({#'[,>..<], ({#'global_var}), "X", -4, 3}), '({11,22}) }),
        args:           0,
        setup:          function void() { global_var = ([:5]); },
        test:           function int(mixed result) { return deep_eq(result, ({11,22})) && deep_eq(global_var, (["X":0;11;22;0;0])); },
    ),
    (<TestCase>
        name:           "#'= (2 args, mapping[,>..>])",
        flags:          0,
        vars:           0,
        expr:           ({#'=, ({#'[,>..>], ({#'global_var}), "X", -4, -3}), '({11,22}) }),
        args:           0,
        setup:          function void() { global_var = ([:5]); },
        test:           function int(mixed result) { return deep_eq(result, ({11,22})) && deep_eq(global_var, (["X":0;11;22;0;0])); },
    ),
    (<TestCase>
        name:           "#'= (2 args, mapping[,>..)",
        flags:          0,
        vars:           0,
        expr:           ({#'=, ({#'[,>.., ({#'global_var}), "X", -4}), '({11,22,33,44}) }),
        args:           0,
        setup:          function void() { global_var = ([:5]); },
        test:           function int(mixed result) { return deep_eq(result, ({11,22,33,44})) && deep_eq(global_var, (["X":0;11;22;33;44])); },
    ),
    (<TestCase>
        name:           "#'= (2 args, &var)",
        flags:          0,
        vars:           0,
        expr:           ({#'=, ({#'&, #'global_var}), 51}),
        args:           0,
        setup:          function void() { global_var2 = ([:3]); global_var = &global_var2; },
        test:           function int(mixed result) { return result == 51 && mappingp(global_var2) && global_var == 51; },
    ),
    (<TestCase>
        name:           "#'= (3 arg)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'=, 'a, 52, 'b}),
    ),
    (<TestCase>
        name:           "#'= (4 args, symbols)",
        flags:          0,
        vars:           0,
        expr:           ({#'=, 'a, 152, 'b, 153}),
        args:           0,
        result:         153,
    ),
    (<TestCase>
        name:           "#'= (4 args, identifier)",
        flags:          0,
        vars:           0,
        expr:           ({#'=, #'global_var, 152, #'global_var2, 153}),
        args:           0,
        test:           function int(mixed result) { return result == 153 && global_var == 152 && global_var2 == 153; },
    ),
    (<TestCase>
        name:           "#'+= (0 args)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'+=}),
    ),
    (<TestCase>
        name:           "#'+= (1 arg)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'+=, 'a}),
    ),
    (<TestCase>
        name:           "#'+= (2 args, no lvalue)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'+=, ({#'+,1,1}), 2}),
    ),
    (<TestCase>
        name:           "#'+= (2 args, symbol, 1)",
        flags:          0,
        vars:           ({'a}),
        expr:           ({#'+=, 'a, 1}),
        args:           ({ 100 }),
        result:         101,
    ),
    (<TestCase>
        name:           "#'+= (2 args, symbol, 2)",
        flags:          0,
        vars:           ({'a}),
        expr:           ({#'+=, 'a, 2}),
        args:           ({ 100 }),
        result:         102,
    ),
    (<TestCase>
        name:           "#'-= (0 args)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'-=}),
    ),
    (<TestCase>
        name:           "#'-= (1 arg)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'-=, 'a}),
    ),
    (<TestCase>
        name:           "#'-= (2 args, no lvalue)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'-=, ({#'+,1,1}), 2}),
    ),
    (<TestCase>
        name:           "#'-= (2 args, symbol, 1)",
        flags:          0,
        vars:           ({'a}),
        expr:           ({#'-=, 'a, 1}),
        args:           ({ 100 }),
        result:         99,
    ),
    (<TestCase>
        name:           "#'-= (2 args, symbol, 2)",
        flags:          0,
        vars:           ({'a}),
        expr:           ({#'-=, 'a, 2}),
        args:           ({ 100 }),
        result:         98,
    ),
    (<TestCase>
        name:           "#'*= (0 args)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'*=}),
    ),
    (<TestCase>
        name:           "#'*= (1 arg)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'*=, 'a}),
    ),
    (<TestCase>
        name:           "#'*= (2 args, no lvalue)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'*=, ({#'+,1,1}), 2}),
    ),
    (<TestCase>
        name:           "#'*= (2 args, symbol)",
        flags:          0,
        vars:           ({'a}),
        expr:           ({#'*=, 'a, 2}),
        args:           ({ 100 }),
        result:         200,
    ),
    (<TestCase>
        name:           "#'/= (0 args)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'/=}),
    ),
    (<TestCase>
        name:           "#'/= (1 arg)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'/=, 'a}),
    ),
    (<TestCase>
        name:           "#'/= (2 args, no lvalue)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'/=, ({#'+,1,1}), 2}),
    ),
    (<TestCase>
        name:           "#'/= (2 args, symbol)",
        flags:          0,
        vars:           ({'a}),
        expr:           ({#'/=, 'a, 2}),
        args:           ({ 100 }),
        result:         50,
    ),
    (<TestCase>
        name:           "#'%= (0 args)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'%=}),
    ),
    (<TestCase>
        name:           "#'%= (1 arg)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'%=, 'a}),
    ),
    (<TestCase>
        name:           "#'%= (2 args, no lvalue)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'%=, ({#'+,1,1}), 2}),
    ),
    (<TestCase>
        name:           "#'%= (2 args, symbol)",
        flags:          0,
        vars:           ({'a}),
        expr:           ({#'%=, 'a, 7}),
        args:           ({ 100 }),
        result:         2,
    ),
    (<TestCase>
        name:           "#'&= (0 args)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'&=}),
    ),
    (<TestCase>
        name:           "#'&= (1 arg)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'&=, 'a}),
    ),
    (<TestCase>
        name:           "#'&= (2 args, no lvalue)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'&=, ({#'+,1,1}), 2}),
    ),
    (<TestCase>
        name:           "#'&= (2 args, symbol)",
        flags:          0,
        vars:           ({'a}),
        expr:           ({#'&=, 'a, 99}),
        args:           ({ 100 }),
        result:         96,
    ),
    (<TestCase>
        name:           "#'|= (0 args)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'|=}),
    ),
    (<TestCase>
        name:           "#'|= (1 arg)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'|=, 'a}),
    ),
    (<TestCase>
        name:           "#'|= (2 args, no lvalue)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'|=, ({#'+,1,1}), 2}),
    ),
    (<TestCase>
        name:           "#'|= (2 args, symbol)",
        flags:          0,
        vars:           ({'a}),
        expr:           ({#'|=, 'a, 99}),
        args:           ({ 100 }),
        result:         103,
    ),
    (<TestCase>
        name:           "#'^= (0 args)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'^=}),
    ),
    (<TestCase>
        name:           "#'^= (1 arg)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'^=, 'a}),
    ),
    (<TestCase>
        name:           "#'^= (2 args, no lvalue)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'^=, ({#'+,1,1}), 2}),
    ),
    (<TestCase>
        name:           "#'^= (2 args, symbol)",
        flags:          0,
        vars:           ({'a}),
        expr:           ({#'^=, 'a, 99}),
        args:           ({ 100 }),
        result:         7,
    ),
    (<TestCase>
        name:           "#'<<= (0 args)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'<<=}),
    ),
    (<TestCase>
        name:           "#'<<= (1 arg)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'<<=, 'a}),
    ),
    (<TestCase>
        name:           "#'<<= (2 args, no lvalue)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'<<=, ({#'+,1,1}), 2}),
    ),
    (<TestCase>
        name:           "#'<<= (2 args, positive symbol)",
        flags:          0,
        vars:           ({'a}),
        expr:           ({#'<<=, 'a, 2}),
        args:           ({ 100 }),
        result:         400,
    ),
    (<TestCase>
        name:           "#'<<= (2 args, negative symbol)",
        flags:          0,
        vars:           ({'a}),
        expr:           ({#'<<=, 'a, 2}),
        args:           ({ -100 }),
        result:         -400,
    ),
    (<TestCase>
        name:           "#'>>= (0 args)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'>>=}),
    ),
    (<TestCase>
        name:           "#'>>= (1 arg)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'>>=, 'a}),
    ),
    (<TestCase>
        name:           "#'>>= (2 args, no lvalue)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'>>=, ({#'+,1,1}), 2}),
    ),
    (<TestCase>
        name:           "#'>>= (2 args, positive symbol)",
        flags:          0,
        vars:           ({'a}),
        expr:           ({#'>>=, 'a, 2}),
        args:           ({ 100 }),
        result:         25,
    ),
    (<TestCase>
        name:           "#'>>= (2 args, negative symbol)",
        flags:          0,
        vars:           ({'a}),
        expr:           ({#'>>=, 'a, 2}),
        args:           ({ -100 }),
        result:         -25,
    ),
    (<TestCase>
        name:           "#'>>>= (0 args)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'>>>=}),
    ),
    (<TestCase>
        name:           "#'>>>= (1 arg)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'>>>=, 'a}),
    ),
    (<TestCase>
        name:           "#'>>>= (2 args, no lvalue)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'>>>=, ({#'+,1,1}), 2}),
    ),
    (<TestCase>
        name:           "#'>>>= (2 args, positive symbol)",
        flags:          0,
        vars:           ({'a}),
        expr:           ({#'>>>=, 'a, 2}),
        args:           ({ 100 }),
        result:         25,
    ),
    (<TestCase>
        name:           "#'>>= (2 args, negative symbol)",
        flags:          0,
        vars:           ({'a}),
        expr:           ({#'>>>=, 'a, 2}),
        args:           ({ -100 }),
        test:           function int(mixed result) { return result > 0; },
    ),
    (<TestCase>
        name:           "#'&&= (0 args)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'&&=}),
    ),
    (<TestCase>
        name:           "#'&&= (1 arg)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'&&=, 'a}),
    ),
    (<TestCase>
        name:           "#'&&= (2 args, no lvalue)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'&&=, ({#'+,1,1}), 2}),
    ),
    (<TestCase>
        name:           "#'&&= (2 args, symbol == 0)",
        flags:          0,
        vars:           ({'a}),
        expr:           ({#'&&=, 'a, 99}),
        args:           ({ 0 }),
        result:         0,
    ),
    (<TestCase>
        name:           "#'&&= (2 args, symbol != 0)",
        flags:          0,
        vars:           ({'a}),
        expr:           ({#'&&=, 'a, 99}),
        args:           ({ 100 }),
        result:         99,
    ),
    (<TestCase>
        name:           "#'||= (0 args)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'||=}),
    ),
    (<TestCase>
        name:           "#'||= (1 arg)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'||=, 'a}),
    ),
    (<TestCase>
        name:           "#'||= (2 args, no lvalue)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'||=, ({#'+,1,1}), 2}),
    ),
    (<TestCase>
        name:           "#'||= (2 args, symbol == 0)",
        flags:          0,
        vars:           ({'a}),
        expr:           ({#'||=, 'a, 99}),
        args:           ({ 0 }),
        result:         99,
    ),
    (<TestCase>
        name:           "#'||= (2 args, symbol != 0)",
        flags:          0,
        vars:           ({'a}),
        expr:           ({#'||=, 'a, 99}),
        args:           ({ 100 }),
        result:         100,
    ),
    (<TestCase>
        name:           "#'++ (0 args)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'++}),
    ),
    (<TestCase>
        name:           "#'++ (1 arg)",
        flags:          0,
        vars:           0,
        expr:           ({#'++, #'global_var}),
        setup:          function void() { global_var = 100; },
        test:           function int(mixed result) { return result == 100 && global_var == 101; },
    ),
    (<TestCase>
        name:           "#'-- (0 args)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'--}),
    ),
    (<TestCase>
        name:           "#'-- (1 arg)",
        flags:          0,
        vars:           0,
        expr:           ({#'--, #'global_var}),
        setup:          function void() { global_var = 100; },
        test:           function int(mixed result) { return result == 100 && global_var == 99; },
    ),
    (<TestCase>
        name:           "#'do (0 args)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'do}),
    ),
    (<TestCase>
        name:           "#'do (1 arg)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'do, 0}),
    ),
    (<TestCase>
        name:           "#'do (2 args)",
        flags:          0,
        vars:           ({'a, 'b}),
        expr:           ({#'do, ({#'--, 'a}), 'b}),
        args:           ({ 5, 100 }),
        result:         100,
    ),
    (<TestCase>
        name:           "#'do (4 args)",
        flags:          0,
        vars:           ({'a, 'b}),
        expr:           ({#'do, ({#'++, #'global_var}), ({#'++, #'global_var2}), ({#'--, 'a}), 'b}),
        args:           ({ 5, 100 }),
        setup:          function void() { global_var = 10; global_var2 = 20; },
        test:           function int(mixed result) { return result == 100 && global_var == 16 && global_var2 == 26; },
    ),
    (<TestCase>
        name:           "#'while (0 args)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'while}),
    ),
    (<TestCase>
        name:           "#'while (1 arg)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'while, 0}),
    ),
    (<TestCase>
        name:           "#'while (2 args)",
        flags:          0,
        vars:           ({'a, 'b}),
        expr:           ({#'while, ({#'--, 'a}), 'b}),
        args:           ({ 5, 100 }),
        result:         100,
    ),
    (<TestCase>
        name:           "#'while (4 args)",
        flags:          0,
        vars:           ({'a, 'b}),
        expr:           ({#'while, ({#'--, 'a}), 'b, ({#'++, #'global_var}), ({#'++, #'global_var2}) }),
        args:           ({ 5, 100 }),
        setup:          function void() { global_var = 30; global_var2 = 40; },
        test:           function int(mixed result) { return result == 100 && global_var == 35 && global_var2 == 45; },
    ),
    (<TestCase>
        name:           "#'foreach (0 args)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'foreach}),
    ),
    (<TestCase>
        name:           "#'foreach (1 arg)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'foreach, ({'a}) }),
    ),
    (<TestCase>
        name:           "#'foreach (2 args)",
        flags:          0,
        vars:           0,
        expr:           ({#'foreach, ({'a}), (["X", "Y"]) }),
        args:           0,
        result:         0,
    ),
    (<TestCase>
        name:           "#'foreach with symbols",
        flags:          0,
        vars:           0,
        expr:           ({#'foreach, ({'a, 'b, 'c}), ([ "X": 111; 222 ]), ({#'+=, #'global_var, 'a}), ({#'+=, #'global_var2, 'c}) }),
        args:           0,
        setup:          function void() { global_var = "*"; global_var2 = 1000; },
        test:           function int(mixed result) { return result == 0 && global_var == "*X" && global_var2 == 1222; },
    ),
    (<TestCase>
        name:           "#'foreach with identifiers",
        flags:          0,
        vars:           0,
        expr:           ({#'foreach, ({#'global_var, #'global_var2}), ([ "A": 555 ]), ({#'+=, #'global_var, ";"}), ({#'+=, #'global_var2, 222}) }),
        args:           0,
        setup:          function void() { global_var = "*"; global_var2 = 1000; },
        test:           function int(mixed result) { return result == 0 && global_var == "A;" && global_var2 == 777; },
    ),
    (<TestCase>
        name:           "#'catch (0 args)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'catch}),
    ),
    (<TestCase>
        name:           "#'catch with throw",
        flags:          0,
        vars:           0,
        expr:           ({#'catch, ({#'throw, 1234}) }),
        args:           0,
        result:         1234,
    ),
    (<TestCase>
        name:           "#'catch with raise_error",
        flags:          0,
        vars:           0,
        expr:           ({#'catch, ({#'raise_error, "Error!\n"}) }),
        args:           0,
        result:         "*Error!\n",
    ),
    (<TestCase>
        name:           "#'catch with throw and all options",
        flags:          0,
        vars:           0,
        expr:           ({#'catch, ({#'throw, 12345}), 'nolog, 'publish, 'reserve, ({#'+=, #'global_var, 1000 }), 'limit, ({#'+=, #'global_var, 10}) }),
        args:           0,
        setup:          function void() { global_var = 2000; },
        test:           function int(mixed result) { return result == 12345 && global_var == 3010; },
    ),
    (<TestCase>
        name:           "#'catch with multiple 'reserve options",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'catch, ({#',, 1}), 'reserve, 1000, 'reserve, 2000 }),
        result:         0,
    ),
    (<TestCase>
        name:           "#'catch with 'limit option",
        flags:          0,
        vars:           0,
        expr:           ({#'catch, ({#'=, #'global_var, ({#'get_eval_cost}) }), 'limit, 2100 }),
        test:           function int(mixed result) { return global_var > 2000 && global_var < 2100; },
    ),
    (<TestCase>
        name:           "#'catch with 'limit and 'reserve option",
        flags:          0,
        vars:           0,
        expr:           ({#',, ({#'catch, ({#'=, 'result, ({#'get_eval_cost}) }), 'limit, ({#'+=, #'global_var, 100}), 'reserve, ({#'*=, #'global_var, 2}) }), 'result }),
        setup:          function void() { global_var = 2000; },
        test:           function int(mixed result) { return result > 2000 && result < 2100 && global_var == 4200; },
    ),
    (<TestCase>
        name:           "#'catch with 'reserve and 'limit option",
        flags:          0,
        vars:           0,
        expr:           ({#',, ({#'catch, ({#'=, 'result, ({#'get_eval_cost}) }), 'reserve, ({#'*=, #'global_var, 2}), 'limit, ({#'+=, #'global_var, 500}) }), 'result }),
        setup:          function void() { global_var = 1000; },
        test:           function int(mixed result) { return result > 2400 && result < 2500 && global_var == 2500; },
    ),
    (<TestCase>
        name:           "#'catch with multiple 'limit options",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'catch, ({#',, 1}), 'limit, 1000, 'limit, 2000 }),
        result:         0,
    ),
    (<TestCase>
        name:           "#'({ (0 args)",
        flags:          0,
        vars:           0,
        expr:           ({#'({}),
        args:           0,
        result:         ({}),
    ),
    (<TestCase>
        name:           "#'({ (1 arg)",
        flags:          0,
        vars:           0,
        expr:           ({#'({, 3}),
        args:           0,
        result:         ({3}),
    ),
    (<TestCase>
        name:           "#'({ (2 args)",
        flags:          0,
        vars:           ({'a, 'b}),
        expr:           ({#'({, 'b, 'a}),
        args:           ({'X','Y'}),
        result:         ({'Y','X'}),
    ),
    (<TestCase>
        name:           "#'([ (0 args)",
        flags:          0,
        vars:           0,
        expr:           ({#'([}),
        args:           0,
        result:         ([:0]),
    ),
    (<TestCase>
        name:           "#'([ (1x1))",
        flags:          0,
        vars:           0,
        expr:           ({#'([, ({10})}),
        args:           0,
        result:         ([10]),
    ),
    (<TestCase>
        name:           "#'([ (1x2))",
        flags:          0,
        vars:           0,
        expr:           ({#'([, ({10, 11})}),
        args:           0,
        result:         ([10:11]),
    ),
    (<TestCase>
        name:           "#'([ (1x3))",
        flags:          0,
        vars:           0,
        expr:           ({#'([, ({10, 11, 12})}),
        args:           0,
        result:         ([10: 11; 12]),
    ),
    (<TestCase>
        name:           "#'([ (2x3))",
        flags:          0,
        vars:           0,
        expr:           ({#'([, ({10, 11, 12}), ({20, 21, 22})}),
        args:           0,
        result:         ([10: 11; 12, 20: 21; 22]),
    ),
    (<TestCase>
        name:           "#'([ (2x2, double key))",
        flags:          0,
        vars:           0,
        expr:           ({#'([, ({"A", '({65}) }), ({"A", '({65})})}),
        args:           0,
        result:         (["A": ({65}) ]),
    ),
    (<TestCase>
        name:           "#'([ (500x2)",
        flags:          0,
        vars:           0,
        expr:           funcall(function mixed*()
                        {
                            mixed *expr = ({ #'([ });
                            foreach (int i: 500)
                                expr += ({ ({ i, 1000+i }) });
                            return expr;
                        }),
        args:           0,
        result:         funcall(function mapping()
                        {
                            mapping result = ([:1]);
                            foreach (int i: 500)
                                m_add(result, i, 1000+i);
                            return result;
                        }),
    ),
    (<TestCase>
        name:           "#'([ (2x500)",
        flags:          0,
        vars:           0,
        expr:           funcall(function mixed*()
                        {
                            mixed *expr = ({ #'([, ({}), ({}) });
                            foreach (int i: 500)
                            {
                                expr[1] += ({ i });
                                expr[2] += ({ 1000+i });
                            }
                            return expr;
                        }),
        args:           0,
        result:         funcall(function mapping()
                        {
                            mapping result = ([:499]);
                            int *entry1 = ({}), *entry2 = ({});
                            foreach (int i: 500)
                            {
                                entry1 += ({ i });
                                entry2 += ({ 1000+i });
                            }
                            m_add(result, entry1...);
                            m_add(result, entry2...);
                            return result;
                        }),
    ),
    (<TestCase>
        name:           "#'(< (0 args)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({ #'(< }),
    ),
    (<TestCase>
        name:           "#'(< (1 arg)",
        flags:          0,
        vars:           0,
        expr:           ({ #'(<, (<TestCase> "Test") }),
        args:           0,
        result:         (<TestCase>),
    ),
    (<TestCase>
        name:           "#'(< (2 args)",
        flags:          0,
        vars:           0,
        expr:           ({ #'(<, (<TestCase> "Test"), "X" }),
        args:           0,
        result:         (<TestCase> "X"),
    ),
    (<TestCase>
        name:           "#'return (0 args)",
        flags:          0,
        vars:           0,
        expr:           ({ #'return }),
        args:           0,
        result:         0,
    ),
    (<TestCase>
        name:           "#'return (1 arg)",
        flags:          0,
        vars:           0,
        expr:           ({ #'return, -1 }),
        args:           0,
        result:         -1,
    ),
    (<TestCase>
        name:           "#'return (2 argss)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({ #'return, 1, 2 }),
    ),
    (<TestCase>
        name:           "#'return (nested)",
        flags:          0,
        vars:           0,
        expr:           ({ #'({, 10, ({ #'return, -2 }), 20 }),
        args:           0,
        result:         -2,
    ),
    (<TestCase>
        name:           "#'switch (0 args)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({ #'switch }),
    ),
    (<TestCase>
        name:           "#'switch (1 arg)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({ #'switch, "A" }),
    ),
    (<TestCase>
        name:           "#'switch (2 args)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({ #'switch, "A", "B" }),
    ),
    (<TestCase>
        name:           "#'switch (3 args)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({ #'switch, "A", "B", "C" }),
    ),
    (<TestCase>
        name:           "#'switch (4 args)",
        flags:          0,
        vars:           0,
        expr:           ({ #'switch, "A", "A", "B", #'break }),
        args:           0,
        result:         "B",
    ),
    (<TestCase>
        name:           "#'switch (without break)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({ #'switch, "A", "A", "B", #', }),
    ),
    (<TestCase>
        name:           "#'switch (cases in array)",
        flags:          0,
        vars:           ({'a}),
        expr:           ({ #'switch, 'a, ({"A","B"}), "X", #'break, ({ "C", "D" }), "Y", #'break }),
        args:           ({"B"}),
        result:         "X",
    ),
    (<TestCase>
        name:           "#'switch (default value)",
        flags:          0,
        vars:           ({'a}),
        expr:           ({ #'switch, 'a, ({"A","B"}), "X", #'break, #'default, "Y", #'break }),
        args:           ({"C"}),
        result:         "Y",
    ),
    (<TestCase>
        name:           "#'switch (fallthrough)",
        flags:          0,
        vars:           ({'a}),
        expr:           ({ #'switch, 'a, ({"A","B"}), "X", #',, ({ "C", "D" }), "Y", #'break }),
        args:           ({"B"}),
        result:         "Y",
    ),
    (<TestCase>
        name:           "#'switch (.. with single integers)",
        flags:          0,
        vars:           ({'a}),
        expr:           ({ #'switch, 'a, ({1}), "X", #'break, ({2}), "Y", #'break }),
        args:           ({2}),
        result:         "Y",
    ),
    (<TestCase>
        name:           "#'switch (.. with integer ranges)",
        flags:          0,
        vars:           ({'a}),
        expr:           ({ #'switch, 'a, ({-50, #'[..], -30}), "X", #'break, ({-20,#'[..],0}), "Y", #'break }),
        args:           ({-42}),
        result:         "X",
    ),
    (<TestCase>
        name:           "#'switch (.. with strings)",
        flags:          TF_LAMBDA_ERROR,
        vars:           ({'a}),
        expr:           ({ #'switch, 'a, ({"A", #'[..], "C"}), "X", #'break, ({"D",#'[..],"H"}), "Y", #'break }),
    ),
    (<TestCase>
        name:           "#'this_object",
        flags:          0,
        vars:           0,
        expr:           ({#'this_object}),
        args:           0,
        result:         this_object(),
    ),
    (<TestCase>
        name:           "#'max (0 args)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'max}),
    ),
    (<TestCase>
        name:           "#'max (1 arg)",
        flags:          0,
        vars:           0,
        expr:           ({#'max, 10}),
        args:           0,
        result:         10,
    ),
    (<TestCase>
        name:           "#'max (2 args)",
        flags:          0,
        vars:           0,
        expr:           ({#'max, 10, 5}),
        args:           0,
        result:         10,
    ),
    (<TestCase>
        name:           "#'max (3 args)",
        flags:          0,
        vars:           0,
        expr:           ({#'max, 10, 5, 20}),
        args:           0,
        result:         20,
    ),
    (<TestCase>
        name:           "#'blueprint (0 args)",
        flags:          0,
        vars:           0,
        expr:           ({#'blueprint}),
        args:           0,
        result:         this_object(),
    ),
    (<TestCase>
        name:           "#'blueprint (1 arg)",
        flags:          0,
        vars:           0,
        expr:           ({#'blueprint, ({#'clone_object, ({#'this_object}) }) }),
        args:           0,
        result:         this_object(),
    ),
    (<TestCase>
        name:           "#'blueprint (2 args)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'blueprint, ({#'this_object}), ({#'this_object}) }),
    ),
    (<TestCase>
        name:           "#'& (0 args)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'&}),
    ),
    (<TestCase>
        name:           "#'& (1 arg)",
        flags:          0,
        vars:           0,
        expr:           ({#'=, #'global_var, ({#'&, #'global_var2}) }),
        args:           0,
        setup:          function void() { global_var = 0; global_var2 = 1; },
        test:           function int(mixed result) { global_var = 10; return result == 1 && global_var2 == 10; },
    ),
    (<TestCase>
        name:           "#'& (2 args)",
        flags:          0,
        vars:           0,
        expr:           ({#'&, 3, 5}),
        args:           0,
        result:         1,
    ),
    (<TestCase>
        name:           "#'& (3 args)",
        flags:          0,
        vars:           0,
        expr:           ({#'&, 11, 10, 18}),
        args:           0,
        result:         2,
    ),
    (<TestCase>
        name:           "#'| (0 args)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'|}),
    ),
    (<TestCase>
        name:           "#'| (1 arg)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'|, 1}),
    ),
    (<TestCase>
        name:           "#'| (2 args)",
        flags:          0,
        vars:           0,
        expr:           ({#'|, 3, 5}),
        args:           0,
        result:         7,
    ),
    (<TestCase>
        name:           "#'| (3 args)",
        flags:          0,
        vars:           0,
        expr:           ({#'|, 11, 10, 18}),
        args:           0,
        result:         27,
    ),
    (<TestCase>
        name:           "#'^ (0 args)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'^}),
    ),
    (<TestCase>
        name:           "#'^ (1 arg)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'^, 1}),
    ),
    (<TestCase>
        name:           "#'^ (2 args)",
        flags:          0,
        vars:           0,
        expr:           ({#'^, 3, 5}),
        args:           0,
        result:         6,
    ),
    (<TestCase>
        name:           "#'^ (3 args)",
        flags:          0,
        vars:           0,
        expr:           ({#'^, 11, 10, 18}),
        args:           0,
        result:         19,
    ),
    (<TestCase>
        name:           "#'! (0 args)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'!}),
    ),
    (<TestCase>
        name:           "#'! (1 arg)",
        flags:          0,
        vars:           0,
        expr:           ({#'!, 21}),
        args:           0,
        result:         0,
    ),
    (<TestCase>
        name:           "#'! (2 args)",
        flags:          TF_LAMBDA_ERROR,
        vars:           0,
        expr:           ({#'!, 2, 1}),
    ),
    (<TestCase>
        name:           "calling another lambda (0 args)",
        flags:          0,
        vars:           0,
        expr:           ({ lambda(0, ({#'this_object})) }),
        args:           0,
        result:         this_object(),
    ),
    (<TestCase>
        name:           "calling another lambda (3 arg)",
        flags:          0,
        vars:           0,
        expr:           ({ lambda(({'a,'b,'c}), ({#'^,'a,'b,'c})), 3, 5, 8 }),
        args:           0,
        result:         14,
    ),
    (<TestCase>
        name:           "calling another unbound lambda",
        flags:          0,
        vars:           0,
        expr:           ({ unbound_lambda(0, ({#'this_object})) }),
        args:           0,
        result:         this_object(),
    ),
    (<TestCase>
        name:           "calling another bound lambda",
        flags:          0,
        vars:           0,
        expr:           ({ bind_lambda(unbound_lambda(0, ({#'this_object}))) }),
        args:           0,
        result:         this_object(),
    ),
    (<TestCase>
        name:           "calling lfun (0 args)",
        flags:          0,
        vars:           0,
        expr:           ({ #'get_global_var }),
        args:           0,
        setup:          function void() { &global_var = 1234; },
        result:         1234,
    ),
    (<TestCase>
        name:           "calling lfun (1 arg)",
        flags:          0,
        vars:           0,
        expr:           ({ #'set_global_var, 2345 }),
        args:           0,
        test:           function int(mixed result) { return result == 0 && global_var == 2345; },
    ),
});


int run_test_with_filter(string name_postfix, closure expr_filter)
{
    int errors = 0;

    foreach (struct TestCase test: cases)
    {
        msg("Running Test %s%s...", test.name, name_postfix);

        test = funcall(expr_filter, test);
        if (test.flags & TF_LAMBDA_ERROR)
        {
            if (!catch(lambda(test.vars, test.expr);publish))
            {
                errors++;
                msg(" FAILURE! (There was no error.)\n");
            }
            else
                msg(" Success.\n");
        }
        else if (test.flags & TF_SURVIVE)
        {
            closure l;
            mixed result;

            catch(l=lambda(test.vars, test.expr);publish);
            catch(funcall(test.setup));
            catch(result = apply(l, test.args || ({})));
            catch(funcall(test.test, result, test.result));
            msg(" Success.\n");
        }
        else
        {
            closure l = lambda(test.vars, test.expr);
            mixed result;

            funcall(test.setup);
            result = apply(l, test.args || ({}));
            if (test.test ? funcall(test.test, result, test.result)
                          : deep_eq(result, test.result))
                msg(" Success.\n");
            else
            {
                errors++;
                msg(" FAILURE! (Wrong result.)\n");
            }
        }
    }

    return errors;
}

#define FILTER_FLAG_CATCH       0x01

mixed filter_rvalue(mixed expr, closure f, int flag)
{
    if (pointerp(expr) && sizeof(expr))
    {
        closure fun = expr[0];

        if (fun in ({#'=, #'+=, #'-=, #'*=, #'/=, #'%=, #'&=, #'|=, #'^=, #'<<=, #'>>=, #'>>>=, #'&&=, #'||=}))
        {
            mixed* result = ({fun});
            for (int i = 1; i < sizeof(expr); i++)
            {
                if (i&1)
                    result += expr[i..i];
                else
                    result += ({ filter_rvalue(expr[i], f, flag) });
            }
            return result;
        }
        else if (fun in ({#'++, #'--}) 
              || (fun == #'& && sizeof(expr) < 3))
        {
            return expr;
        }
        else if (fun == #'([)
        {
            return ({fun}) + map(expr[1..], #'map, #'filter_rvalue, f, flag);
        }
        else if (fun == #'foreach)
        {
            return expr[0..1] + map(expr[2..], #'filter_rvalue, f, flag);
        }
        else if (fun == #'switch)
        {
            mixed* result = ({});
            foreach (int i: sizeof(expr))
            {
                if (i == 0 || (i-2) % 3 != 1)
                    result += expr[i..i];
                else
                    result += ({ filter_rvalue(expr[i], f, flag) });
            }
            return result;
        }
        else if (fun == #'catch)
        {
            return ({fun}) + map(expr[1..], function mixed(mixed expr) { return symbolp(expr) ? expr : filter_rvalue(expr, f, flag | FILTER_FLAG_CATCH); });
        }
        else
        {
            return ({fun}) + map(expr[1..], #'filter_rvalue, f, flag);
        }
    }
    else
        return funcall(f, expr, flag);
}

closure make_rvalue_filter(closure f)
{
    return function struct TestCase (struct TestCase test)
    {
        struct TestCase t = copy(test);

        t.expr = filter_rvalue(test.expr, f, 0);

        return t;
    };
}

mixed filter_array(mixed expr, closure f)
{
    if (pointerp(expr))
    {
        mixed result = &(funcall(f, sizeof(expr)));
        foreach (int i: sizeof(expr))
            result[i] = &(filter_array(expr[i], f));
        return &result;
    }
    else
        return expr;
}

closure make_array_filter(closure f)
{
    return function struct TestCase (struct TestCase test)
    {
        struct TestCase t = copy(test);

        // The outer array cannot be an lvalue.
        t.expr = ({0}) * sizeof(test.expr);
        foreach (int i: sizeof(test.expr))
            t.expr[i] = &(filter_array(test.expr[i], f));

        return t;
    };
}

void filter_svalue(mixed expr, closure f)
{
    funcall(f, &expr);

    if (pointerp(expr))
    {
        foreach (mixed subexpr: &expr)
            filter_svalue(&subexpr, f);
    }
}

closure make_svalue_filter(closure f)
{
    return function struct TestCase (struct TestCase test)
    {
        struct TestCase t = copy(test);

        t.expr = deep_copy(test.expr);
        filter_svalue(&(t.expr), f);

        return t;
    };
}

void run_test()
{
    msg("\nRunning test for lambda():\n"
          "--------------------------\n");

    int errors = 0;

    // Execute the testsuite with different variations:
    //  - Original expression
    errors += run_test_with_filter("", function struct TestCase (struct TestCase test) { return test; });
    //  - Put every expression into a subexpression using #',
    errors += run_test_with_filter(" with #', (1)", make_rvalue_filter(function mixed(mixed expr)
    {
        return ({#',, expr});
    }));
    //  - Put every expression into a subexpression using #', and a throw-away argument.
    errors += run_test_with_filter(" with #', (2)", make_rvalue_filter(function mixed(mixed expr)
    {
        return ({#',, 10, expr});
    }));
    //  - Put every expression into a block with 256 throw-away arguments (testing long jumps)
    errors += run_test_with_filter(" with large blocks", make_rvalue_filter(function mixed(mixed expr, int flag)
    {
        /* #'catch doesn't support large blocks. */
        if (flag & FILTER_FLAG_CATCH)
            return expr;
        return ({#',,}) + 256 * ({255}) + ({expr});
    }));
    //  - Make every array an array range
    errors += run_test_with_filter(" with array ranges", make_array_filter(function mixed*(int size) : mapping storage = ([:1])
    {
        mixed* arr = ({0}) * (5+size);
        mixed result = &(arr[2..<4]);

        m_add(storage, arr, &result);
        return &result;
    }));
    //  - Make every array a mapping range
    errors += run_test_with_filter(" with mapping ranges", make_array_filter(function mixed*(int size) : mapping storage = ([:1])
    {
        mapping m = ([:size+5]);
        mixed result = &(m["*"+size,2..1+size]);

        m_add(storage, m, &result);
        return &result;
    }));
    //  - Make every string a string range
    errors += run_test_with_filter(" with string ranges", make_svalue_filter(function void(mixed expr) : mapping storage = ([:2])
    {
        if (stringp(expr))
        {
            string str = "**" + expr + "***";
            expr = &(str[2..<4]);
            m_add(storage, str, &str, &expr);
        }
    }));

    if (errors)
        shutdown(1);
    else
        start_gc(#'shutdown);
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
