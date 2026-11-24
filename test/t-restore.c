#include "/inc/base.inc"
#include "/inc/deep_eq.inc"

/* Tests for restore_object()/restore_value() mapping and shared-value
 * handling: nested mappings of various widths, the empty-mapping forms,
 * duplicate keys, shared values (also those defined on lines of unknown
 * variables) and lines for variables that don't exist anymore.
 */

mapping m;
mixed a;
mixed b;
int x;

nosave int errors;

void reset_vars()
{
    m = 0;
    a = 0;
    b = 0;
    x = 0;
}

void check(string name, int ok)
{
    if (ok)
        msg("Success: %s\n", name);
    else
    {
        msg("FAILURE: %s\n", name);
        errors++;
    }
}

void run_test()
{
    string s;

    msg("\nRunning restore tests:\n"
          "----------------------\n");

    /* Fail-safe: if an uncaught error aborts run_test(), still
     * shut down (as failure) instead of hanging.
     */
    call_out(#'shutdown, 0, 1);

    /* --- Round trip through save_object()/restore_object() --- */

    m = ([ "outer": ([ "inner": ({ 1, 2, ({ 3, "four" }) }),
                       "wide": 42 ]),
           "str": "hello \"quoted\" and \\ backslash\n",
           "num": -17,
           "flt": 3.25 ]);
    a = ([ "k1": 1; "one", "k2": 2; "two" ]);   /* width 2 */
    b = ([ "solo1", "solo2" ]);                 /* width 0 */
    x = 4711;

    s = save_object();
    reset_vars();
    check("save_object() returned a string", stringp(s) && sizeof(s) > 0);

    check("restore_object(string) round trip", restore_object(s) == 1);
    check("round trip: nested mapping",
          deep_eq(m, ([ "outer": ([ "inner": ({ 1, 2, ({ 3, "four" }) }),
                                    "wide": 42 ]),
                        "str": "hello \"quoted\" and \\ backslash\n",
                        "num": -17,
                        "flt": 3.25 ])));
    check("round trip: width 2 mapping",
          deep_eq(a, ([ "k1": 1; "one", "k2": 2; "two" ])));
    check("round trip: width 0 mapping",
          deep_eq(b, ([ "solo1", "solo2" ])));
    check("round trip: int variable", x == 4711);

    /* --- Explicit restore strings: empty mapping forms --- */

    reset_vars();
    check("restore: empty mapping",
          restore_object("#1:0\nm ([])\nx 42\n") == 1
          && deep_eq(m, ([])) && x == 42);

    reset_vars();
    check("restore: empty mapping with explicit width",
          restore_object("#1:0\nm ([:5])\n") == 1
          && mappingp(m) && sizeof(m) == 0 && widthof(m) == 5);

    /* --- Explicit restore strings: nesting and content --- */

    reset_vars();
    check("restore: deeply nested mapping",
          restore_object("#1:0\nm ([\"l1\":([\"l2\":([\"l3\":({1,2,3,}),]),]),])\n") == 1
          && deep_eq(m, ([ "l1": ([ "l2": ([ "l3": ({1,2,3}) ]) ]) ])));

    reset_vars();
    check("restore: duplicate key keeps last value",
          restore_object("#1:0\nm ([\"a\":1,\"a\":2,])\n") == 1
          && deep_eq(m, ([ "a": 2 ])));

    reset_vars();
    check("restore: escaped characters in keys",
          restore_object("#1:0\nm ([\"a\\\"b\":1,\"c\\\\d\":2,])\n") == 1
          && deep_eq(m, ([ "a\"b": 1, "c\\d": 2 ])));

    reset_vars();
    check("restore: array key",
          restore_object("#1:0\nm ([({1,2,}):3,])\n") == 1
          && sizeof(m) == 1);

    /* --- Shared values --- */

    reset_vars();
    check("restore: shared value across variables",
          restore_object("#1:0\na <1>=({1,2,})\nb <1>\n") == 1
          && deep_eq(a, ({1,2})) && a == b);

    reset_vars();
    check("restore: shared value within one array",
          restore_object("#1:0\na ({<1>=({5,}),<1>,})\n") == 1
          && a[0] == a[1] && deep_eq(a[0], ({5})));

    reset_vars();
    check("restore: self-referencing shared mapping",
          restore_object("#1:0\nm <1>=([\"self\":<1>,])\n") == 1
          && mappingp(m) && m["self"] == m);

    /* --- Lines of unknown variables --- */

    reset_vars();
    check("restore: unknown variable is ignored",
          restore_object("#1:0\nzzz_gone ([\"big\":({1,2,3,}),\"n\":17,])\nx 7\n") == 1
          && x == 7 && !m);

    reset_vars();
    check("restore: shared value defined on unknown variable line",
          restore_object("#1:0\nzzz_gone <1>=({9,8,})\na <1>\n") == 1
          && deep_eq(a, ({9,8})));

    reset_vars();
    check("restore: unknown variable with '<' inside a string",
          restore_object("#1:0\nzzz_gone \"a<b>c\"\nx 5\n") == 1
          && x == 5);

    reset_vars();
    check("restore: shared numbering after unknown variable line",
          restore_object("#1:0\nzzz_gone ([\"p\":1,])\na <1>=({4,})\nb <1>\n") == 1
          && a == b && deep_eq(a, ({4})));

    reset_vars();
    check("restore: malformed value on unknown variable line is skipped",
          restore_object("#1:0\nzzz_gone ([\"broken\":1\nx 3\n") == 1
          && x == 3);

    /* --- restore_value() uses the same mapping parser --- */

    check("restore_value: nested mapping round trip",
          deep_eq(restore_value(save_value(([ "a": ([ "b": ({ 1, ([ "c": 2 ]) }) ]) ]))),
                  ([ "a": ([ "b": ({ 1, ([ "c": 2 ]) }) ]) ])));

    check("restore_value: wide mapping round trip",
          deep_eq(restore_value(save_value(([ "k": 1; 2; 3 ]))),
                  ([ "k": 1; 2; 3 ])));

    check("restore_value: empty mapping round trip",
          deep_eq(restore_value(save_value(([]))), ([])));

    /* --- Errors on malformed mappings must still be errors --- */

    reset_vars();
    check("restore: malformed mapping on known variable throws",
          catch(restore_object("#1:0\nm ([\"a\":1\n")) != 0);

    reset_vars();
    check("restore: unterminated mapping on known variable throws",
          catch(restore_object("#1:0\nm ([\"a\":1,")) != 0);

    remove_call_out(#'shutdown);

    if (errors)
        shutdown(1);
    else
        shutdown(0);
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
