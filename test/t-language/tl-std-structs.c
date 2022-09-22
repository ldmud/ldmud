/* Check usage of standard structs. */
#pragma strong_types, save_types, rtt_checks

#include "/inc/deep_eq.inc"
#include "/inc/msg.inc"

struct Dummy
{
};

struct Derived (compile_string_options)
{
    string text;
};

int run_test()
{
    struct compile_string_options opts1 = (<compile_string_options> functions: #'run_test, as_async: 1);
    struct compile_string_options opts2 = (<compile_string_options> #'run_test);
    opts2.as_async = 1;

    if (!deep_eq(opts1, opts2))
        return 0;

    struct Derived opts3 = (<Derived> functions: #'run_test, as_async: 1, text: "Hi");
    struct compile_string_options opts4 = to_struct(opts3, (<compile_string_options>));
    if (!deep_eq(opts1, opts4))
        return 0;

    if (opts1.(0) != #'run_test)
        return 0;

    return 1;
}
