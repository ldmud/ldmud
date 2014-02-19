/* Overload functions from an inherit and check
 * whether there are any type conflicts.
 *
 * We'll have symbol as an alternative type
 * to prevent warnings about insonsistent redefinitons.
 */

#pragma strong_types, save_types
inherit "ti-types";

int|symbol typed_fun(string*|symbol str)
{
    /* Check, whether we can already call
     * with the new types.
     */
    if(0)
        typed_fun(({"C"}));
    else
        ::typed_fun("C");
    return 1;
}

int|symbol untyped_fun(string*|symbol str)
{
    if(0)
        untyped_fun(({"C"}));
    else
        ::untyped_fun("C");

    return 2;
}

int run_test()
{
    1+typed_fun(({"A"}));
    2+untyped_fun(({"A"}));
    ::typed_fun("B");
    ::untyped_fun("B");

    return 1;
}