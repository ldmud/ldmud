/* Define two functions that can be overloaded:
 * One with and one without saved types.
 *
 * We'll have symbol as an alternative type
 * to prevent warnings about insonsistent redefinitons.
 */

#pragma weak_types

untyped_fun(arg)
{
    return 0;
}

#pragma strong_types, save_types

closure|symbol typed_fun(string|symbol str)
{
    return #'typed_fun;
}
