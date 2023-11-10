/* This sefun is to provide a replacement for the efun filter_array().
 * Feel free to add it to your mudlibs, if you have much code relying on that.
 */

#if ! __EFUN_DEFINED__(filter_array)

mixed* filter_array(mixed *arr, string|closure|mapping f, varargs mixed* args)
{
    if (efun::extern_call())
        efun::set_this_object(efun::previous_object());

    return filter(arr, f, args...);
}

#endif
