/* This sefun is to provide a replacement for the efun map_array().
 * Feel free to add it to your mudlibs, if you have much code relying on that.
 */

#if ! __EFUN_DEFINED__(map_array)

mixed* map_array(mixed *arr, string|closure|mapping f, varargs mixed* args)
{
    if (efun::extern_call())
        efun::set_this_object(efun::previous_object());

    return map(arr, f, args...);
}

#endif
