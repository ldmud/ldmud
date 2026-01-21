/* This sefun is to provide a replacement for the efun filter_mapping().
 * Feel free to add it to your mudlibs, if you have much code relying on that.
 */

#if ! __EFUN_DEFINED__(filter_mapping)

mapping filter_mapping(mapping m, string|closure|mapping f, varargs mixed* args)
{
    if (efun::extern_call())
        efun::set_this_object(efun::previous_object());

    return filter_indices(m, f, args...);
}

#endif
