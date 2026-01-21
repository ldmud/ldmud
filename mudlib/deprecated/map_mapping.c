/* This sefun is to provide a replacement for the efun map_mapping().
 * Feel free to add it to your mudlibs, if you have much code relying on that.
 */

#if ! __EFUN_DEFINED__(map_mapping)

mapping map_mapping(mapping m, string|closure|mapping f, varargs mixed* args)
{
    if (efun::extern_call())
        efun::set_this_object(efun::previous_object());

    return map_indices(m, f, args...);
}

#endif
