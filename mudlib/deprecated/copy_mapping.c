/* This sefun is to provide a replacement for the efun copy_mapping().
 * Feel free to add it to your mudlibs, if you have much code relying on that.
 */

#if ! __EFUN_DEFINED__(copy_mapping)

mapping copy_mapping(mapping m)
{
    return copy(m);
}

#endif
