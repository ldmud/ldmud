/* This sefun is to provide a replacement for the efun allocate_mapping().
 * Feel free to add it to your mudlibs, if you have much code relying on that.
 */
#if ! __EFUN_DEFINED__(allocate_mapping)

mapping allocate_mapping(int size, int width = 1)
{
    return m_allocate(size, width);
}

#endif
