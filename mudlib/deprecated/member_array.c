/* This sefun is to provide a replacement for the efun member_array().
 * Feel free to add it to your mudlibs, if you have much code relying on that.
 */

#if ! __EFUN_DEFINED__(member_array)

int member_array(mixed item, string|mixed* arr)
{
    return member(arr, item);
}

#endif
