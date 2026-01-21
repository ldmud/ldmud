/* This sefun is to provide a replacement for the efun strlen().
 * Feel free to add it to your mudlibs, if you have much code relying on that.
 */

#if ! __EFUN_DEFINED__(strlen)

int strlen(string str)
{
    return sizeof(str);
}

#endif
