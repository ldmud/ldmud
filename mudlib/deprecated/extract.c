/* This sefun is to provide a replacement for the efun extract().
 * Feel free to add it to your mudlibs, if you have much code relying on that.
 */

#if ! __EFUN_DEFINED__(extract)

string extract(string str, int from, int to = -1)
{
    return str[>from..>to];
}

#endif
