/* This sefun is to provide a replacement for the efun file_name().
 * Feel free to add it to your mudlibs, if you have much code relying on that.
 */

#if ! __EFUN_DEFINED__(file_name)

string file_name(object ob = previous_object())
{
    return object_name(ob);
}

#endif
