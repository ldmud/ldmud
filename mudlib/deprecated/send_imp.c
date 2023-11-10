/* This sefun is to provide a replacement for the efun send_imp().
 * Feel free to add it to your mudlibs, if you have much code relying on that.
 */

#if ! __EFUN_DEFINED__(send_imp)

int send_imp(string host, int port, bytes|int*|string message)
{
    if (efun::extern_call())
        efun::set_this_object(efun::previous_object());

    if (stringp(message))
        message = to_bytes(message, "ISO8859-1");

    return efun::send_udp(host, port, message);
}

#endif
