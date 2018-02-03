/* This sefun is to provide a replacement for the efuns set_connection_charset()
 * and get_connection_charset().
 * Feel free to add it to your mudlibs, if you have much code relying on that.
 */

#if ! __EFUN_DEFINED__(set_connection_charset)

#include <configuration.h>
#include "charset.h"

void set_connection_charset(int *|string charset, varargs int* quote_iac)
{
    object ob = efun::this_interactive();
    if(!ob)
        return;

    if(stringp(charset))
        efun::configure_interactive(ob, IC_CONNECTION_CHARSET_AS_STRING, charset);
    else
        efun::configure_interactive(ob, IC_CONNECTION_CHARSET_AS_ARRAY, charset);

    efun::configure_interactive(ob, IC_QUOTE_IAC, sizeof(quote_iac) ? quote_iac[0] : 1);
}

int|int *|string get_connection_charset(int mode)
{
    object ob = efun::this_interactive();
    if(!ob)
        return 0;

    if(mode == CHARSET_STRING)
        return efun::interactive_info(ob, IC_CONNECTION_CHARSET_AS_STRING);
    else if(mode == CHARSET_VECTOR)
        return efun::interactive_info(ob, IC_CONNECTION_CHARSET_AS_ARRAY);
    else if(mode == CHARSET_QUOTE_IAC)
        return efun::interactive_info(ob, IC_QUOTE_IAC);
    else
        raise_error(sprintf("Bad arg 1 to get_connection_charset(): %d, "
              "expected CHARSET_VECTOR (%d), _STRING (%d), or _QUOTE_IAC (%d)\n"
             , mode, CHARSET_VECTOR, CHARSET_STRING, CHARSET_QUOTE_IAC));
}

#endif
