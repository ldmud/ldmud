/* This sefun is to provide a replacement for the efuns set_combine_charset()
 * and get_combine_charset().
 * Feel free to add it to your mudlibs, if you have much code relying on that.
 */

#if ! __EFUN_DEFINED__(set_combine_charset)

#include <configuration.h>
#include "charset.h"

void set_combine_charset(int *|string charset)
{
    object ob = efun::this_interactive();
    if(!efun::interactive(ob))
        return;

    if(stringp(charset))
        efun::configure_interactive(ob, IC_COMBINE_CHARSET_AS_STRING, charset);
    else
        efun::configure_interactive(ob, IC_COMBINE_CHARSET_AS_ARRAY, charset);
}

int *|string get_combine_charset(int mode)
{
    object ob = efun::this_interactive();
    if(!efun::interactive(ob))
        return 0;

    if(mode == CHARSET_STRING)
        return efun::interactive_info(ob, IC_COMBINE_CHARSET_AS_STRING);
    else if(mode == CHARSET_VECTOR)
        return efun::interactive_info(ob, IC_COMBINE_CHARSET_AS_ARRAY);
    else
        raise_error(sprintf("Bad arg 1 to get_combine_charset(): %d, "
              "expected CHARSET_VECTOR (%d) or CHARSET_STRING (%d)\n"
             , mode, CHARSET_VECTOR, CHARSET_STRING));
}

#endif
