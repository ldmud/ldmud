/* This sefun is to provide a replacement for terminal_colour() that
 * restores compatibility with the "%%^^" replacement pattern
 */

#if __EFUN_DEFINED__(terminal_colour)

#include <regexp.h>

varargs string terminal_colour(string str, mapping|closure trans,
        int wrap, int indent)
{
    return efun::terminal_colour(
            efun::regreplace(str, "%%\\^\\^", "%^%^", RE_GLOBAL),
            trans, wrap, indent);
}

#endif
