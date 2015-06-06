/* This sefun is to provide a replacement for the efun query_editing().
 * Feel free to add it to your mudlibs, if you have much code relying on that.
 */

#if ! __EFUN_DEFINED__(query_editing)

#include <interactive_info.h>

int|object query_editing(object ob)
{
    if(!efun::interactive(ob))
        return 0;

    return efun::interactive_info(ob, II_EDITING);
}

#endif
