/* This sefun is to provide a replacement for the efun query_input_pending().
 * Feel free to add it to your mudlibs, if you have much code relying on that.
 */

#if ! __EFUN_DEFINED__(query_input_pending)

#include <interactive_info.h>

object query_input_pending(object ob)
{
    if(!efun::interactive(ob))
        return 0;

    return efun::interactive_info(ob, II_INPUT_PENDING);
}

#endif
