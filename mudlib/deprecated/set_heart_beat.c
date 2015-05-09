/* This sefun is to provide a replacement for the efun set_heart_beat().
 * Feel free to add it to your mudlibs, if you have much code relying on that.
 */

#if ! __EFUN_DEFINED__(set_heart_beat)

#include <configuration.h>

int set_heart_beat(int flag)
{
    object ob = efun::previous_object();
    int hb = efun::object_info(ob, OC_HEART_BEAT);

    if (!flag == !hb)
        return 0;

    efun::configure_object(ob, OC_HEART_BEAT, flag);

    return 1;
}

#endif


