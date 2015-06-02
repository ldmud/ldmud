/* This sefun is to provide a replacement for the efun set_buffer_size().
 * Feel free to add it to your mudlibs, if you have much code relying on that.
 */

#if ! __EFUN_DEFINED__(set_buffer_size)

#include <configuration.h>

int set_buffer_size(int size)
{
    object ob = efun::this_interactive();
    int oldsize = efun::interactive_info(ob, IC_SOCKET_BUFFER_SIZE);

    efun::configure_interactive(ob, IC_SOCKET_BUFFER_SIZE, size);

    return oldsize;
}

#endif


