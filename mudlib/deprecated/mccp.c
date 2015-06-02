/* These sefuns are to provide a replacement for the efun start_mccp_compress(),
 * end_mccp_compress(), query_mccp() and query_mccp_stats().
 * Feel free to add it to your mudlibs, if you have much code relying on that.
 */

#ifdef __MCCP__

#if ! __EFUN_DEFINED__(start_mccp_compress)

#include <configuration.h>
#include <interactive_info.h>

int start_mccp_compress(int telopt)
{
    object ob = efun::previous_object();

    efun::configure_interactive(ob, IC_MCCP, telopt);
    return efun::interactive_info(ob, IC_MCCP) != 0;
}

int end_mccp_compress()
{
    object ob = efun::previous_object();

    efun::configure_interactive(ob, IC_MCCP, 0);
    return efun::interactive_info(ob, IC_MCCP) == 0;
}

varargs int query_mccp(object player)
{
    player ||= efun::this_player();

    return efun::interactive_info(player, IC_MCCP);
}

varargs int * query_mccp_stats(object player)
{
    player ||= efun::this_player();

    return efun::interactive_info(player, II_MCCP_STATS);
}

#endif /* __EFUN_DEFINED__(start_mccp_compress) */

#endif /* __MCCP__ */
