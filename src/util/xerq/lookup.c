/*---------------------------------------------------------------------------
 * XErq - Address Lookup
 * (C) Copyright 1995 by Brian Gerst.
 *---------------------------------------------------------------------------
 * Here are the function to lookup up internet addresses and names.
 * If the first lookup attempt doesn't succeed, a retry is attempted
 * five seconds later.
 *
 * TODO: The functions block the ERQ.
 *---------------------------------------------------------------------------
 */

#include "defs.h"

/*-------------------------------------------------------------------------*/
void
erq_rlookup(char *mesg, int msglen)

/* ERQ_RLOOKUP: look up a hostname by an address.
 */

{
    struct hostent *hp;
    int len;
    char addr[4];

    if (msglen != 13)
    {
        const char status=ERQ_E_ARGLENGTH;
        reply1(get_handle(mesg), &status, 1);
        return;
    }

    memcpy(addr, mesg+9, 4);

    XPRINTF((stderr, "%s rlookup %02x.%02x.%02x.%02x\n"
                   , time_stamp(), addr[0], addr[1], addr[2], addr[3]));

    hp = gethostbyaddr(addr, 4, AF_INET);
    if (!hp && mesg[8] == ERQ_RLOOKUP)
    {
        mesg[8]++; /* No second retry */
        XPRINTF((stderr, "%s   Retry in 5 seconds.\n", time_stamp()));
        add_retry(erq_rlookup, mesg, 13, 5);
        return;
    }

    if (hp)
    {
        XPRINTF((stderr, "%s   rlookup found '%s'\n", time_stamp(), hp->h_name));
        len = strlen(hp->h_name)+1;
    }
    else
    {
        XPRINTF((stderr, "%s   rlookup failed.\n", time_stamp()));
        len = 0;
    }

    if (hp)
        replyn(get_handle(mesg), 0, 2,
            mesg+9, 4,
            hp->h_name, len);
    else
        reply1(get_handle(mesg), mesg+9, 4);
} /* erq_rlookup() */

/*-------------------------------------------------------------------------*/
void
erq_lookup(char *mesg, int len)

/* ERQ_LOOKUP: look up an address by a hostname.
 */

{
    struct hostent *hp;
    char * msg;

    msg = mesg;
    if (mesg[len-1] != 0)
    {
        /* Create a local copy of the message in which the string
         * has a proper termination.
         */
        msg = malloc(len+1);
        if (!msg)
        {
            XPRINTF((stderr, "%s Out of memory.\n", time_stamp()));
            die();
        }
        memcpy(msg, mesg, len);
        msg[len] = 0;
        len++;
    }

    XPRINTF((stderr, "%s lookup '%s'\n", time_stamp(), msg+9));
    hp = gethostbyname(msg+9);
    if (!hp && msg[8] == ERQ_LOOKUP && h_errno == TRY_AGAIN)
    {
        XPRINTF((stderr, "%s   Retry in 5 seconds.\n", time_stamp()));
        mesg[8]++; /* No second retry */
        add_retry(erq_lookup, msg, len, 5);
        if (msg != mesg)
            free(msg);
        return;
    }

    {
        char r_ok[] = { ERQ_OK };
        char r_notfound[] = { ERQ_E_NOTFOUND };
        char r_noaddr[] = { 0, 0, 0, 0 };

        if (hp)
            XPRINTF((stderr, "%s   lookup found %02x.%02x.%02x.%02x\n"
                           , time_stamp(), hp->h_addr[0], hp->h_addr[1]
                           , hp->h_addr[2], hp->h_addr[3]));
        else
            XPRINTF((stderr, "%s   lookup failed.\n", time_stamp()));
        replyn(get_handle(mesg), 0, 3,
            hp ? r_ok : r_notfound, 1,
            hp ? (char *) hp->h_addr : r_noaddr, 4,
            msg+9, len-9);
    }

    if (msg != mesg)
        free(msg);
} /* erq_lookup() */

/*-------------------------------------------------------------------------*/
#ifdef USE_IPV6

void
erq_rlookupv6(char *mesg, int msglen)

/* ERQ_RLOOKUPV6: look up an IPv6 address by a hostname.
 */

{
    int i;
    char *mbuff, *buf;
    size_t buflen;
    struct addrinfo req, *ai, *ai2;
    static char *msg_invalid = "invalid-format";
    static char *msg_nomem = "out-of-memory";

    buflen = msglen -9 +1;
    buf = malloc(msglen -9 +1);
    if (!buf)
    {
        reply1(get_handle(mesg), msg_nomem, strlen(msg_nomem)+1);
        return;
    }

    memcpy(buf, mesg+9, msglen-9);
    buf[buflen-1] = '\0';

    memset(&req, 0, sizeof(struct addrinfo));
    req.ai_family = AF_INET6;
    i = getaddrinfo(buf, NULL, &req, &ai);
    if (!i)
        for (ai2 = ai
            ; ai2 && (ai2->ai_family != AF_INET)
                  && (ai2->ai_family != AF_INET6)
            ; ai2 = ai2->ai_next) NOOP;

    if (!i && ai2)
    {
        int res;
        char *cbuff;

        mbuff = malloc(buflen + 256);
        if (!mbuff)
        {
            free(buf);
            freeaddrinfo(ai);
            reply1(get_handle(mesg), msg_nomem, strlen(msg_nomem)+1);
            return;
        }
        memcpy(mbuff, buf, buflen-1);
        mbuff[buflen-1] = ' ';

        res = getnameinfo(ai2->ai_addr, ai2->ai_addrlen, mbuff + buflen, 256, NULL, 0, NI_NAMEREQD);
        if(!res)
            reply1(get_handle(mesg), mbuff, strlen(mbuff)+1);
        else
            reply1(get_handle(mesg), msg_invalid, strlen(msg_invalid)+1);

        free(mbuff);
    }
    else
        reply1(get_handle(mesg), msg_invalid, strlen(msg_invalid)+1);

    if (!i)
        freeaddrinfo(ai);

    free(buf);
} /* erq_rlookupv6() */

#endif /* USE_IPV6 */

/***************************************************************************/

