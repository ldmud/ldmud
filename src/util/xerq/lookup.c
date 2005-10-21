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

/* ERQ_RLOOKUP: look up an address by a hostname.
 */

{
    struct hostent *hp;
    int len;
    char addr[4];

    memcpy(addr, mesg+9, 4);

    if (msglen != 13)
    {
        const char status=ERQ_E_ARGLENGTH;
        reply1(get_handle(mesg), &status, 1);
        return;
    }

    hp = gethostbyaddr(addr, 4, AF_INET);
    if (!hp && mesg[8] == ERQ_RLOOKUP)
    {
        mesg[8]++; /* No second retry */
        add_retry(erq_rlookup, mesg, 13, 5);
        return;
    }

    if (hp)
        len = strlen(hp->h_name)+1;
    else
        len = 0;

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

/* ERQ_LOOKUP: look up a hostname by an address.
 */

{
    struct hostent *hp;

    if (mesg[len-1] != 0)
    {
        mesg[len] = 0;
        len++;
    }

    hp = gethostbyname(mesg+9);
    if (!hp && mesg[8] == ERQ_LOOKUP)
    {
        mesg[8]++; /* No second retry */
        add_retry(erq_lookup, mesg, len, 5);
        return;
    }

    {
        char r_ok[] = { ERQ_OK };
        char r_notfound[] = { ERQ_E_NOTFOUND };
        char r_noaddr[] = { 0, 0, 0, 0 };

        replyn(get_handle(mesg), 0, 3,
            hp ? r_ok : r_notfound, 1,
            hp ? (char *) hp->h_addr : r_noaddr, 4,
            mesg+9, len-9);
    }
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
    req.ai_flags = AI_CANONNAME;
    i = getaddrinfo(buf, NULL, &req, &ai);
    if (!i)
        for (ai2 = ai
            ; ai2 && (ai2->ai_family != AF_INET)
                  && (ai2->ai_family != AF_INET6)
            ; ai2 = ai2->ai_next) NOOP;

    if (!i && ai2 && ai2->ai_canonname)
    {
        mbuff = malloc(strlen(ai2->ai_canonname)+1+buflen);
        if (!mbuff)
        {
            free(buf);
            reply1(get_handle(mesg), msg_nomem, strlen(msg_nomem)+1);
            return;
        }
        strcpy(mbuff, buf);
        strcat(mbuff, " ");
        strcat(mbuff, ai2->ai_canonname);
        reply1(get_handle(mesg), mbuff, strlen(mbuff)+1);
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

