#include "defs.h"

void erq_rlookup(char *mesg, int msglen)
{
    struct hostent *hp;
    int len;
#ifdef FIX_ALIGNMENT
    char addr[4];
    memcpy(addr, mesg+9, 4);
#else
    register char *addr=mesg+9;
#endif

    if (msglen != 13) {
	const char status=ERQ_E_ARGLENGTH;
	reply1(get_handle(mesg), &status, 1);
        return;
    }
    hp=gethostbyaddr(addr, 4, AF_INET);
    if (!hp && mesg[8]==ERQ_RLOOKUP) {
	mesg[8]++;
	add_retry(erq_rlookup, mesg, 13, 5);
	return;
    }
    if (hp) len=strlen(hp->h_name)+1;
    else len=0;

    if (hp)
	replyn(get_handle(mesg), 0, 2,
	    mesg+9, 4,
	    hp->h_name, len);
    else
	reply1(get_handle(mesg), mesg+9, 4);
}

void erq_lookup(char *mesg, int len)
{
    struct hostent *hp;

    if (mesg[len-1]!=0) {
	mesg[len]=0;
	len++;
    }
    hp=gethostbyname(mesg+9);
    if (!hp && mesg[8]==ERQ_LOOKUP) {
	mesg[8]++;
	add_retry(erq_lookup, mesg, len, 5);
	return;
    }
    replyn(get_handle(mesg), 0, 3,
	hp ? (char[]) { ERQ_OK } : (char[]) { ERQ_E_NOTFOUND }, 1,
	hp ? (char *) hp->h_addr : (char[]) { 0,0,0,0 }, 4,
	mesg+9, len-9);
}
