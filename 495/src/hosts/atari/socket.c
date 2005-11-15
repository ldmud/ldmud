#include <mintbind.h>
#include <osbind.h>
#include "socket.h"

static SOCKET_T all_sockets[32];

int num_sockets = 0;

int socket_write(s, buffer, length)
SOCKET_T s;
char *buffer;
int length;
{
    if (s->out_count) {
	  extern void usleep PROT((unsigned long));

	  usleep(200000);
      if (s->out_count)  usleep(200000);
	  if (s->out_count) {
		errno = EWOULDBLOCK;
		return -1;
	  }
    }
    memcpy(s->out,buffer,length);
    s->out_count = length;
    (void)Pkill(s->pid, SIGUSR1);
    return length;
}

int socket_select(num, readfds, writefds, xfds ,timeout)
int num;
fd_set * readfds, writefds, xfds;
struct timeval *timeout;
{
    long time_left;

    time_left = timeout->tv_sec * 1000000 + timeout->tv_usec;
    for (;;) {
	extern void usleep();
	int count;
	unsigned long rfds, mask, rrfds;
	SOCKET_T *p;

	rfds = *readfds;
	rrfds = 0;
	count = 0;
	mask = 1;
	p = all_sockets;
	do {
	    if (mask & rfds) {
		if ( ((*p)->type == SOCKET_T_CONNECTED && (*p)->in_count) ||
		     ((*p)->type == SOCKET_T_ACCEPT && (*p)->incoming)
		{
		    rrfds |= mask;
		    count++;
		}
	    }
	    p++;
	} while (mask <<= 1);
	if (count) {
	    *readfds = rrfds;
	    return count;
	}
	if (time_left <= 0) return 0;
	usleep(40000);
	time_left -= 40000;
    }
}

void shutdown(s,n)
struct sock_buff *s;
int n;
{}

void socket_close(s)
SOCKET_T s;
{
    if (s->type == SOCKET_T_ACCEPT) {
        struct cookie *c;
        struct bind *b, **bp;

        c = (struct cookie *)Setexc(0x168,0);
        while (c->c != BIND) c++;
        b = (struct bind *)s;
        bp = (struct bind **)&c->v;
        while (*bp != b) bp = &(*bp)->next;
        *bp = b->next;
        pfree(b);
    }
}

struct hostent *gethostbyname(host_name)
char *host_name;
{
    static struct hostent current_hostend = { { 0, 0 }, 2, 0 };

    return &current_hostend;
}

SOCKET_T socket(addr_type, mode, i)
int addr_type;
int mode;
int i;
{
    SOCKET_T res;

    i = num_sockets++;
    if (i > 32) return (SOCKET_T)-1;
    res = (SOCKET_T)permanent_xalloc(sizeof(struct bind));
    all_sockets[i] = res;
    res->type = SOCKET_T_ACCEPT;
    res->index = i;
    return res;
}

int bind(sock, addr, addr_size)
SOCKET_T sock;
struct sockaddr *addr;
int addr_size;
{
    struct cookie *cookie_ptr,*scan_ptr;
    struct bind *s = (struct bind *)sock;

    cookie_ptr = (struct cookie *)Setexc(0x168,0);
    scan_ptr = cookie_ptr - 1;
    do {
        if ( (++scan_ptr)->c == BIND )
            break;
    } while ( scan_ptr->c );
    if (!scan_ptr->c) {
        if (scan_ptr->v <= (scan_ptr-cookie_ptr)+1 ) {
            struct cookie *new_ptr;
            int old_size = scan_ptr->v;
            int size = old_size << 1;
            new_ptr = (struct cookie *)
              permanent_xalloc( size * sizeof( struct cookie[1] ) );
            memcpy(new_ptr,cookie_ptr,old_size * sizeof(struct cookie[1]));
            scan_ptr += new_ptr - cookie_ptr;
            scan_ptr->v = size;
            (void)Setexc(0x168,cookie_ptr=new_ptr);
        }
        scan_ptr[1] = scan_ptr[0];
        scan_ptr->c = BIND;
        scan_ptr->v = 0;
    }
    s->port = addr->a.sin_port;
    s->incoming = 0;
    s->next = (struct bind *)scan_ptr->v;
    				  /* this is a bit sloppy, using the cas2
    				     instruction of the microprocessor
    				     would prevent inconsistent changes,    */
    scan_ptr->v = (long)s;	  /* but who starts two muds simultanously? */
    return 0;
}

SOCKET_T accept(sock, addr, addr_len)
SOCKET_T sock;
struct sockaddr *addr;
int *addr_len;
{
    struct bind *s = (struct bind *)sock;
    SOCKET_T res;

    res = s->incoming;
    if (!res) {
	errno = EWOULDBLOCK;
	return (SOCKET_T)-1;
    }
    s->incoming = 0;
    if (*addr_len > sizeof(long)) *addr_len = sizeof(long);
    bzero(addr, *addr_len);
    res->type  = SOCKET_T_CONNECTED;
    res->index = num_sockets;
    all_sockets[num_sockets++] = res;
    return res;
}

int socket_read(s, buff, limit)
SOCKET_T s;
char *buff;
int limit;
{
    if (s->in_count < limit) limit = s->in_count;
    if (limit) {
	memcpy(buff, s->in, limit);
	s->in_count = 0;
    }
    return limit;
}

int getpeername(s, addr, addr_len)
SOCKET_T s;
struct sockaddr *addr;
int *addr_len;
{
    if (*addr_len > sizeof(long)) *addr_len = sizeof(long);
    bzero(addr, *addr_len);
    return 0;
}
