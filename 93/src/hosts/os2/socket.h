#ifndef SOCKET_H
#define SOCKET_H

#define EWOULDBLOCK  201
#define EMSGSIZE     202
#define ENETUNREACH  203
#define EHOSTUNREACH 204
/*#define EPIPE        205*/ /* defined by emx/gcc */
#define EADDRINUSE   206
#define ETIMEDOUT    207
#define ECONNRESET   208
/*#define EAGAIN       209*/ /* defined by emx/gcc */

#include <string.h>

#define ntohl(x) (x)

/* fds may not be above 31 and these fds
   are real, so non-socket fds exit */
#define MAX_NP_SOCKETS 16

#ifndef MAX_SOCKET_PACKET_SIZE
#define MAX_SOCKET_PACKET_SIZE 1024
#endif

#define IN_EMPTY 0
#define IN_BUFFERED 1
#define IN_SELECTED 2

#define REQUEST_PIPE "/pipe/lpmud_port#%d"
#define CLIENT_PIPE "/pipe/lpmud_client#%d"
#define TIME_OUT 1000L
#define ONE_INSTANCE 1

#if 0
struct general_socket {
    short index;
    short type;
};

struct bind {
    short index;
    short type;
    struct sock_buff *incoming;
    long port;
    struct bind *next;
};
#endif

struct sock_buff {
    short type;
    struct sock_buff *next;
    HPIPE fd;
    TID thread_id;
    HEV event_sem;
    long in_count;
    long out_count;
    char in [MAX_SOCKET_PACKET_SIZE+1];
    /* char out[MAX_SOCKET_PACKET_SIZE+1]; */
    unsigned char incoming;
};

typedef struct sock_buff *SOCKET_T;

#define SOCKET_T_ACCEPT    1
#define SOCKET_T_OPENING   2
#define SOCKET_T_CONNECTED 3
#define SOCKET_T_DISCO     4

extern int socket_write();
extern void socket_close();

#define INADDR_ANY 0

#define htons(n) (n)

#define socket_number(s) ((s)->fd)

struct sockaddr_in {
    struct {
	long s_addr;
    } sin_addr;
    int sin_family;
    long  sin_port;
};

struct in_addr {
    long s_addr;
};

struct sockaddr { struct sockaddr_in a; };

typedef unsigned long fd_set;

#define FD_ZERO(fd_set_p) (*(fd_set_p) = 0)
#define FD_SET(socket, fd_set_p) (*(fd_set_p) |= (1 << socket_number(socket)))
#define FD_CLR(socket, fd_set_p) (*(fd_set_p) &= ~(1 << socket_number(socket)))
#define FD_ISSET(socket, fd_set_p) (*(fd_set_p) & (1 << socket_number(socket)))

struct hostent {
    char h_addr[2];
    char h_length;
    char h_addrtype;
};

#define SOCK_STREAM 0
#define SOCK_DGRAM 1

#define SOL_SOCKET 0
#define SO_REUSEADDR 0

extern struct hostent *gethostbyname();

#define setsockopt(s,a,b,c,d) 0
#define listen(socket, queue_size) (0)
#define socket_ioctl(socket, code, p) (0)

SOCKET_T socket (int, int, int);
int bind (SOCKET_T, struct sockaddr *, int);
SOCKET_T accept (SOCKET_T, struct sockaddr *, int *);
int socket_read (SOCKET_T, char*, int);

char *inet_ntoa();

#endif /* SOCKET_H */
