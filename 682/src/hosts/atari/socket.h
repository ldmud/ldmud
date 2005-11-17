#ifndef SOCKET_H
#define SOCKET_H

#define EWOULDBLOCK  1
#define EMSGSIZE     2
#define ENETUNREACH  3
#define EHOSTUNREACH 4
#define EPIPE        5
#define EADDRINUSE   6
#define ETIMEDOUT    7
#define ECONNRESET   8
#define EAGAIN       9

#include <string.h>

#define BIND ('b'<<12) + ('i'<<8) + ('n'<<4) + 'd'
#ifndef MAX_SOCKET_PACKET_SIZE
#define MAX_SOCKET_PACKET_SIZE 1024
#endif

struct cookie { long c,v; };

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


struct sock_buff {
    short index;
    short type;
    long in_count;
    long out_count;
    long pid;
    char in [MAX_SOCKET_PACKET_SIZE+1];
    char out[MAX_SOCKET_PACKET_SIZE+1];
};

typedef struct sock_buff *SOCKET_T;

#define SOCKET_T_ACCEPT    0
#define SOCKET_T_CONNECTED 1

extern int socket_write();
extern void socket_close();

#define INADDR_ANY 0

#define htons(n) (n)

#define socket_number(s) ((s)->index)

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

#define FD_SET(socket, fd_set_p)   (*(fd_set_p) |= 1 << (socket)->index)
#define FD_CLR(socket, fd_set_p)   (*(fd_set_p) &= ~(1 << (socket)->index))
#define FD_ISSET(socket, fd_set_p) (*(fd_set_p) & (1 << (socket)->index))

struct hostent {
    char h_addr[2];
    char h_length;
    char h_addrtype;
};

#define SOCK_STREAM 0

#define SOL_SOCKET 0
#define SO_REUSEADDR 0

extern struct hostent *gethostbyname();

#define setsockopt(s,a,b,c,d) 0
#define listen(socket, queue_size) (0)
#define socket_ioctl(socket, code, p) (0)

SOCKET_T socket PROT((int, int, int));
int bind PROT((SOCKET_T, struct sockaddr *, int));
SOCKET_T accept PROT((SOCKET_T, struct sockaddr *, int *));
int socket_read PROT((SOCKET_T, char*, int));

char *inet_ntoa();

#endif /* SOCKET_H */
