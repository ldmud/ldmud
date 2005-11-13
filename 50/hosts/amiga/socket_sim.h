/* hosts/amiga/socket_sim.h */

#ifndef SOCKET_SIM_H
#define SOCKET_SIM_H

#include "hosts/amiga/socket_sim_protos.h"

#define EFAULT       14
#define EMSGSIZE     40
#define ENETUNREACH  51
#define EHOSTUNREACH 65
#define EADDRINUSE   48
#define ETIMEDOUT    60
#define ECONNRESET   54
#define ENOBUFS      55

#define SOCK_STREAM 0
#define SOCK_DGRAM 1

#define SOL_SOCKET 0
#define SO_REUSEADDR 0

#define AF_UNIX         1               /* local to host (pipes, portals) */
#define AF_INET         2               /* internetwork: UDP, TCP, etc. */

typedef short SOCKET_T;

#define INADDR_ANY 0

#define htons(n) (n)

#define socket_number(s) (s)
#define htonl(x) (x)

struct in_addr {
  long s_addr;
};

  /* sizeof(sockaddr_in) == sizeof(sockaddr) ! */

struct sockaddr_in {
  short          sin_family;
  unsigned short sin_port;
  struct in_addr sin_addr;
  char           sin_zero[8];
};

struct sockaddr {
  unsigned short sa_family;    /* address family */
  char           sa_data[14];  /* up to 14 bytes of direct address */
};

struct  hostent {
  char    *h_name;        /* official name of host */
  char    **h_aliases;    /* alias list */
  int     h_addrtype;     /* host address type */
  int     h_length;       /* length of address */
  char    **h_addr_list;  /* list of addresses from name server */
#define h_addr  h_addr_list[0]  /* address, for backward compatiblity */
};

#define setsockopt(s,a,b,c,d) (0)
#define listen(socket,queue_size) (0)
#define socket_ioctl(socket,code,p) (0)
#define ntohl(x) (x)

#define recvfrom(s,b,c,f,a,l) (-1)
#define sendto(s,m,n,f,a,l) (-1)

#define socket_close(a) sim_close(a)
#define socket_write(a,b,c) sim_write(a,b,c)
#define socket_read(a,b,c) sim_read(a,b,c)
#define socket_select(s,t,u,v,w) sim_select(s,t,u,v,w)
#define shutdown(a,b) sim_shutdown(a,b)
#define socket(a,b,c) sim_socket(a,b,c)
#define accept(a,b,c) sim_accept(a,b,c)
#define bind(a,b,c) sim_bind(a,b,c)
#define gethostbyname(c) sim_gethostbyname(c)
#define gethostname(c,l) sim_gethostname(c,l)
#define getdomainname(c,l) sim_getdomainname(c,l)
#define getsockname(s,a,l) sim_getsockame(s,a,l)
#define inet_addr(c) sim_inet_addr(c)
#define inet_ntoa(a) sim_inet_ntoa(a)

#endif /* SOCKET_SIM_H */
