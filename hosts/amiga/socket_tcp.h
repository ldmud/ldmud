#ifndef SOCKET_TCP_H
#define SOCKET_TCP_H

#include <exec/types.h>
#include <sys/types.h>
#include <sys/errno.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

#include "hosts/amiga/socket_sim_protos.h"

#define SOCKET_T int
#define socket_number(s) (s)

#ifdef __SASC
#define _CAST(type,val) type val
#else
#define _CAST(type,val) val
#endif

#ifdef AMITCP
#include <clib/socket_protos.h>

#ifdef __SASC
#include <pragma/socket_pragmas.h>
#endif

extern struct Library *SocketBase;

  /* At least DICE doesn't do recursive macro expansion... */
#define socket_close(s)       if (SocketBase) close(s); else sim_close(s)
#define socket_write(s,b,l)   (SocketBase ? write(s,b,l)          : sim_write(s,b,l)    )
#define socket_read(s,b,l)    (SocketBase ? read(s,b,l)           : sim_read(s,b,l)     )
#define shutdown(a,b)         (SocketBase ? shutdown(a,b)         : sim_shutdown(a,b)   )
#define socket(a,b,c)         (SocketBase ? socket(a,b,c)         : sim_socket(a,b,c)   )
#define accept(a,b,c)         (SocketBase ? accept(a,b,_CAST((LONG *),(c))) : sim_accept(a,b,_CAST((LONG *),(c))) )
#define bind(a,b,c)           (SocketBase ? bind(a,b,c)           : sim_bind(a,b,c)     )
#define gethostbyname(c)      (SocketBase ? gethostbyname(c)      : sim_gethostbyname(c))
#define gethostname(c,l)      (SocketBase ? gethostname(c,l)      : sim_gethostname(c,l))
#define getsockname(s,a,l)    (SocketBase ? getsockname(s,a,l)    : sim_getsockname(s,a,l))
#define getdomainname(c,l)    sim_getdomainname(c,l)
#define setsockopt(s,a,b,c,d) (SocketBase ? setsockopt(s,a,b,(char*)(c),d) : 0                   )
#define listen(s,q)           (SocketBase ? listen(s,q)           : 0                   )
#define recvfrom(s,b,c,f,a,l) (SocketBase ? recvfrom(s,b,c,f,a,_CAST((LONG *),(l))) : -1 )
#define sendto(s,m,n,f,a,l)   (SocketBase ? sendto(s,m,n,f,a,l)   : -1                  )
#define inet_addr(a)          (SocketBase ? inet_addr(a)          : sim_inet_addr(a)    )
#define inet_ntoa(a)          (SocketBase ? Inet_NtoA(a.s_addr)   : sim_inet_ntoa(a)    )

#define ntohl(x) (x)

#ifdef __SASC
#define socket_ioctl(s,t,u)   (SocketBase ? IoctlSocket(s,t,(char *)(u))  : 0                   )
#define socket_select(s,t,u,v,w) (SocketBase ? WaitSelect(s,t,u,v,w,NULL) : sim_select(s,t,u,v,w))
#else /* _DCC */
#define socket_ioctl(s,t,u)   (SocketBase ? ioctl(s,t,u)          : 0                   )
#define socket_select(s,t,u,v,w) (SocketBase ? select(s,t,u,v,w) : sim_select(s,t,u,v,w))
#endif

#endif /* AMITCP */

#ifdef AS225
extern struct Library *SockBase;

#define socket_close(s)       if (SockBase) s_close(s); else sim_close(s)
#define socket_write(s,b,l)   (SockBase ? send(s,b,l,0)         : sim_write(s,b,l)    )
#define socket_read(s,b,l)    (SockBase ? recv(s,b,l,0)         : sim_read(s,b,l)     )
#define socket_select(s,t,u,v,w) (SockBase ? select(s,t,u,v,w) : sim_select(s,t,u,v,w))
#define shutdown(a,b)         (SockBase ? shutdown(a,b)         : sim_shutdown(a,b)   )
#define socket(a,b,c)         (SockBase ? socket(a,b,c)         : sim_socket(a,b,c)   )
#define accept(a,b,c)         (SockBase ? accept(a,b,_CAST((LONG *),(c))) : sim_accept(a,b,_CAST((LONG *),(c))) )
#define bind(a,b,c)           (SockBase ? bind(a,b,c)           : sim_bind(a,b,c)     )
#define gethostbyname(c)      (SockBase ? gethostbyname(c)      : sim_gethostbyname(c))
#define gethostname(c,l)      (SockBase ? gethostname(c,l)      : sim_gethostname(c,l))
#define getsockname(s,a,l)    (SockBase ? getsockname(s,a,l)    : sim_getsockname(s,a,l))
#define getdomainname(c,l)    sim_getdomainname(c,l)
#define setsockopt(s,a,b,c,d) (SockBase ? setsockopt(s,a,b,(char*)(c),d) : 0                   )
#define listen(s,q)           (SockBase ? listen(s,q)           : 0                   )
#define recvfrom(s,b,c,f,a,l) (SockBase ? recvfrom(s,b,c,f,a,_CAST((LONG *),(l))) : -1 )
#define sendto(s,m,n,f,a,l)   (SockBase ? sendto(s,m,n,f,a,l)   : -1                  )
#define inet_addr(a)          (SockBase ? inet_addr(a)          : sim_inet_addr(a)    )
#define inet_ntoa(a)          (SockBase ? Inet_NtoA(a)          : sim_inet_ntoa(a)    )

#define ntohl(x) (x)

#ifdef __SASC
#define socket_ioctl(s,t,u)   (SockBase ? s_ioctl(s,t,u)        : 0                   )
#else /* _DCC */
#define socket_ioctl(s,t,u)   (SockBase ? s_ioctl(s,t,u,0)      : 0                   )
#endif

#endif

/* #define perror(str) printf("%s: %d %s\n", str, errno, strerror(errno)) */

#endif /* SOCKET_TCP_H */
