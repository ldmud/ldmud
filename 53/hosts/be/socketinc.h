#ifndef BE_SOCKETINC_H
#define BE_SOCKETINC_H

#include <sys/socket.h>
#include <netinet/in.h>

#define SOCKET_T int
#define socket_number(s) (s)
#define socket_ioctl  ioctl
#define socket_select select
#define socket_read(fd,pt,l)   recv(fd,pt,l,0)
#define socket_write(fd,pt,l)  (send(fd,pt,l,0) >= 0 ? l : -1)
#define socket_close  closesocket
#define socketpair(a,b,c,d) (-1)

#ifdef __INTEL__
/* This function is missing from libroot.so.LIB :-( */
INLINE static uint16 _imp____swap_int16(uint16 uarg)
{
  return ((uarg & 0xff00) >> 16) | ((uarg & 0x00ff) << 16);
}
#endif

#endif /* BE_SOCKETINC_H */
