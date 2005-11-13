#include "pcomm.h"
#include "in.h"
#include "c_msdos.h"
#define SET_BUFFER_SIZE_MAX 65536
#define SIGUSR1    18    /* Not in signal.h 2.5.7 - is in 2.6.0 */
#define SOCKET_T   int
#define socket_number(s)         (s)
#define socket_ioctl             ioctl
#define socket_select(n,r,w,e,t) (timer_expire(),c_select(r,t))
#define socket_read              c_read
#define socket_write             c_write
#define socket_close             c_close
#define shutdown(x,y)            0
#define inet_addr(a)             0
#define getpeername(s,a,l)       c_getaddr(s,a,l)
#define accept(s,a,l)            c_accept(a,l)
#define sockaddr caddr
