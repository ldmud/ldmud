#include <winsock.h>

#include <winbase.h>

extern void set_socket_errno();

#define SOCKET_T int
#define socket_number(s) (s)

socket_select(a,b,c,d,e) ({	\
  int tmp=select(a,b,c,d,e);	\
  if (tmp < 0)			\
    set_socket_errno();		\
  tmp;				\
})

#define socket_read(a,b,c) ({	\
  int tmp = recv(a,b,c,0);	\
  if (tmp < 0)			\
    set_socket_errno();		\
  tmp;				\
})

#define socket_write(a,b,c) ({	\
  int tmp = send(a,b,c,0)	\
  if (tmp < 0)			\
    set_socket_errno();		\
  tmp;				\
})

#define socket_ioctl(a,b,c)	\
  int tmp = ioctlsocket(a,b,c);	\
  if (tmp < 0)			\
    set_socket_errno();		\
  tmp;				\
})

#define socket_close(s)  {					\
  int tmp = 0;							\
  struct linger ling = { 1, 5 };				\
  socket_ioctl(s, FIONBIO, &tmp);				\
  setsockopt (s,SOL_SOCKET,SO_LINGER,&ling,sizeof(struct linger));\
  closesocket(s);						\
}

#define       SIGURG  NSIG+1  /* urgent condition on IO channel */

#define shutdown(s, i) 0

extern OSVERSIONINFO win32osi;
extern SYSTEM_INFO win32si;
