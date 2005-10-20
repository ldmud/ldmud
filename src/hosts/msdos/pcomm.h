/* portable communication, socket emulation - WA */

#include "pcdef.h"
#include "in.h"

#define fd_set int
#define FD_ZERO(mask)       *(mask) = 0
#define FD_SET(chan,mask)   *(mask) |= 1 << (chan)
#define FD_CLR(chan,mask)   (*(mask) &= ~(1 << (chan)))
#define FD_ISSET(chan,mask) (*(mask) & (1 << (chan)))

#define bind  unused1
/* #define bzero unused2 */

#define errno c_errno

#define EMSGSIZE     ECOMUSG
/* #define EINVAL       ECOMLNK */
#define ENETUNREACH  ECOMNIL
#define EHOSTUNREACH (ECOMNIL+1)
/* #define EPIPE        (ECOMNIL+2) */
#define ETIMEDOUT    (ECOMNIL+3)
#define ECONNRESET   (ECOMNIL+4)
#ifndef EWOULDBLOCK
#define EWOULDBLOCK  ECOMFLW
#endif
#ifndef EAGAIN
#define EAGAIN       ECOMFLW
#endif
#ifndef EINTR
#define EINTR        ECOMPRM
#endif

char *inet_ntoa(struct in_addr ad);
