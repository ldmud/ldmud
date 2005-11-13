#include <erq.h>

#define SOCKETD	"secure/sockets/socketd"

#define MAX_SOCKETS	15

#define SOCKET_ACCEPT	0
#define SOCKET_READ	1
#define SOCKET_CLOSE	2
#define SOCKET_ERROR	3
#define SOCKET_READY	4
#define SOCKET_TRANSFER	5

#define SOCKET_BINARY	1

#ifndef __ERQ_MAX_REPLY__
#define __ERQ_MAX_REPLY__ 1024
#endif

#ifndef __ERQ_MAX_SEND__
#define __ERQ_MAX_SEND__ 256
#endif
