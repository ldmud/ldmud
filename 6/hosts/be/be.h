#ifndef HOSTS_BE_H
#define HOSTS_BE_H

#define SOCKET_HEADER "hosts/be/socketinc.h"
#define SOCKET_LIB 1

#define HOST_DEPENDENT_INIT 

#define isascii(c) !((unsigned long)c & 0xFFFFFF80)
#define EMSGSIZE 40
#define FNDELAY O_NONBLOCK
#define CHARBITS (0xFF)

#endif
