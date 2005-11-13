#ifndef HOSTS_BE_H
#define HOSTS_BE_H

#define SOCKET_HEADER "hosts/be/socketinc.h"
#define SOCKET_LIB 1

#undef HOST_DEPENDENT_INIT
#define HOST_DEPENDENT_INIT \
    { no_erq_demon++; /* Be's fork() does not cover sockets, thus no auto-erq */\
    }


#ifdef __MWERKS__
#define isascii(c) !((unsigned long)c & 0xFFFFFF80)
#define EMSGSIZE 40
#endif

#define FNDELAY O_NONBLOCK

#endif
