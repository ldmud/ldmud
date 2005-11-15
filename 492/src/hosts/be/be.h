#ifndef HOSTS_BE_H
#define HOSTS_BE_H

#include <BeBuild.h>

#define SOCKET_HEADER "hosts/be/socketinc.h"
#define SOCKET_LIB 1

#undef HOST_DEPENDENT_INIT
#define HOST_DEPENDENT_INIT \
    { no_erq_demon++; /* Be's fork() does not cover sockets, thus no auto-erq */\
    }


#ifdef __MWERKS__
#    if !defined(B_BEOS_VERSION_5)
#        define isascii(c) !((unsigned long)c & 0xFFFFFF80)
#    endif
#    ifndef EMSGSIZE
#        define EMSGSIZE 42
#    endif
#endif

#define FNDELAY O_NONBLOCK

#endif
