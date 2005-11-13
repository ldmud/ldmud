/* hosts/amiga/amiga.h */

#ifndef _HOST_AMIGA_H_
#define _HOST_AMIGA_H_

#define SOCKET_HEADER "hosts/amiga/socket.h"
#define SOCKET_LIB 1

#undef ixstat
#undef ixopen
#undef ixopen3

#include "hosts/amiga/ixfile.h"

#define NO_IP_DEMON

#if !defined(AMIGA_TCP)
  #undef CATCH_UDP_PORT
  #undef UDP_SEND
#endif

#undef HOST_DEPENDENT_INIT
#define HOST_DEPENDENT_INIT \
    { extern void amiga_init(void), amiga_end(void);            \
      amiga_init();  /* Will exit(20) on wrong OS */            \
      atexit (amiga_end);                                       \
      no_erq_demon++; /* Amiga has no fork(), thus no auto-erq */\
    }

#endif
