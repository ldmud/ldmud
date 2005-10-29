/* hosts/amiga/amiga.h */

#ifndef _HOST_AMIGA_H_
#define _HOST_AMIGA_H_

#define SOCKET_HEADER "hosts/amiga/socket.h"
#define SOCKET_LIB 1

#undef ixstat
#undef ixopen
#undef ixopen3

#include "hosts/amiga/ixfile.h"

#if defined(AMIGA) && defined(__SASC)
#  if __VERSION__ * 1000 + __REVISION__ < 6055
#    error Need SAS/C 6.55 or better to compile.
#  endif
#endif

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
