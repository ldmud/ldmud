/* hosts/amiga/socket.h */

#if defined(AMIGA_TCP)
#  include "hosts/amiga/socket_tcp.h"
#else
#  include "hosts/amiga/socket_sim.h"
#endif

#ifdef SO_OOBINLINE
#undef SO_OOBINLINE
#endif
