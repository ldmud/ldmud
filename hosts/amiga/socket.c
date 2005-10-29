/* hosts/amiga/socket.c */

#if defined(AMIGA_TCP)
#  include "socket_tcp.c"
#endif

#if !defined(AMIGA_TCP) || defined(AMITCP)
#  include "socket_sim.c"
#endif

void amiga_sockinit (void)
{
  #if defined(AMIGA_TCP)
    amiga_tcpinit();
  #endif
  #if !defined(AMIGA_TCP) || defined(AMITCP)
    amiga_siminit();
  #endif
}

void amiga_sockexit (void)
{
  #if defined(AMIGA_TCP)
    amiga_tcpexit();
  #endif
  #if !defined(AMIGA_TCP) || defined(AMITCP)
    amiga_simexit();
  #endif
}

