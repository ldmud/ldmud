/* hosts/amiga/socket_tcp.c
**
** Implement the extra framework to use either the Commodore socket.library
** or the AmiTCP package for LPMud.
**
** Commodore support written by Martin Brenner.
**
**   13-Jan-93 [lars]  Merged.
**   17-Jun-93 [lars]  Added support for AmiTCP.
**   02-Aug-93 [lars]  Adapted for AmiTCP-2.0
**   11-Apr-95 [lars]  Adapted for AmiTCP-3.0 netlib of SAS.
**   20-Aug-95 [lars]  Patched around bogus netlib autoinitialisation.
**   11-Nov-95 [lars]  amiga_sockinit/exit() renamed in amiga_tcpinit/exit().
*/

/*-----------------------------------------------------------------------*/

#ifdef AS225

#include <clib/exec_protos.h>

#include <sys/types.h>
#include <sys/socket.h>

struct Library *SockBase = NULL;

/* int socket_error; */

/* this is the maximum number of sockets that you want */

#define MAXSOCKS 50

/* required for Initialization of socket.library */

void amiga_tcpinit ( void ) {
  if ((SockBase = OpenLibrary("socket.library", 1L)) == NULL)
    printf("  Can't open 'socket.library' - using simulated sockets instead.\n");
  else {
    setup_sockets(MAXSOCKS, &errno);
    printf("  socket.library found and accessed.\n");
  }
}

/* exit in a clean way (close remaining sockets */

void amiga_tcpexit ( void ) {
  if (SockBase != NULL) {
    cleanup_sockets();
    CloseLibrary(SockBase);
    SockBase = NULL;
  }
}

#endif /* AS225 */


/*-----------------------------------------------------------------------*/

#ifdef AMITCP

#include <exec/types.h>
#include <clib/exec_protos.h>
#include <clib/socket_protos.h>
#include <sys/types.h>
#include <stdio.h>

struct Library *SocketBase = NULL;  /* This prevents linking of autocode */

#if defined(__SASC) && AMITCP >= 3

/* AmiTCP's netlib implements an autoinit function _STI_200_openSockets()
 * to automagically open the bsdsocket.library. Unfortunately this function
 * exit()s out if the library can't be found, which we don't want as we
 * could use the socket simulation instead.
 * Therefore we have to remove this very function from the list of
 * autoinit function before it is called, using an autoinit function of
 * slightly higher priority.
 */

extern int (* far __ctors)[](void);

/* Dummyfunction to take the place of the netlib function. */
static int netlib_dummy()
{
  return 0;

}

void _STI_190_fudgeNetlib()
{
  int i;
  for (i = 0; __ctors[i]; i++)
    if (__ctors[i] == _STI_200_openSockets)
      __ctors[i] = netlib_dummy;
}

#endif

void amiga_tcpinit (void) {
  int rc, libversion;

#ifdef __SASC
#  if AMITCP < 3
     libversion = 2;
     rc = _STIopenSockets();
#  else
     /* The provided ...openSockets() bogues out via exit() if AmiTCP
      * is not available, what we can't use. So check for the existance
      * of the bsdsocket.library first.
      */
     libversion = AMITCP;
     SocketBase = OpenLibrary("bsdsocket.library", libversion);
     if (SocketBase)
     {
       CloseLibrary(SocketBase);
       SocketBase = NULL;
     rc = _STI_200_openSockets();
     }
     else
       rc = 3;
#  endif
#else /* _DCC */
  libversion = 2;
  rc = _openSockets();
#endif
  if (rc) {
    printf("  Can't access AmiTCP-%d because ", libversion);
    switch (rc) {
      case 1: printf("of wrong OS version"); break;
      case 2: printf("outdated net.lib was used."); break;
      case 3: printf("OpenLibrary() failed"); break;
      default: printf("of an unknown failure"); break;
    }
    printf(".\n  Socket simulation is used instead.\n");
  }
  else
    printf("  AmiTCP found and accessed.\n");
}

void amiga_tcpexit (void) {
#ifdef __SASC
#  if AMITCP < 3
     _STDcloseSockets();
#  else
     _STD_200_closeSockets();
#  endif
#else /* _DCC */
#endif
}

#endif /* AMITCP */

/*-----------------------------------------------------------------------*/

/*************************************************************************/
