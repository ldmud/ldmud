/*
 *  Configuration file for os2 driver
 */

#include <os2.h>
#include <io.h>
#include "socket.h"

/*
 *  <sys/types.h> does not define FD macros
 *  when FD_SETSIZE is already defined.
 *  "hosts/os2/socket.h" defines different ones.
 */
#define FD_SETSIZE MAX_NP_SOCKETS

#ifndef OS2
#define OS2
#endif
