/*
 *  hosts/os2/socket.c 6-26-94
 *  (based on atari version)
 */

#define noDBUG

#define INCL_DOSPROCESS
#define INCL_DOSSEMAPHORES

#include <os2.h>
#include <stdlib.h>
#include <sys/time.h>
/*#include <sys/types.h>*/
#include <io.h>
#include <string.h>
#include <strings.h>
#include <stdio.h>
#include <process.h>
#include <fcntl.h>

#include "socket.h"

extern int errno;

static SOCKET_T all_sockets;
static int num_sockets = 0;
static HEV select_event = 0;

static void
initialize_sockets ()
{
  if (DosCreateEventSem (NULL, &select_event, 0L, FALSE))
  {
    printf ("OS/2 socket emulation: DosCreateEventSem() failed.\n");
    exit (0);
  }
}

int
socket_write (s, buffer, length)
SOCKET_T s;
char *buffer;
int length;
{
  ULONG count;

  if (DosWrite (s -> fd, buffer, length, &count))
  {
    errno = EWOULDBLOCK;
    return -1;
  }
  return count;
}

#define ACCEPT_FD 0
#define ACCEPT_MASK (1 << ACCEPT_FD)

int
socket_select (num, readfds, writefds, xfds, timeout)
int num;
fd_set * readfds, writefds, xfds;
struct timeval *timeout;
{
  ULONG q, ulTimeout;
  SOCKET_T cc;
  unsigned long rfds, mask, rrfds;
  int count = 0;
  
  DosEnterCritSec ();

  rfds = *readfds;
  for (cc = all_sockets; cc; cc = cc -> next)
    if ( (ACCEPT_MASK & rfds && cc -> type == SOCKET_T_OPENING) ||
	 ( (cc -> incoming != IN_EMPTY || cc -> type == SOCKET_T_DISCO) &&
	   (1 << cc->fd) & rfds ) )
    {
      count = 1;
      break;
    }

  if (count == 0)
  {
    DosExitCritSec ();

    if (timeout == NULL)
      ulTimeout = SEM_INDEFINITE_WAIT;
    else
      ulTimeout = timeout -> tv_sec * 1000 +
	          timeout -> tv_usec / 1000;
  
    DosWaitEventSem (select_event, ulTimeout);

    DosEnterCritSec ();
  }
  
  rrfds = 0;
  count = 0;
  
  for (cc = all_sockets; cc; cc = cc -> next)
  {
    mask = 1 << cc -> fd;
    
    if (mask & rfds)
    {
      if (cc -> incoming != IN_EMPTY ||
	  cc -> type == SOCKET_T_DISCO)
      {
	cc -> incoming = IN_SELECTED;
	rrfds |= mask;
	count++;
      }
    }
    if (ACCEPT_MASK & rfds && cc -> type == SOCKET_T_OPENING) {
	rrfds |= ACCEPT_MASK;
	count++;
    }
  }
  if (count)
    *readfds = rrfds;
  
  DosResetEventSem (select_event, &q);
  
  DosExitCritSec ();
  
  return count;
}

void
shutdown(s,n)
struct sock_buff *s;
int n;
{}

static void
remove_socket (s)
SOCKET_T s;
{
  SOCKET_T cc, prev = NULL;

  if (s -> type == SOCKET_T_ACCEPT)
  {
    free (s);
    return;
  }
  DosEnterCritSec ();

  if (s == all_sockets)
    all_sockets = all_sockets -> next;
  else
    for (cc = all_sockets; cc; cc = cc -> next) {
      if (s == cc) {
        prev -> next = cc -> next;
        break;
      }
      prev = cc;
    }

  free (s);

  DosExitCritSec ();
}

void
socket_close (s)
SOCKET_T s;
{
  num_sockets--;

#if 0
  if (s -> thread_id)
    DosKillThread (s -> thread_id);
#endif
  if (s -> type == SOCKET_T_DISCO)
  {
    remove_socket (s);
  }
  else if (s -> fd)
  {
    DosEnterCritSec ();
    close (s -> fd);
    s -> fd = 0;
    DosExitCritSec ();
  }
}

struct hostent *
gethostbyname (host_name)
char *host_name;
{
  static struct hostent current_hostend = { { 0, 0 }, 2, 0 };

  return &current_hostend;
}

SOCKET_T
socket (addr_type, mode, i)
int addr_type;
int mode;
int i;
{
  static init = 0;
  SOCKET_T nc;

#ifdef DBUG
  printf ("Socket.\n");
#endif

  if (init == 0)
  {
    init = 1;
    initialize_sockets ();
  }

  if (++num_sockets >= MAX_NP_SOCKETS)
    return (SOCKET_T) -1;
  
  nc = (SOCKET_T) malloc (sizeof (struct sock_buff));
  
  if (nc == NULL)
    return (SOCKET_T) -1;
  
  bzero (nc, sizeof (struct sock_buff));
  nc -> type = SOCKET_T_ACCEPT;
  nc -> incoming = IN_EMPTY;
  nc -> next = NULL;
  nc->fd = ACCEPT_FD;
  
  return nc;
}

static VOID
connection_thread (ULONG ulThreadArg)
{
  ULONG q, count;
  SOCKET_T s;

  s = (SOCKET_T) ulThreadArg;

#ifdef DBUG
  printf ("Connect.\n");
#endif

  while (1)
  {
    if (s -> fd == 0)
    {
#ifdef DBUG
      printf ("Disconnect (local).\n");
#endif
      remove_socket (s);    
      _endthread ();
    }
    if (DosRead (s -> fd, s -> in,
        MAX_SOCKET_PACKET_SIZE, &count) || count == 0)
    {
      if (s -> fd == 0) continue;
#ifdef DBUG
      printf ("Disconnect (remote).\n");
#endif
      DosEnterCritSec ();
      close (s -> fd);
      s -> type = SOCKET_T_DISCO;
      s -> thread_id = 0;
      if (s -> fd) s -> fd = 0;
      DosExitCritSec ();
#if 0 
      DosExit (EXIT_THREAD, 0);
#endif
      _endthread ();
    }
    if (s -> fd == 0) continue;

    DosEnterCritSec ();
    s -> incoming = IN_BUFFERED;
    s -> in_count = count;
    DosPostEventSem (select_event);
    DosExitCritSec ();

    DosWaitEventSem (s -> event_sem, SEM_INDEFINITE_WAIT);
    DosResetEventSem (s -> event_sem, &q);
  }
}

static VOID
accept_thread (ULONG ulThreadArg)
{
  char pipename[64];
  SOCKET_T s, n;
  HPIPE fd = 0;
  ULONG count;

  s = (SOCKET_T) ulThreadArg;

  while (1)
  {
    if (s -> fd == 0)
    {
      free (s);
      _endthread ();
    }
    else if (DosConnectNPipe (s -> fd) == 0)
    {
      if (s -> fd == 0) continue;
#ifdef DBUG
      printf ("Client connect (sockets=%d).\n", num_sockets);
#endif
      DosEnterCritSec ();

      close (fd = dup(0));
      sprintf (pipename, CLIENT_PIPE, fd);

      if (DosCreateNPipe (pipename, &fd,
	  NP_ACCESS_DUPLEX, NP_WAIT |
	  NP_READMODE_BYTE | NP_TYPE_BYTE | ONE_INSTANCE,
          MAX_SOCKET_PACKET_SIZE, MAX_SOCKET_PACKET_SIZE,
          TIME_OUT) == 0)
      {
        DosWrite (s -> fd, &fd, 1, &count);
        DosDisConnectNPipe (s -> fd);
        DosConnectNPipe (fd);

        if ((n = socket (0, 0, 0)) != (SOCKET_T) -1) /* arguments are ignored */
	{
          n -> type = SOCKET_T_OPENING;
	  n -> fd = fd;

	  if (DosCreateEventSem (NULL, &n -> event_sem, 0L, FALSE) == 0)
	  {
	    n -> next = all_sockets;
	    all_sockets = n;

#if 1
	    n -> thread_id = _beginthread (connection_thread,
			     NULL, 8192L, (void *) n);
            if (n -> thread_id == -1)
#endif
#if 0
	    if (DosCreateThread (&n -> thread_id, connection_thread,
	      (ULONG) n, 0L, 8192L))
#endif
	    {
	      all_sockets = all_sockets -> next;
	      DosCloseEventSem (n -> event_sem);
	      free (n);
	      close (fd);
	    }
	  }
	  else {
	    free (n);
	    close (fd);
	  }
	} /* socket */
	else
	  close (fd);
      } /* CreatePipe */
      DosExitCritSec ();
    }
  } /* while (1) */
}

int
bind (sock, addr, addr_size)
SOCKET_T sock;
struct sockaddr *addr;
int addr_size;
{
  char pipename[64];
  
  sprintf (pipename, REQUEST_PIPE, addr -> a.sin_port);

  if (DosCreateNPipe (pipename, &sock -> fd,
      NP_ACCESS_DUPLEX, NP_WAIT |
      NP_READMODE_BYTE | NP_TYPE_BYTE | ONE_INSTANCE,
      MAX_SOCKET_PACKET_SIZE, MAX_SOCKET_PACKET_SIZE,
      TIME_OUT))
    return -1;

#if 1
  sock -> thread_id = _beginthread (accept_thread,
		      NULL, 8192L, (void *) sock);
  if (sock -> thread_id == -1)
#endif
#if 0
  if (DosCreateThread (&sock -> thread_id, accept_thread,
      (ULONG) sock, 0L, 8192L))
#endif
  {
    close (sock -> fd);
    return -1;
  }
  return 0;
}

SOCKET_T
accept (sock, addr, addr_len)
SOCKET_T sock;
struct sockaddr *addr;
int *addr_len;
{
  SOCKET_T cc, s = NULL;

  DosEnterCritSec ();

  for (cc = all_sockets; cc; cc = cc -> next)
    if (cc -> type == SOCKET_T_OPENING)
    {
      cc -> type = SOCKET_T_CONNECTED;
      s = cc;
      break;
    }

  if (s == NULL)
  {
    errno = EWOULDBLOCK;
    s = (SOCKET_T) -1;
  }
  DosExitCritSec ();

#ifdef DBUG
  if (s != -1)
    printf ("Accept: %d.\n", s -> fd);
#endif

  return s;
}

int
socket_read (s, buff, limit)
SOCKET_T s;
char *buff;
int limit;
{
  if (s -> type == SOCKET_T_DISCO)
  {
#ifdef DBUG
    printf ("Read: Connection reset.\n");
#endif
    errno = ECONNRESET;
    return -1;
  }
  if (s -> incoming != IN_SELECTED)
  {
#ifdef DBUG
    printf ("Read: not yet selected.\n");
#endif
    return 0;
  }
  DosEnterCritSec ();

  if (s -> in_count < limit)
    limit = s -> in_count;

  s -> incoming = IN_EMPTY;
  s -> in_count = 0;
  DosPostEventSem (s -> event_sem);

  if (limit)
    bcopy (s -> in, buff, limit);

  DosExitCritSec ();

#ifdef DBUG
  printf ("Read: %d bytes.\n", limit);
#endif

  return limit;
}

int
getpeername (s, addr, addr_len)
SOCKET_T s;
struct sockaddr *addr;
int *addr_len;
{
  if (*addr_len > sizeof (long))
    *addr_len = sizeof (long);
  bzero (addr, *addr_len);
  return 0;
}

char *
gethostname (char *name, int length)
{
  char *s;
  int n;

  if ((s = getenv ("SYSTEMNAME")) == NULL)
    if ((s = getenv ("HOSTNAME")) == NULL)
      s = "OS2";

  n = strlen (s) + 1;
  length = n < length ? n : length;   

  strncpy (name, s, n);
  return name;
}

