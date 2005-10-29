/* hosts/amiga/amiga.c
**
** Collects all routines needed for the Amiga which are not specific
** enough to go into one of the other files.
**
**   25-Feb-93 [lars]
**   28-Feb-93 [lars] Moved to DICE 2.07.53
**   02-Apr-93 [lars] send_udp() dummy added.
**   09-Apr-93 [lars] Overloaded break check of the compiler.
**   17-Jun-93 [lars] Put in support for AmiTCP.
**   20-Sep-93 [lars] Fixed small but fatal bug in chmod().
**   23-Nov-93 [lars] Made version string more style conform.
**   30-Nov-93 [lars] ixconvert() et al. exported into ixfile.c
**   16-Dec-93 [lars] SystemTagList() exported into ixfile.c
**   05-Jun-94 [lars] cleanup_alarm() called on shutdown.
**   11-Aug-95 [lars] Added check for stacksize.
**   18-Aug-95 [lars] fcntl() simulation for SAS/C added.
**   01-Nov-95 [lars] dup() simulation for DICE added.
**   06-Nov-95 [lars] Standins for execl(), fork(), dup2(), socketpair() added.
**   11-Nov-95 [lars] amiga_sockini/exit() now also with simulated sockets.
**   01-May-96 [lars] For DICE, the dynamic stack parameters are increased
**                    above the default values. depending on the current
**                    shell stacksize.
*/

/*-----------------------------------------------------------------------*/

#include <exec/types.h>
#include <exec/libraries.h>
#include <exec/execbase.h>
#include <exec/nodes.h>
#include <exec/tasks.h>

#ifdef INCLUDE_VERSION
#include <dos/dos.h>
#include <clib/dos_protos.h>
#include <clib/exec_protos.h>
#else
#include <libraries/dos.h>
#endif

#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <strings.h>
#include "config.h"
#include "patchlevel.h"
#include "nsignal.h"

#if defined(_DCC) && !defined(INCLUDE_VERSION)  /* for fstat() */
#include <sys/stat.h>
#include <stdlib.h>
#include <fcntl.h>
#include <string.h>
#endif

#define STACKNEEDED 50000

extern struct Library *SysBase; /* DICE runtime will open it */

void init_rusage(void); /* in port.c */

/*-----------------------------------------------------------------------*/

#ifdef INCLUDE_VERSION
#  define OSVERSION "2.0"
#  ifndef AMIGA_TCP
#    define NETVERSION
#    define NETSTRING ""
#  elif defined(AMITCP)
#    define NETVERSION ", AmiTCP"
#    define NETSTRING NETVERSION
#  else /* AS225 */
#    define NETVERSION ", AS225"
#    define NETSTRING NETVERSION
#  endif /* type of net interface */
#else  /* OS 1.3 */
#  define OSVERSION "1.3"
#  define NETVERSION
#  define NETSTRING ""
#endif

static char ver[] =
  "\0$VER: Amylaar-LPMud " GAME_VERSION PATCH_LEVEL LOCAL_LEVEL " (" __DATE__ ") OS " OSVERSION NETVERSION " #950205";
static APTR oldException = NULL;
static ULONG oldExceptSig = 0L;

extern ULONG sys_signal_alarm;

extern void amiga_sockinit(void);
extern void amiga_sockexit(void);

/*-----------------------------------------------------------------------
** These variables parametrize DICE' dynamic stack functionality.
** These defaults are overwritten during initialization with values
** derived from the actual stacksize.
*/

long _stack_chunk = 50000;
long _stack_fudge = 25000;

/*-----------------------------------------------------------------------*/
int CheckStacksize (LONG try_size, LONG * pMySize)

/* Check the stacksize of this process agains try_size, return TRUE
 * if the actual stacksize is greater or equal.
 * If pMySize is given, the actual stacksize is store there.
 */

{
  struct Task *pThis;
  LONG dummy;

  if (!pMySize)
    pMySize = &dummy;
  *pMySize = 0L;
  pThis = (struct Task *)FindTask(NULL);
  /* Sanity checks, shouldn't happen anyway */
  if (!pThis || pThis->tc_Node.ln_Type != NT_PROCESS)
    return FALSE;
  *pMySize = (LONG)((char *)pThis->tc_SPUpper - (char *)pThis->tc_SPLower);
  return *pMySize >= try_size;
}

/*-----------------------------------------------------------------------
** void amiga_init(void);
** void amiga_end(void);
**
** Perform all necessary setup/setdown operations.
*/

void amiga_init (void) {
  LONG stackSize;
  char *vp = ver;  /* so the version string won't be optimized away */

  printf ("dr %s%s (Amiga, OS %s%s)\n", GAME_VERSION, PATCH_LEVEL LOCAL_LEVEL, OSVERSION, NETSTRING);
#ifdef INCLUDE_VERSION
  if (SysBase->lib_Version < 36) {
    printf ("Fatal: Need OS 2.0 to run.\n");
    exit(20);
  }
#endif
  if (!CheckStacksize(STACKNEEDED, &stackSize))
  {
    printf("Fatal: Need at least %ld bytes stack.\n", STACKNEEDED);
    exit(20);
  }

#ifdef _DCC
  if (stackSize > _stack_chunk)
  {
    _stack_chunk = stackSize;
    _stack_fudge = stackSize / 2;
  }
#endif
  init_rusage();
  amiga_sockinit();
  oldException = ((struct Task*)FindTask(NULL))->tc_ExceptCode;
  oldExceptSig = SetExcept(0L, 0L);
}

void amiga_end (void) {
  static short done = 0;
  if (done++) return;  /* May be called multiple times */
  cleanup_alarm();
  SetExcept (oldExceptSig, EXT_SIGINT | EXT_SIGHUP | sys_signal_alarm);
  ((struct Task*)FindTask(NULL))->tc_ExceptCode = oldException;
  amiga_sockexit();
}

/*-----------------------------------------------------------------------
** int send_udp (char *to_host, int to_port, char *msg)
**
** This is normally implemented in comm1.c when UDP communications
** are used. Unfortunately, since make_func doesn't incarnate a full
** preprocessor, compiling with simulated sockets makes interpret.c access
** send_udp() even though comm1.c doesn't know about it.
** So this dummy...
*/

#ifndef UDP_SEND
int send_udp(char *to_host, int to_port, char *msg)
{
  return 0;
}
#endif

/*-----------------------------------------------------------------------
** Some Unix-functions we don't support.
** The dummies here satisfy references in comm1.c from the start_erq_demon()
** code.
*/

int fork (void) { errno = EPERM; return -1; }
int dup2 (int a, int b) { errno = EPERM; return -1; }
int socketpair (int a, int b, int c, int *d) { errno = EPERM; return -1; }
int execl (char *a, char *b, char * c, char * d) { errno = EPERM; return -1; }

/*-----------------------------------------------------------------------
** DICE-specifics.
*/

#if defined(_DCC)


/*-----------------------------------------------------------------------
** The routine the DICE runtime lib will call for Ctrl-C checks.
** It is overloaded so it will work with LPMuds special signal handling.
*/

void chkabort(void) { check_signals(); }

/*-----------------------------------------------------------------------
** DICE has the prototype, but not the fun...
*/

void *memchr (const void *buf, int c, size_t s) {
  while (s--) if (*(char *)buf == (char) c) return buf; else ((char *)buf)++;
  return NULL;
}

/*-----------------------------------------------------------------------
** Simulate a dup() to allow main.c to gobble up filedescriptiors.
** The duplicated fds can't be used, fd_exec() takes care of this.
*/

static long fd_exec (void) {
  fputs("Error: attempt to use fd created by simulated dup()\n", stderr);
  errno = ENOENT;
  return -1;
}

int dup (int fd) {
  return MakeFd(0, 0, fd_exec);
}

#ifndef INCLUDE_VERSION /* !OS 2.0 */
/*-----------------------------------------------------------------------
** DICE's implementation of fstat() uses dos ExamineFH() when running
** under OS 2.0. Unfortunately there's no stub for it the 1.3-amiga.lib
** so compiling for OS 1.3 can't be done.
** To circumvent this, this downstripped version of fstat() is used
** when compiling for OS 1.3. Due to that OS limitations, it doesn't
** work properly.
*/

typedef struct FileInfoBlock  FileInfoBlock;

fstat(int fd, struct stat *xstat) {
  int r = -1;
  _IOFDS *d;

  clrmem(xstat, sizeof(*xstat));
  if (d = __getfh(fd)) {
    r = stat(d->fd_FileName, xstat);
    /*
     *        extended size will not show up in examine if we have written
     *        the active handle.
     */
    {
      long pos = Seek(d->fd_Fh, 0L, 0);
      long siz;
      Seek(d->fd_Fh, 0L, 1);
      siz = Seek(d->fd_Fh, pos, -1);
      if (xstat->st_size < siz)
      xstat->st_size = siz;
    }
  }
  return(r);
}

#endif /* !OS 2.0 */

#endif /* DICE */

/*-----------------------------------------------------------------------
** SAS/C-specifics.
*/

#if defined(__SASC)

/*-----------------------------------------------------------------------
** comm1.c needs at least a dummy for fcntl().
*/

int fcntl(int fd, int cmd, int data)
{
  fprintf(stderr, "fcntl(%d,%d) not implemented.\n", fd, cmd);
  return 0;
}

#endif /* __SASC */

/*************************************************************************/
