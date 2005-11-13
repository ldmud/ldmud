/* machine.h.  Generated automatically by configure.  */
#ifndef MACHINE_H
#define MACHINE_H

/* Define if using alloca.c.  */
#define C_ALLOCA 1

/* Define to one of _getb67, GETB67, getb67 for Cray-2 and Cray-YMP systems.
   This function is required for alloca.c support on those systems.  */
/* #undef CRAY_STACKSEG_END */

/* Define if you have dirent.h.  */
#define DIRENT 1

/* Define if you have alloca.h and it should be used (not Ultrix).  */
#define HAVE_ALLOCA_H 1

/* Define as __inline if that's what the C compiler calls it.  */
/* #undef inline */

/* Define if you don't have dirent.h, but have ndir.h.  */
/* #undef NDIR */

/* Define to `int' if <sys/types.h> doesn't define.  */
#define pid_t int

/* Define as the return type of signal handlers (int or void).  */
#define RETSIGTYPE void

/* Define to `unsigned' if <sys/types.h> doesn't define.  */
/* #define size_t unsigned */

/* If using the C implementation of alloca, define if you know the
   direction of stack growth for your system; otherwise it will be
   automatically deduced at run-time.
	STACK_DIRECTION > 0 => grows toward higher addresses
	STACK_DIRECTION < 0 => grows toward lower addresses
	STACK_DIRECTION = 0 => direction of growth unknown
 */
/* #undef STACK_DIRECTION */

/* Define if you have the ANSI C header files.  */
/* #undef STDC_HEADERS */

/* Define if you don't have dirent.h, but have sys/dir.h.  */
/* #undef SYSDIR */

/* Define if you don't have dirent.h, but have sys/ndir.h.  */
/* #undef SYSNDIR */

/* Define if the closedir function returns void instead of int.  */
#define VOID_CLOSEDIR 1

/* Is the library function inet_ntoa() compatible with the compiler ? */
/* #undef INET_NTOA_OK */

/* The following values must be evaluable by the preprocessor */
#define SIZEOF_P_INT 4
#define SIZEOF_INT 4
#define SIZEOF_LONG 4

/* A mask that allows to extract an unsigned char from a signed */
#define CHARBIT_MASK CHARBITS

/* #undef RENAME_HANDLES_DIRECTORIES */

/* Does the system have a getrusage call?  */
/* #define HAVE_GETRUSAGE */
/* If so, is it restricted to user and system time? */
/* #undef GETRUSAGE_RESTRICTED */
/* Is it available as a subfunction of syscall() ? */
/* #undef GETRUSAGE_VIA_SYSCALL */
/* Can ru_utime / ru_stime be accessed as a timeval with tv_sec and tv_usec ? */
#define RUSAGE_USEC 1

/* the atari strtol() used to consider characters '9' < c < 'A' to be numeric */
#define STRTOL_BROKEN 1

/* does the libc consider it normal to free a null pointer? */
/* #undef FREE_NULL_POINTER */

/* needs the first argument of strtol be declared as const ? */
/* #undef STRTOL_CONST_CHARP */

/* Define if you have bcopy, and it handles overlapping ranges correctly. */
/* #undef OVERLAPPING_BCOPY */

#define MALLOC_ALIGN 8

/* does the compiler know of a 'long long' type? */
#define HAVE_LONG_LONG 1

/* what kind of pointer is used by malloc() et al */
#define POINTER void *
#define FREE_RETURNS_VOID 1

/* can we define our own malloc() safely? */
/* #undef SBRK_OK */

/* The following is needed for smalloc without SBRK_OK to use memory
 * efficiently. smalloc will malloc blocks that are a large power of
 * two, minus EXTERN_MALLOC_OVERHEAD. If you have no idea what number to
 * choose, compile & run util/overhead.c
 */
#define EXTERN_MALLOC_OVERHEAD  0

/* How to set a socket non-blocking */
/* #undef USE_IOCTL_FIONBIO */
/* #undef USE_FCNTL_O_NDELAY */
/* #undef USE_FCNTL_FNDELAY */

/* #undef inline */

/* A host specific include file.  */
#define HOST_INCLUDE "hosts/be/be.h"

/* Define if you have _crypt.  */
/* #undef HAVE__CRYPT */

/* Define if you have bzero.  */
#define HAVE_BZERO 1

/* Define if you have crypt.  */
/* #undef HAVE_CRYPT */

/* Define if you have fchmod.  */
/* #undef HAVE_FCHMOD */

/* Define if you have fcntl.  */
#define HAVE_FCNTL 1

/* Define if you have getcwd.  */
#define HAVE_GETCWD 1

/* Define if you have getdomainname.  */
/* #undef HAVE_GETDOMAINNAME */

/* Define if you have getrusage.  */
/* #define HAVE_GETRUSAGE */

/* Define if you have gettimeofday.  */
#define HAVE_GETTIMEOFDAY 1

/* Define if you have memcpy.  */
#define HAVE_MEMCPY 1

/* Define if you have memmem.  */
/* #undef HAVE_MEMMEM */

/* Define if you have memmove.  */
#define HAVE_MEMMOVE 1

/* Define if you have memset.  */
#define HAVE_MEMSET 1

/* Define if you have strchr.  */
#define HAVE_STRCHR 1

/* Define if you have strcspn.  */
#define HAVE_STRCSPN 1

/* Define if you have strrchr.  */
#define HAVE_STRRCHR 1

/* Define if you have sysconf.  */
#define HAVE_SYSCONF 1

/* Define if you have wait3.  */
#define HAVE_WAIT3 1

/* Define if you have waitpid.  */
#define HAVE_WAITPID 1

/* Define if you have the <bstring.h> header file.  */
/* #undef HAVE_BSTRING_H */

/* Define if you have the <crypt.h> header file.  */
/* #undef HAVE_CRYPT_H */

/* Define if you have the <libc.h> header file.  */
/* #undef HAVE_LIBC_H */

/* Define if you have the <memory.h> header file.  */
#define HAVE_MEMORY_H 1

/* Define if you have the <netdb.h> header file.  */
#define HAVE_NETDB_H 1

/* Define if you have the <stdlib.h> header file.  */
#define HAVE_STDLIB_H 1

/* Define if you have the <string.h> header file.  */
#define HAVE_STRING_H 1

/* Define if you have the <sys/rusage.h> header file.  */
#define HAVE_SYS_RUSAGE_H 1

/* Define if you have the <sys/termios.h> header file.  */
#define HAVE_SYS_TERMIOS_H 1

/* Define if you have the <sys/time.h> header file.  */
#define HAVE_SYS_TIME_H 1

/* Define if you have the <unistd.h> header file.  */
#define HAVE_UNISTD_H 1

/* Define if you have the <values.h> header file.  */
/* #undef HAVE_VALUES_H */

/* Define if you have the crypt library (-lcrypt).  */
/* #undef HAVE_LIBCRYPT */

/* Define if you have the m library (-lm).  */
/* #undef HAVE_LIBM */

/* Define if you have the nsl library (-lnsl).  */
/* #undef HAVE_LIBNSL */

/* Define if you have the socket library (-lsocket).  */
/* #undef HAVE_LIBSOCKET */

/* Define if you have the ucb library (-lucb).  */
/* #undef HAVE_LIBUCB */

#endif /* MACHINE_H */
