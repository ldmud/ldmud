/* machine.h for BeOS.
 *
 * Handcrafted to support cross compilation.
 */

#ifndef MACHINE_H
#define MACHINE_H

#include <BeBuild.h>

/* Define if using alloca.c.  */
/* #undef C_ALLOCA */

/* Define to one of _getb67, GETB67, getb67 for Cray-2 and Cray-YMP systems.
   This function is required for alloca.c support on those systems.  */
/* #undef CRAY_STACKSEG_END */

/* Define if you have alloca, as a function or macro.  */
#define HAVE_ALLOCA 1

/* Define if you have <alloca.h> and it should be used (not on Ultrix).  */
#define HAVE_ALLOCA_H 1

/* Define as __inline if that's what the C compiler calls it.  */
/* #undef inline */

/* Define to `int' if <sys/types.h> doesn't define.  */
/* #undef pid_t */

/* Define as the return type of signal handlers (int or void).  */
#define RETSIGTYPE void

/* Set in response to the signal handler return type, since not all
 * compilers understand direct definition comparisons
 */
#define RETSIGTYPE_VOID 1

/* Define to `unsigned' if <sys/types.h> doesn't define.  */
/* #undef size_t */

/* If using the C implementation of alloca, define if you know the
   direction of stack growth for your system; otherwise it will be
   automatically deduced at run-time.
 STACK_DIRECTION > 0 => grows toward higher addresses
 STACK_DIRECTION < 0 => grows toward lower addresses
 STACK_DIRECTION = 0 => direction of growth unknown
 */
/* #undef STACK_DIRECTION */

/* Define if you have the ANSI C header files.  */
#define STDC_HEADERS 1

/* does the compiler provide inline functions?
 * With Metrowerks, WARN_ALL implies '-ansi strict' which makes
 * inlines illegal.
 */

#if !defined(__MWERKS__) || !defined(WARN_ALL)
#    define HAS_INLINE 1
#endif

/* A mask that allows to extract an unsigned char from a signed */
#define CHARBIT_MASK 0xff

#define RENAME_HANDLES_DIRECTORIES 1

#if B_BEOS_VERSION > B_BEOS_VERSION_4

/* Does the system have a getrusage call?  */
#    define HAVE_GETRUSAGE 1
/* If so, is it restricted to user and system time? */
#    define GETRUSAGE_RESTRICTED 1
/* Is it available as a subfunction of syscall() ? */
/* #undef GETRUSAGE_VIA_SYSCALL */

#endif /* BeOS version >= 4.5 */

/* Can ru_utime / ru_stime be accessed as a timeval with tv_sec and tv_usec ? */
#if defined(__MWERKS__) || defined(B_BEOS_VERSION_5)
#    define RUSAGE_USEC 1
#endif

/* the atari strtol() used to consider characters '9' < c < 'A' to be numeric */
/* #undef STRTOL_BROKEN */

/* needs the first argument of strtol be declared as const ? */
#ifndef __MWERKS__
#    undef STRTOL_CONST_CHARP
#endif

/* Define if you have bcopy, and it handles overlapping ranges correctly. */
#ifndef __MWERKS__
#    define OVERLAPPING_BCOPY
#endif

#ifdef __INTEL__
#    define MALLOC_ALIGN 4
#else
#    define MALLOC_ALIGN 8
#endif

/* does the compiler know of a 'ssize_t' type? */
#define HAVE_SSIZE_T 1

/* does the compiler know of a 'long long' type? */
#define HAVE_LONG_LONG 1

/* does the compiler know of a 'bool' type? */
/* #undef HAVE_BOOL */

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
#define EXTERN_MALLOC_OVERHEAD 0

/* How to set a socket non-blocking */
/* #undef USE_IOCTL_FIONBIO */
/* #undef USE_FCNTL_O_NDELAY */
/* #undef USE_FCNTL_FNDELAY */

/* Can F_SETOWN be used on a socket? */
/* TODO: DOes it? */
#define USE_FCNTL_SETOWN 1

/* Can SO_OOBINLINE be used on a socket? */
/* TODO: Can it? */
#define USE_OOBINLINE 1

/* Does the machine offer IPv6? */
/* #undef HAS_IPV6 */

/* Does the machine offer mySQL? */
/* #undef HAS_MYSQL */

/* Does the machine offer pthread library? */
/* #undef HAS_PTHREADS */

/* Does the machine offer pcre library? */
/* #undef HAS_PCRE */

/* define the erq include file. */
#define ERQ_INCLUDE "util/erq/erq.h"

/* define the host-specific include file */
/* #undef HOST_INCLUDE */

/* The number of bytes in a char *.  */
#define SIZEOF_CHAR_P 4

/* The number of bytes in a int.  */
#define SIZEOF_INT 4

/* The number of bytes in a long.  */
#define SIZEOF_LONG 4

/* The number of bytes in a long long.  */
#define SIZEOF_LONG_LONG 8

/* The number of bytes in a short.  */
#define SIZEOF_SHORT 2

/* Define if you have the _crypt function.  */
/* #undef HAVE__CRYPT */

/* Define if you have the bzero function.  */
/* #undef HAVE_BZERO */

/* Define if you have the crypt function.  */
#define HAVE_CRYPT 1

/* Define if you have the fchmod function.  */
/* #undef HAVE_FCHMOD */

/* Define if you have the fcntl function.  */
#define HAVE_FCNTL 1

/* Define if you have the getcwd function.  */
#define HAVE_GETCWD 1

/* Define if you have the getdomainname function.  */
/* #undef HAVE_GETDOMAINNAME */

/* Define if you have the gettimeofday function.  */
#define HAVE_GETTIMEOFDAY 1

/* Define if you have the memcpy function.  */
#define HAVE_MEMCPY 1

/* Define if you have the memmem function.  */
/* #undef HAVE_MEMMEM */

/* Define if you have the memmove function.  */
#define HAVE_MEMMOVE 1

/* Define if you have the memset function.  */
#define HAVE_MEMSET 1

/* Define if you have the strchr function.  */
#define HAVE_STRCHR 1

/* Define if you have the strcspn function.  */
#define HAVE_STRCSPN 1

/* Define if you have the strdup function.  */
#define HAVE_STRDUP 1

/* Define if you have the strrchr function.  */
#define HAVE_STRRCHR 1

/* Define if you have the sysconf function.  */
#define HAVE_SYSCONF 1

/* Define if you have the wait3 function.  */
/* #undef HAVE_WAIT3 */

/* Define if you have the waitpid function.  */
#define HAVE_WAITPID 1

/* Define if you have the <bstring.h> header file.  */
/* #undef HAVE_BSTRING_H */

/* Define if you have the <crypt.h> header file.  */
/* #undef HAVE_CRYPT_H */

/* Define if you have the <dirent.h> header file.  */
#define HAVE_DIRENT_H 1

/* Define if you have the <libc.h> header file.  */
/* #undef HAVE_LIBC_H */

/* Define if you have the <limits.h> header file.  */
#define HAVE_LIMITS_H 1

/* Define if you have the <memory.h> header file.  */
#define HAVE_MEMORY_H 1

/* Define if you have the <ndir.h> header file.  */
/* #undef HAVE_NDIR_H */

/* Define if you have the <netdb.h> header file.  */
#define HAVE_NETDB_H 1

/* Define if you have the <stdlib.h> header file.  */
#define HAVE_STDLIB_H 1

/* Define if you have the <string.h> header file.  */
#define HAVE_STRING_H 1

/* Define if you have the <sys/dir.h> header file.  */
/* #undef HAVE_SYS_DIR_H */

/* Define if you have the <sys/ndir.h> header file.  */
/* #undef HAVE_SYS_NDIR_H */

/* Define if you have the <sys/param.h> header file.  */
#define HAVE_SYS_PARAM_H 1

/* Define if you have the <sys/rusage.h> header file.  */
/* #undef HAVE_SYS_RUSAGE_H */

/* Define if you have the <sys/termios.h> header file.  */
/* #undef HAVE_SYS_TERMIOS_H */

/* Define if you have the <sys/time.h> header file.  */
#define HAVE_SYS_TIME_H 1

/* Define if you have the <unistd.h> header file.  */
#define HAVE_UNISTD_H 1

/* Define if you have the <values.h> header file.  */
#if !defined(__MWERKS__) && !defined(B_BEOS_VERSION_5)
#    define HAVE_VALUES_H 1
#endif

/* Define if you have the m library (-lm).  */
/* #undef HAVE_LIBM */

/* Define if you have the nsl library (-lnsl).  */
/* #undef HAVE_LIBNSL */

/* Define if you have the socket library (-lsocket).  */
/* #undef HAVE_LIBSOCKET */

#endif
