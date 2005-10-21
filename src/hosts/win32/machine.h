/* machine.h.  Generated automatically by configure.  */
/* machine.h.in.  Generated automatically from configure.in by autoheader.  */

#ifndef MACHINE_H
#define MACHINE_H


/* Define if using alloca.c.  */
/* #undef C_ALLOCA */

/* Define to one of _getb67, GETB67, getb67 for Cray-2 and Cray-YMP systems.
   This function is required for alloca.c support on those systems.  */
/* #undef CRAY_STACKSEG_END */

/* Define if you have alloca, as a function or macro.  */
#define HAVE_ALLOCA 1

/* Define if you have <alloca.h> and it should be used (not on Ultrix).  */
/* #undef HAVE_ALLOCA_H */

/* Define as __inline if that's what the C compiler calls it.  */
/* #undef inline */

/* Define to `int' if <sys/types.h> doesn't define.  */
/* #undef pid_t */

/* Define as the return type of signal handlers (int or void).  */
#define RETSIGTYPE void

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

/* does the compiler provide inline functions? */
#define HAS_INLINE 1

/* Is the library function inet_ntoa() compatible with the compiler?
 * TODO: Is there any platform where this is not the case?
 */
#define INET_NTOA_OK 1

/* A mask that allows to extract an unsigned char from a signed */
#define CHARBIT_MASK 0xff

#define RENAME_HANDLES_DIRECTORIES 1

/* Does the system have a getrusage call?  */
#define HAVE_GETRUSAGE 1
/* If so, is it restricted to user and system time? */
/* #undef GETRUSAGE_RESTRICTED */
/* Is it available as a subfunction of syscall() ? */
/* #undef GETRUSAGE_VIA_SYSCALL */
/* Can ru_utime / ru_stime be accessed as a timeval with tv_sec and tv_usec ? */
#define RUSAGE_USEC 1

/* the atari strtol() used to consider characters '9' < c < 'A' to be numeric */
/* #undef STRTOL_BROKEN */

/* needs the first argument of strtol be declared as const ? */
#define STRTOL_CONST_CHARP 1

/* Define if you have bcopy, and it handles overlapping ranges correctly. */
#define OVERLAPPING_BCOPY 1

#define MALLOC_ALIGN 8

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
#define EXTERN_MALLOC_OVERHEAD 4

/* How to set a socket non-blocking */
/* #undef USE_IOCTL_FIONBIO */
#define USE_FCNTL_O_NDELAY 1
#define USE_FCNTL_FNDELAY 1

/* Can F_SETOWN be used on a socket? */
/* #undef USE_FCNTL_SETOWN */

/* Can SO_OOBINLINE be used on a socket? */
#define USE_OOBINLINE 1

/* Does the machine offer IPv6? */
#define HAS_IPV6 1

/* Does the machine offer mySQL? */
/* #undef HAS_MYSQL */

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
#define HAVE_BZERO 1

/* Define if you have the crypt function.  */
#define HAVE_CRYPT 1

/* Define if you have the fchmod function.  */
#define HAVE_FCHMOD 1

/* Define if you have the fcntl function.  */
#define HAVE_FCNTL 1

/* Define if you have the getcwd function.  */
#define HAVE_GETCWD 1

/* Define if you have the getdomainname function.  */
#define HAVE_GETDOMAINNAME 1

/* Define if you have the getrusage function.  */
#define HAVE_GETRUSAGE 1

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
/* ANDERS */
#define HAVE_WAIT3 1

/* Define if you have the waitpid function.  */
#define HAVE_WAITPID 1

/* Define if you have the <bstring.h> header file.  */
/* #undef HAVE_BSTRING_H */

/* Define if you have the <crypt.h> header file.  */
/* ANDERS */
#define HAVE_CRYPT_H 1

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
#define HAVE_SYS_TERMIOS_H 1

/* Define if you have the <sys/time.h> header file.  */
#define HAVE_SYS_TIME_H 1

/* Define if you have the <unistd.h> header file.  */
#define HAVE_UNISTD_H 1

/* Define if you have the <values.h> header file.  */
/* #undef HAVE_VALUES_H */

/* Define if you have the m library (-lm).  */
#define HAVE_LIBM 1

/* Define if you have the nsl library (-lnsl).  */
/* #undef HAVE_LIBNSL */

/* Define if you have the socket library (-lsocket).  */
/* #undef HAVE_LIBSOCKET */

#endif

