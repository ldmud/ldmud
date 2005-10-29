/* Is the library function inet_ntoa() compatible with the compiler ? */
#undef INET_NTOA_OK

/* The following values must be evaluable by the preprocessor */
#define SIZEOF_P_INT 4
#define SIZEOF_INT 4
#define SIZEOF_LONG 4

/* A mask that allows to extract an unsigned char from a signed */
#define CHARBIT_MASK 0xff

#undef RENAME_HANDLES_DIRECTORIES

/* Does the system have a getrusage call?  */
#undef HAVE_GETRUSAGE
/* If so, is it restricted to user and system time? */
#undef GETRUSAGE_RESTRICTED
/* Is it available as a subfunction of syscall() ? */
#undef GETRUSAGE_VIA_SYSCALL
/* Can ru_utime / ru_stime be accessed as a timeval with tv_sec and tv_usec ? */
#undef RUSAGE_USEC

/* the atari strtol() used to consider characters '9' < c < 'A' to be numeric */
#undef STRTOL_BROKEN

/* does the libc consider it normal to free a null pointer? */
#undef FREE_NULL_POINTER

/* needs the first argument of strtol be declared as const ? */
#undef STRTOL_CONST_CHARP

/* Define if you have bcopy, and it handles overlapping ranges correctly. */
#undef OVERLAPPING_BCOPY

#define MALLOC_ALIGN 4

/* does the compiler know of a 'long long' type? */
#undef HAVE_LONG_LONG

/* what kind of pointer is used by malloc() et al */
#define POINTER *
#undef FREE_RETURNS_VOID

/* can we define our own malloc() safely? */
#undef SBRK_OK

/* The following is needed for smalloc without SBRK_OK to use memory
 * efficiently. smalloc will malloc blocks that are a large power of
 * two, minus EXTERN_MALLOC_OVERHEAD. If you have no idea what number to
 * choose, compile & run util/overhead.c
 */
#define EXTERN_MALLOC_OVERHEAD 16

/* How to set a socket non-blocking */
#undef USE_IOCTL_FIONBIO
#undef USE_FCNTL_O_NDELAY
#undef USE_FCNTL_FNDELAY

#undef inline

/* Define what random number generator to use.
 * If no one is specified, a probably good one will be used.
 * possible choices: RANDOM DRAND48 RAND
 */
#undef RANDOM
#undef DRAND48
#undef RAND

/* A host specific include file.  */
#define HOST_INCLUDE "hosts/unix.h"
