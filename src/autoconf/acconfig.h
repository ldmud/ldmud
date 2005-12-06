#ifndef MACHINE_H
#define MACHINE_H

@TOP@

/* Set in response to the signal handler return type, since not all
 * compilers understand direct definition comparisons
 */
#undef RETSIGTYPE_VOID

/* does the compiler provide inline functions? */
#undef HAS_INLINE

/* Is the library function inet_ntoa() compatible with the compiler?
 * TODO: Is there any platform where this is not the case?
 */
#undef INET_NTOA_OK

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

/* does the sys/types.h define the uint*_t types? */
#undef HAVE_INTTYPES

/* does the compiler know of a 'ssize_t' type? */
#undef HAVE_SSIZE_T

/* does the compiler know of a 'long long' type? */
#undef HAVE_LONG_LONG

/* does the compiler know of a 'bool' type? */
#undef HAVE_BOOL

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

/* Can F_SETOWN be used on a socket? */
#undef USE_FCNTL_SETOWN

/* Can SO_OOBINLINE be used on a socket? */
#undef USE_OOBINLINE

/* Does the machine offer IPv6? */
#undef HAS_IPV6

/* Does the machine offer iconv? */
#undef HAS_ICONV

/* Does the machine's iconv take a non-const 'char**' as first arg? */
#undef HAS_ICONV_NONCONST_IN

/* Does the machine offer PCRE? */
#undef HAS_PCRE

/* Does the machine offer mySQL? */
#undef HAS_MYSQL

/* Does the machine offer PostgreSQL? */
#undef HAS_PGSQL

/* Does the machine offer SQLite3? */
#undef HAS_SQLITE3

/* Does SQLite3 use pthreads? */
#undef SQLITE3_USES_PTHREADS

/* Does the machine offer GnuTLS? */
#undef HAS_GNUTLS
#undef HAS_GNUTLS_VERSION

/* Does the machine offer OpenSSL/SSL? */
#undef HAS_OPENSSL

/* define the erq include file. */
#undef ERQ_INCLUDE

/* Does the machine offer pthreads? */
#undef HAS_PTHREADS

/* Does the machine offer pthread_atfork()? */
#undef HAS_PTHREAD_ATFORK

/* define the host-specific include file */
#undef HOST_INCLUDE

@BOTTOM@

#endif
