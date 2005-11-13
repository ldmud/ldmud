#ifndef PORT_H
#define PORT_H

#include "machine.h"
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#if !defined(__STDC__) && !defined(volatile)
#define volatile
#endif

/* p_int : an integer that has the same size as a pointer */
#if SIZEOF_LONG == SIZEOF_P_INT
typedef long		p_int;
typedef unsigned long	p_uint;
#else
#if SIZEOF_INT == SIZEOF_P_INT
typedef int		p_int;
typedef unsigned int	p_uint;
#endif
#if SIZEOF_LONG < SIZEOF_P_INT
typedef long long		p_int;
typedef unsigned long long	p_uint;
#endif
#endif

/* ph_int : an integer that has half the size of a pointer */
#if SIZEOF_P_INT == SIZEOF_INT * 2
typedef int		ph_int;
typedef unsigned int	ph_uint;
#else
#if SIZEOF_P_INT == 4
/* short is assumed to be always 2 bytes. */
typedef short		ph_int;
typedef unsigned short	ph_uint;
#endif
#endif

/* mp_int : an integer that has at least the size of a pointer */
typedef p_int	mp_int;
typedef p_uint	mp_uint;

/* int32 : an integer with 32 bits */
#if SIZEOF_LONG == 4
typedef long		int32;
typedef unsigned long	uint32;
#else
#if SIZEOF_INT == 4
typedef int		int32;
typedef unsigned int	uint32;
#endif
#endif

#ifdef FREE_RETURNS_VOID
#define FREE_RETURN_TYPE void
#define FREE_RETURN return;
#else
#define FREE_RETURN_TYPE int
#define FREE_RETURN return 1;
#endif

#define EXTRACT_UCHAR(p) (*(unsigned char *)(p))
#ifdef __STDC__
#define EXTRACT_SCHAR(p) (*(signed char *)(p))
#else
#define EXTRACT_SCHAR(p) ((short)(*(p) << 8) >> 8)
#endif

#if defined(atarist) || defined(AMIGA) || defined(MSDOS)
#ifndef NO_IP_DEMON
#define NO_IP_DEMON
#endif
#endif

#if defined(M_UNIX) || defined(__linux__) || defined(solaris) || \
    defined(_POSIX_VERSION)
#ifndef POSIX
#define POSIX
#endif
#endif

#ifdef HAVE_GETRUSAGE
#if defined(RUSAGE_USEC)
#define RUSAGE_TIME(t) ( (t).tv_sec * 1000 + (t).tv_usec / 1000 )
#else
#if defined(solaris)
/* the header files are POSIX, but the function is BSD :-( */
#define RUSAGE_TIME(t) ( (t).tv_sec * 1000 + (t).tv_nsec / 1000 )
#else
#define RUSAGE_TIME(t) ( (t).tv_sec * 1000 + (t).tv_nsec / 1000000 )
#endif /* ! solaris */
#endif /* RUSAGE_USEC */
#else /* !HAVE_GETRUSAGE */

#undef GETRUSAGE_RESTRICTED
#define GETRUSAGE_RESTRICTED
#define RUSAGE_SELF     0

#ifndef MSDOS

#if defined(POSIX) && (HAVE_SYSCONF)
/* there is actually a system that pretends to be POSIX, prototypes sysconf,
 * but does not have it.
 */
#define TIMES_FREQ sysconf(_SC_CLK_TCK)

#else /* !POSIX */

#include <sys/time.h>
#include <sys/times.h>
#include <time.h>

#define TIMES_FREQ CLK_TCK
#ifndef CLK_TCK
#define CLK_TCK CLOCKS_PER_SEC
#ifndef CLOCKS_PER_SEC
#define CLOCKS_PER_SEC 60
#endif /* !CLOCKS_PER_SEC */
#endif /* !CLK_TCK */

#endif /* !POSIX */

#define RUSAGE_TIME(t) ( (t) * 1000 / TIMES_FREQ )

#else /* MSDOS */

#define RUSAGE_TIME(t) (t)

#endif /* MSDOS */

struct rusage {
    long ru_utime, ru_stime;
};

#endif /* HAVE_GETRUSAGE */

#ifndef HAVE_MEMCPY
/* The following 'implementation' is suitable for throwing away a value,
   but not to using it; the cast to return int is likely to show a warning
   if the value is used by accident.
 */
#define memcpy(b, a, s) (*((int (*)())(&bcopy)))(a, b, s)
#endif

#ifndef HAVE_BZERO
#define bzero(str, i) memset(str, '\0', i)
#endif

#if !defined(HAVE_CRYPT) && defined(HAVE__CRYPT)
#define crypt(pass, salt) _crypt(pass, salt)
#endif

#ifndef HAVE_STRCHR
#define strchr index
#endif
#ifndef HAVE_STRRCHR
#define strchr rindex
#endif

#define ixstat stat
#define ixopen ((int(*)PROT((char *, int)))open)
#define ixopen3 open

#define HOST_DEPENDENT_INIT

#define ALARM_HANDLER(name, body)				\
void name() {							\
    (void)signal(SIGALRM, (RETSIGTYPE(*)PROT((int)))name);	\
    {body}							\
}

#define ALARM_HANDLER_FIRST_CALL(name)	\
{					\
    void name();			\
    name();				\
}

#include HOST_INCLUDE

#if !defined(MSDOS) && !defined(__BEOS__)
#define O_BINARY 0
#define O_TEXT 0
#endif

#ifdef HAVE_MEMMOVE
#define move_memory(dest, source, size) memmove(dest, source, size)
#endif
#if !defined(HAVE_MEMMOVE) && defined(OVERLAPPING_BCOPY)
#define move_memory(dest, source, size) bcopy(source, dest, size)
#endif

#endif /* PORT_H */
