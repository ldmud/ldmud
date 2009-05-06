#ifndef MY_RUSAGE_H__
#define MY_RUSAGE_H__

/*------------------------------------------------------------------
 * Portable rusage related definitions and prototypes.
 *
 * Use this file instead of <rusage.h> and others.
 *------------------------------------------------------------------
 */

#include "driver.h"

#ifdef GETRUSAGE_VIA_SYSCALL  /* hpux */
#    include <sys/syscall.h>
#    define getrusage(a, b)  syscall(SYS_GETRUSAGE, a, b)
#endif

#if defined(HAVE_GETRUSAGE)
#    if defined(RUSAGE_USEC) || defined(__hpux__)
#        define RUSAGE_TIME(t) ( (t).tv_sec * 1000 + (t).tv_usec / 1000 )
#    else
#        if defined(solaris)
             /* the header files are POSIX, but the function is BSD :-( */
#            define RUSAGE_TIME(t) ( (t).tv_sec * 1000 + (t).tv_nsec / 1000 )
#        else
#            define RUSAGE_TIME(t) ( (t).tv_sec * 1000 + (t).tv_nsec / 1000000 )
#        endif /* ! solaris */
#    endif /* RUSAGE_USEC */

#    ifdef HAVE_SYS_TIME_H
#        include <sys/time.h> /* Needed on some systems for sys/resource.h */
#    endif

#    ifdef HAVE_SYS_RUSAGE_H  /* solaris */
#        include <sys/rusage.h>
#    endif /* HAVE_SYS_RUSAGE */
#    include <sys/resource.h>
#    ifdef sun
         extern int getpagesize();
#    endif
#    if defined(sun)
         extern int getrusage (int, struct rusage *);
#    endif

#else /* !HAVE_GETRUSAGE */

#    undef GETRUSAGE_RESTRICTED
#    define GETRUSAGE_RESTRICTED
#    define RUSAGE_SELF     0

#    if defined(POSIX) && (HAVE_SYSCONF)
     /* there is actually a system that pretends to be POSIX, prototypes sysconf,
      * but does not have it.
      */
#        define TIMES_FREQ sysconf(_SC_CLK_TCK)

#    else /* !POSIX */

#        include <sys/time.h>
#        include <sys/times.h>
#        include <time.h>

#        define TIMES_FREQ CLK_TCK
#        ifndef CLK_TCK
#            define CLK_TCK CLOCKS_PER_SEC
#            ifndef CLOCKS_PER_SEC
#                define CLOCKS_PER_SEC 60
#            endif /* !CLOCKS_PER_SEC */
#        endif /* !CLK_TCK */

#    endif /* !POSIX */

#    define RUSAGE_TIME(t) ( (t) * 1000 / TIMES_FREQ )

struct rusage {
    long ru_utime, ru_stime;
};

extern int getrusage(int, struct rusage*);
  /* Implemented in port.c */

#endif /* HAVE_GETRUSAGE */

#ifndef RUSAGE_SELF
#    define RUSAGE_SELF 0
#endif

#endif /* MY_RUSAGE_H__ */
