#ifndef PORT_H__
#define PORT_H__

/*------------------------------------------------------------------
 * Global Portability Include File.
 *
 * The goal of port.c/h is to wrap up the actual system/compiler
 * combo in a nice uniform shell to keep the main sources clean.
 *
 * This include provides various macros for system dependent
 * features, includes the most common system includes and, more
 * important, the host specific includes. port.h knows about the
 * architectures the driver has been ported to and includes
 * their files automatically.
 *
 * Not everything system dependent is defined here, some stuff
 * are kept in separate my-foo.h files.
 *
 * TODO: Distinguish 'DEBUG' (sanity checks, no inlines), 'NORMAL'
 * TODO:: (sanity checks, inlines), and 'BUGFREE' (no sanity checks, inlines).
 * TODO: Make a NOTREACHED(code) macro, which allows the insertion
 * TODO:: of <code> for compilers without reachability-detection to
 * TODO:: aid the optimizer.
 * TODO: Make CHARBITMASK a define in here depending on CHAR_BIT
 * TODO:: so it can vanish from configure.
 *------------------------------------------------------------------
 */

#if !defined(DRIVER_H__)
#error You must include driver.h instead of port.h
Thats it.
#endif

#if !defined(__STDC__)
/* On useful systems, the configure script catches this, too.
 */
#error You need a Standard-C compiler.
Thats it.
#endif

#include "machine.h"

/*------------------------------------------------------------------
 * Define some system macros.
 */

#if defined(M_UNIX) || defined(__linux__) || defined(solaris) || \
    defined(_POSIX_VERSION)
#    ifndef POSIX
#        define POSIX
#    endif
#endif

#if defined(__GNUC__) && !defined(_GNU_SOURCE)
#    define _GNU_SOURCE
#endif

#if defined(__CYGWIN32__) || defined(__CYGWIN__)
#    define CYGWIN
      /* #define __CYGWIN__ messes up the Cygwin headers */
#endif

/*------------------------------------------------------------------
 * Standard system headers.
 */

#include <sys/types.h>
#include <limits.h>

#include <errno.h>
#ifdef __SASC
#    include <sys/errno.h>
extern int errno;
#endif

#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#ifdef HAVE_LIBC_H
#    include <libc.h>
#endif
#if !defined(STDC_HEADERS) && defined(HAVE_MEMORY_H)
#    include <memory.h>
#endif

#ifdef HAVE_STDLIB_H
#    include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#    include <string.h>
#else
#    include <strings.h>
#endif
#ifdef HAVE_BSTRING_H
#    include <bstring.h>
#endif

#include <math.h>
#include <float.h>

#ifdef HAVE_SYS_PARAM_H
#    include <sys/param.h>
#endif

/*------------------------------------------------------------------
 * Define some macros:
 *   CHAR_BIT     number of bits in a char, if not defined already.
 *   TODO: Lookup what ISO-C says about this.
 *   MAXPATHLEN   max length of a pathname, if not defined already.
 *   NORETURN     attribute for non-returning functions.
 *   UNUSED       attribute for unused functions and variables.
 *   MALLOC       attribute for malloc()-like functions
 *   FORMATDEBUG  attribute for printf-style prototypes.
 *   VARPROT      for portable printf-style prototype definitions.
 *   INLINE       attribute for inline functions, depending on
 *                HAS_INLINE (autoconf) and NO_INLINES (Makefile).
 *   EXTRACT_UCHAR(), EXTRACT_SCHAR():
 *                extract a character from a memory location.
 *   USE_IPV6     check the definition from config.h in relation
 *                to HAS_IPV6 from autoconf.
 *   USE_MYSQL    check the definition from config.h in relation
 *                to HAS_MYSQL from autoconf.
 *   MSDOS_FS     if the filesystem uses MS-DOS semantics
 *                (i.e. backslashes as directory separators)
 */

#ifndef CHAR_BIT
#    define CHAR_BIT 8
#endif

#ifndef MAXPATHLEN
#    ifdef PATH_MAX
#        define MAXPATHLEN PATH_MAX
#    else
#        define MAXPATHLEN 2048
#    endif
#endif

/* some handy gcc attributes, but define everything to nothing if any other
 * compiler ist used. Additional attributes maybe used in the driver are:
   always_inline, const, deprecated, format_arg, nonnull, pure, returns_twice,
   unused, warn_unused_result.
 */
#if defined(__GNUC__)
#    define MALLOC   __attribute__((malloc))
#    define NORETURN __attribute__((noreturn))
#    define UNUSED   __attribute__((unused))
#    define FORMATDEBUG(f,a,b) __attribute__((format (f,a,b)))
#elif defined(__MWERKS__)
#    define  __attribute__(x)  /*NOTHING*/
#    define NORETURN
#    define UNUSED
#    define MALLOC
#    define FORMATDEBUG(f,a,b)
#else
#    define  __attribute__(x)  /*NOTHING*/
#    define NORETURN
#    define UNUSED
#    define MALLOC
#    define FORMATDEBUG(f,a,b)
#endif

#define VARPROT(proto,like,form,var) proto FORMATDEBUG(like,form,var)


// TODO: autoconf defines inline to some suitable keyword if the compiler does
// not understand inline itself. Just use inline in code?
#if defined(HAS_INLINE) && !defined(NO_INLINES)
#    define INLINE inline
     /* configure made sure that 'inline' expands to the proper attribute */
#else
#    define INLINE
#    undef NO_INLINES
#    define NO_INLINES
     /* just so we can recognize if we have inlines or not */
#endif

#define EXTRACT_UCHAR(p) (*(unsigned char *)(p))
#define EXTRACT_SCHAR(p) (*(signed char *)(p))

#if !defined(HAS_IPV6) && defined(USE_IPV6)
#    undef USE_IPV6
#endif

#if !defined(HAS_MYSQL) && defined(USE_MYSQL)
#    undef USE_MYSQL
#endif

#if defined(CYGWIN)
#define MSDOS_FS
#endif

/*------------------------------------------------------------------
 * Test for C99-compatible data types
 * TODO: check if we can remove these checks once (if?) we require a C99
 * compliant build environment.
 */
#if defined(HAVE_INTTYPES_H)
    /* C99 compliant inttypes.h. */
#   include <inttypes.h>
#endif
#if defined(HAVE_STDINT_H)
    /* C99 compliant stdint.h available
     * Usually it gets included also in inttypes.h, but it doesn't hurt to
     * include it specifically. */
#   include <stdint.h>
#endif

/* If stdint.h or inttypes.h don't have int8_t, int16_t, int32_t, int64_t,
 * uint8_t, uint16_t, uint32_t, uint64_t, intmax_t, uintmax_t, intptr_t,
 * or uintmax_t, autoconf will have them defined to a suitable type now.
 * If Autoconf did not find suitable types, there is probably no sense in
 * searching them ourself.
 */
#if !defined(HAVE_INTPTR_T) && !defined(intptr_t)
    /* If there is no intptr_t in stdint.h and autoconf did not find a
     * suitable type, we're out of luck (because in this case we are unlikely
     * to find one). */
#   error Autoconf did not find an integer type with same size as a pointer
    Thats it.
#endif
#if !defined(INT32_MAX) && !defined(int32_t)
#   error Autoconf did not find an integer type with exactly 32 bits.
    Thats it.
#endif

/* If mode_t, off_t, pid_t, size_t or ssize_t are not defined by the standard
 * headers on this system, autoconf will have them defined as well.
 */


/*------------------------------------------------------------------
 * Limits for less-standard integral types:
 *
 *   LONGLONG_MIN, LONGLONG_MAX, ULONGLONG_MAX
 * TODO: Add SIZEOF_SIZET to configure, and SIZET_limits here.
 * TODO:: Then use SIZET_limits in smalloc::smalloc().
 */
#if defined(HAVE_LONG_LONG) && !defined(LONGLONG_MIN)
#    if defined(LONG_LONG_MAX)
#        define LONGLONG_MIN   LONG_LONG_MIN
#        define LONGLONG_MAX   LONG_LONG_MAX
#        define ULONGLONG_MAX  ULONG_LONG_MAX
#    elif SIZEOF_LONG_LONG == 8
#        define LONGLONG_MIN   (-9223372036854775807LL - 1)
#        define LONGLONG_MAX   (9223372036854775807LL)
#        define ULONGLONG_MAX  (0xffffffffffffffffULL)
#    elif SIZEOF_LONG_LONG == SIZEOF_LONG
#        define LONGLONG_MIN   LONG_MIN
#        define LONGLONG_MAX   LONG_MAX
#        define ULONGLONG_MAX  ULONG_MAX
#    elif SIZEOF_LONG_LONG == SIZEOF_INT
#        define LONGLONG_MIN   INT_MIN
#        define LONGLONG_MAX   INT_MAX
#        define ULONGLONG_MAX  UINT_MAX
#    endif
#endif

/*------------------------------------------------------------------
 * Integral types:
 *   Bool, SBool, CBool: boolean type, sized as _Bool or int, short, char.
 *   p_int  : an integer that has the same size as a pointer
 *   ph_int : an integer that has half the size of a pointer
 *   mp_int : an integer that has at least the size of a pointer
 *   int32  : an integer with 32 bits
 *   PTRTYPE: a type to use with constant pointer arithmetic.
 * The unsigned versions use 'uint' instead of 'int'.
 * Additionally to the type themselves we define format specifiers for
 * printing our types with sprintf() (PRN*) and scanning them with sscanf()
 * (SCN*).
 * Changes here must be reflected in my-limits.h .
 * Additional integral types from stdint.h/inttypes.h are available (s. above)
 * TODO: check, if it is feasible to use the C99 data types instead of our own
 * TODO::names in the future.
 */

/* p_int : an integer that has the same size as a pointer */
#if defined(HAVE_INTPTR_T) && SIZEOF_INT != SIZEOF_CHAR_P
    /* just use intptr_t 
     * BUT! glibc on ILP32 platforms unfortunately defines intptr_t as int,
     * not as long. While this doesn't change things in principle, because they
     * have equal size, gcc will output a bunch of warnings on
     * sprintf("%ld",p_int) because %ld is the wrong format specifier for int.
     * Therefore this little hack, which uses intptr_t for the time being
     * only if sizeof(int) != sizeof(char*) (because then intptr_t cannot be
     * an int). As it is not guaranteed that p_int will always be a long or
     * have the size of a long, this %ld for outputting p_int are anyway a
     * problem.
     * TODO: As soon, as all the %ld for p_int are replaced, this hack should
     * TODO::be removed!
     */
    typedef intptr_t                 p_int;
    typedef uintptr_t                p_uint;
#   define PINT_MIN  INTPTR_MIN
#   define PINT_MAX  INTPTR_MAX
#   define PUINT_MAX UINTPTR_MAX
#   define SIZEOF_PINT SIZEOF_INTPTR_T
#   define PRIdPINT  PRIdPTR
#   define PRIuPINT  PRIuPTR
#   define PRIxPINT  PRIxPTR
#   define SCNdPINT SCNdPTR
#   define SCNuPINT SCNuPTR
#   define SCNxPINT SCNxPTR
#   ifdef __PRIPTR_PREFIX
#       define PRI_PINT_PREFIX __PRIPTR_PREFIX
#   else
    /* ugly - it is a pity that the format specifiers are standardized but not
     * the length modifier. But we need one for sprintf.c. *sigh* */
#       if SIZEOF_INTPTR_T == SIZEOF_LONG
#           define PRI_PINT_PREFIX "l"
#       elif SIZEOF_INTPTR_T == SIZEOF_INT
#           define PRI_PINT_PREFIX
#       elif HAVE_LONG_LONG && SIZEOF_INTPTR_T == SIZEOF_LONG_LONG
#           define PRI_PINT_PREFIX "ll"
#       else
#           error Could not find a length modifier for intptr_t.
            Thats it.
#       endif
#   endif
#else
  /* autoconf will have some type defined to intptr_t, but it won't define the
   * limits and we won't have SIZEOF_INTPTR_T available. Therefore we have to
   * search ourselves the old way. 
   * TODO: remove once C99 support is required */
#   define SIZEOF_PINT SIZEOF_CHAR_P
#   if SIZEOF_LONG == SIZEOF_CHAR_P
        typedef long                p_int;
        typedef unsigned long       p_uint;
#       define PINT_MIN  LONG_MIN
#       define PINT_MAX  LONG_MAX
#       define PUINT_MAX ULONG_MAX
#       define PRI_PINT_PREFIX "l"
#   elif SIZEOF_INT == SIZEOF_CHAR_P
        typedef int                 p_int;
        typedef unsigned int        p_uint;
#       define PINT_MIN  INT_MIN
#       define PINT_MAX  INT_MAX
#       define PUINT_MAX UINT_MAX
#       define PRI_PINT_PREFIX
#   elif defined(HAVE_LONG_LONG) && SIZEOF_LONG_LONG == SIZEOF_CHAR_P
        typedef long long           p_int;
        typedef unsigned long long  p_uint;
#       define PINT_MIN  LONGLONG_MIN
#       define PINT_MAX  LONGLONG_MAX
#       define PUINT_MAX ULONGLONG_MAX
#       define PRI_PINT_PREFIX "ll"
#   else
        /* nearly impossible (s. intptr_t check above, but better safe than
         * sorry. */
#       error cannot find an integer type with same size as a pointer
        Thats it.
#   endif
#   define PRIdPINT  PRI_PINT_PREFIX "d"
#   define PRIuPINT  PRI_PINT_PREFIX "u"
#   define PRIxPINT  PRI_PINT_PREFIX "x"
#   define SCNdPINT  PRI_PINT_PREFIX "d"
#   define SCNuPINT  PRI_PINT_PREFIX "u"
#   define SCNxPINT  PRI_PINT_PREFIX "x"
#endif // HAVE_INTPTR_T


/* ph_int : an integer that has half the size of a pointer.
 * Unfortuntately C99 has nothing like this and therefore we have to find our
 * own.
 */
#if SIZEOF_CHAR_P == SIZEOF_INT * 2
     typedef int                 ph_int;
     typedef unsigned int        ph_uint;
#    define PHINT_MIN  INT_MIN
#    define PHINT_MAX  INT_MAX
#    define PHUINT_MAX UINT_MAX
#    define PRI_PHINT_PREFIX
#elif SIZEOF_CHAR_P == SIZEOF_SHORT * 2
     typedef short               ph_int;
     typedef unsigned short      ph_uint;
#    define PHINT_MIN  SHRT_MIN
#    define PHINT_MAX  SHRT_MAX
#    define PHUINT_MAX USHRT_MAX
#    define PRI_PHINT_PREFIX "h"
#elif SIZEOF_CHAR_P == SIZEOF_LONG * 2
     typedef long                 ph_int;
     typedef unsigned long        ph_uint;
#    define PHINT_MIN  LONG_MIN
#    define PHINT_MAX  LONG_MAX
#    define PHUINT_MAX ULONG_MAX
#    define PRI_PHINT_PREFIX "l"
#else
#    error Cannot find an integer of half the size of a pointer.
     Thats it.
#endif
#define PRIdPHINT  PRI_PHINT_PREFIX "d"
#define PRIuPHINT  PRI_PHINT_PREFIX "u"
#define PRIxPHINT  PRI_PHINT_PREFIX "x"
#define SCNdPHINT  PRI_PHINT_PREFIX "d"
#define SCNuPHINT  PRI_PHINT_PREFIX "u"
#define SCNxPHINT  PRI_PHINT_PREFIX "x"

/* mp_int : an integer that has at least the size of a pointer 
 * TODO: use intmax_t intstead once the driver does not assume mp_ints to be
 * TODO::longs? */
typedef p_int        mp_int;
typedef p_uint       mp_uint;
#define MPINT_MIN  PINT_MIN
#define MPINT_MAX  PINT_MAX
#define MPUINT_MAX PUINT_MAX
#define PRIdMPINT  PRIdPINT
#define PRIuMPINT  PRIuPINT
#define PRIxMPINT  PRIxPINT
#define SCNdMPINT  PRIdPINT
#define SCNuMPINT  PRIuPINT
#define SCNxMPINT  PRIxPINT


/* int32 : an integer with 32 bits.
   TODO: just use (u)int32_t instead of (u)int32. */
typedef int32_t   int32;
typedef uint32_t  uint32;
#ifndef PRId32
   /* unfortunately there seems to be no PRId32 from inttypes.h or alike.
      TODO: Once we require C99, we can get rid of the this stuff */
#  if !defined(CHAR_BIT) || CHAR_BIT != 8
#    error CHAR_BIT does not exist or is != 8 which is currently not supported!
     Thats it.
#  endif
   /* now sizeof(int32) has to be sizeof(char) * 4 == 4. */
#  if SIZEOF_INT == 4
#    define __PRId32PREFIX
#  elif SIZEOF_LONG == 4
#    define __PRId32PREFIX "l"
#  elif SIZEOF_SHORT == 4
#    define __PRIx32PREFIX "h"
#  else
#    error Could not find length modifier for (u)int32
     Thats it.
#  endif
#  define PRId32 __PRId32PREFIX "d"
#  define PRIu32 __PRId32PREFIX "u"
#  define PRIx32 __PRId32PREFIX "x"
#endif /* PRId32 */


/* type to use with constant pointer arithmetic. */
#define PTRTYPE char *


/* Boolean datatype and values */
#ifdef HAVE_STDBOOL_H
#   include <stdbool.h>
#else
#   ifndef HAVE__BOOL
        /* _Bool is not available - typedef our own with int. 
         * naming it 'bool' clashes on some machines... */
        typedef int    _Bool;
#   endif
    /* define true and false as stdbool.h does. */
#   define false 0
#   define true 1
#   define __bool_true_false_are_defined 1
#endif // HAVE_STDBOOL_H
/* _Bool looks strange and we anyway used Bool until now. */
typedef _Bool Bool;
/* TODO: check if these two can be merged with Bool */
typedef short SBool;
typedef char  CBool;

#define MY_TRUE  (true)
#define MY_FALSE (false)


/* TODO: This should go into my-malloc.h? */
#ifdef FREE_RETURNS_VOID
#    define FREE_RETURN_TYPE void
#    define FREE_RETURN return;
#else
#    define FREE_RETURN_TYPE int
#    define FREE_RETURN return 1;
#endif


/*------------------------------------------------------------------
 * Provide functions, types and defines missing from the system headers.
 */

#ifndef HAVE_MEMCPY
/* The following 'implementation' is suitable for throwing away a value,
   but not to using it; the cast to return int is likely to show a warning
   if the value is used by accident.
   If you need the return value, use a proper function:#
     char *memcpy(char *memcpy(char *b, char *a, int s) { bcopy(a,b,s); return b; }
 */
#    define memcpy(b, a, s) (*((int (*)())(&bcopy)))(a, b, s)
#endif

#ifndef HAVE_BZERO
#    define bzero(str, i) memset(str, '\0', i)
#endif

#ifndef HAVE_STRCHR
#    define strchr index
#endif

#ifndef HAVE_STRRCHR
#    define strrchr rindex
#endif

/* O_BINARY and O_TEXT don't exist on POSIX conforming platforms, but on Cygwin.
 * On Cygwin it may be needed if the volume with the mudlib is mounted in 'textmode'
 * to open files without any line break conversions (*sigh*).
 * We define them here to 0 if they don't exist and use them in the driver
 * code.
 */
#ifndef O_BINARY
#    define O_BINARY 0
#endif
#ifndef O_TEXT
#    define O_TEXT 0
#endif

#ifdef HAVE_MEMMOVE
    /* apple unix does prototype memmove, but there is no such function in the
     * library. Moreover, bcopy() won't handle overlapping right there.
     */
#    define move_memory(dest, source, size) memmove(dest, source, size)
#endif
#if !defined(HAVE_MEMMOVE) && defined(OVERLAPPING_BCOPY)
#    define move_memory(dest, source, size) bcopy(source, dest, size)
#endif

#define ixstat   stat
#define ixopen   open
#define ixopen3  open


/*------------------------------------------------------------------
 * Posixish defaults for host-specific defines:
 *   HOST_DEPENDENT_INIT: a block of code to be executed in main().
 *   ALARM_HANDLER_PROT:  signal handler prototype.
 *   ALARM_HANDLER     :  implementation of a signal handler.
 *   ALARM_HANDLER_FIRST_CALL: call a signal handler directly.
 */

#define HOST_DEPENDENT_INIT

#define ALARM_HANDLER_PROT(name)  extern RETSIGTYPE name(int)

/* #undef ALARM_HANDLER(name, body): use default in backend.c */

#define ALARM_HANDLER_FIRST_CALL(name)  name(0)

/*------------------------------------------------------------------
 * At last, the functions provided in port.c
 */

extern char current_time_stamp[];
extern mp_int get_current_time(void);
extern char * time_fstring(mp_int t, const char* str, Bool localized) 
                           FORMATDEBUG(strftime,2,0);
extern char * utime_string(mp_int, mp_int);
extern char * time_stamp(void);
extern char *xmemmem(const char *, size_t, const char *, size_t);

#ifndef HAVE_STRCSPN
extern size_t strcspn(const char *s, const char *set);
#endif

#ifndef HAVE_STRDUP
extern char *strdup(const char *);
#endif


#if !defined(HAVE_MEMMOVE) && !defined(OVERLAPPING_BCOPY)
extern void move_memory(char *, char *, size_t);
#endif

#ifdef HAVE_CRYPT_H
     /* solaris defines crypt() here */
#    include <crypt.h>
#endif

#if ((!defined(HAVE_CRYPT) && !defined(HAVE__CRYPT))) || \
    (defined(sgi) && !defined(_MODERN_C)) || defined(ultrix) \
    || defined(sun)
extern char *crypt(const char *, const char *);
#endif

#if !defined(HAVE_CRYPT) && defined(HAVE__CRYPT)
#    define crypt(pass, salt) _crypt(pass, salt)
#endif

#if defined(CYGWIN)
extern void init_rusage(void);
#else
#define init_rusage()
#endif

#endif /* PORT_H__ */
