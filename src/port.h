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

/*------------------------------------------------------------------
 * C99-compatible data types
 */
#include <inttypes.h>
/* Usually it gets included also in inttypes.h, but it doesn't hurt to
 * include it specifically. */
#include <stdint.h>

/* If stdint.h or inttypes.h don't have int8_t, int16_t, int32_t, int64_t,
 * uint8_t, uint16_t, uint32_t, uint64_t, intmax_t, uintmax_t, intptr_t,
 * or uintmax_t, autoconf will have them defined to a suitable type now.
 * If Autoconf did not find suitable types, there is probably no sense in
 * searching them ourself.
 */
#if !defined(HAVE_INTPTR_T) && !defined(intptr_t)
#   error This system does not have intptr_t from C99, which we require.
    Thats it.
#endif

/* If mode_t, off_t, pid_t, size_t or ssize_t are not defined by the standard
 * headers on this system, autoconf will have them defined.
 */


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
 */

/* p_int : an integer that has the same size as a pointer */
typedef intptr_t                 p_int;
typedef uintptr_t                p_uint;
#define PINT_MIN  INTPTR_MIN
#define PINT_MAX  INTPTR_MAX
#define PUINT_MAX UINTPTR_MAX
#define SIZEOF_PINT SIZEOF_INTPTR_T
#define PRIdPINT  PRIdPTR
#define PRIuPINT  PRIuPTR
#define PRIxPINT  PRIxPTR
#define SCNdPINT  SCNdPTR
#define SCNuPINT  SCNuPTR
#define SCNxPINT  SCNxPTR
#ifdef __PRIPTR_PREFIX
    // some libc define __PRIPTR_PREFIX empty. The empty string "" is a bloody
    // workaround for that.
#   define PRI_PINT_PREFIX ""__PRIPTR_PREFIX
#else
    /* ugly - it is a pity that the format specifiers are standardized but not
     * the length modifier. But we need one for sprintf.c. *sigh* */
#   if SIZEOF_INTPTR_T == SIZEOF_LONG
#       define PRI_PINT_PREFIX "l"
#   elif SIZEOF_INTPTR_T == SIZEOF_INT
#       define PRI_PINT_PREFIX ""
#   elif HAVE_LONG_LONG && SIZEOF_INTPTR_T == SIZEOF_LONG_LONG
#       define PRI_PINT_PREFIX "ll"
#   else
#       error Could not find a length modifier for intptr_t.
        Thats it.
#   endif
#endif // __PRIPTR_PREFIX


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


// We need a big integral type for the heavily used statistic counters.
typedef uint64_t statcounter_t;
#define PRIuSTATCOUNTER PRIu64
#define PRIxSTATCOUNTER PRIx64


/* type to use with constant pointer arithmetic. */
#define PTRTYPE char *


/* Boolean datatype and values */
#include <stdbool.h>
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

#ifdef HAVE_CRYPT_H
     /* solaris defines crypt() here */
#    include <crypt.h>
#endif

#if ((!defined(HAVE_CRYPT) && !defined(HAVE__CRYPT))) || \
    (defined(sgi) && !defined(_MODERN_C)) || defined(sun)
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
