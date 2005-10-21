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
 * their files automatically. This process can be bypassed by
 * defining an include file in the macro HOST_INCLUDE.
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

#if defined(sun) && !defined(solaris)
#    define SunOS4
#endif

#if defined(M_UNIX) || defined(__linux__) || defined(solaris) || \
    defined(_POSIX_VERSION)
#    ifndef POSIX
#        define POSIX
#    endif
#endif

#if defined(__GNUC__) && !defined(_GNU_SOURCE)
#    define _GNU_SOURCE
#endif

#if defined(__CYGWIN32__) && !defined(__CYGWIN__)
#    define __CYGWIN__
#endif

/*------------------------------------------------------------------
 * Standard system headers.
 */

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
#ifdef HAVE_VALUES_H
#    include <values.h>
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

#ifdef AMIGA
#    include "hosts/amiga/patchfloat.h"
#endif /* AMIGA */
#ifdef ATARI_TT
#    include <math-688.h>
#    define _MATH_H
#endif
#include <math.h>

#ifdef __BEOS__
     /* BeOS defines some standard non-standard types itself (like int32).
      * Since the defs will be used as part of the networking includes,
      * we force them in here globally and simply adapt the other
      * definitions to avoid clashes.
      */
#    include <SupportDefs.h>
#endif

#ifdef HAVE_SYS_PARAM_H
#    include <sys/param.h>
#endif

#include <sys/types.h>


/*------------------------------------------------------------------
 * Define some macros:
 *   CHAR_BIT     number of bits in a char, if not defined already.
 *   TODO: Lookup what ISO-C says about this.
 *   MAXPATHLEN   max length of a pathname, if not defined already.
 *   PROT(x)      for portable prototype definitions.
 *   NORETURN     attribute for non-returning functions.
 *   UNUSED       attribute for unused functions and variables.
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

#ifdef __STDC__
/* TODO: Remove PROT */
#    define PROT(x) x
#else /* __STDC__ */
#    define PROT(x) ()
#endif /* __STDC */

#if defined(__GNUC__) && __GNUC__ >= 2 && (__GNUC_MINOR__ > 5 || __GNUC__ > 2) && !defined(__APPLE_CC__)
#    define NORETURN __attribute__ ((noreturn))
#    define UNUSED   __attribute__ ((unused))
#elif defined(__MWERKS__)
#    define NORETURN
#    define UNUSED
#else
#    define NORETURN
#    define UNUSED
#endif

#ifdef __GNUC__
#    define FORMATDEBUG(f,a,b) __attribute__ ((format (f,a,b)))
#else
#    define FORMATDEBUG(f,a,b)
#endif

#define VARPROT(proto,like,form,var) proto FORMATDEBUG(like,form,var)

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

#if ( defined( atarist ) && !defined ( minix ) ) || defined( MSDOS ) || defined(__CYGWIN__)
#define MSDOS_FS
#endif


/*------------------------------------------------------------------
 * Integral types:
 *   Bool, SBool, CBool: boolean type, sized as int/short/char.
 *   p_int  : an integer that has the same size as a pointer
 *   ph_int : an integer that has half the size of a pointer
 *   mp_int : an integer that has at least the size of a pointer
 *   int32  : an integer with 32 bits
 *   PTRTYPE: a type to use with constant pointer arithmetic.
 * The unsigned versions use 'uint' instead of 'int'.
 * TODO: Add a type 'uchar', '(u)int8' and '(u)int16'., unless not already
 * TODO:: defined by STDC.
 * TODO: inttypes.h and stdint.h have many interesting types...
 */

/* p_int : an integer that has the same size as a pointer */
#if SIZEOF_LONG == SIZEOF_CHAR_P
typedef long                p_int;
typedef unsigned long       p_uint;
#elif SIZEOF_INT == SIZEOF_CHAR_P
typedef int                 p_int;
typedef unsigned int        p_uint;
#elif defined(HAVE_LONG_LONG) && SIZEOF_LONG_LONG == SIZEOF_CHAR_P
typedef long long           p_int;
typedef unsigned long long  p_uint;
#else
#error cannot find an integer type with same size as a pointer
Thats it.
#endif

/* ph_int : an integer that has half the size of a pointer */
#if SIZEOF_CHAR_P == SIZEOF_INT * 2
typedef int                 ph_int;
typedef unsigned int        ph_uint;
#else
#    if SIZEOF_CHAR_P == 4
/* short is assumed to be always 2 bytes. */
/* TODO: This is a dangerous assumption. */
typedef short               ph_int;
typedef unsigned short      ph_uint;
#    endif
#endif

/* mp_int : an integer that has at least the size of a pointer */
typedef p_int        mp_int;
typedef p_uint        mp_uint;

#ifndef __BEOS__
/* int32 : an integer with 32 bits. */
#    if SIZEOF_LONG == 4
#        if !defined(_AIX)
typedef long                int32;
#        endif
typedef unsigned long       uint32;
#    else
#        if SIZEOF_INT == 4
typedef int                 int32;
typedef unsigned int        uint32;
#        endif
#    endif
#endif /* __BEOS__ */

/* Boolean datatype and values */

typedef int    Bool;  /* naming it 'bool' clashes on some machines... */
typedef short SBool;
typedef char  CBool;

#define MY_TRUE  (1)
#define MY_FALSE (0)

/* TODO: This should go into my-malloc.h? */
#ifdef FREE_RETURNS_VOID
#    define FREE_RETURN_TYPE void
#    define FREE_RETURN return;
#else
#    define FREE_RETURN_TYPE int
#    define FREE_RETURN return 1;
#endif

#if defined(AMIGA) && defined(_DCC)
    /* DICE v2.06 pessimizes constant pointer arithmetic,
     * assuming 0 as difference of constant pointers. */
#    define PTRTYPE unsigned long
#else
#    define PTRTYPE char *
#endif


/*------------------------------------------------------------------
 * Provide functions, types and defines missing from the system headers.
 */

#ifndef HAVE_SSIZE_T
typedef signed long ssize_t;
#endif

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

#if !defined(__BEOS__) && !defined(__CYGWIN__)
#    define O_BINARY 0
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
#define ixopen   ((int(*)(char *, int))open)
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
 * The host specific includes
 */

#if defined(HOST_INCLUDE)
#    include HOST_INCLUDE
#elif defined(AMIGA)
#    include "hosts/amiga/amiga.h"
#elif defined(__BEOS__)
#    include "hosts/be/be.h"
#else
#    include "hosts/unix.h"
#endif

/*------------------------------------------------------------------
 * At last, the functions provided in port.c
 */

extern mp_int get_current_time(void);
extern char * time_string(mp_int);
extern char * utime_string(mp_int, mp_int);
extern char * time_stamp(void);

#ifndef HAVE_STRCSPN
extern size_t strcspn(const char *s, const char *set);
#endif

#ifndef HAVE_STRDUP
extern char *strdup(const char *);
#endif

#ifndef HAVE_MEMMEM
extern char *memmem(const char *, size_t, const char *, size_t);
#endif

#if !defined(HAVE_MEMMOVE) && !defined(OVERLAPPING_BCOPY)
extern void move_memory(char *, char *, size_t);
#endif

#ifdef HAVE_CRYPT_H
     /* solaris defines crypt() here */
#    include <crypt.h>
#endif

#if ((!defined(HAVE_CRYPT) && !defined(HAVE__CRYPT))) || \
    !defined(USE_SYSTEM_CRYPT) || \
    (defined(sgi) && !defined(_MODERN_C)) || defined(ultrix) \
    || defined(sun)
extern char *crypt(const char *, const char *);
#endif

#if !defined(HAVE_CRYPT) && defined(HAVE__CRYPT)
#    define crypt(pass, salt) _crypt(pass, salt)
#endif

#if defined(AMIGA) || defined(__CYGWIN__)
extern void init_rusage(void);
#else
#define init_rusage()
#endif

#endif /* PORT_H__ */
