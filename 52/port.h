#ifndef __PORT_H__
#define __PORT_H__

/*------------------------------------------------------------------
 * Global Portability Include File.
 *
 * The goal of port.c/h is to wrap up the actual system/compiler
 * combo in a nice uniform shell to keep the main sources clean.
 *
 * This include provides various macros for system dependent
 * features, includes the most common system includes and, more
 * important, the HOST_INCLUDE (defined in machine.h). If no
 * HOST_INCLUDE is defined, hosts/unix.h is used as default.
 *
 * Not everything system dependent is defined here, some stuff
 * are kept in separate my-foo.h files.
 *------------------------------------------------------------------
 */

#if !defined(__DRIVER_H__)
#error You must include driver.h instead of port.h
Thats it.
#endif

#if !defined(__STDC__)
/* TODO: Add a ANSI-check to configure and let it fail, too. */
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

/*------------------------------------------------------------------
 * Standard system headers.
 */

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

#include <sys/types.h>

/*------------------------------------------------------------------
 * Define some macros:
 *   CHARBITS     number of bits in a char, if not defined already.
 *   PROT(x)      for portable prototype definitions.
 *   NORETURN     attribute for non-returning functions.
 *   UNUSED       attribute for unused functions and variables.
 *   FORMATDEBUG  attribute for printf-style prototypes.
 *   VARPROT      for portable printf-style prototype definitions.
 *   HAS_INLINE   if 'inline' is available
 *   INLINE       attribute for inline functions.
 *   LOCAL_INLINE attribute for inline functions which shall be
 *                available to other files, too.
 *   EXTRACT_UCHAR(), EXTRACT_SCHAR():
 *                extract a character from a memory location.
 *   MSDOS_FS     if the filesystem uses MS-DOS semantics
 *                (i.e. backslashes as directory separators)
 */

#ifndef CHARBITS
#    define CHARBITS 8
#endif

#ifdef __STDC__
/* TODO: Remove PROT */
#    define PROT(x) x
#else /* __STDC__ */
#    define PROT(x) ()
#endif /* __STDC */

#if defined(__GNUC__) && __GNUC__ >= 2 && (__GNUC_MINOR__ > 5 || __GNUC__ > 2)
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

#ifdef __STDC__
/* TODO: Remove VARPROT */
#    define VARPROT(proto,like,form,var) proto FORMATDEBUG(like,form,var)
#else
#    define VARPROT(proto, like,form,var) ()
#endif

/* TODO: Give machine.h a 'HAS_INLINE' define */
/* TODO: Rethink this as soon as configure/autoconf has a proper inline
 * TODO:: detection - one that just checks for 'inline' vs. '__inline'.
 */

#if (defined(__GNUC__) || defined(__MWERKS__) || defined(inline)) && !defined(DEBUG)
#    define HAS_INLINE 1
#    define INLINE inline
#    if defined(__MWERKS__)
#        define LOCAL_INLINE
#    else
#        define LOCAL_INLINE INLINE
#    endif
#else
#    undef HAS_INLINE
#    define INLINE
#    define LOCAL_INLINE
#endif

#define EXTRACT_UCHAR(p) (*(unsigned char *)(p))
#define EXTRACT_SCHAR(p) (*(signed char *)(p))

#if ( defined( atarist ) && !defined ( minix ) ) || defined( MSDOS )
#define MSDOS_FS
#endif

/*------------------------------------------------------------------
 * Integral types:
 *   p_int  : an integer that has the same size as a pointer
 *   ph_int : an integer that has half the size of a pointer
 *   mp_int : an integer that has at least the size of a pointer
 *   int32  : an integer with 32 bits
 *   PTRTYPE: a type to use with constant pointer arithmetic.
 * The unsigned versions use 'uint' instead of 'int'.
 * TODO: Add a type 'uchar'.
 */

/* p_int : an integer that has the same size as a pointer */
#if SIZEOF_LONG == SIZEOF_P_INT
typedef long                p_int;
typedef unsigned long       p_uint;
#else
#    if SIZEOF_INT == SIZEOF_P_INT
typedef int                 p_int;
typedef unsigned int        p_uint;
#    endif
#    if SIZEOF_LONG < SIZEOF_P_INT
typedef long long           p_int;
typedef unsigned long long  p_uint;
#    endif
#endif

/* ph_int : an integer that has half the size of a pointer */
#if SIZEOF_P_INT == SIZEOF_INT * 2
typedef int                 ph_int;
typedef unsigned int        ph_uint;
#else
#    if SIZEOF_P_INT == 4
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
typedef long                int32;
typedef unsigned long       uint32;
#    else
#        if SIZEOF_INT == 4
typedef int                 int32;
typedef unsigned int        uint32;
#        endif
#    endif
#endif /* __BEOS__ */

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
 * Provide functions and defines missing from the system headers.
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

#if !defined(__BEOS__) && !defined(__CYGWIN32__)
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
 * The host specific include, as defined in machine.h
 */

#ifdef HOST_INCLUDE
#  include HOST_INCLUDE
#endif

/*------------------------------------------------------------------
 * At last, the functions provided in port.c
 */

extern mp_int get_current_time(void);
extern char * time_string(int);

#ifndef HAVE_MEMMEM
extern char *memmem(char *, size_t, char *, size_t);
#endif

#if !defined(HAVE_MEMMOVE) && !defined(OVERLAPPING_BCOPY)
extern void move_memory(char *, char *, size_t);
#endif

#if ((!defined(HAVE_CRYPT) && !defined(HAVE__CRYPT))) || \
    (defined(sgi) && !defined(_MODERN_C)) || defined(ultrix) \
    || defined(sun)
extern char *crypt(const char *, const char *);
#endif

#endif /* __PORT_H__ */
