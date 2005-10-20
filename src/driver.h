#ifndef DRIVER_H__
#define DRIVER_H__

/*------------------------------------------------------------------
 * Global mandatory include file.
 *
 * It contains various global macros and declarations, and takes
 * care of the proper inclusion of the configuration/portability
 * include files.
 *------------------------------------------------------------------
 */

#define __DRIVER_SOURCE__

#include "config.h"

/* Verify some of the definitions in config.h */

#ifndef MASTER_NAME
#  ifdef COMPAT_MODE
#    define MASTER_NAME "obj/master"
#  else
#    define MASTER_NAME "secure/master"
#  endif
#endif

#if !defined(MALLOC_smalloc) && !defined(MALLOC_sysmalloc)
#  define MALLOC_smalloc
#endif

#if defined(MALLOC_sysmalloc)
   /* TODO: Implement allocation tracing for sysmalloc. This
    * TODO:: would also allow us a generic malloced_size().
    */
#  if defined(MALLOC_TRACE)
#    undef MALLOC_TRACE
#  endif
#  if defined(MALLOC_LPC_TRACE)
#    undef MALLOC_LPC_TRACE
#  endif
#endif

/* Do we have full GC support? */

#if defined(MALLOC_smalloc)
#  define GC_SUPPORT 1
#endif
 

/* This one is for backwards compatibility with old config.hs */

#if defined(NATIVE_MODE) && !defined(STRICT_EUIDS)
#  define STRICT_EUIDS
#elif defined(COMPAT_MODE)
#  undef STRICT_EUIDS
#endif

#if !defined(CATCH_UDP_PORT)
#  undef UDP_SEND
#endif

/* The string table is shadowed only in DEBUG mode */

#if !defined(DEBUG) && defined(CHECK_STRINGS)
#  undef CHECK_STRINGS
#endif

/* Define some macros needed in the headers included from ../mudlib/sys */

#ifdef USE_IPV6
#    define __IPV6__
#endif

/* Include the portability headers */
#include "port.h"

/* TODO: this ctype-stuff might go into lex.h (impl in efun_defs.c) */
#define _MCTe 0x01 /* escaped character in save/restore object. */
#define _MCTd 0x02 /* numeric digit                */


#define _MCTs 0x10 /* whitespace EXCLUDING '\n'        */

#define _MCTx 0x40 /* hexadecimal                */
#define _MCTa 0x80 /* alphanumeric or '_'         */
extern unsigned char _my_ctype[];
#define isescaped(c) (_my_ctype[(unsigned char)(c)]&_MCTe)
#define isalunum( c) (_my_ctype[(unsigned char)(c)]&_MCTa)
#define lexdigit( c) (_my_ctype[(unsigned char)(c)]&_MCTd)

#ifndef MAXINT
#    define MAXINT (0x7fffffff)
#endif

/* A define to point out empty loop bodies. */
#define NOOP

/* A macro to wrap statements */
#define MACRO(x) do { x ; } while(0)

#endif /* DRIVER_H__ */
