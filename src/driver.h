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

/* TODO: Some TODO defines */

/* NO_NEGATIVE_RANGES: If defined, assignments to negative ranges
 *   like [4..2] are not allowed. However, they are useful at times
 *   and so this switch should be under control of a pragma or special
 *   syntactic construct. For now and for compatibility reasons, these
 *   ranges remain allowed.
 */
/* #undef NO_NEGATIVE_RANGES */

/*----------------------------------------------------------------*/
/* Verify some of the definitions in config.h */

/* Make sure that YYDEBUG is defined to 1 - just being defined
 * is not sufficient.
 */
#if defined(YYDEBUG)
#  undef YYDEBUG
#  define YYDEBUG 1
#endif

#if !defined(MALLOC_smalloc) && !defined(MALLOC_sysmalloc) && !defined(MALLOC_ptmalloc)
#  define MALLOC_smalloc
#endif

/* Do we have full GC support? */

#if defined(MALLOC_smalloc)
#  define GC_SUPPORT 1
#endif

/* When we have allocation tracing, the allocator annotates every
 * allocation with the source filename and line where the allocation
 * occured. To allow the annotation of the allocations of higher structures
 * like strings with the place where the string as such is allocated (and not
 * the places in the string module), the following macros can be used
 * to declare and pass the necessary information transparently:
 *
 *   MTRACE_DECL: the declaration of the tracing arguments
 *   MTRACE_PASS: the tracing arguments when passed on to lower
 *                functions
 *   MTRACE_ARG:  the tracing arguments on the toplevel call
 */

#ifdef MALLOC_TRACE

#define MTRACE_DECL     , const char * malloc_trace_file, int malloc_trace_line
#define MTRACE_PASS     , malloc_trace_file, malloc_trace_line
#define MTRACE_ARG      , __FILE__, __LINE__

#else

#define MTRACE_DECL
#define MTRACE_PASS
#define MTRACE_ARG

#endif

/* This one is for backwards compatibility with old config.hs */

#if defined(NATIVE_MODE) && !defined(STRICT_EUIDS)
#  define STRICT_EUIDS
#elif defined(COMPAT_MODE)
#  undef STRICT_EUIDS
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
#    define MAXINT (0x7fffffffU)
#endif

/* A define to point out empty loop bodies. */
#define NOOP

/* A macro to wrap statements */
#define MACRO(x) do { x ; } while(0)

#endif /* DRIVER_H__ */
