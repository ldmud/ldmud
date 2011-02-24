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

/* Include the portability headers */
#include "port.h"

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

#if !defined(MALLOC_smalloc) && !defined(MALLOC_sysmalloc) && ~defined(MALLOC_slaballoc)
#  define MALLOC_slaballoc
#endif


/* Do we have full GC support? */

#if defined(MALLOC_smalloc) || defined(MALLOC_slaballoc)
#  define GC_SUPPORT 1
#endif


/* Do some of the selected packages require special treatment? */

/* SQLite in the threadsafe mode needs a normal malloc() */
/* TODO: does it work with mmap, as long as we don't replace malloc? */
#if defined(USE_SQLITE) && defined(SQLITE3_USES_PTHREADS)
#  if defined(MALLOC_SBRK)
#      undef MALLOC_SBRK
#  endif
#  if defined(MALLOC_REPLACEABLE)
#      undef MALLOC_REPLACEABLE
#  endif
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

/* To function properly, MALLOC_(LPC_)TRACE need to be able to
 * test whether a memory block is free or not. This is reliably implemented
 * by MALLOC_CHECK.
 */

#if (defined(MALLOC_TRACE) || defined(MALLOC_LPC_TRACE)) && !defined(MALLOC_CHECK)
#  define MALLOC_CHECK
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

/* The ALARM and HEART_BEAT time need to be at least 1. */

#if !defined(ALARM_TIME)
#define ALARM_TIME 1
#elif ALARM_TIME < 1
#undef ALARM_TIME
#define ALARM_TIME 1
#endif

#if !defined(HEART_BEAT_INTERVAL)
#define HEART_BEAT_INTERVAL 1
#elif HEART_BEAT_INTERVAL < 1
#undef HEART_BEAT_INTERVAL
#define HEART_BEAT_INTERVAL 1
#endif

/* Define some macros needed in the headers included from ../mudlib/sys */

#ifdef USE_IPV6
#    define __IPV6__
#endif

/* If USE_PCRE is defined, check if the libpcre is available on this system.
 * Disable USE_PCRE if not. */
#if defined(USE_PCRE) && !defined(HAS_PCRE)
#undef USE_PCRE
#endif


/* TODO: this ctype-stuff might go into lex.h (impl in efun_defs.c) */
#define _MCTe 0x01 /* escaped character in save/restore object. */
#define _MCTd 0x02 /* numeric digit                */
#define _MCTt 0x04 /* delimiters in save/restore object. */

#define _MCTs 0x10 /* whitespace EXCLUDING '\n'        */

#define _MCTx 0x40 /* hexadecimal                */
#define _MCTa 0x80 /* alphanumeric or '_'         */
extern unsigned char _my_ctype[];
#define isescaped(c) (_my_ctype[(unsigned char)(c)]&_MCTe)
#define issavedel(c) (_my_ctype[(unsigned char)(c)]&_MCTt)
#define isalunum( c) (_my_ctype[(unsigned char)(c)]&_MCTa)
#define lexdigit( c) (_my_ctype[(unsigned char)(c)]&_MCTd)

/* A define to point out empty loop bodies. */
#define NOOP

/* A macro to wrap statements */
#define MACRO(x) do { x ; } while(0)

/* Determine the minimum of two values.
 * Some systems define this in system includes.
 */
#ifndef MIN
#define MIN(a,b) (((a)<(b))?(a):(b))
#endif /* MIN */

/* For the use of mudlib/sys/debug_info.h.
 */
#ifdef EVAL_COST_TRACE
#define __EVAL_COST_TRACE__
#endif

/* Always enable structs.
 * TODO: remove this, once all #ifdef USE_STRUCTS have been removed.
 * Warning: do not #undef this to disable structs. It will produce an
 * inconsistent and buggy driver.
 */
#define USE_STRUCTS 1

/* Always enable new inline closures.
 * TODO: remove this, once all #ifdef USE_NEW_INLINES have been removed.
 * Warning: do not #undef this to disable them. It will produce an
 * inconsistent and buggy driver.
 */
#define USE_NEW_INLINES 1

#endif /* DRIVER_H__ */
