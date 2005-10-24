/*------------------------------------------------------------------
 * Wrapper for the ptmalloc2 memory manager.
 *
 * Compile the ptmalloc2 memory manager when MALLOC_ptmalloc is defined.
 *
 * ptmalloc2 was written by Wolfram Glober (www.malloc.de).
 * ptmalloc2 is based on work of Doug Lea (gee.cs.oswego.edu).
 * ptmalloc2 was adapted to ldmud by Christian Welzel (www.camlann.de)
 *------------------------------------------------------------------
 */

#include "config.h"

#ifdef MALLOC_ptmalloc

/* Define the configuration macros for ptmalloc */

#if !defined(_GNU_SOURCE)
#    define _GNU_SOURCE 1
#endif
#define USE_TSD_DATA_HACK 1
#define _REENTRANT 1
#define USE_DL_PREFIX 1

#if defined(__APPLE__)
#    include <unistd.h>
#    define malloc_getpagesize getpagesize()
#endif

#include "ptmalloc/malloc.c"

#endif /* MALLOC_ptmalloc */

/***************************************************************************/
