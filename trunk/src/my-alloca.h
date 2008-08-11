#ifndef MY_ALLOCA_H__
#define MY_ALLOCA_H__

/*------------------------------------------------------------------
 * Portable definition of alloca().
 *
 * Use this file instead of the normal <alloca.h>.
 *
 * If you have a function making heavy use of alloca(), it is a good
 * idea to use GC_ALLOCA in the calling function after the call.
 * This macro takes care that for the C-version of alloca the memory
 * usage is kept in limits.
 *------------------------------------------------------------------
 */

#include "driver.h"

#if !defined(C_ALLOCA) || defined(MAKE_FUNC)

#ifdef HAVE_ALLOCA_H
#    include <alloca.h>
#else /* !HAVE_ALLOCA_H */
#    ifdef __GNUC__
#        ifndef alloca
#            define alloca(size) __builtin_alloca(size)
#        endif
#    else /* !__GNUC__ */
#        ifdef _AIX
#            pragma alloca
#        endif /* _AIX */
#    endif /* !__GNUC__ */
#endif /* !HAVE_ALLOCA_H */

#define GC_ALLOCA NOOP

#else /* defined(C_ALLOCA) && !defined(MAKE_FUNC) */

#include <sys/types.h>

#ifdef alloca
#    undef alloca
#endif

extern void * alloca(size_t);

#define GC_ALLOCA alloca(0)

#endif  /* C_ALLOCA */

#endif /* MY_ALLOCA_H__ */

