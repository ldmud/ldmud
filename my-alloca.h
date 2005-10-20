#ifndef __MY_ALLOCA_H__
#define __MY_ALLOCA_H__

/*------------------------------------------------------------------
 * Portable definition of alloca().
 *
 * Use this file instead of the normal <alloca.h>.
 *------------------------------------------------------------------
 */

#include "driver.h"

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#else /* !HAVE_ALLOCA_H */
#ifdef __GNUC__
#ifndef alloca
#define alloca(size) __builtin_alloca(size)
#endif
#else /* !__GNUC__ */
#ifdef _AIX
 #pragma alloca
#endif /* _AIX */
#endif /* !__GNUC__ */
#endif /* !HAVE_ALLOCA_H */

#endif /* __MY_ALLOCA_H__ */

