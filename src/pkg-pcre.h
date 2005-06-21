#ifndef PKG_PCRE_H_
#define PKG_PCRE_H_ 1

/*------------------------------------------------------------------
 * Wrapper for the PCRE include.
 *
 * This extra wrapper is needed since we have to add one error code
 * to the ones defined by PCRE itself.
 *------------------------------------------------------------------
 */

#include "driver.h"

#if defined(USE_BUILTIN_PCRE) || !defined(HAS_PCRE)
#  include "pcre/pcre.h"
#  if !defined(USE_BUILTIN_PCRE)
#      define USE_BUILTIN_PCRE
#  endif
#else
#  include <pcre.h>
#endif

/* Error code to be returned if too many backtracks are detected.
 */
#define RE_ERROR_BACKTRACK PCRE_ERROR_MATCHLIMIT

#endif /* PKG_PCRE_H_ */
