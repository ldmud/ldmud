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

#ifdef HAS_PCRE
#include <pcre.h>
#endif

/* Error code to be returned if too many backtracks are detected.
 */
#ifdef PCRE_ERROR_RECURSIONLIMIT
#define RE_ERROR_BACKTRACK PCRE_ERROR_RECURSIONLIMIT
#else
#define RE_ERROR_BACKTRACK (-8) // PCRE_ERROR_MATCHLIMIT from PCRE
#endif

#endif /* PKG_PCRE_H_ */
