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

#if defined(HAS_PCRE2)
#define PCRE2_CODE_UNIT_WIDTH 8
#include <pcre2.h>

#define RE_ERROR_BACKTRACK (-8) // Free error code, see regexp.h for others.
#undef HAS_PCRE

#elif defined(HAS_PCRE)
#include <pcre.h>

/* Error code to be returned if too many backtracks are detected.
 */
#ifdef PCRE_ERROR_RECURSIONLIMIT
#define RE_ERROR_BACKTRACK PCRE_ERROR_RECURSIONLIMIT
#else
#define RE_ERROR_BACKTRACK (-8) // PCRE_ERROR_MATCHLIMIT from PCRE
#endif
#endif

#endif /* PKG_PCRE_H_ */
