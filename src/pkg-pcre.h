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

#ifdef USE_PCRE

#include "pcre/pcre.h"

/* Error code to be returned if too many backtracks are detected.
 */
#define RE_ERROR_BACKTRACK (2 * PCRE_ERROR_NOSUBSTRING)

#endif /* USE_PCRE */

#endif /* PKG_PCRE_H_ */
