/*------------------------------------------------------------------
 * Wrapper for pcre/get.c
 *
 * Compile pcre/get.c only if USE_PCRE is defined.
 *------------------------------------------------------------------
 */

#include "driver.h"

#ifdef USE_PCRE
#include "pcre/get.c"
#endif
