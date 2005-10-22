/*------------------------------------------------------------------
 * Wrapper for pcre/study.c
 *
 * Compile pcre/study.c only if USE_PCRE is defined.
 *------------------------------------------------------------------
 */

#include "driver.h"

#ifdef USE_PCRE
#include "pcre/study.c"
#endif
