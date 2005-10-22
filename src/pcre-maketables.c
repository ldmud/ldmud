/*------------------------------------------------------------------
 * Wrapper for pcre/maketables.c
 *
 * Compile pcre/maketables.c only if USE_PCRE is defined.
 *------------------------------------------------------------------
 */

#include "driver.h"

#ifdef USE_PCRE
#include "pcre/maketables.c"
#endif
