/*------------------------------------------------------------------
 * Wrapper for pcre/pcre.c
 *
 * Compile pcre/pcre.c only if USE_PCRE is defined.
 *------------------------------------------------------------------
 */

#include "driver.h"

#ifdef USE_PCRE
#define NEWLINE '\n'
#include "pcre/pcre.c"
#endif
