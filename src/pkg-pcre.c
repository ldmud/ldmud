/*------------------------------------------------------------------
 * Wrapper for the pcre modules.
 *
 * Compile the pcre modules into one file, but only if USE_PCRE is defined.
 * To make this possible the pcre/internal.h had to be augmented with
 * protection against multiple inclusion.
 *------------------------------------------------------------------
 */

#include "driver.h"

#ifdef USE_PCRE

/* Provide a definition for NEWLINE */
#define NEWLINE '\n'

/* DEBUG has a different meaning for pcre than for us */
#ifdef DEBUG
#    undef DEBUG
#endif

#include "pcre/pcre.c"
#include "pcre/get.c"
#include "pcre/maketables.c"
#include "pcre/study.c"
#endif
