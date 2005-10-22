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

#include "pkg-pcre.h"

#include "interpret.h"
#include "simulate.h"

/* Provide a definition for NEWLINE */
#define NEWLINE '\n'

/* DEBUG has a different meaning for pcre than for us */
#ifdef DEBUG
#    undef DEBUG
#endif

/* To detect a pathological case of backtracking, a call to this
 * macro has to be inserted right after the 'bracked failed' diagnosic
 * in match().
 */

#define LDMUD_CHECK_EVAL_COST \
    eval_cost += 1; \
    if (max_eval_cost && max_eval_cost < eval_cost) { \
        md->errorcode = RE_ERROR_BACKTRACK; \
        return FALSE; \
    }

#include "pcre/pcre.c"
#include "pcre/get.c"
#include "pcre/maketables.c"
#include "pcre/study.c"
#endif
