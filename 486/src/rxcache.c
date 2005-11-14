/*------------------------------------------------------------------
 * Regular Expression Cache
 * Written 1998 by Lars Duening.
 * Share and Enjoy!
 *------------------------------------------------------------------
 * Implementation of a cache for compiled regular expressions and
 * regexp matches. Usage of the cache can reduce the setup time
 * for regexps by factor 4; the actual regexp matching showed up
 * in experiments as being fast enough to make a cache for the
 * results worthless.
 *
 * Compiled expressions are stored together with their generator
 * strings in a hash table, hashed over the generator string content.
 *
 * The table sizes are specified in config.h as follows:
 *   RXCACHE_TABLE: size of the expression hash table
 *
 * If RXCACHE_TABLE is not defined, the whole caching is disabled.
 *
 * The include rxcache.h file offers some macros for transparent use:
 *   REGCOMP() wrap up the calls to hs_regcomp().
 *   RX_DUP() and REGFREE() handle the refcounting necessary.
 * The macros map to the standard uncached, non-refcounted calls
 * if the rxcache is disabled.
 *
 * Beware! hs_regexec() stores result data in the regexp structure (the
 * startp[] and end[] arrays), so the same pattern must not be used
 * in two concurrent regcomp_cache/hs_regexec pairs.
 *
 * TODO: Using shared strings as cache-indices can speed things up,
 * TODO:: especially when knowing where to find the hashvalue from
 * TODO:: the strtable.
 * TODO: Use in-table chaining to handle collisions.
 *------------------------------------------------------------------
 */

/*--------------------------------------------------------------------*/

#include "driver.h"

#include <stdio.h>
#include <string.h>

#define NO_REF_STRING
#include "rxcache.h"

#include "gcollect.h"
#include "hash.h"
#include "regexp.h"
#include "smalloc.h"
#include "stralloc.h"
#include "strfuns.h"
#include "svalue.h"
#include "xalloc.h"

#include "../mudlib/sys/debug_info.h"

#ifdef RXCACHE_TABLE

/*--------------------------------------------------------------------*/

/* Hash functions, inspired by interpret.c */

#if !( (RXCACHE_TABLE) & (RXCACHE_TABLE)-1 )
#define RxStrHash(s) ((s) & ((RXCACHE_TABLE)-1))
#else
#define RxStrHash(s) ((s) % RXCACHE_TABLE)
#endif


/* One expression hashtable entry */

typedef struct RxHashEntry {
    unsigned char * pString;  /* Generator string, a shared string
                               * NULL if unused */
    p_uint   hString;  /* Hash of pString */
    Bool     from_ed;  /* The from_ed value */
    Bool     excompat; /* The excompat value */
    regexp * pRegexp;  /* The generated regular expression from hs_regcomp() */
} RxHashEntry;


/* Variables */
static RxHashEntry xtable[RXCACHE_TABLE];  /* The Expression Hashtable */

/* Expression cache statistics */
static uint32 iNumXRequests   = 0;  /* Number of calls to regcomp() */
static uint32 iNumXFound      = 0;  /* Number of calls satisfied from table */
static uint32 iNumXCollisions = 0;  /* Number of hashcollisions */
static uint32 iNumXEntries    = 0;  /* Number of used cache entries */
static uint32 iXSizeAlloc     = 0;  /* Dynamic memory held in regexp structs */

/*--------------------------------------------------------------------*/
void rxcache_init(void)

/* Initialise the module. */

{
    memset(xtable, 0, sizeof(xtable));
}

/*--------------------------------------------------------------------*/
regexp *
regcomp_cache(unsigned char * expr, Bool excompat, Bool from_ed)

/* Compile a regexp structure from the expression <expr>, more or
 * less ex compatible.
 *
 * If possible, take a ready-compiled structure from the hashtable,
 * else enter the newly compiled structure into the table.
 *
 * The caller gets his own reference to the structure, which he has
 * to rx_free() after use.
 */

{
    p_uint hExpr;
    regexp * pRegexp;
    RxHashEntry *pHash;

    iNumXRequests++;

    hExpr = whashstr((char *)expr, 50);
    pHash = xtable+RxStrHash(hExpr);

    /* Look for a ready-compiled regexp */
    if (pHash->pString != NULL
     && pHash->hString == hExpr
     && pHash->from_ed == from_ed
     && pHash->excompat == excompat
     && !strcmp((char *)pHash->pString, (char *)expr)
       )
    {
        iNumXFound++;
        return rx_dup(pHash->pRegexp);
    }

    /* Regexp not found: compile a new one and enter it
     * into the table.
     */
    pRegexp = hs_regcomp(expr, excompat, from_ed);
    if (NULL == pRegexp)
        return NULL;

    expr = (unsigned char *)make_shared_string((char *)expr);

    if (NULL != pHash->pString)
    {
        iNumXCollisions++;
        iNumXEntries--;
        iXSizeAlloc -= pHash->pRegexp->regalloc;

        free_string((char *)pHash->pString);
        rx_free(pHash->pRegexp);
    }
    pHash->pString = expr; /* refs are transferred */
    pHash->hString = hExpr;
    pHash->pRegexp = pRegexp;
    pHash->from_ed = from_ed;
    pHash->excompat = excompat;

    iNumXEntries++;
    iXSizeAlloc += pRegexp->regalloc;

    return rx_dup(pRegexp);
} /* regcomp_cache() */

/*--------------------------------------------------------------------*/
size_t
rxcache_status (strbuf_t *sbuf, Bool verbose)

/* Gather (and optionally print) the statistics from the rxcache.
 * Return the amount of memory used.
 */

{
    uint32 iNumXReq;  /* Number of regcomp() requests, made non-zero */

#if defined(__MWERKS__) && !defined(WARN_ALL)
#    pragma warn_largeargs off
#endif

    /* In verbose mode, print the statistics */
    if (verbose)
    {
        strbuf_add(sbuf, "\nRegexp cache status:\n");
        strbuf_add(sbuf,   "--------------------\n");
        strbuf_addf(sbuf, "Expressions in cache:  %lu (%.1f%%)\n"
                   , iNumXEntries, 100.0 * (float)iNumXEntries / RXCACHE_TABLE);
        strbuf_addf(sbuf, "Memory allocated:      %lu\n", iXSizeAlloc);
        iNumXReq = iNumXRequests ? iNumXRequests : 1;
        strbuf_addf(sbuf
               , "Requests: %lu - Found: %lu (%.1f%%) - Coll: %lu (%.1f%% req/%.1f%% entries)\n"
               , iNumXRequests, iNumXFound, 100.0 * (float)iNumXFound/(float)iNumXReq
               , iNumXCollisions, 100.0 * (float)iNumXCollisions/(float)iNumXReq
               , 100.0 * (float)iNumXCollisions/(iNumXEntries ? iNumXEntries : 1)
               );
    }
    else
    {
        strbuf_addf(sbuf, "Regexp cache:\t\t\t%8ld %9lu\n", iNumXEntries, iXSizeAlloc);
    }

    return iXSizeAlloc;

#if defined(__MWERKS__)
#    pragma warn_largeargs reset
#endif
} /* rxcache_status() */

/*-------------------------------------------------------------------------*/
void
rxcache_dinfo_status (svalue_t *svp, int value)

/* Return the rxcache information for debug_info(DINFO_DATA, DID_STATUS).
 * <svp> points to the svalue block for the result, this function fills in
 * the spots for the object table.
 * If <value> is -1, <svp> points indeed to a value block; other it is
 * the index of the desired value and <svp> points to a single svalue.
 */

{
#define ST_NUMBER(which,code) \
    if (value == -1) svp[which].u.number = code; \
    else if (value == which) svp->u.number = code
    
    ST_NUMBER(DID_ST_RX_CACHED, iNumXEntries);
    ST_NUMBER(DID_ST_RX_TABLE, RXCACHE_TABLE);
    ST_NUMBER(DID_ST_RX_TABLE_SIZE, iXSizeAlloc);
    ST_NUMBER(DID_ST_RX_REQUESTS, iNumXRequests);
    ST_NUMBER(DID_ST_RX_REQ_FOUND, iNumXFound);
    ST_NUMBER(DID_ST_RX_REQ_COLL, iNumXCollisions);

#undef ST_NUMBER
} /* rxcache_dinfo_status() */

/*--------------------------------------------------------------------*/
regexp *
rx_dup (regexp * expr)

/* Increase the reference count of <expr> and return it.
 */

{
    expr->refs++;
    return expr;
}

/*--------------------------------------------------------------------*/
void
rx_free (regexp * expr)

/* Decrease the reference count of <expr>. If it reaches 0, the
 * structure and all associated data is deallocated.
 */

{
    expr->refs--;
    if (!expr->refs)
        xfree(expr);
}

/*--------------------------------------------------------------------*/
#if defined(GC_SUPPORT)

/*--------------------------------------------------------------------*/
void
clear_rxcache_refs (void)

/* Clear all the refcounts in the hashtables.
 * The refs of the shared strings and of the memory blocks are
 * not of our concern.
 */

{
    int i;

    for (i = 0; i < RXCACHE_TABLE; i++)
        if (NULL != xtable[i].pString)
            xtable[i].pRegexp->refs = 0;
} /* clear_rxcache_refs() */

/*--------------------------------------------------------------------*/
void
count_rxcache_refs (void)

/* Mark all memory referenced from the hashtables. */

{
    int i;

    for (i = 0; i < RXCACHE_TABLE; i++)
    {
        if (NULL != xtable[i].pString)
        {
            count_ref_from_string((char *)xtable[i].pString);
            count_rxcache_ref(xtable[i].pRegexp);
        }
    } /* for (i) */

} /* count_rxcache_refs() */

/*--------------------------------------------------------------------*/
void
count_rxcache_ref (regexp * pRegexp)

/* Mark all memory associated with one regexp structure and count
 * the refs.
 * This function is called both from rxcache as well as from ed.
 */

{
    note_malloced_block_ref((char *)pRegexp);
    pRegexp->refs++;
} /* count_rxcache_ref() */

#endif /* if GC_SUPPORT */

#endif /* if RXCACHE_TABLE */

/*====================================================================*/

