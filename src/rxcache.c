/*------------------------------------------------------------------
 * Regular Expression Wrapper and Cache
 * Written 1998, 2002 by Lars Duening.
 * Share and Enjoy!
 *------------------------------------------------------------------
 * This module serves as wrapper around optional alternative 
 * regular expression implementations. It wraps the unique regexp
 * structures and calls into a unified API.
 *
 * Beware! rx_exec() stores result data in the regexp structure (the
 * startp[] and end[] arrays), so the same pattern must not be used
 * in two concurrent regcomp_cache/regexec pairs.
 *
#ifdef RXCACHE_TABLE
 * Additionally, the regular expressions are held in a cache.
 * Usage of the cache can reduce the setup time
 * for regexps by factor 4; the actual regexp matching showed up
 * in experiments as being fast enough to make a cache for the
 * results worthless.
 *
 * Compiled expressions are stored together with their generator
 * strings in a hash table, hashed over the generator string content.
 *
 * The table sizes are specified in config.h as follows:
 *   RXCACHE_TABLE: size of the expression hash table
#endif
 *
 * TODO: Using shared strings as cache-indices can speed things up,
 * TODO:: especially when knowing where to find the hashvalue from
 * TODO:: the strtable.
 * TODO: Use in-table chaining to improve the use of the table space
 * TODO:: and reduce the number of collisions.
 *------------------------------------------------------------------
 */

/*--------------------------------------------------------------------*/

#include "driver.h"

#include <stdio.h>
#include <string.h>

#include "rxcache.h"

#include "gcollect.h"
#include "hash.h"
#include "mstrings.h"
#include "regexp.h"
#ifdef USE_PCRE
#include "pcre/pcre.h"
#endif
#include "simulate.h"
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


/* One expression hashtable entry, derived from regexp_t. */

typedef struct RxHashEntry {
    regexp_t   base;     /* The base regexp_t structure */

    string_t * pString;  /* Generator string, a counted tabled string
                          * NULL if unused */
    p_uint     hString;  /* Hash of pString */
    size_t     size;     /* Size of regexp expressions for statistics */
#ifndef USE_PCRE
    Bool       excompat; /* The excompat value */
    Bool       from_ed;  /* The from_ed value */
#endif
} RxHashEntry;


/* Variables */
static RxHashEntry * xtable[RXCACHE_TABLE];  /* The Expression Hashtable */

/* Expression cache statistics */
static uint32 iNumXRequests   = 0;  /* Number of calls to regcomp() */
static uint32 iNumXFound      = 0;  /* Number of calls satisfied from table */
static uint32 iNumXCollisions = 0;  /* Number of hashcollisions */
static uint32 iNumXEntries    = 0;  /* Number of used cache entries */
static uint32 iXSizeAlloc     = 0;  /* Dynamic memory held in regexp structs */

#endif /* RXCACHE_TABLE */

#ifdef USE_PCRE
static size_t pcre_malloc_size;
  /* Accumulated size from pcre_malloc() calls. Used when creating
   * a new PCRE to capture its allocated size for statistics.
   */

static const char* pcre_malloc_err;
  /* Set by the caller of PCRE functions so that an out-of-memory
   * condition in the pcre_xalloc() wrapper can get the proper
   * error message.
   */

/*--------------------------------------------------------------------*/
static void *
pcre_xalloc (size_t size)

/* Wrapper function so that PCRE will use our special allocator. */

{
    void * p;

    if (!pcre_malloc_err)
        pcre_malloc_err = "in PCRE function";
    xallocate(p, size, pcre_malloc_err);
    pcre_malloc_size += size;
    return p;
} /* pcre_xalloc() */

#endif /* USE_PCRE */

/*--------------------------------------------------------------------*/
void rx_init(void)

/* Initialise the module. */

{
#ifdef RXCACHE_TABLE
    memset(xtable, 0, sizeof(xtable));
#endif

#ifdef USE_PCRE
    pcre_malloc = pcre_xalloc;
    pcre_free = xfree;
#endif /* USE_PCRE */
} /* rx_init() */

/*--------------------------------------------------------------------*/
regexp_t *
rx_compile (string_t * expr
#ifdef USE_PCRE
           , int opt
#else
           , Bool excompat, Bool from_ed
#endif
           )

/* TODO: Unify the two option settings, and also store them in the cache. */

/* Compile a regexp structure from the expression <expr>, more or
 * less ex compatible.
 *
 * If possible, take a ready-compiled structure from the hashtable,
 * else enter the newly compiled structure into the table.
 *
 * The caller gets his own reference to the structure, which he has
 * to free_regexp() after use.
 */

{
#ifdef USE_PCRE
    pcre       * pProg;   /* The generated regular expression */
    pcre_extra * pHints;  /* Study data */
    const char * pErrmsg;
    int          erridx;
#else
    regexp * pRegexp;
#endif

#ifdef RXCACHE_TABLE
    p_uint hExpr;
    int h;
    RxHashEntry *pHash;

    iNumXRequests++;

    hExpr = whashmem(get_txt(expr), mstrsize(expr), 100);
    h = RxStrHash(hExpr);
    pHash = xtable[h];

    /* Look for a ready-compiled regexp */
    if (pHash != NULL
     && pHash->pString != NULL
     && pHash->hString == hExpr
#ifndef USE_PCRE
     && pHash->from_ed == from_ed
     && pHash->excompat == excompat
#endif
     && mstreq(pHash->pString, expr)
       )
    {
        iNumXFound++;
        return ref_regexp((regexp_t *)pHash);
    }
#endif

    /* Regexp not found: compile a new one.
     */
#ifdef USE_PCRE
     pcre_malloc_size = 0;
     pcre_malloc_err  = "compiling regex";
     pProg = pcre_compile(get_txt(expr), opt, &pErrmsg, &erridx, NULL);
 
     if (NULL == pProg)
     {
         error("pcre: %s at offset %d\n", pErrmsg, erridx);
         /* NOTREACHED */
         return NULL;
     }
 
     pcre_malloc_err  = "studying regex";
     pHints = pcre_study(pProg, 0, &pErrmsg);
 
     if (pErrmsg)
     {
         xfree(pProg);
         error("pcre: %s\n", pErrmsg);
         /* NOTREACHED */
         return NULL;
     }
#else
    pRegexp = regcomp((unsigned char *)get_txt(expr), excompat, from_ed);
    if (NULL == pRegexp)
        return NULL;
#endif

#ifndef RXCACHE_TABLE

    /* Wrap up the new regular expression and return it */
    {
        regexp_t * rc;

        xallocate(rc, sizeof(*rc), "Regexp structure");
        rc->ref = 1;
#ifdef USE_PCRE
        rc->pProg = pProg;
        rc->pHints = pHints;
#else
        rc->rx = pRegexp;
#endif
        return rc;
    }
#else

    /* Wrap up the new regular expression and enter it into the table */
    expr = make_tabled_from(expr);

    if (NULL != pHash)
    {
        iNumXCollisions++;
        iNumXEntries--;
        iXSizeAlloc -= pHash->size;
        free_regexp((regexp_t *)pHash);
    }

    xallocate(pHash, sizeof(*pHash), "Regexp cache structure");
    xtable[h] = pHash;

    pHash->base.ref = 1;
    pHash->pString = expr; /* refs are transferred */
    pHash->hString = hExpr;
#ifdef USE_PCRE
    pHash->base.pProg = pProg;
    pHash->base.pHints = pHints;
    pHash->size = pcre_malloc_size;
#else
    pHash->base.rx = pRegexp;
    pHash->size = pRegexp->regalloc;
    pHash->from_ed = from_ed;
    pHash->excompat = excompat;
#endif

    iNumXEntries++;
    iXSizeAlloc += pHash->size;

    return ref_regexp((regexp_t *)pHash);
#endif
} /* rx_compile() */

/*-------------------------------------------------------------------------*/
Bool
rx_exec (regexp_t *prog, char *string, char *start)

/* Match the regexp <prog> against the <string> starting at the
 * position <start>.
 *
 * Return TRUE if found.
 */

{
#ifdef USE_PCRE
#else
    return regexec(prog->rx, string, start);
#endif
} /* rx_exec() */

/*-------------------------------------------------------------------------*/
char *
rx_sub (regexp_t *prog, char *source, char *dest, int n, Bool quiet)

/* After a regexp match, substitute the tokens '\<num>' in <source>
 * with the appropriate matched () expressions, and store the
 * result in <dest> (max size <n>).
 *
 * Return NULL on failure, and <dest> on success. If <quiet> is
 * FALSE, a failure also generates a call to regerror().
 */

{
#ifdef USE_PCRE
#else
    return regsub (prog->rx, source, dest, n, quiet);
#endif
} /* rx_sub() */

/*--------------------------------------------------------------------*/
void
rx_free (regexp_t * expr)

/* Deallocate a regexp structure <expr> and all associated data.
#ifdef RXCACHE_TABLE
 * <expr> is in fact a RxHashEntry structure.
#endif
 */

{
#ifdef RXCACHE_TABLE
    {
        RxHashEntry * pHash = (RxHashEntry *)expr;
        free_mstring(pHash->pString);
    }
#endif
#ifdef USE_PCRE
    if (expr->pHints) xfree(expr->pHints);
    xfree(expr->pProg);
#else
    xfree(expr->rx);
#endif
    xfree(expr);
} /* rx_free() */

/*--------------------------------------------------------------------*/
size_t
rxcache_status (strbuf_t *sbuf, Bool verbose)

/* Gather (and optionally print) the statistics from the rxcache.
 * Return the amount of memory used.
 */

{
#ifdef RXCACHE_TABLE

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

#else

    return 0;
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
#ifdef RXCACHE_TABLE

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
#endif
} /* rxcache_dinfo_status() */

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
#ifdef RXCACHE_TABLE
    int i;

    for (i = 0; i < RXCACHE_TABLE; i++)
        if (NULL != xtable[i])
        {
            xtable[i]->base.ref = 0;
        }
#endif
} /* clear_rxcache_refs() */

/*--------------------------------------------------------------------*/
void
count_regexp_ref (regexp_t * pRegexp)

/* Mark all memory associated with one regexp structure and count
 * the refs.
 * This function is called both from rxcache as well as from ed.
 */

{
    note_malloced_block_ref(pRegexp);
#ifdef USE_PCRE
    note_malloced_block_ref(pRegexp->pProg);
    if (pRegexp->pHints)
        note_malloced_block_ref(pRegexp->pHints);
#else
    note_malloced_block_ref(pRegexp->rx);
#endif
#ifdef RXCACHE_TABLE
    count_ref_from_string(((RxHashEntry *)pRegexp)->pString);
#endif
    pRegexp->ref++;
} /* count_rxcache_ref() */

/*--------------------------------------------------------------------*/
void
count_rxcache_refs (void)

/* Mark all memory referenced from the hashtables. */

{
#ifdef RXCACHE_TABLE
    int i;

    for (i = 0; i < RXCACHE_TABLE; i++)
    {
        if (NULL != xtable[i])
        {
            count_regexp_ref((regexp_t *)xtable[i]);
        }
    } /* for (i) */
#endif

} /* count_rxcache_refs() */

#endif /* if GC_SUPPORT */

/*====================================================================*/

