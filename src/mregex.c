/*------------------------------------------------------------------
 * Regular Expression Wrapper and Cache
 * Written 1998, 2002, 2004 by Lars Duening.
 * Share and Enjoy!
 *------------------------------------------------------------------
 * This module serves as wrapper around optional alternative
 * regular expression implementations. It wraps the unique regexp
 * structures into a ref-counted structure, and lets the API hand
 * out glorified pointers to this internal structure.
 *
 * Beware! rx_exec() stores result data in the regexp structure, so the
 * same pattern must not be used in two concurrent rx_compile/rx_exec pairs.
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
 * TODO: Separate the results out from HS-Regexp, so that the compiled
 * TODO:: program and the results can be held separate.
 *------------------------------------------------------------------
 */

/*--------------------------------------------------------------------*/

#include "driver.h"

#include <stdio.h>
#include <string.h>

#include "mregex.h"

#include "comm.h" /* add_message() */
#include "gcollect.h"
#include "hash.h"
#include "interpret.h"
#include "main.h"
#include "mstrings.h"
#include "pkg-pcre.h"
#include "regexp.h"
#include "simulate.h"
#include "strfuns.h"
#include "svalue.h"
#include "xalloc.h"

#include "../mudlib/sys/debug_info.h"
#include "../mudlib/sys/driver_hook.h"
#include "../mudlib/sys/regexp.h"

/*--------------------------------------------------------------------*/

/* --- struct regdata_s: the regexp structure wrapper ---
 *
 * This structure wraps the actual regexp structure into a common
 * format.
 *
 * The regular expressions are compiled as needed, with the pointers
 * serving as flags:
 *   .pProg != NULL: PCRE version compiled
 *   .rx    != NULL: traditional regex version compiled.
 *
#ifdef RXCACHE_TABLE
 * The regexp cache embeds this structure into a larger one which
 * it uses to manage the cached expressions.
#endif
 */

typedef struct regdata_s {
    p_uint        ref;       /* Number of refs */
    int           opt;       /* Additional options, but no package flags */
    /* -- PCRE -- */
#ifdef HAS_PCRE
    pcre        * pProg;     /* The generated regular expression */
    pcre_extra  * pHints;    /* Study data */
    int           num_subs;  /* Number of elements in pSubs */
    int         * pSubs;     /* Substring offsets + workarea */
    int           res;       /* Result of last rx_exec() */
#endif // HAS_PCRE
    /* -- Traditional -- */
    regexp      * rx;        /* The actual regular expression */
} regdata_t;

#ifdef RXCACHE_TABLE

/* --- struct RxHashEntry: One expression hashtable entry ---
 * Derived from regexp_i_t
 */

typedef struct RxHashEntry {
    regdata_t base;      /* The base regexp_t structure */

    string_t * pString;  /* Generator string, a counted tabled string
                          * NULL if unused */
    hash32_t    hString;  /* Hash of pString */
    size_t     size;     /* Size of regexp expressions for statistics */
} RxHashEntry;

#endif /* RXCHACHE_TABLE */

/* --- struct regexp_s: the regexp structure pimpl ---
 *
 * This structure is a decorated pointer to the regdata_t structure,
 * allowing to store unique options and data for shared regexps.
 * 
 * Since this structure is recreated for every rx_compile() call,
 * it stores the selection flags of which regexp package to use
 * for this call.
 */

struct regexp_s {
    int         opt;   /* Additional options, incl the package flags. */
    regdata_t * data;  /* Counted pointer to the internal structure */
};

/*--------------------------------------------------------------------*/

/* --- Variables --- */

#ifdef RXCACHE_TABLE

static RxHashEntry * xtable[RXCACHE_TABLE];  /* The Expression Hashtable */

/* Expression cache statistics */
static statcounter_t iNumXRequests   = 0;  /* Number of calls to rx_compile() */
static statcounter_t iNumXFound      = 0;  /* Number of calls satisfied from table */
static uint32 iNumXCollisions = 0;  /* Number of hashcollisions */
static uint32 iNumXEntries    = 0;  /* Number of used cache entries */
static uint32 iXSizeAlloc     = 0;  /* Dynamic memory held in regexp structs */

#endif /* RXCACHE_TABLE */

#ifdef HAS_PCRE
static size_t pcre_malloc_size;
  /* Accumulated size from pcre_malloc() calls. Used when creating
   * a new PCRE to capture its allocated size for statistics.
   */

static const char* pcre_malloc_err;
  /* Set by the caller of PCRE functions so that an out-of-memory
   * condition in the pcre_xalloc() wrapper can get the proper
   * error message.
   */
#endif // HAS_PCRE

/*--------------------------------------------------------------------*/
/* Declarations */
static void rx_free_data (regdata_t * expr); /* forward */
static void rx_free_subdata (regdata_t * expr); /* forward */

/* --- Helpers --- */
static INLINE regdata_t *
ref_regdata(regdata_t *r)
/* regdata_t *ref_regdata(regdata_t *r)
 *   Add another ref to regdata <r> and return the regdata <r>.
 */
{
    r->ref++;
    return r;
}

static INLINE void
free_regdata(regdata_t *r)
/* void free_regdata(regdata_t *r)
 *   Subtract one ref from regdata <r>, and free the regdata fully if
 *   the refcount reaches zero.
 */
{
    if (--(r->ref) <= 0)
        rx_free_data(r);
}


#ifdef RXCACHE_TABLE

/* Hash functions, inspired by interpret.c */

#if !( (RXCACHE_TABLE) & (RXCACHE_TABLE)-1 )
#define RxStrHash(s) ((s) & ((RXCACHE_TABLE)-1))
#else
#define RxStrHash(s) ((s) % RXCACHE_TABLE)
#endif

#endif /* RXCHACHE_TABLE */

/*--------------------------------------------------------------------*/
#ifdef HAS_PCRE
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
#endif // HAS_PCRE

/*--------------------------------------------------------------------*/
const char *
rx_pcre_version (void)

/* Return a string with the name and version of the PCRE package.
 */

{
    static char buf[40];
#ifdef HAS_PCRE
    snprintf(buf, sizeof(buf), "%d.%d", PCRE_MAJOR, PCRE_MINOR);
#else
    sprintf(buf, "not available");
#endif
    return buf;
} /* rx_pcre_version() */

/*--------------------------------------------------------------------*/
void rx_init(void)

/* Initialise the module. */

{
#ifdef RXCACHE_TABLE
    memset(xtable, 0, sizeof(xtable));
#endif
#ifdef HAS_PCRE
    pcre_malloc = pcre_xalloc;
    pcre_free = xfree;
#endif
} /* rx_init() */

/*--------------------------------------------------------------------*/
static const char *
get_error_message (int code, int package)

/* Return a constant string with the error message for <code>
 * generated by <package>.
 * If <code> is not an error, NULL is returned.
 */

{
#ifdef HAS_PCRE
    if (package & RE_PCRE)
    {
        const char* text;

        if (code >= 0)
            return NULL;
        switch (code)
        {
        case PCRE_ERROR_NOMATCH:
            text = "too many capturing parentheses"; break;
        case PCRE_ERROR_NULL:
            text = "code or subject NULL"; break;
        case PCRE_ERROR_BADOPTION:
            text = "unknown option specified"; break;
        case PCRE_ERROR_BADMAGIC:
            text = "regex memory invalid"; break;
        case PCRE_ERROR_UNKNOWN_NODE:
            text = "regex memory violated"; break;
        case PCRE_ERROR_NOMEMORY:
            text = "out of memory"; break;
        case RE_ERROR_BACKTRACK:
            text = "too many backtracks"; break;
        default:
            text = "unknown internal error"; break;
        }
        return text;
    }
#endif // HAS_PCRE

    if (package & RE_TRADITIONAL)
    {
        const char* text;

        if (code >= 0)
            return NULL;
        switch (code)
        {
        case RE_ERROR_NULL:
            text = "program or text NULL"; break;
        case RE_ERROR_CORRUPT:
            text = "regex memory invalid"; break;
        case RE_ERROR_BACKTRACK:
            text = "too many backtracks"; break;
        default:
            text = "unknown internal error"; break;
        }
        return text;
    }

    return NULL;
}  /* get_error_message() */

/*--------------------------------------------------------------------*/
const char *
rx_error_message (int code, const regexp_t * pRegexp)

/* Return a constant string with the error message for <code>
 * and regexp <pRegexp>.
 * If <code> is not an error, NULL is returned.
 */

{
    return get_error_message(code, pRegexp->opt);
}  /* rx_error_message() */

/*--------------------------------------------------------------------*/
static Bool
rx_compile_re (string_t * expr, int opt, Bool from_ed, regdata_t * rdata)

/* Compile the expression <expr> with the options <opt> into a traditional
 * expression and store it into *<rdata>.
 * On success, return TRUE;
 * On failure, if <from_ed> is FALSE, an error is thrown.
 * On failure, if <from_ed> is TRUE, the error message is printed directly
 * to the user and the function returns with  FALSE.
 */

{
    char   * pErrmsg;
    int      erridx;

    regexp * pRegexp = NULL;

    /* Sanitize the <opt> value */
    opt = opt & RE_EXCOMPATIBLE & ~(RE_PACKAGE_MASK);

    /* Compile the RE */
    pRegexp = hs_regcomp((unsigned char *)get_txt(expr)
                        , opt & RE_EXCOMPATIBLE
                        , &pErrmsg, &erridx);
    if (NULL == pRegexp)
    {
        rx_free_subdata(rdata); /* Might have PCRE data in it already */
        if (from_ed)
            add_message("re: %s at offset %d\n", pErrmsg, erridx);
        else
            errorf("re: %s at offset %d\n", pErrmsg, erridx);
        return MY_FALSE;
    }

    /* Compilation complete - store the result in the outgoing regdata
     * structure.
     */
    rdata->rx = pRegexp;

    return MY_TRUE;
} /* rx_compile_re() */

/*--------------------------------------------------------------------*/
#ifdef HAS_PCRE
static size_t
rx_compile_pcre (string_t * expr, int opt, Bool from_ed, regdata_t * rdata)

/* Compile the expression <expr> with the options <opt> into a PCRE
 * expression and store it into *<rdata>.
 * On success, return the accumulated size of the expression.
 * On failure, if <from_ed> is FALSE, an error is thrown.
 * On failure, if <from_ed> is TRUE, the error message is printed directly
 * to the user and the function returns with 0.
 */

{
    const char * pErrmsg;
    int          erridx;

    pcre       * pProg = NULL;     /* The generated regular expression */
    pcre_extra * pHints = NULL;    /* Study data */
    int        * pSubs;     /* Capturing and work area */
    int          pcre_opt;  /* <opt> translated into PCRE opts */
    int          num_subs;  /* Number of capturing parentheses */

    /* Sanitize the <opt> value */
    opt = opt & ~(RE_EXCOMPATIBLE) & ~(RE_PACKAGE_MASK);

    /* Determine the RE compilation options */

    pcre_opt = 0;
    if (opt & RE_CASELESS)       pcre_opt |= PCRE_CASELESS;
    if (opt & RE_MULTILINE)      pcre_opt |= PCRE_MULTILINE;
    if (opt & RE_DOTALL)         pcre_opt |= PCRE_DOTALL;
    if (opt & RE_EXTENDED)       pcre_opt |= PCRE_EXTENDED;
    if (opt & RE_DOLLAR_ENDONLY) pcre_opt |= PCRE_DOLLAR_ENDONLY;
    if (opt & RE_UNGREEDY)       pcre_opt |= PCRE_UNGREEDY;

    /* Compile the RE */
    pcre_malloc_size = 0;
    pcre_malloc_err  = "compiling regex";
    pProg = pcre_compile(get_txt(expr), pcre_opt, &pErrmsg, &erridx, NULL);

    if (NULL == pProg)
    {
        rx_free_subdata(rdata); /* Might have HS data in it already */
        if (from_ed)
            add_message("pcre: %s at offset %d\n", pErrmsg, erridx);
        else
            errorf("pcre: %s at offset %d\n", pErrmsg, erridx);
        return 0;
    }

    pcre_malloc_err  = "studying regex";
    pHints = pcre_study(pProg, 0, &pErrmsg);

    if (pErrmsg)
    {
        xfree(pProg);
        if (from_ed)
            add_message("pcre: %s\n", pErrmsg);
        else
            errorf("pcre: %s\n", pErrmsg);
        return 0;
    }
    /* We have to ensure to have an initialized pHints structure for
       setting the recursion limit later on */
    if (pHints == NULL) 
    {
        pcre_malloc_err = "allocating memory for pHints section in regexp";
        pHints = (pcre_extra *)pcre_xalloc(sizeof(pcre_extra));
        if (pHints == NULL) 
        {
            if (from_ed)
                add_message("pcre: Could not allocate memory for pHints\n");
            else
                errorf("pcre: Could not allocate memory for pHints\n");
            return 0;
        }
        pHints->flags = 0;
    }

    {
       int rc;

        rc = pcre_fullinfo(pProg, pHints, PCRE_INFO_CAPTURECOUNT, &num_subs);
        if (rc != 0)
        {
            xfree(pProg);
            xfree(pHints);
            if (from_ed)
                add_message("pcre: %s\n", get_error_message(rc, RE_PCRE));
            else
                errorf("pcre: %s\n", get_error_message(rc, RE_PCRE));
            return 0;
        }
        num_subs = 3 * (num_subs+1);
    }

    pSubs = xalloc(num_subs * sizeof (*pSubs));
    if (pSubs == NULL)
    {
        xfree(pProg);
        xfree(pHints);
        outofmem(num_subs * sizeof (*pSubs), "regexp work area");
    }
    pcre_malloc_size += num_subs * sizeof (*pSubs);

    /* Compilation complete - store the result in the outgoing regdata
     * structure.
     */

    rdata->pProg = pProg;
    rdata->pHints = pHints;
    rdata->pSubs = pSubs;
    rdata->num_subs = num_subs;
    rdata->res = 0;

    return pcre_malloc_size;
} /* rx_compile_pcre() */
#endif // HAS_PCRE

/*--------------------------------------------------------------------*/
static regdata_t *
rx_compile_data (string_t * expr, int opt, Bool from_ed)

/* Compile a regdata structure from the expression <expr>, according
 * to the options in <opt>. If <from_ed> is TRUE, the RE is used
 * from the editor, so error messages will be printed directly
 * to the user.
 *
 * If possible, take a ready-compiled structure from the hashtable,
 * else enter the newly compiled structure into the table.
 *
 * The caller gets his own reference to the structure, which he has
 * to free_regdata() after use.
 */

{
    size_t    pcre_size;
    regdata_t rdata; /* Local rdata structure to hold compiled expression */

#ifdef RXCACHE_TABLE
    hash32_t hExpr;
    int h;
    RxHashEntry *pHash;
#endif

#ifdef RXCACHE_TABLE
    iNumXRequests++;

    hExpr = mstr_get_hash(expr);
    h = RxStrHash(hExpr);
    pHash = xtable[h];

    /* Look for a ready-compiled regexp */
    if (pHash != NULL
     && pHash->pString != NULL
     && pHash->hString == hExpr
     && pHash->base.opt == (opt & ~(RE_PACKAGE_MASK))
     && mstreq(pHash->pString, expr)
       )
    {
        iNumXFound++;

        /* Regexp found, but it may not have been compiled for us yet.
         */
#ifdef HAS_PCRE
        if ((opt & RE_PCRE) && !pHash->base.pProg)
        {
            pHash->size = rx_compile_pcre(expr, opt, from_ed, &(pHash->base));
            if (!pHash->size)
                return NULL;
        }
#endif // HAS_PCRE
        if ((opt & RE_TRADITIONAL) && !pHash->base.rx)
        {
            if (!rx_compile_re(expr, opt, from_ed, &(pHash->base)))
                return NULL;
        }
        return ref_regdata(&(pHash->base));
    }
#endif

    /* Regexp not found: compile a new one.
     */

    pcre_size = 0;
    memset(&rdata, 0, sizeof(rdata));

#ifdef HAS_PCRE
    if (opt & RE_PCRE)
    {
        pcre_size = rx_compile_pcre(expr, opt, from_ed, &rdata);
        if (!pcre_size)
        {
            return NULL;
        }
    }
#endif // HAS_PCRE

    if (opt & RE_TRADITIONAL)
    {
        if (!rx_compile_re(expr, opt, from_ed, &rdata))
        {
            return NULL;
        }
    }

#ifndef RXCACHE_TABLE

    /* Wrap up the new regular expression and return it */
    {
        regdata_t * rc;

        rc = xalloc(sizeof(*rc));
        if (!rc)
        {
            rx_free_subdata(&rdata);
            outofmem(sizeof(*rc), "Regexp data structure");
            return NULL;
        }

        memcpy(rc, &rdata, sizeof(*rc));

        rc->ref = 1;
        rc->opt = opt & ~(RE_PACKAGE_MASK);
        return rc;
    }
#else

    /* Wrap up the new regular expression and enter it into the table */
    expr = make_tabled_from(expr); /* for faster comparisons */

    if (NULL != pHash)
    {
        iNumXCollisions++;
        iNumXEntries--;
        iXSizeAlloc -= sizeof(*pHash) + pHash->size;
        free_regdata(&(pHash->base));
    }

    pHash = xalloc(sizeof(*pHash));
    if (!pHash)
    {
        rx_free_subdata(&rdata);
        outofmem(sizeof(*pHash), "Regexp cache structure");
        return NULL;
    }
    xtable[h] = pHash;

    memcpy(&(pHash->base), &rdata, sizeof(pHash->base));

    pHash->base.ref = 1;
    pHash->base.opt = opt & ~(RE_PACKAGE_MASK);
    pHash->pString = expr; /* refs are transferred */
    pHash->hString = hExpr;
    pHash->size = pcre_size;

    iNumXEntries++;
    iXSizeAlloc += sizeof(*pHash) + pHash->size;

    return ref_regdata((regdata_t *)pHash);
#endif /* RXCACHE_TABLE */
} /* rx_compile_data() */

/*--------------------------------------------------------------------*/
regexp_t *
rx_compile (string_t * expr, int opt, Bool from_ed)

/* Compile a regexp structure from the expression <expr>, according
 * to the options in <opt>. If <from_ed> is TRUE, the RE is used
 * from the editor, so error messages will be printed directly
 * to the user.
 *
 * If possible, take a ready-compiled structure from the hashtable,
 * else enter the newly compiled structure into the table.
 *
 * The caller gets his own unique regexp structure, which he has 
 * to free_regexp() after use.
 */

{
    regdata_t * pData;
    regexp_t * pRegexp = NULL;

    /* Determine for which package to compile */
    if (!(opt & RE_PACKAGE_MASK))
    {
        if (driver_hook[H_REGEXP_PACKAGE].u.number)
            opt |= driver_hook[H_REGEXP_PACKAGE].u.number;
        else
            opt |= regex_package;
    }

    if (!(opt & RE_PACKAGE_MASK))
    {
        if (from_ed)
            add_message("Missing specification of which regexp package to use.\n");
        else
            errorf("Missing specification of which regexp package to use.\n");
        return NULL;
    }
#ifndef HAS_PCRE
    if (opt & RE_PCRE)
    {
        if (from_ed)
            add_message("Wrong specification of which regexp package to use: PCREs not available.\n");
        else
            errorf("Wrong specification of which regexp package to use: PCREs not available.\n");
        return NULL;
    }
#endif

    pData = rx_compile_data(expr, opt, from_ed);
    if (pData)
    {
        pRegexp = xalloc(sizeof(*pRegexp));
        if (pRegexp == NULL)
        {
            free_regdata(pData);
            outofmem(sizeof(*pRegexp), "regexp structure");
            return NULL;
        }

        pRegexp->opt = opt;
        pRegexp->data = pData;
    }

    return pRegexp;
} /* rx_compile() */

/*-------------------------------------------------------------------------*/
int
rx_exec (regexp_t *pRegexp, string_t * string, size_t start)

/* Match the regexp <pRegexp> against the <string>, starting the match
 * at the position <start>.
 *
 * Return a positive number if pattern matched, 0 if it did not match,
 * or a negative error code (this can be printed with rx_error_message()).
 */

{
    regdata_t * prog = pRegexp->data;

#ifdef HAS_PCRE
    if (pRegexp->opt & RE_PCRE)
    {
        int rc;
        int pcre_opt;
        pcre_extra * pHints;    /* Study data */

        /* Determine the RE compilation options */

        pcre_opt = 0;
        if (prog->opt & RE_ANCHORED) pcre_opt |= PCRE_ANCHORED;
        if (prog->opt & RE_NOTBOL)   pcre_opt |= PCRE_NOTBOL;
        if (prog->opt & RE_NOTEOL)   pcre_opt |= PCRE_NOTEOL;
        if (prog->opt & RE_NOTEMPTY) pcre_opt |= PCRE_NOTEMPTY;

        pHints = prog->pHints;
        // If LD_PCRE_RECURSION_LIMIT is defined we set a limit for match.
        // TODO: make LD_PCRE_RECURSION_LIMIT a run-time configurable setting
#if LD_PCRE_RECURSION_LIMIT > 0
        pHints->flags |= PCRE_EXTRA_MATCH_LIMIT_RECURSION;
        pHints->match_limit_recursion = LD_PCRE_RECURSION_LIMIT;
#else
        pHints->flags &= ~PCRE_EXTRA_MATCH_LIMIT_RECURSION
#endif  /* LD_PCRE_RECURSION_LIMIT */

        rc = pcre_exec( prog->pProg, pHints
                      , get_txt(string), mstrsize(string), start, pcre_opt
                      , prog->pSubs, prog->num_subs
                      );
        prog->res = rc;

        /* Reverse the roles of return codes 0 (not enough entries in subs[])
         * and PCRE_ERROR_NOMATCH.
         */
        if (rc == PCRE_ERROR_NOMATCH) rc = 0;
        else if (rc == 0) rc = PCRE_ERROR_NOMATCH;

        return rc;
    } /* if (use pcre) */
#endif // HAS_PCRE

    /* Fallback: Traditional regexp */
    return hs_regexec(prog->rx, get_txt(string)+start, get_txt(string));
} /* rx_exec() */

/*-------------------------------------------------------------------------*/
int
rx_exec_str (regexp_t *pRegexp, char * string, char * start)

/* Match the regexp <pRegexp> against the <string> whose real begin
 * is at <start>.
 *
 * Return a positive number if pattern matched, 0 if it did not match,
 * or a negative error code (this can be printed with rx_error_message()).
 *
 * This method is used by ed().
 */

{
    regdata_t * prog = pRegexp->data;
#ifdef HAS_PCRE
    if (pRegexp->opt & RE_PCRE)
    {
        int rc;
        int pcre_opt;
        pcre_extra * pHints;    /* Study data */

        /* Determine the RE compilation options */

        pcre_opt = 0;
        if (prog->opt & RE_ANCHORED) pcre_opt |= PCRE_ANCHORED;
        if (prog->opt & RE_NOTBOL)   pcre_opt |= PCRE_NOTBOL;
        if (prog->opt & RE_NOTEOL)   pcre_opt |= PCRE_NOTEOL;
        if (prog->opt & RE_NOTEMPTY) pcre_opt |= PCRE_NOTEMPTY;

        pHints = prog->pHints;
        // If LD_PCRE_RECURSION_LIMIT is defined we set a limit for match.
        // TODO: make LD_PCRE_RECURSION_LIMIT a run-time configurable setting
#if LD_PCRE_RECURSION_LIMIT > 0
        pHints->flags |= PCRE_EXTRA_MATCH_LIMIT_RECURSION;
        pHints->match_limit_recursion = LD_PCRE_RECURSION_LIMIT;
#else
        pHints->flags &= ~PCRE_EXTRA_MATCH_LIMIT_RECURSION
#endif  /* LD_PCRE_RECURSION_LIMIT */

        rc = pcre_exec( prog->pProg, pHints
                      , start, strlen(start), string - start, pcre_opt
                      , prog->pSubs, prog->num_subs
                      );
        prog->res = rc;

        /* Reverse the roles of return codes 0 (not enough entries in subs[])
         * and PCRE_ERROR_NOMATCH.
         */
        if (rc == PCRE_ERROR_NOMATCH) rc = 0;
        else if (rc == 0) rc = PCRE_ERROR_NOMATCH;

        return rc;
    } /* if (use pcre) */
#endif // HAS_PCRE

    /* Fallback: Traditional regexp */
    return hs_regexec(prog->rx, string, start);
} /* rx_exec_str() */

/*-------------------------------------------------------------------------*/
int
rx_num_matches (regexp_t *pRegexp)

/* After a successful match of <pRegexp>, return the number of matched
 * expressions and parenthesized expressions.
 * In other words: the result is at least 1 (for the fully matched
 * expressions), plus the number of matched '()' sub expressions.
 *
 * The function tries to detect when it is called after an unsuccessful
 * match and return 0 in that case.
 */

{
#ifdef HAS_PCRE
    if (pRegexp->opt & RE_PCRE)
    {
        return pRegexp->data->res >= 1 ? pRegexp->data->res : 0;
    } /* if (use pcre) */
#endif

    /* Fallback: Traditional regexp */
    {
        regdata_t * prog = pRegexp->data;
        p_uint num, i;

        for (num = 0, i = 0
            ; i < sizeof(prog->rx->startp) / sizeof(prog->rx->startp[0])
            ; i++)
        {
            if (prog->rx->startp[i] != NULL
             && prog->rx->endp[i] != NULL
               )
                num++;
        }

        return num;
    }
} /* rx_num_matches() */

/*-------------------------------------------------------------------------*/
Bool
rx_get_match_n (regexp_t *pRegexp, string_t * str, int n, size_t * start, size_t * end)

/* After a successful match of <pRegexp> against <str>, store the start
 * and end position of the match <n> in *<start> and *<end> and retuern TRUE.
 * The end position is in fact the position of the first character after
 * the match.
 *
 * <n> is 0 for the whole expression, and any positive number for the
 * matched subexpressions.
 *
 * If the requested match does not exist, *<start> and *<end> are both
 * set to 0 and the function returns FALSE.
 */

{
    Bool rc;
    regdata_t * prog = pRegexp->data;

#ifdef HAS_PCRE
    if (pRegexp->opt & RE_PCRE)
    {
        if (n < 0
         || n >= prog->res
         || prog->pSubs[2*n] < 0
         || prog->pSubs[2*n+1] < 0
           )
        {
            *start = 0;
            *end = 0;
            rc = MY_FALSE;
        }
        else
        {
            *start = (size_t)prog->pSubs[2*n];
            *end = (size_t)prog->pSubs[2*n+1];
            rc = MY_TRUE;
        }
        return rc;
    } /* if (use pcre) */
#endif // HAS_PCRE

    /* Fallback: Traditional regexp */
    if (n < 0
     || n >= (int)(sizeof(prog->rx->startp) / sizeof(prog->rx->startp[0]))
     || prog->rx->startp[n] == NULL
     || prog->rx->endp[n] == NULL
       )
    {
        *start = 0;
        *end = 0;
        rc = MY_FALSE;
    }
    else
    {
        *start = prog->rx->startp[n] - get_txt(str);
        *end = prog->rx->endp[n] - get_txt(str);
        rc = MY_TRUE;
    }

    return rc;
} /* rx_get_match_n() */

/*-------------------------------------------------------------------------*/
void
rx_get_match (regexp_t *pRegexp, string_t * str, size_t * start, size_t * end)

/* After a successful match of <pRegexp> against <str>, return the start
 * and end position of the match in *<start> and *<end>. The end
 * position is in fact the position of the first character after the match.
 */

{
    regdata_t * prog = pRegexp->data;

#ifdef HAS_PCRE
    if (pRegexp->opt & RE_PCRE)
    {
        *start = (size_t)prog->pSubs[0];
        *end = (size_t)prog->pSubs[1];
        return;
    } /* if (use pcre) */
#endif // HAS_PCRE

    /* Fallback: Traditional regexp */
    *start = prog->rx->startp[0] - get_txt(str);
    *end = prog->rx->endp[0] - get_txt(str);
} /* rx_get_match() */

/*-------------------------------------------------------------------------*/
void
rx_get_match_str (regexp_t *pRegexp, char * str, size_t * start, size_t * end)

/* After a successful match of <pRegexp> against <str>, return the start
 * and end position of the match in *<start> and *<end>. The end
 * position is in fact the position of the first character after the match.
 */

{
    regdata_t * prog = pRegexp->data;

#ifdef HAS_PCRE
    if (pRegexp->opt & RE_PCRE)
    {
        *start = (size_t)prog->pSubs[0];
        *end = (size_t)prog->pSubs[1];
        return;
    } /* if (use pcre) */
#endif // HAS_PCRE

    /* Fallback: Traditional regexp */
    *start = prog->rx->startp[0] - str;
    *end = prog->rx->endp[0] - str;
} /* rx_get_match_str() */

/*-------------------------------------------------------------------------*/
string_t *
rx_sub (regexp_t *pRegexp, string_t *source, string_t *subst)

/* <pRegexp> describes a regexp match in string <source>. Take the
 * replacement string <subst> and substitute any matched subparentheses.
 * The result is a new string with one reference.
 *
 * Returns NULL when out of memory.
 */

{
    Bool   copyPass;   /* Pass indicator */
    size_t len;        /* Computed length of the result */
    string_t * result; /* Result string */
    regdata_t * prog = pRegexp->data;

    result = NULL;

    /* Make two passes over the the string: one to compute the size
     * of the result, the second to create the result.
     */
    copyPass = MY_FALSE;
    len = 0;
    do
    {
        char * src;
        char * dst = NULL;
        size_t left;

        left = mstrsize(subst);
        src = get_txt(subst);
        if (copyPass)
            dst = get_txt(result);

        while (left-- > 0)
        {
            int no;
            char c;

            c = *src++;
            if (c == '&')
                no = 0;
            else if (c == '\\' && '0' <= *src && *src <= '9')
            {
                no = *src++ - '0';
                left--;
            }
            else
                no = -1;

            if (no < 0) /* Ordinary character. */
            {
                if (c == '\\' && (*src == '\\' || *src == '&'))
                {
                    c = *src++;
                    left--;
                }
                if (copyPass)
                    *dst++ = c;
                else
                    len++;
            }
            else
            {
#ifdef HAS_PCRE
                if (pRegexp->opt & RE_PCRE)
                {
                    if (no < prog->res
                     && prog->pSubs[2*no] != -1
                     && prog->pSubs[2*no+1] != -1
                       )
                    {
                        size_t start = (size_t)prog->pSubs[2*no];
                        size_t sublen = (size_t)prog->pSubs[2*no+1] - start;

                        if (copyPass)
                        {
                            memcpy(dst, get_txt(source)+start, sublen);
                            dst += sublen;
                        }
                        else
                        {
                            len += sublen;
                        }
                    }
                } /* if (use pcre) */
#endif // HAS_PCRE

                if (pRegexp->opt & RE_TRADITIONAL)
                {
                    if (no < (int)(sizeof(prog->rx->startp) / sizeof(prog->rx->startp[0]))
                     && prog->rx->startp[no] != NULL
                     && prog->rx->endp[no] != NULL
                       )
                    {
                        size_t sublen = prog->rx->endp[no] - prog->rx->startp[no];

                        if (copyPass)
                        {
                            memcpy(dst, prog->rx->startp[no], sublen);
                            dst += sublen;
                        }
                        else
                        {
                            len += sublen;
                        }
                    }
                } /* if (use traditional) */
            }
        } /* while(left-- > 0) */

        /* End of pass */
        if (!copyPass)
        {
            result = alloc_mstring(len);
            if (!result)
                return NULL;
        }
        copyPass = !copyPass;
    } while (copyPass);

    return result;
} /* rx_sub() */

/*-------------------------------------------------------------------------*/
string_t *
rx_sub_str (regexp_t *pRegexp, char *source, char *subst)

/* <pRegexp> describes a regexp match in string <source>. Take the
 * replacement string <subst> and substitute any matched subparentheses.
 * The result is a new string with one reference.
 *
 * Returns NULL when out of memory.
 */

{
    string_t *m_source;
    string_t *m_subst;
    string_t *rc;

    m_source = new_mstring(source);
    if (!m_source)
    {
        return NULL;
    }

    m_subst = new_mstring(subst);
    if (!m_subst)
    {
        free_mstring(m_source);
        return NULL;
    }
    rc = rx_sub(pRegexp, m_source, m_subst);
    free_mstring(m_source);
    free_mstring(m_subst);
    return rc;
} /* rx_sub_str() */

/*-------------------------------------------------------------------------*/
Bool
rx_reganch (regexp_t *pRegexp)
  
/* If <pRegexp> is a traditional regexp, return TRUE if rx->reganch
 * is not NULL. If <pRegexp> is a PCRE, always return FALSE.
 *
 * Used by ed.c
 */

{
    if (pRegexp->opt & RE_PCRE)
        return MY_FALSE;

    /* Traditional regexp */
    return pRegexp->data->rx->reganch != 0;
} /* rx_reganch() */

/*--------------------------------------------------------------------*/
static void
rx_free_subdata (regdata_t * expr)

/* Deallocate all associated data in regdata structure <expr>.
 */

{
#ifdef HAS_PCRE
    if (expr->pSubs)  xfree(expr->pSubs);  expr->pSubs = NULL;
    if (expr->pHints) xfree(expr->pHints); expr->pHints = NULL;
    if (expr->pProg)  xfree(expr->pProg);  expr->pProg = NULL;
#endif // HAS_PCRE
    if (expr->rx)     xfree(expr->rx);     expr->rx = NULL;
} /* rx_free_subdata() */

/*--------------------------------------------------------------------*/
static void
rx_free_data (regdata_t * expr)

/* Deallocate a regdata structure <expr> and all associated data.
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
    rx_free_subdata(expr);
    xfree(expr);
} /* rx_free_data() */

/*--------------------------------------------------------------------*/
void
free_regexp (regexp_t * expr)

/* Deallocate a regexp structure <expr> and all associated data.
 */

{
    free_regdata(expr->data);
    xfree(expr);
} /* free_regexp() */

/*--------------------------------------------------------------------*/
size_t
rxcache_status (strbuf_t *sbuf, Bool verbose)

/* Gather (and optionally print) the statistics from the rxcache.
 * Return the amount of memory used.
 */

{
#ifdef RXCACHE_TABLE

    statcounter_t iNumXReq;  /* Number of rx_compile() requests, made non-zero */

#if defined(__MWERKS__) && !defined(WARN_ALL)
#    pragma warn_largeargs off
#endif

    /* In verbose mode, print the statistics */
    if (verbose)
    {
        strbuf_add(sbuf, "\nRegexp cache status:\n");
        strbuf_add(sbuf,   "--------------------\n");
        strbuf_addf(sbuf, "Expressions in cache:  %"PRIu32" (%.1f%%)\n"
                   , iNumXEntries, 100.0 * (float)iNumXEntries / RXCACHE_TABLE);
        strbuf_addf(sbuf, "Memory allocated:      %"PRIu32"\n", iXSizeAlloc);
        iNumXReq = iNumXRequests ? iNumXRequests : 1;
        strbuf_addf(sbuf
               , "Requests: %"PRIuSTATCOUNTER" - Found: %"PRIuSTATCOUNTER" (%.1f%%) - "
               "Coll: %"PRIu32" (%.1f%% req/%.1f%% entries)\n"
               , iNumXRequests, iNumXFound, 100.0 * (float)iNumXFound/(float)iNumXReq
               , iNumXCollisions, 100.0 * (float)iNumXCollisions/(float)iNumXReq
               , 100.0 * (float)iNumXCollisions/(iNumXEntries ? iNumXEntries : 1)
               );
    }
    else
    {
        strbuf_addf(sbuf, "Regexp cache:\t\t\t%8"PRId32" %9"PRIu32"\n",
            iNumXEntries, iXSizeAlloc);
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
clear_regexp_ref (regexp_t * pRegexp)

/* Clear the refcount for <pRegexp>.
 */

{
    if (pRegexp)
        pRegexp->data->ref = 0;
} /* clear_regexp_ref() */

/*--------------------------------------------------------------------*/
void
count_regdata_ref (regdata_t * pRegexp)

/* Mark all memory associated with one regexp structure and count
 * the refs.
 * This function is called both from rxcache as well as from ed.
 */

{
    note_malloced_block_ref(pRegexp);
#ifdef HAS_PCRE
    if (pRegexp->pProg)
    {
        note_malloced_block_ref(pRegexp->pProg);
        if (pRegexp->pHints)
            note_malloced_block_ref(pRegexp->pHints);
        if (pRegexp->pSubs)
            note_malloced_block_ref(pRegexp->pSubs);
    }
#endif // HAS_PCRE
    if (pRegexp->rx)
        note_malloced_block_ref(pRegexp->rx);
#ifdef RXCACHE_TABLE
    count_ref_from_string(((RxHashEntry *)pRegexp)->pString);
#endif
    pRegexp->ref++;
} /* count_regdata_ref() */

/*--------------------------------------------------------------------*/
void
count_regexp_ref (regexp_t * pRegexp)

/* Mark all memory associated with one regexp structure and count
 * the refs.
 * This function is called both from rxcache as well as from ed.
 */

{
    note_malloced_block_ref(pRegexp);
    count_regdata_ref(pRegexp->data);
} /* count_regexp_ref() */

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
            count_regdata_ref((regdata_t *)xtable[i]);
        }
    } /* for (i) */
#endif

} /* count_rxcache_refs() */

#endif /* if GC_SUPPORT */

/*====================================================================*/

