/*------------------------------------------------------------------
 * Regular Expression Wrapper and Cache
 * Written 1998, 2002 by Lars Duening.
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
#include "mstrings.h"
#ifdef USE_PCRE
#include "pkg-pcre.h"
#else
#include "regexp.h"
#endif
#include "simulate.h"
#include "strfuns.h"
#include "svalue.h"
#include "xalloc.h"

#include "../mudlib/sys/debug_info.h"
#include "../mudlib/sys/regexp.h"

/*--------------------------------------------------------------------*/

/* --- struct regdata_s: the regexp structure wrapper ---
 *
 * This structure wraps the actual regexp structure into a common
 * format.
#ifdef RXCACHE_TABLE
 * The regexp cache embeds this structure into a larger one which
 * it uses to manage the cached expressions.
#endif
 */

typedef struct regdata_s {
    p_uint        ref;       /* Number of refs */
    int           opt;       /* Additional options */
#ifdef USE_PCRE
    pcre        * pProg;     /* The generated regular expression */
    pcre_extra  * pHints;    /* Study data */
    int           num_subs;  /* Number of elements in pSubs */
    int         * pSubs;     /* Substring offsets + workarea */
    int           res;       /* Result of last rx_exec() */
#else
    regexp * rx;  /* The actual regular expression */
#endif
} regdata_t;

#ifdef RXCACHE_TABLE

/* --- struct RxHashEntry: One expression hashtable entry ---
 * Derived from regexp_i_t
 */

typedef struct RxHashEntry {
    regdata_t base;      /* The base regexp_t structure */

    string_t * pString;  /* Generator string, a counted tabled string
                          * NULL if unused */
    whash_t    hString;  /* Hash of pString */
    size_t     size;     /* Size of regexp expressions for statistics */
} RxHashEntry;

#endif /* RXCHACHE_TABLE */

/* --- struct regexp_s: the regexp structure pimpl ---
 *
 * This structure is a decorated pointer to the regdata_t structure,
 * allowing to store unique options and data for shared regexps.
 */

struct regexp_s {
    int         opt;   /* Additional options */
    regdata_t * data;  /* Counted pointer to the internal structure */
};

/*--------------------------------------------------------------------*/

/* --- Variables --- */

#ifdef RXCACHE_TABLE

static RxHashEntry * xtable[RXCACHE_TABLE];  /* The Expression Hashtable */

/* Expression cache statistics */
static uint32 iNumXRequests   = 0;  /* Number of calls to rx_compile() */
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
/* --- Macros --- */

/* regdata_t *ref_regdata(regdata_t *r)
 *   Add another ref to regdata <r> and return the regdata <r>.
 */

#define ref_regdata(r) ((r)->ref++, (r))

/* void free_regdata(regdata_t *r)
 *   Subtract one ref from regdata <r>, and free the regdata fully if
 *   the refcount reaches zero.
 */

static void rx_free_data (regdata_t * expr); /* forward */

#define free_regdata(r) MACRO( if (--((r)->ref) <= 0) rx_free_data(r); )

#ifdef RXCACHE_TABLE

/* Hash functions, inspired by interpret.c */

#if !( (RXCACHE_TABLE) & (RXCACHE_TABLE)-1 )
#define RxStrHash(s) ((s) & ((RXCACHE_TABLE)-1))
#else
#define RxStrHash(s) ((s) % RXCACHE_TABLE)
#endif

#endif /* RXCHACHE_TABLE */

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
const char *
rx_pcre_version (void)

/* Return a string with the name and version of the PCRE package.
 */

{
    static char buf[40];
    sprintf(buf, "%d.%d", PCRE_MAJOR, PCRE_MINOR);
#    ifdef USE_BUILTIN_PCRE
    strcat(buf, " (builtin)");
#    endif
    return buf;
} /* rx_pcre_version() */

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
const char *
rx_error_message (int code
#ifndef USE_PCRE
                           UNUSED
#endif
                 )

/* Return a constant string with the error message for <code>.
 * If <code> is not an error, NULL is returned.
 */

{
#ifdef USE_PCRE
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
#else
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
#endif
}  /* rx_error_message() */

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
#ifdef USE_PCRE
    const char * pErrmsg;
#else
    char       * pErrmsg;
#endif
    int          erridx;
#ifdef USE_PCRE
    pcre       * pProg;     /* The generated regular expression */
    pcre_extra * pHints;    /* Study data */
    int        * pSubs;     /* Capturing and work area */
    int          pcre_opt;  /* <opt> translated into PCRE opts */
    int          num_subs;  /* Number of capturing parentheses */
#else
    regexp * pRegexp;
#endif

#ifdef RXCACHE_TABLE
    whash_t hExpr;
    int h;
    RxHashEntry *pHash;
#endif

    /* Sanitize the <opt> value */
#ifdef USE_PCRE
    opt = opt & ~(RE_EXCOMPATIBLE);

    /* Determine the RE compilation options */

    pcre_opt = 0;
    if (opt & RE_CASELESS)       pcre_opt |= PCRE_CASELESS;
    if (opt & RE_MULTILINE)      pcre_opt |= PCRE_MULTILINE;
    if (opt & RE_DOTALL)         pcre_opt |= PCRE_DOTALL;
    if (opt & RE_EXTENDED)       pcre_opt |= PCRE_EXTENDED;
    if (opt & RE_DOLLAR_ENDONLY) pcre_opt |= PCRE_DOLLAR_ENDONLY;
    if (opt & RE_UNGREEDY)       pcre_opt |= PCRE_UNGREEDY;
#else
    opt = opt & RE_EXCOMPATIBLE;
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
#ifdef USE_PCRE
     && pHash->base.opt == opt
#endif
     && mstreq(pHash->pString, expr)
       )
    {
        iNumXFound++;
        return ref_regdata((regdata_t *)pHash);
    }
#endif

    /* Regexp not found: compile a new one.
     */
#ifdef USE_PCRE
    pcre_malloc_size = 0;
    pcre_malloc_err  = "compiling regex";
    pProg = pcre_compile(get_txt(expr), pcre_opt, &pErrmsg, &erridx, NULL);

    if (NULL == pProg)
    {
        if (from_ed)
            add_message("pcre: %s at offset %d\n", pErrmsg, erridx);
        else
            error("pcre: %s at offset %d\n", pErrmsg, erridx);
        return NULL;
    }

    pcre_malloc_err  = "studying regex";
    pHints = pcre_study(pProg, 0, &pErrmsg);

    if (pErrmsg)
    {
        xfree(pProg);
        if (from_ed)
            add_message("pcre: %s\n", pErrmsg);
        else
            error("pcre: %s\n", pErrmsg);
        return NULL;
    }

    {
       int rc;

        rc = pcre_fullinfo(pProg, pHints, PCRE_INFO_CAPTURECOUNT, &num_subs);
        if (rc != 0)
        {
            xfree(pProg);
            xfree(pHints);
            if (from_ed)
                add_message("pcre: %s\n", rx_error_message(rc));
            else
                error("pcre: %s\n", rx_error_message(rc));
            return NULL;
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
#else
    pRegexp = hs_regcomp((unsigned char *)get_txt(expr)
                        , opt & RE_EXCOMPATIBLE
                        , &pErrmsg, &erridx);
    if (NULL == pRegexp)
    {
        if (from_ed)
            add_message("re: %s at offset %d\n", pErrmsg, erridx);
        else
            error("re: %s at offset %d\n", pErrmsg, erridx);
        return NULL;
    }
#endif

#ifndef RXCACHE_TABLE

    /* Wrap up the new regular expression and return it */
    {
        regdata_t * rc;

        xallocate(rc, sizeof(*rc), "Regexp data structure");
        rc->ref = 1;
        rc->opt = opt;
#ifdef USE_PCRE
        rc->pProg = pProg;
        rc->pHints = pHints;
        rc->pSubs = pSubs;
        rc->num_subs = num_subs;
        rc->res = 0;
#else
        rc->rx = pRegexp;
#endif
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
        free_regexp((regexp_t *)pHash);
    }

    xallocate(pHash, sizeof(*pHash), "Regexp cache structure");
    xtable[h] = pHash;

    pHash->base.ref = 1;
    pHash->base.opt = opt;
    pHash->pString = expr; /* refs are transferred */
    pHash->hString = hExpr;
#ifdef USE_PCRE
    pHash->base.pProg = pProg;
    pHash->base.pHints = pHints;
    pHash->base.pSubs = pSubs;
    pHash->base.num_subs = num_subs;
    pHash->base.res = 0;
    pHash->size = pcre_malloc_size;
#else
    pHash->base.rx = pRegexp;
    pHash->size = pRegexp->regalloc;
#endif

    iNumXEntries++;
    iXSizeAlloc += sizeof(*pHash) + pHash->size;

    return ref_regdata((regdata_t *)pHash);
#endif
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
    regdata_t * pData = rx_compile_data(expr, opt, from_ed);
    regexp_t * pRegexp = NULL;

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

#ifdef USE_PCRE
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
    if (pHints && max_eval_cost)
    {
        pHints->flags |= PCRE_EXTRA_MATCH_LIMIT;
        if (max_eval_cost > eval_cost + 1)
            pHints->match_limit = max_eval_cost - eval_cost - 1;
        else
            pHints->match_limit = 1;
    }
    else if (pHints)
    {
        pHints->flags &= ~PCRE_EXTRA_MATCH_LIMIT;
    }

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
#else
    return hs_regexec(prog->rx, get_txt(string)+start, get_txt(string));
#endif
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

#ifdef USE_PCRE
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
    if (pHints && max_eval_cost)
    {
        pHints->flags |= PCRE_EXTRA_MATCH_LIMIT;
        if (max_eval_cost > eval_cost + 1)
            pHints->match_limit = max_eval_cost - eval_cost - 1;
        else
            pHints->match_limit = 1;
    }
    else if (pHints)
    {
        pHints->flags &= ~PCRE_EXTRA_MATCH_LIMIT;
    }

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
#else
    return hs_regexec(prog->rx, string, start);
#endif
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
#ifdef USE_PCRE
    return pRegexp->data->res >= 1 ? pRegexp->data->res : 0;
#else
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
#endif
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

#ifdef USE_PCRE
#ifdef __MWERKS__
#    pragma unused(str)
#endif

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
#else
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
#endif

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

#ifdef USE_PCRE
#ifdef __MWERKS__
#    pragma unused(str)
#endif
    *start = (size_t)prog->pSubs[0];
    *end = (size_t)prog->pSubs[1];
#else
    *start = prog->rx->startp[0] - get_txt(str);
    *end = prog->rx->endp[0] - get_txt(str);
#endif
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

#ifdef USE_PCRE
#ifdef __MWERKS__
#    pragma unused(str)
#endif
    *start = (size_t)prog->pSubs[0];
    *end = (size_t)prog->pSubs[1];
#else
    *start = prog->rx->startp[0] - str;
    *end = prog->rx->endp[0] - str;
#endif
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

#ifndef USE_PCRE
#ifdef __MWERKS__
#    pragma unused(source)
#endif
#endif
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
#ifdef USE_PCRE
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
#else
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
#endif
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
#ifdef USE_PCRE
    if (expr->pSubs) xfree(expr->pSubs);
    if (expr->pHints) xfree(expr->pHints);
    xfree(expr->pProg);
#else
    xfree(expr->rx);
#endif
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

    uint32 iNumXReq;  /* Number of rx_compile() requests, made non-zero */

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
#ifdef USE_PCRE
    note_malloced_block_ref(pRegexp->pProg);
    if (pRegexp->pHints)
        note_malloced_block_ref(pRegexp->pHints);
    if (pRegexp->pSubs)
        note_malloced_block_ref(pRegexp->pSubs);
#else
    note_malloced_block_ref(pRegexp->rx);
#endif
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

