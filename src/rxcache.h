#ifndef RXCACHE_H_
#define RXCACHE_H_

#include "driver.h"

#include "regexp.h"
#include "mstrings.h"
#include "strfuns.h"
#ifdef USE_PCRE
#include "pcre/pcre.h"
#endif

/* --- Types --- */

/* --- struct regexp_s: the regexp structure wrapper ---
 *
 * This structure wraps the actual regexp structure into a common
 * format.
#ifdef RXCACHE_TABLE
 * The regexp cache embeds this structure into a larger one which
 * it uses to manage the cached expressions.
#endif
 */

struct regexp_s {
    p_uint ref;      /* Number of refs */
    Bool   from_ed;  /* TRUE: Print error msgs directly to user */
    int    opt;      /* Additional options */
#ifdef USE_PCRE
    pcre        * pProg;     /* The generated regular expression */
    pcre_extra  * pHints;    /* Study data */
    int           res;       /* Result of last rx_exec() */
    int           num_subs;  /* Number of elements in pSubs */
    int         * pSubs;     /* Substring offsets + workarea */
#else
    regexp * rx;  /* The actual regular expression */
#endif
};

/* --- Variables --- */

/* --- Macros --- */

/* regexp_t *ref_regexp(regexp_t *r)
 *   Add another ref to regexp <r> and return the regexp <r>.
 */

#define ref_regexp(r) ((r)->ref++, (r))

/* void free_regexp(regexp_t *r)
 *   Subtract one ref from regexp <r>, and free the regexp fully if
 *   the refcount reaches zero.
 */

#define free_regexp(r) MACRO( if (--((r)->ref) <= 0) rx_free(r); )

/* p_int deref_regexp(regexp_t *r)
 *   Subtract one ref from regexp <r>, but don't check if it needs to
 *   be freed. Result is the number of refs left.
 */

#define deref_regexp(r) (--(r)->ref)

/* --- Prototypes --- */

extern void rx_init(void);
extern const char * rx_error_message (int code);
extern regexp_t * rx_compile (string_t * expr, int opt, Bool from_ed);
extern int    rx_exec (regexp_t *prog, string_t * string, size_t start);
extern int    rx_exec_str (regexp_t *prog, char * string, char * start);
extern char * rx_sub (regexp_t *prog, string_t *source, char *dest, int n, Bool quiet);
extern char * rx_sub_str (regexp_t *prog, char *source, char *dest, int n, Bool quiet);
extern void rx_get_match (regexp_t *prog, string_t * str, size_t * start, size_t * end);
extern void rx_get_match_str (regexp_t *prog, char * str, size_t * start, size_t * end);
extern void   rx_free(regexp_t *);
extern size_t rxcache_status(strbuf_t *sbuf, Bool verbose);
extern void   rxcache_dinfo_status(svalue_t *svp, int value);

#if defined(GC_SUPPORT)
extern void clear_rxcache_refs(void);
extern void count_rxcache_refs(void);
extern void count_regexp_ref(regexp_t *);
#endif /* if GC_SUPPORT */

#endif /* RXCACHE_H_ */
