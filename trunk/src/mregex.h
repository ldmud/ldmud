#ifndef MREGEX_H_
#define MREGEX_H_

#include "driver.h"

#include "mstrings.h"
#include "strfuns.h"

/* --- Types --- */

/* --- Variables --- */

/* --- Macros --- */

/* --- Prototypes --- */

extern void rx_init(void);
extern const char * rx_error_message (int code, const regexp_t * pRegexp);
extern regexp_t * rx_compile (string_t * expr, int opt, Bool from_ed);
extern int    rx_exec (regexp_t *pRegexp, string_t * string, size_t start);
extern int    rx_exec_str (regexp_t *pRegexp, char * string, char * start);
extern string_t * rx_sub (regexp_t *pRegexp, string_t *source, string_t *subst);
extern string_t * rx_sub_str (regexp_t *pRegexp, char *source, char *subst);
extern Bool rx_reganch (regexp_t * pRegexp);
extern int rx_num_matches (regexp_t *pRegexp);
extern void rx_get_match (regexp_t *pRegexp, string_t * str, size_t * start, size_t * end);
extern void rx_get_match_str (regexp_t *pRegexp, char * str, size_t * start, size_t * end);
extern Bool rx_get_match_n (regexp_t *pRegexp, string_t * str, int n, size_t * start, size_t * end);
extern void   free_regexp(regexp_t *);
extern const char * rx_pcre_version(void);
extern size_t rxcache_status(strbuf_t *sbuf, Bool verbose);
extern void   rxcache_dinfo_status(svalue_t *svp, int value);

#if defined(GC_SUPPORT)
extern void clear_rxcache_refs(void);
extern void count_rxcache_refs(void);
extern void clear_regexp_ref(regexp_t *);
extern void count_regexp_ref(regexp_t *);
#endif /* if GC_SUPPORT */

#endif /* MREGEX_H_ */
