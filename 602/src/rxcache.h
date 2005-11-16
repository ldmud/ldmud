#ifndef RXCACHE_H_
#define RXCACHE_H_

#include "driver.h"

#include "regexp.h"
#include "strfuns.h"

#ifdef RXCACHE_TABLE

extern void rxcache_init(void);
extern regexp * regcomp_cache(unsigned char * expr, Bool excompat, Bool from_ed);
extern size_t rxcache_status(strbuf_t *sbuf, Bool verbose);
extern void   rxcache_dinfo_status(svalue_t *svp, int value);
extern regexp * rx_dup(regexp *);
extern void rx_free(regexp *);

#if defined(GC_SUPPORT)
extern void clear_rxcache_refs(void);
extern void count_rxcache_refs(void);
extern void count_rxcache_ref(regexp *);
#endif /* if GC_SUPPORT */

#define REGCOMP(x,y,z) regcomp_cache(x,y,z)
#define RX_DUP(x)      rx_dup(x)
#define REGFREE(x)     rx_free(x)

#else

#define REGCOMP(x,y,z) hs_regcomp(x,y,z)
#define RX_DUP(x)      (x)
#define REGFREE(x)     xfree(x)

#endif /* if RXCACHE_TABLE */

#endif /* RXCACHE_H_ */
