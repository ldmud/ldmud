#ifndef _RXCACHE_H_
#define _RXCACHE_H_

#include "config.h"
#include "regexp.h"

#ifdef RXCACHE_TABLE

extern void rxcache_init(void);
extern regexp * regcomp_cache(char * expr, int excompat);
extern int rxcache_status (int verbose);
extern regexp * rx_dup (regexp *);
extern void rx_free (regexp *);

#if defined(MALLOC_smalloc)
extern void clear_rxcache_refs (void);
extern void count_rxcache_refs (void);
extern void count_rxcache_ref (regexp *);
#endif /* if MALLOC_smalloc */

#define REGCOMP(x,y)   regcomp_cache(x,y)
#define RX_DUP(x)      rx_dup(x)
#define REGFREE(x)     rx_free(x)

#else

#define REGCOMP(x,y)   regcomp(x,y)
#define RX_DUP(x)      (x)
#define REGFREE(x)     xfree((char*)x)

#endif /* if RXCACHE_TABLE */

#endif /* _RXCACHE_H_ */
