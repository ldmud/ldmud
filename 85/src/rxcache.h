#ifndef _RXCACHE_H_
#define _RXCACHE_H_

#include "driver.h"

#include "regexp.h"

#ifdef RXCACHE_TABLE

extern void rxcache_init PROT((void));
extern regexp * regcomp_cache PROT((char * expr, int excompat));
extern int rxcache_status PROT((int verbose));
extern regexp * rx_dup PROT((regexp *));
extern void rx_free PROT((regexp *));

#if defined(MALLOC_smalloc)
extern void clear_rxcache_refs PROT((void));
extern void count_rxcache_refs PROT((void));
extern void count_rxcache_ref PROT((regexp *));
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
