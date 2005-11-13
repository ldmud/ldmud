#ifndef _RXCACHE_H_
#define _RXCACHE_H_

#include "config.h"
#include "regexp.h"

#ifdef RXCACHE_TABLE

extern void rxcache_init(void);
extern regexp * regcomp_cache(char * expr, int excompat);
extern int rxcache_status (int verbose);

#if defined(MALLOC_smalloc)
extern void count_rxcache_refs (void);
#endif /* if MALLOC_smalloc */

#endif /* if RXCACHE_TABLE */

#endif /* _RXCACHE_H_ */
