#ifndef MEMPOOLS_H__
#define MEMPOOLS_H__ 1

#include <sys/types.h>

/* --- Types --- */

/* --- Mempool, struct mempool_s: a memory pool ---
 *
 * The structure contains no public data.
 */

struct mempool_s;
typedef struct mempool_s mempool_t;
typedef struct mempool_s * Mempool;

/* --- Prototypes --- */

extern Mempool new_mempool (size_t iSize);
extern Mempool new_fifopool (size_t iSize);
extern size_t  fifopool_size (size_t elemsize, unsigned int num);
extern void    mempool_depend_on (Mempool pSub, Mempool pSuper);
extern void *  mempool_alloc (Mempool pPool, size_t iSize);
extern void    mempool_free (Mempool pPool, void * adr);
extern void    mempool_reset (Mempool pPool);
extern void    mempool_delete (Mempool pPool);
extern size_t  mempool_size (Mempool pPool);

#ifdef GC_SUPPORT
extern void    mempool_clear_refs (Mempool pPool);
extern void    mempool_note_refs (Mempool pPool);
#endif

#endif /* MEMPOOLS_H__ */


