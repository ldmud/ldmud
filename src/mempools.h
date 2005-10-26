#ifndef MEMPOOLS_H__
#define MEMPOOLS_H__ 1

#include <sys/types.h>

#include "typedefs.h" /* strbuf_t */

/* --- Types --- */

/* --- enum membuffer_e: Memory buffer IDs ---
 */

typedef enum membuffer {
    mbFile     /* read_file(), restore_(), read_bytes() */
  , mbSwap     /* Swapper buffer */
  , mbMax
} membuffer_e;

/* --- Mempool, struct mempool_s: a memory pool ---
 *
 * The structure contains no public data.
 */

struct mempool_s;
typedef struct mempool_s mempool_t;
typedef struct mempool_s * Mempool;

/* --- Prototypes --- */

extern Mempool new_mempool (size_t iSize);
extern Mempool new_lifopool (size_t iSize);
extern size_t  size_mempool (size_t elemsize);
extern size_t  size_lifopool (size_t elemsize);
extern void    mempool_depend_on (Mempool pSub, Mempool pSuper);
extern void *  mempool_alloc (Mempool pPool, size_t iSize);
extern void    mempool_free (Mempool pPool, void * adr);
extern void    mempool_reset (Mempool pPool);
extern void    mempool_delete (Mempool pPool);
extern size_t  mempool_size (Mempool pPool);

extern void   mb_init(void);
extern void   mb_release(void);
extern void * mb_alloc(membuffer_e buf, size_t size);
extern void * mb_realloc(membuffer_e buf, size_t size);
extern size_t mb_status (strbuf_t * sbuf, Bool verbose);
extern void   mb_dinfo_status (svalue_t *svp, int value);

#define mb_free(buf) NOOP
  /* Use the above macro to 'free' the memory.
   * While it may redundant, use of the macro improves the readability
   * of the code!
   */

#ifdef GC_SUPPORT

extern void mempool_clear_refs (Mempool pPool);
extern void mempool_note_refs (Mempool pPool);
extern void mb_clear_refs (void);
extern void mb_note_refs (void);

#endif /* GC_SUPPORT */

#endif /* MEMPOOLS_H__ */
