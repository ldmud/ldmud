/*---------------------------------------------------------------------------
 * Specialised Memory Allocators.
 *
 *---------------------------------------------------------------------------
 * Memory buffers provide memory for functions which repeatedly allocate
 * large chunks of temporary memory (restore_object() for example). Instead
 * of deallocating the memory after its use, the memory buffer keeps it
 * around for the next time. This way any interim fragmentation of the free
 * large memory pool won't affect the next use.
 *
 * The driver implements a buffer each for dedicated purposes. Each of
 * these buffers is identified by a number from membuffer_e.
 *---------------------------------------------------------------------------
 * Purpose of memory pools is to provide fast allocation methods for
 * certain allocation patterns. They are most useful for scratchpad
 * purposes where a largish number of small allocations need to be
 * deallocated at once.
 *
 * Mempools: allocation of objects with identical time of death.
 *           Attempts to deallocate single objects have no effect.
 *
 * Lifopools: allocation/deallocation of objects follows (more than less)
 *            a lifo pattern.
 *
 * TODO: A small-block pool, to manage lots of small blocks of equal size
 * TODO:: without the overhead of smalloc. Initialized with the block size,
 * TODO:: the number of initial blocks, and the number of blocks each
 * TODO:: the pool has to grow. These blocks can be freed and are then
 * TODO:: kept in a freelist for later reuse. To help compacting the pool
 * TODO:: a method alloc_before(void *) would allocate a free small block
 * TODO:: before the given one - if there is no such free block, the
 * TODO:: method would return NULL. With this method a user of the
 * TODO:: can compact its data at the front of the pool blocks.
 * TODO:: The last unused pool block is freed only if either another small
 * TODO:: block is freed, or the method flush_free_pool_blocks() (or so)
 * TODO:: is called.
 *
 * Memory pools can be made dependant from other pools. This means that
 * whenever the 'superior' pool is reset or deleted, all pools depending
 * on this superior one are also reset or deleted. This is a
 * 1:n relationship: one pool can have several pools depending on it, but
 * itself can be depending on only one superior pool.
 *
 * The implementation uses one 'mempool' structure to hold the organisational
 * data of the mempool, and several 'memblock' structures providing the
 * actual memory. Memory pool users only get the pointer to their pool (the
 * internals of the mempool structure are private) and have to use it
 * as a handle in all function calls. The memblock structures are used
 * internally only.
 *
 * Every memory pool is created with an 'allocation size' as parameter.
 * This size determines how much memory is available in every memblock.
 * Since Mempools uses an immediate-fit strategy when allocating memory from
 * a pool, it is the responsibility of the caller to chose an block allocation
 * size substantially larger than a typical allocation from the pool.
 * It is possible to allocate memory larger than the allocation size from
 * the pool, in which case the the mempool behaves like a malloc() with
 * (semi)automatic free().
 *
 * The size_xxxpool() utility functions calculate a somewhat optimum
 * pool allocation size based on the element size. The function tries to
 * keep the allocation size under a certain limit in order to avoid running
 * into large block fragmentation (because if that happens, we would be
 * better off not using a memory pool at all).
 *
 * The memory allocated from a mempool is aligned to the size of union align
 * (which is assumed to be a power of 2).
 *---------------------------------------------------------------------------
 */

#include "driver.h"

#include <assert.h>
#include <stddef.h>
#include <stdlib.h>
#include <sys/types.h>

#include "mempools.h"
#include "gcollect.h"
#ifdef DEBUG
#include "simulate.h"
#endif
#include "strfuns.h"
#include "svalue.h"
#include "xalloc.h"

#include "../mudlib/sys/debug_info.h"

/*=========================================================================*/
/*                         M E M B U F F E R S                             */

/*-------------------------------------------------------------------------*/

/* -- struct membuffer_s: one memory buffer -- */

typedef struct membuffer_s {
    void * mem;  /* The allocated memory */
    size_t size;  /* Size of the allocated memory */
} membuffer_t;

static membuffer_t membuffers[mbMax];
  /* The memory buffers.
   */

/*-------------------------------------------------------------------------*/
void
mb_init (void)

/* Initialize the memory buffers.
 */

{
    int i;

    for (i = 0; i < mbMax; i++)
    {
        membuffers[i].mem = NULL;
        membuffers[i].size = 0;
    }
} /* mb_init() */

/*-------------------------------------------------------------------------*/
void
mb_release (void)

/* Free all memory buffers.
 */

{
    int i;

    for (i = 0; i < mbMax; i++)
    {
        if (membuffers[i].mem != NULL)
            xfree(membuffers[i].mem);
        membuffers[i].mem = NULL;
        membuffers[i].size = 0;
    }
} /* mb_release() */

/*-------------------------------------------------------------------------*/
void *
mb_alloc (membuffer_e buf, size_t size)

/* Allocate 'size' bytes of memory for buffer <buf>.
 * Returns NULL when out of memory.
 */

{
#ifdef DEBUG
    if (buf < 0 || buf >= mbMax)
        fatal("mb_alloc: Illegal buf# %d\n", buf);
#endif

    if (membuffers[buf].size >= size)
        return membuffers[buf].mem;
    
    if (membuffers[buf].mem != NULL)
        xfree(membuffers[buf].mem);

    membuffers[buf].mem = xalloc(size);

    if (membuffers[buf].mem != NULL)
        membuffers[buf].size = size;
    else
        membuffers[buf].size = 0;

    return membuffers[buf].mem;
} /* mb_alloc() */

/*-------------------------------------------------------------------------*/
void *
mb_realloc (membuffer_e buf, size_t size)

/* Realloate the memory of buffer <buf> to hold <size> bytes, without
 * losing the current content, and return the new pointer.
 * Returns NULL when out of memory (the old memory block will be unaffected
 * then).
 */

{
    void * mem;

#ifdef DEBUG
    if (buf < 0 || buf >= mbMax)
        fatal("mb_alloc: Illegal buf# %d\n", buf);
#endif

    if (membuffers[buf].size >= size)
        return membuffers[buf].mem;
    
    if (membuffers[buf].mem != NULL)
        mem = rexalloc(membuffers[buf].mem, size);
    else
        mem = xalloc(size);

    if (mem != NULL)
    {
        membuffers[buf].mem = mem;
        membuffers[buf].size = size;
    }

    return mem;
} /* mb_realloc() */

/*-------------------------------------------------------------------------*/
#ifdef GC_SUPPORT

void
mb_clear_refs (void)

/* GC Support: Clear the refs of all memory associated with the
 * memory buffers.
 */

{
    int i;

    for (i = 0; i < mbMax; i++)
    {
        if (membuffers[i].mem != NULL)
            clear_memory_reference(membuffers[i].mem);
    }
} /* mb_clear_refs() */

void
mb_note_refs (void)

/* GC Support: Note the refs of all memory associated with the
 * memory buffers.
 */

{
    int i;

    for (i = 0; i < mbMax; i++)
    {
        if (membuffers[i].mem != NULL)
            note_malloced_block_ref(membuffers[i].mem);
    }
} /* mb_note_refs() */

#endif /* GC_SUPPORT */

/*-------------------------------------------------------------------------*/
size_t
mb_status (strbuf_t * sbuf, Bool verbose)

/* Gather (and optionally print) the statistics from the membuffers.
 * Return the amount of memory used.
 */

{
    size_t res;
    int i;

    for (i = 0, res = 0; i < mbMax; i++)
        res += membuffers[i].size;

#if defined(__MWERKS__) && !defined(WARN_ALL)
#    pragma warn_largeargs off
#endif

    /* In verbose mode, print the statistics */
    if (verbose)
    {
        strbuf_add(sbuf, "\nMemory Buffers:\n");
        strbuf_add(sbuf,   "---------------\n");
        strbuf_addf(sbuf, "File data:    %8zu\n", membuffers[mbFile].size);
        strbuf_addf(sbuf, "Swap buffer:  %8zu\n", membuffers[mbSwap].size);
    }
    else
    {
        strbuf_addf(sbuf, "Memory buffers:\t\t\t\t %9zu\n", res);
    }

    return res;

#if defined(__MWERKS__)
#    pragma warn_largeargs reset
#endif

} /* mb_status() */

/*-------------------------------------------------------------------------*/
void
mb_dinfo_status (svalue_t *svp, int value)

/* Return the rxcache information for debug_info(DINFO_DATA, DID_STATUS).
 * <svp> points to the svalue block for the result, this function fills in
 * the spots for the object table.
 * If <value> is -1, <svp> points indeed to a value block; other it is
 * the index of the desired value and <svp> points to a single svalue.
 */

{
#define ST_NUMBER(which,code) \
    if (value == -1) svp[which].u.number = code; \
    else if (value == which) svp->u.number = code

    ST_NUMBER(DID_ST_MB_FILE, membuffers[mbFile].size);
    ST_NUMBER(DID_ST_MB_SWAP, membuffers[mbSwap].size);

#undef ST_NUMBER
} /* mb_dinfo_status() */

/*=========================================================================*/
/*                           M E M P O O L S                               */

/*-------------------------------------------------------------------------*/

/* --- union align: aligment structure---
 * Combines several basic basic types in order to determined
 * the basic alignment size. This size must be a power of 2, assert()s will
 * check for that.
 */

union align {
    int i;
    long l;
    double d;
    void *p;
};

typedef union align align_t;

/* Round the value x up to the next integral of sizeof(union align)
 */

#define ROUND(x) (((x)+sizeof(align_t)-1) & ~(sizeof(align_t)-1))

/* --- struct memblock_s: manages one block of memory ---
 * Used for the low-level allocation of memory, the various allocators
 * then work within the allocated arena.
 *
 * Mempools fill the arena from the end. pMark marks the break between
 * the (low) unused and (high) used memory, pointing to the first used
 * byte.
 *
 * Lifopools are like Mempools, with the addition that every used
 * block is prepended by a lifo_t structure (allocated to a multiple
 * of the align_t type). pMark points therefore at this structure of the
 * first used block. If a lifo-allocation is freed, it is just marked
 * as free (negative length) unless it is the block pMark points to.
 * In that case, pMark is moved up until it points to a used block,
 * retrieving previously freed blocks as well. When a memblock is
 * totally free, the pool will put it into a freelist for later re-use.
 *
 * The union construct makes sure that data[] is aligned properly.
 */

typedef struct memblock_s * Memblock;

struct memblock_s {
    Memblock pNext;     /* Next memblock in list */
    char    *pMark;     /* End of unused arena */
    size_t   length;    /* Total size of this memblock */
    union {
        align_t a;
        char data[1];   /* Placeholder for data arena */
    } u;
};

#define MEMBLOCK_LIMIT (128)
  /* Maximum size for the userspace of a memory block.
   * Using blocks larger than this is likely to run into fragmentation
   * of the large block heap.
   */

/* --- struct lifo_s: headblock for a lifo-type allocation ---
 *
 * One of these structures, allocated to a multiple of align_t, is
 * prepended to every lifopool allocation. Additionally, the end
 * of every memory block is marked with a 'used' structure as sentinel.
 */

typedef struct lifo_s lifo_t;

struct lifo_s {
    ssize_t length;    /* Size of this block, including this structure.
                        * Positive for allocated blocks, negative for free ones.
                        */
    Memblock pBlock;   /* Backpointer to the memoryblock holding this block */
};

#define SIZEOF_LIFO_T ROUND(sizeof(lifo_t))

/*=========================================================================*/

/*-------------------------------------------------------------------------*/
/* Memory pool types
 */

enum pooltype_u {
    MEMPOOL = 0,
    LIFOPOOL
};

typedef enum pooltype_u pooltype_t;

/*-------------------------------------------------------------------------*/
/* struct mempool_s: the organisational structure of every memory pool.
 */

struct mempool_s {
    pooltype_t type;        /* The type of this pool */
    size_t     iAllocSize;  /* Typical alloc size of a memory block */
    Memblock   pBlocks;     /* List of memory blocks
                             * It is guaranteed that at least one memory block
                             * exists. */
    Memblock   pFree;       /* Lifopools: List of unused memory blocks */
    Mempool    pSuper;      /* The pool this one depends on */
    Mempool    pSubPools;   /* List of depending pools */
    Mempool    pNextSub;    /* Next pool in the dependee list */
};

/*-------------------------------------------------------------------------*/
static INLINE size_t
size_pool (size_t elemsize, size_t o_size)

/* Return the userspace size for a memblock suitable to hold objects of
 * size <elemsize>, taking into account an per-element overhead of <o_size>
 * and the maximum memblock size.
 * The result can be passed as 'size' parameter to new_pool() functions.
 */

{
    size_t esize = (ROUND(elemsize) + o_size);
    unsigned int num;

    num = MEMBLOCK_LIMIT / esize;
    if (num < 1)
        num = 1;
    return num * esize;
} /* size_pool() */

/*-------------------------------------------------------------------------*/
size_t
size_mempool (size_t elemsize)

/* Return the userspace size for a mempool suitable to hold objects of
 * size <elemsize>, taking into account an per-element overhead of <o_size>
 * and the maximum memblock size.
 * The result can be passed as 'size' parameter to new_mempool().
 */

{
    return size_pool(elemsize, 0);
} /* size_mempool() */

/*-------------------------------------------------------------------------*/
size_t
size_lifopool (size_t elemsize)

/* Return the userspace size for a lifopool suitable to hold objects of
 * size <elemsize>, taking into account an per-element overhead of <o_size>
 * and the maximum memblock size.
 * The result can be passed as 'size' parameter to new_lifopool().
 */

{
    return size_pool(elemsize, SIZEOF_LIFO_T);
} /* size_lifopool() */

/*-------------------------------------------------------------------------*/
static INLINE Mempool
new_pool (size_t iSize, pooltype_t type)

/* Create a new memory pool of <type> for a typical allocation size of <iSize>
 * bytes per memory block and prepare
 * Result is the pointer to the mempool structure, or NULL if an error
 * occurs.
 */

{
    Mempool pPool;
    struct memblock_s * pBlock;

    assert(iSize > 0);
    assert(sizeof(union align) == (sizeof(union align) & ~(sizeof(union align)-1)));
      /* If this assert fails, sizeof(union align) is not a power of 2 */

    /* Round iSize up to the next integral of sizeof(union align) */
    iSize = ROUND(iSize);

    pPool = xalloc(sizeof(*pPool));
    if (!pPool)
        return NULL;
    /* There must be at least one memblock in the list! */

    pBlock = xalloc(sizeof(*pBlock) - sizeof(pBlock->u) + iSize);
    if (!pBlock)
    {
        xfree(pPool);
        return NULL;
    }
    pBlock->pNext = NULL;
    pBlock->length = sizeof(*pBlock) - sizeof(pBlock->u) + iSize;
    pBlock->pMark = pBlock->u.data + iSize;

    /* Setup the pool */
    pPool->type = type;
    pPool->iAllocSize = iSize;
    pPool->pBlocks = pBlock;
    pPool->pFree = NULL;
    pPool->pSuper = NULL;
    pPool->pSubPools = NULL;

    return pPool;
} /* new_pool() */

/*-------------------------------------------------------------------------*/
Mempool
new_mempool (size_t iSize)

/* Create a new Mempool for a typical allocation size of <iSize>
 * bytes per memory block and prepare
 * Result is the pointer to the mempool structure, or NULL if an error
 * occurs.
 */

{
    return new_pool(iSize, MEMPOOL);
} /* new_mempool() */

/*-------------------------------------------------------------------------*/
Mempool
new_lifopool (size_t iSize)

/* Create a new Lifopool for a typical allocation size of <iSize>
 * bytes per memory block and prepare
 * Result is the pointer to the mempool structure, or NULL if an error
 * occurs.
 */

{
    Mempool pPool;

    iSize += SIZEOF_LIFO_T; /* Include space for the sentinel block */

    pPool = new_pool(iSize, LIFOPOOL);
    if (pPool)
    {
        /* Add a sentinel (pseudo-used block) at the end of the arena.
         */
        struct memblock_s * pBlock = pPool->pBlocks;
        lifo_t *p = (lifo_t *)(pBlock->pMark - SIZEOF_LIFO_T);

        p->length = 1;
        p->pBlock = pBlock;

        /* Update the pMark pointer */
        pBlock->pMark = (char *)p;
    }
    return pPool;
} /* new_lifopool() */

/*-------------------------------------------------------------------------*/
void
mempool_depend_on (Mempool pSub, Mempool pSuper)

/* Memory pool pSub is made a dependee of pool pSuper.
 * If pSub is a dependee of some pool already or if the pooltypes differ,
 * an assertion is raised.
 */

{
    assert(pSub->pSuper == NULL);
    assert(pSub->type == pSuper->type);

    pSub->pSuper = pSuper;
    pSub->pNextSub = pSuper->pSubPools;
    pSuper->pSubPools = pSub;
} /* mempool_depend_on() */

/*-------------------------------------------------------------------------*/
static INLINE void *
alloc_from_pool (Mempool pPool, size_t iSize)

/* Allocate <iSize> bytes of memory from the mempool <pPool>.
 * Return a pointer to the allocated memory (it is at least aligned to
 * the size of a ALIGNTYPE), or NULL on failure.
 *
 * Within the memblocks, the memory is allocated from the end of
 * the allocated memory.
 */

{
    Memblock pBlock;

    /* Round iSize up to the next integral of sizeof(ALIGNTYPE) */
    iSize = ROUND(iSize);

    /* If it is a big block, allocate it directly and insert
     * it directly _after_ the current 'normal sized' memblock.
     */

    if (iSize >= pPool->iAllocSize)
    {
        assert(pPool->pBlocks != NULL); /* just in case */

        pBlock = xalloc(sizeof(*pBlock)-sizeof(pBlock->u)+iSize);
        if (pBlock == NULL)
            return NULL;
        pBlock->length = sizeof(*pBlock)-sizeof(pBlock->u)+iSize;
        pBlock->pMark = pBlock->u.data;
        pBlock->pNext = pPool->pBlocks->pNext;
        pPool->pBlocks->pNext = pBlock;
        return (void *)(pBlock->u.data);
    }

    /* Normal iSizes are always allocated from the first memblock
     * in the pBlock list. If the current memblock has not enough
     * memory left, a new one is allocated.
     */

    pBlock = pPool->pBlocks;
    if ((ptrdiff_t)iSize > pBlock->pMark - pBlock->u.data)
    {
        pBlock = xalloc(  sizeof(*pBlock)-sizeof(pBlock->u)
                        + pPool->iAllocSize);
        if (pBlock == NULL)
            return NULL;
        pBlock->length =   sizeof(*pBlock)-sizeof(pBlock->u)
                         + pPool->iAllocSize;
        pBlock->pMark = pBlock->u.data + pPool->iAllocSize;
        pBlock->pNext = pPool->pBlocks;
        pPool->pBlocks = pBlock;
    }

    /* pBlock now points to a memblock with enough memory left.
     * Allocate the desired chunk from the end of the .data[] array.
     */

    pBlock->pMark -= iSize;
    return (void *)(pBlock->pMark);

} /* alloc_from_pool() */

/*-------------------------------------------------------------------------*/
static INLINE void *
alloc_from_lifo (Mempool pPool, size_t iSize)

/* Allocate <iSize> bytes of memory from the lifopool <pPool>.
 * Return a pointer to the allocated memory (it is at least aligned to
 * the size of a ALIGNTYPE), or NULL on failure.
 *
 * Within the memblocks, the memory is allocated from the end of
 * the allocated memory.
 */

{
    Memblock pBlock;
    lifo_t *pLifo;

    /* Round iSize up to the next integral of sizeof(ALIGNTYPE) */
    iSize = ROUND(iSize + SIZEOF_LIFO_T);
    /* If it is a big block, allocate it directly and insert
     * it directly _after_ the current 'normal sized' memblock.
     */

    if (iSize >= pPool->iAllocSize)
    {
        assert(pPool->pBlocks != NULL); /* just in case */

        pBlock = xalloc(sizeof(*pBlock)-sizeof(pBlock->u)
                        +iSize+SIZEOF_LIFO_T);
        if (pBlock == NULL)
            return NULL;
        pBlock->length = sizeof(*pBlock)-sizeof(pBlock->u)
                         +iSize+SIZEOF_LIFO_T;
        pBlock->pMark = pBlock->u.data;
        pBlock->pNext = pPool->pBlocks->pNext;
        pPool->pBlocks->pNext = pBlock;

        /* Write the lifo_t for the allocated block */
        pLifo = (lifo_t *)pBlock->pMark;
        pLifo->length = (ssize_t)iSize;
        pLifo->pBlock = pBlock;

        /* Write the sentinel */
        pLifo = (lifo_t *)(pBlock->pMark+iSize);
        pLifo->length = 1;
        pLifo->pBlock = pBlock;

        /* Return the address */
        return (void *)(pBlock->u.data+SIZEOF_LIFO_T);
    }

    /* Normal iSizes are always allocated from the first memblock
     * in the pBlock list. If the current memblock has not enough
     * memory left, a new one is allocated.
     */
    pBlock = pPool->pBlocks;
    if ((ptrdiff_t)iSize > pBlock->pMark - pBlock->u.data)
    {
        /* If there are blocks in the freelist, use those first;
         * otherwise allocate a new one.
         */
        if (pPool->pFree)
        {
            pBlock = pPool->pFree;
            pPool->pFree = pBlock->pNext;
        }
        else
        {
            pBlock = xalloc(  sizeof(*pBlock)-sizeof(pBlock->u)
                            + pPool->iAllocSize);
            if (pBlock == NULL)
                return NULL;
            pBlock->length = sizeof(*pBlock)-sizeof(pBlock->u)
                             + pPool->iAllocSize;
            pBlock->pMark = pBlock->u.data + pPool->iAllocSize;

            /* For lifopools, add a sentinel (pseudo-used block) at the end
             * of the arena.
             */
            pLifo = (lifo_t *)(pBlock->pMark-SIZEOF_LIFO_T);

            pLifo->length = 1;
            pLifo->pBlock = pBlock;

            /* Update the pMark pointer */
            pBlock->pMark = (char *)pLifo;
        }

        /* Link the block into the list of used blocks */
        pBlock->pNext = pPool->pBlocks;
        pPool->pBlocks = pBlock;
    }

    /* pBlock now points to a memblock with enough memory left.
     * Allocate the desired chunk from the end of the .data[] array.
     */
    pBlock->pMark -= iSize;

    /* Put in the lifo_t structure and
     * return the address after the structure.
     */
    pLifo = (lifo_t *)pBlock->pMark;

    pLifo->length = (ssize_t)iSize;
    pLifo->pBlock = pBlock;

    return (void *)(pBlock->pMark + SIZEOF_LIFO_T);

} /* alloc_from_lifo() */

/*-------------------------------------------------------------------------*/
void *
mempool_alloc (Mempool pPool, size_t iSize)

/* Allocate <iSize> bytes of memory from the pool <pPool>.
 * Return a pointer to the allocated memory (it is at least aligned to
 * the size of a ALIGNTYPE), or NULL on failure.
 *
 * Within the memblocks, the memory is allocated from the end of
 * the allocated memory.
 */

{
    assert(pPool != NULL);
    assert(iSize < LONG_MAX);

    if (pPool->type == LIFOPOOL)
        return alloc_from_lifo(pPool, iSize);

    return alloc_from_pool(pPool, iSize);
} /* mempool_alloc() */

/*-------------------------------------------------------------------------*/
void
mempool_free (Mempool pPool, void * adr)

/* Return the block allocated at <adr> to the pool <pPool>.
 * This is a noop for mempools, but (lazily) returns memory to a lifopool.
 */

{
    Memblock pBlock;
    lifo_t * pLifo;
    ssize_t length;

    assert(pPool != NULL);
    assert(adr != NULL);

    if (LIFOPOOL != pPool->type)
        return;

    /* Get the lifo_t structure and its data */
    pLifo = (lifo_t *)((char *)adr - SIZEOF_LIFO_T);
    assert(pLifo->length > 1);
    pBlock = pLifo->pBlock;
    length = pLifo->length;

    /* Mark the block as unused */
    pLifo->length = -length;

    /* If this newly freed block happens to be the first free block in the
     * memblock, return it and all following free blocks to the free
     * arena of the memblock.
     */
    if ((char *)pLifo == pBlock->pMark)
    {
        /* Loop invariant: pMark == pLifo */
        while (pLifo->length < 0)
        {
            pBlock->pMark = pBlock->pMark - pLifo->length;
            pLifo = (lifo_t *)pBlock->pMark;
        }
    }

    /* If the leading memblock(s) of the pool are completely free,
     * move them over into the free list.
     */
    if (pBlock == pPool->pBlocks)
    {
        while (pBlock->pNext != NULL
            &&    pBlock->pMark - (char*)&(pBlock->u)
               >= (ptrdiff_t)(pPool->iAllocSize - SIZEOF_LIFO_T))
        {
            pPool->pBlocks = pBlock->pNext;
            pBlock->pNext = pPool->pFree;
            pPool->pFree = pBlock;
            pBlock = pPool->pBlocks;
        }
    }

    /* That's it */
} /* mempool_free() */

/*-------------------------------------------------------------------------*/
void
mempool_reset (Mempool pPool)

/* Free all memory allocated from the pool <pPool>, but leave the pool
 * around for further usage. If the pool has dependees, they are reset
 * recursively.
 */

{
    Memblock pBlock;

    assert(pPool != NULL);
    assert(pPool->pBlocks != NULL); /* just in case */


    /* Deallocate all memblocks but the first one */

    pBlock = pPool->pBlocks->pNext;
    while (pBlock != NULL)
    {
        Memblock pThis = pBlock;

        pBlock = pBlock->pNext;
        xfree(pThis);
    }

    pBlock = pPool->pFree;
    while (pBlock != NULL)
    {
        Memblock pThis = pBlock;

        pBlock = pBlock->pNext;
        xfree(pThis);
    }
    pPool->pFree = NULL;

    /* Reinitialise the first (and now only) memblock */

    pBlock = pPool->pBlocks;
    pBlock->pMark = pBlock->u.data + pPool->iAllocSize
                    - (LIFOPOOL == pPool->type ? SIZEOF_LIFO_T : 0);
    pBlock->pNext = NULL;

    /* Reset all depending pools */

    for (pPool = pPool->pSubPools; pPool != NULL; pPool = pPool->pNextSub)
    {
        mempool_reset(pPool);
    }
} /* mempool_reset() */

/*-------------------------------------------------------------------------*/
void
mempool_delete (Mempool pPool)

/* Delete the pool <pPool>, all memory allocated from it, and all
 * depending pools. <pPool> is invalid after the function returns.
 */

{
    Memblock pBlock;
    Mempool pSubPool;

    assert(pPool != NULL);

    /* Free all memblocks */

    pBlock = pPool->pBlocks;
    while (pBlock != NULL)
    {
        Memblock pThis = pBlock;

        pBlock = pBlock->pNext;
        xfree(pThis);
    }

    pBlock = pPool->pFree;
    while (pBlock != NULL)
    {
        Memblock pThis = pBlock;

        pBlock = pBlock->pNext;
        xfree(pThis);
    }

    /* If this pool is a dependee, delete it from the list */

    if (pPool->pSuper != NULL)
    {
        Mempool pPrev, pNext;

        pPrev = pPool->pSuper;
        pNext = pPrev->pSubPools;

        if (pPrev->pSubPools == pPool)
        {
            pPrev->pSubPools = pPool->pNextSub;
        }
        else
        {
            for (; pNext != NULL && pNext != pPool
                 ; pPrev = pNext, pNext = pNext->pNextSub
                ) /* SKIP */;
            if (pNext == pPool)
                pPrev->pNextSub = pPool->pNextSub;
        }
    }

    /* Delete all depending pools, but take care that those
     * pools don't start to scan our subpool list.
     */

    for (pSubPool = pPool->pSubPools; pSubPool != NULL; )
    {
        Mempool pThis = pSubPool;
        pSubPool = pSubPool->pNextSub;
        pThis->pSuper = NULL;  /* ! */
        mempool_delete(pThis);
    }

    /* Finally, deallocate this pool */

    xfree(pPool);
} /* mempool_delete() */

/*-------------------------------------------------------------------------*/
size_t
mempool_size (Mempool pPool)

/* Return the total size of the mempool <pPool>.
 */

{
    Memblock pBlock;
    size_t size;

    size = sizeof(*pPool);

    for (pBlock = pPool->pBlocks; pBlock; pBlock = pBlock->pNext)
        size += pBlock->length;
    for (pBlock = pPool->pFree; pBlock; pBlock = pBlock->pNext)
        size += pBlock->length;

    return size;

} /* mempool_size() */

/*-------------------------------------------------------------------------*/
#ifdef GC_SUPPORT

void
mempool_clear_refs (Mempool pPool)

/* GC Support: Clear the refs of all memory associated with <pPool> and
 * its dependees.
 */

{
    Memblock pBlock;

    clear_memory_reference(pPool);

    for (pBlock = pPool->pBlocks; pBlock; pBlock = pBlock->pNext)
        clear_memory_reference(pBlock);
    for (pBlock = pPool->pFree; pBlock; pBlock = pBlock->pNext)
        clear_memory_reference(pBlock);

    for (pPool = pPool->pSubPools; pPool != NULL; pPool = pPool->pNextSub)
        mempool_clear_refs(pPool);

} /* mempool_clear_refs() */

void
mempool_note_refs (Mempool pPool)

/* GC Support: Note the refs of all memory associated with <pPool> and
 * its dependees.
 */

{
    Memblock pBlock;

    note_malloced_block_ref(pPool);

    for (pBlock = pPool->pBlocks; pBlock; pBlock = pBlock->pNext)
        note_malloced_block_ref(pBlock);
    for (pBlock = pPool->pFree; pBlock; pBlock = pBlock->pNext)
        note_malloced_block_ref(pBlock);

    for (pPool = pPool->pSubPools; pPool != NULL; pPool = pPool->pNextSub)
        mempool_note_refs(pPool);

} /* mempool_note_refs() */

#endif /* GC_SUPPORT */

/*-------------------------------------------------------------------------*/
/*=========================================================================*/

/***************************************************************************/

