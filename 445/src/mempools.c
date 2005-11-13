/*---------------------------------------------------------------------------
 * Gamedriver Mempools: Specialised Memory Allocators.
 *
 *---------------------------------------------------------------------------
 * Purpose of the memory pools is to provide fast allocation methods for
 * certain allocation patterns. They are most useful for scratchpad
 * purposes.
 *
 * Mempools: allocation of objects with identical time of death.
 *           Attempts to deallocate single objects have no effect.
 *           
 * Fifopools: allocation/deallocation of objects follows (more than less)
 *            a fifo pattern.
 *            
 * Note: the GC will deallocate all memory pools.
 *---------------------------------------------------------------------------
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
 * The memory allocated from a mempool is aligned to the size of union align
 * (which is assumed to be a power of 2).
 *---------------------------------------------------------------------------
 */

#include "driver.h"

#include <assert.h>
#include <stdlib.h>
#include <sys/types.h>

#include "mempools.h"
#include "gcollect.h"
#include "xalloc.h"

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
 * Fifopools are like Mempools, with the addition that every used
 * block is prepended by a fifo_t structure (allocated to a multiple
 * of the align_t type). pMark points therefore at this structure of the
 * first used block. If a fifo-allocation is freed, it is just marked
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

/* --- struct fifo_s: headblock for a fifo-type allocation ---
 *
 * One of these structures, allocated to a multiple of align_t, is
 * prepended to every fifopool allocation. Additionally, the end
 * of every memory block is marked with a 'used' structure as sentinel.
 */

typedef struct fifo_s fifo_t;

struct fifo_s {
    ssize_t length;    /* Size of this block, including this structure.
                        * Positive for allocated blocks, negative for free ones.
                        */
    Memblock pBlock;   /* Backpointer to the memoryblock holding this block */
};

#define SIZEOF_FIFO_T ROUND(sizeof(fifo_t))

/*=========================================================================*/

/*-------------------------------------------------------------------------*/
/* Memory pool types
 */

enum pooltype_u {
    MEMPOOL = 0,
    FIFOPOOL
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
    Memblock   pFree;       /* Fifopools: List of unused memory blocks */
    Mempool    pSuper;      /* The pool this one depends on */
    Mempool    pSubPools;   /* List of depending pools */
    Mempool    pNextSub;    /* Next pool in the dependee list */
};

/*-------------------------------------------------------------------------*/
size_t
fifopool_size (size_t elemsize, unsigned int num)

/* Return the size for a fifopool suitable to hold <num> object of size
 * <elemsize>, taking into account all the overhead.
 */
  
{
    return num * (ROUND(elemsize) + SIZEOF_FIFO_T);
}

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
    iSize = ROUND(iSize + (type == FIFOPOOL ? SIZEOF_FIFO_T : 0));

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

    /* For fifopools, add a sentinel (pseudo-used block) at the end
     * of the arena.
     */
    if (FIFOPOOL == type)
    {
        fifo_t *p = (fifo_t *)(pBlock->pMark - SIZEOF_FIFO_T);

        p->length = 1;
        p->pBlock = pBlock;

        /* Update the pMark pointer */
        pBlock->pMark = (char *)p;
    }
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
}

/*-------------------------------------------------------------------------*/
Mempool
new_fifopool (size_t iSize)

/* Create a new Fifopool for a typical allocation size of <iSize>
 * bytes per memory block and prepare
 * Result is the pointer to the mempool structure, or NULL if an error
 * occurs.
 */

{
    return new_pool(iSize, FIFOPOOL);
}

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
    if (iSize > pBlock->pMark - pBlock->u.data)
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
alloc_from_fifo (Mempool pPool, size_t iSize)

/* Allocate <iSize> bytes of memory from the fifopool <pPool>.
 * Return a pointer to the allocated memory (it is at least aligned to
 * the size of a ALIGNTYPE), or NULL on failure.
 *
 * Within the memblocks, the memory is allocated from the end of
 * the allocated memory.
 */

{
    Memblock pBlock;
    fifo_t *pFifo;

    /* Round iSize up to the next integral of sizeof(ALIGNTYPE) */
    iSize = ROUND(iSize + SIZEOF_FIFO_T);
    /* If it is a big block, allocate it directly and insert
     * it directly _after_ the current 'normal sized' memblock.
     */

    if (iSize >= pPool->iAllocSize)
    {
        assert(pPool->pBlocks != NULL); /* just in case */

        pBlock = xalloc(sizeof(*pBlock)-sizeof(pBlock->u)
                        +iSize+SIZEOF_FIFO_T);
        if (pBlock == NULL)
            return NULL;
        pBlock->length = sizeof(*pBlock)-sizeof(pBlock->u)
                         +iSize+SIZEOF_FIFO_T;
        pBlock->pMark = pBlock->u.data;
        pBlock->pNext = pPool->pBlocks->pNext;
        pPool->pBlocks->pNext = pBlock;

        /* Write the fifo_t for the allocated block */
        pFifo = (fifo_t *)pBlock->pMark;
        pFifo->length = (ssize_t)iSize;
        pFifo->pBlock = pBlock;

        /* Write the sentinel */
        pFifo = (fifo_t *)(pBlock->pMark+iSize);
        pFifo->length = 1;
        pFifo->pBlock = pBlock;

        /* Return the address */
        return (void *)(pBlock->u.data+SIZEOF_FIFO_T);
    }

    /* Normal iSizes are always allocated from the first memblock
     * in the pBlock list. If the current memblock has not enough
     * memory left, a new one is allocated.
     */
    pBlock = pPool->pBlocks;
    if (iSize > pBlock->pMark - pBlock->u.data)
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
            
            /* For fifopools, add a sentinel (pseudo-used block) at the end
             * of the arena.
             */
            pFifo = (fifo_t *)(pBlock->pMark-SIZEOF_FIFO_T);

            pFifo->length = 1;
            pFifo->pBlock = pBlock;

            /* Update the pMark pointer */
            pBlock->pMark = (char *)pFifo;
        }

        /* Link the block into the list of used blocks */
        pBlock->pNext = pPool->pBlocks;
        pPool->pBlocks = pBlock;
    }

    /* pBlock now points to a memblock with enough memory left.
     * Allocate the desired chunk from the end of the .data[] array.
     */
    pBlock->pMark -= iSize;

    /* Put in the fifo_t structure and
     * return the address after the structure.
     */
    pFifo = (fifo_t *)pBlock->pMark;

    pFifo->length = (ssize_t)iSize;
    pFifo->pBlock = pBlock;

    return (void *)(pBlock->pMark + SIZEOF_FIFO_T);

} /* alloc_from_fifo() */

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

    if (pPool->type == FIFOPOOL)
        return alloc_from_fifo(pPool, iSize);

    return alloc_from_pool(pPool, iSize);
} /* mempool_alloc() */

/*-------------------------------------------------------------------------*/
void
mempool_free (Mempool pPool, void * adr)

/* Return the block allocated at <adr> to the pool <pPool>.
 * This is a noop for mempools, but (lazily) returns memory to a fifopool.
 */

{
    Memblock pBlock;
    fifo_t * pFifo;
    ssize_t length;

    assert(pPool != NULL);
    assert(adr != NULL);

    if (FIFOPOOL != pPool->type)
        return;

    /* Get the fifo_t structure and its data */
    pFifo = (fifo_t *)((char *)adr - SIZEOF_FIFO_T);
    assert(pFifo->length > 1);
    pBlock = pFifo->pBlock;
    length = pFifo->length;

    /* Mark the block as unused */
    pFifo->length = -length;

    /* If this newly freed block happens to be the first free block in the
     * memblock, return it and all following free blocks to the free
     * arena of the memblock.
     */
    if ((char *)pFifo == pBlock->pMark)
    {
        /* Loop invariant: pMark == pFifo */
        while (pFifo->length < 0)
        {
            pBlock->pMark = pBlock->pMark - pFifo->length;
            pFifo = (fifo_t *)pBlock->pMark;
        }
    }
   
    /* If the leading memblock(s) of the pool are completely free,
     * move them over into the free list.
     */
    if (pBlock == pPool->pBlocks)
    {
        while (pBlock->pNext != NULL
            &&    pBlock->pMark - (char*)&(pBlock->u)
               >= pPool->iAllocSize - SIZEOF_FIFO_T)
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
                    - (FIFOPOOL == pPool->type ? SIZEOF_FIFO_T : 0);
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

