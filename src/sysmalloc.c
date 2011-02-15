/*---------------------------------------------------------------------------
 * Wrapper for the system's malloc() routines
 *
 *---------------------------------------------------------------------------
 */

#include "driver.h"

#include <stdlib.h>

#include "sysmalloc.h"

#include "mstrings.h"
#include "stdstrings.h"
#include "svalue.h"

#include "../mudlib/sys/debug_info.h"

/* Defines required by the xalloc.c wrapper */
/* #undef REPLACE_MALLOC */
#define NO_MEM_BLOCK_SIZE
#define MEM_THREADSAFE

/*-------------------------------------------------------------------------*/

static char * heap_start = NULL;
static char * heap_end = NULL;
  /* The start and end of the heap, as we know it.
   */

/*-------------------------------------------------------------------------*/
static inline POINTER
mem_alloc (size_t size)

/* Allocate a memory block for <size> bytes at the source <file>:<line>.
 * Result is the pointer the memory block, or NULL when out of memory.
 */

{
    POINTER rc;

    rc = malloc(size);

    if (heap_start == NULL || (char *)rc < heap_start)
        heap_start = rc;
    if (heap_end == NULL || (char *)rc + size > heap_end)
        heap_end = rc + size;

    assert_stack_gap();

    return rc;
} /* mem_alloc() */

/*-------------------------------------------------------------------------*/
static INLINE void
mem_free (POINTER ptr)

/* Return the memoryblock <ptr> to the allocator.
 */

{
    free(ptr);
} /* mem_free() */

/*-------------------------------------------------------------------------*/
static POINTER
mem_realloc (POINTER p, size_t size)

/* Reallocate block <p> to the new size of <size> and return the pointer.
 */

{
    return realloc(p, size);
} /* mem_realloc() */

/*-------------------------------------------------------------------------*/
static INLINE void
mem_mark_permanent (POINTER p UNUSED)

/* Mark the allocated block at <p> as permanent, ie. it won't be subject
 * to the GC.
 */

{
#   ifdef __MWERKS__
#      pragma unused(p)
#   endif
    /* Nothing */
} /* mem_mark_permanent() */

/*-------------------------------------------------------------------------*/
static INLINE void
mem_mark_collectable (POINTER p UNUSED)

/* Mark the allocated block at <p> as non-permant, ie. it is subject
 * to the GC.
 */

{
#   ifdef __MWERKS__
#      pragma unused(p)
#   endif
    /* Nothing */
} /* mem_mark_collectable() */

/*-------------------------------------------------------------------------*/
static INLINE size_t
mem_block_size (POINTER p UNUSED)

/* Return the size of block <p> (sans internal overhead) in bytes.
 */

{
#   ifdef __MWERKS__
#      pragma unused(p)
#   endif
   return 0;
} /* mem_block_size() */

/*-------------------------------------------------------------------------*/
static INLINE size_t
mem_overhead (void)

/* Return the size of each block's overhead in bytes.
 */

{
   return EXTERN_MALLOC_OVERHEAD;
} /* mem_overhead() */

/*-------------------------------------------------------------------------*/
static INLINE void *
mem_increment_size (void *vp, size_t size)

/* Try to extent the allocation block for <vp> to hold <size> more bytes.
 * If this is not possible, return NULL, otherwise return a pointer
 * to the start of the block extension.
 */

{
    return NULL;
} /* mem_increment_size() */

/*-------------------------------------------------------------------------*/
void
mem_dump_data (strbuf_t *sbuf)

/* For the status commands and functions: add the smalloc statistic
 * to the buffer <sbuf>.
 */

{
    strbuf_add(sbuf, "Using system standard malloc.\n");
    strbuf_addf(sbuf,
                "soft memory limit: %10lu, hard memory limit: %10lu\n\n",
                get_memory_limit(MALLOC_SOFT_LIMIT),
                get_memory_limit(MALLOC_HARD_LIMIT)
               );

} /* mem_dump_data() */

/*-------------------------------------------------------------------------*/
void
mem_dinfo_data (svalue_t *svp, int value)

/* Fill in the data for debug_info(DINFO_DATA, DID_MEMORY) into the
 * svalue-block svp.
 */

{
    if (value == -1)
        put_ref_string(svp+DID_MEM_NAME, STR_SYSTEM_MALLOC);
    else if (value == DID_MEM_NAME)
        put_ref_string(svp, STR_SYSTEM_MALLOC);
} /* mem_dinfo_data() */

/*-------------------------------------------------------------------------*/
void
mem_dump_extdata (strbuf_t *sbuf)

/* For the status commands and functions: add the extended smalloc statistic
 * to the buffer <sbuf>.
 */

{
    strbuf_add(sbuf, "No detailed blocks statistics available.\n");
} /* mem_dump_extdata() */

/*-------------------------------------------------------------------------*/
Bool
mem_dump_memory (int fd)

/* Print the location, size, and (if available) the TRACE information
 * of all memory blocks to file <fd>, and return TRUE.
 * If the allocator doesn't support this operation, print nothing
 * and return FALSE.
 */

{
    return MY_FALSE;
} /* mem_dump_memory() */

/*-------------------------------------------------------------------------*/
void
mem_consolidate (Bool force UNUSED)

/* Consolidate the free small blocks, merging them into larger free blocks
 * where possible, and rebuild the free lists.
 */

{
#   ifdef __MWERKS__
#      pragma unused(force)
#   endif
    /* Nothing */
} /* mem_consolidate() */

/*-------------------------------------------------------------------------*/
#ifdef MALLOC_EXT_STATISTICS
void
mem_update_stats (void)

/* Update whatever extended statistics the allocator has. Called every
 * backend cycle or so to allow for the calculation of averages over time.
 */

{
    /* Nothing */
} /* mem_update_stats() */
#endif /* MALLOC_EXT_STATISTICS */

/*-------------------------------------------------------------------------*/
static INLINE p_int
mem_mem_allocated()
/* The amount of memory currently allocated from the allocator, including 
 * the overhead for the allocator.
 */
{
    return 0;
}

/*-------------------------------------------------------------------------*/
static INLINE p_int
mem_mem_used()
/* The amount of memory currently used for driver data, excluding the 
 * overhead from the allocator.
 */
{
    return 0;
}

/***************************************************************************/
