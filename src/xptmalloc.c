/*---------------------------------------------------------------------------
 * xalloc stubs for the ptmalloc2 Memory Manager
 *
 * ptmalloc2 was written by Wolfram Glober (www.malloc.de).
 * ptmalloc2 is based on work of Doug Lea (gee.cs.oswego.edu).
 * ptmalloc2 was adapted to ldmud by Christian Welzel (www.camlann.de)
 *---------------------------------------------------------------------------
 * This allocator supports REPLACE_MALLOC.
 * This allocator is threadsafe.
 */

#include "mstrings.h"
#include "stdstrings.h"
#include "svalue.h"

#include "ptmalloc/malloc.h"
#include "../mudlib/sys/debug_info.h"

/* Functions defined by ptmalloc */
extern POINTER 	dlmalloc(size_t);
extern POINTER 	dlrealloc(POINTER, size_t);
extern void 	dlfree(POINTER);
extern int 	dlmalloc_usable_size(POINTER);
extern void	dlmalloc_mark_collectable(POINTER);
extern void	dlmalloc_mark_permanent(POINTER);
extern void 	dlmalloc_clear_ref(POINTER);
extern void 	dlmalloc_mark_ref(POINTER);
extern int 	dlmalloc_test_ref(POINTER);
extern void 	dlmalloc_clear_ref_flags();
extern void 	dlmalloc_free_unrefed_memory();
extern int 	dlmalloc_is_freed(POINTER, size_t);
extern struct mallinfo dlmallinfo();

/* Current end address of the heap. */
static word_t *heap_end = NULL;

#define PT_OVERHEAD (2*sizeof(size_t))

/*     to allocate a memory block */
static POINTER mem_alloc(size_t size) {
    return dlmalloc(size);
}

/*     to deallocate a memory block */
static void mem_free(POINTER p) {
    return dlfree(p);
}

/*     reallocate a block. */
static POINTER mem_realloc (POINTER p, size_t size) {
    return dlrealloc(p, size);
}

/*     Increase a block size in place. If not possible, return NULL. */
static void * mem_increment_size (void *vp, size_t size) {
    return NULL;
}

/*     Return the size of an allocated block.
 *     If this value is not available, implement this function as a dummy,
 *     but also #define NO_MEM_BLOCK_SIZE.
 *     For Garbage Collection or replacing malloc() this function must be
 *     valid!
 *     Returns the blocksize excl. overhead.
 */
static size_t mem_block_size (POINTER p) {
    return dlmalloc_usable_size(p);
}

/*     Return the size of the allocators internal overhead. */
static size_t mem_overhead () {
    return PT_OVERHEAD;
}

/*     Mark a block as permanent */
static void mem_mark_permanent (POINTER p) {
    dlmalloc_mark_permanent(p);
}

/*     Mark a block as collectable */
static void mem_mark_collectable (POINTER p) {
    dlmalloc_mark_collectable(p);
}

/*     Do whatever consolidation is useful. */
void mem_consolidate () {
    /* nothing to do here... malloc does it automagically */
}

/* For the status commands and functions: add the ptmalloc statistic
 * to the buffer <sbuf>.
 */
void mem_dump_data (strbuf_t *sbuf) {
    struct mallinfo stats;

    /* Get a snapshot of the statistics - strbuf_add() might do further
     * allocations while we're reading them.
     */
    stats = dlmallinfo();

#   define dump_stat(str,stat) strbuf_addf(sbuf, str,stat.counter,stat.size)

    strbuf_add(sbuf, "Type                   Count      Space (bytes)\n");
#if 0
    dump_stat("xallocs:           %8d        %10lu\n\n", xalloc_st);
    dump_stat("sbrk requests:     %8d        %10lu (a)\n",sbrk_st);
    dump_stat("large blocks:      %8d        %10lu (b)\n",l_alloc);
    strbuf_addf(sbuf
               , "large net avail:                   %10d\n"
               , l_alloc.size - l_alloc.counter * M_OVERHEAD * SINT
               );
    dump_stat("large free blocks: %8d        %10lu (c)\n",l_free);
    dump_stat("large wasted:      %8d        %10lu (d)\n\n",l_wasted);
    dump_stat("small chunks:      %8d        %10lu (e)\n",s_chunk);
    dump_stat("small blocks:      %8d        %10lu (f)\n",s_alloc);
    strbuf_addf(sbuf
               , "small net avail:                   %10d\n"
               , s_alloc.size - s_alloc.counter * M_OVERHEAD * SINT
               );
    dump_stat("small free blocks: %8d        %10lu (g)\n",s_free);
    dump_stat("small wasted:      %8d        %10lu (h)\n",s_wasted);
    strbuf_addf(sbuf,
"unused from current chunk          %10lu (i)\n\n",unused);

    dump_stat("permanent blocks:  %8d        %10lu\n", perm_st);
#ifdef SBRK_OK
    dump_stat("clib allocations:  %8d        %10lu\n", clib_st);
#else
    strbuf_addf(sbuf, "clib allocations:       n/a               n/a\n");
#endif
    strbuf_add(sbuf, "\n");

    strbuf_addf(sbuf,
      "malloc_increment_size: calls %ld success %ld total %ld\n\n",
      malloc_increment_size_calls,
      malloc_increment_size_success,
      malloc_increment_size_total
    );
    strbuf_addf(sbuf
               , "Total storage:        (b+c+d)     %10lu should equal (a) %10lu\n"
               , l_alloc.size + l_free.size + l_wasted.size
               , sbrk_st.size
               );
    strbuf_addf(sbuf
               , "Total small storage:  (f+g+h+i)   %10lu should equal (e) %10lu\n"
               , s_alloc.size + s_free.size + s_wasted.size + unused
               , s_chunk.size
               );
    strbuf_addf(sbuf
               , "Total storage in use: (b-g-h-i)   %10lu net available:   %10lu\n"
               , l_alloc.size - s_free.size - s_wasted.size - unused
               , l_alloc.size - s_free.size - s_wasted.size - unused
                 - l_alloc.counter * M_OVERHEAD * SINT
                 - s_alloc.counter * M_OVERHEAD * SINT
                 - xalloc_st.counter * XM_OVERHEAD_SIZE
               );
    strbuf_addf(sbuf
               , "Total storage unused: (c+d+g+h+i) %10lu\n"
               , l_free.size + l_wasted.size
                 + s_free.size + s_wasted.size + unused
               );
#endif
} /* mem_dump_data() */


/* Fill in the data for debug_info(DINFO_DATA, DID_MEMORY) into the
 * svalue-block svp.
 */
void mem_dinfo_data (svalue_t *svp, int value) {
#define ST_NUMBER(which,code) \
    if (value == -1) svp[which].u.number = code; \
    else if (value == which) svp->u.number = code

    if (value == -1)
        put_ref_string(svp+DID_MEM_NAME, STR_PTMALLOC);
    else if (value == DID_MEM_NAME)
        put_ref_string(svp, STR_PTMALLOC);

#if 0
    ST_NUMBER(DID_MEM_SBRK, sbrk_stat.counter);
    ST_NUMBER(DID_MEM_SBRK_SIZE, sbrk_stat.size);
    ST_NUMBER(DID_MEM_LARGE, large_alloc_stat.counter);
    ST_NUMBER(DID_MEM_LARGE_SIZE, large_alloc_stat.size * SINT);
    ST_NUMBER(DID_MEM_LFREE, large_free_stat.counter);
    ST_NUMBER(DID_MEM_LFREE_SIZE, large_free_stat.size * SINT);
    ST_NUMBER(DID_MEM_LWASTED, large_wasted_stat.counter);
    ST_NUMBER(DID_MEM_LWASTED_SIZE, large_wasted_stat.size);
    ST_NUMBER(DID_MEM_CHUNK, small_chunk_stat.counter);
    ST_NUMBER(DID_MEM_CHUNK_SIZE, small_chunk_stat.size);
    ST_NUMBER(DID_MEM_SMALL, small_alloc_stat.counter);
    ST_NUMBER(DID_MEM_SMALL_SIZE, small_alloc_stat.size);
    ST_NUMBER(DID_MEM_SFREE, small_free_stat.counter);
    ST_NUMBER(DID_MEM_SFREE_SIZE, small_free_stat.size);
    ST_NUMBER(DID_MEM_SWASTED, small_chunk_wasted.counter);
    ST_NUMBER(DID_MEM_SWASTED_SIZE, small_chunk_wasted.size);
    ST_NUMBER(DID_MEM_UNUSED, unused_size);
    ST_NUMBER(DID_MEM_MINC_CALLS, malloc_increment_size_calls);
    ST_NUMBER(DID_MEM_MINC_SUCCESS, malloc_increment_size_success);
    ST_NUMBER(DID_MEM_MINC_SIZE, malloc_increment_size_total);
    ST_NUMBER(DID_MEM_CLIB, clib_alloc_stat.counter);
    ST_NUMBER(DID_MEM_CLIB_SIZE, clib_alloc_stat.size);
    ST_NUMBER(DID_MEM_PERM, perm_alloc_stat.counter);
    ST_NUMBER(DID_MEM_PERM_SIZE, perm_alloc_stat.size);
    ST_NUMBER(DID_MEM_OVERHEAD, T_OVERHEAD * SINT);
    ST_NUMBER(DID_MEM_ALLOCATED, large_alloc_stat.size * SINT
                              - small_free_stat.size
                              - small_chunk_wasted.size
                              - unused_size);
    ST_NUMBER(DID_MEM_USED, large_alloc_stat.size * SINT
                              - small_free_stat.size
                              - small_chunk_wasted.size
                              - unused_size
                              - large_alloc_stat.counter * M_OVERHEAD * SINT
                              - small_alloc_stat.counter * M_OVERHEAD * SINT
                              - xalloc_stat.counter * XM_OVERHEAD_SIZE
             );
    ST_NUMBER(DID_MEM_TOTAL_UNUSED, large_free_stat.size * SINT
                                    + large_wasted_stat.size
                                    + small_free_stat.size
                                    + small_chunk_wasted.size
                                    + unused_size);
#endif
#undef ST_NUMBER
} /* mem_dinfo_data() */


#ifdef GC_SUPPORT
/*     Clear, set, test the 'referenced' marker. */
static void mem_clear_ref (POINTER p) {
    dlmalloc_clear_ref(p);
}

static void mem_mark_ref (POINTER p) {
    dlmalloc_mark_ref(p);
}

static Bool mem_test_ref (POINTER p) {
    return !dlmalloc_test_ref(p);
}

/*     Clear all 'referenced' markers. */
void mem_clear_ref_flags() {
    dlmalloc_clear_ref_flags();
}

/*     Free all memory marked as 'unreferenced'. */
void mem_free_unrefed_memory() {
    dlmalloc_free_unrefed_memory();
}

#ifdef MALLOC_TRACE
/*     Return true if <p> is a free block. */
static Bool mem_is_freed (POINTER p, size_t minsize) {
    return dlmalloc_is_freed(p, minsize);
}
#endif

#endif /* GC_SUPPORT */

/*     the alignment guaranteed by the allocator */
//#define MEM_ALIGN (2*(sizeof(size_t)))
#define MEM_ALIGN (2*SIZEOF_INT)

/*     if the allocator can replace the libc allocation routines. */
#define REPLACE_MALLOC

/* The allocator is threadsafe */
#define MEM_THREADSAFE

/***************************************************************************/
