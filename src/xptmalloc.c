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

#   define dump_stat(str,member) strbuf_addf(sbuf, str, stats.member)

    strbuf_add(sbuf, "Type                         Amount\n");
    dump_stat("total non-mmaped:          %8d bytes\n", arena);
    dump_stat("number of free chunks:     %8d\n", ordblks);
    dump_stat("number of fastbins:        %8d\n", smblks);
    dump_stat("number of mmap:            %8d\n", hblks);
    dump_stat("bytes in mmap:             %8d bytes\n", hblkhd);
    dump_stat("max alloced ever:          %8d bytes\n", usmblks);
    dump_stat("total in fastbin:          %8d bytes\n", fsmblks);
    dump_stat("total alloced:             %8d bytes \n", uordblks);
    dump_stat("total free:                %8d bytes\n", fordblks);
    dump_stat("freeable bytes:            %8d bytes\n", keepcost);

#undef dump_stat
} /* mem_dump_data() */


/* Fill in the data for debug_info(DINFO_DATA, DID_MEMORY) into the
 * svalue-block svp.
 */
void mem_dinfo_data (svalue_t *svp, int value) {
    struct mallinfo stats;

#define ST_NUMBER(which,code) \
    if (value == -1) svp[which].u.number = code; \
    else if (value == which) svp->u.number = code

    stats = dlmallinfo();

    if (value == -1)
        put_ref_string(svp+DID_MEM_NAME, STR_PTMALLOC);
    else if (value == DID_MEM_NAME)
        put_ref_string(svp, STR_PTMALLOC);

    ST_NUMBER(DID_MEM_SBRK_SIZE, stats.arena);
    ST_NUMBER(DID_MEM_FREE_CHUNKS, stats.ordblks);
    ST_NUMBER(DID_MEM_FFREE, stats.smblks);
    ST_NUMBER(DID_MEM_FFREE_SIZE, stats.fsmblks);
    ST_NUMBER(DID_MEM_MMAP, stats.hblks);
    ST_NUMBER(DID_MEM_MMAP_SIZE, stats.hblkhd);
    ST_NUMBER(DID_MEM_MAX_ALLOCATED, stats.usmblks);
    ST_NUMBER(DID_MEM_OVERHEAD, PT_OVERHEAD);
    ST_NUMBER(DID_MEM_ALLOCATED, stats.uordblks);
    ST_NUMBER(DID_MEM_USED, stats.uordblks - stats.fordblks);
    ST_NUMBER(DID_MEM_TOTAL_UNUSED, stats.fordblks);
    ST_NUMBER(DID_MEM_KEEP_COST, stats.keepcost);

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

/* Reference a couple of unused variables and functions to avoid
 * unnecessary warning.
 */
void ptmalloc_ref_unused(void)
{
    in_malloc = 0;
    print_block(0, 0);
    count_up(clib_alloc_stat, 0);
}

/***************************************************************************/
