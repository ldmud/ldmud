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

/*     Return the statistics data. */
void mem_dump_data (strbuf_t *sbuf) {
write(0,"mem_dump_data\n",14);
}

/*     Return the statistics data. */
void mem_dinfo_data (svalue_t *svp, int value) {
write(0, "mem_dinfo_data\n",15);
}

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
