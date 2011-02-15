/*---------------------------------------------------------------------------
 * The Memory Allocator wrapper.
 *
 *---------------------------------------------------------------------------
 * This file #includes the source for the memory allocator selected in
 * in config.h. If sysmalloc is selected, this file provides the basic
 * xalloc()... implementation using malloc()... .
 *
 * It is the task of the memory allocator source included to provide
 * simulations of malloc()... where required.
 *---------------------------------------------------------------------------
 */

#include "driver.h"

#include <ctype.h>
#include <stddef.h>
#include <stdio.h>

#include "xalloc.h"

#include "backend.h"
#include "gcollect.h"
#include "interpret.h"
#include "simulate.h"

#include "exec.h"
#include "object.h"
#include "mstrings.h"

/*-------------------------------------------------------------------------*/

/* Minimum boundary between stack and heap, should be sufficient for
 * error handling (which needs about 10..15KByte).
 * If the gap falls below this value, an error is generated and the
 * gap is no longer checked except for true overlap with the heap.
 */

#define HEAP_STACK_GAP (10 * ERROR_FMT_LEN)


/* A counter type for statistics and its functions.
 */
typedef struct { unsigned long counter, size; } t_stat;
  /* A counter type for statistics and its functions: */

static inline void count_add(t_stat *a, unsigned long b) {
  a->size += b;
}

static inline void count(t_stat *a, unsigned long b) {
  count_add(a, b);
  if (b < 0)
    --a->counter;
  else
    ++a->counter;
}

static inline void count_up(t_stat *a, unsigned long b) {
  count_add(a, b);
  ++a->counter;
}

static inline void count_up_n(t_stat *a, unsigned long b, unsigned long c) {
  count_add(a, b * c);
  a->counter += b;
}

static inline void count_back(t_stat *a, unsigned long b) {
  count_add(a, -b);
  --a->counter;
}

static inline void count_back_n(t_stat *a, unsigned long b, unsigned long c) {
  count_add(a, -(b * c));
  a->counter -= b;
}

typedef p_uint word_t;
  /* Our 'word' type. This should not be changed unless you check the allocators
   * first for assumptions that this is a p_uint.
   */

/*-------------------------------------------------------------------------*/

/* The extra xalloc header fields.
 * GC's write_malloc_trace() expects XM_FILE and XM_LINE at the end 
 * of the header.
 * TODO: Let the GC use the symbolic constants.
 */

enum xalloc_header_fields {
#ifdef MALLOC_LPC_TRACE
    XM_OBJ  = 0,  /* (object_t*) the allocating object */
    XM_PROG = 1,  /* (int32) allocating program's id-number */
    XM_PC   = 2,  /* (bytecode_p) inter_pc at the allocation */
#    ifdef MALLOC_TRACE
         XM_FILE = 3,  /* (const char*) allocating source file */
         XM_LINE = 4,  /* (word_t) allocating line in source file */
         XM_OVERHEAD = 5,
#    else
         XM_OVERHEAD = 3,
#    endif /* MALLOC_TRACE */
#else /* !MALLOC_LPC_TRACE */
#    ifdef MALLOC_TRACE
         XM_FILE = 0,  /* (const char*) allocating source file */
         XM_LINE = 1,  /* (word_t) allocating line in source file */
         XM_OVERHEAD = 2,
#    else
         XM_OVERHEAD = 0,
#    endif /* MALLOC_TRACE */
#endif /* MALLOC_LPC_TRACE */
    XM_OVERHEAD_SIZE = XM_OVERHEAD * sizeof(word_t),
};

/*-------------------------------------------------------------------------*/

/* -- Global Variables/Arguments dealing with memory -- */

Bool out_of_memory = MY_FALSE;              /* True: we are out of memory */
int malloc_privilege = MALLOC_USER; /* Privilege for next allocation */

char *reserved_user_area   = NULL;  /* Reserved memory areas */
char *reserved_master_area = NULL;
char *reserved_system_area = NULL;

mp_int reserved_user_size   = RESERVED_USER_SIZE;   /* The reserved sizes */
mp_int reserved_master_size = RESERVED_MASTER_SIZE;
mp_int reserved_system_size = RESERVED_SYSTEM_SIZE;

/* at startup reserve these amounts of memory for large and small blocks */
mp_int min_malloced       = MIN_MALLOCED;
mp_int min_small_malloced = MIN_SMALL_MALLOCED;
/* this is the hard limit for memory allocations. */
static mp_int max_malloced       = HARD_MALLOC_LIMIT_DEFAULT;
/* this is a soft limit for memory allocations. It serves as a kind of low
 * watermark. If exceeded, the game driver will inform the mudlib by calling
 * low_memory() in the master. */
static mp_int soft_malloc_limit  = SOFT_MALLOC_LIMIT_DEFAULT;

/* Was the low_memory() already called in the master`*/
static Bool low_memory_applied = MY_FALSE;

int stack_direction = 0; /*  0: Unknown stack behaviour
                          * +1: Stack grows upward
                          * -1: Stack grows downward
                          */

static char * initial_stack = NULL; /* The stack at the start of the program */

static int in_malloc = 0;
  /* >0 when the core code in the allocator is executed.
   * This variable serves as primitive safeguard against re-entrant
   * calls to the allocator, for example by threads.
   */

static int going_to_exit = MY_FALSE;
  /* When the allocator detected a fatal error, it sets this
   * variable before starting the fatal() handling in order to
   * prevent recursions.
   */

#ifdef MALLOC_SBRK_TRACE

static size_t mdb_size;
static object_t *mdb_object;
#    if defined(MALLOC_TRACE)
        static const char * mdb_file;
        static int mdb_line;
#    endif
  /* Persistent copy of the latest memory request information,
   * so that the SBRK_TRACE can print it.
   */

#endif /* MALLOC_SBRK_TRACE */

/* --- Statistics --- */

static t_stat xalloc_stat = {0,0};
  /* Total number and size of allocations done by the driver (incl overhead).
   */

#if (defined(MALLOC_SBRK) && defined(SBRK_OK) && defined(MALLOC_REPLACEABLE)) || (defined(HAVE_MMAP) && defined(MALLOC_REPLACEABLE))
static t_stat clib_alloc_stat = {0,0};
  /* Number and size of allocations done through the clib emulation
   * functions (incl overhead).
   */
#endif

/*-------------------------------------------------------------------------*/
/* Forward declarations */

#ifdef MALLOC_LPC_TRACE
static void write_lpc_trace (int d, word_t *p, int oneline) __attribute__((nonnull(2)));
#endif

#ifdef GC_SUPPORT
static void print_block (int d, word_t *block);
#endif /* GC_SUPPORT */

#ifdef MALLOC_SBRK_TRACE

/*-------------------------------------------------------------------------*/
static void
mem_debug_log (const char * name, p_int size)

/* Mem debug log function. Log the given <size> for function <name>
 * to stdout together with the original allocation request size.
 */

{
#if defined(MALLOC_TRACE)
      dprintf4(1, "%s %s(%d) for %d"
                , (p_int)current_time_stamp, (p_int)name
                , (p_int)size, (p_int)mdb_size
                );
      dprintf3(1, " , '%s':%d , obj %s\n"
                , (p_int)mdb_file, (p_int)mdb_line
                , (p_int)(mdb_object ? ( mdb_object->name ? get_txt(mdb_object->name) : "<?>"): "<null>")
                );
#else
      dprintf4(1, "%s %s(%d) for %d"
                , (p_int)current_time_stamp, (p_int)name
                , (p_int)size, (p_int)mdb_size
                );
      dprintf1(1, " , obj %s\n"
                , (p_int)(mdb_object ? ( mdb_object->name ? get_txt(mdb_object->name) : "<?>"): "<null>")
                );
#endif /* MALLOC_TRACE */
} /* mem_debug_log() */

/*-------------------------------------------------------------------------*/
static void
mdb_log_sbrk (p_int size)

/* esbrk() log function: esbrk() is called to allocate <size> bytes
 * from the system. Log this information to stdout together with
 * the original allocation request size.
 */

{
    mem_debug_log("esbrk", size);
} /* mdb_log_sbrk() */

#else 

#define mdb_log_sbrk(size) NOOP

#endif /* MALLOC_SBRK_TRACE */

/*-------------------------------------------------------------------------*/

/* Include the allocator source.
 *
 * The allocator is free to use any information found in xalloc.
 * In return, the allocator has to provide this interface:
 *
 *   static POINTER mem_alloc(size_t size)
 *     to allocate a memory block
 *
 *   static void mem_free(POINTER p)
 *     to deallocate a memory block
 *
 *   static POINTER mem_realloc (POINTER p, size_t size)
 *     reallocate a block.
 *
 *   static void * mem_increment_size (void *vp, size_t size)
 *     Increase a block size in place. If not possible, return NULL.
 *
 *   static static size_t mem_block_size (POINTER p)
 *     Return the size of an allocated block.
 *     If this value is not available, implement this function as a dummy,
 *     but also #define NO_MEM_BLOCK_SIZE.
 *     For Garbage Collection or replacing malloc() this function must be
 *     valid!
 *
 *   static static size_t mem_overhead ()
 *     Return the size of the allocators internal overhead.
 *
 *   static void mem_mark_permanent (POINTER p)
 *   static void mem_mark_collectable (POINTER p)
 *     Mark a block as permanent resp. collectable
 *
 *   void mem_consolidate (bool force)
 *     Do whatever consolidation is useful.
 *     <force> is true after a GC, and false when called from the backend.
 *
 *   void mem_dump_data (strbuf_t *sbuf)
 *   void mem_dump_extdata (strbuf_t *sbuf)
 *   void mem_dinfo_data (svalue_t *svp, int value)
 *     Return the statistics data.
 *
 *   Bool mem_dump_memory (int fd)
 *     Dump the memory layout to <fd>, or return FALSE.
 *     If <fd> is -1, just return TRUE or FALSE (this is used to check if
 *     the allocator supports memory dumps).
 *
#ifdef MALLOC_EXT_STATISTICS
 *   void mem_update_stats (void)
 *     Update whatever extended statistics are available.
 *     Called every backend cycle or so to allow for the calculation of
 *     averages over time.
 *
#endif
#ifdef GC_SUPPORT
 *   static void mem_clear_ref (POINTER p)
 *   static void mem_mark_ref (POINTER p)
 *   static Bool mem_test_ref (POINTER p)
 *     Clear, set, test the 'referenced' marker.
 *
 *   void mem_clear_ref_flags()
 *     Clear all 'referenced' markers.
 *
 *   void mem_free_unrefed_memory()
 *     Free all memory marked as 'unreferenced'.
 *     This routine also has to accordingly adjust xalloc_stat.
 *
#ifdef MALLOC_TRACE
 *   static Bool mem_is_freed (POINTER p, size_t minsize)
 *     Return true if <p> is a free block.
#endif
#endif
 *
 *   #define REPLACE_MALLOC
 *     if the allocator's mem_alloc()/mem_free() can be used to replace the
 *     libc allocation routines (ie. the allocator doesn't use malloc()
 *     itself).  The actual replacement is provided by xalloc.
 *   #define MEM_MAIN_THREADSAFE
 *     The allocator is safe to use from the main thread.
 *   #define MEM_THREADSAFE
 *     The allocator is altogether threadsafe.
 */

#if defined(MALLOC_smalloc)
#  include "smalloc.c"
#elif defined(MALLOC_slaballoc)
#  include "slaballoc.c"
#elif defined(MALLOC_sysmalloc)
#  include "sysmalloc.c"
#else
#  error "No allocator specified."
#endif

#ifdef NO_MEM_BLOCK_SIZE
#  if defined(GC_SUPPORT) || defined(REPLACE_MALLOC)
#    error "For GC_SUPPORT or REPLACE_MALLOC, mem_block_size() must exist!"
#  endif
#endif

#if defined(USE_SQLITE) && defined(SQLITE3_USES_PTHREADS) && !defined(MEM_THREADSAFE) && !defined(MEM_MAIN_THREADSAFE)
#    warning ""
#    warning "-----------------------------------"
#    warning "SQLite3 uses PThreads, but the allocator"
#    warning "is not threadsafe!"
#    warning "-----------------------------------"
#    warning ""
#endif


/*-------------------------------------------------------------------------*/
size_t
xalloced_size (POINTER p
#ifdef NO_MEM_BLOCK_SIZE
                         UNUSED
#endif /* NO_MEM_BLOCK_SIZE */
              )

/* Return the allocation size (incl. overhead) of the block <p>.
 */

{
#ifndef NO_MEM_BLOCK_SIZE
    return mem_block_size((word_t*)p - XM_OVERHEAD);
#else
#   ifdef __MWERKS__
#       pragma unused(p)
#   endif
    return 0;
#endif
} /* xalloced_size() */

/*-------------------------------------------------------------------------*/
size_t
xalloc_overhead (void)

/* Return the total overhead of an allocation - xalloc and allocator.
 */

{
    return mem_overhead() + XM_OVERHEAD_SIZE;
} /* xalloc_overhead() */

/*-------------------------------------------------------------------------*/
static Bool
retry_alloc (size_t size MTRACE_DECL)

/* An allocation for <size> bytes just failed - try to free the reserves.
 * Return TRUE if the allocation can be retried, FALSE is not.
 * If allocation privilege is SYSTEM and the allocation can't be retried,
 * abort the driver.
 */

{
    /* Out of memory - try to recover */

    static char mess1[] =
        "Temporarily out of MEMORY. Freeing user reserve.\n";
    static char mess2[] =
        "Temporarily out of MEMORY. Freeing master reserve.\n";
    static char mess3[] =
        "Temporarily out of MEMORY. Freeing system reserve.\n";
    static char mess4[] =
        "Totally out of MEMORY.\n";
    static char mess_d1[] =
        "Low on MEMORY: Trying to allocate ";
    static char mess_d2[] =
        " bytes for ";
    static char mess_d3[] =
        " bytes request";
    static char mess_d4[] =
        " (";
    static char mess_d7[] =
        ", prog ";
    static char mess_d6[] =
        ")";
#if defined(MALLOC_TRACE)
    static char mess_d5[] =
        " line ";
#endif
    static char mess_nl[] =
        "\n";

    /* Print the Out-Of-Mem diagnostic */
    writes(2, mess_d1);
    writed(2, size);
    writes(2, mess_d2);
    writed(2, size - XM_OVERHEAD_SIZE);
    writes(2, mess_d3);
#ifdef MALLOC_TRACE
    writes(2, mess_d4);
    writes(2, malloc_trace_file);
    writes(2, mess_d5);
    writed(2, malloc_trace_line);
    writes(2, mess_d6);
#endif
    writes(2, mess_d4);
    writes(2, current_object ? get_txt(current_object->name) : "<null>");
    writes(2, mess_d7);
    writes(2, current_prog ? get_txt(current_prog->name) : "<null>");
    writes(2, mess_d6);
    writes(2, mess_nl);

    /* Free the next reserve, the try again */

    if (gc_request == gcDont)
        gc_request = gcMalloc;
    extra_jobs_to_do = MY_TRUE;
    if (reserved_user_area)
    {
        xfree(reserved_user_area);
        reserved_user_area = NULL;
        writes(2, mess1);
        return MY_TRUE;
    }

    if (malloc_privilege >= MALLOC_MASTER && reserved_master_area)
    {
        xfree(reserved_master_area);
        reserved_master_area = NULL;
        writes(2, mess2);
        return MY_TRUE;
    }
    if (malloc_privilege >= MALLOC_SYSTEM && reserved_system_area)
    {
        xfree(reserved_system_area);
        reserved_system_area = 0;
        writes(2, mess3);
        return MY_TRUE;
    }

    if (malloc_privilege < MALLOC_SYSTEM)
    {
        out_of_memory = MY_TRUE;
        return MY_FALSE;
    }

    /* Totally out of memory: exit */
    max_malloced = 0; /* Disable the checking for a clean exit */
    going_to_exit = MY_TRUE; /* Prevent recursions */
    writes(2, mess4);
    (void)dump_trace(MY_FALSE, NULL);
    fatal("Out of memory (%lu bytes)\n", (unsigned long)size);
    /* NOTREACHED */
    return MY_FALSE;
} /* retry_alloc() */

/*-------------------------------------------------------------------------*/
static INLINE Bool
check_max_malloced (void)

/* If max_malloced is set, check if the allocated memory exceeds it.
 * If not, return FALSE.
 * If yes, and malloc_privilege < MALLOC_SYSTEM: return TRUE.
 * If yes, and malloc_privilege == MALLOC_SYSTEM: abort.
 */

{
#ifndef NO_MEM_BLOCK_SIZE
    if (max_malloced > 0 && (mp_int)xalloc_stat.size > max_malloced)
    {
        static const char mess[] = "HARD_MALLOC_LIMIT reached.\n";
        writes(2, mess);
        if (malloc_privilege < MALLOC_SYSTEM)
            return MY_TRUE;

        /* Totally out of memory: exit */
        max_malloced = 0; /* Disable the checking for a clean exit */
        going_to_exit = MY_TRUE; /* Prevent recursions */
        (void)dump_trace(MY_FALSE, NULL);
        fatal("Out of memory.\n");
        /* NOTREACHED */
    }
#endif
    return MY_FALSE;
} /* check_max_malloced() */

/*-------------------------------------------------------------------------*/
POINTER
xalloc_traced (size_t size MTRACE_DECL)

/* Allocate <size> bytes of memory like malloc() does.
 * This function catches out of memory situations and tries to recover
 * from them by using the reserved memory areas. If it totally runs
 * out of memory, the program exit()s with code 3.
 */

{
    word_t *p;

    if (going_to_exit) /* A recursive call while we're exiting */
        exit(3);

#ifdef MALLOC_SBRK_TRACE
    mdb_size = size;
    mdb_object = current_object;
#ifdef MALLOC_TRACE
        mdb_file = malloc_trace_file;
        mdb_line = malloc_trace_line;
#endif
#endif /* MALLOC_SBRK_TRACE */

    size += XM_OVERHEAD_SIZE;

    do {
        p = mem_alloc(size);
    } while (p == NULL && retry_alloc(size MTRACE_PASS));

    if (p == NULL)
    {
        return NULL;
    }

#ifdef MALLOC_TRACE
        p[XM_FILE] = (word_t)malloc_trace_file;
        p[XM_LINE] = (word_t)malloc_trace_line;
#endif
#ifdef MALLOC_LPC_TRACE
        p[XM_OBJ]  = (word_t)current_object;
        p[XM_PROG] = current_prog ? current_prog->id_number : 0;
        p[XM_PC]   = (word_t)inter_pc;
#endif
#ifdef NO_MEM_BLOCK_SIZE
    count_up(&xalloc_stat, XM_OVERHEAD_SIZE);
#else
    count_up(&xalloc_stat, mem_block_size(p));
    if (check_max_malloced())
        return NULL;
#endif
    return (POINTER)(p + XM_OVERHEAD);
} /* xalloc_traced() */

/*-------------------------------------------------------------------------*/
void
xfree (POINTER p)

/* Free the memory block <p>.
 */

{
    if (NULL != p)
    {
        word_t *q = (word_t*)p - XM_OVERHEAD;
#ifdef NO_MEM_BLOCK_SIZE
        count_back(&xalloc_stat, XM_OVERHEAD_SIZE);
#else
        count_back(&xalloc_stat, mem_block_size(q));
#endif
        mem_free(q);
    }
} /* xfree() */

/*-------------------------------------------------------------------------*/
POINTER
pxalloc_traced (size_t size MTRACE_DECL)

/* Allocate a block of <size> bytes - like xalloc(), just that the
 * memory is not subject to GC.
 */

{
    POINTER temp;

    temp = xalloc_traced(size MTRACE_PASS);
    if (temp)
    {
        mem_mark_permanent((word_t *)temp - XM_OVERHEAD);
    }
    return temp;
} /* pxalloc_traced() */

/*-------------------------------------------------------------------------*/
void
pfree (POINTER p)

/* Deallocate a permanent block <p>.
 */

{
    if (p)
    {
        mem_mark_collectable((word_t *)p - XM_OVERHEAD);
    }
    xfree(p);
} /* pfree() */

/*-------------------------------------------------------------------------*/
POINTER
prexalloc_traced (POINTER p, size_t size MTRACE_DECL)

/* Reallocate block <p> to the new size of <size> and return the pointer.
 * The memory is not subject to GC.
#ifdef MALLOC_TRACE
 * The trace arguments are admittedly unused in the function, but come
 * in handy if the allocation code needs to be instrumented for debugging
 * purposes.
#endif
 */

{
    POINTER temp;

    if (p)
    {
        mem_mark_collectable((word_t *)p - XM_OVERHEAD);
    }
    temp = rexalloc_traced(p, size MTRACE_PASS);
    if (temp)
    {
        mem_mark_permanent((word_t *)temp - XM_OVERHEAD);
    }
    return temp;
} /* prexalloc_traced() */

/*-------------------------------------------------------------------------*/
void *
malloc_increment_size (void *vp, size_t size)

/* Try to extent the allocation block for <vp> in place to hold <size> more
 * bytes. If this is not possible, return NULL, otherwise return a pointer
 * to the start of the block extension.
 */
{
    word_t * block = (word_t*)vp - XM_OVERHEAD;
#ifndef NO_MEM_BLOCK_SIZE
    size_t old_size;
#endif
    void * rc;

    if (going_to_exit) /* A recursive call while we're exiting */
        exit(3);

#ifndef NO_MEM_BLOCK_SIZE
    old_size = mem_block_size(block);
#endif

    rc = mem_increment_size(block, size);

#ifndef NO_MEM_BLOCK_SIZE
    if (rc != NULL)
    {
        count_back(&xalloc_stat, old_size);
        count_up(&xalloc_stat, mem_block_size(block));
        if (check_max_malloced())
            return NULL;
    }
#endif

    return rc;
} /* malloc_increment_size() */

/*-------------------------------------------------------------------------*/
POINTER
rexalloc_traced (POINTER p, size_t size MTRACE_DECL
#ifndef MALLOC_SBRK_TRACE
                                                    UNUSED
#endif /* MALLOC_SBRK_TRACE */
                )

/* Reallocate block <p> to the new size of <size> and return the pointer.
 * The memory is not aligned and subject to GC.
#ifdef MALLOC_TRACE
 * The trace arguments are admittedly unused in the function, but come
 * in handy if the allocation code needs to be instrumented for debugging
 * purposes.
#endif
 */

{
#ifndef MALLOC_SBRK_TRACE
#ifdef MALLOC_TRACE
#   ifdef __MWERKS__
#       pragma unused(malloc_trace_file)
#       pragma unused(malloc_trace_line)
#   endif
#endif /* MALLOC_TRACE */
#endif /* MALLOC_SBRK_TRACE */

    word_t *block, *t;
#ifndef NO_MEM_BLOCK_SIZE
    size_t old_size;
#endif

    if (going_to_exit) /* A recursive call while we're exiting */
        exit(3);

    if (!p)
    {
        return xalloc_traced(size MTRACE_ARG);
    }

#ifdef MALLOC_SBRK_TRACE
    mdb_size = size;
    mdb_object = current_object;
#ifdef MALLOC_TRACE
        mdb_file = malloc_trace_file;
        mdb_line = malloc_trace_line;
#endif
#endif /* MALLOC_SBRK_TRACE */

    size += XM_OVERHEAD_SIZE;
    block = (word_t *)p - XM_OVERHEAD;

#ifndef NO_MEM_BLOCK_SIZE
    old_size = mem_block_size(block);
    t = malloc_increment_size(p, size - old_size);
    if (t)
        return p;
#endif

    do {
        t = mem_realloc(block, size);
    } while (t == NULL && retry_alloc(size MTRACE_ARG));

    if (t)
    {
#ifndef NO_MEM_BLOCK_SIZE
        count_back(&xalloc_stat, old_size);
        count_up(&xalloc_stat, mem_block_size(t));
        if (check_max_malloced())
            return NULL;
#endif
        t += XM_OVERHEAD;
    }
    
    return (POINTER)t;
} /* rexalloc() */

/*=========================================================================*/

/*                          GARBAGE COLLECTOR                              */

#ifdef GC_SUPPORT
/*-------------------------------------------------------------------------*/
void
x_clear_ref (POINTER p)

/* GC Support: Clear the 'referenced' flag for block <p>.
 */

{
    mem_clear_ref((word_t *)p - XM_OVERHEAD);
} /* x_clear_ref() */

/*-------------------------------------------------------------------------*/
int
x_mark_ref (POINTER p)

/* GC Support: Set the 'referenced' flag for block <p>.
 * This function returns a value (1) so that it can be used in macros
 * more easily.
 */
{
    mem_mark_ref((word_t *)p - XM_OVERHEAD);
    return 1;
} /* x_mark_ref() */

/*-------------------------------------------------------------------------*/
Bool
x_test_ref (POINTER p)

/* GC Support: Check the memory block marker for <p>, return TRUE if _not_
 * set.
 */

{
    return mem_test_ref((word_t *)p - XM_OVERHEAD);
} /* x_test_ref() */

/*-------------------------------------------------------------------------*/

#ifdef MALLOC_TRACE

static int num_dispatched_types = 0;
  /* Used size of the dispatch_table
   */

static struct {
    char *file;
    word_t line;
    void (*func)(int, void *, int);
} dispatch_table[12];
  /* The dispatch table used to recognize and print datablocks.
   * The recognition is simple and uses the file/line information received
   * from sample allocations done by gcollect. If an allocation matches
   * one entry in the table, the function is called with (filedescriptor,
   * blockaddress, 0).
   */

#ifdef CHECK_OBJECT_GC_REF
/*-------------------------------------------------------------------------*/
/* Some extra variables to explicitely store the location of program
 * and object allocations.
 */

static char * object_file;
static word_t object_line;
static char * program_file;
static word_t program_line;

void
note_object_allocation_info ( void *block )
{
    object_file = (char *)((word_t*)block)[XM_FILE-XM_OVERHEAD];
    object_line = ((word_t*)block)[XM_LINE-XM_OVERHEAD];
}
void
note_program_allocation_info ( void *block )
{
    program_file = (char *)((word_t*)block)[XM_FILE-XM_OVERHEAD];
    program_line = ((word_t*)block)[XM_LINE-XM_OVERHEAD];
}

Bool
is_object_allocation ( void *block )
{
    return (object_file == (char *)((word_t*)block)[XM_FILE-XM_OVERHEAD])
        && (object_line == ((word_t*)block)[XM_LINE-XM_OVERHEAD]);
}
Bool
is_program_allocation ( void *block )
{
    return (program_file == (char *)((word_t*)block)[XM_FILE-XM_OVERHEAD])
        && (program_line == ((word_t*)block)[XM_LINE-XM_OVERHEAD]);
}
#endif

/*-------------------------------------------------------------------------*/
void
store_print_block_dispatch_info (void *block
                                , void (*func)(int, void *, int)
                                )

/* Add a new block type: get the file/line information from the
 * allocation <block> and store it with the <func>tion.
 */

{
    int i;

    i = num_dispatched_types++;
    if (i >= (int)(sizeof(dispatch_table)/sizeof(dispatch_table[0])))
    {
        writes(2, "dispatch_table for print_block() to small\n");
        return;
    }

    dispatch_table[i].file = (char *)((word_t*)block)[XM_FILE-XM_OVERHEAD];
    dispatch_table[i].line = ((word_t*)block)[XM_LINE-XM_OVERHEAD];
    dispatch_table[i].func = func;
} /* store_print_block_dispatch_info() */

/*-------------------------------------------------------------------------*/
Bool
is_freed (void *p, p_uint minsize)

/* Check if block for the allocation <p> is a free block of at least
 * <minsize>. Blocks outside the heap always qualify.
 * The function might return false for joined blocks.
 */

{
    word_t *block;

    block = (word_t *) p - XM_OVERHEAD;

    return mem_is_freed(block, minsize + XM_OVERHEAD_SIZE);
} /* is_freed() */

#endif /* MALLOC_TRACE */

/*-------------------------------------------------------------------------*/
static void
print_block (int d, word_t *block)

/* Block <block> was recognized as lost - print it onto file <d>.
 * If possible, use the information in the dispatch_table, otherwise
 * print the first characters as they are.
 */

{
    word_t size;
    int i;

#ifdef MALLOC_TRACE
    char *file = (char *)block[XM_FILE];
    word_t line = block[XM_LINE];

    for (i = num_dispatched_types; --i >= 0; )
    {
        if (dispatch_table[i].file == file
         && dispatch_table[i].line == line)
        {
            (*dispatch_table[i].func)(d, (char *)(block+XM_OVERHEAD), 0);
            write(d, "\n", 1);
            return;
        }
    }
#endif

    /* Print a hexdump, but not more than 80 bytes */
    {
        int limit = 80;
        char * cp;

        size = mem_block_size(block) - XM_OVERHEAD;
        cp = (char *)(block + XM_OVERHEAD);

        while (size > 0 && limit > 0)
        {
            /* Start of line: print the address */
            dprintf1(d, "%x:", (p_int)cp);

            /* Print the up to 16 bytes after cp as hex values */
            for (i = 0; i < 16 && i < (int)size && i < limit; i++)
                dprintf1(d, " %X", cp[i]);

            /* Align foward to the character interpretation */
            for (; i < 16; i++)
                writes(d, "   ");

            writes(d, "  ");

            /* Print the same data as characters */
            for (i = 0; i < 16 && i < (int)size && i < limit; i++)
            {
                if (isprint((unsigned char)cp[i]))
                    write(d, cp+i, 1);
                else
                    writes(d, ".");
            }

            writes(d, "\n");

            cp += i;
            size -= i;
            limit -= i;
        }
    }

    writes(d, "\n");
} /* print_block() */

#endif /* GC_SUPPORT */

/*-------------------------------------------------------------------------*/
#ifdef MALLOC_LPC_TRACE

static void
write_lpc_trace (int d, word_t *p, int oneline)

/* Write the object and program which allocated the memory block <p>
 * onto file <d>.
 * if <oneline> is true, all information is printed in one line.
 */

{
    object_t *obj, *o;
    bytecode_p pc;
    program_t *prog;
    int line;
    int32 id;

    /* Try to find the object which allocated this block */
    if (!oneline)
    {
        if ( NULL != (obj = (object_t *)p[XM_OBJ]) )
        {
            writes(d, "  By object: ");
            if (obj->flags & O_DESTRUCTED)
                writes(d, "(destructed) ");
            for (o = obj_list; o && o != obj; o = o->next_all) NOOP;
            if (!o)
                writes(d, "(not in list) ");
            else if (o->name)
                writes(d, get_txt(o->name)); /* TODO: If this cores, it has to go again */
        }
        else
            writes(d, "  No object.");
        writes(d, "\n");
    }
    else
    {
        if ( NULL != (obj = (object_t *)p[XM_OBJ]) )
        {
            for (o = obj_list; o && o != obj; o = o->next_all) NOOP;
            if (!o || !o->name)
                writes(d, "?");
            else
                writes(d, get_txt(o->name)); /* TODO: If this cores, it has to go again */

            if (obj->flags & O_DESTRUCTED)
                writes(d, " (destructed)");
        }
        else
            writes(d, "-");
    }

    /* Try to find the program which allocated this block */
    if ( 0 != (id = p[XM_PROG]) )
    {
        pc = NULL;
        prog = NULL;

        for ( o = obj_list
            ;    o
              && !(o->flags & O_DESTRUCTED)
              && ((p_int)o->prog&1 || o->prog->id_number != id); )
            o = o->next_all;

        /* Unlikely, but possible: ids might have been renumbered. */
        if (o)
        {
            pc = (bytecode_p)p[XM_PC];
            prog = o->prog;
            if (prog->program > pc || pc > PROGRAM_END(*prog))
                o = NULL;
        }

        if (o)
        {
            string_t *file;

            line = get_line_number(pc, prog, &file);
            if (!oneline)
            {
                dprintf2(d, "  By program: %s line:%d\n", (p_int)get_txt(file), line);
            }
            else
            {
                dprintf2(d, " , %s %d", (p_int)get_txt(file), line);
            }

            if (file)
                free_mstring(file);
        }
        else
        {
            if (!oneline)
                writes(d, "  By program: Not found at old address.\n");
        }
    }

    if (oneline)
        writes(d, "\n");
} /* write_lpc_trace() */

#endif /* MALLOC_LPC_TRACE */

/*-------------------------------------------------------------------------*/
void
dump_lpc_trace (int d
               , void *p
#ifndef MALLOC_LPC_TRACE
                         UNUSED
#endif
               )

/* Write the object and program which allocated the memory block <p>
 * onto file <d>.
 * In contrast to write_lpc_trace(), the address of the memory block is
 * the one returned by xalloc(), ie. pointing after the memory block
 * header.
 */

{
#if defined(MALLOC_LPC_TRACE)
    write_lpc_trace(d, ((word_t *)p) - XM_OVERHEAD, MY_FALSE);
#else
#   ifdef __MWERKS__
#       pragma unused(p)
#   endif
    writes(d, "No malloc lpc trace.\n");
#endif /* MALLOC_LPC_TRACE */
} /* dump_lpc_trace() */

/*-------------------------------------------------------------------------*/
void
dump_malloc_trace (int d
                  , void *adr
#if !defined(MALLOC_TRACE) && !defined(MALLOC_LPC_TRACE)
                              UNUSED
#endif
                  )

/* Write the allocation information (file, linenumber, object and such) of
 * the memory block <adr> onto file <d>.
 * <adr> is the address returned by xalloc(), ie. pointing after the memory
 * block header.
 */

{
#if !defined(MALLOC_TRACE) && !defined(MALLOC_LPC_TRACE)
#   ifdef __MWERKS__
#       pragma unused(adr)
#   endif
    writes(d, "No malloc trace.\n");
#else
    word_t *p = ((word_t *)adr) - XM_OVERHEAD;

#    ifdef MALLOC_TRACE
        word_t size = mem_block_size(p);

        dprintf3(d, " %s %d size 0x%x\n",
                  p[XM_FILE], p[XM_LINE], size
                );
#    endif
#    ifdef MALLOC_LPC_TRACE
        write_lpc_trace(d, p, MY_FALSE);
#    endif
#endif
} /* dump_malloc_trace() */

/*=========================================================================*/

/*                     CLIB ALLOCATION FUNCTIONS                           */

#if defined(REPLACE_MALLOC)

/*-------------------------------------------------------------------------*/
POINTER
malloc (size_t size)

/* Allocate an empty memory block of size <sizel>.
 * The memory is aligned and not subject to GC.
 */

{
    POINTER result = pxalloc(size);
    if (!result)
    {
        int save_privilege = malloc_privilege;
        malloc_privilege = MALLOC_SYSTEM;
        result = pxalloc(size);
        malloc_privilege = save_privilege;
    }
    
    if (result)
    {
        count_up(&clib_alloc_stat, xalloced_size(result) + mem_overhead());
    }

    return result;
} /* malloc() */

/*-------------------------------------------------------------------------*/
FREE_RETURN_TYPE
free (POINTER ptr)

/* Free the memory block <ptr> which was allocated with malloc().
 */

{
    if (ptr)
    {
        count_back(&clib_alloc_stat, xalloced_size(ptr) + mem_overhead());
    }

    pfree(ptr);
    FREE_RETURN
} /* free() */

/*-------------------------------------------------------------------------*/
POINTER
calloc (size_t nelem, size_t sizel)

/* Allocate an empty memory block for <nelem> objects of size <sizel>.
 * The memory is aligned and not subject to GC.
 */

{
    char *p;

    if (nelem == 0 || sizel == 0)
        return NULL;
    p = malloc(nelem * sizel);
    if (p == NULL)
        return NULL;
    memset(p, '\0', nelem * sizel);
    return p;
} /* calloc() */

/*-------------------------------------------------------------------------*/
POINTER
realloc (POINTER p, size_t size)

/* Reallocate block <p> to the new size of <size> and return the pointer.
 * The memory is aligned and not subject to GC.
 */

{
   size_t old_size;
   POINTER t;

   if (!p)
        return malloc(size);

   old_size = xalloced_size(p) - XM_OVERHEAD_SIZE; // usable size

   if (old_size >= size)
      return p;

   t = malloc(size);
   if (t == NULL)
       return NULL;

   memcpy(t, p, old_size);
   free(p);

   return t;
} /* realloc() */

#endif /* REPLACE_MALLOC */

/* ======================================================================= */

/*-------------------------------------------------------------------------*/
void
get_stack_direction (void)

/* Find the direction of the stackgrowth and store the result (+1 or -1)
 * into the global stack_direction.
 */

{
    char local;  /* to get stack address */

    if (initial_stack == NULL)  /* initial call */
    {
        initial_stack = &local;
        get_stack_direction ();  /* recurse once */
    }
    else  /* recursive call */
    if (&local > initial_stack)
        stack_direction = 1;    /* stack grew upward */
    else
        stack_direction = -1;   /* stack grew downward */
} /* get_stack_direction() */

/*-------------------------------------------------------------------------*/
void
assert_stack_gap (void)

/* Test if the stack is far enough away from the heap area and throw
 * an error if not.
 */

{
    static enum { Initial, Normal, Error, Fatal } condition = Initial;
    char * stack_start, * stack_end;
    ptrdiff_t gap;
    char local; /* used to yield a stack address */

    /* Don't check the gap after a Fatal error or if the system is
     * not fully initialised yet.
     */
    if (stack_direction == 0 || condition == Fatal || heap_end == NULL)
        return;

    /* On the first call, test if checking the gap actually makes sense.
     * If the stack-gap check is not necessary, the 'condition' will be set to
     * Fatal, otherwise the check will be enabled by setting condition to
     * 'Normal'.
     */
    if (condition == Initial)
    {
        /* Currently there are no limitations on checking the heap/stack gap.
         */
        condition = Normal;
    }

    /* Determine the stack limits */
    if (stack_direction < 0)
    {
        stack_start = &local;
        stack_end = initial_stack;
    }
    else
    {
        stack_start = initial_stack;
        stack_end = &local;
    }

    /* Check if the heap and stack overlap */

    if ((stack_end > (char *)heap_end && stack_start < (char *)heap_end)
     || (stack_end > (char *)heap_start && stack_start < (char *)heap_start)
       )
    {
        if (condition != Fatal)
        {
            condition = Fatal;
            fatal("Out of memory: Stack (%p..%p) overlaps heap (%p..%p).\n"
                 , stack_start, stack_end, heap_start, heap_end);
            /* NOTREACHED */
        }
        return; /* else: Recursive call during fatal() handling */
    }

    /* Check if the stack grows towards the heap. If it doesn't
     * we don't have to worry about the gap.
     */
    if ((stack_direction > 0) ? (stack_start > (char *)heap_end)
                              : (stack_end < (char *)heap_start)
       )
    {
        /* No worries about the gap */
        condition = Normal;
        return;
    }

    /* Heap and stack may overlap - do the normal gap checking.
     * Note that on machines with big address spaces the computation
     * may overflow.
     */
    if (stack_direction < 0)
        gap = (char *)stack_start - (char *)heap_end;
    else
        gap = (char *)heap_start - stack_end;

    if (gap < 0)
        gap = HEAP_STACK_GAP + 1;

    /* If the gap is big enough, mark that condition and return */
    if (gap >= HEAP_STACK_GAP)
    {
        condition = Normal;
        return;
    }

    /* The gap is too small.
     * Throw an error only if the condition was normal before,
     * otherwise the error handling would again get an error.
     */
    if (condition == Normal)
    {
        condition = Error;
        errorf("Out of memory: Gap between stack and heap: %ld.\n"
             , (long)gap);
        /* NOTREACHED */
    }
} /* assert_stack_gap() */

/*-------------------------------------------------------------------------*/
void
reserve_memory (void)

/* Reserve the memory blocks according to reserved_xxx_area, and the
 * min_{small}_malloced limits.
 */

{
    void * ptr;

    /* First, check if max_malloced is a sensible value.
     * We overestimate the requirement a bit...
     */
    if (max_malloced > 0)
    {
        mp_int required_mem = 0;
        mp_int required_reserve = 0;

        if (reserved_user_size > 0) required_reserve += reserved_user_size;
        if (reserved_master_size > 0) required_reserve += reserved_master_size;
        if (reserved_system_size > 0) required_reserve += reserved_system_size;

        if (min_malloced > 0)
        {
            if (min_malloced > required_reserve)
                required_mem += min_malloced;
            else
                required_mem += required_reserve + min_malloced;
        }
        else
            required_mem += required_reserve;

        if (min_small_malloced > 0) required_mem += min_small_malloced;

        if (max_malloced < required_mem)
        {
            printf("%s max_malloced is %ld bytes, "
                   "but driver requires %ld bytes.\n"
                  , time_stamp(), (long)max_malloced, (long)required_mem
                  );
            debug_message("%s max_malloced is %ld bytes, "
                          "but driver requires %ld bytes.\n"
                         , time_stamp(), (long)max_malloced, (long)required_mem
                         );
            max_malloced = required_mem;
        }
    }

    if (min_malloced > 0)
    {
        ptr = xalloc(min_malloced);

        if (ptr)
            xfree(ptr);
        else
        {
            printf("%s Failed to allocate MIN_MALLOCED block of %ld bytes.\n"
                  , time_stamp(), (long)min_malloced);
            debug_message("%s Failed to allocate MIN_MALLOCED block of %ld bytes.\n"
                  , time_stamp(), (long)min_malloced);
        }
    }

#ifdef MALLOC_smalloc

    if (min_small_malloced > 0)
        small_chunk_size = min_small_malloced;

#endif /* MALLOC_smalloc */

    if (reserved_system_size > 0)
    {
        reserved_system_area = xalloc((size_t)reserved_system_size);
        if (reserved_system_area == NULL)
        {
            printf("%s Failed to allocate system reserve of %ld bytes.\n"
                  , time_stamp(), (long)reserved_system_area);
            debug_message("%s Failed to allocate system reserve of %ld bytes.\n"
                  , time_stamp(), (long)reserved_system_area);
        }
    }
    if (reserved_master_size > 0)
    {
        reserved_master_area = xalloc((size_t)reserved_master_size);
        if (reserved_master_area == NULL)
        {
            printf("%s Failed to allocate master reserve of %ld bytes.\n"
                  , time_stamp(), (long)reserved_master_area);
            debug_message("%s Failed to allocate master reserve of %ld bytes.\n"
                  , time_stamp(), (long)reserved_master_area);
        }
    }
    if (reserved_user_size > 0)
    {
        reserved_user_area = xalloc((size_t)reserved_user_size);
        if (reserved_user_area == NULL)
        {
            printf("%s Failed to allocate user reserve of %ld bytes.\n"
                  , time_stamp(), (long)reserved_user_area);
            debug_message("%s Failed to allocate user reserve of %ld bytes.\n"
                  , time_stamp(), (long)reserved_user_area);
        }
    }
} /* reserve_memory() */

/*-------------------------------------------------------------------------*/
void
reallocate_reserved_areas (void)

/* Try to reallocate the reserved memory areas. If this is possible,
 * a pending slow-shutdown is canceled and the out_of_memory flag is reset.
 */

{
    char *p;
    malloc_privilege = MALLOC_USER;
    if (reserved_system_size && !reserved_system_area) {
        if ( !(reserved_system_area = xalloc((size_t)reserved_system_size)) ) {
            slow_shut_down_to_do = 1;
            return;
        }
        else {
            p = "Reallocated System reserve.\n";
            write(1, p, strlen(p));
        }
    }
    if (reserved_master_size && !reserved_master_area) {
        if ( !(reserved_master_area = xalloc((size_t)reserved_master_size)) ) {
            slow_shut_down_to_do = 1;
            return;
        }
        else {
            p = "Reallocated Master reserve.\n";
            write(1, p, strlen(p));
        }
    }
    if (reserved_user_size && !reserved_user_area) {
        if ( !(reserved_user_area = xalloc((size_t)reserved_user_size)) ) {
            slow_shut_down_to_do = 6;
            return;
        }
        else {
            p = "Reallocated User reserve.\n";
            write(1, p, strlen(p));
        }
    }
    slow_shut_down_to_do = 0;
    out_of_memory = MY_FALSE;
} /* reallocate_reserved_areas() */

/*-------------------------------------------------------------------------*/
char *
string_copy_traced (const char *str MTRACE_DECL)

/* string_copy() acts like strdup() with the additional bonus that it can
 * trace file/line of the calling place if MALLOC_TRACE is defined.
 */

{
    char *p;
    size_t len;

    len = strlen(str)+1;
    memsafe(p = xalloc_traced(len MTRACE_PASS), len, "string_copy");
    if (p)
    {
        (void)memcpy(p, str, len);
    }
    return p;
} /* string_copy_traced() */

/*-------------------------------------------------------------------------*/
void
notify_lowmemory_condition(enum memory_limit_types what)
/* Calls low_memory(what, <limit>, <memory_consumption>, <reserves> ) 
 * in the master.
 */
{
    short reservestate = 0;
    push_number(inter_sp, what);
    if (what == SOFT_MALLOC_LIMIT_EXCEEDED)
        push_number(inter_sp, soft_malloc_limit);
    else if (what == HARD_MALLOC_LIMIT_EXCEEDED)
        push_number(inter_sp, max_malloced);
    else
        push_number(inter_sp, 0);
    push_number(inter_sp, xalloc_stat.size);
    if (reserved_user_area)
        reservestate |= USER_RESERVE_AVAILABLE;
    if (reserved_master_area)
        reservestate |= MASTER_RESERVE_AVAILABLE;
    if (reserved_system_area)
        reservestate |= SYSTEM_RESERVE_AVAILABLE;
    push_number(inter_sp, reservestate);
    callback_master(STR_LOW_MEMORY, 4);
} /* notify_lowmemory_condition */

/*-------------------------------------------------------------------------*/
void
check_for_soft_malloc_limit (void)
/* If soft_malloc_limit is set, check if the allocated memory exceeds it.
 * If yes and the master was not notified until now, low_memory() is called 
 * in the mudlib master and low_memory_applied ist set to prevent any further
 * notifications.
 * If the limit is not exceeded, low_memory_applied is reset.
 * Should be called from the backend.
 */
{
#ifndef NO_MEM_BLOCK_SIZE
    if (soft_malloc_limit > 0)
    {
        // check first, if the soft limit is > than the hard limit and issue
        // a debug message. (Gnomi wanted to check this here to be able to set
        // a soft limit before a hard limit without error.)
        if (soft_malloc_limit >= max_malloced)
        {
            debug_message("%s The soft memory limit (%"PRIdMPINT") is bigger "
                          "than the hard memory limit (%"PRIdMPINT") - "
                          "disabling the soft limit!",
                          time_stamp(), soft_malloc_limit, max_malloced);
            soft_malloc_limit = 0;
        }
        else if ((mp_int)xalloc_stat.size > soft_malloc_limit)
        {
            if (!low_memory_applied)
            {
                /* call low_memory(malloced_memory) in the master but first
                 * set the flag to prevent calling every backend cycle in case
                 * of errors. */
                low_memory_applied = MY_TRUE;
                notify_lowmemory_condition(SOFT_MALLOC_LIMIT_EXCEEDED);
            }
        }
        else if (low_memory_applied)
        {
            /* OK, memory consumption shrunk below the soft limit. Reset the 
             * warning, so that the next time the limit is exceeded,
             * the master apply is done again. */
            low_memory_applied = MY_FALSE;
        }
    }
#endif
} /* check_for_soft_malloc_limit() */

/*-------------------------------------------------------------------------*/
Bool
set_memory_limit(enum memory_limit_types what, mp_int limit)
/* Sets the <what> memory limit to <limit>.
 * <limit> for SOFT_MLIMIT has to be < HARD_MLIMIT.
 * return TRUE on success and FALSE otherwise.
 * If the current memory allocation is smaller than a new soft limit, the flag
 * low_memory_applied is reset.
 */
{
#ifndef NO_MEM_BLOCK_SIZE
    // limits smaller than the sum of reserves (and some more) are harmful and
    // lead anyway to immediate crashes... But we ignore this here, because
    // reserve_memory() will deal with the needed sizes for the reserves and the
    // minimum allocations. And later on in the game we will just prevent to
    // set the limit smaller then the already allocated memory.
    
    if (what == MALLOC_SOFT_LIMIT)
    {
        if (limit < 0)
            return MY_FALSE;
        
        soft_malloc_limit = limit;
        // reset flag if appropriate.
        if ((mp_int)xalloc_stat.size < soft_malloc_limit
            && low_memory_applied)
            low_memory_applied = MY_FALSE;
    }
    else 
    {
        // setting the limit below the currently allocated memory seems to be 
        // a very bad idea.
        if (limit && limit <= (mp_int)xalloc_stat.size)
            return MY_FALSE;
        
        max_malloced = limit;
    }
    return MY_TRUE;
#else
    return MY_FALSE;
#endif // NO_MEM_BLOCK_SIZE
} /* set_memory_limit */

/*-------------------------------------------------------------------------*/
mp_int
get_memory_limit(enum memory_limit_types what)
/* Return the value of limit <what>. */
{
#ifndef NO_MEM_BLOCK_SIZE
    if (what == MALLOC_SOFT_LIMIT)
        return soft_malloc_limit;
    else
        return max_malloced;
#else
    return 0;
#endif
}

/*-------------------------------------------------------------------------*/
p_int
xalloc_allocated()
/* The amount of memory currently allocated from the allocator, including 
 * the overhead for the allocator.
 */
{
    return mem_mem_allocated();
}

/*-------------------------------------------------------------------------*/
p_int
xalloc_used()
/* The amount of memory currently used for driver data, excluding the 
 * overhead from the allocator.
 */
{
    return mem_mem_used();
}

/***************************************************************************/
