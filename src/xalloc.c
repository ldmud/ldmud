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

#include <stddef.h>
#include <stdio.h>

#include "xalloc.h"

#include "backend.h"
#include "gcollect.h"
#include "interpret.h"
#include "simulate.h"

#ifdef MALLOC_LPC_TRACE
#include "exec.h"
#include "object.h"
#include "mstrings.h"
#endif

/*-------------------------------------------------------------------------*/

/* Minimum boundary between stack and heap, should be sufficient for
 * error handling (which needs about 10..15KByte).
 * If the gap falls below this value, an error is generated and the
 * gap is no longer checked except for true overlap with the heap.
 */

#define HEAP_STACK_GAP (20480)


/* A counter type for statistics and its functions.
 */
typedef struct { unsigned long counter, size; } t_stat;
  /* A counter type for statistics and its functions: */

#define count(a,b)      { a.size+=(b); if ((b)<0) --a.counter; else ++a.counter; }
#define count_up(a,b)   { a.size+=(b); ++a.counter; }
#define count_add(a,b)  { a.size+=(b); }
#define count_back(a,b) { a.size-=(b); --a.counter; }

typedef p_uint word_t;
  /* Our 'word' type.
   */

/*-------------------------------------------------------------------------*/

/* The extra xalloc header fields.
 */

#ifdef MALLOC_LPC_TRACE
#    define XM_OBJ  0  /* (object_t*) the allocating object */
#    define XM_PROG 1  /* (int32) allocating program's id-number */
#    define XM_PC   2  /* (bytecode_p) inter_pc at the allocation */
#    ifdef MALLOC_TRACE
#        define XM_OVERHEAD (6)
#        define XM_FILE   3  /* (const char*) allocating source file */
#        define XM_LINE   4  /* (word_t) allocating line in source file */
#        define XM_MAGIC  5  /* (word_t) The magic word */
#    else
#        define XM_OVERHEAD (3)
#    endif /* MALLOC_TRACE */
#else /* !MALLOC_LPC_TRACE */
#    ifdef MALLOC_TRACE
#        define XM_OVERHEAD (3)
#        define XM_FILE  0  /* (const char*) allocating source file */
#        define XM_LINE  1  /* (word_t) allocating line in source file */
#        define XM_MAGIC 3  /* (word_t) The magic word */
#    else
#        define XM_OVERHEAD (0)
#    endif /* MALLOC_TRACE */
#endif /* MALLOC_LPC_TRACE */

#define XM_OVERHEAD_SIZE (XM_OVERHEAD * sizeof(word_t))

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

mp_int min_malloced       = MIN_MALLOCED;           /* Allocation limits */
mp_int min_small_malloced = MIN_SMALL_MALLOCED;     /* Allocation limits */
mp_int max_malloced       = MAX_MALLOCED;

int stack_direction = 0; /*  0: Unknown stack behaviour
                          * +1: Stack grows upward
                          * -1: Stack grows downward
                          */

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

/* --- Statistics --- */

static t_stat xalloc_stat = {0,0};
  /* Total number and size of allocations done by the driver (incl overhead).
   */

static t_stat clib_alloc_stat = {0,0};
  /* Number and size of allocations done through the clib emulation
   * functions (incl overhead).
   */

/*-------------------------------------------------------------------------*/
/* Forward declarations */

#ifdef MALLOC_LPC_TRACE
static void write_lpc_trace (int d, word_t *p);
#endif

static void print_block (int d, word_t *block);

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
 *   void mem_consolidate ()
 *     Do whatever consolidation is useful.
 *
 *   void mem_dump_data (strbuf_t *sbuf)
 *   void mem_dinfo_data (svalue_t *svp, int value)
 *     Return the statistics data.
 *
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
 *   #define MEM_ALIGN
 *     the alignment guaranteed by the allocator
 *   #define REPLACE_MALLOC
 *     if the allocator's mem_alloc()/mem_free() can be used to replace the
 *     libc allocation routines (ie. the allocator doesn't use malloc()
 *     itself).  The actual replacement is provided by xalloc.
 *   #define MEM_THREADSAFE
 *     The allocator is threadsafe.
 */

#if defined(MALLOC_smalloc)
#  include "smalloc.c"
#elif defined(MALLOC_sysmalloc)
#  include "sysmalloc.c"
#elif defined(MALLOC_ptmalloc)
#  include "xptmalloc.c"
#else
#  error "No allocator specified."
#endif

#ifdef NO_MEM_BLOCK_SIZE
#  if defined(GC_SUPPORT) || defined(REPLACE_MALLOC)
#    error "For GC_SUPPORT or REPLACE_MALLOC, mem_block_size() must exist!"
#  endif
#endif

#if defined(USE_PTHREADS) && !defined(MEM_THREADSAFE)
#    warning ""
#    warning "-----------------------------------"
#    warning "PThreads enabled, but the allocator"
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
#ifdef MALLOC_TRACE
    static char mess_d4[] =
        " (";
    static char mess_d5[] =
        " line ";
    static char mess_d6[] =
        ")";
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
    writes(2, mess_nl);

    /* Free the next reserve, the try again */

    if (gc_request == gcDont)
        gc_request = gcMalloc;
    extra_jobs_to_do = MY_TRUE;
    if (reserved_user_area)
    {
        mem_free(reserved_user_area);
        reserved_user_area = NULL;
        writes(2, mess1);
        return MY_TRUE;
    }

    if (malloc_privilege >= MALLOC_MASTER && reserved_master_area)
    {
        mem_free(reserved_master_area);
        reserved_master_area = NULL;
        writes(2, mess2);
        return MY_TRUE;
    }
    if (malloc_privilege >= MALLOC_SYSTEM && reserved_system_area)
    {
        mem_free(reserved_system_area);
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
    if (max_malloced > 0 && xalloc_stat.size > max_malloced)
    {
        static const char mess[] = "MAX_MALLOCED limit reached.\n";
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

    if (size == 0)
        fatal("Tried to allocate 0 bytes.\n");
    
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
    count_up(xalloc_stat, XM_OVERHEAD_SIZE);
#else
    count_up(xalloc_stat, mem_block_size(p));
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
    word_t *q = (word_t*)p - XM_OVERHEAD;
#ifdef NO_MEM_BLOCK_SIZE
    count_back(xalloc_stat, XM_OVERHEAD_SIZE);
#else
    count_back(xalloc_stat, mem_block_size(q));
#endif
    mem_free(q);
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
} /* pxalloc() */

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
        count_back(xalloc_stat, old_size);
        count_up(xalloc_stat, mem_block_size(block));
        if (check_max_malloced())
            return NULL;
    }
#endif

    return rc;
} /* malloc_increment_size() */

/*-------------------------------------------------------------------------*/
POINTER
rexalloc (POINTER p, size_t size)

/* Reallocate block <p> to the new size of <size> and return the pointer.
 * The memory is not aligned and subject to GC.
 */

{
    word_t *block, *t;
#ifndef NO_MEM_BLOCK_SIZE
    size_t old_size;
#endif

    if (going_to_exit) /* A recursive call while we're exiting */
        exit(3);

    size += XM_OVERHEAD_SIZE;
    block = (word_t *)p - XM_OVERHEAD;

#ifndef NO_MEM_BLOCK_SIZE
    old_size = mem_block_size(block);
    t = malloc_increment_size(block, size - old_size);
    if (t)
        return p;
#endif

    do {
        t = mem_realloc(p, size);
    } while (t == NULL && retry_alloc(size MTRACE_ARG));

    if (t)
    {
#ifndef NO_MEM_BLOCK_SIZE
        count_back(xalloc_stat, old_size);
        count_up(xalloc_stat, mem_block_size(t));
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
    if (i >= sizeof(dispatch_table)/sizeof(dispatch_table[0]))
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

#ifdef MALLOC_TRACE
    int i;
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

    /* Print a hexdump, but not more than 70 characters */
    size = mem_block_size(block);
    if (size > 70)
    {
        write(d, "\n", 1);
        return;
    }
    write(d, (char *)(block+XM_OVERHEAD), size);
    write(d, "\n\n", 2);
} /* print_block() */

#endif /* GC_SUPPORT */

/*-------------------------------------------------------------------------*/
#ifdef MALLOC_LPC_TRACE

static void
write_lpc_trace (int d, word_t *p)

/* Write the object and program which allocated the memory block <p>
 * onto file <d>.
 */

{
    object_t *obj, *o;
    bytecode_p pc;
    program_t *prog;
    int line;
    int32 id;

    /* Try to find the object which allocated this block */
    if ( NULL != (obj = (object_t *)p[XM_OBJ]) )
    {
        writes(d, "By object: ");
        if (obj->flags & O_DESTRUCTED)
            writes(d, "(destructed) ");
        for (o = obj_list; o && o != obj; o = o->next_all) NOOP;
        if (!o)
            writes(d, "(not in list) ");
        else if (o->name)
            writes(d, get_txt(o->name)); /* TODO: If this cores, it has to go again */
    }
    else
        writes(d, "No object.");
    writes(d, "\n");

    /* Try to find the program which allocated this block */
    if ( 0 != (id = p[XM_PROG]) )
    {
        pc = NULL;
        prog = NULL;

        writes(d, "By program: ");
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
            dprintf2(d, "%s line:%d\n", (p_int)get_txt(file), line);

            if (file)
                free_mstring(file);
        }
        else
        {
            writes(d, "Not found at old address.\n");
        }
    }
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
    write_lpc_trace(d, ((word_t *)p) - XM_OVERHEAD);
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
    word_t size = mem_block_size(p);

#    ifdef MALLOC_TRACE
        dprintf3(d, " %s %d size 0x%x\n",
                  p[XM_FILE], p[XM_LINE], size
                );
#    endif
#    ifdef MALLOC_LPC_TRACE
        write_lpc_trace(d, p);
#    endif
#endif
} /* dump_malloc_trace() */

/*=========================================================================*/

/*                     CLIB ALLOCATION FUNCTIONS                           */

#if defined(REPLACE_MALLOC) && defined(SBRK_OK)

/*-------------------------------------------------------------------------*/
static POINTER
amalloc (size_t size)

/* Allocate an aligned block of <size> bytes, if necessary, with
 * SYSTEM privilege. The block is not subject to GC.
 * Result is the pointer to the allocated block, or NULL.
 */

{
    char *temp;

#if MALLOC_ALIGN > MEM_ALIGN

#if defined(HAVE_MADVISE)
    size_t orig_size = size;
#endif

    size += (MALLOC_ALIGN-MEM_ALIGN);
#endif

    temp = (char *)pxalloc(size);
    if (!temp)
    {
        int save_privilege = malloc_privilege;
        malloc_privilege = MALLOC_SYSTEM;
        temp = (char *)pxalloc(size);
        malloc_privilege = save_privilege;
    }

#if MALLOC_ALIGN > MEM_ALIGN
    if (temp)
    {
        /* Set the first byte of the alignment area to 0xAF - afree(0
         * is going to look for it - and the rest to 0.
         */
        *temp++ = 0xAF;
        while ((p_int)temp & (MALLOC_ALIGN-1))
            *temp++ = 0;
        MADVISE(temp, orig_size);
    }
#endif

    return (POINTER)temp;
} /* amalloc() */

/*-------------------------------------------------------------------------*/
static void
afree (POINTER p)

/* Free the aligned memory block <p>.
 */

{
    char *q = (char *)p;

    if (!q)
        return;

#if MALLOC_ALIGN > MEM_ALIGN

    /* amalloc() filled the alignment area with 0s except for the first byte.
     * Search backwards to find that marker and with it the real block begin.
     */
    while (!*--q) NOOP;
#endif

    pfree(q);
} /* afree() */

/*-------------------------------------------------------------------------*/
static INLINE long
get_block_size (POINTER ptr)

/* Get the allocated block size for the block with user area starting
 * at <ptr>. This function is meant only for block allocated with (a)malloc().
 * Result is the size in bytes inclusive overhead.
 */
{
    long size = 0;

    if (ptr)
    {
        /* Get the allocated size of the block for the statistics */

        char *q;

        q = (char *)ptr;
#if MALLOC_ALIGN > MEM_ALIGN
        while ( !(size = *--q) ) NOOP;
#endif
        size = xalloced_size(q) + mem_overhead();
    }

    return size;
} /* get_block_size() */

/*-------------------------------------------------------------------------*/
POINTER
malloc (size_t size)

/* Allocate an empty memory block of size <sizel>.
 * The memory is aligned and not subject to GC.
 */

{
    POINTER result;

    result = amalloc(size);
    if (result)
    {
        count_up(clib_alloc_stat, get_block_size(result));
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
        count_back(clib_alloc_stat, get_block_size(ptr));
    }

    afree(ptr);
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

   old_size = get_block_size(p) - mem_overhead();

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
    static char *addr = NULL;  /* address of first `local', once known */
    char         local;        /* to get stack address */

    if (addr == NULL)  /* initial call */
    {
        addr = &local;
        get_stack_direction ();  /* recurse once */
    }
    else  /* second entry */
    if (&local > addr)
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
    ptrdiff_t gap;

    /* Don't check the gap after a Fatal error or if the system is
     * not fully initialised yet.
     */
    if (stack_direction == 0 || condition == Fatal || heap_end == NULL)
        return;

    /* On the first call, test if checking the gap actually makes sense.
     * Reason: Some systems like Cygwin put the stack below the heap.
     * If checking is not necessary, the 'condition' will be set to Fatal.
     * TODO: Does Cygwin's Heap grow downwards?
     */
    if (condition == Initial)
    {
        condition = Fatal; /* Prevent recursion */
        if ((char *)heap_end > (char *)&gap)
        {
            printf("%s Heap/stack check disabled: heap %p > stack %p\n"
                  , time_stamp(), heap_end, &gap);
            debug_message("%s Heap/stack check disabled: heap %p > stack %p\n"
                  , time_stamp(), heap_end, &gap);
            /* Leave condition at 'Fatal' */
            return;
        }

        /* Yup, we can check the gap */
        condition = Normal;
    }

    /* First check if heap and stack overlap. We do this first to
     * rule out negative 'gap' values which can be caused by overflows.
     */
    gap = 0;
    if (stack_direction < 0)
        gap = (ptrdiff_t)((char *)heap_end >= (char *)&gap);
    else
        gap = (ptrdiff_t)((char *)&gap >= (char *)heap_end);
    if (gap != 0)
    {
        /* Stack and Heap overlap: irrecoverable failure */
        if (condition != Fatal)
        {
            condition = Fatal;
            if (stack_direction < 0)
                gap = (char *)heap_end - (char *)&gap;
            else
                gap = (char *)&gap - (char *)heap_end;
            fatal("Out of memory: Stack overlaps heap by %ld.\n"
                 , (long)gap);
            /* NOTREACHED */
        }
        return; /* Recursive call during fatal() handling */
    }

    /* Heap and stack don't overlap - do the normal gap checking.
     * Note that on machines with big address spaces the computation
     * may overflow.
     */
    if (stack_direction < 0)
        gap = (char *)&gap - (char *)heap_end;
    else
        gap = (char *)heap_end - (char *)&gap;

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
        error("Out of memory: Gap between stack and heap: %ld.\n"
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
char *
string_copy_traced (const char *str MTRACE_DECL)

/* string_copy() acts like strdup() with the additional bonus that it can
 * trace file/line of the calling place if MALLOC_TRACE is defined.
 */

{
    char *p;
    size_t len;

    len = strlen(str)+1;
    p = xalloc_traced(len MTRACE_PASS);
    if (p)
    {
        (void)memcpy(p, str, len);
    }
    return p;
} /* string_copy_traced() */

/***************************************************************************/
