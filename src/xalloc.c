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

#include "interpret.h"
#include "main.h"
#include "simulate.h"

/*-------------------------------------------------------------------------*/

/* Minimum boundary between stack and heap, should be sufficient for
 * error handling (which needs about 10..15KByte).
 * If the gap falls below this value, an error is generated and the
 * gap is no longer checked except for true overlap with the heap.
 */

#define HEAP_STACK_GAP (20480)


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

/*-------------------------------------------------------------------------*/

/* Include the allocator source */

#ifdef MALLOC_smalloc

#include "smalloc.c"

#endif /* MALLOC_smalloc */

/* ======================================================================= */

#ifdef MALLOC_sysmalloc

static char * heap_end = NULL;

/*-------------------------------------------------------------------------*/
POINTER
xalloc (size_t size)

/* Allocate <size> bytes of memory like malloc() does.
 * This function catches out of memory situations and tries to recover
 * from them by using the reserved memory areas. If it totally runs
 * out of memory, the program exit()s with code 3.
 */

{
    char *p;
    static int going_to_exit;

    if (going_to_exit)
        exit(3);
    if (size == 0)
        fatal("Tried to allocate 0 bytes.\n");

    if (in_malloc++)
    {
        in_malloc = 0;
        writes(1, "Multiple threads in smalloc()\n");
        fatal("Multiple threads in smalloc()\n");
        /* NOTREACHED */
        return NULL;
    }

    {
        static mp_int total_malloced = 0;

        if (max_malloced > 0
         && (total_malloced += size + sizeof(p_int)) > max_malloced)
        {
            total_malloced -= size + sizeof(p_int);
            p = NULL;
        }
        else
        {
            p = malloc(size);
        }
    }

    if (p == NULL)
    {
        if (reserved_user_area) {
            free(reserved_user_area);
            reserved_user_area = NULL;
            p = "Temporary out of MEMORY. Freeing reserve.\n";
            write(1, p, strlen(p));
            in_malloc--;
            return xalloc(size);        /* Try again */
        }
        if (malloc_privilege >= MALLOC_MASTER && reserved_master_area) {
            free(reserved_master_area);
            reserved_master_area = NULL;
            p = "Temporary out of MEMORY. Freeing master reserve.\n";
            write(1, p, strlen(p));
            in_malloc--;
            return xalloc(size);        /* Try again */
        }
        if (malloc_privilege >= MALLOC_SYSTEM && reserved_system_area) {
            free(reserved_system_area);
            reserved_system_area = NULL;
            p = "Temporary out of MEMORY. Freeing system reserve.\n";
            write(1, p, strlen(p));
            in_malloc--;
            return xalloc(size);        /* Try again */
        }
        /* We can hardly survive out of memory without the garbage collector */
        going_to_exit = MY_TRUE;
        p = "Totally out of MEMORY.\n";
        write(1, p, strlen(p));
        in_malloc--;
        (void)dump_trace(MY_FALSE, NULL);
        exit(2);
    }

    if (stack_direction > 0)
    {
        if (heap_end == NULL || heap_end < (char*)p)
            heap_end = p;
        assert_stack_gap();
    }
    else if (stack_direction < 0)
    {
        if (heap_end == NULL || heap_end > (char*)p + size)
            heap_end = p;
        assert_stack_gap();
    }

    in_malloc--;
    return p;
} /* xalloc() */

/*-------------------------------------------------------------------------*/
void
dump_lpc_trace (int d, void *adr)

/* Stand in for the malloc lpc trace dump routine which the stdlib allocator
 * lacks.
 */

{
#define WRITES(d, s) write((d), (s), strlen(s))
    WRITES(d, "No malloc lpc trace.\n");
#undef WRITES
} /* dump_lpc_trace() */

/*-------------------------------------------------------------------------*/
void
dump_malloc_trace (int d, void *adr)

/* Stand in for the malloc trace dump routine which the stdlib allocator
 * lacks.
 */

{
#define WRITES(d, s) write((d), (s), strlen(s))
    WRITES(d, "No malloc trace.\n");
#undef WRITES
} /* dump_malloc_trace() */

#endif /* MALLOC_sysmalloc */

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

#ifndef string_copy

/*-------------------------------------------------------------------------*/
char *
string_copy (const char *str)

/* string_copy() acts like strdup(), but uses xalloc() instead of malloc().
 * It also has the additional bonus that it can trace file/line of the
 * calling place if MALLOC_TRACE is defined (and the allocator supports
 * tracing).
 */

{
    char *p;

    p = xalloc(strlen(str)+1);
    if (p) {
        (void)strcpy(p, str);
    }
    return p;
}

#endif /* string_copy */

/***************************************************************************/

