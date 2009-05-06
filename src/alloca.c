/*---------------------------------------------------------------------------
 * Gamedriver alloca -- a portable alloca() implementation
 * Based on the public-domain version by D.A. Gwyn.
 *
 *---------------------------------------------------------------------------
 * This implementation of the PWB library alloca() function,
 * which is used to allocate space off the run-time stack so
 * that it is automatically reclaimed upon procedure exit,
 * was inspired by discussions with J. Q. Johnson of Cornell
 * and later extended by Lars Duening.
 *
 * It should work under any C implementation that uses an
 * actual procedure stack (as opposed to a linked list of
 * frames).  There are some preprocessor constants that can
 * be defined when compiling for your specific system, for
 * improved efficiency; however, the defaults should be okay.
 *
 * The general concept of this implementation is to keep
 * track of all alloca()-allocated blocks, and reclaim any
 * that are found to be deeper in the stack than the current
 * invocation.  This heuristic does not reclaim storage as
 * soon as it becomes invalid, but it will do so eventually.
 *
 * As a special case, alloca(0) reclaims storage without
 * allocating any, including storage 'cached' by the implementation
 * for fast reuse. It is a good idea to use alloca(0) in
 * your main control loop, etc. to force garbage collection.
 *
 * As opposed to the original implementaion using malloc()/free(),
 * this version utilizes the 'mempools' module to allocate the memory.
 *---------------------------------------------------------------------------
 */

#include <stdlib.h>
#include <stdio.h>

#include "mempools.h"

typedef void *pointer;  /* generic pointer type */

/*-------------------------------------------------------------------------*/
/* Define STACK_DIRECTION if you know the direction of stack
 * growth for your system; otherwise it will be automatically
 * deduced at run-time.
 *
 * STACK_DIRECTION > 0 => grows toward higher addresses
 * STACK_DIRECTION < 0 => grows toward lower addresses
 * STACK_DIRECTION = 0 => direction of growth unknown
 */

#ifndef STACK_DIRECTION
#    define STACK_DIRECTION  0  /* direction unknown */
#endif

#if STACK_DIRECTION != 0

#    define STACK_DIR  STACK_DIRECTION  /* known at compile-time */

#else /* STACK_DIRECTION == 0; need run-time code */

static int stack_dir;                   /* 1 or -1 once known */
#    define STACK_DIR stack_dir

#endif /* STACK_DIRECTION == 0 */

/*-------------------------------------------------------------------------*/

/* An "alloca header" is used to:
 * (a) chain together all alloca()ed blocks;
 * (b) keep track of stack depth.
 */

typedef struct hdr
{
    struct hdr * next;  /* for chaining headers */
    char       * deep;  /* for stack depth measure */
    mempool_t  * pool;  /* the memory pool */
} header;

static header *last_alloca_header = NULL; /* -> last alloca header */
static header *free_header = NULL; /* List of unused structures */

/*-------------------------------------------------------------------------*/
#if STACK_DIRECTION == 0

static void
find_stack_direction (void)

/* Find the direction of the stackgrowth and store the result (+1 or -1)
 * into the global stack_dir.
 */

{
    static char *addr = NULL;  /* address of first `dummy', once known */
    char        dummy;         /* to get stack address */

    if (addr == NULL)  /* initial call */
    {
        addr = &dummy;
        find_stack_direction ();  /* recurse once */
    }
    else  /* second entry */
    if (&dummy > addr)
        stack_dir = 1;    /* stack grew upward */
    else
        stack_dir = -1;   /* stack grew downward */
}

#endif  /* STACK_DIRECTION == 0 */

/*-------------------------------------------------------------------------*/
pointer
palloca (size_t size)

/* Allocate at least <size> bytes of memory "on the stack" and return a
 * pointer to the start of the memory block. Return NULL if running out
 * of memory.
 *
 * Allocating "on the stack" means that every allocation is associated with
 * the call depth of the allocator. Allocations of a higher call depth
 * cause the deallocation of any memory allocated on deepter call depths.
 *
 * Originally this memory was meant to be taken from the stack frame of
 * the caller, but some systems and C implementations do not allow this.
 *
 * An allocation of 0 bytes is a special case, causing the deallocation of
 * every memory block allocated so far.
 */

{
    char              probe;         /* probes stack depth: */
    register char   * depth = &probe;
    register header * hp;            /* current header structure */

#if STACK_DIRECTION == 0
    if (STACK_DIR == 0)  /* (yet)unknown growth direction */
        find_stack_direction ();
#endif

    /* Reclaim garbage, defined as all alloca()ed storage that
     * was allocated from deeper in the stack than currently.
     */

    for (hp = last_alloca_header; hp != NULL;)
    {
        if ((STACK_DIR > 0 && hp->deep > depth)
         || (STACK_DIR < 0 && hp->deep < depth))
        {
            register header *np = hp->next;

            mempool_reset(hp->pool);  /* collect garbage */
            hp->next = free_header;
            free_header = hp;
            hp = np;              /* -> next header */
        }
        else
            break; /* rest are not deeper */
    }

    last_alloca_header = hp;  /* -> last valid storage */

    /* From here on, hp is always == last_alloca_header; the
     * use of the local however is a hint for the optimizer.
     */

    if (size == 0)
    {
        while (free_header != NULL)
        {
            header * this = free_header;
            free_header = free_header->next;
            if (this->pool)
            {
                mempool_delete(this->pool);
            }
            free(this);
        }
        return NULL;  /* no allocation required */
    }

    /* Allocate a new header structure if there is non for
     * this particular stack depth. The new structure will
     * be the new bottom of the header list.
     */

    if (hp == NULL || hp->deep != depth)
    {
        if (free_header == NULL)
        {
            hp = malloc (sizeof (*hp));
            if (!hp) { fprintf(stderr, "alloca: failed!\n"); return NULL; }
            hp->pool = new_mempool(size_mempool(1));
            if (!hp->pool)
            {
                free(hp);
                fprintf(stderr, "alloca: failed!\n");
                return NULL;
            }
        }
        else
        {
            hp = free_header;
            free_header = hp->next;
        }

        hp->next = last_alloca_header;
        hp->deep = depth;
        last_alloca_header = hp;
    }

    /* The actual allocation is easy: */
    return mempool_alloc(hp->pool, size);
}

/***************************************************************************/
