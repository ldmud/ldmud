/*---------------------------------------------------------------------------
 * alloca -- (mostly) portable alloca() implementation
 * Written and put into the public-domain by D.A. Gwyn.
 * This version modified for Standard C.
 *
 *---------------------------------------------------------------------------
 * This implementation of the PWB library alloca() function,
 * which is used to allocate space off the run-time stack so
 * that it is automatically reclaimed upon procedure exit,
 * was inspired by discussions with J. Q. Johnson of Cornell.
 *
 * It should work under any C implementation that uses an
 * actual procedure stack (as opposed to a linked list of
 * frames).  There are some preprocessor constants that can
 * be defined when compiling for your specific system, for
 * improved efficiency; however, the defaults should be okay.
 *
 * CAVEAT: Modern C compilers often recognize uses of alloca() and
 *   generate special code instead of a normal function call. To
 *   turn off this feature (and thus make use of this alloca()
 *   implementation), you can try doing the following things:
 *     - define C_ALLOCA when compiling your program
 *     - make sure the compiler sees the prototype for alloca()
 *     - don't include <alloca.h>
 *
 * The general concept of this implementation is to keep
 * track of all alloca()-allocated blocks, and reclaim any
 * that are found to be deeper in the stack than the current
 * invocation.  This heuristic does not reclaim storage as
 * soon as it becomes invalid, but it will do so eventually.
 *
 * As a special case, alloca(0) reclaims storage without
 * allocating any.  It is a good idea to use alloca(0) in
 * your main control loop, etc. to force garbage collection.
 *---------------------------------------------------------------------------
 */

#include <stdlib.h>
#include <stdio.h>

extern void * alloca(size_t size);

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

#if defined(AMIGA)
#    define STACK_DIRECTION -1
#endif

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
 *
 * It is very important that sizeof(header) agree with malloc()
 * alignment chunk size.  The following default should work okay.
 */

#ifndef ALIGN_SIZE
#  define  ALIGN_SIZE  sizeof(double)  /* it's a guess */
#endif

typedef union hdr
{
  char   align[ALIGN_SIZE];      /* to force sizeof(header) */
  struct
  {
      union hdr *next;           /* for chaining headers */
      char *deep;                /* for stack depth measure */
  } h;
} header;

static header *last_alloca_header = NULL; /* -> last alloca header */

/*-------------------------------------------------------------------------*/
#if STACK_DIRECTION == 0

static void
find_stack_direction (void)

/* Find the direction of the stackgrowth and store the result (+1 or -1)
 * into the global stack_dir.
 */

{
    static char *addr = NULL;  /* address of first `dummy', once known */
    auto char    dummy;        /* to get stack address */


    if (addr == NULL) /* initial entry */
    {
        addr = &dummy;

        find_stack_direction ();        /* recurse once */
    }
    else              /* second entry */
    if (&dummy > addr)
        stack_dir = 1;                  /* stack grew upward */
    else
        stack_dir = -1;                 /* stack grew downward */
}

#endif /* STACK_DIRECTION == 0 */

/*-------------------------------------------------------------------------*/
pointer
alloca (size_t size)

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
    auto char        probe;                /* probes stack depth: */
    register char   *depth = &probe;

#if STACK_DIRECTION == 0
    if (STACK_DIR == 0)  /* unknown growth direction */
        find_stack_direction ();
#endif

    /* Reclaim garbage, defined as all alloca()ed storage that
     * was allocated from deeper in the stack than currently.
     */

    {
        register header *hp;   /* traverses linked list */

        for (hp = last_alloca_header; hp != NULL;)
            if (STACK_DIR > 0 && hp->h.deep > depth
             || STACK_DIR < 0 && hp->h.deep < depth)
            {
                register header        *np = hp->h.next;

                free ((pointer) hp);  /* collect garbage */
                hp = np;              /* -> next header */
            }
            else
                break;               /* rest are not deeper */

        last_alloca_header = hp;  /* -> last valid storage */
    }

    if (size == 0)
    {
        return NULL;  /* no allocation required */
    }

    /* Allocate combined header + user data storage. */

    {
        register pointer new = malloc (sizeof (header) + size);
        /* address of header */
        if (!new) { fprintf(stderr,"alloca: failed!"); return NULL; }

        ((header *)new)->h.next = last_alloca_header;
        ((header *)new)->h.deep = depth;

        last_alloca_header = (header *)new;

        /* User storage begins just after header. */

        return (pointer)((char *)new + sizeof(header));
    }
}

/***************************************************************************/

