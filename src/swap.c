/*---------------------------------------------------------------------------
 * Swapping of object programs and variables.
 *
 *---------------------------------------------------------------------------
 * TODO: 'status swap' should also list the swaps/s.
 * TODO: Background defragmentation of the swapfile
 *
 * The swapper helps reducing the resident memory usage of the driver
 * by writing program code and variable values of objects into a file.
 * The decision which object to swap is done by the backend loop, which
 * hopefully selects only objects which are not in use at the moment.
 *
 * The program and variables are swapped independently because the more
 * complicated structure of variable values makes swapping them significantly
 * more expensive than swapping the program. Additionally, since programs
 * don't change, once a program has been swapped out, later swap ins can
 * all be satisfied from the image created on the first swap out - no need
 * for a costly rewrite of the program image.
 *
 * Every program resp. variable block swapped is identified by a "swap
 * number", which incidentally is the offset at which the data can be found
 * in the swap file. The swap number or'ed with 0x01 is stored in place of
 * the .prog resp. .variables pointer in the object structure. Also the
 * object flag O_SWAPPED is set if either one of the data blocks has been
 * swapped.
 *
 * Programs can be swapped only if the have but one reference - that means
 * that inherited or cloned objects can't swap. The line number information
 * for a program is included in the program's swap block.
 *
 * Variables can be swapped all the time, however, some of the _values_
 * can't be removed from memory: arrays and mappings with more than one
 * reference (this conveniently includes recursive data structures), objects,
 * closure, etc. For these values, the swapper writes a binary copy of
 * the referencing svalue into the file. All other values are written into
 * the file by value and removed from memory.
 *
 * The swap file is managed by the swapper and opened as soon as it is
 * needed. If no other name is set, the filename defaults to
 * "SWAP_FILE.<hostname>". The file is kept open over the whole runtime of
 * the driver.
 *
 * The space in the swap file is managed in blocks of different sizes:
 * blocks used for swapped programs and variable sets, and free blocks.
 * Adjacent free blocks are concatenated, of course. The block structure
 * of the file is mirrored in memory by a linked list of associated data
 * structures - this simple structure proved to be efficient enough.
 *
 * When allocating a new block, the swapper follows one of two strategies:
 *
 * swap_compact_mode == TRUE:
 *
 *   In this mode, the swap file is kept short, but possibly heavily
 *   fragmented.
 *
 *   The swapper first searches a suitable free block in the whole file
 *   and extends the swap file only if no existing free block can be
 *   found.
 *
 * swap_compact_mode == FALSE:
 *
 *   In this mode, the swapper tries to keep a "healthy" balance between
 *   free and used blocks. For the price of a larger swap file the
 *   fragmentation and search times are lower.
 *
 *   The swapper always extends the file for a new block until the more
 *   than half of the swap file is unused. At that point the swapper
 *   starts recycling the free blocks as in the compact mode, but only
 *   until the free blocks occupy only 1/4th of the swap file - then
 *   the swapper switches back to immediate extension.
 *
 *---------------------------------------------------------------------------
 */

#include "driver.h"
#include "typedefs.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <stddef.h>
#include <stdio.h>

#include "swap.h"

#include "array.h"
#include "backend.h"
#include "closure.h"
#include "comm.h"
#include "gcollect.h"
#include "interpret.h"
#include "main.h"
#include "mapping.h"
#include "mstrings.h"
#include "object.h"
#include "otable.h"
#include "prolang.h"
#include "random.h"
#include "simulate.h"
#include "simul_efun.h"
#include "stdstrings.h"
#include "strfuns.h"
#include "svalue.h"
#include "wiz_list.h"
#include "xalloc.h"

#include "../mudlib/sys/debug_info.h"
 
/*-------------------------------------------------------------------------*/

typedef struct swap_block_s swap_block_t;
typedef struct varblock_s   varblock_t;
typedef struct free_swapped_mapping_locals_s free_swapped_mapping_locals_t;


/* --- struct swap_block_s
 *
 * A linked list of these structures describes the use of the
 * swapfile. Every structure describes one continguous block
 * in the swapfile with the given size. A positive size denotes
 * a free block, a negative size a used block.
 */

struct swap_block_s
{
  swap_block_t *next;
  mp_int        size;
};


/* --- struct varblock_s
 *
 * Varblocks are used to store the data from variables.
 * The storage area is located _before_ the varblock_t so that
 * data-storing functions need to know only the storage pointer .current
 * and the remaining size .rest.
 */

struct varblock_s
{
    /* unsigned char data[]: Allocated memory */

    unsigned char *current;  /* First free byte */
    mp_int rest;             /* Number of free bytes */
    char *start;             /* Start of the memory area */
};


/* --- struct free_swapped_mapping_locals_s
 *
 * Auxiliary datastructure to free a mapping after swapping out.
 */

struct free_swapped_mapping_locals_s
{
    p_int          num_values; /* width of the mapping */
    unsigned char *p;          /* current position in buffer */
};


#define LOW_WATER_MARK  (swapfile_size >> 2)
#define HIGH_WATER_MARK (swapfile_size >> 1)
  /* The two limits for non-compact swapping.
   */

#define SWAP_ABS(a) ((a)>0 ? (a) : (-a))

/*-------------------------------------------------------------------------*/

Bool swap_compact_mode = MY_FALSE;
  /* When true, the swapper tries to keep the swapfile short.
   */

static char file_name[MAXPATHLEN+1] = "";
  /* Name of the swap file.
   * Defaults to "SWAP_FILE.<hostname>".
   */

static FILE *swap_file = NULL;
  /* The swapfile - it is kept open all the time.
   */

static Bool recycle_free_space = MY_FALSE;
  /* True when freespace should be re-used, false if not
   */

static swap_block_t swap_list;
  /* Headblock of the list of swap blocks.
   */

static swap_block_t *swap_rover = NULL;
  /* Pointer to the current swap_block. By anchoring searches
   * at the rover, we potentially increase the locality of
   * file accesses.
   */

static swap_block_t *swap_previous = &swap_list;
  /* One prior to swap_rover.
   */

static /* TODO: offset_t */ mp_int current_offset;
  /* File offset corresponding to swap_rover.
   */

static mp_int swapfile_size = 0;
  /* Total size of the swapfile.
   */

mp_int total_bytes_swapfree = 0;
  /* Free bytes in the swapfile.
   */

static unsigned char *last_variable_block;
  /* Recursive swap during a GC: address of the last variable
   * block to be written.
   */

static mp_int last_variable_swap_num;
  /* Recursive swap during a GC: swap number of the last variable
   * block to be written.
   */

static char *last_changed_swapped_svalue;
  /* The last stored svalue free_swapped_svalues() had to change.
   */


/* Statistics (some are accessed directly from the outside) */

static mp_int num_swap_structs = 0;
  /* Number of swap block structs allocated.
   */

mp_int num_swapped = 0;
  /* Number of program blocks used in the swapfile (including unswapped
   * blocks).
   */

mp_int num_unswapped = 0;
  /* Number of program blocks read back in from the swapfile (but still
   * marked as allocated).
   */

mp_int total_bytes_swapped = 0;
  /* Number of program bytes stored in the swapfile (including unswapped
   * bytes).
   */

mp_int total_bytes_unswapped = 0;
  /* Number of program bytes read back in from the swapfile (but still
   * marked as allocated).
   */

static mp_int num_swapfree = 0;
  /* Number of free swap blocks.
   */

mp_int num_vb_swapped = 0;
  /* Number of variables blocks in the swapfile.
   */
   
mp_int total_vb_bytes_swapped = 0;
  /* Total size of variables stored in the swapfile.
   */

static mp_int total_swap_reused = 0;
  /* Size of bytes reused from previously freed blocks.
   */

static long swap_num_searches;
  /* Number of searches for a free block to allocate (as opposed to
   * simply allocating it).
   */

static long swap_total_searchlength;
  /* Sum of search steps done when allocating a new block.
   */

static long swap_free_searches;
  /* Number of searches for a block to free.
   */

static long swap_free_searchlength;
  /* Sum of search steps done when freeing a block.
   */

mp_int total_num_prog_blocks;
  /* Number of program blocks in memory.
   */

mp_int total_prog_block_size;
  /* Total size of program blocks in memory.
   */

/*-------------------------------------------------------------------------*/
/* Forward declarations */

static varblock_t *swap_svalues(svalue_t *, mp_int, varblock_t *);
static unsigned char *free_swapped_svalues(svalue_t *, mp_int, unsigned char *);

/*-------------------------------------------------------------------------*/
static Bool
locate_out (program_t *prog)

/* Prepare program <prog> for swap out: all pointers within the program
 * memory block are changed into offsets relative to the start of the
 * area.
 *
 * Return TRUE on success.
 */

{
    char *p = NULL; /* keep cc happy */

    if (!prog)
        return MY_FALSE;

#define MAKEOFFSET(type, name) (type)&p[(char *)prog->name - (char *)prog]

    prog->program        = MAKEOFFSET(bytecode_p, program);
    prog->functions      = MAKEOFFSET(uint32*, functions);
    prog->function_names = MAKEOFFSET(unsigned short *, function_names);
    prog->strings        = MAKEOFFSET(string_t**, strings);
    prog->variable_names = MAKEOFFSET(variable_t *, variable_names);
    prog->inherit        = MAKEOFFSET(inherit_t *, inherit);
#ifdef USE_STRUCTS
    prog->struct_defs    = MAKEOFFSET(struct_def_t *, struct_defs);
    prog->struct_members = MAKEOFFSET(struct_member_t *, struct_members);
#endif /* USE_STRUCTS */
    prog->includes       = MAKEOFFSET(include_t *, includes);
    if (prog->type_start)
    {
        prog->argument_types = MAKEOFFSET(unsigned short *, argument_types);
        prog->type_start = MAKEOFFSET(unsigned short *, type_start);
    }
    return MY_TRUE;

#undef MAKEOFFSET
} /* locate_out() */


/*-------------------------------------------------------------------------*/
static Bool
locate_in (program_t *prog)

/* After <prog> was swapped in, restore the intra-block pointers
 * from the stored offsets.
 * .line_numbers is not modified, and the program will get a new
 * id-number.
 *
 * Return TRUE on success.
 */

{
    char *p = (char *)prog;


    if (!prog)
        return MY_FALSE;

    prog->id_number = ++current_id_number
                      ? current_id_number
                      : renumber_programs();

#define MAKEPTR(type, name) (type)&p[(char *)prog->name - (char *)0]

    prog->program        = MAKEPTR(bytecode_p, program);
    prog->functions      = MAKEPTR(uint32*, functions);
    prog->function_names = MAKEPTR(unsigned short *, function_names);
    prog->strings        = MAKEPTR(string_t**, strings);
    prog->variable_names = MAKEPTR(variable_t*, variable_names);
    prog->inherit        = MAKEPTR(inherit_t*, inherit);
#ifdef USE_STRUCTS
    prog->struct_defs    = MAKEPTR(struct_def_t*, struct_defs);
    prog->struct_members = MAKEPTR(struct_member_t *, struct_members);
#endif /* USE_STRUCTS */
    prog->includes       = MAKEPTR(include_t*, includes);
    if (prog->type_start)
    {
        prog->argument_types = MAKEPTR(unsigned short *, argument_types);
        prog->type_start     = MAKEPTR(unsigned short *, type_start);
    }

    return MY_TRUE;

#undef MAKEPTR
} /* locate_in() */

/*-------------------------------------------------------------------------*/
static mp_int
swap_alloc (mp_int size)

/* Find a free block of sufficient <size> in the swap file and allocate
 * it. If there is none, add one at the end of the file.
 * Return the offset of the block from the beginning of the file,
 * and let the swap_rover point to the swap_block_t.
 */

{
    swap_block_t *mark, *last;
    int save_privilege;

    /* Make sure the size is something even, to meet the requirement
     * that all swap offsets are even.
     */
    size = (size + sizeof(char*)-1) & ~(sizeof(char*)-1);

    save_privilege = malloc_privilege;
    malloc_privilege = MALLOC_SYSTEM;

    if (!swap_compact_mode)
    {
        /* Determine the allocation mode */
        if (!recycle_free_space)
        {
            if (total_bytes_swapfree < HIGH_WATER_MARK)
                goto alloc_new_space;
            recycle_free_space = MY_TRUE;
        }
        else
        {
            if (total_bytes_swapfree < LOW_WATER_MARK)
            {
                recycle_free_space = MY_FALSE;
                goto alloc_new_space;
            }
        }
    } /* if (swap_compact_mode) */

    swap_num_searches++;

    /* Search for a free block, and if necessary allocated it */
    mark = swap_rover;
    for (;;)
    {
        swap_total_searchlength++;

        /* Wrap-around the end of the list? */
        if (!swap_rover)
        {
            swap_rover = &swap_list;
            swap_previous = NULL;
            current_offset = 0;
        }

        if (size <= swap_rover->size)
        {
            /* Found a suitable block */
            total_bytes_swapfree -= size;
            total_swap_reused += size;

            /* perfect fit? */
            if (size == swap_rover->size)
            {
                swap_rover->size = -size;
                num_swapfree--;
                malloc_privilege = save_privilege;

                return current_offset;
            }

            /* Unperfect fit: split the block in two.
             * num_swapfree remains unchanged.
             */
            num_swap_structs++;
            mark = pxalloc(sizeof(swap_block_t));
            mark->size = swap_rover->size - size;
            swap_rover->size = -size;
            mark->next = swap_rover->next;
            swap_rover->next = mark;
            malloc_privilege = save_privilege;

            return current_offset;
        }

        /* Block too small: try the next */
        current_offset += SWAP_ABS(swap_rover->size);
        swap_previous = swap_rover;
        swap_rover = swap_rover->next;

        if (swap_rover == mark) /* Once around the list without success */
        {
alloc_new_space:
            /* Allocate a new block and add it to the swap file */

            last = swap_previous;
            while ( NULL != (mark = last->next) )
                last = mark;
            num_swap_structs++;
            mark = pxalloc(sizeof(swap_block_t));
            mark->next = NULL;
            last->next = mark;
            mark->size = -size;
            if (!swap_rover)
                swap_rover = mark;
            swapfile_size += size;
            malloc_privilege = save_privilege;

            return swapfile_size - size;
        }
    } /* for() */

    /* NOTREACHED */
} /* swap_alloc() */

/*-------------------------------------------------------------------------*/
static void
swap_free (mp_int offset)

/* Free the swap block at the given <offset>.
 */

{
    swap_free_searches++;

    /* If we are already after the block, reset the rover
     */
    if (offset < current_offset)
    {
        swap_rover = swap_list.next;
        swap_previous = &swap_list;
        current_offset = 0;
    }

    /* Set the rover on the block to free
     */
    while (current_offset < offset && swap_rover)
    {
        swap_free_searchlength++;
        swap_previous = swap_rover;
        current_offset += SWAP_ABS(swap_rover->size);
        swap_rover = swap_rover->next;
    }

    /* Sanity checks
     */
    if (current_offset != offset || !swap_rover)
        fatal("Bad swapfile offset.\n");
    if (swap_rover->size > 0)
        fatal("Freeing non-allocated block within swap file.\n");

    swap_rover->size = -swap_rover->size; /* Make the size positive */
    total_bytes_swapfree += swap_rover->size;
    num_swapfree++;

    /* first skip any allocated block adjacent to the one just freed */
    if (swap_previous->size <= 0)
    {
        swap_previous = swap_rover;
        current_offset += swap_rover->size;
        swap_rover = swap_rover->next;
    }

    /* now collapse adjacent free blocks */
    while (swap_rover && swap_rover->size > 0)
    {
        swap_previous->size += swap_rover->size;
        current_offset += swap_rover->size;
        swap_previous->next = swap_rover->next;
        num_swap_structs--;
        pfree(swap_rover);
        num_swapfree--;
        swap_rover = swap_previous->next;
    }
} /* swap_free() */

/*-------------------------------------------------------------------------*/
static p_int
store_swap_block (void * buffer, mp_int size)

/* Store the memory block <buffer> of <size> bytes into the swapfile
 * and return the offset at which it was stored.
 * Return -1 on a failure.
 *
 * The swapfile is opened it necessary.
 */

{
    mp_int offset;

    /* Make sure the swap file is open. */
    if (swap_file == NULL)
    {
        if (*file_name == '\0')
        {
            sprintf(file_name, "%s.%s", SWAP_FILE, query_host_name());
        }
        swap_file = fopen(file_name, "w+b");
        /* Leave this file pointer open! */
        if (swap_file == NULL)
        {
            debug_message("%s Couldn't open swap file.\n", time_stamp());
            return -1;
        }
    }

    /* Find a free swap block */
    offset = swap_alloc(size);

    /* Seek and write the data */
    if (fseek(swap_file, offset, 0) == -1)
    {
        debug_message("%s Couldn't seek the swap file, errno %d, offset %ld.\n"
                     , time_stamp(), errno, offset);
        return -1;
    }

    if (fwrite((char *)buffer, size, 1, swap_file) != 1)
    {
        debug_message("%s I/O error in swap.\n", time_stamp());
        return -1;
    }

    return offset;
} /* store_swap_block() */

/*-------------------------------------------------------------------------*/
static p_int
store_swap_block2 ( void * buffer1, mp_int size1
                  , void * buffer2, mp_int size2 )

/* Store the memory blocks <buffer1> of <size1> bytes and <buffer2> of
 * <size2> bytes into one block in the swapfile and return the offset at
 * which it was stored.
 * Return -1 on a failure.
 *
 * The swapfile is opened it necessary.
 */

{
    mp_int offset;

    /* Make sure the swap file is open. */
    if (swap_file == NULL)
    {
        if (*file_name == '\0')
        {
            sprintf(file_name, "%s.%s", SWAP_FILE, query_host_name());
        }
        swap_file = fopen(file_name, "w+b");
        /* Leave this file pointer open! */
        if (swap_file == NULL)
        {
            debug_message("%s Couldn't open swap file.\n", time_stamp());
            return -1;
        }
    }

    /* Find a free swap block */
    offset = swap_alloc(size1 + size2);

    /* Seek and write the data */
    if (fseek(swap_file, offset, 0) == -1)
    {
        debug_message("%s Couldn't seek the swap file, errno %d, offset %ld.\n"
                     , time_stamp(), errno, offset);
        return -1;
    }

    if (fwrite((char *)buffer1, size1, 1, swap_file) != 1)
    {
        debug_message("%s I/O error in swap.\n", time_stamp());
        return -1;
    }

    if (fwrite((char *)buffer2, size2, 1, swap_file) != 1)
    {
        debug_message("%s I/O error in swap.\n", time_stamp());
        return -1;
    }

    return offset;
} /* store_swap_block2() */

/*-------------------------------------------------------------------------*/
Bool
swap_program (object_t *ob)

/* Swap out the program of object <ob>. This is only possible if the
 * program has only one reference, ie. is neither cloned nor inherited.
 *
 * Result is TRUE if the program could be swapped.
 */

{
    program_t *prog;
    p_int swap_num;

    if (d_flag > 1)
    {
        debug_message("%s Swap object %s (obj ref %ld, prog ref %ld)\n"
                     , time_stamp(), get_txt(ob->name), ob->ref, ob->prog->ref);
    }

    prog = ob->prog;

    /* May we swap? */
    if (prog->ref > 1)
    {
        if (d_flag > 1)
        {
            debug_message ("%s Program not swapped - cloned or inherited.\n"
                          , time_stamp());
        }
        return MY_FALSE;
    }

    /* Has this object already been swapped, and read in again ?
     * Then it is very easy to swap it out again.
     */
    if (prog->swap_num >= 0)
    {
        total_bytes_unswapped -= prog->total_size;
        if (prog->line_numbers)
            total_bytes_unswapped -= prog->line_numbers->size;
        ob->prog = (program_t *)(prog->swap_num | 1);
        free_prog(prog, MY_FALSE);  /* Do not free the strings or blueprint */
        ob->flags |= O_SWAPPED;
        num_unswapped--;
        return MY_TRUE;
    }

    /* relocate the internal pointers */
    locate_out(prog);
    swap_num = store_swap_block2(prog, prog->total_size
                                , prog->line_numbers, prog->line_numbers->size);
    if (swap_num == -1)
    {
        locate_in(prog);
        return MY_FALSE;
    }

    total_bytes_swapped += prog->total_size + prog->line_numbers->size;
    num_swapped++;

    /* Free the program */
    free_prog(prog, MY_FALSE);  /* Don't free the shared strings or the blueprint */

    /* Mark the program as swapped */
    ob->prog = (program_t *)(swap_num | 1);
    ob->flags |= O_SWAPPED;

    return MY_TRUE;
} /* swap_program() */

/*-------------------------------------------------------------------------*/
static varblock_t *
reallocate_block (unsigned char *p, mp_int rest, mp_int count)

/* Reallocate the varblock for address <p> and remaining size <rest>
 * to hold space for at least <count> bytes more.
 *
 * Result is the new varblock_t, or NULL on failure.
 */

{
    varblock_t *tmp;
    char *start1, *start2;
    mp_int size, size2;

    /* Get the varblock structure */
    tmp = (varblock_t *)(p + rest);
    start1 = tmp->start;
    size = (char *)tmp - start1;

    /* Compute the required allocation size */
    size2 = size;
    do {
        rest += size2;
        size2 <<= 1;
    } while (rest < count);

    /* Allocate the new memory area and copy the data stored so far.
     */
    if ( !(start2 = xalloc(size2 + sizeof(varblock_t))) )
        return NULL;
    memcpy(start2, start1, size2 - rest);
    xfree(start1);

    /* Set up the new varblock */
    tmp = (varblock_t *)(start2 + size2);
    tmp->current = (unsigned char *)tmp - rest;
    tmp->rest = rest;
    tmp->start = start2;

    return tmp;
} /* reallocate_block() */

/*-------------------------------------------------------------------------*/
static void
swap_mapping_filter (svalue_t *key, svalue_t *values, void *extra)

/* Filter to swap one mapping entry. <extra> is the varblock_t** for
 * the varblock to store the data in, and may be changed during the
 * course of this function.
 */

{
    varblock_t *block = *((varblock_t **)extra);

    if (block->current)
        block = swap_svalues(key, 1, block);

    if (block->current)
        block = swap_svalues(values, *((p_int *)block->start), block);

    *((varblock_t **)extra) = block;
} /* swap_mapping_filter() */

/*-------------------------------------------------------------------------*/
static varblock_t *
swap_svalues (svalue_t *svp, mp_int num, varblock_t *block)

/* Store the <num> svalues starting at <svp> into the varblock <block>.
 * Return the (possibly reallocated) varblock.
 *
 * The data values are added to the end of the varblock in the following
 * formats:
 *
 * swtype := (ph_int)svp->type | TYPE_MOD_SWAPPED.
 *
 * STRING, SYMBOL:
 *    swtype, (ph_int)svp->x, (mp_int)mstrsize(string), string
 *
 *    For strings, svp->x.generic is 1 for untabled strings, and 0 for
 *    tabled string.
 *
 * POINTER, STRUCTS:
 *    swtype, (size_t)size, (wiz_list_t*) user, values...
 *
 * QUOTED_ARRAY:
 *    swtype, (ph_int)quotes, (size_t)size, (wiz_list_t*) user, values...
 *
 * MAPPING:
 *    swtype, (p_int)width, (p_int)size, (wiz_list_t*) user, entries...
 *
 * Opaque, NUMBER, FLOAT, OBJECT, CLOSURE
 *    svp->type, svp->x, svp->u
 *
 * Opaque are: contents of alists, empty arrays, arrays or mappings with
 * more than one ref (this also protects against recursive data structures),
 * mappings with closure or object keys.
 */

{
    static Bool swapping_alist = MY_FALSE;

    unsigned char *p;
    mp_int rest;

#   define CHECK_SPACE(count) \
        if (rest < (mp_int)(count)) { \
            varblock_t *CStmp; \
            if ( !(CStmp = reallocate_block(p, rest, count)) ) {\
                CStmp = (varblock_t *)(p + rest); \
                CStmp->current = NULL; \
                return CStmp; \
            } \
            p = CStmp->current; \
            rest = CStmp->rest; \
        }

#   define ADD_TO_BLOCK(var) \
        memcpy(p, &var, sizeof(var)); \
        p += sizeof(var); \
        rest -= sizeof(var); \

#   define SWAP_SVALUES(svp, num) {\
        varblock_t *tmp; \
        tmp = (varblock_t *)(p + rest); \
        tmp->current = p; \
        tmp->rest = rest; \
        tmp = swap_svalues(svp, num, tmp); \
        if ( !(p = tmp->current) ) { \
            return tmp; \
        } \
        rest = tmp->rest; \
    }

    p = block->current;
    rest = block->rest;

    /* Loop over all values */
    for (; --num >= 0; svp++)
    {
        switch(svp->type)
        {

        case T_STRING:
            svp->x.generic = (mstr_tabled(svp->u.str) ? 0 : 1);
            /* FALL THROUGH */

        case T_SYMBOL:
          {
            mp_int len, size;

            if (swapping_alist)
                goto swap_opaque;

            len = mstrsize(svp->u.str);
            size = 1 + sizeof svp->x + sizeof(len) + len;
            CHECK_SPACE(size)
            rest -= size;

            *p++ = svp->type | T_MOD_SWAPPED;
            memcpy(p, &svp->x, sizeof(svp->x));
            p += sizeof svp->x;
            memcpy(p, &len, sizeof(len));
            p += sizeof len;
            memcpy(p, get_txt(svp->u.str), len);
            p += len;
            break;
          }

        case T_POINTER:
#ifdef USE_STRUCTS
        case T_STRUCT:
#endif
          {
            size_t size;

            size = VEC_SIZE(svp->u.vec);
            if (svp->u.vec->ref > 1 || !size || swapping_alist)
                goto swap_opaque;

            if (size > 1 && is_ordered(svp->u.vec))
                swapping_alist = MY_TRUE;

            CHECK_SPACE(1 + sizeof(size) + sizeof(wiz_list_t *))
            *p++ = svp->type | T_MOD_SWAPPED;
            rest--;
            ADD_TO_BLOCK(size)
            ADD_TO_BLOCK(svp->u.vec->user)
            SWAP_SVALUES(svp->u.vec->item, size)
            swapping_alist = MY_FALSE;
            break;
          }

        case T_QUOTED_ARRAY:
          {
            size_t size;

            size = VEC_SIZE(svp->u.vec);
            if (svp->u.vec->ref > 1 || !size || swapping_alist)
                goto swap_opaque;

            CHECK_SPACE(
              1 + sizeof svp->x.quotes +
              sizeof size + sizeof(wiz_list_t *)
            )

            *p++ = T_QUOTED_ARRAY | T_MOD_SWAPPED;
            rest--;
            ADD_TO_BLOCK(svp->x.quotes)
            ADD_TO_BLOCK(size)
            ADD_TO_BLOCK(svp->u.vec->user)
            SWAP_SVALUES(svp->u.vec->item, size)
            break;
          }

        case T_MAPPING:
          {
            mapping_t *m = svp->u.map;
            p_int size, num_values, save;
            varblock_t *tmp;
            svalue_t *svp2;

            if (m->ref > 1 || swapping_alist)
                goto swap_opaque;

            /* mappings with object or closure keys can get stale, which
             * necessiates special treatment in garbage_collection(),
             * which in turn is not prepared to face swapping.
             */
            if (m->cond)
            {
                size = m->cond->size;
                svp2 = &(m->cond->data[0]);
                while ( --size >= 0)
                {
                    if (svp2->type == T_OBJECT || svp2->type == T_CLOSURE)
                        goto swap_opaque;
                    svp2++;
                }
            }

            CHECK_SPACE(
              1 + sizeof num_values + sizeof size + sizeof m->user
            )

            *p++ = T_MAPPING | T_MOD_SWAPPED;
            rest--;
            /* num_values might be wider than m->num_values. */
            num_values = m->num_values;
            ADD_TO_BLOCK(num_values);
            size = MAP_SIZE(m);
            ADD_TO_BLOCK(size);
            ADD_TO_BLOCK(m->user);

            tmp = (varblock_t *)(p + rest);
            tmp->current = p;
            tmp->rest = rest;
            save = *((p_int *)tmp->start);
            *((p_int *)tmp->start) = m->num_values;
            walk_mapping(m, swap_mapping_filter, &tmp);
            *((p_int *)tmp->start) = save;
            if ( !(p = tmp->current) )
            {
                return tmp;
            }
            rest = tmp->rest;
            break;
          }

        case T_NUMBER:
        case T_FLOAT:
        case T_OBJECT:
        case T_CLOSURE:
swap_opaque:
            /* opaque swapped data must be prevented from recursive freeing */
            CHECK_SPACE(sizeof(*svp))
            *p++ = svp->type;
            rest--;
            ADD_TO_BLOCK(svp->x)
            ADD_TO_BLOCK(svp->u)
            break;

        default:
           fatal("bad type %d in swap_svalues()\n", svp->type);
        }
    }

    /* All saved - construct the varblock to return */
    {
        varblock_t *tmp;

        tmp = (varblock_t *)(p + rest);
        tmp->current = p;
        tmp->rest = rest;
        return tmp;
    }

#   undef SWAP_SVALUES
#   undef ADD_TO_BLOCK
#   undef CHECK_SPACE

} /* swap_svalues() */

/*-------------------------------------------------------------------------*/
static void
free_swapped_mapping_filter (svalue_t *key, svalue_t *values, void *extra)

/* Filterfunction to free a swapped-out mapping.
 * <extra> is a (free_swapped_mapping_locals_t *), and <extra>->p is updated.
 */

{
    free_swapped_mapping_locals_t *l;
    unsigned char *p;

    l = (free_swapped_mapping_locals_t *)extra;
    p = l->p;
    p = free_swapped_svalues(key, 1, p);
    p = free_swapped_svalues(values, l->num_values, p);
    l->p = p;
} /* free_swapped_mapping_filter */

/*-------------------------------------------------------------------------*/
static unsigned char *
free_swapped_svalues (svalue_t *svp, mp_int num, unsigned char *p)

/* Free the <num> svalues starting at <svp> after they have been swapped.
 * <p> is the pointer into the buffer where this block of svalues has
 * been stored in.
 *
 * The algorithm is that this function mirrors the saving algorithm
 * of swap_svalues(), just that it reads what swap_svalues() has stored
 * in the buffer and frees all svalues with a T_MOD_SWAPPED flag.
 *
 * The function acknowledges that it may be called from the garbage
 * collector, which means that some of the object and closure values
 * stored with swap_svalues() might have become invalid meanwhile.
 * In those cases, the stored data is adjusted.
 * 
 * Take care to not interfere with a garbage_collection in progress!
 */

{
    for (; --num >= 0; svp++)
    {
        switch(*p)
        {
        case T_STRING | T_MOD_SWAPPED:
        case T_SYMBOL | T_MOD_SWAPPED:
            if (!gc_status)
            {
                p += 1 + sizeof(svp->x) + sizeof(mp_int) + mstrsize(svp->u.str);
                free_mstring(svp->u.str);
            }
            else
            {
                /* GC active: svp no longer holds the actual string */
                mp_int strsize;
                p += 1 + sizeof(svp->x);
                memcpy(&strsize, p, sizeof(strsize));
                p += sizeof(strsize) + strsize;
            }
            break;

        case T_QUOTED_ARRAY | T_MOD_SWAPPED:
            p += sizeof svp->x;
            /* FALLTHROUGH */
            
        case T_POINTER | T_MOD_SWAPPED:
#ifdef USE_STRUCTS
        case T_STRUCT | T_MOD_SWAPPED:
#endif
            p += 1 + sizeof(size_t) + sizeof(wiz_list_t *);
            p =
              free_swapped_svalues(svp->u.vec->item, VEC_SIZE(svp->u.vec), p);
            free_empty_vector(svp->u.vec);
            break;

        case T_MAPPING | T_MOD_SWAPPED:
          {
            /* beware: a mapping can get unswappable when it is entered
             * in the stale_mapping list. Or the stale_mapping list has to
             * be recoded to include swapped mappings.
             */
            free_swapped_mapping_locals_t l;

            p +=
              1 +
              sizeof(p_int) +
              sizeof(p_int) +
              sizeof(wiz_list_t *);
            l.num_values = svp->u.map->num_values;
            l.p = p;
            walk_mapping(svp->u.map, free_swapped_mapping_filter, &l);
            p = l.p;
            free_empty_mapping(svp->u.map);
            break;
          }

        case T_OBJECT:
            if (svp->type == T_NUMBER)
            {
                /* Object was destructed */
                *p++ = T_NUMBER;
                memcpy(p, &svp->x, sizeof svp->x);
                p += sizeof svp->x;
                memcpy(p, &svp->u, sizeof svp->u);
                p += sizeof svp->u;
                last_changed_swapped_svalue = (char*)p;
                break;
            }
            goto advance;

        case T_CLOSURE:
            /* the garbage collector replaces closures bound to destructed
             * objects by F_UNDEF
             */
            if (is_undef_closure(svp))
            {
              if ( memcmp(p+1, &svp->x, sizeof svp->x)
                || memcmp(p+1+sizeof svp->x, &svp->u, sizeof svp->u))
              {
                  p++;
                  memcpy(p, (char *)&svp->x, sizeof svp->x);
                  p += sizeof svp->x;
                  memcpy(p, (char *)&svp->u, sizeof svp->u);
                  p += sizeof svp->u;
                  last_changed_swapped_svalue = (char*)p;
                  break;
              }
            }

        case T_STRING:
        case T_SYMBOL:
        case T_POINTER:
#ifdef USE_STRUCTS
        case T_STRUCT:
#endif
        case T_QUOTED_ARRAY:
        case T_MAPPING:
        case T_NUMBER:
        case T_FLOAT:
advance:
            /* Opaque storage: skip it */
            p += 1 + sizeof svp->x + sizeof svp->u;
            break;

        default:
            fatal("bad type %d in free_swapped_svalues()\n", *p);
        }
    }

    return p;
} /* free_swapped_svalues() */

/*-------------------------------------------------------------------------*/
Bool
swap_variables (object_t *ob)

/* Swap the variables of object <ob> into the swap file.
 * The simul_efun object is not swapped.
 *
 * This function might be called recursively through the garbagecollector.
 * In that case it completes the swap operation begun before.
 *
 * Return TRUE on success, FALSE else.
 */

{
    char *start;
    p_int total_size;
    varblock_t *block;
    p_int swap_num;
    unsigned short num_variables;

#define VARBLOCK_STARTSIZE 0x800


    if (!ob->variables)
        return MY_TRUE;

    if (ob == simul_efun_object)
        return MY_TRUE;

    if (gc_status)
    {
        /* Complete the previous swap operation */
        num_variables = ob->prog->num_variables;
        last_changed_swapped_svalue = NULL;
        (void)free_swapped_svalues(
          ob->variables, num_variables, last_variable_block
        );

        if (last_changed_swapped_svalue)
        {
            if (fseek(swap_file, last_variable_swap_num + sizeof(p_int), 0) ==
                                                                            -1)
            {
                fatal("Couldn't seek the swap file, errno %d, offset %ld.\n",
                      errno, last_variable_swap_num + sizeof(p_int));
            }
            if (fwrite(
                  last_variable_block,
                  last_changed_swapped_svalue - (char *)last_variable_block,
                  1, swap_file) != 1)
            {
                fatal("I/O error in swap.\n");
            }
        }
        xfree(last_variable_block);
        xfree(ob->variables);
        ob->variables = (svalue_t *)(last_variable_swap_num | 1);
        ob->flags |= O_SWAPPED;

#ifdef CHECK_OBJECT_STAT
        if (check_object_stat)
        {
            fprintf(stderr, "DEBUG: OSTAT: (%ld:%ld) swapout( %p '%s') gc %d vars : %ld -> (%ld:%ld)\n"
                          , tot_alloc_object, tot_alloc_object_size, ob, ob->name ? get_txt(ob->name) : "<null>"
                          , num_variables
                          , (long)(num_variables * sizeof (svalue_t))
                          , tot_alloc_object, tot_alloc_object_size - (num_variables * sizeof (svalue_t))
                          );
        }
#endif
        tot_alloc_object_size -= num_variables * sizeof (svalue_t);

        return MY_TRUE;
    }

    /* Get the number of variables from the program without
     * swapping it in.
     */
    swap_num = (p_int)ob->prog;
    if (swap_num & 1)
    {
        swap_num &= ~1;
        swap_num += offsetof(program_t, num_variables);
        if (swapfile_size <= swap_num)
            fatal("Attempt to swap in from beyond the end of the swapfile.\n");
        if (fseek(swap_file, swap_num, 0) == -1)
            fatal("Couldn't seek the swap file, errno %d, offset %ld.\n",
                  errno, swap_num);
        if (fread(&num_variables, sizeof num_variables, 1, swap_file)
            != 1)
        {
            fatal("Couldn't read the swap file.\n");
        }
    }
    else
    {
        num_variables = ob->prog->num_variables;
    }

    /* Allocate the initial varblock and initialize it
     */
    start = xalloc(VARBLOCK_STARTSIZE + sizeof(varblock_t));
    if (!start)
        return MY_FALSE;
    block = (varblock_t *)(start + VARBLOCK_STARTSIZE);
    block->current = (unsigned char *)start + sizeof total_size;
    block->rest = VARBLOCK_STARTSIZE - sizeof total_size;
    block->start = start;

    /* Store the values */
    block = swap_svalues(ob->variables, num_variables, block);
    if (!block->current)
    {
        /* Oops */
        xfree(block->start);
        return MY_FALSE;
    }

    /* Store the block's total size in the first word of the block */
    *(p_int*)block->start = total_size =
      (((char *)block->current - block->start) + (sizeof(p_int) - 1)) &
        (~(sizeof(p_int) - 1));

    /* Write the values */
    swap_num = store_swap_block(block->start, total_size);
    if (swap_num  == -1)
    {
        xfree(block->start);
        return MY_FALSE;
    }

    /* Free the swapped values */
    (void)free_swapped_svalues(
      ob->variables, num_variables
      , (unsigned char *)block->start + sizeof total_size
    );
    
    num_vb_swapped++;
    total_vb_bytes_swapped += total_size - sizeof total_size;
    xfree(block->start);
    xfree(ob->variables);
#ifdef CHECK_OBJECT_STAT
    if (check_object_stat)
    {
        fprintf(stderr, "DEBUG: OSTAT: (%ld:%ld) swapout( %p '%s') %d vars : %ld -> (%ld:%ld)\n"
                      , tot_alloc_object, tot_alloc_object_size, ob, ob->name ? get_txt(ob->name) : "<null>"
                      , num_variables
                      , (long)(num_variables * sizeof (svalue_t))
                      , tot_alloc_object, tot_alloc_object_size - (num_variables * sizeof (svalue_t))
                      );
    }
#endif
    tot_alloc_object_size -= num_variables * sizeof (svalue_t);

    /* Mark the variables as swapped */
    ob->variables = (svalue_t *)(swap_num | 1);
    ob->flags |= O_SWAPPED;
    return MY_TRUE;

#undef VARBLOCK_STARTSIZE

} /* swap_variables() */

/*-------------------------------------------------------------------------*/
Bool
swap (object_t *ob, int mode)

/* Swap the object <ob> according to <mode>:
 *
 *   <mode> & 0x01: swap program
 *   <mode> & 0x02: swap variables
 *
 * The same flags are returned by load_ob_from_swap().
 *
 * Result is TRUE if all requested swaps succeeded.
 */

{
    Bool result = MY_TRUE;

    if (ob->flags & O_DESTRUCTED)
        return MY_FALSE;

    if (mode & 2)
        result = swap_variables(ob) && result;
    if (mode & 1)
        result = swap_program(ob) && result;
    return result;
} /* swap() */

/*-------------------------------------------------------------------------*/
static INLINE void
clear_svalues (svalue_t *svp, mp_int num)

/* Auxiliary function: clear all <num> svalues starting at <svp> to 0.
 */

{
    for (; --num >= 0;) {
        *svp++ = const0;
    }
} /* clear_svalues() */

/*-------------------------------------------------------------------------*/
static unsigned char *
read_unswapped_svalues (svalue_t *svp, mp_int num, unsigned char *p)

/* Restore the <num> swapped values into <svp>, reading from the buffer
 * position <p>.
 *
 * Note: when garbage collection is done, restoring strings would give nothing
 * but trouble, thus, a dummy number is inserted instead.
 *
 * Return the next byte to read, or NULL when out of memory.
 */
 
{
    /* For all values yadda yadda... */
    for (;--num >= 0; svp++)
    {
        svp->type = *p & ~T_MOD_SWAPPED; /* get the original type */
        
        switch(*p++)
        {
        case T_STRING | T_MOD_SWAPPED:
        case T_SYMBOL | T_MOD_SWAPPED:
          {
            string_t *s;
            mp_int    len;

            memcpy(&svp->x, p, sizeof svp->x);
            p += sizeof svp->x;
            memcpy(&len, p, sizeof len);
            p += sizeof len;
            if (gc_status)
            {
                svp->type = T_NUMBER;
            }
            else
            {
                if (svp->type == T_STRING
                 && svp->x.generic != 0)
                {
                    s = new_n_mstring((char *)p, len);
                }
                else
                {
                    s = new_n_tabled((char *)p, len);
                }

                if (!s)
                {
                    clear_svalues(svp, num + 1);
                    return NULL;
                }

                svp->u.str = s;
            }
            p += len;
            break;
          }

        case T_QUOTED_ARRAY | T_MOD_SWAPPED:
            memcpy(&svp->x, p, sizeof svp->x);
            p += sizeof svp->x;
            /* FALLTHROUGH */

        case T_POINTER | T_MOD_SWAPPED:
#ifdef USE_STRUCTS
        case T_STRUCT | T_MOD_SWAPPED:
#endif
          {
            size_t size;
            wiz_list_t *user;
            vector_t *v;

            memcpy(&size, p, sizeof size);
            p += sizeof size;
            memcpy(&user, p, sizeof user);
            p += sizeof user;
            current_object->user = user;
            v = allocate_array_unlimited(size);
            svp->u.vec = v;
            if (!v)
            {
                clear_svalues(svp, num + 1);
                return NULL;
            }
            p = read_unswapped_svalues(v->item, size, p);
            if (!p)
            {
                clear_svalues(svp + 1, num);
                return NULL;
            }
#ifdef GC_SUPPORT
            if (gc_status == gcCountRefs)
            {
                /* Pretend that this memory block already existing
                 * in the clear phase.
                 */
                clear_memory_reference(v);
                v->ref = 0;
            }
#endif
            break;
          }

        case T_MAPPING | T_MOD_SWAPPED:
          {
            mapping_t *m;
            p_int num_values;
            wiz_list_t *user;
            p_int num_keys;

            memcpy(&num_values, p, sizeof num_values);
            p += sizeof num_values;
            memcpy(&num_keys, p, sizeof num_keys);
            p += sizeof num_keys;
            memcpy(&user, p, sizeof user);
            p += sizeof user;
            if (gc_status)
            {
                /* The garbage collector is not prepared to handle hash
                 * mappings. On the other hand, the order of keys does
                 * not matter here.
                 * We can assume here that all allocation functions succeed
                 * because the garbage collector runs with
                 * malloc_privilege == MALLOC_SYSTEM .
                 */
                mapping_cond_t *cm;
                mp_int size;
                svalue_t *data, *svp2;

                m = allocate_cond_mapping(user, num_keys, num_values);
                svp->u.map = m;
                if (m->cond)
                {
                    cm = m->cond;

                    svp2 = &(cm->data[0]);
                    size = cm->size;
                    data = COND_DATA(cm, 0, num_values);
                    while ( --size >= 0) {
                        p = read_unswapped_svalues(svp2++, 1, p);
                        p = read_unswapped_svalues(data, num_values, p);
                        data += num_values;
                    }
#ifdef GC_SUPPORT
                    if (gc_status == gcCountRefs)
                    {
                        /* Pretend that this memory block already existing
                         * in the clear phase.
                         */
                        clear_memory_reference(m);
                        clear_memory_reference(cm);
                        m->ref = 0;
                    }
#endif
                }
            }
            else
            {
                mp_int i;
                wiz_list_t *save;

                save = current_object->user;
                current_object->user = user;
                m = allocate_mapping(num_keys, num_values);
                current_object->user = save;
                if (!m)
                {
                    clear_svalues(svp, num + 1);
                    return NULL;
                }
                svp->u.map = m;
                for (i = num_keys; --i >= 0;)
                {
                    svalue_t sv, *data;

                    p = read_unswapped_svalues(&sv, 1, p); /* adds 1 ref */
                    if (!p)
                        break;
                    data = get_map_lvalue_unchecked(m, &sv); /* adds another ref */
                    free_svalue(&sv);
                    if (!data)
                        break;
                    p = read_unswapped_svalues(data, num_values, p);
                    if (!p)
                        break;
                }
                if (!p)
                {
                    clear_svalues(svp + 1, num);
                    return NULL;
                }
            }
            break;
          }

        case T_STRING:
        case T_SYMBOL:
        case T_POINTER:
#ifdef USE_STRUCTS
        case T_STRUCT:
#endif
        case T_QUOTED_ARRAY:
        case T_MAPPING:
        case T_NUMBER:
        case T_FLOAT:
        case T_OBJECT:
        case T_CLOSURE:
            memcpy(&svp->x, p, sizeof svp->x);
            p += sizeof svp->x;
            memcpy(&svp->u, p, sizeof svp->u);
            p += sizeof svp->u;
            break;

        default:
            fatal("bad type %d in read_unswapped_svalues()\n", svp->type);
        }
    } /* for() */
    
    return p;
} /* read_unswapped_svalues() */

/*-------------------------------------------------------------------------*/
static void
dummy_handler(const char * fmt UNUSED, ...)

/* Dummy error handler for array allocations in a swap-in.
 */

{
#ifdef __MWERKS__
#    pragma unused(fmt)
#endif
} /* dummy_handler() */

/*-------------------------------------------------------------------------*/
int
load_ob_from_swap (object_t *ob)

/* Load an object <ob> from the swap, both variables and program (without
 * the linenumbers).
 * The swap block for the variables is removed, the program swap block
 * is kept.
 *
 * Results are: 0: object was not swapped out
 *              > 0: object swapped in
 *              < 0: out of memory.
 *
 * The result (both positive and negative) can be interpreted
 * more detailed as well:
 *
 *    result & 0x01: program swapped in
 *    result & 0x02: variables swapped in
 *
 * The same flags are accepted by swap() as argument.
 */

{
    p_int swap_num;
    int result;

    result = 0;

    swap_num = (p_int)ob->prog;
    if (swap_num & 1)
    {
        /* Swap in the program */
        
        program_t tmp_prog, *prog;

        swap_num &= ~1;

        if (swapfile_size <= swap_num)
            fatal("Attempt to swap in from beyond the end of the swapfile.\n");
        if (fseek(swap_file, swap_num, 0) == -1)
            fatal("Couldn't seek the swap file, errno %d, offset %ld.\n",
                  errno, swap_num);
        if (d_flag > 1)
        {
            debug_message("%s Unswap object %s (ref %ld)\n", time_stamp()
                         , get_txt(ob->name), ob->ref);
        }

        /* The size of the program is unkown, so read first part to
         * find out. For greater efficiency we read in the full program_t
         * structure.
         */
        if (fread(&tmp_prog, sizeof tmp_prog, 1, swap_file) != 1)
        {
            fatal("Couldn't read the swap file.\n");
        }
        tmp_prog.swap_num = swap_num;

        /* Allocate the memory for the program, except for the
         * line numbers.
         */
        if ( !(prog = xalloc(tmp_prog.total_size)) )
            return -0x80;
        memcpy(prog, &tmp_prog, sizeof tmp_prog);

        /* Read in the rest of the program */
        if (tmp_prog.total_size - sizeof tmp_prog)
        {
            if (fread( ((char *)prog) + sizeof tmp_prog,
              tmp_prog.total_size - sizeof tmp_prog, 1, swap_file) != 1)
            {
                fatal("Couldn't read the swap file.\n");
            }
        }

        ob->prog = prog;
        locate_in (prog); /* relocate the internal pointers */
        prog->line_numbers = NULL;

        /* The reference count will already be 1 ! */

        total_bytes_unswapped += ob->prog->total_size;
        num_unswapped++;
        total_prog_block_size += ob->prog->total_size;
        total_num_prog_blocks += 1;
        result = 1;
    }

    swap_num = (p_int)ob->variables;
    if (swap_num & 1)
    {
        /* Swap in the variables */
        
        p_int total_size;
        unsigned char *block;
        mp_int size;
        svalue_t *variables;
        object_t dummy;
        object_t *save_current = current_object;
        void (*save_handler)(const char *, ...);

        swap_num &= ~1;
        if (swapfile_size <= swap_num)
            fatal("Attempt to swap in from beyond the end of the swapfile.\n");
        if (fseek(swap_file, swap_num, 0) == -1)
            fatal("Couldn't seek the swap file, errno %d, offset %ld.\n",
                  errno, swap_num);
        if (d_flag > 1)
        {
            debug_message("%s Unswap variables of %s\n", time_stamp()
                         , get_txt(ob->name));
        }

        /* Read the size of the block from the file */
        if (fread(&total_size, sizeof total_size, 1, swap_file) != 1)
        {
            fatal("Couldn't read the swap file.\n");
        }
        size = total_size - sizeof total_size;

        /* Allocate the memory buffer */
        if ( !(block = (unsigned char *)xalloc(size)) )
            return result | -0x80;

        /* Allocate the variable space */
        if ( !(variables = xalloc(
                sizeof(svalue_t) * ob->prog->num_variables
        )) )
        {
            xfree(block);
            return result | -0x80;
        }

        fread(block, size, 1, swap_file);

        /* Prepare to restore */
        current_object = &dummy;
#ifdef MALLOC_LPC_TRACE
        dummy.name = ob->name;
        dummy.prog = ob->prog;
#endif
        save_handler = allocate_array_error_handler;
        allocate_array_error_handler = dummy_handler;
        if (read_unswapped_svalues(variables, ob->prog->num_variables, block))
        {
            ob->variables = variables;
            result |= 2;
#ifdef CHECK_OBJECT_STAT
            if (check_object_stat)
            {
                fprintf(stderr, "DEBUG: OSTAT: (%ld:%ld) swapin( %p '%s') %d vars : %ld -> (%ld:%ld)\n"
                              , tot_alloc_object, tot_alloc_object_size, ob, ob->name ? get_txt(ob->name) : "<null>"
                              , ob->prog->num_variables
                              , (long)(ob->prog->num_variables * sizeof (svalue_t))
                              , tot_alloc_object, tot_alloc_object_size + (ob->prog->num_variables * sizeof (svalue_t))
                              );
            }
#endif
            tot_alloc_object_size += ob->prog->num_variables * sizeof (svalue_t);

            if (gc_status)
            {
                /* Called from within the GC - free later.
                 * Also keep the swap-block
                 */
                last_variable_block = block;
                last_variable_swap_num = swap_num;
            }
            else
            {
                xfree(block);
                swap_free(swap_num);
                num_vb_swapped--;
                total_vb_bytes_swapped -= total_size - sizeof total_size;
            }
        }
        else
        {
            /* Out of memory */
#ifdef CHECK_OBJECT_STAT
            if (check_object_stat)
            {
                fprintf(stderr, "DEBUG: OSTAT: (%ld:%ld) swapin( %p '%s') %d vars failed\n"
                              , tot_alloc_object, tot_alloc_object_size, ob, ob->name ? get_txt(ob->name) : "<null>"
                              , ob->prog->num_variables
                              );
            }
#endif

            xfree(block);
            result |= -0x80;
        }

        current_object = save_current;
        allocate_array_error_handler = save_handler;
    }

    if (!result)
        fatal("Loading unswapped object from swap.\n");

    /* Update the object flags */
    ob->flags &= ~O_SWAPPED;

    return result;
} /* load_ob_from_swap() */

/*-------------------------------------------------------------------------*/
Bool
load_line_numbers_from_swap (program_t *prog)

/* Load the line numbers for program <prog> from the swap.
 * Result is TRUE on success.
 */

{
    linenumbers_t tmp_numbers;
    linenumbers_t *lines;
    p_int swap_num;

    swap_num = prog->swap_num + prog->total_size;

    if (swapfile_size <= swap_num)
        fatal("Attempt to swap in from beyond the end of the swapfile.\n");
    if (fseek(swap_file, swap_num, 0) == -1)
        fatal("Couldn't seek the swap file, errno %d, offset %ld.\n",
              errno, swap_num);
    if (fread((char *)&tmp_numbers, sizeof tmp_numbers, 1, swap_file) != 1) {
        fatal("Couldn't read the swap file.\n");
    }

    if ( !(lines = xalloc(tmp_numbers.size)) )
        return MY_FALSE;

    *lines = tmp_numbers;

    if (tmp_numbers.size > sizeof(tmp_numbers))
    {
        fread(lines+1, tmp_numbers.size - sizeof(tmp_numbers)
             , 1, swap_file);
    }

    prog->line_numbers = lines;
    total_prog_block_size += lines->size;
    total_bytes_unswapped += lines->size;

    return MY_TRUE;
} /* load_line_numbers_from_swap() */

/*-------------------------------------------------------------------------*/
void
remove_prog_swap (program_t *prog, Bool load_line_numbers)

/* Program <prog> is going to be deleted or has been changed - remove its
 * swapfile entry if it has one. If <load_line_numbers> is true, load
 * the line number information back into memory before removing the swap entry.
 */

{
    p_int swap_num;

    swap_num = prog->swap_num;

    if (swap_num == -1) /* then program not swapped */
        return;

    /* This test is good not only for debugging, but also when the
     * processor is on fire, to stop subsequent damage.
     */
    if (swapfile_size <= swap_num)
        fatal("Attempt to remove swap entry beyond the end of the swapfile.\n");

    if (!prog->line_numbers && load_line_numbers)
    {
        if (!load_line_numbers_from_swap(prog))
            fatal("Can't unswap the line numbers.\n");
    }

    if (!prog->line_numbers)
    {
        /* The linenumber information has not been unswapped, thus
         * we need to read the linenumber size directly from the
         * swapfile.
         */
        linenumbers_t tmp_lines;

        if (fseek(swap_file, swap_num + prog->total_size, 0 ) == -1)
            fatal("Couldn't seek the swap file, errno %d, offset %ld.\n",
                  errno, swap_num);
        if (fread(&tmp_lines, sizeof tmp_lines, 1, swap_file) != 1)
        {
            fatal("Couldn't read the swap file.\n");
        }
        total_bytes_swapped -= tmp_lines.size;
    }
    else
    {
        total_bytes_unswapped -= prog->line_numbers->size;
        total_bytes_swapped -= prog->line_numbers->size;
    }

    swap_free(prog->swap_num);
    total_bytes_unswapped -= prog->total_size;
    total_bytes_swapped -= prog->total_size;
    num_unswapped--;
    num_swapped--;
    prog->swap_num = -1;
} /* remove_prog_swap() */

/*-------------------------------------------------------------------------*/
void
name_swap_file (const char *name)

/* Set the swap file name to a copy of <name>.
 */
 
{
    xstrncpy(file_name, name, sizeof file_name);
    file_name[sizeof file_name - 1] = '\0';
} /* name_swap_file()*/

/*-------------------------------------------------------------------------*/
void
unlink_swap_file (void)

/* Called at shutdown: Remove the swap file.
 */

{
    if (swap_file == NULL)
        return;
    unlink(file_name);
    fclose(swap_file);
} /* unlink_swap_file() */

/*-------------------------------------------------------------------------*/
size_t
swap_overhead (void)

/* Return the overhead size of the swap structures.
 */

{
    return num_swap_structs * sizeof(swap_block_t);
} /* swap_overhead() */

/*-------------------------------------------------------------------------*/
void
swap_status (strbuf_t *sbuf)

/* Add the information for "status swap" to the stringbuffer <sbuf>.
 */

{
    /* maximum seen so far: 10664 var blocks swapped,    5246112 bytes */
    strbuf_addf(sbuf, "%6ld prog blocks swapped,%10ld bytes\n"
                      "%6ld prog blocks unswapped,%8ld bytes\n"
                      "%6ld var blocks swapped,%11ld bytes\n"
                      "%6ld free blocks in swap,%10ld bytes\n"
                      "Swapfile size:%23ld bytes\n"
                , num_swapped - num_unswapped
                , total_bytes_swapped - total_bytes_unswapped
                , num_unswapped, total_bytes_unswapped
                , num_vb_swapped, total_vb_bytes_swapped
                , num_swapfree, total_bytes_swapfree
                , swapfile_size
    );
    strbuf_addf(sbuf, "Total reused space:%18ld bytes\n\n"
                    , total_swap_reused);
    strbuf_addf(sbuf
               , "Swap: searches: %5ld average search length: %3.1f\n"
                 "Free: searches: %5ld average search length: %3.1f\n"
               , swap_num_searches
               , (double)swap_total_searchlength /
                 ( swap_num_searches ? swap_num_searches : 1 )
               , swap_free_searches
               , (double)swap_free_searchlength /
                 ( swap_free_searches ? swap_free_searches : 1 )
    );
    strbuf_addf(sbuf, "Overhead: %ld blocks using %ld bytes.\n"
               , num_swap_structs, num_swap_structs * sizeof(swap_block_t)
               );
    strbuf_addf(sbuf, "Mode: %s - Freespace recycling: %s\n"
               , swap_compact_mode ? "compact" : "non-compact"
               , (recycle_free_space || !swap_compact_mode) ? "on" : "off"
    );
} /* swap_status() */

/*-------------------------------------------------------------------------*/
void
swap_dinfo_data (svalue_t *svp, int value)

/* Fill in the data for debug_info(DINFO_DATA, DID_SWAP)
 * into the svalue block <svp>.
 * If <value> is -1, <svp> points indeed to a value block; other it is
 * the index of the desired value and <svp> points to a single svalue.
 */
 
{
#define ST_NUMBER(which,code) \
    if (value == -1) svp[which].u.number = code; \
    else if (value == which) svp->u.number = code
    
    ST_NUMBER(DID_SW_PROGS, num_swapped - num_unswapped);
    ST_NUMBER(DID_SW_PROG_SIZE, total_bytes_swapped - total_bytes_unswapped);
    ST_NUMBER(DID_SW_PROG_UNSWAPPED, num_unswapped);
    ST_NUMBER(DID_SW_PROG_U_SIZE, total_bytes_unswapped);
    ST_NUMBER(DID_SW_VARS, num_vb_swapped);
    ST_NUMBER(DID_SW_VAR_SIZE, total_vb_bytes_swapped);
    ST_NUMBER(DID_SW_FREE, num_swapfree);
    ST_NUMBER(DID_SW_FREE_SIZE, total_bytes_swapfree);
    ST_NUMBER(DID_SW_FILE_SIZE, swapfile_size);
    ST_NUMBER(DID_SW_REUSED, total_swap_reused);
    ST_NUMBER(DID_SW_SEARCHES, swap_num_searches);
    ST_NUMBER(DID_SW_SEARCH_LEN, swap_total_searchlength);
    ST_NUMBER(DID_SW_F_SEARCHES, swap_free_searches);
    ST_NUMBER(DID_SW_F_SEARCH_LEN, swap_free_searchlength);
    ST_NUMBER(DID_SW_COMPACT, swap_compact_mode);
    ST_NUMBER(DID_SW_RECYCLE_FREE, recycle_free_space);

#undef ST_NUMBER
} /* swap_dinfo_data() */

/***************************************************************************/

