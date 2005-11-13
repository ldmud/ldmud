/* TODO: 'status swap' should also list the mode, if freespace is recycled
 * TODO:: and the swaps/s.
 * TODO: Background defragmentation of the swapfile
 */
#include "driver.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#ifdef HAVE_VALUES_H
#include <values.h>
#endif

#define NO_INCREMENT_STRING_REF
#include "swap.h"

#include "array.h"
#include "backend.h"
#include "comm.h"
#include "exec.h"
#include "gcollect.h"
#include "interpret.h"
#include "main.h"
#include "mapping.h"
#include "object.h"
#include "random.h"
#include "prolang.h"
#include "simulate.h"
#include "simul_efun.h"
#include "stralloc.h"
#include "wiz_list.h"

/* TODO: Use get_host_name() instead of gethostname() */
#ifdef AMIGA
#include "hosts/amiga/socket.h"
#endif

#define LOW_WATER_MARK  (swapfile_size >> 2)
#define HIGH_WATER_MARK (swapfile_size >> 1)

/*
 * Swap out programs / variables from objects.
 */

mp_int num_swapped = 0, num_unswapped = 0;
mp_int total_bytes_swapped = 0, total_bytes_unswapped = 0;
mp_int num_swapfree = 0;
mp_int num_vb_swapped = 0;
mp_int total_vb_bytes_swapped = 0;
mp_int total_bytes_swapfree = 0;
mp_int swapfile_size = 0;
/* Number of bytes that have been reused from freed swap prog blocks */
mp_int total_swap_reused = 0;
long swap_num_searches, swap_total_searchlength;
long swap_free_searches, swap_free_searchlength;

/* TODO: bool */ short swap_compact_mode = MY_FALSE;
static int /* TODO: bool */ recycle_free_space = MY_FALSE;
  /* True when freespace should be re-used, false if not
   */

static char file_name[100] = "";

static FILE *swap_file = (FILE *) 0;        /* The swap file is opened once */

mp_int total_num_prog_blocks, total_prog_block_size;

/*
 * Free space is kept in a simple linked list. The size field is < 0
 * for allocated blocks or > 0 for free blocks. swap_rover walks the
 * list when allocating and freeing blocks. This is an attempt to
 * reduce head movement in comparison to normal first fit algorithms.
 */

struct swap_block
{
  struct swap_block *next;
  mp_int size;
};

static struct swap_block swap_list;
static struct swap_block *swap_rover = 0; /* pointer to the current swap_block */
static struct swap_block *swap_previous = &swap_list; /* one prior to swap_rover */
static mp_int current_offset; /* file offset corresponding to swap_rover */

#if 0
check() {
    static mp_int old_total, old_vb_total, old_swapfree, old_size,
        old_reused, old_num_swapped, old_num_unswapped;

    if (total_bytes_swapped + total_vb_bytes_swapped + total_bytes_swapfree +
         (num_vb_swapped << 2) != swapfile_size ||
        num_swapped  < num_unswapped)
    {
        fatal("");
    }
    old_total = total_bytes_swapped;
    old_vb_total = total_vb_bytes_swapped;
    old_swapfree = total_bytes_swapfree;
    old_size = swapfile_size;
    old_reused = total_swap_reused;
    old_num_swapped = num_swapped;
    old_num_unswapped = num_unswapped;
}
#endif
/*
 * marion - adjust pointers for swap out and later relocate on swap in
 *   program
 *   line_numbers
 *   functions
 *   strings
 *   variable_names
 *   inherit
 *   argument_types
 *   type_start
 */
static int
locate_out (prog) struct program *prog; {
    char *p = 0; /* keep cc happy */

    if (!prog) return 0;
    if (d_flag > 1) {
        debug_message ("locate_out: %lX %lX %lX %lX %lX %lX %lX %lX\n",
            (long)prog->program, (long)prog->line_numbers,
            (long)prog->functions, (long)prog->strings,
            (long)prog->variable_names, (long)prog->inherit,
            (long)prog->argument_types, (long)prog->type_start);
    }
    prog->program        = &p[prog->program - (char *)prog];
    prog->line_numbers        = (char *)
        &p[(char *)prog->line_numbers - (char *)prog];
    prog->functions        = (uint32 *)
        &p[(char *)prog->functions - (char *)prog];
    prog->function_names= (unsigned short *)
        &p[(char *)prog->function_names - (char *)prog];
    prog->strings        = (char **)
        &p[(char *)prog->strings - (char *)prog];
    prog->variable_names= (struct variable *)
        &p[(char *)prog->variable_names - (char *)prog];
    prog->inherit        = (struct inherit *)
        &p[(char *)prog->inherit - (char *)prog];
    if (prog->type_start) {
        prog->argument_types = (unsigned short *)
            &p[(char *)prog->argument_types - (char *)prog];
        prog->type_start = (unsigned short *)
            &p[(char *)prog->type_start - (char *)prog];
    }
    return 1;
}


/*
 * marion - relocate pointers after swap in
 *   program
 *   line_numbers
 *   functions
 *   strings
 *   variable_names
 *   inherit
 *   argument_types
 *   type_start
 */
static int
locate_in (prog) struct program *prog; {
    char *p = (char *)prog;

    if (!prog) return 0;
    prog->id_number        =
        ++current_id_number ? current_id_number : renumber_programs();
    prog->program        = &p[prog->program - (char *)0];
    prog->line_numbers        = (char *)0;
    prog->functions        = (uint32 *)
        &p[(char *)prog->functions - (char *)0];
    prog->function_names= (unsigned short *)
        &p[(char *)prog->function_names - (char *)0];
    prog->strings        = (char **)
        &p[(char *)prog->strings - (char *)0];
    prog->variable_names= (struct variable *)
        &p[(char *)prog->variable_names - (char *)0];
    prog->inherit        = (struct inherit *)
        &p[(char *)prog->inherit - (char *)0];
    if (prog->type_start) {
        prog->argument_types = (unsigned short *)
            &p[(char *)prog->argument_types - (char *)0];
        prog->type_start     = (unsigned short *)
            &p[(char *)prog->type_start - (char *)0];
    }
    if (d_flag > 1) {
        debug_message ("locate_in: %lX %lX %lX %lX %lX %lX %lX %lX\n",
            (long)prog->program, (long)prog->line_numbers,
            (long)prog->functions, (long)prog->strings,
            (long)prog->variable_names, (long)prog->inherit,
            (long)prog->argument_types, (long)prog->type_start);
    }
    return 1;
}

#define SWAP_ABS(a) ((a)>0 ? (a) : (-a))

/*
 * Find a free block of sufficient size in the swap file and allocate
 * it. If there is none, add one at the end of the file. Return the
 * offset from the beginning of the file.
 */
static int swap_alloc(size)
  mp_int size;
{
  struct swap_block *mark,*last;
  int save_privilege;

  save_privilege = malloc_privilege;
  malloc_privilege = MALLOC_SYSTEM;

  if (!swap_compact_mode)
  {
      if (!recycle_free_space) {
          if (total_bytes_swapfree < HIGH_WATER_MARK)
              goto alloc_new_space;
          recycle_free_space = 1;
      } else {
          if (total_bytes_swapfree < LOW_WATER_MARK) {
              recycle_free_space = 0;
              goto alloc_new_space;
          }
      }
  } /* if (swap_compact_mode) */

  swap_num_searches++;
  mark = swap_rover;
  for (;;)
  {
    swap_total_searchlength++;
    if (!swap_rover)
    {
      swap_rover = &swap_list;
      swap_previous = 0;
      current_offset = 0;
    }
    if (size <= swap_rover->size)
    {
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
      /* split the block in two */
      /* num_swapfree remains unchanged */
      mark = (struct swap_block *) permanent_xalloc(sizeof(struct swap_block));
      mark->size = swap_rover->size - size;
      swap_rover->size = -size;
      mark->next = swap_rover->next;
      swap_rover->next = mark;
      malloc_privilege = save_privilege;
      return current_offset;
    }
    current_offset += SWAP_ABS(swap_rover->size);
    swap_previous = swap_rover;
    swap_rover = swap_rover->next;
    if (swap_rover == mark) /* Once around the list without success */
    {
alloc_new_space:
      last = swap_previous;
      while ( NULL != (mark = last->next) )
        last = mark;
      mark = (struct swap_block *) permanent_xalloc(sizeof(struct swap_block));
      mark->next = 0;
      last->next = mark;
      mark->size = -size;
      if (!swap_rover)
        swap_rover = mark;
      swapfile_size += size;
      malloc_privilege = save_privilege;
      return swapfile_size - size;
    }
  }
}

static int swap_free(offset, size)
  mp_int offset, size;
{
  if (offset < current_offset)
  {
    swap_rover = swap_list.next;
    swap_previous = &swap_list;
    current_offset = 0;
  }
  swap_free_searches++;
  while (current_offset < offset && swap_rover)
  {
    swap_free_searchlength++;
    swap_previous = swap_rover;
    current_offset += SWAP_ABS(swap_rover->size);
    swap_rover = swap_rover->next;
  }
  if (current_offset != offset || !swap_rover)
    fatal("Bad swapfile offset.\n");
  if (swap_rover->size > 0)
    fatal("Freeing non-allocated block within swap file.\n");
  swap_rover->size = -swap_rover->size; /* Make the size positive */
#ifdef DEBUG
  /* The B-Tree free storage administration will rely on a correct size */
  if (size != swap_rover->size)
    fatal("swap_free: size mismatch.\n");
#endif
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
    pfree((char *)swap_rover);
    num_swapfree--;
    swap_rover = swap_previous->next;
  }
  return 0; /* success */
}

static p_int store_swap_block(buffer, size)
    char *buffer;
    mp_int size;
{
    mp_int offset;

    if (swap_file == 0) {
        if (!*file_name) {
    sprintf(file_name, "%s.%s", SWAP_FILE, query_host_name());
        }
        swap_file = fopen(file_name, "w+b");
        /* Leave this file pointer open ! */
        if (swap_file == 0) {
            debug_message("Couldn't open swap file.\n");
            return -1;
        }
    }
    offset = swap_alloc(size);
    if (fseek(swap_file, offset, 0) == -1) {
        debug_message("Couldn't seek the swap file, errno %d, offset %ld.\n"
                     , errno, offset);
        return -1;
    }
    if (fwrite(buffer, size, 1, swap_file) != 1) {
        debug_message("I/O error in swap.\n");
        return -1;
    }
    return offset;
}

/*
 * Swap out an object. Only the program is swapped, not the 'struct object'.
 */
int swap_program(ob)
    struct object *ob;
{
    struct program *prog;
    p_int swap_num;

    if (d_flag > 1) { /* marion */
        debug_message("Swap object %s (ref %ld)\n", ob->name, ob->ref);
    }
    prog = ob->prog;
    if (prog->ref > 1) {
        if (d_flag > 1) {
            debug_message ("  program not swapped - cloned or inherited.\n");
        }
        return 0;
    }
    /*
     * Has this object already been swapped, and read in again ?
     * Then it is very easy to swap it out again.
     */
    if (prog->swap_num >= 0) {
        total_bytes_unswapped -= prog->total_size;
        ob->prog = (struct program *)(prog->swap_num | 1);
        free_prog(prog, MY_FALSE);        /* Do not free the strings or swapfile */
        ob->flags |= O_SWAPPED;
        num_unswapped--;
        return 1;
    }
    /* relocate the internal pointers */
    locate_out(prog);
    swap_num = store_swap_block((char *)prog, prog->total_size);
    if (swap_num  == -1) {
        locate_in(prog);
        return 0;
    }
    total_bytes_swapped += prog->total_size;
    num_swapped++;
    prog->swap_num = -1; /* for free_prog() , don't free linenumbers */
    free_prog(prog, MY_FALSE);        /* Don't free the shared strings or the swapfile */
    ob->prog = (struct program *)(swap_num | 1);
    ob->flags |= O_SWAPPED;
    return 1;
}

struct varblock {
    unsigned char *current;
    mp_int rest;
    char *start;
};

#define CHECK_SPACE(count) \
    if (rest < (mp_int)(count)) { \
        struct varblock *CStmp; \
        if ( !(CStmp = reallocate_block(p, rest, count)) ) {\
            CStmp = (struct varblock *)(p + rest); \
            CStmp->current = 0; \
            return CStmp; \
        } \
        p = CStmp->current; \
        rest = CStmp->rest; \
    }

static struct varblock *reallocate_block(p, rest, count)
    unsigned char *p;
    mp_int rest;
    mp_int count;
{
    struct varblock *tmp;
    char *start1, *start2;
    mp_int size, size2;

    tmp = (struct varblock *)(p + rest);
    start1 = tmp->start;
    size = (char *)tmp - start1;
    size2 = size;
    do {
        rest += size2;
        size2 <<= 1;
    } while (rest < count);
    if ( !(start2 = xalloc(size2 + sizeof(struct varblock))) )
        return 0;
    memcpy(start2, start1, size2 - rest);
    xfree(start1);
    tmp = (struct varblock *)(start2 + size2);
    tmp->current = (unsigned char *)tmp - rest;
    tmp->rest = rest;
    tmp->start = start2;
    return tmp;
}

#define ADD_TO_BLOCK(var) \
    memcpy((char *)p, &var, sizeof(var)); \
    p += sizeof(var); \
    rest -= sizeof(var); \

#define SWAP_SVALUES(svp, num) {\
    struct varblock *tmp; \
    tmp = (struct varblock *)(p + rest); \
    tmp->current = p; \
    tmp->rest = rest; \
    tmp = swap_svalues(svp, num, tmp); \
    if ( !(p = tmp->current) ) { \
        return tmp; \
    } \
    rest = tmp->rest; \
}

static struct varblock *swap_svalues
  PROT((struct svalue *, mp_int, struct varblock *));

static void swap_mapping_filter(key, values, extra)
    struct svalue *key;
    struct svalue *values;
    char *extra;
{
    struct varblock *block = *((struct varblock **)extra);
    if (block->current)
        block = swap_svalues(key, 1, block);
    if (block->current)
        block = swap_svalues(values, *((p_int *)block->start), block);
    *((struct varblock **)extra) = block;
}

static struct varblock *swap_svalues(svp, num, block)
    struct svalue *svp;
    mp_int num;
    struct varblock *block;
{
    static int swapping_alist = 0;

    unsigned char *p;
    mp_int rest;

    p = block->current;
    rest = block->rest;
    for (; --num >= 0; svp++) {
        switch(svp->type) {
          case T_STRING:
          case T_SYMBOL:
          {
            mp_int len, size;

            if (swapping_alist)
                goto swap_opaque;
            len = strlen(svp->u.string) + 1;
            size = 1 + sizeof svp->x + len;
            CHECK_SPACE(size)
            rest -= size;
            *p++ = svp->type | T_MOD_SWAPPED;
            memcpy((char *)p, &svp->x, sizeof(svp->x));
            p += sizeof svp->x;
            memcpy((char *)p, svp->u.string, len);
            p += len;
            break;
          }
          case T_POINTER:
          {
            int32 size;

            size = VEC_SIZE(svp->u.vec);
            if (svp->u.vec->ref > 1 || !size || swapping_alist)
                goto swap_opaque;
            if (size > 1 && is_alist(svp->u.vec))
                swapping_alist = 1;
            CHECK_SPACE(1 + sizeof(size) + sizeof(struct wiz_list *))
            *p++ = T_POINTER | T_MOD_SWAPPED;
            rest--;
            ADD_TO_BLOCK(size)
            ADD_TO_BLOCK(svp->u.vec->user)
            SWAP_SVALUES(svp->u.vec->item, size)
            swapping_alist = 0;
            break;
          }
          case T_QUOTED_ARRAY:
          {
            int32 size;

            size = VEC_SIZE(svp->u.vec);
            if (svp->u.vec->ref > 1 || !size || swapping_alist)
                goto swap_opaque;
            CHECK_SPACE(
              1 + sizeof svp->x.quotes +
              sizeof size + sizeof(struct wiz_list *)
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
            struct mapping *m = svp->u.map;
            p_int size, num_values, save;
            struct varblock *tmp;
            struct svalue *svp2;

            if (m->ref > 1 || swapping_alist)
                goto swap_opaque;
            /* mappings with object or closure keys can get stale, which
             * necessiates special treatment in garbage_collection(),
             * which in turn is not prepared to face swapping.
             */
            svp2 = CM_MISC(m->condensed);
            size = m->condensed->misc_size;
            while ( (size -= sizeof(struct svalue)) >= 0) {
                if ((--svp2)->type == T_OBJECT || svp2->type == T_CLOSURE)
                    goto swap_opaque;
            }
            CHECK_SPACE(
              1 + sizeof num_values + sizeof size + sizeof m->user
            )
            *p++ = T_MAPPING | T_MOD_SWAPPED;
            rest--;
            /* num_values might be wider than m->num_values. */
            num_values = m->num_values;
            ADD_TO_BLOCK(num_values);
            size = m->condensed->string_size/sizeof(char *) +
                m->condensed->misc_size/sizeof(struct svalue) +
                (m->hash ? m->hash->used - m->hash->condensed_deleted : 0);
            ADD_TO_BLOCK(size);
            ADD_TO_BLOCK(m->user);
            tmp = (struct varblock *)(p + rest);
            tmp->current = p;
            tmp->rest = rest;
            save = *((p_int *)tmp->start);
            *((p_int *)tmp->start) = m->num_values;
            walk_mapping(m, swap_mapping_filter, &tmp);
            *((p_int *)tmp->start) = save;
            if ( !(p = tmp->current) ) {
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
    {
        struct varblock *tmp;

        tmp = (struct varblock *)(p + rest);
        tmp->current = p;
        tmp->rest = rest;
        return tmp;
    }
}

/* When garbage collection is done, restoring strings would give nothing
 * but trouble, thus, a dummy number is inserted instead.
 */
#ifndef MALLOC_smalloc
#define garbage_collection_in_progress 0
#endif
static unsigned char *last_variable_block;
static mp_int last_variable_swap_num;
static char *last_zeroed_swapped_object;

struct free_swapped_mapping_locals {
    p_int num_values;
    unsigned char *p;
};

static unsigned char *free_swapped_svalues
        PROT((struct svalue *, mp_int, unsigned char *));

static void free_swapped_mapping_filter(key, values, extra)
    struct svalue *key, *values;
    char *extra;
{
    struct free_swapped_mapping_locals *l;
    unsigned char *p;

    l = (struct free_swapped_mapping_locals *)extra;
    p = l->p;
    p = free_swapped_svalues(key, 1, p);
    p = free_swapped_svalues(values, l->num_values, p);
    l->p = p;
}

static unsigned char *free_swapped_svalues(svp, num, p)
    struct svalue *svp;
    mp_int num;
    unsigned char *p;
{
    for (; --num >= 0; svp++) {
        switch(*p) {
          case T_STRING | T_MOD_SWAPPED:
            if (svp->x.string_type == STRING_MALLOC) {
                if (!garbage_collection_in_progress)
                    xfree(svp->u.string);
                p = (unsigned char *)strchr(
                  (char *)p + 1 + sizeof svp->x, 0
                ) + 1;
                break;
            }
            /* else fall through */
          case T_SYMBOL | T_MOD_SWAPPED:
            if (!garbage_collection_in_progress)
                free_string(svp->u.string);
            p = (unsigned char *)strchr((char *)p + 1 + sizeof svp->x, 0) + 1;
            break;
          case T_QUOTED_ARRAY | T_MOD_SWAPPED:
            p += sizeof svp->x;
          case T_POINTER | T_MOD_SWAPPED:
            p += 1 + sizeof(int32) + sizeof(struct wizlist *);
            p =
              free_swapped_svalues(svp->u.vec->item, VEC_SIZE(svp->u.vec), p);
            free_empty_vector(svp->u.vec);
            break;
          case T_MAPPING | T_MOD_SWAPPED:
          {
            /* beware: a mapping can get unswappable when it is entered
               in the stale_mapping list. Or the stale_mapping list has to
               be recoded to include swapped mappings.
             */
            struct free_swapped_mapping_locals l;

            p +=
              1 +
              sizeof(p_int) +
              sizeof(p_int) +
              sizeof(struct wiz_list *);
            l.num_values = svp->u.map->num_values;
            l.p = p;
            walk_mapping(svp->u.map, free_swapped_mapping_filter, (char *)&l);
            p = l.p;
            free_empty_mapping(svp->u.map);
            break;
          }
          case T_OBJECT:
            if (svp->type == T_NUMBER) {
                *p++ = T_NUMBER;
                memcpy(p, (char *)&svp->x, sizeof svp->x);
                p += sizeof svp->x;
                memcpy(p, (char *)&svp->u, sizeof svp->u);
                p += sizeof svp->u;
                last_zeroed_swapped_object = (char*)p;
                break;
            }
          case T_STRING:
          case T_SYMBOL:
          case T_POINTER:
          case T_QUOTED_ARRAY:
          case T_MAPPING:
          case T_NUMBER:
          case T_FLOAT:
          case T_CLOSURE:
            p += 1 + sizeof svp->x + sizeof svp->u;
            break;
          default:
            fatal("bad type %d in free_swapped_svalues()\n", *p);
        }
    }
    return p;
}

#define VARBLOCK_STARTSIZE 0x800

int swap_variables(ob)
    struct object *ob;
{
    char *start;
    p_int total_size;
    struct varblock *block;
    p_int swap_num;
    unsigned short num_variables;

    if (!ob->variables)
        return 1;
    if (ob == simul_efun_object)
        return 1;
    if (garbage_collection_in_progress) {
        num_variables = ob->prog->num_variables;
        last_zeroed_swapped_object = 0;
        (void)free_swapped_svalues(
          ob->variables, num_variables, last_variable_block
        );
        if (last_zeroed_swapped_object) {
            if (fseek(swap_file, last_variable_swap_num + sizeof(p_int), 0) ==
                                                                            -1)
            {
                fatal("Couldn't seek the swap file, errno %d, offset %ld.\n",
                      errno, last_variable_swap_num + sizeof(p_int));
            }
            if (fwrite(
                  last_variable_block,
                  last_zeroed_swapped_object - (char *)last_variable_block,
                  1, swap_file) != 1)
            {
                fatal("I/O error in swap.\n");
            }
        }
        xfree((char *)last_variable_block);
        xfree((char *)ob->variables);
        ob->variables = (struct svalue *)(last_variable_swap_num | 1);
        ob->flags |= O_SWAPPED;
        return 1;
    }
    swap_num = (p_int)ob->prog;
    if (swap_num & 1) {
        swap_num &= ~1;
        swap_num +=
          (PTRTYPE)(&((struct program *)0)->num_variables) - (PTRTYPE)0;
        if (swapfile_size <= swap_num)
            fatal("Attempt to swap in from beyond the end of the swapfile.\n");
        if (fseek(swap_file, swap_num, 0) == -1)
            fatal("Couldn't seek the swap file, errno %d, offset %ld.\n",
                  errno, swap_num);
        if (fread(
                (char *)&num_variables,
                sizeof num_variables, 1, swap_file
        ) != 1) {
            fatal("Couldn't read the swap file.\n");
        }
    } else {
        num_variables = ob->prog->num_variables;
    }
    start = xalloc(VARBLOCK_STARTSIZE + sizeof(struct varblock));
    if (!start)
        return 0;
    block = (struct varblock *)(start + VARBLOCK_STARTSIZE);
    block->current = (unsigned char *)start + sizeof total_size;
    block->rest = VARBLOCK_STARTSIZE - sizeof total_size;
    block->start = start;
    block = swap_svalues(ob->variables, num_variables, block);
    if (!block->current)
    {
        xfree(block->start);
        return 0;
    }
    *(p_int*)block->start = total_size =
      (((char *)block->current - block->start) + (sizeof(p_int) - 1)) &
        (~(sizeof(p_int) - 1));
    swap_num = store_swap_block(block->start, total_size);
    if (swap_num  == -1) {
        xfree(block->start);
        return 0;
    }
    (void)free_swapped_svalues(
      ob->variables, num_variables, block->start + sizeof total_size
    );
    num_vb_swapped++;
    total_vb_bytes_swapped += total_size - sizeof total_size;
    xfree(block->start);
    xfree(ob->variables);
    ob->variables = (struct svalue *)(swap_num | 1);
    ob->flags |= O_SWAPPED;
    return 1;
}

int swap(ob, mode)
    struct object *ob;
    int mode;
{
    int result = 1;

    if (ob->flags & O_DESTRUCTED)
        return 0;
    if (mode & 2)
        result &= swap_variables(ob);
    if (mode & 1)
        result &= swap_program(ob);
    return result;
}

static void clear_svalues(svp, num)
    struct svalue *svp;
    mp_int num;
{
    for (; --num >= 0;) {
        svp->type = T_NUMBER;
    }
}

/* return next byte to read if successful, or 0 for out of memory failure. */
static unsigned char *read_unswapped_svalues(svp, num, p)
    struct svalue *svp;
    mp_int num;
    unsigned char *p;
{
    for (;--num >= 0; svp++) {
        svp->type = *p & ~T_MOD_SWAPPED;
        switch(*p++) {
          case T_STRING | T_MOD_SWAPPED:
          case T_SYMBOL | T_MOD_SWAPPED:
          {
            char *s;

            memcpy((char *)&svp->x, p, sizeof svp->x);
            p += sizeof svp->x;
            if (garbage_collection_in_progress) {
                svp->type = T_NUMBER;
            } else {
                if (svp->type == T_STRING &&
                    svp->x.string_type == STRING_MALLOC)
                {
                    s = string_copy((char *)p);
                } else {
                    s = make_shared_string((char *)p);
                }
                if (!s) {
                    clear_svalues(svp, num + 1);
                    return 0;
                }
                svp->u.string = s;
            }
            p = (unsigned char *)strchr((char *)p, 0) + 1;
            break;
          }
          case T_QUOTED_ARRAY | T_MOD_SWAPPED:
            memcpy((char *)&svp->x, p, sizeof svp->x);
            p += sizeof svp->x;
          case T_POINTER | T_MOD_SWAPPED:
          {
            int32 size;
            struct wiz_list *user;
            struct vector *v;

            memcpy((char *)&size, p, sizeof size);
            p += sizeof size;
            memcpy((char *)&user, p, sizeof user);
            p += sizeof user;
            current_object->user = user;
            v = allocate_array(size);
            svp->u.vec = v;
            if (!v) {
                clear_svalues(svp, num + 1);
                return 0;
            }
            p = read_unswapped_svalues(v->item, size, p);
            if (!p) {
                clear_svalues(svp + 1, num);
                return 0;
            }
#ifdef MALLOC_smalloc
            if (garbage_collection_in_progress == 3) {
                clear_memory_reference((char *)v);
                v->ref = 0;
            }
#endif
            break;
          }
          case T_MAPPING | T_MOD_SWAPPED:
          {
            struct mapping *m;
            p_int num_values;
            struct wiz_list *user;
            p_int num_keys;

            memcpy((char *)&num_values, p, sizeof num_values);
            p += sizeof num_values;
            memcpy((char *)&num_keys, p, sizeof num_keys);
            p += sizeof num_keys;
            memcpy((char *)&user, p, sizeof user);
            p += sizeof user;
            if (garbage_collection_in_progress) {
                /* The garbage collector is not prepared to handle hash
                 * mappings. On the other hand, the order of keys does
                 * not matter here.
                 * We can assume here that all allocation functions succeed
                 * because the garbage collector runs with
                 * malloc_privilege == MALLOC_SYSTEM .
                 */
                struct condensed_mapping *cm;
                mp_int size;
                struct svalue *data, *svp2;

                size =
                  sizeof *cm + num_keys * sizeof(struct svalue) * (1 + num_values);
                cm = (struct condensed_mapping *)
                  ( (char *)xalloc(size) + size - sizeof *cm );
                m = (struct mapping *)xalloc(sizeof *m);
                cm->string_size = 0;
                cm->misc_size = num_keys * sizeof(struct svalue);
                m->hash = 0;
                m->condensed = cm;
                m->num_values = num_values;
                m->ref = 1;
                m->user = user;
                user->mapping_total +=
                    sizeof *m + sizeof(char*) + size + sizeof(char*);
                num_mappings++;
                svp->u.map = m;
                svp2 = CM_MISC(cm);
                size = cm->misc_size;
                data = (struct svalue *)((char *)svp2 - size);
                while ( (size -= sizeof(struct svalue)) >= 0) {
                    data -= num_values;
                    p = read_unswapped_svalues(--svp2, 1, p);
                    p = read_unswapped_svalues(data, num_values, p);
                }
#ifdef MALLOC_smalloc
                if (garbage_collection_in_progress == 3) {
                    clear_memory_reference((char *)m);
                    clear_memory_reference(
                      (char *)CM_MISC(cm) -
                        cm->misc_size * (m->num_values + 1)
                    );
                    m->ref = 0;
                }
#endif
            } else {
                mp_int i;
                struct wiz_list *save;

                save = current_object->user;
                current_object->user = user;
                m = allocate_mapping(num_keys, num_values);
                current_object->user = save;
                if (!m) {
                    clear_svalues(svp, num + 1);
                    return 0;
                }
                svp->u.map = m;
                for (i = num_keys; --i >= 0;) {
                    struct svalue sv, *data;

                    p = read_unswapped_svalues(&sv, 1, p); /* adds 1 ref */
                    if (!p)
                        break;
                    data = get_map_lvalue(m, &sv, MY_TRUE); /* adds another ref */
                    free_svalue(&sv);
                    if (!data)
                        break;
                    p = read_unswapped_svalues(data, num_values, p);
                    if (!p)
                        break;
                }
                if (!p) {
                    clear_svalues(svp + 1, num);
                    return 0;
                }
            }
            break;
          }
          case T_STRING:
          case T_SYMBOL:
          case T_POINTER:
          case T_QUOTED_ARRAY:
          case T_MAPPING:
          case T_NUMBER:
          case T_FLOAT:
          case T_OBJECT:
          case T_CLOSURE:
            memcpy((char *)&svp->x, p, sizeof svp->x);
            p += sizeof svp->x;
            memcpy((char *)&svp->u, p, sizeof svp->u);
            p += sizeof svp->u;
            break;
          default:
            fatal("bad type %d in read_unswapped_svalues()\n", svp->type);
        }
    }
    return p;
}

static void dummy_handler(char * fmt UNUSED, ...)
{
#ifdef __MWERKS__
#    pragma unused(fmt)
#endif
}

#ifdef DEBUG
static p_int debug_var_swap_num, debug_prog_swap_num;
#endif

int load_ob_from_swap(ob)
    struct object *ob;
{
    p_int swap_num;
    int result;

    result = 0;
    /* If the old reset time has passed, schedule a new one for the near
     * future. Do not give the player a safe haven for the first
     * TIME_TO_RESET/2 seconds as the canonical formula would do.
     */
    if (ob->next_reset <= current_time)
        ob->next_reset = current_time + random_number(TIME_TO_RESET);
    swap_num = (p_int)ob->prog;
    if (swap_num & 1) {
        struct program tmp_prog, *prog;

#ifdef DEBUG
        debug_prog_swap_num = swap_num;
#endif
        swap_num &= ~1;
        if (swapfile_size <= swap_num)
            fatal("Attempt to swap in from beyond the end of the swapfile.\n");
        if (fseek(swap_file, swap_num, 0) == -1)
            fatal("Couldn't seek the swap file, errno %d, offset %ld.\n",
                  errno, swap_num);
        if (d_flag > 1) { /* marion */
            debug_message("Unswap object %s (ref %ld)\n", ob->name, ob->ref);
        }
        /*
         * The size of the program is unkown, so read first part to
         * find out.
         *
         * marion - again, the read in a block is more efficient
         */
        if (fread((char *)&tmp_prog, sizeof tmp_prog, 1, swap_file) != 1) {
            fatal("Couldn't read the swap file.\n");
        }
        tmp_prog.swap_num = swap_num;
        tmp_prog.total_size = tmp_prog.line_numbers - (char *)0;
        if ( !(prog = (struct program *)xalloc(tmp_prog.total_size)) )
            return -0x80;
        memcpy((char *)prog, (char *)&tmp_prog, sizeof tmp_prog);
        if (tmp_prog.total_size - sizeof tmp_prog) {
            if (fread((char *)prog + sizeof tmp_prog,
              tmp_prog.total_size - sizeof tmp_prog, 1, swap_file) != 1)
            {
                fatal("Couldn't read the swap file.\n");
            }
        }
        ob->prog = prog;
        /*
         * to be relocated:
         *   program
         *   functions
         *   strings
         *   variable_names
         *   inherit
         *   argument_types
         *   type_start
         *   to be replaced: id_number
         */
        locate_in (prog); /* relocate the internal pointers */

        /* The reference count will already be 1 ! */
        total_bytes_unswapped += ob->prog->total_size;
        num_unswapped++;
        total_prog_block_size += ob->prog->total_size;
        total_num_prog_blocks += 1;
        result = 1;
    }
    swap_num = (p_int)ob->variables;
    if (swap_num & 1) {
        p_int total_size;
        unsigned char *block;
        mp_int size;
        struct svalue *variables;
        struct object dummy, *save_current = current_object;
        void (*save_handler)(char *, ...);

#ifdef DEBUG
        debug_var_swap_num = swap_num;
#endif
        swap_num &= ~1;
        if (swapfile_size <= swap_num)
            fatal("Attempt to swap in from beyond the end of the swapfile.\n");
        if (fseek(swap_file, swap_num, 0) == -1)
            fatal("Couldn't seek the swap file, errno %d, offset %ld.\n",
                  errno, swap_num);
        if (d_flag > 1) {
            debug_message("Unswap variables of %s\n", ob->name);
        }
        if (fread((char *)&total_size, sizeof total_size, 1, swap_file) != 1) {
            fatal("Couldn't read the swap file.\n");
        }
        size = total_size - sizeof total_size;
        if ( !(block = (unsigned char *)xalloc(size)) )
            return result | -0x80;
        if ( !(variables = (struct svalue *)xalloc(
                sizeof(struct svalue) * ob->prog->num_variables
        )) ) {
            xfree((char *)block);
            return result | -0x80;
        }
        fread((char *)block, size, 1, swap_file);
        current_object = &dummy;
#ifdef SMALLOC_LPC_TRACE
        dummy.name = ob->name;
        dummy.prog = ob->prog;
#endif
        save_handler = allocate_array_error_handler;
        allocate_array_error_handler = dummy_handler;
        if (read_unswapped_svalues(variables, ob->prog->num_variables, block)) {
            ob->variables = variables;
            result |= 2;
            if (garbage_collection_in_progress) {
                last_variable_block = block;
                last_variable_swap_num = swap_num;
            } else {
                xfree((char *)block);
                if (swap_free(swap_num, total_size)) {
                    debug_message("I/O error in swap.\n");
                    result ^= -0x80 | 2;
                } else {
                    num_vb_swapped--;
                    total_vb_bytes_swapped -= total_size - sizeof total_size;
                }
            }
        } else {
            xfree((char *)block);
            result |= -0x80;
        }
        current_object = save_current;
        allocate_array_error_handler = save_handler;
    }
#ifndef BUG_FREE
    if (!result)
        fatal("Loading not swapped object.\n");
#endif
    ob->flags &= ~O_SWAPPED;
    if (!(ob->flags & O_DESTRUCTED) && function_exists("clean_up",ob)) {
        ob->flags |= O_WILL_CLEAN_UP;
    }
    return result;
}

int load_line_numbers_from_swap(prog)
    struct program *prog;
{
    struct program tmp_prog;
    char *lines;
    p_int swap_num, size;

    swap_num = prog->swap_num;
    if (swapfile_size <= swap_num)
        fatal("Attempt to swap in from beyond the end of the swapfile.\n");
    if (fseek(swap_file, swap_num, 0) == -1)
        fatal("Couldn't seek the swap file, errno %d, offset %ld.\n",
              errno, swap_num);
    if (fread((char *)&tmp_prog, sizeof tmp_prog, 1, swap_file) != 1) {
        fatal("Couldn't read the swap file.\n");
    }
    swap_num += tmp_prog.line_numbers - (char *)0;
    if (fseek(swap_file, swap_num, 0) == -1)
        fatal("Couldn't seek the swap file, errno %d, offset %ld.\n",
              errno, swap_num);
    size = tmp_prog.total_size - prog->total_size;
    if ( !(lines = xalloc(size)) )
        return 0;
    fread(lines, size, 1, swap_file);
    prog->total_size = tmp_prog.total_size + sizeof(p_int);
    prog->line_numbers = lines;
    total_prog_block_size += size + sizeof(p_int);
    return 1;
}

int remove_swap_file(prog)
    struct program *prog;
{
    p_int swap_num;
    struct program tmp_prog;

    swap_num = prog->swap_num;
    /* This test is good not only for debugging, but also when the
     * processor is on fire, to stop subsequent damage.
     */
    if (swapfile_size <= swap_num)
        fatal("Attempt to remove swap entry beyond the end of the swapfile.\n");
    /* The linenumber information has probably not been unswapped, thus
     * prog->total_size is just the current size in memory.
     */
    if (fseek(swap_file, swap_num, 0 ) == -1)
        fatal("Couldn't seek the swap file, errno %d, offset %ld.\n",
              errno, swap_num);
    if (fread((char *)&tmp_prog, sizeof tmp_prog, 1, swap_file) != 1) {
        fatal("Couldn't read the swap file.\n");
    }
    if (swap_free(prog->swap_num, tmp_prog.total_size)) {
        debug_message("I/O error in swap.\n");
        return 0;
    }
    total_bytes_unswapped -= prog->total_size;
    total_bytes_swapped -= tmp_prog.total_size;
    num_unswapped--;
    num_swapped--;
    prog->swap_num = -1;
    return 1;
}

void name_swap_file(const char *name)
{
    strncpy(file_name, name, sizeof file_name);
    file_name[sizeof file_name - 1] = '\0';
}

/*
 * This one is called at shutdown. Remove the swap file.
 */
void unlink_swap_file() {
    if (swap_file == 0)
        return;
    unlink(file_name);
    fclose(swap_file);
}
