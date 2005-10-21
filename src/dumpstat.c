/*---------------------------------------------------------------------------
 * Gamedriver - Dump object statistics.
 *
 *---------------------------------------------------------------------------
 * Function to compute the memory usage of objects and dump their
 * statistics.
 *---------------------------------------------------------------------------
 */

#include "driver.h"
#include "typedefs.h"

#include <stdio.h>

#include "dumpstat.h"
#include "array.h"
#include "closure.h"
#include "exec.h"
#include "filestat.h"
#include "mapping.h"
#include "mstrings.h"
#include "object.h"
#include "ptrtable.h"
#include "simulate.h"
#include "stdstrings.h"
#include "smalloc.h" /* malloced_size() */
#include "svalue.h"

/*-------------------------------------------------------------------------*/

static size_t svalue_size(svalue_t *); /* forward */

/* Auxiliary structure for counting a mapping */

struct svalue_size_locals
{
    mp_uint total;       /* Return: total memory usage of all elements */
    int     num_values;  /* Passed: width of the mapping */
};

/*-------------------------------------------------------------------------*/

static struct pointer_table *ptable;
  /* The pointer_table to register all arrays and mappings.
   */

/*-------------------------------------------------------------------------*/
static void
svalue_size_map_filter (svalue_t *key, svalue_t *values, void *extra)

/* Called for all keys in a mapping, it counts the size of the values
 * and returns the total in extra->num_values.
 */

{
    struct svalue_size_locals *locals;
    int i;

    locals = (struct svalue_size_locals*)extra;
    locals->total += svalue_size(key);
    for(i = locals->num_values; --i >= 0; )
    {
        locals->total += svalue_size(values++) + sizeof(svalue_t);
    }
}

/*-------------------------------------------------------------------------*/
static size_t
svalue_size (svalue_t *v)

/* Compute the memory usage of *<v>, calling svalue_size() recursively
 * if necessary, and return it. The size of *v itself is not included.
 */

{
    mp_uint i, total;

    switch(v->type)
    {
    case T_OBJECT:
    case T_NUMBER:
    case T_FLOAT:
        return 0;

    case T_STRING:
    case T_SYMBOL:
        return mstr_mem_size(v->u.str);

    case T_MAPPING:
    {
        struct svalue_size_locals locals;
        struct condensed_mapping *cm;

        if (NULL == register_pointer(ptable, v->u.map) ) return 0;

        cm = v->u.map->condensed;
        locals.total = (mp_uint)(cm->string_size + cm->misc_size);
        locals.num_values = v->u.map->num_values;
        walk_mapping(v->u.map, svalue_size_map_filter, &locals);
        if (v->u.map->hash)
            locals.total +=
              sizeof(struct hash_mapping) +
                v->u.map->hash->mask * sizeof(struct map_chain *) +
                  v->u.map->hash->used *
                    (sizeof (struct map_chain) - sizeof(svalue_t));
        return locals.total;
    }

    case T_POINTER:
    case T_QUOTED_ARRAY:
    {
        if (v->u.vec == &null_vector) return 0;
        if (NULL == register_pointer(ptable, v->u.vec) ) return 0;
#ifdef MALLOC_smalloc
        total = malloced_size(v->u.vec) * sizeof(p_int);
#else
        total = sizeof *v->u.vec - sizeof v->u.vec->item +
          sizeof(svalue_t) * v->u.vec->size + sizeof(char *);
#endif
        for (i=0; i < (mp_int)VEC_SIZE(v->u.vec); i++) {
            total += svalue_size(&v->u.vec->item[i]);
        }
        return total;
    }

    case T_CLOSURE:
    {
        int num_values;
        svalue_t *svp;
        lambda_t *l;

        if (!CLOSURE_MALLOCED(v->x.closure_type)) return 0;
        if (!CLOSURE_REFERENCES_CODE(v->x.closure_type)) {
            /* CLOSURE_LFUN || CLOSURE_IDENTIFIER || CLOSURE_PRELIMINARY */
            return sizeof *v->u.lambda + sizeof(char *);
        }
        /* CLOSURE_LAMBDA */
        total = 0;
        l = v->u.lambda;
        if (v->x.closure_type == CLOSURE_BOUND_LAMBDA)
        {
            total += sizeof *l - sizeof l->function + sizeof l->function.lambda;
            l = l->function.lambda;
        }
        num_values = EXTRACT_UCHAR(&l->function.code[0]);
        if (num_values == 0xff)
            num_values = ((svalue_t *)l)[-0xff].u.number;
        svp = (svalue_t *)l - num_values;
        if (NULL == register_pointer(ptable, svp)) return 0;
#ifdef MALLOC_smalloc
        total += malloced_size(svp) * sizeof(p_int);
#else
        total += sizeof(svalue_t) * num_values + sizeof (char *);
        {
            bytecode_p p = &l->function.code[2];
            do {
                ++p;
                switch(GET_CODE(p)) {
                  case F_RETURN:
                  case F_RETURN0:
                    break;
                  default:
                    continue;
                }
                break;
            } while (1);
            total += p - (bytecode_p)l
                     + (sizeof(bytecode_p) - 1) & ~(sizeof(bytecode_p) - 1);
        }
#endif
        while (--num_values >= 0) {
            total += svalue_size(svp++);
        }
        return total;
    }

    default:
        fatal("Illegal type: %d\n", v->type);
    }

    /*NOTREACHED*/
    return 0;
}

/*-------------------------------------------------------------------------*/
mp_int
data_size (object_t *ob)

/* Compute the memory usage of the data held by object <ob> and
 * return it. If the object is swapped out or has no variables,
 * return 0.
 */

{
    mp_int total = sizeof(p_int); /* smalloc overhead */
    int i;
    svalue_t *svp;

    if (ob->flags & O_SWAPPED || !(i = ob->prog->num_variables) )
        return 0;
    ptable = new_pointer_table();
    if (!ptable)
        error("(dumpstat) Out of memory for new pointer table.\n");
    for (svp = ob->variables; --i >= 0; svp++)
        total += svalue_size(svp) + sizeof (svalue_t);
    free_pointer_table(ptable);
    return total;
}

/*-------------------------------------------------------------------------*/
Bool
dumpstat (string_t *fname)

/* Called by the command parser, this function dumps statistics about
 * all objects into the file $MUDLIB/<fname>.
 * Return TRUE on success, FALSE if <fname> can't be written.
 */

{
    FILE *f;
    object_t *ob;

    static char *swapstrings[] =
        {"", "PROG SWAPPED", "VAR SWAPPED", "SWAPPED", };

    fname = check_valid_path(fname, current_object, STR_OBJDUMP, MY_TRUE);
    if (!fname)
        return MY_FALSE;
    f = fopen(get_txt(fname), "w");
    if (!f)
        return MY_FALSE;
    FCOUNT_WRITE(get_txt(fname));

    for (ob = obj_list; ob; ob = ob->next_all)
    {
        mp_int tmp;
#ifdef DEBUG
        if (ob->flags & O_DESTRUCTED) /* TODO: Can't happen */
            continue;
#endif
        if (!O_PROG_SWAPPED(ob)
         && (ob->prog->ref == 1 || !(ob->flags & (O_CLONE|O_REPLACED))))
        {
            tmp = ob->prog->total_size;
        }
        else
        {
            tmp = 0;
        }
        fprintf(f, "%-20s %5ld ref %2ld %s ",
                 get_txt(ob->name),
                tmp + (long)data_size(ob) + sizeof (object_t) +
                sizeof(p_int) /* smalloc overhead */ ,
                ob->ref,
                ob->flags & O_HEART_BEAT ? "HB" : "  "
        );
        if (ob->super)
            fprintf(f, "%s ", get_txt(ob->super->name));
        else
            fprintf(f, "-- ");

        if (ob->gigaticks)
            fprintf(f, " (%lu%09lu)", ob->gigaticks, ob->ticks);
        else
            fprintf(f, " (%lu)", ob->ticks);
        fprintf(f, " %s\n",
                swapstrings[(O_PROG_SWAPPED(ob)?1:0) | (O_VAR_SWAPPED(ob)?2:0)]
        );
    }
    fclose(f);
    return MY_TRUE;
}

/***************************************************************************/

