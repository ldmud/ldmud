/*---------------------------------------------------------------------------
 * Gamedriver - Dump object statistics.
 *
 *---------------------------------------------------------------------------
 * Function to compute the memory usage of objects and dump their
 * statistics.
 *---------------------------------------------------------------------------
 */

#include "driver.h"

#include <stdio.h>

#define USES_SVALUE_STRLEN
#include "dumpstat.h"
#include "array.h"
#include "comm.h"
#include "exec.h"
#include "filestat.h"
#include "interpret.h"
#include "instrs.h"
#include "mapping.h"
#include "object.h"
#include "prolang.h"
#include "simulate.h"
#include "smalloc.h"

/*-------------------------------------------------------------------------*/

static mp_int svalue_size(struct svalue *); /* forward */

/* Auxiliary structure for counting a mapping */

struct svalue_size_locals
{
    mp_int total;       /* Return: total memory usage of all elements */
    int    num_values;  /* Passed: width of the mapping */
};

/*-------------------------------------------------------------------------*/
static void
svalue_size_map_filter (struct svalue *key, struct svalue *values, void *extra)

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
        locals->total += svalue_size(values++) + sizeof(struct svalue);
    }
}

/*-------------------------------------------------------------------------*/
static mp_int
svalue_size (struct svalue *v)

/* Compute the memory usage of *<v>, calling svalue_size() recursively
 * if necessary, and return it. The size of *v itself is not included.
 */

{
    mp_int i, total;

    switch(v->type)
    {
    case T_OBJECT:
    case T_NUMBER:
    case T_FLOAT:
        return 0;

    case T_STRING:
        /* Include some malloc overhead. */
        if (v->x.string_type == STRING_SHARED)
            return (strlen(v->u.string) + sizeof(short) + 3*sizeof(char*)) &
              (~(sizeof(char *) - 1));
        return (svalue_strlen(v) + 2*sizeof(char *)) & (~(sizeof(char *) - 1));

    case T_SYMBOL:
        return (strlen(v->u.string) + sizeof(short) + 3*sizeof(char*)) &
          (~(sizeof(char *) - 1));

    case T_MAPPING:
    {
        struct svalue_size_locals locals;
        struct condensed_mapping *cm;

        if (register_pointer( (char *)(v->u.map) ) ) return 0;

        cm = v->u.map->condensed;
        locals.total = cm->string_size + cm->misc_size;
        locals.num_values = v->u.map->num_values;
        walk_mapping(v->u.map, svalue_size_map_filter, &locals);
        if (v->u.map->hash)
            locals.total +=
              sizeof(struct hash_mapping) +
                v->u.map->hash->mask * sizeof(struct map_chain *) +
                  v->u.map->hash->used *
                    (sizeof (struct map_chain) - sizeof(struct svalue));
        return locals.total;
    }

    case T_POINTER:
    case T_QUOTED_ARRAY:
    {
        if (v->u.vec == &null_vector) return 0;
        if (register_pointer( (char *)(v->u.vec) ) ) return 0;
#ifdef MALLOC_smalloc
        total = malloced_size(v->u.vec) * sizeof(p_int);
#else
        total = sizeof *v->u.vec - sizeof v->u.vec->item +
          sizeof(struct svalue) * v->u.vec->size + sizeof(char *);
#endif
        for (i=0; i < (mp_int)VEC_SIZE(v->u.vec); i++) {
            total += svalue_size(&v->u.vec->item[i]);
        }
        return total;
    }

    case T_CLOSURE:
    {
        int num_values;
        struct svalue *svp;
        struct lambda *l;

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
            num_values = ((struct svalue *)l)[-0xff].u.number;
        svp = (struct svalue *)l - num_values;
        if (register_pointer( (char *)svp ) ) return 0;
#ifdef MALLOC_smalloc
        total += malloced_size(svp) * sizeof(p_int);
#else
        total += sizeof(struct svalue) * num_values + sizeof (char *);
        {
            char *p = &l->function.code[2];
            do {
                switch(*++p) {
                  case F_RETURN -F_OFFSET:
                  case F_RETURN0-F_OFFSET:
                    break;
                  default:
                    continue;
                }
                break;
            } while (1);
            total += p - (char *)l + (sizeof(char *) - 1) &
                ~(sizeof(char *) - 1);
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
data_size (struct object *ob)

/* Compute the memory usage of the data held by object <ob> and
 * return it. If the object is swapped out or has no variables,
 * return 0.
 */

{
    mp_int total = sizeof(p_int); /* smalloc overhead */
    int i;
    struct pointer_record *pointer_table_space[PTABLE_SIZE];
    struct svalue *svp;

    if (ob->flags & O_SWAPPED || !(i = ob->prog->num_variables) )
        return 0;
    init_pointer_table(pointer_table_space);
    for (svp = ob->variables; --i >= 0; svp++)
        total += svalue_size(svp) + sizeof (struct svalue);
    free_pointer_table();
    return total;
}

/*-------------------------------------------------------------------------*/
void
dumpstat(void)

/* Called by the command parser, this function dumps statistics about
 * all objects into the file $MUDLIB/OBJ_DUMP.
 */

{
    FILE *f;
    struct object *ob;

    static char *swapstrings[] =
        {"", "PROG SWAPPED", "VAR SWAPPED", "SWAPPED", };
    f = fopen("OBJ_DUMP", "w");
    if (f == 0)
        return;
    FCOUNT_WRITE("OBJ_DUMP");
    add_message("Dumping to OBJ_DUMP ...");
    for (ob = obj_list; ob; ob = ob->next_all)
    {
        mp_int tmp;
        if (ob->flags & O_DESTRUCTED)
            continue;
        if (!O_PROG_SWAPPED(ob)
         && (ob->prog->ref == 1 || !(ob->flags & O_CLONE)))
        {
            tmp = ob->prog->total_size;
        }
        else
        {
            tmp = 0;
        }
        fprintf(f, "%-20s %5ld ref %2ld %s %s ", ob->name,
                tmp + (long)data_size(ob) + sizeof (struct object) +
                sizeof(p_int) /* smalloc overhead */ ,
                ob->ref,
                ob->flags & O_HEART_BEAT ? "HB" : "  ",
                ob->super ? ob->super->name : "--"
        );
        if (ob->gigaticks)
            fprintf(f, " (%lu%09lu)", ob->gigaticks, ob->ticks);
        else
            fprintf(f, " (%lu)", ob->ticks);
        fprintf(f, " %s\n",
                swapstrings[(O_PROG_SWAPPED(ob)?1:0) | (O_VAR_SWAPPED(ob)?2:0)]
        );
    }
    add_message("done.\n");
    fclose(f);
}

/***************************************************************************/

