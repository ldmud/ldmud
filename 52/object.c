/*---------------------------------------------------------------------------
 * Mapping handling functions.
 *
 *---------------------------------------------------------------------------
 * Mappings, or 'associative arrays', are similar to normal arrays, with
 * the principal difference that they can use every value to index their
 * stored data, whereas arrays only index with integer values. On the
 * other hand this means that in mappings the data is not stored in any
 * particular order, whereas arrays imply an order through their indexing.
 *
 *---------------------------------------------------------------------------
 */

#include "driver.h"

#include "my-alloca.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <fcntl.h>

#define NO_INCREMENT_STRING_REF
#include "object.h"

#include "array.h"
#include "backend.h"
#include "closure.h"
#include "comm.h"
#include "exec.h"
#include "filestat.h"
#include "interpret.h"
#include "main.h"
#include "mapping.h"
#include "otable.h"
#include "prolang.h"
#include "random.h"
#include "sent.h"
#include "simulate.h"
#include "stralloc.h"
#include "swap.h"
#include "wiz_list.h"

extern pid_t getpid();  /* TODO: Where is this defined? */

/*-------------------------------------------------------------------------*/

struct replace_ob *obj_list_replace = NULL;
  /* List of scheduled program replacements.
   */

int tot_alloc_object = 0;
int tot_alloc_object_size = 0;
  /* Total number of allocated object, and the sum of memory they use.
   */

struct object NULL_object = { 0 };
  /* static null object for initialisations. memset() is not sufficient
   * because some machines (e.g. Bull) have a (char*)0 which is not
   * binary zero. Structure assignment otoh works.
   */

/*-------------------------------------------------------------------------*/
/* Pointer_table structures and variables
 */

/* Structure representing one mapping/array
 */

struct pointer_record
{
    mp_int key;      /* The pointer registered with this entry */
    struct pointer_record *next;      /* Next entry in hash chain */
    struct pointer_record *next_all;  /* Next entry in global list */
    long id_number;  /* ID for the registered pointer */
    long ref_count;  /* Number of registrations of this pointer */
};

/* One sub table.
 */

struct sub_table
{
    struct pointer_record *records[PTABLE_SIZE];
      /* The table of hash chains */
    char used[PTABLE_SIZE / CHARBITS];
      /* Bitvector denoting which record[] entries are valid */
    struct sub_table *next_all;  /* Next subtable in global list */
};

static struct pointer_record **pointer_table;
  /* The top-level hashtable, in fact an array with 256 entries.
   * It is provided by the user of the pointer_table.
   */

static char hash_usage[ PTABLE_SIZE * 2 / CHARBITS ];
  /* Bit vector describing the state of every entry in pointer_table[],
   * two bits each.
   *
   * The state of entry table[hash] is described
   * in hash_usage[(hash/8 ) * 2] and hash_usage[(hash/8) * 2 + 1],
   * bit hash%8 each. The first bit is true if the associated entry
   * is in use, the second if the entry holds a sub table.
   */

static struct pointer_record *all_pointer_records;
static struct sub_table *all_sub_tables;
  /* List of all allocated records and sub tables, used for final
   * deallocation.
   * TODO: Pooling would be nice here.
   */

/*-------------------------------------------------------------------------*/

#ifdef DEBUG
void
_free_object (struct object *ob, char *from)

#else /* DEBUG */

int
_free_object (struct object *ob)

#endif /* DEBUG */

/* Deallocate/dereference all memory and structures held by <ob>.
 * At the time of call, the object must be destructed and removed
 * from the object table.
 *
 * !DEBUG: the function is called from the macro free_object()
 *         only after ob->ref has already been decremented and
 *         found to be 0.
 *         The result is always 0.
 *
 * DEBUG: <from> gives the location of the free_object() call.
 */

{

#ifdef DEBUG

    /* Decrement and check the reference count */

    ob->ref--;
    if (d_flag > 1)
        printf("Subtr ref to ob %s: %ld (%s)\n", ob->name
              , ob->ref, from);
    if (ob->ref > 0)
        return;
    if (d_flag)
        printf("free_object: %s.\n", ob->name);

    /* Freeing a non-destruct object should never happen */
    
    if (!(ob->flags & O_DESTRUCTED)) {
        fatal("Object 0x%lx %s ref count 0, but not destructed (from %s).\n"
             , (long)ob, ob->name, from);
    }

#endif /* DEBUG */

    if (ob->sent)
        fatal("Tried to free an object with sentences.\n");

    /* If the program is freed, then we can also free the variable
     * declarations.
     */
    if (ob->prog)
    {
        struct program *prog = ob->prog;
        tot_alloc_object_size -=
            prog->num_variables * sizeof (struct svalue) +
                sizeof (struct object) - sizeof (struct svalue);
        free_prog(prog, 1);
        ob->prog = NULL;
    }

    /* Deallocate the name */
    if (ob->name)
    {
        if (d_flag > 1)
            debug_message("Free object %s\n", ob->name);
        if (lookup_object_hash(ob->name) == ob)
            fatal("Freeing object %s but name still in name table\n", ob->name);
        xfree(ob->name);
        ob->name = NULL;
    }

    /* Dereference the load_name */
    if (ob->load_name)
    {
        free_string(ob->load_name);
        ob->load_name = NULL;
    }

    /* Free the object structure */
    tot_alloc_object--;
    xfree((char *)ob);

#ifndef DEBUG
    return 0;
#endif

} /* _free_object() */

/*-------------------------------------------------------------------------*/
#ifndef add_ref  /* implies DEBUG */

void
add_ref (struct object *ob, char *from)

/* Increment the refcount of object <ob>, with the function called from
 * <from>
 *
 * !DEBUG: This function is implemented as a macro.
 */

{
    ob->ref++;
    if (d_flag > 1)
        printf("Add reference to object %s: %ld (%s)\n", ob->name,
               ob->ref, from);
}

#endif

/*-------------------------------------------------------------------------*/
#ifdef INITIALIZATION_BY___INIT

struct object *
get_empty_object (int num_var)

#else

struct object *get_empty_object ( int num_var
                                , struct variable *variables
                                , struct svalue *initializers)

#endif

/* Allocate a new, empty object with <numvar> variables and return it.
 * Return NULL when out of memory.
 *
 * !__INIT: every variable, which is flagged in <variables> as
 *          NAME_INITIALIZED, is set to the corresponding value
 *          in <initializers>. Both <variables> and <initializers>
 *          are arrays of size <num_var>.
 *
 * !__INIT: All variables are set to 0.
 */

{
    struct object *ob;
    int size = sizeof (struct object);
    int size2 = num_var * sizeof (struct svalue);
    int i;
    struct svalue *ob_vars;

    /* Allocate the object structure */

    if ( !(ob = (struct object *)xalloc(size)) )
        return NULL;

    ob_vars = NULL;

    /* Allocated the variable block */

    if (size2 && !(ob_vars = (struct svalue *)xalloc(size2)) )
    {
        xfree((char *)ob);
        return NULL;
    }

    tot_alloc_object++;
    tot_alloc_object_size += size + size2;

    /* Clear and initialise the object (no memset!) */
    
    *ob = NULL_object; 
    ob->ref = 1;
#ifdef DEBUG
    ob->extra_num_variables = num_var;
#endif
    ob->variables = ob_vars;

    /* Initialize the variables */

    for (i = num_var; --i >= 0; )
    {
#ifndef INITIALIZATION_BY___INIT
        if (variables[i].flags & NAME_INITIALIZED)
        {
            assign_svalue_no_free(&ob_vars[i], &initializers[i]);
        } else
#endif
            ob_vars[i] = const0;
    }

    /* That's it. */
    
    return ob;
}  /* get_empty_object() */

/*-------------------------------------------------------------------------*/
#ifdef DEALLOCATE_MEMORY_AT_SHUTDOWN

void
remove_all_objects (void)

/* Call destruct_object() for every object on the object list, then
 * call remove_destructed_objects() to actually remove them.
 *
 * This function is called from simulate.c when the game is shut down.
 */

{
    struct object *ob;
    struct svalue v;

    v.type = T_OBJECT;
    for (ob = obj_list; ob; ob = ob->next_all)
    {
        if (ob->flags & O_DESTRUCTED)
            continue;
        v.u.ob = ob;
        destruct_object(&v);
        if ( !(ob->flags & O_DESTRUCTED) )
            break;
    }
    remove_destructed_objects();
}

#endif

/*-------------------------------------------------------------------------*/
void
reference_prog (struct program *progp, char *from)

/* Increment the refcount of program <progp>, called from location <from>.
 */

{
    progp->ref++;
    if (d_flag)
        printf("reference_prog: %s ref %ld (%s)\n",
            progp->name, progp->ref, from);
}

/*-------------------------------------------------------------------------*/
void
do_free_sub_strings (int num_strings,   char **strings
                    ,int num_variables, struct variable *variable_names
                    )

/* Free a bunch of shared strings used in connection with an object:
 * the <num_strings> strings in the array <strings>, and the
 * the <num_variables> names of the vars in array <variable_names>.
 *
 * The function is called from free_prog() and from the compiler epilog().
 */

{
    int i;

    /* Free all strings */
    for (i = 0; i < num_strings; i++)
        free_string(strings[i]);

    /* Free all variable names */
    for (i = num_variables; --i >= 0; )
    {
        free_string(variable_names[i].name);
    }
}

/*-------------------------------------------------------------------------*/
void
free_prog (struct program *progp, short /* TODO: BOOL */ free_sub_strings)

/* Decrement the refcount for program <progp>. If it reaches 0, the program
 * is freed.
 *
 * If free_sub_strings is TRUE, all object strings are freed, and
 * free_prog() is called for all inherited programs.
 *
 * The only case when free_sub_strings is not true, is, when the swapper
 * swapped out the program and now attempts to free the memory.
 * This means that the strings are kept in memory all the time.
 * TODO: Swap the strings?
 */

{
    /* Decrement the refcount */

    progp->ref--;
    if (progp->ref > 0)
        return;

    if (d_flag)
        printf("free_prog: %s\n", progp->name);
    if (progp->ref < 0)
        fatal("Negative ref count for prog ref.\n");

    /* Update the statistics */
    total_prog_block_size -= progp->total_size;
    total_num_prog_blocks -= 1;

    /* Free the line numbers.
     *
     * This has to be done before the program is removed from the
     * swapper, else the following test would fail.
     */
    if (progp->swap_num != -1 && progp->line_numbers)
    {
        xfree(progp->line_numbers);
    }

    /* Is it a 'real' free? Then dereference all the
     * things held by the program, too.
     */
    if (free_sub_strings)
    {
        int i;
        unsigned char *program;
        uint32 *functions;

        /* Remove the swap entry */
        if (progp->swap_num != -1)
            remove_swap_file(progp);

        program = progp->program;
        functions = progp->functions;

        /* Free all function names. */
        for (i = progp->num_functions; --i >= 0; )
        {
            if ( !(functions[i] & NAME_INHERITED) )
            {
                char *name;

                /* TODO: the function header needs a struct */
                memcpy(
                  (char *)&name,
                  program + (functions[i] & FUNSTART_MASK) - 1 - sizeof name,
                  sizeof name
                );
                free_string(name);
            }
        }

        /* Free the strings and variable names */
        do_free_sub_strings( progp->num_strings, progp->strings
                           , progp->num_variables, progp->variable_names
                           );

        /* Free all inherited objects */
        for (i=0; i < progp->num_inherited; i++)
            free_prog(progp->inherit[i].prog, MY_TRUE);

        /* Free the program name */
        xfree(progp->name);
    }

    /* Remove the program structure */
    xfree((char *)progp);
}

/*-------------------------------------------------------------------------*/
void
reset_object (struct object *ob, int arg)

/* Depending on <arg>, call one of the initialisation functions in <ob>.
 * The actual function is given in <arg> through its hook index.
 * Accepted values are: H_RESET, H_CREATE_SUPER, H_CREATE_OB,
 *   H_CREATE_CLONE.
 *
 * The value of the hooks can be function names (strings) or closures.
 *
 * For strings, the name is the function called in <ob>. It gets passed
 * one argument: 0 for H_CREATE_*, 1 for H_RESET. If on a H_RESET call
 * the function can not be found, the object will never be reset again.
 *
 * For closures, the code distinguishes closures which take no arguments
 * (only for H_CREATE_* calls) from those which take at least one argument.
 * In the former case, the closure is bound to <ob> and called; in the
 * latter case, the closure is bound to the current object and gets <ob>
 * passed as argument. If the closure returns a numeric result, it is
 * used as the time delay before the next reset.
 *
 * If the delay to the next (resp. first) reset is not determined by
 * the called function, it is set to a random value between TIME_TO_RESET/2
 * and TIME_TO_RESET.
 */

{
    /* Be sure to update time first ! */
    ob->next_reset = current_time + TIME_TO_RESET/2
                                  + random_number(TIME_TO_RESET/2);

#ifdef INITIALIZATION_BY___INIT

    /* Initialize the variables first in case of a H_CREATE_* call.
     */
    if (arg != H_RESET)
    {
        sapply(STR_VARINIT, ob, 0);
        if (ob->flags & O_DESTRUCTED)
            return;
    }
#endif

    if (closure_hook[arg].type == T_CLOSURE)
    {
        struct lambda *l;

        l = closure_hook[arg].u.lambda;
        if (l->function.code[1] && arg != H_RESET)
        {
            /* closure accepts arguments, presumably one, so
             * give it the target object and bind to the current
             * object.
             */
            l->ob = current_object;
            push_object(ob);
            call_lambda(&closure_hook[arg], 1);
        }
        else
        {
            /* no arguments, just bind to target */
            l->ob = ob;
            call_lambda(&closure_hook[arg], 0);
        }

        /* If the call returned a number, use it as the current
         * reset interval
         */
        if (inter_sp->type == T_NUMBER && inter_sp->u.number)
            ob->next_reset = current_time + inter_sp->u.number;

        pop_stack();
    }
    else if (closure_hook[arg].type == T_STRING)
    {
        push_number(arg == H_RESET);
        if (!sapply(closure_hook[arg].u.string, ob, 1) && arg == H_RESET)
            ob->next_reset = MAXINT;
    }

    /* Object is reset now */
    ob->flags |= O_RESET_STATE;
} /* reset_object() */

/*-------------------------------------------------------------------------*/
void
replace_programs (void)

/* Called from backend::remove_destructed_objects(), this function
 * performs all pending program replacements listed in obj_list_replace.
 *
 * If the function runs out of memory, the processing ends at that point
 * and will be retried in the next call.
 *
 * Sideeffects of this action are: the objects are marked as clones,
 * and current shadows are removed.
 */

{
    struct replace_ob *r_ob, *r_next;  /* List pointers */
    struct svalue *svp;
    int i, j;

#ifdef DEBUG
    if (d_flag)
        debug_message("start of replace_programs\n");
#endif

    for (r_ob = obj_list_replace; r_ob; r_ob = r_next)
    {
        struct program *old_prog;

        /* Swap in the program. This can't fail when called during
         * a garbage collection because then the malloc privilege
         * is MALLOC_SYSTEM.
         */
        if (r_ob->ob->flags & O_SWAPPED && load_ob_from_swap(r_ob->ob) < 0)
        {
            obj_list_replace = r_ob;
            return; /* Hope for more memory next time... */
        }

        /* If the number of variables changes, allocate a new variables
         * block and copy the old values over as far as possible.
         * Note that the change can only be a reduction.
         */
        i = r_ob->ob->prog->num_variables - r_ob->new_prog->num_variables;
        if (i)
        {
            struct svalue *new_vars;

            /* Get the memory */
            new_vars = xalloc(  r_ob->new_prog->num_variables
                              * sizeof *new_vars);

            if (!new_vars)
            {
                obj_list_replace = r_ob;
                return; /* Hope for more memory next time... */
            }
#ifdef DEBUG
            if (d_flag)
                debug_message("%d less variables\n",i);
            r_ob->ob->extra_num_variables = r_ob->new_prog->num_variables;
#endif

            /* Adjust the statistics */
            tot_alloc_object_size -= i * sizeof(struct svalue[1]);

            svp = r_ob->ob->variables; /* the old variables */

            /* Deref those variables of ob which won't be copied */

            j = r_ob->var_offset;      /* number of unique vars of ob */
            i -= j;

#ifdef DEBUG
            if (d_flag)
                debug_message("freeing %d variables:\n",j);
#endif
            while (--j >= 0) free_svalue(svp++);
#ifdef DEBUG
            if (d_flag)
                debug_message("freed.\n");
#endif

            /* Copy the others */
            j = r_ob->new_prog->num_variables;
            memcpy(
                (char *)new_vars,
                (char *)svp,
                j * sizeof(struct svalue[1])
            );
            svp += j;
#ifdef DEBUG
            if (d_flag)
                debug_message("freeing %d variables:\n",i);
#endif

            /* Deref the remaining non-copied variables */
            while (--i >= 0) free_svalue(svp++);
#ifdef DEBUG
            if (d_flag)
                debug_message("freed.\n");
#endif

            /* Free the old variable block and set the new one */
            xfree(r_ob->ob->variables);
            r_ob->ob->variables = new_vars;
        } /* if (change in vars) */

        /* Replace the old program with the new one */
        old_prog = r_ob->ob->prog;
        r_ob->new_prog->ref++;
        r_ob->ob->prog = r_ob->new_prog;
        r_ob->ob->flags |= O_CLONE;

        r_next = r_ob->next;  /* remove it from the list */

        /* Handle a possible lambda adjustment */
        if (r_ob->lambda_rpp)
        {
            obj_list_replace = r_next;
            replace_program_lambda_adjust(r_ob);
        }

        /* Free the old program, finally */
        free_prog(old_prog, MY_TRUE);

#ifdef DEBUG
        if (d_flag)
            debug_message("program freed.\n");
#endif

        /* Remove current shadows */

        if (r_ob->ob->flags & O_SHADOW)
        {
            struct shadow_sentence *shadow_sent;

            if ((shadow_sent = O_GET_SHADOW(r_ob->ob))->shadowing)
            {
                /* The master couldn't decide if it's a legal shadowing
                 * before the program was actually replaced. It is possible
                 * that the blueprint to the replacing program is already
                 * destructed, and it's source changed.
                 * On the other hand, if we called the master now, all kind
                 * of volatile data structures could result, even new entries
                 * for obj_list_replace. This would eventually require to
                 * reference it, and all the lrpp's , in check_a_lot_ref_counts()
                 * and garbage_collection() . Being able to use replace_program()
                 * in shadows is hardly worth this effort. Thus, we simply
                 * stop the shadowing.
                 */
                O_GET_SHADOW(shadow_sent->shadowing)->shadowed_by =
                  shadow_sent->shadowed_by;
                if (shadow_sent->shadowed_by)
                {
                    O_GET_SHADOW(shadow_sent->shadowed_by)->shadowing =
                      shadow_sent->shadowing;
                    shadow_sent->shadowed_by = NULL;
                }
                shadow_sent->shadowing = NULL;
            }
        }
        xfree((char *)r_ob);
    }

    /* Done with the list */
    obj_list_replace = NULL;

#ifdef DEBUG
    if (d_flag)
        debug_message("end of replace_programs\n");
#endif

}  /* replace_programs() */

/*-------------------------------------------------------------------------*/
void
tell_npc (struct object *ob, char *str)

/* Call the lfun 'catch_tell()' in object <ob> with <str> as argument.
 *
 * This function is used to talk to non-i nteractive commandgivers
 * (aka NPCs).
 */

{
    push_volatile_string(str);
    (void)sapply(STR_CATCH_TELL, ob, 1);
}

/*-------------------------------------------------------------------------*/
void
tell_object (struct object *ob, char *str)

/* Send message <str> to object <ob>. If <ob> is an interactive player,
 * it will go to his screen (unless a shadow catches it - see shadow_catch_
 * message() ). If <ob> is not interactive, the message will go
 * to the lfun 'catch_tell()' via a call to tell_npc().
 */

{
    struct object *save_command_giver;
    struct interactive *ip;

    if (ob->flags & O_DESTRUCTED)
        return;

    if (NULL != (ip = O_GET_INTERACTIVE(ob))
     && ip->sent.type == SENT_INTERACTIVE)
     {
        save_command_giver = command_giver;
        command_giver = ob;
        add_message("%s", str);
        command_giver = save_command_giver;
        return;
    }
    tell_npc(ob, str);
}

/*-------------------------------------------------------------------------*/
/* TODO: BOOL */ int
shadow_catch_message (struct object *ob, char *str)


/* Called by comm:add_message() when it is detected that the
 * interactive object <ob> is shadowed. This function checks all shadows
 * of <ob> if they contain the lfun catch_tell(), and calls the lfun
 * in the first shadow where it exists with message <str> as argument.
 * Result is true if there is such a function, and false if not. In
 * the latter case, the flag ob->ip.catch_tell_activ is cleared to
 * speed up later calls.
 *
 * The function returns immediately with false, if ob->ip.catch_tell_activ
 * is cleared, or if ob is the current_object.
 *
 * Beware that one of the shadows may be the originator of the message,
 * which means that we must not send the message to that shadow, or any
 * shadows in the linked list before that shadow.
 */
{
    struct interactive *ip;

    ip = O_GET_INTERACTIVE(ob);

    if (!ip->catch_tell_activ || ob == current_object)
        return MY_FALSE;

    trace_level |= ip->trace_level;
    push_volatile_string(str);
    if (sapply(STR_CATCH_TELL, ob, 1))
        return MY_TRUE;

    /* The call failed, thus, current_object wasn't changed
     * (e.g. destructed and set to 0 ) .
     * !current_object is true when a prompt is given.
     */
    if (!current_object
     || !(current_object->flags & O_SHADOW)
     || !O_GET_SHADOW(current_object)->shadowing)
    {
        ip->catch_tell_activ = MY_FALSE;
    }
    return MY_FALSE;
}

/*-------------------------------------------------------------------------*/
static void
clear_program_id (struct program *p)

/* Clear the id_number of program <p> and all inherited programs.
 */

{
    int i;

    if (!p->id_number)
        return;

    p->id_number = 0;
    for (i = 0; i< p->num_inherited; i++)
    {
        clear_program_id(p->inherit[i].prog);
    }
}

/*-------------------------------------------------------------------------*/
static void
renumber_program (struct program *p)

/* Renumber program <p> and all inherited programs.
 * Assumes that all id_numbers have been cleared before.
 */

{
    int i;

    if (p->id_number)
        return;
    p->id_number = ++current_id_number;
    for (i=0; i< p->num_inherited; i++) {
        renumber_program(p->inherit[i].prog);
    }
}

/*-------------------------------------------------------------------------*/
int32
renumber_programs (void)

/* Renumber all programs in the game, recycling number from old
 * objects. Return the first free new id_number and modifies
 * the global current_id_number.
 *
 * The function is called by swap.c and lang.c when current_id_number
 * overflows.
 */

{
    struct object *ob;

    current_id_number = 0;
    for (ob = obj_list; ob; ob = ob->next_all)
    {
        if (ob->flags & O_DESTRUCTED)
            continue;
        if ( !O_PROG_SWAPPED(ob) )
            clear_program_id(ob->prog);
    }

    for (ob = obj_list; ob; ob = ob->next_all)
    {
        if (ob->flags & O_DESTRUCTED)
            continue;
        if ( !O_PROG_SWAPPED(ob) )
            renumber_program(ob->prog);
    }
    invalidate_apply_low_cache();
    return ++current_id_number;
}

/*=========================================================================*/

/* Saving and restoring an object state involves a depth search through
 * all referenced mappings and arrays, and the detection of cylic
 * references.
 *
 * This is solved with a two-dimension hash structure, recording every
 * mapping/array pointer encountered. As the same functionality
 * is needed in some other places of the driver, too, the whole bunch
 * of functions are made public.
 *
 * The table is used to create a mirror of the array/mapping relations:
 * every mapping/array is represented by one record, recording an
 * ID number and how often the represented array/mapping was encountered.
 *
 * The entries of the top-level hashtable may be unused, contain plain
 * records or one of the second-level hashtables. The bit vector hash_usage
 * keeps track of which entry is what.
 *
 * 
 * TODO: Instead of using global variables, the code should take
 * TODO:: a 'cookie' approach, like stdio does.
 * TODO: And it needs to be more general.
 */

/*-------------------------------------------------------------------------*/
void
init_pointer_table (struct pointer_record **space)

/* Set up a new pointer-table run, using caller-provided <space>
 * as the top-level hashtable.
 */

{
    pointer_table = space;
    memset(hash_usage, 0, sizeof hash_usage);
    all_pointer_records = NULL;
    all_sub_tables = NULL;
}

/*-------------------------------------------------------------------------*/
/* TODO: BOOL */ int
register_pointer (void *pointer)

/* Register the <pointer> in the table. If it is already in there,
 * the number of registrations is incremented and the functions returns TRUE.
 * If the pointer is not in the table, a new entry is generated, the
 * ID number entry is cleared and the function returns FALSE.
 */

{
    mp_int key;     /* The <pointer> as a normal int */
    int hash;       /* hash computed from <key> aka <pointer> */
    int mask;       /* mask for <hash> in to <usage_p> */
    char *usage_p;  /* First usage vector byte for entry <hash> */
    struct pointer_record *old;      /* Record to add a new one after */
    struct pointer_record *new;      /* New record to add */
    struct pointer_record **insert;  /* Pointer to hashed table entry */

    key = (mp_int)pointer;

    /* Compute the hash value, and the index and mask for
     * the usage vector
     */

    /* TODO: This code assumes that a pointer is 32 Bits long */
    hash = key ^ key >> 16;
    hash ^= hash >> 8;
    hash &= (PTABLE_SIZE-1);
    mask = 1 << (hash & (CHARBITS-1));
    /* TODO: this statement assumes CHARBITS == 8 */
    usage_p = hash_usage + (hash >> 2 & ~1);

    insert = &pointer_table[hash];

    /* Search the pointer among the existing entries.
     *
     * The switch() allows an easy way out of the if() when
     * a new entry has to be generated.
     */

    old = NULL;
    if (usage_p[0] & mask) switch (0) { default:

        /* The place in the main hash table has been used before */

        if (usage_p[1] & mask)
        {
            /* This place is already used for a sub-table.
             * Continue the search in there.
             */
            struct sub_table *table;

            table = *(struct sub_table**)insert;

            hash = (key ^ key >> 16) & (PTABLE_SIZE-1);
            mask = 1 << (hash & (CHARBITS-1));
            /* TODO: this statement assumes CHARBITS == 8 */
            usage_p = &table->used[hash >> 3];

            insert = &table->records[hash];

            if ( !(usage_p[0] & mask) )
                /* need to insert in free place */
                break;

            old = *insert;
            do {
                if (old->key == key)
                {
                    old->ref_count++;
                    return MY_TRUE;
                }
            } while ( NULL != (old = old->next) );
            old = *insert;
            /* need to insert at top of sub hash chain */
            break;
        }
        else
        {
            /* The entry is occupied by a record */

            struct sub_table *table;
            int old_hash;

            old = *insert;
            if (old->key == key) {
                old->ref_count++;
                return MY_TRUE;
            }

            /* Hash collision: create a new sub-table. */

            usage_p[1] |= mask;

            table = (struct sub_table *)xalloc(sizeof *table);
            if (!table)
                error("Out of memory.\n");
            *insert = (struct pointer_record *)table;
            table->next_all = all_sub_tables;
            all_sub_tables = table;
            memset(table->used, 0, sizeof table->used);

            /* Put the old entry into the new subtable */

            /* TODO: This code yaddayadda... */
            old_hash = (old->key ^ old->key >> 16) & (PTABLE_SIZE-1);
            table->used[old_hash >> 3] |= 1 << (old_hash & (CHARBITS-1));
            table->records[old_hash] = old;

            /* Compute the position for the new entry */

            hash = (key ^ key >> 16) & (PTABLE_SIZE-1);
            if (hash != old_hash)
            {
                old = NULL;
            }
            insert = &table->records[hash];
            mask = 1 << (hash & (CHARBITS-1));
            usage_p = &table->used[hash >> 3];
        }
    }

    /* At this point, a new record has to be created */
    usage_p[0] |= mask;
    new = (struct pointer_record *)xalloc(sizeof *new);
    if (!new)
        error("Out of memory.\n");
    *insert = new;
    new->key = key;
    new->next = old;
    new->next_all = all_pointer_records;
    new->ref_count = 0;
    new->id_number = 0;
    all_pointer_records = new;

    return MY_FALSE;
} /* register_pointer() */

/*-------------------------------------------------------------------------*/
void
free_pointer_table (void)

/* Deallocate all memory allocated by the previous pointertable run.
 */

{
    struct pointer_record *record;
    struct sub_table *table;

    for (record = all_pointer_records; record;)
    {
        struct pointer_record *next = record->next_all;
        xfree((char *)record);
        record = next;
    }

    for (table = all_sub_tables; table;)
    {
        struct sub_table *next = table->next_all;
        xfree((char *)table);
        table = next;
    }
} /* free_pointer_table() */


/*=========================================================================*/

/*                        Save/Restore an Object                           */

/*-------------------------------------------------------------------------*/
/* The 'version' of each savefile is given in the first line as
 *   # <version>:<host>
 *
 * <version> is currently 0
 * <host> is 1 for Atari ST and Amiga, and 0 for everything else.
 *    The difference lies in the handling of float numbers (see datatypes.h).
 */

#define SAVE_OBJECT_VERSION '0'
#define CURRENT_VERSION 0
  /* Current version of new save files
   */

#ifdef FLOAT_FORMAT_0
#    define SAVE_OBJECT_HOST '0'
#    define CURRENT_HOST 0
#endif

#ifdef FLOAT_FORMAT_1
#    define SAVE_OBJECT_HOST '1'
#    define CURRENT_HOST 1
#endif


/*-------------------------------------------------------------------------*/
/* Forward Declarations */

static void save_svalue(struct svalue *, int);
static void save_array(struct vector *);
static int restore_size(char **str);
INLINE static int restore_array(struct svalue *, char **str);
static int restore_svalue(struct svalue *, char **, int);
static void register_array(struct vector *);

/*-------------------------------------------------------------------------*/

#define SAVE_OBJECT_BUFSIZE 4096
  /* Size of the read/write buffer.
   */

static const char save_file_suffix[] = ".o";
  /* The suffix of the save file, in an array for easier computations.
   * (sizeof() vs. strlen().
   */

#if defined(MSDOS_FS)
static int old_format;
#endif

static char number_buffer[36];
  /* Buffer to create numbers in - big enough for 32 Bit uints.
   */

static char *save_object_bufstart;
  /* Start of the write buffer.
   */

static char *buf_pnt;
  /* Current position in the write buffer.
   */

static int buf_left;
  /* Space left in the write buffer.
   */

static mp_int bytes_written;
  /* Number of bytes so far written to the file.
   */

static /* TODO: BOOL */ int failed;
  /* An IO error occured.
   */

static int save_object_descriptor;
  /* FD of the savefile.
   */

static mp_int current_sv_id_number;
  /* The highest ID number so far assigned to a shared value when
   * writing a savefile.
   */

static int restored_host = -1;

/*-------------------------------------------------------------------------*/
/* Macros */

#define MY_PUTC(ch) {\
    *buf_pnt++ = ch;\
    if (!--  buf_left) {\
        buf_pnt = write_buffer();\
        buf_left = SAVE_OBJECT_BUFSIZE;\
    }\
}
/* Put <ch> into the write buffer, flushing the buffer to
 * the file if necessary.
 */


/* The following three macros handle the write buffer access
 * through local variables to achieve a greater speed:
 */

#define L_PUTC_PROLOG register char *l_buf_pnt = buf_pnt;\
                      register int  l_buf_left = buf_left;
/* Declare and initialize the local variables.
 */

#define L_PUTC(ch) {\
    *l_buf_pnt++ = ch;\
    if (!--l_buf_left) {\
        l_buf_pnt = write_buffer();\
        l_buf_left = SAVE_OBJECT_BUFSIZE;\
    }\
}
/* Put <ch> into the write buffer, flushing the buffer to
 * the file if necessary.
 */

#define L_PUTC_EPILOG buf_pnt = l_buf_pnt; buf_left = l_buf_left;
/* Update the global buffer variables with the local values.
 */

/*-------------------------------------------------------------------------*/
static char*
write_buffer (void)

/* Write the current content of the write buffer to the savefile
 * and return a pointer to its start.
 *
 * On an error, set failed to TRUE.
 */

{
    char *start;

    start = save_object_bufstart;
    if (write( save_object_descriptor, start, SAVE_OBJECT_BUFSIZE )
      != SAVE_OBJECT_BUFSIZE )
        failed = MY_TRUE;
    bytes_written += SAVE_OBJECT_BUFSIZE;
    return start;
} /* write_buffer() */

/*-------------------------------------------------------------------------*/
static /* TODO: BOOL */ int
recall_pointer (void *pointer)

/* Lookup the (known to be registered) <pointer> in the pointertable and
 * check the number of registrations.
 *
 * If it was registered just once, just return FALSE.
 * If it was registered several times (ie. it is a shared array/mapping),
 * write its ID number (which is assigned if necessary) as '<id>'
 * to the write buffer. If this is not the first time this particular
 * pointer was recalled, add a '=' and return FALSE, else return TRUE.
 *
 * If the function returns FALSE, the caller has to write the actual
 * data of the array/mapping.
 */

{
    mp_int key;
    int hash;
    struct pointer_record *record;

    key = (long)pointer;
    /* We know for sure that we will find the key, because it has been
     * registered before.
     * TODO: This code makes assumptions about type sizes.
     */
    hash = key ^ key >> 16;
    hash ^= hash >> 8;
    hash &= (PTABLE_SIZE-1);
    record = pointer_table[hash];
    if ( hash_usage[1 + (hash >> 2 & ~1)] & 1 << (hash & (CHARBITS-1)) ) {
        hash = (key ^ key >> 16) & (PTABLE_SIZE-1);
        record = ((struct sub_table *)record)->records[hash];
        while (record->key != key)
            record = record->next;
    }

    if (!record->ref_count)
        /* Used only once. No need for special treatment. */
        return MY_FALSE;

    if (pointer == (char*)&null_vector)
        /* Sharing enforced by the game driver */
        return MY_FALSE;

    /* Write the '<id>' text */

    {
        long old_id, id;
        char *source, c;
        L_PUTC_PROLOG

        /* If this pointer was recalled the first time, assign
         * an ID number.
         */
        if ( !(old_id = id = record->id_number) )
        {
            id = ++current_sv_id_number;
            record->id_number = id;
        }

        /* Write '<id>' */

        L_PUTC('<')
        source = number_buffer;
        (void)sprintf(source, "%ld", id);
        c = *source++;
        do L_PUTC(c) while ( '\0' != (c = *source++) );
        L_PUTC('>')

        if (old_id)
        {
            /* has been written before */
            L_PUTC_EPILOG
            return MY_TRUE;
        }

        /* First encounter: add a '=' */
        L_PUTC('=')
        L_PUTC_EPILOG
        return MY_FALSE;
    }

    /* NOTREACHED */
}

/*-------------------------------------------------------------------------*/
static void
save_string (char *src)

/* Write string <src> to the write buffer, but escape all funny
 * characters.
 */

{
    register char c;
    L_PUTC_PROLOG

    L_PUTC('\"')
    while ( '\0' != (c = *src++) )
    {
        if (isescaped(c))
        {
            switch(c) {
            case '\007': c = 'a'; break;
            case '\b'  : c = 'b'; break;
            case '\t'  : c = 't'; break;
            case '\n'  : c = 'n'; break;
            case '\013': c = 'v'; break;
            case '\014': c = 'f'; break;
            case '\r'  : c = 'r'; break;
            }
            L_PUTC('\\')
        }
        L_PUTC(c)
    }
    L_PUTC('\"')
    L_PUTC_EPILOG
}

/* ===                    Handling of Mappings                         === */

/*-------------------------------------------------------------------------*/
static void
save_mapping_filter (struct svalue *key, struct svalue *data, char *extra)

/* Filter used by save_mapping: write <key> and (p_int)<extra> values
 * in <data>[] to the write buffer.
 */

{
    int i;

    i = (p_int)extra;
    save_svalue(key, i ? ':' : ',' );
    while (--i >= 0)
        save_svalue(data++, i ? ';' : ',' );
}

/*-------------------------------------------------------------------------*/
static void
save_mapping (struct mapping *m)

/* Write the mapping <m> to the write buffer.
 * Empty mappings with width != 1 are written as '([:<width>])'.
 */

{
    mp_int old_written;

    /* If it is shared, write its ID, and maybe we're already
     * done then.
     */
    if ( recall_pointer( (char *)m ) )
        return;

    /* Nope, write it */

    MY_PUTC('(')
    MY_PUTC('[')
    check_map_for_destr(m);
    old_written = bytes_written - buf_left;
    walk_mapping(m, save_mapping_filter, (char *)(p_int)m->num_values);

    /* If the mapping is empty and has width other than 1,
     * use a special format
     */
    if (m->num_values != 1 && old_written == bytes_written - buf_left)
    {
        char *source, c;

        MY_PUTC(':')
        source = number_buffer;
        (void)sprintf(source, "%d", m->num_values);
        c = *source++;
        do MY_PUTC(c) while ( '\0' != (c = *source++) );
    }

    MY_PUTC(']')
    MY_PUTC(')')
} /* save_mapping() */

/*-------------------------------------------------------------------------*/
struct rms_parameters
{
    char *str;
    int num_values;
};

static int restore_map_size(parameters)
    struct rms_parameters *parameters;
{
    char *pt;
    int siz, tsiz;
    int num_values = -1, current_num_values = 0;

    pt = parameters->str;
    siz = 0;

    if (!pt) return -1;
    while (1) {
        switch (*pt) {
          case ']':
          {
            if (pt[1] != ')') return -1;
            parameters->str = &pt[2];
            parameters->num_values = siz ? num_values : 1;
            return siz;
          }
          case ':':
          {
            if (siz || current_num_values) return -1;
            pt++;
            num_values = atoi(pt);
            pt = strchr(pt,']');
            if (!pt || pt[1] != ')') return -1;
            parameters->str = &pt[2];
            parameters->num_values = num_values;
            return siz;
          }
          case '\"':
          {
            int backslashes;

            do {
                pt = strchr(&pt[1],'\"');
                if (!pt) return -1;
                /* the quote is escaped if and only
                 * if the number of slashes is odd. */
                for (backslashes = -1; pt[backslashes] == '\\'; backslashes--) ;
            } while ( !(backslashes & 1) ) ;
            pt++;
            break;
          }
          case '(':
          {
            parameters->str = pt + 2;
            if (pt[1] == '{')
                tsiz = restore_size(&parameters->str);
            else if (pt[1] == '[')
                tsiz = restore_map_size(parameters);
            else return -1;
            pt = parameters->str;
            if (tsiz < 0)
                return -1;
            break;
          }
          case '<':
          {
            pt = strchr(pt, '>');
            if (!pt) return -1;
            pt++;
            if (pt[0] == '=') {
                pt++;
                continue;
            }
            break;
          }
          case '-':
            pt++;
            if (!*pt)
                return -1;
            /* fall through */
          case '0': case '1': case '2': case '3': case '4':
          case '5': case '6': case '7': case '8': case '9':
            if (pt[1] == '.') {
                char *pt2;

                pt2 = strpbrk(pt, "=:;,");
                if (!pt2)
                    return -1;
                if (*pt2 != '=')
                    break;
                pt = strchr(pt2, ':');
                if (!pt) return -1;
                pt++;
            }
            /* fall through */
          default:
          {
            pt = strpbrk(pt, ":;,");
            if (!pt)
                return -1;
            break;
          }
        }
        switch (*pt) {
          case ':':
            if (current_num_values)
                return -1;
          case ';':
            current_num_values++;
            break;
          case ',':
            siz++;
            if (current_num_values != num_values) {
                if (num_values >= 0)
                return -1;
                num_values = current_num_values;
            }
            current_num_values = 0;
            break;
          default:
            return -1;
        }
        pt++;
    }
    return -1;
}

INLINE static int restore_mapping(svp, str)
    struct svalue *svp;
    char **str;
{
    struct mapping *z;
    struct svalue key, *data;
    int i;
    struct rms_parameters tmp_par;
    int siz;

    tmp_par.str = *str;
    siz = restore_map_size(&tmp_par);
    if (siz < 0) {
        *svp = const0;
        return 0;
    }
    z = allocate_mapping(siz, tmp_par.num_values);
    svp->type = T_MAPPING;
    svp->u.map = z;
    while (--siz >= 0) {
        i = tmp_par.num_values;
        key.type = T_NUMBER;
        if (!restore_svalue(&key, str, i ? ':' : ',' )) {
            free_svalue(&key);
            return 0;
        }
        data = get_map_lvalue(z, &key, MY_TRUE);
        free_svalue(&key);
        while (--i >= 0) {
            if (!restore_svalue(data++, str, i ? ';' : ',' )) return 0;
        }
    }
    *str = tmp_par.str;
    return 1;
}

static void register_mapping PROT((struct mapping *map));

static void register_mapping_filter(key, data, extra)
    struct svalue *key, *data;
    char *extra;
{
    int i;

    if (key->type == T_POINTER) {
        register_array  (key->u.vec);
    } else if (key->type == T_MAPPING) {
        register_mapping(key->u.map);
    }
    for (i = (p_int)extra; --i >= 0; data++) {
        if (data->type == T_POINTER) {
            register_array  (data->u.vec);
        } else if (data->type == T_MAPPING) {
            register_mapping(data->u.map);
        }
    }
}

static void register_mapping(map)
    struct mapping *map;
{
    if ( register_pointer( (char *)(map) ) ) return;
    walk_mapping(map, register_mapping_filter, (char *)(p_int)map->num_values);
}

/*
 * Encode an element into a contiguous string.
 */
static void save_svalue(v, delimiter)
    struct svalue *v;
    int delimiter;
{
    switch(v->type) {
        case T_STRING:
            save_string(v->u.string);
            break;
        case T_POINTER:
            save_array(v->u.vec);
            break;
        case T_NUMBER:
        {
            L_PUTC_PROLOG
            char *source, c;

            source = number_buffer;
            (void)sprintf(source, "%ld", v->u.number);
            c = *source++;
            do L_PUTC(c) while ( '\0' != (c = *source++) );
            L_PUTC(delimiter);
            L_PUTC_EPILOG
            return;
        }
        case T_FLOAT:
        {
            L_PUTC_PROLOG
            char *source, c;

            source = number_buffer;
            (void)sprintf(source, "%.12e=%x:%lx",
                READ_DOUBLE(v),
                v->x.exponent & 0xffff,
                v->u.mantissa);
            c = *source++;
            do L_PUTC(c) while ( '\0' != (c = *source++) );
            L_PUTC(delimiter);
            L_PUTC_EPILOG
            return;
        }
        case T_MAPPING:
            save_mapping(v->u.map);
            break;
        default:
        {
            L_PUTC_PROLOG
            L_PUTC('0');        /* Objects can't be saved. */
            L_PUTC(delimiter);
            L_PUTC_EPILOG
            return;
        }
    }
    MY_PUTC(delimiter);
}

/*
 * Encode an array of elements into a contiguous string.
 */
static void save_array(v)
    struct vector *v;
{
    int i;
    struct svalue *val;
    if ( recall_pointer( (char *)v ) )
        return;
    {
        L_PUTC_PROLOG
        L_PUTC('(')
        L_PUTC('{')
        L_PUTC_EPILOG
    }
    for (i = VEC_SIZE(v), val = v->item; --i >= 0; ) {
        save_svalue(val++, ',');
    }
    {
        L_PUTC_PROLOG
        L_PUTC('}')
        L_PUTC(')')
        L_PUTC_EPILOG
    }
}

static void register_array(vec)
    struct vector *vec;
{
    struct svalue *v;
    int i;

    if ( register_pointer( (char *)(vec) ) ) return;
    v = vec->item;
    for (i = VEC_SIZE(vec); --i >= 0; v++) {
        if (v->type == T_POINTER) {
            register_array  (v->u.vec);
        } else if (v->type == T_MAPPING) {
            register_mapping(v->u.map);
        }
    }
}

static struct svalue *shared_restored_values;
static long max_shared_restored, current_shared_restored;

/*
 * Save an object to a file.
 * The routine checks with the function "valid_write()" in /obj/master.c
 * to assertain that the write is legal.
 */
void save_object(ob, file)
    struct object *ob;
    char *file;
{
    static char save_object_header[] = {
        '#', SAVE_OBJECT_VERSION, ':', SAVE_OBJECT_HOST, '\n'
    };
    char *name, *tmp_name, save_buffer[SAVE_OBJECT_BUFSIZE];
    struct pointer_record *pointer_table_space[256];
    int len, i;
    int f;
    struct svalue *v;
    struct variable *names;

    if (ob->flags & O_DESTRUCTED)
        return;
    /* COMPAT_MODE stuff moved to master.c by amylaar
     * master.c will access current_object->prog->name instead of
     * current_prog->name , but then, this is probably better in COMPAT_MODE
     * anyway.
     */
    file = check_valid_path(file, ob, "save_object", 1);
    if (file == 0)
        error("Illegal use of save_object()\n");
    len = strlen(file);
    name = alloca(len + (sizeof save_file_suffix) +
                  len + (sizeof save_file_suffix) + 4);
    tmp_name = name + len + sizeof save_file_suffix;
    (void)strcpy(name, file);
#ifndef MSDOS_FS
    (void)strcpy(name+len, save_file_suffix);
#endif
    /*
     * Write the save-files to different directories, just in case
     * they are on different file systems.
     */
    sprintf(tmp_name, "%s.tmp", name);
#ifdef MSDOS_FS
    (void)strcpy(name+len, save_file_suffix);
#endif
    save_object_descriptor =
      f = ixopen3(tmp_name, O_CREAT|O_TRUNC|O_WRONLY|O_TEXT, 0640);
    if (f < 0) {
        error("Could not open %s for a save.\n", tmp_name);
    }
    FCOUNT_SAVE(tmp_name);

    /* identify arrays/mappings that are used more than once.*/
    init_pointer_table(pointer_table_space);
    v = ob->variables;
    names = ob->prog->variable_names;
    for (i = ob->prog->num_variables; --i >= 0; v++, names++) {
        if (names->flags & TYPE_MOD_STATIC)
            continue;
        if (v->type == T_POINTER) {
            register_array  (v->u.vec);
        } else if (v->type == T_MAPPING) {
            register_mapping(v->u.map);
        }
    }
    failed = 0;
    current_sv_id_number = 0;
    bytes_written = 0;
    save_object_bufstart = save_buffer;
    memcpy(save_buffer, save_object_header, sizeof(save_object_header));
    buf_left = SAVE_OBJECT_BUFSIZE - sizeof(save_object_header);
    buf_pnt = save_buffer + sizeof(save_object_header);
    v = ob->variables;
    names = ob->prog->variable_names;
    for (i = ob->prog->num_variables; --i >= 0; v++, names++) {
        if (names->flags & TYPE_MOD_STATIC)
            continue;
        {
            char *var_name, c;
            L_PUTC_PROLOG

            var_name = names->name;
            c = *var_name++;
            do {
                L_PUTC(c)
            } while ( '\0' != (c = *var_name++) );
            L_PUTC(' ')
            L_PUTC_EPILOG
        }
        save_svalue(v, '\n');
    }
    free_pointer_table();
    if (
      write(
        save_object_descriptor,
        save_object_bufstart,
        SAVE_OBJECT_BUFSIZE-buf_left
      ) !=  SAVE_OBJECT_BUFSIZE-buf_left ) failed = 1;
    if (failed) {
        (void)close(f);
        unlink(tmp_name);
        add_message("Failed to save to file. Disk could be full.\n");
        return;
    }
    (void)unlink(name);
#if !defined(MSDOS_FS) && !defined(AMIGA) && !defined(OS2) && !defined(__BEOS__)
    if (link(tmp_name, name) == -1)
#else
    (void) close(f);
    if (rename(tmp_name,name) < 0)
#endif
    {
        perror(name);
        printf("Failed to link %s to %s\n", tmp_name, name);
        add_message("Failed to save object !\n");
    }
#if !defined(MSDOS_FS) && !defined(AMIGA) && !defined(OS2) && !defined(__BEOS__)
    (void)close(f);
    unlink(tmp_name);
#endif
}

/*
 * Find the size of an array. Return -1 for failure.
 */
static int restore_size(str)
     char **str;
{
  char *pt,*pt2;
  int siz,tsiz;

  pt = *str;
  siz = 0;

  while ((pt) && (*pt)) {
    if (pt[0] == '}') {
      if (pt[1] != ')') return -1;
      *str = &pt[2];
      return siz;
    }
    if (pt[0] == '\"') {
      int backslashes;

      do {
        pt = strchr(&pt[1],'\"');
        if (!pt) return -1;
        /* the quote is escaped if and only if the number of slashes is odd. */
        for (backslashes = -1; pt[backslashes] == '\\'; backslashes--) ;
      } while ( !(backslashes & 1) ) ;
      if (pt[1] != ',') return -1;
      siz++;
      pt += 2;
    }
    else if (pt[0] == '(') {
      /* Lazy way of doing it, a bit inefficient */
      struct rms_parameters tmp_par;

      tmp_par.str = pt + 2;
      if (pt[1] == '{')
          tsiz = restore_size(&tmp_par.str);
      else if (pt[1] == '[')
          tsiz = restore_map_size(&tmp_par);
      else return -1;
      pt = tmp_par.str;
      if (tsiz < 0)
          return -1;
      pt++;
      siz++;
    }
    else if (pt[0] == '<') {
      pt = strchr(pt, '>');
      if (!pt) return -1;
      if (pt[1] == ',') {
        siz++;
        pt += 2;
      } else if (pt[1] == '=') {
        pt += 2;
      } else return -1;
    }
    else {
    pt2 = strchr(pt, ',');
      if (!pt2)
          return -1;
      siz++;
      pt = &pt2[1];
    }
  }
  return -1;
}

INLINE static int restore_array(svp, str)
    struct svalue *svp;
    char **str;
{
    struct vector *v;
    char *pt, *end;
    int siz;

    end = *str;
    siz = restore_size(&end);
    if (siz < 0) {
        *svp = const0;
        return 0;
    }
    v = allocate_array(siz);

    /* We need to do this now, so that the value can be used inside. */
    svp->type = T_POINTER;
    svp->u.vec = v;

    for ( svp = v->item; --siz >= 0; svp++) {
        if ( !restore_svalue(svp, str, ',') ) {
            return 0;
        }
    }
    pt = *str;
    if (*pt++ != '}' || *pt++ != ')' ) {
        return 0;
    }
    *str = pt;
    return 1;
}

static int restore_svalue(svp, pt, delimiter)
    struct svalue *svp;
    char **pt;
    int delimiter;
{
    char *cp;

    switch( *(cp = *pt) ) {
      case '\"':
      {
        char *source, *start, c;

        start = cp;
        source = cp+1;

        for(;;) {
            if ( !(c = *source++) ) {
                *svp = const0;
                return 0;
            }
#if 1 /* for compatiblity reasons */
#ifndef MSDOS_FS
            if (c == '\r')
#else
            if (c == 30 && old_format)
#endif
                c = '\n';
#endif
            if (c == '\\') {
                if ( !(c = *source++) ) {
                    *svp = const0;
                    return 0;        /* String ends with a \\ buggy probably */
                }
                switch(c) {
                    case 'a': c = '\007'; break;
                    case 'b': c = '\b'  ; break;
                    case 't': c = '\t'  ; break;
                    case 'n': c = '\n'  ; break;
                    case 'v': c = '\013'; break;
                    case 'f': c = '\014'; break;
                    case 'r': c = '\r'  ; break;
                }
            } else if (c == '\"') break;
            *cp++ = c;
        }
        *cp=0;
        *pt = source;
        svp->type = T_STRING;
        svp->x.string_type = STRING_SHARED;
        svp->u.string = make_shared_string(start);
        break;
      }
      case '(':
        *pt = cp+2;
        switch ( cp[1] ) {
          case '[':
          {
            if ( !restore_mapping(svp, pt) ) {
                return 0;
            }
            break;
          }
          case '{':
          {
            if ( !restore_array(svp, pt) ) {
                return 0;
            }
            break;
          }
          default:
            return 0;
        }
        break;
      case '-':
      case '0': case '1': case '2': case '3': case '4':
      case '5': case '6': case '7': case '8': case '9':
      {
        char c, *numstart = cp;
        int nega = 0;
        long l = 0;

        if (*cp == '-') {
            nega = 1;
            cp++;
        }
        while(lexdigit(c = *cp++)) l = (((l << 2) + l) << 1) + (c - '0');
        if (c != '.') {
            svp->type = T_NUMBER;
            svp->u.number = nega ? -l : l;
            *pt = cp;
            return c == delimiter;
        }
        svp->type = T_FLOAT;
        if ( NULL != (cp = strchr(cp, '=')) &&  restored_host == CURRENT_HOST) {
            int tmp;

            cp++;
            if (sscanf(cp, "%x:%lx", &tmp, &svp->u.mantissa) != 2)
                return 0;
            svp->x.exponent = tmp;
        } else {
            STORE_DOUBLE_USED
            double d;

            d = atof(cp = numstart);
            STORE_DOUBLE(svp, d);
        }
        cp = strchr(cp, delimiter);
        *pt = cp+1;
        return cp != 0;
      }
      case '<':
      {
        int id;

        id = atoi(cp+1);
        cp = strchr(cp, '>');
        if (!cp) {
            *svp = const0;
            return 0;
        }
        if (cp[1] == '=') {
            int res;

            *pt = cp+2;

            /* Shared values can be used even before they have been read in
             * completely.
             */
            if (id != ++current_shared_restored) {
                *svp = const0;
                return 0;
            }
            if (id > max_shared_restored) {
                max_shared_restored <<= 1;
                shared_restored_values = (struct svalue *)
                  rexalloc((char*)shared_restored_values,
                    sizeof(struct svalue)*max_shared_restored
                  );
            }
            id--;
            res = restore_svalue(&shared_restored_values[id], pt, delimiter);
            *svp = shared_restored_values[id];
            return res;
        }
        if (id <= 0 || id > current_shared_restored) {
            *svp = const0;
            return 0;
        }
        assign_svalue_no_free(svp, &shared_restored_values[id-1]);
        cp = strchr(cp, delimiter);
        *pt = cp+1;
        return cp != 0;
      }
      default:
        *svp = const0;
        return 0;
    }
    cp = *pt;
    if (*cp++ != delimiter) return 0;
    *pt = cp;
    return 1;
}

/* Needed to restore backslashes correctly... */
static int old_restore_string(v, str)
    struct svalue *v;
    char *str;
{
    char *cp, c;

    cp = ++str;
    if ( '\0' != (c = *cp++) ) {
        do {
#ifndef MSDOS_FS
            if (c == '\r')
#else
            if (c == 30)
#endif
                cp[-1] = '\n';
        } while ( '\0' != (c = *cp++) );
        if (cp[-2] == '\n' && cp[-3] == '\"') {
            cp[-3] = '\0';
            v->type = T_STRING;
            v->x.string_type = STRING_SHARED;
            v->u.string = make_shared_string(str);
            return 1;
        }
    }
    *v = const0;
    return 0;
}

int restore_object(ob, file)
    struct object *ob;
    char *file;
{
    char *name, *var, *buff, *space;
    int len;
    FILE *f;
    struct stat st;
    struct variable *rover = NULL;
    int var_rest, num_var;
#ifndef MSDOS_FS
    int old_format;
#endif
    struct discarded {
        struct svalue v;
        struct discarded *next;
    } * dp = 0;

    if (current_object != ob)
        fatal("Bad argument to restore_object()\n");
    if (ob->flags & O_DESTRUCTED)
        return 0;

    file = check_valid_path(file, ob, "restore_object", 0);
    if (file == 0)
        error("Illegal use of restore_object()\n");

    len = strlen(file);
    name = alloca(len + (sizeof save_file_suffix));
    (void)strcpy(name, file);
    if (name[len-2] == '.' && name[len-1] == 'c')
        len -= 2;
    (void)strcpy(name+len, save_file_suffix);
    f = fopen(name, "r");
    if (!f || fstat(fileno(f), &st) == -1) {
        if (f)
            (void)fclose(f);
        return 0;
    }
    if (st.st_size == 0) {
        (void)fclose(f);
        return 0;
    }
    FCOUNT_REST(name);

    buff = xalloc(st.st_size + 1);
    if (!buff) {
        error("Out of memory.\n");
        return 0; /* flow control hint */
    }
    shared_restored_values = (struct svalue *)
      xalloc(sizeof(struct svalue)*256);
    max_shared_restored = 256;
    current_shared_restored = 0;
    num_var = ob->prog->num_variables;
    var_rest = 0;
    old_format = 1;
    restored_host = -1;
    while(1) {
        struct svalue *v;
        char *pt;

        if (fgets(buff, st.st_size + 1, f) == 0)
            break;
        /* Remember that we have a newline at end of buff ! */
        space = strchr(buff, ' ');
        if (space == 0) {
            if (buff[0] == '#') {
                int i;
                int restored_version;

                i = sscanf(buff+1, "%d:%d", &restored_version, &restored_host);
                if (i <= 0 || (i == 2 && restored_version <= CURRENT_VERSION) ) {
                    old_format = 0;
                    continue;
                }
            }
            (void)fclose(f);
            if (dp) do free_svalue(&dp->v); while ( NULL != (dp=dp->next) );
            xfree((char*)shared_restored_values);
            xfree(buff);
            error("Illegal format when restoring %s.\n", name);
            return 0; /* flow control hint */
        }
        *space = 0;
        do {
            if ( NULL != (var = findstring(buff)) ) {
                do
                    rover++;
                while (--var_rest > 0 &&
                  (rover->name != var || rover->flags & TYPE_MOD_STATIC) );
                if (var_rest <= 0) {
                    rover = ob->prog->variable_names-1;
                    var_rest = num_var + 1;
                    do
                        rover++;
                    while (--var_rest > 0 &&
                      (rover->name != var || rover->flags & TYPE_MOD_STATIC) );
                    if (var_rest > 0)
                        continue;
                } else
                    continue;
            } {
                struct discarded *tmp;

                tmp = dp;
                dp = (struct discarded *)alloca(sizeof(struct discarded));
                dp->next = tmp;
                v = &dp->v;
                v->type = T_NUMBER;
                break;
            }
        } while ((v = &ob->variables[num_var-var_rest]),MY_FALSE);
        free_svalue(v);
        pt = space+1;
        if ( old_format && pt[0] == '\"' ?
             !old_restore_string(v, pt)   :
             !restore_svalue(v, &pt, '\n')        )
        {
            (void)fclose(f);
            if (dp) do free_svalue(&dp->v); while ( NULL != (dp=dp->next) );
            xfree((char*)shared_restored_values);
            xfree(buff);
            error("Illegal format when restoring %s.\n", name);
            return 0;
        }
    }
    if (dp) do free_svalue(&dp->v); while ( NULL != (dp=dp->next) );
    if (d_flag > 1)
        debug_message("Object %s restored from %s.\n", ob->name, name);
    (void)fclose(f);
    xfree((char*)shared_restored_values);
    xfree(buff);
    return 1;
}

/***************************************************************************/

