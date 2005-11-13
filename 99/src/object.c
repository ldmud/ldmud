/*---------------------------------------------------------------------------
 * Object handling.
 *
 *---------------------------------------------------------------------------
 * Objects are the core of LPMud: everything a players sees, handles and
 * is, is an object. Unfortunately this also means that a lot of object
 * related functions are scattered all over the driver, so this file is
 * actually quite short.
 *
 * The first part deals with the creation and deallocation of objects
 * and programs. The second parts implements a pointer table which can
 * be used to sequentialize the set of arrays and mappings, detecting
 * cycles and shared usages while doing this. The last and largest part
 * are the functions to save an object into a save file, and restoring
 * it from there.
 *
 * -- struct object --
 *
 *   struct object {
 *       unsigned short    flags;
 *       short             total_light;
#ifndef OLD_RESET
 *       mp_int            time_reset;
#else
 *       mp_int            next_reset;
#endif
 *       mp_int            time_of_ref;
 *       p_int             ref;
 *       p_int             extra_ref;            (ifdef DEBUG)
 *       struct program  * prog;
 *       char            * name;
 *       char            * load_name;
 *       struct object   * next_all;
 *       struct object   * prev_all;
 *       struct object   * next_hash;
 *       struct object   * next_inv;
 *       struct object   * contains;
 *       struct object   * super;
 *       struct sentence * sent;
 *       struct wiz_list * user;
 *       struct wiz_list * eff_user;             (ifdef EUIDS)
 *       int               extra_num_variables;  (ifdef DEBUG)
 *       struct svalue   * variables;
 *       unsigned long     ticks, gigaticks;
 *   }
 *
 * The .flags collect some vital information about the object:
 *     O_HEART_BEAT       : the object has a heartbeat
 *     O_IS_WIZARD        : the object is a 'wizard' - this bit is set with
 *                          the efun set_is_wizard()
 *     O_ENABLE_COMMANDS  : can execute commands ("is a living")
 *     O_CLONE            : is a clone, or uses a replaced program
 *     O_DESTRUCTED       : has actually been destructed
 *     O_SWAPPED          : program and/or variables have been swapped out
 *     O_ONCE_INTERACTIVE : is or was interactive
 *     O_APPROVED         : inherits "/std/object"
 *     O_RESET_STATE      : is in a virgin resetted state
 *     O_WILL_CLEAN_UP    : call clean_up() when time is due
 *     O_LAMBDA_REFERENCED: a reference to a lambda was taken; this may
 *                          inhibit a replace_program().
 *     O_SHADOW           : object is shadowed
 *     O_REPLACED         : program was replaced.
 *
 * .ref counts the number of references to this object: it is this count
 * which can keep a destructed object around. Destructed objects are
 * stripped of everything but the basic struct object, but this one
 * is kept until the last reference is gone.
 *
 * .time_of_ref is the time() of the last apply on this object. The swapper
 * uses this timestamp to decide whether to swap the object or not.
 *
#ifndef OLD_RESET
 * Similar, .time_reset is the time() when the object should be reset
 * again. A time of 0 means: never.
 * The timing is not strict: any time after the given time is
 * sufficient. A reset object has its O_RESET_STATE flag set, which is
 * reset in an apply. If the time of reset is reached, but the object
 * is still in a reset state, or it is swapped out, the backend simply
 * sets a new .time_reset time, but does not do any real action.
 * To reduce the lag caused by the reset calls, all objects are kept
 * in the reset_table sorted by their time_reset. The .next_reset pointer
 * is used to built the table.
#else
 * Similar, .next_reset is the time() when the object should be reset
 * again. The timing is not strict: any time after the given time is
 * sufficient. A reset object has its O_RESET_STATE flag set, which is
 * reset in an apply. If the time of reset is reached, but the object
 * is still in a reset state, or it is swapped out, the backend simply
 * sets a new .next_reset time, but does not do any real action.
#endif
 *
 * .prog is a pointer to the struct programm, the bunch of bytecode for
 * this object. The program is shared between the master object (the
 * blueprint) and its clones. It is possible to replace the program
 * of a single object with a different one, but special care has
 * to be taken if lambda closures have been created.
 *
 * .variables is the block of variables of this object. Obviously they
 * can't be shared between master and clones. The number of variables
 * implicitely known by the program.
 *
 * .name and .load_name are the two names of the object. .name (an allocated
 * string) is the objects 'real' name: something like "std/thing" for
 * a blueprint, and "std/thing#45" for a clone. This name never has
 * a leading '/'. However, this name can be changed with the efun
 * rename_object().
 *
 * The .load_name (a shared string) is the name of the file from which
 * the object was created. It is identical in both blueprint and clones
 * (in our example "/std/thing") and can't be changed. In compat mode,
 * this name has no leading '/'. However, for virtual objects .load_name
 * is the virtual name - the real program name is .prog->name.
 *
 * .sent is the list of annotations to the object. Primary use is to
 * hold the list of commands ("sentences") defined by this object. Just
 * the first entry in this list has a special role: if the object is
 * shadowed, it is a "shadow_sentence" and keeps the list of shadows,
 * if the object is interactive, it is a "struct interactive" which
 * the comm.c module uses to store its data in. The interactive struct
 * also doubles as shadow_sentence.
 *
 * .user points to the wizlist entry of the wizard who 'owns' this
 * object. The entry is used to collect several stats for this user.
 * .eff_user exists only when EUIDS are used and describes the rights
 * of this object. .eff_user can be NULL, while .user can't.
 *
 * .ticks and .gigaticks count how much time the interpreter spent
 * in this particular object. The number is kept in two variables
 * to prevent overflows, it can be computed as gigaticks * 1E9 + ticks.
 *
 * .next_all, .prev_all and .next_hash are used to store the object.
 * .next_all and .prev_all are the link pointers in the list of all
 * objects, .next_hash is the link pointer in the object table (see otable.c).
 *
 * The gamedriver implements an environment/inventory system. .super
 * points to an object's surrounding object (and can be NULL), .contains
 * is the head of the list of contained objects. This inventory list
 * is linked by the .next_inv pointer.
 *
 * Related to the environment system is .total_light, which gives
 * total light emitted by the object including all its inventory. The
 * system is very crude and hardly used anymore.
 *
 * .extra_ref and .extra_num_variables are used by check_a_lot_of_refcounts().
 *
 *
 * A word about swapping: when an object is not in use for longer time,
 * the driver swaps out the program and/or the objects variables, and the
 * swapper assigns an even 'swapnum' each. If an object is swapped, the
 * O_SWAPPED flag is set and the affected pointer (.prog or .variables)
 * is replaced by the assigned swap_num _with the lowest bit set_. Since
 * pointers are assumed to always be even, this allows to distinguish
 * swapped programs/variables from unswapped ones.
 *
 * The exact structure of programs and variables is explained in exec.h .
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
#include "ptrtable.h"
#include "random.h"
#include "sent.h"
#include "simulate.h"
#include "stralloc.h"
#include "swap.h"
#include "wiz_list.h"

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

#ifdef DEBUG
void
_free_object (struct object *ob, char *from)

#else /* DEBUG */

int
_free_object (struct object *ob)

#endif /* DEBUG */

/* Deallocate/dereference all memory and structures held by <ob>.
 * At the time of call, the object must be destructed and removed
 * from the object table and list.
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
            sizeof (struct object);
        free_prog(prog, MY_TRUE);
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
    xfree(ob);

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

struct object *
get_empty_object ( int num_var
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
        xfree(ob);
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
#ifdef DEBUG
        if (ob->flags & O_DESTRUCTED) /* TODO: Can't happen */
            continue;
#endif
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
free_prog (struct program *progp, Bool free_sub_strings)

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
 * and TIME_TO_RESET. Upon time of call, the object must not be
 * in the reset table; this function will enter it there.
 * TODO: Change all non-shared strings in shared ones after finishing the
 * TODO:: reset.
 */

{
    /* Be sure to update time first ! */
#ifndef OLD_RESET
    ob->time_reset = current_time + TIME_TO_RESET/2
                                  + random_number(TIME_TO_RESET/2);
#else
    ob->next_reset = current_time + TIME_TO_RESET/2
                                  + random_number(TIME_TO_RESET/2);
#endif

#ifdef INITIALIZATION_BY___INIT

    /* Initialize the variables first in case of a H_CREATE_* call.
     */
    if (arg != H_RESET)
    {
        sapply_int(STR_VARINIT, ob, 0, MY_TRUE);
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
#ifndef OLD_RESET
        if (inter_sp->type == T_NUMBER && inter_sp->u.number)
            ob->time_reset = (inter_sp->u.number > 0)
                             ? current_time + inter_sp->u.number
                             : 0;
#else
        if (inter_sp->type == T_NUMBER && inter_sp->u.number)
            ob->next_reset = current_time + inter_sp->u.number;
#endif

        pop_stack();
    }
    else if (closure_hook[arg].type == T_STRING)
    {
        push_number(arg == H_RESET);
#ifndef OLD_RESET
        if (!sapply(closure_hook[arg].u.string, ob, 1) && arg == H_RESET)
            ob->time_reset = 0;
#else
        if (!sapply(closure_hook[arg].u.string, ob, 1) && arg == H_RESET)
            ob->next_reset = MAXINT;
#endif
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
        r_ob->ob->flags |= O_REPLACED;

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
Bool
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
#ifdef DEBUG
        if (ob->flags & O_DESTRUCTED) /* TODO: Can't happen */
            continue;
#endif
        if ( !O_PROG_SWAPPED(ob) )
            clear_program_id(ob->prog);
    }

    for (ob = obj_list; ob; ob = ob->next_all)
    {
#ifdef DEBUG
        if (ob->flags & O_DESTRUCTED)  /* TODO: Can't happen */
            continue;
#endif
        if ( !O_PROG_SWAPPED(ob) )
            renumber_program(ob->prog);
    }
    invalidate_apply_low_cache();
    return ++current_id_number;
}

/*=========================================================================*/
/*                        Save/Restore an Object                           */

/* TODO: Adapt these functions so that saving/restoring of single variables
 * TODO:: to strings are possible.
 * TODO: The function don't work properly if an object contains several
 * TODO:: variables of the same name, and their order/location in the
 * TODO:: variable block change between save and restore.
 * TODO: The functions should push an error-handler-svalue on the stack so
 * TODO:: that in case of errors everything (memory, files, svalues) can
 * TODO:: be deallocated properly. Right now, some stuff may be left behind.
 */

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

static void save_svalue(struct svalue *, char);
static void save_array(struct vector *);
static int restore_size(char **str);
INLINE static Bool restore_array(struct svalue *, char **str);
static int restore_svalue(struct svalue *, char **, char);
static void register_array(struct vector *);
static void register_mapping (struct mapping *map);

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

static struct pointer_table *ptable;
  /* The pointer_table used to register all arrays and mappings.
   */

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

static Bool failed;
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
  /* Type of the host which wrote the savefile being restored.
   */

static long current_shared_restored;
  /* ID of the shared value currently restored
   */

static struct svalue *shared_restored_values;
  /* Array of restored shared values, so that later references
   * can do a simple lookup by ID-1 (IDs start at 1).
   */

static long max_shared_restored;
  /* Current size of shared_restored_values.
   */

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

#define CTRLZ 30
/* MS-DOS and Windows files sometimes have this character :-(
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
static Bool
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
    struct pointer_record *record;

    /* We know for sure that we will find the key, because it has been
     * registered before.
     */
    record = lookup_pointer(ptable, pointer);

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

/*-------------------------------------------------------------------------*/
static void
save_mapping_filter (struct svalue *key, struct svalue *data, void *extra)

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
    if ( recall_pointer(m) )
        return;

    /* Nope, write it */

    MY_PUTC('(')
    MY_PUTC('[')
    check_map_for_destr(m);
    old_written = bytes_written - buf_left;
    walk_mapping(m, save_mapping_filter, (void *)(p_int)m->num_values);

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
static void
register_mapping_filter (struct svalue *key, struct svalue *data, void *extra)

/* Callback to register one mapping entry of (p_int)<extra> values.
 */

{
    int i;

    if (key->type == T_POINTER)
    {
        register_array  (key->u.vec);
    }
    else if (key->type == T_MAPPING)
    {
        register_mapping(key->u.map);
    }

    for (i = (p_int)extra; --i >= 0; data++)
    {
        if (data->type == T_POINTER)
        {
            register_array  (data->u.vec);
        }
        else if (data->type == T_MAPPING)
        {
            register_mapping(data->u.map);
        }
    }
} /* register_mapping_filter() */

/*-------------------------------------------------------------------------*/
static void
register_mapping (struct mapping *map)

/* Register the mapping <map> in the pointer table. If it was not
 * in there, also register all array/mapping values.
 */

{
    if (NULL == register_pointer(ptable, map))
        return;
    walk_mapping(map, register_mapping_filter, (void *)(p_int)map->num_values);
}

/*-------------------------------------------------------------------------*/
static void
save_svalue (struct svalue *v, char delimiter)

/* Encode the value <v> and write it to the write buffer, terminate
 * the output with <delimiter>.
 */

{
    switch(v->type)
    {
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
        /* To minimize rounding losses, the floats are written
         * in two forms: the nominal value, and the internal
         * representation.
         */

        L_PUTC_PROLOG
        char *source, c;

        source = number_buffer;
        (void)sprintf(source, "%.12e=%x:%lx"
                     , READ_DOUBLE(v), v->x.exponent & 0xffff
                     , v->u.mantissa);
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
        /* Objects and closures can't be saved */
        L_PUTC_PROLOG
        L_PUTC('0');
        L_PUTC(delimiter);
        L_PUTC_EPILOG
        return;
      }
    }

    MY_PUTC(delimiter);
}  /* save_svalue() */

/*-------------------------------------------------------------------------*/
static void
save_array (struct vector *v)

/* Encode the array <v> and write it to the write buffer.
 */

{
    int i;
    struct svalue *val;

    /* Recall the array from the pointer table.
     * If it is a shared one, there's nothing else to do.
     */
    if (recall_pointer(v))
        return;

    /* Write the '({'... */
    {
        L_PUTC_PROLOG
        L_PUTC('(')
        L_PUTC('{')
        L_PUTC_EPILOG
    }

    /* ... the values ... */
    for (i = VEC_SIZE(v), val = v->item; --i >= 0; )
    {
        save_svalue(val++, ',');
    }

    /* ... and the '})' */
    {
        L_PUTC_PROLOG
        L_PUTC('}')
        L_PUTC(')')
        L_PUTC_EPILOG
    }
} /* save_array() */

/*-------------------------------------------------------------------------*/
static void
register_array (struct vector *vec)

/* Register the array <vec> in the pointer table. If it was not
 * in there, also register all array/mapping values.
 */

{
    struct svalue *v;
    int i;

    if (NULL == register_pointer(ptable, vec))
        return;

    v = vec->item;
    for (i = VEC_SIZE(vec); --i >= 0; v++)
    {
        if (v->type == T_POINTER)
        {
            register_array  (v->u.vec);
        }
        else if (v->type == T_MAPPING)
        {
            register_mapping(v->u.map);
        }
    }
} /* register_array() */

/*-------------------------------------------------------------------------*/
void
save_object (struct object *ob, char *file)

/* Save the variables of object <ob> to the file <file> (the suffix ".o"
 * will be appended.
 *
 * The <file>.o will not be written immediately: first the savefile will
 * be created as <file>.o.tmp, which is after completion renamed to <file>.o.
 *
 * The validity of the filename is checked with a call to check_valid_path().
 */

{
    static char save_object_header[]
      = { '#', SAVE_OBJECT_VERSION, ':', SAVE_OBJECT_HOST, '\n'
        };
      /* The version string to write
       */

    char *name;
      /* Buffer for the final and the temporary filename.
       * name itself points to the final filename.
       */
    char *tmp_name;
      /* Pointer to the temporary filename in the buffer of name.
       */
    char save_buffer[SAVE_OBJECT_BUFSIZE];
      /* The write buffer
       */
    size_t len;
    int i;
    int f;
    struct svalue *v;
    struct variable *names;

    /* No need in saving destructed objects */

    if (ob->flags & O_DESTRUCTED)
        return;


    /* Get a valid filename */

    file = check_valid_path(file, ob, "save_object", MY_TRUE);
    if (file == NULL)
        error("Illegal use of save_object()\n");


    /* Create the final and the temporary filename */

    len = strlen(file);
    name = alloca(len + (sizeof save_file_suffix) +
                  len + (sizeof save_file_suffix) + 4);

    if (!name)
        error("Stack overflow in save_object()\n");

    tmp_name = name + len + sizeof save_file_suffix;
    (void)strcpy(name, file);

#ifndef MSDOS_FS
    (void)strcpy(name+len, save_file_suffix);
#endif
    sprintf(tmp_name, "%s.tmp", name);
#ifdef MSDOS_FS
    (void)strcpy(name+len, save_file_suffix);
#endif


    /* Open the file */

    save_object_descriptor =
      f = ixopen3(tmp_name, O_CREAT|O_TRUNC|O_WRONLY|O_TEXT, 0640);
    if (f < 0) {
        error("Could not open %s for a save.\n", tmp_name);
    }
    FCOUNT_SAVE(tmp_name);


    /* First pass through the variables to identify arrays/mappings
     * that are used more than once.
     */

    ptable = new_pointer_table();
    if (!ptable)
    {
        close(f);
        unlink(tmp_name);
        error("Out of memory.\n");
    }

    v = ob->variables;
    names = ob->prog->variable_names;
    for (i = ob->prog->num_variables; --i >= 0; v++, names++)
    {
        if (names->flags & TYPE_MOD_STATIC)
            continue;

        if (v->type == T_POINTER)
        {
            register_array  (v->u.vec);
        }
        else if (v->type == T_MAPPING)
        {
            register_mapping(v->u.map);
        }
    }

    /* Prepare the actual save */

    failed = MY_FALSE;
    current_sv_id_number = 0;
    bytes_written = 0;
    save_object_bufstart = save_buffer;
    memcpy(save_buffer, save_object_header, sizeof(save_object_header));
    buf_left = SAVE_OBJECT_BUFSIZE - sizeof(save_object_header);
    buf_pnt = save_buffer + sizeof(save_object_header);

    /* Second pass through the variables, actually saving them */

    v = ob->variables;
    names = ob->prog->variable_names;
    for (i = ob->prog->num_variables; --i >= 0; v++, names++)
    {
        if (names->flags & TYPE_MOD_STATIC)
            continue;

        /* Write the variable name */
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

    free_pointer_table(ptable);


    /* Finish up the file */

    len =  write( save_object_descriptor
                , save_object_bufstart
                , SAVE_OBJECT_BUFSIZE-buf_left);
    if (len != SAVE_OBJECT_BUFSIZE-buf_left )
        failed = MY_TRUE;


    /* On failure, delete the temporary file and return */

    if (failed)
    {
        (void)close(f);
        unlink(tmp_name);
        add_message("Failed to save to file. Disk could be full.\n");
        return;
    }

    /* Delete any existing savefile, then rename the temporary
     * file to the real name.
     */

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
} /* save_object() */


/*-------------------------------------------------------------------------*/
/* Structure used by restore_mapping() and restore_map_size()
 * to exchange data.
 */

struct rms_parameters
{
    char *str;       /* Current position in the input stream */
    int num_values;  /* Recognized number of values per key */
};

/*-------------------------------------------------------------------------*/
static int
restore_map_size (struct rms_parameters *parameters)

/* Determine the size of a mapping to be restored.
 * The mapping text starts at parameters->str, which points after the
 * initial '(['.
 *
 * The recognized width of the mapping is returned in parameters->num_values,
 * parameters->str is set to the character after the mapping text, and
 * the size (number of entries) is returned directly.
 * If the mapping text is ill formed, the function returns -1.
 *
 * The function calls itself and restore_array_size() recursively
 * for embedded arrays and mappings.
 */

{
    char *pt;                    /* Read pointer */
    int siz;                     /* Number of entries (so far) */
    int num_values = -1;         /* Last recognized width of the mapping */
    int current_num_values = 0;  /* Width of current entry */

    pt = parameters->str;
    siz = 0;

    /* Saveguard */
    if (!pt)
      return -1;

    /* The parse loop */

    while (MY_TRUE)
    {
        /* Parse the next element */
        switch (*pt)
        {

        case ']':  /* End of mapping */
          {
            if (pt[1] != ')')
                return -1;
            parameters->str = &pt[2];
            parameters->num_values = siz ? num_values : 1;
            return siz;
          }

        case ':':  /* Special case: ([:<width>]) */
          {
            if (siz || current_num_values)
                return -1;
            pt++;
            num_values = atoi(pt);
            pt = strchr(pt,']');
            if (!pt || pt[1] != ')' || num_values < 0)
                return -1;
            parameters->str = &pt[2];
            parameters->num_values = num_values;
            return siz;
          }

        case '\"':  /* A string */
          {
            int backslashes;

            do {
                pt = strchr(&pt[1],'\"');
                if (!pt)
                    return -1;
                /* the quote is escaped if and only
                 * if the number of slashes is odd. */
                for (backslashes = -1; pt[backslashes] == '\\'; backslashes--) ;
            } while ( !(backslashes & 1) ) ;
            pt++;
            break;
          }

        case '(':  /* An embedded mapping or array */
          {
            int tsiz;

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

        case '<':  /* A shared mapping/array */
          {
            pt = strchr(pt, '>');
            if (!pt)
                return -1;
            pt++;
            if (pt[0] == '=')
            {
                pt++;
                continue;
            }
            break;
          }

        case '-':  /* A negative number */
            pt++;
            if (!*pt)
                return -1;
            /* FALL THROUGH */

        case '0': case '1': case '2': case '3': case '4':  /* A number */
        case '5': case '6': case '7': case '8': case '9':
            if (pt[1] == '.')
            {
                /* A float: test for the <float>=<exp>:<mantissa> syntax */

                char *pt2;

                pt2 = strpbrk(pt, "=:;,");
                if (!pt2)
                    return -1;
                if (*pt2 != '=')
                    break;
                pt = strchr(pt2, ':');
                if (!pt)
                    return -1;
                pt++;
            }
            /* FALL THROUGH */

        /* Numbers and default: advance pt to the next terminal */
        default:
          {
            pt = strpbrk(pt, ":;,");
            if (!pt)
                return -1;
            break;
          }
        } /* switch() */

        /* At this point, pt points just after the preceeding
         * non-terminal
         */
        switch (*pt)
        {
          case ':':
            /* current_num_values is 0 on the first encounter */

            if (current_num_values)
                return -1;

            /* FALL THROUGH */

          case ';':
            current_num_values++;
            break;

          case ',':  /* End of entry */
            siz++;
            if (current_num_values != num_values)
            {
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

    /* NOTREACHED */

    return -1;
} /* restore_map_size() */

/*-------------------------------------------------------------------------*/
INLINE static void
free_shared_restored_values (void)

/* Deref all svalues in shared_restored_values[] up to
 * current_shared_restored, then deallocate the array itself.
 */

{
    while (current_shared_restored > 0)
        free_svalue(&shared_restored_values[--current_shared_restored]);
    xfree(shared_restored_values);
}

/*-------------------------------------------------------------------------*/
INLINE static Bool
restore_mapping (struct svalue *svp, char **str)

/* Restore a mapping from the text starting at *<str> (which points
 * just after the leading '([') and store it into *<svp>.
 * Return TRUE if the restore was successful, FALSE else (*<svp> is
 * set to const0 in that case).
 * On a successful return, *<str> is set to point after the mapping
 * restored.
 */

{
    struct mapping *z;
    struct svalue key, *data;
    int i;
    struct rms_parameters tmp_par;
    int siz;

    /* Determine the size and width of the mapping */

    tmp_par.str = *str;
    siz = restore_map_size(&tmp_par);
    if (siz < 0)
    {
        *svp = const0;
        return MY_FALSE;
    }

    /* Allocate the mapping */
    z = allocate_mapping(siz, tmp_par.num_values);

    if (!z)
    {
        *svp = const0;
        free_shared_restored_values();
        error("Out of memory\n");
        return MY_FALSE;
    }

    svp->type = T_MAPPING;
    svp->u.map = z;

    /* Loop through size and width, restoring the values */
    while (--siz >= 0)
    {
        i = tmp_par.num_values;
        key.type = T_NUMBER;
        if (!restore_svalue(&key, str, i ? ':' : ',' ))
        {
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

/*-------------------------------------------------------------------------*/
static int
restore_size (char **str)

/* Determine the size of an array to be restored.
 * The array text starts at *str, which points after the initial '({'.
 *
 * The recognized size of the array is returned, or -1 if the array text
 * is ill formed. *<str> is set to point to the character after the
 * array text.
 *
 * The function calls itself and restore_array_size() recursively
 * for embedded arrays and mappings.
 */

{
    char *pt, *pt2;
    int siz;

    pt = *str;
    siz = 0;

    while (pt && *pt)
    {
        switch(*pt)
        {
        case '}':  /* End of array */
          {
            if (pt[1] != ')')
                return -1;
            *str = &pt[2];
            return siz;
          }

        case '\"':  /* String */
          {
            int backslashes;

            do {
                pt = strchr(&pt[1],'\"');
                if (!pt)
                    return -1;
               /* the quote is escaped if and only
                * if the number of slashes is odd.
                */
               for (backslashes = -1; pt[backslashes] == '\\'; backslashes--)
                   NOOP;
            } while ( !(backslashes & 1) ) ;

            if (pt[1] != ',')
                return -1;
            siz++;
            pt += 2;
            break;
          }

        case '(':  /* Embedded array or mapping */
          {
            /* Lazy way of doing it, a bit inefficient */
            struct rms_parameters tmp_par;
            int tsiz;

            tmp_par.str = pt + 2;
            if (pt[1] == '{')
                tsiz = restore_size(&tmp_par.str);
            else if (pt[1] == '[')
                tsiz = restore_map_size(&tmp_par);
            else
                return -1;

            pt = tmp_par.str;
            if (tsiz < 0)
                return -1;

            pt++;
            siz++;

            break;
          }

        case '<': /* A shared array or mapping */
          {
            pt = strchr(pt, '>');
            if (!pt)
                return -1;
            if (pt[1] == ',')
            {
                siz++;
                pt += 2;
            }
            else if (pt[1] == '=')
            {
                pt += 2;
            }
            else
                return -1;
            break;
          }

        default:
            pt2 = strchr(pt, ',');
            if (!pt2)
                return -1;
            siz++;
            pt = &pt2[1];
            break;

        } /* switch() */
    } /* while() */

    return -1;
} /* restore_size() */

/*-------------------------------------------------------------------------*/
INLINE static Bool
restore_array (struct svalue *svp, char **str)

/* Restore an array from the text starting at *<str> (which points
 * just after the leading '({') and store it into *<svp>.
 * Return TRUE if the restore was successful, FALSE else (*<svp> is
 * set to const0 in that case).
 * On a successful return, *<str> is set to point after the array text
 * restored.
 */

{
    struct vector *v;
    char *pt, *end;
    int siz;

    end = *str;

    /* Get the size of the array */

    siz = restore_size(&end);
    if (siz < 0)
    {
        *svp = const0;
        return MY_FALSE;
    }

    if (siz > MAX_ARRAY_SIZE)
    {
        *svp = const0;
        free_shared_restored_values();
        error("Illegal array size: %ld.\n", (long int)siz);
        return MY_FALSE;
    }

    /* Allocate the array */

    *svp = const0; /* in case allocate_array() throws an error */

    v = allocate_array(siz);
    svp->type = T_POINTER;
    svp->u.vec = v;

    /* Restore the values */

    for ( svp = v->item; --siz >= 0; svp++)
    {
        if (!restore_svalue(svp, str, ','))
        {
            return MY_FALSE;
        }
    }

    /* Check for the trailing '})' */

    pt = *str;
    if (*pt++ != '}' || *pt++ != ')' ) {
        return MY_FALSE;
    }


    *str = pt;
    return MY_TRUE;
} /* restore_array() */

/*-------------------------------------------------------------------------*/
static Bool
restore_svalue (struct svalue *svp, char **pt, char delimiter)

/* Restore an svalue from the text starting at *<pt> up to the <delimiter>,
 * storing the value in *<svp>.
 * On success, set *<pt> to the character after the <deliminter> and return
 * TRUE, else return FALSE.
 */

{
    char *cp;

    switch( *(cp = *pt) )
    {

    case '\"':  /* A string */
      {
        char *source, *start, c;

        start = cp;
        source = cp+1;

        for(;;)
        {
            if ( !(c = *source++) )
            {
                *svp = const0;
                return MY_FALSE;
            }

#ifndef MSDOS_FS
            if (c == '\r')
                c = '\n';
#else
            if (c == CTRLZ && old_format)
                c = '\n';
#endif

            if (c == '\\')
            {
                if ( !(c = *source++) )
                {
                    *svp = const0;
                    return MY_FALSE; /* String ends with a \\ buggy probably */
                }
                switch(c)
                {
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
        *cp = '\0';
        *pt = source;
        svp->type = T_STRING;
        svp->x.string_type = STRING_SHARED;
        svp->u.string = make_shared_string(start);
        if (!svp->u.string)
        {
            *svp = const0;
            free_shared_restored_values();
            error("Out of memory\n");
        }
        break;
      }

    case '(': /* Unshared mapping or array */
        *pt = cp+2;
        switch ( cp[1] )
        {
        case '[':
          {
            if ( !restore_mapping(svp, pt) )
                return MY_FALSE;
            break;
          }

        case '{':
          {
            if ( !restore_array(svp, pt) )
                return MY_FALSE;
            break;
          }

        default:
            *svp = const0;
            return MY_FALSE;
        }
        break;

    case '-':  /* A number */
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
      {
        char c, *numstart = cp;
        int nega = 0;
        long l = 0;

        if (*cp == '-')
        {
            nega = 1;
            cp++;
        }

        while(lexdigit(c = *cp++)) l = (((l << 2) + l) << 1) + (c - '0');
        if (c != '.')
        {
            svp->type = T_NUMBER;
            svp->u.number = nega ? -l : l;
            *pt = cp;
            return c == delimiter;
        }

        /* If a float was written by the same host type as we are using,
         * restore the internal representation.
         * Otherwise, parse the float normally.
         */
        svp->type = T_FLOAT;
        if ( NULL != (cp = strchr(cp, '=')) &&  restored_host == CURRENT_HOST)
        {
            int tmp;

            cp++;
            if (sscanf(cp, "%x:%lx", &tmp, &svp->u.mantissa) != 2)
                return 0;
            svp->x.exponent = tmp;
        }
        else
        {
            STORE_DOUBLE_USED
            double d;

            d = atof(cp = numstart);
            STORE_DOUBLE(svp, d);
        }
        cp = strchr(cp, delimiter);
        *pt = cp+1;
        return cp != NULL;
      }

    case '<': /* A shared array or mapping */
      {
        int id;

        id = atoi(cp+1);
        cp = strchr(cp, '>');
        if (!cp)
        {
            *svp = const0;
            return MY_FALSE;
        }

        /* If a '=' follows, this is the first occurance of this
         * array/mapping, therefore restore it normally.
         */
        if (cp[1] == '=')
        {
            int res;

            *pt = cp+2;

            /* Shared values can be used even before they have been read in
             * completely.
             */
            if (id != ++current_shared_restored)
            {
                current_shared_restored--;
                *svp = const0;
                return MY_FALSE;
            }

            /* Increase shared_restored_values[] if necessary */

            if (id > max_shared_restored)
            {
                struct svalue *new;

                max_shared_restored <<= 1;
                new = (struct svalue *)
                  rexalloc((char*)shared_restored_values
                  , sizeof(struct svalue)*max_shared_restored
                  );
                if (!new)
                {
                    current_shared_restored--;
                    free_shared_restored_values();
                    *svp = const0;
                    error("Out of memory\n");
                    return MY_FALSE;
                }
                shared_restored_values = new;
            }

            /* in case of an error... */
            *svp = const0;
            shared_restored_values[id-1] = const0;
            
            /* Restore the value */
            res = restore_svalue(&shared_restored_values[id-1], pt, delimiter);
            assign_svalue_no_free(svp, &shared_restored_values[id-1]);
            return res;
        }

        if (id <= 0 || id > current_shared_restored)
        {
            *svp = const0;
            return MY_FALSE;
        }

        /* We know this value already: simply assign it */

        assign_svalue_no_free(svp, &shared_restored_values[id-1]);

        cp = strchr(cp, delimiter);
        *pt = cp+1;
        return cp != NULL;
      }

    default:
        *svp = const0;
        return MY_FALSE;

    } /* switch()*/

    cp = *pt;
    if (*cp++ != delimiter)
        return MY_FALSE;
    *pt = cp;

    return MY_TRUE;
} /* restore_svalue() */

/*-------------------------------------------------------------------------*/
static Bool
old_restore_string (struct svalue *v, char *str)

/* Called to restore the string starting at <str> into the *<v>
 * from old-format savefiles
 * Return TRUE on success, FALSE else.
 *
 * In this format, no escaped characters exist.
 */

{
    char *cp, c;

    cp = ++str;
    if ( '\0' != (c = *cp++) )
    {
        do {
#ifndef MSDOS_FS
            if (c == '\r')
                cp[-1] = '\n';
#else
            if (c == CTRLZ)
                cp[-1] = '\n';
#endif
        } while ( '\0' != (c = *cp++) );

        if (cp[-2] == '\n' && cp[-3] == '\"')
        {
            cp[-3] = '\0';
            v->type = T_STRING;
            v->x.string_type = STRING_SHARED;
            v->u.string = make_shared_string(str);
            if (!v->u.string)
            {
                *v = const0;
                free_shared_restored_values();
                error("Out of memory\n");
            }
            return MY_TRUE;
        }
    }
    *v = const0;
    return MY_FALSE;
}

/*-------------------------------------------------------------------------*/
Bool
restore_object (struct object *ob, char *file)

/* Restore the variables of <object> from file <file>.o (if <file> ends
 * in ".c", this is stripped).
 *
 * The validity of the filename is checked with a call to check_valid_path().
 */

{
    char *name;      /* Full name of the file to read */
    char *var;
    char *buff;      /* Current line read from the savefile */
    char *space;
    size_t len;
    FILE *f;
    struct stat st;  /* stat() info of the savefile */

    int var_rest;    /* Number of variables left after rover */
    int num_var;     /* Number of variables in the object */
    struct variable *rover = NULL;
      /* Roving pointer through the variable block. The next variable
       * to restore is searched from here, taking advantage of
       * the locality of save_object().
       */

#ifndef MSDOS_FS
    Bool old_format;
#endif

    struct discarded {
        struct svalue v;
        struct discarded *next;
    } * dp = NULL;
      /* List of values for which the variables no longer exist. */


    /* No use in restoring a destruct object */

    if (ob->flags & O_DESTRUCTED)
        return MY_FALSE;


    /* Get a valid filename */

    file = check_valid_path(file, ob, "restore_object", 0);
    if (file == NULL)
        error("Illegal use of restore_object()\n");


    /* Create the full filename */

    len = strlen(file);
    name = alloca(len + (sizeof save_file_suffix));

    if (!name)
        error("Stack overflow in restore_object()\n");
        
    (void)strcpy(name, file);
    if (name[len-2] == '.' && name[len-1] == 'c')
        len -= 2;
    (void)strcpy(name+len, save_file_suffix);


    /* Open the file and gets its length */

    f = fopen(name, "r");
    if (!f || fstat(fileno(f), &st) == -1) {
        if (f)
            (void)fclose(f);
        return MY_FALSE;
    }
    if (st.st_size == 0) {
        (void)fclose(f);
        return MY_FALSE;
    }
    FCOUNT_REST(name);


    /* Allocate the linebuffer. Unfortunately, the whole file
     * can be one single line.
     */

    buff = xalloc(st.st_size + 1);
    if (!buff)
    {
        fclose(f);
        error("Out of memory.\n");
        /* NOTREACHED */
        return MY_FALSE; /* flow control hint */
    }


    /* Initialise the variables */

    shared_restored_values = (struct svalue *)
                             xalloc(sizeof(struct svalue)*256);
    
    if (!shared_restored_values)
    {
        fclose(f);
        xfree(buff);
        error("Out of memory.\n");
        return MY_FALSE; /* flow control hint */
    }
    
    max_shared_restored = 256;
    current_shared_restored = 0;
    num_var = ob->prog->num_variables;
    var_rest = 0;
#ifndef MSDOS_FS
    old_format = MY_TRUE;
#endif
    restored_host = -1;

    /* Loop until we run out of text to parse */

    while(1)
    {
        struct svalue *v;
        char *pt;

        /* Get the next line from the text */
        if (fgets(buff, st.st_size + 1, f) == NULL)
            break;

        /* Remember that we have a newline at end of buff! */

        space = strchr(buff, ' ');
        if (space == NULL)
        {
            /* No space? It must be the version line! */

            if (buff[0] == '#')
            {
                int i;
                int restored_version;

                i = sscanf(buff+1, "%d:%d", &restored_version, &restored_host);
                if (i <= 0 || (i == 2 && restored_version <= CURRENT_VERSION) ) {
                    old_format = MY_FALSE;
                    continue;
                }
            }

            /* No version line: illegal format.
             * Deallocate what we allocated so far and return.
             */
            (void)fclose(f);
            if (dp)
                do
                    free_svalue(&dp->v);
                while ( NULL != (dp=dp->next) );

            free_shared_restored_values();
            xfree(buff);
            error("Illegal format when restoring %s.\n", name);
            /* NOTREACHED */
            return MY_FALSE; /* flow control hint */
        }

        /* Split the line at the position of the space.
         * Left of it is the variable name, to the right is the value.
         */
        *space = '\0';

        /* Set 'v' to the variable to restore */

        v = NULL;

        do { /* A simple try.. environment */

            if ( NULL != (var = findstring(buff)) )
            {
                /* The name exists in an object somewhere, now check if it
                 * is one of our variables
                 */

                do
                    rover++;
                while ( --var_rest > 0
                     && (rover->name != var || rover->flags & TYPE_MOD_STATIC)
                      );

                if (var_rest > 0)
                {
                    v = &ob->variables[num_var-var_rest];
                    break;
                }

                /* Wrap around and search again */

                rover = ob->prog->variable_names-1;
                var_rest = num_var + 1;
                do
                    rover++;
                while (--var_rest > 0
                   &&  (rover->name != var || rover->flags & TYPE_MOD_STATIC)
                      );
                if (var_rest > 0)
                {
                    v = &ob->variables[num_var-var_rest];
                    break;
                }
            }

            /* No 'else', but if we come here, the variable name was
             * not found in the shared string table or in the object.
             * That means we can eventually discard this line, but first
             * we have to parse it in case it contains the definition
             * of a shared array some other variable might use.
             *
             * Therefore we create a dummy variable and initialize
             * it to svalue-int, so that it can be freed without remorse.
             */

            {
                struct discarded *tmp;

                tmp = dp;
                dp = (struct discarded *)alloca(sizeof(struct discarded));
                if (!dp)
                {
                    free_shared_restored_values();
                    xfree(buff);
                    fclose(f);
                    if (tmp)
                    {
                        do
                            free_svalue(&tmp->v);
                        while (NULL != (tmp = tmp->next));
                    }
                    error("Stack overflow in restore_object()\n");
                    return MY_FALSE; /* flow control hint */
                }
                    
                dp->next = tmp;
                v = &dp->v;
                v->type = T_NUMBER;
                break;
            }

        } while (MY_FALSE);

        /* Get rid of the old value in v */

        free_svalue(v);
        *v = const0;

        /* ...and set it to the new one */

        pt = space+1;
        if ( (old_format && pt[0] == '\"')
             ? !old_restore_string(v, pt)
             : !restore_svalue(v, &pt, '\n')
           )
        {

            /* Whoops, illegal format */

            fclose(f);
            if (dp)
            {
                do
                    free_svalue(&dp->v);
                while ( NULL != (dp=dp->next) );
            }
            free_shared_restored_values();
            xfree(buff);
            error("Illegal format when restoring %s.\n", name);
            return MY_FALSE;
        }
    }

    /* Restore complete - now clean up */

    if (dp)
    {
        do
            free_svalue(&dp->v);
        while ( NULL != (dp=dp->next) );
    }
    if (d_flag > 1)
        debug_message("Object %s restored from %s.\n", ob->name, name);
    (void)fclose(f);
    free_shared_restored_values();
    xfree(buff);

    return MY_TRUE;
}

/***************************************************************************/

