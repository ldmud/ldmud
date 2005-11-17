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
 * -- object_t --
 *
 *   object_t {
 *       unsigned short  flags;
 *       p_int           ref;
#ifdef USE_SET_LIGHT
 *       short           total_light;
#endif
 *       mp_int          time_reset;
 *       mp_int          time_of_ref;
 *       mp_int          load_time;
 *       p_int           load_id;
 *       p_int           extra_ref;            (ifdef DEBUG)
 *       program_t     * prog;
 *       char          * name;
 *       char          * load_name;
 *       object_t      * next_all;
 *       object_t      * prev_all;
 *       object_t      * next_hash;
 *       object_t      * next_inv;
 *       object_t      * contains;
 *       object_t      * super;
 * TODO: The environment members (plus light) could be put into a special
 * TODO:: sentence and thus concentrated in a separated source file.
 *       sentence_t    * sent;
 *       wiz_list_t    * user;
 *       wiz_list_t    * eff_user;
 *       int             extra_num_variables;  (ifdef DEBUG)
 *       svalue_t      * variables;
 *       unsigned long   ticks, gigaticks;
 *   }
 *
 * The .flags collect some vital information about the object:
 *     O_HEART_BEAT       : the object has a heartbeat
#ifdef USE_SET_IS_WIZARD
 *     O_IS_WIZARD        : the object is a 'wizard' - this bit is set with
 *                          the efun set_is_wizard()
#endif
 *     O_ENABLE_COMMANDS  : can execute commands ("is a living")
 *     O_CLONE            : is a clone, or uses a replaced program
 *     O_DESTRUCTED       : has actually been destructed
 *     O_SWAPPED          : program and/or variables have been swapped out
 *     O_ONCE_INTERACTIVE : is or was interactive
 *     O_RESET_STATE      : is in a virgin resetted state
 *     O_WILL_CLEAN_UP    : call clean_up() when time is due
 *     O_LAMBDA_REFERENCED: a reference to a lambda was taken; this may
 *                          inhibit a replace_program().
 *     O_SHADOW           : object is shadowed
 *     O_REPLACED         : program was replaced.
 *
 * .ref counts the number of references to this object: it is this count
 * which can keep a destructed object around. Destructed objects are
 * stripped of everything but the basic object_t, but this one
 * is kept until the last reference is gone.
 *
 * .time_of_ref is the time() of the last apply on this object. The swapper
 * uses this timestamp to decide whether to swap the object or not.
 *
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
 *
 * .load_time simply is the time when the object was created. .load_id
 * serves to determine the creation order of objects created at the
 * same .load_time - this is used mostly for efuns like clones().
 *
 * .prog is a pointer to the program_tm, the bunch of bytecode for
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
 * shadowed, interactive, or using the editor, it is a "shadow_t"
 * and keeps the list of shadows resp. the other information.
 *
 * .user points to the wizlist entry of the wizard who 'owns' this
 * object. The entry is used to collect several stats for this user.
 * .eff_user describes the rights of this object. .eff_user can be
 * NULL, while .user can't.
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
 * system is very crude and hardly used anymore. There it is completely
 * deactivated if the efun set_light() is not defined.
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
#include "typedefs.h"

#include "my-alloca.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#include <stdio.h>
#include <fcntl.h>

#define USES_SVALUE_STRLEN
#include "object.h"

#include "actions.h"
#include "array.h"
#include "backend.h"
#include "closure.h"
#include "comm.h"
#include "exec.h"
#include "filestat.h"
#include "interpret.h"
#include "lang.h"
#include "lex.h"
#include "main.h"
#include "mapping.h"
#include "mempools.h"
#include "otable.h"
#include "prolang.h"
#include "ptrtable.h"
#include "random.h"
#include "sent.h"
#include "smalloc.h"
#include "simulate.h"
#include "simul_efun.h"
#include "stdstrings.h"
#include "stralloc.h"
#include "strfuns.h"
#include "swap.h"
#include "svalue.h"
#include "wiz_list.h"
#include "xalloc.h"

#include "../mudlib/sys/functionlist.h"
#include "../mudlib/sys/driver_hook.h"

/*-------------------------------------------------------------------------*/

replace_ob_t *obj_list_replace = NULL;
  /* List of scheduled program replacements.
   */

long tot_alloc_object = 0;
long tot_alloc_object_size = 0;
  /* Total number of allocated object, and the sum of memory they use.
   */

object_t NULL_object = { 0 };
  /* static null object for initialisations. memset() is not sufficient
   * because some machines (e.g. Bull) have a (char*)0 which is not
   * binary zero. Structure assignment otoh works.
   */

/*-------------------------------------------------------------------------*/
void
_free_object (object_t *ob)

/* Deallocate/dereference all memory and structures held by <ob>.
 * At the time of call, the object must be have at no refcount left,
 * must be destructed and removed from the object table and lists.
 */

{

#ifdef DEBUG

    /* Check the reference count */

    if (ob->ref > 0)
        fatal("Object with %ld refs passed to _free_object()\n", ob->ref);

    if (d_flag)
        printf("%s free_object: %s.\n", time_stamp(), ob->name);

    /* Freeing a non-destructed object should never happen */

    if (!(ob->flags & O_DESTRUCTED)) {
        fatal("Object 0x%lx %s ref count 0, but not destructed.\n"
             , (long)ob, ob->name);
    }

#endif /* DEBUG */

    if (ob->sent)
        fatal("free_object: Object '%s' (ref %ld, flags %08x) "
              "still has sentences.\n"
             , ob->name, ob->ref, ob->flags);

    /* If the program is freed, then we can also free the variable
     * declarations.
     */
    if (ob->prog)
    {
        program_t *prog = ob->prog;
        tot_alloc_object_size -=
            prog->num_variables * sizeof (svalue_t) +
            sizeof (object_t);
        free_prog(prog, MY_TRUE);
        ob->prog = NULL;
    }

    /* Deallocate the name */
    if (ob->name)
    {
        if (d_flag > 1)
            debug_message("%s Free object %s\n", time_stamp(), ob->name);
        if (lookup_object_hash(ob->name) == ob)
            fatal("Freeing object %s but name still in name table\n", ob->name);
        tot_alloc_object_size -= strlen(ob->name)+1;
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
} /* _free_object() */

/*-------------------------------------------------------------------------*/
#ifdef INITIALIZATION_BY___INIT

object_t *
get_empty_object (int num_var)

#else

object_t *
get_empty_object ( int num_var
                 , variable_t *variables
                 , svalue_t *initializers)

#endif

/* Allocate a new, empty object with <numvar> variables and return it.
 * Return NULL when out of memory.
 *
 * !__INIT: every variable, which is flagged in <variables> as
 *          NAME_INITIALIZED, is set to the corresponding value
 *          in <initializers>. Both <variables> and <initializers>
 *          are arrays of size <num_var>.
 *
 *  __INIT: All variables are set to 0.
 */

{
static mp_int last_time = 0;
static mp_int last_id = 0;

    object_t *ob;
    size_t size = sizeof (object_t);
    size_t size2 = num_var * sizeof (svalue_t);
    int i;
    svalue_t *ob_vars;

    /* Allocate the object structure */

    if ( !(ob = xalloc(size)) )
        return NULL;

    ob_vars = NULL;

    /* Allocated the variable block */

    if (size2 && !(ob_vars = xalloc(size2)) )
    {
        xfree(ob);
        return NULL;
    }

    tot_alloc_object++;
    tot_alloc_object_size += size + size2;

    /* Clear and initialise the object (no memset!) */

    *ob = NULL_object;
    ob->ref = 1;
    ob->load_time = current_time;
    if (last_time == current_time)
        ob->load_id = ++last_id;
    else
    {
        ob->load_id = last_id = 0;
        last_time = current_time;
    }
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
        {
            ob_vars[i] = const0;
        }
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
    object_t *ob;
    svalue_t v;

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
reference_prog (program_t *progp, char *from)

/* Increment the refcount of program <progp>, called from location <from>.
 */

{
    progp->ref++;
    if (d_flag)
        printf("%s reference_prog: %s ref %ld (%s)\n"
              , time_stamp(), progp->name, progp->ref, from);
}

/*-------------------------------------------------------------------------*/
void
do_free_sub_strings (int num_strings,   char **strings
                    ,int num_variables, variable_t *variable_names
                    ,int num_includes,  include_t *includes
                    )

/* Free a bunch of shared strings used in connection with an object:
 * the <num_strings> strings in the array <strings>,
 * the <num_variables> names of the vars in array <variable_names>, and
 * the <num_includes> names of the includes in array <includes>.
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

    /* Free all include names */
    for (i = num_includes; --i >= 0; )
    {
        free_string(includes[i].name);
        free_string(includes[i].filename);
    }
} /* do_free_sub_strings() */

/*-------------------------------------------------------------------------*/
void
free_prog (program_t *progp, Bool free_all)

/* Decrement the refcount for program <progp>. If it reaches 0, the program
 * is freed.
 *
 * If free_all is TRUE, all object strings and the blueprint reference are
 * freed, and free_prog() is called for all inherited programs.
 *
 * The only case when free_all is not true, is, when the swapper
 * swapped out the program and now attempts to free the memory.
 * This means that the string data is kept in memory all the time.
 * TODO: Swapping the strings is tricky, as they are all shared.
 * TODO:: Maybe swap them together with the variables - this is costly
 * TODO:: enough to make the lookup time needed when swapping in the
 * TODO:: strings look small.
 */

{
    /* Decrement the refcount */

    progp->ref--;
    if (progp->ref > 0)
        return;

    if (d_flag)
        printf("%s free_prog: %s\n", time_stamp(), progp->name);
    if (progp->ref < 0)
        fatal("Negative ref count for prog ref.\n");

    if (free_all && progp->blueprint)
    {
        object_t * blueprint = progp->blueprint;
        progp->blueprint = NULL;
        remove_prog_swap(progp, MY_TRUE);
        free_object(blueprint, "free_prog");
    }

    /* Update the statistics */
    total_prog_block_size -= progp->total_size;
    total_num_prog_blocks -= 1;

    /* Free the line numbers.
     *
     * This has to be done before the program is removed from the
     * swapper, else the following test would fail.
     */
    if (progp->line_numbers)
    {
        total_prog_block_size -= progp->line_numbers->size;
        xfree(progp->line_numbers);
        progp->line_numbers = NULL;
    }

    /* Is it a 'real' free? Then dereference all the
     * things held by the program, too.
     */
    if (free_all)
    {
        int i;
        bytecode_p program;
        funflag_t *functions;

        /* Remove the swap entry */
        remove_prog_swap(progp, MY_FALSE);

        program = progp->program;
        functions = progp->functions;

        /* Free all function names. */
        for (i = progp->num_functions; --i >= 0; )
        {
            if ( !(functions[i] & NAME_INHERITED) )
            {
                char *name;

                memcpy(
                  &name,
                  FUNCTION_NAMEP(program + (functions[i] & FUNSTART_MASK)),
                  sizeof name
                );
                free_string(name);
            }
        }

        /* Free the strings, variable names and include filenames. */
        do_free_sub_strings( progp->num_strings, progp->strings
                           , progp->num_variables, progp->variable_names
                           , progp->num_includes, progp->includes
                           );

        /* Free all inherited objects */
        for (i = 0; i < progp->num_inherited; i++)
            free_prog(progp->inherit[i].prog, MY_TRUE);

        /* Free the program name */
        total_prog_block_size -= strlen(progp->name)+1;
        xfree(progp->name);
    }

    /* Remove the program structure */
    xfree(progp);
} /* free_prog() */

/*-------------------------------------------------------------------------*/
void
reset_object (object_t *ob, int arg)

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
 * the called function, it is set to a random value between time_to_reset/2
 * and time_to_reset. Upon time of call, the object must not be
 * in the reset table; this function will enter it there.
 * TODO: Change all non-shared strings in shared ones after finishing the
 * TODO:: reset.
 */

{
    /* Be sure to update time first ! */
    if (time_to_reset > 0)
        ob->time_reset = current_time + time_to_reset/2
                         + (mp_int)random_number((uint32)time_to_reset/2);

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

    if (driver_hook[arg].type == T_CLOSURE)
    {
        lambda_t *l;

        if (arg == H_RESET)
            previous_ob = current_object = ob;

        l = driver_hook[arg].u.lambda;
        if (l->function.code[1] && arg != H_RESET)
        {
            /* closure accepts arguments, presumably one, so
             * give it the target object and bind to the current
             * object.
             */
            l->ob = current_object;
            push_object(ob);
            call_lambda(&driver_hook[arg], 1);
        }
        else
        {
            /* no arguments, just bind to target */
            l->ob = ob;
            call_lambda(&driver_hook[arg], 0);
        }

        /* If the call returned a non-zero number, use it as the current
         * reset interval, overwriting the default set above.
         */
        if (inter_sp->type == T_NUMBER && inter_sp->u.number)
            ob->time_reset = (inter_sp->u.number > 0)
                             ? current_time + inter_sp->u.number
                             : 0;

        pop_stack();
    }
    else if (driver_hook[arg].type == T_STRING)
    {
        if (arg == H_RESET)
            previous_ob = current_object = ob;

        push_number(arg == H_RESET);
        if (!sapply_int(driver_hook[arg].u.string, ob, 1, MY_TRUE)
          && arg == H_RESET)
            ob->time_reset = 0;
    }

    /* Object is reset now */
    ob->flags |= O_RESET_STATE;
} /* reset_object() */

/*-------------------------------------------------------------------------*/
void
replace_programs (void)

/* Called from the backend loop, this function
 * performs all pending program replacements listed in obj_list_replace.
 *
 * If the function runs out of memory, the processing ends at that point
 * and will be retried in the next call.
 *
 * Sideeffects of this action are: the objects are marked as 'replaced',
 * and current shadows are removed.
 */

{
    replace_ob_t *r_ob, *r_next;  /* List pointers */
    svalue_t *svp;
    int i, j;

#ifdef DEBUG
    if (d_flag)
        debug_message("%s start of replace_programs\n", time_stamp());
#endif

    for (r_ob = obj_list_replace; r_ob; r_ob = r_next)
    {
        program_t *old_prog;

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
         * Note that the change can only be a reduction, and that
         * the new program may not have variables at all. However, if
         * 'i' is not 0, the old program is guaranteed to have vars.
         */
        i = r_ob->ob->prog->num_variables - r_ob->new_prog->num_variables;
        if (i)
        {
            svalue_t *new_vars;

            /* Get the memory */

            if (r_ob->new_prog->num_variables)
            {
                new_vars = xalloc(  r_ob->new_prog->num_variables
                                  * sizeof *new_vars);

                if (!new_vars)
                {
                    obj_list_replace = r_ob;
                    return; /* Hope for more memory next time... */
                }
            }
            else
                new_vars = NULL;
#ifdef DEBUG
            if (d_flag)
                debug_message("%s %d less variables\n", time_stamp(), i);
            r_ob->ob->extra_num_variables = r_ob->new_prog->num_variables;
#endif

            /* Adjust the statistics */
            tot_alloc_object_size -= i * sizeof(svalue_t);

            svp = r_ob->ob->variables; /* the old variables */

            /* Deref those variables of ob which won't be copied */

            j = r_ob->var_offset;      /* number of unique vars of ob */
            i -= j;

#ifdef DEBUG
            if (d_flag)
                debug_message("%s freeing %d variables:\n", time_stamp(), j);
#endif
            while (--j >= 0) free_svalue(svp++);
#ifdef DEBUG
            if (d_flag)
                debug_message("%s freed.\n", time_stamp());
#endif

            /* Copy the others */
            j = r_ob->new_prog->num_variables;
            if (j)
            {
                memcpy(
                    (char *)new_vars,
                    (char *)svp,
                    j * sizeof(svalue_t)
                );
                svp += j;
            }
#ifdef DEBUG
            if (d_flag)
                debug_message("%s freeing %d variables:\n", time_stamp(), i);
#endif

            /* Deref the remaining non-copied variables */
            while (--i >= 0) free_svalue(svp++);
#ifdef DEBUG
            if (d_flag)
                debug_message("%s freed.\n", time_stamp());
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

        /* Remove current shadows */

        if (r_ob->ob->flags & O_SHADOW)
        {
            shadow_t *shadow_sent;

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
        xfree(r_ob);

        /* Free the old program, finally */
        free_prog(old_prog, MY_TRUE);

#ifdef DEBUG
        if (d_flag)
            debug_message("%s program freed.\n", time_stamp());
#endif
    }

    /* Done with the list */
    obj_list_replace = NULL;

#ifdef DEBUG
    if (d_flag)
        debug_message("%s end of replace_programs\n", time_stamp());
#endif

}  /* replace_programs() */

/*-------------------------------------------------------------------------*/
void
tell_npc (object_t *ob, char *str)

/* Call the lfun 'catch_tell()' in object <ob> with <str> as argument.
 *
 * This function is used to talk to non-interactive commandgivers
 * (aka NPCs).
 */

{
    push_volatile_string(str);
    (void)sapply(STR_CATCH_TELL, ob, 1);
}

/*-------------------------------------------------------------------------*/
void
tell_object (object_t *ob, char *str)

/* Send message <str> to object <ob>. If <ob> is an interactive player,
 * it will go to his screen (unless a shadow catches it - see shadow_catch_
 * message() ). If <ob> is not interactive, the message will go
 * to the lfun 'catch_tell()' via a call to tell_npc().
 */

{
    object_t *save_command_giver;
    interactive_t *ip;

    if (ob->flags & O_DESTRUCTED)
        return;

    if (O_SET_INTERACTIVE(ip, ob))
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
shadow_catch_message (object_t *ob, char *str)

/* Called by comm:add_message() to handle the case that messages <str> sent
 * to interactive objects <ob> are to be delivered to shadows of the
 * OR to a function in the interactive itself.
 *
 * This function checks all shadows of <ob> if they contain the lfun
 * catch_tell(), and calls the lfun in the first shadow where it exists
 * with message <str> as argument.
 *
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
 *
 * If the interactive user itself contains the lfun catch_tell(), the
 * messages counts as caught, too.
 */

{
    interactive_t *ip;

    ip = O_GET_INTERACTIVE(ob);

    if (!ip || !ip->catch_tell_activ || ob == current_object)
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
} /* shadow_catch_message() */

/*-------------------------------------------------------------------------*/
static void
clear_program_id (program_t *p)

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
renumber_program (program_t *p)

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
    object_t *ob;

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

/*-------------------------------------------------------------------------*/
static char *
function_exists ( char *fun, object_t *ob, Bool show_hidden
                , char ** prog_name, uint32 * prog_line
                )

/* Search for the function <fun> in the object <ob>. If existing, return
 * the name of the program (without added reference), if not return NULL.
 *
 * If <prog_name> and <prog_line> are both non-NULL, they are set to
 * the name of the program _file_ and the line where the function is found.
 * The program file name will have one reference added.
 *
 * Visibility rules apply: static and protected functions can't be
 * found from the outside unless <show_hidden> is true.
 */

{
    char *shared_name;
    fun_hdr_p funstart;
    program_t *progp;
    int ix;
    funflag_t flags;

#ifdef DEBUG
    if (ob->flags & O_DESTRUCTED)
        fatal("function_exists() on destructed object\n");
#endif

    if (prog_name)
        *prog_name = NULL;

    /* Make the program resident */
    if (O_PROG_SWAPPED(ob))
    {
        ob->time_of_ref = current_time;
        if (load_ob_from_swap(ob) < 0)
            error("Out of memory\n");
    }

    shared_name = findstring(fun);
    progp = ob->prog;

    /* Check if the function exists at all */
    if ( (ix = find_function(shared_name, progp)) < 0)
        return NULL;

    /* Is it visible for the caller? */
    flags = progp->functions[ix];

    if (!show_hidden
     && (   flags & TYPE_MOD_PRIVATE
         || (flags & TYPE_MOD_STATIC && current_object != ob))
       )
        return NULL;

    /* Resolve inheritance */
    while (flags & NAME_INHERITED)
    {
        inherit_t *inheritp;

        inheritp = &progp->inherit[flags & INHERIT_MASK];
        ix -= inheritp->function_index_offset;
        progp = inheritp->prog;
        flags = progp->functions[ix];
    }

    funstart = progp->program  + (flags & FUNSTART_MASK);

    /* And after all this, the function may be undefined */
    if (FUNCTION_CODE(funstart)[0] == F_ESCAPE
     && FUNCTION_CODE(funstart)[1] == F_UNDEF  - 0x100)
    {
        return NULL;
    }

    if (prog_line && prog_name)
        *prog_line = get_line_number(funstart, progp, prog_name);

    /* We got it. */
    return progp->name;
} /* function_exists() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_function_exists (svalue_t *sp, int num_arg)

/* EXEC function_exists()
 *
 *   mixed function_exists (string str [, int flags])
 *   mixed function_exists (string str , object ob, [, int flags])
 *
 * Look up a function <str> in the current object, respectively
 * in the object <ob>. Depending on the value of <flags>, one
 * of the following informations is returned:
 *
 * <flags> == FEXISTS_PROGNAME (0, default):
 *   Return the name of the program the function is defined in.
 *   This can be either object_name(ob), or the name of an inherited
 *   program. If !compat mode, the returned name always begins
 *   with a '/'.
 *
 * <flags> == FEXISTS_FILENAME (1):
 *   Return the name of the file the function is defined in (this
 *   may be an include file). If !compat mode, the returned name
 *   always begins with a '/'.
 *
 * <flags> == FEXISTS_LINENO (2):
 *   Return the line number within the source file.
 *
 * <flags> == FEXISTS_ALL (3):
 *   Return an array with all the above information. The above
 *   flag values are the indices into that array.
 *
 * The <flags> value can be or-ed to NAME_HIDDEN to return
 * information about static and protected functions in other objects.
 * It is not possible to return information about private functions.
 *
 * If the function cannot be found (because it doesn't exist or
 * it is not visible to the caller), the result is 0.
 */

{
    char *str, *prog_name;
    uint32 prog_line;
    svalue_t *argp;
    object_t *ob;
    p_int mode_flags;

    /* Evaluate arguments */
    argp = sp - num_arg + 1;

    ob = NULL;
    mode_flags = 0;

    inter_sp = sp;

    if (argp[0].type != T_STRING)
        bad_xefun_vararg(1, sp);

    if (num_arg < 2)
    {
        ob = current_object;
        mode_flags = 0;
    }

    if (num_arg >= 2)
    {
        if (argp[1].type == T_NUMBER)
        {
            ob = current_object;
            mode_flags = argp[1].u.number;

            if ((mode_flags & ~NAME_HIDDEN) < 0
             || (mode_flags & ~NAME_HIDDEN) > FEXISTS_ALL
               )
            if (mode_flags != 0 && mode_flags != NAME_HIDDEN)
            {
                error("Bad argument 2 to variable_exists(): "
                      "value %ld (%ld sans NAME_HIDDEN) out of range %d..%d.\n"
                     , (long)mode_flags, (long)(mode_flags & ~NAME_HIDDEN)
                     , FEXISTS_ALL, FEXISTS_LINENO
                    );
                /* NOTREACHED */
                return sp;
            }
        }
        else if (argp[1].type == T_OBJECT)
        {
            ob = argp[1].u.ob;
            mode_flags = 0;
        }
        else
            bad_xefun_vararg(2, sp);
    }

    if (num_arg >= 3)
    {
        /* The last argument must be a number. On the other
         * side, we can't have two numbers at once.
         */
        if (argp[1].type != T_OBJECT)
            bad_xefun_vararg(2, sp);

        if (argp[2].type != T_NUMBER)
            bad_xefun_vararg(3, sp);

        mode_flags = argp[2].u.number;

        if ((mode_flags & ~NAME_HIDDEN) < 0
         || (mode_flags & ~NAME_HIDDEN) > FEXISTS_ALL
           )
        if (mode_flags != 0 && mode_flags != NAME_HIDDEN)
        {
            error("Bad argument 3 to function_exists(): "
                  "value %ld (%ld sans NAME_HIDDEN) out of range %d..%d.\n"
                 , (long)mode_flags, (long)(mode_flags & ~NAME_HIDDEN)
                 , FEXISTS_ALL, FEXISTS_LINENO
                );
            /* NOTREACHED */
            return sp;
        }
    }

    if (ob->flags & O_DESTRUCTED)
    {
        error("Bad argument to function_exists(): Object is destructed.\n");
        /* NOTREACHED */
        return sp;
    }

    /* Get the information */
    prog_name = NULL;
    str = function_exists( argp->u.string, ob, (mode_flags & NAME_HIDDEN)
                         , &prog_name, &prog_line);
    sp = pop_n_elems(num_arg, sp);

    if (str)
    {
        switch (mode_flags & ~NAME_HIDDEN)
        {
        case FEXISTS_ALL:
          {
            char *res;
            char * p;
            vector_t *vec;

            /* Make a copy of the string so that we can remove
             * a the trailing '.c'. In non-compat mode, we also
             * have to add the leading '/'.
             */
            p = strrchr (str, '.');

            if (p)
                *p = '\0';  /* temporarily mask out the '.c' */

            if (compat_mode)
                res = string_copy (str);
            else
                res = add_slash(str);

            if (p)
                *p = '.';  /* undo the change above */

            if (!res)
            {
                error("Out of memory\n");
            }
            vec = allocate_uninit_array(FEXISTS_LINENO+1);
            put_malloced_string(vec->item+FEXISTS_PROGNAME, res);
            if (prog_name)
            {
                if (compat_mode)
                    res = string_copy (prog_name);
                else
                    res = add_slash(prog_name);
                if (!res)
                {
                    error("Out of memory\n");
                }
                put_malloced_string(vec->item+FEXISTS_FILENAME, res);
            }
            else
            {
                vec->item[FEXISTS_FILENAME].type = T_NUMBER;
                vec->item[FEXISTS_FILENAME].u.number = 0;
            }
            put_number(vec->item+FEXISTS_LINENO, prog_line);

            sp++;
            put_array(sp, vec);
            break;
          }
        case FEXISTS_PROGNAME:
          {
            char * p, * res;

            /* Make a copy of the string so that we can remove
             * a the trailing '.c'. In non-compat mode, we also
             * have to add the leading '/'.
             */
            p = strrchr (str, '.');

            if (p)
                *p = '\0';  /* temporarily mask out the '.c' */

            if (compat_mode)
                res = string_copy(str);
            else
                res = add_slash(str);

            if (p)
                *p = '.';  /* undo the change above */

            if (!res)
            {
                error("Out of memory\n");
            }
            sp++;
            put_malloced_string(sp, res);
            break;
          }

        case FEXISTS_FILENAME:
            if (prog_name)
            {
                char *res;

                if (compat_mode)
                    res = string_copy(prog_name);
                else
                    res = add_slash(prog_name);
                if (!res)
                {
                    error("Out of memory\n");
                }
                sp++;
                put_malloced_string(sp, res);
            }
            else
            {
                sp++;
                sp->type = T_NUMBER;
                sp->u.number = 0;
            }
            break;

        case FEXISTS_LINENO:
            sp++;
            sp->type = T_NUMBER;
            sp->u.number = prog_line;
            break;

        default:
            fatal("function_exists(): flags value %ld (from %ld) not implemented.\n"
                 , mode_flags & ~NAME_HIDDEN, mode_flags);
            /* NOTREACHED */
        }
    }
    else
    {
        sp++;
        sp->type = T_NUMBER;
        sp->u.number = 0;
    }

    /* str and prog_name have no ref on their own */

    return sp;
} /* f_function_exists() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_variable_exists (svalue_t *sp, int num_arg)

/* EXEC variable_exists()
 *
 *   string variable_exists (string str [, int flags])
 *   string variable_exists (string str , object ob, [, int flags])
 *
 * Look up a variable <str> in the current object, respectively
 * in the object <ob>.
 *
 * The result is the name of the program the variable is defined in. This can
 * be either object_name(ob), or the name of an inherited program. If !compat
 * mode, the returned name always begins with a '/'.
 *
 * If <flags> can be passed as NAME_HIDDEN to return information about static
 * and protected variables in other objects. It is not possible to return
 * information about private variables.
 *
 * If the variable cannot be found (because it doesn't exist or
 * it is not visible to the caller), the result is 0.
 */

{
    char *str;
    svalue_t *argp;
    object_t *ob;
    p_int mode_flags;

    /* Evaluate arguments */
    argp = sp - num_arg + 1;

    ob = NULL;
    mode_flags = 0;

    inter_sp = sp;

    if (argp[0].type != T_STRING)
        bad_xefun_vararg(1, sp);

    if (num_arg < 2)
    {
        ob = current_object;
        mode_flags = 0;
    }

    if (num_arg >= 2)
    {
        if (argp[1].type == T_NUMBER)
        {
            ob = current_object;
            mode_flags = argp[1].u.number;

            if (mode_flags != 0 && mode_flags != NAME_HIDDEN)
            {
                error("Bad argument 2 to variable_exists(): "
                      "value %ld, expected 0 or %d (NAME_HIDDEN).\n"
                     , (long)mode_flags, NAME_HIDDEN
                    );
                /* NOTREACHED */
                return sp;
            }
        }
        else if (argp[1].type == T_OBJECT)
        {
            ob = argp[1].u.ob;
            mode_flags = 0;
        }
        else
            bad_xefun_vararg(2, sp);
    }

    if (num_arg >= 3)
    {
        /* The last argument must be a number. On the other
         * side, we can't have two numbers at once.
         */
        if (argp[1].type != T_OBJECT)
            bad_xefun_vararg(2, sp);

        if (argp[2].type != T_NUMBER)
            bad_xefun_vararg(3, sp);

        mode_flags = argp[2].u.number;

        if (mode_flags != 0 && mode_flags != NAME_HIDDEN)
        {
            error("Bad argument 3 to variable_exists(): "
                  "value %ld, expected 0 or %d (NAME_HIDDEN).\n"
                 , (long)mode_flags, NAME_HIDDEN
                );
            /* NOTREACHED */
            return sp;
        }
    }

    if (ob->flags & O_DESTRUCTED)
    {
        error("Bad argument to variable_exists(): Object is destructed.\n");
        /* NOTREACHED */
        return sp;
    }

    /* Make the program resident */
    if (O_PROG_SWAPPED(ob))
    {
        ob->time_of_ref = current_time;
        if (load_ob_from_swap(ob) < 0)
            error("Out of memory: unswap object '%s'\n", ob->name);
    }

    /* Get the information */
    str = NULL;

    do
    {
        char *shared_name;
        program_t *progp;
        int ix;
        fulltype_t flags;

        shared_name = findstring(argp->u.string);
        if (!shared_name)
            break;

        progp = ob->prog;

        /* Check if the function exists at all */
        for (ix = 0; ix < progp->num_variables; ix++)
        {
            if (shared_name == progp->variable_names[ix].name)
                break;
        }

        if (ix >= progp->num_variables)
            break;

        /* Is it visible for the caller? */
        flags = progp->variable_names[ix].flags;

        if (!(mode_flags & NAME_HIDDEN)
         && (   (flags & TYPE_MOD_PRIVATE)
             || ((flags & TYPE_MOD_PROTECTED) && current_object != ob))
           )
            break;

        /* Resolve inheritance */
        while (flags & NAME_INHERITED)
        {
            int ic;
 
            for (ic = 0; ic < progp->num_inherited; ic++)
            {
                inherit_t *ip = &progp->inherit[ic];

                if (ix >= ip->variable_index_offset + ip->prog->num_variables
                 || ix < ip->variable_index_offset
                   )
                    continue;
                ix -= ip->variable_index_offset;
                progp = ip->prog;
                flags = progp->variable_names[ix].flags;
            }
        }

        /* progp now points to the program which really defines
         * the variable var.
         */

        /* We got it. */
        str = progp->name;
    } while(0);

    /* Put the result onto the stack */
    sp = pop_n_elems(num_arg, sp);
    inter_sp = sp;

    if (str)
    {
        /* Make a copy of the string so that we can remove
         * a the trailing '.c'. In non-compat mode, we also
         * have to add the leading '/'.
         */
        char * p, * res;
        p = strrchr (str, '.');

        if (p)
            *p = '\0';  /* temporarily mask out the '.c' */

        if (compat_mode)
            res = string_copy (str);
        else
            res = add_slash(str);

        if (p)
            *p = '.';  /* undo the change above */

        if (!res)
        {
            error("Out of memory\n");
        }
        sp++;
        put_malloced_string(sp, res);
    }
    else
    {
        sp++;
        put_number(sp, 0);
    }

    /* str had no ref on its own */

    return sp;
} /* f_variable_exists() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_variable_list (svalue_t *sp)

/* EFUN variable_list()
 *
 *   mixed *variable_list (object ob, int flags = RETURN_FUNCTION_NAME)
 *
 * Return an array with information about <ob>s variables. For every
 * variable, 1 to 4 values (depending on <flags>) are stored in
 * the result array conveying in this order:
 *   - the name of the variable
 *   - the variable flags (see below)
 *   - the return type (listed in mudlib/sys/lpctypes.h)
 *   - the value of the variable
 *
 * <ob> may be given as true object or as a filename. In the latter
 * case, the efun does not try to load the object before proceeding.
 *
 * If <ob> is not the current object and the value of the variable is
 * requested, a privilege_violation ("variable_list", <ob>) occurs.
 *
 * <flags> determines both which information is returned for every
 * variable, and which variables should be considered at all.
 * Its value is created by bin-or'ing together following flags from
 * mudlib/sys/functionlist.h:
 *
 *   Control of returned information:
 *     RETURN_FUNCTION_NAME    include the variable name
 *     RETURN_FUNCTION_FLAGS   include the variable flags
 *     RETURN_FUNCTION_TYPE    include the return type
 *     RETURN_VARIABLE_VALUE   include the variable value
 *
 *   Control of listed variables:
 *     NAME_INHERITED      don't list if defined by inheritance
 *     TYPE_MOD_NOSAVE ==
 *     TYPE_MOD_STATIC     don't list if nosave ('static') variable
 *     TYPE_MOD_PRIVATE    don't list if private
 *     TYPE_MOD_PROTECTED  don't list if protected
 *     NAME_HIDDEN         don't list if not visible through inheritance
 *
 * The 'flags' information consists of the bin-or of the list control
 * flags given above, plus the following:
 *
 *     TYPE_MOD_VIRTUAL    variable is inherited virtually
 *     TYPE_MOD_NO_MASK    variable is nomask
 *     TYPE_MOD_PUBLIC     variable is public
 *
 * All these flags are defined in mudlib/sys/functionlist.h, which
 * should be copied into an accessible place in the mudlib. The
 * return types are defined in mudlib/sys/lpctypes.h which also
 * should be copied into the mudlib.
 *
 * TODO: All these defs are in mudlib/sys/functionlist.h and mudlib/sys/lpctypes.h
 * TODO:: as well as in exec.h and this file. This should be centralized.
 * TODO:: Maybe write the files on mud startup?
 * TODO:: Include mudlib/sys/functionlist.h doesn't help because then
 * TODO:: mkdepend stumbles over the embedded include <sys/lpctypes.h>.
 */

{
    object_t *ob;         /* <ob> argument to list */
    mp_int mode_flags;    /* <flags> argument */
    program_t *prog;      /* <ob>'s program */
    unsigned short num_variables;  /* Number of variables to list */
    char *vis_tags;
      /* Bitflag array describing the visibility of every variable in prog
       * in relation to the passed <flags>: */
#define VISTAG_INVIS '\0'  /* Variable should not be listed */
#define VISTAG_VIS   '\1'  /* Variable matches the <flags> list criterium */
#define VISTAG_ALL   '\2'  /* Variable should be listed, no list restrictions */
#define VISTAG_NAMED '\4'  /* Variable is neither hidden nor private */

#define FILTERFLAGS (NAME_HIDDEN|TYPE_MOD_PRIVATE|TYPE_MOD_STATIC|TYPE_MOD_PROTECTED|NAME_INHERITED)

    vector_t *list;       /* Result vector */
    svalue_t *svp;        /* Last element in list which was filled in. */
    variable_t *var;      /* Current variable under examination */
    uint32 flags;
    long i, j;

    inter_sp = sp; /* In case of errors leave a clean stack */

    /* Extract the arguments from the vm stack.
     */
    if (sp[-1].type != T_OBJECT)
    {
        if (sp[-1].type != T_STRING)
            bad_xefun_arg(1, sp);
        if (!(ob = find_object(sp[-1].u.string)))
            error("Object '%s' not found.\n", sp[-1].u.string);
    }
    else
        ob = sp[-1].u.ob;

    if (sp[0].type != T_NUMBER)
        bad_xefun_arg(2, sp);

    mode_flags = sp->u.number;

    if (ob != current_object && (mode_flags & RETURN_VARIABLE_VALUE))
    {
        assert_master_ob_loaded();
        if (privilege_violation3("variable_list", sp-1, sp) <= 0)
        {
            free_svalue(sp);
            sp--;
            free_svalue(sp);
            sp->type = T_NUMBER;
            sp->u.number = 0;
            return sp;
        }
    }

    if (O_PROG_SWAPPED(ob))
        if (load_ob_from_swap(ob) < 0)
        {
            error("Out of memory: unswap object '%s'\n", ob->name);
            /* NOTREACHED */
            return NULL;
        }

    prog = ob->prog;

    /* Initialize the vistag[] flag array.
     */
    num_variables = prog->num_variables;
    vis_tags = alloca(num_variables);
    if (!vis_tags)
    {
        error("Stack overflow in variable_list()");
        /* NOTREACHED */
        return NULL;
    }

    /* Preset the visibility. By default, if there is any listing
     * modifier, the variables are not visible. If there is none, the
     * variables are visible.
     */
    memset( vis_tags, (mode_flags & FILTERFLAGS) ?  VISTAG_INVIS : VISTAG_ALL
          , num_variables);

    /* Count how many named variables need to be listed in the result.
     * Flag every variable to list in vistag[].
     */
    num_variables = 0;

    /* First, check all variables for which we have a name */
    flags = mode_flags & (FILTERFLAGS ^ NAME_HIDDEN);
    var = prog->variable_names;
    i = prog->num_variables;
    while ( --i >= 0 )
    {
        if (!(var[i].flags & flags) )
        {
            vis_tags[i] = VISTAG_NAMED|VISTAG_VIS;
            num_variables++;
        }
        else
        {
            vis_tags[i] |= VISTAG_NAMED;
        }
    }


    /* If the user wants to see the hidden or private variables, we loop
     * through the full variable table and check all variables not yet
     * touched by the previous 'named' scan.
     * TODO: Due to the dedicated 'find hidden name' loop, this shouldn't
     * TODO:: be necessary, nor the VISTAG_ALL at all.
     */
    if ((mode_flags & (TYPE_MOD_PRIVATE|NAME_HIDDEN)) == 0)
    {
        var = prog->variable_names;
        for (i = prog->num_variables; --i >= 0; )
        {
            if (!(vis_tags[i] & VISTAG_NAMED)
             && !(var[i].flags & flags)
               )
            {
                vis_tags[i] = VISTAG_VIS;
                num_variables++;
            }
        }
    }

    /* If <flags> accepts all variables, use the total number of variables
     * instead of the count computed above.
     */
    if ( !(mode_flags & FILTERFLAGS))
    {
        num_variables = prog->num_variables;
    }

    /* Compute the size of the result vector to
     *  2**(number of RETURN_FUNCTION_ bits set)
     */
    for (i = (mode_flags & RETURN_VARIABLE_MASK), j = 0; i; i >>= 1) {
        if (i & 1)
            j += num_variables;
    }

    /* Allocate the result vector and set svp to its end
     */
    list = allocate_array(j);
    svp = list->item + j;

    /* Loop backwards through all variables, check their flags if
     * they are to be listed and store the requested data in
     * the result vector.
     */

    for (i = prog->num_variables, var = prog->variable_names + i; --i >= 0; )
    {
        uint32 active_flags;  /* A variable's definition status flags */
        var--;

        if ((vis_tags[i] & (VISTAG_ALL|VISTAG_VIS)) == VISTAG_INVIS)
            continue; /* Don't list this one */

        flags = var->flags;

        active_flags = (flags & ~INHERIT_MASK);
        if (!(vis_tags[i] & VISTAG_NAMED))
            active_flags |= NAME_HIDDEN;

        /* Add the data to the result vector as <flags> determines.
         */

        if (mode_flags & RETURN_VARIABLE_VALUE)
        {
            svp--;
            assign_svalue_no_free(svp, &ob->variables[i]);
        }

        if (mode_flags & RETURN_FUNCTION_TYPE)
        {
            svp--;
            svp->u.number = var->flags & TYPE_MOD_MASK;
        }

        if (mode_flags & RETURN_FUNCTION_FLAGS)
        {
            svp--;
            svp->u.number = (p_int)active_flags;
        }

        if (mode_flags & RETURN_FUNCTION_NAME) {
            svp--;
            put_ref_string(svp, var->name);
        }
    } /* for() */

    /* Cleanup and return */
    free_svalue(sp);
    sp--;
    free_svalue(sp);

    put_array(sp, list);
    return sp;

#undef FILTERFLAGS

#undef VISTAG_INVIS
#undef VISTAG_VIS
#undef VISTAG_ALL
#undef VISTAG_NAMED
} /* f_variable_list() */

/*=========================================================================*/
/*                        Save/Restore an Object                           */

/*
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
 * <version> is currently 1
 *    Version 0 didn't allow the saving of non-lambda closures, symbols
 *    or quoted arrays.
 * <host> is 1 for Atari ST and Amiga, and 0 for everything else.
 *    The difference lies in the handling of float numbers (see datatypes.h).
 */

#define SAVE_OBJECT_VERSION '1'
#define CURRENT_VERSION 1
  /* Current version of new save files, expressed as char and as int.
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

static Bool save_svalue(svalue_t *, char, Bool);
static void save_array(vector_t *);
static int restore_size(char **str);
static int restore_svalue(svalue_t *, char **, char);
static void register_svalue(svalue_t *);

/*-------------------------------------------------------------------------*/

#define SAVE_OBJECT_BUFSIZE 4096
  /* Size of the read/write buffer.
   */

static int save_version = -1;
  /* The version of the savefile to write.
   */

static const char save_file_suffix[] = ".o";
  /* The suffix of the save file, in an array for easier computations.
   * (sizeof() vs. strlen()+1.
   */

static struct pointer_table *ptable = NULL;
  /* The pointer_table used to register all arrays and mappings.
   * If an error happens during the save, this table probably won't
   * be deallocated.
   */

static char number_buffer[36];
  /* Buffer to create numbers in - big enough for 32 Bit uints.
   */

static char *save_object_bufstart;
  /* Start of the write buffer (which lives on the stack).
   */

static char *buf_pnt;
  /* Current position in the write buffer.
   */

static int buf_left;
  /* Space left in the write buffer.
   */

static mp_int bytes_written;
  /* Number of bytes so far written to the file or strbuf.
   */

static Bool failed;
  /* An IO error occured.
   */

static int save_object_descriptor = -1;
  /* FD of the savefile, -1 if not assigned.
   */

static strbuf_t save_string_buffer;
  /* When saving to a string: the string buffer.
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

static svalue_t *shared_restored_values = NULL;
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
    if (!--buf_left) {\
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

#define L_PUTC_PROLOG char *l_buf_pnt = buf_pnt;\
                      int  l_buf_left = buf_left;
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
void
free_save_object_buffers(void)

/* Deallocate all lingering buffers from previous save operations, preferably
 * before the GC does it.
 */

{
    if (ptable)
        free_pointer_table(ptable);
    ptable = NULL;
} /* free_save_object_buffers() */

/*-------------------------------------------------------------------------*/
static char*
write_buffer (void)

/* Write the current content of the write buffer to the savefile
 * resp. to the string buffer and return a pointer to its start.
 *
 * On an error, set failed to TRUE.
 */

{
    char *start;

    start = save_object_bufstart;
    if (save_object_descriptor >= 0)
    {

        if (write( save_object_descriptor, start, SAVE_OBJECT_BUFSIZE )
          != SAVE_OBJECT_BUFSIZE )
            failed = MY_TRUE;
    }
    else
        strbuf_addn(&save_string_buffer, start, SAVE_OBJECT_BUFSIZE);

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
} /* recall_pointer() */

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
} /* save_string() */

/*-------------------------------------------------------------------------*/
static void
save_mapping_filter (svalue_t *key, svalue_t *data, void *extra)

/* Filter used by save_mapping: write <key> and (p_int)<extra> values
 * in <data>[] to the write buffer.
 */

{
    int i;

    i = (p_int)extra;
    if (save_svalue(key, (char)(i ? ':' : ','), MY_TRUE))
    {
        while (--i >= 0)
            save_svalue(data++, (char)(i ? ';' : ','), MY_FALSE );
    }
} /* save_mapping_filter() */

/*-------------------------------------------------------------------------*/
static void
save_mapping (mapping_t *m)

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
        (void)sprintf(source, "%ld", (long)m->num_values);
        c = *source++;
        do MY_PUTC(c) while ( '\0' != (c = *source++) );
    }

    MY_PUTC(']')
    MY_PUTC(')')
} /* save_mapping() */

/*-------------------------------------------------------------------------*/
static void
register_mapping_filter (svalue_t *key, svalue_t *data, void *extra)

/* Callback to register one mapping entry of (p_int)<extra> values.
 */

{
    int i;

    register_svalue(key);

    for (i = (p_int)extra; --i >= 0; data++)
    {
        register_svalue(data);
    }
} /* register_mapping_filter() */

/*-------------------------------------------------------------------------*/
static void
register_mapping (mapping_t *map)

/* Register the mapping <map> in the pointer table. If it was not
 * in there, also register all array/mapping values.
 */

{
    if (NULL == register_pointer(ptable, map))
        return;
    walk_mapping(map, register_mapping_filter, (void *)(p_int)map->num_values);
}

/*-------------------------------------------------------------------------*/
static Bool
save_svalue (svalue_t *v, char delimiter, Bool writable)

/* Encode the value <v> and write it to the write buffer, terminate
 * the output with <delimiter>.
 * If <writable> is false, unwritable svalues like objects are written
 * as '0'. If <writable> is true, unwritable svalues are not written at all.
 *
 * Return is true if something was written, and false otherwise.
 */

{
    Bool rc = MY_TRUE;

    assert_stack_gap();

    switch(v->type)
    {
    case T_STRING:
        save_string(v->u.string);
        break;

    case T_QUOTED_ARRAY:
      {
        L_PUTC_PROLOG
        char * source, c;

        source = number_buffer;
        (void)sprintf(source, "#%hd:", v->x.quotes);
        c = *source++;
        do L_PUTC(c) while ( '\0' != (c = *source++) );
        L_PUTC_EPILOG
        /* FALLTHROUGH to T_POINTER */
      }

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
        return rc;
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
        return rc;
      }

    case T_MAPPING:
        save_mapping(v->u.map);
        break;

    case T_SYMBOL:
      {
        L_PUTC_PROLOG
        char * source, c;

        source = number_buffer;
        (void)sprintf(source, "#%hd:", v->x.quotes);
        c = *source++;
        do L_PUTC(c) while ( '\0' != (c = *source++) );
        L_PUTC_EPILOG
        save_string(v->u.string);
        break;
      }

    case T_CLOSURE:
      {
        int type;

        if (save_version > 0)
        {
            switch(type = v->x.closure_type)
            {
            case CLOSURE_LFUN:
              {
                if (recall_pointer(v->u.lambda))
                    break;

                {
                    L_PUTC_PROLOG

                    if (v->u.lambda->ob == current_object)
                    {
                        lambda_t  *l;
                        program_t *prog;
                        int        ix;
                        funflag_t  flags;
                        char      *function_name, c;
                        object_t  *ob;

                        l = v->u.lambda;
                        ob = l->ob;
                        ix = l->function.index;

                        prog = ob->prog;
                        flags = prog->functions[ix];
                        while (flags & NAME_INHERITED)
                        {
                            inherit_t *inheritp;

                            inheritp = &prog->inherit[flags & INHERIT_MASK];
                            ix -= inheritp->function_index_offset;
                            prog = inheritp->prog;
                            flags = prog->functions[ix];
                        }

                        memcpy(&function_name
                              , FUNCTION_NAMEP(prog->program + (flags & FUNSTART_MASK))
                              , sizeof function_name
                              );

                        L_PUTC('#');
                        L_PUTC('l');
                        L_PUTC(':');
                        c = *function_name++;
                        do L_PUTC(c) while ( '\0' != (c = *function_name++) );
                        /* TODO: Once we have inherit-conscious lfun closures,
                         * TODO:: save them as #'l:<inherit>-<name>
                         */
                    }
                    else
                    {
                        L_PUTC('0');
                    }

                    L_PUTC_EPILOG
                }
                break;
              }

            case CLOSURE_IDENTIFIER:
              {
                lambda_t *l;
                char * source, c;

                if (recall_pointer(v->u.lambda))
                    break;

                l = v->u.lambda;
                if (l->function.index == VANISHED_VARCLOSURE_INDEX)
                {
                    rc = MY_FALSE;
                    break;
                }
                if (l->ob->flags & O_DESTRUCTED
                 || l->ob != current_object
                   )
                {
                    rc = MY_FALSE;
                    break;
                }

                source = l->ob->prog->variable_names[l->function.index].name;

                {
                    L_PUTC_PROLOG

                    L_PUTC('#');
                    L_PUTC('v');
                    L_PUTC(':');
                    c = *source++;
                    do L_PUTC(c) while ( '\0' != (c = *source++) );

                    L_PUTC_EPILOG
                }
                break;
              }

            default:
                if (type < 0)
                {
                    switch(type & -0x0800)
                    {
                    case CLOSURE_OPERATOR:
                      {
                        char *s = NULL;
                        switch(type - CLOSURE_OPERATOR)
                        {
                        case F_POP_VALUE:
                            s = ",";
                            break;

                        case F_BBRANCH_WHEN_NON_ZERO:
                            s = "do";
                            break;

                        case F_BBRANCH_WHEN_ZERO:
                            s = "while";
                            break;

                        case F_BRANCH:
                            s = "continue";
                            break;

                        case F_CSTRING0:
                            s = "default";
                            break;

                        case F_BRANCH_WHEN_ZERO:
                            s = "?";
                            break;

                        case F_BRANCH_WHEN_NON_ZERO:
                            s = "?!";
                            break;

                        case F_RANGE:
                            s = "[..]";
                            break;

                        case F_NR_RANGE:
                            s = "[..<]";
                            break;

                        case F_RR_RANGE:
                            s = "[<..<]";
                            break;

                        case F_RN_RANGE:
                            s = "[<..]";
                            break;

                        case F_MAP_INDEX:
                            s = "[,]";
                            break;

                        case F_NX_RANGE:
                            s = "[..";
                            break;

                        case F_RX_RANGE:
                            s = "[<..";
                            break;

                        }

                        if (s)
                        {
                            L_PUTC_PROLOG
                            char c;

                            L_PUTC('#');
                            L_PUTC('e');
                            L_PUTC(':');

                            c = *s++;
                            do L_PUTC(c) while ( '\0' != (c = *s++) );

                            L_PUTC_EPILOG
                            break;
                        }
                        type += CLOSURE_EFUN - CLOSURE_OPERATOR;
                      }
                    /* default action for operators: FALLTHROUGH */

                    case CLOSURE_EFUN:
                      {
                        L_PUTC_PROLOG
                        char * source, c;

                        source = instrs[type - CLOSURE_EFUN].name;

                        L_PUTC('#');
                        L_PUTC('e');
                        L_PUTC(':');

                        c = *source++;
                        do L_PUTC(c) while ( '\0' != (c = *source++) );

                        L_PUTC_EPILOG
                        break;
                      }

                    case CLOSURE_SIMUL_EFUN:
                      {
                        L_PUTC_PROLOG
                        char * source, c;

                        source = simul_efunp[type - CLOSURE_SIMUL_EFUN].name;

                        L_PUTC('#');
                        L_PUTC('s');
                        L_PUTC(':');

                        c = *source++;
                        do L_PUTC(c) while ( '\0' != (c = *source++) );

                        L_PUTC_EPILOG
                        break;
                      }
                    }
                    break;
                }
                else /* type >= 0: one of the lambda closures */
                {
                    rc = MY_FALSE;
                }
                break;

            } /* switch(closure type) */
        }
        else
        {
            rc = 0;
        }

        /* 'rc' at this point signifies whether the closure could be written.
         * If it couldn't, maybe write a default '0', and also adjust rc
         * to serve as function result.
         */
        if (!rc)
        {
            if (writable)
                rc = MY_FALSE;
            else
            {
                L_PUTC_PROLOG

                rc = MY_TRUE; /* Writing a default '0' counts */
                L_PUTC('0');
                L_PUTC(delimiter);
                L_PUTC_EPILOG
            }
            return rc;
        }
        break;
      } /* case T_CLOSURE */

    default:
      {
        /* Objects can't be saved */
        if (writable)
            rc = MY_FALSE;
        else
        {
            L_PUTC_PROLOG
            L_PUTC('0');
            L_PUTC(delimiter);
            L_PUTC_EPILOG
        }
        return rc;
      }
    }

    if (rc)
        MY_PUTC(delimiter);

    return rc;
}  /* save_svalue() */

/*-------------------------------------------------------------------------*/
static void
save_array (vector_t *v)

/* Encode the array <v> and write it to the write buffer.
 */

{
    long i;
    svalue_t *val;

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
    for (i = (long)VEC_SIZE(v), val = v->item; --i >= 0; )
    {
        save_svalue(val++, ',', MY_FALSE);
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
register_closure (svalue_t *cl)

/* Register closure <cl> in the pointer table. If it was not
 * in there, also register all associated svalues (if any).
 */

{
    int type;

    switch(type = cl->x.closure_type)
    {
    case CLOSURE_LFUN:
    case CLOSURE_IDENTIFIER:
        if (NULL == register_pointer(ptable, cl->u.lambda))
            return;
        break;

    default:
        /* Operator- or an unsaveable lambda closure */
        return;
    }
} /* register_closure() */

/*-------------------------------------------------------------------------*/
static void
register_array (vector_t *vec)

/* Register the array <vec> in the pointer table. If it was not
 * in there, also register all array/mapping values.
 */

{
    svalue_t *v;
    long i;

    if (NULL == register_pointer(ptable, vec))
        return;

    v = vec->item;
    for (i = (long)VEC_SIZE(vec); --i >= 0; v++)
    {
        register_svalue(v);
    }
} /* register_array() */

/*-------------------------------------------------------------------------*/
static void
register_svalue (svalue_t *svp)

/* If <svp> is a struct, array, or mapping, register it in the pointer
 * table, and also register all sub structures.
 */

{
    if (svp->type == T_POINTER || svp->type == T_QUOTED_ARRAY)
    {
        register_array(svp->u.vec);
    }
    else if (svp->type == T_MAPPING)
    {
        register_mapping(svp->u.map);
    }
    else if (svp->type == T_CLOSURE)
    {
        register_closure(svp);
    }
} /* register_svalue() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_save_object (svalue_t *sp, int numarg)

/* VEFUN save_object()
 *
 *   int    save_object (string file, [int version])
 *   string save_object ([int version])
 *
 * Save the variables of the current object to the file <file> (the suffix
 * ".o" will be appended. Returns 0 if the save file could be created,
 * and non-zero otherwise (file could not be written, or current object
 * is destructed).
 *
 * The <file>.o will not be written immediately: first the savefile will
 * be created as <file>.o.tmp, which is after completion renamed to <file>.o.
 *
 * The validity of the filename is checked with a call to check_valid_path().
 *
 * In the second form, the a string with all variables and values is
 * returned directly, or 0 if an error occurs. This string can be used
 * with restore_object() to restore the variable values.
 *
 * In both forms, the optional argument <version> determines the format
 * of the save file. A value of '-1' creates the format native to the
 * driver. Currently the formats 0 and 1 are supported.
 *
 * TODO: "save_object()" looks nice, but maybe call that "save_variables()"?
 */

{
    static char save_object_header[]
      = { '#', SAVE_OBJECT_VERSION, ':', SAVE_OBJECT_HOST, '\n'
        };
      /* The version string to write
       */

    object_t *ob;
      /* The object to save - just a local copy of current_object.
       */
    char *file;
      /* The filename read from the stack, NULL if not saving
       * to a file.
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
    long len;
    int i;
    int f;
    svalue_t *v;
    variable_t *names;

    f = -1;
    file = NULL;
    name = NULL;
    tmp_name = NULL;
    save_version = CURRENT_VERSION;

    switch (numarg)
    {
    case 0:
        strbuf_zero(&save_string_buffer);
        break;

    case 1:
        if (sp->type == T_STRING)
        {
           file = sp->u.string;
        }
        else if (sp->type == T_NUMBER)
        {
            if (sp->u.number < -1 || sp->u.number > CURRENT_VERSION)
            {
                error("Illegal value for arg 1 to save_object(): %ld, "
                      "expected -1..%d\n"
                     , (long)sp->u.number, CURRENT_VERSION
                     );
                /* NOTREACHED */
                return sp;
            }

            strbuf_zero(&save_string_buffer);
            save_version = sp->u.number >= 0 ? sp->u.number
                                             : CURRENT_VERSION;
        }
        else
        {
            bad_xefun_arg(1, sp);
            /* NOTREACHED */
            return sp;
        }
        break;

    case 2:
        if (sp[-1].type != T_STRING)
            bad_xefun_arg(1, sp);
        if (sp->type != T_NUMBER)
            bad_xefun_arg(2, sp);

        file = sp[-1].u.string;

        if (sp->u.number < -1 || sp->u.number > CURRENT_VERSION)
        {
            error("Illegal value for arg 1 to save_object(): %ld, "
                  "expected -1..%d\n"
                 , (long)sp->u.number, CURRENT_VERSION
                 );
            /* NOTREACHED */
            return sp;
        }

        save_version = sp->u.number >= 0 ? sp->u.number
                                         : CURRENT_VERSION;

        /* The main code wants sp == filename */
        sp--;
        numarg--;
        break;

    default:
        fatal("Too many arguments to save_object(): %d, expected 0..2\n"
             , numarg);
    } /* switch(numarg) */

    save_object_header[1] = '0' + save_version;

    /* No need in saving destructed objects */

    ob = current_object;
    if (ob->flags & O_DESTRUCTED)
    {
        if (numarg)
            sp = pop_n_elems(numarg, sp);
        sp++;
        put_number(sp, 0);
        return sp;
    }

    /* If saving to a file, get the proper name and open it
     * The code assumes that sp is the filename argument.
     */
    if (file)
    {
        /* Get a valid filename */

        file = check_valid_path(file, ob, "save_object", MY_TRUE);
        if (file == NULL)
        {
            error("Illegal use of save_object()\n");
            /* NOTREACHED */
            return sp;
        }

        /* Strip a trailing '.c', if any */
        {
            char * p = strrchr(file, '.');
            len = (long)strlen(file);

            if (p && (long)(p - file) + 2 == len && p[1] == 'c')
            {
                char * tmp = alloca((size_t)len - 2);
                if (!tmp)
                {
                    error("Stack overflow in save_object()\n");
                    /* NOTREACHED */
                    return sp;
                }
                memcpy(tmp, file, (size_t)len-2);
                tmp[len-2] = '\0';

                file = tmp;
            }
        }

        /* Create the final and the temporary filename */

        len = (long)strlen(file);
        name = alloca(len + (sizeof save_file_suffix) +
                      len + (sizeof save_file_suffix) + 4);

        if (!name)
        {
            error("Stack overflow in save_object()\n");
            /* NOTREACHED */
            return sp;
        }

        tmp_name = name + len + sizeof save_file_suffix;
        strcpy(name, file);

#ifndef MSDOS_FS
        strcpy(name+len, save_file_suffix);
#endif
        sprintf(tmp_name, "%s.tmp", name);
#ifdef MSDOS_FS
        strcpy(name+len, save_file_suffix);
#endif


        /* Open the file */

#ifdef MSDOS_FS
        /* We have to use O_BINARY with cygwin to be sure we have no CRs
         * the file. See cygwin-faq for more information.
         */
        f = ixopen3(tmp_name, O_CREAT|O_TRUNC|O_WRONLY|O_BINARY, 0640);
#else
        f = ixopen3(tmp_name, O_CREAT|O_TRUNC|O_WRONLY|O_TEXT, 0640);
#endif

        if (f < 0) {
            char * emsg, * buf;

            emsg = strerror(errno);
            buf = alloca(strlen(emsg)+1);
            if (buf)
            {
                strcpy(buf, emsg);
                error("Could not open %s for a save: %s.\n", tmp_name, buf);
            }
            else
            {
                perror("save object");
                error("Could not open %s for a save: errno %d.\n"
                     , tmp_name, errno);
            }
            /* NOTREACHED */
            return sp;
        }
        FCOUNT_SAVE(tmp_name);
    } /* if (file) */

    /* Publish where we are going to save the data (-1 means using
     * the string buffer.
     */
    save_object_descriptor = f;

    /* First pass through the variables to identify arrays/mappings
     * that are used more than once.
     */

    if (ptable)
    {
        debug_message("%s (save_object) Freeing lost pointertable\n", time_stamp());
        free_pointer_table(ptable);
    }

    ptable = new_pointer_table();
    if (!ptable)
    {
        if (file)
        {
            close(f);
            unlink(tmp_name);
        }
        error("(save_object) Out of memory for pointer table.\n");
        /* NOTREACHED */
        return sp;
    }

    v = ob->variables;
    names = ob->prog->variable_names;
    for (i = ob->prog->num_variables; --i >= 0; v++, names++)
    {
        if (names->flags & TYPE_MOD_STATIC)
            continue;

        register_svalue(v);
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
        save_svalue(v, '\n', MY_FALSE);
    }

    free_pointer_table(ptable);
    ptable = NULL;

    if (file)
    {
        /* Finish up the file */

        len =  write( save_object_descriptor
                    , save_object_bufstart
                    , (size_t)(SAVE_OBJECT_BUFSIZE-buf_left));
        if (len != SAVE_OBJECT_BUFSIZE-buf_left )
            failed = MY_TRUE;


        /* On failure, delete the temporary file and return */

        if (failed)
        {
            close(f);
            unlink(tmp_name);
            add_message("Failed to save to file '%s'. Disk could be full.\n", file);
            if (numarg)
                sp = pop_n_elems(numarg, sp);
            sp++;
            put_number(sp, 1);
            return sp;
        }

        /* Delete any existing savefile, then rename the temporary
         * file to the real name.
         */

        i = 0; /* Result from efun */

        unlink(name);
#if !defined(MSDOS_FS) && !defined(AMIGA) && !(defined(OS2) || defined(__EMX__)) && !defined(__BEOS__)
        if (link(tmp_name, name) == -1)
#else
        close(f);
        if (rename(tmp_name,name) < 0)
#endif
        {
            perror(name);
            printf("%s Failed to link %s to %s\n"
                  , time_stamp(), tmp_name, name);
            add_message("Failed to save object !\n");
            i = 1;
        }
#if !defined(MSDOS_FS) && !defined(AMIGA) && !(defined(__EMX__) || defined(OS2)) && !defined(__BEOS__)
        close(f);
        unlink(tmp_name);
#endif

        if (numarg)
            sp = pop_n_elems(numarg, sp);
        sp++;
        put_number(sp, i);
    }
    else
    {
        /* Finish up the operation. Note that there propably is some
         * data pending in the save_buffer.
         */

        if (numarg)
            sp = pop_n_elems(numarg, sp);
        sp++;

        if (failed)
            put_number(sp, 0); /* Shouldn't happen */
        else if (buf_left != SAVE_OBJECT_BUFSIZE)
        {
            /* Data pending in the save_buffer. */
            if (!bytes_written)
            {
                /* Less than SAVE_OBJECT_BUFSIZE bytes generated
                 * we bypass the strbuf for speed.
                 */
                char *s;

                len = SAVE_OBJECT_BUFSIZE-buf_left;

                xallocate(s, len+1, "resulting value string");
                memcpy(s, save_object_bufstart, len);
                s[len] = '\0';
                put_malloced_string(sp, s);
                strbuf_free(&save_string_buffer);
            }
            else
            {
                /* More than SAVE_OBJECT_BUFSIZE of data generated
                 * Fill up the stringbuffer and create the result.
                 */
                strbuf_addn(&save_string_buffer, save_object_bufstart
                       , SAVE_OBJECT_BUFSIZE-buf_left);
                strbuf_store(&save_string_buffer, sp);
            }
        }
        else
            /* The save_buffer[] is empty, what means
             * that at least one buffer full was written into
             * the strbuf.
             */
            strbuf_store(&save_string_buffer, sp);
    } /* if (file or not file) */

    return sp;
} /* f_save_object() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_save_value (svalue_t *sp, int numarg)

/* TEFUN save_value()
 *
 *   string save_value(mixed value, [int version])
 *
 * Encode the <value> into a string suitable for restoration with
 * restore_value() and return it.
 *
 * The created string consists of two lines, each terminated with a newline
 * character: the first line describes the format used to save the value in
 * the '#x:y' notation; the second line is the representation of the value
 * itself.
 *
 * The optional argument <version> determines the format
 * of the save file. A value of '-1' creates the format native to the
 * driver. Currently the formats 0 and 1 are supported.
 */

{
    static char save_value_header[]
      = { '#', SAVE_OBJECT_VERSION, ':', SAVE_OBJECT_HOST, '\n'
        };
      /* The version string to write
       */

    char save_buffer[SAVE_OBJECT_BUFSIZE];
      /* The write buffer.
       */

    /* Set up the globals */
    if (ptable)
    {
        debug_message("%s (save_value) Freeing lost pointer table.\n", time_stamp());
        free_pointer_table(ptable);
    }
    ptable = new_pointer_table();
    if (!ptable)
    {
        error("(save_value) Out of memory for pointer table.\n");
        return sp; /* flow control hint */
    }

    strbuf_zero(&save_string_buffer);
    save_object_descriptor = -1;
    save_version = CURRENT_VERSION;

    /* Evaluate the arguments */
    switch (numarg)
    {
    case 1:
        /* Ok */
        break;

    case 2:
        if (sp->type == T_NUMBER)
        {
            if (sp->u.number < -1 || sp->u.number > CURRENT_VERSION)
            {
                error("Illegal value for arg 1 to save_object(): %ld, "
                      "expected -1..%d\n"
                     , (long)sp->u.number, CURRENT_VERSION
                     );
                /* NOTREACHED */
                return sp;
            }

            save_version = sp->u.number >= 0 ? sp->u.number
                                                    : CURRENT_VERSION;

            sp--;
        }
        else
        {
            bad_xefun_arg(2, sp);
            /* NOTREACHED */
            return sp;
        }
        break;

    default:
        fatal("Illegal number of arguments to save_value(): %d, expected 1..2\n"
             , numarg);
    } /* switch(numarg) */

    save_value_header[1] = '0' + save_version;

    /* First look at the value for arrays and mappings
     */
    register_svalue(sp);

    /* Prepare the actual save */

    failed = MY_FALSE;
    current_sv_id_number = 0;
    bytes_written = 0;
    save_object_bufstart = save_buffer;
    memcpy(save_buffer, save_value_header, sizeof(save_value_header));
    buf_left = SAVE_OBJECT_BUFSIZE - sizeof(save_value_header);
    buf_pnt = save_buffer + sizeof(save_value_header);

    /* Save the value */
    save_svalue(sp, '\n', MY_FALSE);

    /* Finish up the operation. Note that there propably is some
     * data pending in the save_buffer.
     */

    free_svalue(sp);  /* No longer needed */

    if (failed)
        put_number(sp, 0); /* Shouldn't happen */
    else if (buf_left != SAVE_OBJECT_BUFSIZE)
    {
        /* Data pending in the save_buffer. */
        if (!bytes_written)
        {
            /* Less than SAVE_OBJECT_BUFSIZE bytes generated
             * we bypass the strbuf for speed.
             */
            char *s;
            size_t len = SAVE_OBJECT_BUFSIZE-buf_left;

            xallocate(s, len+1, "resulting value string");
            memcpy(s, save_object_bufstart, len);
            s[len] = '\0';
            put_malloced_string(sp, s);
            strbuf_free(&save_string_buffer);
        }
        else
        {
            /* More than SAVE_OBJECT_BUFSIZE of data generated
             * Fill up the stringbuffer and create the result.
             */
            strbuf_addn(&save_string_buffer, save_object_bufstart
                       , SAVE_OBJECT_BUFSIZE-buf_left);
            strbuf_store(&save_string_buffer, sp);
        }
    }
    else
        /* The save_buffer[] is empty, what means
         * that at least one buffer full was written into
         * the strbuf.
         */
        strbuf_store(&save_string_buffer, sp);

    /* Clean up */
    free_pointer_table(ptable);
    ptable = NULL;

    return sp;
} /* f_save_value() */

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
 * The function calls itself and restore_size() recursively
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

        case '#': /* A closure: skip the header and restart this check
                   * again from the data part.
                   */
          {
            pt = strchr(pt, ':');
            if (!pt)
                return -1;
            pt++;
            continue;
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
    shared_restored_values = NULL;
}

/*-------------------------------------------------------------------------*/
INLINE static Bool
restore_mapping (svalue_t *svp, char **str)

/* Restore a mapping from the text starting at *<str> (which points
 * just after the leading '([') and store it into *<svp>.
 * Return TRUE if the restore was successful, FALSE else (*<svp> is
 * set to const0 in that case).
 * On a successful return, *<str> is set to point after the mapping
 * restored.
 */

{
    mapping_t *z;
    svalue_t key, *data;
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

    if (max_mapping_size && siz > max_mapping_size)
    {
        *svp = const0;
        free_shared_restored_values();
        error("Illegal mapping size: %ld.\n", (long int)siz);
        return MY_FALSE;
    }

    /* Allocate the mapping */
    z = allocate_mapping(siz, tmp_par.num_values);

    if (!z)
    {
        *svp = const0;
        free_shared_restored_values();
        error("(restore) Out of memory: mapping[%u, %u]\n"
             , siz, tmp_par.num_values);
        return MY_FALSE;
    }

    svp->type = T_MAPPING;
    svp->u.map = z;

    /* Loop through size and width, restoring the values */
    while (--siz >= 0)
    {
        i = tmp_par.num_values;
        key.type = T_NUMBER;
        if (!restore_svalue(&key, str, (char)(i ? ':' : ',') ))
        {
            free_svalue(&key);
            return MY_FALSE;
        }
        data = get_map_lvalue_unchecked(z, &key);
        if (!data)
        {
            outofmemory("restored mapping entry");
            /* NOTREACHED */
            return MY_FALSE;
        }
        free_svalue(&key);
        while (--i >= 0) {
            if (data->type != T_INVALID && data->type != T_NUMBER)
            {
                /* Duplicate key: this shouldn't happen - but it did */
                free_svalue(data);
            }
            if (!restore_svalue(data++, str, (char)(i ? ';' : ',') ))
                return MY_FALSE;
        }
    }
    *str = tmp_par.str;
    return MY_TRUE;
} /* restore_mapping() */

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
 * The function calls itself and restore_map_size() recursively
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

        case '#': /* A closure: skip the header and restart this check
                   * again from the data part.
                   */
            pt2 = strchr(pt, ':');
            if (!pt2)
                return -1;
            pt = &pt2[2];
              /* 'pt = &pt2[1]' is the logical statement, but
               * would run afoul the '#e:,' operator closure.
               * Because every closure has at least one character
               * following the ':', this solution is safe.
               */
            break;

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
restore_array (svalue_t *svp, char **str)

/* Restore an array from the text starting at *<str> (which points
 * just after the leading '({') and store it into *<svp>.
 * Return TRUE if the restore was successful, FALSE else (*<svp> is
 * set to const0 in that case).
 * On a successful return, *<str> is set to point after the array text
 * restored.
 */

{
    vector_t *v;
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

    if (max_array_size && siz > max_array_size)
    {
        *svp = const0;
        free_shared_restored_values();
        error("Illegal array size: %ld.\n", (long int)siz);
        return MY_FALSE;
    }

    /* Allocate the array */

    *svp = const0; /* in case allocate_array() throws an error */

    v = allocate_array(siz);
    put_array(svp, v);

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
restore_svalue (svalue_t *svp, char **pt, char delimiter)

/* Restore an svalue from the text starting at *<pt> up to the <delimiter>,
 * storing the value in *<svp>.
 * On success, set *<pt> to the character after the <delimiter> and return
 * TRUE, else return FALSE.
 */

{
    char *cp;

    assert_stack_gap();

    switch( *(cp = *pt) )
    {

    case '#':  /* A closure or quoted thing */
      {
        switch(*++cp)
        {
        case 'e': /* An efun closure */
        case 's': /* A sefun closure */
        case 'v': /* A variable closure */
        case 'l': /* A lfun closure */
          {
            char ct = *cp;
            char * name, c;

            /* Parse the name of the closure item */
            if (*++cp != ':')
            {
                *svp = const0;
                return MY_FALSE;
            }

            name = ++cp;
            for(;;)
            {
                if ( !(c = *cp++) )
                {
                    *svp = const0;
                    return MY_FALSE;
                }

                /* Break at the delimiter, but make sure that it's not
                 * part of an operator closure ('#e:,' for example).
                 */
                if (c == delimiter
                 && !(cp[-4] == '#' && cp[-3] == 'e' && cp[-2] == ':')
                   ) break;
            }
            cp[-1] = '\0'; /* Overwrites the delimiter */
            *pt = cp;

            /* Create the proper closure */
            switch (ct)
            {
            case 'e': /* An efun closure */
              {
                char * end;
                int code;
                ident_t *p, *pp;
                Bool found;

                /* Try parsing it as operator closure */
                if ((code = symbol_operator(name, &end)) >= 0)
                {
                    svp->type = T_CLOSURE;
                    code += CLOSURE_EFUN_OFFS;
                    svp->x.closure_type
                      = (short)(  instrs[code - CLOSURE_EFUN_OFFS].Default == -1
                                ? code + CLOSURE_OPERATOR-CLOSURE_EFUN
                                : code);
                    svp->u.ob = ref_object(current_object, "restore_svalue");
                    break;
                }

                /* Not an operator: look for an efun with this name */
                p = make_shared_identifier(name, I_TYPE_GLOBAL, 0);
                if (!p) {
                    *svp = const0;
                    break; /* switch(ct) */
                }

                /* #e: can be used only on identifiers with global visibility
                 * or better. Look along the .inferior chain for such an
                 * identifier. If the identifier happens to be a reserved
                 * word, the better for us.
                 */
                for ( pp = p, found = MY_FALSE
                    ; !found && pp && pp->type > I_TYPE_GLOBAL
                    ; pp = pp->inferior)
                {
                    if (pp->type == I_TYPE_RESWORD)
                    {
                        switch(code = pp->u.code)
                        {
                        default:
                            /* There aren't efuns with reswords as names, and
                             * it is impossible to define local / global vars
                             * or functions with such a name.
                             * Thus, !p->inferior .
                             */
                            code = CLOSURE_OPERATOR;
                            break;
                        case L_IF:
                            code = F_BRANCH_WHEN_ZERO+CLOSURE_OPERATOR;
                            break;
                        case L_DO:
                            code =
                              F_BBRANCH_WHEN_NON_ZERO+CLOSURE_OPERATOR;
                            break;
                        case L_WHILE:
                            /* the politically correct code was already taken,
                             * see above.
                             */
                            code = F_BBRANCH_WHEN_ZERO+CLOSURE_OPERATOR;
                            break;
                        case L_FOREACH:
                            code = F_FOREACH+CLOSURE_OPERATOR;
                            break;
                        case L_CONTINUE:
                            code = F_BRANCH+CLOSURE_OPERATOR;
                            break;
                        case L_DEFAULT:
                            code = F_CSTRING0+CLOSURE_OPERATOR;
                            /* as bogus as we can possibliy get :-) */
                            break;
                        case L_BREAK:
                            code = F_BREAK  + CLOSURE_OPERATOR;
                            break;
                        case L_RETURN:
                            code = F_RETURN  + CLOSURE_OPERATOR;
                            break;
#ifdef SUPPLY_PARSE_COMMAND
                        case L_PARSE_COMMAND:
                            code = F_PARSE_COMMAND  + CLOSURE_OPERATOR;
                            break;
#endif
                        case L_SSCANF:
                            code = F_SSCANF  + CLOSURE_OPERATOR;
                            break;
                        case L_CATCH:
                            code = F_CATCH  + CLOSURE_OPERATOR;
                            break;
                        case L_SWITCH:
                            code = F_SWITCH  + CLOSURE_OPERATOR;
                            break;
                        }
                        found = MY_TRUE;
                        break; /* for() */
                    } /* if (pp is resword) */
                } /* for () */

                /* Did we find a suitable identifier? */
                if (found)
                {
                    svp->type = T_CLOSURE;
                    svp->x.closure_type = code;
                }
                else if (pp)
                {
                    Bool efun_ok = MY_TRUE;

                    /* An attempt to override a nomask simul-efun causes a
                     * privilege violation. If it's denied, the efun can't
                     * be restored.
                     */
                    if (pp->u.global.sim_efun >= 0
                     && simul_efunp[pp->u.global.sim_efun].flags & TYPE_MOD_NO_MASK
                     && pp->u.global.efun >= 0
                     && master_ob
                     && (!EVALUATION_TOO_LONG())
                       )
                    {
                        svalue_t *res;

                        push_volatile_string("nomask simul_efun");
                        push_valid_ob(current_object);
                        push_shared_string(pp->name);
                        res = apply_master(STR_PRIVILEGE, 3);
                        if (!res || res->type != T_NUMBER || res->u.number <= 0)
                        {
                            error("Privilege violation: nomask simul_efun %s\n"
                                 , pp->name);
                            /* NOTREACHED */
                            efun_ok = MY_FALSE;
                        }
                        else if (res->u.number == 0)
                        {
                            efun_ok = MY_FALSE;
                        }
                    }
                    else if (EVALUATION_TOO_LONG())
                    {
                        /* Can't call the master to check */
                        efun_ok = MY_FALSE;
                    }

                    if (efun_ok && pp->u.global.efun >= 0)
                    {
                        code = pp->u.global.efun + CLOSURE_EFUN_OFFS;
                        if (code > LAST_INSTRUCTION_CODE + CLOSURE_EFUN_OFFS)
                        {
                            code =
                              efun_aliases[
                                code - CLOSURE_EFUN_OFFS
                                  - LAST_INSTRUCTION_CODE - 1
                              ] + CLOSURE_EFUN_OFFS;
                        }
                        svp->type = T_CLOSURE;
                        svp->x.closure_type = code;
                    }
                    else
                    {
                        /* Can't restore the efun */
                        *svp = const0;
                    }
                }
                else
                {
                    /* Undefined function */
                    *svp = const0;
                }

                if (svp->type == T_CLOSURE)
                    svp->u.ob = ref_object(current_object, "restore_svalue");

                if (p && p->type == I_TYPE_UNKNOWN)
                    free_shared_identifier(p);

                break;
              }

            case 's': /* A sefun closure */
              {
                ident_t *p, *pp;

                /* Look for an efun with this name */
                p = make_shared_identifier(name, I_TYPE_GLOBAL, 0);
                if (!p) {
                    *svp = const0;
                    break; /* switch(ct) */
                }

                /* #s: can be used only on identifiers with global visibility
                 * or better. Look along the .inferior chain for such an
                 * identifier.
                 */
                for ( pp = p
                    ; pp && pp->type > I_TYPE_GLOBAL
                    ; pp = pp->inferior)
                {
                  NOOP;
                } /* for () */

                /* Did we find a suitable identifier? */
                if (pp && pp->u.global.sim_efun >= 0)
                {
                    svp->type = T_CLOSURE;
                    svp->x.closure_type
                      = pp->u.global.sim_efun + CLOSURE_SIMUL_EFUN_OFFS;
                    svp->u.ob = ref_object(current_object, "restore_svalue");
                }
                else
                {
                    /* Undefined function */
                    *svp = const0;
                }

                if (p && p->type == I_TYPE_UNKNOWN)
                    free_shared_identifier(p);

                break;
              }

            case 'v': /* A variable closure */
              {
                char *str;
                object_t *ob;
                variable_t *var;
                program_t *prog;
                int num_var;
                int n;
                lambda_t *l;

                ob = current_object;
                if (!current_variables
                 || !ob->variables
                 || current_variables < ob->variables
                 || current_variables >= ob->variables + ob->prog->num_variables)
                {
                    /* efun closures are called without changing current_prog
                     * nor current_variables. This keeps the program scope for
                     * variables for calls inside this_object(), but would
                     * give trouble with calling from other ones if it were
                     * not for this test.
                     */
                    current_prog = ob->prog;
                    current_variables = ob->variables;
                }

                /* If the variable exists, it must exist as shared
                 * string.
                 */
                str = findstring(name);
                if (!str)
                {
                    *svp = const0;
                    break; /* switch(ct) */
                }

                prog = current_prog;
                var = prog->variable_names;
                num_var = prog->num_variables;
                for (n = num_var; --n >= 0; var++)
                {
                    if (var->name == str && !(var->flags & NAME_HIDDEN))
                        break;
                }
                if (n < 0)
                {
                    *svp = const0;
                    break; /* switch(ct) */
                }

                n = num_var - n - 1;
                l = xalloc(sizeof *l);
                if (!l)
                {
                    *svp = const0;
                    break; /* switch(ct) */
                }

                l->ob = ref_object(current_object, "symbol_variable");
                l->ref = 1;
                l->function.index = (unsigned short)(n + (current_variables - current_object->variables));

                svp->type = T_CLOSURE;
                svp->x.closure_type = CLOSURE_IDENTIFIER;
                svp->u.lambda = l;

                /* Handle replace_program() */
                if ( !(current_object->prog->flags & P_REPLACE_ACTIVE)
                  || !lambda_ref_replace_program(l, svp->x.closure_type, 0, 0, 0) )
                {
                    current_object->flags |= O_LAMBDA_REFERENCED;
                }
                break;
              }

            case 'l': /* A lfun closure */
              {
                char *str;
                char *super;
                int i;

                /* Check if it's an inherited lfun closure */
                super = strchr(name, '-');
                if (super)
                {
                    *super = '\0';
                    super++;
                }

                /* If the function exists, it must exist as shared
                 * string.
                 */
                str = findstring(name);
                if (!str)
                {
                    *svp = const0;
                    break; /* switch(ct) */
                }

                if (super)
                    i = find_inherited(super, name);
                else
                    i = find_function(str, current_object->prog);

                /* If the function exists and is visible, create the closure
                 * TODO: Handle inherit-conscious closures.
                 */
                if (i >= 0)
                {
                    lambda_t *l;
                    ph_int closure_type;

                    l = xalloc(sizeof *l);
                    if (!l)
                    {
                        *svp = const0;
                        break; /* switch(ct) */
                    }

                    l->ref = 1;
                    l->ob = ref_object(current_object, "restore_svalue");

                    /* Set the closure */
                    if (!(current_object->prog->flags & P_REPLACE_ACTIVE)
                     || !lambda_ref_replace_program( l
                                                   , CLOSURE_ALIEN_LFUN
                                                   , 0, NULL, NULL)
                       )
                    {
                        current_object->flags |= O_LAMBDA_REFERENCED;
                        l->function.index = (unsigned short)i;
                        closure_type = CLOSURE_LFUN;
                    }
                    else
                    {
                        l->function.alien.ob
                          = ref_object(current_object, "restore_svalue");
                        l->function.alien.index = (unsigned short)i;
                        closure_type = CLOSURE_ALIEN_LFUN;
                    }

                    svp->type = T_CLOSURE;
                    svp->x.closure_type = closure_type;
                    svp->u.lambda = l;
                }
                else
                {
                    *svp = const0;
                }
                break;
              }

            default:
                fatal("Unsupported closure-type '%c'\n", ct);
                break;
            }

            return MY_TRUE;
            break;
          }

        case '0': case '1': case '2': case '3': case '4':
        case '5': case '6': case '7': case '8': case '9':
          {
            long quotes;
            char * end;
            Bool   rc;

            quotes = strtol(cp, &end, 10);
            if (!end || end == cp || *end != ':')
            {
                *svp = const0;
                return MY_FALSE;
            }
            *pt = end+1;
            rc = restore_svalue(svp, pt, delimiter);
            if (rc)
            {
                svp->x.quotes = (ph_int)quotes;
                if (svp->type == T_STRING)
                    svp->type = T_SYMBOL;
                else if (svp->type == T_POINTER)
                    svp->type = T_QUOTED_ARRAY;
                return MY_TRUE;
            }
            else
                return MY_FALSE;
            break;
          }

        default:
            *svp = const0;
            return MY_FALSE;
        }

        break;
      }

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
        put_string(svp, make_shared_string(start));
        if (!svp->u.string)
        {
            *svp = const0;
            free_shared_restored_values();
            error("(restore) Out of memory (%lu bytes) for string.\n"
                 , (unsigned long) strlen(start));
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
            {
                return MY_FALSE;
            }
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
            put_number(svp, nega ? -l : l);
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
            svp->x.exponent = (short)tmp;
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
                svalue_t *new;

                max_shared_restored *= 2;
                new = rexalloc(shared_restored_values
                              , sizeof(svalue_t)*max_shared_restored
                              );
                if (!new)
                {
                    current_shared_restored--;
                    free_shared_restored_values();
                    *svp = const0;
                    error("(restore) Out of memory (%lu bytes) for "
                          "%lu shared values.\n"
                          , max_shared_restored * sizeof(svalue_t)
                          , max_shared_restored);
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
    if (delimiter == '\n' && *cp == '\r')
        cp++;
    if (*cp++ != delimiter)
        return MY_FALSE;
    *pt = cp;

    return MY_TRUE;
} /* restore_svalue() */

/*-------------------------------------------------------------------------*/
static Bool
old_restore_string (svalue_t *v, char *str)

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
            put_string(v, make_shared_string(str));
            if (!v->u.string)
            {
                *v = const0;
                free_shared_restored_values();
                error("(restore) Out of memory (%lu bytes) for string\n"
                     , (unsigned long) strlen(str));
            }
            return MY_TRUE;
        }
    }
    *v = const0;
    return MY_FALSE;
} /* old_restore_string() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_restore_object (svalue_t *sp)

/* TEFUN restore_object()
 *
 *   int restore_object (string name)
 *   int restore_object (string str)
 *
 * Restore values of variables for current object from the file <name>,
 * or directly from the string <str>.
 *
 * To restore directly from a string <str>, the string must begin
 * with the typical line "#x:y" as it is created by the save_object()
 * efun.
 *
 * When restoring from a file, the name may end in ".c" which is stripped
 * off by the parser. The master object will probably append a .o to the
 * <name>. The validity of the filename is checked with a call to
 * check_valid_path().
 *
 * Return 1 on success, 0 if there was nothing to restore.
 * TODO: This double-mode is rather ugly, maybe call the restoring
 * TODO:: from string-mode "restore_variables()"?
 */

{
static int nesting = 0;  /* Used to detect recursive calls */
    int restored_version; /* Formatversion of the saved data */
    char *name;      /* Full name of the file to read */
    char *file;      /* Filename passed, NULL if restoring from a string */
    char *var;
    char *buff;      /* Current line read from the savefile
                      * resp. a copy of the string passed.
                      */
    Bool adopted_arg; /* TRUE if buff adopted the memory of sp->u.string */
    char *cur;       /* Current position in the string passed */
    char *space;
    object_t *ob;    /* Local copy of current_object */
    size_t len;
    FILE *f;
    struct stat st;  /* stat() info of the savefile */

    int var_rest;    /* Number of variables left after rover */
    int num_var;     /* Number of variables in the object */
    variable_t *rover = NULL;
      /* Roving pointer through the variable block. The next variable
       * to restore is searched from here, taking advantage of
       * the locality of save_object().
       */

    struct discarded {
        svalue_t v;
        struct discarded *next;
    } * dp = NULL;
      /* List of values for which the variables no longer exist. */

#define FREE_BUFF() MACRO( \
          if (nesting < 2) mb_free(mbFile); \
          if (adopted_arg || nesting > 1) xfree(buff); \
        )
      /* Free buff correctly. Note that adopted_arg and 'nesting < 2'
       * can be true at the same time.
       */

    /* Keep track of recursive calls */
    nesting++;

    /* Test the arguments */
    if (sp->type != T_STRING)
    {
        nesting--;
        bad_xefun_arg(1, sp);
        return sp;
    }

    /* No use in restoring a destructed object, or an object
     * with no variables. Do this check before analysing the
     * arguments, as freeing buff is complicated.
     */
    ob = current_object;
    if (ob->flags & O_DESTRUCTED)
    {
        free_svalue(sp);
        put_number(sp, 0);
        nesting--;
        return sp;
    }

    if (ob->prog->num_variables == 0)
    {
        free_svalue(sp);
        put_number(sp, 1);
        nesting--;
        return sp;
    }

    /* Check if got a filename or the value string itself */
    buff = NULL;
    adopted_arg = MY_FALSE;
    name = NULL;
    file = NULL;
    f = NULL;
    if (sp->u.string[0] == '#')
    {
        /* We need a copy of the value string because we're
         * going to modify it a bit.
         */
        if (sp->x.string_type != STRING_MALLOC)
        {
            len = svalue_strlen(sp);
            buff = (nesting > 1) ? xalloc(len+1) : mb_alloc(mbFile, len+1);
            if (buff == NULL)
            {
                nesting--;
                outofmem(len+1, "copy of value string");
            }
            memcpy(buff, sp->u.string, len);
            buff[len] = '\0';
        }
        else
        {
            /* We can adopt the string */
            buff = sp->u.string;
            put_number(sp, 0);
            adopted_arg = MY_TRUE;
        }
    }
    else
        file = sp->u.string;


    /* If restoring from a file, set it up */

    if (file)
    {
        /* Get a valid filename */

        file = check_valid_path(file, ob, "restore_object", MY_FALSE);
        if (file == NULL)
        {
            nesting--;
            error("Illegal use of restore_object()\n");
            /* NOTREACHED */
            return sp;
        }


        /* Create the full filename */

        len = strlen(file);
        name = alloca(len + (sizeof save_file_suffix));

        if (!name)
        {
            nesting--;
            error("Stack overflow in restore_object()\n");
            /* NOTREACHED */
            return sp;
        }

        strcpy(name, file);
        if (name[len-2] == '.' && name[len-1] == 'c')
            len -= 2;
        strcpy(name+len, save_file_suffix);


        /* Open the file and gets its length */

        f = fopen(name, "r");
        if (!f || fstat(fileno(f), &st) == -1) {
            if (f)
                fclose(f);
            free_svalue(sp);
            put_number(sp, 0);
            nesting--;
            return sp;
        }
        if (st.st_size == 0)
        {
            fclose(f);
            free_svalue(sp);
            put_number(sp, 0);
            nesting--;
            return sp;
        }
        FCOUNT_REST(name);

        /* Allocate the linebuffer. Unfortunately, the whole file
         * can be one single line.
         */

        buff = (nesting > 1) ? xalloc((size_t)(st.st_size + 1))
                             : mb_alloc(mbFile, (size_t)(st.st_size+1));
        if (!buff)
        {
            fclose(f);
            nesting--;
            error("(restore) Out of memory (%lu bytes) for linebuffer.\n"
                 , (unsigned long) st.st_size+1);
            /* NOTREACHED */
            return sp;
        }
    } /* if (file) */

    /* Initialise the variables */

    max_shared_restored = 64;
    current_shared_restored = 0;

    if (shared_restored_values)
    {
        debug_message("(restore) Freeing lost shared_restored_values.\n");
        free_shared_restored_values();
    }

    shared_restored_values = xalloc(sizeof(svalue_t)*max_shared_restored);
    if (!shared_restored_values)
    {
        if (f)
            fclose(f);
        FREE_BUFF();
        nesting--;
        error("(restore) Out of memory (%lu bytes) for shared values.\n"
             , sizeof(svalue_t)*max_shared_restored);
        /* NOTREACHED */
        return sp;
    }

    num_var = ob->prog->num_variables;
    var_rest = 0;
    restored_version = -1;
    restored_host = -1;

    /* Loop until we run out of text to parse */

    cur = buff;
    while(1)
    {
        svalue_t *v;
        char *pt;

        if (file)
        {
            /* Get the next line from the text */
            if (fgets(buff, (int)st.st_size + 1, f) == NULL)
                break;
            cur = buff;
        }
        else if (cur[0] == '\0')
            break;

        /* Remember that we have a newline, and maybe even a CRLF at end of
         * buff!
         */
        pt = strchr(cur, '\r');
        if (pt && pt[1] == '\n') /* Convert a CRLF into a LF */
            *pt = '\n';
        pt = NULL;

        space = strchr(cur, ' ');
        if (!file)
            pt = strchr(cur, '\n');
        else
            pt = NULL;

        if (space == NULL || (!file && pt && pt < space))
        {
            /* No space? It must be the version line! */

            if (cur[0] == '#')
            {
                int i;

                i = sscanf(cur+1, "%d:%d", &restored_version, &restored_host);
                if (i > 0 && (i == 2 || restored_version >= CURRENT_VERSION) )
                {
                    if (pt)
                        cur = pt+1;
                    else if (!file)
                        break;
                    continue;
                }
            }

            /* No version line: illegal format.
             * Deallocate what we allocated so far and return.
             */
            if (f)
                fclose(f);
            if (dp)
                do
                    free_svalue(&dp->v);
                while ( NULL != (dp=dp->next) );

            free_shared_restored_values();
            FREE_BUFF();
            nesting--;
            error("Illegal format (version line) when restoring %s.\n"
                  , file ? name : current_object->name);
            /* NOTREACHED */
            return sp;
        }

        /* Split the line at the position of the space.
         * Left of it is the variable name, to the right is the value.
         */
        *space = '\0';

        /* Set 'v' to the variable to restore */

        v = NULL;

        do { /* A simple try.. environment */

            if ( NULL != (var = findstring(cur)) )
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
                    FREE_BUFF();
                    if (f)
                        fclose(f);
                    if (tmp)
                    {
                        do
                            free_svalue(&tmp->v);
                        while (NULL != (tmp = tmp->next));
                    }
                    nesting--;
                    error("Stack overflow in restore_object()\n");
                    /* NOTREACHED */
                    return sp;
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
        if ( (restored_version < 0 && pt[0] == '\"')
             ? !old_restore_string(v, pt)
             : !restore_svalue(v, &pt, '\n')
           )
        {

            /* Whoops, illegal format */

            if (f)
                fclose(f);
            if (dp)
            {
                do
                    free_svalue(&dp->v);
                while ( NULL != (dp=dp->next) );
            }
            free_shared_restored_values();
            FREE_BUFF();
            nesting--;
            error("Illegal format (value string) when restoring %s.\n"
                 , file ? name : current_object->name);
            /* NOTREACHED */
            return sp;
        }

        cur = pt;
    } /* while(1) */

    /* Restore complete - now clean up */

    if (dp)
    {
        do
            free_svalue(&dp->v);
        while ( NULL != (dp=dp->next) );
    }
    if (d_flag > 1)
        debug_message("%s Object %s restored from %s.\n"
                     , time_stamp(), ob->name, file ? name : "passed value");
    if (f)
        fclose(f);
    free_shared_restored_values();
    FREE_BUFF();

    nesting--;

    free_svalue(sp);
    put_number(sp, 1);
    return sp;

#undef FREE_BUFF
} /* f_restore_object() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_restore_value (svalue_t *sp)

/* TEFUN restore_value()
 *
 *   mixed restore_value (string str)
 *
 * Decode the string representation <str> of a value back into the value
 * itself and return it. <str> is a string as generated by save_value(),
 * the '#x:y' specification of the saveformat however is optional.
 */

{
    int restored_version; /* Formatversion of the saved data */
    char *buff;  /* The string to parse */
    char *p;

    if (sp->type != T_STRING)
    {
        bad_xefun_arg(1, sp);
        return sp; /* flow control hint */
    }

    /* The restore routines will put \0s into the string, so we
     * need to make a copy of all but malloced strings.
     */
    if (sp->x.string_type != STRING_MALLOC)
    {
        size_t len;

        len = strlen(sp->u.string);
        buff = alloca(len+1);
        if (!buff)
        {
            error("(restore) Out of stack (%lu bytes).\n"
                 , (unsigned long) len+1);
            /* NOTREACHED */
            return sp;
        }
        memcpy(buff, sp->u.string, len);
        buff[len] = '\0';
    }
    else
        buff = sp->u.string;

    restored_version = -1;
    restored_host = -1;

    /* Check if there is a version line */
    if (buff[0] == '#')
    {
        int i;

        i = sscanf(buff+1, "%d:%d", &restored_version, &restored_host);

        /* Advance to the next line */
        p = strchr(buff, '\n');
        if (!p)
        {
            error("No data given.\n");
            return sp-1;
        }
        buff = p+1;
    }

    /* Initialise the shared value table */

    max_shared_restored = 64;

    if (shared_restored_values)
    {
        debug_message("(restore) Freeing lost shared_restored_values.\n");
        free_shared_restored_values();
    }

    shared_restored_values = xalloc(sizeof(svalue_t)*max_shared_restored);
    if (!shared_restored_values)
    {
        error("(restore) Out of memory (%lu bytes) for shared values.\n"
             , max_shared_restored * sizeof(svalue_t));
        return sp; /* flow control hint */
    }
    current_shared_restored = 0;

    /* Place the result variable onto the stack */
    inter_sp = ++sp;
    *sp = const0;

    /* Now parse the value in buff[] */

    p = buff;
    if ( (restored_version < 0 && p[0] == '\"')
         ? !old_restore_string(sp, p)
         : !restore_svalue(sp, &p, '\n')
       )
    {
        /* Whoops, illegal format */

        free_shared_restored_values();
        error("Illegal format when restoring a value.\n");
        /* NOTREACHED */
        return sp; /* flow control hint */
    }

    free_shared_restored_values();

    if (*p != '\0')
    {
        error("Illegal format when restoring a value: extraneous characters "
              "at the end.\n");
        /* NOTREACHED */
        return sp; /* flow control hint */
    }

    /* Restore complete - now clean up and return the result */

    inter_sp = --sp;
    free_string_svalue(sp);
    *sp = sp[1];

    return sp;
} /* f_restore_value() */

/***************************************************************************/
