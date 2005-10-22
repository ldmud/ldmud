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
#ifdef F_SET_LIGHT
 *       short           total_light;
#endif
 *       mp_int          time_reset;
 *       mp_int          time_of_ref;
 *       mp_int          load_time;
 *       p_int           load_id;
 *       p_int           extra_ref;            (ifdef DEBUG)
 *       program_t     * prog;
 *       string_t      * name;
 *       string_t      * load_name;
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
#ifdef F_SET_IS_WIZARD
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
 * .name and .load_name are the two names of the object. .name (an untabled
 * string) is the objects 'real' name: something like "std/thing" for
 * a blueprint, and "std/thing#45" for a clone. This name never has
 * a leading '/'. However, this name can be changed with the efun
 * rename_object().
 *
 * The .load_name (a tabled string) is the name of the file from which
 * the object was created. It is identical in both blueprint and clones
 * (in our example "/std/thing") and can't be changed. In compat mode,
 * this name has no leading '/'. However, for virtual objects .load_name
 * is the virtual name - the real program name is .prog->name.
 *
 * Both .name and .load_name never contain a '\0' as part of the name.
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
#include <stdio.h>
#include <fcntl.h>

#include "object.h"

#include "actions.h"
#include "array.h"
#include "backend.h"
#include "closure.h"
#include "comm.h"
#include "exec.h"
#include "filestat.h"
#include "interpret.h"
#include "instrs.h"
#include "main.h"
#include "mapping.h"
#include "mstrings.h"
#include "otable.h"
#include "prolang.h"
#include "ptrtable.h"
#include "random.h"
#include "sent.h"
#include "simulate.h"
#include "simul_efun.h"
#include "stdstrings.h"
#include "strfuns.h"
#include "swap.h"
#include "svalue.h"
#include "wiz_list.h"
#include "xalloc.h"

#include "../mudlib/sys/driver_hook.h"

/*-------------------------------------------------------------------------*/

replace_ob_t *obj_list_replace = NULL;
  /* List of scheduled program replacements.
   */

int tot_alloc_object = 0;
int tot_alloc_object_size = 0;
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
 * At the time of call, the object must be have no refcounts lefts,
 * must be destructed and removed from the object table and list.
 */

{

#ifdef DEBUG

    /* Decrement and check the reference count */

    if (ob->ref > 0)
        fatal("Object with %ld refs passed to _free_object()\n", ob->ref);

    if (d_flag)
        printf("%s free_object: %s.\n", time_stamp(), get_txt(ob->name));

    /* Freeing a non-destructed object should never happen */

    if (!(ob->flags & O_DESTRUCTED)) {
        fatal("Object 0x%lx %s ref count 0, but not destructed.\n"
             , (long)ob, get_txt(ob->name));
    }

#endif /* DEBUG */

    if (ob->sent)
        fatal("Tried to free an object with sentences.\n");

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
            debug_message("%s Free object %s\n", time_stamp(), get_txt(ob->name));
        if (lookup_object_hash(ob->name) == ob)
            fatal("Freeing object %s but name still in name table\n"
                 , get_txt(ob->name));
        tot_alloc_object_size -= mstrsize(ob->name);
        free_mstring(ob->name);
        ob->name = NULL;
    }

    /* Dereference the load_name */
    if (ob->load_name)
    {
        free_mstring(ob->load_name);
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
              , time_stamp(), get_txt(progp->name), progp->ref, from);
}

/*-------------------------------------------------------------------------*/
void
do_free_sub_strings (int num_strings,   string_t **strings
                    ,int num_variables, variable_t *variable_names
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
        free_mstring(strings[i]);

    /* Free all variable names */
    for (i = num_variables; --i >= 0; )
    {
        free_mstring(variable_names[i].name);
    }
}

/*-------------------------------------------------------------------------*/
void
free_prog (program_t *progp, Bool free_sub_strings)

/* Decrement the refcount for program <progp>. If it reaches 0, the program
 * is freed.
 *
 * If free_sub_strings is TRUE, all object strings are freed, and
 * free_prog() is called for all inherited programs.
 *
 * The only case when free_sub_strings is not true, is, when the swapper
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
        printf("%s free_prog: %s\n", time_stamp(), get_txt(progp->name));
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
        bytecode_p program;
        funflag_t *functions;

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
                string_t *name;

                memcpy(
                  &name,
                  FUNCTION_NAMEP(program + (functions[i] & FUNSTART_MASK)),
                  sizeof name
                );
                free_mstring(name);
            }
        }

        /* Free the strings and variable names */
        do_free_sub_strings( progp->num_strings, progp->strings
                           , progp->num_variables, progp->variable_names
                           );

        /* Free all inherited objects */
        for (i = 0; i < progp->num_inherited; i++)
            free_prog(progp->inherit[i].prog, MY_TRUE);

        /* Free the program name */
        total_prog_block_size -= mstrsize(progp->name);
        free_mstring(progp->name);
    }

    /* Remove the program structure */
    xfree(progp);
}

/*-------------------------------------------------------------------------*/
string_t *
function_exists (string_t *fun, object_t *ob)

/* Search for the function <fun> in the object <ob>. If existing, return
 * the name of the program (without added reference), if not return NULL.
 *
 * Visibility rules apply: static and protected functions can't be
 * found from the outside.
 */

{
    string_t *shared_name;
    fun_hdr_p funstart;
    program_t *progp;
    int ix;
    funflag_t flags;

#ifdef DEBUG
    if (ob->flags & O_DESTRUCTED)
        fatal("function_exists() on destructed object\n");
#endif

    /* Make the program resident */
    if (O_PROG_SWAPPED(ob))
    {
        ob->time_of_ref = current_time;
        if (load_ob_from_swap(ob) < 0)
            error("Out of memory: unswap object '%s'\n", get_txt(ob->name));
    }

    shared_name = find_tabled(fun);
    progp = ob->prog;

    /* Check if the function exists at all */
    if ( (ix = find_function(shared_name, progp)) < 0)
        return NULL;

    /* Is it visible for the caller? */
    flags = progp->functions[ix];

    if (flags & TYPE_MOD_PRIVATE
     || (flags & TYPE_MOD_STATIC && current_object != ob))
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
    if (FUNCTION_CODE(funstart)[0] == F_UNDEF)
    {
        return NULL;
    }

    /* We got it. */
    return progp->name;
} /* function_exists() */

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

    if (closure_hook[arg].type == T_CLOSURE)
    {
        lambda_t *l;

        if (arg == H_RESET)
            previous_ob = ob;

        l = closure_hook[arg].u.lambda;
        if (l->function.code[1] && arg != H_RESET)
        {
            /* closure accepts arguments, presumably one, so
             * give it the target object and bind to the current
             * object.
             */
            l->ob = current_object;
            push_ref_object(inter_sp, ob, "reset");
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
            ob->time_reset = (inter_sp->u.number > 0)
                             ? current_time + inter_sp->u.number
                             : 0;

        pop_stack();
    }
    else if (closure_hook[arg].type == T_STRING)
    {
        if (arg == H_RESET)
            previous_ob = ob;

        push_number(inter_sp, arg == H_RESET);
        if (!sapply(closure_hook[arg].u.str, ob, 1) && arg == H_RESET)
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
            tot_alloc_object_size -= i * sizeof(svalue_t[1]);

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
                    j * sizeof(svalue_t[1])
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

        /* Free the old program, finally */
        free_prog(old_prog, MY_TRUE);

#ifdef DEBUG
        if (d_flag)
            debug_message("%s program freed.\n", time_stamp());
#endif

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
    }

    /* Done with the list */
    obj_list_replace = NULL;

#ifdef DEBUG
    if (d_flag)
        debug_message("%s end of replace_programs\n", time_stamp());
#endif

}  /* replace_programs() */

/*-------------------------------------------------------------------------*/
static replace_ob_t *
retrieve_replace_program_entry (void)

/* Auxiliary function to efun replace_program(): test if a program
 * replacement is already scheduled for the current object. If yes,
 * return the pointer to the replace_ob struct, else return NULL.
 */

{
    replace_ob_t *r_ob;

    for (r_ob = obj_list_replace; r_ob; r_ob = r_ob->next)
    {
        if (r_ob->ob == current_object)
            return r_ob;
    }
    return NULL;
}

/*-------------------------------------------------------------------------*/
static program_t *
search_inherited (string_t *str, program_t *prg, int *offpnt)

/* Auxiliary function to efun replace_program(): check if program <str>
 * is inherited by <prg>. If yes, return the originating program and
 * store the (accumulated) variable and function offsets in offpnt[0]
 * and offpnt[1] resp.
 *
 * If the program is not found, return NULL.
 *
 * Nested inherits are handled in a depth search, the function recurses
 * for this.
 */

{
    program_t *tmp;
    int i;
#ifdef DEBUG
    char *ts;
#endif

#ifdef DEBUG
    ts = NULL;
    if (d_flag)
    {
        ts = time_stamp();
        debug_message("%s search_inherited started\n", ts);
        debug_message("%s searching for PRG(%s) in PRG(%s)\n"
                     , ts, get_txt(str), get_txt(prg->name));
        debug_message("%s num_inherited=%d\n", ts, prg->num_inherited);
    }
#endif

    /* Loop through all inherited programs, returning directly when
     * the name program was found.
     */
    for ( i = 0; i < prg->num_inherited; i++)
    {
#ifdef DEBUG
        if (d_flag)
        {
            debug_message("%s index %d:\n", ts, i);
            debug_message("%s checking PRG(%s)\n"
                         , ts, get_txt(prg->inherit[i].prog->name));
        }
#endif
        /* Duplicate virtual inherits don't count */
        if ( prg->inherit[i].inherit_type & INHERIT_TYPE_DUPLICATE )
            continue;

        if (mstreq(str, prg->inherit[i].prog->name ))
        {
#ifdef DEBUG
            if (d_flag)
                debug_message("%s match found\n", ts);
#endif
            offpnt[0] = prg->inherit[i].variable_index_offset;
            offpnt[1] = prg->inherit[i].function_index_offset;
            return prg->inherit[i].prog;
        }
        else if ( NULL != (tmp = search_inherited(str, prg->inherit[i].prog,offpnt)) )
        {
#ifdef DEBUG
            if (d_flag)
                debug_message("%s deferred match found\n", ts);
#endif
            offpnt[0] += prg->inherit[i].variable_index_offset;
            offpnt[1] += prg->inherit[i].function_index_offset;
            return tmp;
        }
    }

#ifdef DEBUG
    if (d_flag)
        debug_message("%s search_inherited failed\n", ts);
#endif

    return NULL;
} /* search_inherited() */

/*-------------------------------------------------------------------------*/
void
tell_npc (object_t *ob, string_t *str)

/* Call the lfun 'catch_tell()' in object <ob> with <str> as argument.
 *
 * This function is used to talk to non-interactive commandgivers
 * (aka NPCs).
 */

{
    push_ref_string(inter_sp, str);
    (void)sapply(STR_CATCH_TELL, ob, 1);
}

/*-------------------------------------------------------------------------*/
void
tell_npc_str (object_t *ob, const char *str)

/* Call the lfun 'catch_tell()' in object <ob> with <str> as argument.
 *
 * This function is used to talk to non-interactive commandgivers
 * (aka NPCs).
 */

{
    push_c_string(inter_sp, str);
    (void)sapply(STR_CATCH_TELL, ob, 1);
}

/*-------------------------------------------------------------------------*/
void
tell_object (object_t *ob, string_t *str)

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
        add_message("%s", get_txt(str));
        command_giver = save_command_giver;
        return;
    }
    tell_npc(ob, str);
}

/*-------------------------------------------------------------------------*/
void
tell_object_str (object_t *ob, const char *str)

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
    tell_npc_str(ob, str);
}

/*-------------------------------------------------------------------------*/
Bool
shadow_catch_message (object_t *ob, const char *str)

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
    push_c_string(inter_sp, str);
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

/*=========================================================================*/
/*                                EFUNS                                    */

/*-------------------------------------------------------------------------*/
svalue_t *
f_function_exists (svalue_t *sp)

/* EXEC function_exists()
 *
 *   string function_exists(string str, object ob)
 *
 * Return the file name of the object that defines the function
 * str in object ob. The returned value can be different from
 * file_name(ob) if the function is defined in an inherited
 * object. In native mode, the returned name always begins with a
 * '/' (absolute path). 0 is returned if the function was not
 * defined, or was defined as static.
 */

{
    string_t *str, *res;

    str = function_exists((sp-1)->u.str, sp->u.ob);
    free_svalue(sp);
    free_svalue(--sp);
    if (str)
    {
        res = cvt_progname(str);

        if (!res)
        {
            sp--;
            inter_sp = sp;
            error("Out of memory\n");
        }
        put_string(sp, res);
    }
    else
    {
        put_number(sp, 0);
    }

    return sp;
} /* f_function_exists() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_functionlist (svalue_t *sp)

/* EFUN functionlist()
 *
 *   mixed *functionlist (object ob, int flags = RETURN_FUNCTION_NAME)
 *
 * Return an array with information about <ob>s lfunctions. For every
 * function, 1 to 4 values (depending on <flags>) are stored in
 * the result array conveying in this order:
 *   - the name of the function
 *   - the function flags (see below)
 *   - the return type (listed in mudlib/sys/lpctypes.h)
 *   - the number of accepted argumens
 *
 * <ob> may be given as true object or as a filename. In the latter
 * case, the efun does not try to load the object before proceeding.
 *
 * <flags> determines both which information is returned for every
 * function, and which functions should be considered at all.
 * Its value is created by bin-or'ing together following flags from
 * mudlib/sys/functionlist.h:
 *
 *   Control of returned information:
 *     RETURN_FUNCTION_NAME    include the function name
 *     RETURN_FUNCTION_FLAGS   include the function flags
 *     RETURN_FUNCTION_TYPE    include the return type
 *     RETURN_FUNCTION_NUMARG  include the number of arguments.
 *
 *     The name RETURN_FUNCTION_ARGTYPE is defined but not implemented.
 *
 *   Control of listed functions:
 *     NAME_INHERITED      don't list if defined by inheritance
 *     TYPE_MOD_STATIC     don't list if static function
 *     TYPE_MOD_PRIVATE    don't list if private
 *     TYPE_MOD_PROTECTED  don't list if protected
 *     NAME_HIDDEN         don't list if not visible through inheritance
 *
 * The 'flags' information consists of the bin-or of the list control
 * flags given above, plus the following:
 *
 *     TYPE_MOD_VARARGS    function takes varargs
 *     NAME_UNDEFINED      function not defined yet, but referenced.
 *     NAME_CROSS_DEFINED  function is defined to be in a different program
 *     TYPE_MOD_NO_MASK    function is nomask
 *     TYPE_MOD_PUBLIC     function is public
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
#define RETURN_FUNCTION_NAME    0x01
#define RETURN_FUNCTION_FLAGS   0x02
#define RETURN_FUNCTION_TYPE    0x04
#define RETURN_FUNCTION_NUMARG  0x08

#define RETURN_FUNCTION_MASK    0x0f  /* union of all RETURN_FUNCTION_ defs */

#define RETURN_FUNCTION_ARGTYPE 0x10 /* not implemented */

    object_t *ob;         /* <ob> argument to list */
    mp_int mode_flags;    /* <flags> argument */
    program_t *prog;      /* <ob>'s program */
    unsigned short num_functions;  /* Number of functions to list */
    char *vis_tags;
      /* Bitflag array describing the visibility of every function in prog
       * in relation to the passed <flags>: */
#define VISTAG_INVIS '\0'  /* Function should not be listed */
#define VISTAG_VIS   '\1'  /* Function matches the <flags> list criterium */
#define VISTAG_ALL   '\2'  /* Function should be listed, no list restrictions */
#define VISTAG_NAMED '\4'  /* Function is neither hidden nor private */

    vector_t *list;       /* Result vector */
    svalue_t *svp;        /* Last element in list which was filled in. */
    uint32 *fun;          /* Current function under examination */
    uint32 active_flags;  /* A functions definition status flags */
    program_t *defprog;   /* Program which actually defines *fun */
    uint32 flags;
    unsigned short *ixp;
    long i, j;

    inter_sp = sp; /* In case of errors leave a clean stack */

    /* Extract the arguments from the vm stack.
     */
    if (sp[-1].type != T_OBJECT)
    {
        if (!(ob = find_object(sp[-1].u.str)))
            error("Object '%s' not found.\n", get_txt(sp[-1].u.str));
    }
    else
        ob = sp[-1].u.ob;

    mode_flags = sp->u.number;

    if (O_PROG_SWAPPED(ob))
        if (load_ob_from_swap(ob) < 0)
        {
            error("Out of memory: unswap object '%s'\n", get_txt(ob->name));
            /* NOTREACHED */
            return NULL;
        }

    prog = ob->prog;

    /* Initialize the vistag[] flag array.
     */
    num_functions = prog->num_functions;
    vis_tags = alloca(num_functions);
    if (!vis_tags)
    {
        error("Stack overflow in functionlist()");
        /* NOTREACHED */
        return NULL;
    }

    /* Preset the visibility. By default, if there is any listing
     * modifier, the functions are not visible. If there is none, the functions
     * are visible.
     */
    memset(
      vis_tags,
      mode_flags &
      (NAME_HIDDEN|TYPE_MOD_PRIVATE|TYPE_MOD_STATIC|TYPE_MOD_PROTECTED|
       NAME_INHERITED) ?
        VISTAG_INVIS :
        VISTAG_ALL  ,
      num_functions
    );

    /* Count how many named functions need to be listed in the result.
     * Flag every function to list in vistag[].
     */
    num_functions = 0;

    /* First, check all functions for which we have a name */
    flags = mode_flags &
        (TYPE_MOD_PRIVATE|TYPE_MOD_STATIC|TYPE_MOD_PROTECTED|NAME_INHERITED);
    fun = prog->functions;
    j = prog->num_function_names;
    for (ixp = prog->function_names + j; --j >= 0; ) {
        i = *--ixp;
        if (!(fun[i] & flags) )
        {
            vis_tags[i] = VISTAG_NAMED|VISTAG_VIS;
            num_functions++;
        }
        else
        {
            vis_tags[i] |= VISTAG_NAMED;
        }
    }

    /* If the user wants to see the hidden or private functions, we loop
     * through the full function table and check all functions not yet
     * touched by the previous 'named' scan.
     * TODO: Due to the dedicated 'find hidden name' loop, this shouldn't
     * TODO:: be necessary, nor the VISTAG_ALL at all.
     */
    if ((mode_flags & (TYPE_MOD_PRIVATE|NAME_HIDDEN)) == 0)
    {
        fun = prog->functions;
        for (i = prog->num_functions; --i >= 0; )
        {
            if (!(vis_tags[i] & VISTAG_NAMED)
             && !(fun[i] & flags)
               )
            {
                vis_tags[i] = VISTAG_VIS;
                num_functions++;
            }
        }
    }

    /* If <flags> accepts all functions, use the total number of functions
     * instead of the count computed above.
     */
    if ( !(mode_flags &
           (NAME_HIDDEN|TYPE_MOD_PRIVATE|TYPE_MOD_STATIC|TYPE_MOD_PROTECTED|
            NAME_INHERITED) ) )
    {
        num_functions = prog->num_functions;
    }

    /* Compute the size of the result vector to
     *  2**(number of RETURN_FUNCTION_ bits set)
     */
    for (i = mode_flags & RETURN_FUNCTION_MASK, j = 0; i; i >>= 1) {
        if (i & 1)
            j += num_functions;
    }

    /* Allocate the result vector and set svp to its end
     */
    list = allocate_array(j);
    svp = list->item + j;

    /* Loop backwards through all functions, check their flags if
     * they are to be listed and store the requested data in
     * the result vector.
     */

    for (i = prog->num_functions, fun += i; --i >= 0; ) {
        fun_hdr_p funstart; /* Pointer to function in the executable */

        fun--;

        if ((vis_tags[i] & (VISTAG_ALL|VISTAG_VIS)) == VISTAG_INVIS)
            continue; /* Don't list this one */

        flags = *fun;

        active_flags = (flags & ~INHERIT_MASK);
        if (!(vis_tags[i] & VISTAG_NAMED))
            active_flags |= NAME_HIDDEN;

        defprog = prog;

        /* If its a cross-defined function, get the flags from
         * real definition and let j point to it.
         */
        if ( !~(flags | ~(NAME_INHERITED|NAME_CROSS_DEFINED) ) ) {
            active_flags |= NAME_CROSS_DEFINED;
            j = (long)CROSSDEF_NAME_OFFSET(flags);
            flags = fun[j];
            j += i;
        } else {
            j = i;
        }

        /* If the function is inherited, find the original definition.
         */
        while (flags & NAME_INHERITED) {
            inherit_t *ip = &defprog->inherit[flags & INHERIT_MASK];

            defprog = ip->prog;
            j -= ip->function_index_offset;
            flags = defprog->functions[j];
        }

        /* defprog now points to the program which really defines
         * the function fun.
         */

        funstart = defprog->program + (flags & FUNSTART_MASK);

        /* Add the data to the result vector as <flags> determines.
         */

        if (mode_flags & RETURN_FUNCTION_NUMARG) {
            svp--;
            svp->u.number = FUNCTION_NUM_ARGS(funstart) & 0x7f;
        }

        if (mode_flags & RETURN_FUNCTION_TYPE) {
            svp--;
            svp->u.number = FUNCTION_TYPE(funstart); /* return type */
        }

        if (mode_flags & RETURN_FUNCTION_FLAGS) {

            /* If the function starts with the bytecodes F_UNDEF,
             * it referenced but undefined. But you know that.
             */
            if (FUNCTION_CODE(funstart)[0] == F_UNDEF)
            {
                active_flags |= NAME_UNDEFINED;
            }
            svp--;
            svp->u.number = (p_int)active_flags;
        }

        if (mode_flags & RETURN_FUNCTION_NAME) {
            svp--;
            svp->type = T_STRING;
            memcpy( &svp->u.str, FUNCTION_NAMEP(funstart)
                  , sizeof svp->u.str);
            (void)ref_mstring(svp->u.str);
        }
    } /* for() */

    /* Cleanup and return */
    free_svalue(sp);
    sp--;
    free_svalue(sp);

    put_array(sp, list);
    return sp;

#undef VISTAG_INVIS
#undef VISTAG_VIS
#undef VISTAG_ALL

#undef RETURN_FUNCTION_NAME
#undef RETURN_FUNCTION_FLAGS
#undef RETURN_FUNCTION_TYPE
#undef RETURN_FUNCTION_NUMARG
#undef RETURN_FUNCTION_ARGTYPE
#undef RETURN_FUNCTION_MASK
} /* f_function_list() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_inherit_list (svalue_t *sp)

/* EFUN inherit_list()
 *
 *   string* inherit_list (object ob = this_object())
 *
 * Return a list with the filenames of all programs inherited by <ob>, include
 * <ob>'s program itself.
 * TODO: Must be fixed so that any number of files can be returned, not just 256.
 */

{
    object_t *ob;           /* Analyzed object */
    vector_t *vec;          /* Result vector */
    svalue_t *svp;          /* Pointer to next vec entry to fill in */
    program_t *pr;          /* Next program to count */
    program_t **prp;        /* Pointer to pr->inherit[x].prog */
      /* Incrementing prp by sizeof(inherit) bytes walks along the
       * the vector of inherited programs.
       */
    program_t *plist[256];  /* Table of found programs */
    int next;               /* Next free entry in plist[] */
    int cur;                /* Current plist[] entry analyzed */

    /* Get the argument */
    ob = sp->u.ob;

    inter_sp = sp;
      /* three possibilities for 'out of memory' follow, so clean
       * up the stack now.
       */

    if (O_PROG_SWAPPED(ob))
        if (load_ob_from_swap(ob) < 0) {
            error("Out of memory: unswap object '%s'\n", get_txt(ob->name));
            /* NOTREACHED */
            return NULL;
        }

    /* Perform a breadth search on ob's inherit tree and store the
     * program pointers into plist[] while counting them.
     */

    plist[0] = ob->prog;
    next = 1;
    for (cur = 0; cur < next; cur++)
    {
        int cnt;
        inherit_t *inheritp;

        pr = plist[cur];
        cnt = pr->num_inherited;
        if (next + cnt > (int)(sizeof plist/sizeof *plist))
            break;

        /* Store the inherited programs in the list.
         */
        for (inheritp = &pr->inherit[0]; cnt--; inheritp++)
        {
            if (inheritp->inherit_type == INHERIT_TYPE_NORMAL)
                plist[next++] = inheritp->prog;
        }
    }

    /* next is also the actual number of files found :-) */
    vec = allocate_array(next);

    /* Take the filenames of the program and copy them into
     * the result vector.
     */
    for (svp = vec->item, prp = plist; --next >= 0; svp++) {
        string_t *str;

        pr = *prp++;

        if (compat_mode)
            str = ref_mstring(pr->name);
        else
            str = add_slash(pr->name);

        if (!str)
        {
            free_array(vec);
            error("(inherit_list) Out of memory: (%lu bytes) for filename\n"
                 , (unsigned long)mstrsize(pr->name));
        }
        put_string(svp, str);
    }

    free_object_svalue(sp);

    put_array(sp, vec);
    return sp;
} /* f_inherit_list() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_load_name (svalue_t *sp)

/* EFUN load_name()
 *
 *   string load_name()
 *   string load_name(object obj)
 *   string load_name(string obj)
 *
 * Return the load name for the object <obj> which may be given
 * directly or by its name.
 *
 * If <obj> is a clone, return the load_name() of <obj>'s blueprint.
 * If <obj> is a blueprint, return the filename from which the
 * blueprint was compiled.
 *
 * If <obj> is given by name but not/no longer existing, the
 * function synthesizes the load name as it should be and returns
 * that. If the given name is illegal, the function returns 0.
 *
 * For virtual objects this efun of course returns the virtual
 * filename.  If <obj> is omitted, the name for the current object is
 * returned.
 *
 * In contrast to the object_name(), the load name can not be changed
 * by with rename_object(). However, if an object uses
 * replace_program() the load name no longer reflects the actual
 * behaviour of an object.
 *
 * The returned name starts with a '/', unless the driver is running
 * in COMPAT mode.
 */

{
    string_t *s;   /* String argument */
    char *name;    /* Result string, maybe 's' itself */
    char *hash;    /* Position of the hash in the name */
    char *mem;     /* Allocated memory blocks */
    object_t *ob;

    /* If the argument is an object, we just need to read the name */
    if (sp->type == T_OBJECT)
    {
        s = sp->u.ob->load_name;
        free_object_svalue(sp);
        put_ref_string(sp, s);
        return sp;
    }

    /* Argument is a string: try to find the object for it */
    s = sp->u.str;
    ob = find_object(s);
    if (ob)
    {
        /* Got it */
        s = ob->load_name;
        free_string_svalue(sp);
        put_ref_string(sp, s);
        return sp;
    }

    /* There is no object for the string argument: just normalize
     * the string. First check if it ends in #<number>.
     */
    mem = NULL;
    hash = strchr(get_txt(s), '#');
    if (!hash)
    {
        /* No '#' at all: make the name sane directly */
        name = (char *)make_name_sane(get_txt(s), !compat_mode);
        if (!name)
            name = get_txt(s);
    }
    else
    {
        char *p;
        size_t len;
        
        /* All characters after the '#' must be digits */
        for (p = hash+1; '\0' != *p; p++)
            if (*p < '0' || *p > '9')
                /* Illegal name: break to return svalue 0 */
                break;

        if ('\0' != *p)
        {
            /* Illegal name: break to return svalue 0 */
            free_string_svalue(sp);
            put_number(sp, 0);
            return sp;
        }

        /* Good, we can slash off the '#<number>' */
        len = (size_t)(hash - get_txt(s));
        p = mem = xalloc(len+1);
        if (!p)
            error("(load_name) Out of memory (%lu bytes) for filename."
                 , (long)len+1);
        strncpy(p, get_txt(s), len);
        p[len] = '\0';

        /* Now make the name sane */
        name = (char *)make_name_sane(p, !compat_mode);
        if (!name)
            name = p;
    }

    /* name now points to the synthesized load_name and
     * may be the argument (== s), allocated (== mem), or
     * points to a static buffer otherwise.
     */

    /* '/.c' is a legal object name, so make sure that
     * the result will be '/' (in compat mode).
     */
    if (compat_mode && '\0' == *name)
        name = "/";

    /* Now return the result */
    if (get_txt(s) != name)
    {
        free_string_svalue(sp);
        put_c_string(sp, name);
    }

    if (mem)
        xfree(mem);
     
    return sp;
} /* f_load_name() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_object_name (svalue_t *sp)

/* EFUN object_name()
 *
 *   string object_name()
 *   string object_name(object ob)
 *
 * Get the name of an object <ob> or, if no argument is given, of
 * the current object.
 *
 * This name is the name under which the object is stored in the
 * muds object table. It is initialised at the creation of the
 * object such that blueprints are named after the file they are
 * compiled from (without the trailing '.c'), and clones receive
 * the name of their blueprint, extended by '#' followed by
 * a unique non-negative number. These rules also apply to
 * virtual objects - the real name/type of virtual objects
 * is ignored.
 *
 * The name of an object can be changed with rename_object(), and
 * object_name() will reflect any of these changes.
 *
 * The returned name always begins with '/' (absolute path),
 * except when the parser runs in COMPAT mode.
 */

{
    string_t *name, *res;

    name = sp->u.ob->name;
    if (compat_mode)
        res = ref_mstring(name);
    else
        res = add_slash(name);
    if (!res)
        error("Out of memory\n");
    free_object_svalue(sp);
    put_string(sp, res);

    return sp;
} /* f_object_name() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_object_time (svalue_t *sp)

/* EFUN object_time()
 *
 *   int object_time()
 *   int object_time(object ob)
 *
 * Returns the creation time of the object.
 * Default is this_object(), if no arg is given.
 */

{
    mp_int load_time;

    load_time = sp->u.ob->load_time;

    free_object_svalue(sp);
    put_number(sp, load_time);

    return sp;
} /* f_object_time() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_program_name (svalue_t *sp)

/* EFUN program_name()
 *
 *   string program_name()
 *   string program_name(object obj)
 *
 * Returns the name of the program of <obj>, resp. the name of the
 * program of the current object if <obj> is omitted.
 *
 * The returned name is usually the name from which the blueprint
 * of <obj> was compiled (the 'load name'), but changes if an
 * object replaces its programs with the efun replace_program().
 *
 * The name always ends in '.c'. It starts with a '/' unless the
 * driver is running in COMPAT mode.
 */

{
    string_t *name, *res;
    object_t *ob;

    ob = sp->u.ob;
    if (O_PROG_SWAPPED(ob))
    {
        ob->time_of_ref = current_time;
        if (load_ob_from_swap(ob) < 0)
        {
            error("Out of memory: unswap object '%s'\n", get_txt(ob->name));
        }
    }
    name = ob->prog->name;
    if (compat_mode)
        res = ref_mstring(name);
    else
        res = add_slash(name);
    if (!res)
        error("Out of memory\n");
    free_object_svalue(sp);
    put_string(sp, res);

    return sp;
} /* f_program_name() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_program_time (svalue_t *sp)

/* EFUN program_time()
 *
 *   int program_time()
 *   int program_time(object ob)
 *
 * Returns the creation (compilation) time of the object's
 * program. Default is this_object(), if no arg is given.
 */

{
    mp_int load_time;

    if (O_PROG_SWAPPED(sp->u.ob))
    {
        sp->u.ob->time_of_ref = current_time;
        if (load_ob_from_swap(sp->u.ob) < 0)
        {
            sp--;
            error("Out of memory: unswap object '%s'\n", get_txt(sp->u.ob->name));
        }
    }
    load_time = sp->u.ob->prog->load_time;

    free_object_svalue(sp);
    put_number(sp, load_time);

    return sp;
} /* f_program_time() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_query_once_interactive (svalue_t *sp)

/* EFUN query_once_interactive()
 *
 *   int query_once_interactive(object ob)
 *
 * True if the object is or once was interactive.
 */

{
    object_t *obj;

    obj = sp->u.ob;
    put_number(sp, obj->flags & O_ONCE_INTERACTIVE ? 1 : 0);
    deref_object(obj, "query_once_interactive");

    return sp;
} /* f_query_once_interactive() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_rename_object (svalue_t *sp)

/* EFUN rename_object()
 *
 *   void rename_object (object ob, string new_name);
 *
 * Give the current object a new file_name. Causes a privilege
 * violation. The new name must not contain a # character, except
 * at the end, to avoid confusion with clone numbers.
 *
 * Raises a privilege violation ("rename_object", this_object(), ob, name).
 */

{
    object_t *ob;
    char     *name;
    string_t *m_name;
    mp_int length;

    inter_sp = sp; /* this is needed for assert_master_ob_loaded(), and for
                    * the possible errors before.
                    */
    ob = sp[-1].u.ob;
    name = get_txt(sp[0].u.str);

    /* Remove leading '/' if any. */
    while(name[0] == '/')
        name++;

    /* Truncate possible .c in the object name. */
    length = strlen(name);
    if (name[length-2] == '.' && name[length-1] == 'c') {
        /* A new writeable copy of the name is needed. */
        char *p;
        p = (char *)alloca(length+1);
        strcpy(p, name);
        name = p;
        name[length -= 2] = '\0';
    }

    {
        char c;
        char *p;
        mp_int i;

        i = length;
        p = name + length;
        while (--i > 0)
        {
            /* isdigit would need to check isascii first... */
            if ( (c = *--p) < '0' || c > '9' )
            {
                if (c == '#' && length - i > 1) {
                    error("Illegal name to rename_object: '%s'.\n", name);
                }
                break;
            }
        }
    }

    assert_master_ob_loaded();
    if (master_ob == ob)
        error("Attempt to rename the master object\n");

    m_name = new_mstring(name);
    if (!m_name)
        error("Out of memory for object name (%ld bytes)\n", (long)strlen(name));

    if (lookup_object_hash(m_name))
    {
        free_mstring(m_name);
        error("Attempt to rename to existing object '%s'\n", name);
    }

    if (privilege_violation4(STR_RENAME_OBJECT, ob, m_name, 0, sp))
    {
        remove_object_hash(ob);
        free_mstring(ob->name);
        ob->name = m_name;
        enter_object_hash(ob);
    }
    else
        free_mstring(m_name);

    free_svalue(sp--);
    free_svalue(sp--);
    return sp;
} /* f_rename_object() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_replace_program (svalue_t *sp)

/* EFUN replace_program()
 *
 *   void replace_program(string program)
 *
 * Substitutes a program with an inherited one. This is useful if you
 * consider the performance and memory consumption of the driver. A
 * program which doesn't need any additional variables and functions
 * (except during creation) can call replace_program() to increase the
 * function-cache hit-rate of
 * the driver which decreases with the number of programs in the
 * system. Any object can call replace_program() but looses all extra
 * variables and functions which are not defined by the inherited
 * program.
 *
 * When replace_program() takes effect, shadowing is stopped on
 * the object since 3.2@166.
 *
 * It is not possible to replace the program of an object after
 * (lambda) closures have been bound to it. It is of course
 * possible to first replace the program and then bind lambda
 * closures to it.
 *
 * The program replacement does not take place with the call to
 * the efun, but is merely scheduled to be carried out at the end
 * of the backend cycle. This may cause closures to have
 * references to then vanished lfuns of the object. This poses no
 * problem as long as these references are never executed after
 * they became invalid.
 */

{
    replace_ob_t *tmp;
    long name_len;
    char *name;
    string_t *sname;
    program_t *new_prog;  /* the replacing program */
    int offsets[2];            /* the offsets of the replacing prog */

    if (!current_object)
        error("replace_program called with no current object\n");
    if (current_object == simul_efun_object)
        error("replace_program on simul_efun object\n");
    if (current_object->flags & O_LAMBDA_REFERENCED)
        error(
          "Cannot schedule replace_program after binding lambda closures\n");

    /* Create the full program name with a trailing '.c' and without
     * a leading '/' to match the internal name representation.
     */
    name_len = (long)mstrsize(sp->u.str);
    name = alloca((size_t)name_len+3);
    strcpy(name, get_txt(sp->u.str));
    if (name[name_len-2] != '.' || name[name_len-1] != 'c')
        strcat(name,".c");
    if (*name == '/')
        name++;
    memsafe(sname = new_mstring(name), name_len+3, "normalized program name");

    new_prog = search_inherited(sname, current_object->prog, offsets);
    if (!new_prog)
    {
        /* Given program not inherited, maybe it's the current already.
         */
        if (mstreq(sname, current_object->prog->name ))
        {
            new_prog = current_object->prog;
            offsets[0] = offsets[1] = 0;
        }
        else
        {
            free_mstring(sname);
            error("program to replace the current one with has "
                  "to be inherited\n");
        }
    }

    /* Program found, now create a new replace program entry, or
     * change an existing one.
     */
    if (!(current_object->prog->flags & P_REPLACE_ACTIVE)
     || !(tmp = retrieve_replace_program_entry()) )
    {
        tmp = xalloc(sizeof *tmp);
        tmp->lambda_rpp = NULL;
        tmp->ob = current_object;
        tmp->next = obj_list_replace;
        obj_list_replace = tmp;
        current_object->prog->flags |= P_REPLACE_ACTIVE;
    }

    tmp->new_prog = new_prog;
    tmp->var_offset = offsets[0];
    tmp->fun_offset = offsets[1];

    free_mstring(sname);

    free_svalue(sp);
    sp--;

    return sp;
} /* f_replace_program() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_set_next_reset (svalue_t *sp)

/* EFUN set_next_reset()
 *
 *   int set_next_reset (int delay)
 *
 * Instruct the gamedriver to reset this object not earlier than in
 * <delay> seconds. If a negative value is given as delay, the object
 * will never reset (useful for blueprints). If 0 is given, the
 * object's reset time is not changed.
 *
 * Result is the former delay to the objects next reset (which can be
 * negative if the reset was overdue).
 */

{
    int new_time;

    new_time = sp->u.number;
    if (current_object->flags & O_DESTRUCTED)
            sp->u.number = 0;
    else
    {
        sp->u.number = current_object->time_reset - current_time;
        if (new_time < 0)
            current_object->time_reset = 0;
        else if (new_time > 0)
            current_object->time_reset = new_time + current_time;
    }
    return sp;
} /* f_set_next_reset() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_tell_object (svalue_t *sp)

/* EFUN tell_object()
 *
 *   void tell_object(object ob, string str)
 *
 * Send a message str to object ob. If it is an interactive
 * object (a user), then the message will go to him (her?),
 * otherwise the lfun catch_tell() of the living will be called
 * with the message as argument.
 */

{
    tell_object((sp-1)->u.ob, sp->u.str);
    free_string_svalue(sp);
    sp--;
    if (sp->type == T_OBJECT) /* not self-destructed */
        free_object_svalue(sp);
    sp--;

    return sp;
} /* f_tell_object() */

/*-------------------------------------------------------------------------*/
#ifdef F_EXPORT_UID

svalue_t *
f_export_uid (svalue_t *sp)

/* EFUN export_uid()
 *
 *   void export_uid(object ob)
 *
 * Set the uid of object ob to the current object's effective uid.
 * It is only possible when object ob has an effective uid of 0.
 * TODO: seteuid() goes through the mudlib, why not this one, too?
 * TODO:: Actually, this efun is redundant, archaic and should
 * TODO:: vanish altogether.
 */

{
    object_t *ob;

    if (!current_object->eff_user)
        error("Illegal to export uid 0\n");
    ob = sp->u.ob;
    if (!ob->eff_user)        /* Only allowed to export when null */
        ob->user = current_object->eff_user;
    free_object(ob, "export_uid");
    sp--;

    return sp;
} /* f_export_uid() */

#endif

/*-------------------------------------------------------------------------*/
#ifdef F_GETEUID

svalue_t *
f_geteuid (svalue_t *sp)

/* EFUN geteuid()
 *
 *   string geteuid(object ob)
 *
 * Get the effective user-id of the object (mostly a wizard or
 * domain name). Standard objects cloned by this object will get
 * that userid. The effective userid is also used for checking access
 * permissions. If ob is omitted, is this_object() as default.
 */

{
    object_t *ob;

     ob = sp->u.ob;

    if (ob->eff_user)
    {
        string_t *tmp;
        tmp = ref_mstring(ob->eff_user->name);
        free_svalue(sp);
        put_string(sp, tmp);
    }
    else
    {
        free_svalue(sp);
        put_number(sp, 0);
    }

    return sp;
} /* f_geteuid() */

#endif

/*-------------------------------------------------------------------------*/
#ifdef F_SETEUID

svalue_t *
f_seteuid (svalue_t *sp)

/* EFUN seteuid()
 *
 *   int seteuid(string str)
 *
 * Set effective uid to str. The calling object must be
 * privileged to do so by the master object. In most
 * installations it can always be set to the current uid of the
 * object, to the uid of the creator of the object file, or to 0.
 *
 * When this value is 0, the current object's uid can be changed
 * by export_uid(), and only then.
 *
 * Objects with euid 0 cannot load or clone other objects.
 */

{
    svalue_t *ret;
    svalue_t *argp;

    argp = sp;
    if (argp->type == T_NUMBER)
    {
        /* Clear the euid of this_object */

        if (argp->u.number != 0)
            efun_arg_error(1, T_STRING, sp->type, sp);
        current_object->eff_user = 0;
        free_svalue(argp);
        put_number(argp, 1);
        return sp;
    }

    /* Call the master to clear this use of seteuid() */

    push_ref_valid_object(sp, current_object, "seteuid");
    push_ref_string(sp, argp->u.str);
    inter_sp = sp;
    ret = apply_master_ob(STR_VALID_SETEUID, 2);
    if (!ret || ret->type != T_NUMBER || ret->u.number != 1)
    {
        if (out_of_memory)
        {
            error("Out of memory\n");
            /* NOTREACHED */
            return sp;
        }
        free_svalue(argp);
        put_number(argp, 0);
    }
    else
    {
        current_object->eff_user = add_name(argp->u.str);
        free_svalue(argp);
        put_number(argp, 1);
    }

    return argp;
} /* f_seteuid() */

#endif

/*-------------------------------------------------------------------------*/
#if defined(F_GETUID) || defined(F_CREATOR)
#ifdef F_GETUID

svalue_t *
f_getuid (svalue_t *sp)

#else

svalue_t *
f_creator (svalue_t *sp)

#endif

/* EFUN getuid()
 *
 *   string getuid(object ob)
 *   string creator(object ob)
 *
 * User-ids are not used in compat mode, instead the uid is
 * then called 'creator'.
 * Get user-id of the object, i.e. the name of the wizard or
 * domain that is responsible for the object. This name is also
 * the name used in the wizlist. If no arg is given, use
 * this_object() as default.
 */

{
    object_t *ob;
    string_t *name;

    ob = sp->u.ob;
    deref_object(ob, "getuid");
    if ( NULL != (name = ob->user->name) )
        put_ref_string(sp, name);
    else
        put_number(sp, 0);

    return sp;
} /* f_getuid() == f_creator() */

#endif

/*=========================================================================*/
/*                             INVENTORIES                                 */

/*-------------------------------------------------------------------------*/
#ifdef F_SET_LIGHT

void
add_light (object_t *p, int n)

/* The light emission of <p> and all surrounding objects is
 * changed by <n>. This is used by the efun set_light() and when
 * moving and destructing objects.
 */

{
    if (n == 0)
        return;
    do {
        p->total_light += n;
    } while ( NULL != (p = p->super) );
} /* add_light() */
#endif

/*-------------------------------------------------------------------------*/
static void
move_object (void)

/* Move the object inter_sp[-1] into object inter_sp[0]; both objects
 * are removed from the stack.
 *
 * The actual move performed by the hooks H_MOVE_OBJECT0/1, this
 * function is called to implement the efuns move_object() and transfer().
 */

{
    lambda_t *l;
    object_t *save_command = command_giver;

    if (NULL != ( l = closure_hook[H_MOVE_OBJECT1].u.lambda) ) {
        l->ob = inter_sp[-1].u.ob;
        call_lambda(&closure_hook[H_MOVE_OBJECT1], 2);
    } else if (NULL != ( l = closure_hook[H_MOVE_OBJECT0].u.lambda) ) {
        l->ob = current_object;
        call_lambda(&closure_hook[H_MOVE_OBJECT0], 2);
    }
    else
        error("Don't know how to move objects.\n");
    command_giver = check_object(save_command);
} /* move_object() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_all_environment (svalue_t *sp, int num_arg)

/* EFUN all_environment()
 *
 *   object *all_environment()
 *   object *all_environment(object o)
 *
 * Returns an array with all environments object <o> is in. If <o>
 * is omitted, the environments of the current object is returned.
 *
 * If <o> has no environment, or if <o> is destructed, 0 is
 * returned.
 */

{
    object_t *o;

    /* Get the arg from the stack, if any */
    if (num_arg)
    {
        o = ref_object(sp->u.ob, "all_environment");
        free_object_svalue(sp);
    }
    else
    {
        o = current_object;
        sp++;
    }


    /* Default return value: 0 */
    put_number(sp, 0);

    if (!(o->flags & O_DESTRUCTED))
    {
        mp_int num;
        object_t *env;
        vector_t *v;
        svalue_t *svp;

        /* Count the number of environments */
        for ( num = 0, env = o->super
            ; NULL != env
            ; num++, env = env->super)
            NOOP;

        if (num)
        {
            /* Get the array and fill it */
            v = allocate_uninit_array(num);
            for ( svp = v->item, env = o->super
                ; NULL != env
                ; svp++, env = env->super)
            {
                put_ref_object(svp, env, "all_environment");
            }

            /* Put the result on the stack and return */
            put_array(sp, v);
        }
    }

    if (num_arg)
        free_object(o, "all_environment");

    return sp;
} /* f_all_environment() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_all_inventory (svalue_t *sp)

/* EFUN all_inventory()
 *
 *     object *all_inventory(object ob = this_object())
 *
 * Returns an array of the objects contained in the inventory
 * of ob.
 */

{
    vector_t *vec;
    object_t *ob;
    object_t *cur;  /* Current inventory object */
    int cnt, res;

    ob = sp->u.ob;

    /* Count how many inventory objects there are. */
    cnt = 0;
    for (cur = ob->contains; cur; cur = cur->next_inv)
        cnt++;

    if (!cnt)
        vec = allocate_array(0);
    else
    {
        vec = allocate_array(cnt);

        /* Copy the object references */
        cur = ob->contains;
        for (res = 0; res < cnt; res++) {
            put_ref_object(vec->item+res, cur, "all_inventory");
            cur = cur->next_inv;
        }
    }

    free_object_svalue(sp);

    if (vec == NULL)
        put_number(sp, 0);
    else
        put_array(sp, vec);
    
    return sp;
} /* f_all_inventory() */

/*-------------------------------------------------------------------------*/
static int
deep_inventory_size (object_t *ob)

/* Helper function for deep_inventory()
 *
 * Count the size of <ob>'s inventory by counting the contained objects,
 * invoking this function for every object and then returning the sum
 * of all numbers.
 */

{
    int n;

    n = 0;
    do {
        if (ob->contains)
            n += deep_inventory_size(ob->contains);
        n++;
    } while ( NULL != (ob = ob->next_inv) );

    return n;
} /* deep_inventory_size() */

/*-------------------------------------------------------------------------*/
static svalue_t *
write_deep_inventory (object_t *first, svalue_t *svp)

/* Helper function for deep_inventory()
 *
 * Copy into <svp> and following a reference to all objects in the
 * inventory chain starting with <first>; then invoke this function
 * for every inventory chain in the found objects.
 *
 * <svp> has to point into a suitably big area of svalue elements, like
 * a vector.
 *
 * Result is the updated <svp>, pointing to the next free svalue element
 * in the storage area.
 */

{
    object_t *ob;

    ob = first;
    do {
        put_ref_object(svp, ob, "deep_inventory");
        svp++;
    } while ( NULL != (ob = ob->next_inv) );

    ob = first;
    do {
        if (ob->contains)
            svp = write_deep_inventory(ob->contains, svp);
    } while ( NULL != (ob = ob->next_inv) );

    return svp;
} /* write_deep_inventory() */

/*-------------------------------------------------------------------------*/
#if !defined(SUPPLY_PARSE_COMMAND) || defined(COMPAT_MODE)
static
#endif
       vector_t *
deep_inventory (object_t *ob, Bool take_top)

/* Return a vector with the full inventory of <ob>, i.e. all objects contained
 * by <ob> and all deep inventories of those objects, too. The resulting
 * vector is created by a recursive breadth search.
 *
 * If <take_top> is true, <ob> itself is included as first element in the
 * result vector.
 *
 * The function is used for the efuns deep_inventory() and parse_command().
 */

{
    vector_t *dinv;  /* The resulting inventory vector */
    svalue_t *svp;   /* Next element to fill in dinv */
    int n;                /* Number of elements in dinv */

    /* Count the contained objects */
    n = take_top ? 1 : 0;
    if (ob->contains) {
        n += deep_inventory_size(ob->contains);
    }

    /* Get the array */
    dinv = allocate_array(n);
    svp = dinv->item;

    /* Fill in <ob> if desired */
    if (take_top) {
        put_ref_object(svp, ob, "deep_inventory");
        svp++;
    }

    /* Fill in the deep inventory */
    if (ob->contains) {
        write_deep_inventory(ob->contains, svp);
    }

    return dinv;
} /* deep_inventory() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_deep_inventory (svalue_t *sp)

/* EFUN deep_inventory()
 *
 *   object *deep_inventory(void)
 *   object *deep_inventory(object ob)
 *
 * Returns an array of the objects contained in the inventory of
 * ob (or this_object() if no arg given) and in the inventories
 * of these objects, climbing down recursively.
 */

{
    vector_t *vec;

    vec = deep_inventory(sp->u.ob, MY_FALSE);

    free_object_svalue(sp);
    put_array(sp, vec);

    return sp;
} /* f_deep_inventory() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_environment (svalue_t *sp, int num_arg)

/* EFUN environment()
 *
 *   object environment(void)
 *   object environment(object obj)
 *   object environment(string obj)
 *
 * Returns the surrounding object of obj (which may be specified
 * by name). If no argument is given, it returns the surrounding
 * of the current object.
 *
 * Destructed objects do not have an environment.
 */

{
    object_t *ob;

    if (num_arg)
    {
        if (sp->type == T_OBJECT)
        {
            ob = sp->u.ob->super;
            free_object_svalue(sp);
        }
        else /* it's a string */
        {
            ob = find_object(sp->u.str);
            if (!ob || ob->super == NULL || (ob->flags & O_DESTRUCTED))
                ob = NULL;
            else
                ob = ob->super;
            free_string_svalue(sp);
        }
    }
    else if (!(current_object->flags & O_DESTRUCTED))
    {
        ob = current_object->super;
        sp++;
    }
    else
    {
        ob = NULL; /* != environment(this_object()) *boggle* */
        sp++;
    }

    if (ob)
        put_ref_object(sp, ob, "environment");
    else
        put_number(sp, 0);

    return sp;
} /* f_environment() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_first_inventory (svalue_t *sp)

/* EFUN first_inventory()
 *
 *   object first_inventory()
 *   object first_inventory(string ob)
 *   object first_inventory(object ob)
 *
 * Get the first object in the inventory of ob, where ob is
 * either an object or the file name of an object. If ob is not
 * given, the current object is assumed.
 */

{
    object_t *ob;

    ob = NULL;
    if (sp->type == T_OBJECT)
    {
        ob = sp->u.ob->contains;
        free_object_svalue(sp);
    }
    else if (sp->type == T_STRING)
    {
        ob = get_object(sp->u.str);
        if (!ob)
            error("No object '%s' for first_inventory()\n", get_txt(sp->u.str));
        free_string_svalue(sp);
        ob = ob->contains;
    }

    if (ob)
        put_ref_object(sp, ob, "first_inventory");
    else
        put_number(sp, 0);

    return sp;
} /* f_first_inventory() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_next_inventory (svalue_t *sp)

/* EFUN next_inventory()
 *
 *   object next_inventory()
 *   object next_inventory(object ob)
 *
 * Get next object in the same inventory as ob. If ob is not
 * given, the current object will be used.
 *
 * This efun is mostly used together with the efun
 * first_inventory().
 */

{
    object_t *ob;

    ob = sp->u.ob;
    free_object(ob, "next_inventory");
    if (ob->next_inv)
        put_ref_object(sp, ob->next_inv, "next_inventory");
    else
        put_number(sp, 0);

    return sp;
} /* f_next_inventory() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_move_object (svalue_t *sp)

/* EFUN move_object()
 *
 *   void move_object(mixed item, mixed dest)
 *
 * The item, which can be a file_name or an object, is moved into
 * it's new environment dest, which can also be file_name or an
 * object.
 *
 * In !compat mode, the only object that can be moved with
 * move_object() is the calling object itself.
 *
 * Since 3.2.1, the innards of move_object() are implemented in
 * the mudlib, using the M_MOVE_OBJECT driver hooks.
 */

{
    object_t *item, *dest;

    inter_sp = sp;

    if ((sp-1)->type == T_STRING)
    {
        item = get_object((sp-1)->u.str);
        if (!item)
            error("Bad arg 1 to move_object(): object '%s' not found.\n"
                 , get_txt(sp[-1].u.str));
        free_string_svalue(sp-1);
        put_ref_object(sp-1, item, "move_object");
    }

    if (sp->type == T_STRING)
    {
        dest = get_object(sp->u.str);
        if (!dest)
            error("Bad arg 2 to move_object(): object '%s' not found.\n"
                 , get_txt(sp[0].u.str));
        free_string_svalue(sp);
        put_ref_object(sp, dest, "move_object");
    }

    /* move_object() reads its arguments directly from the stack */
    move_object();
    sp -= 2;

    return sp;
} /* f_move_object() */

/*-------------------------------------------------------------------------*/
static object_t *
object_present_in (string_t *str, object_t *ob)

/* <ob> is the first object in an environment: test all the objects there
 * if they match the id <str>.
 * <str> may be of the form "<id> <num>" - then the <num>th object with
 * this <id> is returned, it it is found.
 */

{
    svalue_t *ret;
    int   count = 0; /* >0: return the <count>th object */
    int   length;
    char *p, *item;
    string_t *sitem;

    length = mstrsize(str);
    item = get_txt(str);

    /* Check if there is a number in the string.
     * If yes parse it, and use the remainder as 'the' id string.
     */
    p = item + length - 1;
    if (*p >= '0' && *p <= '9')
    {
        while(p > item && *p >= '0' && *p <= '9')
            p--;

        if (p > item && *p == ' ')
        {
            count = atoi(p+1) - 1;
            length = p - item;
        }
    }

    if (length != mstrsize(str))
    {
        memsafe(sitem = new_n_mstring(item, length), length, "id string");
    }
    else
        sitem = ref_mstring(str);

    push_string(inter_sp, sitem); /* free on error */

    /* Now look for the object */
    for (; ob; ob = ob->next_inv)
    {
        push_ref_string(inter_sp, sitem);
        ret = sapply(STR_ID, ob, 1);
        if (ob->flags & O_DESTRUCTED)
        {
            free_mstring(sitem);
            inter_sp--;
            return NULL;
        }

        if (ret == NULL || (ret->type == T_NUMBER && ret->u.number == 0))
            continue;

        if (count-- > 0)
            continue;
        free_mstring(sitem);
        inter_sp--;
        return ob;
    }
    free_mstring(sitem);
    inter_sp--;

    /* Not found */
    return NULL;
} /* object_present_in() */

/*-------------------------------------------------------------------------*/
static object_t *
e_object_present (svalue_t *v, object_t *ob)

/* Implementation of the efun present().
 *
 * Look for an object matching <v> in <ob> and return it if found.
 */

{
    svalue_t *ret;
    object_t *ret_ob;
    Bool specific = MY_FALSE;

    /* Search where? */
    if (!ob)
        ob = current_object;
    else
        specific = MY_TRUE;

    if (ob->flags & O_DESTRUCTED)
        return NULL;

    if (v->type == T_OBJECT)
    {
        /* Oooh, that's easy. */

        if (specific)
        {
            if (v->u.ob->super == ob)
                return v->u.ob;
            else
                return NULL;
        }
        if (v->u.ob->super == ob
         || (v->u.ob->super == ob->super && ob->super != 0))
            return v->u.ob;
        return NULL;
    }

    /* Always search in the object's inventory */
    ret_ob = object_present_in(v->u.str, ob->contains);
    if (ret_ob)
        return ret_ob;

    if (specific)
        return NULL;

    /* Search in the environment of <ob> if it was not specified */
    if (!specific && ob->super)
    {
        /* Is it _the_ environment? */
        push_ref_string(inter_sp, v->u.str);
        ret = sapply(STR_ID, ob->super, 1);
        if (ob->super->flags & O_DESTRUCTED)
            return NULL;
        if (ret && !(ret->type == T_NUMBER && ret->u.number == 0))
            return ob->super;

        /* No, search the other objects here. */
        return object_present_in(v->u.str, ob->super->contains);
    }

    /* Not found */
    return NULL;
} /* e_object_present() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_present (svalue_t *sp, int num_arg)

/* EFUN present()
 *
 *   object present(mixed str)
 *   object present(mixed str, object ob)
 *
 * If an object that identifies (*) to the name ``str'' is present
 * in the inventory or environment of this_object (), then return
 * it. If "str" has the form "<id> <n>" the <n>-th object matching
 * <id> will be returned.
 *
 * "str" can also be an object, in which case the test is much faster
 * and easier.
 *
 * A second optional argument ob is the enviroment where the search
 * for str takes place. Normally this_player() is a good choice.
 * Only the inventory of ob is searched, not its environment.
 */

{
    svalue_t *arg;
    object_t *ob;

    arg = sp - num_arg + 1;

    /* Get the arguments */
    ob = NULL;
    if (num_arg > 1)
    {
        ob = arg[1].u.ob;
        free_svalue(sp--);
    }

    inter_sp = sp;
    ob = e_object_present(arg, ob);

    free_svalue(arg);
    if (ob)
        put_ref_object(sp, ob, "present");
    else
        put_number(sp, 0);
    
    return sp;
} /* f_present() */

/*-------------------------------------------------------------------------*/
static void
e_say (svalue_t *v, vector_t *avoid)

/* Implementation of the EFUN say().
 * <v> is the value to say, <avoid> the array of objects to exclude.
 * If the first element of <avoid> is not an object, the function
 * will store its command_giver object into it.
 */
 
{
    static svalue_t ltmp = { T_POINTER };
    static svalue_t stmp = { T_OBJECT };

    object_t *ob;
    object_t *save_command_giver = command_giver;
    object_t *origin;
    char buff[256];
    char *message;
#define INITIAL_MAX_RECIPIENTS 48
    int max_recipients = INITIAL_MAX_RECIPIENTS;
      /* Current size of the recipients table.
       */
    object_t *first_recipients[INITIAL_MAX_RECIPIENTS];
      /* Initial table of recipients.
       */
    object_t **recipients = first_recipients;
      /* Pointer to the current table of recipients.
       * The end is marked with a NULL entry.
       */
    object_t **curr_recipient = first_recipients;
      /* Next recipient to enter.
       */
    object_t **last_recipients =
                 &first_recipients[INITIAL_MAX_RECIPIENTS-1];
      /* Last entry in the current table.
       */
    object_t *save_again;

    /* Determine the command_giver to use */
    if (current_object->flags & O_ENABLE_COMMANDS)
    {
        command_giver = current_object;
    }
    else if (current_object->flags & O_SHADOW
          && O_GET_SHADOW(current_object)->shadowing)
    {
        command_giver = O_GET_SHADOW(current_object)->shadowing;
    }

    /* Determine the originating object */
    if (command_giver)
    {
        interactive_t *ip;

        if (O_SET_INTERACTIVE(ip, command_giver))
        {
            trace_level |= ip->trace_level;
        }
        origin = command_giver;

        /* Save the commandgiver to avoid, if needed */
        if (avoid->item[0].type == T_NUMBER)
        {
            put_ref_object(avoid->item, command_giver, "say");
        }
    }
    else
        origin = current_object;

    /* Sort the avoid vector for fast lookups
     */
    ltmp.u.vec = avoid;
    avoid = order_alist(&ltmp, 1, 1);
    push_array(inter_sp, avoid); /* in case of errors... */
    avoid = avoid->item[0].u.vec;

    /* Collect the list of propable recipients.
     * First, look in the environment.
     */
    if ( NULL != (ob = origin->super) )
    {
        interactive_t *ip;

        /* The environment itself? */
        if (ob->flags & O_ENABLE_COMMANDS
         || O_SET_INTERACTIVE(ip, ob))
        {
            *curr_recipient++ = ob;
        }

        for (ob = ob->contains; ob; ob = ob->next_inv)
        {
            if (ob->flags & O_ENABLE_COMMANDS
             || O_SET_INTERACTIVE(ip,ob))
            {
                if (curr_recipient >= last_recipients)
                {
                    /* Increase the table */
                    max_recipients *= 2;
                    curr_recipient = alloca(max_recipients * sizeof(object_t *));
                    memcpy( curr_recipient, recipients
                           , max_recipients * sizeof(object_t *) / 2);
                    recipients = curr_recipient;
                    last_recipients = &recipients[max_recipients-1];
                    curr_recipient += (max_recipients / 2) - 1;
                }
                *curr_recipient++ = ob;
            }
        } /* for() */
    } /* if(environment) */

    /* Now check this environment */
    for (ob = origin->contains; ob; ob = ob->next_inv)
    {
        interactive_t *ip;

        if (ob->flags & O_ENABLE_COMMANDS
         || O_SET_INTERACTIVE(ip, ob))
        {
            if (curr_recipient >= last_recipients)
            {
                /* Increase the table */
                max_recipients *= 2;
                curr_recipient = alloca(max_recipients * sizeof(object_t *));
                memcpy( curr_recipient, recipients
                      , max_recipients * sizeof(object_t *) / 2);
                recipients = curr_recipient;
                last_recipients = &recipients[max_recipients-1];
                curr_recipient += (max_recipients / 2) - 1;
            }
            *curr_recipient++ = ob;
        }
    }

    *curr_recipient = NULL;  /* Mark the end of the list */

    /* Construct the message. */

    switch(v->type)
    {
    case T_STRING:
        message = get_txt(v->u.str);
        break;

    case T_OBJECT:
        xstrncpy(buff, get_txt(v->u.ob->name), sizeof buff);
        buff[sizeof buff - 1] = '\0';
        message = buff;
        break;

    case T_NUMBER:
        sprintf(buff, "%ld", v->u.number);
        message = buff;
        break;

    case T_POINTER:
        /* say()'s evil twin: send <v> to all recipients' catch_msg() lfun */
        
        for (curr_recipient = recipients; NULL != (ob = *curr_recipient++) ; )
        {
            if (ob->flags & O_DESTRUCTED)
                continue;
            stmp.u.ob = ob;
            if (assoc(&stmp, avoid) >= 0)
                continue;
            push_ref_array(inter_sp, v->u.vec);
            push_ref_object(inter_sp, origin, "say");
            sapply(STR_CATCH_MSG, ob, 2);
        }
        pop_stack(); /* free avoid alist */
        command_giver = check_object(save_command_giver);
        return;

    default:
        error("Invalid argument to say(): expected '%s', got '%s'.\n"
              , efun_arg_typename(T_POINTER|T_NUMBER|T_STRING|T_OBJECT)
              , typename(v->type));
    }

    /* Now send the message to all recipients */
    
    for (curr_recipient = recipients; NULL != (ob = *curr_recipient++); )
    {
        interactive_t *ip;

        if (ob->flags & O_DESTRUCTED)
            continue;
        stmp.u.ob = ob;
        if (assoc(&stmp, avoid) >= 0)
            continue;
        if (!(O_SET_INTERACTIVE(ip, ob)))
        {
            tell_npc_str(ob, message);
            continue;
        }
        save_again = command_giver;
        command_giver = ob;
        add_message("%s", message);
        command_giver = save_again;
    }

    pop_stack(); /* free avoid alist */
    command_giver = check_object(save_command_giver);
} /* e_say() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_say (svalue_t *sp, int num_arg)

/* EFUN say()
 *
 *   void say(string str)
 *   void say(string str, object exclude)
 *   void say(string str, object *excludes)
 *   void say(mixed *arr)
 *   void say(mixed *arr, object exclude)
 *   void say(mixed *arr, object *excludes)
 *
 * There are two major modes of calling:
 *
 * If the first argument is a string <str>, it will be send to
 * all livings in the current room	except to the initiator.
 *
 * If the first argument is an array <arr>, the lfun catch_msg()
 * of all living objects except the initiator will be called.
 * This array will be given as first argument to the lfun, and
 * the initiating object as the second.
 *
 * By specifying a second argument to the efun one can exclude
 * more objects than just the initiator. If the second argument
 * is a single object <exclude>, both the given object and the
 * initiator are excluded from the call. If the second argument
 * is an array <excludes>, all objects and just the objects in
 * this array are excluded from the call.
 *
 * The 'initiator' is determined according to these rules:
 *   - if the say() is called from within a living object, this
 *     becomes the initiator
 *   - if the say() is called from within a dead object as result
 *     of a user action (i.e. this_player() is valid), this_player()
 *     becomes the initiator.
 *   - Else the object calling the say() becomes the initiator.
 */

{
    static LOCAL_VEC2(vtmp, T_NUMBER, T_OBJECT);
      /* Default 'avoid' array passed to say() giving the object
       * to exclude in the second item. The first entry is reserved
       * for e_say() to insert its command_giver object.
       */

    if (num_arg == 1)
    {
        /* No objects to exclude */

        vtmp.v.item[0].type = T_NUMBER;
          /* this marks the place for the command_giver */
        vtmp.v.item[1].type = T_NUMBER;
          /* nothing to exclude... */
        e_say(sp, &vtmp.v);
    }
    else
    {
        /* We have objects to exclude */

        if ( sp->type == T_POINTER )
        {
            e_say(sp-1, sp->u.vec);
        }
        else /* it's an object */
        {
            vtmp.v.item[0].type = T_NUMBER;
            put_ref_object(vtmp.v.item+1, sp->u.ob, "say");
            e_say(sp-1, &vtmp.v);
        }
        free_svalue(sp--);
    }

    free_svalue(sp--);

    return sp;
} /* f_say() */

/*-------------------------------------------------------------------------*/
static void
e_tell_room (object_t *room, svalue_t *v, vector_t *avoid)

/* Implementation of the EFUN tell_room().
 *
 * Value <v> is sent to all living objects in <room>, except those
 * in <avoid>. <avoid> has to be in order_alist() order.
 */

{
    object_t *ob;
    object_t *save_command_giver;
    int num_recipients = 0;
    object_t *some_recipients[20];
    object_t **recipients;
    object_t **curr_recipient;
    char buff[256], *message;
    static svalue_t stmp = { T_OBJECT, } ;

    /* Like in say(), collect the possible recipients.
     * First count how many there are.
     */
    
    for (ob = room->contains; ob; ob = ob->next_inv)
    {
        interactive_t *ip;

        if ( ob->flags & O_ENABLE_COMMANDS
         ||  O_SET_INTERACTIVE(ip, ob))
        {
            num_recipients++;
        }
    }

    /* Allocate the table */
    if (num_recipients < 20)
        recipients = some_recipients;
    else
        recipients = 
          alloca( (num_recipients+1) * sizeof(object_t *) );

    /* Now fill the table */
    curr_recipient = recipients;
    for (ob = room->contains; ob; ob = ob->next_inv)
    {
        interactive_t *ip;

        if ( ob->flags & O_ENABLE_COMMANDS
         ||  O_SET_INTERACTIVE(ip, ob))
        {
            *curr_recipient++ = ob;
        }
    }

    *curr_recipient = NULL; /* Mark the end of the table */

    /* Construct the message */
    switch(v->type)
    {
    case T_STRING:
        message = get_txt(v->u.str);
        break;

    case T_OBJECT:
        xstrncpy(buff, get_txt(v->u.ob->name), sizeof buff);
        buff[sizeof buff - 1] = '\0';
        message = buff;
        break;

    case T_NUMBER:
        sprintf(buff, "%ld", v->u.number);
        message = buff;
        break;

    case T_POINTER:
      {
        /* say()s evil brother: send <v> to all recipients'
         * catch_msg() lfun
         */
        object_t *origin = command_giver;

        if (!origin)
            origin = current_object;

        for (curr_recipient = recipients; NULL != (ob = *curr_recipient++); )
        {
            if (ob->flags & O_DESTRUCTED)
                continue;
            stmp.u.ob = ob;
            if (assoc(&stmp, avoid) >= 0)
                continue;
            push_ref_array(inter_sp, v->u.vec);
            push_ref_object(inter_sp, origin, "tell_room");
            sapply(STR_CATCH_MSG, ob, 2);
        }
        return;
      }

    default:
        error("Invalid argument to tell_room(): expected '%s', got '%s'.\n"
              , efun_arg_typename(T_POINTER|T_NUMBER|T_STRING|T_OBJECT)
              , typename(v->type));
    }

    /* Now send the message to all recipients */
    
    for (curr_recipient = recipients; NULL != (ob = *curr_recipient++); )
    {
        interactive_t *ip;

        if (ob->flags & O_DESTRUCTED) continue;
        stmp.u.ob = ob;
        if (assoc(&stmp, avoid) >= 0) continue;
        if (!(O_SET_INTERACTIVE(ip, ob)))
        {
            tell_npc_str(ob, message);
            continue;
        }
        save_command_giver = command_giver;
        command_giver = ob;
        add_message("%s", message);
        command_giver = save_command_giver;
    }
} /* e_tell_room() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_tell_room (svalue_t *sp, int num_arg)

/* EFUN tell_room()
 *
 *   void tell_room(string|object ob, string str)
 *   void tell_room(string|object ob, string str, object *exclude)
 *   void tell_room(string|object ob, mixed *msg)
 *   void tell_room(string|object ob, mixed *msg, object *exclude)
 *
 * Send a message str to all living objects in the room ob. ob
 * can also be the name of the room given as a string. If a
 * receiving object is not a interactive user the lfun
 * catch_tell() of the object will be invoked with the message as
 * argument. If living objects define catch_tell(), the string
 * will also be sent to that instead of being written to the
 * user. If the object is given as its filename, the driver
 * looks up the object under that name, loading it if necessary.
 * If array *exclude is given, all objects contained in
 * *exclude are excluded from the message str.
 *
 * If the second arg is an array, catch_msg() will be called in
 * all listening livings.
 */

{
    svalue_t *arg;
    vector_t *avoid;
    object_t *ob;

    arg = sp- num_arg + 1;

    /* Test the arguments */
    if (arg[0].type == T_OBJECT)
        ob = arg[0].u.ob;
    else /* it's a string */
    {
        ob = get_object(arg[0].u.str);
        if (!ob)
            error("Object '%s' not found.\n", get_txt(arg[0].u.str));
    }

    if (num_arg == 2)
    {
        avoid = &null_vector;
    }
    else
    {
        /* Sort the list of objects to exclude for faster
         * operation.
         */

        vector_t *vtmpp;
        static svalue_t stmp = { T_POINTER };

        stmp.u.vec = arg[2].u.vec;
        vtmpp = order_alist(&stmp, 1, MY_TRUE);
        avoid = vtmpp->item[0].u.vec;
        sp->u.vec = avoid; /* in case of an error, this will be freed. */
        sp--;
        vtmpp->item[0].u.vec = stmp.u.vec;
        free_array(vtmpp);
    }

    e_tell_room(ob, sp, avoid);

    if (num_arg > 2)
        free_array(avoid);
    free_svalue(sp--);
    free_svalue(sp--);

    return sp;
} /* f_tell_room() */

/*-------------------------------------------------------------------------*/
#ifdef F_SET_LIGHT

svalue_t *
f_set_light (svalue_t *sp)

/* EFUN set_light()
 *
 * int set_light(int n)
 *
 * An object is by default dark. It can be set to not dark by
 * calling set_light(1). The environment will then also get this
 * light. The returned value is the total number of lights in
 * this room. So if you call set_light(0) it will return the
 * light level of the current object.
 *
 * Note that the value of the argument is added to the light of
 * the current object.
 */

{
    object_t *o1;

    add_light(current_object, sp->u.number);
    o1 = current_object;
    while (o1->super)
        o1 = o1->super;
    sp->u.number = o1->total_light;

    return sp;
} /* f_set_light() */

#endif

/*-------------------------------------------------------------------------*/
svalue_t *
f_set_environment (svalue_t *sp)

/* EFUN set_environment()
 *
 *   void set_environment(object item, object env)
 *
 * The item is moved into its new environment env, which may be 0.
 * This efun is to be used in the H_MOVE_OBJECTx hook, as it does
 * nothing else than moving the item - no calls to init() or such.
 */

{
    object_t *item, *dest;
    object_t **pp, *ob;
    object_t *save_cmd = command_giver;

    /* Get and test the arguments */
    
    item = sp[-1].u.ob;

    if (item->flags & O_SHADOW && O_GET_SHADOW(item)->shadowing)
        error("Can't move an object that is shadowing.\n");

    if (sp->type != T_OBJECT)
    {
        dest = NULL;
    }
    else
    {
        dest = sp->u.ob;
        /* Recursive moves are not allowed. */
        for (ob = dest; ob; ob = ob->super)
            if (ob == item)
                error("Can't move object inside itself.\n");

#       ifdef F_SET_LIGHT
            add_light(dest, item->total_light);
#       endif
        dest->flags &= ~O_RESET_STATE;
    }

    item->flags &= ~O_RESET_STATE; /* touch it */

    if (item->super)
    {
        /* First remove the item out of its current environment */
        Bool okey = MY_FALSE;

        if (item->sent)
        {
            remove_environment_sent(item);
        }

        if (item->super->sent)
            remove_action_sent(item, item->super);

#       ifdef F_SET_LIGHT
            add_light(item->super, - item->total_light);
#       endif

        for (pp = &item->super->contains; *pp;)
        {
            if (*pp != item)
            {
                if ((*pp)->sent)
                    remove_action_sent(item, *pp);
                pp = &(*pp)->next_inv;
                continue;
            }
            *pp = item->next_inv;
            okey = MY_TRUE;
        }

        if (!okey)
            fatal("Failed to find object %s in super list of %s.\n",
                  get_txt(item->name), get_txt(item->super->name));
    }

    /* Now put it into its new environment (if any) */
    item->super = dest;
    if (!dest)
    {
        item->next_inv = NULL;
    }
    else
    {
        item->next_inv = dest->contains;
        dest->contains = item;
    }

    command_giver = check_object(save_cmd);
    free_svalue(sp);
    sp--;
    free_svalue(sp);
    return sp - 1;
} /* f_set_environment() */

/*-------------------------------------------------------------------------*/
#ifdef F_TRANSFER

svalue_t *
f_transfer (svalue_t *sp)

/* EFUN transfer()
 *
 *   int transfer(object item, object dest)
 *
 * This efun is for backward compatibility only. It is only
 * available in compat mode.
 *
 * Move the object "item" to the object "dest". All kinds of
 * tests are done, and a number is returned specifying the
 * result:
 *
 *     0: Success.
 *     1: To heavy for destination.
 *     2: Can't be dropped.
 *     3: Can't take it out of it's container.
 *     4: The object can't be inserted into bags etc.
 *     5: The destination doesn't allow insertions of objects.
 *     6: The object can't be picked up.
 *
 * If an object is transfered to a newly created object, make
 * sure that the new object first is transfered to it's
 * destination.
 *
 * The efun calls add_weight(), drop(), get(), prevent_insert(),
 * add_weight(), and can_put_and_get() where needed.
 */

{
    object_t *ob, *to;
    svalue_t *v_weight, *ret;
    int       weight;
    object_t *from;
    int       result;

    /* Get and test the arguments */
    ob = sp[-1].u.ob;

    if (sp->type == T_OBJECT)
        to = sp->u.ob;
    else /* it's a string */
    {
        to = get_object(sp->u.string);
        if (!to)
            error("Object %s not found.\n", get_txt(sp->u.str));
        free_string_svalue(sp);
        put_ref_object(sp, to, "transfer"); /* for move_object() below */
    }
        
    from = ob->super;
    result = 0; /* Default: success result */

    /* Perform the transfer step by step */
    switch(0){default:

        /* Get the weight of the object
         */
        weight = 0;
        v_weight = sapply(STR_QUERY_WEIGHT, ob, 0);
        if (v_weight && v_weight->type == T_NUMBER)
            weight = v_weight->u.number;

        if (ob->flags & O_DESTRUCTED)
        {
            result = 3;
            break;
        }

        /* If the original place of the object is a living object,
         * then we must call drop() to check that the object can be dropped.
         */
        if (from && (from->flags & O_ENABLE_COMMANDS))
        {
            ret = sapply(STR_DROP, ob, 0);
            if (ret && (ret->type != T_NUMBER || ret->u.number != 0))
            {
                result = 2;
                break;
            }

            /* This should not happen, but we can not trust LPC hackers. :-) */
            if (ob->flags & O_DESTRUCTED)
            {
                result = 2;
                break;
            }
        }

        /* If 'from' is not a room and not a player, check that we may
         * remove things out of it.
         */
        if (from && from->super && !(from->flags & O_ENABLE_COMMANDS))
        {
            ret = sapply(STR_CANPUTGET, from, 0);
            if (!ret || (ret->type != T_NUMBER && ret->u.number != 1)
             || (from->flags & O_DESTRUCTED))
            {
                result = 3;
                break;
            }
        }

        /* If the destination is not a room, and not a player,
         * Then we must test 'prevent_insert', and 'can_put_and_get'.
         */
        if (to->super && !(to->flags & O_ENABLE_COMMANDS))
        {
            ret = sapply(STR_PREVENT_INSERT, ob, 0);
            if (ret && (ret->type != T_NUMBER || ret->u.number != 0))
            {
                result = 4;
                break;
            }

            ret = sapply(STR_CANPUTGET, to, 0);
            if (!ret || (ret->type != T_NUMBER && ret->type != 0)
             || (to->flags & O_DESTRUCTED) || (ob->flags & O_DESTRUCTED))
            {
                result = 5;
                break;
            }
        }

        /* If the destination is a player, check that he can pick it up.
         */
        if (to->flags & O_ENABLE_COMMANDS)
        {
            ret = sapply(STR_GET, ob, 0);
            if (!ret || (ret->type == T_NUMBER && ret->u.number == 0)
             || (ob->flags & O_DESTRUCTED))
            {
                result = 6;
                break;
            }

            /* If it is not a room, correct the total weight in
             * the destination.
             */
            if (to->super && weight)
            {
                /* Check if the destination can carry that much.
                 */
                push_number(inter_sp, weight);
                ret = sapply(STR_ADD_WEIGHT, to, 1);
                if (ret && ret->type == T_NUMBER && ret->u.number == 0)
                {
                    result = 1;
                    break;
                }

                if (to->flags & O_DESTRUCTED)
                {
                    result = 1;
                    break;
                }
            }

            /* If it is not a room, correct the weight in
             * the 'from' object.
             */
            if (from && from->super && weight)
            {
                push_number(inter_sp, -weight);
                (void)sapply(STR_ADD_WEIGHT, from, 1);
            }
        }

        /* When we come here, the move is ok */
    } /* pseudo-switch() */
        
    if (result)
    {
        /* All the applys might have changed these */
        free_svalue(sp);
        free_svalue(sp-1);
    }
    else
    {
        /* The move is ok: do it (and use up both arguments) */
        inter_sp = sp;
        move_object();
    }

    put_number(sp-1, result);
    return sp-1;
} /* f_transfer() */

#endif /* F_TRANSFER */

/*=========================================================================*/
/*                        Save/Restore an Object                           */

/*
 * TODO: The functions don't work properly if an object contains several
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

static Bool save_svalue(svalue_t *, char, Bool);
static void save_array(vector_t *);
static int restore_size(char **str);
INLINE static Bool restore_array(svalue_t *, char **str);
static int restore_svalue(svalue_t *, char **, char);
static void register_array(vector_t *);
static void register_mapping (mapping_t *map);

/*-------------------------------------------------------------------------*/

#define SAVE_OBJECT_BUFSIZE 4096
  /* Size of the read/write buffer.
   */

static const char save_file_suffix[] = ".o";
  /* The suffix of the save file, in an array for easier computations.
   * (sizeof() vs. strlen()+1.
   */

static struct pointer_table *ptable = NULL;
  /* The pointer_table used to register all arrays and mappings.
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
save_string (string_t *src)

/* Write string <src> to the write buffer, but escape all funny
 * characters.
 */

{
    register char c, *cp;
    size_t len;

    L_PUTC_PROLOG

    L_PUTC('\"')
    len = mstrsize(src);
    cp = get_txt(src);
    while ( len-- )
    {
        c = *cp++;
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
        else if (c == '\0')
        {
            c = '0';
            L_PUTC('\\')
        }
        L_PUTC(c)
    }
    L_PUTC('\"')
    L_PUTC_EPILOG
}

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
}

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
        (void)sprintf(source, "%d", m->num_values);
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

    switch(v->type)
    {
    case T_STRING:
        save_string(v->u.str);
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

    default:
      {
        /* Objects and closures can't be saved */
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
svalue_t *
f_save_object (svalue_t *sp, int numarg)

/* EFUN save_object()
 *
 *   int    save_object (string file)
 *   string save_object ()
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
    
    /* Test the arguments */
    if (!numarg)
    {
        strbuf_zero(&save_string_buffer);
    }
    else
      file = get_txt(sp->u.str);
    
    /* No need in saving destructed objects */

    ob = current_object;
    if (ob->flags & O_DESTRUCTED)
    {
        if (numarg)
            free_string_svalue(sp);
        else
            sp++;
        put_number(sp, 0);
        return sp;
    }

    /* If saving to a file, get the proper name and open it
     */
    if (file)
    {
        string_t *sfile;

        /* Get a valid filename */

        sfile = check_valid_path(sp->u.str, ob, STR_SAVE_OBJECT, MY_TRUE);
        if (sfile == NULL)
        {
            error("Illegal use of save_object()\n");
            /* NOTREACHED */
            return sp;
        }


        /* Create the final and the temporary filename */

        len = (long)mstrsize(sfile);
        name = alloca(len + (sizeof save_file_suffix) +
                      len + (sizeof save_file_suffix) + 4);

        if (!name)
        {
            free_mstring(sfile);
            error("Stack overflow in save_object()\n");
            /* NOTREACHED */
            return sp;
        }

        tmp_name = name + len + sizeof save_file_suffix;
        strcpy(name, get_txt(sfile));

#ifndef MSDOS_FS
        strcpy(name+len, save_file_suffix);
#endif
        sprintf(tmp_name, "%s.tmp", name);
#ifdef MSDOS_FS
        strcpy(name+len, save_file_suffix);
#endif

        free_mstring(sfile);

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
        debug_message("(save_object) Freeing lost pointertable\n");
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

        if (v->type == T_POINTER)
            register_array  (v->u.vec);
        else if (v->type == T_MAPPING)
            register_mapping(v->u.map);
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

            var_name = get_txt(names->name);
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
                free_string_svalue(sp);
            else
                sp++;
            put_number(sp, 1);
            return sp;
        }

        /* Delete any existing savefile, then rename the temporary
         * file to the real name.
         */

        i = 0; /* Result from efun */
    
        unlink(name);
#if !defined(MSDOS_FS) && !defined(AMIGA) && !defined(OS2) && !defined(__BEOS__)
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
#if !defined(MSDOS_FS) && !defined(AMIGA) && !defined(OS2) && !defined(__BEOS__)
        close(f);
        unlink(tmp_name);
#endif

        if (numarg)
            free_string_svalue(sp);
        put_number(sp, i);
    }
    else
    {
        /* Finish up the operation. Note that there propably is some
         * data pending in the save_buffer.
         */

        sp++; /* We're returning a result. */
        
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
                len = SAVE_OBJECT_BUFSIZE-buf_left;
                save_object_bufstart[len] = '\0';
                put_c_string(sp, save_object_bufstart);
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
f_save_value (svalue_t *sp)

/* EFUN save_value()
 *
 *   string save_value(mixed value)
 *
 * Encode the <value> into a string suitable for restoration with
 * restore_value() and return it.
 *
 * The created string consists of two lines, each terminated with a newline
 * character: the first line describes the format used to save the value in
 * the '#x:y' notation; the second line is the representation of the value
 * itself.
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
        debug_message("(save_value) Freeing lost pointer table.\n");
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

    /* First look at the value for arrays and mappings
     */
    if (sp->type == T_POINTER)
        register_array(sp->u.vec);
    else if (sp->type == T_MAPPING)
        register_mapping(sp->u.map);

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
            size_t len = SAVE_OBJECT_BUFSIZE-buf_left;

            save_object_bufstart[len] = '\0';
            put_c_string(sp, save_object_bufstart);
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
static INLINE Bool
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
                    case '0': c = '\0';   break;
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
        put_string(svp, new_tabled(start));
        if (!svp->u.str)
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
            put_string(v, new_tabled(str));
            if (!v->u.str)
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
}

/*-------------------------------------------------------------------------*/
svalue_t *
f_restore_object (svalue_t *sp)

/* EFUN restore_object()
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
    int restored_version; /* Formatversion of the saved data */
    char *name;      /* Full name of the file to read */
    char *file;      /* Filename passed, NULL if restoring from a string */
    string_t *var;
    char *buff;      /* Current line read from the savefile
                      * resp. a copy of the string passed.
                      */
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


    /* Check if got a filename or the value string itself */
    buff = NULL;
    name = NULL;
    file = NULL;
    f = NULL;
    if (get_txt(sp->u.str)[0] == '#')
    {
        /* We need a copy of the value string because we're
         * going to modify it a bit.
         */
        len = mstrsize(sp->u.str);
        xallocate(buff, len+1, "copy of value string");
        memcpy(buff, get_txt(sp->u.str), len);
        buff[len] = '\0';
    }
    else
        file = get_txt(sp->u.str);
    
    
    /* No use in restoring a destructed object, or an object
     * with no variables.
     */
    ob = current_object;
    if (ob->flags & O_DESTRUCTED)
    {
        free_svalue(sp);
        put_number(sp, 0);
        if (buff) xfree(buff);
        return sp;
    }

    if (ob->prog->num_variables == 0)
    {
        free_svalue(sp);
        put_number(sp, 1);
        if (buff) xfree(buff);
        return sp;
    }

    /* If restoring from a file, set it up */

    if (file)
    {
        string_t *sfile;

        /* Get a valid filename */

        sfile = check_valid_path(sp->u.str, ob, STR_RESTORE_OBJECT, MY_FALSE);
        if (file == NULL)
        {
            error("Illegal use of restore_object()\n");
            /* NOTREACHED */
            return sp;
        }


        /* Create the full filename */

        len = mstrsize(sfile);
        name = alloca(len + (sizeof save_file_suffix));

        if (!name)
        {
            free_mstring(sfile);
            error("Stack overflow in restore_object()\n");
            /* NOTREACHED */
            return sp;
        }

        strcpy(name, get_txt(sfile));
        if (name[len-2] == '.' && name[len-1] == 'c')
            len -= 2;
        strcpy(name+len, save_file_suffix);

        free_mstring(sfile);

        /* Open the file and gets its length */

        f = fopen(name, "r");
        if (!f || fstat(fileno(f), &st) == -1) {
            if (f)
                fclose(f);
            free_svalue(sp);
            put_number(sp, 0);
            return sp;
        }
        if (st.st_size == 0)
        {
            fclose(f);
            free_svalue(sp);
            put_number(sp, 0);
            return sp;
        }
        FCOUNT_REST(name);

        /* Allocate the linebuffer. Unfortunately, the whole file
         * can be one single line.
         */

        buff = xalloc((size_t)(st.st_size + 1));
        if (!buff)
        {
            fclose(f);
            error("(restore) Out of memory (%lu bytes) for linebuffer.\n"
                 , (unsigned long) st.st_size+1);
            /* NOTREACHED */
            return sp;
        }
    } /* if (file) */

    /* Initialise the variables */

    max_shared_restored = 256;
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
        xfree(buff);
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

        /* Remember that we have a newline at end of buff! */

        space = strchr(cur, ' ');
        if (!file)
            pt = strchr(cur, '\n');
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
            xfree(buff);
            error("Illegal format (version line) when restoring %s.\n"
                  , file ? name : get_txt(current_object->name));
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

            if ( NULL != (var = find_tabled_str(cur)) )
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
                    if (f)
                        fclose(f);
                    if (tmp)
                    {
                        do
                            free_svalue(&tmp->v);
                        while (NULL != (tmp = tmp->next));
                    }
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
            xfree(buff);
            error("Illegal format (value string) when restoring %s.\n"
                 , file ? name : get_txt(current_object->name));
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
                     , time_stamp(), get_txt(ob->name)
                     , file ? name : "passed value");
    if (f)
        fclose(f);
    free_shared_restored_values();
    xfree(buff);

    free_svalue(sp);
    put_number(sp, 1);
    return sp;
} /* f_restore_object() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_restore_value (svalue_t *sp)

/* EFUN restore_value()
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

    /* The restore routines will put \0s into the string, so we
     * need to make a copy of all but malloced strings.
     */
    {
        size_t len;

        len = mstrsize(sp->u.str);
        buff = alloca(len+1);
        if (!buff)
        {
            error("(restore) Out of stack (%lu bytes).\n"
                 , (unsigned long) len+1);
            /* NOTREACHED */
            return sp;
        }
        memcpy(buff, get_txt(sp->u.str), len);
        buff[len] = '\0';
    }

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

    max_shared_restored = 256;

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

    /* Restore complete - now clean up and return the result */

    free_shared_restored_values();
    inter_sp = --sp;
    free_string_svalue(sp);
    *sp = sp[1];

    return sp;
} /* f_restore_value() */

/***************************************************************************/

