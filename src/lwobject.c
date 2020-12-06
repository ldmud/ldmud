/*---------------------------------------------------------------------------
 * Lightweight objects
 *
 *---------------------------------------------------------------------------
 */

#include <assert.h>

#include "closure.h"
#include "dumpstat.h"
#include "exec.h"
#include "gcollect.h"
#include "interpret.h"
#include "lwobject.h"
#include "main.h"
#include "mstrings.h"
#include "object.h"
#include "simulate.h"
#include "stdstrings.h"
#include "swap.h"
#include "wiz_list.h"
#include "xalloc.h"

#include "i-current_object.h"

#include "../mudlib/sys/driver_hook.h"
#include "../mudlib/sys/lwobject_info.h"

/*-------------------------------------------------------------------------*/
void
_free_lwobject (lwobject_t *lwob)

/* Deallocate the lightweight object <lwob>,
 * propertly freeing all its variables.
 */

{
    int num_vars = lwob->prog->num_variables;

    for (int i = 0; i < num_vars; i++)
        free_svalue(lwob->variables + i);

    free_prog(lwob->prog, true);
    xfree(lwob);
} /* _free_lwobject() */

/*-------------------------------------------------------------------------*/
static lwobject_t *
create_empty_lwobject (int num_variables)

/* Allocate a new uninitialized lightweight object.
 * Returns NULL when out of memory.
 */

{
    lwobject_t * result = xalloc(sizeof(lwobject_t) + sizeof(svalue_t) * num_variables);

    if (!result)
        return NULL;

    result->ref = 1;
    result->prog = NULL;
    result->user = NULL;
    result->eff_user = NULL;
    for (int i = 0; i < num_variables; i++)
        result->variables[i] = svalue_number(0);

    return result;
} /* create_empty_lwobject() */

/*-------------------------------------------------------------------------*/
lwobject_t *
create_lwobject (object_t *blueprint)

/* Create a lightweight object of the object <blueprint>.
 * The variables will be zero-initialized.
 */

{
    lwobject_t *result;
    program_t *prog = blueprint->prog;

    result = create_empty_lwobject(prog->num_variables);
    if (!result)
        errorf("Out of memory for new lwobject of '%s'\n", get_txt(prog->name));

    result->prog = prog;
    reference_prog(prog, "create_lwobject");

    push_lwobject(inter_sp, result); /* In case of an error. */
    give_uid_to_lwobject(result, blueprint);
    inter_sp--;

    return result;
} /* create_lwobject() */

/*-------------------------------------------------------------------------*/
lwobject_t *
copy_lwobject (lwobject_t *orig, bool copy_variables)

/* Create a copy of <orig>.
 * If <copy_variables> is false, the variables will be left empty.
 */

{
    lwobject_t *result = create_empty_lwobject(orig->prog->num_variables);
    if (!result)
        errorf("Out of memory for new lwobject of '%s'\n", get_txt(orig->prog->name));

    result->prog = orig->prog;
    reference_prog(orig->prog, "copy_lwobject");
    result->user = orig->user;
    result->eff_user = orig->eff_user;

    if (copy_variables)
    {
        for (int i=0; i < result->prog->num_variables; i++)
            assign_rvalue_no_free(result->variables+i, orig->variables+i);
    }

    return result;
} /* copy_lwobject() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_new_lwobject (svalue_t *sp, int num_arg)

/* EFUN new_lwobject()
 *
 *   lwobject new_lwobject(string name, ...)
 *
 * Create a new lightweight object from the program <name> and return it.
 * Any additional parameters are passed to the H_CREATE_LWOBJECT driver hook.
 */

{
    svalue_t *argp = sp - num_arg + 1;
    object_t *blueprint;
    lwobject_t *result;

    if (strict_euids && current_object.type != T_NUMBER && !get_current_eff_user())
        errorf("Illegal to call new_object() with effective user 0\n");

    /* Get the program via its blueprint. */
    blueprint = get_object(argp->u.str);
    if (!blueprint)
    {
        /* Object vanished? */
        pop_n_elems(num_arg, sp);
        put_number(argp, 0);
        return argp;
    }

    if (blueprint->flags & O_CLONE)
        errorf("Can't create lwobject from a clone\n");

    /* We need the program and might need the variables. */
    if ((blueprint->flags & O_SWAPPED) && load_ob_from_swap(blueprint) < 0)
        errorf("Out of memory: unswap object '%s'\n", get_txt(blueprint->name));

    if (blueprint->prog->flags & P_NO_LIGHTWEIGHT)
        errorf("Lightweight objects from '%s' due to pragma not allowed.\n", get_txt(blueprint->name));

    /* Create the lightweight object. */
    blueprint->time_of_ref = current_time;
    result = create_empty_lwobject(blueprint->prog->num_variables);
    if (!result)
        errorf("Out of memory for new lwobject of '%s'\n", get_txt(sp->u.str));

    result->prog = blueprint->prog;
    reference_prog(result->prog, "v_new_lwobject");

    free_mstring(argp->u.str);
    put_lwobject(argp, result); /* Result, and in case of an error. */
    give_uid_to_lwobject(result, blueprint);

    /* Copy shared variables from blueprint. */
    if (!(blueprint->flags & O_DESTRUCTED))
    {
        int num_vars = result->prog->num_variables;
        variable_t *p_vars = result->prog->variables;
        svalue_t *src_vars = blueprint->variables;
        svalue_t *dst_vars = result->variables;

        for (int i = 0; i < num_vars; i++)
        {
            if (!(p_vars[i].type.t_flags & VAR_INITIALIZED))
            {
                /* In case the UID hook already did something. */
                free_svalue(dst_vars + i);
                assign_rvalue_no_free(dst_vars + i, src_vars + i);
            }
        }
    }

    sapply_lwob_ign_prot(STR_VARINIT, result, 0);

    /* Call the H_CREATE_LWOBJECT hook. */
    if (driver_hook[H_CREATE_LWOBJECT].type == T_CLOSURE)
    {
        lambda_t *l = driver_hook[H_CREATE_LWOBJECT].u.lambda;

        if (l->function.code.num_arg)
        {
            /* Closure accepts at least one argument,
             * so it gets the target object there and we execute
             * it in the context of the creator (current object).
             */
            assign_current_object(&(l->ob), "new_lwobject");

            /* We need to add the lightweight object as an
             * argument before all the others.
             */
            sp++;
            for (int i = 1; i < num_arg; i++)
                sp[1-i] = sp[-i];
            put_ref_lwobject(sp - num_arg + 1, result);

            inter_sp = sp;
            call_lambda(&driver_hook[H_CREATE_LWOBJECT], num_arg);
            pop_stack(); /* Ignore result. */
        }
        else
        {
            /* No arguments, just bind to target */
            free_svalue(&(l->ob));
            put_ref_lwobject(&(l->ob), result);
            call_lambda(&driver_hook[H_CREATE_LWOBJECT], 0);
            inter_sp = pop_n_elems(num_arg, inter_sp); /* arguments & result */
        }
    }
    else if (driver_hook[H_CREATE_LWOBJECT].type == T_STRING)
    {
        sapply_lwob_ign_prot(driver_hook[H_CREATE_LWOBJECT].u.str, result, num_arg-1);
    }

    assert(inter_sp == argp); /* All arguments should be freed by now. */
    return argp;
} /* v_new_lwobject() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_lwobject_info (svalue_t *sp)

/* EFUN lwobject_info()
 *
 *    mixed lwobject_info(lwobject lwob, int what)
 *
 * Return information about the lwobject <lwob>.
 * <what> can either be a configuration option as given to
 * configure_object() or one of the LI_xxx options.
 */

{
    lwobject_t *lwob;
    svalue_t result;

    if (sp[-1].type == T_LWOBJECT)
        lwob = sp[-1].u.lwob;
    else
        lwob = NULL;

    if (!lwob && sp[0].u.number < 0)
        errorf("There is no default value for non-configuration values.\n");

    switch (sp[0].u.number)
    {
        default:
            errorf("Illegal value %"PRIdPINT" for lwobject_info().\n", sp[0].u.number);
            return sp; /* NOTREACHED */

        /* Configuration */
        case LC_EUID:
            if (!lwob)
                errorf("Default value for LC_EUID is not supported.\n");
            if (lwob->eff_user && lwob->eff_user->name)
                put_ref_string(&result, lwob->eff_user->name);
            else
                put_number(&result, 0);
            break;

        /* Lightweight Object Statistics */
        case LI_LWOBJECT_REFS:
            put_number(&result, lwob->ref);
            break;

        case LI_DATA_SIZE:
        case LI_DATA_SIZE_TOTAL:
        {
            mp_int totalsize, datasize;

            datasize = data_size_vec(lwob->variables, lwob->prog->num_variables, &totalsize);

            put_number(&result, (sp[0].u.number == LI_DATA_SIZE) ? datasize : totalsize);
            break;
        }

        /* Program flags */
        case LI_NO_INHERIT:
            put_number(&result, (lwob->prog->flags & P_NO_INHERIT) ? 1 : 0);
            break;

        case LI_NO_CLONE:
            put_number(&result, (lwob->prog->flags & P_NO_CLONE) ? 1 : 0);
            break;

        case LI_NO_LIGHTWEIGHT:
            put_number(&result, (lwob->prog->flags & P_NO_LIGHTWEIGHT) ? 1 : 0);
            break;

        case LI_SHARE_VARIABLES:
            put_number(&result, (lwob->prog->flags & P_SHARE_VARIABLES) ? 1 : 0);
            break;

        /* Program Statistics */
        case LI_PROG_REFS:
            put_number(&result, lwob->prog->ref);
            break;

        case LI_NUM_FUNCTIONS:
            put_number(&result, lwob->prog->num_functions);
            break;

        case LI_NUM_VARIABLES:
            put_number(&result, lwob->prog->num_variables);
            break;

        case LI_NUM_STRINGS:
            put_number(&result, lwob->prog->num_strings);
            break;

        case LI_NUM_INHERITED:
            {
                /* Need to filter artificial entries. */
                int i = lwob->prog->num_inherited;
                int cnt = 0;
                inherit_t *inheritp;

                for (inheritp = lwob->prog->inherit; i--; inheritp++)
                {
                    if (inheritp->inherit_type == INHERIT_TYPE_NORMAL
                     || inheritp->inherit_type == INHERIT_TYPE_VIRTUAL
                       )
                        cnt++;
                }

                put_number(&result, cnt);
                break;
            }

        case LI_NUM_INCLUDED:
            put_number(&result, lwob->prog->num_includes);
            break;

        case LI_SIZE_FUNCTIONS:
            put_number(&result, (p_int)
                ( lwob->prog->num_functions      * sizeof(*lwob->prog->functions)
                + lwob->prog->num_function_names * sizeof(*lwob->prog->function_names)));
            break;

        case LI_SIZE_VARIABLES:
            put_number(&result, (p_int)
                ( lwob->prog->num_variables      * sizeof(*lwob->prog->variables)));
            break;

        case LI_SIZE_STRINGS:
            put_number(&result, (p_int)
                ( lwob->prog->num_strings        * sizeof(*lwob->prog->strings)));
            break;

        case LI_SIZE_STRINGS_DATA:
        case LI_SIZE_STRINGS_DATA_TOTAL:
        {
            mp_int size, total, overhead;

            size = program_string_size(lwob->prog, &overhead, &total);

            put_number(&result, (sp[0].u.number == LI_SIZE_STRINGS_DATA) ? size : total);
            break;
        }

        case LI_SIZE_INHERITED:
            put_number(&result, (p_int)
                ( lwob->prog->num_inherited      * sizeof(*lwob->prog->inherit)));
            break;

        case LI_SIZE_INCLUDED:
            put_number(&result, (p_int)
                ( lwob->prog->num_includes       * sizeof(*lwob->prog->includes)));
            break;

        case LI_PROG_SIZE:
            put_number(&result, (long)(PROGRAM_END(*lwob->prog) - lwob->prog->program));
            break;

        case LI_PROG_SIZE_TOTAL:
            put_number(&result, lwob->prog->total_size);
            break;
    }

    sp = pop_n_elems(2, sp);

    sp++;
    *sp = result;
    return sp;
} /* f_lwobject_info() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_configure_lwobject (svalue_t *sp)

/* EFUN configure_lwobject()
 *
 *    void configure_lwobject(lwobject lwob, int what, mixed data)
 *
 * Configures several aspects of the lightweight object <lwob>,
 * or the default for all objects if <ob> is 0.
 *
 * If the first argument <ob> is not this_object(), the privilege violation
 * ("configure_lwobject", this_object(), lwob, what, data) occurs.
 */

{
    lwobject_t *lwob;

    if (sp[-2].type == T_LWOBJECT)
        lwob = sp[-2].u.lwob;
    else
        lwob = NULL;

    if (is_current_object_destructed()
     || ((!lwob || lwob != get_current_lwobject() || sp[-1].u.number == LC_EUID)
      && !privilege_violation_n(STR_CONFIGURE_LWOBJECT, sp[-2], sp, 2)))
    {
        sp = pop_n_elems(3, sp);
        return sp;
    }

    switch (sp[-1].u.number)
    {
        default:
            errorf("Illegal value %"PRIdPINT" for configure_lwobject().\n", sp[-1].u.number);
            return sp; /* NOTREACHED */

        case LC_EUID:
            if (!lwob)
                errorf("Default value for OC_EUID is not supported.\n");

            if (sp->type == T_STRING)
                lwob->eff_user = add_name(sp->u.str);
            else if (sp->type == T_NUMBER && sp->u.number == 0)
                lwob->eff_user = 0;
            else
                efun_arg_error(2, T_STRING, sp->type, sp);
        break;
    }

    sp = pop_n_elems(3, sp);
    return sp;

} /* f_configure_lwobject() */

#ifdef GC_SUPPORT

/*-------------------------------------------------------------------------*/
lwobject_t*
new_sample_lwobject ()

/* Creates a sample lightweight object for the garbage collection to recognize.
 * This object must be destroyed with free_sample_lwobject().
 */

{
    return create_empty_lwobject(0);
} /* new_sample_lwobject() */

/*-------------------------------------------------------------------------*/
void
free_sample_lwobject (lwobject_t* lwob)

/* Free the sample lightweight object created by new_sample_lwobject().
 */

{
    xfree(lwob);
} /* free_sample_lwobject() */

/*-------------------------------------------------------------------------*/
void
clear_lwobject_ref (lwobject_t *lwob)

/* Clear all references held by <lwob>.
 */

{
    if (lwob->ref != 0)
    {
        clear_memory_reference(lwob);
        lwob->ref = 0;

        clear_program_ref(lwob->prog, true);
        clear_ref_in_vector(lwob->variables, lwob->prog->num_variables);
    }
} /* clear_lwobject_ref() */

/*-------------------------------------------------------------------------*/
void
count_lwobject_ref (lwobject_t *lwob)

/* Add a reference to <lwob> and count additional references.
 */

{
    lwob->ref++;
    if (test_memory_reference(lwob))
    {
        note_malloced_block_ref(lwob);

        mark_program_ref(lwob->prog);
        count_ref_in_vector(lwob->variables, lwob->prog->num_variables);
    }
} /* count_lwobject_ref() */

#endif /* GC_SUPPORT */
