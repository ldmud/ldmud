/*---------------------------------------------------------------------------
 * Coroutines
 *
 *---------------------------------------------------------------------------
 */

#include <assert.h>
#include <stdio.h>

#include "closure.h"
#include "coroutine.h"
#include "gcollect.h"
#include "interpret.h"
#include "swap.h"
#include "xalloc.h"

#include "i-current_object.h"

/*-------------------------------------------------------------------------*/
long num_coroutines = 0;
long total_coroutine_size = 0;

/*-------------------------------------------------------------------------*/
static void
clear_coroutine (coroutine_t *cr, bool clear_variables)

/* Clear all fields in the <cr>, except the variables (if <clear_variables>
 * is false) and the awaiter/awaitee entries.
 */

{
    free_svalue(&cr->ob);
    if (cr->prog)
        free_prog(cr->prog, true);

    if (cr->closure)
    {
        svalue_t svp = { T_CLOSURE, {.closure_type = CLOSURE_LFUN}, {.lfun_closure = cr->closure } };
        free_closure(&svp);
        cr->closure = NULL;
    }

    if (clear_variables)
    {
        int num_vars = cr->num_variables;
        int num_vals = cr->num_values;

        if (num_vals > CR_RESERVED_EXTRA_VALUES)
        {
            svalue_t *extra = cr->variables[cr->num_variables].u.lvalue;
            for (int i = 0; i < num_vals; i++)
                free_svalue(extra + i);
            xfree(extra);
            cr->num_values = 0;

            total_coroutine_size -= sizeof(svalue_t) * cr->variables[cr->num_variables+1].u.number;
        }
        else
            num_vars += num_vals;

        for (int i = 0; i < num_vars; i++)
            free_svalue(cr->variables + i);
    }
} /* _clear_coroutine() */

/*-------------------------------------------------------------------------*/
void
_free_coroutine (coroutine_t *cr)

/* Deallocate the coroutine <cr>,
 * propertly freeing all its variables.
 */

{
    /* There should always be a reference when running. */
    assert(cr->state != CS_RUNNING);

    /* Check first, that the whole awaiting list is free. */
    for (coroutine_t *next = cr->awaitee; next; next = next->awaitee)
        if (next->ref != 0)
            return;
    for (coroutine_t *prev = cr->awaiter; prev; prev = prev->awaiter)
        if (prev->ref != 0)
            return;

    clear_coroutine(cr, cr->state != CS_FINISHED);

    /* Free the whole list. */
    for (coroutine_t *next = cr->awaitee; next;)
    {
        coroutine_t *awaitee = next->awaitee;

        clear_coroutine(next, next->state != CS_FINISHED);

        num_coroutines--;
        total_coroutine_size -= sizeof(coroutine_t) + sizeof(svalue_t) * (next->num_variables + CR_RESERVED_EXTRA_VALUES);

        xfree(next);
        next = awaitee;
    }
    for (coroutine_t *prev = cr->awaiter; prev;)
    {
        coroutine_t *awaiter = prev->awaiter;

        clear_coroutine(prev, prev->state != CS_FINISHED);

        num_coroutines--;
        total_coroutine_size -= sizeof(coroutine_t) + sizeof(svalue_t) * (prev->num_variables + CR_RESERVED_EXTRA_VALUES);

        xfree(prev);
        prev = awaiter;
    }

    num_coroutines--;
    total_coroutine_size -= sizeof(coroutine_t) + sizeof(svalue_t) * (cr->num_variables + CR_RESERVED_EXTRA_VALUES);

    xfree(cr);
} /* _free_coroutine() */

/*-------------------------------------------------------------------------*/
static coroutine_t *
create_empty_coroutine (int num_variables)

/* Allocate a new uninitialized coroutine.
 * The variables are left uninitialized.
 * Returns NULL when out of memory.
 */

{
    coroutine_t * result = xalloc(sizeof(coroutine_t) + sizeof(svalue_t) * (num_variables + CR_RESERVED_EXTRA_VALUES));

    if (!result)
        return NULL;

    result->ref = 1;
    result->num_variables = num_variables;
    result->num_values = 0;
#ifdef DEBUG
    result->num_hidden_variables = 0;
#endif

    num_coroutines++;
    total_coroutine_size += sizeof(coroutine_t) + sizeof(svalue_t) * (num_variables + CR_RESERVED_EXTRA_VALUES);

    return result;
} /* create_empty_coroutine() */

/*-------------------------------------------------------------------------*/
coroutine_t *
create_coroutine (svalue_t *closure)

/* Create a new coroutine from the current running function. It is assumed,
 * that the current program counter points to the start of the coroutine
 * (just behind the F_TRANSFORM_TO_COROUTINE opcode.)
 * If the coroutine is created from an inline closure, <closure> will point
 * to it. Returns NULL when out of memory.
 *
 * On success the local variables are removed from the stack.
 */

{
    coroutine_t * result = create_empty_coroutine(csp->num_local_variables);
    svalue_t * var;
    if (!result)
        return NULL;

    assign_current_object_no_free(&result->ob, "create_coroutine");
    result->prog = current_prog;
    if (closure)
    {
        assert(closure->type == T_CLOSURE && closure->x.closure_type == CLOSURE_LFUN);
        result->closure = closure->u.lfun_closure;
        result->closure->base.ref++;
    }
    else
        result->closure = NULL;
    result->funstart = csp->funstart;
    result->num_variable_names = *(unsigned char*)inter_pc;
    result->pc = inter_pc + 1 + 2*result->num_variable_names;
    result->function_index_offset = function_index_offset;
    result->variable_index_offset = variable_index_offset;
    reference_prog(result->prog, "create_coroutine");

    result->state = CS_SLEEPING;
    result->awaiter = NULL;
    result->awaitee = NULL;

    var = result->variables + result->num_variables;
    for (int i = 0; i < result->num_variables; i++)
        transfer_svalue_no_free(--var, inter_sp--);

    result->last_frame = inter_sp;

    return result;
} /* create_coroutine() */

/*-------------------------------------------------------------------------*/
bool
suspend_coroutine (coroutine_t *cr, svalue_t *fp)

/* Save the current program state into <cr>.
 * Returns true on success, false when out of memory.
 */

{
    int num_values;
    svalue_t *extra, *sp, *values;
#ifdef DEBUG
    function_t *header = cr->prog->function_headers + FUNCTION_HEADER_INDEX(cr->funstart);
#endif

    /* We can only suspend a running coroutine. */
    assert(cr->state == CS_RUNNING);

    /* This stuff shouldn't have changed. */
    assert(cr->prog == current_prog);
    assert(cr->funstart == csp->funstart);
    assert(cr->function_index_offset == function_index_offset);
    assert(cr->variable_index_offset == variable_index_offset);
#ifdef DEBUG
    assert(cr->num_variables == header->num_locals + header->num_arg);
#else
    assert(cr->num_variables == csp->num_local_variables);
#endif

    /* Now save the pc and the stack. */
    num_values = inter_sp - fp + 1 - cr->num_variables;
    assert(num_values >= 0);

    if (num_values > CR_RESERVED_EXTRA_VALUES)
    {
        /* We need additional space for the values. */
        if (cr->num_values > CR_RESERVED_EXTRA_VALUES)
        {
            p_int extra_size = cr->variables[cr->num_variables+1].u.number;
            if (num_values > extra_size)
            {
                /* We need a bigger block. */
                extra = xalloc(sizeof(svalue_t) * num_values);
                if (!extra)
                    return false;
                xfree(cr->variables[cr->num_variables].u.lvalue);
                total_coroutine_size += sizeof(svalue_t) * (num_values - cr->variables[cr->num_variables+1].u.number);
                cr->variables[cr->num_variables].u.lvalue = extra;
                cr->variables[cr->num_variables+1].u.number = num_values;
            }
            else
                extra = cr->variables[cr->num_variables].u.lvalue;
        }
        else
        {
            extra = xalloc(sizeof(svalue_t) * num_values);
            if (!extra)
                return false;
            total_coroutine_size += sizeof(svalue_t) * num_values;
            cr->variables[cr->num_variables].u.lvalue = extra;
            cr->variables[cr->num_variables+1].u.number = num_values;
        }
    }
    else
    {
        /* We can save any extra values in the structure itself. */
        if (cr->num_values > CR_RESERVED_EXTRA_VALUES)
        {
            xfree(cr->variables[cr->num_variables].u.lvalue);
            total_coroutine_size -= sizeof(svalue_t) * cr->variables[cr->num_variables+1].u.number;
        }
        extra = cr->variables + cr->num_variables;
    }

    sp = fp;
    values = cr->variables;
    for (int i = 0; i < cr->num_variables; i++)
        transfer_svalue_no_free(values++, sp++);

    values = extra;
    for (int i = 0; i < num_values; i++)
        transfer_svalue_no_free(values++, sp++);

    assert(sp == inter_sp+1);
    inter_sp = fp-1;

    cr->last_frame = inter_sp;
    cr->num_values = num_values;
#ifdef DEBUG
    cr->num_hidden_variables = csp->num_local_variables - cr->num_variables;
#endif
    cr->num_variable_names = *(unsigned char*)inter_pc;
    cr->pc = inter_pc + 1 + 2*cr->num_variable_names;
    cr->state = CS_SLEEPING;

    return true;
} /* suspend_coroutine() */

/*-------------------------------------------------------------------------*/
coroutine_t *
get_resumable_coroutine (coroutine_t* cr)

/* Given <cr> return a coroutine that actually can be resumed.
 * If <cr> is sleeping return it, if <cr> is waiting for another coroutine
 * return that, otherwise return NULL.
 */

{
    while (cr->state == CS_AWAITING)
    {
        if (cr->ob.type != T_LWOBJECT
         && (cr->ob.type != T_OBJECT || (cr->ob.u.ob->flags & O_DESTRUCTED)))
            return NULL;

        cr = cr->awaitee;
    }

    if (cr->state == CS_SLEEPING
     && (cr->ob.type == T_LWOBJECT
      || (cr->ob.type == T_OBJECT && !(cr->ob.u.ob->flags & O_DESTRUCTED))))
        return cr;

    return NULL;
} /* get_resumable_coroutine() */

/*-------------------------------------------------------------------------*/
bool
resume_coroutine (coroutine_t *cr)

/* Resume the given sleeping coroutine.
 * A control stack entry must already exist and will be updated accordingly.
 * <inter_sp> should point just below the current frame pointer.
 * Returns true on success, false when out of memory.
 */

{
    svalue_t *extra;
    svalue_t *new_frame = inter_sp;
    svalue_t *last_frame_end = cr->last_frame + cr->num_variables + cr->num_values;

    assert(cr->state == CS_SLEEPING);

    if (cr->ob.type == T_OBJECT
     && (cr->ob.u.ob->flags & O_SWAPPED)
     && load_ob_from_swap(cr->ob.u.ob) < 0)
        return false;

    current_object = cr->ob;
    current_prog = cr->prog;
    inter_pc = cr->pc;
    csp->funstart = cr->funstart;
#ifdef DEBUG
    csp->num_local_variables = cr->num_variables + cr->num_hidden_variables;
#else
    csp->num_local_variables = cr->num_variables;
#endif

    function_index_offset = cr->function_index_offset;
    variable_index_offset = cr->variable_index_offset;
    current_variables = get_current_object_variables() + variable_index_offset;

    if (cr->closure && cr->closure->context_size > 0)
        inter_context = cr->closure->context;
    else
        inter_context = NULL;

    if (cr->num_values > CR_RESERVED_EXTRA_VALUES)
        extra = cr->variables[cr->num_variables].u.lvalue;
    else
        extra = cr->variables + cr->num_variables;

    for (int i = 0; i < cr->num_variables; i++)
        transfer_svalue_no_free(++inter_sp, cr->variables + i);
    for (int i = 0; i < cr->num_values; i++)
    {
        transfer_svalue_no_free(++inter_sp, extra + i);

        if (((inter_sp->type == T_LVALUE && inter_sp->x.lvalue_type == LVALUE_UNPROTECTED)
           || inter_sp->type == T_ARG_FRAME)
          && inter_sp->u.lvalue > cr->last_frame
          && inter_sp->u.lvalue <= last_frame_end)
        {
            /* We need to update any pointers into the stack,
             * our frame might have moved.
             */
            inter_sp->u.lvalue = inter_sp->u.lvalue - cr->last_frame + new_frame;
        }
    }

    cr->state = CS_RUNNING;

    return true;
} /* resume_coroutine() */

/*-------------------------------------------------------------------------*/
void
await_coroutine (coroutine_t *awaiter, coroutine_t *awaitee)

/* Set <awaiter> as waiting for completion of <awaitee>.
 * Both coroutines must be sleeping.
 */

{
    assert(awaiter->state == CS_SLEEPING);
    assert(awaiter->awaitee == NULL);
    assert(awaitee->state == CS_SLEEPING || awaitee->state == CS_AWAITING);
    assert(awaitee->awaiter == NULL);

    awaiter->state = CS_AWAITING;
    awaiter->awaitee = awaitee;
    awaitee->awaiter = awaiter;

} /* await_coroutine() */

/*-------------------------------------------------------------------------*/
coroutine_t *
finish_coroutine (coroutine_t *cr)

/* The given (running) coroutine finished with a return. Update its state.
 * If there's a waiting coroutine, return it. Otherwise return NULL.
 */

{
    coroutine_t *awaiter;

    assert(cr->state == CS_RUNNING);
    cr->state = CS_FINISHED;

    clear_coroutine(cr, false);
    cr->ob.type = T_NUMBER;
    cr->prog = NULL;

    /* We are not in CS_AWAITING. */
    assert(cr->awaitee == NULL);

    if (cr->num_values > CR_RESERVED_EXTRA_VALUES)
    {
        xfree(cr->variables[cr->num_variables].u.lvalue);
        total_coroutine_size -= sizeof(svalue_t) * cr->variables[cr->num_variables+1].u.number;
    }
    total_coroutine_size -= sizeof(svalue_t) * cr->num_variables;
    cr->num_values = 0;
    cr->num_variables = 0;

    awaiter = cr->awaiter;
    if (awaiter != NULL)
    {
        cr->awaiter = NULL;
        assert(awaiter->state == CS_AWAITING);
        awaiter->state = CS_SLEEPING;
        awaiter->awaitee = NULL;
    }

    return awaiter;
} /* finish_coroutine() */

/*-------------------------------------------------------------------------*/
void
abort_coroutine (coroutine_t *cr)

/* The given (running) coroutine aborted due to an error. Update its state
 * and all waiting coroutines.
 */

{
    coroutine_t *awaiter = finish_coroutine(cr);

    while (awaiter)
    {
        coroutine_t *next;

        /* finish_coroutine() will have set the state to CS_SLEEPING already. */
        assert(awaiter->state == CS_AWAITING || awaiter->state == CS_SLEEPING);
        awaiter->state = CS_FINISHED;

        clear_coroutine(awaiter, true);
        awaiter->ob.type = T_NUMBER;
        awaiter->prog = NULL;

        next = awaiter->awaiter;
        awaiter->awaitee = NULL;
        awaiter->awaiter = NULL;

        if (awaiter->ref == 0)
            _free_coroutine(awaiter);

        awaiter = next;
    }
} /* abort_coroutine() */

/*-------------------------------------------------------------------------*/
bool
valid_coroutine (coroutine_t *cr)

/* Returns true, if <cr> is a valid coroutine.
 * A coroutine is invalid, if it's either in the CS_FINISHED state or
 * its object is destructed. For coroutines in CS_AWAITING state this
 * must also be true for the coroutine it's waiting for.
 */

{
    while (cr->state == CS_AWAITING)
    {
        if (cr->ob.type != T_LWOBJECT
         && (cr->ob.type != T_OBJECT || (cr->ob.u.ob->flags & O_DESTRUCTED)))
            return false;

        cr = cr->awaitee;
    }

    if (cr->state == CS_FINISHED)
        return false;

    if (cr->ob.type != T_LWOBJECT
     && (cr->ob.type != T_OBJECT || (cr->ob.u.ob->flags & O_DESTRUCTED)))
        return false;

    return true;

} /* valid_coroutine() */

/*-------------------------------------------------------------------------*/
string_t*
coroutine_to_string (coroutine_t *cr)

/* Generate a printable string for the coroutine <cr> and return it.
 */

{
    char buf[1024];
    string_t *result;
    size_t len;

    if (cr->prog == NULL)
        snprintf(buf, sizeof(buf), "<coroutine in destructed object>");
    else
    {
        function_t *header = cr->prog->function_headers + FUNCTION_HEADER_INDEX(cr->funstart);

        switch (cr->ob.type)
        {
            case T_OBJECT:
                snprintf(buf, sizeof(buf), "<coroutine /%s->%s>"
                        , get_txt(cr->ob.u.ob->name)
                        , get_txt(header->name));
                break;

            case T_LWOBJECT:
                snprintf(buf, sizeof(buf), "<coroutine /%s->%s>"
                        , get_txt(cr->ob.u.lwob->prog->name)
                        , get_txt(header->name));
                break;

            default:
                snprintf(buf, sizeof(buf), "<coroutine in destructed object>");
                break;
        }
    }

    len = strlen(buf);
    memsafe(result = new_n_unicode_mstring(buf, len), len, "coroutine name");
    return result;

} /* coroutine_to_string() */

#ifdef GC_SUPPORT

/*-------------------------------------------------------------------------*/
coroutine_t*
new_sample_coroutine ()

/* Creates a sample coroutine for the garbage collection to recognize.
 * This coroutine must be destroyed with free_sample_coroutine().
 */

{
    return create_empty_coroutine(0);
} /* new_sample_coroutine() */

/*-------------------------------------------------------------------------*/
void
free_sample_coroutine (coroutine_t* cr)

/* Free the sample coroutine created by new_sample_coroutine().
 */

{
    xfree(cr);

    num_coroutines--;
    total_coroutine_size -= sizeof(coroutine_t) + sizeof(svalue_t) * CR_RESERVED_EXTRA_VALUES;
} /* free_sample_coroutine() */

/*-------------------------------------------------------------------------*/
void
clear_coroutine_ref (coroutine_t *cr)

/* Clear all references held by <cr>.
 */

{
    if (cr->ref != 0)
    {
        clear_memory_reference(cr);
        cr->ref = 0;

        if (cr->prog)
            clear_program_ref(cr->prog, true);
        clear_ref_in_vector(&cr->ob, 1);
        if (cr->closure && cr->closure->base.ref != 0)
        {
            svalue_t svp = { T_CLOSURE, {.closure_type = CLOSURE_LFUN}, {.lfun_closure = cr->closure } };
            clear_ref_in_vector(&svp, 1);
        }
        if (cr->num_values > CR_RESERVED_EXTRA_VALUES)
        {
            svalue_t *extra = cr->variables[cr->num_variables].u.lvalue;

            clear_memory_reference(extra);
            clear_ref_in_vector(cr->variables, cr->num_variables);
            clear_ref_in_vector(extra, cr->num_values);
        }
        else
            clear_ref_in_vector(cr->variables, cr->num_variables + cr->num_values);
        if (cr->awaitee != NULL)
            clear_coroutine_ref(cr->awaitee);
        if (cr->awaiter != NULL)
            clear_coroutine_ref(cr->awaiter);
    }
} /* clear_coroutine_ref() */

/*-------------------------------------------------------------------------*/
void
count_coroutine_ref (coroutine_t *cr)

/* Add a reference to <cr> and count additional references.
 */

{
    cr->ref++;
    if (test_memory_reference(cr))
    {
        note_malloced_block_ref(cr);

        if (cr->prog)
            mark_program_ref(cr->prog);
        count_ref_in_vector(&cr->ob, 1);
        if (cr->closure)
        {
            if (cr->closure->base.ref == 0)
            {
                svalue_t svp = { T_CLOSURE, {.closure_type = CLOSURE_LFUN}, {.lfun_closure = cr->closure } };
                count_ref_in_vector(&svp, 1);
            }
            else
                cr->closure->base.ref++;
        }
        if (cr->num_values > CR_RESERVED_EXTRA_VALUES)
        {
            svalue_t *extra = cr->variables[cr->num_variables].u.lvalue;

            note_malloced_block_ref(extra);
            count_ref_in_vector(cr->variables, cr->num_variables);
            count_ref_in_vector(extra, cr->num_values);
        }
        else
            count_ref_in_vector(cr->variables, cr->num_variables + cr->num_values);
        if (cr->awaitee != NULL)
        {
            count_coroutine_ref(cr->awaitee);
            cr->awaitee->ref--;
        }
        if (cr->awaiter != NULL)
        {
            count_coroutine_ref(cr->awaiter);
            cr->awaiter->ref--;
        }
    }
} /* count_coroutine_ref() */

#endif /* GC_SUPPORT */
