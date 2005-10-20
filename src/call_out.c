/*---------------------------------------------------------------------------
 * Gamedriver Callout handling.
 *
 *---------------------------------------------------------------------------
 * Callouts are delayed calls to (non-static) functions, with delays
 * measured in seconds. The minimal resolution is of course the duration
 * a backend cycle. The command_giver is saved over the delay.
 *
 * As a simplistic measure against 'rabbits' (think fork-bombs using
 * callouts), the callouts of one user can use only MAX_EVAL_COST at
 * one time altogether.
 *
 * Pending call outs are held in a list, sorted in ascending order of
 * remaining delaytime. The actual delay values stored are deltas: the
 * delay this callout is to be scheduled after its predecessor in
 * the list.
 *
 * A second list holds freed call structures to reduce memory management
 * overhead.
 *
 * TODO: The CHUNKed allocation would be nice to have as generic module.
 *---------------------------------------------------------------------------
 */

#include "driver.h"

#include "call_out.h"
#include "array.h"
#include "backend.h"
#include "closure.h"
#include "comm.h"
#include "exec.h"
#include "gcollect.h"
#include "interpret.h"
#include "main.h"
#include "object.h"
#include "simulate.h"
#include "stralloc.h"
#include "wiz_list.h"

/*-------------------------------------------------------------------------*/

  /* The description of one callout.
   *
   * The function to call be either given by object:name or as closure.
   */

struct call {
    int delta;           /* Delay in relation to the previous structure */
    union {              /* The function to call */
        struct {
            char          *name;  /* a shared string */
            struct object *ob;
        } named;
        struct svalue lambda;
    } function;
    /* TODO: BOOL */ int is_lambda; /* Closure or named function? */
    struct svalue v;
      /* The argument value to pass to the function.
       * To pass several arguments, v is of type LVALUE,
       * v.u.lvalue points to the real arguments values,
       * v.x.num_args gives the number of arguments.
       */
    struct call *next;   /* link to next structure */
    struct object *command_giver;  /* the saved command_giver */
};


#define CHUNK_SIZE 20
  /* Structures are allocated in chunks of this size, to reduce the
   * malloc overhead.
   */

static struct call *call_list = NULL;
  /* The list of pending call_outs, sorted in ascending order of delay.
   */

static struct call *call_list_free = NULL;
  /* The list of unused call structures.
   */

static long num_call = 0;
  /* Number of allocated callouts.
   */

static struct object call_out_nil_object;
  /* Callouts with no argument let call.v use this variable as
   * placeholder.
   */

/*-------------------------------------------------------------------------*/
/*
 * Free a call out structure.
 */
static void
free_call (struct call *cop)

/* Deallocate all resources bound to <cop> and put <cop> into the free list.
 */

{
    /* Free the argument(s) listed in cop.v */

    if (cop->v.type == T_LVALUE)
    {
        int i;
        struct svalue *v;

        i = cop->v.x.num_arg;
        v = cop->v.u.lvalue;
        do {
            free_svalue(v++);
        } while (--i);
        xfree((char *)cop->v.u.lvalue);
    }
    else
    {
        if (cop->v.u.ob != &call_out_nil_object || cop->v.type != T_OBJECT)
            free_svalue(&cop->v);
    }

    /* Free the function designation */

    if (cop->is_lambda)
    {
        free_closure(&cop->function.lambda);
    }
    else
    {
        free_string(cop->function.named.name);
        free_object(cop->function.named.ob, "free_call");
    }

    if (cop->command_giver)
        free_object(cop->command_giver, "free_call");

    /* Put cop into the free list */

    cop->next = call_list_free;
    call_list_free = cop;
}

/*-------------------------------------------------------------------------*/
static INLINE void
free_used_call (struct call *cop)

/* Free <cop> after it was used: deallocate the remaining(!)
 * resources and put it into the free list.
 */

{
    if (cop->command_giver)
        free_object(cop->command_giver, "free_call");

    /* Everything else was already deallocated by using it */

    cop->next = call_list_free;
    call_list_free = cop;
}

/*-------------------------------------------------------------------------*/
struct svalue *
new_call_out (struct svalue *sp, int num_arg)

/* EFUN: call_out()
 *
 *   void call_out(string fun, int delay, mixed arg, ...)
 *   void call_out(closure cl, int delay, mixed arg, ...)
 *
 * Set up a call to function fun or closure cl in the current
 * object. The call will take place in delay seconds, with the
 * remaining argument list provided. References in the argument list
 * will be passed as number 0, though.
 */

{
    struct svalue  *arg;    /* Pointer to efun arguments */
    int             delay;
    struct call    *cop;    /* New callout structure */
    struct call   **copp;   /* Auxiliary pointers for list insertion */
    struct call    *cop2;

    arg = sp - num_arg + 1;

    /* First, find a new call structure.
     * If possible from the free list, else allocate a new chunk of them.
     * Note that we don't yet remove the structure from the freelist - in
     * case errors happen.
     */

    if ( !(cop = call_list_free) )
    {
        int i;

        cop = call_list_free =
            (struct call *)permanent_xalloc(CHUNK_SIZE * sizeof (struct call));
        for ( i = 0; i < CHUNK_SIZE - 1; i++)
            call_list_free[i].next = &call_list_free[i+1];
        call_list_free[CHUNK_SIZE-1].next = NULL;
        call_out_nil_object.flags |= O_DESTRUCTED;
        num_call += CHUNK_SIZE;
    }

    /* Get the function designation from the stack */

    if (arg[0].type == T_STRING)
    {
        struct object *ob;

        if (arg[0].x.string_type == STRING_SHARED) {
            cop->function.named.name = arg[0].u.string;
        } else {
            cop->function.named.name = make_shared_string(arg[0].u.string);
            if (arg[0].x.string_type == STRING_MALLOC)
                xfree(arg[0].u.string);
            arg[0].u.string = cop->function.named.name;
            arg[0].x.string_type = STRING_SHARED;
        }
        cop->function.named.ob = ob = current_object;
        add_ref(ob, "call_out");
        cop->is_lambda = MY_FALSE;
    }
    else if(arg[0].type == T_CLOSURE && CLOSURE_CALLABLE(arg->x.closure_type))
    {
        cop->function.lambda = arg[0];
        cop->is_lambda = MY_TRUE;
    }
    else
    {
        bad_efun_vararg(1, sp);
        /* NOTREACHED */
    }

    /* Test if the delay time is on the stack */

    if (arg[1].type != T_NUMBER)
    {
        bad_efun_vararg(2, sp);
        /* NOTREACHED */
    }

    /* If the current object is destructed, free everything on the stack
     * and return.
     */

    if (current_object->flags & O_DESTRUCTED)
    {
        if (!cop->is_lambda)
        {
            /* ref count was incremented above */
            current_object->ref--;
        }
        do
        {
            free_svalue(sp--);
        } while (--num_arg);
        return sp;
    }

    num_arg -= 2;  /* How many arguments to pass to the function? */

    /* We can do the callout, so lets get it from the freelist and
     * store the arguments to pass on the call.
     */

    call_list_free = cop->next;
    cop->command_giver = command_giver; /* save current player context */
    if (command_giver)
        add_ref(command_giver, "new_call_out");  /* Bump its ref */

    if (num_arg > 1)
    {
        /* Multiple arguments: wrap them into a pseudo-LVALUE */

        struct svalue *v, *w;

        v = xalloc(num_arg * sizeof *v);
        if (!v)
        {
            fatal("Out of memory.");
            /* NOTREACHED */
        }
        cop->v.type = T_LVALUE;
        cop->v.x.num_arg = num_arg;
        cop->v.u.lvalue = v;
        w = &arg[2];
        do {
            if (w->type == T_LVALUE)
            {
                free_svalue(w);
                w->type = T_NUMBER;
                w->u.number = 0;
            }
            transfer_svalue_no_free(v++, w++);
        } while (--num_arg);
    }
    else if (num_arg)
    {
        /* Get the one argument from the stack */

        if (arg[2].type != T_LVALUE)
        {
            transfer_svalue_no_free(&cop->v, &arg[2]);
        }
        else
        {
            free_svalue(&arg[2]);
            cop->v.type = T_NUMBER;
            cop->v.u.number = 0;
        }
    }
    else
    {
        /* Use the 'no argument' argument */

        cop->v.type = T_OBJECT;
        cop->v.u.ob = &call_out_nil_object;
    }

    /* Adjust the stack and get the delay */
    sp = arg - 1;
    delay = arg[1].u.number;
    if (delay < 1)
        delay = 1;

    /* Insert the new structure at its proper place in the list */

    for (copp = &call_list; NULL != (cop2 = *copp); copp = &cop2->next)
    {
        int delta;
        if ((delta = cop2->delta) >= delay)
        {
            cop2->delta -= delay;
            cop->delta = delay;
            cop->next = *copp;
            *copp = cop;
            return sp;
        }
        delay -= delta;
    }
    *copp = cop;
    cop->delta = delay;
    cop->next = NULL;

    return sp;
} /* new_call_out() */

/*-------------------------------------------------------------------------*/
void
call_out (void)

/* Check if there is any callout due to be called. If yes, do so.
 * This function is called from the heart_beat handling in the backend.c.
 * It sets up its own error recovery context so that errors during an
 * execution won't disturb the rest of the game.
 */

{
    static int last_time;
      /* Last time this function was called */

    static struct call *current_call_out;
      /* Current callout, static so that longjmp() won't clobber it. */

    struct error_recovery_info error_recovery_info;
    struct svalue *sp;

    /* No calls pending: just update the last_time and return */

    if (call_list == NULL)
    {
        last_time = current_time;
        return;
    }

    /* If not set yet, initialize last_time on the first call */
    if (last_time == 0)
        last_time = current_time;

    /* Update the first .delta in the list (so it won't happen
     * twice in case of an error.
     */
    call_list->delta -= current_time - last_time;

    last_time = current_time;
    current_interactive = NULL;

    /* Activate the local error recovery context */

    error_recovery_info.last = error_recovery_pointer;
    error_recovery_info.type = ERROR_RECOVERY_BACKEND;
    error_recovery_pointer = &error_recovery_info;

    if (setjmp(error_recovery_info.con.text))
    {
        /* An error occured: recover and delete the guilty callout */

        struct call *cop;
        struct object *ob;
        struct wiz_list *user;

        clear_state();
        debug_message("Error in call out.\n");
        cop = current_call_out;
        if (cop->is_lambda)
        {
            ob = !CLOSURE_MALLOCED(cop->function.lambda.x.closure_type) ?
                cop->function.lambda.u.ob :
                cop->function.lambda.u.lambda->ob;
        } else {
            ob = cop->function.named.ob;
        }
        user = ob->user;
        user->call_out_cost = eval_cost;
        cop->v.type = T_INVALID;
        free_call(cop);
    }

    /* (Re)initialize stack and tracing */

    tracedepth = 0;
    sp = inter_sp+1;

    /* Loop over the call list until it is empty or until all
     * due callouts are processed.
     */
    while (call_list && call_list->delta <= 0)
    {
        struct call *cop;
        ph_int       type;
        int          num_arg;

        /* Move the first callout out of the chain.
         */
        cop = call_list;
        call_list = cop->next;
        current_call_out = cop;

        /* A special case:
         * If a lot of time has passed, so that current call out was missed,
         * then it will have a negative delta. This negative delta implies
         * that the next call out in the list has to be adjusted.
         */
        if (cop->delta < 0 && call_list)
            call_list->delta += cop->delta;

        /* Determine the command_giver for the call.
         * If a command_giver is given in the callout structure, use that one.
         * Else test the object to be called or the object it's shadowing for
         * being a command_giver.
         * Remember that a now-destructed commandgiver is different from having
         * no commandgiver to begin with.
         */
        if (cop->command_giver)
        {
            if (!(cop->command_giver->flags & O_DESTRUCTED))
            {
                command_giver = cop->command_giver;
                if (O_GET_INTERACTIVE(command_giver)
                 && O_GET_INTERACTIVE(command_giver)->sent.type == SENT_INTERACTIVE)
                {
                    trace_level = O_GET_INTERACTIVE(command_giver)->trace_level;
                }
            }
            else
                command_giver = NULL;
        }
        else
        {
            struct object *ob;

            /* Get the object for the function call */

            if (cop->is_lambda)
                ob = !CLOSURE_MALLOCED(cop->function.lambda.x.closure_type)
                      ? cop->function.lambda.u.ob
                      : cop->function.lambda.u.lambda->ob;
            else
            {
                ob = cop->function.named.ob;
                if (ob->flags & O_DESTRUCTED)
                {
                    free_call(cop);
                    continue;
                }
            }

            if (ob->flags & O_SHADOW)
            {
                /* Look at the object which is at the end of the shadow chain.
                 */
                struct shadow_sentence *shadow_sent;

                while((shadow_sent = O_GET_SHADOW(ob)), shadow_sent->shadowing)
                    ob = shadow_sent->shadowing;

                if (ob->flags & O_ENABLE_COMMANDS)
                {
                    command_giver = ob;
                    if (shadow_sent->type == SENT_INTERACTIVE)
                        trace_level =
                          ((struct interactive *)shadow_sent)->trace_level;
                    else
                        trace_level = 0;
                }
                else
                {
                    command_giver = NULL;
                    trace_level = 0;
                }
            }
            else
            {
                /* If at all, this object must be the command_giver */

                if (ob->flags & O_ENABLE_COMMANDS)
                {
                    command_giver = ob;
                }
                else
                {
                    command_giver = NULL;
                }
                trace_level = 0;
            }
        }

        /* Put the arguments for the call onto the stack.
         */

        if ((type = cop->v.type) == T_LVALUE)
        {
            /* Multiple arguments */

            struct svalue *v;
            int i;
            struct object *ob;

            v = cop->v.u.lvalue;
            num_arg = i = cop->v.x.num_arg;
            sp--;
            do {
                if (v->type == T_OBJECT &&
                    (ob = v->u.ob)->flags & O_DESTRUCTED)
                {
                    sp++;
                    free_object(ob, "call_out");
                    sp->type = T_NUMBER;
                    sp->u.number = 0;
                    v++;
                }
                else
                {
                    *++sp = *v++;
                }
            } while (--i);
            xfree((char *)cop->v.u.lvalue);
            inter_sp = sp;
            sp -= num_arg - 1;
        }
        else
        {
            /* Single argument */

            struct object *ob;

            num_arg = 1;
            inter_sp = sp;
            if (type == T_OBJECT)
            {
                ob = cop->v.u.ob;
                if (ob->flags & O_DESTRUCTED)
                {
                    if (ob == &call_out_nil_object)
                    {
                        num_arg = 0;
                        inter_sp = sp - 1;
                    }
                    else
                    {
                        free_object(ob, "call_out");
                        sp->type = T_NUMBER;
                        sp->u.number = 0;
                    }
                }
                else
                {
                    sp->type = T_OBJECT;
                    sp->u.ob = ob;
                }
            }
            else
            {
                *sp = cop->v;
            }
        }

        /* Finally, call the function (unless the object was destructed).
         */

        if (cop->is_lambda)
        {
            struct object *ob;
            struct wiz_list *user;

            ob = !CLOSURE_MALLOCED(cop->function.lambda.x.closure_type)
                  ? cop->function.lambda.u.ob
                  : cop->function.lambda.u.lambda->ob;
            if (ob->flags & O_DESTRUCTED)
            {
                while (--num_arg >= 0)
                    pop_stack();
            }
            else
            {
                current_object = ob;
                user = ob->user;
                if (user->last_call_out != current_time)
                {
                    user->last_call_out = current_time;
                    CLEAR_EVAL_COST;
                }
                else
                {
                    assigned_eval_cost = eval_cost = user->call_out_cost;
                }
                if (cop->function.lambda.x.closure_type < CLOSURE_SIMUL_EFUN
                 && cop->function.lambda.x.closure_type >= CLOSURE_EFUN)
                    current_prog = NULL;
                    /* Setting current_prog to NULL is OK in this case since
                     * there is no higher-level program from which this
                     * closure is called.
                     */
                call_lambda(&cop->function.lambda, num_arg);
                user->call_out_cost = eval_cost;
                free_svalue(sp);
            }
            free_closure(&cop->function.lambda);
        }
        else
        {
            struct object *ob;

            if ((ob = cop->function.named.ob)->flags & O_DESTRUCTED)
            {
                while (--num_arg >= 0)
                    pop_stack();
            }
            else
            {
                struct wiz_list *user;

                current_object = ob;
                user = ob->user;
                if (user->last_call_out != current_time)
                {
                    user->last_call_out = current_time;
                    CLEAR_EVAL_COST;
                }
                else
                {
                    assigned_eval_cost = eval_cost = user->call_out_cost;
                }
                sapply_int(cop->function.named.name, ob, num_arg, MY_TRUE);
                user->call_out_cost = eval_cost;
            }
            free_string(cop->function.named.name);
            free_object(cop->function.named.ob, "free_call");
        }

        /* The function call used up all the arguments, now free
         * the rest */
        free_used_call(cop);

    } /* while (callouts pending) */

    inter_sp = sp - 1;
    error_recovery_pointer = error_recovery_info.last;

}

/*-------------------------------------------------------------------------*/
void
find_call_out (struct object *ob, struct svalue *fun, /* TODO: BOOL */ int do_free_call)

/* Find the (first) callout for <ob>/<fun> (or <fun> if it is a closure).
 * If <do_free_call> is true, the found callout is removed.
 *
 * In either case, *<fun> is modified into a NUMBER holding the time left
 * for the found/removed callout. If no callout was found, -1 is returned.
 */

{
    struct call **copp, *cop;
    int delay = 0;
    char *fun_name;

    /* Find callout by closure */

    if (fun->type != T_STRING)
    {
        ph_int type;

        if (fun->type == T_CLOSURE)
        {
            if (!CLOSURE_MALLOCED(type = fun->x.closure_type)
             && type >= CLOSURE_EFUN)
            {
                for (copp = &call_list; NULL != (cop = *copp); copp = &cop->next)
                {
                    delay += cop->delta;
                    if (cop->is_lambda
                     && cop->function.lambda.x.closure_type == type
                     && cop->function.lambda.u.ob == ob)
                    {
                        goto found;
                    }
                }
                goto not_found;
            }
            else if (type != CLOSURE_UNBOUND_LAMBDA)
            {
                struct lambda *l;

                l = fun->u.lambda;
                if (type != CLOSURE_LFUN)
                type = CLOSURE_UNBOUND_LAMBDA;
                for (copp = &call_list; NULL != (cop = *copp); copp = &cop->next)
                {
                    delay += cop->delta;
                    if (cop->is_lambda
                     && (   cop->function.lambda.u.lambda == l
                         || (   cop->function.lambda.x.closure_type == type
                             && cop->function.lambda.u.lambda->ob == l->ob
                             && cop->function.lambda.u.lambda->function.index
                                == l->function.index)
                        )
                       )
                    {
                        goto found;
                    }
                }
                /* FALLTHROUGH*/
not_found:
                free_svalue(fun);
                fun->type = T_NUMBER;
                fun->u.number = -1;
                return;
found:
                free_svalue(fun);
                fun->type = T_NUMBER;
                if (do_free_call)
                {
                    if (cop->next)
                        cop->next->delta += cop->delta;
                    *copp = cop->next;
                    free_call(cop);
                }
                /* It is possible to have delay < 0 if we are
                 * called from inside call_out() .
                 */
                if (delay < 0)
                    delay = 0;
                fun->u.number = delay;
                return;
            }
        }
        bad_efun_arg(1, -1, fun);
        /* NOTREACHED */
    }


    /* Find callout by object/name */

    if (fun->x.string_type == STRING_SHARED) {
        fun_name = fun->u.string;
    }
    else
    {
        fun_name = make_shared_string(fun->u.string);
        if (fun->x.string_type == STRING_MALLOC)
            xfree(fun->u.string);
    }

    fun->type = T_NUMBER;
    for (copp = &call_list; NULL != (cop = *copp); copp = &cop->next)
    {
        delay += cop->delta;
        if (cop->function.named.ob == ob
         && cop->function.named.name == fun_name
         && !cop->is_lambda)
        {
            decrement_string_ref(fun_name);
            if (do_free_call)
            {
                if (cop->next)
                    cop->next->delta += cop->delta;
                *copp = cop->next;
                free_call(cop);
            }
            if (delay < 0)
                delay = 0;
            fun->u.number = delay;
            return;
        }
    }
    free_string(fun_name);
    fun->u.number = -1;
}

/*-------------------------------------------------------------------------*/
int
print_call_out_usage (/* TODO: BOOL */ int verbose)

/* Compute and return the amount of memory used by callouts.
 * If <verbose> is true, write detailed statistics to the current user.
 */

{
    long i;
    struct call *cop;

    for (i=0, cop = call_list; cop; cop = cop->next)
        i++;
    if (verbose)
    {
        add_message("\nCall out information:\n");
        add_message("---------------------\n");
        add_message("Number of allocated call outs: %8ld, %8ld bytes\n",
                    num_call, num_call * sizeof (struct call));
        add_message("Current length: %ld\n", i);
    }
    else
    {
        add_message("call out:\t\t\t%8ld %8ld (current length %ld)\n", num_call,
                    num_call * sizeof (struct call), i);
    }

    return num_call * sizeof (struct call);
}

/*-------------------------------------------------------------------------*/
#ifdef DEBUG

void
count_extra_ref_from_call_outs (void)

/* Used to debug refcounts: count all refcounts in the callout handling.
 */

{
    struct call *cop;

    for (cop = call_list; cop; cop = cop->next) {
        if (cop->v.type == T_LVALUE)
        {
            count_extra_ref_in_vector(cop->v.u.lvalue, cop->v.x.num_arg);
        } else if (cop->v.u.ob != &call_out_nil_object ||
                   cop->v.type != T_OBJECT)
        {
            count_extra_ref_in_vector(&cop->v, 1);
        }
        if (cop->is_lambda) {
            count_extra_ref_in_vector(&cop->function.lambda, 1);
        } else {
            count_extra_ref_in_object(cop->function.named.ob);
        }
        if (cop->command_giver)
            count_extra_ref_in_object(cop->command_giver);
    }
}

#endif

/*-------------------------------------------------------------------------*/
void
remove_stale_call_outs (void)

/* GC Support: Remove all callouts referencing destructed objects.
 */

{
    struct call **copp, *cop;

    for (copp = &call_list; NULL != (cop = *copp); )
    {
        if ( (cop->is_lambda
              ? (CLOSURE_MALLOCED(cop->function.lambda.x.closure_type)
                 ? cop->function.lambda.u.lambda->ob
                 : cop->function.lambda.u.ob)
              : cop->function.named.ob
             )->flags & O_DESTRUCTED)
        {
            if (cop->next)
                cop->next->delta += cop->delta;
            *copp = cop->next;
            free_call(cop);
            continue;
        }
        copp = &cop->next;
    }
}


#ifdef MALLOC_smalloc

/*-------------------------------------------------------------------------*/
void
clear_ref_from_call_outs (void)

/* GC Support: Clear all refs from the callout handling.
 */

{
    struct call *cop;

    for (cop = call_list; cop; cop = cop->next) {
        struct object *ob;

        if (cop->v.type == T_LVALUE) {
            struct svalue *v;

            v = cop->v.u.lvalue;
            clear_ref_in_vector(v, cop->v.x.num_arg);
        } else {
            if (cop->v.u.ob != &call_out_nil_object || cop->v.type != T_OBJECT)
                clear_ref_in_vector(&cop->v, 1);
        }
        if (cop->is_lambda) {
            clear_ref_in_vector(&cop->function.lambda, 1);
            /* else: cop->function.named.ob isn't destructed */
        }
        if (NULL != (ob = cop->command_giver) && ob->flags & O_DESTRUCTED) {
            ob->ref = 0;
            ob->prog->ref = 0;
        }
    }
}

/*-------------------------------------------------------------------------*/
void
count_ref_from_call_outs (void)

/* GC Support: Clear all refs from the callout handling.
 */

{
    struct call *cop;
    struct object *ob;

    for (cop = call_list; cop; cop = cop->next) {
        if (cop->v.type == T_LVALUE) {
            struct svalue *v;

            v = cop->v.u.lvalue;
            note_malloced_block_ref((char *)v);
            count_ref_in_vector(v, cop->v.x.num_arg);
        } else {
            if (cop->v.u.ob != &call_out_nil_object || cop->v.type != T_OBJECT)
                count_ref_in_vector(&cop->v, 1);
        }
        if (cop->is_lambda) {
            count_ref_in_vector(&cop->function.lambda, 1);
        } else {
            /* destructed objects have been taken care of beforehand */
            cop->function.named.ob->ref++;
            count_ref_from_string(cop->function.named.name);
        }
        if ( NULL != (ob = cop->command_giver) ) {
            if (ob->flags & O_DESTRUCTED) {
                reference_destructed_object(ob);
                cop->command_giver = 0;
            } else {
                ob->ref++;
            }
        }
    }
}

#endif /* MALLOC_smalloc */

/*-------------------------------------------------------------------------*/
struct vector *
get_all_call_outs (void)

/* Construct an array of all pending call_outs (whose object is not
 * destructed). Every item in the array is itself an array of 4 or
 * more entries:
 *  0:   The object (only if the function is a string).
 *  1:   The function (string or closure).
 *  2:   The delay.
 *  3..: The argument(s).
 */
{
    int i, next_time;
    struct call *cop;
    struct vector *v;

    /* Count the number of pending callouts and allocate
     * the result array.
     */
    for (i = 0, cop = call_list; cop; cop = cop->next)
    {
        if (!cop->is_lambda && cop->function.named.ob->flags & O_DESTRUCTED)
            continue;
        i++;
    }
    v = allocate_array(i); /* assume that all elements are inited to 0 */

    /* Create the result array contents.
     */

    next_time = 0;
    for (i=0, cop = call_list; cop; cop = cop->next)
    {
        struct vector *vv;
        char *function_name;

        next_time += cop->delta;

        /* Get the subarray */

        vv = allocate_array(
          cop->v.type == T_LVALUE
           ? 3 + cop->v.x.num_arg
           : (cop->v.u.ob == &call_out_nil_object && cop->v.type == T_OBJECT
              ? 3 : 4)
        );

        if (cop->is_lambda)
        {
            assign_svalue_no_free(&vv->item[1], &cop->function.lambda);
            /* assuming that item[0] was inited to 0 */
        }
        else
        {
            struct object *ob;

            ob = cop->function.named.ob;
            if (ob->flags & O_DESTRUCTED)
            {
                free_vector(vv);
                continue;
            }
            vv->item[0].type = T_OBJECT;
            vv->item[0].u.ob = ob;
            add_ref(ob, "get_all_call_outs");
            vv->item[1].type = T_STRING;
            vv->item[1].x.string_type = STRING_SHARED;
            function_name = cop->function.named.name;
            increment_string_ref(function_name);
            vv->item[1].u.string = function_name;
        }

        vv->item[2].u.number = next_time;

        if (cop->v.type == T_LVALUE)
        {
            struct svalue *source, *dest;
            int j;

            source = cop->v.u.lvalue;
            dest = &vv->item[3];
            j = cop->v.x.num_arg;
            do {
                assign_svalue_no_free(dest++, source++);
            } while (--j);
        }
        else if (cop->v.u.ob != &call_out_nil_object
              || cop->v.type != T_OBJECT)
        {
            assign_svalue_no_free(&vv->item[3], &cop->v);
        }

        v->item[i].type = T_POINTER;
        v->item[i].u.vec = vv;        /* Ref count is already 1 */
        i++;
    }

    return v;
}

/***************************************************************************/

