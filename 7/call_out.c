#include "lint.h"
#include "interpret.h"
#include "object.h"
#include "comm.h"
#include "stralloc.h"
#include "exec.h"
#include "wiz_list.h"

extern int trace_level;

/*
 * This file implements delayed calls of functions.
 * Static functions can not be called this way.
 *
 * Allocate the structures several in one chunk, to get rid of malloc
 * overhead.
 */

#define CHUNK_SIZE	20

struct call {
    int delta;
    union {
	struct {
	    char *name;
	    struct object *ob;
	} named;
	struct svalue lambda;
    } function;
    int is_lambda;
    struct svalue v;
    struct call *next;
    struct object *command_giver;
};

static struct call *call_list=0, *call_list_free=0;
static int num_call = 0;

static struct object call_out_nil_object; /* To mark no argument. */

/*
 * Free a call out structure.
 */
static void free_call(cop)
    struct call *cop;
{
    if (cop->v.type == T_LVALUE) {
	int i;
	struct svalue *v;

	i = cop->v.x.num_arg;
	v = cop->v.u.lvalue;
	do {
	    free_svalue(v++);
	} while (--i);
	xfree((char *)cop->v.u.lvalue);
    } else {
	if (cop->v.u.ob != &call_out_nil_object || cop->v.type != T_OBJECT)
	    free_svalue(&cop->v);
    }
    cop->next = call_list_free;
    if (cop->is_lambda) {
	free_closure(&cop->function.lambda);
    } else {
	free_string(cop->function.named.name);
	free_object(cop->function.named.ob, "free_call");
    }
    if (cop->command_giver)
	free_object(cop->command_giver, "free_call");
    call_list_free = cop;
}

static INLINE void free_used_call(cop)
    struct call *cop;
{
    cop->next = call_list_free;
    if (cop->command_giver)
	free_object(cop->command_giver, "free_call");
    call_list_free = cop;
}

/*
 * Setup a new call out.
 */
struct svalue *new_call_out(sp, num_arg)
    struct svalue *sp;
    int num_arg;
{
    struct svalue *arg;
    int delay;
    struct call *cop, **copp, *cop2;

    arg = sp - num_arg + 1;
    if ( !(cop = call_list_free) ) {
	int i;

	cop = call_list_free =
	    (struct call *)permanent_xalloc(CHUNK_SIZE * sizeof (struct call));
	for (i=0; i<CHUNK_SIZE - 1; i++)
	    call_list_free[i].next  = &call_list_free[i+1];
	call_list_free[CHUNK_SIZE-1].next = 0;
	call_out_nil_object.flags |= O_DESTRUCTED;
	num_call += CHUNK_SIZE;
    }
    if (arg[0].type == T_STRING) {
	struct object *ob;

	if (arg[0].x.string_type == STRING_SHARED) {
	    cop->function.named.name = arg[0].u.string;
	} else {
	    cop->function.named.name = make_shared_string(arg[0].u.string);
	    if (arg[0].x.string_type == STRING_MALLOC)
		xfree(arg[0].u.string);
	    /* make sure that fullowing error won't bug */
	    arg[0].u.string = cop->function.named.name;
	    arg[0].x.string_type = STRING_SHARED;
	}
	cop->function.named.ob = ob = current_object;
	add_ref(ob, "call_out");
	cop->is_lambda = 0;
    } else if(arg[0].type == T_CLOSURE && CLOSURE_CALLABLE(arg->x.closure_type))
    {
	cop->function.lambda = arg[0];
	cop->is_lambda = 1;
    } else {
	bad_efun_vararg(1, sp);
    }
    if (arg[1].type != T_NUMBER)
	bad_efun_vararg(2, sp);
    if (current_object->flags & O_DESTRUCTED) {
	if (!cop->is_lambda) {
	    /* ref count was incremented above */
	    current_object->ref--;
	}
	do {
	    free_svalue(sp--);
	} while (--num_arg);
	return sp;
    }
    num_arg -= 2;
    call_list_free = cop->next;
    cop->command_giver = command_giver; /* save current player context */
    if (command_giver)
	add_ref(command_giver, "new_call_out");		/* Bump its ref */
    if (num_arg > 1) {
	struct svalue *v, *w;

	v = (struct svalue *)xalloc(num_arg * sizeof *v);
	cop->v.type = T_LVALUE;
	cop->v.x.num_arg = num_arg;
	cop->v.u.lvalue = v;
	w = &arg[2];
	do {
	    if (w->type == T_LVALUE) {
		/* We could give an error here, but it would be more work */
		free_svalue(w);
		w->type = T_NUMBER;
		w->u.number = 0;
	    }
	    transfer_svalue_no_free(v++, w++);
	} while (--num_arg);
    } else if (num_arg) {
	if (arg[2].type != T_LVALUE) {
	    transfer_svalue_no_free(&cop->v, &arg[2]);
	} else {
	    free_svalue(&arg[2]);
	    cop->v.type = T_NUMBER;
	    cop->v.u.number = 0;
	}
    } else {
	cop->v.type = T_OBJECT;
	cop->v.u.ob = &call_out_nil_object;
    }
    sp = arg - 1;
    delay = arg[1].u.number;
    if (delay < 1)
	delay = 1;
    for (copp = &call_list; cop2 = *copp; copp = &cop2->next) {
	int delta;
	if ((delta = cop2->delta) >= delay) {
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
    cop->next = 0;
    return sp;
}

/*
 * See if there are any call outs to be called. Set the 'command_giver'
 * if it is a living object. Check for shadowing objects, which may also
 * be living objects.
 */
void call_out() {
    extern struct object *command_giver;
    extern struct object *current_interactive;
    extern int current_time;
    extern int tracedepth;
    extern struct svalue *inter_sp;
    extern int32 initial_eval_cost, eval_cost, assigned_eval_cost;

    static int last_time;

    static struct call *current_call_out; /* don't set back with longjmp() */
    struct error_recovery_info error_recovery_info;
    struct svalue *sp;

    if (call_list == 0) {
	last_time = current_time;
	return;
    }
    if (last_time == 0)
	last_time = current_time;
    current_interactive = 0;
    call_list->delta -= current_time - last_time;
    last_time = current_time;
    error_recovery_info.last = error_recovery_pointer;
    error_recovery_info.type = ERROR_RECOVERY_BACKEND;
    error_recovery_pointer = &error_recovery_info;
    if (setjmp(error_recovery_info.con.text)) {
	extern void clear_state();

	struct call *cop;
	struct object *ob;
	struct wiz_list *user;

	clear_state();
	debug_message("Error in call out.\n");
	cop = current_call_out;
	if (cop->is_lambda) {
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
    tracedepth = 0;
    sp = inter_sp+1;
    while (call_list && call_list->delta <= 0) {
	struct call *cop;
	ph_int type;
	int num_arg;
	/*
	 * Move the first call_out out of the chain.
	 */
	cop = call_list;
	call_list = cop->next;
	current_call_out = cop;
	/*
	 * A special case:
	 * If a lot of time has passed, so that current call out was missed,
	 * then it will have a negative delta. This negative delta implies
	 * that the next call out in the list has to be adjusted.
	 */
	if (cop->delta < 0 && call_list)
	    call_list->delta += cop->delta;
	if (cop->command_giver && !(cop->command_giver->flags & O_DESTRUCTED))
	{
	    command_giver = cop->command_giver;
	    if (O_GET_INTERACTIVE(command_giver) &&
		O_GET_INTERACTIVE(command_giver)->sent.type == SENT_INTERACTIVE)
	    {
		trace_level = O_GET_INTERACTIVE(command_giver)->trace_level;
	    }
	} else {
	    struct object *ob;

	    if (cop->is_lambda)
		ob = !CLOSURE_MALLOCED(cop->function.lambda.x.closure_type) ?
		    cop->function.lambda.u.ob :
		    cop->function.lambda.u.lambda->ob;
	    else {
		ob = cop->function.named.ob;
		if (ob->flags & O_DESTRUCTED) {
		    free_call(cop);
		    continue;
		}
	    }
	    if (ob->flags & O_SHADOW) {
		struct shadow_sentence *shadow_sent;

		while((shadow_sent = O_GET_SHADOW(ob)), shadow_sent->shadowing)
		    ob = shadow_sent->shadowing;
		if (ob->flags & O_ENABLE_COMMANDS) {
		    command_giver = ob;
		    if (shadow_sent->type == SENT_INTERACTIVE)
			trace_level =
			  ((struct interactive *)shadow_sent)->trace_level;
		    else
			trace_level = 0;
		} else {
		    command_giver = 0;
		    trace_level = 0;
		}
	    } else {
		if (ob->flags & O_ENABLE_COMMANDS) {
		    command_giver = ob;
		} else {
		    command_giver = 0;
		}
		trace_level = 0;
	    }
	}
	if ((type = cop->v.type) == T_LVALUE) {
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
		} else {
		    *++sp = *v++;
		}
	    } while (--i);
	    xfree((char *)cop->v.u.lvalue);
	    inter_sp = sp;
	    sp -= num_arg - 1;
	} else {
	    /* single argument */
	    struct object *ob;

	    num_arg = 1;
	    inter_sp = sp;
	    if (type == T_OBJECT) {
		ob = cop->v.u.ob;
		if (ob->flags & O_DESTRUCTED) {
		    if (ob == &call_out_nil_object) {
			num_arg = 0;
			inter_sp = sp - 1;
		    } else {
			free_object(ob, "call_out");
			sp->type = T_NUMBER;
			sp->u.number = 0;
		    }
		} else {
		    sp->type = T_OBJECT;
		    sp->u.ob = ob;
		}
	    } else {
		*sp = cop->v;
	    }
	}
	if (cop->is_lambda) {
	    struct object *ob;
	    struct wiz_list *user;

	    ob = !CLOSURE_MALLOCED(cop->function.lambda.x.closure_type) ?
		cop->function.lambda.u.ob :
		cop->function.lambda.u.lambda->ob;
	    if (ob->flags & O_DESTRUCTED) {
		while (--num_arg >= 0)
		    pop_stack();
	    } else {
		current_object = ob;
		user = ob->user;
		if (user->last_call_out != current_time) {
		    user->last_call_out = current_time;
		    CLEAR_EVAL_COST;
		} else {
		    assigned_eval_cost = eval_cost = user->call_out_cost;
		}
		call_lambda(&cop->function.lambda, num_arg);
		user->call_out_cost = eval_cost;
		free_svalue(sp);
	    }
	    free_closure(&cop->function.lambda);
	} else {
	    struct object *ob;

	    if ((ob = cop->function.named.ob)->flags & O_DESTRUCTED) {
		while (--num_arg >= 0)
		    pop_stack();
	    } else {
		struct wiz_list *user;

		current_object = ob;
		user = ob->user;
		if (user->last_call_out != current_time) {
		    user->last_call_out = current_time;
		    CLEAR_EVAL_COST;
		} else {
		    assigned_eval_cost = eval_cost = user->call_out_cost;
		}
	        sapply(cop->function.named.name, ob, num_arg);
		user->call_out_cost = eval_cost;
	    }
	    free_string(cop->function.named.name);
	    free_object(cop->function.named.ob, "free_call");
	}
	free_used_call(cop);
    }
    inter_sp = sp - 1;
    error_recovery_pointer = error_recovery_info.last;
}

/*
 * Find / Throw away a call out. The first matching call_out is discarded if
 * do_free_call is true.
 * The time left until execution is returned.
 * -1 is returned if no call out pending.
 */
void find_call_out(ob, fun, do_free_call)
    struct object *ob;
    struct svalue *fun;
    int do_free_call;
{
    struct call **copp, *cop;
    int delay = 0;
    char *fun_name;

    if (fun->type != T_STRING) {
	ph_int type;

	if (fun->type == T_CLOSURE) {
	    if (!CLOSURE_MALLOCED(type = fun->x.closure_type) &&
		type >= CLOSURE_EFUN)
	    {
		for (copp = &call_list; cop = *copp; copp = &cop->next) {
		    delay += cop->delta;
		    if (cop->is_lambda &&
			cop->function.lambda.x.closure_type == type &&
			cop->function.lambda.u.ob == ob)
		    {
			goto found;
		    }
		}
		goto not_found;
	    } else if (type != CLOSURE_UNBOUND_LAMBDA) {
		struct lambda *l;

		l = fun->u.lambda;
		if (type != CLOSURE_LFUN)
		type = CLOSURE_UNBOUND_LAMBDA;
		for (copp = &call_list; cop = *copp; copp = &cop->next) {
		    delay += cop->delta;
		    if (cop->is_lambda && (
			  cop->function.lambda.u.lambda == l ||
			    cop->function.lambda.x.closure_type == type &&
			    cop->function.lambda.u.lambda->ob == l->ob &&
			    cop->function.lambda.u.lambda->function.index ==
			    l->function.index))
		    {
			goto found;
		    }
		}
not_found:
		free_svalue(fun);
		fun->type = T_NUMBER;
		fun->u.number = -1;
		return;
found:
		free_svalue(fun);
		fun->type = T_NUMBER;
		if (do_free_call) {
		    if (cop->next)
			cop->next->delta += cop->delta;
		    *copp = cop->next;
		    free_call(cop);
		}
		/* It is possible to have delay < 0 if we are
		 * inside call_out() .
		 */
		if (delay < 0)
		    delay = 0;
		fun->u.number = delay;
		return;
	    }
	}
	bad_efun_arg(1, -1, fun);
    }
    if (fun->x.string_type == STRING_SHARED) {
	fun_name = fun->u.string;
    } else {
	fun_name = make_shared_string(fun->u.string);
	if (fun->x.string_type == STRING_MALLOC)
	    xfree(fun->u.string);
    }
    fun->type = T_NUMBER;
    for (copp = &call_list; cop = *copp; copp = &cop->next) {
	delay += cop->delta;
	if (cop->function.named.ob == ob &&
	    cop->function.named.name == fun_name &&
	    !cop->is_lambda)
	{
	    decrement_string_ref(fun_name);
	    if (do_free_call) {
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

int print_call_out_usage(verbose)
    int verbose;
{
    int i;
    struct call *cop;

    for (i=0, cop = call_list; cop; cop = cop->next)
	i++;
    if (verbose) {
	add_message("\nCall out information:\n");
	add_message("---------------------\n");
	add_message("Number of allocated call outs: %8d, %8d bytes\n",
		    num_call, num_call * sizeof (struct call));
	add_message("Current length: %d\n", i);
    } else {
	add_message("call out:\t\t\t%8d %8d (current length %d)\n", num_call,
		    num_call * sizeof (struct call), i);
    }
    return num_call * sizeof (struct call);
}

#ifdef DEBUG
void count_extra_ref_from_call_outs()
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

void remove_stale_call_outs() {
    struct call **copp, *cop;

    for (copp = &call_list; cop = *copp; ) {
	if ( (cop->is_lambda ?
		(CLOSURE_MALLOCED(cop->function.lambda.x.closure_type) ?
		  cop->function.lambda.u.lambda->ob :
		  cop->function.lambda.u.ob) :
		cop->function.named.ob
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
void clear_ref_from_call_outs()
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
	if ((ob = cop->command_giver) && ob->flags & O_DESTRUCTED) {
	    ob->ref = 0;
	    ob->prog->ref = 0;
	}
    }
}

void count_ref_from_call_outs()
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
	if (ob = cop->command_giver) {
	    if (ob->flags & O_DESTRUCTED) {
		reference_destructed_object(ob);
		cop->command_giver = 0;
	    } else {
		ob->ref++;
	    }
	}
    }
}
#endif

/*
 * Construct an array of all pending call_outs. Every item in the array
 * consists of 4 items (but only if the object not is destructed):
 * 0:	The object.
 * 1:	The function (string).
 * 2:	The delay.
 * 3:	The argument.
 *      If there is more than one argument, the extra arguments are put
 *      in extra members of the array.
 */
struct vector *get_all_call_outs() {
    int i, next_time;
    struct call *cop;
    struct vector *v;

    for (i=0, cop = call_list; cop; cop = cop->next) {
	if (!cop->is_lambda && cop->function.named.ob->flags & O_DESTRUCTED)
	    continue;
	i++;
    }
    v = allocate_array(i);
    next_time = 0;
    /*
     * Take for granted that all items in an array are initialized to
     * number 0.
     */
    for (i=0, cop = call_list; cop; cop = cop->next) {
	struct vector *vv;
	char *function_name;

	next_time += cop->delta;
	vv = allocate_array(
	  cop->v.type == T_LVALUE ?
	    3 + cop->v.x.num_arg :
	    (cop->v.u.ob == &call_out_nil_object && cop->v.type == T_OBJECT ?
		3 : 4)
	);
	if (cop->is_lambda) {
	    assign_svalue_no_free(&vv->item[1], &cop->function.lambda);
	} else {
	    struct object *ob;

	    ob = cop->function.named.ob;
	    if (ob->flags & O_DESTRUCTED) {
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
	if (cop->v.type == T_LVALUE) {
	    struct svalue *source, *dest;
	    int j;

	    source = cop->v.u.lvalue;
	    dest = &vv->item[3];
	    j = cop->v.x.num_arg;
	    do {
		assign_svalue_no_free(dest++, source++);
	    } while (--j);
	} else if (cop->v.u.ob != &call_out_nil_object ||
		   cop->v.type != T_OBJECT)
	{
	    assign_svalue_no_free(&vv->item[3], &cop->v);
	}

	v->item[i].type = T_POINTER;
	v->item[i].u.vec = vv;		/* Ref count is already 1 */
	i++;
    }
    return v;
}
