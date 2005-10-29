#include <stdio.h>
#include <setjmp.h>
#include <string.h>
#include <ctype.h>
#include "y.tab.h"
#include "lnode.h"
#include "interpret.h"
#include "config.h"
#include "object.h"
#include "wiz_list.h"

#define DEBUG_EXECUTION (d_flag)
#define ALLOC_CHUNK 50

static struct value *print_function(), *print_expr(), *print_expr_list(),
    *inter_sscanf(), *get_one_expr();
extern int transfer_object();
char *string_copy(), *xalloc(), *crypt();

extern struct value *call_indirect();
extern struct object *previous_ob;

struct lnode_var_def *find_status();

extern void free(), debug_message(), debug_message_value(), fatal(), abort(),
    add_message(), error(), exit(), add_ref();

extern long time();

#ifdef DRAND48
extern double drand48();
#endif
#ifdef RANDOM
extern long random();
#endif

int d_flag;

extern int current_line, eval_cost;

struct value **current_local_names;

/*
 * This variable is set to true when return is called inside a function.
 */
static int stop_function;

/*
 * The only reason that the long jump data is in a struct, is that
 * it will be easy to copy. The copy is done so we can stack up
 * several return contexts, and pop back them.
 */
struct context {
    jmp_buf a;
    int valid;
};

/*
 * This is used for 'break' in while-statements.
 */
struct context break_context;

#ifdef TRACE
struct trace {
    char *function_name;
    char *object_name;
    char *command_name;
    int line;
};

#define MAX_TRACE	100
struct trace trace_back[MAX_TRACE];
int trace_depth = 0, worst_depth;
#endif /* TRACE */

struct value *return_value;	/* Here is a value when "return". */
struct value *current_argument;	/* Arguments to current function */

/*
 * The 'eval' flag is true if the function is supposed to be evaluated,
 * not printed.
 */

struct value *make_string(str)
    char *str;
{
    struct value *ret = alloc_value();

    ret->type = T_STRING;
    ret->u.string = string_copy(str);
    return ret;
}

struct value *make_number(n)
    int n;
{
    struct value *ret = alloc_value();

    ret->type = T_NUMBER;
    ret->u.number = n;
    return ret;
}

/*
 * Assign a value to a variable or argument.
 */

void assign(p, v)
    struct lnode_number *p;
    struct value *v;
{
    if (p->type == F_LOCAL_NAME) {
	struct value *l;
	if (v == 0) {
	    current_local_names[p->number] = &const0;
	    return;
	}
	l = alloc_value();
	current_local_names[p->number] = l;
	l->type = v->type;
	l->u = v->u;
	if (l->type == T_STRING)
	    l->u.string = string_copy(v->u.string);
	else if (l->type == T_OBJECT)
	    add_ref(l->u.ob, "ass to loc var");
	return;
    }
    if (p->type == F_ARGUMENT) {		/* Is it an argument ? */
	if (v == 0) {
	    current_argument = &const0;
	    return;
	}
	current_argument = alloc_value();
	current_argument->type = v->type;
	current_argument->u = v->u;
	if (v->type == T_STRING)
	    current_argument->u.string = string_copy(v->u.string);
	else if (v->type == T_OBJECT)
	    add_ref(v->u.ob, "ass to arg");
    } else {
	struct value *dest = find_value((struct lnode_variable *)p);
	if (dest->type == T_STRING)
	    free(dest->u.string);
	else if (dest->type == T_OBJECT)
	    free_object(dest->u.ob, "ass to var");
	if (v == 0) {
	    dest->type = T_NUMBER;
	    dest->u.number = 0;
	    return;
	}
	dest->type = v->type;
	dest->u = v->u;
	if (v->type == T_STRING)
	    dest->u.string = string_copy(v->u.string);
	else if (v->type == T_OBJECT)
	    add_ref(dest->u.ob, "ass to var");
    }
}

static struct value *free_value_list;
static struct value *alloced_value_list;
static int num_alloc;
int tot_alloc_value;

/*
 * Free all values previously allocated.
 */
void free_all_values() {
    for(num_alloc = 0; alloced_value_list; num_alloc++) {
	struct value *tmp;
	tmp = alloced_value_list;
	alloced_value_list = alloced_value_list->next;
	if (tmp->type == T_STRING) {
	    free(tmp->u.string);
	    tmp->u.string = (char *)1;	    /* Just an illegal value */
	}
	/* The following is a major kludge by Drax.  Dbx(1) told me in a
	   core analysis that this routine passed a null pointer to
	   free_object.  Therefore, we should just check to make sure
	   that the pointer does in fact have a value. The following code
	   used to be
	else if (tmp->type == T_OBJECT) {
	    free_object(tmp->u.ob, "free_all_values");
	    tmp->u.ob = 0;
	}
	   but has been changed to: */
	else if ((tmp->type == T_OBJECT) && tmp->u.ob) {
	    free_object(tmp->u.ob, "free_all_values");
	    tmp->u.ob = 0;
	}
	tmp->next = free_value_list;
	free_value_list = tmp;
    }
    if (d_flag)
	debug_message("%d alloced values.\n", num_alloc);
}

int count_value_ref(ob)
    struct object *ob;
{
    int tot = 0;
    struct value *v;
    for(v = alloced_value_list; v; v = v->next) {
	if (v->type == T_OBJECT && v->u.ob == ob)
	    tot++;
    }
    return tot;
}

struct value *alloc_value() {
    register struct value *tmp;

    if (free_value_list == 0) {
 	int i;
 	tmp = (struct value *)xalloc(ALLOC_CHUNK * sizeof (struct value));
 	tot_alloc_value += ALLOC_CHUNK;
 	for (i=0; i < ALLOC_CHUNK - 1; i++)
 	    tmp[i].next = &tmp[i+1];
 	tmp[ALLOC_CHUNK - 1].next = 0;
 	free_value_list = tmp;
    }
    tmp = free_value_list;
    free_value_list = free_value_list->next;
    tmp->next = alloced_value_list;
    alloced_value_list = tmp;
    /*
     * We preset the type to number, to ensure that no one forgets to
     * use the value, as it would have an old, undefined type when arriving
     * to free_all_values().
     */
    tmp->type = T_NUMBER;
    return tmp;
}
/*
 * Print or interpret a function.
 * Set up the long jump context, to enable the 'return' statment.
 */

static struct value *print_function(p)
    struct lnode_def *p;
{
    struct value **save_current_local_names = current_local_names;
    struct value *new_local_names[MAX_LOCAL];
    int i;
#ifdef TRACE
    int save_trace_depth = trace_depth;
    char *get_current_object_name(), *get_command_giver_name();
    if (trace_depth < MAX_TRACE) {
	trace_back[trace_depth].function_name = p->name;
	trace_back[trace_depth].object_name = get_current_object_name();
	trace_back[trace_depth].command_name = get_command_giver_name();
	trace_back[trace_depth].line = -1;
	trace_depth++;
    } else {
	error("Recursion is too deep to trace.\n");
    }
#endif

    if (p->type == F_IDENTIFIER) {
	if (d_flag) {
#ifdef TRACE
	    debug_message("%s: ", get_current_object_name());
#endif
	    debug_message("%s(", p->name);
	    debug_message_value(current_argument);
	    debug_message(")\n");
	}
	for (i=0; i<p->num_var; i++) {
	    new_local_names[i] = &const0;
	}
	current_local_names = new_local_names;
	return_value = &const0;
	(void)print_expr(p->block);
	stop_function = 0;
#ifdef TRACE
 	if (trace_depth > worst_depth) {
 	    fprintf(stderr, "New worst trace depth: %d\n", trace_depth);
 	    worst_depth = trace_depth;
 	}
	trace_depth = save_trace_depth;
#endif
	current_local_names = save_current_local_names;
	return return_value;
    }
    fatal("Illegal type %d\n", p->type);
    return 0;
}

static struct value *print_expr(p)
    struct lnode *p;
{
    struct value *ret = &const0;
    struct value *arg, *arg1, *arg2;
    struct lnode_block *lb;
    char *block;
    int i;

    if (const1.u.number != 1)
	fatal("Change in constant 1\n");
    if (const0.u.number != 0)
	fatal("Change in constant 0\n");
    eval_cost++;
    if (current_object->wl)
	current_object->wl->cost++;
    if (eval_cost > MAX_COST) {
        eval_cost = 0;
	error("Too long evaluation. Execution aborted.\n");
    }
    if (p == 0) {
	fatal("print_expr called with null pointer.\n");
	abort();
    }
    current_line = p->line & ~L_MASK;
    switch(p->type) {
    default:
	fatal("UNKNOWN(%d)", p->type);
	break;
    case F_CLONE_OBJECT:
	if (DEBUG_EXECUTION)
	    debug_message("clone_object()\n");
	arg1 = print_expr_list(p->a1);
	if (arg1 == 0 || arg1->type != T_STRING) {
	    error("Illegal type argument to clone_object()\n");
	    exit(1);
	}
	ret = call_indirect(F_CLONE_OBJECT, arg1->u.string);
	break;
    case F_FUNCTION:
	{
	    struct lnode_funcall *f = (struct lnode_funcall *)p;
	    if (DEBUG_EXECUTION)
		debug_message("%s()\n", f->name);
#ifdef TRACE
	    trace_back[trace_depth-1].line = f->line & ~L_MASK;
#endif
	    if (p->a1)
		arg = print_expr(p->a1);
	    else
		arg = 0;
	    ret = call_indirect(F_FUNCTION, f->name, arg);
	    break;
	}
    case F_SAVE_OBJECT:
	if (DEBUG_EXECUTION)
	    debug_message("save_object()\n");
	arg = print_expr_list(p->a1);
	if (arg == 0 || arg->type != T_STRING)
	    error("Bad argument to save_object().\n");
	ret = call_indirect(F_SAVE_OBJECT, arg->u.string);
	break;
    case F_FIND_OBJECT:
	if (DEBUG_EXECUTION)
	    debug_message("find_object()");
	arg = print_expr_list(p->a1);
	if (arg == 0 || arg->type != T_STRING)
	    error("Bad argument to find_object().\n");
	ret = call_indirect(F_FIND_OBJECT, arg->u.string);
	break;
    case F_FIND_LIVING:
	if (DEBUG_EXECUTION)
	    debug_message("find_living()");
	arg = print_expr_list(p->a1);
	if (arg == 0 || arg->type != T_STRING)
	    error("Bad argument to find_living().\n");
	ret = call_indirect(F_FIND_LIVING, arg->u.string);
	break;
    case F_FIND_PLAYER:
	if (DEBUG_EXECUTION)
	    debug_message("find_player()");
	arg = print_expr_list(p->a1);
	if (arg == 0 || arg->type != T_STRING)
	    error("Bad argument to find_player().\n");
	ret = call_indirect(F_FIND_PLAYER, arg->u.string);
	break;
    case F_TELL_OBJECT:
	if (DEBUG_EXECUTION)
	    debug_message("tell_object()\n");
	{
	    struct value *who, *what;
	    struct lnode *l = p->a1;

	    who = get_one_expr(&l, "tell_object()");
	    what = get_one_expr(&l, "tell_object()");
	    if (who == 0 || who->type != T_OBJECT)
		error("tell_object() first argument not an object.\n");
	    if (what == 0 || what->type != T_STRING)
		error("tell_object() second argument not a string.\n");
	    (void)call_indirect(F_TELL_OBJECT, who->u.ob, what->u.string);
	}
	break;
    case F_RESTORE_OBJECT:
	if (DEBUG_EXECUTION)
	    debug_message("restore_object()\n");
	arg = print_expr_list(p->a1);
	if (arg == 0 || arg->type != T_STRING)
	    error("Bad argument to restore_object().\n");
	ret = call_indirect(F_RESTORE_OBJECT, arg->u.string);
	break;
    case F_THIS_PLAYER:
	if (DEBUG_EXECUTION)
	    debug_message("this_player()\n");
	ret = call_indirect(F_THIS_PLAYER);
	break;
    case F_FIRST_INVENTORY:
	if (DEBUG_EXECUTION)
	    debug_message("first_inventory()\n");
	arg = print_expr_list(p->a1);
	if (arg == 0 || (arg->type != T_STRING && arg->type != T_OBJECT))
	    error("Bad type argument to first_inventory()\n");
	ret = call_indirect(F_FIRST_INVENTORY, arg);
	break;
    case F_LIVING:
	if (DEBUG_EXECUTION)
	    debug_message("living()\n");
	arg = print_expr_list(p->a1);
	if (arg == 0 || arg->type != T_OBJECT)
	    error("Bad type argument to living()\n");
	ret = call_indirect(F_LIVING, arg->u.ob);
	break;
    case F_SHUTDOWN:
	if (DEBUG_EXECUTION)
	    debug_message("shutdown()\n");
	(void)call_indirect(F_SHUTDOWN);
	return 0;
    case F_NEXT_INVENTORY:
	if (DEBUG_EXECUTION)
	    debug_message("next_inventory()\n");
	arg = print_expr_list(p->a1);
	if (arg == 0 || (arg->type != T_STRING && arg->type != T_OBJECT))
	    error("Bad type argument to next_inventory()\n");
	ret = call_indirect(F_NEXT_INVENTORY, arg);
	break;
    case F_ENVIRONMENT:
	if (DEBUG_EXECUTION)
	    debug_message("environment()\n");
	arg = print_expr_list(p->a1);
	if (arg && arg->type != T_STRING && arg->type != T_OBJECT)
	    error("Wrong type to optional arg to environment()\n");
	ret = call_indirect(F_ENVIRONMENT, arg);
	break;
    case F_THIS_OBJECT:
	if (DEBUG_EXECUTION)
	    debug_message("this_object()\n");
	ret = call_indirect(F_THIS_OBJECT);
	break;
    case F_PEOPLE:
	if (DEBUG_EXECUTION)
	    debug_message("people()\n");
	(void)call_indirect(F_PEOPLE);
	return 0;
    case F_LOCALCMD:
	if (DEBUG_EXECUTION)
	    debug_message("localcmd()\n");
	print_local_commands();
	return 0;
    case F_SWAP:
	if (DEBUG_EXECUTION)
	    debug_message("swap()");
	arg = print_expr_list(p->a1);
	if (arg == 0 || arg->type != T_OBJECT)
	    error("bad argument to swap()\n");
	swap(arg->u.ob);
	return 0;
    case F_TIME:
 	if (DEBUG_EXECUTION)
 	    debug_message("time()");
 	ret = make_number(time(0l));
 	break;
    case F_WIZLIST:
	if (DEBUG_EXECUTION)
	    debug_message("wizlist()\n");
	(void)call_indirect(F_WIZLIST);
	return 0;
    case F_TRANSFER:
	if (DEBUG_EXECUTION)
	    debug_message("transfer()\n");
    {
	struct object *dest;
	struct lnode *l = p->a1;
	int i;
	arg = get_one_expr(&l, "transfer");
	if (arg->type != T_OBJECT)
	    error("Bad type of artgument 1 to transfer()\n");
	arg1 = get_one_expr(&l, "transfer");
	if (arg1->type != T_OBJECT && arg1->type != T_STRING)
	    error("Bad type of artgument 2 to transfer()\n");
	if (arg1->type == T_STRING) {
	    dest = find_object(arg1->u.string);
	    if (dest == 0)
		error("Object not found.\n");
	} else {
	    dest = arg1->u.ob;
	}
	i = transfer_object(arg->u.ob, dest);
	if (i == 0)
	    ret = &const0;	/* Usually this */
	else if (i == 1)
	    ret = &const1;
	else
	    ret = make_number(i);
    }
	break;
    case F_ADD_WORTH:
	if (DEBUG_EXECUTION)
	    debug_message("add_worth()\n");
    {
	struct lnode *l = p->a1;
	arg = get_one_expr(&l, "add_worth");
	if (arg->type != T_NUMBER)
	    error("Bad type of argument 1 to add_worth()\n");
	arg1 = get_one_expr(&l, 0);
	if (arg1 != 0 && arg1->type != T_OBJECT)
	    error("Bad type of argument 2 to add_worth()\n");
	if (strncmp(current_object->name, "obj/", 4) != 0 &&
	    strncmp(current_object->name, "room/", 5) != 0)
	    error("Illegal call of add_worth.\n");
	if (arg1) {
	    if (arg1->u.ob->wl)
		arg1->u.ob->wl->total_worth += arg->u.number;
	    return 0;
	}
	if (previous_ob == 0)
	    return 0;
	if (previous_ob->wl)
	    previous_ob->wl->total_worth += arg->u.number;
    }
	return 0;
    case F_ADD:
	if (DEBUG_EXECUTION)
	    debug_message("+()\n");
	ret = alloc_value();
	arg1 = print_expr(p->a1);
	arg2 = print_expr(p->a2);
	if (arg1 == 0 || arg2 == 0)
	    error("Bad type on arg to '+'\n");
	if (arg1->type == T_STRING && arg2->type == T_STRING) {
	    ret->u.string = xalloc(strlen(arg1->u.string) +
				   strlen(arg2->u.string) + 1);
	    (void)strcpy(ret->u.string, arg1->u.string);
	    (void)strcat(ret->u.string, arg2->u.string);
	    ret->type = T_STRING;
	    break;
	}
	if (arg1->type == T_NUMBER && arg2->type == T_STRING) {
	    char buff[20];
	    sprintf(buff, "%d", arg1->u.number);
	    ret->type = T_STRING;
	    ret->u.string = xalloc(strlen(arg2->u.string) +
				   strlen(buff) + 1);
	    strcpy(ret->u.string, buff);
	    strcat(ret->u.string, arg2->u.string);
	    break;
	}
	if (arg2->type == T_NUMBER && arg1->type == T_STRING) {
	    char buff[20];
	    sprintf(buff, "%d", arg2->u.number);
	    ret->type = T_STRING;
	    ret->u.string = xalloc(strlen(arg1->u.string) +
				   strlen(buff) + 1);
	    strcpy(ret->u.string, arg1->u.string);
	    strcat(ret->u.string, buff);
	    break;
	}
	if (arg1->type == T_NUMBER && arg2->type == T_NUMBER) {
	    ret->u.number = arg1->u.number + arg2->u.number;
	    ret->type = T_NUMBER;
	    break;
	}
	error("Bad type of arg to '+'\n");
	exit(1);
    case F_SUBTRACT:
	if (DEBUG_EXECUTION)
	    debug_message("-()\n");
	arg1 = print_expr(p->a1);
	arg2 = print_expr(p->a2);
	if ((arg1 && arg1->type != T_NUMBER) || (arg2 && arg2->type != T_NUMBER)) {
	    error("Bad type on arg to '-'\n");
	    exit(1);
	}
	ret = make_number(arg1->u.number - arg2->u.number);
	break;
    case F_AND:
	if (DEBUG_EXECUTION)
	    debug_message("and()\n");
	arg1 = print_expr(p->a1);
	if (arg1 == 0 || (arg1->type == T_NUMBER && arg1->u.number == 0))
	    break;
	arg2 = print_expr(p->a2);
	if (arg2 == 0 || (arg2->type == T_NUMBER && arg2->u.number == 0))
	    break;
	ret = &const1;
	break;
    case F_OR:
	if (DEBUG_EXECUTION)
	    debug_message("or()\n");
	ret = print_expr(p->a1);
	if (ret && ((ret->type == T_NUMBER && ret->u.number != 0) ||
		    ret->type != T_NUMBER))
	    break;
	ret = print_expr(p->a2);
	if (ret && ((ret->type == T_NUMBER && ret->u.number != 0) ||
		    ret->type != T_NUMBER))
	    break;
	ret = &const0;
	break;
    case F_MULTIPLY:
	if (DEBUG_EXECUTION)
	    debug_message("mult()\n");
	arg1 = print_expr(p->a1);
	arg2 = print_expr(p->a2);
	if ((arg1 && arg1->type != T_NUMBER) || (arg2 && arg2->type != T_NUMBER)) {
	    error("Bad type on arg to '*'\n");
	    exit(1);
	}
	ret = make_number(arg1->u.number * arg2->u.number);
	break;
    case F_DIVIDE:
	if (DEBUG_EXECUTION)
	    debug_message("div()\n");
	arg1 = print_expr(p->a1);
	arg2 = print_expr(p->a2);
	if ((!arg1 || arg1->type != T_NUMBER || !arg2 || arg2->type != T_NUMBER)) {
	    error("Bad type on arg to '/'\n");
	    exit(1);
	}
	if (arg2->u.number == 0)
	    error("Division by zero.\n");
	ret = make_number(arg1->u.number / arg2->u.number);
	break;
    case F_MOD:
	if (DEBUG_EXECUTION)
	    debug_message("mod()\n");
	arg1 = print_expr(p->a1);
	arg2 = print_expr(p->a2);
	if (arg1->type != T_NUMBER || arg2->type != T_NUMBER) {
	    error("Bad type on arg to %%\n");
	    exit(1);
	}
	if (arg2->u.number == 0)
	    error("Modulus by zero.\n");
	ret = make_number(arg1->u.number % arg2->u.number);
	break;
    case F_MOD_EQ:
	if (DEBUG_EXECUTION)
	    debug_message("%%=\n");
	{
	    struct value *v;
	    ret = print_expr(p->a2);
	    if (ret == 0 || ret->type != T_NUMBER)
		error("Bad type to %%=.\n");
	    v = print_expr(p->a1);
	    if (v == 0 || v->type != T_NUMBER)
		error("Bad type to %%=");
	    if (ret->u.number == 0)
		error("Modulus by zero.");
	    assign((struct lnode_number *)(p->a1),
		   make_number(v->u.number % ret->u.number));
	}
	return 0;
    case F_GT:
	if (DEBUG_EXECUTION)
	    debug_message("gt()\n");
	arg1 = print_expr(p->a1);
	arg2 = print_expr(p->a2);
	if ((!arg1 || arg1->type != T_NUMBER || !arg2 || arg2->type != T_NUMBER)) {
	    error("Bad type on arg to '>'\n");
	    exit(1);
	}
	if (arg1->u.number > arg2->u.number)
	    ret = &const1;
	else
	    ret = &const0;
	break;
    case F_GE:
	if (DEBUG_EXECUTION)
	    debug_message("GE()\n");
	arg1 = print_expr(p->a1);
	arg2 = print_expr(p->a2);
	if ((!arg1 || arg1->type != T_NUMBER || !arg2 || arg2->type != T_NUMBER)) {
	    error("Bad type on arg to '>='\n");
	    exit(1);
	}
	if (arg1->u.number >= arg2->u.number)
	    ret = &const1;
	else
	    ret = &const0;
	break;
    case F_LT:
	if (DEBUG_EXECUTION)
	    debug_message("lt()\n");
	arg1 = print_expr(p->a1);
	arg2 = print_expr(p->a2);
	if ((!arg1 || arg1->type != T_NUMBER || !arg2 || arg2->type != T_NUMBER)) {
	    error("Bad type on arg to '<'\n");
	    exit(1);
	}
	if (arg1->u.number < arg2->u.number)
	    ret = &const1;
	else
	    ret = &const0;
	break;
    case F_LE:
	if (DEBUG_EXECUTION)
	    debug_message("le()\n");
	arg1 = print_expr(p->a1);
	arg2 = print_expr(p->a2);
	if ((!arg1 || arg1->type != T_NUMBER || !arg2 || arg2->type != T_NUMBER)) {
	    error("Bad type on arg to '<='\n");
	    exit(1);
	}
	if (arg1->u.number <= arg2->u.number)
	    ret = &const1;
	else
	    ret = &const0;
	break;
    case F_EQ:
	if (DEBUG_EXECUTION)
	    debug_message("eq()\n");
	arg1 = print_expr(p->a1);
	arg2 = print_expr(p->a2);
	if (arg1 == 0 && arg2 != 0 || arg1 != 0 && arg2 == 0)
	    break;
	ret = alloc_value();
	ret->type = T_NUMBER;
	if (arg1 == 0 && arg2 == 0) {
	    ret->u.number = 1;
	} else if (arg1->type == T_NUMBER && arg2->type == T_NUMBER) {
	    ret->u.number = arg1->u.number == arg2->u.number;
	} else if (arg1->type == T_STRING && arg2->type == T_STRING) {
	    ret->u.number = !strcmp(arg1->u.string, arg2->u.string);
	} else if (arg1->type == T_OBJECT && arg2->type == T_OBJECT) {
	    ret->u.number = arg1->u.ob == arg2->u.ob;
	} else
	    ret->u.number = 0;
	break;
    case F_NE:
	if (DEBUG_EXECUTION)
	    debug_message("ne()\n");
	arg1 = print_expr(p->a1);
	arg2 = print_expr(p->a2);
	if (arg1 == 0) {
	    if (arg2 == 0)
		break;
	    ret = &const1;
	    break;
	}
	if (arg2 == 0) {
	    ret = &const1;
	    break;
	}
	if (arg1->type != arg2->type) {
	    ret = &const1;
	    break;
	}
	ret = alloc_value();
	ret->type = T_NUMBER;
	if (arg1->type == T_NUMBER) {
	    ret->u.number = arg1->u.number != arg2->u.number;
	} else if (arg1->type == T_STRING) {
	    ret->u.number = strcmp(arg1->u.string, arg2->u.string);
	} else if (arg1->type == T_OBJECT) {
	    ret->u.number = arg1->u.ob != arg2->u.ob;
	} else
	    ret->u.number = 0;
	break;
    case F_BLOCK:	/* Sequential list of statements. */
	if (DEBUG_EXECUTION)
	    debug_message("block block:\n");
    {
	lb = (struct lnode_block *)p;
	block = lb->block;
	
	for (i=0; i < lb->num_nodes && !stop_function; i++) {
	    print_expr((struct lnode *)block);
	    block += lnode_size[((struct lnode *)block)->line >> L_SHIFT];
	}
	return 0;
    }
    case F_CONS:	/* Linked list of statements. */
	if (DEBUG_EXECUTION)
	    debug_message("cons block:\n");
	while(p && !stop_function) {
	    (void)print_expr(p->a1);
	    p = p->a2;
	}
	if (DEBUG_EXECUTION)
	    debug_message("end block.\n");
	return 0;
    case F_IF:
	if (DEBUG_EXECUTION)
	    debug_message("if()\n");
	ret = print_expr(p->a1);
	if (ret && ((ret->type == T_NUMBER && ret->u.number) ||
		    ret->type == T_STRING || ret->type == T_OBJECT))
	    (void)print_expr(p->a2);
	else if (p->a3)
	    (void)print_expr(p->a3);
	return 0;
    case F_ARGUMENT:
	if (DEBUG_EXECUTION)
	    debug_message("ARG\n");
	if (current_argument == 0) {
	    ret = &const0;
	    break;
	}
	ret = alloc_value();
	ret->type = current_argument->type;
	ret->u = current_argument->u;
	if (ret->type == T_STRING)
	    ret->u.string = string_copy(current_argument->u.string);
	else if (ret->type == T_OBJECT) {
	    if (ret->u.ob->destructed)
		ret = &const0;
	    add_ref(current_argument->u.ob, "cp arg");
	}
	break;
    case F_IDENTIFIER:
	if (DEBUG_EXECUTION)
	    debug_message("var(%d)\n", ((struct lnode_number *)p)->number);
        {
	    struct value *v = find_value((struct lnode_variable *)p);
	    ret = alloc_value();
	    ret->type = v->type;
	    ret->u = v->u;
	    if (v->type == T_STRING)
		ret->u.string = string_copy(v->u.string);
	    else if (v->type == T_OBJECT) {
		if (v->u.ob->destructed)
		    ret = &const0;
		add_ref(v->u.ob, "cp var");
	    }
	}
	break;
    case F_RETURN:
	if (DEBUG_EXECUTION)
	    debug_message("return()\n");
	if (p->a1)
	    return_value = print_expr(p->a1);
	else
	    return_value = &const0;
	stop_function = 1;
	return 0;
    case F_BREAK:
	if (DEBUG_EXECUTION)
	    debug_message("break\n");
	if (!break_context.valid)
	    error("Illegal break statement!\n");
	longjmp(break_context.a, 1);
	fatal("Return from longjmp\n");
    case F_CONTINUE:
	if (DEBUG_EXECUTION)
	    debug_message("continue\n");
	if (!break_context.valid)
	    error("Illegal continue statement!\n");
	longjmp(break_context.a, 2);
	fatal("Return from longjmp\n");
    case F_LOG_FILE:
	if (DEBUG_EXECUTION)
	    debug_message("log_file");
        {
	    struct lnode *l;
	    l = p->a1;
	    arg1 = get_one_expr(&l, "log_file");
	    arg2 = get_one_expr(&l, "log_file");
	    if (arg1 == 0 || arg1->type != T_STRING ||
		arg2 == 0 || arg2->type != T_STRING)
		error("Bad type argument to log_file().\n");
	    (void)call_indirect(F_LOG_FILE, arg1->u.string, arg2->u.string);
	}
	return 0;
    case F_NOT:
	if (DEBUG_EXECUTION)
	    debug_message("not()\n");
	arg = print_expr(p->a1);
	if (arg == 0 || (arg->type == T_NUMBER && arg->u.number == 0))
	    ret = &const1;
	else
	    ret = &const0;
	break;
    case F_NEGATE:
	if (DEBUG_EXECUTION)
	    debug_message("negate()\n");
	arg = print_expr(p->a1);
	if (arg == 0 || arg->type != T_NUMBER)
	    error("Bad argument to unary '-'\n");
	ret = make_number(- arg->u.number);
	break;
    case F_CALL_OTHER:
	if (DEBUG_EXECUTION)
	    debug_message("call_other()\n");
	{
	    struct value *a1, *a2, *a3;
	    struct lnode *l = p->a1;
	    a1 = get_one_expr(&l, "call_other");
	    a2 = get_one_expr(&l, "call_other");
	    a3 = get_one_expr(&l, (char *)0);
	    if (a1 == 0 || (a1->type != T_STRING &&
			    a1->type != T_OBJECT)) {
		error("Wrong type arg 1 to call_other()\n");
		exit(1);
	    }
	    if (a2->type != T_STRING) {
		error("Wrong type arg 2 to call_other()\n");
		exit(1);
	    }
	    ret = call_indirect(F_CALL_OTHER, a1, a2->u.string, a3);
	}
	break;
    case F_WRITE:
	if (DEBUG_EXECUTION)
	    debug_message("write()\n");
	arg = print_expr_list(p->a1);
	if (arg == 0)
	    error("Bad argument to write()");
	(void)call_indirect(F_WRITE, arg);
	return 0;
    case F_REGCOMP:
	if (DEBUG_EXECUTION)
	    debug_message("regcomp()\n");
	arg = print_expr_list(p->a1);
	if (arg == 0 || arg->type != T_STRING)
	    error("Bad argument to regcomp()");
	ret = alloc_value();
	ret->type = T_STRING;
	ret->u.string = (char *)regcomp(arg->u.string);
debug_message("regcomp: u.string: %x\n",ret->u.string);
	break;
    case F_REGEXEC:
	if (DEBUG_EXECUTION)
	    debug_message("regexec()");
    {
	struct lnode *l = p->a1;
	arg1 = get_one_expr(&l, "regexec");
	arg2 = get_one_expr(&l, "regexec");
	if (arg1 == 0 || arg2->type != T_STRING)
	    error("Bad arg 1 to regexec.\n");
	if (arg2 == 0 || arg2->type != T_STRING)
	    error("Bad arg 2 to regexec.\n");
debug_message("regexec: arg1 u.string: %x\n",arg1->u.string);
debug_message("regexec: arg2 u.string: %x\n",arg2->u.string);
	if (regexec(arg1->u.string, arg2->u.string))
	    ret = &const1;
	else
	    ret = &const0;
    }
    case F_MOVE_OBJECT:
	if (DEBUG_EXECUTION)
	    debug_message("move_object()\n");
	{
	    struct lnode *l = p->a1;
	    arg1 = get_one_expr(&l, "move_object");
	    arg2 = get_one_expr(&l, "move_object");
	    if (arg1 == 0 || (arg1->type != T_STRING &&
			      arg1->type != T_OBJECT)) {
		error("Bad type argument to move_object()\n");
		return 0;
	    }
	    if (arg2 == 0 || (arg2->type != T_STRING &&
			      arg2->type != T_OBJECT)) {
		error("Bad type argument to move_object()\n");
		return 0;
	    }
	    (void)call_indirect(F_MOVE_OBJECT, arg1, arg2);
	    return 0;
	}
    case F_SNOOP:
	if (DEBUG_EXECUTION)
	    debug_message("snoop()\n");
	{
	    struct lnode *l = p->a1;
	    arg1 = get_one_expr(&l, (char *)0);
	    if (arg1 && arg1->type != T_OBJECT)
		error("Bad type arg to snoop.\n");
	    (void)call_indirect(F_SNOOP, arg1 ? arg1->u.ob : 0);
	    return 0;
	}
    case F_ADD_ACTION:
	if (DEBUG_EXECUTION)
	    debug_message("add_action()\n");
	arg1 = print_expr_list(p->a1);
	if (arg1 == 0 || arg1->type != T_STRING) {
	    error("Bad type argument to add_action()\n");
	    exit(1);
	}
	(void)call_indirect(F_ADD_ACTION, arg1->u.string, 0);
	return 0;
    case F_ADD_VERB:
	if (DEBUG_EXECUTION)
	    debug_message("add_verb()\n");
	ret = print_expr_list(p->a1);
	if (ret == 0 || ret->type != T_STRING) {
	    error("Bad type argument to add_verb()\n");
	    exit(1);
	}
	(void)call_indirect(F_ADD_VERB, ret->u.string);
	return 0;
    case F_ED:
	if (DEBUG_EXECUTION)
	    debug_message("ed()\n");
	arg = print_expr_list(p->a1);
	if (arg && arg->type != T_STRING)
	    error("Bad argument to ed().\n");
	(void)call_indirect(F_ED,
			    arg ? arg->u.string : 0);
	return 0;
    case F_CRYPT:
	if (DEBUG_EXECUTION)
	    debug_message("crypt()\n");
	arg = print_expr_list(p->a1);
	if (arg == 0 || arg->type != T_STRING)
	    error("Bad type arg to crypt.\n");
	ret = alloc_value();
	ret->type = T_STRING;
#ifdef sun
	ret->u.string = string_copy(_crypt(arg->u.string, arg->u.string));
#else
	ret->u.string = string_copy(crypt(arg->u.string, arg->u.string));
#endif
	break;
    case F_CREATE_WIZARD:
	if (DEBUG_EXECUTION)
	    debug_message("create_wizard()\n");
	arg = print_expr_list(p->a1);
	if (arg == 0 || arg->type != T_STRING)
	    error("Bad argument to create_wizard().\n");
	ret = call_indirect(F_CREATE_WIZARD, arg->u.string);
	break;
    case F_DESTRUCT:
	if (DEBUG_EXECUTION)
	    debug_message("destruct()\n");
	ret = print_expr_list(p->a1);
	if (ret == 0 || ret->type != T_STRING && ret->type != T_OBJECT) {
	    error("Bad type argument to destruct()\n");
	    exit(1);
	}
	(void)call_indirect(F_DESTRUCT, ret);
	return 0;
    case F_RANDOM:
	if (DEBUG_EXECUTION)
	    debug_message("random()\n");
	arg = print_expr_list(p->a1);
	if (arg == 0 || arg->type != T_NUMBER)
	    error("Bad type arg to random()\n");
	if (arg->u.number <= 0) {
	    ret = &const0;
	    break;
	}
#ifdef DRAND48
	ret = make_number(drand48() * arg->u.number);
#else
#ifdef RANDOM
	ret = make_number(random() % arg->u.number);
#else
	ret = make_number((((int)arg + (int) ret)>>2 + time(0l)) %
	    arg->u.number);
#endif /* RANDOM */
#endif /* DRAND48 */
	break;
    case F_SAY:
	if (DEBUG_EXECUTION)
	    debug_message("say()\n");
	{
	    struct lnode *l = p->a1;
	    arg1 = get_one_expr(&l, "say()");
	    if (arg1 == 0) {
		error("Bad type argument to say()\n");
		exit(1);
	    }
	    arg2 = get_one_expr(&l, (char *)0);
	    if (arg2 && arg2->type != T_OBJECT)
		error("Bad type argument to say()\n");
	    (void)call_indirect(F_SAY, arg1, arg2 ? arg2->u.ob : 0);
	}
	return 0;
    case F_TELL_ROOM:
	if (DEBUG_EXECUTION)
	    debug_message("tell_room()\n");
	{
	    struct lnode *l = p->a1;
	    arg1 = get_one_expr(&l, "tell_room");
	    if (arg1 == 0 || arg1->type != T_OBJECT)
		error("Bad type argument to tell_room()\n");
	    arg2 = get_one_expr(&l, "tell_room");
	    if (arg2 == 0 || arg2->type != T_STRING)
		error("Bad type second argument to tell_room()\n");
	    (void)call_indirect(F_TELL_ROOM, arg1->u.ob, arg2);
	}
	return 0;
    case F_SHOUT:
	if (DEBUG_EXECUTION)
	    debug_message("shout()\n");
	ret = print_expr_list(p->a1);
	if (ret == 0 || ret->type != T_STRING) {
	    error("Bad type argument to shout()\n");
	    exit(1);
	}
	(void)call_indirect(F_SHOUT, ret->u.string);
	return 0;
    case F_WHILE:
	if (DEBUG_EXECUTION)
	    debug_message("while()\n");
	{
	    struct context old_context;

	    old_context = break_context;
	    /*
	     * We come to the next statement in three ways:
	     * 0: Set up of long jump.
	     * 1: break statement.
	     * 2: continue statement.
	     */
	    if (setjmp(break_context.a) == 1) {
		break_context.valid = 0;
		break;
	    }
	    break_context.valid = 1;
	    while(!stop_function) {
		arg = print_expr(p->a1);
		if (arg == 0 || (arg->type == T_NUMBER && arg->u.number == 0))
		    break;
		(void)print_expr(p->a2);
	    }
	    break_context = old_context;
	    return 0;
	}
    case F_SUBSCRIPT:
	if (DEBUG_EXECUTION)
	    debug_message("subscript()\n");
	arg1 = print_expr(p->a1);
	arg2 = print_expr(p->a2);
	if (arg1 == 0 || arg2 == 0 || arg1->type != T_STRING ||
	    arg2->type != T_NUMBER)
	    error("Bad type argument to subscripts.\n");
	if (arg2->u.number >= strlen(arg1->u.string))
	    break;
	ret = alloc_value();
	ret->type = T_NUMBER;
	ret->u.number = arg1->u.string[arg2->u.number];
	break;
    case F_STRLEN:
	if (DEBUG_EXECUTION)
	    debug_message("strlen()\n");
	arg = print_expr_list(p->a1);
	if (arg == 0 || arg->type != T_STRING)
	    break;
	ret = alloc_value();
	ret->type = T_NUMBER;
	ret->u.number = strlen(arg->u.string);
	break;
    case F_LOWER_CASE:
	if (DEBUG_EXECUTION)
	    debug_message("lower_case()\n");
	ret = print_expr_list(p->a1);
	if (ret == 0 || ret->type != T_STRING)
	    error("Bad type argument to lowercase\n");
	{
	    int i;
	    for (i = strlen(ret->u.string)-1; i>=0; i--)
		if (isalpha(ret->u.string[i]))
		    ret->u.string[i] |= 'a' - 'A';
	}
	break;
    case F_SET_HEART_BEAT:
	if (DEBUG_EXECUTION)
	    debug_message("set_heart_beat()\n");
	arg = print_expr_list(p->a1);
	if (arg == 0 || arg->type != T_NUMBER)
	    error("Bad type argument to set_heart_beat()\n");
	call_indirect(F_SET_HEART_BEAT, arg->u.number);
	return 0;
    case F_CAPITALIZE:
	if (DEBUG_EXECUTION)
	    debug_message("capitalize()\n");
	ret = print_expr_list(p->a1);
	if (ret == 0 || ret->type != T_STRING)
	    error("Bad type arg to capitalize\n");
	if (ret->u.string[0] == '\0')
	    break;
	if (islower(ret->u.string[0]))
	    ret->u.string[0] += 'A' - 'a';
	break;
    case F_COMMAND:
	if (DEBUG_EXECUTION)
	    debug_message("command()\n");
	ret = print_expr_list(p->a1);
	if (ret == 0 || ret->type != T_STRING) {
	    error("Bad type argument to command()\n");
	    exit(1);
	}
	(void)call_indirect(F_COMMAND, ret->u.string);
	return 0;
    case F_LS:
	if (DEBUG_EXECUTION)
	    debug_message("ls()\n");
	ret = print_expr_list(p->a1);
	if (ret == 0 || ret->type != T_STRING && ret->type != T_NUMBER) {
	    error("Bad type argument to command()\n");
	    exit(1);
	}
	(void)call_indirect(F_LS,
			    ret->type == T_STRING ? ret->u.string : 0);
	return 0;
    case F_RM:
	if (DEBUG_EXECUTION)
	    debug_message("rm()\n");
	ret = print_expr_list(p->a1);
	if (ret == 0 || ret->type != T_STRING) {
	    error("Bad type argument to rm()\n");
	    exit(1);
	}
	(void)call_indirect(F_RM, ret->u.string);
	return 0;
    case F_CAT:
	if (DEBUG_EXECUTION)
	    debug_message("cat()\n");
	ret = print_expr_list(p->a1);
	if (ret == 0 || ret->type != T_STRING) {
	    error("Bad type argument to cat()\n");
	    exit(1);
	}
	(void)call_indirect(F_CAT, ret->u.string);
	return 0;
    case F_INPUT_TO:
	if (DEBUG_EXECUTION)
	    debug_message("input_to()\n");
	ret = print_expr_list(p->a1);
	if (ret == 0 || ret->type != T_STRING) {
	    error("Bad type argument to input_to()\n");
	    exit(1);
	}
	ret = call_indirect(F_INPUT_TO, ret->u.string);
	break;
    case F_SSCANF:
	if (DEBUG_EXECUTION)
	    debug_message("sscanf()\n");
	ret = inter_sscanf(p->a1);
	break;
    case F_ENABLE_COMMANDS:
	if (DEBUG_EXECUTION)
	    debug_message("enable_commands()\n");
	(void)call_indirect(F_ENABLE_COMMANDS);
	return 0;
    case F_PRESENT:
	if (DEBUG_EXECUTION)
	    debug_message("present()\n");
	{
	    struct lnode *l = p->a1;
	    arg1 = get_one_expr(&l, "present()");
	    if (arg1->type != T_STRING && arg1->type != T_OBJECT)
		error("Bad type argument to present()\n");
	    arg2 = get_one_expr(&l, NULL);
	    if (arg2 && arg2->type != T_OBJECT)
		error("Bad second argument to present()\n");
	    ret = call_indirect(F_PRESENT, arg1, arg2 ? arg2->u.ob : 0);
	}
	break;
    case F_SET_LIGHT:
	if (DEBUG_EXECUTION)
	    debug_message("set_light()\n");
	arg = print_expr_list(p->a1);
	if (arg == 0 || arg->type != T_NUMBER)
	    error("Bad type argument to set_light()\n");
	ret = call_indirect(F_SET_LIGHT, arg->u.number);
	break;
    case F_CONST0:
	if (DEBUG_EXECUTION)
	    debug_message("const 0\n");
	ret = &const0;
	break;
    case F_CONST1:
	if (DEBUG_EXECUTION)
	    debug_message("const 1\n");
	ret = &const1;
	break;
    case F_NUMBER:
	if (DEBUG_EXECUTION)
	    debug_message("number(%d)\n", (int)p->a1);
	ret = alloc_value();
	ret->type = T_NUMBER;
	ret->u.number = (int)p->a1;
	break;
    case F_ASSIGN:
	if (DEBUG_EXECUTION)
	    debug_message("assign()\n");
	ret = print_expr(p->a2);
	assign((struct lnode_number *)(p->a1), ret);
	return 0;
    case F_ADD_EQ:
	if (DEBUG_EXECUTION)
	    debug_message("+=\n");
        {
	    struct value *v;
	    ret = print_expr(p->a2);
	    if (ret == 0 || ret->type != T_NUMBER)
		error("Bad type to +=.\n");
	    v = print_expr(p->a1);
	    if (v == 0 || v->type != T_NUMBER)
		error("Bad type to +=");
	    assign((struct lnode_number *)(p->a1),
		   make_number(v->u.number + ret->u.number));
	}
	return 0;
    case F_SUB_EQ:
	if (DEBUG_EXECUTION)
	    debug_message("-=\n");
	{
	    struct value *v;
	    ret = print_expr(p->a2);
	    if (ret == 0 || ret->type != T_NUMBER)
		error("Bad type to -=.\n");
	    v = print_expr(p->a1);
	    if (v == 0 || v->type != T_NUMBER)
		error("Bad type to -=");
	    assign((struct lnode_number *)(p->a1),
		   make_number(v->u.number - ret->u.number));
	}
	return 0;
    case F_MULT_EQ:
	if (DEBUG_EXECUTION)
	    debug_message("*=\n");
	{
	    struct value *v;
	    ret = print_expr(p->a2);
	    if (ret == 0 || ret->type != T_NUMBER)
		error("Bad type to *=.\n");
	    v = print_expr(p->a1);
	    if (v == 0 || v->type != T_NUMBER)
		error("Bad type to *=");
	    assign((struct lnode_number *)(p->a1),
		   make_number(v->u.number * ret->u.number));
	}
	return 0;
    case F_LOCAL_NAME:
	if (DEBUG_EXECUTION)
	    debug_message("LOCAL(%d)\n", p->a1);
	ret = alloc_value();
	ret->type = current_local_names[(int)p->a1]->type;
	ret->u = current_local_names[(int)p->a1]->u;
	if (ret->type == T_STRING)
	    ret->u.string =
		string_copy(current_local_names[(int)p->a1]->u.string);
	else if (ret->type == T_OBJECT) {
	    if (ret->u.ob->destructed)
		ret = &const0;
	    add_ref(current_local_names[(int)p->a1]->u.ob, "cp loc");
	}
	break;
    case F_DIV_EQ:
	if (DEBUG_EXECUTION)
	    debug_message("/=\n");
	{
	    struct value *v;
	    ret = print_expr(p->a2);
	    if (ret == 0 || ret->type != T_NUMBER)
		error("Bad type to /=.\n");
	    v = print_expr(p->a1);
	    if (v == 0 || v->type != T_NUMBER)
		error("Bad type to /=");
	    if (ret->u.number == 0)
		error("Division by zero.");
	    assign((struct lnode_number *)(p->a1),
		   make_number(v->u.number / ret->u.number));
	}
	return 0;
    case F_STRING:
	{
	    struct lnode_name *n = (struct lnode_name *)p;
	    if (DEBUG_EXECUTION)
		debug_message("\"%s\"", n->name);
	    ret = alloc_value();
	    ret->u.string = string_copy(n->name);
	    ret->type = T_STRING;
	    break;
	}
    }
    if (d_flag) {
	debug_message("RESULT: '");
	debug_message_value(ret);
	debug_message("'\n");
    }
    return ret;
}

static struct value *print_expr_list(p)
    struct lnode *p;
{
    if (p == 0)
	return 0;
    if (p->type != F_CONS)
	fatal("Bad type to print_expr_list()\n");
    return print_expr(p->a1);
}

/*
 * Apply a fun 'fun' to the program in object 'ob', with the
 * optional argument 'arg'.
 */
struct value *apply(fun, ob, arg)
    char *fun;
    struct object *ob;
    struct value *arg;
{
    struct lnode_def *pr = ob->prog;
    struct value *old_argument = current_argument;
    struct object *save_current_object = current_object;
    extern int num_error;

    /*
     * We don't want to keep looping through errors here, yet we should try
     * even if there has been one error for the benefit of the apply's called
     * by the error() routine in simulate.c.
     */
    if (num_error > 1)
	return 0;
    if (ob->destructed)
	error("Executing '%s' in destructed object.\n", fun);
    if (ob->swapped)
	load_ob_from_swap(ob);
    pr = ob->prog;
    if (pr == 0)
	return 0;
    for(; pr; pr = pr->next) {
	if (pr->type == F_IDENTIFIER && strcmp(pr->name, fun) == 0) {
	    struct value *ret;
	    current_argument = arg;
	    current_object = ob;
	    ret = print_function(pr);
	    current_object = save_current_object;
	    current_argument = old_argument;
	    return ret;
	}
    }
    current_argument = old_argument;
    if (d_flag)
	debug_message("--Could not find the function %s\n", fun);
    return 0;
}

/*
 * Call a specific function in an object.
 * Make sure that current_object is set up, and the the program
 * is not swapped.
 */
void call_function(pr)
    struct lnode_def *pr;
{
    (void)print_function(pr);
}

#ifdef TRACE
/*
 * Write out a trace. If there are an heart_beat(), then return the
 * object that had that heart beat.
 */
char *dump_trace() {
    int i;
    char *ret = 0;

    if (trace_depth == 0) {
	(void)printf("No trace.\n");
	debug_message("No trace.\n");
	return 0;
    }
    for (i=0; i<trace_depth; i++) {
	(void)printf("%-3d: '%-10s' in '%-10s' by %-10s line %d\n", i,
		     trace_back[i].function_name,
		     trace_back[i].object_name,
		     trace_back[i].command_name,
		     trace_back[i].line);
	debug_message("%-3d: '%20s' in '%20s' line %d\n", i,
		      trace_back[i].function_name, trace_back[i].object_name,
		      trace_back[i].line);
	if (strcmp(trace_back[i].function_name, "heart_beat") == 0)
	    ret = trace_back[i].object_name;
    }
    return ret;
}
#endif /* TRACE */

static struct value *get_one_expr(pp, from)
    struct lnode **pp;
    char *from;
{
    struct value *v;

    if (*pp == 0) {
	if (from == 0)
	    return 0;
	error("Wrong number of arguments to %s", from);
    }
    v = print_expr((*pp)->a1);
    *pp = (*pp)->a2;
    return v;
}

static char *find_percent(str)
    char *str;
{
    while(1) {
	str = strchr(str, '%');
	if (str == 0)
	    return 0;
	if (str[1] != '%')
	    return str;
	str++;
    }
}

static struct value *inter_sscanf(l)
    struct lnode *l;
{
    struct value *p;
    char *fmt;		/* Format description */
    char *in_string;	/* The string o be parsed. */
    int number_of_matches;
    char *cp;

    /*
     * First get the string to be parsed.
     */
    p = get_one_expr(&l, "sscanf");
    if (p == 0 || p->type != T_STRING)
	error("Bad first argument to sscanf.");
    in_string = p->u.string;
    if (in_string == 0)
	return 0;
    /*
     * Now get the format description.
     */
    p = get_one_expr(&l, "sscanf");
    if (p == 0 || p->type != T_STRING)
	error("Bad second argument to sscanf.");
    fmt = p->u.string;
    /*
     * First, skip and match leading text.
     */
    for (cp=find_percent(fmt); fmt != cp; fmt++, in_string++) {
	if (in_string[0] == '\0' || fmt[0] != in_string[0])
	    return &const0;
    }
    /*
     * For every % or substring in the format.
     */
    for (number_of_matches=0; l && l->a1; number_of_matches++, l = l->a2) {
	int i, type;

	if (fmt[0] == '\0') {
	    /*
	     * We have reached end of the format string.
	     * If there are any chars left in the in_string,
	     * then we put them in the last variable (if any).
	     */
	    if (in_string[0]) {
		assign((struct lnode_number *)(l->a1),
		       make_string(in_string));
		number_of_matches++;
	    }
	    break;
	}
	if (fmt[0] != '%')
	    fatal("Should be a %% now !\n");
	type = T_STRING;
	if (fmt[1] == 'd')
	    type = T_NUMBER;
	else if (fmt[1] != 's')
	    error("Bad type : '%%%c' in sscanf fmt string.", fmt[1]);
	fmt += 2;
	/*
	 * Parsing a number is the easy case. Just use strtol() to
	 * find the end of the number.
	 */
	if (type == T_NUMBER) {
	    extern long strtol();
	    char *tmp = in_string;
	    int tmp_num;

	    tmp_num = (int) strtol(in_string, &in_string, 0);
	    if(tmp == in_string) {
		/* No match */
		break;
	    }
	    assign((struct lnode_number *)(l->a1), make_number(tmp_num));
	    while(fmt[0] && fmt[0] == in_string[0])
		fmt++, in_string++;
	    if (fmt[0] != '%') {
		number_of_matches++;
		break;
	    }
	    continue;
	}
	/*
	 * Now we have the string case.
	 */
	cp = find_percent(fmt);
	if (cp == fmt)
	    error("Illegal to have 2 adjacent %'s in fmt string in sscanf.");
	if (cp == 0)
	    cp = fmt + strlen(fmt);
	/*
	 * First case: There was no extra characters to match.
	 * Then this is the last match.
	 */
	if (cp == fmt) {
	    assign((struct lnode_number *)(l->a1), make_string(in_string));
	    number_of_matches++;
	    break;
	}
	for (i=0; in_string[i]; i++) {
	    if (strncmp(in_string+i, fmt, cp - fmt) == 0) {
		char *match;
		/*
		 * Found a match !
		 */
		match = xalloc(i+1);
		(void)strncpy(match, in_string, i);
		in_string += i + cp - fmt;
		match[i] = '\0';
		assign((struct lnode_number *)(l->a1), make_string(match));
		free(match);
		fmt = cp;	/* Advance fmt to next % */
		break;
	    }
	}
	if (fmt == cp)	/* If match, then do continue. */
	    continue;
	/*
	 * No match was found. Then we stop here, and return
	 * the result so far !
	 */
	break;
    }
    return make_number(number_of_matches);
}
