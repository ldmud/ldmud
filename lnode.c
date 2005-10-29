#include "y.tab.h"
#include "lnode.h"
#include "config.h"

#define NUM_CHUNK		50	/* Number of chunks to allocate */

extern char *xalloc(), *find_string_space();
extern int current_line;
int tot_alloc_lnode;
int tot_alloc_strings;
extern void add_message();
extern char string_space[], *end_of_string_space;

void free_lnode();
void free_sub_part();

static int tot_lnode_single;
struct lnode_single *alloc_lnode_single(type)
{
    struct lnode_single *p;
    p = (struct lnode_single *)xalloc(sizeof (struct lnode_single));
    p->type = type;
    p->line = current_line + L_SINGLE;
    tot_lnode_single++;
    return p;
}

static int tot_lnode_number;
struct lnode_number *alloc_lnode_number(type, number)
{
    struct lnode_number *p;
    p = (struct lnode_number *)xalloc(sizeof (struct lnode_number));
    p->type = type;
    p->number = number;
    p->line = current_line + L_NUMBER;
    tot_lnode_number++;
    return p;
}

/*
 * Try tro use string_space if possible.
 * Be carfule not to free() such an area.
 */
static int tot_lnode_name;
struct lnode_name *alloc_lnode_name(type, name)
    char *name;
{
    struct lnode_name *p;
    char *spn;
    p = (struct lnode_name *)xalloc(sizeof (struct lnode_name));
    p->type = type;
    spn = find_string_space(name);
    if (spn) {
	free(name);
	p->name = spn;
    } else {
	p->name = name;
	tot_alloc_strings += strlen(name) + 1;
    }
    p->line = current_line + L_NAME;
    tot_lnode_name++;
    return p;
}

static int tot_lnode_variable;
struct lnode_variable *alloc_lnode_variable(type, number)
{
    struct lnode_variable *p;
    p = (struct lnode_variable *)xalloc(sizeof (struct lnode_variable));
    p->type = type;
    p->number = number;
    p->line = current_line + L_VARIABLE;
    tot_lnode_variable++;
    return p;
}

static int tot_lnode_1;
struct lnode_1 *alloc_lnode_1(type, expr)
    struct lnode *expr;
{
    struct lnode_1 *p;
    p = (struct lnode_1 *)xalloc(sizeof (struct lnode_1));
    p->type = type;
    p->expr = expr;
    p->line = current_line + L_1;
    tot_lnode_1++;
    return p;
}

static struct lnode_2 *lnode_2_list;
static int tot_lnode_2;
struct lnode_2 *alloc_lnode_2(type, expr1, expr2)
    struct lnode *expr1, *expr2;
{
    struct lnode_2 *p;
    if (!lnode_2_list) {
	int i;
	lnode_2_list = (struct lnode_2 *)xalloc(NUM_CHUNK *
						sizeof (struct lnode_2));
	for (p = lnode_2_list, i=0; i<NUM_CHUNK - 1; p++, i++)
	    p->expr1 = (struct lnode *)(p+1);
	p->expr1 = 0;
	tot_lnode_2 += NUM_CHUNK;
    }
    p = lnode_2_list;
    lnode_2_list = (struct lnode_2 *)lnode_2_list->expr1;
    p->type = type;
    p->line = current_line + L_2;
    p->expr1 = expr1;
    p->expr2 = expr2;
    return p;
}

static int tot_lnode_3;
struct lnode_3 *alloc_lnode_3(type, expr1, expr2, expr3)
    struct lnode *expr1, *expr2, *expr3;
{
    struct lnode_3 *p;
    p = (struct lnode_3 *)xalloc(sizeof (struct lnode_3));
    p->type = type;
    p->line = current_line + L_3;
    p->expr1 = expr1;
    p->expr2 = expr2;
    p->expr3 = expr3;
    tot_lnode_3++;
    return p;
}

static int tot_lnode_def;
struct lnode_def *alloc_lnode_def(type, name, block, num_var)
    struct lnode *block;
    char *name;
{
    struct lnode_def *p;
    char *spn;
    p = (struct lnode_def *)xalloc(sizeof (struct lnode_def));
    p->type = type;
    p->line = current_line + L_DEF;
    spn = find_string_space(name);
    if (spn) {
	free(name);
	p->name = spn;
    } else {
	p->name = name;
	tot_alloc_strings += strlen(name) + 1;
    }
    p->num_var = num_var;
    p->block = block;
    p->next = 0;
    p->num_ref = 0;
    tot_lnode_def++;
    return p;
}

static int tot_var_def;
void alloc_lnode_var_def(type, name, num_var)
    char *name;
{
    struct lnode_var_def *p;
    p = (struct lnode_var_def *)xalloc(sizeof (struct lnode_var_def));
    p->type = type;
    p->line = current_line + L_VAR_DEF;
    p->name = name;
    p->num_var = num_var;
    p->next = prog_status;
    prog_status = p;
    tot_var_def++;
}

static int tot_funcall;
struct lnode_funcall *alloc_lnode_funcall(type, name, arg)
    struct lnode *arg;
    char *name;
{
    struct lnode_funcall *p;
    char *spn;

    p = (struct lnode_funcall *)xalloc(sizeof (struct lnode_funcall));
    p->type = type;
    p->line = current_line + L_FUNCALL;
    spn = find_string_space(name);
    if (spn) {
	free(name);
	p->name = spn;
    } else {
	p->name = name;
	tot_alloc_strings += strlen(name) + 1;
    }
    p->expr = arg;
    tot_funcall++;
    return p;
}

int tot_lnode_block;
struct lnode_block *alloc_lnode_block(p)
    struct lnode *p;
{
    int size, num;
    char *block, *block2;
    struct lnode *l, *l2, *next;
    struct lnode_block *ret;

    for (num = 0, size = 0, l=p; l; l = l->a2) {
	size += lnode_size[l->a1->line >> L_SHIFT];
	num++;
    }
    if (num == 1) {
	/* Only one statement. No need to make a block ! */
	struct lnode *stat;

	stat = p->a1;
	free_lnode(p, 1);
	return (struct lnode_block *)stat;
    }
    block = xalloc(sizeof (int) + size);
    block2 = block;
    for (l=p; l; l = next) {
	int s;

	s = lnode_size[l->a1->line >> L_SHIFT];
	memcpy(block2, (char *)l->a1, s);
	free_lnode(l->a1, 0);	/* Node has been copied. */
	l->a1 = 0;
	block2 += s;
	next = l->a2;
	free_lnode(l, 1);
    }
    ret = (struct lnode_block *)xalloc(sizeof (struct lnode_block));
    ret->type = F_BLOCK;
    ret->line = current_line + L_BLOCK;
    ret->block = block;
    ret->num_nodes = num;
    tot_lnode_block++;
    return ret;
}

void print_lnode_status(tot_before_this)
    int tot_before_this;
{
    int tot =
	tot_lnode_number * sizeof (struct lnode_number) +
	tot_lnode_name * sizeof (struct lnode_name) +
	tot_lnode_variable * sizeof (struct lnode_variable) +
	tot_lnode_1 * sizeof (struct lnode_1) +
	tot_lnode_2 * sizeof (struct lnode_2) +
	tot_lnode_3 * sizeof (struct lnode_3) +
	tot_var_def * sizeof (struct lnode_var_def) +
	tot_funcall * sizeof (struct lnode_funcall) +
	tot_lnode_def * sizeof (struct lnode_def) +
	tot_lnode_block * sizeof (struct lnode_block) +
	tot_before_this;
    add_message("lnode:\n");
    add_message("   single:     %5d %6d\n", tot_lnode_single,
		tot_lnode_single * sizeof (struct lnode_single));
    add_message("   numbers:    %5d %6d\n", tot_lnode_number,
		tot_lnode_number * sizeof (struct lnode_number));
    add_message("   names:      %5d %6d\n", tot_lnode_name,
		tot_lnode_name * sizeof (struct lnode_name));
    add_message("   variables:  %5d %6d\n", tot_lnode_variable,
		tot_lnode_variable * sizeof (struct lnode_variable));
    add_message("   1 size expr:%5d %6d\n", tot_lnode_1,
		tot_lnode_1 * sizeof (struct lnode_1));
    add_message("   2 size expr:%5d %6d\n", tot_lnode_2,
		tot_lnode_2 * sizeof (struct lnode_2));
    add_message("   3 size expr:%5d %6d\n", tot_lnode_3,
		tot_lnode_3 * sizeof (struct lnode_3));
    add_message("   functions:  %5d %6d\n", tot_lnode_def,
		tot_lnode_def * sizeof (struct lnode_def));
    add_message("   glob vars:  %5d %6d\n", tot_var_def,
		tot_var_def * sizeof (struct lnode_var_def));
    add_message("   fun calls:  %5d %6d\n", tot_funcall,
		tot_funcall * sizeof (struct lnode_funcall));
    add_message("   blocks:     %5d %6d\n", tot_lnode_block,
		tot_lnode_block * sizeof (struct lnode_block));
    add_message("   Total bytes       %6d\n", tot);
}

int lnode_size[(L_MAX >> L_SHIFT) + 1];

/*
 * Setup fast access to the size of an lnode.
 */

void compute_lnode_size(p)
    struct lnode *p;
{
    lnode_size[L_SINGLE >> L_SHIFT] = sizeof (struct lnode_single);
    lnode_size[L_NUMBER >> L_SHIFT] = sizeof (struct lnode_number);
    lnode_size[L_NAME >> L_SHIFT] = sizeof (struct lnode_name);
    lnode_size[L_VARIABLE >> L_SHIFT] = sizeof (struct lnode_variable);
    lnode_size[L_1 >> L_SHIFT] = sizeof (struct lnode_1);
    lnode_size[L_2 >> L_SHIFT] = sizeof (struct lnode_2);
    lnode_size[L_3 >> L_SHIFT] = sizeof (struct lnode_3);
    lnode_size[L_DEF >> L_SHIFT] = sizeof (struct lnode_def);
    lnode_size[L_VARIABLE >> L_SHIFT] = sizeof (struct lnode_var_def);
    lnode_size[L_FUNCALL >> L_SHIFT] = sizeof (struct lnode_funcall);
    lnode_size[L_BLOCK >> L_SHIFT] = sizeof (struct lnode_block);
}

void free_lnode(p, update_count)
    struct lnode *p;
{
    switch(p->line & L_MASK) {
    case L_SINGLE:
	if (update_count)
	    tot_lnode_single--;
	break;
    case L_NUMBER:
	if (update_count)
	    tot_lnode_number--;
	break;
    case L_NAME:
	if (update_count)
	    tot_lnode_name--;
	break;
    case L_VARIABLE:
	if (update_count)
	    tot_lnode_variable--;
	break;
    case L_1:
	if (update_count)
	    tot_lnode_1--;
	break;
    case L_2:
	((struct lnode_2 *)p)->expr1 = (struct lnode *)lnode_2_list;
	lnode_2_list = (struct lnode_2 *)p;
	return;
    case L_3:
	if (update_count)
	    tot_lnode_3--;
	break;
    case L_DEF:
	if (update_count)
	    tot_lnode_def--;
	break;
    case L_VAR_DEF:
	if (update_count)
	    tot_var_def--;
	break;
    case L_FUNCALL:
	if (update_count)
	    tot_funcall--;
	break;
    case L_BLOCK:
	if (update_count)
	    tot_lnode_block--;
	break;
    default:
	fatal("Bad type in lnode_size(): 0x%x\n", p->line & L_MASK);
    }
    free(p);
}

add_prog_ref(p)
    struct lnode_def *p;
{
    p->num_ref++;
}

free_prog(p)
    struct lnode_def *p;
{
    p->num_ref--;
    if (p->num_ref > 0)
	return 0;
    free_sub_part(p, 1);
    return 1;
}

/*
 * Free the space of one node, and all linkes from it.
 * When a L_BLOCK is freed, don't call free_lnode() with the nodes
 * in the top of the block, because these are allocated in one big
 * chunk.
 */
void free_sub_part(p, do_free)
    struct lnode *p;
    int do_free;
{
    extern int tot_alloc_strings;
    if (p == 0)
	return;
    switch(p->line & L_MASK) {
    case L_SINGLE:
	break;
    case L_NUMBER:
	break;
    case L_NAME:
	if (((struct lnode_name *)p)->name < string_space ||
	  ((struct lnode_name *)p)->name >= end_of_string_space) {
	    tot_alloc_strings -= strlen(((struct lnode_name *)p)->name) + 1;
	    free(((struct lnode_name *)p)->name);
	}
	((struct lnode_name *)p)->name = 0;
	break;
    case L_VARIABLE:
	break;
    case L_1:
	free_sub_part(((struct lnode_1 *)p)->expr, 1);
	((struct lnode_1 *)p)->expr = 0;
	break;
    case L_2:
	free_sub_part(((struct lnode_2 *)p)->expr1, 1);
	((struct lnode_2 *)p)->expr1 = 0;
	free_sub_part(((struct lnode_2 *)p)->expr2, 1);
	((struct lnode_2 *)p)->expr2 = 0;
	break;
    case L_3:
	free_sub_part(((struct lnode_3 *)p)->expr1, 1);
	((struct lnode_3 *)p)->expr1 = 0;
	free_sub_part(((struct lnode_3 *)p)->expr2, 1);
	((struct lnode_3 *)p)->expr2 = 0;
	free_sub_part(((struct lnode_3 *)p)->expr3, 1);
	((struct lnode_3 *)p)->expr3 = 0;
	break;
    case L_DEF:
    {
	struct lnode_def *dp = (struct lnode_def *)p;
	if (dp->name < string_space || dp->name >= end_of_string_space) {
	    tot_alloc_strings -= strlen(dp->name) + 1;
	    free(dp->name);
	}
	dp->name = 0;
	free_sub_part(dp->block, 1);
	dp->block = 0;
	if (dp->next) {
	    free_sub_part(dp->next, 1);
	    dp->next = 0;
	}
	break;
    }
    case L_VAR_DEF:
	free(((struct lnode_var_def *)p)->name);
	((struct lnode_var_def *)p)->name = 0;
	break;
    case L_FUNCALL:
	if (((struct lnode_funcall *)p)->name < string_space ||
	  ((struct lnode_funcall *)p)->name >= end_of_string_space) {
	    tot_alloc_strings -= strlen(((struct lnode_funcall *)p)->name) + 1;
	    free(((struct lnode_funcall *)p)->name);
	}
	((struct lnode_funcall *)p)->name = 0;
	free_sub_part(((struct lnode_funcall *)p)->expr, 1);
	((struct lnode_funcall *)p)->expr = 0;
	break;
    case L_BLOCK:
    {
	struct lnode_block *lb = (struct lnode_block *)p;
	char *block = lb->block;
	int i;
	
	for (i=0; i < lb->num_nodes; i++) {
	    free_sub_part((struct lnode *)block, 0);
	    block += lnode_size[((struct lnode *)block)->line >> L_SHIFT];
	}
	free(lb->block);
	break;
    }
    default:
	fatal("Bad type in free_sub_part(): 0x%x\n", p->line & L_MASK);
    }
    if (do_free)
	free_lnode(p, 1);
}
