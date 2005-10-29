#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include "object.h"
#include "lnode.h"
#include "config.h"

#define SWAP_SIZE	50000	/* Big enough for most objects. */

extern char *realloc(), *xalloc();
static void move_prog_to_swap_area();
static struct lnode *load_prog_swap_area();

static int swap_number = 1;

static char *swap_area;
static int swap_max_size, swap_size;

int num_swapped;
int total_bytes_swapped;

swap(ob)
    struct object *ob;
{
    char *file;
    FILE *f;

    if (NUM_RESET_TO_SWAP == 0)
	return 0;
    if (ob->heart_beat && ob->enable_heart_beat)
	return 0;
    if (ob->prog->num_ref > 1)
	return 0;
    file = xalloc(strlen(SWAP_DIR) + strlen("/LPMUDswapXXXXXXXX") + 1);
    sprintf(file, "%s/LPMUDswap%d", SWAP_DIR, swap_number++);
    if (ob->swap_num > 0) {
	struct stat st;
	/* There is already a swap file for this object. */
	free_prog(ob->prog);
	ob->prog = 0;
	ob->swapped = 1;
	ob->heart_beat = 0;	/* This pointer has to be set up again */
	num_swapped++;
	if (stat(file, &st) == -1) {
	    perror(file);
	    fatal("Coulnd't stat a swap file.\n");
	}
	total_bytes_swapped += st.st_size;
	free(file);
	return 1;
    }
    f = fopen(file, "w");
    if (f == 0) {
	perror(file);
	debug_message("Couldn't open swap file %s for write\n", file);
	free(file);
	return 0;
    }
    swap_area = xalloc(SWAP_SIZE);
    swap_size = 0;
    swap_max_size = SWAP_SIZE;
    move_prog_to_swap_area(ob->prog);
    if (fwrite(swap_area, 1, swap_size, f) != swap_size) {
	debug_message("Error in writing to swap.\n");
	free(swap_area);
	free(file);
	return 0;
    }
    total_bytes_swapped += swap_size;
    fclose(f);
    free(swap_area);
    free(file);
    free_prog(ob->prog);
    ob->prog = 0;
    ob->swapped = 1;
    num_swapped++;
    ob->swap_num = swap_number - 1;
    ob->heart_beat = 0;		/* This pointer has to be set up again */
    return 1;
}

static void ass_size(s)
unsigned int s;
{
    while(s + swap_size > swap_max_size) {
	swap_max_size *= 2;
	swap_area = realloc(swap_area, swap_max_size);
	if (swap_area == 0)
	    fatal("swap realloc %d\n", swap_max_size);
    }
}

static void move_prog_to_swap_area(p)
    struct lnode *p;
{
    unsigned int s;
    int beg = swap_size;

    switch(p->line & L_MASK) {
    case L_SINGLE:
	s = sizeof(struct lnode_single);
	ass_size(s);
	memcpy(swap_area + swap_size, (char *)p, s);
	swap_size += s;
	break;
    case L_NUMBER:
	s = sizeof(struct lnode_number);
	ass_size(s);
	memcpy(swap_area + swap_size, (char *)p, s);
	swap_size += s;
	break;
    case L_NAME:
	s = sizeof(struct lnode_name);
	ass_size(s);
	memcpy(swap_area + swap_size, (char *)p, s);
	swap_size += s;
	((struct lnode_name *)(swap_area+beg))->name = (char *)swap_size;
	s = strlen(((struct lnode_name *)p)->name) + 1;
	ass_size(s);
	strcpy(swap_area + swap_size, ((struct lnode_name *)p)->name);
	swap_size += s;
	break;
    case L_1:
	s = sizeof(struct lnode_1);
	ass_size(s);
	memcpy(swap_area + swap_size, (char *)p, s);
	swap_size += s;
	if (((struct lnode_1 *)p)->expr) {
	    ((struct lnode_1 *)(swap_area+beg))->expr =
		(struct lnode *)swap_size;
	    move_prog_to_swap_area(((struct lnode_1 *)p)->expr);
	}
	break;
    case L_2:
	s = sizeof(struct lnode_2);
	ass_size(s);
	memcpy(swap_area + swap_size, (char *)p, s);
	swap_size += s;
	if (((struct lnode_2 *)p)->expr1) {
	    ((struct lnode_2 *)(swap_area+beg))->expr1 =
		(struct lnode *)swap_size;
	    move_prog_to_swap_area(((struct lnode_2 *)p)->expr1);
	}
	if (((struct lnode_2 *)p)->expr2) {
	    ((struct lnode_2 *)(swap_area+beg))->expr2 =
		(struct lnode *)swap_size;
	    move_prog_to_swap_area(((struct lnode_2 *)p)->expr2);
	}
	break;
    case L_3:
	s = sizeof(struct lnode_3);
	ass_size(s);
	memcpy(swap_area + swap_size, (char *)p, s);
	swap_size += s;
	if (((struct lnode_3 *)p)->expr1) {
	    ((struct lnode_3 *)(swap_area+beg))->expr1 =
		(struct lnode *)swap_size;
	    move_prog_to_swap_area(((struct lnode_3 *)p)->expr1);
	}
	if (((struct lnode_3 *)p)->expr2) {
	    ((struct lnode_3 *)(swap_area+beg))->expr2 =
		(struct lnode *)swap_size;
	    move_prog_to_swap_area(((struct lnode_3 *)p)->expr2);
	}
	if (((struct lnode_3 *)p)->expr3) {
	    ((struct lnode_3 *)(swap_area+beg))->expr3 =
		(struct lnode *)swap_size;
	    move_prog_to_swap_area(((struct lnode_3 *)p)->expr3);
	}
	break;
    case L_DEF:
    {
	struct lnode_def *dp = (struct lnode_def *)p;
	s = sizeof(struct lnode_def);
	ass_size(s);
	memcpy(swap_area + swap_size, (char *)p, s);
	swap_size += s;
	((struct lnode_def *)(swap_area+beg))->name = (char *)swap_size;
	s = strlen(dp->name) + 1;
	ass_size(s);
	strcpy(swap_area + swap_size, dp->name);
	swap_size += s;
	((struct lnode_def *)(swap_area+beg))->block =
	    (struct lnode *)swap_size;
	move_prog_to_swap_area(dp->block);
	if (dp->next) {
	    ((struct lnode_def *)(swap_area+beg))->next =
		(struct lnode_def *)swap_size;
	    move_prog_to_swap_area(dp->next);
	}
	break;
    }
    case L_FUNCALL:
	s = sizeof (struct lnode_funcall);
	ass_size(s);
	memcpy(swap_area + swap_size, (char *)p, s);
	swap_size += s;
	((struct lnode_funcall *)(swap_area+beg))->name = (char *)swap_size;
	s = strlen(((struct lnode_funcall *)p)->name) + 1;
	ass_size(s);
	strcpy(swap_area + swap_size, ((struct lnode_funcall *)p)->name);
	swap_size += s;
	if (((struct lnode_funcall *)p)->expr) {
	    ((struct lnode_funcall *)(swap_area+beg))->expr =
		(struct lnode *)swap_size;
	    move_prog_to_swap_area(((struct lnode_funcall *)p)->expr);
	} 
	break;
    case L_BLOCK:
    {
	struct lnode_block *lb = (struct lnode_block *)p;
	char *block = lb->block;
	int i, *block_start;
	
	s = sizeof (struct lnode_block);
	ass_size(s);
	memcpy(swap_area + swap_size, (char *)p, s);
	swap_size += s;
	if (lb->num_nodes) {
	    block_start = (int *)(swap_area + swap_size);
	    ((struct lnode_block *)(swap_area+beg))->block =
		(char *)swap_size;
	    ass_size(lb->num_nodes * sizeof (int));
	    swap_size += lb->num_nodes * sizeof (int);
	}
	for (i=0; i < lb->num_nodes; i++) {
	    block_start[i] = swap_size;
	    move_prog_to_swap_area((struct lnode *)block);
	    block += lnode_size[((struct lnode *)block)->line >> L_SHIFT];
	}
	break;
    }
    case L_VAR_DEF:
    case L_VARIABLE:
    default:
	fatal("Bad type in free_sub_part(): 0x%x\n", p->line & L_MASK);
    }
}

load_ob_from_swap(ob)
    struct object *ob;
{
    FILE *f;
    char file[250];
    struct stat st;
    char *buffer;
    struct lnode_def *pr;

    sprintf(file, "%s/LPMUDswap%d", SWAP_DIR, ob->swap_num);
    f = fopen(file, "r");
    if (f == 0) {
	perror(file);
	fatal("Can't open swap file %s\n", file);
    }
    if (fstat(fileno(f), &st) == -1) {
	perror(file);
	fatal("Can't fstat swap file %s\n", file);
    }
    buffer = xalloc(st.st_size);
    if (fread(buffer, 1, st.st_size, f) != st.st_size)
	fatal("Couldn't read the swap file.\n");
    ob->prog = (struct lnode_def *)load_prog_swap_area(buffer, buffer);
    add_prog_ref(ob->prog);
    ob->swapped = 0;
    free(buffer);
    num_swapped--;
    total_bytes_swapped -= st.st_size;
    /*
     * Now we have to restore the heart_beat pointer.
     */
    for (pr = ob->prog; pr; pr = pr->next) {
	if (strcmp(pr->name, "heart_beat") == 0) {
	    ob->heart_beat = (struct lnode *)pr;
	    break;
	}
    }
}

static struct lnode *load_prog_swap_area(base, s)
    char *base, *s;
{
    struct lnode *p = (struct lnode *)s;
    struct lnode *new, *e1, *e2, *e3;

    switch(p->line & L_MASK) {
    case L_SINGLE:
	new = (struct lnode *)alloc_lnode_single(p->type);
	new->line = p->line;
	break;
    case L_NUMBER:
	new = (struct lnode *)alloc_lnode_number(p->type,
				   ((struct lnode_number *)p)->number);
	new->line = p->line;
	break;
    case L_NAME:
	new = (struct lnode *)alloc_lnode_name(p->type,
	        string_copy((int)(((struct lnode_name *)p)->name) + base));
	new->line = p->line;
	break;
    case L_1:
	if (((struct lnode_1 *)p)->expr)
	    e1 = (struct lnode *)load_prog_swap_area(base, base +
			   (int)((struct lnode_1 *)p)->expr);
	else
	    e1 = 0;
	new = (struct lnode *)alloc_lnode_1(p->type, e1);
	new->line = p->line;
	break;
    case L_2:
	if (((struct lnode_2 *)p)->expr1)
	    e1 = load_prog_swap_area(base, base +
				 (int)((struct lnode_2 *)p)->expr1);
	else
	    e1 = 0;
	if (((struct lnode_2 *)p)->expr2)
	    e2 = load_prog_swap_area(base, base +
				 (int)((struct lnode_2 *)p)->expr2);
	else
	    e2 = 0;
	new = (struct lnode *)alloc_lnode_2(p->type, e1, e2);
	new->line = p->line;
	break;
    case L_3:
	if (((struct lnode_3 *)p)->expr1)
	    e1 = load_prog_swap_area(base, base +
				 (int)((struct lnode_3 *)p)->expr1);
	else
	    e1 = 0;
	if (((struct lnode_3 *)p)->expr2)
	    e2 = load_prog_swap_area(base, base +
				 (int)((struct lnode_3 *)p)->expr2);
	else
	    e2 = 0;
	if (((struct lnode_3 *)p)->expr3)
	    e3 = load_prog_swap_area(base, base +
				 (int)((struct lnode_3 *)p)->expr3);
	else
	    e3 = 0;
	new = (struct lnode *)alloc_lnode_3(p->type, e1, e2, e3);
	new->line = p->line;
	break;
    case L_DEF:
    {
	struct lnode_def *dp = (struct lnode_def *)p;
	e1 = load_prog_swap_area(base, base + (int)dp->block);
	new = (struct lnode *)alloc_lnode_def(p->type,
			      string_copy(base + (int)dp->name),
			      e1, dp->num_var);
	new->line = p->line;
	if (dp->next)
	    ((struct lnode_def *)new)->next =
		(struct lnode_def *)load_prog_swap_area(base,
							base + (int)dp->next);
	break;
    }
    case L_FUNCALL:
	if (((struct lnode_funcall *)p)->expr)
	    e1 = load_prog_swap_area(base, base +
				 (int)((struct lnode_funcall *)p)->expr);
	else
	    e1 = 0;
	new = (struct lnode *)alloc_lnode_funcall(p->type,
	        string_copy(base + (int)((struct lnode_funcall *)p)->name),
				  e1);
	new->line = p->line;
	break;
    case L_BLOCK:
    {
	int size, num, *block_start;
	char *block, *block2;
	struct lnode *l;
	struct lnode_block *lb = (struct lnode_block *)p;
	extern int tot_lnode_block;

	block_start = (int *)(base + (int)lb->block);
	for (num = 0, size = 0; num < lb->num_nodes; num++) {
	    l = load_prog_swap_area(base, base + block_start[num]);
	    *((struct lnode **)(base + block_start[num])) = l;
	    size += lnode_size[l->line >> L_SHIFT];
	}
	new = (struct lnode *)xalloc(sizeof (struct lnode_block));
	tot_lnode_block++;
	block = xalloc(size);
	((struct lnode_block *)new)->block = block;
	((struct lnode_block *)new)->num_nodes = lb->num_nodes;
	new->type = p->type;
	new->line = p->line;
	for (num = 0, block2 = block; num < lb->num_nodes; num++) {
	    int size;
	    l = (struct lnode *)*(int *)(base + block_start[num]);
	    size = lnode_size[l->line >> L_SHIFT];
	    memcpy(block2, (char *)l, size);
	    block2 += size;
	    free_lnode(l, 1);	/* Should the second argument be 1 here? */
	}
	break;
    }
    case L_VAR_DEF:
    case L_VARIABLE:
    default:
	fatal("Bad type in free_sub_part(): 0x%x\n", p->line & L_MASK);
    }
    return new;
}

void remove_swap_file(ob)
    struct object *ob;
{
    char file[250];

    if (!ob->swap_num)
	fatal("Remove non-existing swap file.\n");
    sprintf(file, "%s/LPMUDswap%d", SWAP_DIR, ob->swap_num);
    if (unlink(file) == -1) {
	perror(file);
	debug_message("Could not unlink swap file %s\n", file);
    }
    ob->swap_num = 0;
}
