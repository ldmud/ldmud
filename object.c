#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <string.h>
#include "interpret.h"
#include "object.h"
#include "lnode.h"
#include "sent.h"
#include "config.h"

extern int d_flag;

extern char *xalloc(), *string_copy();

extern void fatal(), exit(), free(), debug_message(), error(), add_message();
extern void free_sentence();

extern int atoi();

int tot_alloc_object;

/*
 * Replace newlines in a string with a carriage return, to make the string
 * writeable on one line.
 */

static void replace_newline(str)
    char *str;
{
    for (; *str; str++) {
	if (str[0] == '\n')
	    str[0] = '\r';
    }
}

/*
 * Replace carriage return in a string with newlines.
 */

static void restore_newline(str)
    char *str;
{
    for (; *str; str++) {
	if (str[0] == '\r')
	    str[0] = '\n';
    }
}


void save_object(ob, file)
    struct object *ob;
    char *file;
{
    char *name;
    int len;
    FILE *f;
    struct lnode_var_def *p;

    if (strchr(file, '.') || file[0] == '/' || file[0] == ' ')
	error("Illegal file name to save_object(%s).\n", file);
    if (strncmp(current_object->name, "obj/", 4) != 0 &&
	strncmp(current_object->name, "room/", 5) != 0)
	error("Illegal use of save_object()\n");
    len = strlen(file);
    name = xalloc(len + 3);
    (void)strcpy(name, file);
    (void)strcat(name, ".o");
    f = fopen(name, "w");
    if (f == 0) {
	error("Could not open %s for a save.\n", name);
#ifdef DO_ABORT
	abort();
#else
	return;
#endif
    }
    for (p = ob->status; p; p = p->next) {
	struct value *v = &ob->variables[p->num_var];
	char *new_string;
	if (v->type == T_NUMBER) {
	    (void)fprintf(f, "%s %d\n", p->name, v->u.number);
	} else if (v->type == T_STRING) {
	    new_string = string_copy(v->u.string);
	    replace_newline(new_string);
	    (void)fprintf(f, "%s \"%s\"\n", p->name, new_string);
	    free(new_string);
	}
    }
    free(name);
    (void)fclose(f);
}

int restore_object(ob, file)
    struct object *ob;
    char *file;
{
    char *name, var[100], *val, *buff, *space;
    int len;
    FILE *f;
    struct lnode_var_def *p;
    struct object *save = current_object;
    struct stat st;

    if (strchr(file, '.') || file[0] == '/' || file[0] == ' ')
	error("Illegal file name to restore_object(%s).\n", file);
    len = strlen(file);
    name = xalloc(len + 3);
    (void)strcpy(name, file);
    if (name[len-2] == '.' && name[len-1] == 'c')
	name[len-1] = 'o';
    else
	(void)strcat(name, ".o");
    f = fopen(name, "r");
    if (f == 0)
	return 0;
    if (fstat(fileno(f), &st) == -1) {
	perror(name);
	return 0;
    }
    if (st.st_size == 0)
	return 0;
    val = xalloc(st.st_size + 1);
    buff = xalloc(st.st_size + 1);
    current_object = ob;
    while(1) {
	struct value *v;

	if (fgets(buff, st.st_size, f) == 0)
	    break;
	/* Remember that we have a newline at end of buff ! */
	space = strchr(buff, ' ');
	if (space == 0)
	    fatal("Illegal format when restore %s.\n", name);
	(void)strncpy(var, buff, space - buff);
	var[space - buff] = '\0';
	(void)strcpy(val, space+1);
	p = find_status(var, 0);
	if (p == 0)
	    continue;
	v = &ob->variables[p->num_var];
	if (val[0] == '"') {
	    v->type = T_STRING;
	    v->u.string = xalloc(strlen(val)-2);
	    (void)strncpy(v->u.string, val+1, strlen(val)-3);
	    v->u.string[strlen(val)-3] = '\0';
	    restore_newline(v->u.string);
	    continue;
	}
	v->type = T_NUMBER;
	v->u.number = atoi(val);
    }
    current_object = save;
    if (d_flag)
	debug_message("Object %s restored from %s.\n", ob->name, name);
    free(name);
    free(buff);
    free(val);
    (void)fclose(f);
    return 1;
}

/*
 * Search for a living object which answers to the id NAME.
 * If there are many living objects, this algorithm can become quite
 * heavy. It should be hashed in the future !
 */

struct object *find_living_object(name)
    char *name;
{
    struct object *ob;
    struct value *ret;
    struct value thing;

    for (ob = obj_list; ob; ob = ob->next_all) {
	if (!ob->enable_commands || ob->super == 0)
	    continue;
	thing.type = T_STRING,
	thing.u.string = name;
	ret = apply("id", ob, &thing);
	if (ret == 0 || (ret->type == T_NUMBER && ret->u.number == 0))
	    continue;
	return ob;
    }
    return 0;
}

/*
 * Search for a player object which answers to the id NAME.
 * Like find_living_object(), this should really be hashed.
 */

struct object *find_player(name)
    char *name;
{
    struct object *ob;
    struct value *ret;
    struct value thing;

    for (ob = obj_list; ob; ob = ob->next_all) {
	if (!ob->enable_commands || ob->super == 0)
	    continue;
	thing.type = T_STRING,
	thing.u.string = name;
	/* Is this a player? */
	if (apply("is_player", ob, &const0))
	    /* Yep... but is it the right one? */
	    ret = apply("id", ob, &thing);
	else
	    /* Nope... try again. */
	    continue;
	if (ret == 0 || (ret->type == T_NUMBER && ret->u.number == 0))
	    continue;
	return ob;
    }
    return 0;
}

void tell_npc(ob, str)
    struct object *ob;
    char *str;
{
    struct value thing;

    thing.type = T_STRING;
    thing.u.string = str;
    (void)apply("catch_tell", ob, &thing);
}

/*
 * Send a message to an object.
 * If it is an interactive object, it will go to his
 * screen. Otherwise, it will go to a local function
 * catch_tell() in that object. This enables communications
 * between players and NPC's, and between other NPC's.
 */
void tell_object(ob, str)
    struct object *ob;
    char *str;
{
    struct object *save_command_giver;

    if (ob->interactive) {
	save_command_giver = command_giver;
	command_giver = ob;
	add_message("%s", str);
	command_giver = save_command_giver;
	return;
    }
    tell_npc(ob, str);
}

free_object(ob, from)
    struct object *ob;
    char *from;
{
    struct sentence *s;

    ob->ref--;
    if (d_flag)
	debug_message("Subtr ref to ob %s: %d (%s)\n", ob->name,
		      ob->ref, from);
    if (ob->ref > 0)
	return;
    if (!ob->destructed) {
	/*
	 * This should never happen, but it does. I had to do this
	 * kludge, or the game would crash.
	 * When this bug is found, fatal() should be called if this would
	 * happen again.
	 */
	debug_message("Remove a not destructed object.\n");
	fprintf(stderr, "Remove a not destructed object.\n");
	ob->ref++;
	return;
    }
    ob->reset = 0;
    if (ob->interactive)
	fatal("Tried to free an interactive object.\n");
    /*
     * If the program is freed, then we can also free the variable
     * deklarations.
     */
    if (ob->prog) {
	if (free_prog((struct lnode_def *)(ob->prog))) {
	    free_sub_part(ob->status, 1);
	    ob->status = 0;
	}
    }
    ob->prog = 0;
    if (ob->swap_num)
	remove_swap_file(ob);
    for (s = ob->sent; s;) {
	struct sentence *next;
	next = s->next;
	free_sentence(s);
	s = next;
    }
    if (ob->name) {
	if (d_flag)
	    debug_message("Free object %s\n", ob->name);
	free(ob->name);
	ob->name = 0;
    }
    if (ob->variables) {
	free(ob->variables);
	ob->variables = 0;
    }
    tot_alloc_object--;
    free(ob);
}

void add_ref(ob, from)
    struct object *ob;
    char *from;
{
    ob->ref++;
    if (d_flag)
	debug_message("Add reference to object %s: %d (%s)\n", ob->name,
		      ob->ref, from);
}

struct object *get_empty_object()
{
    struct object *ob;

    tot_alloc_object++;
    ob = (struct object *)xalloc(sizeof (struct object));
    ob->ref = 1;
    ob->swapped = 0;
    ob->not_touched = 0;
    ob->swap_num = 0;
    ob->ed_buffer = 0;
    ob->next_inv = 0;
    ob->contains = 0;
    ob->reset = 0;
    ob->interactive = 0;
    ob->enable_commands = 0;
    ob->total_light = 0;
    ob->sent = 0;
    ob->cloned = 0;
    ob->variables = 0;
    ob->super = 0;
    ob->enable_heart_beat = 0;
    ob->wl = 0;
    ob->destructed = 0;
    return ob;
}

dump_all_objects()
{
    struct object *ob;
    FILE *d;

    d = fopen("OBJ_DUMP", "w");
    if (!d) {
        add_message("Couldn't open dump file.\n");
	return;
    }
    add_message("Dumping data to 'OBJ_DUMP'... ");
    for (ob = obj_list; ob; ob = ob->next_all) {
        if (!ob->name)
	    fprintf(d, "<NULL> (0x%x)  ", ob);
	else
	    fprintf(d, "%s (0x%x)  ", ob->name, ob);
	if (ob->super) {
	    if (ob->super->name)
	        fprintf(d, "Super %s (0x%x)\n", ob->super->name, ob->super);
	    else
	        fprintf(d, "Super <NULL> (0x%x)\n", ob->super);
	} else
	    fprintf(d, "\n");
	fprintf(d, "\tRef %2d Rst %d Enbl_cmd %d Clnd %d Hrt_bt %d lang ref %d swp %d\n",
		ob->ref, ob->reset, ob->enable_commands, ob->cloned,
		ob->enable_heart_beat,
		(struct lnode_def *)(ob->prog)->num_ref, ob->swapped);
    }
    fclose(d);
    add_message("DONE\n");
}

/*
 * This one is used for extreme emergency.
 */
verify(str, where)
    char *str, *where;
{
    static struct object *harry;
    struct object *ob;
    int num_ref, i;

    if (!harry)
	harry = find_living_object(str);
    if (!harry)
	return;
    if (harry->ref == 0) {
	debug_message("%20s verify %s ref 0\n", where, str);
	harry = 0;
    }
    num_ref = 0;
    for (ob = obj_list; ob; ob = ob->next_all) {
	if (ob->num_variables == 0)
	    continue;
	for (i=0; i < ob->num_variables; i++) {
	    if (ob->variables[i].type == T_OBJECT &&
		ob->variables[i].u.ob == harry) {
		num_ref++;
		debug_message("var in '%s'\n", ob->name);
	    }
	}
    }
    i = count_value_ref(harry);
    debug_message("%20s: num ref %2d : %2d+%2d = %2d\n", where, harry->ref,
		  num_ref, i, i + num_ref);
}

