#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#include <errno.h>
#include <setjmp.h>
#include <stdio.h>
#include "config.h"
#include "stdio.h"
#include "lnode.h"
#include "y.tab.h"
#include "interpret.h"
#include "object.h"
#include "sent.h"
#include "wiz_list.h"
#include "security.h"

extern int errno;

extern FILE *vpopen();

extern void  *xalloc();

extern char *string_copy(), *dump_trace(), *check_file_name();

struct value *apply(), *make_number(), *clone_object();

struct sentence *alloc_sentence();

extern int stat(), mkdir(), fork(), wait(), execl(), restore_object(),
    special_parse(), yyparse(), set_call(), vfork();

extern void error(), print_all(), fatal(), add_message(), free(),
    pre_compile(), perror(), debug_message(), exit(),
    parse_command(), remove_interactive(), add_light(), save_object(),
    do_write(), move_object(), add_action(), debug_message(),
    add_verb(), remove_sent(), inventory(), look_at_room(),
    print_local_commands(), ipc_remove(), abort(), alarm(),
    free_sentence(), tell_object(), show_info_about(), ed_start(), say(),
    tell_npc(), set_snoop(), tell_room(), add_ref(), print_lnode_status(),
    remove_all_players(), start_new_file();

extern int d_flag;

struct object *obj_list, *obj_list_destruct;
extern struct object *find_living_object(), *find_player();

extern int current_line;

struct object *current_object;	/* The object interpreting a function. */
struct object *command_giver;	/* Where the current command came from. */

struct object *find_object();
static struct object *find_object2();

int num_parse_error;		/* Number of errors in the parser. */

void shutdowngame();

struct value *find_value(p)
    struct lnode_variable *p;
{
    return &current_object->variables[p->number];
}

struct lnode_var_def *find_status(str, must_find)
    char *str;
    int must_find;
{
    struct lnode_var_def *p;

    for (p=current_object->status; p; p = p->next) {
	if (strcmp(p->name, str) == 0)
	    return p;
    }
    if (!must_find)
	return 0;
    error("--Status %s not found in prog for %s\n", str,
	   current_object->name);
    return 0;
}

void remove_file(path)
    char *path;
{
    char buff[1000];
    FILE *f;

    path = check_file_name(path, 1);
    if (path == 0)
        return;
    if (unlink(path) == -1)
        add_message("No such file: %s\n", path);
    return;
}

struct object *load_object(name)
    char *name;
{
    FILE *f;
/* Why is command giver saved and restored ? */
    struct object *ob, *save_command_giver = command_giver;
    extern struct lnode *prog, *heart_beat;
    extern char *current_file;
    extern int variable_count;
    struct stat i_st, c_st;
    int i, name_length;
    char *real_name;
    
    /* Truncate possible .c in the object name. */
    name_length = strlen(name);
    name = string_copy(name);
    if (name[name_length-2] == '.' && name[name_length-1] == 'c') {
	name[name_length-2] = '\0';
	name_length -= 2;
    }
    /*
     * First check that the c-file exists.
     */
    real_name = (char *)xalloc(name_length + 3);
    (void)strcpy(real_name, name);
    (void)strcat(real_name, ".c");
    if (stat(real_name, &c_st) == -1) {
	free(name);
	free(real_name);
	error("Could not load descr for %s\n", real_name);
	return 0;
    }
    /*
     * Now check if the i-file is newer or nonexisting.
     */
    real_name[name_length+1] = 'i';
    if (stat(real_name, &i_st) == -1 || i_st.st_mtime < c_st.st_mtime) {
	real_name[name_length+1] = 'c';
	pre_compile(name);
	real_name[name_length+1] = 'i';
    }
    if (stat(real_name, &i_st) == -1 || i_st.st_mtime < c_st.st_mtime) {
        if (command_giver)
	    add_message("Failed to compile the new file.\n");
    }
    f = fopen(real_name, "r");
    if (f == 0) {
	perror(real_name);
	free(name);
	free(real_name);
	error("Could not read the file.\n");
    }
    start_new_file(f);
    current_file = string_copy(real_name);
    current_line = 0;
    prog = heart_beat = 0;
    prog_status = 0;
    variable_count = 0;
    num_parse_error = 0;
    (void)yyparse();
    (void)fclose(f);
    if (num_parse_error > 0 || prog == 0) {
	free(name);
	free(real_name);
	free(current_file);
	if (prog) {
	    /*
	     * Set the reference count to one.
	     * We don't want to confuse free_prog().
	     */
	    add_prog_ref((struct lnode_def *)prog);
	    free_prog(prog);
	}
	/* The following unlinks are security patches - Drax */
	if (num_parse_error == 0 && prog == 0) {
	    unlink(real_name);
	    error("No program in object !\n");
	}
	unlink(real_name);
	error("Error in loading %s\n", real_name);
    }
    ob = get_empty_object();
    ob->name = string_copy(name);
    ob->name_length = strlen(name);
    /* Is this object wizard defined ? */
    {
        char wiz_name[100];
	if (sscanf(real_name, "players/%s", wiz_name) == 1) {
	    char *np;
	    np = strchr(wiz_name, '/');
	    if (np)
	        *np = '\0';
	    ob->wl = add_name(wiz_name);
	} else {
	    ob->wl = 0;
	}
    }
    /* Allocate space for local variables. */
    if (variable_count > 0)
	ob->variables =
	    (struct value *)xalloc(variable_count * sizeof (struct value));
    ob->num_variables = variable_count;
    /* Initialize variables... */
    for (i=0; i<variable_count; i++) {
	ob->variables[i].u.number = 0;
	ob->variables[i].type = T_NUMBER;
    }
    ob->status = prog_status;
    if (prog) {
	ob->prog = (struct lnode_def *)prog;
	add_prog_ref((struct lnode_def *)prog);
    } else {
	ob->prog = 0;
    }
    ob->next_all = obj_list;
    obj_list = ob;
    ob->heart_beat = heart_beat;
    ob->reset = 1;
    (void)apply("reset", ob, 0);
    command_giver = save_command_giver;
    if (d_flag)
	debug_message("--%s loaded\n", ob->name);
    free(real_name);
    free(name);
    return ob;
}
struct object *previous_ob;

/*
 * Call function in another object.
 * Save the caller object in previous_ob, so some functions can
 * see who called them.
 */
struct value *call_other(obj, name, arg)
    struct value *obj;
    char *name;
    struct value *arg;
{
    struct object *ob, *save_command_giver = command_giver,
	*save_previous_ob = previous_ob;
    struct value *ret;

    if (obj->type == T_OBJECT)
	ob = obj->u.ob;
    else
	ob = find_object(obj->u.string);
    if (ob == 0)
	error("Could not find object %s\n", obj);
    /*
     * Test if this really is an existing object.
     */
    if (ob->reset == 0)
	return 0;
    ob->not_touched = 0;
    previous_ob = current_object;
    ret = apply(name, ob, arg);
    previous_ob = save_previous_ob;
    command_giver = save_command_giver;
    if (ret == 0)
	return &const0;
    return ret;
}

struct value *this_player() {
    struct value *p = alloc_value();

    if (command_giver == 0)
	return &const0;
    p->type = T_OBJECT;
    p->u.ob = command_giver;
    add_ref(command_giver, "this_player()");
    return p;
}

struct value *this_object() {
    struct value *p = alloc_value();

    p->type = T_OBJECT;
    p->u.ob = current_object;
    add_ref(current_object, "this_object()");
    return p;
}

struct value *call_local_function(name, arg)
    char *name;
    struct value *arg;
{
    struct value *ret;

    ret = apply(name, current_object, arg);
    if (ret == 0)
	error("Local function %s not found.\n", name);
    return ret;
}

char *make_new_name(str)
    char *str;
{
    static int i;
    char *p = (char *)xalloc(strlen(str) + 10);

    (void)sprintf(p, "%s%d/", str, i);
    i++;
    return p;
}

struct value *clone_object(str1)
    char *str1;
{
    struct object *ob, *new_ob;
    struct value *ret;
    int i;

    ob = find_object(str1);
    if (!ob) {
	if (strcmp(str1, "obj/sing") == 0)
	    fatal("No singularity.\n");
	ret = clone_object("obj/sing");
	if (!ret)
	    fatal("No singularity.\n");
	return ret;
    }
    if (ob->super || ob->cloned)
	error("Cloning a used object !\n");
    ob->reset = 0;
    if (ob == 0)
	error("Object %s not found\n", str1);
    new_ob = get_empty_object();
    new_ob->name = make_new_name(ob->name);
    new_ob->cloned = 1;
    new_ob->prog = ob->prog;
    if (new_ob->prog)
	add_prog_ref((struct lnode_def *)(new_ob->prog));
    if (current_object && current_object->wl && !ob->wl)
	new_ob->wl = current_object->wl;
    else
	new_ob->wl = ob->wl;		/* Possibly a null pointer */
    new_ob->status = ob->status;
    new_ob->heart_beat = ob->heart_beat;
    new_ob->next_all = obj_list;
    obj_list = new_ob;
    new_ob->num_variables = ob->num_variables;
    /* Allocate space for local variables. */
    if (ob->num_variables > 0)
	new_ob->variables =
	    (struct value *)xalloc(ob->num_variables * sizeof (struct value));
    /* Initialize variables... */
    for (i=0; i<ob->num_variables; i++) {
	new_ob->variables[i].u.number = 0;
	new_ob->variables[i].type = T_NUMBER;
    }
    new_ob->reset = 1;
    (void)apply("reset", new_ob, 0);
    if (d_flag)
	debug_message("--%s cloned to %s\n", ob->name, new_ob->name);
    ret = alloc_value();
    ret->type = T_OBJECT;
    ret->u.ob = new_ob;
    add_ref(new_ob, "clone_object");
    return ret;
}

struct value *environment(arg)
    struct value *arg;
{
    struct value *ret;
    struct object *ob = current_object;

    if (arg && arg->type == T_OBJECT)
	ob = arg->u.ob;
    else if (arg && arg->type == T_STRING)
	ob = find_object2(arg->u.string);
    else
	ob = current_object;
    if (ob == 0 || ob->super == 0)
	return &const0;
    if (ob->destructed)
	error("environment() off destructed object.\n");
    ret = alloc_value();
    ret->type = T_OBJECT;
    ret->u.ob = ob->super;
    add_ref(ret->u.ob, "environment");
    return ret;
}

/*
 * Execute a command for an object. Copy the command into a
 * new buffer, because 'parse_command()' can modify the command.
 */
void command_for_object(str)
    char *str;
{
    char buff[1000];
    struct object *ob;

    ob = current_object;
    strncpy(buff, str, sizeof buff);
    parse_command(buff, ob);
}

/*
 * To find if an object is present, we have to look in two inventory
 * lists. The first list is the inventory of the current object.
 * The second list is all things that have the same ->super as
 * current_object.
 * Also test the environment.
 * If the second argument 'ob' is non zero, only search in the
 * inventory of 'ob'. The argument 'ob' will be mandatory, later.
 */

static struct value *object_present2();

struct value *object_present(v, ob)
    struct value *v;
    struct object *ob;
{
    struct value *ret;
    int specific = 0;

    if (ob == 0)
	ob = current_object;
    else
	specific = 1;
    if (v->type == T_OBJECT) {
	if (specific) {
	    if (v->u.ob->super == ob)
		return v;
	    else
		return &const0;
	}
	if (v->u.ob->super == ob ||
	    (v->u.ob->super == ob->super && ob->super != 0))
	    return v;
	return &const0;
    }
    ret = object_present2(v, ob->contains);
    if (ret)
	return ret;
    if (specific)
	return &const0;
    if (ob->super) {
	ret = apply("id", ob->super, v);
	if (ret && !(ret->type == T_NUMBER && ret->u.number == 0)) {
	    ret = alloc_value();
	    ret->type = T_OBJECT;
	    ret->u.ob = ob->super;
	    add_ref(ret->u.ob, "present()");
	    return ret;
	}
	return object_present2(v, ob->super->contains);
    }
    return &const0;
}

static struct value *object_present2(v, ob)
    struct value *v;
    struct object *ob;
{
    struct value *ret;
    char *p, *tmp;
    int count = 0, length;
    struct value item;

    item.type = T_STRING;
    item.u.string = string_copy(v->u.string);
    length = strlen(item.u.string);
    p = item.u.string + length - 1;
    if (*p >= '0' && *p <= '9') {
	while(p > item.u.string && *p >= '0' && *p <= '9')
	    p--;
	if (p > item.u.string && *p == ' ') {
	    count = atoi(p+1) - 1;
	    *p = '\0';
	    length = p - item.u.string;
	}
    }
    /* We need to put the cropped string into v->u.string to pass it to apply */
    tmp = v->u.string;
    v->u.string = item.u.string;
    for (; ob; ob = ob->next_inv) {
	ret = apply("id", ob, v);
	if (ret == 0 || (ret->type == T_NUMBER && ret->u.number == 0))
	    continue;
	if (count-- > 0)
	    continue;
	ret = alloc_value();
	ret->type = T_OBJECT;
	ret->u.ob = ob;
	add_ref(ret->u.ob, "object_present2");
	v->u.string = tmp;
	free(item.u.string);
	return ret;
    }
    v->u.string = tmp;
    free(item.u.string);
    return 0;
}

/*
 * Remove an object. It is first moved into the destruct list, and
 * not really destructed until later. (destruct2()).
 */
void destruct_object(v)
    struct value *v;
{
    struct object *ob, *inv, *super;
    struct object **pp;
    int removed;
    struct value *weight, neg_weight;
    extern struct object *current_reset;

    if (v->type == T_OBJECT)
	ob = v->u.ob;
    else {
	ob = find_object2(v->u.string);
	if (ob == 0)
	    error("destruct_object: Could not find %s\n", v->u.string);
    }
    if (ob->destructed)
	error("Destruct destructed object.\n");
    if (d_flag)
	debug_message("Destruct object %s (ref %d)\n", ob->name, ob->ref);
    /*
     * Now remove us out of the list of all objects.
     */
    removed = 0;
    for (pp = &obj_list; *pp; pp = &(*pp)->next_all) {
	if (*pp != ob)
	    continue;
	*pp = (*pp)->next_all;
	removed = 1;
	break;
    }
    if (!removed && command_giver)
        add_message("Failed to delete object.\n");
    if (ob == current_reset)
	current_reset = current_reset->next_all;
    super = ob->super;
    if (super == 0) {
	super = find_object("room/void");	/* Take any existing void. */
    } else {
	/* Call exit in current room, if player or npc */
	if(ob->enable_commands) {
	    struct value room;
	    room.type = T_OBJECT;
	    room.u.ob = ob;
	    apply("exit",super,&room);
	}
	weight = apply("query_weight", ob, 0);
	if (weight && weight->type == T_NUMBER) {
	    neg_weight.type = T_NUMBER;
	    neg_weight.u.number = - weight->u.number;
	    (void)apply("add_weight", super, &neg_weight);
	}
    }
    if (super == 0)
	fatal("Could not find the void.\n");

    while(ob->contains)
	move_or_destruct(ob->contains, super);
    /*
     * Remove us out of this current room (if any).
     * Remove all sentences defined by this object from all objects here.
     */
    if (ob->super) {
	if (ob->super->enable_commands)
	    remove_sent(ob, ob->super);
	add_light(ob->super, - ob->total_light);
	for (pp = &ob->super->contains; *pp;) {
	    if ((*pp)->enable_commands)
		remove_sent(ob, *pp);
	    if (*pp != ob)
		pp = &(*pp)->next_inv;
	    else
		*pp = (*pp)->next_inv;
	}
    }
    ob->super = 0;
    ob->next_inv = 0;
    ob->heart_beat = 0;
    ob->contains = 0;
    ob->enable_commands = 0;
    ob->next_all = obj_list_destruct;
    obj_list_destruct = ob;
    ob->destructed = 1;
}

/*
 * This one is called when no program is execuiting.
 * The super pointer is still maintained.
 */
destruct2(ob)
    struct object *ob;
{
    if (ob->interactive)
	remove_interactive(ob);
    /*
     * We must deallocate variables here, not in 'free_object()'.
     * That is because one of the local variables may point to this object,
     * and deallocation of this pointer will also decrease the reference
     * count of this object. Otherwise, an object with a variable pointing
     * to itself, would never be freed.
     * Just in case the program in this object would continue to
     * execute, change string and object variables into the number 0.
     */
    if (ob->variables) {
	/*
	 * Deallocate variables in this object.
	 * The space of the variables are not deallocated until
	 * the object structure is freed in free_object().
	 */
	int i;
	for (i=0; i<ob->num_variables; i++) {
	    if (ob->variables[i].type == T_STRING) {
		free(ob->variables[i].u.string);
	    } else if (ob->variables[i].type == T_OBJECT) {
		free_object(ob->variables[i].u.ob, "destruct_object var");
	    }
	    ob->variables[i].type = T_NUMBER;
	    ob->variables[i].u.number = 0;
	}
    }
    free_object(ob, "destruct_object");
}

struct value *create_wizard(owner)
    char *owner;
{
    struct stat st;
    char name[200], cmd[200];
    FILE *f;
    struct value *ret;
    struct object *owner_obj;

    owner_obj = find_player(owner);
    if (owner_obj == 0)
	fatal("create_wizard: Could not find the player %s.\n", owner);
    if (owner_obj->super == 0 || owner_obj->super->cloned) {
	struct value v;
	v.type = T_STRING;
	v.u.string = 
	  "There is a crash, the room collapses and the castle disappears.\n";
	say(&v, 0);
	return 0;
    }
    if (stat(PLAYER_DIR, &st) == -1)
	if (mkdir(PLAYER_DIR, 0777) == -1)
	    error("Could not mkdir %s\n", PLAYER_DIR);
    (void)sprintf(name, "%s/%s", PLAYER_DIR, owner);
    if (stat(name, &st) == 0)
	error("Player %s already has a castle!\n", owner);
    if (mkdir(name, 0777) == -1) {
	perror(name);
	error("Could not mkdir %s\n", name);
    }
    (void)sprintf(name, "%s/%s/%s", PLAYER_DIR, owner, "castle.c");
    f = fopen(name, "w");
    if (f == NULL)
	error("Could not create a castle file %s!\n", name);
    (void)fprintf(f, "#define NAME \"%s\"\n", owner);
    (void)fprintf(f, "#define DEST \"%s\"\n", owner_obj->super->name);
    (void)fclose(f);
    (void)sprintf(cmd, "cat %s >> %s", DEFAULT_CASTLE, name);
    (void)system(cmd);
    f = fopen(INIT_FILE, "a");
    if (f == NULL)
	error("Could not add the new castle to the %s\n", INIT_FILE);
    (void)fprintf(f, "%s\n", name);
    (void)fclose(f);
    ret = alloc_value();
    ret->type = T_STRING;
    ret->u.string = string_copy(name);
    return ret;
}

/*
 * A message from an object will reach
 * all objects in the inventory,
 * all objects in the same environment and
 * the surrounding object.
 * Non interactive objects gets no messages.
 *
 * There are two cases to take care of. If this routine is called from
 * a player (indirectly), then the message goes to all in his
 * environment. Otherwise, the message goes to all in the current_object's
 * environment (as the case when called from a heart_beat()).
 *
 * If there is a second argument 'avoid_ob', then do not send the message
 * to that object.
 */

void say(v, avoid_ob)
    struct value *v;
    struct object *avoid_ob;
{
    struct object *ob, *save_command_giver = command_giver;
    struct object *origin;
    char buff[256];

    if (current_object->enable_commands)
	command_giver = current_object;
    if (command_giver)
	origin = command_giver;
    else
	origin = current_object;
    switch(v->type) {
    case T_STRING:
	strncpy(buff, v->u.string, sizeof buff);
	break;
    case T_OBJECT:
	strncpy(buff, v->u.ob->name, sizeof buff);
	break;
    case T_NUMBER:
	sprintf(buff, "%d", v->u.number);
	break;
    default:
	error("Invalid argument %d to say()\n", v->type);
    }
    for (ob = origin->contains; ob; ob = ob->next_inv) {
	struct object *save_again;
	if (ob->interactive == 0) {
	    if (ob->enable_commands && ob != command_giver && ob != avoid_ob)
		tell_npc(ob, buff);
	    continue;
	}
	if (ob == save_command_giver || ob == avoid_ob)
	    continue;
	save_again = command_giver;
	command_giver = ob;
	add_message("%s", buff);
	command_giver = save_again;
    }
    if (origin->super) {
	if (origin->super->interactive && origin != command_giver &&
		origin->super != avoid_ob) {
	    command_giver = origin->super;
	    add_message("%s", buff);
	} else if (origin->super->interactive == 0 &&
		   origin->super != avoid_ob &&
		   origin->super->enable_commands && ob != command_giver) {
	    tell_npc(origin->super, buff);
	}
	for (ob = origin->super->contains; ob; ob = ob->next_inv) {
	    struct object *save_again;
	    if (ob == avoid_ob)
		continue;
	    if (ob->interactive == 0) {
		if (ob->enable_commands && ob != command_giver)
		    tell_npc(ob, buff);
		continue;
	    }
	    if (ob == command_giver)
		continue;
	    save_again = command_giver;
	    command_giver = ob;
	    add_message("%s", buff);
	    command_giver = save_again;
	}
    }
    command_giver = save_command_giver;
}

/*
 * Send a message to all objects inside an object.
 * Non interactive objects gets no messages.
 * Compare with say().
 */

void tell_room(room, v)
    struct object *room;
    struct value *v;
{
    struct object *ob, *save_command_giver = command_giver;
    char buff[256];

    switch(v->type) {
    case T_STRING:
	strncpy(buff, v->u.string, sizeof buff);
	break;
    case T_OBJECT:
	strncpy(buff, v->u.ob->name, sizeof buff);
	break;
    case T_NUMBER:
	sprintf(buff, "%d", v->u.number);
	break;
    default:
	error("Invalid argument %d to tell_room()\n", v->type);
    }
    for (ob = room->contains; ob; ob = ob->next_inv) {
	if (ob->interactive == 0) {
	    if (ob->enable_commands)
		tell_npc(ob, buff);
	    continue;
	}
	command_giver = ob;
	add_message("%s", buff);
    }
    command_giver = save_command_giver;
}

void shout_string(str)
    char *str;
{
    struct object *ob, *save_command_giver = command_giver;
    int emergency;
    struct value *muffled;

    emergency = (str[0] == '!');
    if (emergency) str++;
    for (ob = obj_list; ob; ob = ob->next_all) {
	if (ob->interactive == 0 || ob == save_command_giver)
	    continue;
	muffled = apply("query_muffled",ob,&const0);
	if (!emergency && muffled->type == T_NUMBER && muffled->u.number)
	    continue;
	command_giver = ob;
	add_message("%s", str);
    }
    command_giver = save_command_giver;
}

struct value *first_inventory(arg)
    struct value *arg;
{
    struct object *ob;
    struct value *ret;

    if (arg->type == T_STRING)
	ob = find_object(arg->u.string);
    else
	ob = arg->u.ob;
    if (ob == 0)
	error("No object to first_inventory()");
    if (ob->contains == 0)
	return 0;
    ret = alloc_value();
    ret->type = T_OBJECT;
    ret->u.ob = ob->contains;
    add_ref(ret->u.ob, "first_inventory");
    return ret;
}

struct value *next_inventory(arg)
    struct value *arg;
{
    struct object *ob;
    struct value *ret;

    if (arg->type == T_STRING)
	ob = find_object(arg->u.string);
    else
	ob = arg->u.ob;
    if (ob == 0)
	error("No object to next_inventory()");
    if (ob->next_inv == 0)
	return 0;
    ret = alloc_value();
    ret->type = T_OBJECT;
    ret->u.ob = ob->next_inv;
    add_ref(ret->u.ob, "next_inventory");
    return ret;
}

/*
 * This will enable an object to use commands normally only
 * accessible by interactive players.
 */

void enable_commands() {
    current_object->enable_commands++;
    command_giver = current_object;
}

/*
 * Set up a function in this object to be called with the next
 * user input string.
 */
struct value *input_to(fun)
    char *fun;
{
    struct sentence *s = alloc_sentence();

    if (set_call(command_giver, s)) {
	s->function = string_copy(fun);
	s->ob = current_object;
	return &const1;
    }
    free_sentence(s);
    return &const0;
}

void list_files(path)
    char *path;
{
    char buff[1000];
    FILE *f;

    if (!path)
        path = ".";
    path = check_file_name(path, 0);
    if (path == 0)
        return;
    sprintf(buff, "%s %s", LIST_FILES, path);
    f = vpopen(buff, "r");
    if (f == 0)
	return;
    while(1) {
	if (fgets(buff, sizeof buff, f) == 0)
	    break;
	add_message("%s", buff);
    }
    vpclose(f);
}

void print_file(path)
    char *path;
{
    char buff[1000];
    FILE *f;

    path = check_file_name(path, 0);
    if (path == 0)
        return;
    f = fopen(path, "r");
    if (f == 0)
	return;
    while(1) {
	if (fgets(buff, sizeof buff, f) == 0)
	    break;
	add_message("%s", buff);
    }
    fclose(f);
}

/* 5/10/90 - version to deal with invisibility */
void people() {
    struct object *ob;
    struct value *player,*invis,*level;
    int level_of_user;  
    char name[30];

    name[0]=0;
    level= apply("query_level",command_giver,0); 
    if (level->type = T_NUMBER)  
        level_of_user = level->u.number;  
    else  
        level_of_user = 0; 
    for (ob = obj_list; ob; ob=ob->next_all) {
	if (!ob->interactive)
	    continue;
	if (!ob->super)
	    continue;
	player = apply("query_real_name", ob, 0);
	if (player == 0 || player->type != T_STRING)
	    continue;
        player->u.string[0] -= 32;
	invis = apply("query_invis",ob,level);
        if (invis->type != T_NUMBER) continue;
	if (!(invis->u.number)) strcpy(name,player->u.string);
	if ((invis->u.number) < 0)
	    sprintf(name,"(%s)  %d",player->u.string,-invis->u.number);
	if ((invis->u.number) > 0) strcpy(name,"Someone");
	if ((invis->u.number) < 100)
	    show_info_about(name, ob->super->name, ob->interactive);
    }
}

void log_file(file, str)
    char *file, *str;
{
    FILE *f;
    char file_name[100];

    if (strchr(file, '/') || file[0] == '.' || strlen(file) > 30)
	error("Illegal file name to log_file(%s)\n", file);
    sprintf(file_name, "log/%s", file);
    f = fopen(file_name, "a");
    if (f == 0)
	return;
    fwrite(str, strlen(str), 1, f);
    fclose(f);
}

/*VARARGS1*/
struct value *call_indirect(fun, arg, arg2, arg3)
    int fun;
    int arg, arg2, arg3;
{
    struct value *ret;
    struct object *o1, *o2;

    switch(fun) {
    case F_WIZLIST:
        wizlist();
	break;
    case F_FIND_OBJECT:
	o1 = find_object2((char *)arg);
	if (!o1)
	    return &const0;
	ret = alloc_value();
	ret->type = T_OBJECT;
	ret->u.ob = o1;
	add_ref(o1, "F_FIND_OBJECT");
	return ret;
    case F_SNOOP:
	if (arg && ((struct object *)arg)->destructed)
	     error("snoop() on destructed object.\n");
	set_snoop(current_object, (struct object *)arg);
	return 0;
    case F_SET_HEART_BEAT:
	current_object->enable_heart_beat = arg;
	return 0;
    case F_LOG_FILE:
	log_file((char *)arg, (char *)arg2);
	return 0;
    case F_SHUTDOWN:
	shutdowngame();
	return 0;
    case F_LIVING:
	if (((struct object *)arg)->destructed)
	    return &const0;
	ret = alloc_value();
	ret->type = T_NUMBER;
	ret->u.number = ((struct object *)arg)->enable_commands;
	return ret;
    case F_ED:
	if (arg && !legal_path((char *)arg)) {
	    add_message("Illegal path\n");
	    return 0;
	}
	ed_start((char *)arg);
	return 0;
    case F_PEOPLE:
	people();
	return 0;
    case F_TELL_OBJECT:
	if (((struct object *)arg)->destructed)
	    error("Tell_object to destructed object./\n");
	tell_object((struct object *)arg, (char *)arg2);
	return 0;
    case F_FIND_LIVING:
	o1 = find_living_object((char *)arg);
	if (o1 == 0)
	    return &const0;
	ret = alloc_value();
	ret->type = T_OBJECT;
	ret->u.ob = o1;
	add_ref(ret->u.ob, "find_living_object");
	return ret;
    case F_FIND_PLAYER:
	o1 = find_player((char *)arg);
	if (o1 == 0)
	    return &const0;
	ret = alloc_value();
	ret->type = T_OBJECT;
	ret->u.ob = o1;
	add_ref(ret->u.ob, "find_player");
	return ret;
    case F_RM:
	remove_file((char *)arg);
	return 0;
    case F_LS:
	list_files((char *)arg);
	return 0;
    case F_CAT:
	print_file((char *)arg);
	return 0;
    case F_INPUT_TO:
	if (current_object->destructed)
	     error("input_to() on destructed object.\n");
	return input_to((char *)arg);
	break;
    case F_ENABLE_COMMANDS:
	enable_commands();
	return 0;
    case F_FIRST_INVENTORY:
	return first_inventory((struct value *)arg);
    case F_NEXT_INVENTORY:
	return next_inventory((struct value *)arg);
    case F_SHOUT:
	shout_string((char *)arg);
	return 0;
    case F_SAY:
	say((struct value *)arg, (struct object *)arg2);
	return 0;
    case F_TELL_ROOM:
	if (((struct object *)arg)->destructed)
	    error("tell_room to destructed object.\n");
	tell_room((struct object *)arg, (struct value *)arg2);
	return 0;
    case F_CREATE_WIZARD:
	return create_wizard((char *)arg);
    case F_DESTRUCT:
	destruct_object((struct value *)arg);
	return 0;
    case F_SET_LIGHT:
	add_light(current_object, arg);
	ret = alloc_value();
	ret->type = T_NUMBER;
	o1 = current_object;
	while(o1->super)
	    o1 = o1->super;
	ret->u.number = o1->total_light;
	return ret;
    case F_COMMAND:
	command_for_object((char *)arg);
	return 0;
    case F_PRESENT:
	if (arg2 && ((struct object *)arg2)->destructed)
	    error("present() on destructed object.\n");
	return object_present((struct value *)arg, (struct object *)arg2);
    case F_ENVIRONMENT:
	return environment((struct value *)arg);
    case F_SAVE_OBJECT:
	save_object(current_object, (char *)arg);
	return 0;
    case F_RESTORE_OBJECT:
	ret = alloc_value();
	ret->type = T_NUMBER;
	ret->u.number = restore_object(current_object, (char *)arg);
	return ret;
    case F_CLONE_OBJECT:
	return clone_object((char *)arg);
    case F_FUNCTION:
	return call_local_function((char *)arg, (struct value *)arg2);
    case F_CALL_OTHER:
	return call_other((struct value *)arg, (char *)arg2,
			  (struct value *)arg3);
    case F_WRITE:
	do_write((struct value *)arg);
	break;
    case F_MOVE_OBJECT:
	if (((struct value *)arg)->type == T_OBJECT)
	    o1 = ((struct value *)arg)->u.ob;
	else {
	    o1 = find_object(((struct value *)arg)->u.string);
	    if (o1 == 0)
		error("Object %s not found.\n", ((struct value *)arg)->u.string);
	}
	if (((struct value *)arg2)->type == T_OBJECT)
	    o2 = ((struct value *)arg2)->u.ob;
	else {
	    o2 = find_object(((struct value *)arg2)->u.string);
	    if (o2 == 0)
		error("Object %s not found.\n",((struct value *)arg2)->u.string);
	}
	if (((struct object *)o1)->destructed)
	     error("move_object() of destructed object.\n");
	if (((struct object *)o2)->destructed)
	     error("move_object() to destructed object.\n");
	move_object(o1, o2);
	break;
    case F_ADD_ACTION:
	add_action((char *)arg, (struct value *)arg2);
	break;
    case F_ADD_VERB:
	add_verb((char *)arg);
	break;
    case F_THIS_PLAYER:
	return this_player();
    case F_THIS_OBJECT:
	return this_object();
    default:
	fatal("Unimplemented hard linked function %d\n", fun);
	abort();
    }
    return 0;
}

void do_write(arg)
    struct value *arg;
{
    if (arg == 0)
	add_message("<NULL>");
    else if (arg->type == T_STRING)
	add_message("%s", arg->u.string);
    else if (arg->type == T_OBJECT)
	add_message("OBJ(%s)", arg->u.ob->name);
    else if (arg->type == T_NUMBER)
	add_message("%d", arg->u.number);
    else
	add_message("<UNKNOWN>");
}

/* Find an object. If not loaded, load it ! */

struct object *find_object(str)
    char *str;
{
    struct object *ob;

    ob = find_object2(str);
    if (ob)
	return ob;
    ob = load_object(str);
    return ob;
}

/* Look for an loaded object. */
static struct object *find_object2(str)
    char *str;
{
    register struct object *ob;
    register int length;
    char *name;

    /* Truncate possible .c in the object name. */
    length = strlen(str);
    name = string_copy(str);
    if (name[length-2] == '.' && name[length-1] == 'c') {
	name[length-2] = '\0';
	length -= 2;
    }
    for (ob=obj_list; ob; ob = ob->next_all) {
	if (length == ob->name_length && strcmp(ob->name, name) == 0) {
	    free(name);
	    return ob;
	}
    }
    free(name);
    return 0;
}

void apply_command(com)
    char *com;
{
    struct value *ret;

    if (command_giver == 0)
	error("command_giver == 0 !\n");
    ret = apply(com, command_giver->super, 0);
    if (ret != 0) {
	add_message("Result:");
	if (ret->type == T_STRING)
	    add_message("%s\n", ret->u.string);
	if (ret->type == T_NUMBER)
	    add_message("%d\n", ret->u.number);
    } else {
	add_message("Error apply_command: function %s not found.\n", com);
    }
}

void set_current_room(ob, dest)
    struct object *ob, *dest;
{
    struct object **pp, *p;
    struct object *save_command_giver = command_giver;

    if (dest == 0)
	dest = find_object("room/void.c");	/* Get any existing void. */
    if (dest == 0)
	fatal("Not even a void !\n");
    if (ob->enable_commands)
	command_giver = ob;
    if (ob->super) {
	if (!ob->super->destructed) {
	    struct value v;
	    v.type = T_OBJECT;
	    v.u.ob = ob;		/* No need to increment ref count */
	    (void)apply("exit", ob->super, &v);
	    add_light(ob->super, - ob->total_light);
	}
	remove_sent(ob->super, ob);
	/*
	 * Now we link the ob out of its list.
	 * Remove sentences tied to objects that stays in this room.
	 */
	for (pp = &ob->super->contains; *pp;) {
	    if (*pp == ob)
		*pp = (*pp)->next_inv;
	    else {
		remove_sent(*pp, ob);
		pp = &(*pp)->next_inv;
	    }
	}
    }
    ob->next_inv = dest->contains;
    dest->contains = ob;
    add_light(dest, ob->total_light);
    ob->super = dest;
    if (d_flag)
	debug_message("--Current room: %s\n", dest->name);
    (void)apply("init", dest, 0);
    for (p = dest->contains; p; p = p->next_inv) {
	if (p == ob)
	    continue;
	(void)apply("init", p, 0);
    }
    command_giver = save_command_giver;
}

/*
 * Transfer an object.
 */
void move_object(item, dest)
    struct object *item, *dest;
{
    struct object **pp, *ob;
    struct object *save_cmd = command_giver;

    /* Recursive moves are not allowed. */
    for (ob = dest; ob; ob = ob->super)
	if (ob == item)
	    return;
    item->not_touched = 0;
    dest->not_touched = 0;
    if (item->enable_commands) {
	set_current_room(item, dest);
	return;
    }
    if (item->super) {
	int okey = 0;
	if (item->super->enable_commands)
	    remove_sent(item, item->super);
	add_light(item->super, - item->total_light);
	for (pp = &item->super->contains; *pp;) {
	    if (*pp != item) {
		if ((*pp)->enable_commands)
		    remove_sent(item, *pp);
		pp = &(*pp)->next_inv;
		continue;
	    }
	    *pp = item->next_inv;
	    okey = 1;
	}
	if (!okey)
	    error("Failed to find object %s in super list of %s.\n",
		  item->name, item->super->name);
    }
    item->next_inv = dest->contains;
    dest->contains = item;
    item->super = dest;
    /*
     * Run init of the item once for every present player, and
     * for the environment (which can be a player).
     */
    for (ob = dest->contains; ob; ob=ob->next_inv) {
	if (ob->enable_commands) {
	    command_giver = ob;
	    (void)apply("init", item, 0);
	}
    }
    if (dest->enable_commands) {
	command_giver = dest;
	(void)apply("init", item, 0);
    }
    command_giver = save_cmd;
    add_light(dest, item->total_light);
    if (d_flag)
	debug_message("--move_object: %s to %s\n", item->name, dest->name);
}

/*
 * Every object as a count of number of light sources it contains.
 * Update this.
 */

void add_light(p, n)
    struct object *p;
    int n;
{
    if (n == 0)
	return;
    p->total_light += n;
    if (p->super)
	add_light(p->super, n);
}

struct sentence *sent_free = 0;
int tot_alloc_sentence;

struct sentence *alloc_sentence() {
    struct sentence *p;

    if (sent_free == 0) {
	p = (struct sentence *)xalloc(sizeof *p);
	tot_alloc_sentence++;
    } else {
	p = sent_free;
	sent_free = sent_free->next;
    }
    p->verb = 0;
    p->function = 0;
    p->next = 0;
    return p;
}

void free_sentence(p)
    struct sentence *p;
{
    if (p->function)
	free(p->function);
    p->function = 0;
    if (p->verb)
	free(p->verb);
    p->verb = 0;
    p->next = sent_free;
    sent_free = p;
}

void player_parser(buff)
    char *buff;
{
    struct sentence *s;
    char *p;
    int length;

    if (d_flag)
	debug_message("cmd [%s]: %s\n", command_giver->name, buff);
    /* strip trailing spaces. */
    for (p = buff + strlen(buff) - 1; p > buff; p--) {
	if (*p != ' ')
	    break;
	*p = '\0';
    }
    if (buff[0] == '\0')
	return;
    if (special_parse(buff))
	return;
    p = strchr(buff, ' ');
    if (p == 0)
	length = strlen(buff);
    else
	length = p - buff;
    for (s=command_giver->sent; s; s = s->next) {
	struct value *ret;
	struct value arg;
	
	if (s->verb == 0)
	    continue;
	if (strlen(s->verb) != length)
	    continue;
	if (strncmp(buff, s->verb, length) != 0)
	    continue;
	/*
	 * Now we have found a special sentence !
	 */
	if (d_flag)
	    debug_message("Local command %s on %s\n", s->function, s->ob->name);
	/*
	 * If we have a second argument which was not used for id()
	 * verification, then we send it to the function.
	 */
	if (buff[length] == ' ') {
	    arg.type = T_STRING;
	    arg.u.string = &buff[length+1];
	    ret = apply(s->function, s->ob, &arg);
	} else {
	    ret = apply(s->function, s->ob, 0);
	}
	/* If we get fail from the call, it was wrong second argument. */
	if (ret && ret->type == T_NUMBER && ret->u.number == 0)
	    continue;
	if (s->ob->wl && command_giver->interactive)
	    s->ob->wl->score++;
	if (ret == 0)
	    add_message("Error: function %s not found.\n", s->function);
	break;
    }
    if (s == 0)
	add_message("What?\n");
}

void add_action(str, id)
    char *str;
    struct value *id;
{
    struct sentence *p;

    if (d_flag)
	debug_message("--Add action %s\n", str);
    if (command_giver) {
	p = alloc_sentence();
	p->function = string_copy(str);
	p->ob = current_object;
	p->next = command_giver->sent;
	p->verb = 0;
	command_giver->sent = p;
    }
    else
	yyerror("Add_action called with no command_giver");
}

void add_verb(str)
    char *str;
{
	if (d_flag)
	    debug_message("--Adding verb %s to action %s\n", str,
			  command_giver->sent->function);
	if (command_giver) {
	    if (command_giver->sent == 0)
		error("No add_action().\n");
	    if (command_giver->sent->verb != 0)
		error("Tried to set verb again.\n");
	    command_giver->sent->verb = string_copy(str);
	}
	else
	    yyerror("Add_verb called with no command_giver");
}

void remove_sent(ob, player)
    struct object *ob, *player;
{
    struct sentence **s;

    for (s= &player->sent; *s;) {
	struct sentence *tmp;
	if ((*s)->ob == ob) {
	    if (d_flag)
		debug_message("--Unlinking sentence %s\n", (*s)->function);
	    tmp = *s;
	    *s = tmp->next;
	    free_sentence(tmp);
	} else
	    s = &((*s)->next);
    }
}

void display_all_players() {
    struct object *ob;
    struct value *ret;

    for (ob = obj_list; ob; ob = ob->next_all) {
	if (ob->interactive == 0)
	    continue;
	ret = apply("who", ob, &const0);
	if (ret && ret->type == T_STRING)
	    add_message("%s.\n", ret->u.string);
    }
}

int special_parse(buff)
    char *buff;
{
    char *tmpbuff;
    struct value *player;

    if (buff[0] == '#')
	return 0;
    if (strcmp(buff, "who") == 0) {
	display_all_players();
	return 1;
    }
    if (strcmp(buff, "dumpallobj") == 0) {
        if (!command_giver) return 0;
	player = apply("query_level",command_giver,0);
	if (player->type != T_NUMBER ||
	    player->u.number < ALL_POWER) return 0;
        dump_all_objects();
	return 1;
    }
    if (strcmp(buff, "status") == 0) {
	extern int tot_alloc_sentence, tot_alloc_value, tot_alloc_object,
		   tot_alloc_lnode, num_swapped, total_bytes_swapped,
		   tot_string_space, tot_alloc_strings;
	add_message("Sentences:   %5d %6d\n", tot_alloc_sentence,
	       tot_alloc_sentence * sizeof (struct sentence));
	add_message("Objects:     %5d %6d (%d swapped, %d Kbyte)\n",
		    tot_alloc_object,
		    tot_alloc_object * sizeof (struct object), num_swapped,
		    total_bytes_swapped / 1024);
	add_message("Values:      %5d %6d\n\n", tot_alloc_value,
	       tot_alloc_value * sizeof (struct value));
	add_message("Strings:          %6d\n", tot_alloc_strings);
	print_lnode_status(tot_alloc_sentence * sizeof (struct sentence) +
			   tot_alloc_object * sizeof (struct object) +
			   tot_alloc_value * sizeof (struct value) +
			   tot_alloc_strings);
	add_message("String space saved: %d bytes.\n", tot_string_space);
	return 1;
    }
    /* This probably shouldn't be done this way, since buff is only 2000
       bytes long, and will therefore crash if the input line is over
       1996 characters long. */
    if ((*buff == '\'') || (*buff == '"')) {
        tmpbuff = string_copy(buff);
	(void)strcpy(buff, "say ");
	(void)strcpy(buff + 4, tmpbuff + 1);
	free(tmpbuff);
	return 0;
    }
    if (*buff == ':') {
	tmpbuff = string_copy(buff);
	(void)strcpy(buff, "emote ");
	(void)strcpy(buff + 6, tmpbuff + 1);
	free(tmpbuff);
	return 0;
    }
    if (strcmp(buff, "e") == 0) {
	(void)strcpy(buff, "east");
	return 0;
    }
    if (strcmp(buff, "w") == 0) {
	(void)strcpy(buff, "west");
	return 0;
    }
    if (strcmp(buff, "s") == 0) {
	(void)strcpy(buff, "south");
	return 0;
    }
    if (strcmp(buff, "n") == 0) {
	(void)strcpy(buff, "north");
	return 0;
    }
    if (strcmp(buff, "ne") == 0) {
	(void)strcpy(buff, "northeast");
	return 0;
    }
    if (strcmp(buff, "nw") == 0) {
	(void)strcpy(buff, "northwest");
	return 0;
    }
    if (strcmp(buff, "se") == 0) {
	(void)strcpy(buff, "southeast");
	return 0;
    }
    if (strcmp(buff, "sw") == 0) {
	(void)strcpy(buff, "southwest");
	return 0;
    }
    if (strcmp(buff, "d") == 0) {
	(void)strcpy(buff, "down");
	return 0;
    }
    if (strcmp(buff, "u") == 0) {
	(void)strcpy(buff, "up");
	return 0;
    }
    if (strcmp(buff, "nw") == 0) {
	(void)strcpy(buff, "northwest");
	return 0;
    }
    if (strcmp(buff, "ne") == 0) {
	(void)strcpy(buff, "northeast");
	return 0;
    }
    if (strcmp(buff, "sw") == 0) {
	(void)strcpy(buff, "southwest");
	return 0;
    }
    if (strcmp(buff, "se") == 0) {
	(void)strcpy(buff, "southeast");
	return 0;
    }
    return 0;
}

void print_local_commands() {
    struct sentence *s;

    add_message("Current local commands:\n");
    for (s = command_giver->sent; s; s = s->next)
	add_message("%s ", s->verb);
    add_message("\n");
}

/*VARARGS1*/
void fatal(fmt, a, b, c, d, e, f, g, h)
    char *fmt;
    int a, b, c, d, e, f, g, h;
{
    (void)fprintf(stderr, fmt, a, b, c, d, e, f, g, h);
    (void)fprintf(stderr, "Current object was %s\n", current_object->name);
    debug_message(fmt, a, b, c, d, e, f, g, h);
    debug_message("Current object was %s\n", current_object->name);
    debug_message("Dump of variables:\n");
    if (current_object) {
	struct lnode_var_def *p;
	for (p=current_object->status; p; p = p->next) {
	    debug_message("%20s: ", p->name);
	    debug_message_value(&current_object->variables[p->num_var]);
	    debug_message("\n");
	}
    }
#ifdef TRACE
    (void)dump_trace();
#endif
    ipc_remove();	/* Shut down the ipc communication. */
    abort();
}

int num_error = 0;

/*VARARGS1*/
void error(fmt, a, b, c, d, e, f, g, h)
    char *fmt;
    int a, b, c, d, e, f, g, h;
{
    struct object *dest;
    extern int error_recovery_context_exists;
    extern jmp_buf error_recovery_context;
    extern struct object *current_heart_beat;
    char *object_name;
#ifdef TRACE
    extern int trace_depth;
#endif

    num_error++;
    if (num_error > 2)
	fatal("Too many simultaneous errors.\n");
    debug_message(fmt, a, b, c, d, e, f, g, h);
    if (current_object)
	debug_message("Current object was %s, line %d\n",
		      current_object->name, current_line);
#ifdef TRACE
    object_name = dump_trace();
    if (object_name) {
	struct object *ob;
	ob = find_object2(object_name);
	if (!ob) {
	    if (command_giver)
		add_message("error when executing program in destroyed object %s\n",
			    object_name);
	    debug_message("error when executing program in destroyed object %s\n",
			object_name);
	}
    }
    trace_depth = 0;
#endif
    if (command_giver) {
	add_message(fmt, a, b, c, d, e, f, g, h);
	if (current_object)
	    add_message("Current object was %s, line %d\n",
			current_object->name, current_line);
	/*
	 * If this is a player, send him to the church.  Otherwise, send it to
	 * the void.  Also make sure we have a church!
	 */
	if (!(command_giver->enable_commands &&
	     apply("is_player",command_giver,&const0) &&
	     (dest = find_object("room/church.c"))))
	    dest = find_object("room/void.c");
	if (dest == 0)
	    fatal("Could not find the void room.\n");
	move_object(command_giver, dest);
    }
    if (current_heart_beat) {
        current_heart_beat->heart_beat = 0;
	debug_message("Heart beat in %s turned off.\n",
		      current_heart_beat->name);
	current_heart_beat = 0;
    }
    debug_message("Dump of variables:\n");
    if (current_object) {
	struct lnode_var_def *p;
	for (p=current_object->status; p; p = p->next) {
	    debug_message("%20s: ", p->name);
	    debug_message_value(&current_object->variables[p->num_var]);
	    debug_message("\n");
	}
    }
    num_error--;
    if (error_recovery_context_exists)
	longjmp(error_recovery_context, 1);
    abort();
}

#ifdef TRACE
char *get_current_object_name() {
    if (current_object == 0)
	return "NONE";
    return current_object->name;
}

char *get_command_giver_name() {
    if (command_giver == 0)
	return "NONE";
    return command_giver->name;
}
#endif

void pre_compile(str)
    char *str;
{
    char *c_name, *i_name, buff[1000];
    FILE *f;
    int pid;

    if (!legal_path(str))
	error("Illegal attempt to access %s\n", str);
    i_name = (char *)xalloc(strlen(str)+3);
    (void)strcpy(i_name, str);
    (void)strcat(i_name, ".i");
    c_name = (char *)xalloc(strlen(str)+3);
    (void)strcpy(c_name, str);
    (void)strcat(c_name, ".c");
    sprintf(buff, "%s %s %s", PRE_COMPILE, c_name, i_name);
    f = (FILE *)vpopen(buff, "r");
    if (f == 0) {
	error("Unable to invoke precompiler!\n");
	alarm(0);
    }
    while(1) {
	if (fgets(buff, sizeof buff, f) == 0)
	    break;
	add_message("%s", buff);
    }
    vpclose(f);
}

/*
 * Check that it is an legal path. It must not contain a space
 * or '..' or begin with '/'.
 */
legal_path(path)
    char *path;
{
    char *p;

    if (path == NULL || strchr(path, ' '))
	return 0;
    if (path[0] == '/')
        return 0;
    for(p = strchr(path, '.'); p; p = strchr(p+1, '.')) {
	if (p[1] == '.')
	    return 0;
    }
    return 1;
}

smart_log(error_file, line, what)
     char *error_file, *what;
     int line;
{
    char buff2[100], buff[100], *p;
    int n;

    if (error_file == 0)
	return;
    n = sscanf(error_file, "players/%s", buff2);
    if (n != 1)
        return;
    p = strchr(buff2, '/');
    if (p)
        *p = '\0';
    sprintf(buff, "%s line %d:%s\n", error_file, line, what);
    log_file(buff2, buff);
}

/*
 * Check that a file name is valid for read or write.
 * Also change the name as if the current directory was at the players
 * own directory.
 * This is done by functions in the player object.
 */
char *check_file_name(file, writeflg)
    char *file;
    int writeflg;
{
    struct value v, *ret;

    if (!command_giver) {
	yyerror("Check_file_name called with no command_giver");
	return 0;
    }
    v.type = T_STRING;
    v.u.string = file;
    /*
     * We don't have to free the string in ret. This is done
     * by the garbage collection.
     */
    if (writeflg)
	ret = apply("valid_write", command_giver, &v);
    else
	ret = apply("valid_read",  command_giver, &v);
    if (!ret || ret->type != T_STRING) {
	add_message("Bad file name.\n");
	return 0;
    }
    if (!legal_path(ret->u.string)) {
        add_message("Illegal path\n");
	return 0;
    }
    return ret->u.string;
}

/*
 * This one is called from HUP, and from the command "shutdown".
 */
void shutdowngame() {
    struct value *player;

    if (!command_giver) return ;
    player = apply("query_level",command_giver,0);
    if (player->type != T_NUMBER || player->u.number < SHUTDOWN) return; 
    shout_string("LPmud shutting down immediately.\n");
    fprintf(stderr, "simulate.c: shutdowngame: LP-mud shut down.\n");
    remove_all_players();
    ipc_remove();
    save_wiz_file();
    exit(0);
}

/*
 * Transfer an object from an object to an object.
 * Call add_weight(), drop(), get(), prevent_insert(), add_weight(),
 * and can_put_and_get() where needed.
 * Return 0 on success, and special code on failure:
 *
 * 1: Too heavy for destination.
 * 2: Can't be dropped.
 * 3: Can't take it out of it's container.
 * 4: The object can't be inserted into bags etc.
 * 5: The destination doesn't allow insertions of objects.
 * 6: The object can't be picked up.
 */
int transfer_object(ob, to)
    struct object *ob, *to;
{
    struct value *weight, neg_weight, *ret;
    struct object *from = ob->super;

    /*
     * Get the weight of the object
     */
    weight = apply("query_weight", ob, 0);
    if (weight && weight->type != T_NUMBER)
	error("Bad type of weight of object in transfer()\n");
    /*
     * If the original place of the object is a living object,
     * then we must call drop() to check that the object can be dropped.
     */
    if (from && from->enable_commands) {
	ret = apply("drop", ob, 0);
	if (ret && (ret->type != T_NUMBER || ret->u.number != 0))
	    return 2;
    }
    /*
     * If 'from' is not a room and not a player, check that we may
     * remove things out of it.
     */
    if (from && from->super && !from->enable_commands) {
	ret = apply("can_put_and_get", from, 0);
	if (!ret || (ret->type != T_NUMBER && ret->u.number != 1))
	    return 3;
    }
    /*
     * If the destination is not a room, and not a player,
     * Then we must test 'prevent_insert', and 'can_put_and_get'.
     */
    if (to->super && to->enable_commands == 0) {
	ret = apply("prevent_insert", ob, 0);
	if (ret && (ret->type != T_NUMBER || ret->u.number != 0))
	    return 4;
	ret = apply("can_put_and_get", to, 0);
	if (!ret || (ret->type != T_NUMBER && ret->type != 0))
	    return 5;
    }
    /*
     * If the destination is a player, check that he can pick it up.
     */
    if (to->enable_commands) {
	ret = apply("get", ob, 0);
	if (!ret || (ret->type == T_NUMBER && ret->u.number == 0))
	    return 6;
    }
    /*
     * If it is not a room, correct the total weight in the destination.
     */
    if (to->super && weight) {
	/*
	 * Check if the destination can carry that much.
	 */
	ret = apply("add_weight", to, weight);
	if (ret && ret->type == T_NUMBER && ret->u.number == 0)
	    return 1;
    }
    /*
     * If it is not a room, correct the weight in the 'from' object.
     */
    if (from && from->super && weight) {
	neg_weight.type = T_NUMBER;
	neg_weight.u.number = - weight->u.number;
	(void)apply("add_weight", from, &neg_weight);
    }
    move_object(ob, to);
    return 0;
}

/*
 * Move or destruct one object.
 */
move_or_destruct(what, to)
    struct object *what, *to;
{
    struct object *super;
    int res;
    struct value v;

    res = transfer_object(what, to);
    if (res == 0)
	return;
    if (res == 1 || res == 4 || res == 5) {
	move_or_destruct(what, to->super);
	return;
    }
    /*
     * No need to add the reference count of 'what', as this
     * local 'v' is not deallocated by 'free_all_value()'
     */
    v.type = T_OBJECT;
    v.u.ob = what;
    destruct_object(&v);
}
