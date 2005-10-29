#include <stdio.h>
#include <string.h>
#include <errno.h>
#include "lnode.h"	/* Only for sake of -g flag to cc (SYSV) ! */
#include "comm.h"
#include "object.h"
#include "sent.h"
#include "interpret.h"

extern char *malloc(), *string_copy();
extern int errno;

extern void error(), exit();

extern struct object *current_object;

extern struct value *clone_object();

void prepare_ipc()
{
}

/*VARARGS1*/
void add_message(fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9)
    char *fmt;
    int a1, a2, a3, a4, a5, a6, a7, a8, a9;
{
    if (command_giver == 0 || command_giver->interactive == 0)
	return;
    (void)printf(fmt, a1, a2,a3, a4, a5, a6, a7, a8, a9);
}

static struct object *logon_object;

int get_message(buff, size)
    char *buff;
    int size;
{
    char *p;
    if (logon_object == 0) {
	struct object *ob;

	current_object = 0;
	ob = (clone_object("obj/player"))->u.ob;
	if (ob == 0)
	    fatal("Could not load 'obj/player'\n");
	logon_object = ob;
	command_giver = ob;
	logon_object->interactive =
	    (struct interactive *)malloc(sizeof (struct interactive));
	ob->interactive->input_to = 0;
	ob->interactive->prompt = "> ";
	logon(ob);
    }
    (void)fflush(stdout);
    if (fgets(buff, size, stdin) == 0) {
	if (errno == EINTR)
	    return 0;
	exit(0);
    }
    p = strchr(buff, '\n');
    if (p)
	*p = '\0';
    command_giver = logon_object;
    return 1;
}

void remove_interactive(ob)
    struct object *ob;
{
    if (ob != logon_object)
	fatal("Illegal interactive object in remove_interactive().\n");
    logon_object = 0;
    return;
}

void ipc_remove()
{
}

call_function_interactive(i, str)
    struct interactive *i;
    char *str;
{
    char *function;
    struct object *save_current_object = current_object;
    struct value *val;

    if (!i->input_to)
	return 0;
    /*
     * Special feature: input_to() has been called to setup
     * a call to a function.
     */
    if (command_giver == 0 || command_giver->interactive == 0 ||
	command_giver->interactive->input_to == 0)
	fatal("Bad status in call_function_interactive().\n");
    function = string_copy(command_giver->interactive->input_to->function);
    current_object = command_giver->interactive->input_to->ob;
    val = alloc_value();
    val->type = T_STRING;
    val->u.string = string_copy(str);
    /*
     * We must clear this reference to sentence before the call to apply(),
     * because someone
     * might want to set up a new input_to().
     */
    free_sentence(command_giver->interactive->input_to);
    command_giver->interactive->input_to = 0;
    (void)apply(function, current_object->prog, val);
    free(function);
    current_object = save_current_object;
    return 1;
}

int set_call(ob, sent)
    struct object *ob;
    struct sentence *sent;
{
    if (ob->interactive->input_to)
	return 0;
    ob->interactive->input_to = sent;
    return 1;
}

void show_info_about(str, room, i)
    char *str, *room;
    struct interactive *i;
{
    add_message("%s %s %s\n", str, room, logon_object->name);
}

void remove_all_players()
{
    current_object = logon_object;
    (void)apply("quit", logon_object->prog, 0);
}

set_prompt(str)
    char *str;
{
    command_giver->interactive->prompt = str;
}

print_prompt()
{
    if (command_giver == 0)
	fatal("command_giver == 0.\n");
    add_message(command_giver->interactive->prompt);
}
