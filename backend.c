#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <setjmp.h>
#include "config.h"
#include "lnode.h"
#include "object.h"
#include "interpret.h"
#include "wiz_list.h"

jmp_buf error_recovery_context;
int error_recovery_context_exists = 0;

extern char *string_copy();
extern struct object *command_giver, *obj_list_destruct;
extern int num_player;

struct object *current_heart_beat;
struct object *current_reset;

void call_heart_beat(), catch_alarm();
extern void free(), add_message(), print_all(), fatal(), set_current_room(),
    player_parser(), free_all_values(), load_first_objects(), prepare_ipc(),
    call_function(), alarm(), add_ref();

extern int get_message();

extern int t_flag;
int time_to_call_heart_beat;

void logon(ob)
    struct object *ob;
{
    struct value *ret;

    ret = apply("logon", ob, 0);
    if (ret == 0) {
	add_message("prog %s:\n", ob->name);
	fatal("Could not find logon on the player %s\n", ob->name);
    }
}

/*
 * Take a player command and parse it.
 * The command can also come from a NPC.
 * Beware that 'str' can be modified and extended !
 */
void parse_command(str, ob)
    char *str;
    struct object *ob;
{
    struct object *save = command_giver;

    command_giver = ob;
    /* search_for_it(str); */
    player_parser(str);
    command_giver = save;
}

/*
 * This is the backend. We will stay here for ever (almost).
 */
int eval_cost;
void backend()
{
    char buff[2000];
    int worst_eval_cost = MAX_COST/5;
    extern void shutdowngame();
    extern int e_flag;
#ifdef TRACE
    extern int trace_depth;
#endif

    free_all_values();
    (void)printf("Loading init file %s\n", INIT_FILE);
    load_first_objects();
    /* This is a major kludge.  For some reason the environment of the last
       castle preloaded doesn't get its long description! */
    load_object("room/rum2.c");
    (void)printf("Setting up ipc.\n");
    prepare_ipc();
    (void)signal(SIGHUP, shutdowngame);
    if (!t_flag)
	call_heart_beat();
    if (setjmp(error_recovery_context))
	add_message("Anomaly in the fabric of world space.\n");
    error_recovery_context_exists = 1;
#ifdef TRACE
    trace_depth = 0;
#endif
    while(1) {
	eval_cost = 0;
	remove_destructed_objects();
	if (get_message(buff, sizeof buff)) {
	    /*
	     * Now we have a string from the player. This string can go to
	     * one of several places. If it is prepended with a '!', then
	     * it is an escape from the 'ed' editor, so we send it
	     * as a command to the parser.
	     * If any object function is waiting for an input string, then
	     * send it there.
	     * Otherwise, send the string to the parser.
	     */
	    if (!command_giver->interactive)
		fatal("Non interactive player in main loop !\n");
	    if (command_giver->reset > 1)
		command_giver->reset = 1;
	    if (call_function_interactive(command_giver->interactive,buff))
		;	/* Do nothing else ! */
	    else if (buff[0] == '!')
		parse_command(buff+1, command_giver);
	    else if (command_giver->ed_buffer)
		ed_cmd(buff);
	    else
		parse_command(buff, command_giver);
	    /*
	     * Print a prompt if player is still here.
	     */
	    if (command_giver->interactive)
		print_prompt();
	}
	command_giver = 0;
	if (time_to_call_heart_beat)
	    call_heart_beat();
	free_all_values();
	if (eval_cost > worst_eval_cost) {
	    worst_eval_cost = eval_cost;
	    fprintf(stderr, "New worst eval cost: %d\n", eval_cost);
	}
    }
}

/*
 * Call all heart_beat() functions in all objects.
 */
void call_heart_beat() {
    struct object *ob, *hide_current = current_object,
                  *hide_command = command_giver;
    jmp_buf save_error_recovery_context;
    int save_rec_exists;

    if (num_player > 0) {
	for (ob = obj_list; ob; ob = ob->next_all) {
	    if (ob->heart_beat == 0 || ob->reset == 0 ||
		ob->enable_heart_beat == 0)
		continue;
	    if (ob->swapped)
		fatal("Heart beat in swapped object.\n");
	    current_object = ob;
	    current_heart_beat = ob;
	    if (ob->enable_commands)
		command_giver = ob;
	    else
		command_giver = 0;
	    if (ob->wl)
		ob->wl->heart_beats++;
	    call_function(ob->heart_beat);
	}
	command_giver = hide_command;
	current_object = hide_current;
	    current_heart_beat = 0;
	move_current_reset();
    }
    time_to_call_heart_beat = 0;
    (void)signal(SIGALRM, catch_alarm);
    alarm(2);
}

/*
 * There is a file with a list of objects to be initialized at
 * start up.
 */

void load_first_objects() {
    FILE *f;
    char buff[1000];
    char *p;
    extern int e_flag;

    if (!load_object("room/void"))
	fatal("Could not load the void.\n");
    if (e_flag)
	return;
    f = fopen(INIT_FILE, "r");
    if (f == 0)
	return;
    if (setjmp(error_recovery_context))
	add_message("Anomaly in the fabric of world space.\n");
    error_recovery_context_exists = 1;
    while(1) {
	if (fgets(buff, sizeof buff, f) == NULL)
	    break;
	if (buff[0] == '#')
	    continue;
	p = strchr(buff, '\n');
	if (p != 0)
	    *p = 0;
	if (buff[0] == '\0')
	    continue;
	(void)printf("Preloading: %s\n", buff);
	(void)load_object(buff);
    }
    error_recovery_context_exists = 0;
    fclose(f);
}

void catch_alarm() {
    time_to_call_heart_beat = 1;
}

/*
 * Move reset to next object, one step for every call of this
 * routine.
 * Beware that the current_reset object might have been destructed
 * by destruct_object(), but then, it was moved to the next object.
 */
move_current_reset() {
    static long delay = 0;
    struct value arg;
    extern long time();
    jmp_buf save_error_recovery_context;
    int save_rec_exists;
    
    delay--;
    if (delay > time(0))
	return;
    arg.type = T_NUMBER;
    arg.u.number = 1;
    if (current_reset == 0)
	current_reset = obj_list;
    if (current_reset == 0) {
        return;
    }
    memcpy((char *) save_error_recovery_context,
	   (char *) error_recovery_context, sizeof error_recovery_context);
    save_rec_exists = error_recovery_context_exists;
    error_recovery_context_exists = 1;
    /* We have to catch an error here, locally.
     * It is not good if the error is catched globally, as the current
     * reset wouldn't be moved forward.
     */
    if (setjmp(error_recovery_context)) {
        if (!current_reset)
	    fatal("Error in reset, but no object was called with reset!\n");
	current_reset->reset = 0;
	debug_message("Error in reset.\n");
    } else {
	int save_not_touched = current_reset->not_touched;
        if (current_reset->reset && current_reset->not_touched == 0 &&
	    current_reset->swapped == 0){
	    (void)apply("reset", current_reset, &arg);
	}
	current_reset->not_touched = save_not_touched + 1;
	if (current_reset->not_touched >= NUM_RESET_TO_SWAP &&
	  !current_reset->swapped)
	    swap(current_reset);
    }
    memcpy((char *) error_recovery_context,
	   (char *) save_error_recovery_context,
	   sizeof error_recovery_context);
    error_recovery_context_exists = save_rec_exists;
    current_reset = current_reset->next_all;
    if (!current_reset) {
        delay = 15 - num_player;
	if (delay < 0)
	    delay = 0;
	delay = time(0) + 60 * delay;
	wiz_decay();
	save_wiz_file();
    }
}

/*
 * All destructed objects are moved int a sperate linked list,
 * and deallocated after program execution.
 */
remove_destructed_objects()
{
    struct object *ob, *next;
    for (ob=obj_list_destruct; ob; ob = next) {
	next = ob->next_all;
	destruct2(ob);
    }
    obj_list_destruct = 0;
}
