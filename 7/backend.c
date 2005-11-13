#include "config.h"

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/time.h>
#ifdef AMIGA
#include "hosts/amiga/nsignal.h"
#else
#include <signal.h>
#include <sys/times.h>
#endif
#include <sys/stat.h>
#include <fcntl.h>
#include <time.h>
#include <math.h>

#include "lint.h"
#include "interpret.h"
#include "object.h"
#include "wiz_list.h"
#include "exec.h"
#include "comm.h"
#include "rxcache.h"
#include "stralloc.h"

struct error_recovery_info toplevel_error_recovery_info = {
    (struct error_recovery_info*)0,
    ERROR_RECOVERY_NONE
};

struct error_recovery_info *error_recovery_pointer =
	&toplevel_error_recovery_info;
/*
 * The 'current_time' is updated at every heart beat.
 */
int current_time;

extern struct object *command_giver, *current_interactive;
extern int num_player, d_flag;
extern struct object *previous_ob, *master_ob;
extern int trace_level;
extern int tracedepth;
extern long time_to_swap; /* marion - for invocation parameter */
extern long time_to_swap_variables;


struct object *current_heart_beat;

void call_heart_beat();
void load_first_objects(), prepare_ipc(),
    shutdowngame(), ed_cmd PROT((char *)),
    print_prompt(), call_out(),
    destruct2 PROT((struct object *));

extern int get_message PROT((char *, int)), player_parser PROT((char *)),
    call_function_interactive PROT((struct interactive *, char *)),
    resort_free_list();

extern void flush_all_player_mess();

extern int t_flag;
int time_to_call_heart_beat;
int comm_time_to_call_heart_beat = 0; /* this is set by interrupt, */
	/* comm sets time_to_call_heart_beat sometime after */
mp_int total_alarms = 0;

/*
 * There are global variables that must be zeroed before any execution.
 * In case of errors, there will be a longjmp(), and the variables will
 * have to be cleared explicitely. They are normally maintained by the
 * code that use them.
 *
 * This routine must only be called from top level, not from inside
 * stack machine execution (as stack will be cleared).
 */
void clear_state() {
    extern struct object *previous_ob;
    extern char *current_file;

    current_file = 0;
    current_object = 0;
    command_giver = 0;
    current_interactive = 0;
    previous_ob = 0;
    current_prog = 0;
    reset_machine(0);	/* Pop down the stack. */
}

extern int check_state();

void logon(ob)
    struct object *ob;
{
    struct svalue *ret;
    struct object *save = current_object;

    /*
     * current_object must be set here, so that the static "logon" in
     * player.c can be called.
     */
    current_object = ob;
    ret = apply(STR_LOGON, ob, 0);
    if (ret == 0) {
	add_message("prog %s:\n", ob->name);
	error("Could not find logon on the player %s\n", ob->name);
    }
    current_object = save;
}

/*
 * Take a player command and parse it.
 * The command can also come from a NPC.
 * Beware that 'str' can be modified and extended !
 */
int parse_command(str, ob)
    char *str;
    struct object *ob;
{
    struct object *save = command_giver;
    int res;

    command_giver = ob;
    res = player_parser(str);
    command_giver = check_object(save);
    return res;
}

#ifdef AMIGA
/* Clean up the alarm timer, this is set as atexit() function */

void exit_alarm_timer() { alarm(0); }
#endif

/*
 * This is the backend. We will stay here for ever (almost).
 */
int32 initial_eval_cost = -MAX_COST;
int32 eval_cost, assigned_eval_cost;
int extra_jobs_to_do = 0;
int garbage_collect_to_do = 0;
void backend()
{
    char buff[MAX_TEXT+4];
    extern int game_is_being_shut_down;
    extern int slow_shut_down_to_do;
    extern int master_will_be_updated;
    extern mp_int num_dirty_mappings;
    extern int malloc_privilege;

    (void)printf("Setting up ipc.\n");
    fflush(stdout);
    prepare_ipc();
    (void)signal(SIGHUP,  (RETSIGTYPE(*)PROT((int)))f_shutdown);
    (void)signal(SIGUSR1, (RETSIGTYPE(*)PROT((int)))startmasterupdate);
    if (!t_flag) {
	ALARM_HANDLER_FIRST_CALL(catch_alarm)
	call_heart_beat();
    }
#ifdef AMIGA
    atexit(exit_alarm_timer);
#endif
    toplevel_error_recovery_info.type = ERROR_RECOVERY_BACKEND;
    setjmp(toplevel_error_recovery_info.con.text);
    /*
     * We come here after errors, and have to clear some global variables.
     */
    clear_state();
    flush_all_player_mess();
    while(1) {
	/*
	 * The call of clear_state() should not really have to be done
	 * once every loop. However, there seem to be holes where the
	 * state is not consistent. If these holes are removed,
	 * then the call of clear_state() can be moved to just before the
	 * while() - statment. *sigh* /Lars
	 */
	/* amylaar: I think inconsistencys should be found, rather than
	 * the effects patched
	 */
#ifdef DEBUg /* don't need it currently */
	if ( check_state() ) {
	    debug_message("Inconsistency in main loop\n");
	    dump_trace(1);
#ifdef TRACE_CODE
	    last_instructions(TOTAL_TRACE_LENGTH, 1, 0);
#endif
	    clear_state();
	}
#endif
	CLEAR_EVAL_COST;
#ifndef __GNUC__
	alloca(0); /* free alloca'd values from deeper levels of nesting */
#endif
	remove_destructed_objects(); /* marion - before ref checks! */
#ifdef DEBUG
	{
	  extern int check_a_lot_ref_counts_flag;

	  if (check_a_lot_ref_counts_flag)
	    check_a_lot_ref_counts(0);
	}
#endif
	if (extra_jobs_to_do) {
	    current_interactive = 0;
	    if (game_is_being_shut_down) {
		command_giver = 0; /* This statement was removed from the end
				    * of the main loop. We have to compensate
				    * for this.
	    			    */
		current_object = 0;
		shutdowngame();
	    }
	    if (master_will_be_updated) {
		extern struct object dummy_current_object_for_loads;

		emergency_destruct(master_ob);
		master_will_be_updated = 0;
		/* maybe you'll want the new master to reload the player files
		 * as well :-)
		 */
		command_giver = 0;
		current_object = &dummy_current_object_for_loads;
		apply_master_ob(STR_EXT_RELOAD, 0);
		current_object = 0;
	    }
	    if (garbage_collect_to_do) {
		extern void garbage_collection();
		extern time_t time_last_gc;
                time_t time_now = time(NULL);
		char buf[90];

		if (time_now - time_last_gc >= 300)
		{
		  sprintf(buf, "Garbage collection, slow_shut: %d\n", slow_shut_down_to_do);
		  write(1, buf, strlen(buf));
		  command_giver = 0;
		  current_object = 0;
		  garbage_collection();
		}
		else
		{
		  extern void reallocate_reserved_areas();
		  sprintf(buf, "No garbage collection, slow_shut: %d\n", slow_shut_down_to_do);
		  write(1, buf, strlen(buf));
                  reallocate_reserved_areas();		    
		}
		garbage_collect_to_do = 0;
		if (slow_shut_down_to_do) {
		    int tmp = slow_shut_down_to_do;
		    slow_shut_down_to_do = 0;
		    malloc_privilege = MALLOC_MASTER;
		    slow_shut_down(tmp);
		}
		malloc_privilege = MALLOC_USER;
	    }
	    extra_jobs_to_do = 0;
	    if (num_dirty_mappings) {
		extern void compact_mappings PROT((mp_int));

		compact_mappings(num_dirty_mappings+80 >> 5);
		malloc_privilege = MALLOC_USER;
	    }
	}
	if (get_message(buff, sizeof buff)) {
	    void update_load_av PROT((void));

	    update_load_av();
	    /*
	     * Now we have a string from the player. This string can go to
	     * one of several places. If it is prepended with a '!', then
	     * it is an escape from the 'ed' editor, so we send it
	     * as a command to the parser.
	     * If any object function is waiting for an input string, then
	     * send it there.
	     * Otherwise, send the string to the parser.
	     * The player_parser() will find that current_object is 0, and
	     * then set current_object to point to the object that defines
	     * the command. This will enable such functions to be static.
	     */
	    current_object = 0;
	    current_interactive = command_giver;

#ifdef DEBUG
	    if (!O_GET_INTERACTIVE(command_giver) ||
		O_GET_INTERACTIVE(command_giver)->sent.type != SENT_INTERACTIVE)
	    {
		fatal("Non interactive player in main loop !\n");
	    }
#endif
	    tracedepth = 0;
	    if (buff[0] == '!' && buff[1] && command_giver->super &&
		!(O_GET_INTERACTIVE(command_giver)->noecho & IGNORE_BANG))
	    {
		if (O_GET_INTERACTIVE(command_giver)->noecho & NOECHO) {
		    add_message("%s\n",
		      buff + O_GET_INTERACTIVE(command_giver)->chars_ready);
		    O_GET_INTERACTIVE(command_giver)->chars_ready = 0;
		}
		parse_command(buff+1, command_giver);
	    } else if (O_GET_INTERACTIVE(command_giver)->sent.ed_buffer)
		ed_cmd(buff);
	    else if (
	      call_function_interactive(O_GET_INTERACTIVE(command_giver),buff))
		;	/* Do nothing ! */
	    else
		parse_command(buff, command_giver);
	    /*
	     * Print a prompt if player is still here.
	     */
	    if (command_giver) {
		struct interactive *ip;

		if ((ip = O_GET_INTERACTIVE(command_giver)) &&
		    ip->sent.type == SENT_INTERACTIVE)
		{
		    print_prompt();
		}
	    }
	}
	if (time_to_call_heart_beat)
	    call_heart_beat();
    } /* end of main loop */
}

/*
 * Despite the name, this routine takes care of several things.
 * It will loop through all objects once every 15 minutes.
 *
 * If an object is found in a state of not having done reset, and the
 * delay to next reset has passed, then reset() will be done.
 *
 * If the object has a existed more than the time limit given for swapping,
 * then 'clean_up' will first be called in the object, after which it will
 * be swapped out if it still exists.
 *
 * There are some problems if the object self-destructs in clean_up, so
 * special care has to be taken of how the linked list is used.
 */
static void look_for_objects_to_swap() {
    static int next_time;
    static struct object *next_ob; /* don't change back with longjmp() */
    struct object *ob;
    struct error_recovery_info error_recovery_info;
    union {
	double dummy; /* force alignment */
	char c[sizeof(struct program)+SCAN_SWAP_BUFSIZE];
    } swapbuf;

    if (current_time < next_time)
	return;				/* Not time to look yet */
#if TIME_TO_SWAP >= RESET_GRANULARITY || !TIME_TO_SWAP
    next_time = current_time + RESET_GRANULARITY;
#else
    next_time = current_time + TIME_TO_SWAP;
#endif
    /*
     * Objects object can be destructed, which means that
     * next object to investigate is saved in next_ob. If very unlucky,
     * that object can be destructed too. In that case, the loop is simply
     * restarted.
     */
    set_swapbuf(swapbuf.c);
    next_ob = obj_list;
    error_recovery_info.last = error_recovery_pointer;
    error_recovery_info.type = ERROR_RECOVERY_BACKEND;
    error_recovery_pointer = &error_recovery_info;
    if (setjmp(error_recovery_info.con.text)) {		/* amylaar */
        clear_state();
        debug_message("Error in look_for_objects_to_swap.\n");
    }
    for (; ob = next_ob; ) {
	int time_since_ref;
	next_ob = ob->next_all;
	if (ob->flags & O_DESTRUCTED) {
	    continue;
	}
	/*
	 * Check reference time before reset() is called.
	 */
	time_since_ref = current_time - ob->time_of_ref;
	/*
	 * Should this object have reset(1) called ?
	 */
	if (ob->next_reset < current_time && !(ob->flags & O_RESET_STATE)) {
	    if (d_flag)
		fprintf(stderr, "RESET %s\n", ob->name);
	    CLEAR_EVAL_COST;
	    command_giver = 0;
	    trace_level = 0;
	    reset_object(ob, H_RESET);
	    if (ob->flags & O_DESTRUCTED)
		continue;
	}
#if TIME_TO_CLEAN_UP > 0
	/*
	 * Has enough time passed, to give the object a chance
	 * to self-destruct ? Save the O_RESET_STATE, which will be cleared.
	 *
	 * Only call clean_up in objects that has defined such a function.
	 *
	 * Only if the clean_up returns a non-zero value, will it be called
	 * again.
	 */
	else if (time_since_ref > TIME_TO_CLEAN_UP &&
	    (ob->flags & O_WILL_CLEAN_UP))
	{
	    int was_swapped = ob->flags & O_SWAPPED ;
	    int save_reset_state = ob->flags & O_RESET_STATE;
	    struct svalue *svp;

	    if (d_flag)
		fprintf(stderr, "clean up %s\n", ob->name);
	    /*
	     * Supply a flag to the object that says if this program
	     * is inherited by other objects. Cloned objects might as well
	     * believe they are not inherited. Swapped objects will not
	     * have a ref count > 1 (and will have an invalid ob->prog
	     * pointer).
	     */
	    push_number(ob->flags & O_CLONE ? 0 :
	      ( O_PROG_SWAPPED(ob) ? 1 : ob->prog->ref) );
	    CLEAR_EVAL_COST;
	    command_giver = 0;
	    trace_level = 0;
	    if (closure_hook[H_CLEAN_UP].type == T_CLOSURE) {
		extern struct svalue *inter_sp;

		struct lambda *l;

		l = closure_hook[H_CLEAN_UP].u.lambda;
		if (closure_hook[H_CLEAN_UP].x.closure_type == CLOSURE_LAMBDA)
		    l->ob = ob;
		push_object(ob);
		call_lambda(&closure_hook[H_CLEAN_UP], 2);
		svp = inter_sp;
		pop_stack();
	    } else if (closure_hook[H_CLEAN_UP].type == T_STRING) {
		svp = apply(closure_hook[H_CLEAN_UP].u.string, ob, 1);
	    } else {
		pop_stack();
		goto no_clean_up;
	    }
	    if (ob->flags & O_DESTRUCTED)
		continue;
	    if ((!svp || (svp->type == T_NUMBER && svp->u.number == 0)) &&
		was_swapped )
		ob->flags &= ~O_WILL_CLEAN_UP;
	    ob->flags |= save_reset_state;
no_clean_up:
	    ;
	}
#endif /* TIME_TO_CLEAN_UP > 0 */
#if TIME_TO_SWAP > 0 || TIME_TO_SWAP_VARIABLES > 0
	/*
	 * At last, there is a possibility that the object can be swapped
	 * out.
	 */
	if ((time_to_swap <= 0 && time_to_swap_variables <= 0)
	||  (ob->flags & O_HEART_BEAT))
	    continue;
	if (time_since_ref >= time_to_swap_variables) {
	    if (!O_VAR_SWAPPED(ob)) {
		if (d_flag)
		    fprintf(stderr, "swap vars of %s\n", ob->name);
		swap_variables(ob);
	    }
	}
	if (time_since_ref >= time_to_swap) {
	    if (!O_PROG_SWAPPED(ob)) {
	        if (d_flag)
		    fprintf(stderr, "swap %s\n", ob->name);
	        swap_program(ob);	/* See if it is possible to swap out to disk */
	    }
	}
#endif
    }
    set_swapbuf((char *)0);
    error_recovery_pointer = error_recovery_info.last;
}

/*
 * Call all heart_beat() functions in all objects.  Also call the next reset,
 * and the call out.
 * We keep calling heart beats until a timeout or we have done num_heart_objs
 * calls.
 *
 * Set command_giver to current_object if it is a living object. If the object
 * is shadowed, check the shadowed object if living. There is no need to save
 * the value of the command_giver, as the caller resets it to 0 anyway.
 */
static struct object **hb_list = 0; /* head */
static struct object **hb_tail = 0; /* for sane wrap around */
static struct object **hb_last_called, **hb_last_to_call;

static mp_int num_hb_objs = 0; /* current number of objects in list */
static mp_int num_hb_to_do;    /* number of objects to do this round */
static mp_int hb_num_done;
static mp_int hb_max = 0;

static long num_hb_calls = 0; /* stats */
static long avg_num_hb_objs = 0, avg_num_hb_done = 0; /* decaying average */


void call_heart_beat() {
    struct object *ob, *hide_current = current_object;
    
    time_to_call_heart_beat = 0; /* interrupt loop if we take too long */
    comm_time_to_call_heart_beat = 0;
    /* If the host is swapping madly, it can be too late here to reinstate
     * the SIGALRM handler here.
     */
    alarm(2);

    current_time = get_current_time();
    current_interactive = 0;

    hb_last_to_call = hb_last_called;
    hb_num_done = 0;
    if (num_player > 0 && (num_hb_to_do = num_hb_objs)) {
        num_hb_calls++;
	while ( (
#ifdef MSDOS
	       timer_expire(),
#endif
	       !comm_time_to_call_heart_beat) )
	{
	    hb_num_done++;
	    if (++hb_last_called == hb_tail)
		hb_last_called = hb_list;
	    ob = *hb_last_called;
#ifdef DEBUG
	    if (!(ob->flags & O_HEART_BEAT))
		fatal("Heart beat not set in object on heart beat list!");
	    if (ob->flags & O_SWAPPED)
		fatal("Heart beat in swapped object.\n");
#endif
	    /* move ob to end of list, do ob */
	    if (ob->prog->heart_beat == -1) {
		if (hb_num_done == num_hb_to_do)
		    break;
		continue;
	    }
	    current_prog = ob->prog;
	    current_object = ob;
	    current_heart_beat = ob;
	    command_giver = ob;
	    if (command_giver->flags & O_SHADOW) {
		struct shadow_sentence *shadow_sent;

		while(shadow_sent = O_GET_SHADOW(command_giver),
		      shadow_sent->shadowing)
		{
		    command_giver = shadow_sent->shadowing;
		}
		if (!(command_giver->flags & O_ENABLE_COMMANDS)) {
		    command_giver = 0;
		    trace_level = 0;
		} else {
		    trace_level =
		      shadow_sent->type == SENT_INTERACTIVE ?
			((struct interactive *)shadow_sent)->trace_level : 0;
		}
	    } else {
		if (!(command_giver->flags & O_ENABLE_COMMANDS))
		    command_giver = 0;
		trace_level = 0;
	    }
	    ob->user->heart_beats++;
	    CLEAR_EVAL_COST;
	    call_function(ob->prog, ob->prog->heart_beat);
	    /* (hb_last_called == hb_last_to_call) is not a sufficient
	     * condition, since the first object with heart beat might
	     * call set_heart_beat(0) in the heart beat.
	     */
	    if (hb_num_done == num_hb_to_do)
		break;
	}
	avg_num_hb_objs += num_hb_to_do - (avg_num_hb_objs >> 10);
	avg_num_hb_done += hb_num_done  - (avg_num_hb_done >> 10);
    }
    current_heart_beat = 0;
    current_object = 0;
    look_for_objects_to_swap();
    call_out();	/* some things depend on this, even without players! */
    command_giver = 0;
    trace_level = 0;
    current_object = hide_current;
    wiz_decay();
}

/* If linear search & moving memory becomes too expensive due to a large
 * number of heart_beats, this should be implemented with a hash table
 * to map objects to a struct heart_heat, and a double linked list so
 * that easy removal is possible.
 * But with only a few hundred heart_beats in current muds, this is a
 * non-issue.
 */
/*
 * add or remove an object from the heart beat list; does the major check...
 * If an object removes something from the list from within a heart beat,
 * various pointers in call_heart_beat could be stuffed, so we must
 * check current_heart_beat and adjust pointers.
 */

int set_heart_beat(ob, to)
    struct object * ob;
    int to;
{
    if (ob->flags & O_DESTRUCTED)
	return 0;
    if (to)
	to = O_HEART_BEAT;
    if (to == (ob->flags & O_HEART_BEAT))
	return(0);
    if (to) {
	struct object **new_op;

	if (++num_hb_objs > hb_max) {
	    if (!hb_max) {
		hb_max = 16;
		hb_list =
		  (struct object **)xalloc(hb_max * sizeof(struct object **));
		if (!hb_list) {
		    hb_max = 0;
		    return 0;
		}
		hb_last_called = hb_last_to_call = (hb_tail = hb_list) - 1;
	    } else {
		struct object **new;
		mp_int diff;

		hb_max <<= 1;
		new = (struct object **)
		  rexalloc((char *)hb_list, hb_max * sizeof(struct object **));
		if (!new) {
		    hb_max >>= 1;
		    return 0;
		}
		diff = new - hb_list;
		hb_list = new;
		hb_tail += diff;
		hb_last_called += diff;
		hb_last_to_call += diff;
	    }
	}
	ob->flags |= O_HEART_BEAT;
	new_op = ++hb_last_called;
	move_memory(
	  (char *)(new_op+1),
	  (char *)new_op,
	  (char *)hb_tail++ - (char *)new_op
	);
	*new_op = ob;
	if (hb_last_to_call >= new_op)
	    hb_last_to_call++;
    } else {
	struct object **op;
	int active;

	ob->flags &= ~O_HEART_BEAT;
	for (op=hb_list; *op != ob; op++);
	move_memory(
	  (char *)op,
	  (char *)(op+1),
	  (char *)hb_tail-- - (char *)(op+1)
	);
	active = hb_last_called >= hb_last_to_call;
	if (hb_last_called >= op) {
	    hb_last_called--;
	    active ^= 1;
	}
	if (hb_last_to_call >= op) {
	    hb_last_to_call--;
	    active ^= 1;
	}
	/* hb_last_called == hb_last_to_call can mean either all called or
	 * all to be called - if the first object did a set_heart_beat(0) .
	 * If we decremented num_hb_to_do anyways, the statistics would
	 * be wrong.
	 */
	if (num_hb_to_do > hb_num_done)
	    num_hb_to_do -= active;
	num_hb_objs--;
    }
    return 1;
}

/*
 * sigh.  Another status function.
 */
int heart_beat_status(verbose)
    int verbose;
{
    char buf[20];

    if (verbose) {
	add_message("\nHeart beat information:\n");
	add_message("-----------------------\n");
	add_message("Number of objects with heart beat: %ld, starts: %ld\n",
		    (long)num_hb_objs, (long)num_hb_calls);
	sprintf(buf, "%.2f",
	  avg_num_hb_objs ?
	    100 * (double) avg_num_hb_done / avg_num_hb_objs :
	    100.0
	);
	add_message("Percentage of HB calls completed last time: %s\n", buf);
    }
    return 0;
}

/*
 * New version of preloading objects. The epilog() in master.c is
 * supposed to return an array of files (castles in 2.4.5) to load. The array
 * returned by apply() will be freed at next call of apply(), which means that
 * the ref count has to be incremented to protect against deallocation.
 *
 * The master object is asked to do the actual loading.
 */
void preload_objects(eflag)
    int eflag;
{
    struct vector *prefiles;
    struct svalue *ret;
    static mp_int ix0;
    mp_int ix;
    mp_int num_prefiles;

    push_number(eflag);
    ret = apply_master_ob(STR_EPILOG, 1);

    if ((ret == 0) || (ret->type != T_POINTER))
	return;
    else
	prefiles = ret->u.vec;

    if ((prefiles == 0) || ((num_prefiles = VEC_SIZE(prefiles)) < 1))
	return;

    prefiles->ref++;

    ix0 = -1;
    toplevel_error_recovery_info.type = ERROR_RECOVERY_BACKEND;
    if (setjmp(toplevel_error_recovery_info.con.text)) {
	clear_state();
	add_message("Anomaly in the fabric of world space.\n");
    }

    while ((ix = ++ix0) < num_prefiles) {
	if (prefiles->item[ix].type != T_STRING)
	    continue;

	CLEAR_EVAL_COST;
	push_string_malloced(prefiles->item[ix].u.string);
	(void)apply_master_ob(STR_PRELOAD, 1);

#ifdef MALLOC_malloc
	resort_free_list();
#endif
    }
    free_vector(prefiles);
    toplevel_error_recovery_info.type = ERROR_RECOVERY_NONE;
}

struct svalue *f_debug_message(sp)
    struct svalue *sp;
{
    if (sp->type != T_STRING)
	bad_xefun_arg(1, sp);
    printf("%s", sp->u.string);
    free_svalue(sp);
    return sp - 1;
}

/*
 * catch alarm, set flag for comms code and heart_beat to catch.
 * comms code sets time_to_call_heart_beat for the backend when
 * it has completed the current round of player commands.
 */

ALARM_HANDLER(catch_alarm, comm_time_to_call_heart_beat = 1; total_alarms++;)

/*
 * All destructed objects are moved int a sperate linked list,
 * and deallocated after program execution.
 */
void remove_destructed_objects()
{
    struct object *ob, **obp;
    if (obj_list_replace) {
	extern void replace_programs();
	replace_programs();
    }
    if (new_destructed) for (obp = &obj_list;;) {
	ob = *obp;
	if (ob->flags & O_DESTRUCTED) {
	    *obp = ob->next_all;
	    destruct2(ob);
	    if (!--new_destructed)
		break;
	} else {
	    obp = &ob->next_all;
	}
    }
}

/*
 * Append string to file. Return 0 for failure, otherwise 1.
 */
int write_file(file, str)
    char *file;
    char *str;
{
    FILE *f;

    file = check_valid_path(file, current_object, "write_file", 1);
    if (!file)
	return 0;
    f = fopen(file, "a");
    if (f == 0) {
	if (errno == EMFILE
#ifdef ENFILE
	 || errno == ENFILE
#endif
	) {
	    extern void lex_close PROT((char *));
	    extern void push_apply_value(), pop_apply_value();

	    /* lex_close() calls lexerror(). lexerror() calls yyerror().
	     * yyerror calls smart_log(). smart_log calls apply_master_ob().
	     * This is why the value of file needs to be preserved.
	     */
	    push_apply_value();
	    lex_close(0);
	    pop_apply_value();
	    f = fopen(file, "a");
	}
	if (f == 0) {
	    perror("write_file");
	    error("Wrong permissions for opening file %s for append.\n", file);
	}
    }
    fwrite(str, strlen(str), 1, f);
    fclose(f);
    return 1;
}

#if ( defined( atarist ) && !defined ( minix ) ) || defined( MSDOS )
#define MSDOS_FS
#endif

char *read_file(file,start,len)
    char *file;
    int start,len;
{
    struct stat st;
    FILE *f;
    char *str,*p,*p2,*end,c;
    int size;

    if (len < 0 && len != -1) return 0;
    file = check_valid_path(file, current_object, "read_file", 0);

    if (!file)
	return 0;
#ifdef __STDC__
    /* If the file would be opened in text mode, the size from fstat would
     * not match the number of characters that we can read.
     */
    f = fopen(file, "rb");
#else
    /* there is propably no such thing as a text and binary mode distinction */
    f = fopen(file, "r");
#endif
    if (f == 0)
	return 0;
    if (fstat(fileno(f), &st) == -1)
	fatal("Could not stat an open file.\n");
    size = st.st_size;
    if (size > READ_FILE_MAX_SIZE) {
	if ( start || len ) size = READ_FILE_MAX_SIZE;
	else {
	    fclose(f);
	    return 0;
	}
    }
    if (!start) start = 1;
    if (!len) len = READ_FILE_MAX_SIZE;
    str = xalloc(size + 2);
    if (!str) {
	fclose(f);
	error("Out of memory\n");
    }
    *str++ = ' '; /* this way, we can always read the 'previous' char... */
    str[size] = '\0';
    do {
	if (size > st.st_size)
	    size = st.st_size;
        if (!size && start > 1 || fread(str, size, 1, f) != 1) {
    	    fclose(f);
	    xfree(str-1);
    	    return 0;
        }
	st.st_size -= size;
	end = str+size;
        for (p=str; ( p2=memchr(p,'\n',end-p) ) && --start; ) p=p2+1;
    } while ( start > 1 );
    for (p2=str; p != end; ) {
        c = *p++;
	if ( c == '\n' ) {
#ifdef MSDOS_FS
	    if ( p2[-1] == '\r' ) p2--;
#endif
	    if (!--len) {
		*p2++=c;
	        break;
	    }
	}
	*p2++=c;
    }
    if ( len && st.st_size ) {
	size -= ( p2-str) ; 
	if (size > st.st_size)
	    size = st.st_size;
        if (fread(p2, size, 1, f) != 1) {
    	    fclose(f);
	    xfree(str-1);
    	    return 0;
        }
	st.st_size -= size;
	end = p2+size;
        for (p=p2; p != end; ) {
	    c = *p++;
	    if ( c == '\n' ) {
#ifdef MSDOS_FS
	        if ( p2[-1] == '\r' ) p2--;
#endif
		if (!--len) {
		    *p2++ = c;
		    break;
		}
	    }
	    *p2++ = c;
	}
	if ( st.st_size && len > 0) {
	    /* tried to read more than READ_MAX_FILE_SIZE */
	    fclose(f);
	    xfree(str-1);
	    return 0;
	}
    }
    *p2='\0';
    fclose(f);
#if 0
    if ( st.st_size = (p2-str) )
	return str;
#endif
    p2=string_copy(str);
    xfree(str-1);
    if (!p2)
	error("Out of memory\n");
    return p2;
}


char *read_bytes(file,start,len)
    char *file;
    int start,len;
{
    struct stat st;

    char *str,*p;
    int size, f;

    if (len < 0)
	return 0;
    if(len > MAX_BYTE_TRANSFER)
	return 0;
    file = check_valid_path(file, current_object, "read_bytes", 0);

    if (!file)
	return 0;
    f = ixopen(file, O_RDONLY);
    if (f < 0)
	return 0;

    if (fstat(f, &st) == -1)
	fatal("Could not stat an open file.\n");
    size = st.st_size;
    if(start < 0) 
	start = size + start;

    if (start >= size) {
	close(f);
	return 0;
    }
    if ((start+len) > size) 
	len = (size - start);

    if ((size = lseek(f,start, 0)) < 0) {
	close(f);
	return 0;
    }

    str = xalloc(len + 1);
    if (!str) {
	close(f);
	return 0;
    }

    size = read(f, str, len);

    close(f);

    if (size <= 0) {
	xfree(str);
	return 0;
    }

    /* We want to allow all characters to pass untouched! */
    /*
     * The string has to end to '\0'!!!
     */
    str[size] = '\0';

    p = string_copy(str);
    xfree(str);

    return p;
}

int write_bytes(file,start,str)
    char *file, *str;
    int start;
{
    struct stat st;

    mp_int size, len;
    int f;

    file = check_valid_path(file, current_object, "write_bytes", 1);

    if (!file)
	return 0;
    len = strlen(str);
    if(len > MAX_BYTE_TRANSFER)
	return 0;
    f = ixopen(file, O_WRONLY);
    if (f < 0)
	return 0;

    if (fstat(f, &st) == -1)
	fatal("Could not stat an open file.\n");
    size = st.st_size;
    if(start < 0) 
	start = size + start;

    if (start > size) {
	close(f);
	return 0;
    }
    if ((size = lseek(f,start, 0)) < 0) {
	close(f);
	return 0;
    }

    size = write(f, str, len);

    close(f);

    if (size <= 0) {
	return 0;
    }

    return 1;
}


int file_size(file)
    char *file;
{
    struct stat st;

    file = check_valid_path(file, current_object, "file_size", 0);
    if (!file)
	return -1;
    if (ixstat(file, &st) == -1)
	return -1;
    if (S_IFDIR & st.st_mode)
	return -2;
    return st.st_size;
}

static double load_av = 0.0;

void update_load_av() {
    extern double consts[5];
    extern int current_time;
    static int last_time;
    int n;
    double c;
    static int acc = 0;

    acc++;
    if (current_time == last_time)
	return;
    n = current_time - last_time;
    if (n < sizeof consts / sizeof consts[0])
	c = consts[n];
    else
	c = exp(- n / 900.0);
    load_av = c * load_av + acc * (1 - c) / n;
    last_time = current_time;
    acc = 0;
}

static double compile_av = 0.0;

void update_compile_av(lines)
    int lines;
{
    extern double consts[5];
    extern int current_time;
    static int last_time;
    int n;
    double c;
    static int acc = 0;

    acc += lines;
    if (current_time == last_time)
	return;
    n = current_time - last_time;
    if (n < sizeof consts / sizeof consts[0])
	c = consts[n];
    else
	c = exp(- n / 900.0);
    compile_av = c * compile_av + acc * (1 - c) / n;
    last_time = current_time;
    acc = 0;
}

char *query_load_av() {
    static char buff[100];

    sprintf(buff, "%.2f cmds/s, %.2f comp lines/s", load_av, compile_av);
    return buff;
}

/*
 * Constructs an array of all objects that have a heart_beat.
 */
struct svalue *f_heart_beat_info(sp)
    struct svalue *sp;
{
    int i;
    struct object *ob, **op;
    struct vector *vec;
    struct svalue *v;

    vec = allocate_array(i = num_hb_objs);
    for (v = vec->item, op = hb_list; --i >= 0; v++) {
	v->type = T_OBJECT;
	v->u.ob = ob = *op++;
	add_ref(ob, "heart_beat_info");
    }
    sp++;
    sp->type = T_POINTER;
    sp->u.vec = vec;
    return sp;
}

#ifdef MALLOC_smalloc
void count_heart_beat_refs() {
    if (hb_list)
	note_malloced_block_ref((char *)hb_list);
}
#endif

#include "regexp.h"
#include "smalloc.h"
/* string regreplace(string line,string pat,string replaceby,int flag); */
struct svalue*
f_regreplace(sp)
struct svalue *sp;
{
	struct regexp *pat;
	int	flags;
	char	*oldbuf,*buf,*curr,*new,*start,*old,*sub;
	int	space,origspace;
	extern struct svalue *inter_sp;

	/*
	 * Must set inter_sp before call to regcomp,
	 * because it might call regerror.
	 */
	inter_sp = sp;
	if (sp->type!=T_NUMBER)
		bad_xefun_arg(4, sp);
	/* 
	 * flag 	F_GLOBAL	1
	 * 		F_EXCOMPAT	2
	 */
	flags	= sp->u.number;

	if (sp[-1].type!=T_STRING)
		bad_xefun_arg(3, sp);
	sub 	= sp[-1].u.string;

	if (sp[-2].type!=T_STRING)
		bad_xefun_arg(2, sp);

	if (sp[-3].type!=T_STRING)
		bad_xefun_arg(1, sp);

	start	= curr	= sp[-3].u.string;

	origspace = space = (strlen(start)+1)*2;

/* reallocate on the fly */
#define XREALLOC \
	space  += origspace;\
	origspace = origspace*2;\
	oldbuf	= buf;\
	buf	= (char*)rexalloc(buf,origspace*2);\
	if (!buf) { \
	    xfree(oldbuf); \
	    if (pat) xfree(pat); \
	    error("Out of memory\n"); \
	} \
	new	= buf + (new-oldbuf)


	new	= buf	= (char*)xalloc(space);
	pat	= REGCOMP(sp[-2].u.string,(flags&2)?1:0);
	/* regcomp returns NULL on bad regular expressions. */
	if (pat && regexec(pat,curr,start)) {
		do {
			int	diff = pat->startp[0]-curr;
			space -= diff;
			while (space<0) {
				XREALLOC;
			}
			strncpy(new,curr,diff);
			new	+= diff;
			old	 = new;
			*old='\0';
			/* Now what may have happen here. We *could*
			 * be out of mem (as in 'space') or it could be
			 * a regexp problem. the 'space' problem we
			 * can handle, the regexp problem not
			 * hack: we store a \0 into *old ... if it is
			 * still there on failure, it is a real failure.
			 * if not, increase space. The player could get
			 * some irritating messages from regerror()
			 * ... (should we switch them off?)
			 */
			while (NULL==(new=regsub(pat,sub,new,space,1))) {
				int	i,xold;

				if (!*old) {
					xfree(buf);
					if (pat) xfree(pat);
					error("Out of memory\n");
				}
				xold=old-buf;
				XREALLOC;
				new =buf+xold;
				old =buf+xold;
				*old='\0';
			}
			space -= new-old;
			while (space<0) {
				XREALLOC;
			}
			if (curr == pat->endp[0]) {
				/* prevent infinite loop 
				 * by advancing one character.
				 */
				if (!*curr) break;
				--space;
				while (space<0) {
					XREALLOC;
				}
				*new++ = *curr++;
			} else
				curr = pat->endp[0];
		} while ((flags&1) && !pat->reganch && regexec(pat,curr,start));
		space -= strlen(curr)+1;
		if (space<0) {
			XREALLOC;
		}
		strcpy(new,curr);
	} else {
		strcpy(buf,start);
	}
	if (pat) REGFREE(pat);
	free_svalue(sp);
	sp--;
	free_svalue(sp);
	sp--;
	free_svalue(sp);
	sp--;
	free_svalue(sp);
	sp->type = T_STRING;
	sp->x.string_type = STRING_MALLOC;
	sp->u.string = string_copy(buf);
	xfree(buf);
	return sp;
}
