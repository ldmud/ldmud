/*---------------------------------------------------------------------------
 * Gamedriver Backend.
 *
 *---------------------------------------------------------------------------
 * This module holds the backend loop, which polls the connected sockets
 * for new input, passes the text read to the command parser (and thus
 * executes the commands), and keeps track of regular house-keeping chores
 * like garbage collection, swapping, heart beats and the triggers of
 * call outs and mapping compaction.
 *
 * The error recovery stack is placed here since on the top level any
 * uncaught error will reset the virtual machine and restart the backend
 * loop.
 *
 * Timing is implemented using a one-shot 2 second alarm(). When the
 * alarm is triggered, the handler sets the variable comm_time_to_call_heart-
 * _beat which is monitored by various functions.
 *
 * One backend cycle begins with starting the alarm(). Then the pending
 * heartbeats are evaluated, but no longer until the time runs out. Any
 * heartbeat remaining will be evaluated in the next cycle. After the
 * heartbeat, the call_outs are evaluated. Callouts have no time limit,
 * but are bound by the eval_cost limit. Next, the object list is scanned
 * for objects in need of a reset, cleanup or swap. The driver will
 * (due objects given) perform at least one of each operation, but only
 * as many as it can before the time runs out. Last, player commands are
 * retrieved with get_message(). The semantic is so that all players are
 * considered once before get_message() checks for a timeout. If a timeout
 * is detected, get_message() select()s, but returns immediately with the
 * variable time_to_call_heart_beat set, else it selects() in one second
 * intervals until either commands come in or the time runs out.
 *
 * Also in this file (slighly misplaced) are the file handling efuns,
 * regreplace.
 *
 * TODO: Move the file efuns into separate files.
 *---------------------------------------------------------------------------
 */

#include "driver.h"

#include <stddef.h>
#include <ctype.h>
#include <limits.h>
#include <stdio.h>
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

#define NO_INCREMENT_STRING_REF
#include "backend.h"
#include "array.h"
#include "call_out.h"
#include "comm.h"
#include "ed.h"
#include "exec.h"
#include "filestat.h"
#include "gcollect.h"
#include "heartbeat.h"
#include "interpret.h"
#include "lex.h"
#include "main.h"
#include "mapping.h"
#include "my-alloca.h"
#include "object.h"
#include "otable.h"
#include "random.h"
#include "regexp.h"
#include "rxcache.h"
#include "simulate.h"
#include "smalloc.h"
#include "stralloc.h"
#include "swap.h"
#include "wiz_list.h"

/*-------------------------------------------------------------------------*/

#define ALARM_TIME  2  /* The granularity of alarm() calls */

/*-------------------------------------------------------------------------*/

/* The error recovery stack.
 *
 * error_recover_infos are maintained in a linked list, with
 * error_recover_pointer pointing to the most recently pushed context.
 * From there, the links go back through the less recently pushed contexts
 * and end with the toplevel_error_recovery_info.
 * TODO: Move this stack into interpret.c?
 */

struct error_recovery_info toplevel_error_recovery_info
 = {
    (struct error_recovery_info*) NULL,
    ERROR_RECOVERY_NONE
 };

struct error_recovery_info *error_recovery_pointer
 = &toplevel_error_recovery_info;

/*-------------------------------------------------------------------------*/
mp_int current_time;
  /* The current time, updated every heart beat.
   * TODO: Should be time_t if it is given that time_t is always a skalar.
   */

Bool time_to_call_heart_beat;
  /* True: It's time to call the heart beat. Set by comm1.c when it recognizes
   *   an alarm(). */

volatile Bool comm_time_to_call_heart_beat = MY_FALSE;
  /* True: An heart beat alarm() happened. Set from the alarm handler, this
   *   causes comm.c to set time_to_call_heart_beat.
   */

volatile mp_int total_alarms = 0;
  /* The total number of alarm()s so far, incremented from the alarm handler.
   */

uint32 total_player_commands = 0;
  /* Total number of player commands so far.
   */

/* TODO: *eval_cost and -time belong into interpret.c */
int32 initial_eval_cost = -MAX_COST;
  /* The max eval cost available for one execution thread. Stored as negative
   * value for easier initialisation (see eval_cost).
   * CLEAR_EVAL_COST uses this value to re-initialize (assigned_)eval_cost.
   */

int32 eval_cost;
  /* The amount of eval cost left for the current execution thread, stored
   * as negative value. This way costs can be counted up and the exhaustion
   * test is a simple test for >= 0.
   */

int32 assigned_eval_cost;
  /* Auxiliary variable used to account eval costs to single objects and
   * their user's wizlist entry.
   * Whenver the execution thread enters a different object,
   * assigned_eval_cost is set to the current value of eval_cost. When the
   * thread leaves the object again, the difference between the actual
   * eval_cost value and the older assigned_eval_cost is accounted to
   * the current object.
   * (The implementation combines both actions in one function
   * 'assign_eval_cost()' in interpret.c).
   */

#ifndef OLD_RESET

uint num_listed_objs = 0;
  /* Number of objects in the object list.
   */

uint num_last_processed = 0;
  /* Number of object processed in last process_objects().
   */

long avg_last_processed = 0;
long avg_in_list = 0;
  /* Decaying average number of objects processed and objects in the list.
   */

#endif

Bool extra_jobs_to_do = MY_FALSE;
  /* True: the backend has other things to do in this cycle than just
   *   parsing commands or calling the heart_beat.
   */

Bool garbage_collect_to_do = MY_FALSE;
  /* True: A garbage collection is due (requires extra_jobs_to_do).
   */

/* TODO: all the 'extra jobs to do' should be collected here, in a nice
 * TODO:: struct.
 */

static double load_av = 0.0;
  /* The load average (player commands/second), weighted over the
   * last period of time.
   */

static double compile_av = 0.0;
  /* The average of compiled lines/second, weighted over the last period
   * of time.
   */

static struct svalue *old_hooks = NULL;
  /* Array of entries holding all the old driver hook closures replaced 
   * during this and the previous execution threads. The closures are 
   * not freed immediately on replacement in case they are still used. 
   * Instead, the backend frees them explicitely.
   */

static int num_old_hooks = 0;
  /* The current number of entries in <old_hooks>
   */

static int max_old_hooks = 0;
  /* The allocated length of <old_hooks>
   */

/*-------------------------------------------------------------------------*/

/* --- Forward declarations --- */

#ifndef OLD_RESET
static void process_objects(void);
#else
static void look_for_objects_to_swap(void);
#endif
static void update_load_av (void);

/*-------------------------------------------------------------------------*/
void
clear_state (void)

/* Clear the global variables of the virtual machine. This is necessary
 * after errors, which return directly to the backend, thus skipping the
 * clean-up code in the VM itself.
 *
 * This routine must only be called from top level, not from inside
 * stack machine execution (as the stack will be cleared).
 * TODO: This too belongs into interpret.c
 */

{
    current_file = NULL;
    current_object = NULL;
    command_giver = NULL;
    current_interactive = NULL;
    previous_ob = NULL;
    current_prog = NULL;
    reset_machine(0);        /* Pop down the stack. */
}

/*-------------------------------------------------------------------------*/
void
logon (struct object *ob)

/* Call the logon() lfun in the object <ob>.
 *
 * current_object is temporarily set to <ob> in order to allow logon()
 * to be static (security measure). Doing so is harmless as there is no
 * previous_object to consider.
 *
 * TODO: This should go into simulate.c or comm.c
 */

{
    struct svalue *ret;
    struct object *save = current_object;

    current_object = ob;
    ret = apply(STR_LOGON, ob, 0);
    if (ret == 0) {
        add_message("prog %s:\n", ob->name);
        error("Could not find logon() on the player %s\n", ob->name);
    }
    current_object = save;
}

/*-------------------------------------------------------------------------*/
int
parse_command (char *str, struct object *ob)

/* Parse and execute the command <str> for object <ob> as command_giver.
 * <ob> may be an interactive player or a NPC.
 * Return the result from the player_parser (TODO: What is it?)
 *
 * Note that the buffer of str may be modified and/or extended by this
 * call.
 *
 * TODO: Move this function into simulate.c
 * TODO: Document the command flow.
 */

{
    struct object *save = command_giver;
    int res;

    command_giver = ob;
    res = player_parser(str);
    command_giver = check_object(save);
    return res;
}

/*-------------------------------------------------------------------------*/
#ifdef AMIGA

static void
exit_alarm_timer (void)

/* Clean up the alarm timer on program exit.
 * This is set as atexit() function.
 */

{
    alarm(0);
}

#endif

/*-------------------------------------------------------------------------*/
void 
free_closure_hooks (struct svalue *svp, int count)

/* "Free" the <count> closures in <svp>[], ie. store them for later
 * deletion by the backend.
 * TODO: Can this be done better by introducing a refcount in the
 * TODO:: closures?
 */

{
    struct svalue *new;
    
    if (max_old_hooks < num_old_hooks + count)
    {
        int delta;

        delta = (count > NUM_CLOSURE_HOOKS) ? count : NUM_CLOSURE_HOOKS;
        
        if (old_hooks)
            new = rexalloc(old_hooks
                          , (max_old_hooks + delta) * sizeof(*new));
        else
            new = xalloc(delta * sizeof(*new));
        if (!new)
            return;
        old_hooks = new;
        max_old_hooks += delta;
    }
    memcpy(old_hooks + num_old_hooks, svp, count * sizeof(*svp));
    num_old_hooks += count;
}

/*-------------------------------------------------------------------------*/
void
free_old_driver_hooks (void)

/* Free all closures queued in <old_hooks>, and the <old_hooks> array itself.
 * This function is called from the backend and from the garbage collector.
 */

{
    int i;
    
    if (!old_hooks)
        return;

    for (i = num_old_hooks; i--;) 
    {
        if (old_hooks[i].type == T_CLOSURE
         && old_hooks[i].x.closure_type == CLOSURE_LAMBDA)
        {
            old_hooks[i].x.closure_type = CLOSURE_UNBOUND_LAMBDA;
        }
        free_svalue(&old_hooks[i]);
    }

    xfree(old_hooks);
    old_hooks = NULL;
    num_old_hooks = max_old_hooks = 0;
}

/*-------------------------------------------------------------------------*/
void
backend (void)

/* The backend loop, the backbone of the driver's operations.
 * It never returns (at least not the normal way).
 */

{
    char buff[MAX_TEXT+4];


    /*
     * Set up.
     */

    (void)printf("Setting up ipc.\n");
    fflush(stdout);

    prepare_ipc();

    (void)signal(SIGHUP,  (RETSIGTYPE(*)PROT((int)))f_shutdown);
    (void)signal(SIGUSR1, (RETSIGTYPE(*)PROT((int)))startmasterupdate);
    if (!t_flag) {
        ALARM_HANDLER_FIRST_CALL(catch_alarm);
        current_time = get_current_time();
#ifndef OLD_RESET
        /* Start the first alarm */
        comm_time_to_call_heart_beat = MY_FALSE;
        time_to_call_heart_beat = MY_FALSE;
        alarm(ALARM_TIME);
#else
        call_heart_beat();
           /* This also starts the ongoing alarm() chain */
#endif
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

    /*
     * The Loop.
     */
    while(1)
    {
#if 0 && defined(DEBUG)
        /* The call to check_state()/clear_state() should not be done
         * in every loop as it is quite costly. However, it is helpful
         * when debugging the driver.
         */
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

        /* Execute pending deallocations */
#ifdef C_ALLOCA
        alloca(0); /* free alloca'd values from deeper levels of nesting */
#endif

        /* Replace programs */
        if (obj_list_replace)
        {
            replace_programs();
        }

        /* Free older driver hooks
         */
        free_old_driver_hooks();

        /* Remove all destructed objects.
         */
        remove_destructed_objects();

#ifdef DEBUG
        if (check_a_lot_ref_counts_flag)
            check_a_lot_ref_counts(0); /* after removing destructed objects! */
#endif

        /*
         * Do the extra jobs, if any.
         */

        if (extra_jobs_to_do) {

            current_interactive = 0;
            if (game_is_being_shut_down) {
                command_giver = NULL;
                current_object = NULL;
                shutdowngame();
            }

            if (master_will_be_updated) {
                emergency_destruct(master_ob);
                master_will_be_updated = MY_FALSE;
                command_giver = NULL;
                current_object = &dummy_current_object_for_loads;
                apply_master_ob(STR_EXT_RELOAD, 0);
                current_object = NULL;
            }

            if (garbage_collect_to_do) {
                time_t time_now = time(NULL);
                char buf[90];

                if (time_now - time_last_gc >= 300)
                {
                  sprintf(buf, "Garbage collection, slow_shut: %d\n", slow_shut_down_to_do);
                  write(1, buf, strlen(buf));
                  command_giver = NULL;
                  current_object = NULL;
                  garbage_collection();
                }
                else
                {
                  sprintf(buf, "No garbage collection, slow_shut: %d\n", slow_shut_down_to_do);
                  write(1, buf, strlen(buf));
                  reallocate_reserved_areas();
                }

                garbage_collect_to_do = MY_FALSE;

                if (slow_shut_down_to_do) {
                    int tmp = slow_shut_down_to_do;
                    slow_shut_down_to_do = 0;
                    malloc_privilege = MALLOC_MASTER;
                    slow_shut_down(tmp);
                }
                malloc_privilege = MALLOC_USER;
            }

            extra_jobs_to_do = MY_FALSE;

            if (num_dirty_mappings) {
                compact_mappings((num_dirty_mappings+80) >> 5);
                malloc_privilege = MALLOC_USER;
            }
        } /* if (extra_jobs_to_do */

        /*
         * Call comm.c and wait for player input, or until the next
         * heart beat is due.
         */

        if (get_message(buff))
        {
            /* Note that the size of buff[] is determined by MAX_TEXT, which
             * is the max size of the network receive buffer. Iow: no
             * buffer overruns possible.
             */

            total_player_commands++;
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
            current_object = NULL;
            current_interactive = command_giver;

#ifdef DEBUG
            if (!O_GET_INTERACTIVE(command_giver)
             ||  O_GET_INTERACTIVE(command_giver)->sent.type != SENT_INTERACTIVE)
            {
                fatal("Non interactive player in main loop !\n");
                /* NOTREACHED */
            }
#endif
            tracedepth = 0;

            if (buff[0] == '!'
             && buff[1] != '\0'
             && command_giver->super
             && !(O_GET_INTERACTIVE(command_giver)->noecho & IGNORE_BANG))
            {
                if (O_GET_INTERACTIVE(command_giver)->noecho & NOECHO) {
                    add_message("%s\n",
                          buff + O_GET_INTERACTIVE(command_giver)->chars_ready);
                    O_GET_INTERACTIVE(command_giver)->chars_ready = 0;
                }
                parse_command(buff+1, command_giver);
            }
            else if (O_GET_INTERACTIVE(command_giver)->sent.ed_buffer)
                ed_cmd(buff);
            else if (
              call_function_interactive(O_GET_INTERACTIVE(command_giver),buff))
                NOOP;
            else
                parse_command(buff, command_giver);

            /*
             * Print a prompt if player is still here.
             */
            if (command_giver)
            {
                struct interactive *ip;

                if (NULL != (ip = O_GET_INTERACTIVE(command_giver))
                 && ip->sent.type == SENT_INTERACTIVE
                 && !ip->do_close)
                {
                    print_prompt();
                }
            }
        } /* if (get_message()) */

        /* Do the periodic functions if it's time.
         */
        if (time_to_call_heart_beat)
        {
            struct object *hide_current = current_object;
              /* TODO: Is there any point to this? */

            current_time = get_current_time();

#ifndef OLD_RESET
            current_object = NULL;

            /* Start the next alarm */
            comm_time_to_call_heart_beat = MY_FALSE;
            time_to_call_heart_beat = MY_FALSE;
            alarm(ALARM_TIME);

            /* Do the timed events */
            call_heart_beat();
            call_out();

            /* Reset/cleanup/swap objects.
             * TODO: Reset and cleanup/swap should be separated.
             */
            process_objects();
#else
            call_heart_beat();
            current_object = NULL;
            look_for_objects_to_swap();
              /* TODO: Swap after call_out(), so re-loading objects can be avoided? */
            call_out();
#endif

            command_giver = NULL;
            trace_level = 0;
            current_object = hide_current;
            wiz_decay();
        }

    } /* end of main loop */

    /* NOTREACHED */
}

/*-------------------------------------------------------------------------*/

/*
 * catch alarm()
 *
 * Set flag for comms code and heart_beat to catch.
 * comms code then sets time_to_call_heart_beat for the backend when
 * it has completed the current round of player commands.
 */

#ifdef ALARM_HANDLER

ALARM_HANDLER(catch_alarm, comm_time_to_call_heart_beat = 1; total_alarms++; )

#else

void catch_alarm (int dummy UNUSED)
{
#ifdef __MWERKS__
#    pragma unused(dummy)
#endif
    (void)signal(SIGALRM, (RETSIGTYPE(*)(int))catch_alarm);
    comm_time_to_call_heart_beat = 1;
    total_alarms++;
}

#endif

/*-------------------------------------------------------------------------*/
#ifndef OLD_RESET

static void
process_objects (void)

/* This function performs almost all of the less important tasks in the
 * mud: resetting, cleaning and swapping objects.
 *
 * It is called from the backend loop in every new cycle before
 * player commands are retrieved, but after heartbeats and callouts
 * have been evaluated. It tries to process as many objects as possible
 * before the current timeslot runs out (as registered by comm_time_to-
 * _call_heart_beat), but will do at least one cleanup/swap and reset.
 *
 * Objects are moved to the end of the list before they are processed,
 * so that a the next object to handle is always the first object
 * in the list. Even arbitrary object destruction doesn't change this.
 *
 * The functions in detail:
 *
 *  - An object will be reset if it is found in a state of not being reset
 *    and the delay to the next reset has passed.
 *
 *  - An object's clean_up() lfun will be called if the object has not
 *    been used for quite some time, and if it was not reset in this
 *    very round.
 *
 *  - An object's program and/or its variables will be swapped if it exists
 *    for at least time_to_swap(_variables) seconds since the last reference.
 *    Since swapping of variables is costly, care is taken that variables
 *    are not swapped out right before the next reset which in that case
 *    would cause a swap in/swap out-yoyo. Instead, such variable swapping
 *    is delayed until after the reset occured.
 *
 *    To disable swapping, set the swapping times (either in config.h or per
 *    commandline option) to a value <= 0.
 *
 * The function maintains its own error recovery info so that errors
 * in reset() or clean_up() won't mess up the handling.
 *
 * TODO: The objlist should be split into resets and cleanup/swaps, which
 * TODO:: are then sorted by due times. This would make this function
 * TODO:: much more efficient.
 * TODO: It might be a good idea to distinguish between the time_of_ref
 * TODO:: (when the object was last used/called) and the time_of_swap,
 * TODO:: when it was last swapped in or out. Then, maybe not.
 */

{
static Bool did_reset;
static Bool did_swap;
  /* True if one reset call or cleanup/swap was performed.
   * static so that errors won't clobber it.
   */

    struct object *obj;   /* Current object worked on */

    struct error_recovery_info error_recovery_info;
      /* Local error recovery info */

    /* Housekeeping */

    num_last_processed = 0;
    did_reset = MY_FALSE;
    did_swap = MY_FALSE;

    error_recovery_info.last = error_recovery_pointer;
    error_recovery_info.type = ERROR_RECOVERY_BACKEND;
    error_recovery_pointer = &error_recovery_info;

    if (setjmp(error_recovery_info.con.text))
    {
        clear_state();
        debug_message("Error in process_objects().\n");
    }

    /* The processing loop, runs until either time or objects
     * run short.
     *
     * To be safe against errors and destruction of objects,
     * the object processed is moved immediately to the end
     * of the object list, so that obj_list always points to
     * the next object to process.
     *
     * The loop ends (apart from timeouts) when num_last_processed
     * exceeds num_listed_objs.
     */
    while (NULL != (obj = obj_list)
        && (!did_reset || !did_swap || !comm_time_to_call_heart_beat)
        && num_last_processed < num_listed_objs
          )
    {
        mp_int time_since_ref; /* Time since last reference */
        Bool bResetCalled;  /* TRUE: reset() called */

        num_last_processed++;

        /* Move obj to the end of the list */
        if (obj != obj_list_end)
        {
            obj_list = obj->next_all;
            obj_list->prev_all = NULL;
            obj->next_all = NULL;
            obj->prev_all = obj_list_end;
            obj_list_end->next_all = obj;
            obj_list_end = obj;
        }

        /* Set Variables */
        time_since_ref = current_time - obj->time_of_ref;
        bResetCalled = MY_FALSE;

        /* ------ Reset ------ */

        /* Check if a reset() is due. Objects which have not been touched
         * since the last reset just get a new due time set.
         * It is tempting to skip the reset handling for objects which
         * are swapped out, but then swapper would have to call reset_object()
         * for due objects on swap-in (just setting a new due-time is not
         * sufficient).
         * TODO: Do exactly that?
         */

        if (obj->time_reset
         && obj->time_reset < current_time)
        {
            if (obj->flags & O_RESET_STATE)
            {
#ifdef DEBUG
                if (d_flag)
                    fprintf(stderr, "RESET (virtual) %s\n", obj->name);
#endif
                obj->time_reset = current_time+TIME_TO_RESET/2
                                             +random_number(TIME_TO_RESET/2);
            }
            else if (!did_reset || !comm_time_to_call_heart_beat)
            {
#ifdef DEBUG
                if (d_flag)
                    fprintf(stderr, "RESET %s\n", obj->name);
#endif
                if (obj->flags & O_SWAPPED
                 && load_ob_from_swap(obj) < 0)
                   continue;
                bResetCalled = MY_TRUE;
                did_reset = MY_TRUE;
                CLEAR_EVAL_COST;
                command_giver = 0;
                trace_level = 0;
                reset_object(obj, H_RESET);
                if (obj->flags & O_DESTRUCTED)
                    continue;

#if TIME_TO_SWAP > 0 || TIME_TO_SWAP_VARIABLES > 0 || TIME_TO_CLEAN_UP > 0
                /* Restore old time_of_ref. This might result in a quick
                 * swap-in/swap-out yoyo if this object was swapped out
                 * in the first place. To make this less costly, variables
                 * are not swapped out short before a reset (see below).
                 */
                obj->time_of_ref = current_time - time_since_ref;
#endif
            } /* if (call reset or not) */
        } /* if (needs reset?) */

#if TIME_TO_CLEAN_UP > 0

        /* ------ Clean Up ------ */

        /* If enough time has passed, give the object a chance to self-
         * destruct. The O_RESET_STATE is saved over the call to clean_up().
         *
         * Only call clean_up in objects that have defined such a function.
         * Only if the clean_up returns a non-zero value, it will be called
         * again.
         *
         * The cleanup is not called if the object was actively reset
         * just before.
         */
        if (obj->flags & O_WILL_CLEAN_UP
          && time_since_ref > TIME_TO_CLEAN_UP
          && !bResetCalled
          && (!did_swap || !comm_time_to_call_heart_beat))
        {
            int was_swapped = obj->flags & O_SWAPPED ;
            int save_reset_state = obj->flags & O_RESET_STATE;
            struct svalue *svp;

#ifdef DEBUG
            if (d_flag)
                fprintf(stderr, "clean up %s\n", obj->name);
#endif

            did_swap = MY_TRUE;

            /* Supply a flag to the object that says if this program
             * is inherited by other objects. Cloned objects might as well
             * believe they are not inherited. Swapped objects will not
             * have a ref count > 1 (and will have an invalid ob->prog
             * pointer).
             */
            push_number(obj->flags & (O_CLONE|O_REPLACED) ? 0 :
              ( O_PROG_SWAPPED(obj) ? 1 : obj->prog->ref) );
            CLEAR_EVAL_COST;
            command_giver = NULL;
            trace_level = 0;
            if (closure_hook[H_CLEAN_UP].type == T_CLOSURE)
            {
                struct lambda *l;

                l = closure_hook[H_CLEAN_UP].u.lambda;
                if (closure_hook[H_CLEAN_UP].x.closure_type == CLOSURE_LAMBDA)
                    l->ob = obj;
                push_object(obj);
                call_lambda(&closure_hook[H_CLEAN_UP], 2);
                svp = inter_sp;
                pop_stack();
            }
            else if (closure_hook[H_CLEAN_UP].type == T_STRING)
            {
                svp = apply(closure_hook[H_CLEAN_UP].u.string, obj, 1);
            }
            else
            {
                pop_stack();
                goto no_clean_up;
            }
            if (obj->flags & O_DESTRUCTED)
                continue;

            if ((!svp || (svp->type == T_NUMBER && svp->u.number == 0)) &&
                was_swapped )
                obj->flags &= ~O_WILL_CLEAN_UP;
            obj->flags |= save_reset_state;

no_clean_up:
            obj->time_of_ref = current_time;
                  /* in case the hook didn't update it */
        }

#endif /* TIME_TO_CLEAN_UP > 0 */

#if TIME_TO_SWAP > 0 || TIME_TO_SWAP_VARIABLES > 0

        /* ------ Swapping ------ */

        /* At last, there is a possibility that the object can be swapped
         * out.
         *
         * Variables are swapped after time_to_swap_variables has elapsed
         * since the last ref, and if the object is either still reset or
         * the next reset is at least time_to_swap_variables/2 in the
         * future. When a reset is due, this second condition delays the
         * costly variable swapping until after the reset.
         *
         * Programs are swapped after time_to_swap has elapsed, and if
         * they have only one reference, ie are not cloned or inherited.
         * Since program swapping is relatively cheap, no care is
         * taken of resets.
         */

        if ( (time_to_swap > 0 || time_to_swap_variables > 0)
         && !(obj->flags & O_HEART_BEAT)
         && (!did_swap || !comm_time_to_call_heart_beat))
        {
            /* Swap the variables, if possible */
            if (!O_VAR_SWAPPED(obj)
             && time_since_ref >= time_to_swap_variables
             && time_to_swap_variables > 0
             && obj->variables
             && ( obj->flags & O_RESET_STATE
               || !obj->time_reset
               || (obj->time_reset - current_time > time_to_swap_variables/2)))
            {
#ifdef DEBUG
                if (d_flag)
                   fprintf(stderr, "swap vars of %s\n", obj->name);
#endif
                swap_variables(obj);
                if (O_VAR_SWAPPED(obj))
                    did_swap = MY_TRUE;
            }

            /* Swap the program, if possible */
            if (!O_PROG_SWAPPED(obj)
             && obj->prog->ref == 1
             && time_since_ref >= time_to_swap
             && time_to_swap > 0)
            {
#ifdef DEBUG
                if (d_flag)
                    fprintf(stderr, "swap %s\n", obj->name);
#endif
                swap_program(obj);
                if (O_PROG_SWAPPED(obj))
                    did_swap = MY_TRUE;
            }
        } /* if (obj can be swapped) */
#endif

        /* TODO: Here would be nice place to convert all strings in an
         * TODO:: object to shared strings, if the object was reset, cleant
         * TODO:: up, or not used for some time. Also, all the destructed
         * TODO:: object references in the object could be cleared.
         * TODO:: The function (in simulate) could have the signature:
         * TODO:: void clean_vars(object *ob, pointer_table *ptable).
         */

    } /* End of loop */

    /* Update the processing averages
     */
    avg_last_processed += num_last_processed - (avg_last_processed >> 10);
    avg_in_list += num_listed_objs - (avg_in_list >> 10);

    /* Restore the error recovery context */
    error_recovery_pointer = error_recovery_info.last;
}

#endif /* !OLD_RESET */

/*-------------------------------------------------------------------------*/
#ifdef OLD_RESET

static void
look_for_objects_to_swap(void)

/* Scan through all objects and test if they need to be reset and/or
 * swapped. The scan will take place every RESET_GRANULARITY or TIME_TO_SWAP
 * seconds, whatever is smaller.
 *
 * An object will be reset if it is found in a state of not being reset,
 * and the delay to the next reset has passed.
 *
 * An object's program and/or its variables will be swapped if it exists
 * for at least time_to_swap(_variables) seconds since the last reference.
 * Before the swapping, the lfun clean_up() will be called to give the
 * object a chance for selfdestruction.
 *
 * To disable swapping, set the swapping times (either in config.h or per
 * commandline option) to a value <= 0.
 *
 * The function maintains its own error recovery info so that errors
 * in reset() or clean_up() won't mess up the handling.
 */

{
static int next_time;  /* Next time a swap/reset is due */

static struct object *next_ob;
  /* Next object to test for swap/reset. It is static so that it won't
   * be clobbered by errors.
   */

    struct object *ob;  /* Current object tested */
    struct error_recovery_info error_recovery_info;
      /* Local error recovery info */

    /* Is it time? */
    if (current_time < next_time)
        return;

    /* Compute the 'next time'
     */
    if ( time_to_swap > 0
     && time_to_swap < RESET_GRANULARITY
     && (time_to_swap_variables <= 0 || time_to_swap_variables > time_to_swap)
       )

        next_time = current_time + time_to_swap;

    else if (time_to_swap_variables > 0
     && time_to_swap_variables < RESET_GRANULARITY
       )

        next_time = current_time + time_to_swap_variables;

    else
        next_time = current_time + RESET_GRANULARITY;

    /*
     * Objects object can be destructed, which means that
     * next object to investigate is saved in next_ob. If very unlucky,
     * that object can be destructed too. In that case, the loop is simply
     * restarted.
     */
    next_ob = obj_list;
    error_recovery_info.last = error_recovery_pointer;
    error_recovery_info.type = ERROR_RECOVERY_BACKEND;
    error_recovery_pointer = &error_recovery_info;

    if (setjmp(error_recovery_info.con.text)) {
        clear_state();
        debug_message("Error in look_for_objects_to_swap.\n");
    }

    for (; NULL != (ob = next_ob); ) {
        int time_since_ref;

        /* If this happens, the 'next_ob' was destructed in the
         * previous iteration. This means that we lost track where
         * we are in the object list and have to restart.
         */
        if (ob->flags & O_DESTRUCTED)
        {
            next_ob = obj_list;
            continue;
        }

        next_ob = ob->next_all;

        /* Time since last reference */
        time_since_ref = current_time - ob->time_of_ref;

        /* Check if a reset() is due. Objects which have not been touched
         * since the last reset just get a new due time set. Objects which
         * are not swapped in are not reset (the swapper will assign a new
         * due-time on swap-in).
         */
        if (ob->next_reset < current_time)
        {
            if (ob->flags & O_RESET_STATE)
                ob->next_reset = current_time+TIME_TO_RESET/2
                                             +random_number(TIME_TO_RESET/2);
            else
            {
                if (d_flag)
                    fprintf(stderr, "RESET %s\n", ob->name);
                if (ob->flags & O_SWAPPED && load_ob_from_swap(ob) < 0)
                    continue;
                CLEAR_EVAL_COST;
                command_giver = 0;
                trace_level = 0;
                reset_object(ob, H_RESET);
                if (ob->flags & O_DESTRUCTED)
                    continue;
            }
        }

#if TIME_TO_CLEAN_UP > 0
        /* If enough time has passed, give the object a chance to self-
         * destruct. The O_RESET_STATE is saved over the call to clean_up().
         *
         * Only call clean_up in objects that have defined such a function.
         * Only if the clean_up returns a non-zero value, it will be called
         * again.
         */
        else if (time_since_ref > TIME_TO_CLEAN_UP
              && (ob->flags & O_WILL_CLEAN_UP))
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
            push_number(ob->flags & (O_CLONE|O_REPLACED) ? 0 :
              ( O_PROG_SWAPPED(ob) ? 1 : ob->prog->ref) );
            CLEAR_EVAL_COST;
            command_giver = NULL;
            trace_level = 0;
            if (closure_hook[H_CLEAN_UP].type == T_CLOSURE) {
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
        if (time_since_ref >= time_to_swap_variables && time_to_swap_variables > 0) {
            if (!O_VAR_SWAPPED(ob)) {
                if (d_flag)
                    fprintf(stderr, "swap vars of %s\n", ob->name);
                swap_variables(ob);
            }
        }
        if (time_since_ref >= time_to_swap && time_to_swap_variables > 0) {
            if (!O_PROG_SWAPPED(ob)) {
                if (d_flag)
                    fprintf(stderr, "swap %s\n", ob->name);
                swap_program(ob);
            }
        }
#endif
    }

    /* Restore the error recovery context */
    error_recovery_pointer = error_recovery_info.last;
}

#endif /* OLD_RESET */

/*-------------------------------------------------------------------------*/
void
preload_objects (int eflag)

/* Perform some initialisation of the mudlib before the sockets are opened
 * for users. Traditionally, back in 2.4.5 times, this was used to preload
 * the wizard castles, but you can do everything else here.
 *
 * <eflag> is the number of times the '-e' ('--no-preload') option was
 * given on the command line. Traditionally, a non-zero value disabled
 * any preloading.
 *
 * The preloading is a two-step process. First, the lfun epilog() in the
 * master object is called with the value of <eflag> as argument. The
 * result is expected to be an array of strings (filenames). These strings
 * (or at least those array elements which are strings) are then fed as
 * argument to the master object lfun preload().
 */

{
    struct vector *prefiles;
    struct svalue *ret;
    static mp_int ix0;
    static mp_int num_prefiles;
    mp_int ix;

    /* Call master->epilog(<eflag>)
     */
    push_number(eflag);
    ret = apply_master_ob(STR_EPILOG, 1);

    if ((ret == 0) || (ret->type != T_POINTER))
        return;
    else
        prefiles = ret->u.vec;

    if ((prefiles == 0) || ((num_prefiles = VEC_SIZE(prefiles)) < 1))
        return;

    prefiles->ref++;
      /* Without this, the next apply call would free the array prefiles.
       */

    ix0 = -1;

    /* In case of errors, return here and simply continue with
     * the loop.
     */
    toplevel_error_recovery_info.type = ERROR_RECOVERY_BACKEND;
    if (setjmp(toplevel_error_recovery_info.con.text)) {
        clear_state();
        add_message("Anomaly in the fabric of world space.\n");
    }

    /* Loop through the prefiles array, calling master->preload()
     * for every string in it.
     */
    while ((ix = ++ix0) < num_prefiles) {
        if (prefiles->item[ix].type != T_STRING)
            continue;

        CLEAR_EVAL_COST;
        push_string_malloced(prefiles->item[ix].u.string);
        (void)apply_master_ob(STR_PRELOAD, 1);

    }

    free_vector(prefiles);
    toplevel_error_recovery_info.type = ERROR_RECOVERY_NONE;
}

/*-------------------------------------------------------------------------*/
static void
update_load_av (void)

/* Compute the load average (player commands/second), weighted over the
 * last period of time.
 *
 * The function is called after every player command and basically counts
 * how many times it is called in one backend loop.
 */

{
static int last_time;  /* Time of last backend loop */
static int acc = 0;    /* Number of calls in this backend loop */

    int n;
    double c;

    acc++;
    if (current_time == last_time)
        return;
    n = current_time - last_time;
    if (n < (int) (sizeof consts / sizeof consts[0]) )
        c = consts[n];
    else
        c = exp(- n / 900.0);
    load_av = c * load_av + acc * (1 - c) / n;
    last_time = current_time;
    acc = 0;
}

/*-------------------------------------------------------------------------*/
void
update_compile_av (int lines)

/* Compute the average of compiled lines/second, weighted over the
 * last period of time.
 *
 * The function is called after every compilation and basically sums up
 * the number of compiled lines in one backend loop.
 */

{
static int last_time;  /* Time of the last backend loop */
static int acc = 0;    /* Sum of lines for this backend loop */

    int n;
    double c;

    acc += lines;
    if (current_time == last_time)
        return;
    n = current_time - last_time;
    if (n < (int) (sizeof consts / sizeof consts[0]) )
        c = consts[n];
    else
        c = exp(- n / 900.0);
    compile_av = c * compile_av + acc * (1 - c) / n;
    last_time = current_time;
    acc = 0;
}

/*=========================================================================*/

/*                               EFUNS                                     */

/*-------------------------------------------------------------------------*/
char *
query_load_av (void)

/* EFUN query_load_average()
 *
 * Return a string with the current load_av and compile_av.
 * The string returned points to a local static buffer.
 */

{
static char buff[100];

    sprintf(buff, "%.2f cmds/s, %.2f comp lines/s", load_av, compile_av);
    return buff;
}

/*-------------------------------------------------------------------------*/
struct svalue *
f_debug_message (struct svalue *sp)

/* TEFUN debug_message()
 *
 *   debug_message(string text)
 *
 * Print the <text> to stdout.
 */

{
    if (sp->type != T_STRING)
        bad_xefun_arg(1, sp);
    printf("%s", sp->u.string);
    free_svalue(sp);
    return sp - 1;
}

/*-------------------------------------------------------------------------*/
int
write_file (char *file, char *str)

/* EFUN write_file()
 *
 * Append the text <str> to the end of <file>
 * Return 0 for failure, otherwise 1.
 *
 * Note that this function might be called recursively (see below).
 */

{
    FILE *f;

    file = check_valid_path(file, current_object, "write_file", MY_TRUE);
    if (!file)
        return 0;

    f = fopen(file, "a");
    if (f == NULL) {
        if (errno == EMFILE
#ifdef ENFILE
         || errno == ENFILE
#endif
        ) {
            /* lex_close() calls lexerror(). lexerror() calls yyerror().
             * yyerror calls smart_log(). smart_log calls apply_master_ob(),
             * which might call us again.
             * This is why the value of file needs to be preserved.
             * TODO: Why is lex_close() called at all - just for the nice
             * TODO:: message? Hmm, it could happen during the processing
             * TODO:: of a compilation error on mudlib level.
             */
            push_apply_value();
            lex_close(NULL);
            pop_apply_value();
            f = fopen(file, "a");
        }
        if (f == NULL) {
            perror("write_file");
            error("Wrong permissions for opening file %s for append.\n", file);
        }
    }
    FCOUNT_WRITE(file);
    fwrite(str, strlen(str), 1, f);
    fclose(f);
    return 1;
}

/*-------------------------------------------------------------------------*/
char *
read_file (char *file, int start, int len)

/* EFUN read_file()
 *
 * Read <len> lines from <file>, starting with line <start> (counting
 * up from 1). If <len> is 0, the whole file is read.
 *
 * Result is a pointer to a buffer with the read text, or NULL
 * on failures. The single lines of the text read are always
 * terminated with a single '\n'.
 *
 * When <start> or <len> are given, the function returns up READ_FILE_MAX_SIZE
 * bytes of text. If the whole file should be read, but is greater than
 * the above limit, or if not all of <len> lines fit into the buffer,
 * NULL is returned.
 *
 * TODO: What does <len> == -1 do?
 */

{
    struct stat st;
    FILE *f;
    char *str, *p, *p2, *end, c;
    int size;

    if (len < 0 && len != -1)
        return NULL;

    file = check_valid_path(file, current_object, "read_file", MY_FALSE);
    if (!file)
        return NULL;

    /* If the file would be opened in text mode, the size from fstat would
     * not match the number of characters that we can read.
     */
    f = fopen(file, "rb");
    if (f == NULL)
        return NULL;
    FCOUNT_READ(file);

    /* Check if the file is small enough to be read. */

    if (fstat(fileno(f), &st) == -1)
    {
        fatal("Could not stat an open file.\n");
        /* NOTREACHED */
        return NULL;
    }

    size = st.st_size;
    if (size > READ_FILE_MAX_SIZE) {
        if ( start || len ) size = READ_FILE_MAX_SIZE;
        else {
            fclose(f);
            return NULL;
        }
    }

    /* Make the arguments sane */
    if (!start) start = 1;
    if (!len) len = READ_FILE_MAX_SIZE;

    /* Get the memory */
    str = xalloc(size + 2); /* allow a trailing \0 and leading ' ' */
    if (!str) {
        fclose(f);
        error("Out of memory\n");
        /* NOTREACHED */
        return NULL;
    }

    *str++ = ' '; /* this way, we can always read the 'previous' char... */
    str[size] = '\0';

    /* Search for the first line to read.
     * For this, the file is read in chunks of <size> bytes, st.st_size
     * records the remaining length of the file.
     */
    do {
        /* Read the next chunk */
        if (size > st.st_size) /* Happens with the last block */
            size = st.st_size;

        if ((!size && start > 1) || fread(str, size, 1, f) != 1) {
                fclose(f);
            xfree(str-1);
                return NULL;
        }
        st.st_size -= size;
        end = str+size;

        /* Find all the '\n' in the chunk and count them */
        for (p = str; NULL != ( p2 = memchr(p, '\n', end-p) ) && --start; )
            p = p2+1;

    } while ( start > 1 );

    /* p now points to the first requested line.
     * st.st_size is the remaining size of the file.
     */

    /* Shift the found lines back to the front of the buffer, and
     * count them.
     * Also convert \r\n pairs into \n on MS-DOS filesystems.
     */
    for (p2 = str; p != end; ) {
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
        *p2++ = c;
    }

    /* If there are still some lines missing, and parts of the file
     * are not read yet, read and scan those remaining parts.
     */

    if ( len && st.st_size ) {

        /* Read the remaining file, but only as much as there is
         * space left in the buffer. As that one is READ_FILE_MAX_SIZE
         * long, it has to be sufficient.
         */

        size -= ( p2-str) ;
        if (size > st.st_size)
            size = st.st_size;

        if (fread(p2, size, 1, f) != 1) {
                fclose(f);
            xfree(str-1);
                return NULL;
        }

        st.st_size -= size;
        end = p2+size;

        /* Count the remaining lines, again converting \r\n into \n
         * when necessary.
         */
        for (p = p2; p != end; ) {
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

        /* If there are lines missing and the file is not at its end,
         * we have a failure.
         */
        if ( st.st_size && len > 0) {
            /* tried to read more than READ_MAX_FILE_SIZE */
            fclose(f);
            xfree(str-1);
            return NULL;
        }
    }

    *p2 = '\0';
    fclose(f);

    /* Make a copy of the valid parts of the str buffer, then
     * get rid of the largish buffer itself.
     */
    p2 = string_copy(str); /* TODO: string_n_copy() */
    xfree(str-1);
    if (!p2)
        error("Out of memory\n");

    return p2;
} /* read_file() */

/*-------------------------------------------------------------------------*/
char *
read_bytes (char *file, int start, int len)

/* EFUN read_bytes()
 *
 * Read <len> bytes (but mostly MAX_BYTE_TRANSFER) from <file>, starting
 * with byte <start> (counting up from 0). If <start> is negative, it is
 * counted from the end of the file. If <len> is 0, an empty (but valid)
 * string is returned.
 *
 * Result is a pointer to a buffer with the read text, or NULL
 * on failures.
 */

{
    struct stat st;

    char *str,*p;
    int size, f;

    /* Perform some sanity checks */
    if (len < 0 || len > MAX_BYTE_TRANSFER)
        return NULL;

    file = check_valid_path(file, current_object, "read_bytes", MY_FALSE);
    if (!file)
        return NULL;

    /* Open the file and determine its size */
    f = ixopen(file, O_RDONLY);
    if (f < 0)
        return NULL;
    FCOUNT_READ(file);

    if (fstat(f, &st) == -1)
        fatal("Could not stat an open file.\n");
    size = st.st_size;

    /* Determine the proper start and len to use */
    if (start < 0)
        start = size + start;

    if (start >= size) {
        close(f);
        return NULL;
    }
    if ((start+len) > size)
        len = (size - start);

    /* Seek and read */
    if ((size = lseek(f,start, 0)) < 0) {
        close(f);
        return NULL;
    }

    str = xalloc(len + 1);
    if (!str) {
        close(f);
        return NULL;
    }

    size = read(f, str, len);

    close(f);

    if (size <= 0) {
        xfree(str);
        return NULL;
    }

    /* No postprocessing, except for the adding of the '\0' without
     * the gamedriver won't be happy.
     */
    str[size] = '\0';

    /* We return a copy of the life parts of the buffer, and get rid
     * of the largish buffer itself.
     */
    p = string_copy(str);
    xfree(str);

    return p;
} /* read_bytes() */

/*-------------------------------------------------------------------------*/
int
write_bytes (char *file, int start, char *str)

/* EFUN write_bytes()
 *
 * Write <str> (but not more than MAX_BYTE_TRANSFER bytes) to <file>,
 * starting with byte <start> (counting up from 0). If <start> is negative,
 * it is counted from the end of the file.
 *
 * Result is 0 on failure, and 1 otherwise.
 */

{
    struct stat st;

    mp_int size, len;
    int f;

    /* Sanity checks */
    file = check_valid_path(file, current_object, "write_bytes", MY_TRUE);
    if (!file)
        return 0;

    len = strlen(str);
    if(len > MAX_BYTE_TRANSFER)
        return 0;

    f = ixopen(file, O_WRONLY);
    if (f < 0)
        return 0;
    FCOUNT_WRITE(file);

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
} /* write_bytes() */

/*-------------------------------------------------------------------------*/
int
file_size (char *file)

/* EFUN file_size()
 *
 * Determine the length of <file> and return it.
 * Return -1 if the file doesn't exist, -2 if the name points to a directory.
 */

{
    struct stat st;

    file = check_valid_path(file, current_object, "file_size", MY_FALSE);
    if (!file)
        return -1;
    if (ixstat(file, &st) == -1)
        return -1;
    if (S_IFDIR & st.st_mode)
        return -2;
    return st.st_size;
}

/*-------------------------------------------------------------------------*/
struct svalue*
f_regreplace (struct svalue *sp)

/* TEFUN regreplace()
 *
 *     string regreplace (string txt, string pattern, string replace
 *                                                  , int flags)
 *
 * Search through <txt> for one/all occurences of <pattern> and replace them
 * with the <replace> pattern, returning the result. <flags> is the bit-or
 * of these values:
 *   F_GLOBAL   = 1: when given, all occurences of <pattern> are replace,
 *                   else just the first
 *   F_EXCOMPAT = 2: when given, the expressions are ex-compatible,
 *                   else they aren't.
 * TODO: The gamedriver should write these values into an include file.
 *
 * The function behaves like the s/<pattern>/<replace>/<flags> command
 * in sed or vi. It offers an efficient and far more powerful replacement
 * for implode(regexplode()).
 */

{
#define F_GLOBAL   0x1
#define F_EXCOMPAT 0x2

    struct regexp *pat;
    int   flags;
    char *oldbuf, *buf, *curr, *new, *start, *old, *sub;
    int   space, origspace;

    /*
     * Must set inter_sp before call to regcomp,
     * because it might call regerror.
     */
    inter_sp = sp;

    /* Extract the arguments */
    if (sp->type!=T_NUMBER)
        bad_xefun_arg(4, sp);
    flags = sp->u.number;

    if (sp[-1].type!=T_STRING)
            bad_xefun_arg(3, sp);
    sub = sp[-1].u.string;

    if (sp[-2].type!=T_STRING)
        bad_xefun_arg(2, sp);

    if (sp[-3].type!=T_STRING)
        bad_xefun_arg(1, sp);

    start = curr = sp[-3].u.string;

    origspace = space = (strlen(start)+1)*2;

/* reallocate on the fly */
#define XREALLOC \
    space  += origspace;\
    origspace = origspace*2;\
    oldbuf        = buf;\
    buf        = (char*)rexalloc(buf,origspace*2);\
    if (!buf) { \
        xfree(oldbuf); \
        if (pat) xfree(pat); \
        error("Out of memory\n"); \
    } \
    new = buf + (new-oldbuf)


    new = buf = (char*)xalloc(space);
    if (!new)
    {
        error("Out of memory.\n");
        /* NOTREACHED */
        return NULL;
    }
    pat = REGCOMP(sp[-2].u.string,(flags & F_EXCOMPAT) ? 1 : 0);
    /* regcomp returns NULL on bad regular expressions. */

    if (pat && regexec(pat,curr,start)) {
        do {
            int diff = pat->startp[0]-curr;
            space -= diff;
            while (space <= 0) {
                XREALLOC;
            }
            strncpy(new,curr,diff);
            new += diff;
            old  = new;
            *old = '\0';

            /* Now what may have happen here. We *could*
             * be out of mem (as in 'space') or it could be
             * a regexp problem. the 'space' problem we
             * can handle, the regexp problem not.
             * hack: we store a \0 into *old ... if it is
             * still there on failure, it is a real failure.
             * if not, increase space. The player could get
             * some irritating messages from regerror()
             * ... (should we switch them off?)
             */
            while (NULL == (new = regsub(pat, sub, new, space, 1)) )
            {
                int xold;

                if (!*old)
                {
                    xfree(buf);
                    if (pat)
                        REGFREE(pat);
                    error("Out of memory\n");
                    /* NOTREACHED */
                    return NULL;
                }
                xold = old - buf;
                XREALLOC;
                new = buf + xold;
                old = buf + xold;
                *old='\0';
            }
            space -= new - old;
            while (space <= 0) {
                XREALLOC;
            }
            if (curr == pat->endp[0])
            {
                /* prevent infinite loop
                 * by advancing one character.
                 */
                if (!*curr) break;
                --space;
                while (space <= 0) {
                    XREALLOC;
                }
                *new++ = *curr++;
            }
            else
                curr = pat->endp[0];
        } while ((flags&F_GLOBAL) && !pat->reganch && regexec(pat,curr,start));
        space -= strlen(curr)+1;
        if (space <= 0) {
            XREALLOC;
        }
        strcpy(new,curr);
    }
    else
    {
        /* Pattern not found -> no editing necessary */
        strcpy(buf,start);
    }

    if (pat)
        REGFREE(pat);

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

#undef F_EXCOMPAT
#undef F_GLOBAL
#undef XREALLOC
}

/***************************************************************************/

