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
#include "typedefs.h"

#include <stddef.h>
#include <ctype.h>
#include <limits.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/time.h>
#if defined(AMIGA) && !defined(__GNUC__)
#include "hosts/amiga/nsignal.h"
#else
#include <signal.h>
#include <sys/times.h>
#endif
#include <sys/stat.h>
#include <fcntl.h>
#include <time.h>
#include <math.h>

#define NO_REF_STRING
#include "backend.h"
#include "actions.h"
#include "array.h"
#include "call_out.h"
#include "closure.h"
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
#include "mempools.h"
#include "my-alloca.h"
#include "object.h"
#include "otable.h"
#include "random.h"
#include "regexp.h"
#include "rxcache.h"
#include "simulate.h"
#include "smalloc.h"
#include "stdstrings.h"
#include "stralloc.h"
#include "swap.h"
#include "wiz_list.h"
#include "xalloc.h"

#include "../mudlib/sys/driver_hook.h"
#include "../mudlib/sys/debug_message.h"

/*-------------------------------------------------------------------------*/

#define ALARM_TIME  2  /* The granularity of alarm() calls */

/*-------------------------------------------------------------------------*/

mp_int current_time;
  /* The current time, updated every heart beat.
   * TODO: Should be time_t if it is given that time_t is always a skalar.
   */

Bool time_to_call_heart_beat;
  /* True: It's time to call the heart beat. Set by comm.c when it recognizes
   *   an alarm(). */

volatile mp_int alarm_called = MY_FALSE;
  /* The alarm() handler sets this to TRUE whenever it is called,
   * to allow check_alarm() to verify that the alarm is still alive.
   */

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

Bool extra_jobs_to_do = MY_FALSE;
  /* True: the backend has other things to do in this cycle than just
   *   parsing commands or calling the heart_beat.
   */

GC_Request gc_request = gcDont;
  /* gcDont: No garbage collection is due.
   * gcMalloc: The mallocator requested a gc (requires extra_jobs_to_do).
   * gcEfun: GC requested by efun (requires extra_jobs_to_do).
   */

/* TODO: all the 'extra jobs to do' should be collected here, in a nice
 * TODO:: struct.
 */

Bool mud_is_up = MY_FALSE;
  /* True: the driver is finished with the initial processing
   * and has entered the main loop. This flag is currently not
   * used by the driver, but can be useful for printf()-style debugging.
   */

static double load_av = 0.0;
  /* The load average (player commands/second), weighted over the
   * last period of time.
   */

static double compile_av = 0.0;
  /* The average of compiled lines/second, weighted over the last period
   * of time.
   */

static time_t time_last_slow_shut = 0;
  /* Time of the last call to slow_shut_down(), to avoid repeated
   * calls while the previous ones are still working.
   */

/*-------------------------------------------------------------------------*/

/* --- Forward declarations --- */

static void process_objects(void);
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
    reset_machine(MY_FALSE);   /* Pop down the stack. */
}

/*-------------------------------------------------------------------------*/
#ifdef DEBUG

static void
do_state_check (int minlvl, const char *where)

/* Perform the simplistic interpret::check_state() if the check_state_lvl
 * is at least <minlvl>. If an inconsistency is detected, the message
 * "<timestamp> Inconsistency <where>" and a trace of the last instructions
 * is logged, then the state is cleared.
 *
 * Be careful that the call to check_state()/clear_state() is
 * not too costly, as it is done in every loop.
 */
{
    if (check_state_level >= minlvl && check_state() )
    {
        debug_message("%s Inconsistency %s\n", time_stamp(), where);
        printf("%s Inconsistency %s\n", time_stamp(), where);
        dump_trace(MY_TRUE, NULL);
#ifdef TRACE_CODE
        last_instructions(TOTAL_TRACE_LENGTH, 1, 0);
#endif
        clear_state();
    }
} /* do_state_check() */

#else

#define do_state_check(minlvl, where)

#endif

/*-------------------------------------------------------------------------*/
void
logon (object_t *ob)

/* Call the logon() lfun in the object <ob>.
 *
 * current_object is temporarily set to <ob> in order to allow logon()
 * to be static (security measure). Doing so is harmless as there is no
 * previous_object to consider.
 *
 * TODO: This should go into simulate.c or comm.c
 */

{
    svalue_t *ret;
    object_t *save = current_object;

    current_object = ob;
    ret = apply(STR_LOGON, ob, 0);
    if (ret == 0) {
        /* add_message("prog %s:\n", ob->name); */
        error("Could not find logon() on the player %s\n", ob->name);
    }
    current_object = save;
} /* logon() */

/*-------------------------------------------------------------------------*/
#if defined(AMIGA) && !defined(__GNUC__)

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
static RETSIGTYPE
handle_hup (int sig UNUSED)

/* SIGHUP handler: request a game shutdown.
 */
{
#ifdef __MWERKS__
#    pragma unused(sig)
#endif
    extra_jobs_to_do = MY_TRUE;
    game_is_being_shut_down = MY_TRUE;
#ifndef RETSIGTYPE_VOID
    return 0;
#endif
} /* handle_hup() */

/*-------------------------------------------------------------------------*/
static RETSIGTYPE
handle_usr1 (int sig UNUSED)

/* SIGUSR1 handler: request a master update.
 */

{
#ifdef __MWERKS__
#    pragma unused(sig)
#endif
    extra_jobs_to_do = MY_TRUE;
    master_will_be_updated = MY_TRUE;
    eval_cost += max_eval_cost >> 3;
    (void)signal(SIGUSR1, handle_usr1);
#ifndef RETSIGTYPE_VOID
    return 0;
#endif
} /* handle_usr1() */

/*-------------------------------------------------------------------------*/
static RETSIGTYPE
handle_usr2 (int sig UNUSED)

/* SIGUSR2 handler: reopen the debug.log file.
 */

{
#ifdef __MWERKS__
#    pragma unused(sig)
#endif
    reopen_debug_log = MY_TRUE;
    (void)signal(SIGUSR2, handle_usr2);
#ifndef RETSIGTYPE_VOID
    return 0;
#endif
} /* handle_usr1() */

/*-------------------------------------------------------------------------*/
static INLINE void
cleanup_stuff (void)

/* Perform a number of clean up operations: replacing programs, freeing
 * driver hooks, and removing destructed object.
 * They are collected in one function so that they can be easily called from
 * various places (backend loop and the process_objects loops); especially
 * since it is not advisable to remove destructed objects before replacing
 * the programs.
 */

{
    /* Reset the VM to avoid dangling references to invalid objects */
    clear_state();

    /* Replace programs */
    if (obj_list_replace)
    {
        replace_programs();
    }

#ifdef USE_FREE_CLOSURE_HOOK
    /* Free older driver hooks
     */
    free_old_driver_hooks();
#endif

    /* Finish up all newly destructed objects.
     */
    handle_newly_destructed_objects();

    /* Remove all unreferenced destructed objects.
     */
    remove_destructed_objects();

} /* cleanup_stuff() */

/*-------------------------------------------------------------------------*/
void
backend (void)

/* The backend loop, the backbone of the driver's operations.
 * It only returns when the game has to be shut down.
 */

{
    char buff[MAX_TEXT+4];
        /* Note that the size of buff[] is determined by MAX_TEXT, which
         * is the max size of the network receive buffer. Iow: no
         * buffer overruns possible.
         */



    /*
     * Set up.
     */

    prepare_ipc();

    (void)signal(SIGHUP,  handle_hup);
    (void)signal(SIGUSR1, handle_usr1);
    (void)signal(SIGUSR2, handle_usr2);
    if (!t_flag) {
        /* Start the first alarm */
        ALARM_HANDLER_FIRST_CALL(catch_alarm);
        current_time = get_current_time();
        comm_time_to_call_heart_beat = MY_FALSE;
        time_to_call_heart_beat = MY_FALSE;
        alarm(ALARM_TIME);
    }
#if defined(AMIGA) && !defined(__GNUC__)
    atexit(exit_alarm_timer);
#endif

    mud_is_up = MY_TRUE;

    printf("%s LDMud ready for users.\n", time_stamp());
    fflush(stdout);
    debug_message("%s LDMud ready for users.\n", time_stamp());

    toplevel_context.rt.type = ERROR_RECOVERY_BACKEND;
    setjmp(toplevel_context.con.text);

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
        do_state_check(1, "in main loop");

        check_alarm();

        RESET_LIMITS;
        CLEAR_EVAL_COST;

#ifdef C_ALLOCA
        /* Execute pending deallocations */
        alloca(0); /* free alloca'd values from deeper levels of nesting */
#endif

        /* Replace programs, remove destructed objects, and similar stuff */
        cleanup_stuff();

#ifdef DEBUG
        if (check_a_lot_ref_counts_flag)
            check_a_lot_ref_counts(NULL);
            /* after removing destructed objects! */
#endif
#ifdef CHECK_STRINGS
        if (check_string_table_flag)
            check_string_table();
#endif

        check_for_out_connections();

        /*
         * Do the extra jobs, if any.
         */

        if (extra_jobs_to_do) {

            current_interactive = NULL;
            if (game_is_being_shut_down)
            {
                command_giver = NULL;
                current_object = NULL;
                return;
            }

            if (master_will_be_updated) {
                deep_destruct(master_ob);
                master_will_be_updated = MY_FALSE;
                command_giver = NULL;
                current_object = &dummy_current_object_for_loads;
                callback_master(STR_EXT_RELOAD, 0);
                current_object = NULL;
            }

            if (gc_request != gcDont) {
                time_t time_now = time(NULL);
                char buf[120];

                if (gc_request == gcEfun
                 || time_now - time_last_gc >= 60)
                {
                  sprintf(buf, "%s Garbage collection req by %s "
                               "(slow_shut to do: %d, "
                               "time since last gc: %ld)\n"
                             , time_stamp()
                             , gc_request == gcEfun ? "efun" : "allocator"
                             , slow_shut_down_to_do
                             , (long)(time_now - time_last_gc));
                  write(1, buf, strlen(buf));
                  command_giver = NULL;
                  current_object = NULL;
                  garbage_collection();
                }
                else
                {
                  sprintf(buf, "%s Garbage collection req by %s refused "
                               "(slow_shut to do: %d, "
                               "time since last gc: %ld)\n"
                             , time_stamp()
                             , gc_request == gcEfun ? "efun" : "allocator"
                             , slow_shut_down_to_do
                             , (long)(time_now - time_last_gc));
                  write(1, buf, strlen(buf));
                  reallocate_reserved_areas();
                }

                gc_request = gcDont;

                if (slow_shut_down_to_do)
                {
                    if (time_now - time_last_slow_shut
                        >= slow_shut_down_to_do * 60
                       )
                    {
                        int minutes = slow_shut_down_to_do;
                        char shut_msg[90];

                        slow_shut_down_to_do = 0;
                        time_last_slow_shut = time_now;
                        malloc_privilege = MALLOC_MASTER;
                        sprintf(shut_msg, "%s slow_shut_down(%d)\n", time_stamp(), minutes);
                        write(1, shut_msg, strlen(shut_msg));

                        previous_ob = NULL;
                        command_giver = NULL;
                        current_interactive = NULL;

                        push_number(minutes);
                        callback_master(STR_SLOW_SHUT, 1);
                    }
                    else
                    {
                        sprintf(buf, "%s Last slow_shut_down() still pending.\n"
                                   , time_stamp()
                               );
                        write(1, buf, strlen(buf));
                    }
                }
                malloc_privilege = MALLOC_USER;
            }

            extra_jobs_to_do = MY_FALSE;

            if (num_dirty_mappings) {
                compact_mappings((num_dirty_mappings+80) >> 5, MY_FALSE);
                malloc_privilege = MALLOC_USER;
            }
        } /* if (extra_jobs_to_do */

        do_state_check(2, "before get_message()");

        /*
         * Call comm.c and wait for player input, or until the next
         * heart beat is due.
         */

        if (get_message(buff))
        {
            interactive_t *ip;

            /* Create the new time_stamp string in the function's local
             * buffer.
             */
            (void)time_stamp();

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
             * The command_parser will find that current_object is 0, and
             * then set current_object to point to the object that defines
             * the command. This will enable such functions to be static.
             */
            current_object = NULL;
            current_interactive = command_giver;

            (void)O_SET_INTERACTIVE(ip, command_giver);
#ifdef DEBUG
            if (!ip)
            {
                fatal("Non interactive player in main loop !\n");
                /* NOTREACHED */
            }
#endif

            ip->set_input_to = MY_FALSE;
            tracedepth = 0;

            if (buff[0] == '!'
             && buff[1] != '\0'
             && command_giver->super)
            {
                if(!call_function_interactive(ip, buff)) {
                    /* We got a bang-input, but no input context wants
                    * to handle it - treat it as a normal command.
                    */
                    if (ip->noecho & NOECHO)
                    {
                        /* !message while in NOECHO - simulate the
                        * echo by sending the (remaining) raw data we got.
                        */
                        add_message("%s\n", buff + ip->chars_ready);
                        ip->chars_ready = 0;
                    }
                    execute_command(buff+1, command_giver);
                }
            }
            else if (O_GET_EDBUFFER(command_giver))
                ed_cmd(buff);
            else if (
              call_function_interactive(ip, buff))
                NOOP;
            else
                execute_command(buff, command_giver);

            /* ip might be invalid again here */

            /*
             * Print a prompt if player is still here.
             */
            if (command_giver)
            {
                if (O_SET_INTERACTIVE(ip, command_giver)
                 && !ip->do_close)
                {
                    print_prompt();
                }
            }

            do_state_check(2, "after handling message");
        }
        else
        {
            /* No new message, just reate the new time_stamp string in
             * the function's local buffer.
             */
            (void)time_stamp();
        } /* if (get_message()) */

        /* Do the periodic functions if it's time.
         */
        if (time_to_call_heart_beat)
        {
            current_time = get_current_time();

            current_object = NULL;

            /* Start the next alarm */
            comm_time_to_call_heart_beat = MY_FALSE;
            time_to_call_heart_beat = MY_FALSE;
            alarm(ALARM_TIME);

            /* Do the timed events */
            do_state_check(2, "before heartbeat");
            call_heart_beat();
            do_state_check(2, "after heartbeat");
            call_out();
            do_state_check(2, "after call_out");

            /* Reset/cleanup/swap objects.
             */
            process_objects();
            do_state_check(2, "after swap/cleanup/reset");

            /* Other periodic processing */
            command_giver = NULL;
            trace_level = 0;

            wiz_decay();
            comm_cleanup_interactives();

            mem_consolidate(MY_FALSE);
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

ALARM_HANDLER(catch_alarm, alarm_called = MY_TRUE; comm_time_to_call_heart_beat = MY_TRUE; total_alarms++; )

#else

RETSIGTYPE
catch_alarm (int dummy UNUSED)
{
#ifdef __MWERKS__
#    pragma unused(dummy)
#endif
    (void)signal(SIGALRM, (RETSIGTYPE(*)(int))catch_alarm);
    alarm_called = MY_TRUE;
    comm_time_to_call_heart_beat = MY_TRUE;
    total_alarms++;
#ifndef RETSIGTYPE_VOID
    return 0;
#endif
}

#endif

/*-------------------------------------------------------------------------*/
void check_alarm (void)

/* Check the time since the last recorded call to the alarm handler.
 * If it is longer than a limit, assume that the alarm died and restart it.
 *
 * This function is necessary especially for Cygwin on Windows, where it
 * is not unusual that the driver process receives so few cycles that it
 * loses its alarm.
 * TODO: It should be possible to get rid of alarms altogether if all
 * TODO:: timebound methods check the time since the last checkpoint.
 */

{
    static mp_int last_alarm_time = 0;
    mp_int curtime = get_current_time();

    if (t_flag)  /* Timing turned off? */
        return;

    if (!last_alarm_time) /* initialize it */
        last_alarm_time = curtime;

    if (alarm_called)
    {
        /* We got an alarm - restart the timer */
        last_alarm_time = curtime;
    }
    else if (curtime - last_alarm_time > 15)
    {
        debug_message("%s Last alarm was %ld seconds ago - restarting it.\n"
                     , time_stamp(), curtime - last_alarm_time);

        alarm(0); /* stop alarm in case it is still alive, but just slow */
        comm_time_to_call_heart_beat = MY_TRUE;
        time_to_call_heart_beat = MY_TRUE;
        (void)signal(SIGALRM, (RETSIGTYPE(*)(int))catch_alarm);
        alarm(ALARM_TIME);

        last_alarm_time = curtime; /* Since we just restarted it */
    }

    alarm_called = MY_FALSE;
} /* check_alarm() */

/*-------------------------------------------------------------------------*/
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

    object_t *obj;   /* Current object worked on */

    struct error_recovery_info error_recovery_info;
      /* Local error recovery info */

    /* Housekeeping */

    num_last_processed = 0;
    did_reset = MY_FALSE;
    did_swap = MY_FALSE;

    error_recovery_info.rt.last = rt_context;
    error_recovery_info.rt.type = ERROR_RECOVERY_BACKEND;
    rt_context = (rt_context_t *)&error_recovery_info;

    if (setjmp(error_recovery_info.con.text))
    {
        clear_state();
        debug_message("%s Error in process_objects().\n", time_stamp());
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
        mp_int min_time_to_swap; /* Variable swap exclusion time before reset */
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

        /* Variables won't be swapped if a reset is due shortly.
         * "shortly" means half the var swap interval, but at max 5 minutes.
         */
        min_time_to_swap = 5 * 60;
        if (time_to_swap_variables / 2 < min_time_to_swap)
            min_time_to_swap = time_to_swap_variables/2;

        /* ------ Reset ------ */

        /* Check if a reset() is due. Objects which have not been touched
         * since the last reset just get a new due time set.
         * It is tempting to skip the reset handling for objects which
         * are swapped out, but then swapper would have to call reset_object()
         * for due objects on swap-in (just setting a new due-time is not
         * sufficient).
         * TODO: Do exactly that?
         */

        if (time_to_reset > 0
         && obj->time_reset
         && obj->time_reset < current_time)
        {
            if (obj->flags & O_RESET_STATE)
            {
#ifdef DEBUG
                if (d_flag)
                    fprintf(stderr, "%s RESET (virtual) %s\n", time_stamp(), obj->name);
#endif
                obj->time_reset = current_time+time_to_reset/2
                                  +(mp_int)random_number((uint32)time_to_reset/2);
            }
            else if (!did_reset || !comm_time_to_call_heart_beat)
            {
#ifdef DEBUG
                if (d_flag)
                    fprintf(stderr, "%s RESET %s\n", time_stamp(), obj->name);
#endif
                if (obj->flags & O_SWAPPED
                 && load_ob_from_swap(obj) < 0)
                   continue;
                bResetCalled = MY_TRUE;
                did_reset = MY_TRUE;
                RESET_LIMITS;
                CLEAR_EVAL_COST;
                command_giver = 0;
                previous_ob = NULL;
                trace_level = 0;
                reset_object(obj, H_RESET);
                if (obj->flags & O_DESTRUCTED)
                    continue;

                if (time_to_swap > 0 || time_to_swap_variables > 0)
                {
                    /* Restore old time_of_ref. This might result in a quick
                     * swap-in/swap-out yoyo if this object was swapped out
                     * in the first place. To make this less costly, variables
                     * are not swapped out short before a reset (see below).
                     */
                    obj->time_of_ref = current_time - time_since_ref;
                }
            } /* if (call reset or not) */
        } /* if (needs reset?) */


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
        if (time_to_cleanup > 0
          && obj->flags & O_WILL_CLEAN_UP
          && time_since_ref > time_to_cleanup
          && !bResetCalled
          && (!did_swap || !comm_time_to_call_heart_beat))
        {
            int save_reset_state = obj->flags & O_RESET_STATE;
            svalue_t *svp;

#ifdef DEBUG
            if (d_flag)
                fprintf(stderr, "%s CLEANUP %s\n", time_stamp(), obj->name);
#endif

            did_swap = MY_TRUE;

            /* Remove all pending destructed objects, to get a true refcount.
             * But make sure that we don't clobber anything else while
             * doing so.
             */
            cleanup_stuff();

            /* Supply a flag to the object that says if this program
             * is inherited by other objects. Cloned objects might as well
             * believe they are not inherited. Swapped objects will not
             * have a ref count > 1 (and will have an invalid ob->prog
             * pointer). If the object is a blueprint, the extra reference
             * from the program will not be counted.
             */
            if (obj->flags & (O_CLONE|O_REPLACED))
                push_number(0);
            else if (O_PROG_SWAPPED(obj))
                push_number(1);
            else if (obj->prog->blueprint == obj)
                push_number(obj->prog->ref - 1);
            else
                push_number(obj->prog->ref);

            RESET_LIMITS;
            CLEAR_EVAL_COST;
            command_giver = NULL;
            previous_ob = NULL;
            trace_level = 0;
            if (driver_hook[H_CLEAN_UP].type == T_CLOSURE)
            {
                lambda_t *l;

                l = driver_hook[H_CLEAN_UP].u.lambda;
                if (driver_hook[H_CLEAN_UP].x.closure_type == CLOSURE_LAMBDA)
                    l->ob = obj;
                push_object(obj);
                call_lambda(&driver_hook[H_CLEAN_UP], 2);
                svp = inter_sp;
                pop_stack();
            }
            else if (driver_hook[H_CLEAN_UP].type == T_STRING)
            {
                svp = apply(driver_hook[H_CLEAN_UP].u.string, obj, 1);
            }
            else
            {
                pop_stack();
                goto no_clean_up;
            }
            if (obj->flags & O_DESTRUCTED)
                continue;

            if (!svp
             || (svp->type == T_NUMBER && svp->u.number == 0)
               )
                obj->flags &= ~O_WILL_CLEAN_UP;
            obj->flags |= save_reset_state;

no_clean_up:
            obj->time_of_ref = current_time;
                  /* in case the hook didn't update it */
        }


        /* ------ Swapping ------ */

        /* At last, there is a possibility that the object can be swapped
         * out.
         *
         * Variables are swapped after time_to_swap_variables has elapsed
         * since the last ref, and if the object is either still reset or
         * the next reset is at least min(5 minutes, time_to_swap_variables/2)
         * in the future. When a reset is due, this second condition delays the
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
               || (obj->time_reset - current_time > min_time_to_swap)))
            {
#ifdef DEBUG
                if (d_flag)
                   fprintf(stderr, "%s swap vars of %s\n", time_stamp(), obj->name);
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
                    fprintf(stderr, "%s swap %s\n", time_stamp(), obj->name);
#endif
                swap_program(obj);
                if (O_PROG_SWAPPED(obj))
                    did_swap = MY_TRUE;
            }
        } /* if (obj can be swapped) */

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
    rt_context = error_recovery_info.rt.last;
} /* process_objects() */

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
    vector_t *prefiles;
    svalue_t *ret;
    static mp_int ix0;
    static size_t num_prefiles;
    mp_int ix;

    /* Call master->epilog(<eflag>)
     */
    push_number(eflag);
    ret = callback_master(STR_EPILOG, 1);

    if ((ret == 0) || (ret->type != T_POINTER))
        return;
    else
        prefiles = ret->u.vec;

    if ((prefiles == 0) || ((num_prefiles = VEC_SIZE(prefiles)) < 1))
        return;

    ref_array(prefiles);
      /* Without this, the next apply call would free the array prefiles.
       */

    ix0 = -1;

    /* In case of errors, return here and simply continue with
     * the loop.
     */
    toplevel_context.rt.type = ERROR_RECOVERY_BACKEND;
    if (setjmp(toplevel_context.con.text)) {
        clear_state();
        add_message("Anomaly in the fabric of world space.\n");
    }

    /* Loop through the prefiles array, calling master->preload()
     * for every string in it.
     */
    while ((ix = ++ix0) < num_prefiles) {
        if (prefiles->item[ix].type != T_STRING)
            continue;

        RESET_LIMITS;
        CLEAR_EVAL_COST;
        push_string_malloced(prefiles->item[ix].u.string);
        (void)apply_master(STR_PRELOAD, 1);

    }

    free_array(prefiles);
    toplevel_context.rt.type = ERROR_RECOVERY_NONE;
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

#if defined(__MWERKS__) && !defined(WARN_ALL)
#    pragma warn_largeargs off
#endif
    sprintf(buff, "%.2f cmds/s, %.2f comp lines/s", load_av, compile_av);
#if defined(__MWERKS__)
#    pragma warn_largeargs reset
#endif
    return buff;
}

/*-------------------------------------------------------------------------*/
svalue_t *
f_debug_message (svalue_t *sp)

/* TEFUN debug_message()
 *
 *   debug_message(string text)
 *   debug_message(string text, int flags)
 *
 * Print the <text> to stdout, stderr, and/or the <host>.debug.log file.
 *
 * The parameter <flags> is a combination of bitflags determining the
 * target and the mode of writing.
 *
 * The target flags are: DMSG_STDOUT, DMSG_STDERR and DMSG_LOGFILE.
 * If the flag DMSG_STAMP is given, the message is prepended with the
 * current date and time in the format 'YYYY.MM.DD HH:MM:SS '.
 *
 * If <flags> is given as 0, left out, or contains no target
 * definition, debug_message() will print to stdout and to the logfile.
 */

{
    if ((sp-1)->type != T_STRING)
        bad_xefun_arg(1, sp);
    if (sp->type != T_NUMBER
     || (sp->u.number & ~(DMSG_STDOUT|DMSG_STDERR|DMSG_LOGFILE|DMSG_STAMP)) != 0)
        bad_xefun_arg(2, sp);
    if (!(sp->u.number & DMSG_TARGET) || (sp->u.number & DMSG_STDOUT))
    {
        if (sp->u.number & DMSG_STAMP)
            printf("%s %s", time_stamp(), (sp-1)->u.string);
        else
            printf("%s", (sp-1)->u.string);
    }
    if (sp->u.number & DMSG_STDERR)
    {
        if (sp->u.number & DMSG_STAMP)
            fprintf(stderr, "%s %s", time_stamp(), (sp-1)->u.string);
        else
            fprintf(stderr, "%s", (sp-1)->u.string);
    }
    if (!(sp->u.number & DMSG_TARGET) || (sp->u.number & DMSG_LOGFILE))
    {
        if (sp->u.number & DMSG_STAMP)
            debug_message("%s %s", time_stamp(), (sp-1)->u.string);
        else
            debug_message("%s", (sp-1)->u.string);
    }
    free_svalue(sp);
    free_svalue(sp-1);
    return sp - 2;
}

/*-------------------------------------------------------------------------*/
int
e_write_file (char *file, char *str)

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
        if ((errno == EMFILE
#ifdef ENFILE
             || errno == ENFILE
            ) && current_file
#endif
        ) {
            /* We are called from within the compiler, probably to write
             * an error message into a log.
             * Call lex_close() (-> lexerror() -> yyerror() -> parse_error()
             * -> apply_master_ob() ) to try to close some files, the try
             * again.
             */
            push_apply_value();
            lex_close(NULL);
            pop_apply_value();
            f = fopen(file, "a");
        }
        if (f == NULL) {
            char * emsg;
            int err = errno;

            emsg = strerror(err);
            if (emsg)
            {
                error("Could not open %s for append: (%d) %s.\n"
                     , file, err, emsg);
            }
            else
            {
                perror("write_file");
                error("Could not open %s for append: errno %d.\n"
                     , file, err);
            }
            /* NOTREACHED */
        }
    }
    FCOUNT_WRITE(file);
    fwrite(str, strlen(str), 1, f);
    fclose(f);
    return 1;
} /* e_write_file() */

/*-------------------------------------------------------------------------*/
char *
e_read_file (char *file, int start, int len)

/* EFUN read_file()
 *
 * Read <len> lines from <file>, starting with line <start> (counting
 * up from 1). If <len> is 0, the whole file is read.
 *
 * Result is a pointer to a buffer with the read text, or NULL
 * on failures. The single lines of the text read are always
 * terminated with a single '\n'.
 *
 * When <start> or <len> are given, the function returns up max_file_xfer
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
    long size; /* TODO: fpos_t? */

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

    size = (long)st.st_size;
    if (max_file_xfer && size > max_file_xfer)
    {
        if ( start || len )
            size = max_file_xfer;
        else {
            fclose(f);
            return NULL;
        }
    }

    /* Make the arguments sane */
    if (!start) start = 1;
    if (!len) len = size;

    /* Get the memory */
    str = mb_alloc(mbFile, (size_t)size+2);
      /* allow a trailing \0 and leading ' ' */
    if (!str) {
        fclose(f);
        error("(read_file) Out of memory (%ld bytes) for buffer\n", size+2);
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
            size = (long)st.st_size;

        if ((!size && start > 1) || fread(str, (size_t)size, 1, f) != 1)
        {
            fclose(f);
            mb_free(mbFile);
            return NULL;
        }
        st.st_size -= size;
        end = str+size;

        /* Find all the '\n' in the chunk and count them */
        for (p = str; NULL != ( p2 = memchr(p, '\n', (size_t)(end-p)) ) && --start; )
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
         * space left in the buffer. As that one is max_file_xfer
         * long, it has to be sufficient.
         */

        size -= ( p2-str) ;
        if (size > st.st_size)
            size = (long)st.st_size;

        if (fread(p2, (size_t)size, 1, f) != 1)
        {
            fclose(f);
            mb_free(mbFile);
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
            mb_free(mbFile);
            return NULL;
        }
    }

    *p2 = '\0';
    fclose(f);

    /* Make a copy of the valid parts of the str buffer, then
     * get rid of the largish buffer itself.
     */
    p2 = string_copy(str); /* TODO: string_n_copy() */
    mb_free(mbFile);
    if (!p2)
        error("(read_file) Out of memory for result\n");

    return p2;
} /* e_read_file() */

/*-------------------------------------------------------------------------*/
char *
e_read_bytes (char *file, int start, int len)

/* EFUN read_bytes()
 *
 * Read <len> bytes (but mostly max_byte_xfer) from <file>, starting
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
    int f;
    long size; /* TODO: fpos_t? */

    /* Perform some sanity checks */
    if (len < 0 || (max_byte_xfer && len > max_byte_xfer))
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
    size = (long)st.st_size;

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
    if ((size = (long)lseek(f,start, 0)) < 0) {
        close(f);
        return NULL;
    }

    str = mb_alloc(mbFile, (size_t)len + 1);
    if (!str) {
        close(f);
        return NULL;
    }

    size = read(f, str, (size_t)len);

    close(f);

    if (size <= 0) {
        mb_free(mbFile);
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
    mb_free(mbFile);

    return p;
} /* e_read_bytes() */

/*-------------------------------------------------------------------------*/
int
e_write_bytes (char *file, int start, char *str)

/* EFUN write_bytes()
 *
 * Write <str> (but not more than max_byte_xfer bytes) to <file>,
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

    len = (mp_int)strlen(str);
    if (max_byte_xfer && len > max_byte_xfer)
        return 0;

    f = ixopen(file, O_WRONLY);
    if (f < 0)
        return 0;
    FCOUNT_WRITE(file);

    if (fstat(f, &st) == -1)
        fatal("Could not stat an open file.\n");
    size = (mp_int)st.st_size;

    if(start < 0)
        start = size + start;

    if (start > size) {
        close(f);
        return 0;
    }
    if ((size = (mp_int)lseek(f,start, 0)) < 0) {
        close(f);
        return 0;
    }

    size = write(f, str, (size_t)len);

    close(f);

    if (size <= 0) {
        return 0;
    }

    return 1;
} /* e_write_bytes() */

/*-------------------------------------------------------------------------*/
long
e_file_size (char *file)

/* EFUN file_size()
 *
 * Determine the length of <file> and return it.
 * Return -1 if the file doesn't exist, -2 if the name points to a directory.
 * These values must match the definitions in mudlib/sys/files.h.
 */

{
    struct stat st;

    st.st_mode = 0; /* Silences ZeroFault/AIX under high optimizations */
    file = check_valid_path(file, current_object, "file_size", MY_FALSE);
    if (!file)
        return -1;
    if (ixstat(file, &st) == -1)
        return -1;
    if (S_IFDIR & st.st_mode)
        return -2;
    return (long)st.st_size;
} /* e_file_size() */

/*-------------------------------------------------------------------------*/
svalue_t*
f_regreplace (svalue_t *sp)

/* TEFUN regreplace()
 *
 *     string regreplace (string txt, string pattern, closure|string replace
 *                                                  , int flags)
 *
 * Search through <txt> for one/all occurences of <pattern> and replace them
 * with the <replace> pattern, returning the result.
 * <replace> can be a string, or a closure returning a string. If it is
 * a closure, it will be called with the matched substring and
 * the position at which it was found as arguments.
 * <flags> is the bit-or of these values:
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
    char *oldbuf, *buf, *curr, *new, *start, *old, *sub, *match;
    size_t matchsize = 0;
    svalue_t *subclosure = NULL;
    long  space;
    size_t  origspace;

    /*
     * Must set inter_sp before call to REGCOMP,
     * because it might call hs_regerror.
     */
    inter_sp = sp;

    /* Extract the arguments */
    if (sp->type != T_NUMBER)
        bad_xefun_arg(4, sp);
    flags = sp->u.number;

    if (sp[-1].type != T_STRING && sp[-1].type != T_CLOSURE)
        bad_xefun_arg(3, sp);
    if (sp[-1].type == T_STRING)
    {
        sub = sp[-1].u.string;
        subclosure = NULL;
        match = NULL;
    }
    else
    {
        sub = NULL;
        subclosure = sp-1;
        match = NULL;
    }

    if (sp[-2].type != T_STRING)
        bad_xefun_arg(2, sp);

    if (sp[-3].type != T_STRING)
        bad_xefun_arg(1, sp);

    start = curr = sp[-3].u.string;

    space = (long)(origspace = (strlen(start)+1)*2);

/* reallocate on the fly */
#define XREALLOC \
    space += origspace;\
    origspace = origspace*2;\
    oldbuf = buf;\
    buf = (char*)rexalloc(buf,origspace);\
    if (!buf) { \
        xfree(oldbuf); \
        if (pat) REGFREE(pat); \
        error("(regreplace) Out of memory (%lu bytes) for buffer\n"\
             , (unsigned long)origspace); \
    } \
    new = buf + (new-oldbuf)

/* The rexalloc() above read originally 'rexalloc(buf, origspace*2)'.
 * Marcus inserted the '*2' since he experienced strange driver
 * crashes without. I think that the error corrected in dev.28 was the
 * real reason for the crashes, so that the safety factor is no longer
 * necessary. However, if regreplace() causes crashes again, this
 * is one thing to try.
 */

    xallocate(buf, (size_t)space, "buffer");
    new = buf;
    pat = REGCOMP((unsigned char *)(sp[-2].u.string),(flags & F_EXCOMPAT) ? 1 : 0, MY_FALSE);
    /* REGCOMP returns NULL on bad regular expressions. */

    if (pat && hs_regexec(pat,curr,start)) {
        do {
            size_t diff = (size_t)(pat->startp[0]-curr);
            space -= diff;
            while (space <= 0) {
                XREALLOC;
            }
            strncpy(new,curr,(size_t)diff);
            new += diff;
            old  = new;

            /* Determine the replacement string.
             */
            if (subclosure != NULL)
            {
                size_t patsize = pat->endp[0] - pat->startp[0];

                if (patsize+1 > matchsize)
                {
                    char * nmatch;

                    matchsize = patsize+1;
                    if (match)
                        nmatch = rexalloc(match, matchsize);
                    else
                        nmatch = xalloc(matchsize);
                    if (!nmatch)
                    {
                        xfree(buf);
                        if (pat)
                            REGFREE(pat);
                        error("Out of memory for matched string (%lu bytes)\n"
                             , (unsigned long)patsize+1);
                        /* NOTREACHED */
                        return NULL;
                    }
                    match = nmatch;
                }

                memcpy(match, pat->startp[0], patsize);
                match[patsize] = '\0';

                push_volatile_string(match);
                push_number(pat->startp[0] - start);
                call_lambda(subclosure, 2);
                transfer_svalue(&apply_return_value, inter_sp);
                inter_sp--;

                if (apply_return_value.type != T_STRING)
                {
                    xfree(buf);
                    if (pat)
                        REGFREE(pat);
                    if (match)
                        xfree(match);
                    error("Invalid type for string replacement\n");
                    /* NOTREACHED */
                    return NULL;
                }

                sub = apply_return_value.u.string;
            }

            /* Now what may have happen here. We *could*
             * be out of mem (as in 'space') or it could be
             * a regexp problem. the 'space' problem we
             * can handle, the regexp problem not.
             * hack: we store a \0 into *old ... if it is
             * still there on failure, it is a real failure.
             * if not, increase space. The player could get
             * some irritating messages from hs_regerror()
             */
            *old = '\0';
            while (NULL == (new = hs_regsub(pat, sub, new, space, 1)) )
            {
                int xold;

                if (!*old)
                {
                    xfree(buf);
                    if (pat)
                        REGFREE(pat);
                    if (match)
                        xfree(match);
                    error("Internal error in regreplace().\n");
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
        } while (  (flags & F_GLOBAL)
                 && !pat->reganch
                 && *curr != '\0'
                 && hs_regexec(pat,curr,start)
                );
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

    if (match)
        xfree(match);
    if (pat)
        REGFREE(pat);

    free_svalue(sp);
    sp--;
    free_svalue(sp);
    sp--;
    free_svalue(sp);
    sp--;
    free_svalue(sp);
    put_malloced_string(sp, string_copy(buf));
    xfree(buf);
    return sp;

#undef F_EXCOMPAT
#undef F_GLOBAL
#undef XREALLOC
}

/***************************************************************************/

