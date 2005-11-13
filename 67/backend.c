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
 * Timing is implemented this way: The driver usually stays in comm1.c,
 * waiting in 1 second intervals for incoming data. An alarm() is triggered
 * every 2 seconds and sets the flag variable comm_time_to_call_heart_beat.
 * comm1.c checks this variable every second and, it it is set, aborts its
 * input loop and returns here. To mark the cause of the return, the
 * variable time_to_call_heart_beat is set by comm1.c. The heartbeat
 * evaluation itself keeps an eye on comm_time_to_call_heart_beat so it
 * can terminate the evaluation if it takes too long. Unfortunately,
 * call out evaluation is only bound by the eval_cost limit.
 *
 * If MAX_TIME is defined and the variable max_time holds a positive value,
 * the backend also implements a watchdog counter 'eval_time', which is
 * incremented by every alarm. The evaluator monitors this variable and
 * aborts execution when it reaches 0. Parts of the driver which are
 * expected to take some time (compiling objects on a slow machine for
 * example) can freeze the watchdog for some time with the macros
 * SUSPEND_WATCHDOG and WAKE_WATCHDOG (the macros can be nested).
 * A CLEAR_EVAL_COST (which is part of the handling of an uncaught error)
 * will start the watchdog unconditionally.
 *
 * Also in this file (slighly misplaced) are the file handling efuns,
 * regreplace and the heartbeat code.
 *
 * TODO: Move the file efuns and the heart beat code into separate files.
 *---------------------------------------------------------------------------
 */

#include "driver.h"

#include <stddef.h>
#include <ctype.h>
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
 */

struct error_recovery_info toplevel_error_recovery_info
 = {
    (struct error_recovery_info*)0,
    ERROR_RECOVERY_NONE
 };

struct error_recovery_info *error_recovery_pointer
 = &toplevel_error_recovery_info;

/*-------------------------------------------------------------------------*/
mp_int current_time;
  /* The current time, updated every heart beat. */
  /* TODO: Should be time_t */

struct object *current_heart_beat = NULL;
  /* The object whose heart_beat() is currently executed.
   * interpret.c needs to know this for the heart-beat tracing, and
   * simulate.c test this in the error() function to react properly.
   */

/* TODO: BOOL */ int time_to_call_heart_beat;
  /* True: It's time to call the heart beat. Set by comm1.c when it recognizes
   *   an alarm(). */

volatile /* TODO: BOOL */ int comm_time_to_call_heart_beat = MY_FALSE;
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

int32 assigned_eval_cost; /* TODO: What is this? */

#ifdef MAX_TIME
volatile int eval_time;
  /* The number of seconds left for the current execution thread, stored
   * as negative value. This way costs can be counted up and the
   * exhaustion test is a simple test for >= 0.
   * However, see suspend_watchdog.
   */

uint suspend_watchdog = 0;
  /* When set to a value > 0, the eval_time counter is frozen at its
   * current value. This allows time consuming operations (like compilations
   * on slow machines) to proceed peacefully.
   * Change the value of this variable with the macros SUSPEND_WATCHDOG
   * and WAKE_WATCHDOG.
   */

#endif

#ifndef OLD_RESET

/* Define this macro to get lots of debug output. */
/* #define DEBUG_RESET */

uint num_listed_objs = 0;
  /* Number of objects in the object list.
   */

uint num_last_processed = 0;
  /* Number of object processed in last process_objects().
   */

double avg_last_processed = 0.0;
  /* Average number of object processed by process_objects(),
   * weighted of the last period of time.
   */

static mp_int num_hb_objs = 0;
  /* Number of objects with a heart beat.
   */

static mp_int hb_num_done;
  /* Number of heartbeats done in last process_objects().
   */

static long avg_num_hb_objs = 0;
static long avg_num_hb_done = 0;
  /* Decaying average of num_hb_objs and hb_num_done.
   */

#endif

/* TODO: BOOL */ int extra_jobs_to_do = MY_FALSE;
  /* True: the backend has other things to do in this cycle than just
   *   parsing commands or calling the heart_beat.
   */

/* TODO: BOOL */ int garbage_collect_to_do = MY_FALSE;
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

/*-------------------------------------------------------------------------*/

#ifdef OLD_RESET
/* The 'ring list' of objects with heart beats.
 *
 * Actually it is an array of <hb_max> (struct object *), pointing to the
 * objects. hb_list points to the beginning of the array, hb_tail to the
 * currently last used entry.
 *
 * hb_last_called and hb_last_to_call determine which objects' heart_beat()
 * needs to be called next. Depending on the context, hb_last_called ==
 * hb_last_to_call can mean 'all objects called' or 'no objects called'.
 *
 * In every round as many objects are called as possible, and the latter
 * two pointers are adjusted accordingly. This way, the next round can take
 * off where the first one left off.
 *
 * TODO: Consider changing the structure into a linked list similar to
 * TODO:: the call outs. Problematic are muds with both lots of heartbeat
 * TODO:: objects and lots of starts/stops.
 */

static struct object **hb_list = NULL; /* head of the array */
static struct object **hb_tail = NULL; /* for sane wrap around */

static struct object **hb_last_called, **hb_last_to_call;

static mp_int num_hb_objs = 0; /* current number of objects in list */
static mp_int num_hb_to_do;    /* number of objects to do this round */
static mp_int hb_num_done;     /* number of objects done this round */
static mp_int hb_max = 0;      /* max size of the allocated array */

static long num_hb_calls = 0; /* stats */
static long avg_num_hb_objs = 0, avg_num_hb_done = 0; /* decaying average */

#endif

/* --- Forward declarations --- */

#ifndef OLD_RESET
static void process_objects(/* TODO: BOOL */ short bAll);
#else
static void call_heart_beat (void);
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
 * stack machine execution (as stack will be cleared).
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
           /* This also starts the ongoing alarm() chain */
        current_time = get_current_time();
#ifdef OLD_RESET
        call_heart_beat();
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

#ifndef OLD_RESET
        /* Remove all destructed objects if the normal process_objects()
         * starts to lag behind.
         */
        if (new_destructed > num_listed_objs / 10)
            remove_destructed_objects();
#else
        remove_destructed_objects();
#endif

#if !defined(OLD_RESET) && defined(DEBUG)
        /* Check the sanity of the object list */
        /* TODO: Remove me for 3.2.6 */
        {
            struct object *ob;
            short bEndFound = MY_FALSE;
            p_int num_dest = 0;
            p_int num_found = 0;
            p_int num_hb = 0;

            for (ob = obj_list; ob; ob = ob->next_all)
            {
                num_found++;
                if (ob->flags & O_DESTRUCTED)
                    num_dest++;
                if (ob->flags & O_HEART_BEAT)
                {
                    num_hb++;
                    if (ob->flags & O_DESTRUCTED)
                        fatal("DEBUG: destructed obj (%p '%s') with heartbeat in the list.\n"
                             , ob, ob->name
                             );
                }
                if (ob == obj_list_end)
                {
                    bEndFound = MY_TRUE;
                    if (ob->next_all)
                        fatal("DEBUG: obj_list_end (%p '%s') is not the end of the list.\n"
                             , ob, ob->name
                             );
                }
            }
            if (num_dest != new_destructed)
                fatal("DEBUG: %ld destructed objects, %ld expected.\n"
                       , num_dest, new_destructed);
            if (obj_list && !bEndFound)
                fatal("DEBUG: obj_list_end (%p '%s') does not point into the object list.\n"
                     , obj_list_end, obj_list_end ? obj_list_end->name : "<null>");
            if (num_hb != num_hb_objs)
                fatal("DEBUG: %ld hb-objects in list, %ld expected.\n"
                       , num_hb, num_hb_objs);
            if (num_found != num_listed_objs)
                fatal("DEBUG: %ld objects in list, %ld expected.\n"
                       , num_found, num_listed_objs);
        }
#endif

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

            /* If a garbage collection is due, process all objects in
             * an attempt to cleanup/swap a good deal of them.
             */
            process_objects(garbage_collect_to_do);
              /* TODO: Swap after call_out(), so re-loading objects can be avoided? */
#else
            call_heart_beat();
            current_object = NULL;
            look_for_objects_to_swap();
              /* TODO: Swap after call_out(), so re-loading objects can be avoided? */
#endif
            call_out();

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

#ifdef MAX_TIME
ALARM_HANDLER(catch_alarm, comm_time_to_call_heart_beat = 1; total_alarms++; if (!suspend_watchdog && max_time > 0) eval_time += ALARM_TIME; alarm(ALARM_TIME); )
#else
ALARM_HANDLER(catch_alarm, comm_time_to_call_heart_beat = 1; total_alarms++; alarm(ALARM_TIME);)
#endif

#else

void catch_alarm (int dummy UNUSED)
{
#ifdef __MWERKS__
#    pragma unused(dummy)
#endif
    (void)signal(SIGALRM, (RETSIGTYPE(*)(int))catch_alarm);
    comm_time_to_call_heart_beat = 1;
    total_alarms++;
#ifdef MAX_TIME
    if (!suspend_watchdog && max_time > 0)
        eval_time += ALARM_TIME;
#endif
    alarm(ALARM_TIME);
}

#endif

/*-------------------------------------------------------------------------*/
#ifndef OLD_RESET

static void
process_objects (/* TODO: BOOL */ short bAll)

/* This function performs almost all of the background tasks in the
 * mud: removing destructed objects from the object list, resetting, 
 * cleaning and swapping objects, and calling heartbeats.
 *
 * It is called from the backend loop whenever one ALARM_TIME has
 * expired and tries to process as many objects as possible before
 * the next alarm arrives, starting from the head of the object
 * list. If the alarm arrives too soon, the function takes the list
 * of objects processed so far and moves them to the end of the
 * object list. This way the next call will continue with the yet
 * unprocessed batch of objects, eventually completing the round.
 *
 * However, if <bAll> is true, all objects are processed, no matter
 * how long it takes. This is used mainly when a garbage collection
 * due in order to clean/swap as many objects as possible.
 *
 * The functions in detail:
 *
 *  - If an object is flagged as 'destructed', it is removed from the
 *    object list and really deallocated. The backend loop keeps an
 *    eye on the number of such pending objects and calls
 *    remove_destructed_objects() if this function is too slow.
 *
 *  - If an object is flagged as having a heartbeat, it is performed.
 *
 *  - An object will be reset if it is found in a state of not being reset,
 *    not swapped out, and the delay to the next reset has passed.
 *
 *  - An object's clean_up() lfun will be called if the object has not
 *    been used for quite some time, and if it was not reset in this
 *    very round.
 *
 *  - An object's program and/or its variables will be swapped if it exists
 *    for at least time_to_swap(_variables) seconds since the last reference.
 *    To disable swapping, set the swapping times (either in config.h or per
 *    commandline option) to a value <= 0.
 *
 * The function maintains its own error recovery info so that errors
 * in heart_beat(), reset() or clean_up() won't mess up the handling.
 */

{
static mp_int last_time = 0;  /* Time of the last call */

static struct object *s_obj;
  /* Copy of the current object of <obj> in object list,
   * static so it can't be clobbered by errors.
   */

    struct object *obj;   /* Current object worked on */
    struct object *prev;  /* Predecessor of <obj> in object list. */

    struct error_recovery_info error_recovery_info;
      /* Local error recovery info */

#ifdef DEBUG_RESET
printf("DEBUG: process(): %ld: list %p '%s', end %p '%s'\n", tot_alloc_object, obj_list, obj_list->name, obj_list_end, obj_list_end->name);
#endif
    /* Housekeeping */

    time_to_call_heart_beat = MY_FALSE; /* interrupt loop if we take too long */
    comm_time_to_call_heart_beat = MY_FALSE;

    num_last_processed = 0;
    hb_num_done = 0;

    /* Objects can cause errors in their reset()/cleanup(), so we
     * save the current and the next object in order to restart
     * the loop after such an error.
     */

    obj = obj_list;
    prev = NULL;

    error_recovery_info.last = error_recovery_pointer;
    error_recovery_info.type = ERROR_RECOVERY_BACKEND;
    error_recovery_pointer = &error_recovery_info;

    if (setjmp(error_recovery_info.con.text))
    {
        clear_state();
        debug_message("Error in process_objects().\n");

        current_heart_beat = NULL;
        prev = s_obj;
        obj = s_obj->next_all;
#ifdef DEBUG_RESET
printf("DEBUG: process(): restart after error: obj %p '%s', prev %p '%s'\n", obj, obj ? obj->name : "<null>", prev, prev ? prev->name : "<null>");
#endif
    }

    /* The processing loop, runs until either time or objects
     * run short.
     *
     * On the begin of every iteration, <obj> is the new object
     * to process, and <prev> points to the one before.
     */
    while (NULL != obj && (bAll || !comm_time_to_call_heart_beat) )
    {
        int time_since_ref;
        /* TODO: BOOL */ short bResetCalled;

        num_last_processed++;

#ifdef DEBUG_RESET
printf("DEBUG: process: loop: %ld: obj %p '%s', prev %p '%s'\n", num_last_processed, obj, obj ? obj->name : "<null>", prev, prev ? prev->name : "<null>");
#endif
        /* Prepare against a runtime error */
        s_obj = obj;
        
        /* Time since last reference */
        time_since_ref = current_time - obj->time_of_ref;
        
        /* ------ Destructed Objects ------ */

        /* If the object is destructed, remove it from the list
         * and deallocate it.
         */
        if (obj->flags & O_DESTRUCTED)
        {
            struct object * next_obj = obj->next_all;
#ifdef DEBUG_RESET
printf("DEBUG: process(): remove destructed object %p '%s'\n", obj, obj->name ? obj->name : "<null>");
#endif

            /* Unlink the object from the list */
            if (prev)
                prev->next_all = next_obj;
            else
                obj_list = next_obj;

            if (obj == obj_list_end)
                obj_list_end = prev;

            num_listed_objs--;

            /* Get rid of the object, finally */
            destruct2(obj);
            new_destructed--;
#ifdef DEBUG_RESET
printf("DEBUG: process(): tot_alloc_object %ld\n", tot_alloc_object);
#endif
            /* Advance to next object, but keep prev where it is */
            obj = next_obj;
            continue;
        }

        /* ------ Heartbeat ------ */

        /* If this object is flagged having a heart beat, call it.
         */

        if (obj->flags & O_HEART_BEAT)
        {
#ifdef DEBUG_RESET
printf("DEBUG: process(): heartbeat %p '%s'\n", obj, obj->name ? obj->name : "<null>");
#endif
            hb_num_done++;
            current_interactive = NULL;

#ifdef DEBUG
            if (obj->flags & O_SWAPPED)
                fatal("Heart beat in swapped object.\n");
#endif

            if (obj->prog->heart_beat == -1)
            {
                /* Swapped? No heart_beat()-lfun? Turn off heartbeat
                 */
#ifdef DEBUG_RESET
printf("DEBUG: process(): no hb function in %p '%s'\n", obj, obj->name ? obj->name : "<null>");
#endif
                obj->flags &= ~O_HEART_BEAT;
                num_hb_objs--;
            }
            else
            {
                /* Prepare to call <ob>->heart_beat().
                 */
                current_prog = obj->prog;
                current_object = obj;
                current_heart_beat = obj;

                /* Determine the command_giver. Usually it's object
                 * itself, but it may be one of the shadows if there
                 * some. In any case, it must be a living.
                 */
                command_giver = obj;
                if (command_giver->flags & O_SHADOW)
                {
                    struct shadow_sentence *shadow_sent;

                    while (shadow_sent = O_GET_SHADOW(command_giver),
                           shadow_sent->shadowing)
                    {
                        command_giver = shadow_sent->shadowing;
                    }
                    if (!(command_giver->flags & O_ENABLE_COMMANDS))
                    {
                        command_giver = NULL;
                        trace_level = 0;
                    }
                    else
                    {
                        trace_level =
                          shadow_sent->type == SENT_INTERACTIVE ?
                            ((struct interactive *)shadow_sent)->trace_level : 0;
                    }
                }
                else
                {
                    if (!(command_giver->flags & O_ENABLE_COMMANDS))
                        command_giver = NULL;
                    trace_level = 0;
                }

                obj->user->heart_beats++;
                CLEAR_EVAL_COST;
                call_function(obj->prog, obj->prog->heart_beat);
    
                current_heart_beat = NULL;

            } /* hb function exists */

        } /* object with heart_beat */


        /* ------ Reset ------ */
        
        /* Check if a reset() is due. Objects which have not been touched
         * since the last reset just get a new due time set. Objects which
         * are not swapped in are not reset (the swapper will assign a new
         * due-time on swap-in).
         */

        bResetCalled = MY_FALSE;

        if (obj->time_reset
         && obj->time_reset < current_time
         && !(obj->flags & O_SWAPPED))
        {
            if (obj->flags & O_RESET_STATE)
            {
#ifdef DEBUG_RESET
printf("DEBUG: process(): resched reset object %p '%s'\n", obj, obj->name ? obj->name : "<null>");
#endif
                if (d_flag)
                    fprintf(stderr, "RESET (virtual) %s\n", obj->name);
                obj->time_reset = current_time+TIME_TO_RESET/2
                                             +random_number(TIME_TO_RESET/2);
            }
            else
            {
#ifdef DEBUG_RESET
printf("DEBUG: process(): reset object %p '%s'\n", obj, obj->name ? obj->name : "<null>");
#endif
                if (d_flag)
                    fprintf(stderr, "RESET %s\n", obj->name);
                CLEAR_EVAL_COST;
                command_giver = 0;
                trace_level = 0;
                reset_object(obj, H_RESET);
                if (obj->flags & O_DESTRUCTED)
                {
                    /* Don't advance obj/prev, so that the next
                     * iteration will get rid of this object
                     */
                    num_last_processed--;
                    continue;
                }
                bResetCalled = MY_TRUE;
            }
        }

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
        if (!bResetCalled
          && time_since_ref > TIME_TO_CLEAN_UP
          && (obj->flags & O_WILL_CLEAN_UP))
        {
            int was_swapped = obj->flags & O_SWAPPED ;
            int save_reset_state = obj->flags & O_RESET_STATE;
            struct svalue *svp;

            if (d_flag)
                fprintf(stderr, "clean up %s\n", obj->name);

            /* Supply a flag to the object that says if this program
             * is inherited by other objects. Cloned objects might as well
             * believe they are not inherited. Swapped objects will not
             * have a ref count > 1 (and will have an invalid ob->prog
             * pointer).
             */
            push_number(obj->flags & O_CLONE ? 0 :
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
            {
                /* Don't advance obj/prev, so that the next
                 * iteration will get rid of this object
                 */
                num_last_processed--;
                continue;
            }

            if ((!svp || (svp->type == T_NUMBER && svp->u.number == 0)) &&
                was_swapped )
                obj->flags &= ~O_WILL_CLEAN_UP;
            obj->flags |= save_reset_state;

no_clean_up: NOOP;
        }

#endif /* TIME_TO_CLEAN_UP > 0 */

#if TIME_TO_SWAP > 0 || TIME_TO_SWAP_VARIABLES > 0

        /* ------ Swapping ------ */
        
        /* At last, there is a possibility that the object can be swapped
         * out.
         */

        if ( (time_to_swap > 0 || time_to_swap_variables > 0)
         && !(obj->flags & O_HEART_BEAT))
        {

            if (time_since_ref >= time_to_swap_variables
             && time_to_swap_variables > 0)
            {
                if (!O_VAR_SWAPPED(obj))
                {
                    if (d_flag)
                        fprintf(stderr, "swap vars of %s\n", obj->name);
                    swap_variables(obj);
                }
            }

            if (time_since_ref >= time_to_swap
             && time_to_swap_variables > 0)
            {
                if (!O_PROG_SWAPPED(obj))
                {
                    if (d_flag)
                        fprintf(stderr, "swap %s\n", obj->name);
                    swap_program(obj);
                }
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

        /* Normal end of loop body: advance pointers */
        prev = obj;
        obj = obj->next_all;

    } /* End of loop */

#ifdef DEBUG_RESET
printf("DEBUG: process: loop end: %ld: obj %p '%s', prev %p '%s'\n"
      , num_last_processed
      , obj, obj ? obj->name : "<null>", prev, prev ? prev->name : "<null>");
#endif
    /* If we loop was aborted by a timeout, modify the head and tail
     * of the object list so that the next call will continue
     * where we left of now.
     */
    if (obj && prev && obj_list != obj_list_end)
    {
        /* We are really in the middle of a non-trivial object list */

        obj_list_end->next_all = obj_list;  /* make the list a ring */
        obj_list_end = prev;
        obj_list_end->next_all = NULL;      /* split it again */
        obj_list = obj;
#ifdef DEBUG_RESET
printf("DEBUG: process(): list modified: %p '%s', end %p '%s'\n", obj_list, obj_list->name, obj_list_end, obj_list_end->name);
#endif
    }

    /* Update the processing averages
     */
    if (last_time != current_time)
    {
        mp_int n;
        double c;

        /* General processing stats */
        
        n = current_time - last_time;
        if (n < (int) (sizeof consts / sizeof consts[0]) )
            c = consts[n];
        else
            c = exp(- n / 900.0);
        avg_last_processed =   c * avg_last_processed
                             + num_last_processed * (1 - c) / n;
        last_time = current_time;
        
        /* Heartbeat stats */
        avg_num_hb_objs += num_hb_objs - (avg_num_hb_objs >> 10);
        avg_num_hb_done += hb_num_done  - (avg_num_hb_done >> 10);
    }

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
#ifndef OLD_RESET
    if ( time_to_swap > 0
     && (time_to_swap_variables <= 0 || time_to_swap_variables > time_to_swap)
       )

        next_time = current_time + time_to_swap;
    else
        next_time = current_time + time_to_swap_variables;
#else
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
#endif

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

        next_ob = ob->next_all;
        if (ob->flags & O_DESTRUCTED) {
            continue;
        }

        /* Time since last reference */
        time_since_ref = current_time - ob->time_of_ref;

        /* Check if a reset() is due. Objects which have not been touched
         * since the last reset just get a new due time set. Objects which
         * are not swapped in are not reset (the swapper will assign a new
         * due-time on swap-in).
         */
        if (ob->next_reset < current_time && !(ob->flags & O_SWAPPED))
        {
            if (ob->flags & O_RESET_STATE)
                ob->next_reset = current_time+TIME_TO_RESET/2
                                             +random_number(TIME_TO_RESET/2);
            else
            {
                if (d_flag)
                    fprintf(stderr, "RESET %s\n", ob->name);
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
            push_number(ob->flags & O_CLONE ? 0 :
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

/*-------------------------------------------------------------------------*/
static void
call_heart_beat (void)

/* Call the heart_beat() lfun in all registered heart beat objects; or at
 * at least call as many as possible until the next alarm() timeout
 * occurs.
 *
 * If the object in question (or one of its shadows) is living, command_giver
 * is set to the object, else it is set to NULL. If there are no players
 * in the game, no heart_beat() will be called (but the call outs will!).
 */

{
    struct object *ob;

    /* Housekeeping */

    time_to_call_heart_beat = MY_FALSE; /* interrupt loop if we take too long */
    comm_time_to_call_heart_beat = MY_FALSE;

    current_interactive = NULL;

    /* Set this new round through the hb list */
    hb_last_to_call = hb_last_called;
    hb_num_done = 0;
    if (num_player > 0 && 0 != (num_hb_to_do = num_hb_objs))
    {
        num_hb_calls++;
        while (!comm_time_to_call_heart_beat)
        {
            /* Step to the next object to call, wrapping
             * around the end of the list if necessary.
             * Objects without hb lfun are skipped.
             */
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

            if (ob->prog->heart_beat == -1) {
                /* Swapped? No heart_beat()-lfun? TODO: Dunno */
                if (hb_num_done == num_hb_to_do)
                    break;
                continue;
            }

            /* Prepare to call <ob>->heart_beat().
             */
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
             * call set_heart_beat(0) in the heart beat, causing
             * hb_last_to_call to move.
             */
            if (hb_num_done == num_hb_to_do)
                break;
        } /* while (no timeout) */

        /* Update stats */
        avg_num_hb_objs += num_hb_to_do - (avg_num_hb_objs >> 10);
        avg_num_hb_done += hb_num_done  - (avg_num_hb_done >> 10);
    }
    current_heart_beat = 0;
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
    mp_int ix;
    mp_int num_prefiles;

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
void
remove_destructed_objects (void)

/* Remove all destructed objects with are kept pending for deallocation.
 *
 * TODO: Move this into simulate.c or object.c.
 */

{
#ifndef OLD_RESET
    struct object *ob, *prev;

#ifdef DEBUG_RESET
printf("DEBUG: remove_destructed: %d: list %p '%s', end %p '%s'\n", new_destructed, obj_list, obj_list ? obj_list->name: "<null>", obj_list_end, obj_list_end ? obj_list_end->name: "<null>" );
#endif
    for (prev = NULL, ob = obj_list; new_destructed && ob; )
    {
        if (ob->flags & O_DESTRUCTED)
        {
            struct object *next = ob->next_all;

#ifdef DEBUG_RESET
printf("DEBUG: remove %p '%s'\n", ob, ob->name ? ob->name : "<null>");
#endif
            /* Unlink the object from the list */
            if (prev)
                prev->next_all = next;
            else
                obj_list = next;

            if (ob == obj_list_end)
                obj_list_end = prev;

            num_listed_objs--;

            /* Get rid of the object, finally */
            destruct2(ob);
            new_destructed--;

            ob = next;
        }
        else
        {
            prev = ob;
            ob = ob->next_all;
        }
    }

#ifdef DEBUG_RESET
printf("DEBUG: remove_destructed done: %d: list %p '%s', end %p '%s'\n", new_destructed, obj_list, obj_list ? obj_list->name: "<null>", obj_list_end, obj_list_end ? obj_list_end->name: "<null>" );
#endif

#ifdef DEBUG
    if (new_destructed)
        fatal("%ld destructed objects not found in object list.\n"
             , new_destructed
             );
#endif
#else
    struct object *ob, **obp;

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
#endif
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

/*-------------------------------------------------------------------------*/
int
set_heart_beat (struct object *ob, /* TODO: BOOL */ int to)

/* EFUN set_heart_beat() and internal use.
 *
 * Add (<to> != 0) or remove (<to> == 0) object <ob> to/from the list
 * of heartbeat objects, thus activating/deactivating its heart beat.
 * Return 0 on failure (including calls for destructed objects or if
 * the object is already in the desired state) and 1 on success.
 *
 * The function is aware that it might be called from within a heart_beat()
 * and adjusts the correct pointers if that is so.
 */

{
    /* Safety checks */
    if (ob->flags & O_DESTRUCTED)
        return 0;
    if (to)
        to = O_HEART_BEAT; /* ...so that the following comparison works */
    if (to == (ob->flags & O_HEART_BEAT))
        return 0;

#ifndef OLD_RESET
    if (to)
    {
        num_hb_objs++;
        ob->flags |= O_HEART_BEAT;
    }
    else
    {
        num_hb_objs--;
        ob->flags &= ~O_HEART_BEAT;
    }
#else
    if (to)
    {

        /*
         * --- Add <ob> to the list of heartbeat objects ---
         *
         * The new object will be added right after hb_last_called,
         * and hb_last_called will then be set to point at it.
         */

        struct object **new_op;

        if (++num_hb_objs > hb_max) {

            /* We need to (re)allocate more memory for the array. */

            if (!hb_max) {

                /* First allocation */

                hb_max = 16;
                hb_list =
                  (struct object **)xalloc(hb_max * sizeof(struct object **));
                if (!hb_list) {
                    hb_max = 0;
                    return 0;
                }
                hb_last_called = hb_last_to_call = (hb_tail = hb_list) - 1;

            } else {

                /* Double the size of the current allocation */

                struct object **new;
                ptrdiff_t tail_diff, called_diff, to_call_diff;

                tail_diff    = hb_tail         - hb_list;
                called_diff  = hb_last_called  - hb_list;
                to_call_diff = hb_last_to_call - hb_list;

                hb_max <<= 1;
                new = (struct object **)
                  rexalloc((char *)hb_list, hb_max * sizeof(struct object **));
                if (!new) {
                    hb_max >>= 1;
                    return 0;
                }

                /* Adjust the hb_* ptr to point into the new memory block. */
                hb_list = new;
                hb_tail         = new + tail_diff;
                hb_last_called  = new + called_diff;
                hb_last_to_call = new + to_call_diff;
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
    }
    else {

        /*
         * --- Remove <ob> from the list of heartbeat objects ---
         */

        struct object **op;
        int active;

        ob->flags &= ~O_HEART_BEAT;

        /* Search the object in the list and remove it. */
        for (op = hb_list; *op != ob; op++) NOOP;
        move_memory(
          (char *)op,
          (char *)(op+1),
          (char *)hb_tail-- - (char *)(op+1)
        );

        /* Check which pointers need to be adjusted */
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
#endif

    /* That's it */
    return 1;
}

/*-------------------------------------------------------------------------*/
#ifdef MALLOC_smalloc

void
count_heart_beat_refs (void)

/* Count the reference to the hb_list array in a garbage collection.
 */

{
#ifdef OLD_RESET
    if (hb_list)
        note_malloced_block_ref((char *)hb_list);
#endif
}
#endif

/*-------------------------------------------------------------------------*/
int
heart_beat_status (int /* TODO: BOOL */ verbose)

/* If <verbose> is true, print the heart beat status information directly
 * to the current interactive user. In any case, return the amount of
 * memory used by the heart beat functions.
 */

{
    char buf[20];

    if (verbose) {
        add_message("\nHeart beat information:\n");
        add_message("-----------------------\n");
#ifndef OLD_RESET
        add_message("Number of objects with heart beat: %ld\n",
                    (long)num_hb_objs);
        add_message("HB calls completed in last cycle:  %ld (%.2f%%)\n"
                   , (long)hb_num_done
                   , 100.0 * (float)hb_num_done / (float)num_hb_objs
                   );
#else
        add_message("Number of objects with heart beat: %ld, starts: %ld, reserved %ld\n",
                    (long)num_hb_objs, (long)num_hb_calls, (long)hb_max);
#endif
        sprintf(buf, "%.2f",
          avg_num_hb_objs ?
            100 * (double) avg_num_hb_done / avg_num_hb_objs :
            100.0
        );
        add_message("Average of HB calls completed:     %s%%\n", buf);
    }
    return 0;
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

/* EFUN debug_message()
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

    file = check_valid_path(file, current_object, "write_file", 1);
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

    file = check_valid_path(file, current_object, "read_file", 0);
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

    file = check_valid_path(file, current_object, "read_bytes", 0);
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
    file = check_valid_path(file, current_object, "write_bytes", 1);
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

    file = check_valid_path(file, current_object, "file_size", 0);
    if (!file)
        return -1;
    if (ixstat(file, &st) == -1)
        return -1;
    if (S_IFDIR & st.st_mode)
        return -2;
    return st.st_size;
}

/*-------------------------------------------------------------------------*/
struct svalue *
f_heart_beat_info (struct svalue *sp)

/* EFUN heart_beat_info()
 *
 * Create a vector of all objects with a heart beat and push it
 * onto the stack. The resulting vector may be empty.
 *
 * This efun is expensive!
 * TODO: Invent something like a hash table where all objects are store
 * TODO:: which had a heartbeat at least once. Repeated starts and stops
 * TODO:: of the heartbeat would be cheap, also deletion when an object
 * TODO:: is destructed.
 */

{
    int i;
#ifndef OLD_RESET
    struct object *ob;
#else
    struct object *ob, **op;
#endif
    struct vector *vec;
    struct svalue *v;

    vec = allocate_array(i = num_hb_objs);
#ifndef OLD_RESET
    for (v = vec->item, ob = obj_list; i >= 0 && ob; ob = ob->next_all)
    {
        if (ob->flags & O_HEART_BEAT)
        {
#ifdef DEBUG
            if (ob->flags & O_DESTRUCTED)  /* Shouldn't happen. */
                continue;
#endif
            v->type = T_OBJECT;
            v->u.ob = ob;
            v++;
            i--;
            add_ref(ob, "heart_beat_info");
        }
    }
#else
    for (v = vec->item, op = hb_list; --i >= 0; v++) {
        v->type = T_OBJECT;
        v->u.ob = ob = *op++;
        add_ref(ob, "heart_beat_info");
    }
#endif
    sp++;
    sp->type = T_POINTER;
    sp->u.vec = vec;
    return sp;
}

/*-------------------------------------------------------------------------*/
struct svalue*
f_regreplace (struct svalue *sp)

/* EFUN regreplace()
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

