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
 *---------------------------------------------------------------------------
 */

#include "driver.h"
#include "typedefs.h"

#include "my-alloca.h"
#include <stddef.h>
#include <ctype.h>
#include <limits.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/time.h>
#include <signal.h>
#include <sys/times.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <time.h>
#include <math.h>

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
#include "mregex.h"
#include "mstrings.h"
#include "object.h"
#include "otable.h"
#include "random.h"
#include "simulate.h"
#include "stdstrings.h"
#include "svalue.h"
#include "swap.h"
#include "wiz_list.h"
#include "xalloc.h"

#include "i-eval_cost.h"

#include "../mudlib/sys/driver_hook.h"
#include "../mudlib/sys/debug_message.h"
#include "../mudlib/sys/signals.h"

/*-------------------------------------------------------------------------*/

mp_int current_time;
  /* The current time, updated every heart beat.
   * TODO: Should be time_t if it is given that time_t is always a skalar.
   */

mp_int time_of_last_hb = 0;
  /* For synchronous heart beats: the time of the last beat.
   */

Bool time_to_call_heart_beat = MY_FALSE;
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

uint num_last_data_cleaned = 0;
  /* Number of object data-cleaned in last process_objects().
   */

statistic_t stat_last_processed = { 0, 0, 0.0 };
statistic_t stat_last_data_cleaned = { 0, 0, 0.0 };
statistic_t stat_in_list = { 0, 0, 0.0 };
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

statistic_t stat_load = { 0, 0, 0.0 };
  /* The load average (player commands/second), weighted over the
   * last period of time.
   */

statistic_t stat_compile = { 0, 0, 0.0 };
  /* The average of compiled lines/second, weighted over the last period
   * of time.
   */

static time_t time_last_slow_shut = 0;
  /* Time of the last call to slow_shut_down(), to avoid repeated
   * calls while the previous ones are still working.
   */

static sigset_t pending_signals;
/* The pending signals which should be delivered to the mudlib master.
 */

/*-------------------------------------------------------------------------*/

/* --- Forward declarations --- */

static void process_objects(void);

/*-------------------------------------------------------------------------*/
void
update_statistic (statistic_t * pStat, long number)

/* Add the <number> to the statistics in <pStat> and update the weighted
 * average over the last period of time.
 */

{
    mp_int n;
    double c;

    pStat->sum += number;
    if (current_time == pStat->last_time)
        return;
    n = current_time - pStat->last_time;
    if (n < (int) (sizeof avg_consts / sizeof avg_consts[0]) )
        c = avg_consts[n];
    else
        c = exp(- n / 900.0);
    pStat->weighted_avg = c * pStat->weighted_avg + pStat->sum * (1 - c) / n;
    pStat->last_time = current_time;
    pStat->sum = 0;
} /* update_statistic() */

/*-------------------------------------------------------------------------*/
void
update_statistic_avg (statistic_t * pStat, long number)

/* Add the <number> to the statistics in <pStat> and update the weighted
 * average by degrading the previous value.
 */

{
    double c;

    pStat->sum += number;
    c = avg_consts[2];
    pStat->weighted_avg = (1 - c) * pStat->weighted_avg + number * c;
} /* update_statistic_avg() */

/*-------------------------------------------------------------------------*/
double
relate_statistics (statistic_t sStat, statistic_t sRef)

/* Express the <sStat>.weighted_avg as a multiple of <sRef>.weighted_avg
 * and return the factor.
 */

{
    if (sRef.weighted_avg == 0.0)
        return 0.0;

    return sStat.weighted_avg / sRef.weighted_avg;
} /* relate_statistics() */

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
    update_statistic(&stat_compile, lines);
} /* update_compile_av() */

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
    current_loc.file = NULL;
    current_object = NULL;
    command_giver = NULL;
    current_interactive = NULL;
    previous_ob = NULL;
    current_prog = NULL;
    reset_machine(MY_FALSE);   /* Pop down the stack. */
    num_warning = 0;
} /* clear_state() */

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
        (void)dump_trace(MY_TRUE, NULL);
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
static RETSIGTYPE
handle_signal (int sig)

/* General signal handler: store the signal in a flag
 * Note: If we receive the same signal again before it is processed, the second
 *       signal will be lost.
 */
{
    sigaddset(&pending_signals, sig);
    extra_jobs_to_do = MY_TRUE;
#ifndef RETSIGTYPE_VOID
    return 0;
#endif
} /* handle_signal() */

/*-------------------------------------------------------------------------*/
static INLINE Bool
defer_signal_to_master(int sig)
// Notifies master about the signal <sig>.
// Returns 1 if the master returns anything != 0. In this case it is assumed,
// the master handled the signal.
{
    svalue_t *res;
    push_number(inter_sp, sig);
    res = callback_master(STR_HANDLE_EXTERNAL_SIGNAL, 1);
    if (res && res->type == T_NUMBER && res->u.number != 0)
    {
        return MY_TRUE;
    }
    return MY_FALSE;
} /* defer_signal_to_master() */

/*-------------------------------------------------------------------------*/
static INLINE void
process_pending_signals (void)
/* processes the pending signals: SIGHUP, SIGUSR1, SIGUSR2, SIGINT
 * informs the master by applying handle_external_signal() in it and
 * handles the signal, if the master does not.
 */
{
    if (sigismember(&pending_signals, SIGTERM))
    {
        // SIGTERM: shutdown gracefully, but inform the mudlib master first.
        defer_signal_to_master(LPC_SIGTERM);
        game_is_being_shut_down = MY_TRUE;
        // ignore the rest of the signals
        sigemptyset(&pending_signals);
        return;
    }
    if (sigismember(&pending_signals, SIGHUP))
    {
        // SIGHUP: request a game shutdown.
        sigdelset(&pending_signals, SIGHUP);
        if (!defer_signal_to_master(LPC_SIGHUP))
        {
            // master did not handle it, shut down.
            extra_jobs_to_do = MY_TRUE;
            game_is_being_shut_down = MY_TRUE;
            // ignore the rest of the signals
            sigemptyset(&pending_signals);
            return;
        }
    }
    if (sigismember(&pending_signals, SIGUSR1))
    {
        // SIGUSR1: request a master update.
        sigdelset(&pending_signals, SIGUSR1);
        if (!defer_signal_to_master(LPC_SIGUSR1))
        {
            // Master did not handle it, update the master
            extra_jobs_to_do = MY_TRUE;
            master_will_be_updated = MY_TRUE;
            eval_cost += max_eval_cost >> 3;
        }
    }
    if (sigismember(&pending_signals, SIGUSR2))
    {
        // SIGUSR2: reopen the debug.log file.
        sigdelset(&pending_signals, SIGUSR2);
        if (!defer_signal_to_master(LPC_SIGUSR2))
        {
            // Master did not handle it, re-open the log
            reopen_debug_log = MY_TRUE;
        }
    }
    if (sigismember(&pending_signals, SIGINT))
    {
        // SIGINT: standard behaviour is process termination. If the mudlib
        // does not handle the signal, we send us the signal again (which terminates
        // us).
        sigdelset(&pending_signals, SIGINT);
        if (!defer_signal_to_master(LPC_SIGINT))
        {
            // if anything goes wrong, we terminate ourself, because it is the
            // standard behaviour in case of SIGINT.
            if (kill(getpid(), SIGINT))
                fatal("%s Could not send ourself the SIGINT signal: %s\n",
                      time_stamp(), strerror(errno));
                
            return;
        }
        // Otherwise we install our own handler again (the signal handler for
        // SIGINT is installed with the SA_RESETHAND flag and thus reset upon 
        // signal delivery)
        else
        {
            struct sigaction sa;
            sigemptyset(&sa.sa_mask);
            sa.sa_flags = SA_RESTART|SA_RESETHAND;
            sa.sa_handler = handle_signal;
            if (sigaction(SIGINT, &sa, NULL) == -1)
                debug_message("%s Unable to reinstall signal handler for SIGINT: %s",
                      time_stamp(), strerror(errno));
        }
    }
} // process_pending_signals

/*-------------------------------------------------------------------------*/
static INLINE void
cleanup_stuff (void)

/* Perform a number of clean up operations: replacing programs, freeing
 * driver hooks, and handling newly destructed objects.
 * They are collected in one function so that they can be easily called from
 * various places (backend loop and the process_objects loops); especially
 * since it is not advisable to remove destructed objects before replacing
 * the programs.
 *
 * This function does not call remove_destructed_objects() as this
 * call might become costly, and it is also sufficient to call it in the
 * context of process_objects() every couple of seconds.
 */

{
    int i;

    /* Reset the VM to avoid dangling references to invalid objects */
    clear_state();

    /* Replace programs */
    if (obj_list_replace)
    {
        replace_programs();
    }

    /* Rebind all bindable closures back to the master */
    for  (i = 0; i < NUM_DRIVER_HOOKS; i++)
    {
        if (driver_hook[i].type == T_CLOSURE
         && driver_hook[i].x.closure_type == CLOSURE_LAMBDA)
        {
            lambda_t *l;

            l = driver_hook[i].u.lambda;
            if (l->ob != master_ob)
            {
                free_object(l->ob, "backend");
                l->ob = ref_object(master_ob, "backend");
            }
        }
    }

    /* Finish up all newly destructed objects.
     */
    handle_newly_destructed_objects();
} /* cleanup_stuff() */

/*-------------------------------------------------------------------------*/
void install_signal_handlers()
/* Installs the signal handlers for those signals the driver responds to. */
{
    struct sigaction sa; // for installing the signal handlers

    // empty the pending signals
    sigemptyset(&pending_signals);

    sigemptyset(&sa.sa_mask);
    sa.sa_flags = SA_RESTART; // restart syscalls after handling a signal
 
    sa.sa_handler = catch_alarm;
    if (sigaction(SIGALRM, &sa, NULL) == -1)
        fatal("Could not install the alarm (SIGALRM) handler: %s\n",
              strerror(errno));

    // install the general signal handler for some signals
    // TODO: should we abort the startup if we can't install a handler?
    sa.sa_handler = handle_signal;
    if (sigaction(SIGHUP, &sa, NULL) == -1)
        perror("Unable to install signal handler for SIGHUP");
    if (sigaction(SIGUSR1, &sa, NULL) == -1)
        perror("Unable to install signal handler for SIGUSR1");
    if (sigaction(SIGUSR2, &sa, NULL) == -1)
        perror("Unable to install signal handler for SIGUSR2");
    // Profiling signal handler for the detection of long executions.
    sa.sa_handler = handle_profiling_signal;
    if (sigaction(SIGPROF, &sa, NULL) == -1) {
        profiling_timevalue.tv_sec = 0;
        profiling_timevalue.tv_usec = 0;
        debug_message("%s Unable to install signal handler for SIGPROF - profiling "
            "not available\n", time_stamp());
    }
    // for SIGTERM and SIGINT the default handler should be restored upon signal
    // delivery, so that a repeated signal is handled immediately with the 
    // default action and not only in the next backend cycle.
    sa.sa_flags |= SA_RESETHAND;
    if (sigaction(SIGINT, &sa, NULL) == -1)
        perror("Unable to install signal handler for SIGINT");
    if (sigaction(SIGTERM, &sa, NULL) == -1)
        perror("Unable to install signal handler for SIGTERM");

}
/*-------------------------------------------------------------------------*/
void
backend (void)

/* The backend loop, the backbone of the driver's operations.
 * It only returns when the game has to be shut down.
 */

{
    char buff[MAX_TEXT+4];
        /* Note that the size of buff[] is determined by MAX_TEXT, which
         * is the max size of the network receive buffer. IOW: no
         * buffer overruns are possible.
         */
    Bool prevent_object_cleanup;
        /* Implement a low/high water mark handling for the call to
         * cleanup_all_objects(), as it turns out that a single
         * cleanup doesn't always remove enough destructed objects.
         */
    
    /*
     * Set up.
     */

    prepare_ipc();

    if (!disable_timers_flag) {
        /* Setup the alarm timer. We set it up in a way that it expires on
         * full seconds to synchronize mud time to host time.
         */
        struct itimerval timer_value;
        struct timeval cur_time;
        timer_value.it_interval.tv_usec = 0;
        timer_value.it_value.tv_usec = 0;
        if (gettimeofday(&cur_time, NULL))
        {
            // no sync with host time.
            perror("Could not get current time with gettimeofday, host and mud time will be out of sync");
            timer_value.it_interval.tv_sec = alarm_time;
            timer_value.it_value.tv_sec = alarm_time;
        }
        else
        {
            // the first timer will be a little bit shorter (<=1s)
            timer_value.it_interval.tv_sec = alarm_time;
            timer_value.it_value.tv_sec = alarm_time - 1;
            timer_value.it_value.tv_usec = 1000000 - cur_time.tv_usec;
        }
        if(setitimer( ITIMER_REAL, &timer_value, NULL ))
        {
            fatal("Could not initialize the alarm timer: %s\n",
                  strerror(errno));
        }
        current_time = get_current_time();
    }

    printf("%s LDMud ready for users.\n", time_stamp());
    fflush(stdout);
    debug_message("%s LDMud ready for users.\n", time_stamp());

    toplevel_context.rt.type = ERROR_RECOVERY_BACKEND;
    setjmp(toplevel_context.con.text);

    /*
     * We come here after errors, and have to clear some global variables.
     */
    mark_end_evaluation();
    clear_state();
    flush_all_player_mess();
    prevent_object_cleanup = MY_FALSE;

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

        mud_is_up = MY_TRUE;

        /* Replace programs, remove destructed objects, and similar stuff */
        cleanup_stuff();

#ifdef DEBUG
        if (check_a_lot_ref_counts_flag)
            check_a_lot_ref_counts(NULL);
            /* after removing destructed objects! */
#endif

        /*
         * Do the extra jobs, if any.
         */

        check_for_out_connections();

        check_for_soft_malloc_limit();
        
        if (prevent_object_cleanup)
        {
            if (num_listed_objs >= num_destructed/2)
                prevent_object_cleanup = MY_FALSE;
        }
        else
        {
            if (num_listed_objs <= num_destructed)
            {
                cleanup_all_objects();
                prevent_object_cleanup = MY_TRUE;
            }
        }

        if (extra_jobs_to_do) {

            current_interactive = NULL;

            // process the pending signals we received since last time. Do
            // this first, because some flags may be set which cause some
            // extra jobs to done.
            process_pending_signals();

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
                               "time since last gc: %ld\n"
                             , time_stamp()
                             , gc_request == gcEfun ? "efun" : "allocator"
                             , slow_shut_down_to_do
                             , (long)(time_now - time_last_gc)
                             );
                  write(1, buf, strlen(buf));
                  command_giver = NULL;
                  current_object = NULL;
                  /* if the GC was not requested by an efun call, low_memory()
                   * in the master is called to inform the game. */
                  notify_lowmemory_condition(gc_request == gcEfun ?
                                             NO_MALLOC_LIMIT_EXCEEDED : 
                                             HARD_MALLOC_LIMIT_EXCEEDED);
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

                        push_number(inter_sp, minutes);
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
            update_statistic(&stat_load, 1);
#ifdef MALLOC_EXT_STATISTICS
            mem_update_stats();
#endif /* MALLOC_EXT_STATISTICS */

            /*
             * Now we have a string from the player. This string can go to
             * one of several places. If it is prepended with input escape
             * char, then it is an escape from the 'ed' editor, so we send it
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

            tracedepth = 0;

            mark_start_evaluation();

            if (buff[0] == input_escape
             && buff[1] != '\0'
               )
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
            else if (call_function_interactive(ip, buff))
                NOOP;
            else
                execute_command(buff, command_giver);

            mark_end_evaluation();

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
            /* No new message, just create the new time_stamp string in
             * the function's local buffer.
             */
            (void)time_stamp();
        } /* if (get_message()) */

        /* Do the periodic functions if it's time.
         */
        if (time_to_call_heart_beat)
        {
            struct timeval cur_time;
            gettimeofday(&cur_time, NULL);
            // Round the time. This prevents problems with tv_sec fluctuating between
            // values very near of whole seconds (e.g. .99999s and .000001s) which
            // leads to wrong timings in the backend (because it works with a granularity
            // of seconds). See bug #743.
            if (cur_time.tv_usec < 500000)
                current_time = cur_time.tv_sec;
            else
                current_time = cur_time.tv_sec + 1;
            
            
            current_object = NULL;

            /* Start the next alarm */
            comm_time_to_call_heart_beat = MY_FALSE;
            time_to_call_heart_beat = MY_FALSE;

            /* So call_outs from heart_beats are
             * correctly timed.
             */
            next_call_out_cycle();

            /* Do the timed events */
            if (!synch_heart_beats
                || time_of_last_hb + heart_beat_interval <= current_time)
            {
                do_state_check(2, "before heartbeat");
                call_heart_beat();
                time_of_last_hb = current_time;
            }

            do_state_check(2, "after heartbeat");
            call_out();
            do_state_check(2, "after call_out");

            /* Reset/cleanup/swap objects.
             */
            remove_destructed_objects(MY_FALSE);
            process_objects();
            do_state_check(2, "after swap/cleanup/reset");

            /* Other periodic processing */
            command_giver = NULL;
            trace_level = 0;
            wiz_decay();

            mem_consolidate(MY_FALSE);
        }

    } /* end of main loop */

    /* NOTREACHED */
} /* backend() */

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
 * This function should not really be necessary any more, because the alarm
 * set with setitimer automatically recurs.
 *
 * TODO: It should be possible to get rid of alarms altogether if all
 * TODO::timebound methods check the time since the last checkpoint.
 */

{
    static mp_int last_alarm_time = 0;
    mp_int curtime = get_current_time();

    if (disable_timers_flag)  /* Timing turned off? */
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
        struct itimerval timer_value = { {0,0}, {0,0} };
        struct timeval cur_time;
        debug_message("%s Last alarm was %"PRIdMPINT" seconds ago "
                      "- restarting it.\n",
                      time_stamp(), curtime - last_alarm_time);

        // stop alarm in case it is still alive, but just slow
        setitimer( ITIMER_REAL, &timer_value, NULL );

        // just to sure: re-install the handler. TODO: necessary?
        struct sigaction sa; // for installing the signal handlers
        sigemptyset(&sa.sa_mask);
        sa.sa_flags = SA_RESTART; // restart syscalls after handling a signal
        sa.sa_handler = catch_alarm;
        if (sigaction(SIGALRM, &sa, NULL) == -1)
            fatal("Could not re-install the alarm (SIGALRM) handler: %s\n",
                  strerror(errno));

        // setup new timer (to full seconds)
        if (gettimeofday(&cur_time, NULL))
        {
            // no sync with host time.
            perror("Could not get current time with gettimeofday, host and mud time will be out of sync");
            timer_value.it_interval.tv_sec = alarm_time;
            timer_value.it_value.tv_sec = alarm_time;
        }
        else
        {
            // the first timer will be a little bit shorter (<=1s)
            timer_value.it_interval.tv_sec = alarm_time;
            timer_value.it_value.tv_sec = alarm_time - 1;
            timer_value.it_value.tv_usec = 1000000 - cur_time.tv_usec;
        }

        if(setitimer( ITIMER_REAL, &timer_value, NULL )) {
          fatal("Could not re-initialize the alarm timer: %s\n",
                strerror(errno));
        }
        last_alarm_time = curtime; /* Since we just restarted it */

        comm_time_to_call_heart_beat = MY_TRUE;
        time_to_call_heart_beat = MY_TRUE;
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
 *  - If the object hasn't been data-cleaned for a sufficient time, it
 *    will be.
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
   *
   * The flags are used to make sure that at least one object is successfully
   * processed per call, even if there is no time left to begin with.
   */

    object_t *obj;               /* Current object worked on */
    long      limit_data_clean;  /* Max number of objects to dataclean */

    struct error_recovery_info error_recovery_info;
      /* Local error recovery info */

    /* Housekeeping */

    num_last_processed = 0;
    num_last_data_cleaned = 0;
    did_reset = MY_FALSE;
    did_swap = MY_FALSE;

    error_recovery_info.rt.last = rt_context;
    error_recovery_info.rt.type = ERROR_RECOVERY_BACKEND;
    rt_context = (rt_context_t *)&error_recovery_info.rt;

    if (setjmp(error_recovery_info.con.text))
    {
        mark_end_evaluation();
        clear_state();
        debug_message("%s Error in process_objects().\n", time_stamp());
    }

    /* Don't attempt to cleanup the fixed driver structures if we're
     * already out of time. But do do it before the regular loop
     * to ensure that they get cleaned up at least occasionally.
     */
    if (!comm_time_to_call_heart_beat)
        cleanup_driver_structures();

    /* Determine how many objects to data-clean at max in this cycle, since
     * sometimes hundreds of objects can become eligible at a time (despite
     * the random factor in the timing). The number is determined so that all
     * objects are cleaned within half of the configured cleanup period,
     * but at least the number of recently destructed objects in this cycle.
     */
    limit_data_clean = 2 * num_listed_objs / time_to_data_cleanup;
    if (limit_data_clean < 2)
        limit_data_clean = 2;
    if (limit_data_clean < num_newly_destructed)
        limit_data_clean = num_newly_destructed;

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

        clear_state();

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
                    fprintf(stderr, "%s RESET (virtual) %s\n", time_stamp(), get_txt(obj->name));
#endif
                obj->time_reset = current_time+time_to_reset/2
                                  +(mp_int)random_number((uint32)time_to_reset/2);
            }
            else if (!did_reset || !comm_time_to_call_heart_beat)
            {
#ifdef DEBUG
                if (d_flag)
                    fprintf(stderr, "%s RESET %s\n", time_stamp(), get_txt(obj->name));
#endif
                mark_start_evaluation();
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
                mark_end_evaluation();
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
                fprintf(stderr, "%s CLEANUP %s\n", time_stamp(), get_txt(obj->name));
#endif

            did_swap = MY_TRUE;

            /* Remove all pending destructed objects, to get a true refcount.
             * But make sure that we don't clobber anything else while
             * doing so.
             */
            cleanup_stuff();
            remove_destructed_objects(MY_FALSE);

            /* Supply a flag to the object that says if this program
             * is inherited by other objects. Cloned objects might as well
             * believe they are not inherited. Swapped objects will not
             * have a ref count > 1 (and will have an invalid ob->prog
             * pointer). If the object is a blueprint, the extra reference
             * from the program will not be counted.
             */
            if (obj->flags & (O_CLONE|O_REPLACED))
                push_number(inter_sp, 0);
            else if (O_PROG_SWAPPED(obj))
                push_number(inter_sp, 1);
            else if (obj->prog->blueprint == obj)
                push_number(inter_sp, obj->prog->ref - 1);
            else
                push_number(inter_sp, obj->prog->ref);

            RESET_LIMITS;
            CLEAR_EVAL_COST;
            command_giver = NULL;
            previous_ob = NULL;
            trace_level = 0;
            if (driver_hook[H_CLEAN_UP].type == T_CLOSURE)
            {
                lambda_t *l;

                mark_start_evaluation();
                l = driver_hook[H_CLEAN_UP].u.lambda;
                if (driver_hook[H_CLEAN_UP].x.closure_type == CLOSURE_LAMBDA)
                {
                    free_object(l->ob, "clean_up");
                    l->ob = ref_object(obj, "clean_up");
                }
                push_ref_object(inter_sp, obj, "clean up");
                call_lambda(&driver_hook[H_CLEAN_UP], 2);
                svp = inter_sp;
                pop_stack();
                mark_end_evaluation();
            }
            else if (driver_hook[H_CLEAN_UP].type == T_STRING)
            {
                mark_start_evaluation();
                svp = apply(driver_hook[H_CLEAN_UP].u.str, obj, 1);
                mark_end_evaluation();
            }
            else
            {
                pop_stack();
                goto no_clean_up;
            }
            if (obj->flags & O_DESTRUCTED)
            {
                continue;
            }

            if (!svp
             || (svp->type == T_NUMBER && svp->u.number == 0)
               )
                obj->flags &= ~O_WILL_CLEAN_UP;
            obj->flags |= save_reset_state;

no_clean_up:
            obj->time_of_ref = current_time;
                  /* in case the hook didn't update it */
        }


        /* ------ Data Cleanup ------ */

        /* Objects are processed at intervals determined by their
         * time to clean up (or DEFAULT_CLEANUP_TIME if the stored time
         * is 0).
         */
        if ( (num_last_data_cleaned == 0 || !comm_time_to_call_heart_beat)
         && (unsigned long)obj->time_cleanup < (unsigned long)current_time
         && num_last_data_cleaned < limit_data_clean
           )
        {
#ifdef DEBUG
            if (d_flag)
                fprintf(stderr, "%s DATA CLEANUP %s\n"
                              , time_stamp(), get_txt(obj->name));
#endif

            cleanup_object(obj);
            num_last_data_cleaned++;
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
                   fprintf(stderr, "%s swap vars of %s\n", time_stamp(), get_txt(obj->name));
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
                    fprintf(stderr, "%s swap %s\n", time_stamp(), get_txt(obj->name));
#endif
                swap_program(obj);
                if (O_PROG_SWAPPED(obj))
                    did_swap = MY_TRUE;
            }
        } /* if (obj can be swapped) */

    } /* End of loop */

    /* Update the processing averages
     */
    update_statistic(&stat_last_processed, num_last_processed);
    update_statistic(&stat_last_data_cleaned, num_last_data_cleaned);
    update_statistic(&stat_in_list, num_listed_objs);

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
    push_number(inter_sp, eflag);
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
    while ((size_t)(ix = ++ix0) < num_prefiles) {
        if (prefiles->item[ix].type != T_STRING)
            continue;

        RESET_LIMITS;
        CLEAR_EVAL_COST;
        push_ref_string(inter_sp, prefiles->item[ix].u.str);
        (void)apply_master(STR_PRELOAD, 1);

    }

    free_array(prefiles);
    toplevel_context.rt.type = ERROR_RECOVERY_NONE;
} /* preload_objects() */

/*=========================================================================*/

/*                               EFUNS                                     */

/*-------------------------------------------------------------------------*/
svalue_t *
v_garbage_collection (svalue_t *sp, int num_arg)

/* EFUN garbage_collection()
 *
 *   void garbage_collection ()
 *   void garbage_collection (string filename)
 *   void garbage_collection (string filename, int flag)
 *
 * Tell the parser to initiate a garbage collection after the
 * current execution ended.
 *
 * With the <filename> argument a log file for the GC output
 * different from the default log file can be specified.
 *
 * If <flag> is not given or 0, the output from the next
 * and only the next GC will go into the log file.
 * If <flag> is 1, the <filename> will be used as the new
 * default log file for all following GCs.
 */

{
    // check privilege violation first.
    svalue_t *fname = &const0;
    svalue_t *flag = NULL; // the second arg to priv_vio2 may be NULL.
    switch (num_arg)
    {
        case 2:
            flag = sp;
            fname = sp - 1;
            break;
        case 1:
            fname = sp;
            break;
        case 0:
            // NOOP
            break;
        default:
            // This should never happen.
            errorf("garbage_collection() called with more than 2 args.\n");
    }
    if (!privilege_violation2(STR_GARBAGE_COLLECTION, fname, flag, sp))
    {
        return pop_n_elems(num_arg, sp);
    }


    if (num_arg > 0)
    {
#ifdef GC_SUPPORT
        Bool change_default = MY_FALSE;
        svalue_t * argp = sp - num_arg + 1;
        int fd;
        string_t * path;

        if (num_arg > 1)
        {
            change_default = argp[1].u.number != 0;
        }

        restore_default_gc_log();

        path = check_valid_path( argp[0].u.str, current_object
                               , STR_GARBAGE_COLLECTION, MY_TRUE);
        if (path == NULL)
        {
            if (change_default)
            {
                errorf("Illegal use of garbage_collection(): "
                      "No privilege to use file '%s' as default log.\n"
                      , get_txt(argp[0].u.str));
            }
            else
            {
                errorf("Illegal use of garbage_collection(): "
                      "No privilege to use file '%s' as default log.\n"
                      , get_txt(argp[0].u.str));
            }
            /* NOTREACHED */
            return NULL;
        }

        if (change_default)
            fd = ixopen3(get_txt(path), O_CREAT|O_TRUNC|O_WRONLY, 0640);
        else
            fd = ixopen3(get_txt(path), O_CREAT|O_APPEND|O_WRONLY, 0640);

        if (fd < 0)
        {
            /* Store the <path> on the stack so the error handling
             * can free it.
             */
            inter_sp = ++sp; put_string(sp, path);
            errorf("Can't open GC log file '%s': errno %d\n"
                  , get_txt(path), errno);
            /* NOTREACHED */
            return sp;
        }

        set_cloexec_flag(fd);
        if (change_default)
            new_default_gc_log(fd);
        else
            gcollect_outfd = fd;

        /* Clean up */
        free_mstring(path);
#endif
        sp = pop_n_elems(num_arg, sp);
    }

    extra_jobs_to_do = MY_TRUE;
    gc_request = gcEfun;

    return sp;
} /* v_garbage_collection() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_query_load_average (svalue_t *sp)

/* EFUN query_load_average()
 *
 * Return a string with the current load_av and compile_av.
 * The string returned points to a local static buffer.
 */

{
    char buff[100];

#if defined(__MWERKS__) && !defined(WARN_ALL)
#    pragma warn_largeargs off
#endif
    sprintf(buff, "%.2f cmds/s, %.2f comp lines/s"
                , stat_load.weighted_avg
                , stat_compile.weighted_avg
                );
#if defined(__MWERKS__)
#    pragma warn_largeargs reset
#endif
    push_c_string(sp, buff);
    return sp;
} /* f_query_load_average() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_debug_message (svalue_t *sp)

/* EFUN debug_message()
 *
 *   debug_message(string text)
 *   debug_message(string text, int flags)
 *
 * Print the <text> to stdout, stderr, and/or the <host>.debug.log file.
 * The <text> may contain embedded \0 characters, which are ignored.
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
    mp_int  slen;
    char   *pTxt;

    if ((sp->u.number & ~(DMSG_STDOUT|DMSG_STDERR|DMSG_LOGFILE|DMSG_STAMP)) != 0)
        errorf("Argument 2 to debug_message() out of range: %ld, expected 0..15\n"
             , (long)sp->u.number);

    slen = (mp_int)mstrsize((sp-1)->u.str);
    pTxt = get_txt((sp-1)->u.str);

    if (sp->u.number & DMSG_STAMP)
    {
        if (!(sp->u.number & DMSG_TARGET) || (sp->u.number & DMSG_STDOUT))
            printf("%s ", time_stamp());
        if (sp->u.number & DMSG_STDERR)
            fprintf(stderr, "%s ", time_stamp());
        if (sp->u.number & DMSG_LOGFILE)
            debug_message("%s ", time_stamp());
    }

    while (slen > 0)
    {
        while (slen > 0 && *pTxt == '\0')
        {
            pTxt++;
            slen--;
        }
        if (slen > 0)
        {
            if (!(sp->u.number & DMSG_TARGET) || (sp->u.number & DMSG_STDOUT))
                fputs(pTxt, stdout);
            if (sp->u.number & DMSG_STDERR)
                fputs(pTxt, stderr);
            if (sp->u.number & DMSG_LOGFILE)
                debug_message("%s", pTxt);

            slen -= strlen(pTxt);
            pTxt += strlen(pTxt);
        }
    }

    free_svalue(sp);
    free_svalue(sp-1);
    return sp - 2;
} /* f_debug_message() */

/***************************************************************************/

