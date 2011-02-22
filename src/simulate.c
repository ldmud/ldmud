/*---------------------------------------------------------------------------
 * The runtime module.
 *
 *---------------------------------------------------------------------------
 * simulate is a collection of structures and functions which provide the
 * basic runtime functionality:
 *
 *   - the object list
 *   - loading, cloning, and destructing objects
 *   - the runtime context stack
 *   - error handling
 *   - function callbacks
 *   - management of the driver hooks
 *   - handling of object inventories and shadows.
 *
 * The data structures, especially the runtime stack, are described where
 * they are defined.
 *---------------------------------------------------------------------------
 */

#include "driver.h"
#include "typedefs.h"

#include "my-alloca.h"
#include <fcntl.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdarg.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>

/*-------------------------------------------------------------------------*/

#include "simulate.h"

#include "actions.h"
#include "array.h"
#include "backend.h"
#include "call_out.h"
#include "closure.h"
#include "comm.h"
#include "ed.h"
#include "filestat.h"
#include "gcollect.h"
#include "heartbeat.h"
#include "lex.h"
#include "main.h"
#include "mapping.h"
#include "mempools.h"
#include "mregex.h"
#include "mstrings.h"
#include "object.h"
#include "otable.h"
#ifdef USE_TLS
#include "pkg-tls.h"
#endif
#ifdef USE_SQLITE
#include "pkg-sqlite.h"
#endif
#include "prolang.h"
#include "sent.h"
#include "simul_efun.h"
#include "stdstrings.h"
#include "strfuns.h"
#ifdef USE_STRUCTS
#include "structs.h"
#endif
#include "swap.h"
#include "svalue.h"
#include "wiz_list.h"
#include "xalloc.h"

#include "i-eval_cost.h"

#include "../mudlib/sys/debug_info.h"
#include "../mudlib/sys/driver_hook.h"
#include "../mudlib/sys/files.h"
#include "../mudlib/sys/regexp.h"
#include "../mudlib/sys/rtlimits.h"

/*-------------------------------------------------------------------------*/

/* --- struct limits_context_s: last runtime limits context ---
 *
 * This structure saves the runtime limits on the runtime context stack.
 * It is also used as a temporary when parsing limit specifications.
 */

struct limits_context_s
{
    rt_context_t rt;     /* the rt_context superclass */
    size_t max_array;    /* max array size */
    size_t max_mapping;  /* max mapping size in values */
    size_t max_map_keys; /* max mapping size in entries */
    int32  max_eval;     /* max eval cost */
    int32  max_byte;     /* max byte xfer */
    int32  max_file;     /* max file xfer */
    int32  max_callouts; /* max callouts */
    int32  use_cost;     /* the desired cost of the evaluation */
    int32  eval_cost;    /* the then-current eval costs used */
    p_int  max_memory;   /* max memory newly allocated _per execution thread_ */
    p_int  used_memory;  /* the then-current memory allocation */
};


/* --- struct give_uid_error_context ---
 *
 * A structure of this type is pushed as error handler on the
 * interpreter stack while a newly created object is given its uids.
 */

struct give_uid_error_context
{
    error_handler_t head;  /* A T_ERROR_HANDLER with this struct as arg */
    object_t *new_object;  /* The object under processing */
};


/* --- struct namechain ---
 *
 * This structure is used by load_object() to build the current inheritence
 * chain in the frames on the stack. The information is used to generate
 * proper error messages.
 */

typedef struct namechain_s
{
    struct namechain_s * prev; /* Pointer to the previous element, or NULL */
    char               * name; /* Pointer to the name to load */
} namechain_t;

/*-------------------------------------------------------------------------*/

/* The runtime context stack.
 *
 * Runtime context informations are maintained in a linked list, with
 * cur_context pointing to the most recently pushed context.
 * From there, the links go back through the less recently pushed contexts
 * and end with the toplevel_context.
 */

struct error_recovery_info toplevel_context
 = {
     { NULL,
       ERROR_RECOVERY_NONE
     }
 };

rt_context_t * rt_context
 = (rt_context_t *)&toplevel_context.rt;

/*-------------------------------------------------------------------------*/

static p_int alloc_shadow_sent = 0;
  /* Statistic: how many shadow sentences have been allocated.
   */

object_t *obj_list = NULL;
  /* Head of the list of all objects. The reference by this list
   * is counted.
   * The first object in the list has its .prev_all member cleared.
   */

object_t *obj_list_end = NULL;
  /* Last object in obj_list. This object also has its .next_all member
   * cleared.
   */

#ifdef CHECK_OBJECT_REF
object_shadow_t * destructed_obj_shadows = NULL;
object_shadow_t * newly_destructed_obj_shadows = NULL;
#endif /* CHECK_OBJECT_REF */

object_t *destructed_objs = NULL;
  /* List holding destructed but not yet fully dereferenced objects.
   * Only the name and the program pointer are guarantueed to be valid.
   * The reference by this list is counted.
   * Objects with only the list reference left are finally freed by
   * the function remove_destructed_objects() called from the backend.
#ifdef GC_SUPPORT
   * They are also freed by a GC.
#endif
   * TODO: If this turns out to be not soon enough, modify the free_object()
   * TODO:: call to recognize the destructed+one-ref-left situation.
   *
   * This list is not exactly necessary, as destructed objects would be
   * deallcoated automatically once the last reference is gone, but it
   * helps mud admins to figure out where all the memory goes.
   */

long num_destructed = 0;
  /* Statistics: Number of objects in the destructed_objs list.
   */

object_t *newly_destructed_objs = NULL;
  /* List holding objects destructed in this execution thread.
   * They are no longer part of the obj_list, but since programs may still
   * be executing in them, the aren't fully destructed yet.
   */

long num_newly_destructed = 0;
  /* Statistics: Number of objects in the newly_destructed_objs list.
   */

uint32_t destructed_ob_counter = 0;
  /* this is incremented for each destructed object. The primary purpose is an
   * optimization for sizeof(): it has to check for destructed objects in
   * mappings only when this number here changed.
   * Note: this may (and probably will) overflow and wraparound.
   */

object_t *master_ob = NULL;
  /* The master object.
   */

object_t *current_object = NULL;
  /* The object interpreting a function.
   */

object_t *current_interactive;
  /* The user who caused this execution.
   */

object_t *previous_ob;
  /* The previous object which called the current_object.
   */

svalue_t driver_hook[NUM_DRIVER_HOOKS];
  /* The table with all driver hooks.
   */

Bool game_is_being_shut_down = MY_FALSE;
  /* TRUE if a shutdown was requested resp. is in progress.
   */

Bool master_will_be_updated = MY_FALSE;
  /* TRUE if a master-update was requested.
   */

static Bool in_fatal = MY_FALSE;
  /* TRUE if fatal() is being processed.
   */

int num_error = 0;
  /* Number of recursive calls to errorf().
   */

int num_warning = 0;
  /* Number of recursive calls to warnf().
   */

static char emsg_buf[ERROR_BUF_LEN];
  /* The buffer for the error message to be created.
   */

string_t *current_error;
string_t *current_error_file;
string_t *current_error_object_name;
mp_int    current_error_line_number;
  /* When an error occured during secure_apply(), these four
   * variables receive allocated copies (resp. counted refs) of
   * the error message, the name of the active program and object, and the
   * line number in the program.
   */

vector_t *uncaught_error_trace = NULL;
vector_t *current_error_trace = NULL;
  /* When an error occured, these variables hold the call chain in the
   * format used by efun debug_info() for evaluation by the mudlib.
   * The variables are kept until the next error, or until a GC.
   * 'uncaught_error_trace': the most recent uncaught error
   * 'current_error_trace': the most recent error, caught or uncaught.
   */

/* --- Runtime limits --- */

/* Each of these limits comes as pair: one def_... value which holds the
 * limit set at startup or with the set_limits() efun, and the max_...
 * value which holds the limit currently in effect. Before every execution,
 * max_... are initialised from def_... with the RESET_LIMITS macro.
 *
 * A limit of 0 usually means 'no limit'.
 */

size_t def_array_size = MAX_ARRAY_SIZE;
size_t max_array_size = MAX_ARRAY_SIZE;
  /* If != 0: the max. number of elements in an array.
   */

size_t def_mapping_size = MAX_MAPPING_SIZE;
size_t max_mapping_size = MAX_MAPPING_SIZE;
  /* If != 0: the max. number of elements in a mapping.
   */

size_t def_mapping_keys = MAX_MAPPING_KEYS;
size_t max_mapping_keys = MAX_MAPPING_KEYS;
  /* If != 0: the max. number of entries in a mapping.
   */

int32 def_eval_cost = MAX_COST;
int32 max_eval_cost = MAX_COST;
  /* The max eval cost available for one execution thread. Stored as negative
   * value for easier initialisation (see eval_cost).
   * CLEAR_EVAL_COST uses this value to re-initialize (assigned_)eval_cost.
   */

int32 use_eval_cost = DEF_USE_EVAL_COST;
  /* How to account for the cost of the current evaluation.
   * > 0: the cost to use regardless of actual cost.
   * == 0: use the actual cost if the max_eval limit was less than the
   *       default; use 10 ticks if it was more.
   * < 0: use -val% of the actual cost
   */

int32 def_byte_xfer = MAX_BYTE_TRANSFER;
int32 max_byte_xfer = MAX_BYTE_TRANSFER;
  /* Maximum number of bytes to read/write in one read/write_bytes() call.
   * If 0, it is unlimited.
   */

int32 def_file_xfer = READ_FILE_MAX_SIZE;
int32 max_file_xfer = READ_FILE_MAX_SIZE;
  /* Maximum number of bytes to read/write in one read/write_file() call.
   */

int32 def_callouts = MAX_CALLOUTS;
int32 max_callouts = MAX_CALLOUTS;
  /* If != 0: the max. number of callouts at one time.
   */

p_int def_memory = 0;
p_int max_memory = 0;
/* If != 0: the max. number of freshly allocated memory per execution thread.
 * By default there is no limit.
 */

/*-------------------------------------------------------------------------*/
/* Forward declarations */

static void free_shadow_sent (shadow_t *p);

/*-------------------------------------------------------------------------*/
Bool
catch_instruction ( int flags, uint offset
                  , volatile svalue_t ** volatile i_sp
                  , bytecode_p i_pc, svalue_t * i_fp
                  , int32 reserve_cost
#ifdef USE_NEW_INLINES
                  , svalue_t * i_context
#endif /* USE_NEW_INLINES */
                  )

/* Implement the F_CATCH instruction.
 *
 * At the time of call, all important locals from eval_instruction() are
 * have been stored in their global locations.
 *
 * Result is TRUE on a normal exit (error or not), and FALSE if the
 * guarded code terminated with a 'return' itself;
 *
 * Hard experience showed that it is advantageous to have setjmp()
 * to have its own stackframe, and call the longjmp() from a deeper
 * frame. Additionally it prevents over-optimistic optimizers from
 * removing vital reloads of possibly clobbered local variables after
 * the setjmp().
 */

{
#define INTER_SP ((svalue_t *)(*i_sp))

    Bool rc;
    volatile Bool old_out_of_memory = out_of_memory;

    bytecode_p new_pc;  /* Address of first instruction after the catch() */

    /* Compute address of next instruction after the CATCH statement.
     */
    new_pc = i_pc + offset;

    /* 'Fake' a subroutine call from <new_pc>
     */
#ifdef USE_NEW_INLINES
    push_control_stack(INTER_SP, new_pc, i_fp, i_context);
#else
    push_control_stack(INTER_SP, new_pc, i_fp);
#endif /* USE_NEW_INLINES */
    csp->ob = current_object;
    csp->extern_call = MY_FALSE;
    csp->catch_call = MY_TRUE;
#ifndef DEBUG
    csp->num_local_variables = 0;        /* No extra variables */
#else
    csp->num_local_variables = (csp-1)->num_local_variables;
      /* TODO: Marion added this, but why? For 'expected_stack'? */
#endif
    csp->funstart = csp[-1].funstart;

    /* Save some globals on the error stack that must be restored
     * separately after a longjmp, then set the jump.
     */
    if ( setjmp( push_error_context(INTER_SP, flags)->text ) )
    {
        /* A throw() or error occured. We have to restore the
         * control and error stack manually here.
         *
         * The error value to return will be stored in
         * the global <catch_value>.
         */
        svalue_t *sp;
        svalue_t catch_value;

        /* Remove the catch context and get the old stackpointer setting */
        sp = pull_error_context(INTER_SP, &catch_value);

        /* beware of errors after set_this_object() */
        current_object = csp->ob;

        /* catch() faked a subroutine call internally, which has to be
         * undone again. This will also set the pc to the proper
         * continuation address.
         */
        pop_control_stack();

        /* Push the catch return value */
        *(++sp) = catch_value;

        *i_sp = (volatile svalue_t *)sp;

        /* Restore the old eval costs */
        eval_cost -= reserve_cost;
        assigned_eval_cost -= reserve_cost;

        /* If we ran out of memory, throw a new error */
        if (!old_out_of_memory && out_of_memory)
        {
            errorf("(catch) Out of memory detected.\n");
        }

        rc = MY_TRUE;
    }
    else
    {

        /* Increase the eval_cost for the duration of the catch so that
         * there is enough time left to handle an eval-too-big error.
         * Do this before the check as the error handling will subtract
         * the reserve again.
         */
        eval_cost += reserve_cost;
        assigned_eval_cost += reserve_cost;

        if (max_eval_cost && eval_cost >= max_eval_cost)
        {
            errorf("Not enough eval time left for catch(): required %"PRId32
                   ", available %"PRId32"\n", reserve_cost, 
                   (max_eval_cost - eval_cost + reserve_cost)
                 );
            /* NOTREACHED */
            return MY_TRUE;
        }

        /* Recursively call the interpreter */
        rc = eval_instruction(i_pc, INTER_SP);

        if (rc)
        {
            /* Get rid of the code result */
            pop_stack();

            /* Restore the old execution context */
            pop_control_stack();
            pop_error_context();

            /* Since no error happened, push 0 onto the stack */
            push_number(inter_sp, 0);
        }

        eval_cost -= reserve_cost;
        assigned_eval_cost -= reserve_cost;
    }

    return rc;
} /* catch_instruction() */

/*-------------------------------------------------------------------------*/
static INLINE void
save_limits_context (struct limits_context_s * context)

/* Save the current limits context into <context> (but don't put it
 * onto the context stack).
 */

{
    context->rt.type = LIMITS_CONTEXT;
    context->max_array = max_array_size;
    context->max_callouts = max_callouts;
    context->max_mapping = max_mapping_size;
    context->max_map_keys = max_mapping_keys;
    context->max_eval = max_eval_cost;
    context->eval_cost = eval_cost;
    context->max_byte = max_byte_xfer;
    context->max_file = max_file_xfer;
    context->use_cost = use_eval_cost;
    context->max_memory = max_memory;
    context->used_memory = used_memory_at_eval_start;
    
} /* save_limits_context() */

/*-------------------------------------------------------------------------*/
static INLINE void
restore_limits_context (struct limits_context_s * context)

/* Restore the last runtime limits from <context>.
 *
 * Restoring max_eval_cost is a bit tricky since eval_cost
 * itself might be a bit too high for the restored limit, but
 * avoiding a 'eval-cost too high' was the point of the exercise
 * in the first place. Therefore, if we ran under a less limited
 * eval-cost limit, we fake an effective cost of 10 ticks.
 */

{
    assign_eval_cost();
    if (use_eval_cost == 0)
    {
        if (!max_eval_cost || max_eval_cost > context->max_eval)
        {
            assigned_eval_cost = eval_cost = context->eval_cost+10;
        }
    }
    else if (use_eval_cost > 0)
    {
        int32 elapsed_cost = eval_cost - context->eval_cost; 

        if (elapsed_cost > use_eval_cost)
            assigned_eval_cost = eval_cost = use_eval_cost + context->eval_cost;
        assigned_eval_cost = eval_cost;
    }
    else /* (use_eval_cost < 0) */
    {
        int32 elapsed_cost = eval_cost - context->eval_cost; 
        int32 whole_fact = (-use_eval_cost) / 100;
        int32 fract_fact = (-use_eval_cost) % 100;
        eval_cost =   context->eval_cost
                    + elapsed_cost * whole_fact 
                    + elapsed_cost * fract_fact / 100;
        assigned_eval_cost = eval_cost;
    }
    max_array_size = context->max_array;
    max_mapping_size = context->max_mapping;
    max_mapping_keys = context->max_map_keys;
    max_callouts = context->max_callouts;
    max_eval_cost = context->max_eval;
    max_byte_xfer = context->max_byte;
    max_file_xfer = context->max_file;
    use_eval_cost = context->use_cost;
    max_memory = context->max_memory;
    used_memory_at_eval_start = context->used_memory;
    
} /* restore_limits_context() */

/*-------------------------------------------------------------------------*/
static void
unroll_context_stack (void)

/* Remove entries from the rt_context stack until the last entry
 * is an ERROR_RECOVERY context.
 */

{
    while (!ERROR_RECOVERY_CONTEXT(rt_context->type))
    {
        rt_context_t * context = rt_context;

        rt_context = rt_context->last;
        switch(context->type)
        {
        case COMMAND_CONTEXT:
            restore_command_context(context);
            break;

        case LIMITS_CONTEXT:
            restore_limits_context((struct limits_context_s *)context);
            break;

        default:
            fatal("Unimplemented context type %d.\n", context->type);
            /* NOTREACHED */
        }
    }
} /* unroll_context_stack() */

/*-------------------------------------------------------------------------*/
static INLINE void dump_core(void) NORETURN;

static INLINE void
dump_core(void)

/* A wrapper around abort() to make sure that we indeed dump a core.
 */

{
    /* we want a core dump, and abort() seems to fail for linux and sun */
    (void)signal(SIGFPE, SIG_DFL);
    {
        int a = 0;  /* avoids a pesky diagnostic */
        *((char*)0) = 0/a;
        *((char*)fatal) = 0/a;
    }
    abort();
} /* dump_core() */

/*-------------------------------------------------------------------------*/
void
fatal (const char *fmt, ...)

/* A fatal error occured. Generate a message from printf-style <fmt>, including
 * a timestamp, dump the backtrace and abort.
 */

{
    va_list va;
    char *ts;

    /* Prevent double fatal. */
    if (in_fatal)
    {
        dump_core();
    }
    in_fatal = MY_TRUE;

    ts = time_stamp();

    va_start(va, fmt);

    fflush(stdout);
    fprintf(stderr, "%s ", ts);
    vfprintf(stderr, fmt, va);
    fflush(stderr);
    if (current_object)
        fprintf(stderr, "%s Current object was %s\n"
                      , ts, current_object->name
                            ? get_txt(current_object->name) : "<null>");
    debug_message("%s ", ts);
    vdebug_message(fmt, va);
    if (current_object)
        debug_message("%s Current object was %s\n"
                     , ts, current_object->name
                           ? get_txt(current_object->name) : "<null>");
    debug_message("%s Dump of the call chain:\n", ts);
    (void)dump_trace(MY_TRUE, NULL);
    printf("%s LDMud aborting on fatal error.\n", time_stamp());
    fflush(stdout);

    sleep(1); /* let stdout settle down... abort can ignore the buffer... */

    va_end(va);

    /* Before shutting down, try to inform the game about it */
    push_ref_string(inter_sp, STR_FATAL_ERROR);
    callback_master(STR_NOTIFY_SHUTDOWN, 1);

    /* Mandatory cleanups */
#ifdef USE_TLS
    tls_global_deinit();
#endif

    /* Dump core and exit */
    dump_core();
} /* fatal() */

/*-------------------------------------------------------------------------*/
char *
limit_error_format (char *fixed_fmt, size_t fixed_fmt_len, const char *fmt)

/* Safety function for error messages: in the error message <fmt>
 * every '%s' spec is changed to '%.200s' to avoid buffer overflows.
 * The modified format string is stored in <fixed_fmt> (a caller provided
 * buffer of size <fixed_fmd_len>) which is also returned as result.
 */

{
    char *ffptr;

    ffptr = fixed_fmt;
    while (*fmt && ffptr < fixed_fmt + fixed_fmt_len-1)
    {
      if ((*ffptr++=*fmt++)=='%')
      {
        if (*fmt == 's')
        {
          *ffptr++ = '.';
          *ffptr++ = '2';
          *ffptr++ = '0';
          *ffptr++ = '0';
        }
      }
    }

    if (*fmt)
    {
        /* We reached the end of the fixed_fmt buffer before
         * the <fmt> string was complete: mark this error message
         * as truncated.
         * ffptr points to the last byte in the <fixed_fmt> buffer.
         */
        ffptr[-3] = '.';
        ffptr[-2] = '.';
        ffptr[-1] = '.';
    }

    *ffptr = '\0';
    return fixed_fmt;
} /* limit_error_format() */

/*-------------------------------------------------------------------------*/
void
errorf (const char *fmt, ...)

/* A system runtime error occured: generate a message from printf-style
 * <fmt> with a timestamp, and handle it.
 * If the error is caught, just dump the trace on stderr, and jump to the
 * error handler, otherwise call the mudlib's error functions (this may cause
 * recursive calls to errorf()) and jump back to wherever the current error
 * recovery context points to.
 *
 * The runtime context stack is unrolled as far as necessary.
 * TODO: Add a perrorf(<prefmt>, <postfmt>,...) function which translates the
 * TODO:: errno into a string and calls errorf(<prefmt><errmsg><postfmt>, ...).
 */

{
    rt_context_t *rt;
    string_t *object_name = NULL;
    char     *ts;
    svalue_t *svp;
    Bool      error_caught;
      /* TRUE: User catches this error.
       */
    Bool      published_catch;
      /* TRUE: this is a catch which wants runtime_error to be called
       */
    Bool      do_save_error;
    string_t *file;                  /* program name */
    string_t *malloced_error;        /* copy of emsg_buf+1 */
    string_t *malloced_file = NULL;  /* copy of program name */
    string_t *malloced_name = NULL;  /* copy of the object name */
    object_t *curobj = NULL;         /* Verified current object */
    char      fixed_fmt[ERROR_FMT_LEN];
      /* Note: When changing this buffer, also change the HEAP_STACK_GAP
       * limit in xalloc.c!
       */
    mp_int    line_number = 0;
    va_list   va;

    /* Errors during the fatal() processing will abort the process
     * immediately.
     */
    if (in_fatal)
        fatal("Error during fatal().");

    ts = time_stamp();

    /* Find the last error recovery context, but do not yet unroll
     * the stack: the current command context might be needed
     * in the runtime error apply.
     */
    for ( rt = rt_context
        ; !ERROR_RECOVERY_CONTEXT(rt->type)
        ; rt = rt->last) NOOP;

    va_start(va, fmt);

    /* Make fmt sane */
    fmt = limit_error_format(fixed_fmt, sizeof(fixed_fmt), fmt);

    /* Check the current object */
    curobj = NULL;
    if (current_object != NULL
     && current_object != &dummy_current_object_for_loads)
        curobj = current_object;

    if (curobj)
        assign_eval_cost();

    /* We allow recursive errors only from "sensitive" environments.
     */
    if (num_error && rt->type <= ERROR_RECOVERY_APPLY)
    {
        static char *times_word[] = {
          "",
          "Double",
          "Triple",
          "Quadruple",
        };
        debug_message("%s %s fault, last error was: %s"
                     , ts, times_word[num_error]
                     , emsg_buf + 1
        );
    }

    /* Generate the error message */
    vsprintf(emsg_buf+1, fmt, va);
    va_end(va);

    emsg_buf[0] = '*';  /* all system errors get a * at the start */

    error_caught = MY_FALSE;
    published_catch = MY_FALSE;

    if (rt->type >= ERROR_RECOVERY_CATCH)
    {
        /* User catches this error */

        error_caught = MY_TRUE;

        /* Try to copy the error message into the catch value.
         * If we run out of memory here, we won't execute the catch.
         */
        {
            string_t * str = new_mstring(emsg_buf);

            if (NULL != str)
            {
                svalue_t stmp;

                put_string(&stmp, str);
                transfer_error_message(&stmp, rt);
            }
            else
            {
                error_caught = MY_FALSE;

                /* Unroll the  context stack even further until the
                 * previous non-catch error recovery frame.
                 */
                for ( 
                    ; !ERROR_RECOVERY_CONTEXT(rt->type)
                       && rt->type >= ERROR_RECOVERY_CATCH
                    ; rt = rt->last) NOOP;
            }
        }
    }

    if (error_caught)
    {
        struct error_recovery_info * eri = (struct error_recovery_info *)rt;

        published_catch = (eri->flags & CATCH_FLAG_PUBLISH);

        if (!out_of_memory)
        {
            if (!(eri->flags & CATCH_FLAG_NOLOG))
            {
                /* Even though caught, dump the backtrace - it makes mudlib
                 * debugging much easier.
                 */
                debug_message("%s Caught error: %s", ts, emsg_buf + 1);
                printf("%s Caught error: %s", ts, emsg_buf + 1);
                if (current_error_trace)
                {
                    free_array(current_error_trace);
                    current_error_trace = NULL;
                }
                object_name = dump_trace(MY_FALSE, &current_error_trace);
                debug_message("%s ... execution continues.\n", ts);
                printf("%s ... execution continues.\n", ts);
            }
            else
            {
                /* No dump of the backtrace into the log, but we want it
                 * available for debug_info().
                 */
                if (current_error_trace)
                {
                    free_array(current_error_trace);
                    current_error_trace = NULL;
                }
                object_name = collect_trace(NULL, &current_error_trace);
            }
        }
        else /* We're running low on memory. */
        {
            if (current_error_trace)
            {
                free_array(current_error_trace);
                current_error_trace = NULL;
            }
            object_name = STR_UNKNOWN_OBJECT;
        }

        if (!published_catch)
        {
            unroll_context_stack();
            longjmp(((struct error_recovery_info *)rt_context)->con.text, 1);
            fatal("Catch() longjump failed");
        }
    }

    /* Error not caught by the program, or catch() requests the
     * runtime_error() is to be called.
     */

    num_error++;
    if (num_error > 3)
        fatal("Too many simultaneous errors.\n");

    debug_message("%s ", ts);
    debug_message("%s", emsg_buf+1);

    do_save_error = MY_FALSE;

    /* Get a copy of the error message */
    malloced_error = new_mstring(emsg_buf+1);

    /* If we have a current_object, determine the program location
     * of the fault.
     */
    if (curobj)
    {
        line_number = get_line_number_if_any(&file);
        debug_message("%s program: %s, object: %s line %"PRIdMPINT"\n"
                     , ts, get_txt(file), get_txt(curobj->name)
                     , line_number);
        if (current_prog && num_error < 3)
        {
            do_save_error = MY_TRUE;
        }

        malloced_file = file; /* Adopt reference */
        malloced_name = ref_mstring(curobj->name);
    }

    /* On a triple error, duplicate the error messages so far on stdout */

    if (num_error == 3)
    {
        /* Error context is secure_apply() */

        printf("%s error in function call: %s", ts, emsg_buf+1);
        if (curobj)
        {
            printf("%s program: %s, object: %s line %"PRIdMPINT"\n"
                  , ts, get_txt(file), get_txt(curobj->name)
                  , line_number
                  );
        }
    }

    /* Dump the backtrace (unless already done) */
    if (!published_catch)
    {
        if (uncaught_error_trace)
        {
            free_array(uncaught_error_trace);
            uncaught_error_trace = NULL;
        }
        if (current_error_trace)
        {
            free_array(current_error_trace);
            current_error_trace = NULL;
        }

        object_name = dump_trace(num_error == 3, &current_error_trace);
        if (!published_catch)
            uncaught_error_trace = ref_array(current_error_trace);
        fflush(stdout);
    }

    if (rt->type == ERROR_RECOVERY_APPLY)
    {
        /* Error context is secure_apply() */

        current_error = malloced_error;
        current_error_file = malloced_file;
        current_error_object_name = malloced_name;
        current_error_line_number = line_number;

        if (out_of_memory)
        {
            if (malloced_error)
            {
                free_mstring(malloced_error);
                malloced_error = NULL;
            }
            if (malloced_file)
            {
                free_mstring(malloced_file);
                malloced_file = NULL;
            }
            if (malloced_name)
            {
                free_mstring(malloced_name);
                malloced_name = NULL;
            }
            if (current_error_trace)
            {
                free_array(current_error_trace);
                current_error_trace = NULL;
            }
            if (uncaught_error_trace)
            {
                free_array(uncaught_error_trace);
                uncaught_error_trace = NULL;
            }
        }
        unroll_context_stack();
        longjmp(((struct error_recovery_info *)rt_context)->con.text, 1);
    }

    /* If the error is not caught at all, the stack must be brought in a
     * usable state. After the call to reset_machine(), all arguments to
     * errorf() are invalid, and may not be used any more. The reason is that
     * some strings may have been on the stack machine stack, and have been
     * deallocated.
     */

    if (!published_catch)
        reset_machine(MY_FALSE);

    if (do_save_error)
    {
        save_error(emsg_buf, get_txt(file), line_number);
    }

    if (object_name)
    {
        /* Error occured in a heart_beat() function */

        object_t *ob;

        ob = find_object(object_name);
        if (!ob)
        {
            if (command_giver && num_error < 2)
                add_message("error when executing program in destroyed object %s\n",
                            get_txt(object_name));
            debug_message("%s error when executing program in destroyed object %s\n"
                         , ts, get_txt(object_name));
        }
    }

    if (num_error == 3)
    {
        debug_message("%s Master failure: %s", ts, emsg_buf+1);
        printf("%s Master failure: %s", ts, emsg_buf+1);
    }
    else if (!out_of_memory)
    {
        /* We have memory: call master:runtime(), and maybe
         * also master:heart_beat_error().
         */

        int a;
        object_t *save_cmd;
        object_t *culprit = NULL;


        if (!published_catch)
        {
            CLEAR_EVAL_COST;
            RESET_LIMITS;
        }

        push_ref_string(inter_sp, malloced_error);
        a = 1;
        if (curobj)
        {
            push_ref_string(inter_sp, malloced_file);
            push_ref_string(inter_sp, malloced_name);
            push_number(inter_sp, line_number);
            a += 3;
        }

        if (current_heart_beat)
        {
            /* Heartbeat error: turn off the heartbeat in the object
             * and also pass it to RUNTIME_ERROR.
             */

            culprit = current_heart_beat;
            current_heart_beat = NULL;
            set_heart_beat(culprit, MY_FALSE);
            debug_message("%s Heart beat in %s turned off.\n"
                         , time_stamp(), get_txt(culprit->name));
            push_ref_valid_object(inter_sp, culprit, "heartbeat error");
            a++;
        }
        else
        {
            if (!curobj)
            {
                /* Push dummy values to keep the argument order correct */
                push_number(inter_sp, 0);
                push_number(inter_sp, 0);
                push_number(inter_sp, 0);
                a += 3;
            }

            /* Normal error: push -1 instead of a culprit. */
            push_number(inter_sp, -1);
            a++;
        }

        push_number(inter_sp, error_caught ? 1 : 0);
        a++;

        save_cmd = command_giver;
        apply_master(STR_RUNTIME, a);
        command_giver = save_cmd;

        if (culprit)
        {
            /* TODO: Merge heart_beat_error() in to runtime_error() */

            /* Heartbeat error: call the master to log it
             * and to see if the heartbeat shall be turned
             * back on for this object.
             */

            push_ref_valid_object(inter_sp, culprit, "runtime_error");
            push_ref_string(inter_sp, malloced_error);
            a = 2;
            if (curobj)
            {
                push_ref_string(inter_sp, malloced_file);
                push_ref_string(inter_sp, malloced_name);
                push_number(inter_sp, line_number);
                a += 3;
            }

            push_number(inter_sp, error_caught ? 1 : 0);
            a++;

            svp = apply_master(STR_HEART_ERROR, a);
            command_giver = save_cmd;
            if (svp && (svp->type != T_NUMBER || svp->u.number) )
            {
                debug_message("%s Heart beat in %s turned back on.\n"
                             , time_stamp(), get_txt(culprit->name));
                set_heart_beat(culprit, MY_TRUE);
            }
        }

        /* Handling errors is expensive! */
        if (!published_catch)
            assigned_eval_cost = eval_cost += MASTER_RESERVED_COST;
    }

    /* Clean up */
    if (malloced_error)
    {
        free_mstring(malloced_error);
        malloced_error = NULL;
    }
    if (malloced_file)
    {
        free_mstring(malloced_file);
        malloced_file = NULL;
    }
    if (malloced_name)
    {
        free_mstring(malloced_name);
        malloced_name = NULL;
    }

    num_error--;

    if (current_interactive)
    {
        interactive_t *i;

        if (O_SET_INTERACTIVE(i, current_interactive)
         && i->noecho & NOECHO_STALE)
        {
            set_noecho(i, 0,  MY_FALSE, MY_FALSE);
        }
    }

    /* Unroll the context stack and find the recovery context to jump to. */

    if (published_catch)
    {
        unroll_context_stack();
        longjmp(((struct error_recovery_info *)rt_context)->con.text, 1);
        fatal("Catch() longjump failed");
    }

    unroll_context_stack();
    if (rt_context->type != ERROR_RECOVERY_NONE)
        longjmp(((struct error_recovery_info *)rt_context)->con.text, 1);

    fatal("Can't recover from error (longjmp failed)\n");
} /* errorf() */

/*-------------------------------------------------------------------------*/
void
warnf (char *fmt, ...)

/* A system runtime warning occured: generate a message from printf-style
 * <fmt> with a timestamp, and print it using debug_message(). The message
 * is also passed to master::runtime_warning().
 *
 * Note: Both 'warn' and 'warning' are already taken on some systems.
 * TODO: Add a pwarnf(<prefmt>, <postfmt>,...) function which translates the
 * TODO:: errno into a string and calls errorf(<prefmt><errmsg><postfmt>, ...).
 */

{
    char     *ts;
    string_t *file = NULL;           /* program name */
    object_t *curobj = NULL;         /* Verified current object */
    char      msg_buf[10000];
      /* The buffer for the error message to be created.
       */
    char      fixed_fmt[2000];
      /* Note: When changing this buffer, also change the HEAP_STACK_GAP
       * limit in xalloc.c!
       */
    mp_int    line_number = 0;
    Bool      inside_catch;
      /* TRUE: Code is executed inside a catch.
       */
    va_list   va;

    num_warning++;

    ts = time_stamp();
    
    /* Check if this warning occurs inside a catch. */
    inside_catch = MY_FALSE;

    {
        rt_context_t *rt;

        for ( rt = rt_context
            ; !ERROR_RECOVERY_CONTEXT(rt->type)
            ; rt = rt->last) NOOP;

        inside_catch = (rt->type >= ERROR_RECOVERY_CATCH);
    }

    va_start(va, fmt);

    /* Make fmt sane */
    fmt = limit_error_format(fixed_fmt, sizeof(fixed_fmt), fmt);

    /* Check the current object */
    curobj = NULL;
    if (current_object != NULL
     && current_object != &dummy_current_object_for_loads)
        curobj = current_object;

    if (curobj)
        assign_eval_cost();

    /* Generate the error message */
    vsprintf(msg_buf, fmt, va);
    va_end(va);

    debug_message("%s ", ts);
    debug_message("%s", msg_buf);

    /* If we have a current_object, determine the program location
     * of the fault.
     */
    if (curobj)
    {
        line_number = get_line_number_if_any(&file);
        debug_message("%s program: %s, object: %s line %"PRIdMPINT"\n"
                     , ts, get_txt(file), get_txt(curobj->name)
                     , line_number);
    }

    fflush(stdout);

    if (num_warning < 3)
    {
        /* Call master::runtime_warning().
         */

        object_t * save_cmd = command_giver;

        put_c_string(++inter_sp, msg_buf);
        if (curobj)
        {
            if (compat_mode)
                push_ref_string(inter_sp, curobj->name);
            else
                push_string(inter_sp, add_slash(curobj->name));
        }
        else
            push_number(inter_sp, 0);
        if (file)
            push_ref_string(inter_sp, file);
        else
            push_number(inter_sp, 0);
        push_number(inter_sp, line_number);
        push_number(inter_sp, inside_catch ? 1 : 0);

        apply_master(STR_WARNING, 5);
        command_giver = save_cmd;
    }
    else
    {
        if (file)
            free_mstring(file);
        errorf("Too many nested warnings.\n");
    }

    if (file)
        free_mstring(file);
    
    num_warning--;
} /* warnf() */

/*-------------------------------------------------------------------------*/
void
parse_error (Bool warning, const char *error_file, int line, const char *what
            , const char *context)

/* The compiler found an error <what> (<warning> is FALSE) resp.
 * a warning <what> (<warning> is TRUE) while compiling <line> of
 * file <error_file>. The context of the error location is <context>.
 *
 * Log the error by calling master:log_error() (but do not reload
 * the master if not existing - the compiler is busy).
 */

{
    char buff[500];

    if (error_file == NULL)
        return;
    if (strlen(what) + strlen(error_file) > sizeof buff - 100)
        what = "...[too long error message]...";
    if (strlen(what) + strlen(error_file) > sizeof buff - 100)
        error_file = "...[too long filename]...";
    sprintf(buff, "%s line %d%s: %s\n", error_file, line, context, what);

    /* Don't call the master if it isn't loaded! */
    if (master_ob && !(master_ob->flags & O_DESTRUCTED) )
    {
        push_c_string(inter_sp, error_file);
        push_c_string(inter_sp, buff);
        push_number(inter_sp, warning ? 1 : 0);
        apply_master(STR_LOG_ERROR, 3);
    }
} /* parse_error() */

/*-------------------------------------------------------------------------*/
void
throw_error (svalue_t *v)

/* The efun throw(). We have to save the message <v> in the
 * error context and then do the proper longjmp. <v> is freed.
 */

{
    unroll_context_stack();
    if (rt_context->type >= ERROR_RECOVERY_CATCH)
    {
        transfer_error_message(v, rt_context);
        longjmp(((struct error_recovery_info *)rt_context)->con.text, 1);
        fatal("Throw_error failed!");
    }
    free_svalue(v);
    errorf("Throw with no catch.\n");
} /* throw_error() */

/*-------------------------------------------------------------------------*/
void
set_svalue_user (svalue_t *svp, object_t *owner)

/* Set the owner of <svp> to object <owner>, if the svalue knows of
 * this concept. This may cause a recursive call to this function again.
 */

{
    switch(svp->type)
    {
    case T_POINTER:
    case T_QUOTED_ARRAY:
        set_vector_user(svp->u.vec, owner);
        break;
    case T_MAPPING:
      {
        set_mapping_user(svp->u.map, owner);
        break;
      }
    case T_CLOSURE:
      {
        set_closure_user(svp, owner);
      }
    }
} /* set_svalue_user() */

/*-------------------------------------------------------------------------*/
static void
give_uid_error_handler (error_handler_t *arg)

/* Error handler for give_uid_to_object(), called automatically when
 * the stack is cleant up during the error handling.
 * <arg> is a (struct give_uid_error_context*), the action is to destruct
 * the object.
 */

{
    struct give_uid_error_context *ecp;
    object_t *ob;

    ecp = (struct give_uid_error_context *)arg;
    ob = ecp->new_object;
    xfree(ecp);

    if (ob)
    {
        destruct(ob);
    }
} /* give_uid_error_handler() */

/*-------------------------------------------------------------------------*/
static void
push_give_uid_error_context (object_t *ob)

/* Object <ob> will be given its uids. Push an error handler onto the
 * interpreter stack which will clean up <ob> in case of an error.
 */

{
    struct give_uid_error_context *ecp;

    ecp = xalloc(sizeof *ecp);
    if (!ecp)
    {
        destruct(ob);
        errorf("Out of memory (%zu bytes) for new object '%s' uids\n"
             , sizeof(*ecp), get_txt(ob->name));
    }
    ecp->new_object = ob;
    push_error_handler(give_uid_error_handler, &(ecp->head));
} /* push_give_uid_error_context() */

/*-------------------------------------------------------------------------*/
static Bool
give_uid_to_object (object_t *ob, int hook, int numarg)

/* Object <ob> was just created - call the driver_hook <hook> with <numarg>
 * arguments to give it its uid and euid.
 * Return TRUE on success - on failure, destruct <ob>ject and raise
 * an error; return FALSE in the unlikely case that errorf() does return.
 */

{
    lambda_t *l;
    char *err, errtxt[1024];
    svalue_t arg, *ret;

    ob->user = &default_wizlist_entry;  /* Default uid */

    if ( NULL != (l = driver_hook[hook].u.lambda) )
    {
        if (driver_hook[hook].x.closure_type == CLOSURE_LAMBDA)
        {
            free_object(l->ob, "give_uid_to_object");
            l->ob = ref_object(ob, "give_uid_to_object");
        }
        call_lambda(&driver_hook[hook], numarg);
        ret = inter_sp;
        xfree(ret[-1].u.lvalue); /* free error context */

        if (ret->type == T_STRING)
        {
            ob->user = add_name(ret->u.str);
            ob->eff_user = ob->user;
            pop_stack();        /* deallocate result */
            inter_sp--;         /* skip error context */
            return MY_TRUE;
        }
        else if (ret->type == T_POINTER && VEC_SIZE(ret->u.vec) == 2
              && (   ret->u.vec->item[0].type == T_STRING
                  || (!strict_euids && ret->u.vec->item[0].u.number)
                 )
                )
        {
            ret = ret->u.vec->item;
            ob->user =   ret[0].type != T_STRING
                       ? &default_wizlist_entry
                       : add_name(ret[0].u.str);
            ob->eff_user = ret[1].type != T_STRING
                           ? 0
                           : add_name(ret[1].u.str);
            pop_stack();
            inter_sp--;
            return MY_TRUE;
        }
        else if (!strict_euids && ret->type == T_NUMBER && ret->u.number)
        {
            ob->user = &default_wizlist_entry;
            ob->eff_user = NULL;
            pop_stack();
            inter_sp--;
            return MY_TRUE;
        }
        else
        {
            pop_stack(); /* deallocate result */
            sprintf(errtxt, "Object '%.900s' illegal to load (no uid).\n"
                          , get_txt(ob->name));
            err = errtxt;
        }
    }
    else
    {
        do pop_stack(); while (--numarg); /* deallocate arguments */
        xfree(inter_sp->u.lvalue);
        err = "closure to set uid not initialized!\n";
    }

    inter_sp--;  /* skip error context */

    if (master_ob == NULL)
    {
        /* Only for the master object. */
        ob->user = add_name(STR_NONAME);
        ob->eff_user = NULL;
        return MY_TRUE;
    }

    ob->user = add_name(STR_NONAME);
    ob->eff_user = ob->user;
    put_object(&arg, ob);
    destruct_object(&arg);
    errorf("%s", err);
    /* NOTREACHED */
    return MY_FALSE;
} /* give_uid_to_object() */

/*-------------------------------------------------------------------------*/
const char *
make_name_sane (const char *pName, Bool addSlash)

/* Make a given object name sane.
 *
 * The function removes leading '/' (if addSlash is true, all but one leading
 * '/' are removed), a trailing '.c', and folds consecutive
 * '/' into just one '/'. The '.c' removal does not work when given
 * clone object names (i.e. names ending in '#<number>').
 *
 * The function returns a pointer to a static(!) buffer with the cleant
 * up name, or NULL if the given name already was sane.
 */

{
    static char buf[MAXPATHLEN+1];
    const char *from = pName;
    char *to;
    short bDiffers = MY_FALSE;

    to = buf;

    /* Skip leading '/' */
    if (!addSlash)
    {
        while (*from == '/') {
            bDiffers = MY_TRUE;
            from++;
        }
    }
    else
    {
        *to++ = '/';
        if (*from != '/')
            bDiffers = MY_TRUE;
        else
        {
            from++;
            while (*from == '/') {
                bDiffers = MY_TRUE;
                from++;
            }
        }
    }
    /* addSlash or not: from now points to the first non-'/' */

    /* Copy the name into buf, doing the other operations */
    for (; '\0' != *from && (size_t)(to - buf) < sizeof(buf)
         ; from++, to++)
    {
        if ('/' == *from)
        {
            *to = '/';
            while ('/' == *from) {
                from++;
                bDiffers = MY_TRUE;
            }

            from--;
        }
        else if ('.' == *from && 'c' == *(from+1) && '\0' == *(from+2))
        {
            bDiffers = MY_TRUE;
            break;
        }
        else
            *to = *from;
    }
    *to = '\0';

    if (!bDiffers)
        return NULL;

    return (const char *)buf;
} /* make_name_sane() */

/*-------------------------------------------------------------------------*/
Bool
check_no_parentdirs (const char *path)

/* Check that there are no '/../' constructs in the path.
 * Return TRUE if there aren't.
 */

{
    char *p;

    if (path == NULL)
        return MY_FALSE;

    for (p = strchr(path, '.'); p; p = strchr(p+1, '.'))
    {
        if (p[1] != '.')
            continue;
        if ((p[2] == '\0' || p[2] == '/')
         && (p == path    || p[-1] == '/')
           )
            return MY_FALSE;

        /* Skip the next '.' as it's safe to do so */
        p++;
    }
    return MY_TRUE;
} /* check_no_parentdirs() */

/*-------------------------------------------------------------------------*/
Bool
legal_path (const char *path)

/* Check that <path> is a legal relative path. This means no spaces
 * and no '/../' are allowed.
 * TODO: This should go into a 'files' module.
 */
{
    if (path == NULL
     || (!allow_filename_spaces && strchr(path, ' '))
     || path[0] == '/')
        return MY_FALSE;

    return check_no_parentdirs(path);
} /* legal_path() */

/*-------------------------------------------------------------------------*/
static void load_object_error(const char *msg, const char *name, namechain_t *chain) NORETURN;

static void
load_object_error(const char *msg, const char *name, namechain_t *chain)

/* Generate a compilation error message <msg>. If <name> is not NULL,
 * ": '<name>'" is appended to the message. If <chain> is not NULL,
 * " (inherited by <chain...>)" is appended to the message.
 * The message is then printed to stderr and an errorf() with it is thrown.
 */

{
    strbuf_t sbuf;
    namechain_t *ptr;
    char * buf;

    strbuf_zero(&sbuf);

    strbuf_add(&sbuf, msg);
    if (name != NULL)
    {
        strbuf_add(&sbuf, ": '");
        strbuf_add(&sbuf, name);
        strbuf_add(&sbuf, "'");
    }

    if (chain != NULL)
    {
        strbuf_add(&sbuf, " (inherited");
        for (ptr = chain; ptr != NULL; ptr = ptr->prev)
        {
            strbuf_add(&sbuf, " by '");
            strbuf_add(&sbuf, ptr->name);
            strbuf_add(&sbuf, "'");
        }
        strbuf_add(&sbuf, ")");
    }

    strbuf_add(&sbuf, ".\n");

    /* Make a local copy of the message so as not to leak memory */
    buf = alloca(strbuf_length(&sbuf)+1);
    if (!buf)
        errorf("Out of stack memory (%zu bytes)\n"
             , strlen(sbuf.buf)+1);
    strbuf_copy(&sbuf, buf);
    strbuf_free(&sbuf);

    fprintf(stderr, "%s %s", time_stamp(), buf);
    errorf("%.*s", MIN(ERROR_BUF_LEN - 200, (int)strlen(buf)), buf);
} /* load_object_error() */

/*-------------------------------------------------------------------------*/
#define MAX_LOAD_DEPTH 60 /* Make this a configurable constant */

static object_t *
load_object (const char *lname, Bool create_super, int depth
            , Bool isMasterObj, namechain_t *chain)

/* Load (compile) an object blueprint from the file <lname>.
 * <create_super> is true if the object has to be
 * initialized with CREATE_SUPER, and false if CREATE_OB is to be used.
 * <depth> is the current recursive load depth and is checked
 * against MAX_LOAD_DEPTH.
 * <isMasterObj> is TRUE if the top-level object to be compiled is the master
 * object.
 * <chain> is the pointer to the calling frame's namechain structure.
 *
 * If the object can't be loaded because it inherits some other unloaded
 * object, call load_object() recursively to load the inherited object, then
 * try to load the original object again. This is done in a loop so that
 * eventually all missing inherits are loaded.
 *
 * The name <lname> must be sane object name, and can be a clone name.
 *
 * If there is no source file <lname>.c, the function calls
 * master:compile_object() in case it is a virtual object.
 *
 * Result is the pointer to the loaded object, or NULL on failure.
 */

{
    int         fd;
    object_t   *ob;
    object_t   *save_command_giver = command_giver;
    long        i;
    struct stat c_st;
    size_t      name_length;
    char       *name; /* Copy of <lname> */
    char       *fname; /* Filename for <name> */
    program_t  *prog;
    namechain_t nlink;

#ifdef DEBUG
    if ('/' == lname[0])
        fatal("Improper filename '%s' passed to load_object()\n", lname);
#endif

    /* Empty lnames (/.c in Mudlib) *should* be OK. But use at your own risk. ;-)
     */
    name_length = strlen(lname);

    /* It could be that the passed filename is one of an already loaded
     * object. In that case, simply return that object.
     */
    ob = lookup_object_hash_str((char *)lname);
    if (ob)
    {
        return ob;
    }

    /* We need two copies of <lname>: one to construct the filename in,
     * the second because lname might be a buffer which is deleted
     * during the compilation process.
     * The memory is allocated in one chunk for both strings and an error 
     * handler is pushed on the stack (additionally is needed: memory for '/' 
     * and '\0’ (sizeof("/")) and '/', '\0', '.' and 'c' (sizeof("/.c"))).
     */
    name = xalloc_with_error_handler(2 * name_length + sizeof("/") + 
                                     sizeof("/.c"));
    fname = name + name_length + sizeof("/") + 1;
    if (!name)
        errorf("Out of memory (%zu bytes) in load_object() for temporary name "
               "buffers.\n", 2*name_length + sizeof("/") + sizeof("/.c"));
    
    if (!compat_mode)
        *name++ = '/';  /* Add and hide a leading '/' */
    strcpy(name, lname);
    strcpy(fname, lname);

    nlink.name = name;
    nlink.prev = chain;

    if (strict_euids && current_object && current_object->eff_user == 0
     && current_object->name)
        errorf("Can't load objects when no effective user.\n");

    if (master_ob && master_ob->flags & O_DESTRUCTED)
    {
        /* The master has been destructed, and it has not been noticed yet.
         * Reload it, because it can't be done inside of yyparse.
         * assert_master_ob_loaded() will clear master_ob while reloading is
         * in progress, thus preventing a fatal recursion.
         */
        assert_master_ob_loaded();
        /* has the object been loaded by assert_master_ob_loaded ? */
        if ( NULL != (ob = find_object_str(name)) )
        {
            if (ob->flags & O_SWAPPED && load_ob_from_swap(ob) < 0)
                /* The master has swapped this object and used up most
                 * memory... strange, but thinkable
                 */
                errorf("Out of memory: unswap object '%s'\n", get_txt(ob->name));
            pop_stack(); /* free error handler */
            return ob;
        }
    }

    /* Check if the name follows the "name#number" pattern */
    {
        char c;
        char *p;

        i = name_length;
        p = name+name_length;
        while (--i > 0) {
            /* isdigit would need to check isascii first... */
            if ( (c = *--p) < '0' || c > '9' ) {
                if (c == '#' && name_length - i > 1)
                {
                    load_object_error("Illegal file to load", name, chain);
                    /* NOTREACHED */
                }
                break;
            }
        }
    }

    /* Check if we were already trying to compile this object */
    if (chain != NULL)
    {
        namechain_t * ptr;

        for (ptr = chain; ptr != NULL; ptr = ptr->prev)
        {
            if (!strcmp(name, ptr->name))
                load_object_error("Recursive inherit", name, chain);
        }
    }

    /* Check that the c-file exists.
     */
    (void)strcpy(fname+name_length, ".c");
    if (ixstat(fname, &c_st) == -1)
    {
        /* The file does not exist - maybe it's a virtual object */

        svalue_t *svp;

        push_c_string(inter_sp, fname);
        svp = apply_master(STR_COMPILE_OBJECT, 1);
        if (svp && svp->type == T_OBJECT)
        {
            /* We got an object from the call, but is it what it
             * claims to be?
             */
            if ( NULL != (ob = lookup_object_hash_str(name)) )
            {
                /* An object for <name> magically appeared - is it
                 * the one we received?
                 */
                if (ob == svp->u.ob)
                {
                    /* If this object is a clone, clear the clone flag
                     * but mark it as replaced.
                     */
                    if (ob->flags & O_CLONE)
                    {
                        ob->flags &= ~O_CLONE;
                        ob->flags |= O_REPLACED;
                    }
                    pop_stack(); /* free error handler */
                    return ob;
                }
            }
            else if (ob != master_ob)
            {
                /* Rename the object we got to the name it
                 * is supposed to have.
                 */
                ob = svp->u.ob;
                remove_object_hash(ob);
                free_mstring(ob->name);
                ob->name = new_mstring(name);
                enter_object_hash(ob);

                /* If this object is a clone, clear the clone flag
                 * but mark it as replaced.
                 */
                if (ob->flags & O_CLONE)
                {
                    ob->flags &= ~O_CLONE;
                    ob->flags |= O_REPLACED;
                }
                pop_stack(); /* free error handler */
                return ob;
            }
            fname[name_length] = '.';
        }
        load_object_error("Failed to load file", name, chain);
        /* NOTREACHED */
        return NULL;
    }

    /* Check if it's a legal name.
     */
    if (!legal_path(fname))
    {
        load_object_error("Illegal pathname", fname, chain);
        /* NOTREACHED */
        return NULL;
    }

    /* The compilation loop. It will run until either <name> is loaded
     * or an error occurs. If the compilation is aborted because an
     * inherited object was not found, that object is loaded in a
     * recursive call, then the loop will try again on the original
     * object.
     */

    while (MY_TRUE)
    {
        /* This can happen after loading an inherited object: */
        ob = lookup_object_hash_str((char *)name);
        if (ob)
        {
            pop_stack(); /* free error handler */
            return ob;
        }

        if (comp_flag)
            fprintf(stderr, "%s compiling %s ...", time_stamp(), fname);

        if (current_loc.file)
        {
            errorf("Can't load '%s': compiler is busy with '%s'.\n"
                 , name, current_loc.file->name);
        }

        fd = ixopen(fname, O_RDONLY | O_BINARY);
        if (fd <= 0)
        {
            perror(fname);
            errorf("Could not read the file.\n");
        }
        FCOUNT_COMP(fname);

        /* The file name is needed before compile_file(), in case there is
         * an initial 'line too long' error.
         */
        compile_file(fd, fname, isMasterObj);
        if (comp_flag)
        {
            if (NULL == inherit_file)
                fprintf(stderr, " done\n");
            else
            {
                fprintf(stderr, " needs inherit\n");
            }
        }

        update_compile_av(total_lines);
        total_lines = 0;
        (void)close(fd);

        /* If there is no inherited file to compile, we can
         * end the loop here.
         */
        if (NULL == inherit_file)
            break;

        /* This object wants to inherit an unloaded object. We discard
         * current object, load the object to be inherited and reload
         * the current object again. The global variable "inherit_file"
         * was set by lang.y to point to a file name.
         */
        {
            char * pInherited;
            const char * tmp;

            tmp = make_name_sane(get_txt(inherit_file), MY_FALSE);
            if (!tmp)
            {
                pInherited = get_txt(inherit_file);
            }
            else
            {
                pInherited = alloca(strlen(tmp)+1);
                strcpy(pInherited, tmp);
            }

            push_string(inter_sp, inherit_file);
              /* Automagic freeing in case of errors */
            inherit_file = NULL;

            /* Now that the inherit_file-string will be freed in case
             * of an error, we can check if there were other errors
             * besides the missing inherit.
             */
            if (num_parse_error > 0)
            {
                load_object_error("Error in loading object", name, chain);
            }

            if (strcmp(pInherited, name) == 0)
            {
                errorf("Illegal to inherit self.\n");
            }

            if (depth >= MAX_LOAD_DEPTH)
            {
                load_object_error("Too deep inheritance", name, chain);
            }

            ob = load_object(pInherited, MY_TRUE, depth+1, isMasterObj, &nlink);
            free_mstring(inter_sp->u.str);
            inter_sp--;
            if (!ob || ob->flags & O_DESTRUCTED)
            {
                load_object_error("Error in loading object "
                      "(inheritance failed)\n", name, chain);
            }
        } /* handling of inherit_file */
    } /* while() - compilation loop */

    /* Did the compilation succeed? */
    if (num_parse_error > 0)
    {
        load_object_error("Error in loading object", name, chain);
    }

    /* We got the program. Now create the blueprint to hold it.
     */

    if (NULL != (ob = lookup_object_hash_str(name)))
    {
        /* The object magically appeared!
         * This can happen if rename_object() is used carelessly
         * in the mudlib handler for compiler warnings.
         */
        free_prog(compiled_prog, MY_TRUE);
        load_object_error("Object appeared while it was compiled"
                         , name, chain);
        /* NOTREACHED */
        return NULL;
    }

    prog = compiled_prog;

    ob = get_empty_object(prog->num_variables);

    if (!ob)
        errorf("Out of memory for new object '%s'\n", name);

    ob->name = new_mstring(name);
#ifdef CHECK_OBJECT_STAT
    if (check_object_stat)
    {
        fprintf(stderr, "DEBUG: OSTAT: (%ld:%ld) load( %p '%s') name: %zu -> (%ld:%ld)\n"
                      , tot_alloc_object, tot_alloc_object_size, ob, ob->name ? get_txt(ob->name) : "<null>"
                      , mstrsize(ob->name)
                      , tot_alloc_object
                      , tot_alloc_object_size + (mstrsize(ob->name))
                      );
    }
#endif
    tot_alloc_object_size += mstrsize(ob->name);
      /* Tabling this unique string is of not much use.
       * Note that the string must be valid for the ref_object()
       * below to work in debugging mode.
       */

    prog->blueprint = ref_object(ob, "load_object: blueprint reference");

    if (!compat_mode)
        name--;  /* Make the leading '/' visible again */
    ob->load_name = new_tabled(name);  /* but here it is */
    ob->prog = prog;
    ob->ticks = ob->gigaticks = 0;
    ob->next_all = obj_list;
    ob->prev_all = NULL;
    if (obj_list)
        obj_list->prev_all = ob;
    obj_list = ob;
    if (!obj_list_end)
        obj_list_end = ob;
    num_listed_objs++;
    enter_object_hash(ob);        /* add name to fast object lookup table */

    /* Give the object its uids */
    push_give_uid_error_context(ob);
    push_ref_string(inter_sp, ob->name);
    if (give_uid_to_object(ob, H_LOAD_UIDS, 1))
    {
        /* The object has an uid - now we can update the .user
         * of its initializers.
         */
        svalue_t *svp;
        int j;
        object_t *save_current;

        save_current = current_object;
        current_object = ob; /* just in case */
        svp = ob->variables;
        for (j = ob->prog->num_variables;  --j >= 0; svp++)
        {
            if (svp->type == T_NUMBER)
                continue;
            set_svalue_user(svp, ob);
        }

        if (save_current == &dummy_current_object_for_loads)
        {
            /* The master object is loaded with no current object */
            current_object = NULL;
        }
        else
        {
            current_object = save_current;
        }

        if (ob->flags & O_DESTRUCTED)
        {
            warnf("Object '%s' was destroyed before initialization.\n"
                 , get_txt(ob->name));
            return NULL;
        }
        init_object_variables(ob, NULL);

        if (ob->flags & O_DESTRUCTED)
        {
            warnf("Object '%s' was destroyed during initialization.\n"
                 , get_txt(ob->name));
            return NULL;
        }
        reset_object(ob, create_super ? H_CREATE_SUPER : H_CREATE_OB);

        /* If the master inherits anything -Ugh- we have to have
         * some object to attribute initialized variables to.
         */
        current_object = save_current;
    }

    if ( !(ob->flags & O_DESTRUCTED))
        ob->flags |= O_WILL_CLEAN_UP;

    /* free the error handler with the buffer for name and fname. */
    pop_stack();
    
    /* Restore the command giver */
    command_giver = check_object(save_command_giver);

    if (d_flag > 1 && ob)
    {
        debug_message("%s --%s loaded\n", time_stamp(), get_txt(ob->name));
    }

#if 0 && defined(CHECK_OBJECT_REF)
    if (strchr(get_txt(ob->name), '#') == NULL)
        printf("DEBUG: new_object(%p '%s') ref %"PRIdPINT" flags %x\n"
              , ob, get_txt(ob->name), ob->ref, ob->flags);
#endif
    return ob;
} /* load_object() */

/*-------------------------------------------------------------------------*/
static string_t *
make_new_name (string_t *str)

/* <str> is a basic object name - generate a clone name "<str>#<num>"
 * and return it. The result will be an untabled string with one reference.
 *
 * The number is guaranteed to be unique in combination with this name.
 */

{
    static unsigned long clone_id_number = 0;
      /* The next number to use for a clone name */

    static int test_conflict = MY_FALSE;
      /* TRUE if the generated clone name has to be tested for uniqueness.
       * This is not the case before clone_id_number wraps around the
       * first time.
       */

    string_t *p;
    char buff[40];

    str = del_slash(str);

    for (;;)
    {
        /* Generate the clone name */
        (void)sprintf(buff, "#%lu", clone_id_number);
        p = mstr_add_txt(str, buff, strlen(buff));

        clone_id_number++;
        if (clone_id_number == 0) /* Wrap around */
            test_conflict = MY_TRUE;

        if (!test_conflict || !find_object(p))
        {
            free_mstring(str);
            return p;
        }

        /* The name was already taken */
        free_mstring(p);
    }
} /* make_new_name() */

/*-------------------------------------------------------------------------*/
static object_t *
clone_object (string_t *str1)

/* Create a clone of the object named <str1>, which may be a clone itself.
 * On success, return the new object, otherwise NULL.
 */

{
    object_t *ob, *new_ob;
    object_t *save_command_giver = command_giver;
    string_t *name;

    if (strict_euids && current_object && current_object->eff_user == NULL)
        errorf("Illegal to call clone_object() with effective user 0\n");

    ob = get_object(str1);

    /* If the object self-destructed...
     */
    if (ob == NULL)
        return NULL;

    /* If ob is a clone, try finding the blueprint first via the object's
     * program, then via the load_name.
     */
    if (ob->flags & O_CLONE)
    {
        object_t *bp = NULL;

        /* If the object's program hasn't been replaced, it most likely
         * contains a pointer to the blueprint we're looking for.
         */
        if (!(ob->flags & O_REPLACED))
        {
            bp = ob->prog->blueprint;
            if (bp && (bp->flags & O_DESTRUCTED))
            {
                free_object(bp, "clone_object");
                bp = ob->prog->blueprint = NULL;
            }
        }

        /* Fallback: find/load the blueprint by the load_name */
        if (!bp)
            bp = get_object(ob->load_name);
        if (bp)
            ob = bp;
    }

    if (ob->super)
        errorf("Cloning a bad object: '%s' is contained in '%s'.\n"
             , get_txt(ob->name), get_txt(ob->super->name));

    name = ob->name;

    /* If the ob is a clone, we have to test if its name is something
     * illegal like 'foobar#34'. In that case, we have to use the
     * load_name as template.
     */
    if (ob->flags & O_CLONE)
    {
        char c;
        char *p;
        mp_int name_length, i;

        name_length = mstrsize(name);
        i = name_length;
        p = get_txt(ob->name)+name_length;
        while (--i > 0) {
            /* isdigit would need to check isascii first... */
            if ( (c = *--p) < '0' || c > '9' )
            {
                if (c == '#' && name_length - i > 1)
                {
                    /* Well, unusable name format - use the load_name */
                    name = ob->load_name;
                }
                break;
            }
        }
    }

    if ((ob->flags & O_SWAPPED) && load_ob_from_swap(ob) < 0)
        errorf("Out of memory: unswap object '%s'\n", get_txt(ob->name));

    if (ob->prog->flags & P_NO_CLONE)
        errorf("Cloning a bad object: '%s' sets '#pragma no_clone'.\n"
             , get_txt(ob->name));

    ob->time_of_ref = current_time;

    /* Got the blueprint - now get a new object */

    new_ob = get_empty_object(ob->prog->num_variables);
    if (!new_ob)
        errorf("Out of memory for new clone '%s'\n", get_txt(name));

    new_ob->name = make_new_name(name);

#ifdef CHECK_OBJECT_STAT
    if (check_object_stat)
    {
        fprintf(stderr, "DEBUG: OSTAT: (%ld:%ld) clone( %p '%s') name: %zu -> (%ld:%ld)\n"
                      , tot_alloc_object, tot_alloc_object_size, new_ob, new_ob->name ? get_txt(new_ob->name) : "<null>"
                      , mstrsize(new_ob->name)
                      , tot_alloc_object
                      , tot_alloc_object_size + (mstrsize(new_ob->name))
                      );
    }
#endif
    tot_alloc_object_size += mstrsize(new_ob->name);
    new_ob->load_name = ref_mstring(ob->load_name);
    new_ob->flags |= O_CLONE | O_WILL_CLEAN_UP;
    new_ob->prog = ob->prog;
    reference_prog (ob->prog, "clone_object");
    new_ob->ticks = new_ob->gigaticks = 0;
#ifdef DEBUG
    if (!current_object)
        fatal("clone_object() from no current_object !\n");
#endif
    new_ob->next_all = obj_list;
    new_ob->prev_all = NULL;
    if (obj_list)
        obj_list->prev_all = new_ob;
    obj_list = new_ob;
    if (!obj_list_end)
        obj_list_end = new_ob;
    num_listed_objs++;
    enter_object_hash(new_ob);        /* Add name to fast object lookup table */
    push_give_uid_error_context(new_ob);
    push_ref_object(inter_sp, ob, "clone_object");
    push_ref_string(inter_sp, new_ob->name);
    give_uid_to_object(new_ob, H_CLONE_UIDS, 2);

    if (new_ob->flags & O_DESTRUCTED)
    {
        warnf("Object '%s' was destroyed before initialization.\n"
             , get_txt(new_ob->name));
        return NULL;
    }
    init_object_variables(new_ob, ob);

    if (new_ob->flags & O_DESTRUCTED)
    {
        warnf("Object '%s' was destroyed during initialization.\n"
             , get_txt(new_ob->name));
        return NULL;
    }
    reset_object(new_ob, H_CREATE_CLONE);
    command_giver = check_object(save_command_giver);

    /* Never know what can happen ! :-( */
    if (new_ob->flags & O_DESTRUCTED)
        return NULL;

    return new_ob;
} /* clone_object() */

/*-------------------------------------------------------------------------*/
object_t *
lookfor_object (string_t * str, Bool bLoad)

/* Look for a named object <str>, optionally loading it (<bLoad> is true).
 * Return a pointer to the object structure, or NULL.
 *
 * If <bLoad> is true, the function tries to load the object if it is
 * not already loaded.
 * If <bLoad> is false, the function just checks if the object is loaded.
 *
 * The object is not swapped in.
 *
 * For easier usage, the macros find_object() and get_object() expand
 * to the no-load- resp. load-call of this function.
 *
 * TODO: It would be nice if all loading uses of lookfor would go through
 * TODO:: the efun load_object() or a driver hook so that the mudlib
 * TODO:: has a chance to interfere with it. Dito for clone_object(), so
 * TODO:: that the euid-check can be done there?
 */
{
    object_t *ob;
    const char * pName;
    Bool isMasterObj = MY_FALSE;

    if (mstreq(str, master_name_str))
        isMasterObj = MY_TRUE;

    /* TODO: It would be more useful to check all callers of lookfor()
     * TODO:: and move the make_name_sane() into those where it can
     * TODO:: be dirty.
     */
    pName = make_name_sane(get_txt(str), MY_FALSE);
    if (!pName)
        pName = get_txt(str);

    if (!isMasterObj && !strcmp(pName, get_txt(master_name_str)))
        isMasterObj = MY_TRUE;

    ob = lookup_object_hash_str(pName);
    if (!bLoad)
        return ob;

    if (!ob)
    {
        ob = load_object(pName, 0, 0, isMasterObj, NULL);
    }
    if (!ob || ob->flags & O_DESTRUCTED)
        return NULL;
    return ob;
} /* lookfor_object() */

/*-------------------------------------------------------------------------*/
object_t *
find_object_str (const char * str)

/* Look for a named object <str>.
 * Return a pointer to the object structure, or NULL.
 *
 * The object is not swapped in.
 */
{
    const char * pName;

    /* TODO: It would be more useful to check all callers of lookfor()
     * TODO:: and move the make_name_sane() into those where it can
     * TODO:: be dirty.
     */
    pName = make_name_sane(str, MY_FALSE);
    if (!pName)
        pName = str;

    return lookup_object_hash_str(pName);
} /* find_object_str() */

/*-------------------------------------------------------------------------*/
void
destruct_object (svalue_t *v)

/* Destruct the object named/passed in svalue <v>.
 * This is the full program: the master:prepare_destruct() is called
 * to clean the inventory of the object, and if it's an interactive,
 * it is given the chance to save a pending editor buffer.
 *
 * The actual destruction work is then done in destruct().
 */

{
    object_t *ob;
    svalue_t *result;

    /* Get the object to destruct */
    if (v->type == T_OBJECT)
        ob = v->u.ob;
    else
    {
        ob = find_object(v->u.str);
        if (ob == 0)
            errorf("destruct_object: Could not find %s\n", get_txt(v->u.str));
    }

    if (ob->flags & O_DESTRUCTED)
        return;

    if (ob->flags & O_SWAPPED)
        if (load_ob_from_swap(ob) < 0)
            errorf("Out of memory: unswap object '%s'\n", get_txt(ob->name));

    if (d_flag)
    {
        debug_message("%s destruct_object: %s (ref %"PRIdPINT")\n"
                     , time_stamp(), get_txt(ob->name), ob->ref);
    }

    push_ref_object(inter_sp, ob, "destruct");
    result = apply_master(STR_PREP_DEST, 1);
    if (!result)
        errorf("No prepare_destruct\n");

    if (result->type == T_STRING)
        errorf("%s", get_txt(result->u.str));

    if (result->type != T_NUMBER || result->u.number != 0)
        return;

    if (ob->contains)
    {
        errorf("Master failed to clean inventory in prepare_destruct\n");
    }

    if (O_IS_INTERACTIVE(ob))
    {
        interactive_t *ip = O_GET_INTERACTIVE(ob);
        object_t *save = command_giver;

        command_giver = ob;
        trace_level |= ip->trace_level;

        abort_input_handler(ip);

        command_giver = save;
    }
    destruct(ob);
} /* destruct_object() */

/*-------------------------------------------------------------------------*/
void
deep_destruct (object_t *ob)

/* Destruct an object <ob> and the blueprint objects of all inherited
 * programs. The actual destruction work is done by destruct().
 *
 * The objects are still kept around until the end of the execution because
 * it might still hold a running program. The destruction will be completed
 * from the backend by a call to handle_newly_destructed_objects().
 */

{
    program_t *prog;

    /* Destruct the object itself */
    destruct(ob);

    /* Loop through all the inherits and destruct the blueprints
     * of the inherited programs.
     */
    prog = ob->prog;
    if (prog != NULL)
    {
        int i;

        for (i = 0; i < prog->num_inherited; ++i)
        {
            program_t *iprog = prog->inherit[i].prog;

            if (iprog != NULL && iprog->blueprint != NULL)
            {
                destruct(iprog->blueprint);
            }
        }
    }
} /* deep_destruct() */

/*-------------------------------------------------------------------------*/
void
destruct (object_t *ob)

/* Really destruct an object <ob>. This function is called from
 * destruct_object() to do the actual work, and also directly in situations
 * where the master is out of order or the object not fully initialized.
 *
 * The function:
 *   - marks the object as destructed
 *   - moves it out of the global object list and the object able, into
 *     the list of destructed objects
 *   - changes all references on the interpreter stack to svalue-0
 *   - moves it out of its environment
 *   - removes all shadows.
 *
 * The object is still kept around until the end of the execution because
 * it might still hold a running program. The destruction will be completed
 * from the backend by a call to handle_newly_destructed_objects().
 */

{
    object_t **pp, *item, *next;
#ifdef CHECK_OBJECT_REF
    object_shadow_t *shadow;
#endif /* CHECK_OBJECT_REF */

    if (ob->flags & O_DESTRUCTED)
        return;

#ifdef CHECK_OBJECT_REF
    xallocate(shadow, sizeof(*shadow), "destructed object shadow");
#endif /* CHECK_OBJECT_REF */
#ifdef USE_SQLITE
    if (ob->open_sqlite_db)
        sl_close(ob);
#endif
    ob->time_reset = 0;

    /* We need the object in memory */
    if (ob->flags & O_SWAPPED)
    {
        int save_privilege;

        save_privilege = malloc_privilege;
        malloc_privilege = MALLOC_SYSTEM;
        load_ob_from_swap(ob);
        malloc_privilege = save_privilege;
    }

    /* If there are shadows, remove them */
    if (ob->flags & O_SHADOW)
    {
        shadow_t *shadow_sent;
        object_t *shadowing, *shadowed_by;

        shadow_sent = O_GET_SHADOW(ob);

        /* The chain of shadows is a double linked list. Take care to update
         * it correctly.
         */
        if ( NULL != (shadowing = shadow_sent->shadowing) )
        {
            shadow_t *shadowing_sent;

            /* Remove the shadow sent from the chain */
            shadowing_sent = O_GET_SHADOW(shadowing);
            shadow_sent->shadowing = NULL;
            shadowing_sent->shadowed_by = shadow_sent->shadowed_by;
            check_shadow_sent(shadowing);

            /* This object, the shadow, may have added actions to
             * the shadowee, or it's vicinity. Take care to remove
             * them all.
             */
            remove_shadow_actions(ob, shadowing);
        }

        if ( NULL != (shadowed_by = shadow_sent->shadowed_by) )
        {
            shadow_t *shadowed_by_sent;

            /* Remove the shadow sent from the chain */
            shadowed_by_sent = O_GET_SHADOW(shadowed_by);
            shadow_sent->shadowed_by = NULL;
            shadowed_by_sent->shadowing = shadowing;
            check_shadow_sent(shadowed_by);

            /* Our shadows may have added actions to us or to our
             * environment. Take care to remove them all.
             */
            do {
                remove_shadow_actions(shadowed_by, ob);
                if (O_GET_SHADOW(shadowed_by) != NULL)
                    shadowed_by = O_GET_SHADOW(shadowed_by)->shadowed_by;
                else
                    shadowed_by = NULL;
            } while (shadowed_by != NULL);
        }

        check_shadow_sent(ob);
    }

    /* Move all objects in the inventory into the "void" */
    for (item = ob->contains; item; item = next)
    {
        remove_action_sent(ob, item);
        item->super = NULL;
        next = item->next_inv;
        item->next_inv = NULL;
    }

    remove_object_from_stack(ob);

    if (ob == simul_efun_object)
    {
        simul_efun_object = NULL;
        invalidate_simul_efuns();
    }

    set_heart_beat(ob, MY_FALSE);

    /* Remove us out of this current room (if any).
     * Remove all sentences defined by this object from all objects here.
     */
    if (ob->super)
    {
        if (ob->super->sent)
            remove_action_sent(ob, ob->super);

#       ifdef USE_SET_LIGHT
            add_light(ob->super, - ob->total_light);
#       endif

        for (pp = &ob->super->contains; *pp;)
        {
            if ((*pp)->sent)
                remove_action_sent(ob, *pp);
            if (*pp != ob)
                pp = &(*pp)->next_inv;
            else
                *pp = (*pp)->next_inv;
        }
    }

    /* Now remove us out of the list of all objects.
     * This must be done last, because an error in the above code would
     * halt execution.
     */
    remove_object_hash(ob);
    if (ob->prev_all)
        ob->prev_all->next_all = ob->next_all;
    if (ob->next_all)
        ob->next_all->prev_all = ob->prev_all;
    if (ob == obj_list)
        obj_list = ob->next_all;
    if (ob == obj_list_end)
        obj_list_end = ob->prev_all;

    --num_listed_objs;
    ++destructed_ob_counter;

    ob->super = NULL;
    ob->next_inv = NULL;
    ob->contains = NULL;
    ob->flags &= ~O_ENABLE_COMMANDS;
    ob->flags |= O_DESTRUCTED;  /* must come last! */
    if (command_giver == ob)
        command_giver = NULL;

    /* Put the object into the list of newly destructed objects */
    ob->prev_all = NULL;
    ob->next_all = newly_destructed_objs;
    newly_destructed_objs = ob;
    num_newly_destructed++;
#ifdef CHECK_OBJECT_REF
    shadow->obj = ob;
    shadow->ref = ob->ref;
    shadow->flags = ob->flags;
    shadow->sent = ob->sent;
    shadow->next = newly_destructed_obj_shadows;
    newly_destructed_obj_shadows = shadow;
#endif /* CHECK_OBJECT_REF */
} /* destruct() */

#ifdef CHECK_OBJECT_REF
/*-------------------------------------------------------------------------*/
void
check_object_shadow (object_t *ob, object_shadow_t *sh)
{
    if (sh->obj != ob)
        fatal("DEBUG: Obj %p '%s', shadow %p -> obj %p '%s'\n"
             , ob, get_txt(ob->name), sh, sh->obj, get_txt(sh->obj->name));
    if ((sh->flags & O_DESTRUCTED) != (ob->flags & O_DESTRUCTED)
     || sh->sent != ob->sent
       )
        fatal("DEBUG: Obj %p '%s': ref %"PRIdPINT", flags %x, sent %p;"
              "shadow ref %"PRIdPINT", flags %x, sent %p\n"
             , ob, get_txt(ob->name), ob->ref, ob->flags, ob->sent
             , sh->ref, sh->flags, sh->sent
             );
} /* check_object_shadow() */

void
check_all_object_shadows (void)
{
    object_shadow_t *sh;
    object_t * ob;

    for (ob = newly_destructed_objs, sh = newly_destructed_obj_shadows
        ; ob != NULL
        ; ob = ob->next_all, sh = sh->next
        )
        check_object_shadow(ob, sh);

    for (ob = destructed_objs, sh = destructed_obj_shadows
        ; ob != NULL
        ; ob = ob->next_all, sh = sh->next
        )
        check_object_shadow(ob, sh);
} /* check_object_shadows() */

void
update_object_sent(object_t *obj, sentence_t *new_sent)
{
    object_shadow_t *sh;
    if (!(obj->flags & O_DESTRUCTED))
    {
        obj->sent = new_sent;
        return;
    }
    for (sh = newly_destructed_obj_shadows; sh != NULL; sh = sh->next)
        if (sh->obj == obj)
            break;
    if (sh == NULL)
        for (sh = newly_destructed_obj_shadows; sh != NULL; sh = sh->next)
            if (sh->obj == obj)
                break;
    if (sh == NULL)
    {
        fatal("DEBUG: Obj %p '%s': ref %"PRIdPINT", flags %x, sent %p; no shadow found\n"
             , obj, get_txt(obj->name), obj->ref, obj->flags, obj->sent
             );
    }
    check_object_shadow(obj, sh);
    obj->sent = new_sent;
    sh->sent = new_sent;
}

#endif /* CHECK_OBJECT_REF */
/*-------------------------------------------------------------------------*/
static void
remove_object (object_t *ob
#ifdef CHECK_OBJECT_REF
              , object_shadow_t *sh
#endif /* CHECK_OBJECT_REF */
              )

/* This function is called from outside any execution thread to finally
 * remove object <ob>. <ob> must have been unlinked from all object lists
 * already (but the associated reference count must still exist).
 *
 * The function frees all variables and remaining sentences in the object.
 * If then only one reference (from the original object list) remains, the
 * object is freed immediately with a call to free_object(). If more
 * references exist, the object is linked into the destructed_objs list
 * for freeing at a future date.
 *
 * The object structure and the program will be freed as soon as there
 * are no further references to the object (the program will remain behind
 * in case it was inherited).
 * TODO: Distinguish data- and inheritance references?
 */

{
    sentence_t *sent;

#ifdef CHECK_OBJECT_REF
    check_object_shadow(ob, sh);
#endif
    if (d_flag > 1)
    {
        debug_message("%s remove_object: object %s (ref %"PRIdPINT")\n"
                     , time_stamp(), get_txt(ob->name), ob->ref);
    }

    if (O_IS_INTERACTIVE(ob))
        remove_interactive(ob, MY_FALSE);

    /* If this is a blueprint object, NULL out the pointer in the program
     * to remove the extraneous reference.
     */
    if (ob->prog->blueprint == ob)
    {
        ob->prog->blueprint = NULL;
        remove_prog_swap(ob->prog, MY_TRUE);
        free_object(ob, "remove_object: blueprint reference");
    }

    /* We must deallocate variables here, not in 'free_object()'.
     * That is because one of the local variables may point to this object,
     * and deallocation of this pointer will also decrease the reference
     * count of this object. Otherwise, an object with a variable pointing
     * to itself would never be freed.
     * Just in case the program in this object would continue to
     * execute, change string and object variables into the number 0.
     */
    if (ob->prog->num_variables > 0)
    {
        /* Deallocate variables in this object.
         */
        int i;
        for (i = 0; i < ob->prog->num_variables; i++)
        {
            free_svalue(&ob->variables[i]);
            put_number(ob->variables+i, 0);
        }
        xfree(ob->variables);
    }
#ifdef DEBUG
    else if (ob->variables != NULL)
    {
        debug_message("%s Warning: Object w/o variables, but variable block "
                      "at %p\n", time_stamp(), ob->variables);
    }
#endif

    /* This should be here to avoid using up memory as long as the object
     * isn't released. It must be here because gcollect doesn't expect
     * sentences in destructed objects.
     */
    if ( NULL != (sent = ob->sent) )
    {
        sentence_t *next;
        do {

            next = sent->next;
            if (sent->type == SENT_SHADOW)
                free_shadow_sent((shadow_t *)sent);
            else
                free_action_sent((action_t *)sent);
        } while ( NULL != (sent = next) );
        ob->sent = NULL;
#ifdef CHECK_OBJECT_REF
        sh->sent = NULL;
#endif /* CHECK_OBJECT_REF */
    }

    /* Either free the object, or link it up for future freeing. */
    if (ob->ref <= 1)
    {
        free_object(ob, "destruct_object");
#ifdef CHECK_OBJECT_REF
        xfree(sh);
#endif /* CHECK_OBJECT_REF */
    }
    else
    {
        if (destructed_objs != NULL)
            destructed_objs->prev_all = ob;
        ob->next_all = destructed_objs;
        destructed_objs = ob;
        ob->prev_all = NULL;
        num_destructed++;
#ifdef CHECK_OBJECT_REF
        sh->next = destructed_obj_shadows;
        destructed_obj_shadows = sh;
#endif /* CHECK_OBJECT_REF */
    }
} /* remove_object() */

/*-------------------------------------------------------------------------*/
void
handle_newly_destructed_objects (void)

/* Finish up all newly destructed objects kept in the newly_destructed_objs
 * list: deallocate as many associated resources and, if there are
 * more than one references to the object, put it into the destructed_objs list.
 */

{
    while (newly_destructed_objs)
    {
        object_t *ob = newly_destructed_objs;

#ifdef CHECK_OBJECT_REF
        object_t *next_ob = ob->next_all;
        object_shadow_t *sh = newly_destructed_obj_shadows;
        object_shadow_t *next_sh = sh->next;
#else
        newly_destructed_objs = ob->next_all;
#endif /* CHECK_OBJECT_REF */

#ifdef DEBUG
        if (!(ob->flags & O_DESTRUCTED))
            fatal("Non-destructed object %p '%s' in list of destructed objects.\n"
                 , ob, ob->name ? get_txt(ob->name) : "<null>"
                 );
#endif
#ifdef CHECK_OBJECT_REF
        remove_object(ob, sh);
        newly_destructed_objs = next_ob;
        newly_destructed_obj_shadows = next_sh;
#else
        remove_object(ob);
#endif /* CHECK_OBJECT_REF */
        num_newly_destructed--;
    }
}  /* handle_newly_destructed_objects() */

/*-------------------------------------------------------------------------*/
void
remove_destructed_objects (Bool force)

/* Scan the list of destructed objects and free those with no references
 * remaining.
 * If <force> is FALSE, the call immediately returns if the flag
 * <dest_last_ref_gone> (in object.c) is FALSE - this flag is set by
 * free_object() if all but one reference to a destructed object is gone.
 * If <force> is TRUE, the scan takes place unconditionally (this is used by
 * the GC).
 */

{
    object_t *ob;
#ifdef CHECK_OBJECT_REF
    object_shadow_t *sh = destructed_obj_shadows;
    object_shadow_t *prev = NULL;
#endif /* CHECK_OBJECT_REF */

    if (!force && !dest_last_ref_gone)
        return;

    dest_last_ref_gone = MY_FALSE;

    for (ob = destructed_objs; ob != NULL; )
    {
        object_t *victim;

#ifdef CHECK_OBJECT_REF
        check_object_shadow(ob, sh);
#endif /* CHECK_OBJECT_REF */
        /* Check if only the list reference remains.
         * If not, go to the next object.
         */
        if (ob->ref > 1)
        {
            ob = ob->next_all;
#ifdef CHECK_OBJECT_REF
            prev = sh;
            sh = sh->next;
#endif /* CHECK_OBJECT_REF */
            continue;
        }

        /* This object can be freed - remove it from the list */
        victim = ob;
        if (ob->prev_all != NULL)
            ob->prev_all->next_all = ob->next_all;
        if (ob->next_all != NULL)
            ob->next_all->prev_all = ob->prev_all;
        if (destructed_objs == ob)
            destructed_objs = ob->next_all;
        ob = ob->next_all;

        free_object(victim, "remove_destructed_objects");
        num_destructed--;
#ifdef CHECK_OBJECT_REF
        {
            object_shadow_t * next = sh->next;
            if (prev == NULL)
            {
                destructed_obj_shadows = next;
            }
            else
            {
                prev->next = next;
            }
            xfree(sh);
            sh = next;
        }
#endif /* CHECK_OBJECT_REF */
    }
}  /* remove_destructed_objects() */

/*-------------------------------------------------------------------------*/
static INLINE shadow_t *
new_shadow_sent(void)

/* Allocate a new empty shadow sentence and return it.
 */

{
    shadow_t *p;

    xallocate(p, sizeof *p, "new shadow sentence");
    alloc_shadow_sent++;

    p->sent.type = SENT_SHADOW;
    p->shadowing = NULL;
    p->shadowed_by = NULL;
    p->ip = NULL;
    return p;
} /* new_shadow_sent() */

/*-------------------------------------------------------------------------*/
static void
free_shadow_sent (shadow_t *p)

/* Free the shadow sentence <p>.
 */

{
#ifdef DEBUG
    if (SENT_SHADOW != p->sent.type)
        fatal("free_shadow_sent() received non-shadow sent type %d\n"
             , p->sent.type);
#endif

    xfree(p);
    alloc_shadow_sent--;
} /* free_shadow_sent() */

/*-------------------------------------------------------------------------*/
void
check_shadow_sent (object_t *ob)

/* Check if object <ob> has a shadow sentence and really needs it.
 * If yes and no, the sentence is removed.
 */

{
    if (ob->flags & O_SHADOW)
    {
        shadow_t *sh;

        sh = O_GET_SHADOW(ob);

        if (!sh->ip
         && !sh->shadowing
         && !sh->shadowed_by
           )
        {
#ifdef CHECK_OBJECT_REF
            update_object_sent(ob, sh->sent.next);
#else
            ob->sent = sh->sent.next;
#endif /* CHECK_OBJECT_REF */
            free_shadow_sent(sh);
            ob->flags &= ~O_SHADOW;
        }
    }
} /* check_shadow_sent() */

/*-------------------------------------------------------------------------*/
void
assert_shadow_sent (object_t *ob)

/* Make sure that object <ob> has a shadow sentence.
 */

{
    if (!(ob->flags & O_SHADOW))
    {
        shadow_t *sh;

        sh = new_shadow_sent();
        sh->sent.next = ob->sent;
#ifdef CHECK_OBJECT_REF
        update_object_sent(ob, (sentence_t *)sh);
#else
        ob->sent = (sentence_t *)sh;
#endif /* CHECK_OBJECT_REF */
        ob->flags |= O_SHADOW;
    }
} /* assert_shadow_sent() */

/*-------------------------------------------------------------------------*/
Bool
status_parse (strbuf_t * sbuf, char * buff)

/* Parse the status request in <buff> and if recognized, dump the
 * data into the stringbuffer <sbuf>.
 *
 * Return TRUE if the request was recognised, and FALSE otherwise.
 *
 * The function is called from actions:special_parse() to implement
 * the hardcoded commands, and from the efun debug_info().
 */

{
    if (sbuf)
        strbuf_zero(sbuf);

    if (!buff || *buff == 0 || strcmp(buff, "tables") == 0)
    {
        size_t tot, res;
        Bool verbose = MY_FALSE;

        if (strcmp(buff, "tables") == 0)
            verbose = MY_TRUE;

        res = 0;
        if (reserved_user_area)
            res = reserved_user_size;
        if (reserved_master_area)
            res += reserved_master_size;
        if (reserved_system_area)
            res += reserved_system_size;
        if (!verbose)
        {
            strbuf_addf(sbuf, "Actions:\t\t\t%8"PRIdPINT" %9"PRIdPINT"\n"
                            , alloc_action_sent
                            , alloc_action_sent * sizeof (action_t));
            strbuf_addf(sbuf, "Shadows:\t\t\t%8"PRIdPINT" %9"PRIdPINT"\n"
                            , alloc_shadow_sent
                            , alloc_shadow_sent * sizeof (shadow_t));
            strbuf_addf(sbuf, "Objects:\t\t\t%8ld %9ld (%ld destructed;"
                              " %"PRIdMPINT" swapped: %"PRIdMPINT" Kbytes)\n"
                            , tot_alloc_object, tot_alloc_object_size
                            , num_destructed  
                            , num_vb_swapped, total_vb_bytes_swapped / 1024);
            strbuf_addf(sbuf, "Prog blocks:\t\t\t%8"PRIdMPINT" %9"PRIdMPINT
                              " (%"PRIdMPINT" swapped: %"PRIdMPINT" Kbytes)\n"
                            , total_num_prog_blocks + num_swapped - num_unswapped
                            , total_prog_block_size + total_bytes_swapped
                                                    - total_bytes_unswapped
                            , num_swapped - num_unswapped
                            , (total_bytes_swapped - total_bytes_unswapped) / 1024);
            strbuf_addf(sbuf, "Arrays:\t\t\t\t%8ld %9ld\n"
                            , (long)num_arrays, total_array_size() );
            strbuf_addf(sbuf, "Mappings:\t\t\t%8"PRIdMPINT" %9"PRIdMPINT
                              " (%"PRIdMPINT" hybrid, %"PRIdMPINT" hash)\n"
                            , num_mappings, total_mapping_size()
                            , num_dirty_mappings, num_hash_mappings
                            );
            strbuf_addf(sbuf, "Memory reserved:\t\t\t %9zu\n", res);
        }
        if (verbose) {
/* TODO: Add these numbers to the debug_info statistics. */
            strbuf_add(sbuf, "\nVM Execution:\n");
            strbuf_add(sbuf,   "-------------\n");
            strbuf_addf(sbuf
                      , "Last:    %10lu ticks, %3ld.%06ld s\n"
                        "Average: %10.0lf ticks, %10.6lf s\n"
                      , last_total_evalcost
                      , last_eval_duration.tv_sec, (long)last_eval_duration.tv_usec
                      , stat_total_evalcost.weighted_avg
                      , stat_eval_duration.weighted_avg / 1000000.0
                      );
            strbuf_addf(sbuf
                      , "Load: %.2lf cmds/s, %.2lf comp lines/s\n"
                      , stat_load.weighted_avg
                      , stat_compile.weighted_avg
                      );

#ifdef COMM_STAT
            strbuf_add(sbuf, "\nNetwork IO:\n");
            strbuf_add(sbuf,   "-----------\n");
            strbuf_addf(sbuf
                       , "In:  Packets: %10"PRIuSTATCOUNTER" - Sum: %10"PRIuSTATCOUNTER" - "
                         "Average packet size: %7.2f\n"
                       , inet_packets_in
                       , inet_volume_in
                       , inet_packets_in ? (float)inet_volume_in/(float)inet_packets_in : 0.0
            );
            strbuf_addf(sbuf
                       , "Out: Packets: %10"PRIuSTATCOUNTER" - Sum: %10"PRIuSTATCOUNTER" - "
                         "Average packet size: %7.2f\n"
                         "     Calls to add_message: %"PRIuSTATCOUNTER"\n"
                       , inet_packets
                       , inet_volume
                       , inet_packets ? (float)inet_volume/(float)inet_packets : 0.0
                       , add_message_calls
            );
#endif
#ifdef APPLY_CACHE_STAT
            strbuf_add(sbuf, "\nApply Cache:\n");
            strbuf_add(sbuf,   "------------\n");
            strbuf_addf(sbuf
                       , "Calls to apply_low: %10"PRIuSTATCOUNTER"\n"
                         "Cache hits:         %10"PRIuSTATCOUNTER" (%.2f%%)\n"
                       , (apply_cache_hit+apply_cache_miss)
                       , apply_cache_hit
                       , 100.*(float)apply_cache_hit/
                         (float)(apply_cache_hit+apply_cache_miss) );
#endif
        }
        tot =  alloc_action_sent * sizeof(action_t);
        tot += alloc_shadow_sent * sizeof(shadow_t);
        tot += total_prog_block_size;
        tot += total_array_size();
        tot += tot_alloc_object_size;
        if (verbose)
        {
#ifdef DEBUG
            unsigned long count;
            object_t *ob;
#endif

            strbuf_add(sbuf, "\nObject status:\n");
            strbuf_add(sbuf, "--------------\n");
            strbuf_addf(sbuf, "Objects total:\t\t\t %8ld\n"
                             , tot_alloc_object);
#ifndef DEBUG
            strbuf_addf(sbuf, "Objects in list:\t\t %8lu\n"
                             , (unsigned long)num_listed_objs);
            strbuf_addf(sbuf, "Objects newly destructed:\t\t %8ld\n"
                             , num_newly_destructed);
            strbuf_addf(sbuf, "Objects destructed:\t\t %8ld\n"
                             , num_destructed);
#else
            for (count = 0, ob = obj_list; ob != NULL; ob = ob->next_all)
                count++;
            if (count != (long)num_listed_objs)
            {
                debug_message("DEBUG: num_listed_objs mismatch: listed %lu, counted %lu\n"
                             , (unsigned long)num_listed_objs, count);
                strbuf_addf(sbuf, "Objects in list:\t\t %8lu (counted %lu)\n"
                                 , (unsigned long)num_listed_objs, count);
            }
            else
                strbuf_addf(sbuf, "Objects in list:\t\t %8lu\n"
                                 , (unsigned long)num_listed_objs);
            for (count = 0, ob = newly_destructed_objs; ob != NULL; ob = ob->next_all)
                count++;
            if (count != num_newly_destructed)
            {
                debug_message("DEBUG: num_newly_destructed mismatch: listed %ld, counted %lu\n"
                             , num_newly_destructed, count);
                strbuf_addf(sbuf, "Objects newly destructed:\t\t %8ld (counted %lu)\n"
                                 , num_newly_destructed, count);
            }
            else
                strbuf_addf(sbuf, "Objects newly destructed:\t %8ld\n"
                                 , num_newly_destructed);
            for (count = 0, ob = destructed_objs; ob != NULL; ob = ob->next_all)
                count++;
            if (count != num_destructed)
            {
                debug_message("DEBUG: num_destructed mismatch: listed %ld, counted %lu\n"
                             , num_destructed, count);
                strbuf_addf(sbuf, "Objects destructed:\t\t %8ld (counted %lu)\n"
                                 , num_destructed, count);
            }
            else
                strbuf_addf(sbuf, "Objects destructed:\t\t %8ld\n"
                                 , num_destructed);
#endif

            strbuf_addf(sbuf, "Objects processed in last cycle: "
                               "%8lu (%5.1lf%% - avg. %5.1lf%%)\n"
                       , (unsigned long)num_last_processed
                       , (float)num_last_processed / (float)num_listed_objs * 100.0
                       , 100.0 * relate_statistics(stat_last_processed, stat_in_list)
                       );
#ifdef NEW_CLEANUP
            strbuf_addf(sbuf, "Objects data-cleaned in last cycle: "
                               "%5lu (%5.1lf%% - avg. %5.1lf : %5.1lf%%)\n"
                       , (unsigned long)num_last_data_cleaned
                       , (double)num_last_data_cleaned / (double)num_listed_objs * 100.0
                       , stat_last_data_cleaned.weighted_avg
                       , 100.0 * relate_statistics(stat_last_data_cleaned, stat_in_list)
                       );
#endif
        }
        tot += show_otable_status(sbuf, verbose);
        tot += heart_beat_status(sbuf, verbose);
        tot += add_string_status(sbuf, verbose);
        tot += call_out_status(sbuf, verbose);
        tot += total_mapping_size();
#ifdef USE_STRUCTS
        tot += total_struct_size(sbuf, verbose);
#endif
        tot += rxcache_status(sbuf, verbose);
        if (verbose)
        {
            strbuf_add(sbuf, "\nOther:\n");
            strbuf_add(sbuf, "------\n");
        }
        tot += show_lexer_status(sbuf, verbose);
        tot += show_comm_status(sbuf, verbose);
        if (!verbose)
        {
            size_t other;

            other = wiz_list_size();
            other += swap_overhead();
            other += num_simul_efun * sizeof(function_t);
            other += interpreter_overhead();
            strbuf_addf(sbuf, "Other structures\t\t\t %9zu\n", other);
            tot += other;
        }
        tot += mb_status(sbuf, verbose);
        tot += res;

        if (!verbose) {
            strbuf_add(sbuf, "\t\t\t\t\t ---------\n");
            strbuf_add(sbuf, "Total:\t\t\t\t\t ");
            strbuf_addf(sbuf, "%9zu\n", tot);
        }
        return MY_TRUE;
    }

    if (strcmp(buff, "swap") == 0)
    {
        swap_status(sbuf);
        return MY_TRUE;
    }

    if (strcmp(buff, "malloc") == 0) {
        mem_dump_data(sbuf);
        return MY_TRUE;
    }

    if (strcmp(buff, "malloc extstats") == 0) {
        mem_dump_extdata(sbuf);
        return MY_TRUE;
    }

    return MY_FALSE;
} /* status_parse() */

/*-------------------------------------------------------------------------*/
void
dinfo_data_status (svalue_t *svp, int value)

/* Fill in the "status" data for debug_info(DINFO_DATA, DID_STATUS)
 * into the svalue-block <svp>.
 * If <value> is -1, <svp> points indeed to a value block; other it is
 * the index of the desired value and <svp> points to a single svalue.
 */

{
    STORE_DOUBLE_USED;

#define ST_NUMBER(which,code) \
    if (value == -1) svp[which].u.number = code; \
    else if (value == which) svp->u.number = code

#define ST_DOUBLE(which,code) \
    if (value == -1) { \
        svp[which].type = T_FLOAT; \
        STORE_DOUBLE(svp+which, code); \
    } else if (value == which) { \
        svp->type = T_FLOAT; \
        STORE_DOUBLE(svp, code); \
    }

    ST_NUMBER(DID_ST_ACTIONS,           alloc_action_sent);
    ST_NUMBER(DID_ST_ACTIONS_SIZE,      alloc_action_sent * sizeof (action_t));
    ST_NUMBER(DID_ST_SHADOWS,           alloc_shadow_sent);
    ST_NUMBER(DID_ST_SHADOWS_SIZE,      alloc_shadow_sent * sizeof (shadow_t));

    ST_NUMBER(DID_ST_OBJECTS,           tot_alloc_object);
    ST_NUMBER(DID_ST_OBJECTS_SIZE,      tot_alloc_object_size);
    ST_NUMBER(DID_ST_OBJECTS_SWAPPED,   num_vb_swapped);
    ST_NUMBER(DID_ST_OBJECTS_SWAP_SIZE, total_vb_bytes_swapped);
    ST_NUMBER(DID_ST_OBJECTS_LIST,      num_listed_objs);
    ST_NUMBER(DID_ST_OBJECTS_NEWLY_DEST, num_newly_destructed);
    ST_NUMBER(DID_ST_OBJECTS_DESTRUCTED, num_destructed);
    ST_NUMBER(DID_ST_OBJECTS_PROCESSED, num_last_processed);
    ST_DOUBLE(DID_ST_OBJECTS_AVG_PROC, relate_statistics(stat_last_processed, stat_in_list));
    /* TODO: Maybe add number of objects data cleaned here as well. */

    ST_NUMBER(DID_ST_ARRAYS,         num_arrays);
    ST_NUMBER(DID_ST_ARRAYS_SIZE,    total_array_size());

    ST_NUMBER(DID_ST_MAPPINGS,       num_mappings);
    ST_NUMBER(DID_ST_MAPPINGS_SIZE,  total_mapping_size());
    ST_NUMBER(DID_ST_HYBRID_MAPPINGS, num_dirty_mappings);
    ST_NUMBER(DID_ST_HASH_MAPPINGS,   num_hash_mappings);

    ST_NUMBER(DID_ST_PROGS,          total_num_prog_blocks + num_swapped
                                                           - num_unswapped);
    ST_NUMBER(DID_ST_PROGS_SIZE,     total_prog_block_size + total_bytes_swapped
                                                           - total_bytes_unswapped);
    ST_NUMBER(DID_ST_PROGS_SWAPPED,   num_swapped - num_unswapped);
    ST_NUMBER(DID_ST_PROGS_SWAP_SIZE, total_bytes_swapped - total_bytes_unswapped);

    ST_NUMBER(DID_ST_USER_RESERVE,   reserved_user_size);
    ST_NUMBER(DID_ST_MASTER_RESERVE, reserved_master_size);
    ST_NUMBER(DID_ST_SYSTEM_RESERVE, reserved_system_size);

#ifdef COMM_STAT
    ST_NUMBER(DID_ST_ADD_MESSAGE, add_message_calls);
    ST_NUMBER(DID_ST_PACKETS,     inet_packets);
    ST_NUMBER(DID_ST_PACKET_SIZE, inet_volume);
    ST_NUMBER(DID_ST_PACKETS_IN,     inet_packets_in);
    ST_NUMBER(DID_ST_PACKET_SIZE_IN, inet_volume_in);
#else
    ST_NUMBER(DID_ST_ADD_MESSAGE, -1);
    ST_NUMBER(DID_ST_PACKETS,     -1);
    ST_NUMBER(DID_ST_PACKET_SIZE, -1);
    ST_NUMBER(DID_ST_PACKETS_IN,     -1);
    ST_NUMBER(DID_ST_PACKET_SIZE_IN, -1);
#endif
#ifdef APPLY_CACHE_STAT
    ST_NUMBER(DID_ST_APPLY,      apply_cache_hit+apply_cache_miss);
    ST_NUMBER(DID_ST_APPLY_HITS, apply_cache_hit);
#else
    ST_NUMBER(DID_ST_APPLY,      -1);
    ST_NUMBER(DID_ST_APPLY_HITS, -1);
#endif

#undef ST_NUMBER
#undef ST_DOUBLE
} /* dinfo_data_status() */

/*-------------------------------------------------------------------------*/
string_t *
check_valid_path (string_t *path, object_t *caller, string_t* call_fun, Bool writeflg)

/* Object <caller> will read resp. write (<writeflg>) the file <path>
 * for the efun <call_fun>.
 *
 * Check the validity of the operation by calling master:valid_read() resp.
 * valid_write().
 *
 * If the operation is valid, the path to use is returned (always without
 * leading '/', the path "/" will be returned as ".").
 *
 * The result string has its own reference, but may be <path> again.
 *
 * If the operation is invalid, NULL is returned.
 */

{
    svalue_t *v;
    wiz_list_t *eff_user;

    if (path)
        push_ref_string(inter_sp, path);
    else
        push_number(inter_sp, 0);

    if ( NULL != (eff_user = caller->eff_user)  && NULL != eff_user->name)
        push_ref_string(inter_sp, eff_user->name);
    else
        push_number(inter_sp, 0);

    push_ref_string(inter_sp, call_fun);
    push_ref_valid_object(inter_sp, caller, "check_valid_path");
    if (writeflg)
        v = apply_master(STR_VALID_WRITE, 4);
    else
        v = apply_master(STR_VALID_READ, 4);

    if (!v || (v->type == T_NUMBER && v->u.number == 0))
        return NULL;

    if (v->type != T_STRING)
    {
        if (!path)
        {
            debug_message("%s master returned bogus filename\n", time_stamp());
            return NULL;
        }
        (void)ref_mstring(path);
    }
    else if (v->u.str == path)
    {
        (void)ref_mstring(path);
    }
    else
    {
        path = ref_mstring(v->u.str);
    }

    if (get_txt(path)[0] == '/')
    {
        string_t *npath;
        memsafe(npath = del_slash(path), mstrsize(path)-1
               , "path for file operation");
        free_mstring(path);
        path = npath;
    }

    /* The string "/" will be converted to "." */
    if (mstreq(path, STR_EMPTY))
    {
        free_mstring(path);
        path = ref_mstring(STR_PERIOD);
    }

    if (legal_path(get_txt(path)))
    {
        return path;
    }

    /* Push the path onto the VM stack so that errorf() can free it */
    push_string(inter_sp, path);
    errorf("Illegal path '%s' for %s() by %s\n", get_txt(path), get_txt(call_fun)
         , get_txt(caller->name));
    return NULL;
} /* check_valid_path() */

/*-------------------------------------------------------------------------*/
void
init_empty_callback (callback_t *cb)

/* Initialize *<cb> to be an empty initialized callback.
 * Use this to initialize callback structures which might be freed before
 * completely filled in.
 */

{
    cb->num_arg = 0;
    cb->is_lambda = MY_FALSE;
    cb->function.named.ob = NULL;
    cb->function.named.name = NULL;
} /* init_empty_callback() */

/*-------------------------------------------------------------------------*/
static INLINE void
free_callback_args (callback_t *cb)

/* Free the function arguments in the callback <cb>.
 */

{
    svalue_t *dest;
    int nargs;

    nargs = cb->num_arg;

    if (nargs == 1)
    {
        if (cb->arg.type != T_INVALID)
            free_svalue(&(cb->arg));
    }
    else if (nargs > 1 && !cb->arg.x.extern_args)
    {
        dest = cb->arg.u.lvalue;

        while (--nargs >= 0)
            if (dest->type != T_INVALID)
                free_svalue(dest++);

        xfree(cb->arg.u.lvalue);
    }

    cb->arg.type = T_INVALID;
    cb->num_arg = 0;
} /* free_callback_args() */

/*-------------------------------------------------------------------------*/
void
free_callback (callback_t *cb)

/* Free the data and references held by callback structure <cb>.
 * The structure itself remains because usually it is embedded within
 * another structure.
 *
 * Repeated calls for the same callback structure are legal.
 */

{
    if (cb->is_lambda && cb->function.lambda.type != T_INVALID)
    {
        free_svalue(&(cb->function.lambda));
        cb->function.lambda.type = T_INVALID;
    }
    else if (!(cb->is_lambda))
    {
        if (cb->function.named.ob)
            free_object(cb->function.named.ob, "free_callback");
        if (cb->function.named.name)
            free_mstring(cb->function.named.name);
        cb->function.named.ob = NULL;
        cb->function.named.name = NULL;
    }

    free_callback_args(cb);
} /* free_callback() */

/*-------------------------------------------------------------------------*/
static INLINE int
setup_callback_args (callback_t *cb, int nargs, svalue_t * args
                    , Bool delayed_callback)

/* Setup the function arguments in the callback <cb> to hold the <nargs>
 * arguments starting from <args>. If <delayed_callback> is FALSE, 
 * the callback will happen within the current LPC cycle:  no argument may be
 * a protected lvalue, but normal lvalues are ok. If TRUE, the callback
 * will happen at a later time: protected lvalues are ok, but not normal ones.
 *
 * The arguments are transferred into the callback structure.
 *
 * Result is -1 on success, or, when encountering an illegal argument,
 * the index of the faulty argument (but even then all caller arguments
 * have been transferred or freed).
 *
 * TODO: It should be possible to accept protected lvalues by careful
 * TODO:: juggling of the protector structures. That, or rewriting the
 * TODO:: lvalue system.
 */

{
    svalue_t *dest;

    cb->num_arg = nargs;

    if (nargs < 1)
    {
        cb->arg.type = T_INVALID;
        cb->num_arg = 0;
    }
    else
    {
        /* Transfer the arguments into the callback structure */

        if (nargs > 1)
        {
            xallocate(dest, sizeof(*dest) * nargs, "callback structure");
            cb->arg.type = T_LVALUE;
            cb->arg.u.lvalue = dest;
            cb->arg.x.extern_args = MY_FALSE;
        }
        else
            dest = &(cb->arg);

        while (--nargs >= 0)
        {
            Bool dontHandle = MY_FALSE;

            if (args->type == T_LVALUE)
            {
                /* Check if we are allowed to handle the lvalues. */
                Bool isProtected
                  = (   args->u.lvalue->type == T_PROTECTED_CHAR_LVALUE
                     || args->u.lvalue->type == T_PROTECTED_STRING_RANGE_LVALUE
                     || args->u.lvalue->type == T_PROTECTED_POINTER_RANGE_LVALUE
                     || args->u.lvalue->type == T_PROTECTED_LVALUE
                    );

                dontHandle =    ( delayed_callback && !isProtected)
                             || (!delayed_callback &&  isProtected)
                             ;
            }

            if (dontHandle)
            {
                /* We don't handle the lvalue - abort the process.
                 * But to do that, we first have to free all
                 * remaining arguments from the caller.
                 */

                int error_index = cb->num_arg - nargs - 1;

                do {
                    free_svalue(args++);
                    (dest++)->type = T_INVALID;
                } while (--nargs >= 0);

                free_callback_args(cb);

                return error_index;
            }

            transfer_svalue_no_free(dest++, args++);
        }
    }

    /* Success */
    return -1;
} /* setup_callback_args() */

/*-------------------------------------------------------------------------*/
int
setup_function_callback ( callback_t *cb, object_t * ob, string_t * fun
                        , int nargs, svalue_t * args, Bool delayed_callback)

/* Setup the empty/uninitialized callback <cb> to hold a function
 * call to <ob>:<fun> with the <nargs> arguments starting from <args>.
 * If <delayed_callback> is FALSE, the callback will happen within the current
 * LPC cycle:  no argument may be a protected lvalue, but normal lvalues are
 * ok. If TRUE, the callback will happen at a later time: protected lvalues
 * are ok, but not normal ones.
 *
 * Both <ob> and <fun> are copied from the caller, but the arguments are
 * adopted (taken away from the caller).
 *
 * Result is -1 on success, or, when encountering an illegal argument,
 * the index of the faulty argument (but even then all caller arguments
 * have been transferred or freed).
 */

{
    int error_index;

    cb->is_lambda = MY_FALSE;
    cb->function.named.name = make_tabled_from(fun); /* for faster apply()s */
    cb->function.named.ob = ref_object(ob, "callback");

    error_index = setup_callback_args(cb, nargs, args, delayed_callback);
    if (error_index >= 0)
    {
        free_object(cb->function.named.ob, "callback");
        free_mstring(cb->function.named.name);
        cb->function.named.ob = NULL;
        cb->function.named.name = NULL;
    }

    return error_index;
} /* setup_function_callback() */

/*-------------------------------------------------------------------------*/
int
setup_closure_callback ( callback_t *cb, svalue_t *cl
                       , int nargs, svalue_t * args, Bool delayed_callback)

/* Setup the empty/uninitialized callback <cb> to hold a closure
 * call to <cl> with the <nargs> arguments starting from <args>.
 * If <delayed_callback> is FALSE, the callback will happen within the current
 * LPC cycle:  no argument may be a protected lvalue, but normal lvalues are
 * ok. If TRUE, the callback will happen at a later time: protected lvalues
 * are ok, but not normal ones.
 *
 * Both <cl> and the arguments are adopted (taken away from the caller).
 *
 * Result is -1 on success, or, when encountering an illegal argument,
 * the index of the faulty argument (but even then all caller arguments
 * have been transferred or freed).
 */

{
    int error_index = -1;

    cb->is_lambda = MY_TRUE;
    transfer_svalue_no_free(&(cb->function.lambda), cl);

    if (cb->function.lambda.x.closure_type == CLOSURE_UNBOUND_LAMBDA
     || cb->function.lambda.x.closure_type == CLOSURE_PRELIMINARY
       )
    {
        /* Uncalleable closure  */
        error_index = 0;
        free_svalue(&(cb->function.lambda));
        cb->function.lambda.type = T_INVALID;
    }
    else
    {
        error_index = setup_callback_args(cb, nargs, args, delayed_callback);
        if (error_index >= 0)
        {
            free_svalue(&(cb->function.lambda));
            cb->function.lambda.type = T_INVALID;
            error_index++;
        }
    }

    return error_index;
} /* setup_closure_callback() */

/*-------------------------------------------------------------------------*/
int
setup_efun_callback_base ( callback_t *cb, svalue_t *args, int nargs
                         , Bool bNoObj)

/* Setup the empty/uninitialized callback <cb> with the <nargs>
 * values starting at <args>. This function is used to implement the
 * callbacks for efuns like map_array() and accepts these forms:
 *
 *   (string fun)
 *   (string fun, mixed extra, ...) TODO: This form is UGLY!
 *   (closure cl, mixed extra, ...)
 *
 * If bNoObj is FALSE (the usual case), this form is also allowed:
 *
 *   (string fun, string|object obj, mixed extra, ...)
 *
 * If the first argument is a string and the second neither an object
 * nor a string, this_object() is used as object specification. Ditto
 * if bNoObj is used.
 *
 * All arguments are adopted (taken away from the caller). Protected lvalues
 * like &(i[0]) are not allowed as 'extra' arguments.
 *
 * Result is -1 on success, or, when encountering an illegal argument,
 * the index of the faulty argument (but even then all caller arguments
 * have been transferred or freed).
 *
 * This function is #defined to two macros:
 *
 *   setup_efun_callback(cb,args,nargs) -> bNoObj == FALSE
 *   setup_efun_callback_noobj(cb,args,nargs) -> bNoObj == TRUE
 *
 * The no-object feature is to support old-fashioned efun
 * unique_array().
 */

{
    int error_index;

    if (args[0].type == T_CLOSURE)
    {
        error_index = setup_closure_callback(cb, args, nargs-1, args+1, MY_FALSE);
    }
    else if (args[0].type == T_STRING)
    {
        object_t *ob;
        int       first_arg;

        first_arg = 1;

        if (nargs > 1)
        {
            if (bNoObj)
            {
                ob = current_object;
                first_arg = 1;
            }
            else
            {
                if (args[1].type == T_OBJECT)
                {
                    ob = args[1].u.ob;
                    first_arg = 2;
                }
                else if (args[1].type == T_STRING)
                {
                    ob = get_object(args[1].u.str);
                    first_arg = 2;
                }
                else
                {
                    /* TODO: It would be better to throw an error here */
                    ob = current_object;
                    first_arg = 1;
                }
            }
        }
        else
            ob = current_object;

        if (ob != NULL)
        {
            error_index = setup_function_callback(cb, ob, args[0].u.str
                                                 , nargs-first_arg
                                                 , args+first_arg
                                                 , MY_FALSE);
            if (error_index >= 0)
                error_index += first_arg;
        }
        else
        {
            /* We couldn't find an object to call, so we have
             * to manually prepare the error condition.
             */
            int i;

            for (i = first_arg; i < nargs; i++)
                free_svalue(args+i);

            error_index = 1;
        }

        /* Free the function spec */
        free_svalue(args);
        if (first_arg > 1)
            free_svalue(args+1);
    }
    else
    {
        /* We couldn't find anything to call, so we have
         * to manually prepare the error condition.
         */
        int i;

        for (i = 0; i < nargs; i++)
            free_svalue(args+i);

        error_index = 0;
    }

    return error_index;
} /* setup_efun_callback_base() */

/*-------------------------------------------------------------------------*/
void
callback_change_object (callback_t *cb, object_t *obj)

/* Change the object the callback is bound to, if it is a function callback.
 * A new reference is added to <obj>.
 */

{
    object_t *old;
    if (cb->is_lambda)
    {
        fatal("callback_change_object(): Must not be called with a closure callback.");
        /* NOTREACHED */
        return;
    }
    
    old = cb->function.named.ob;
    cb->function.named.ob = ref_object(obj, "change callback");

    if (old)
        free_object(old, "change_callback");
} /* callback_change_object() */

/*-------------------------------------------------------------------------*/
object_t *
callback_object (callback_t *cb)

/* Return the object to call from the callback structure <cb>.
 * If the object is destructed, return NULL.
 */

{
    object_t *ob;

    if (cb->is_lambda)
        ob = !CLOSURE_MALLOCED(cb->function.lambda.x.closure_type)
             ? cb->function.lambda.u.ob
             : cb->function.lambda.u.lambda->ob;
    else
        ob = cb->function.named.ob;

    return check_object(ob);
} /* callback_object() */

/*-------------------------------------------------------------------------*/
svalue_t *
callback_function (callback_t *cb)

/* Returns the function to call from the callback structure <cb>.
 * It returns a pointer to a statically allocated svalue_t,
 * which contains either a T_STRING or T_CLOSURE.
 * It's the caller's responsibility to free its contents.
 */

{
    static svalue_t fun;

    if (cb->is_lambda)
        assign_svalue_no_free(&fun, &(cb->function.lambda));
    else
        put_ref_string(&fun, cb->function.named.name);

    return &fun;
} /* callback_object() */

/*-------------------------------------------------------------------------*/
svalue_t *
execute_callback (callback_t *cb, int nargs, Bool keep, Bool toplevel)

/* Call the callback <cb> with the <nargs> arguments already pushed
 * onto the stack. Result is a pointer to a static area with the
 * result from the call.
 *
 * If an error occurs (the object to call has been destructed or can't
 * be swapped in), NULL is returned.
 *
 * If <keep> is TRUE, the callback structure will not be freed.
 * If <toplevel> is TRUE, the callback is called directly from
 * the backend (as opposed to from a running program) which makes
 * certain extra setups for current_object and current_prog necessary.
 *
 * This function is #defined to two macros:
 *
 *   apply_callback(cb,nargs): call a callback from a running program,
 *                             the callback is kept.
 *   backend_callback(cb,nargs): call a callback from the backend
 *                             and free it afterwards.
 */

{
    object_t *ob;
    int num_arg;

    ob = callback_object(cb);
    if (!ob
     || (O_PROG_SWAPPED(ob) && load_ob_from_swap(ob) < 0)
       )
    {
        while (nargs-- > 0)
            free_svalue(inter_sp--);
        free_callback(cb);
        return NULL;
    }

    /* Push the arguments, if any, onto the stack */

    num_arg = cb->num_arg;

    if (num_arg)
    {
        svalue_t * argp;
        int j;

        if (num_arg > 1)
            argp = cb->arg.u.lvalue;
        else
            argp = &(cb->arg);

        for (j = 0; j < num_arg; j++, argp++)
        {
            inter_sp++;
            if (destructed_object_ref(argp))
            {
                *inter_sp = const0;
                assign_svalue(argp, &const0);
            }
            else if (keep)
                assign_svalue_no_free(inter_sp, argp);
            else
                transfer_svalue_no_free(inter_sp, argp);
        }

    }

    if (!keep)
    {
        /* The arguments are gone from the callback */

        if (cb->num_arg > 1)
            xfree(cb->arg.u.lvalue);
        cb->num_arg = 0;
        cb->arg.type = T_INVALID;
    }

    /* Now call the function */

    if (toplevel)
        current_object = ob; /* Need something valid here */

    if (cb->is_lambda)
    {
        if (toplevel
         && cb->function.lambda.x.closure_type < CLOSURE_SIMUL_EFUN
         && cb->function.lambda.x.closure_type >= CLOSURE_EFUN)
        {
            /* efun, operator or sefun closure called from the backend:
             * we need the program for a proper traceback. We made sure
             * before that the program has been swapped in.
             */
            current_prog = ob->prog;
        }

        call_lambda(&(cb->function.lambda), num_arg + nargs);
        transfer_svalue(&apply_return_value, inter_sp);
        inter_sp--;

        if (toplevel)
            current_prog = NULL;
    }
    else
    {
        if (toplevel)
            tracedepth = 0;

        if (!sapply(cb->function.named.name, ob, num_arg + nargs))
            transfer_svalue(&apply_return_value, &const0);
    }

    if (!keep)
    {
        /* Free the remaining information from the callback structure */
        free_callback(cb);
    }

    /* Return the result */
    return &apply_return_value;
} /* execute_callback() */

/*-------------------------------------------------------------------------*/
#ifdef DEBUG

void
count_callback_extra_refs (callback_t *cb)

/* Count all the refs in the callback to verify the normal refcounting. */

{
    if (!cb->is_lambda)
        count_extra_ref_in_object(cb->function.named.ob);
    else
        count_extra_ref_in_vector(&cb->function.lambda, 1);
    if (cb->num_arg == 1)
        count_extra_ref_in_vector(&(cb->arg), 1);
    else if (cb->num_arg > 1)
        count_extra_ref_in_vector(cb->arg.u.lvalue, (size_t)cb->num_arg);
} /* count_callback_extra_refs() */

#endif /* DEBUG */

#ifdef GC_SUPPORT

/*-------------------------------------------------------------------------*/
void
clear_ref_in_callback (callback_t *cb)

/* GC support: clear the refs in the memory held by the callback
 * structure (but not of the structure itself!)
 */

{
    if (cb->num_arg == 1)
        clear_ref_in_vector(&(cb->arg), 1);
    else if (cb->num_arg > 1)
    {
        clear_ref_in_vector(cb->arg.u.lvalue, (size_t)cb->num_arg);
        if (!cb->arg.x.extern_args)
            clear_memory_reference(cb->arg.u.lvalue);
    }

    if (cb->is_lambda)
        clear_ref_in_vector(&(cb->function.lambda), 1);
    else
    {
#ifdef DEBUG
        if (!callback_object(cb))
            fatal("GC run on callback with stale object.\n");
#endif
        clear_object_ref(cb->function.named.ob);
    }
} /* clear_ref_in_callback() */

/*-------------------------------------------------------------------------*/
void
count_ref_in_callback (callback_t *cb)

/* GC support: count the refs in the memory held by the callback
 * structure (but not of the structure itself!)
 */

{
    if (cb->num_arg == 1)
        count_ref_in_vector(&(cb->arg), 1);
    else if (cb->num_arg > 1)
    {
        count_ref_in_vector(cb->arg.u.lvalue, (size_t)cb->num_arg);
        if (!cb->arg.x.extern_args)
            note_malloced_block_ref(cb->arg.u.lvalue);
    }

#ifdef DEBUG
    if (!callback_object(cb))
        fatal("GC run on callback with stale object.\n");
#endif
    if (cb->is_lambda)
        count_ref_in_vector(&(cb->function.lambda), 1);
    else
    {
        cb->function.named.ob->ref++;
        count_ref_from_string(cb->function.named.name);
    }
} /* count_ref_in_callback() */

#endif

/*-------------------------------------------------------------------------*/
void
init_driver_hooks()

/* Init the driver hooks.
 */

{
    int i;

    for (i = NUM_DRIVER_HOOKS; --i >= 0; )
    {
        put_number(driver_hook + i, 0);
    }
} /* init_driver_hooks() */

/*-------------------------------------------------------------------------*/
Bool
match_string (const char * match, const char * str, mp_int len)

/* Test if the string <str> of length <len> matches the pattern <match>.
 * Allowed wildcards are
 *   *: matches any sequence
 *   ?: matches any single character
 *   \: escapes the following wildcard
 *
 * The function is used by the compiler for inheritance specs, and by
 * f_get_dir().
 * TODO: Another utils.c candidate.
 */

{
    /* Loop over match and str */
    for (;;)
    {
        /* Act on the current match character */
        switch(*match)
        {
        case '?':
            if (--len < 0)
                return MY_FALSE;
            str++;
            match++;
            continue;

        case '*':
          {
            char *str2;
            mp_int matchlen;

            for (;;)
            {
                switch (*++match)
                {
                case '\0':
                    return len >= 0;
                case '?':
                    --len;
                    str++;
                case '*':
                    continue;
                case '\\':
                    match++;
                default:
                    break;
                }
                break;
            }

            if (len <= 0)
                return MY_FALSE;

            str2 = strpbrk(match + 1, "?*\\");
            if (!str2)
            {
                if ( (matchlen = strlen(match)) > len)
                    return MY_FALSE;
                return strncmp(match, str + len - matchlen, matchlen) == 0;
            }
            else
            {
                matchlen = str2 - match;
            }
            /* matchlen >= 1 */
            if ((len -= matchlen) >= 0) do
            {
                if ( !(str2 = xmemmem(str, len + matchlen, match, matchlen)) )
                    return MY_FALSE;
                len -= str2 - str;
                if (match_string(match + matchlen, str2 + matchlen, len))
                    return MY_TRUE;
                str = str2 + 1;
            } while (--len >= 0);
            return MY_FALSE;
          }

        case '\0':
            return len == 0;

        case '\\':
            match++;
            if (*match == '\0')
                return MY_FALSE;
            /* Fall through ! */

        default:
            if (--len >= 0 && *match == *str)
            {
                match++;
                str++;
                continue;
            }
            return MY_FALSE;
        } /* switch(*match) */
    } /* for(;;) */
} /* match_string() */

/*-------------------------------------------------------------------------*/
void
print_svalue (svalue_t *arg)

/* Print the value <arg> to the interactive user (exception: strings
 * are also written to non-interactive command_givers via tell_npc()).
 * The function is called for the efun write() and from
 * interpret:do_trace_call().
 *
 * The function can only print scalar values - arrays, mappings and
 * closures are only hinted at.
 */

{
    if (arg == NULL)
    {
        add_message("<NULL>");
    }
    else if (arg->type == T_STRING)
    {
        interactive_t *ip;

        /* Strings sent to monsters are now delivered */
        if (command_giver && (command_giver->flags & O_ENABLE_COMMANDS)
         && !(O_SET_INTERACTIVE(ip, command_giver)) )
        {
            tell_npc(command_giver, arg->u.str);
        }
        else
        {
            add_message(FMT_STRING, arg->u.str);
        }
    }
    else if (arg->type == T_OBJECT)
        add_message("OBJ(%s)", get_txt(arg->u.ob->name));
    else if (arg->type == T_NUMBER)
        add_message("%"PRIdPINT, arg->u.number);
    else if (arg->type == T_FLOAT)
    {
        char buff[120];

        snprintf(buff, sizeof(buff), "%g", READ_DOUBLE( arg ) );
        add_message("%s", buff);
    }
    else if (arg->type == T_POINTER)
        add_message("<ARRAY>");
    else if (arg->type == T_MAPPING)
        add_message("<MAPPING>");
    else if (arg->type == T_CLOSURE)
        add_message("<CLOSURE>");
    else
        add_message("<OTHER:%"PRIdPHINT">", arg->type);
} /* print_svalue() */

/*=========================================================================*/

/*                              EFUNS                                      */

/*-------------------------------------------------------------------------*/
svalue_t *
f_clone_object (svalue_t * sp)

/* EFUN clone_object()
 *
 *   object clone_object(string name)
 *   object clone_object(object template)
 *
 * Clone a new object from definition <name>, or alternatively from
 * the object <template>. In both cases, the new object is given an
 * unique name and returned.
 */

{
    object_t *ob;

    /* Get the argument and clone the object */
    if (sp->type == T_STRING)
    {
        ob = clone_object(sp->u.str);
    }
    else
    {
        ob = clone_object(sp->u.ob->load_name);
    }

    free_svalue(sp);

    if (ob)
    {
        put_ref_object(sp, ob, "F_CLONE_OBJECT");
    }
    else
        put_number(sp, 0);

    return sp;
} /* f_clone_object() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_destruct (svalue_t * sp)

/* EFUN destruct()
 *
 *   void destruct(object ob)
 *
 * Completely destroy and remove object ob (if not already done so).
 * After the call to destruct(), no global variables will exist any
 * longer, only local ones, and arguments.
 *
 * If an object self-destructs, it will not immediately terminate
 * execution. If the efun this_object() will be called by the
 * destructed object, the result will be 0.
 *
 * The efun accepts destructed objects as argument (which appear
 * as the number 0) and the simply acts as a no-op in that case.
 *
 * Internally, the object is not destructed immediately, but
 * instead put into a list and finally destructed after the
 * current execution has ended.
 */

{
    if (T_NUMBER != sp->type || sp->u.number)
    {
        if (sp->type != T_OBJECT)
            efun_arg_error(1, T_OBJECT, sp->type, sp);
        destruct_object(sp);
    }
    free_svalue(sp);
    sp--;

    return sp;
} /* f_destruct() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_find_object (svalue_t *sp)

/* EFUN find_object()
 *
 *   object find_object(string str)
 *
 * Find an object with the file_name str. If the object isn't loaded,
 * it will not be found.
 */

{
    object_t *ob;

    ob = find_object(sp->u.str);
    free_svalue(sp);
    if (ob)
        put_ref_object(sp, ob, "find_object");
    else
        put_number(sp, 0);

    return sp;
} /* f_find_object() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_load_object (svalue_t *sp)

/* EFUN load_object()
 *
 *   object load_object(string name)
 *
 * Load the object from the file <name> and return it. If the
 * object already exists, just return it.
 *
 * This efun can be used only to load blueprints - for clones, use
 * the efun clone_object().
 */

{
    object_t *ob;

    ob = get_object(sp->u.str);
    free_svalue(sp);
    if (ob)
        put_ref_object(sp, ob, "F_LOAD_OBJECT");
    else
        put_number(sp, 0);
    return sp;
} /* f_load_object() */

/*-------------------------------------------------------------------------*/
static Bool
validate_shadowing (object_t *ob)

/* May current_object shadow object 'ob'? We perform a number of tests
 * including calling master:query_allow_shadow().
 * TODO: Move all shadow functions into a separate file.
 */

{
    int i, j;
    object_t *cob;
    program_t *shadow, *victim;
    svalue_t *ret;

    cob = current_object;
    shadow = cob->prog;

    if (cob->flags & O_DESTRUCTED)
        return MY_FALSE;

    if (O_PROG_SWAPPED(ob))
        if (load_ob_from_swap(ob) < 0)
            errorf("Out of memory: unswap object '%s'\n", get_txt(ob->name));

    victim = ob->prog;

    if (victim->flags & P_NO_SHADOW)
        errorf("shadow '%s' on '%s': Can't shadow a 'no_shadow' program.\n"
             , get_txt(cob->name), get_txt(ob->name));

    if (cob->flags & O_SHADOW)
    {
        shadow_t *shadow_sent = O_GET_SHADOW(cob);

        if (shadow_sent->shadowing)
            errorf("shadow '%s' on '%s': Already shadowing.\n"
                 , get_txt(cob->name), get_txt(ob->name));
        if (shadow_sent->shadowed_by)
            errorf("shadow '%s' on '%s': Can't shadow when shadowed.\n"
                 , get_txt(cob->name), get_txt(ob->name));
    }

    if (cob->super)
        errorf("shadow '%s' on '%s': The shadow resides inside another object ('%s').\n"
             , get_txt(cob->name), get_txt(ob->name)
             , get_txt(cob->super->name));

    if (ob->flags & O_SHADOW && O_GET_SHADOW(ob)->shadowing)
        errorf("shadow '%s' on '%s': Can't shadow a shadow.\n"
             , get_txt(cob->name), get_txt(ob->name));

    if (ob == cob)
        errorf("shadow '%s' on '%s': Can't shadow self.\n"
             , get_txt(cob->name), get_txt(ob->name));

    /* Make sure that we don't shadow 'nomask' functions.
     */
    for (i = shadow->num_function_names; --i >= 0; )
    {
        funflag_t flags;
        string_t *name;
        program_t *progp;

        j = shadow->function_names[i];
        flags = shadow->functions[j];
        progp = shadow;
        while (flags & NAME_INHERITED)
        {
            inherit_t *inheritp;

            inheritp = &progp->inherit[flags & INHERIT_MASK];
            j -= inheritp->function_index_offset;
            progp = inheritp->prog;
            flags = progp->functions[j];
        }

        memcpy(&name, FUNCTION_NAMEP(progp->program + (flags & FUNSTART_MASK))
              , sizeof name
              );

        if ( (j = find_function(name, victim)) >= 0
         && victim->functions[j] & TYPE_MOD_NO_MASK )
        {
            errorf("shadow '%s' on '%s': Illegal to shadow 'nomask' function '%s'.\n"
                 , get_txt(ob->name), get_txt(cob->name), get_txt(name));
        }
    }

    push_ref_object(inter_sp, ob, "shadow");
    ret = apply_master(STR_QUERY_SHADOW, 1);

    if (!((ob->flags|cob->flags) & O_DESTRUCTED)
     && ret && !(ret->type == T_NUMBER && ret->u.number == 0))
    {
        return MY_TRUE;
    }

    return MY_FALSE;
} /* validate_shadowing() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_shadow (svalue_t *sp)

/* EFUN shadow()
 *
 *   object shadow(object ob, int flag)
 *
 * If flag is non-zero then the current object will shadow ob. If
 * flag is 0 then either 0 will be returned or the object that is
 * shadowing ob.
 * 	
 * The calling object must be permitted by the master object to
 * do the shadowing. In most installations, an object that
 * defines the function query_prevent_shadow() to return 1
 * can't be shadowed, and the shadow() function will return 0
 * instead of ob.
 *
 * shadow() also fails if the calling object tries to shadow
 * a function that was defined as ``nomask'', if the program was
 * compiled with the #pragma no_shadow, or if the calling
 * object is already shadowing, is being shadowed, or has an
 * environment. Also the target ob must not be shadowing
 * something else.
 * 	
 * If an object A shadows an object B then all call_other() to B
 * will be redirected to A. If object A has not defined the
 * function, then the call will be passed to B. There is only on
 * object that can call functions in B with call_other(), and
 * that is A. Not even object B can call_other() itself. All
 * normal (internal) function calls inside B will however remain
 * internal to B.
 */	

{
    object_t *ob;

    /* Get the arguments */
    sp--;
    ob = sp->u.ob;
    deref_object(ob, "shadow");

    if (sp[1].u.number == 0)
    {
        /* Just look for a possible shadow */
        ob = (ob->flags & O_SHADOW) ? O_GET_SHADOW(ob)->shadowed_by : NULL;
        if (ob)
            sp->u.ob = ref_object(ob, "shadow");
        else
            put_number(sp, 0);
        return sp;
    }

    sp->type = T_NUMBER; /* validate_shadowing might destruct ob */
    assign_eval_cost();
    inter_sp = sp;
    if (validate_shadowing(ob))
    {
        /* Shadowing allowed */

        shadow_t *shadow_sent, *co_shadow_sent;

        /* The shadow is entered first in the chain.
         */
        assert_shadow_sent(ob);
        if (O_IS_INTERACTIVE(ob))
            O_GET_INTERACTIVE(ob)->catch_tell_activ = MY_TRUE;
        shadow_sent = O_GET_SHADOW(ob);

        while (shadow_sent->shadowed_by)
        {
            ob = shadow_sent->shadowed_by;
            shadow_sent = O_GET_SHADOW(ob);
        }

        assert_shadow_sent(current_object);
        co_shadow_sent = O_GET_SHADOW(current_object);

        co_shadow_sent->shadowing = ob;
        shadow_sent->shadowed_by = current_object;
        put_ref_object(sp, ob, "shadow");
        return sp;
    }

    /* Shadowing not allowed */
    put_number(sp, 0);
    return sp;
} /* f_shadow() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_query_shadowing (svalue_t *sp)

/* EFUN query_shadowing()
 *
 *   object query_shadowing (object obj)
 *
 * The function returns the object which <obj> is currently
 * shadowing, or 0 if <obj> is not a shadow.
 */

{

    object_t *ob;

    ob = sp->u.ob;
    deref_object(ob, "shadow");
    ob = (ob->flags & O_SHADOW) ? O_GET_SHADOW(ob)->shadowing : NULL;
    if (ob)
        sp->u.ob = ref_object(ob, "shadow");
    else
        put_number(sp, 0);

    return sp;
} /* f_query_shadowing() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_unshadow (svalue_t *sp)

/* EFUN unshadow()
 *
 *   void unshadow(void)
 *
 * The calling object stops shadowing any other object.
 * If the calling object is being shadowed, that is also stopped.
 */

{
    shadow_t *shadow_sent, *shadowing_sent;
    object_t *shadowing, *shadowed_by;

    if (current_object->flags & O_SHADOW
     && NULL != (shadowing = (shadow_sent = O_GET_SHADOW(current_object))->shadowing) )
    {
        shadowing_sent = O_GET_SHADOW(shadowing);

        /* Our victim is now shadowed by our shadow */
        shadowed_by = shadow_sent->shadowed_by;
        shadowing_sent->shadowed_by = shadowed_by;

        if ( NULL != shadowed_by )
        {
            /* Inform our shadow about its new victim */
            O_GET_SHADOW(shadowed_by)->shadowing = shadow_sent->shadowing;
        }
        else
        {
            /* Our victim is no longer shadowed, so maybe it
             * doesn't need its shadow sentence anymore.
             */
            remove_shadow_actions(current_object, shadowing);
            check_shadow_sent(shadowing);
        }

        shadow_sent->shadowed_by = NULL;
        shadow_sent->shadowing = NULL;

        check_shadow_sent(current_object);
    }

    return sp;
} /* f_unshadow() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_set_driver_hook (svalue_t *sp)

/* EFUN set_driver_hook()
 *
 *   void set_driver_hook(int what, closure arg)
 *   void set_driver_hook(int what, string arg)
 *   void set_driver_hook(int what, string * arg)
 *
 * This privileged efun sets the driver hook 'what' (values are
 * defined in /sys/driverhooks.h) to 'arg'.
 * The exact meanings and types of 'arg' depend of the hook set.
 * To remove a hook, set 'arg' to 0.
 *
 * Raises a privilege violation ("set_driver_hook", this_object, what).
 *
 * See hooks(C) for a detailed discussion.
 */

{
    p_int n;
    svalue_t old;

    /* Get the arguments */
    n = sp[-1].u.number;

    if (n < 0 || n >= NUM_DRIVER_HOOKS)
    {
        errorf("Bad hook number: %"PRIdPINT", expected 0..%ld\n"
             , n, (long)NUM_DRIVER_HOOKS-1);
        /* NOTREACHED */
        return sp;
    }

    /* Legal call? */
    if (!privilege_violation(STR_SET_DRIVER_HOOK, sp-1, sp))
    {
        free_svalue(sp);
        return sp - 2;
    }

    old = driver_hook[n]; /* Remember this for freeing */

    /* Check the type of the hook and set it if ok
     */
    switch(sp->type)
    {
    case T_NUMBER:
        if (sp->u.number == 0)
        {
            put_number(driver_hook + n, 0);
            break;
        }
        else if (n == H_REGEXP_PACKAGE)
        {
#ifdef HAS_PCRE
            if (sp->u.number != RE_PCRE
             && sp->u.number != RE_TRADITIONAL )
            {
                errorf("Bad value for hook %"PRIdPINT": got 0x%"PRIxPINT
                       ", expected RE_PCRE (0x%lx) or RE_TRADITIONAL (0x%lx).\n"
                     , n, sp->u.number
                     , (long)RE_PCRE, (long)RE_TRADITIONAL
                     );
                break;
            }
#else
            if (sp->u.number != RE_TRADITIONAL )
            {
                errorf("Bad value for hook %"PRIdPINT": got 0x%"PRIxPINT
                       ", RE_TRADITIONAL (0x%lx).\n"
                       , n, sp->u.number, (long)RE_TRADITIONAL
                       );
                break;
            }
#endif // HAS_PCRE
            goto default_test;
        }
        else
        {
            errorf("Bad value for hook %"PRIdPINT": got number, expected %s or 0.\n"
                 , n
                 , efun_arg_typename(hook_type_map[n]));
            break;
        }
        break;

    case T_STRING:
      {
        string_t *str;

        if ( !((1 << T_STRING) & hook_type_map[n]) )
            errorf("Bad value for hook %"PRIdPINT": got string, expected %s.\n"
                 , n
                 , efun_arg_typename(hook_type_map[n]));

        str = make_tabled_from(sp->u.str); /* for faster apply()s */
        put_string(driver_hook + n, str);
        free_svalue(sp);
        if (n == H_NOECHO)
            mudlib_telopts();
        break;
      }

    case T_MAPPING:
        if (!sp->u.map->num_values
         ||  sp->u.map->ref != 1 /* add_to_mapping() could zero num_values */)
        {
            errorf("Bad value for hook %"PRIdPINT": mapping is empty "
                  "or has other references.\n", n);
            return sp;
        }
        goto default_test;

    case T_POINTER:
      {
        vector_t *v = sp->u.vec;

        if (v->ref > 1)
        {
            /* We need a genuine copy of the array */
            deref_array(v);
            sp->u.vec = v = slice_array(v, 0, VEC_SIZE(v)-1);
        }

        if (n == H_INCLUDE_DIRS)
        {
            inter_sp = sp;
            set_inc_list(v);
        }
        goto default_test;
      }

    case T_CLOSURE:
        if (sp->x.closure_type == CLOSURE_UNBOUND_LAMBDA
         && sp->u.lambda->ref == 1)
        {
            driver_hook[n] = *sp;
            driver_hook[n].x.closure_type = CLOSURE_LAMBDA;
            driver_hook[n].u.lambda->ob = ref_object(master_ob, "hook closure");
            if (n == H_NOECHO)
            {
                mudlib_telopts();
            }
            break;
        }
        else if (!CLOSURE_IS_LFUN(sp->x.closure_type))
        {
            errorf("Bad value for hook %"PRIdPINT": unbound lambda or "
                  "lfun closure expected.\n", n);
        }
        /* FALLTHROUGH */

    default:
default_test:
        if ( !((1 << sp->type) & hook_type_map[n]) )
        {
            errorf("Bad value for hook %"PRIdPINT": got %s, expected %s.\n"
                 , n, typename(sp->type), efun_arg_typename(hook_type_map[n]));
            break; /* flow control hint */
        }

        driver_hook[n] = *sp;

        if (n == H_NOECHO)
        {
            mudlib_telopts();
        }
        break;
    }

    if (old.type != T_NUMBER)
        free_svalue(&old);

    return sp - 2;
} /* f_set_driver_hook() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_write (svalue_t *sp)

/* EFUN write()
 *
 *   void write (mixed msg)
 *
 * Write out something to the current user. What exactly will
 * be printed in the end depends of the type of msg.
 *	
 * If it is a string or a number then just prints it out.
 *	
 * If it is an object then the object will be printed in the
 * form: "OBJ("+file_name((object)mix)+")"
 * 	
 * If it is an array just "<ARRAY>" will be printed.
 * If it is a mapping just "<MAPPING>" will be printed.
 * If it is a closure just "<CLOSURE>" will be printed.
 * 	
 * If the write() function is invoked by a command of an living
 * but not interactive object and the given argument is a string
 * then the lfun catch_tell() of the living will be invoked with
 * the message as argument.
 */

{
    object_t *save_command_giver = command_giver;

    if (!command_giver
     && current_object->flags & O_SHADOW
     && O_GET_SHADOW(current_object)->shadowing)
    {
        command_giver = current_object;
    }

    if (command_giver)
    {
        /* Send the message to the first object in the shadow list */
        if (command_giver->flags & O_SHADOW)
            while( O_GET_SHADOW(command_giver)->shadowing )
                command_giver = O_GET_SHADOW(command_giver)->shadowing;
    }

    print_svalue(sp);
    command_giver = check_object(save_command_giver);

    free_svalue(sp); sp--;

    return sp;
} /* f_write() */

/*-------------------------------------------------------------------------*/
static void
set_single_limit ( struct limits_context_s * result
                 , int  limit
                 , svalue_t *svp
                 )

/* Set the limit #<limit> in *<result> to the value in <svp>.
 *
 * If the function encounters illegal limit tags or values, it throws
 * an error.
 */

{
    static char * limitnames[] = { "LIMIT_EVAL", "LIMIT_ARRAY", "LIMIT_MAPPING"
                                 , "LIMIT_MAPPING_SIZE", "LIMIT_BYTE"
                                 , "LIMIT_FILE", "LIMIT_COST", "LIMIT_MEMORY"};

    p_int val;

    if (svp->type != T_NUMBER)
        errorf("Illegal %s value: got a %s, expected a number\n"
             , limitnames[limit], typename(svp[limit].type));

    val = svp->u.number;

    if (limit == LIMIT_COST)
    {
        if (val == LIMIT_DEFAULT)
            result->use_cost = DEF_USE_EVAL_COST;
        else if (val != LIMIT_KEEP)
            result->use_cost = val;
    }
    else
    {
        if (val >= 0)
        {
            switch(limit)
            {
            case LIMIT_EVAL:          result->max_eval = val;     break;
            case LIMIT_ARRAY:         result->max_array = val;    break;
            case LIMIT_MAPPING_KEYS:  result->max_map_keys = val; break;
            case LIMIT_MAPPING_SIZE:  result->max_mapping = val;  break;
            case LIMIT_BYTE:          result->max_byte = val;     break;
            case LIMIT_FILE:          result->max_file = val;     break;
            case LIMIT_CALLOUTS:      result->max_callouts = val; break;
            case LIMIT_MEMORY:        
                result->max_memory = val;
                // LIMIT_MEMORY should be not set to low, otherwise Armageddon
                // will reign upon the world...
                if (result->max_memory > 0 && result->max_memory < 50000)
                {
                    errorf("Illegal LIMIT_MEMORY: %"PRIdPINT", must be 0 or "
                           ">=50000.\n", result->max_memory);
                }
                break;
            default: errorf("Unimplemented limit #%d\n", limit);
            }
        }
        else if (val == LIMIT_DEFAULT)
        {
            switch(limit)
            {
            case LIMIT_EVAL:     result->max_eval = def_eval_cost;
                                 break;
            case LIMIT_ARRAY:    result->max_array = def_array_size;
                                 break;
            case LIMIT_MAPPING_KEYS:
                                 result->max_map_keys = def_mapping_keys;
                                 break;
            case LIMIT_MAPPING_SIZE:
                                 result->max_mapping = def_mapping_size;
                                 break;
            case LIMIT_BYTE:     result->max_byte = def_byte_xfer;
                                 break;
            case LIMIT_FILE:     result->max_file = def_file_xfer;
                                 break;
            case LIMIT_CALLOUTS: result->max_callouts = def_callouts;
                                 break;
            case LIMIT_MEMORY:   result->max_memory = def_memory;
                                 break;            
            default: errorf("Unimplemented limit #%d\n", limit);
            }
        }
        else if (val != LIMIT_KEEP)
            errorf("Illegal %s value: %"PRIdPINT"\n", limitnames[limit], val);
    }
} /* set_single_limit() */

/*-------------------------------------------------------------------------*/
static void
extract_limits ( struct limits_context_s * result
               , svalue_t *svp
               , int  num
               , Bool tagged
               )

/* Extract the user-given runtime limits from <svp>...
 * and store them into <result>. If <tagged> is FALSE, <svp> points to an array
 * with the <num> values stored at the proper indices, otherwise <svp> points
 * to a series of <num>/2 (tag, value) pairs.
 *
 * If the function encounters illegal limit tags or values, it throws
 * an error.
 */

{
    /* Set the defaults (unchanged) limits */
    result->max_eval = max_eval_cost;
    result->max_array = max_array_size;
    result->max_mapping = max_mapping_size;
    result->max_map_keys = max_mapping_keys;
    result->max_callouts = max_callouts;
    result->max_byte = max_byte_xfer;
    result->max_file = max_file_xfer;
    result->max_memory = max_memory;
    result->use_cost = 0;

    if (!tagged)
    {
        int limit;

        for (limit = 0; limit < LIMIT_MAX && limit < num; limit++)
        {
            set_single_limit(result, limit, svp+limit);
        }
    }
    else
    {
        int i;

        for (i = 0; i < num - 1; i += 2)
        {
            p_int limit;

            if (svp[i].type != T_NUMBER)
                errorf("Illegal limit value: got a %s, expected a number\n"
                     , typename(svp[i].type));
            limit = svp[i].u.number;
            if (limit < 0 || limit >= LIMIT_MAX)
                errorf("Illegal limit tag: %"PRIdPINT"\n", limit);

            set_single_limit(result, limit, svp+i+1);
        }
    }
} /* extract_limits() */

/*-------------------------------------------------------------------------*/
static vector_t *
create_limits_array (struct limits_context_s * rtlimits)

/* Create an array with the values from <rtlimits> and return it.
 * Return NULL if out of memory.
 */

{
    vector_t *vec;

    vec = allocate_uninit_array(LIMIT_MAX);
    if (vec)
    {
        put_number(vec->item+LIMIT_EVAL,     rtlimits->max_eval);
        put_number(vec->item+LIMIT_ARRAY,    rtlimits->max_array);
        put_number(vec->item+LIMIT_MAPPING_KEYS,  rtlimits->max_map_keys);
        put_number(vec->item+LIMIT_MAPPING_SIZE,  rtlimits->max_mapping);
        put_number(vec->item+LIMIT_BYTE,     rtlimits->max_byte);
        put_number(vec->item+LIMIT_FILE,     rtlimits->max_file);
        put_number(vec->item+LIMIT_CALLOUTS, rtlimits->max_callouts);
        put_number(vec->item+LIMIT_COST,     rtlimits->use_cost);
        put_number(vec->item+LIMIT_MEMORY,   rtlimits->max_memory);
    }

    return vec;
} /* create_limits_array() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_limited (svalue_t * sp, int num_arg)

/* EFUN limited()
 *
 *   mixed limited(closure fun)
 *   mixed limited(closure fun, int tag, int value, ...)
 *   mixed limited(closure fun, int * limits [, mixed args...] )
 *
 * Call the function <fun> and execute it with the given runtime limits.
 * After the function exits, the currently active limits are restored.
 * Result of the efun is the result of the closure call.
 *
 * The arguments can be given in two ways: as an array (like the one
 * returned from query_limits(), or as a list of tagged values.
 * If the efun is used without any limit specification, all limits
 * are supposed to be 'unlimited'.
 *
 * The limit settings recognize three special values:
 *     LIMIT_UNLIMITED: the limit is deactivated
 *     LIMIT_KEEP:      the former setting is kept
 *     LIMIT_DEFAULT:   the 'global' default setting is used.
 *
 * The efun causes a privilege violation ("limited", current_object, closure).
 */

{
    svalue_t *argp;
    vector_t *vec;
    struct limits_context_s limits;
    int cl_args;

    if (!num_arg)
        errorf("No arguments given.\n");

    argp = sp - num_arg + 1;
    cl_args = 0;

    /* Get the limits */
    if (num_arg == 1)
    {
        limits.max_eval = 0;
        limits.max_array = 0;
        limits.max_mapping = 0;
        limits.max_map_keys = 0;
        limits.max_callouts = 0;
        limits.max_byte = 0;
        limits.max_file = 0;
        limits.max_memory = 0;
        limits.use_cost = 1; /* smallest we can do */
    }
    else if (argp[1].type == T_POINTER && VEC_SIZE(argp[1].u.vec) < INT_MAX)
    {
        extract_limits(&limits, argp[1].u.vec->item
                      , (int)VEC_SIZE(argp[1].u.vec)
                      , MY_FALSE);
        cl_args = num_arg - 2;
    }
    else if (num_arg % 2 == 1)
    {
        extract_limits(&limits, argp+1, num_arg-1, MY_TRUE);
        cl_args = 0;
    }
    else
    {
        errorf("limited(): Invalid limit specification.\n");
        /* NOTREACHED */
        return sp;
    }

    /* Create an array with the parsed limits to pass
     * to privilege violation and store it in argp[1] so that
     * it can be cleared in case of an error.
     */
    if (num_arg > 1)
        free_svalue(argp+1);
    else
    {
        push_number(sp, 0);
        num_arg++;
    }

    vec = create_limits_array(&limits);
    if (!vec)
    {
        inter_sp = sp;
        errorf("(set_limits) Out of memory: array[%d] for call.\n"
             , LIMIT_MAX);
        /* NOTREACHED */
        return sp;
    }
    put_array(argp+1, vec);

    /* If this object is destructed, no extern calls may be done */
    if (current_object->flags & O_DESTRUCTED
     || !privilege_violation2(STR_LIMITED, argp, argp+1, sp)
        )
    {
        sp = pop_n_elems(num_arg, sp);
        sp++;
        put_number(sp, 0);
    }
    else
    {
        struct limits_context_s context;

        /* Save the current runtime limits and set the new ones */
        save_limits_context(&context);
        context.rt.last = rt_context;
        rt_context = (rt_context_t *)&context.rt;

        max_eval_cost = limits.max_eval ? limits.max_eval + eval_cost : 0;
          /* Make sure that we get the requested amount of ticks, but remember
           * that '0' means 'limitless'
           */
        max_array_size = limits.max_array;
        max_mapping_size = limits.max_mapping;
        max_mapping_keys = limits.max_map_keys;
        max_byte_xfer = limits.max_byte;
        max_file_xfer = limits.max_file;
        max_callouts = limits.max_callouts;
        max_memory = limits.max_memory;
        used_memory_at_eval_start = xalloc_used();
        use_eval_cost = limits.use_cost;

        assign_eval_cost();
        inter_sp = sp;
        call_lambda(argp, cl_args);
        sp = inter_sp;

        /* Overwrite the closure with the result */
        free_svalue(argp); /* The closure might have self-destructed */
        *argp = *sp;
        sp--;

        /* Free the remaining arguments from the efun call */
        sp = pop_n_elems(num_arg - cl_args - 1, sp);

        /* Restore the old limits */
        max_eval_cost = limits.max_eval;
          /* the +eval_cost above was good for proper execution,
           * but might mislead the eval_cost evaluation in the
           * restore().
           */
        rt_context = context.rt.last;
        restore_limits_context(&context);
    }

    /* Stack is clean and sp points to the result */
    return sp;
} /* v_limited() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_set_limits (svalue_t * sp, int num_arg)

/* EFUN set_limits()
 *
 *   void set_limits(int tag, int value, ...)
 *   void set_limits(int * limits)
 *
 * Set the default runtime limits from the given arguments. The new limits
 * will be in effect for the next execution thread.
 *
 * The arguments can be given in two ways: as an array (like the one
 * returned from query_limits(), or as a list of tagged values.
 * The limit settings recognize three special values:
 *     LIMIT_UNLIMITED: the limit is deactivated
 *     LIMIT_DEFAULT:   the global setting is used.
 *     LIMIT_KEEP:      the former setting is kept
 *
 * The efun causes a privilege violation ("set_limits", current_object, first
 * arg).
 */

{
    svalue_t *argp;
    struct limits_context_s limits;
    vector_t *vec;

    if (!num_arg)
        errorf("No arguments given.\n");

    argp = sp - num_arg + 1;

    if (num_arg == 1 && argp->type == T_POINTER && VEC_SIZE(argp->u.vec) < INT_MAX)
        extract_limits(&limits, argp->u.vec->item, (int)VEC_SIZE(argp->u.vec)
                      , MY_FALSE);
    else if (num_arg % 2 == 0)
        extract_limits(&limits, argp, num_arg, MY_TRUE);
    else
    {
        errorf("set_limits(): Invalid limit specification.\n");
        /* NOTREACHED */
        return sp;
    }

    /* On the stack, create an array with the parsed limits to pass
     * to privilege violation.
     */
    sp = pop_n_elems(num_arg, sp); /* sp == argp now */
    vec = create_limits_array(&limits);
    if (!vec)
    {
        inter_sp = sp;
        errorf("(set_limits) Out of memory: array[%d] for call.\n"
             , LIMIT_MAX);
        /* NOTREACHED */
        return sp;
    }
    push_array(sp, vec);
    num_arg = 1;

    if (privilege_violation(STR_SET_LIMITS, argp, sp))
    {
        /* Now store the parsed limits into the variables */
        def_eval_cost = limits.max_eval;
        def_array_size = limits.max_array;
        def_mapping_size = limits.max_mapping;
        def_mapping_keys = limits.max_map_keys;
        def_byte_xfer = limits.max_byte;
        def_file_xfer = limits.max_file;
        def_callouts = limits.max_callouts;
        def_memory = limits.max_memory;
    }

    sp = pop_n_elems(num_arg, sp);
    return sp;
} /* v_set_limits() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_query_limits (svalue_t * sp)

/* EFUN query_limits()
 *
 *   int * query_limits(int defaults)
 *
 * Return an array with the current runtime limits, resp. if defaults
 * is true, the default runtime limits. The entries in the returned
 * array are:
 *
 *   int[LIMIT_EVAL]:    the max number of eval costs
 *   int[LIMIT_ARRAY]:   the max number of array entries
 *   int[LIMIT_MAPPING_SIZE]: the max number of mapping values
 *   int[LIMIT_MAPPING_KEYS]: the max number of mapping entries
 *      (LIMIT_MAPPING is an alias for LIMIT_MAPPING_KEYS)
 *   int[LIMIT_BYTE]:    the max number of bytes for one read/write_bytes()
 *   int[LIMIT_FILE]:    the max number of bytes for one read/write_file()
 *   int[LIMIT_COST]:    how to account for the evaluation cost
 *
 * A limit of '0' means 'no limit', except for LIMIT_COST.
 */

{
    vector_t *vec;
    Bool def;

    def = sp->u.number != 0;

    vec = allocate_uninit_array(LIMIT_MAX);
    if (!vec)
    {
        errorf("(query_limits) Out of memory: array[%d] for result.\n"
             , LIMIT_MAX);
        /* NOTREACHED */
        return sp;
    }

    put_number(vec->item+LIMIT_EVAL,     def ? def_eval_cost : max_eval_cost);
    put_number(vec->item+LIMIT_ARRAY,    def ? def_array_size : max_array_size);
    put_number(vec->item+LIMIT_MAPPING_KEYS
              , def ? def_mapping_keys : max_mapping_keys);
    put_number(vec->item+LIMIT_MAPPING_SIZE
              , def ? def_mapping_size : max_mapping_size);
    put_number(vec->item+LIMIT_BYTE,     def ? def_byte_xfer : max_byte_xfer);
    put_number(vec->item+LIMIT_FILE,     def ? def_file_xfer : max_file_xfer);
    put_number(vec->item+LIMIT_CALLOUTS, def ? def_callouts : max_callouts);
    put_number(vec->item+LIMIT_COST,     def ? DEF_USE_EVAL_COST : use_eval_cost);
    put_number(vec->item+LIMIT_MEMORY,   def ? def_memory : max_memory);

    /* No free_svalue: sp is a number */
    put_array(sp, vec);
    return sp;
} /* f_query_limits() */

/***************************************************************************/

