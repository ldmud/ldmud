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
 *   - a few file efuns.
 *
 * The data structures, especially the runtime stack, are described where
 * they are defined.
 *---------------------------------------------------------------------------
 */

#include "driver.h"
#include "typedefs.h"

#include "my-alloca.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <setjmp.h>
#include <stdio.h>
#include <sys/stat.h>
#include <stdarg.h>

#if defined(AMIGA) && !defined(__GNUC__)
#    include "hosts/amiga/nsignal.h"
#else
#    include <signal.h>
#endif

#if defined(HAVE_DIRENT_H) || defined(_POSIX_VERSION)
#    include <dirent.h>
#    define generic_dirent dirent
#    define DIRENT_NLENGTH(dirent) (strlen((dirent)->d_name))
#else /* not (DIRENT or _POSIX_VERSION) */
#    define generic_dirent direct
#    define DIRENT_NLENGTH(dirent) ((dirent)->d_namlen)
#    ifdef HAVE_SYS_NDIR_H
#        include <sys/ndir.h>
#    endif /* SYSNDIR */
#    ifdef HAVE_SYS_DIR_H
#        include <sys/dir.h>
#    endif /* SYSDIR */
#    ifdef HAVE_NDIR_H
#        include <ndir.h>
#    endif /* NDIR */
#endif /* not (HAVE_DIRENT_H or _POSIX_VERSION) */

#if defined(CYGWIN)
extern int lstat(const char *, struct stat *);
#endif

#ifndef S_ISDIR
#    define S_ISDIR(m) (((m)&S_IFMT) == S_IFDIR)
#endif

#ifndef S_ISREG
#    define S_ISREG(m) (((m)&S_IFMT) == S_IFREG)
#endif


#ifdef SunOS4
#    if !defined (__GNUC__) || __GNUC__ < 2 || __GNUC__ == 2 && __GNUC_MINOR__ < 7
extern int lstat PROT((CONST char *, struct stat *));
#    endif
extern int fchmod PROT((int, int));
#endif

#if defined(OS2) || defined(__EMX__)
#    define lstat stat
#endif

/*-------------------------------------------------------------------------*/

#include "simulate.h"

#include "actions.h"
#include "array.h"
#include "backend.h"
#include "call_out.h"
#include "closure.h"
#include "comm.h"
#include "dumpstat.h"
#include "ed.h"
#include "exec.h"
#include "filestat.h"
#include "gcollect.h"
#include "heartbeat.h"
#include "interpret.h"
#include "instrs.h"
#include "lex.h"
#include "main.h"
#include "mapping.h"
#include "object.h"
#include "otable.h"
#include "prolang.h"
#include "rxcache.h"
#include "sent.h"
#include "simul_efun.h"
#include "stdstrings.h"
#include "stralloc.h"
#include "strfuns.h"
#include "swap.h"
#include "svalue.h"
#include "wiz_list.h"
#include "xalloc.h"

#include "../mudlib/sys/debug_info.h"
#include "../mudlib/sys/driver_hook.h"
#include "../mudlib/sys/files.h"
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
    size_t max_mapping;  /* max mapping size */
    int32  max_eval;     /* max eval cost */
    int32  max_byte;     /* max byte xfer */
    int32  max_file;     /* max file xfer */
    int32  max_callouts; /* max callouts */
    int32  eval_cost;    /* the then-current eval costs used */
};


/* --- struct give_uid_error_context ---
 *
 * A structure of this type is pushed as error handler on the
 * interpreter stack while a newly created object is given its uids.
 */

struct give_uid_error_context
{
    svalue_t  head;        /* A T_ERROR_HANDLER with this struct as arg */
    object_t *new_object;  /* The object under processing */
};


/* --- struct namechain ---
 *
 * This structure is used by load_object() to build the current inheritence tree
 * in the frames on the stack. The information is used to generate
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
 = (rt_context_t *)&toplevel_context;

/*-------------------------------------------------------------------------*/

static p_int alloc_shadow_sent = 0;
  /* Statistic: how many shadow sentences have been allocated.
   */

static sentence_t * free_sent = NULL;
  /* List of allocated but unused shadow sentences.
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
   * the function remove_destructed_objs() called from the backend.
#ifdef MALLOC_smalloc
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

object_t *master_ob = NULL;
  /* The master object.
   */

object_t *current_object;
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

#ifdef USE_FREE_CLOSURE_HOOK
static svalue_t *old_hooks = NULL;
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
#endif

Bool game_is_being_shut_down = MY_FALSE;
  /* TRUE if a shutdown was requested resp. is in progress.
   */

Bool master_will_be_updated = MY_FALSE;
  /* TRUE if a master-update was requested.
   */

int num_error = 0;
  /* Number of recursive calls to error().
   */

static char emsg_buf[2000];
  /* The buffer for the error message to be created.
   */

char     *current_error;
char     *current_error_file;
char     *current_error_object_name;
mp_int    current_error_line_number;
  /* When an error occured during secure_apply(), these four
   * variables receive allocated copies of the error message,
   * the name of the active program and object, and the
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

int32 def_eval_cost = MAX_COST;
int32 max_eval_cost = MAX_COST;
  /* The max eval cost available for one execution thread. Stored as negative
   * value for easier initialisation (see eval_cost).
   * CLEAR_EVAL_COST uses this value to re-initialize (assigned_)eval_cost.
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

/*-------------------------------------------------------------------------*/
/* Forward declarations */

static void free_shadow_sent (shadow_t *p);

/*-------------------------------------------------------------------------*/
Bool
catch_instruction (bytecode_t catch_inst, uint offset
                  , volatile svalue_t ** volatile i_sp
                  , bytecode_p i_pc, svalue_t * i_fp)

/* Implement the F_CATCH/F_CATCH_NO_LOG instruction.
 *
 * At the time of call, all important locals from eval_instruction() are
 * have been stored in their global locations.
 *
 * Result is TRUE on a normal exit (error or not), and FALSE if the
 * guarded code terminated with a 'return' itself;
 *
 * Hard experience showed that it is advantageous to have the setjmp()
 * have its own stackframe, and call the longjmp() from a deeper
 * frame. Additionally it prevents over-optimistic optimizers from
 * removing vital reloads of possibly clobbered local variables after
 * the setjmp().
 */

{
#define INTER_SP ((svalue_t *)(*i_sp))

    Bool rc;

    bytecode_p new_pc;  /* Address of first instruction after the catch() */

    /* Compute address of next instruction after the CATCH statement.
     */
    new_pc = i_pc + offset;

    /* Increase the eval_cost for the duration of the catch so that
     * there is enough time left to handle an eval-too-big error.
     */
    if (max_eval_cost && eval_cost + CATCH_RESERVED_COST >= max_eval_cost)
    {
        error("Not enough eval time left for catch(): required %ld, available %ld\n"
             , (long)CATCH_RESERVED_COST, (long)(max_eval_cost - eval_cost)
             );
        /* NOTREACHED */
        return MY_TRUE;
    }

    eval_cost += CATCH_RESERVED_COST;
    assigned_eval_cost += CATCH_RESERVED_COST;

    /* 'Fake' a subroutine call from <new_pc>
     */
    push_control_stack(INTER_SP, new_pc, i_fp);
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
    if ( setjmp( push_error_context(INTER_SP, catch_inst)->text ) )
    {
        /* A throw() or error occured. We have to restore the
         * control and error stack manually here.
         *
         * The error value to return will be stored in
         * the global <catch_value>.
         */
        svalue_t *sp;

        /* Remove the catch context and get the old stackpointer setting */
        sp = pull_error_context(INTER_SP);

        /* beware of errors after set_this_object() */
        current_object = csp->ob;

        /* catch() faked a subroutine call internally, which has to be
         * undone again. This will also set the pc to the proper
         * continuation address.
         */
        pop_control_stack();

        /* Push the catch return value */
        *(++sp) = catch_value;
        catch_value.type = T_INVALID;

        *i_sp = (volatile svalue_t *)sp;

        /* Restore the old eval costs */
        eval_cost -= CATCH_RESERVED_COST;
        assigned_eval_cost -= CATCH_RESERVED_COST;

        /* If we are out of memory, throw a new error */
        if (out_of_memory)
        {
            error("(catch) Out of memory detected.\n");
        }

        rc = MY_TRUE;
    }
    else
    {

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
            push_number(0);
        }

        /* Restore the old eval costs */
        eval_cost -= CATCH_RESERVED_COST;
        assigned_eval_cost -= CATCH_RESERVED_COST;
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
    context->max_eval = max_eval_cost;
    context->eval_cost = eval_cost;
    context->max_byte = max_byte_xfer;
    context->max_file = max_file_xfer;
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
    if (!max_eval_cost || max_eval_cost > context->max_eval)
    {
        assigned_eval_cost = eval_cost = context->eval_cost+10;
    }
    max_array_size = context->max_array;
    max_mapping_size = context->max_mapping;
    max_callouts = context->max_callouts;
    max_eval_cost = context->max_eval;
    max_byte_xfer = context->max_byte;
    max_file_xfer = context->max_file;
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
#if (defined(__GNUC__) || !defined(AMIGA)) && !defined(__BEOS__)
    /* we want a core dump, and abort() seems to fail for linux and sun */
    (void)signal(SIGFPE, SIG_DFL);
    {
        int a = 0;  /* avoids a pesky diagnostic */
        *((char*)0) = 0/a;
        *((char*)fatal) = 0/a;
    }
#endif
    abort();
} /* dump_core() */

/*-------------------------------------------------------------------------*/
void
fatal (char *fmt, ...)

/* A fatal error occured. Generate a message from printf-style <fmt>, including
 * a timestamp, dump the backtrace and abort.
 */

{
    va_list va;
    char *ts;
    static Bool in_fatal = MY_FALSE;

    /* Prevent double fatal. */
    if (in_fatal)
        dump_core();
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
                            ? current_object->name : "<null>");
    debug_message("%s ", ts);
    vdebug_message(fmt, va);
    if (current_object)
        debug_message("%s Current object was %s\n"
                     , ts, current_object->name
                           ? current_object->name : "<null>");
    debug_message("%s Dump of the call chain:\n", ts);
    (void)dump_trace(MY_TRUE, NULL);

    printf("%s LDMud aborting on fatal error.\n", time_stamp());
    fflush(stdout);

#if defined(__GNUC__) || !defined(AMIGA) || !defined(__SASC)
    sleep(1); /* let stdout settle down... abort can ignore the buffer... */
#else
    Delay(50);        /* Call Dos.library to wait... */
#endif

    va_end(va);

    /* Before shutting down, try to inform the game about it */
    push_volatile_string("Fatal Error");
    callback_master(STR_SHUTDOWN, 1);

    /* Dump core and exit */
    dump_core();
} /* fatal() */

/*-------------------------------------------------------------------------*/
char *
limit_error_format (char *fixed_fmt, size_t fixed_fmt_len, char *fmt)

/* Safety function for error messages: in the error message <fmt>
 * every '%s' spec is changed to '%.200s' to avoid buffer overflows.
 * The modified format string is stored in <fixed_fmt> (a caller provided
 * buffer of size <fixed_fmd_len>) which is also returned as result.
 */

{
    char *ffptr;

    ffptr = fixed_fmt;
    while (*fmt && ffptr - fixed_fmt < fixed_fmt_len-1)
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
error (char *fmt, ...)

/* A system runtime error occured: generate a message from printf-style
 * <fmt> with a timestamp, and handle it.
 * If the error is caught, just dump the trace on stderr, and jump to the
 * error handler, otherwise call the mudlib's error functions (this may cause
 * recursive calls to error()) and jump back to wherever the current error
 * recovery context points to.
 *
 * The runtime context stack is unrolled as far as necessary.
 * TODO: Add a perrorf(<prefmt>, <postfmt>,...) function which translates the
 * TODO:: errno into a string and calls error(<prefmt><errmsg><postfmt>, ...).
 */

{
    rt_context_t *rt;
    char     *object_name;
    char     *ts;
    svalue_t *svp;
    Bool      do_save_error;
    char     *file;                  /* program name */
    char     *malloced_error;        /* copy of emsg_buf+1 */
    char     *malloced_file = NULL;  /* copy of program name */
    char     *malloced_name = NULL;  /* copy of the object name */
    object_t *curobj = NULL;         /* Verified current object */
    char      fixed_fmt[10000];
      /* Note: When changing this buffer, also change the HEAP_STACK_GAP
       * limit in xalloc.c!
       */
    mp_int    line_number = 0;
    va_list   va;

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

    if (rt->type >= ERROR_RECOVERY_CATCH)
    {
        /* User catches this error */

        put_malloced_string(&catch_value, string_copy(emsg_buf));
          /* always reallocate */

        if (rt->type != ERROR_RECOVERY_CATCH_NOLOG)
        {
            /* Even though caught, dump the backtrace - it makes mudlib
             * debugging much easier.
             */
            debug_message("%s Caught error: %s", ts, emsg_buf + 1);
            printf("%s Caught error: %s", ts, emsg_buf + 1);
            if (current_error_trace)
                free_array(current_error_trace);
            dump_trace(MY_FALSE, &current_error_trace);
            debug_message("%s ... execution continues.\n", ts);
            printf("%s ... execution continues.\n", ts);
        }

        unroll_context_stack();
        longjmp(((struct error_recovery_info *)rt_context)->con.text, 1);
        fatal("Catch() longjump failed");
    }

    /* Error not caught by the program */

    num_error++;
    if (num_error > 3)
        fatal("Too many simultaneous errors.\n");

    debug_message("%s ", ts);
    debug_message("%s", emsg_buf+1);

    do_save_error = MY_FALSE;

    /* Get a copy of the error message */
    if ( NULL != (malloced_error = xalloc(strlen(emsg_buf))) )
    {
        strcpy(malloced_error, emsg_buf+1);
    }

    /* If we have a current_object, determine the program location
     * of the fault.
     */
    if (curobj)
    {
        line_number = get_line_number_if_any(&file);
        debug_message("%s program: %s, object: %s line %ld\n"
                     , ts, file, curobj->name, line_number);
        if (current_prog && num_error < 3)
        {
            do_save_error = MY_TRUE;
        }

        if ( NULL != (malloced_file = xalloc(strlen(file) + 1)) )
        {
            strcpy(malloced_file, file);
        }

        if ( NULL != (malloced_name = xalloc(strlen(curobj->name) + 1)) )
        {
            strcpy(malloced_name, curobj->name);
        }
    }

    /* On a triple error, duplicate the error messages so far on stdout */

    if (num_error == 3)
    {
        /* Error context is secure_apply() */

        printf("%s error in function call: %s", ts, emsg_buf+1);
        if (curobj)
        {
            printf("%s program: %s, object: %s line %ld\n"
                  , ts, file, curobj->name, line_number
                  );
        }
    }

    /* Dump the backtrace */
    if (uncaught_error_trace)
        free_array(uncaught_error_trace);
    if (current_error_trace)
        free_array(current_error_trace);

    object_name = dump_trace(num_error == 3, &current_error_trace);
    uncaught_error_trace = ref_array(current_error_trace);
    fflush(stdout);

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
                xfree(malloced_error);
            if (malloced_file)
                xfree(malloced_file);
            if (malloced_name)
                xfree(malloced_name);
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

    /* Error is not caught at all.
     *
     * The stack must be brought in a usable state. After the
     * call to reset_machine(), all arguments to error() are invalid,
     * and may not be used any more. The reason is that some strings
     * may have been on the stack machine stack, and have been deallocated.
     */

    reset_machine(MY_FALSE);

    if (do_save_error)
    {
        save_error(emsg_buf, file, line_number);
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
                            object_name);
            debug_message("%s error when executing program in destroyed object %s\n"
                         , ts, object_name);
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


        CLEAR_EVAL_COST;
        RESET_LIMITS;
        push_volatile_string(malloced_error);
        a = 1;
        if (curobj)
        {
            push_volatile_string(malloced_file);
            push_volatile_string(malloced_name);
            push_number(line_number);
            a += 3;
        }

        if (current_heart_beat)
        {
            /* Heartbeat error: turn off the heartbeat in the object
             * and also pass it to RUNTIME_ERROR.
             */

            culprit = current_heart_beat;
            current_heart_beat = NULL;
            set_heart_beat(culprit, 0);
            debug_message("%s Heart beat in %s turned off.\n"
                         , time_stamp(), culprit->name);
            push_valid_ob(culprit);
            a++;
        }
        else
        {
            if (!curobj)
            {
                /* Pass dummy values */
                push_number(0);
                push_number(0);
                push_number(0);
                a += 3;
            }
            /* Normal error: push -1 instead of a culprit. */
            push_number(-1);
            a++;
        }

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

            push_valid_ob(culprit);
            push_volatile_string(malloced_error);
            a = 2;
            if (curobj)
            {
                push_volatile_string(malloced_file);
                push_volatile_string(malloced_name);
                push_number(line_number);
                a += 3;
            }

            svp = apply_master(STR_HEART_ERROR, a);
            command_giver = save_cmd;
            if (svp && (svp->type != T_NUMBER || svp->u.number) )
            {
                debug_message("%s Heart beat in %s turned back on.\n"
                             , time_stamp(), culprit->name);
                set_heart_beat(culprit, 1);
            }
        }

        /* Handling errors is expensive! */
        assigned_eval_cost = eval_cost += MASTER_RESERVED_COST;
    }

    /* Clean up */
    if (malloced_error)
        xfree(malloced_error);
    if (malloced_file)
        xfree(malloced_file);
    if (malloced_name)
        xfree(malloced_name);

    num_error--;

    if (current_interactive)
    {
        interactive_t *i;

        if (O_SET_INTERACTIVE(i, current_interactive)
         && i->noecho & NOECHO_STALE)
        {
            set_noecho(i, 0);
        }
    }

    /* Unroll the context stack and find the recovery context to jump to. */
    unroll_context_stack();
    if (rt_context->type != ERROR_RECOVERY_NONE)
        longjmp(((struct error_recovery_info *)rt_context)->con.text, 1);

    fatal("Can't recover from error (longjmp failed)\n");
} /* error() */

/*-------------------------------------------------------------------------*/
void
warnf (char *fmt, ...)

/* A system runtime warning occured: generate a message from printf-style
 * <fmt> with a timestamp, and print it using debug_message().
 *
 * Note: Both 'warn' and 'warning' are already taken on some systems.
 * TODO: Extend this to let the mudlib handle warnings.
 * TODO: Add a pwarnf(<prefmt>, <postfmt>,...) function which translates the
 * TODO:: errno into a string and calls error(<prefmt><errmsg><postfmt>, ...).
 */

{
    char     *ts;
    char     *file;                  /* program name */
    object_t *curobj = NULL;         /* Verified current object */
    char      msg_buf[2000];
      /* The buffer for the error message to be created.
       */
    char      fixed_fmt[10000];
      /* Note: When changing this buffer, also change the HEAP_STACK_GAP
       * limit in xalloc.c!
       */
    mp_int    line_number = 0;
    va_list   va;

    ts = time_stamp();

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

    /* Generate the warning message */
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
        debug_message("%s program: %s, object: %s line %ld\n"
                     , ts, file, curobj->name, line_number);
    }

    fflush(stdout);
} /* warnf() */

/*-------------------------------------------------------------------------*/
void
parse_error (Bool warning, char *error_file, int line, char *what
            , char *context)

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
        push_volatile_string(error_file);
        push_volatile_string(buff);
        push_number(warning ? 1 : 0);
        apply_master(STR_LOG_ERROR, 3);
    }
} /* parse_error() */

/*-------------------------------------------------------------------------*/
void
throw_error()

/* The second part of the efun throw(): the caller stored the message
 * into catch_value, now our job is to do the proper longjmp.
 */

{
    unroll_context_stack();
    if (rt_context->type >= ERROR_RECOVERY_CATCH)
    {
        longjmp(((struct error_recovery_info *)rt_context)->con.text, 1);
        fatal("Throw_error failed!");
    }
    free_svalue(&catch_value);
    catch_value.type = T_INVALID;
    error("Throw with no catch.\n");
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
give_uid_error_handler (svalue_t *arg)

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
        error("Out of memory (%lu bytes) for new object '%s' uids\n"
             , (unsigned long) sizeof(*ecp), ob->name);
    }
    ecp->head.type = T_ERROR_HANDLER;
    ecp->head.u.error_handler = give_uid_error_handler;
    ecp->new_object = ob;
    inter_sp++;
    inter_sp->type = T_LVALUE;
    inter_sp->u.lvalue = &ecp->head;
} /* push_give_uid_error_context() */

/*-------------------------------------------------------------------------*/
static Bool
give_uid_to_object (object_t *ob, int hook, int numarg)

/* Object <ob> was just created - call the driver_hook <hook> with <numarg>
 * arguments to give it its uid and euid.
 * Return TRUE on success - on failure, destruct <ob>ject and raise
 * an error; return FALSE in the unlikely case that error() does return.
 */

{
    lambda_t *l;
    char *err, errtxt[1000];
    svalue_t arg, *ret;

    ob->user = &default_wizlist_entry;  /* Default uid */

    if ( NULL != (l = driver_hook[hook].u.lambda) )
    {
        if (driver_hook[hook].x.closure_type == CLOSURE_LAMBDA)
            l->ob = ob;
        call_lambda(&driver_hook[hook], numarg);
        ret = inter_sp;
        xfree(ret[-1].u.lvalue); /* free error context */

        if (ret->type == T_STRING)
        {
            ob->user = add_name(ret->u.string);
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
                       : add_name(ret[0].u.string);
            ob->eff_user = ret[1].type != T_STRING
                           ? 0
                           : add_name(ret[1].u.string);
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
                          , ob->name);
            err = errtxt;
        }
    }
    else
    {
        do pop_stack(); while (--numarg); /* deallocate arguments */
        xfree(inter_sp->u.lvalue);
        err = "Closure to set uid not initialized!\n";
    }

    inter_sp--;  /* skip error context */

    if (master_ob == NULL)
    {
        /* Only for the master object. */
        ob->user = add_name("NONAME");
        ob->eff_user = NULL;
        return MY_TRUE;
    }

    ob->user = add_name("NONAME");
    ob->eff_user = ob->user;
    put_object(&arg, ob);
    destruct_object(&arg);
    error(err);
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
    for (; '\0' != *from && (to - buf) < sizeof(buf)
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
check_no_parentdirs (char *path)

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
legal_path (char *path)

/* Check that <path> is a legal relative path. This means no spaces
 * and no '/../' are allowed.
 * TODO: This should go into a 'files' module.
 */
{
    if (path == NULL || strchr(path, ' ') || path[0] == '/')
        return MY_FALSE;

#ifdef MSDOS_FS
    {
        char *name;

        if (strchr(path,'\\'))
            return MY_FALSE; /* better save than sorry ... */
        if (strchr(path,':'))
            return MY_FALSE; /* \B: is okay for DOS .. *sigh* */
        name = strrchr(path,'/');
        if (NULL != name)
            name++;
        else
            name = path;
        if (!strcasecmp(name,"NUL")
         || !strcasecmp(name,"CON")
         || !strcasecmp(name,"PRN")
         || !strcasecmp(name,"AUX")
         || !strcasecmp(name,"COM1")
         || !strcasecmp(name,"COM2")
         || !strcasecmp(name,"COM3")
         || !strcasecmp(name,"COM4")
         || !strcasecmp(name,"LPT1")
         || !strcasecmp(name,"LPT2")
         || !strcasecmp(name,"LPT3")
         || !strcasecmp(name,"LPT4")
           )
            return MY_FALSE;
    }
#endif

    return check_no_parentdirs(path);
} /* legal_path() */

/*-------------------------------------------------------------------------*/
static void load_object_error(const char *msg, const char *name, namechain_t *chain) NORETURN;

static void
load_object_error(const char *msg, const char *name, namechain_t *chain)

/* Generate a compilation error message <msg>. If <name> is not NULL,
 * ": '<name>'" is appended to the message. If <chain> is not NULL,
 * " (inherited by <chain...>)" is appended to the message.
 * The message is then printed to stderr and an error() with it is thrown.
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
    buf = alloca(strlen(sbuf.buf)+1);
    if (!buf)
        error("Out of stack memory (%lu bytes)\n"
             , (unsigned long) strlen(sbuf.buf)+1);
    strcpy(buf, sbuf.buf);
    strbuf_free(&sbuf);

    fprintf(stderr, "%s %s", time_stamp(), buf);
    error("%.*s", (int)strlen(buf), buf);
} /* load_object_error() */

/*-------------------------------------------------------------------------*/
#define MAX_LOAD_DEPTH 60 /* Make this a configurable constant */

static object_t *
load_object (const char *lname, Bool create_super, int depth, namechain_t *chain)

/* Load (compile) an object blueprint from the file <lname>. <create_super>
 * is true if the object has to be initialized with CREATE_SUPER, and false
 * if CREATE_OB is to be used. <depth> is the current recursive load depth
 * and is checked against MAX_LOAD_DEPTH.
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
 * <chain> is the internal list of inherits.
 *
 * Result is the pointer to the loaded object, or NULL on failure.
 */

{
    int         fd;
    object_t   *ob;
    object_t   *save_command_giver = command_giver;
    int         i;
    struct stat c_st;
    int         name_length;
    char       *name; /* Copy of <lname> */
    char       *fname; /* Filename for <name> */
    program_t  *prog;
    namechain_t nlink;

#ifdef DEBUG
    if ('/' == lname[0])
        fatal("Improper filename '%s' passed to load_object()\n", lname);
#endif

    /* It could be that the passed filename is one of an already loaded
     * object. In that case, simply return that object.
     */
    ob = lookup_object_hash((char *)lname);
    if (ob)
    {
        return ob;
    }

    /* We need two copies of <lname>: one to construct the filename in,
     * the second because lname might be a buffer which is deleted
     * during the compilation process.
     */
    name_length = strlen(lname);
    name = alloca(name_length+2);
    fname = alloca(name_length+4);
    if (!name || !fname)
        fatal("Stack overflow in load_object()");
    if (!compat_mode)
        *name++ = '/';  /* Add and hide a leading '/' */
    strcpy(name, lname);
    strcpy(fname, lname);

    nlink.name = name;
    nlink.prev = chain;

    if (strict_euids && current_object && current_object->eff_user == 0
     && current_object->name)
        error("Can't load objects when no effective user.\n");

    if (master_ob && master_ob->flags & O_DESTRUCTED)
    {
        /* The master has been destructed, and it has not been noticed yet.
         * Reload it, because it can't be done inside of yyparse.
         * assert_master_ob_loaded() will clear master_ob while reloading is
         * in progress, thus preventing a fatal recursion.
         */
        assert_master_ob_loaded();
        /* has the object been loaded by assert_master_ob_loaded ? */
        if ( NULL != (ob = find_object(name)) )
        {
            if (ob->flags & O_SWAPPED && load_ob_from_swap(ob) < 0)
                /* The master has swapped this object and used up most
                 * memory... strange, but thinkable
                 */
                error("Out of memory: unswap object '%s'\n", ob->name);
            return ob;
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
                }
                break;
            }
        }
    }

    /* Check that the c-file exists.
     */
    (void)strcpy(fname+name_length, ".c");
    if (ixstat(fname, &c_st) == -1)
    {
        /* The file does not exist - maybe it's a virtual object */

        svalue_t *svp;

        push_volatile_string(fname);
        svp = apply_master(STR_COMP_OBJ, 1);
        if (svp && svp->type == T_OBJECT)
        {
            /* We got an object from the call, but is it what it
             * claims to be?
             */
            if ( NULL != (ob = lookup_object_hash(name)) )
            {
                /* An object for <name> magically appeared - is it
                 * the one we received?
                 */
                if (ob == svp->u.ob)
                    return ob;
            }
            else if (ob != master_ob)
            {
                /* Rename the object we got to the name it
                 * is supposed to have.
                 */
                ob = svp->u.ob;
                remove_object_hash(ob);
                xfree(ob->name);
                ob->name = string_copy(name);
                enter_object_hash(ob);
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
        ob = lookup_object_hash((char *)name);
        if (ob)
        {
            return ob;
        }

        if (comp_flag)
            fprintf(stderr, "%s compiling %s ...", time_stamp(), fname);

        if (current_file)
        {
            error("Can't load '%s': compiler is busy with '%s'.\n"
                 , name, current_file);
        }

        fd = ixopen(fname, O_RDONLY | O_BINARY);
        if (fd <= 0)
        {
            perror(fname);
            error("Could not read the file.\n");
        }
        FCOUNT_COMP(fname);

        current_file = fname;

        /* The file name is needed before compile_file(), in case there is
         * an initial 'line too long' error.
         */
        compile_file(fd);
        if (comp_flag)
        {
            if (NULL == inherit_file)
                fprintf(stderr, " done\n");
            else
                fprintf(stderr, " needs inherit\n");
        }

        update_compile_av(total_lines);
        total_lines = 0;
        (void)close(fd);
        current_file = NULL;

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

            tmp = make_name_sane(inherit_file, MY_FALSE);
            if (!tmp)
            {
                pInherited = inherit_file;
            }
            else
            {
                pInherited = alloca(strlen(tmp)+1);
                strcpy(pInherited, tmp);
            }

            push_referenced_shared_string(inherit_file);
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
                error("Illegal to inherit self.\n");
            }

            if (depth >= MAX_LOAD_DEPTH)
            {
                load_object_error("Too deep inheritance", name, chain);
            }

            ob = load_object(pInherited, MY_TRUE, depth+1, &nlink);
            free_string(inter_sp->u.string);
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

    if (NULL != (ob = lookup_object_hash(name)))
    {
        /* The object magically appeared!
         * This can happen if rename_object() is used carelessly
         * in the mudlib handler for compiler warnings.
         */
#ifndef INITIALIZATION_BY___INIT
        for (i = compiled_prog->num_variables; --i >= 0; )
            free_svalue(&prog_variable_values[i]);
#endif
        free_prog(compiled_prog, MY_TRUE);
        load_object_error("Object appeared while it was compiled"
                         , name, chain);
        /* NOTREACHED */
        return NULL;
    }

    prog = compiled_prog;

#ifdef INITIALIZATION_BY___INIT
    ob = get_empty_object(prog->num_variables);
#else
    ob = get_empty_object( prog->num_variables, prog->variable_names
                         , prog_variable_values);
    /* TODO: The initializers should be stored in the program.
     * TODO:: See clone_object() for the reason.
     * TODO:: To implement this efficiently, use special 'const' arrays
     * TODO:: and mappings with a copy-on-write strategy: value copies
     * TODO:: of such arrays are made on assignment (to catch m = ([...]);
     * TODO:: m_delete(m, ...)) and lvalue/ref computation.
     */
    for (i = prog->num_variables; --i >= 0; )
        free_svalue(&prog_variable_values[i]);
    xfree(prog_variable_values);
#endif

    if (!ob)
        error("Out of memory for new object '%s'\n", name);

    ob->name = string_copy(name);
#ifdef CHECK_OBJECT_STAT
    if (check_object_stat)
    {
        fprintf(stderr, "DEBUG: OSTAT: (%ld:%ld) load( %p '%s') name: %ld -> (%ld:%ld)\n"
                      , tot_alloc_object, tot_alloc_object_size, ob, ob->name ? ob->name : "<null>"
                      , (long)(strlen(ob->name)+1)
                      , tot_alloc_object, tot_alloc_object_size + (strlen(ob->name)+1)
                      );
    }
#endif
    tot_alloc_object_size += strlen(ob->name)+1;
      /* Tabling this unique string is of not much use.
       * Note that the string must be valid for the ref_object()
       * below to work in debugging mode.
       */

#ifndef NO_BLUEPRINT
    prog->blueprint = ref_object(ob, "load_object: blueprint reference");
#endif /* !NO_BLUEPRINT */

    if (!compat_mode)
        name--;  /* Make the leading '/' visible again */
    ob->load_name = make_shared_string(name);  /* but here it is */
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
    push_string_shared(ob->name);
    if (give_uid_to_object(ob, H_LOAD_UIDS, 1))
    {
        /* The object has an uid - now we can update the .user
         * of its initializers.
         */
        svalue_t *svp;
        int j;
        object_t *save_current;

        save_current = current_object;
        current_object = ob; /* for lambda_ref_replace_program */
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
            reset_object(ob, create_super ? H_CREATE_SUPER : H_CREATE_OB);

            /* If the master inherits anything -Ugh- we have to have
             * some object to attribute initialized variables to.
             */
            current_object = save_current;
        }
        else
        {
            current_object = save_current;
            reset_object(ob, create_super ? H_CREATE_SUPER : H_CREATE_OB);
        }
    }

    if ( !(ob->flags & O_DESTRUCTED))
        ob->flags |= O_WILL_CLEAN_UP;

    /* Restore the command giver */
    command_giver = check_object(save_command_giver);

    if (d_flag > 1 && ob)
        debug_message("%s --%s loaded\n", time_stamp(), ob->name);

#if 0 && defined(CHECK_OBJECT_REF)
    if (strchr(ob->name, '#') == NULL)
        printf("DEBUG: new_object(%p '%s') ref %ld flags %x\n"
              , ob, ob->name, (long)ob->ref, ob->flags);
#endif
    return ob;
} /* load_object() */

/*-------------------------------------------------------------------------*/
static char *
make_new_name (char *str)

/* <str> is a basic object name - generate a clone name "<str>#<num>"
 * and return it.
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

    char *p;
    size_t l;
    char buff[40];

    if ('/' == *str)
        str++;

    for (;;)
    {
        /* Generate the clone name */
        (void)sprintf(buff, "#%lu", clone_id_number);
        l = strlen(str);
        p = xalloc(l + strlen(buff) + 1);
        strcpy(p, str);
        strcpy(p+l, buff);

        clone_id_number++;
        if (clone_id_number == 0) /* Wrap around */
            test_conflict = MY_TRUE;

        if (!test_conflict || !find_object(p))
            return p;

        /* The name was already taken */
        xfree(p);
    }
} /* make_new_name() */

/*-------------------------------------------------------------------------*/
object_t *
clone_object (char *str1)

/* Create a clone of the object named <str1>, which may be a clone itself.
 * On success, return the new object, otherwise NULL.
 */

{
    object_t *ob, *new_ob;
    object_t *save_command_giver = command_giver;
    char *name;

    if (strict_euids && current_object && current_object->eff_user == NULL)
        error("Illegal to call clone_object() with effective user 0\n");

    ob = get_object(str1);

    /* If the object self-destructed...
     */
    if (ob == NULL)
        return NULL;

    /* If ob is a clone, try finding the blueprint via the load_name */
    if (ob->flags & O_CLONE)
    {
        object_t *bp;

        bp = get_object(ob->load_name);
        if (bp)
            ob = bp;
    }

    if (ob->super)
        error("Cloning a bad object: '%s' is contained in '%s'.\n"
             , ob->name, ob->super->name);

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

        name_length = strlen(name);
        i = name_length;
        p = ob->name+name_length;
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
        error("Out of memory: unswap object '%s'\n", ob->name);

    if (ob->prog->flags & P_NO_CLONE)
        error("Cloning a bad object: '%s' sets '#pragma no_clone'.\n"
             , ob->name);

    ob->time_of_ref = current_time;

    /* We do not want the heart beat to be running for unused copied objects */

    if (!(ob->flags & O_CLONE) && ob->flags & O_HEART_BEAT)
        set_heart_beat(ob, 0);

    /* Got the blueprint - now get a new object */

    new_ob = get_empty_object(ob->prog->num_variables
#ifndef INITIALIZATION_BY___INIT
                             , ob->prog->variable_names
                             , ob->variables
#endif
    );
    /* TODO: Yeech: the new objects variables are initialised from the
     * TODO:: template object variables. These values should be stored
     * TODO:: in the program.
     */
    if (!new_ob)
        error("Out of memory for new clone '%s'\n", name);

    new_ob->name = make_new_name(name);
#ifdef CHECK_OBJECT_STAT
    if (check_object_stat)
    {
        fprintf(stderr, "DEBUG: OSTAT: (%ld:%ld) clone( %p '%s') name: %ld -> (%ld:%ld)\n"
                      , tot_alloc_object, tot_alloc_object_size, new_ob, new_ob->name ? new_ob->name : "<null>"
                      , (long)(strlen(new_ob->name)+1)
                      , tot_alloc_object, tot_alloc_object_size + (strlen(new_ob->name)+1)
                      );
    }
#endif
    tot_alloc_object_size += strlen(new_ob->name)+1;
    new_ob->load_name = ref_string(ob->load_name);
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
    push_object(ob);
    push_volatile_string(new_ob->name);
    give_uid_to_object(new_ob, H_CLONE_UIDS, 2);
    reset_object(new_ob, H_CREATE_CLONE);
    command_giver = check_object(save_command_giver);

    /* Never know what can happen ! :-( */
    if (new_ob->flags & O_DESTRUCTED)
        return NULL;

    return new_ob;
} /* clone_object() */

/*-------------------------------------------------------------------------*/
object_t *
lookfor_object(char * str, Bool bLoad)

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

    /* TODO: It would be more useful to check all callers of lookfor()
     * TODO:: and move the make_name_sane() into those where it can
     * TODO:: be dirty.
     */
    pName = make_name_sane(str, MY_FALSE);
    if (!pName)
        pName = str;

    ob = lookup_object_hash((char *)pName);
    if (!bLoad)
        return ob;

    if (!ob)
        ob = load_object(pName, 0, 0, NULL);
    if (!ob || ob->flags & O_DESTRUCTED)
        return NULL;
    return ob;
} /* lookfor_object() */

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
        ob = find_object(v->u.string);
        if (ob == 0)
            error("destruct_object: Could not find %s\n", v->u.string);
    }

    if (ob->flags & O_DESTRUCTED)
        return;

    if (ob->flags & O_SWAPPED)
        if (load_ob_from_swap(ob) < 0)
            error("Out of memory: unswap object '%s'\n", ob->name);

    if (d_flag)
    {
        debug_message("%s destruct_object: %s (ref %ld)\n"
                     , time_stamp(), ob->name, ob->ref);
    }

    push_object(ob);
    result = apply_master(STR_PREP_DEST, 1);
    if (!result)
        error("No prepare_destruct\n");

    if (result->type == T_STRING)
        error(result->u.string);

    if (result->type != T_NUMBER || result->u.number != 0)
        return;

    if (ob->contains)
    {
        error("Master failed to clean inventory in prepare_destruct\n");
    }

    if (ob->flags & O_SHADOW)
    {
        shadow_t *sh;
        object_t *save = command_giver;

        command_giver = ob;
        sh = O_GET_SHADOW(ob);
        if (sh->ip)
            trace_level |= sh->ip->trace_level;
        if (sh->ed_buffer)
            save_ed_buffer();
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
#ifndef NO_BLUEPRINT
        int i;

        for (i = 0; i < prog->num_inherited; ++i)
        {
            program_t *iprog = prog->inherit[i].prog;

            if (iprog != NULL && iprog->blueprint != NULL)
            {
                destruct(iprog->blueprint);
            }
        }
#endif /* !NO_BLUEPRINT */
    }
} /* deep_destruct() */

/*-------------------------------------------------------------------------*/
void
destruct (object_t *ob)

/* Destruct an object <ob>. This function is called from
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

        if (shadow_sent->ed_buffer)
        {
            object_t *save = command_giver;

            command_giver = ob;
            free_ed_buffer();
            command_giver = save;
        }

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

    set_heart_beat(ob, 0);

    /* Remove us out of this current room (if any).
     * Remove all sentences defined by this object from all objects here.
     */
    if (ob->super)
    {
        if (ob->super->sent)
            remove_action_sent(ob, ob->super);

#       ifdef F_SET_LIGHT
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

    num_listed_objs--;

    ob->super = NULL;
    ob->next_inv = NULL;
    ob->contains = NULL;
    ob->flags &= ~O_ENABLE_COMMANDS;
    ob->flags |= O_DESTRUCTED;  /* should come last! */
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
             , ob, ob->name, sh, sh->obj, sh->obj->name);
    if ((sh->flags & O_DESTRUCTED) != (ob->flags & O_DESTRUCTED)
     || sh->sent != ob->sent
       )
        fatal("DEBUG: Obj %p '%s': ref %ld, flags %x, sent %p; shadow ref %ld, flags %x, sent %p\n"
             , ob, ob->name, ob->ref, ob->flags, ob->sent
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
        fatal("DEBUG: Obj %p '%s': ref %ld, flags %x, sent %p; no shadow found\n"
             , obj, obj->name, obj->ref, obj->flags, obj->sent
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
        debug_message("%s remove_object: object %s (ref %ld)\n"
                     , time_stamp(), ob->name, ob->ref);
    }

    if (O_IS_INTERACTIVE(ob))
        remove_interactive(ob, MY_FALSE);

    /* If this is a blueprint object, NULL out the pointer in the program
     * to remove the extraneous reference.
     */
#ifndef NO_BLUEPRINT
    if (ob->prog->blueprint == ob)
    {
        ob->prog->blueprint = NULL;
        remove_prog_swap(ob->prog, MY_TRUE);
        free_object(ob, "remove_object: blueprint reference");
    }
#endif /* !NO_BLUEPRINT */

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
            fatal("Non-destructed object %p '%s' in list of newly destructed objects.\n"
                 , ob, ob->name ? ob->name : "<null>"
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
remove_destructed_objects (void)

/* Scan the list of destructed objects and free those with no references
 * remaining.
 */

{
    object_t *ob;
#ifdef CHECK_OBJECT_REF
    object_shadow_t *sh = destructed_obj_shadows;
    object_shadow_t *prev = NULL;
#endif /* CHECK_OBJECT_REF */

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

    if (free_sent == NULL)
    {
        xallocate(p, sizeof *p, "new shadow sentence");
        alloc_shadow_sent++;
    }
    else
    {
        p = (shadow_t *)free_sent;
        free_sent = free_sent->next;
    }
    p->sent.type = SENT_SHADOW;
    p->shadowing = NULL;
    p->shadowed_by = NULL;
    p->ed_buffer = NULL;
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

    p->sent.next = free_sent;
    free_sent = (sentence_t *)p;
} /* free_shadow_sent() */

/*-------------------------------------------------------------------------*/
void
purge_shadow_sent(void)

/* Actually deallocate all shadow sentences held in the free list.
 * Called during a GC and shutdown.
 */

{
    sentence_t *p;

    for (;free_sent; free_sent = p) {
        p = free_sent->next;
        xfree(free_sent);
        alloc_shadow_sent--;
    }
} /* purge_shadow_sent() */

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
         && !sh->ed_buffer
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
            strbuf_addf(sbuf, "Actions:\t\t\t%8ld %9ld\n"
                            , alloc_action_sent
                            , alloc_action_sent * sizeof (action_t));
            strbuf_addf(sbuf, "Shadows:\t\t\t%8ld %9ld\n"
                            , alloc_shadow_sent
                            , alloc_shadow_sent * sizeof (shadow_t));
            strbuf_addf(sbuf, "Objects:\t\t\t%8ld %9ld (%ld swapped, %ld Kbytes)\n"
                            , tot_alloc_object, tot_alloc_object_size
                            , num_vb_swapped, total_vb_bytes_swapped / 1024);
            strbuf_addf(sbuf, "Arrays:\t\t\t\t%8ld %9ld\n"
                            , (long)num_arrays, total_array_size() );
            strbuf_addf(sbuf, "Mappings:\t\t\t%8ld %9ld\n"
                             , num_mappings, total_mapping_size() );
            strbuf_addf(sbuf, "Prog blocks:\t\t\t%8ld %9ld (%ld swapped, %ld Kbytes)\n"
                            , total_num_prog_blocks + num_swapped - num_unswapped
                            , total_prog_block_size + total_bytes_swapped
                                                    - total_bytes_unswapped
                            , num_swapped - num_unswapped
                            , (total_bytes_swapped - total_bytes_unswapped) / 1024);
            strbuf_addf(sbuf, "Memory reserved:\t\t\t %9d\n", res);
        }
        if (verbose) {
#ifdef COMM_STAT
            strbuf_addf(sbuf
                       , "Calls to add_message: %d   Packets: %d   "
                         "Average packet size: %.2f\n\n"
                       , add_message_calls
                       , inet_packets
                       , inet_packets ? (float)inet_volume/(float)inet_packets : 0.0
            );
#endif
#ifdef APPLY_CACHE_STAT
            strbuf_addf(sbuf
                       , "Calls to apply_low: %ld    "
                         "Cache hits: %ld (%.2f%%)\n\n"
                       , (long)(apply_cache_hit+apply_cache_miss)
                       , (long)apply_cache_hit
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
            long count;
            object_t *ob;
#endif

            strbuf_add(sbuf, "\nObject status:\n");
            strbuf_add(sbuf, "--------------\n");
            strbuf_addf(sbuf, "Objects total:\t\t\t %8ld\n"
                             , (long)tot_alloc_object);
#ifndef DEBUG
            strbuf_addf(sbuf, "Objects in list:\t\t %8ld\n"
                             , (long)num_listed_objs);
            strbuf_addf(sbuf, "Objects newly destructed:\t\t %8ld\n"
                             , (long)num_newly_destructed);
            strbuf_addf(sbuf, "Objects destructed:\t\t %8ld\n"
                             , (long)num_destructed);
#else
            for (count = 0, ob = obj_list; ob != NULL; ob = ob->next_all)
                count++;
            if (count != num_listed_objs)
            {
                debug_message("DEBUG: num_listed_objs mismatch: listed %ld, counted %ld\n"
                             , (long)num_listed_objs, count);
                strbuf_addf(sbuf, "Objects in list:\t\t %8ld (counted %ld)\n"
                                 , (long)num_listed_objs, count);
            }
            else
                strbuf_addf(sbuf, "Objects in list:\t\t %8ld\n"
                                 , (long)num_listed_objs);
            for (count = 0, ob = newly_destructed_objs; ob != NULL; ob = ob->next_all)
                count++;
            if (count != num_newly_destructed)
            {
                debug_message("DEBUG: num_newly_destructed mismatch: listed %ld, counted %ld\n"
                             , (long)num_newly_destructed, count);
                strbuf_addf(sbuf, "Objects newly destructed:\t\t %8ld (counted %ld)\n"
                                 , (long)num_newly_destructed, count);
            }
            else
                strbuf_addf(sbuf, "Objects newly destructed:\t %8ld\n"
                                 , (long)num_newly_destructed);
            for (count = 0, ob = destructed_objs; ob != NULL; ob = ob->next_all)
                count++;
            if (count != num_destructed)
            {
                debug_message("DEBUG: num_destructed mismatch: listed %ld, counted %ld\n"
                             , (long)num_destructed, count);
                strbuf_addf(sbuf, "Objects destructed:\t\t %8ld (counted %ld)\n"
                                 , (long)num_destructed, count);
            }
            else
                strbuf_addf(sbuf, "Objects destructed:\t\t %8ld\n"
                                 , (long)num_destructed);
#endif

            strbuf_addf(sbuf, "Objects processed in last cycle: "
                               "%8ld (%5.1f%% - avg. %5.1f%%)\n"
                       , (long)num_last_processed
                       , (float)num_last_processed / (float)num_listed_objs * 100.0
                       , !avg_in_list
                         ? 0.0
                         : ((avg_in_list || avg_last_processed > avg_in_list)
                            ? 100.0
                            : 100.0 * (float)avg_last_processed / avg_in_list
                           )
                       );
        }
        tot += show_otable_status(sbuf, verbose);
        tot += heart_beat_status(sbuf, verbose);
        tot += add_string_status(sbuf, verbose);
        tot += call_out_status(sbuf, verbose);
        tot += total_mapping_size();
#ifdef RXCACHE_TABLE
        tot += rxcache_status(sbuf, verbose);
#endif
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
            strbuf_addf(sbuf, "Other structures\t\t\t %9lu\n", other);
            tot += other;
        }
        tot += res;

        if (!verbose) {
            strbuf_add(sbuf, "\t\t\t\t\t ---------\n");
            strbuf_addf(sbuf, "Total:\t\t\t\t\t %9d\n", tot);
        }
        return MY_TRUE;
    }

    if (strcmp(buff, "swap") == 0)
    {
        swap_status(sbuf);
        return MY_TRUE;
    }

    if (strcmp(buff, "malloc") == 0) {
#if defined(MALLOC_smalloc)
        dump_malloc_data(sbuf);
#endif
#ifdef MALLOC_sysmalloc
        strbuf_add(sbuf, "Using system standard malloc.\n");
#endif
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
    ST_DOUBLE(DID_ST_OBJECTS_AVG_PROC
             , !avg_in_list
               ? 0.0
               : ((avg_in_list || avg_last_processed > avg_in_list)
                  ? 1.0
                  : (double)avg_last_processed / avg_in_list
                 )
             );

    ST_NUMBER(DID_ST_ARRAYS,         num_arrays);
    ST_NUMBER(DID_ST_ARRAYS_SIZE,    total_array_size());

    ST_NUMBER(DID_ST_MAPPINGS,       num_mappings);
    ST_NUMBER(DID_ST_MAPPINGS_SIZE,  total_mapping_size());

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
#else
    ST_NUMBER(DID_ST_ADD_MESSAGE, -1);
    ST_NUMBER(DID_ST_PACKETS,     -1);
    ST_NUMBER(DID_ST_PACKET_SIZE, -1);
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
char *
check_valid_path (char *path, object_t *caller, char* call_fun, Bool writeflg)

/* Object <caller> will read resp. write (<writeflg>) the file <path>
 * for the efun <call_fun>.
 *
 * Check the validity of the operation by calling master:valid_read() resp.
 * valid_write().
 *
 * If the operation is valid, the path to use is returned (always without
 * leading '/', the path "/" will be returned as "."). This path is either
 * a pointer into the <path> argument, or a pointer to a static buffer in
 * apply().
 *
 * If the operation is invalid, NULL is returned.
 */

{
    svalue_t *v;
    wiz_list_t *eff_user;

    if (path)
        push_string_malloced(path);
    else
        push_number(0);

    if ( NULL != (eff_user = caller->eff_user) )
        push_shared_string(eff_user->name);
    else
        push_number(0);

    push_volatile_string(call_fun);
    push_valid_ob(caller);
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
    }
    else
    {
        path = v->u.string;
    }

    if (path[0] == '/')
        path++;

    /* The string "/" will be converted to "." */
    if (path[0] == '\0')
        path = ".";

    if (legal_path(path))
        return path;

    error("Illegal path %s for %s() by %s\n", path, call_fun, caller->name);
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
            free_string(cb->function.named.name);
        cb->function.named.ob = NULL;
        cb->function.named.name = NULL;
    }

    free_callback_args(cb);
} /* free_callback() */

/*-------------------------------------------------------------------------*/
static INLINE int
setup_callback_args (callback_t *cb, int nargs, svalue_t * args
                    , Bool allow_prot_lvalues)

/* Setup the function arguments in the callback <cb> to hold the <nargs>
 * arguments starting from <args>. If <allow_prot_lvalues> is FALSE, no
 * argument may be a protected lvalue. The arguments are transferred into the
 * callback structure.
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
            if (!allow_prot_lvalues && args->type == T_LVALUE
             && (   args->u.lvalue->type == T_PROTECTED_CHAR_LVALUE
                 || args->u.lvalue->type == T_PROTECTED_STRING_RANGE_LVALUE
                 || args->u.lvalue->type == T_PROTECTED_POINTER_RANGE_LVALUE
                 || args->u.lvalue->type == T_PROTECTED_LVALUE
                )
               )
            {
                /* We don't handle protected lvalues - abort the process.
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
setup_function_callback ( callback_t *cb, object_t * ob, char * fun
                        , int nargs, svalue_t * args, Bool allow_prot_lvalues)

/* Setup the empty/uninitialized callback <cb> to hold a function
 * call to <ob>:<fun> with the <nargs> arguments starting from <args>.
 * If <allow_prot_lvalues> is FALSE, no argument may be a protected lvalue.
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
    cb->function.named.name = make_shared_string(fun);
    cb->function.named.ob = ref_object(ob, "callback");

    error_index = setup_callback_args(cb, nargs, args, allow_prot_lvalues);
    if (error_index >= 0)
    {
        free_object(cb->function.named.ob, "callback");
        free_string(cb->function.named.name);
        cb->function.named.ob = NULL;
        cb->function.named.name = NULL;
    }

    return error_index;
} /* setup_function_callback() */

/*-------------------------------------------------------------------------*/
int
setup_closure_callback ( callback_t *cb, svalue_t *cl
                       , int nargs, svalue_t * args, Bool allow_prot_lvalues)

/* Setup the empty/uninitialized callback <cb> to hold a closure
 * call to <cl> with the <nargs> arguments starting from <args>.
 * If <allow_prot_lvalues> is FALSE, no argument may be a protected lvalue.
 *
 * Both <cl> and the arguments are adopted (taken away from the caller).
 *
 * Result is -1 on success, or, when encountering an illegal argument,
 * the index of the faulty argument (but even then all caller arguments
 * have been transferred or freed).
 */

{
    int error_index;

    cb->is_lambda = MY_TRUE;
    transfer_svalue_no_free(&(cb->function.lambda), cl);

    error_index = setup_callback_args(cb, nargs, args, allow_prot_lvalues);
    if (error_index >= 0)
    {
        free_svalue(&(cb->function.lambda));
        cb->function.lambda.type = T_INVALID;
    }

    return error_index;
} /* setup_closure_callback() */

/*-------------------------------------------------------------------------*/
int
setup_efun_callback ( callback_t *cb, svalue_t *args, int nargs)

/* Setup the empty/uninitialized callback <cb> with the <nargs>
 * values starting at <args>. This function is used to implement the
 * callbacks for efuns like map_array() and accepts these forms:
 *
 *   (string fun)
 *   (string fun, mixed extra, ...) TODO: This form is UGLY!
 *   (string fun, string|object obj, mixed extra, ...)
 *   (closure cl, mixed extra, ...)
 *
 * If the first argument is a string and the second neither an object
 * nor a string, this_object() is used as object specification.
 *
 * All arguments are adopted (taken away from the caller). Protected lvalues
 * like &(i[0]) are not allowed as 'extra' arguments.
 *
 * Result is -1 on success, or, when encountering an illegal argument,
 * the index of the faulty argument (but even then all caller arguments
 * have been transferred or freed).
 */

{
    int error_index;

    if (args[0].type == T_CLOSURE)
    {
        error_index = setup_closure_callback(cb, args, nargs-1, args+1, MY_FALSE);
        if (error_index >= 0)
            error_index++;
    }
    else if (args[0].type == T_STRING)
    {
        object_t *ob;
        int       first_arg;

        first_arg = 1;

        if (nargs > 1)
        {
            if (args[1].type == T_OBJECT)
            {
                ob = args[1].u.ob;
                first_arg = 2;
            }
            else if (args[1].type == T_STRING)
            {
                ob = get_object(args[1].u.string);
                first_arg = 2;
            }
            else
            {
                /* TODO: It would be better to throw an error here */
                ob = current_object;
                first_arg = 1;
            }
        }
        else
            ob = current_object;

        if (ob != NULL)
        {
            error_index = setup_function_callback(cb, ob, args[0].u.string
                                                 , nargs-first_arg, args+first_arg
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
        error_index = 0;

    return error_index;
} /* setup_efun_callback() */

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
    }
    else
    {
        if (!apply(cb->function.named.name, ob, num_arg + nargs))
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

#ifdef USE_FREE_CLOSURE_HOOK
/*-------------------------------------------------------------------------*/
void
free_closure_hooks (svalue_t *svp, int count)

/* "Free" the <count> closures in <svp>[], ie. store them for later
 * deletion by the backend.
 *
 * This is used for closures which are held by the gamedriver, ie.
 * have only one reference like the hooks or the prompt, and may be
 * freed while they are executed.
 */

{
    svalue_t *new;

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
} /* free_closure_hooks() */

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
} /* free_old_driver_hooks() */

#endif /* USE_FREE_CLOSURE_HOOK */

/*-------------------------------------------------------------------------*/
#ifdef F_SET_LIGHT

void
add_light (object_t *p, int n)

/* The light emission of <p> and all surrounding objects is
 * changed by <n>.
 */

{
    if (n == 0)
        return;
    do {
        p->total_light += n;
    } while ( NULL != (p = p->super) );
} /* add_light() */
#endif

/*-------------------------------------------------------------------------*/
Bool
match_string (char * match, char * str, mp_int len)

/* Test if the string <str> of length <len> matches the pattern <match>.
 * Allowed wildcards are
 *   *: matches any sequence
 *   ?: matches any single character
 *   \: escapes the following wildcard
 *
 * The function is used by the compiler for inheritance specs, and by
 * e_get_dir().
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
                if ( !(str2 = memmem(match, matchlen, str, len + matchlen)) )
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
            tell_npc(command_giver, arg->u.string);
        }
        else
        {
            add_message("%s", arg->u.string);
        }
    }
    else if (arg->type == T_OBJECT)
        add_message("OBJ(%s)", arg->u.ob->name);
    else if (arg->type == T_NUMBER)
        add_message("%ld", arg->u.number);
    else if (arg->type == T_FLOAT)
    {
        char buff[120];

        sprintf(buff, "%g", READ_DOUBLE( arg ) );
        add_message(buff);
    }
    else if (arg->type == T_POINTER)
        add_message("<ARRAY>");
    else if (arg->type == T_MAPPING)
        add_message("<MAPPING>");
    else if (arg->type == T_CLOSURE)
        add_message("<CLOSURE>");
    else
        add_message("<OTHER:%d>", arg->type);
} /* print_svalue() */

/*=========================================================================*/

/*                              EFUNS                                      */

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
            error("Out of memory: unswap object '%s'\n", ob->name);

    victim = ob->prog;

    if (victim->flags & P_NO_SHADOW)
        error("shadow '%s' on '%s': Can't shadow a 'no_shadow' program.\n"
             , cob->name, ob->name);

    if (cob->flags & O_SHADOW)
    {
        shadow_t *shadow_sent = O_GET_SHADOW(cob);

        if (shadow_sent->shadowing)
            error("shadow '%s' on '%s': Already shadowing.\n"
                 , cob->name, ob->name);
        if (shadow_sent->shadowed_by)
            error("shadow '%s' on '%s': Can't shadow when shadowed.\n"
                 , cob->name, ob->name);
    }

    if (cob->super)
        error("shadow '%s' on '%s': The shadow resides inside another object ('%s').\n"
             , cob->name, ob->name, cob->super->name);

    if (ob->flags & O_SHADOW && O_GET_SHADOW(ob)->shadowing)
        error("shadow '%s' on '%s': Can't shadow a shadow.\n"
             , cob->name, ob->name);

    if (ob == cob)
        error("shadow '%s' on '%s': Can't shadow self.\n"
             , cob->name, ob->name);

    /* Make sure that we don't shadow 'nomask' functions.
     */
    for (i = shadow->num_function_names; --i >= 0; )
    {
        funflag_t flags;
        char *name;
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
            error("shadow '%s' on '%s: Illegal to shadow 'nomask' function \"%s\".\n"
                 , ob->name, cob->name, name);
        }
    }

    push_object(ob);
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

/* TEFUN shadow()
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
    if (sp[-1].type != T_OBJECT)
        bad_xefun_arg(1, sp);
    if (sp->type != T_NUMBER)
        bad_xefun_arg(2, sp);

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

/* TEFUN query_shadowing()
 *
 *   object query_shadowing (object obj)
 *
 * The function returns the object which <obj> is currently
 * shadowing, or 0 if <obj> is not a shadow.
 */

{

    object_t *ob;

    if (sp->type != T_OBJECT)
        bad_xefun_arg(1, sp);

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

/* TEFUN unshadow()
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

/* TEFUN set_driver_hook()
 *
 *   void set_driver_hook(int what, closure arg)
 * 	void set_driver_hook(int what, string arg)
 * 	void set_driver_hook(int what, string * arg)
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
    if (sp[-1].type != T_NUMBER
     || (n = sp[-1].u.number) < 0 || n > NUM_DRIVER_HOOKS)
    {
        bad_xefun_arg(1, sp);
    }

    /* Legal call? */
    if (!_privilege_violation("set_driver_hook", sp-1, sp))
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
        if (sp->u.number != 0)
            goto bad_arg_2;
        put_number(driver_hook + n, 0);
        break;

    case T_STRING:
      {
        char *str;

        if ( !((1 << T_STRING) & hook_type_map[n]) )
            goto bad_arg_2;

        if ( NULL != (str = make_shared_string(sp->u.string)) )
        {
            free_svalue(sp);
            put_string(driver_hook + n, str);
            if (n == H_NOECHO)
                mudlib_telopts();
        }
        else
        {
            error("Out of memory (%lu bytes) for driver hook\n"
                 , (unsigned long) strlen(sp->u.string));
        }
        break;
      }

    case T_MAPPING:
        if (!sp->u.map->num_values
         ||  sp->u.map->ref != 1 /* add_to_mapping() could zero num_values */)
        {
            goto bad_arg_2;
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
            driver_hook[n].u.lambda->ob = master_ob;
            if (n == H_NOECHO)
            {
                mudlib_telopts();
            }
            break;
        }
        else if (!CLOSURE_IS_LFUN(sp->x.closure_type))
        {
            goto bad_arg_2;
        }
        /* FALLTHROUGH */

    default:
default_test:
        if ( !((1 << sp->type) & hook_type_map[n]) )
        {
bad_arg_2:
            bad_xefun_arg(2, sp);
            break; /* flow control hint */
        }

        driver_hook[n] = *sp;
        if (n == H_NOECHO)
        {
            mudlib_telopts();
        }
        break;
    }

#ifdef USE_FREE_CLOSURE_HOOK
    if (old.type != T_NUMBER)
        free_closure_hooks(&old, 1); /* free it in the backend */
#else
    /* The object reference in bound closures is not counted! */
    if (old.type == T_CLOSURE &&
        old.x.closure_type == CLOSURE_LAMBDA)
    {
        old.x.closure_type = CLOSURE_UNBOUND_LAMBDA;
    }
    free_svalue(&old);
#endif

    return sp - 2;
} /* f_set_driver_hook() */

/*-------------------------------------------------------------------------*/
#ifdef F_SET_AUTO_INCLUDE_STRING
svalue_t *
f_set_auto_include_string (svalue_t *sp)

/* EFUN set_auto_include_string()
 *
 *    void set_auto_include(string arg)
 *
 * If <arg> is a string, it will be automatically included into every
 * compiled LPC object.
 *
 * This is useful to enforce global definitions, e.g.
 * ``#pragma combine_strings'' or ``#pragma strict_types''.  The
 * calling object needs to be privileged by the master object.
 *
 * Note that this efun is just a deprecated frontend for
 * set_driver_hook(H_AUTO_INCLUDE).
 */

{
    if (sp->type != T_STRING)
        bad_xefun_arg(1, sp);

    if (_privilege_violation("set_auto_include_string", sp, sp) > 0)
    {
        char *str;
        svalue_t old;

        old = driver_hook[H_AUTO_INCLUDE]; /* Remember this for freeing */

        if ( NULL != (str = make_shared_string(sp->u.string)) )
        {
            put_string(driver_hook + H_AUTO_INCLUDE, str);
        }
        else
        {
            error("Out of memory (%lu bytes) for driver hook\n"
                 , (unsigned long) strlen(sp->u.string));
        }

#ifdef USE_FREE_CLOSURE_HOOK
        if (old.type != T_NUMBER)
            free_closure_hooks(&old, 1); /* free it in the backend */
#else
        /* The object reference in bound closures is not counted! */
        if (old.type == T_CLOSURE &&
            old.x.closure_type == CLOSURE_LAMBDA)
        {
            old.x.closure_type = CLOSURE_UNBOUND_LAMBDA;
        }
        free_svalue(&old);
#endif

    }

    return sp - 1;
} /* set_auto_include() */

#endif /* F_SET_AUTO_INCLUDE_STRING */

/*-------------------------------------------------------------------------*/
svalue_t *
f_rename_object (svalue_t *sp)

/* TEFUN rename_object()
 *
 *   void rename_object (object ob, string new_name);
 *
 * Give the object <ob> a new object name <new_name>. Causes a privilege
 * violation. The new name must not contain a # character, except
 * at the end, to avoid confusion with clone numbers.
 *
 * Raises a privilege violation ("rename_object", this_object(), ob, name).
 */

{
    object_t *ob;
    char *name;
    mp_int length;

    inter_sp = sp; /* this is needed for assert_master_ob_loaded(), and for
                    * the possible errors before.
                    */
    if (sp[-1].type != T_OBJECT)
        bad_xefun_arg(1, sp);
    if (sp[0].type != T_STRING)
        bad_xefun_arg(2, sp);
    ob = sp[-1].u.ob;
    name = sp[0].u.string;

    /* Remove leading '/' if any. */
    while(name[0] == '/')
        name++;

    /* Truncate possible .c in the object name. */
    length = strlen(name);
    if (name[length-2] == '.' && name[length-1] == 'c') {
        /* A new writeable copy of the name is needed. */
        char *p;
        p = (char *)alloca(length+1);
        strcpy(p, name);
        name = p;
        name[length -= 2] = '\0';
    }

    {
        char c;
        char *p;
        mp_int i;

        i = length;
        p = name + length;
        while (--i > 0)
        {
            /* isdigit would need to check isascii first... */
            if ( (c = *--p) < '0' || c > '9' )
            {
                if (c == '#' && length - i > 1) {
                    error("Illegal name to rename_object: '%s'.\n", name);
                }
                break;
            }
        }
    }

    if (lookup_object_hash(name))
    {
        error("Attempt to rename to object '%s'\n", name);
    }

    assert_master_ob_loaded();
    if (master_ob == ob)
        error("Attempt to rename the master object\n");

    if (privilege_violation4("rename_object", ob, name, 0, sp))
    {
        remove_object_hash(ob);
        xfree(ob->name);
        ob->name = string_copy(name);
        enter_object_hash(ob);
    }

    free_svalue(sp--);
    free_svalue(sp--);
    return sp;
} /* f_rename_object() */

/*-------------------------------------------------------------------------*/
void
e_write (svalue_t *arg)

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

    print_svalue(arg);
    command_giver = check_object(save_command_giver);

} /* e_write() */

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
    char * limitnames[] = { "LIMIT_EVAL", "LIMIT_ARRAY", "LIMIT_MAPPING"
                          , "LIMIT_BYTE", "LIMIT_FILE" };

    /* Set the defaults (unchanged) limits */
    result->max_eval = max_eval_cost;
    result->max_array = max_array_size;
    result->max_mapping = max_mapping_size;
    result->max_callouts = max_callouts;
    result->max_byte = max_byte_xfer;
    result->max_file = max_file_xfer;

    if (!tagged)
    {
        p_int val;
        int limit;

        for (limit = 0; limit < LIMIT_MAX && limit < num; limit++)
        {

            if (svp[limit].type != T_NUMBER)
                error("Illegal %s value: not a number\n", limitnames[limit]);
                /* TODO: Give type and value */
            val = svp[limit].u.number;
            if (val >= 0)
            {
                switch(limit)
                {
                case LIMIT_EVAL:     result->max_eval = val;    break;
                case LIMIT_ARRAY:    result->max_array = val;   break;
                case LIMIT_MAPPING:  result->max_mapping = val; break;
                case LIMIT_BYTE:     result->max_byte = val;    break;
                case LIMIT_FILE:     result->max_file = val;    break;
                case LIMIT_CALLOUTS: result->max_callouts = val; break;
                default: error("Unimplemented limit #%d\n", limit);
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
                case LIMIT_MAPPING:  result->max_mapping = def_mapping_size;
                                     break;
                case LIMIT_BYTE:     result->max_byte = def_byte_xfer;
                                     break;
                case LIMIT_FILE:     result->max_file = def_file_xfer;
                                     break;
                case LIMIT_CALLOUTS: result->max_callouts = def_callouts;
                                     break;
                default: error("Unimplemented limit #%d\n", limit);
                }
            }
            else if (val != LIMIT_KEEP)
                error("Illegal %s value: %ld\n", limitnames[limit], val);
        }
    }
    else
    {
        int i;

        for (i = 0; i < num - 1; i += 2)
        {
            p_int val;
            int limit;

            if (svp[i].type != T_NUMBER)
                error("Illegal limit tag: not a number.\n");
                /* TODO: Give type and value */
            limit = (int)svp[i].u.number;
            if (limit < 0 || limit >= LIMIT_MAX)
                error("Illegal limit tag: %ld\n", (long)limit);

            if (svp[i+1].type != T_NUMBER)
                error("Illegal %s value: not a number\n", limitnames[limit]);
                /* TODO: Give type and value */
            val = svp[i+1].u.number;
            if (val >= 0)
            {
                switch(limit)
                {
                case LIMIT_EVAL:     result->max_eval = val;    break;
                case LIMIT_ARRAY:    result->max_array = val;   break;
                case LIMIT_MAPPING:  result->max_mapping = val; break;
                case LIMIT_BYTE:     result->max_byte = val;    break;
                case LIMIT_FILE:     result->max_file = val;    break;
                case LIMIT_CALLOUTS: result->max_callouts = val;    break;
                default: error("Unimplemented limit #%d\n", limit);
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
                case LIMIT_MAPPING:  result->max_mapping = def_mapping_size;
                                     break;
                case LIMIT_BYTE:     result->max_byte = def_byte_xfer;
                                     break;
                case LIMIT_FILE:     result->max_file = def_file_xfer;
                                     break;
                case LIMIT_CALLOUTS: result->max_callouts = def_callouts;
                                     break;
                default: error("Unimplemented limit #%d\n", limit);
                }
            }
            else if (val != LIMIT_KEEP)
                error("Illegal %s value: %ld\n", limitnames[limit], val);
        }
    }
} /* extract_limits() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_limited (svalue_t * sp, int num_arg)

/* VEFUN limited()
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
 * The limit settings recognize two special values:
 *     LIMIT_UNLIMITED: the limit is deactivated
 *     LIMIT_KEEP:      the former setting is kept
 *     LIMIT_DEFAULT:   the 'global' default setting is used.
 *
 * The efun causes a privilege violation ("limited", current_object, closure).
 */

{
    svalue_t *argp;
    struct limits_context_s limits;
    int cl_args;

    if (!num_arg)
        error("No arguments given.\n");

    argp = sp - num_arg + 1;
    cl_args = 0;

    if (argp->type != T_CLOSURE)
        bad_xefun_vararg(1, sp);

    /* Get the limits */
    if (num_arg == 1)
    {
        limits.max_eval = 0;
        limits.max_array = 0;
        limits.max_mapping = 0;
        limits.max_callouts = 0;
        limits.max_byte = 0;
        limits.max_file = 0;
    }
    else if (argp[1].type == T_POINTER)
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
        bad_xefun_vararg(num_arg, sp);

    /* If this object is destructed, no extern calls may be done */
    if (current_object->flags & O_DESTRUCTED
     || !_privilege_violation("limited", argp, sp)
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
        rt_context = (rt_context_t *)&context;

        max_eval_cost = limits.max_eval ? limits.max_eval + eval_cost : 0;
          /* Make sure that we get the requested amount of ticks, but remember
           * that '0' means 'limitless'
           */
        max_array_size = limits.max_array;
        max_mapping_size = limits.max_mapping;
        max_byte_xfer = limits.max_byte;
        max_file_xfer = limits.max_file;
        max_callouts = limits.max_callouts;

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
} /* f_limited() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_set_limits (svalue_t * sp, int num_arg)

/* VEFUN set_limits()
 *
 *   void set_limits(int tag, int value, ...)
 *   void set_limits(int * limits)
 *
 * Set the default runtime limits from the given arguments. The new limits
 * will be in effect for the next execution thread.
 *
 * The arguments can be given in two ways: as an array (like the one
 * returned from query_limits(), or as a list of tagged values.
 * The limit settings recognize two special values:
 *     LIMIT_UNLIMITED: the limit is deactivated
 *     LIMIT_KEEP:      the former setting is kept
 *
 * The efun causes a privilege violation ("set_limits", current_object, first
 * arg).
 */

{
    svalue_t *argp;
    struct limits_context_s limits;

    if (!num_arg)
        error("No arguments given.\n");

    argp = sp - num_arg + 1;

    if (num_arg == 1 && argp->type == T_POINTER)
        extract_limits(&limits, argp->u.vec->item, (int)VEC_SIZE(argp->u.vec)
                      , MY_FALSE);
    else if (num_arg % 2 == 0)
        extract_limits(&limits, argp, num_arg, MY_TRUE);
    else
        bad_xefun_vararg(num_arg, sp);

    if (_privilege_violation("set_limits", argp, sp))
    {
        /* Now store the parsed limits into the variables */
        def_eval_cost = limits.max_eval;
        def_array_size = limits.max_array;
        def_mapping_size = limits.max_mapping;
        def_byte_xfer = limits.max_byte;
        def_file_xfer = limits.max_file;
        def_callouts = limits.max_callouts;
    }

    sp = pop_n_elems(num_arg, sp);
    return sp;
} /* f_set_limits() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_query_limits (svalue_t * sp)

/* TEFUN query_limits()
 *
 *   int * query_limits(int defaults)
 *
 * Return an array with the current runtime limits, resp. if defaults
 * is true, the default runtime limits. The entries in the returned
 * array are:
 *
 *   int[LIMIT_EVAL]:    the max number of eval costs
 *   int[LIMIT_ARRAY]:   the max number of array entries
 *   int[LIMIT_MAPPING]: the max number of mapping entries
 *   int[LIMIT_BYTE]:    the max number of bytes for one read/write_bytes()
 *   int[LIMIT_FILE]:    the max number of bytes for one read/write_file()
 *
 * A limit of '0' means 'no limit'.
 */

{
    vector_t *vec;
    Bool def;

    if (sp->type != T_NUMBER)
        bad_xefun_arg(1, sp);
    def = sp->u.number != 0;

    vec = allocate_uninit_array(LIMIT_MAX);
    if (!vec)
        error("(query_limits) Out of memory: array[%d] for result.\n"
             , LIMIT_MAX);

    put_number(vec->item+LIMIT_EVAL,     def ? def_eval_cost : max_eval_cost);
    put_number(vec->item+LIMIT_ARRAY,    def ? def_array_size : max_array_size);
    put_number(vec->item+LIMIT_MAPPING,  def ? def_mapping_size : max_mapping_size);
    put_number(vec->item+LIMIT_BYTE,     def ? def_byte_xfer : max_byte_xfer);
    put_number(vec->item+LIMIT_FILE,     def ? def_file_xfer : max_file_xfer);
    put_number(vec->item+LIMIT_CALLOUTS, def ? def_callouts : max_callouts);

    /* No free_svalue: sp is a number */
    put_array(sp, vec);
    return sp;
} /* f_query_limits() */

/*=========================================================================*/

/*                          INVENTORY EFUNS                                */

/*-------------------------------------------------------------------------*/
void
move_object (void)

/* Move the object inter_sp[-1] into object inter_sp[0]; both objects
 * are removed from the stack.
 *
 * The actual move performed by the hooks H_MOVE_OBJECT0/1, this
 * function is called to implement the efuns move_object() and transfer().
 */

{
    lambda_t *l;
    object_t *save_command = command_giver;

    if (NULL != ( l = driver_hook[H_MOVE_OBJECT1].u.lambda) ) {
        l->ob = inter_sp[-1].u.ob;
        call_lambda(&driver_hook[H_MOVE_OBJECT1], 2);
    } else if (NULL != ( l = driver_hook[H_MOVE_OBJECT0].u.lambda) ) {
        l->ob = current_object;
        call_lambda(&driver_hook[H_MOVE_OBJECT0], 2);
    }
    else
        error("Don't know how to move objects.\n");
    command_giver = check_object(save_command);
} /* move_object() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_set_environment (svalue_t *sp)

/* TEFUN set_environment()
 *
 *   void set_environment(object item, object env)
 *
 * The item is moved into its new environment env, which may be 0.
 * This efun is to be used in the H_MOVE_OBJECTx hook, as it does
 * nothing else than moving the item - no calls to init() or such.
 */

{
    object_t *item, *dest;
    object_t **pp, *ob;
    object_t *save_cmd = command_giver;

    /* Get and test the arguments */

    if (sp[-1].type != T_OBJECT)
        bad_xefun_arg(1, sp);

    item = sp[-1].u.ob;

    if (item->flags & O_SHADOW && O_GET_SHADOW(item)->shadowing)
        error("Can't move an object that is shadowing.\n");

    if (sp->type != T_OBJECT)
    {
        if (sp->type != T_NUMBER || sp->u.number)
            bad_xefun_arg(2, sp);
        dest = NULL;
    }
    else
    {
        dest = sp->u.ob;
        /* Recursive moves are not allowed. */
        for (ob = dest; ob; ob = ob->super)
            if (ob == item)
                error("Can't move object inside itself.\n");

#       ifdef F_SET_LIGHT
            add_light(dest, item->total_light);
#       endif
        dest->flags &= ~O_RESET_STATE;
    }

    item->flags &= ~O_RESET_STATE; /* touch it */

    if (item->super)
    {
        /* First remove the item out of its current environment */
        Bool okey = MY_FALSE;

        if (item->sent)
        {
            remove_environment_sent(item);
        }

        if (item->super->sent)
            remove_action_sent(item, item->super);

#       ifdef F_SET_LIGHT
            add_light(item->super, - item->total_light);
#       endif

        for (pp = &item->super->contains; *pp;)
        {
            if (*pp != item)
            {
                if ((*pp)->sent)
                    remove_action_sent(item, *pp);
                pp = &(*pp)->next_inv;
                continue;
            }
            *pp = item->next_inv;
            okey = MY_TRUE;
        }

        if (!okey)
            fatal("Failed to find object %s in super list of %s.\n",
                  item->name, item->super->name);
    }

    /* Now put it into its new environment (if any) */
    item->super = dest;
    if (!dest)
    {
        item->next_inv = NULL;
    }
    else
    {
        item->next_inv = dest->contains;
        dest->contains = item;
    }

    command_giver = check_object(save_cmd);
    free_svalue(sp);
    sp--;
    free_svalue(sp);
    return sp - 1;
} /* f_set_environment() */

/*=========================================================================*/

/*                            FILE EFUNS                                   */

/*-------------------------------------------------------------------------*/
#ifdef atarist
/* this code is provided to speed up ls() on an Atari ST/TT . */

#include <support.h>
#include <limits.h>
#include <osbind.h>

extern long _unixtime PROT((unsigned, unsigned));

struct xdirect {
    /* inode and position in directory aren't usable in a portable way,
     * so why support them anyway?
     */
    short d_namlen;
    char  d_name[16];
    int   size;
    int   time;
};

typedef struct
{
    _DTA dta;
    char *dirname;
    long status;
} xdir;
#define XDIR xdir

static long olddta;

/*-------------------------------------------------------------------------*/
static XDIR *xopendir(path)
char *path;
{
    char pattern[MAXPATHLEN+1];
    XDIR *d;
    long status;

    d = (XDIR *)xalloc(sizeof(XDIR));
    _unx2dos(path, pattern);
    strcat(pattern, "\\*.*");
    olddta = Fgetdta();
    Fsetdta(&d->dta);
    d->status = status = Fsfirst(pattern, 0xff);
    if (status && status != -ENOENT) {
        xfree(d);
        return 0;
    }
    d->dirname = string_copy(pattern);
    return d;
}

/*-------------------------------------------------------------------------*/
#define XOPENDIR(dest, path) ((dest) = xopendir(path))

static struct xdirect *xreaddir(d)
XDIR *d;
{
    static struct xdirect xde;

    if (d->status)
        return 0;
    _dos2unx(d->dta.dta_name, xde.d_name);
    xde.d_namlen = strlen(xde.d_name);
    if (FA_DIR & d->dta.dta_attribute)
        xde.size = -2;
    else
        xde.size = d->dta.dta_size;
    xde.time = _unixtime(d->dta.dta_time, d->dta.dta_date);
    d->status = Fsnext();
    return &xde;
}

/*-------------------------------------------------------------------------*/
static void xclosedir(d)
XDIR *d;
{
    Fsetdta(olddta);
    xfree(d->dirname);
    xfree(d);
}

/*-------------------------------------------------------------------------*/
static void xrewinddir(d)
XDIR *d;
{
    long status;

    Fsetdta(&d->dta);
    d->status = status = Fsfirst(d->dirname, 0xff);
}

#endif /* atarist */

/*-------------------------------------------------------------------------*/
#ifndef XDIR

struct xdirect
{
    /* inode and position in directory aren't usable in a portable way,
     * so why support them anyway?
     */
    short d_namlen;
    char  *d_name;
    int   size;
    int   time;
};

#define XOPENDIR(dest, path) (\
    (!chdir(path) &&\
    NULL != ((dest) = opendir("."))) ||\
        (chdir(mud_lib),MY_FALSE)\
)

#define xclosedir(dir_ptr)   (chdir(mud_lib),closedir(dir_ptr))
#define xrewinddir(dir_ptr)  rewinddir(dir_ptr)
#define XDIR DIR

/*-------------------------------------------------------------------------*/
static struct xdirect *
xreaddir (XDIR *dir_ptr, int mask)

/* Read the next entry from <dir_ptr> and return it via a pointer
 * to a static xdirect structure.
 * <mask> is tested for GETDIR_SIZES and GETDIR_DATES - only the data
 * for requested items is returned.
 */

{
    static struct xdirect xde;
    struct generic_dirent *de;
    int namelen;
    struct stat st;

    de = readdir(dir_ptr);
    if (!de)
        return NULL;
    namelen = DIRENT_NLENGTH(de);
    xde.d_namlen = namelen;
    xde.d_name   = de->d_name;
    if (mask & (GETDIR_SIZES|GETDIR_DATES) )
    {
        if (ixstat(xde.d_name, &st) == -1) /* who knows... */
        {
            xde.size = FSIZE_NOFILE;
            xde.time = 0;
        }
        else
        {
            if (S_IFDIR & st.st_mode)
                xde.size = FSIZE_DIR;
            else
                xde.size = st.st_size;
            xde.time = st.st_mtime;
        }
    }
    return &xde;
} /* xreaddir() */

#endif /* XDIR */

/*-------------------------------------------------------------------------*/
static int
pstrcmp (const void *p1, const void *p2)

/* qsort() comparison function: strcmp() on two svalue-strings.
 */

{
    return strcmp(((svalue_t*)p1)->u.string, ((svalue_t*)p2)->u.string);
} /* pstrcmp() */


/*-------------------------------------------------------------------------*/
struct get_dir_error_context
{
    svalue_t head;
    XDIR *dirp;
    vector_t *v;
};

/*-------------------------------------------------------------------------*/
static void
get_dir_error_handler (svalue_t *arg)

/* T_ERROR_HANDLER function: <arg> is a (struct get_dir_error_context*)
 * with the directory which needs to be closed.
 */

{
    struct get_dir_error_context *ecp;

    ecp = (struct get_dir_error_context *)arg;
    xclosedir(ecp->dirp);
    if (ecp->v)
        free_array(ecp->v);
} /* get_dir_error_handler() */

/*-------------------------------------------------------------------------*/
vector_t *
e_get_dir (char *path, int mask)

/* EFUN get_dir()
 *
 *     string *get_dir(string str)
 *     string *get_dir(string str, int mask)
 *
 * This function takes a path as argument and returns an array of file
 * names and attributes in that directory.
 *
 * Returns 0 if the directory to search in does not exist.
 *
 * The filename part of the path may contain '*' or '?' as wildcards:
 * every '*' matches an arbitrary amount of characters (or just itself).
 * Thus get_dir("/path/ *") would return an alphabetically sorted array
 * of all files in directory "/path/", or just ({ "/path/ *" }) if this
 * file happens to exist.
 *
 * To query the content of a directory, use the directory name with a
 * trailing '/' or '/.', for example get_dir("/path/."). Use the
 * directory name as it is to get information about the directory itself.
 *
 * The optional second argument mask can be used to get
 * information about the specified files.
 *
 * GETDIR_EMPTY    (0x00)  get_dir returns an empty array (not very
 *                         useful).
 * GETDIR_NAMES    (0x01)  put the alphabetically sorted file names into
 *                         the returned array.
 * GETDIR_SIZES    (0x02)  put the file sizes unsorted into the returned
 *                         array. directories have size FSIZE_DIR (-2).
 * GETDIR_DATES    (0x04)  put the file modification dates unsorted into
 *                         the returned array.
 * GETDIR_PATH     (0x10)  if this mask bit is set, the filenames with
 *                         the full path will be returned
 *                         (GETDIR_NAMES is implied).
 * GETDIR_UNSORTED (0x20)  if this mask bit is set, the result of will
 *                         _not_ be sorted.
 * GETDIR_ALL      (0x07)  GETDIR_NAMES|GETDIR_SIZES|GETDIR_DATES (see
 *                         examples).
 *
 * Note: You should use GETDIR_NAMES|GETDIR_UNSORTED to get the entries
 * in the same order as with GETDIR_SIZES and GETDIR_DATES.
 *
 * The values of mask can be added together.
 */

{
    static struct get_dir_error_context ec; /* must survive errors */

    vector_t       *v, *w;
    int             i, j, count = 0;
    XDIR           *dirp;
    int             namelen;
    Bool            do_match = MY_FALSE;
    struct xdirect *de;
    struct stat     st;
    char           *temppath;
    size_t          templen;
    Bool            in_top_dir = MY_FALSE;
    char           *p;
    char           *regexpr = 0;
    int             nqueries;

    /* Adjust the mask for implied bits */
    if (mask & GETDIR_PATH)
        mask |= GETDIR_NAMES;

    if (!path)
        return NULL;

    path = check_valid_path(path, current_object, "get_dir", MY_FALSE);

    if (path == NULL)
        return NULL;

    /* We need to modify the returned path, and thus to make a
     * writeable copy.
     * The path "" needs 2 bytes to store ".\0".
     */
    temppath = alloca(strlen(path) + 2);
    if (strlen(path) < 2)
    {
        temppath[0] = path[0] ? path[0] : '.';
        temppath[1] = '\000';
        p = temppath;
        in_top_dir = MY_TRUE;
    }
    else
    {
        strcpy(temppath, path);

        /* If path ends with '/' or "/." remove it
         */
        if ((p = strrchr(temppath, '/')) == NULL)
            p = temppath;

        if ((p[0] == '/' && p[1] == '.' && p[2] == '\0')
         || (p[0] == '/' && p[1] == '\0')
           )
            *p = '\0';

        in_top_dir = (p == temppath);
    }

    /* Number of data items per file */
    nqueries =   ((mask & GETDIR_NAMES) != 0)
               + ((mask & GETDIR_SIZES) != 0)
               + ((mask & GETDIR_DATES) != 0);

    if (strchr(p, '*') || ixstat(temppath, &st) < 0)
    {
        /* We got a wildcard and/or a directory:
         * prepare to match.
         */
        if (*p == '\0')
            return NULL;
        regexpr = alloca(strlen(p)+2);
        if (p != temppath)
        {
            strcpy(regexpr, p + 1);
            *p = '\0';
        }
        else
        {
            strcpy(regexpr, p);
            strcpy(temppath, ".");
            in_top_dir = MY_TRUE;
        }
        do_match = MY_TRUE;
    }
    else if (*p != '\0' && strcmp(temppath, "."))
    {
        /* We matched a single file */

        svalue_t *stmp;

        if (*p == '/' && *(p + 1) != '\0')
            p++;
        v = allocate_array(nqueries);
        stmp = v->item;
        if (mask & GETDIR_NAMES)
        {
            if (mask & GETDIR_PATH)
            {
                if (compat_mode)
                    put_malloced_string(stmp, string_copy(temppath));
                else
                    put_malloced_string(stmp, add_slash(temppath));
            }
            else
            {
                put_malloced_string(stmp, string_copy(p));
            }

            stmp++;
        }
        if (mask & GETDIR_SIZES){
            put_number(stmp, (S_IFDIR & st.st_mode) ? FSIZE_DIR : st.st_size);
            stmp++;
        }
        if (mask & GETDIR_DATES)
        {
            put_number(stmp, st.st_mtime);
            stmp++;
        }
        return v;
    }

    templen = strlen(temppath);

    if ( XOPENDIR(dirp, temppath) == 0)
        return NULL;

    /* Prepare the error handler to do clean up.
     */
    ec.head.type = T_ERROR_HANDLER;
    ec.head.u.error_handler = get_dir_error_handler;
    ec.dirp = dirp;
    ec.v = NULL;
    inter_sp++;
    inter_sp->type = T_LVALUE;
    inter_sp->u.lvalue = &ec.head;

    /* Count files
     */
    for (de = xreaddir(dirp, 1); de; de = xreaddir(dirp, 1))
    {
        namelen = de->d_namlen;
        if (do_match)
        {
            if ( !match_string(regexpr, de->d_name, namelen) )
                continue;
        }
        else
        {
            if (namelen <= 2 && *de->d_name == '.'
             && (namelen == 1 || de->d_name[1] == '.' ) )
                continue;
        }
        count += nqueries;
        if (max_array_size && count >= max_array_size)
            break;
    }

    if (nqueries)
        count /= nqueries;

    /* Make array and put files on it.
     */
    v = allocate_array(count * nqueries);
    if (count == 0)
    {
        /* This is the easy case :-) */
        inter_sp--;
        xclosedir(dirp);
        return v;
    }

    ec.v = v;
    xrewinddir(dirp);
    w = v;
    j = 0;

    /* Taken into account that files might be added/deleted from outside. */
    for(i = 0, de = xreaddir(dirp,mask); de; de = xreaddir(dirp,mask))
    {

        namelen = de->d_namlen;
        if (do_match)
        {
            if ( !match_string(regexpr, de->d_name, namelen) )
                continue;
        }
        else
        {
            if (namelen <= 2 && *de->d_name == '.'
             && (namelen == 1 || de->d_name[1] == '.' ) )
                continue;
        }
        if (i >= count)
        {
            /* New file. Don't need efficience here, but consistence. */

            vector_t *tmp, *new;

            count++;
            tmp = allocate_array(nqueries);
            new = add_array(v, tmp);
            free_array(v);
            free_array(tmp);
            ec.v = v = new;
            w = v;
        }

        if (mask & GETDIR_NAMES)
        {
            char *result;
            char *name;

            if ((mask & GETDIR_PATH) && !in_top_dir)
            {
                if (compat_mode)
                {
                    xallocate(result, (size_t)namelen+templen+2, "getdir() names");
                    name = result;
                }
                else
                {
                    xallocate(result, (size_t)namelen+templen+3, "getdir() names");
                    result[0] = '/';
                    name = result+1;
                }
                memcpy(name, temppath, templen);
                name[templen] = '/';
                name += templen+1;
            }
            else
            {
                xallocate(result, (size_t)namelen+1, "getdir() names");
                name = result;
            }

            if (namelen)
                memcpy(name, de->d_name, namelen);
            name[namelen] = '\0';
            put_malloced_string(w->item+j, result);
            j++;
        }
        if (mask & GETDIR_SIZES)
        {
            put_number(w->item + j, de->size);
            j++;
        }
        if (mask & GETDIR_DATES)
        {
            put_number(w->item + j, de->time);
            j++;
        }
        i++;
    }
    xclosedir(dirp);
    inter_sp--;

    if ( !((mask ^ 1) & (GETDIR_NAMES|GETDIR_UNSORTED)) )
    {
        /* Sort by names. */
        qsort(v->item, i, sizeof v->item[0] * nqueries, pstrcmp);
    }

    return v;
} /* e_get_dir() */

/*-------------------------------------------------------------------------*/
Bool
e_tail (char *path)

/* EFUN tail()
 *
 *   void tail(string file)
 *
 * Print out the tail of a file. There is no specific amount of
 * lines given to the output. Only a maximum of 1000 bytes will
 * be printed.
 *
 * Return TRUE on success.
 */
{
    char buff[1000];
    FILE *f;
    struct stat st;
    int offset;

    path = check_valid_path(path, current_object, "tail", MY_FALSE);

    if (path == NULL)
        return MY_FALSE;
    f = fopen(path, "r");
    if (f == NULL)
        return MY_FALSE;
    FCOUNT_READ(path);
    if (fstat(fileno(f), &st) == -1)
        fatal("Could not stat an open file.\n");
    if ( !S_ISREG(st.st_mode) ) {
        fclose(f);
        return MY_FALSE;
    }
    offset = st.st_size - 54 * 20;
    if (offset < 0)
        offset = 0;
    if (fseek(f, offset, 0) == -1)
        fatal("Could not seek.\n");

    /* Throw away the first incomplete line. */
    if (offset > 0)
        (void)fgets(buff, sizeof buff, f);

    while(fgets(buff, sizeof buff, f))
    {
        add_message("%s", buff);
    }
    fclose(f);
    return MY_TRUE;
} /* e_tail() */

/*-------------------------------------------------------------------------*/
int
e_print_file (char *path, int start, int len)

/* EFUN cat()
 *
 *     int cat(string pathi [, int start [, int num]])
 *
 * List the file found at path.
 * The optional arguments start and num are start line
 * number and number of lines. If they are not given the
 * file is printed from the beginning.
 *
 * Result is the number of lines printed, but never more than 50.
 */

{
#   define MAX_LINES 50

    char buff[1000];
    FILE *f;
    int i;

    if (len < 0)
        return 0;

    path = check_valid_path(path, current_object, "print_file", MY_FALSE);

    if (path == 0)
        return 0;
    if (start < 0)
        return 0;
    f = fopen(path, "r");
    if (f == NULL)
        return 0;
    FCOUNT_READ(path);

    if (len == 0)
        len = MAX_LINES;
    if (len > MAX_LINES)
        len = MAX_LINES;

    if (start == 0)
        start = 1;

    for (i = 1; i < start + len; i++)
    {
        if (fgets(buff, sizeof buff, f) == 0)
            break;
        if (i >= start)
            add_message("%s", buff);
    }
    fclose(f);

    if (i <= start)
        return 0;

    if (i == MAX_LINES + start)
        add_message("*****TRUNCATED****\n");

    return i-start;

#   undef MAX_LINES
} /* e_print_file() */

/*-------------------------------------------------------------------------*/
Bool
e_remove_file (char *path)

/* EFUN rm()
 *
 *   int rm(string file)
 *
 * Remove the file. Returns 0 for failure and 1 for success.
 */
{
    path = check_valid_path(path, current_object, "remove_file", MY_TRUE);

    if (path == 0)
        return MY_FALSE;
    if (unlink(path) == -1)
        return MY_FALSE;
    FCOUNT_DEL(path);
    return MY_TRUE;
} /* e_remove_file() */

/*-------------------------------------------------------------------------*/
static Bool
isdir (char *path)

/* Helper function for copy and move: test if <path> is a directory.
 */

{
    struct stat stats;

    return ixstat (path, &stats) == 0 && S_ISDIR (stats.st_mode);
} /* isdir() */

/*-------------------------------------------------------------------------*/
static void
strip_trailing_slashes (char *path)

/* Strip trailing slashed from <path>, which is modified in-place.
 */

{
    int last;

    last = strlen (path) - 1;
    while (last > 0 && path[last] == '/')
        path[last--] = '\0';
} /* strip_trailing_slashes() */

/*-------------------------------------------------------------------------*/
static int
copy_file (char *from, char *to, int mode)

/* Copy the file <from> to <to> with access <mode>.
 * Return 0 on success, 1 or errno on failure.
 */

{
    int ifd;
    int ofd;
    char buf[1024 * 8];
    int len;                        /* Number of bytes read into `buf'. */

    if (unlink(to) && errno != ENOENT)
    {
        error("cannot remove `%s'\n", to);
        return 1;
    }

    ifd = ixopen3(from, O_RDONLY | O_BINARY, 0);
    if (ifd < 0)
    {
        error("%s: open failed\n", from);
        return errno;
    }

    ofd = ixopen3(to, O_WRONLY | O_CREAT | O_TRUNC | O_BINARY, 0600);
    if (ofd < 0)
    {
        error("%s: open failed\n", to);
        close(ifd);
        return 1;
    }

#ifdef HAVE_FCHMOD
    if (fchmod(ofd, mode))
    {
        error("%s: fchmod failed\n", to);
        close(ifd);
        close(ofd);
        unlink(to);
        return 1;
    }
#endif

    FCOUNT_READ(from);
    FCOUNT_WRITE(to);

    while ((len = read(ifd, buf, sizeof (buf))) > 0)
    {
        int wrote = 0;
        char *bp = buf;

        do
        {
            wrote = write(ofd, bp, len);
            if (wrote < 0)
            {
                error("%s: write failed\n", to);
                close(ifd);
                close(ofd);
                unlink(to);
                return 1;
            }
            bp += wrote;
            len -= wrote;
        } while (len > 0);
    }

    if (len < 0)
    {
        error("%s: read failed\n", from);
        close(ifd);
        close(ofd);
        unlink(to);
        return 1;
    }

    if (close (ifd) < 0)
    {
        error("%s: close failed\n", from);
        close(ofd);
        return 1;
    }

    if (close (ofd) < 0)
    {
        error("%s: close failed\n", to);
        return 1;
    }

#ifndef HAVE_FCHMOD
    if (chmod (to, mode))
    {
        error("%s: chmod failed\n", to);
        return 1;
    }
#endif

    return 0;
} /* copy_file() */

/*-------------------------------------------------------------------------*/
static int
move_file (char *from, char *to)

/* Move the file or directory <from> to <to>, copying it if necessary.
 * Result is 0 on success, 1 or errno on failure.
 */

{
    struct stat to_stats, from_stats;

    if (lstat(from, &from_stats) != 0)
    {
        error("%s: lstat failed\n", from);
        return 1;
    }

    if (lstat (to, &to_stats) == 0)
    {
        if (from_stats.st_dev == to_stats.st_dev
          && from_stats.st_ino == to_stats.st_ino)
        {
            error("`%s' and `%s' are the same file\n", from, to);
            return 1;
        }

        if (S_ISDIR (to_stats.st_mode))
        {
            error("%s: cannot overwrite directory\n", to);
            return 1;
        }

    }
    else if (errno != ENOENT)
    {
        perror("do_move");
        error("%s: unknown error\n", to);
        return 1;
    }
#ifndef RENAME_HANDLES_DIRECTORIES
    /* old SYSV */
    if (isdir(from))
    {
        char cmd_buf[100];

        if (strchr(from, '\'') || strchr(to, '\''))
            return 0;
        sprintf(cmd_buf, "/usr/lib/mv_dir '%s' '%s'", from, to);
        return system(cmd_buf);
    }
    else
#endif /* RENAME_HANDLES_DIRECTORIES */
    if (rename (from, to) == 0)
    {
        FCOUNT_DEL(from);
        return 0;
    }

#if !defined(AMIGA) || defined(__GNUC__)
    if (errno != EXDEV)
    {
        error("cannot move '%s' to '%s'\n", from, to);
        return 1;
    }
#endif

    /* rename failed on cross-filesystem link.  Copy the file instead. */

    if (!S_ISREG(from_stats.st_mode))
    {
        error("cannot move '%s' across filesystems: Not a regular file\n", from);
        return 1;
    }

    if (copy_file(from, to, from_stats.st_mode & 0777))
       return 1;

    if (unlink(from))
    {
        error("cannot remove '%s'\n", from);
        return 1;
    }
    FCOUNT_DEL(from);

    return 0;
} /* move_file() */

/*-------------------------------------------------------------------------*/
int
e_rename (char *fr, char *t)

/* EFUN rename()
 *
 *   int rename(string from, string to)
 *
 * The efun rename() will move from to the new name to. If from
 * is a file, then to may be either a file or a directory. If
 * from is a directory, then to has to be a directory. If to
 * exists and is a directory, then from will be placed in that
 * directory and keep its original name.
 *
 * You must have write permission for from to rename the file.
 *
 * On successfull completion rename() will return 0. If any error
 * occurs 1 is returned.
 */

{
    char *from, *to;
    int i;

    from = check_valid_path(fr, current_object, "rename_from", MY_TRUE);
    if (!from)
        return 1;

    push_apply_value();

    to = check_valid_path(t, current_object, "rename_to", MY_TRUE);
    if (!to)
    {
        pop_apply_value();
        return 1;
    }

    if (!strlen(to) && !strcmp(t, "/"))
    {
        to = alloca(3);
        sprintf(to, "./");
    }

    strip_trailing_slashes (from);

    if (isdir(to))
    {
        /* Target is a directory; build full target filename. */
        char *cp;
        char *newto;

        cp = strrchr(from, '/');
        if (cp)
            cp++;
        else
            cp = from;

        newto = alloca(strlen(to) + 1 + strlen(cp) + 1);
        sprintf(newto, "%s/%s", to, cp);
        pop_apply_value();
        return move_file(from, newto);
    }

    /* File to file move */
    i = move_file(from, to);
    pop_apply_value();
    return i;
} /* e_rename() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_copy_file (svalue_t *sp)

/* TEFUN copy_file()
 *
 *   int copy_file(string from, string to)
 *
 * The efun rename() will copy the file <from> to the new name <to>.
 * If <to> is a directory, then <from> will be placed in that
 * directory and keep its original name.
 *
 * You must have read permission for <from> and write permission
 * for the target name to copy the file.
 *
 * On successfull completion copy_file() will return 0. If any error
 * occurs, 1 is returned, or a runtime is generated.
 *
 * TODO: Add two more args: start, length to implement slicing?
 * TODO:: See f-981229-10 "truncate_file()".
 */

{
    struct stat to_stats, from_stats;
    char *from, *to, *cp;
    int result;

    /* Check the arguments */
    if (sp[-1].type != T_STRING)
        bad_xefun_arg(1, sp);
    if (sp->type != T_STRING)
        bad_xefun_arg(2, sp);

    switch(0){default:
        result = 1; /* Default: failure */

        from = check_valid_path(sp[-1].u.string, current_object, "copy_file"
                               , MY_FALSE);

        if (!from || isdir(from))
            break;

        /* We need our own copy of the result */
        cp = alloca(strlen(from)+1);
        strcpy(cp, from);
        from = cp;

        to = check_valid_path(sp->u.string, current_object, "copy_file"
                             , MY_TRUE);
        if (!to)
            break;

        if (!strlen(to) && !strcmp(sp->u.string, "/"))
        {
            to = alloca(3);
            strcpy(to, "./");
        }

        strip_trailing_slashes(from);

        if (isdir(to))
        {
            /* Target is a directory; build full target filename. */

            char *newto;

            cp = strrchr(from, '/');
            if (cp)
                cp++;
            else
                cp = from;

            newto = alloca(strlen(to) + 1 + strlen(cp) + 1);
            strcpy(newto, to);
            strcat(newto, "/");
            strcat(newto, cp);
            to = newto;
        }

        /* Now copy the file */

        if (lstat(from, &from_stats) != 0)
        {
            error("%s: lstat failed\n", from);
            break;
        }

        if (lstat(to, &to_stats) == 0)
        {
            if (from_stats.st_dev == to_stats.st_dev
              && from_stats.st_ino == to_stats.st_ino)
            {
                error("'%s' and '%s' are the same file\n", from, to);
                break;
            }

            if (S_ISDIR(to_stats.st_mode))
            {
                error("%s: cannot overwrite directory\n", to);
                break;
            }

        }
        else if (errno != ENOENT)
        {
            perror("copy_file");
            error("%s: unknown error\n", to);
            break;
        }

        if (!S_ISREG(from_stats.st_mode))
        {
            error("cannot copy `%s': Not a regular file\n", from);
            break;
        }

        result = copy_file(from, to, from_stats.st_mode & 0777);
    } /* switch(0) */

    /* Clean up the stack and return the result */
    free_svalue(sp);
    free_svalue(sp-1);
    put_number(sp-1, result);

    return sp-1;
} /* f_copy_file() */

/***************************************************************************/

