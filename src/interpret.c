/*---------------------------------------------------------------------------
 * Gamedriver: Bytecode Interpreter
 *
 *---------------------------------------------------------------------------
 * This module implements the bytecode interpreter for the compiled LPC
 * programs. The machine is implemented as a stackmachine with separate
 * stacks for values and control.
 *
 * See also 'exec.h' for the details of program storage, and 'svalue.h'
 * for the details of value storage.
 *
 * --- Evaluator Stack ---
 *
 *    The evaluation stack is an array of 'svalue_t's (see datatypes.h
 *    for information about this type) with EVALUATOR_SIZE<<1 elements.
 *    <inter_sp> resp. <sp> points to the last (that is topmost) valid
 *    entry in the stack, the framepointer <inter_fp> resp. <fp> points
 *    to the bottom of the frame of one function. Single values in the
 *    frame are then accessed by indexing the frame pointer.
 *    A typical stack layout looks like this:
 *
 *                    ^
 *    (inter_)sp   -> |  Top stack value
 *                    |  ...
 *                    |  Temporary stack values
 *                    |  Break addresses for switch instructions
 *    break_sp     -> |   (forming a sub-stack growing _down_).
 *                    |  ...
 *                    |  Local variable number 1
 *                    |  Local variable number 0
 *                    |  ...
 *                    |  Argument number 1
 *    (inter_)fp   -> |  Argument number 0
 *                    |
 *                    |
 *    VALUE_STACK     -----
 *
 *    The interpreter assumes that there are no destructed objects
 *    on the stack - to aid in this, the functions remove_object_from_stack()
 *    and (in array.c) check_for_destr() replace destructed objects by
 *    value 0.
 *
 *
 * --- Context Variables ---
 *
 *    In order to implement 'real' inline closures, lfun closure carry with
 *    them a set of svalue_t's, called the 'context'. When such a closure
 *    is created, the context is filled with values from selected local
 *    function variables.
 *
 *    The interpreter keeps the pointer <inter_context> pointed to the
 *    currently value context, if there is one, or NULL if there is no
 *    context. The context variables are accessed with a set of instructions
 *    mirroring those to access object variables.
 *
 *    TODO: In fact, these contexts could be implemented as light-weight
 *    TODO:: objects, removing the need for special cases.
 *
 *
 * --- Control Stack ---
 *
 *    During nested function calls, the return information to the higher
 *    functions are stored on the control stack.
 *
 *    One particularity about the current implementation is that every
 *    inter-object call (ie. every 'extern_call') and every catch()
 *    constitutes in a recursive call to eval_instruction().
 *
 *
 * --- Error Recovery Stack --- (implemented in backend.c)
 *
 *    Error recovery in general is implemented using setjmp()/longjmp().
 *    The error recovery stack holds the (possibly nested) longjmp() contexts
 *    together with an indication where the jump will lead. Currently these
 *    context types are defined:
 *      ERROR_RECOVERY_NONE:     No error recovery available
 *                               (used by the top entry in the stack)
 *      ERROR_RECOVERY_BACKEND:  Errors fall back to the backend,
 *                               e.g. process_objects(), call_heart_beat()
 *                               and others.
 *      ERROR_RECOVERY_APPLY:    Errors fall back into the secure_apply()
 *                               function used for sensitive applies.
 *      ERROR_RECOVERY_CATCH:    Errors are caught by the catch() construct.
 *
 *    The _CATCH contexts differs from the others in that it allows the
 *    continuing execution after the error. In order to achieve this, the
 *    stack entry holds the necessary additional information to re-init
 *    the interpreter.
 *    TODO: Elaborate on the details.
 *
 *
 * --- Bytecode ---
 *
 *    The machine instructions are stored as unsigned characters and read
 *    sequentially. A single machine instruction consists of one or two
 *    bytes defining the instruction, optionally followed by more bytes
 *    with parameters (e.g. number of arguments on the stack, branch offsets,
 *    etc).
 *
 *    Apart from the usual machine instructions (branches, stack
 *    manipulation, summarily called 'codes'), the machine implements every
 *    efun by its own instruction code. Since this leads to more than
 *    256 instructions, the most of the efuns are encoded using prefix
 *    bytes. The unprefixed opcodes in the range 0..255 are used for the
 *    internal machine instructions and LPC operators, and for the small
 *    and/or often used efuns. The prefix byte for the other efun
 *    instructions reflects the type of the efun:
 *
 *      F_EFUN0: efuns taking no argument
 *      F_EFUN1: efuns taking one argument
 *      F_EFUN2: efuns taking two arguments
 *      F_EFUN3: efuns taking three arguments
 *      F_EFUN4: efuns taking four arguments
 *      F_EFUNV: efuns taking more than four or a variable number of arguments
 *
 *    The implementation is such that the unprefixed instructions are
 *    implemented directly in the interpreter loop in a big switch()
 *    statement, whereas the prefixed instructions are implemented
 *    in separate functions and called via the lookup tables efun_table[]
 *    and vefun_table[].
 *
 *    Every machine instruction, efun or else, is assigned a unique number
 *    and a preprocessor symbol F_<name>. The exact translation into
 *    the prefix/opcode bytecodes depends on the number of instructions
 *    in the various classes, but is linear and holds the following
 *    conditions:
 *
 *       - all non-efun instructions do not need a prefix byte and start
 *         at instruction code 0.
 *       - selected efuns also don't need a prefix byte and directly follow
 *         the non-efun instructions.
 *       - the instruction codes for all tabled efuns are consecutive.
 *       - the instruction codes for all tabled varargs efuns are consecutive.
 *
 *    All existing machine instructions are defined in the file func_spec,
 *    which during the compilation of the driver is evaluated by make_func.y
 *    to create the LPC compiler lang.y from prolang.y, the symbolic
 *    instruction names and numbers in instrs.h, and the definition of the
 *    tables efuns in efun_defs.c .
 *
 *
 * --- Calling Convention ---
 *
 *    All arguments for a function are evaluated and pushed to the value
 *    stack. The last argument is the last pushed. It is important that
 *    the called function gets exactly as many arguments as it wants; for
 *    LPC functions ('lfuns') this means that the actual function call will
 *    remove excessive arguments or push '0's for missing arguments. The
 *    number of arguments will be stored in the control stack, so that
 *    the return instruction not needs to know it explicitly.
 *
 *    If the function called is an lfun (inherited or not), the number
 *    of arguments passed to the call is encoded in the bytecode stream,
 *    and the number of arguments expected can be determined from the
 *    functions 'struct function' entry.
 *
 *    Efuns, operators and internal bytecode usually operate on a fixed
 *    number of arguments and the compiler makes sure that the right
 *    number is given. If an efun takes a variable number of arguments,
 *    the actual number is stored in the byte following the efun's opcode.
 *
 *    The called function must ensure that exactly one value remains on the
 *    stack when returning. The caller is responsible of deallocating the
 *    returned value. This includes 'void' lfuns, which just push the
 *    value 0 as return value.
 *
 *    When a LPC function returns, it will use the instruction F_RETURN, which
 *    will deallocate all arguments and local variables, and only let the
 *    top of stack entry remain. The number of arguments and local variables
 *    are stored in the control stack, so that the evaluator knows how much
 *    to deallocate.
 *
 *    If flag 'extern_call' is set, then the evaluator should return from
 *    eval_instruction(). Otherwise, the evaluator will continue to execute
 *    the instruction at the returned address. In the current implementation,
 *    every inter-object call (call_other) receives its own (recursive)
 *    call to eval_instruction().
 *
 *---------------------------------------------------------------------------
 * TODO: The virtual machine should be reconsidered, using the DGD and MudOS
 * TODO:: machines for inspiration. This applies to implementation as well
 * TODO:: as to the instruction set.
 */

/*-------------------------------------------------------------------------*/

#include "driver.h"
#include "typedefs.h"

#include "my-alloca.h"
#include <assert.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <setjmp.h>
#include <ctype.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef MARK
#include "profile.h"
#endif

#include "interpret.h"

#include "actions.h"
#include "array.h"
#include "backend.h"
#include "call_out.h"
#include "closure.h"
#include "comm.h"
#include "efuns.h"
#include "filestat.h"
#include "gcollect.h"
#include "heartbeat.h"
#include "instrs.h"
#include "lex.h"
#include "mapping.h"
#include "mstrings.h"
#include "object.h"
#include "otable.h"
#include "parse.h"
#include "prolang.h"
#include "simulate.h"
#include "simul_efun.h"
#include "stdstrings.h"
#include "strfuns.h"
#include "structs.h"
#include "svalue.h"
#include "swap.h"
#include "switch.h"
#include "wiz_list.h"
#include "xalloc.h"

#include "i-eval_cost.h"
#include "i-svalue_cmp.h"

#include "pkg-python.h"

#include "../mudlib/sys/driver_hook.h"
#include "../mudlib/sys/driver_info.h"
#include "../mudlib/sys/trace.h"

/*-------------------------------------------------------------------------*/
/* Types */

/* --- struct catch_context: error_recovery subclass for catch() ---
 *
 * This extension of the struct error_recovery_info (see backend.h)
 * stores the additional information needed to reinitialize the global
 * variables when bailing out of a catch(). The type is always
 * ERROR_RECOVERY_CATCH.
 *
 * It is handled by the functions push_, pop_ and pull_error_context().
 */

struct catch_context
{
    struct error_recovery_info recovery_info;
      /* The subclassed recovery info.
       */
    struct control_stack * save_csp;
    object_t             * save_command_giver;
    svalue_t             * save_sp;
      /* The saved global values
       */

    svalue_t catch_value;
      /* Holds the value throw()n from within a catch() while the throw
       * is executed.
       */
};

/* --- struct cache: one entry of the apply cache
 *
 * Every entry in the apply cache holds information about a function
 * call, both for functions found and not found.
 */

struct cache
{
    string_t *name;
      /* The name of the cached function, shared for existing functions,
       * allocated if the object does not have the function.
       * This pointer counts as reference.
       */
    program_t *progp;
      /* The pointer to the program code of the function, or NULL if the
       * object does not implement the function.
       */
    int32 id;
      /* The id_number of the program. */

    funflag_t flags;
      /* Copy of the _MOD_STATIC and _MOD_PROTECTED flags of the function.
       */
    bytecode_p funstart;
      /* Pointer to the function.
       */
    int function_index_offset;
    int variable_index_offset;
      /* Function and variable index offset.
       */
};

/*-------------------------------------------------------------------------*/
/* Macros */

#define ERRORF(s) do{inter_pc = pc; inter_sp = sp; errorf s ;}while(0)
#define ERROR(s) ERRORF((s))
  /* ERRORF((...)) acts like errorf(...), except that first the local pc and sp
   * are copied into the global variables.
   * ERROR() is an easier to type form of ERRORF() when your error message
   * is just one string. It will be redefined below for the tabled
   * efuns.
   */

#define WARNF(s) do{inter_pc = pc; inter_sp = sp; warnf s ;}while(0)
#define WARN(s) WARNF((s))
#define FATALF(s) do{inter_pc = pc; inter_sp = sp; fatal s ;}while(0)
#define FATAL(s) FATALF((s))
  /* Analogue.
   */

#if APPLY_CACHE_BITS < 1
#    error APPLY_CACHE_BITS must be at least 1.
#else
#    define CACHE_SIZE (1 << APPLY_CACHE_BITS)
#endif
  /* Number of entries in the apply cache.
   */
#if CACHE_SIZE > INT_MAX
#error CACHE_SIZE is > INT_MAX.
#endif
  /* sanity check - some functions rely that CACHE_SIZE fits into int */

/*-------------------------------------------------------------------------*/
/* Tracing */

#if TOTAL_TRACE_LENGTH > INT_MAX
#error TOTAL_TRACE_LENGTH is > INT_MAX.
#endif
/* sanity check - some functions rely that TOTAL_TRACE_LENGTH fits into int */

int tracedepth;
  /* Current depth of traced functions.
   */

int trace_level;
  /* Current set of active trace options.
   * This set can be different from interactive->trace_level if several
   * nested trace() calls occur.
   */

static int traceing_recursion = -1;
  /* Kind of mutex, used to turn off tracing while doing trace output.
   * Necessary because output with add_message() might result result
   * in further code to be executed.
   */

static Bool trace_exec_active = MY_FALSE;
  /* TRUE whenever TRACE_EXEC is not just requested, but actually
   * active. This distinction is necessary as tracing might be limited
   * to one object only, and testing the object name for every instruction
   * would be too expensive. Hence, the tracing condition is checked
   * only on object changes and this variable is updated accordingly.
   * See macros SET_TRACE_EXEC and TRACE_EXEC_P.
   */

#ifdef TRACE_CODE
/* The buffers for the traced code:
 */

static int              previous_instruction[TOTAL_TRACE_LENGTH];
static ptrdiff_t        stack_size[TOTAL_TRACE_LENGTH];
static ptrdiff_t        abs_stack_size[TOTAL_TRACE_LENGTH];
static bytecode_p       previous_pc[TOTAL_TRACE_LENGTH];
static program_t * previous_programs[TOTAL_TRACE_LENGTH];
static object_t  * previous_objects[TOTAL_TRACE_LENGTH];
  /* These arrays, organized as ring buffers, hold the vitals of the
   * last TOTAL_TRACE_LENGTH instructions executed. Yet unused entries
   * are 0 resp. NULL.
   */

static int              last = TOTAL_TRACE_LENGTH - 1;
  /* Index to the last used entry in the ringbuffers above.
   */

#endif

// Forward declarations.
static Bool trace_test (int b);

/* --- Static helper functions --- */
static INLINE Bool TRACE_IS_INTERACTIVE()
/* Return TRUE if the current command_giver is interactive.
 * TODO: Instead of disabling all traceoutput whenever the command_giver
 * TODO:: turns non-interactive, output should be redirected (with a
 * TODO:: special mark) to the current_interactive.
 */
{
    return command_giver && O_IS_INTERACTIVE(command_giver);
}

static INLINE Bool TRACETST(int b)
/* Return TRUE if the any of the tracing options <b> are requested
 * by the interactive user.
 */
{
    return TRACE_IS_INTERACTIVE() && (O_GET_INTERACTIVE(command_giver)->trace_level & (b));
}

static INLINE Bool TRACEP(int b)
  /* Return TRUE if tracing options <b> are both active in trace_level
   * and requested by the interactive user.
   */
{
    return trace_level & (b) && trace_test(b);
}

static INLINE Bool TRACEHB()
  /* Return TRUE if either the current execution is not caused
   * by a heart beat call, or if heartbeat tracing is allowed.
   */
{
    return current_heart_beat == NULL || TRACETST(TRACE_HEART_BEAT);
}

static void INLINE SET_TRACE_EXEC()
  /* If TRACE_EXEC is requested, (re)activate it.
   * See trace_exec_active for the background.
   */
{
    if (trace_level & TRACE_EXEC)
        trace_exec_active = MY_TRUE;
}
static INLINE Bool TRACE_EXEC_P()
  /* If TRACE_EXEC is still requested, return TRUE, otherwise deactivate
   * it and return FALSE.
   * See trace_exec_active for the background.
   */
{
    return TRACEP(TRACE_EXEC) ||  (trace_exec_active = MY_FALSE, MY_FALSE);
}

/*-------------------------------------------------------------------------*/
/* The names for the svalue types */

static const char * svalue_typename[]
 = { /* T_INVALID */  "invalid"
   , /* T_LVALUE  */  "lvalue"
   , /* T_NUMBER  */  "number"
   , /* T_STRING  */  "string"
   , /* T_POINTER */  "array"
   , /* T_OBJECT  */  "object"
   , /* T_MAPPING */  "mapping"
   , /* T_FLOAT   */  "float"
   , /* T_CLOSURE */  "closure"
   , /* T_SYMBOL  */  "symbol"
   , /* T_QUOTED_ARRAY */   "quoted-array"
   , /* T_STRUCT */         "struct"
   , /* T_BYTES  */         "bytes"
   , /* T_CALLBACK */       "callback"
   , /* T_ERROR_HANDLER */  "error-handler"
   , /* T_BREAK_ADDR */     "break-address"
   , /* T_NULL */           "null"
   , /* T_BYTES */          "bytes"
   };

/*-------------------------------------------------------------------------*/
/* Variables */

/* The virtual machine's registers.
 *
 * While the interpreter is in eval_instruction(), some of the values are
 * kept in local variables for greater speed, with the globals being updated
 * only when necessary.
 * The affected variables are: inter_pc, inter_sp, TODO: which else?
 */

svalue_t *inter_sp;
  /* Points to last valid value on the value stack.
   */
bytecode_p inter_pc;
  /* Next bytecode to interpret.
   */

static svalue_t *inter_fp;
  /* Framepointer: pointer to first argument.
   */

static svalue_t *inter_context;
  /* Contextpointer: pointer to first context variable.
   * May be NULL if no context is available.
   */

static svalue_t *break_sp;
  /* Points to address to branch to at next F_BREAK from within a switch().
   * This is actually a stack of addresses with break_sp pointing to the
   * bottom with the most recent entry. This break stack is stored on
   * the evaluator stack, one address per svalue_t (which have T_BREAK_ADDR
   * set as the type), between the functions temporary values and its local
   * variables (the compiler added the needed stack depth to the number
   * of local variables).
   */

program_t *current_prog;
  /* The current program. This is usually current_object->prog, but can
   * differ when executing an inherited program.
   */

static svalue_t current_lambda;
  /* If the VM is executing a lambda, this variable holds a counted
   * reference to it to make sure that it isn't destructed while it is
   * still executing.
   */

static string_t **current_strings;
  /* Pointer to the string literal block of the current program for
   * faster access.
   */

int function_index_offset;
  /* Index of current program's function block within the functions of
   * the current objects program (needed for inheritance).
   */

static int variable_index_offset;
  /* Index of current program's non-virtual variable block within
   * the variables of the current object (needed for inheritance).
   *
   * Note that when executing lambda closures, the offset will
   * always be zero, even though the program has virtual variables.
   * This is because indices for F_IDENTIFIER in the lambda code
   * are always relative to the whole variable block.
   * Also note that when executing efun closures, this offset won't
   * be changed at all. Efuns accessing variables will need to
   * update variable_index_offset and current_variables themselves.
   */

svalue_t *current_variables;
  /* Pointer to begin of the current variable block.
   * This is current_object->variables + variable_index_offset for
   * faster access.
   */


/* Other Variables */

int32 eval_cost;
  /* The amount of eval cost used in the current execution thread.
   */

int32 assigned_eval_cost;
  /* Auxiliary variable used to account eval costs to single objects and
   * their user's wizlist entry.
   * Whenver the execution thread enters a different object,
   * assigned_eval_cost is set to the current value of eval_cost. When the
   * thread leaves the object again, the difference between the actual
   * eval_cost value and the older assigned_eval_cost is accounted to
   * the current object.
   * The implementation combines both actions in one function
   * assign_eval_cost().
   */



svalue_t apply_return_value = { T_NUMBER };
  /* This variable holds the result from a call to apply(), transferred
   * properly from the interpreter stack where the called function
   * left it.
   * push_ and pop_apply_value() handle this particular transfer.
   * Note: The process_string() helper function process_value() takes
   * direct advantage of this variable.
   */

static svalue_t struct_member_temporary = { T_NUMBER };
  /* This variable holds the a temporary used for relaxed struct member
   * lookups. get_struct_item() will return a pointer to this variable
   * whenever a struct member is not found. An lvalue may be created
   * from it.
   */

#define SIZEOF_STACK (EVALUATOR_STACK_SIZE<<1)
#if SIZEOF_STACK > INT_MAX
#error SIZEOF_STACK is > INT_MAX.
#endif
/* sanity check - some function rely that SIZEOF_STACK fits into int */

static svalue_t value_stack_array[SIZEOF_STACK+1];
#define VALUE_STACK (value_stack_array+1)

  /* The evaluator stack, sized with (hopefully) enough fudge to handle
   * function arguments and overflows.
   * The stack grows upwards, and <inter_sp> points to last valid entry.
   *
   * The first entry of value_stack_array[] is not used and serves as
   * dummy so that underflows can be detected in a portable way
   * (Standard C disallows indexing before an array). Instead, VALUE_STACK
   * is the real bottom of the stack.
   */

#if MAX_TRACE > INT_MAX
#error MAX_TRACE is > INT_MAX.
#endif
#if MAX_USER_TRACE >= MAX_TRACE
#error MAX_USER_TRACE value must be smaller than MAX_TRACE!
#endif
  /* Sanity check for the control stack definition.
   */

static struct control_stack control_stack_array[MAX_TRACE+2];
#define CONTROL_STACK (control_stack_array+2)
struct control_stack *csp;
  /* The control stack holds copies of the machine registers for previous
   * function call levels, with <csp> pointing to the last valid
   * entry, describing the last context.
   * This also means that CONTROL_STACK[0] (== control_stack_array[2]) will
   * have almost no interesting values as it will terminate execution.
   * Especially CONTROL_STACK[0].prog is NULL to mark the bottom.
   *
   * The first two entries of control_stack_array[] are not used and
   * serve as dummies so that underflows can be detected in a portable
   * way (Standard C disallows indexing before an array).
   */

static Bool runtime_no_warn_deprecated = MY_FALSE;
  /* Set to TRUE if the current instruction is not to warn about usage
   * of deprecated features; reset at the end of the instruction.
   * This flag is set by the NO_WARN_DEPRECATED instruction, generated
   * by the bytecode compiler in response to the warn-deprecated pragma.
   */

static Bool runtime_array_range_check = MY_FALSE;
  /* Set to TRUE if the current instruction is to warn about using
   * an illegal range.
   * This flag is set by the ARRAY_RANGE_CHECK instruction, generated
   * by the bytecode compiler in response to the range-check pragma.
   */

#ifdef APPLY_CACHE_STAT
statcounter_t apply_cache_hit  = 0;
statcounter_t apply_cache_miss = 0;
  /* Number of hits and misses in the apply cache.
   */
#endif

static struct cache cache[CACHE_SIZE];
  /* The apply cache.
   */

  /* --- struct unprotected_char: a single character in a string */
struct unprotected_char
{
    char     *charp;              /* The indexed character. */
    string_t *str;                /* The string that is indexed
                                   * (not refcounted). */
    struct protected_lvalue *var; /* A (uncounted) protected_lvalue
                                   * referencing that string.
                                   * Can be NULL. */
} current_unprotected_char =      /* Static buffer, because there is only */
  { NULL, NULL, NULL };           /* one unprotected lvalue at a time. */

  /* --- struct unprotected_range: a range in a string or vector */
struct unprotected_range
{
    svalue_t *vec;                /* The string or vector containing
                                   * the range. */
    mp_int    index1, index2;     /* first and last (exc.) index of the range */
    struct protected_lvalue *var; /* A (uncounted) protected_lvalue
                                   * referencing that string or vector.
                                   * Can be NULL. */
} current_unprotected_range =     /* Static buffer, because there is only */
  { NULL, 0, 0 };                 /* one unprotected lvalue at a time. */
  /* When assigning to vector and string ranges or elements, the
   * target information is stored in this structure.
   *
   * Used knowingly by: push_index_lvalue(), transfer_pointer_range(),
   *                    assign_string_range().
   * Used unknowingly by: assign_svalue(), transfer_svalue(),
   *                    add_number_to_lvalue(), F_VOID_ASSIGN.
   */

  /* --- struct unprotected_mapentry: an entry into a mapping */
struct unprotected_mapentry
{
    mapping_t *map;               /* The mapping that'll get the entry.
                                   * (not refcounted). */
    svalue_t   key;               /* The key (refcounted) */
    int        index;             /* The column. */
} current_unprotected_mapentry =  /* Static buffer, because there is only */
  { NULL, { T_INVALID }, 0 };     /* one unprotected lvalue at a time. */
  /* This is used for assignments to mapping entries that do not(!)
   * exist, yet. In contrast to protected mapping entries there is no
   * time here, that an entry will appear.
   */

static svalue_t indexing_quickfix = { T_NUMBER };
  /* When indexing arrays and mappings with just one ref, especially
   * for the purpose of getting a lvalue, the indexed item is copied
   * into this variable and indexed from here.
   * Used by operators: push_(r)indexed_lvalue, push_indexed_map_lvalue.
   * TODO: Rename this variable, or better: devise a nice solution.
   * TODO:: Use the protected_lvalues instead?
   * TODO:: To quote Marion:
   * TODO::     marion says: but this is crude too
   * TODO::     marion blushes.
   * TODO: Is it made sure that this var will be vacated before the
   * TODO:: next use? Otoh, if not it's no problem as the value is
   * TODO:: by definition volatile.
   */

#ifdef OPCPROF

#define MAXOPC (LAST_INSTRUCTION_CODE)
  /* Number of different instructions to trace.
   */

static int opcount[MAXOPC];
  /* Counter array for instruction profiling: each entry counts the
   * usages of one instruction. The full instruction code (not the
   * opcode) is used as index.
   */

#endif

#ifdef DEBUG

static program_t *check_a_lot_ref_counts_search_prog;
  /* Program you developer are especially interested in.
   */

static struct pointer_table *ptable;
  /* check_a_lot_of_ref_counts: the table of structures already
   * visited.
   */

#endif

p_uint eval_number;
  /* evaluation number. (may overflow)
   */

unsigned long total_evalcost;
static struct timeval eval_begin;
  /* Current total evalcost counter, and start of the evaluation.
   */

unsigned long last_total_evalcost = 0;
struct timeval last_eval_duration = { 0 };
  /* Last total evaluation cost and duration.
   */

statistic_t stat_total_evalcost = { 0 };
statistic_t stat_eval_duration = { 0 };
  /* Weighted statistics of evaluation cost and duration.
   */

struct timeval profiling_timevalue = {0, 0};
  /* timer value for the profiling timer for detection of long executions,
   * Default: 0ms (detection deactivated)
   */

static Bool received_prof_signal = MY_FALSE;

p_int used_memory_at_eval_start = 0;
  /* used memory (in bytes) at the beginning of the current execution,
   * set by mark_start_evaluation() (and v_limited()).
   */

int num_protected_lvalues = 0;
  /* Number of protected_(char_|range_)lvalue objects in play.
   */

/*-------------------------------------------------------------------------*/
/* Forward declarations */

enum { APPLY_NOT_FOUND = 0, APPLY_FOUND, APPLY_DEFAULT_FOUND };
static int int_apply(string_t *, object_t *, int, Bool, Bool);
enum call_other_error_handling
{
    CO_IGNORE, /* call_other    */
    CO_RESULT, /* call_resolved */
    CO_ERROR   /* call_strict   */
};
static svalue_t* int_call_other(bool b_use_default, enum call_other_error_handling error_handling, char* efunname, svalue_t *sp, int num_arg);
static int expand_argument(svalue_t *sp);
static void call_simul_efun(unsigned int code, object_t *ob, int num_arg);
#ifdef DEBUG
static void check_extra_ref_in_vector(svalue_t *svp, size_t num);
#endif

/*-------------------------------------------------------------------------*/

/* Assign the evaluation cost elapsed since the last call to the
 * current_object and it's user's wizlist entry. Then set assigned_eval_cost
 * to the current eval_cost so that later calls can do the same.
 *
 * This function must be called at least whenever the execution leaves
 * one object for another one.
 *
 * assign_eval_cost_inl() is the inlinable version used here, 
 * assign_eval_cost() is used by other compilation units.
 */

static INLINE void
assign_eval_cost_inl(void)
{
    unsigned long carry;
    if (current_object->user)
    {
        current_object->user->cost += eval_cost - assigned_eval_cost;
        carry = current_object->user->cost / 1000000000;
        if (carry)
        {
            current_object->user->gigacost += carry;
            current_object->user->cost %= 1000000000;
        }
        current_object->user->total_cost += eval_cost - assigned_eval_cost;
        carry = current_object->user->total_cost / 1000000000;
        if (carry)
        {
            current_object->user->total_gigacost += carry;
            current_object->user->total_cost %= 1000000000;
        }
    }
    current_object->ticks += eval_cost - assigned_eval_cost;
    {
        carry = current_object->ticks / 1000000000;
        if (carry)
        {
            current_object->gigaticks += carry;
            current_object->ticks %= 1000000000;
        }
    }
    assigned_eval_cost = eval_cost;
}

void assign_eval_cost(void) { assign_eval_cost_inl(); }

/*-------------------------------------------------------------------------*/
void
handle_profiling_signal(int ignored)
/* signal handler for the SIGPROF signal. Just sets a flag which is checked in
 * eval_instruction() at the end of each instruction.
 */
{
    received_prof_signal = MY_TRUE;
} // handle_prof()

/*-------------------------------------------------------------------------*/
void
mark_start_evaluation (void)

/* Called before a new evaluation; resets the current evaluation statistics.
 */

{
    // .it_interval is always zero (no auto-repeat), .it_value will be set later
    static struct itimerval prof_time_val = { {0,0}, {0,0} };

    total_evalcost = 0;
    eval_number++;

    // start the profiling timer if enabled
    if (profiling_timevalue.tv_usec || profiling_timevalue.tv_sec)
    {
        prof_time_val.it_value = profiling_timevalue;
        setitimer(ITIMER_PROF, &prof_time_val, NULL);
    }

    if (gettimeofday(&eval_begin, NULL))
    {
        eval_begin.tv_sec = eval_begin.tv_usec = 0;
    }
    
    used_memory_at_eval_start = xalloc_used();

    /* Also reset any interrupt flag that happened between threads. */
    interrupt_execution = false;

} /* mark_start_evaluation() */

/*-------------------------------------------------------------------------*/
void
mark_end_evaluation (void)

/* Called after an evaluation; updates the evaluation statistics.
 */

{
    static struct itimerval prof_time_val = { {0,0}, {0,0} };

    // disable the profiling timer
    if (profiling_timevalue.tv_usec || profiling_timevalue.tv_sec)
        setitimer(ITIMER_PROF, &prof_time_val, NULL);

    if (total_evalcost == 0)
        return;

    last_total_evalcost = total_evalcost;

    if (eval_begin.tv_sec == 0
     || gettimeofday(&last_eval_duration, NULL))
    {
        last_eval_duration.tv_sec = last_eval_duration.tv_usec = 0;
    }
    else
    {
        last_eval_duration.tv_sec -= eval_begin.tv_sec;
        last_eval_duration.tv_usec -= eval_begin.tv_usec;
        
        if (last_eval_duration.tv_usec < 0)
        {
            last_eval_duration.tv_sec--;
            last_eval_duration.tv_usec += 1000000;
        }

        update_statistic_avg( &stat_eval_duration
                            , last_eval_duration.tv_sec * 1000000L
                              + last_eval_duration.tv_usec
                            );
    }

    update_statistic_avg(&stat_total_evalcost, last_total_evalcost);
} /* mark_end_evaluation() */

/*-------------------------------------------------------------------------*/
void
init_interpret (void)

/* Initialize the interpreter data structures, especially the apply cache.
 */

{
    struct cache invalid_entry;
    int i;

    /* The cache is inited to hold entries for 'functions' in a non-existing
     * program (id 0). The first real apply calls will thus see a (virtual)
     * collision with 'older' cache entries.
     */

    invalid_entry.id = 0;
    invalid_entry.progp = (program_t *)1;
    invalid_entry.name = NULL;
    
    /* To silence the compiler: */
    invalid_entry.variable_index_offset = 0;
    invalid_entry.function_index_offset = 0;
    invalid_entry.funstart = 0;
    invalid_entry.flags = 0;

    for (i = 0; i < CACHE_SIZE; i++)
        cache[i] = invalid_entry;
} /* init_interpret()*/

/*-------------------------------------------------------------------------*/
static INLINE Bool
is_sto_context (void)

/* Return TRUE if the current call context has a set_this_object()
 * in effect.
 */

{
    struct control_stack *p;

    for (p = csp; !p->extern_call; p--) NOOP;

    return (p->extern_call & CS_PRETEND) != 0;
} /* is_sto_context() */

/*=========================================================================*/

/*                         S V A L U E S                                   */

/*-------------------------------------------------------------------------*/
/* The following functions handle svalues, ie. the data referenced
 * by the svalue_ts. 'Freeing' in this context therefore never means
 * a svalue_t, only the data referenced by it.
 *
 * destructed_object_ref(v): test if <v> references a destructed object.
 * object_ref(v,o):          test if <v> references object <o>
 *
 * free_string_svalue(v): free string svalue <v>.
 * free_object_svalue(v): free object svalue <v>.
 * zero_object_svalue(v): replace the object in svalue <v> by number 0.
 * free_svalue(v):        free the svalue <v>.
 *
 * normalize_svalue(svp, collapse): Replace references to destructed
 *                        objects with the number 0, reduce lvalue lists to
 *                        a single entry. And if <collapse> is true,
 *                        replace a singular lvalue with its rvalue.
 *
 * internal_assign_svalue_no_free(to, from): put a copy of <from> into <to>;
 *                        <to> is considered empty.
 *
 * internal_assign_rvalue_no_free(to, from): put a copy of <from> into <to>;
 *                        <to> is considered empty. If <from> is an lvalue,
 *                        copy its rvalue instead.
 *
 * assign_svalue_no_free(to,from): put a copy of <from> into <to>; <to>
 *                        is considered empty, but normalize <from> first.
 *
 * assign_rvalue_no_free(to,from): put a copy of <from> into <to>; <to>
 *                        is considered empty, but normalize <from> first.
 *                        If <from> is an lvalue, copy its rvalue instead.
 *
 * copy_svalue_no_free(to,from): put a shallow value copy of <from> into <to>;
 *                        <to> is considered empty.
 *
 * get_rvalue(v, &last_ref): If <v> is a range lvalue, return NULL.
 *                        If <v> is any other lvalue, return a pointer to its
 *                        rvalue, otherwise return <v>. If the result will
 *                        go away, if <v> is overwritten, set <last_ref> to true.
 *
 * get_rvalue_no_collapse(v, &last_ref): Same as get_rvalue(), but don't
 *                        collapse any singular lvalues.
 *
 * link_protected_lvalue(dest, lv): Assign the lvalue <lv> to the lvalue <dest>.
 *
 * assign_svalue(dest,v): assign <v> to <dest>, freeing <dest> first.
 *                        Also handles assigns to lvalues.
 *
 * transfer_svalue_no_free(dest,v): move <v> into <dest>; <dest> is
 *                        considered empty.
 *
 * transfer_rvalue_no_free(dest,v): move <v> into <dest>; <dest> is
 *                        considered empty. If <from> is an lvalue,
 *                        copy its rvalue and remove the lvalue.
 *
 * transfer_svalue(dest,v): move <v> into <dest>; freeing <dest> first.
 *                        Also handles transfers to lvalues.
 *
 * static add_number_to_lvalue(dest,i,pre,post): add <i> to lvalue <dest>.
 *
 * In addition there are some helper functions.
 *
 * TODO: All these functions and vars should go into a separate module.
 */

/*-------------------------------------------------------------------------*/
/* Forward declarations */

static void transfer_pointer_range(svalue_t *source);
static void transfer_protected_pointer_range(
    struct protected_range_lvalue *dest, svalue_t *source);
static void assign_string_range(svalue_t *source, Bool do_free);
static void assign_protected_string_range(
    struct protected_range_lvalue *dest,svalue_t *source, Bool do_free);
static p_int read_unprotected_char();
static p_int read_protected_char(struct protected_char_lvalue *src);
static p_int assign_unprotected_char(p_int ch);
static p_int assign_protected_char(struct protected_char_lvalue *dest, p_int ch);

/*-------------------------------------------------------------------------*/
void INLINE
free_string_svalue (svalue_t *v)

/* Free the string svalue <v>; <v> must be of type T_STRING or T_BYTES.
 */

{
#ifdef DEBUG
    if (v->type != T_STRING && v->type != T_BYTES)
    {
        fatal("free_string_svalue(): Expected string|bytes, "
              "received svalue type (%"PRIdPHINT":%"PRIdPHINT")\n"
            , v->type, v->x.generic);
        /* NOTREACHED */
        return;
    }
#endif

    free_mstring(v->u.str);
}

/*-------------------------------------------------------------------------*/
void
free_object_svalue (svalue_t *v)

/* Free the object svalue <v>; <v> must be of type T_OBJECT.
 */

{
    object_t *ob = v->u.ob;

#ifdef DEBUG
    if (v->type != T_OBJECT)
    {
        fatal("free_object_svalue(): Expected object, "
              "received svalue type (%"PRIdPHINT":%"PRIdPHINT")\n"
            , v->type, v->x.generic);
        /* NOTREACHED */
        return;
    }
#endif

    free_object(ob, "free_object_svalue");
}

/*-------------------------------------------------------------------------*/
void
zero_object_svalue (svalue_t *v)

/* Change <v> from an object svalue to the svalue-number 0.
 */

{
    object_t *ob = v->u.ob;

    free_object(ob, "zero_object_svalue");
    put_number(v, 0);
}

/*-------------------------------------------------------------------------*/
void
deref_protected_lvalue (struct protected_lvalue* lv)

/* Decrement the reference count of <lv> by one,
 * free the structure if it reaches zero.
 * lv can be NULL, then it is ignored.
 */

{
    if (lv && --(lv->ref) <= 0)
    {
        free_svalue(&(lv->val));
        xfree(lv);

        num_protected_lvalues--;
    }
} /* deref_protected_lvalue() */

/*-------------------------------------------------------------------------*/
static void
int_free_svalue (svalue_t *v)

/* Free the svalue <v>, which may be of any type.
 * Afterwards, the content of <v> is undefined.
 */

{
    ph_int type = v->type;

    v->type = T_INVALID;
      /* If freeing the value throws an error, it is most likely that
       * we ran out of stack. To avoid the error handling running
       * out of stack on the same value again, we mask it before we free
       * it - at the risk of leaking memory.
       */

    assert_stack_gap();

    switch (type)
    {
    default:
        fatal("(free_svalue) Illegal svalue %p type %d\n", v, type);
        /* NOTREACHED */
        break;

    case T_INVALID:
    case T_NUMBER:
    case T_FLOAT:
        NOOP;
        break;

    case T_STRING:
      {
        string_t *str = v->u.str;

        assert(str->info.unicode != STRING_BYTES);
        free_mstring(str);
        break;
      }

    case T_BYTES:
      {
        string_t *str = v->u.str;

        assert(str->info.unicode == STRING_BYTES);
        free_mstring(str);
        break;
      }

    case T_OBJECT:
      {
        object_t *ob = v->u.ob;
        free_object(ob, "free_svalue");
        break;
      }

    case T_QUOTED_ARRAY:
    case T_POINTER:
        free_array(v->u.vec);
        break;

    case T_STRUCT:
        free_struct(v->u.strct);
        break;

    case T_MAPPING:
        free_mapping(v->u.map);
        break;

    case T_SYMBOL:
        assert(v->u.str->info.unicode != STRING_BYTES);
        free_mstring(v->u.str);
        break;

    case T_CLOSURE:
        free_closure(v);
        break;

    case T_CALLBACK:
        free_callback(v->u.cb);
        xfree(v->u.cb);
        break;

    case T_ERROR_HANDLER:
        v->u.error_handler->fun(v->u.error_handler);
        break;

    case T_BREAK_ADDR:
        NOOP;
        break;

    case T_LVALUE:
        switch (v->x.lvalue_type)
        {
        default:
            fatal("(free_svalue) Illegal lvalue %p type %d\n", v, v->x.lvalue_type);
            /* NOTREACHED */
            break;

        case LVALUE_UNPROTECTED:
        case LVALUE_UNPROTECTED_CHAR:
        case LVALUE_UNPROTECTED_RANGE:
        case LVALUE_UNPROTECTED_MAPENTRY:
            NOOP;
            break;

        case LVALUE_PROTECTED:
            deref_protected_lvalue(v->u.protected_lvalue);
            break;

        case LVALUE_PROTECTED_CHAR:
            if (--(v->u.protected_char_lvalue->ref) <= 0)
            {
                struct protected_char_lvalue *p;
                p = v->u.protected_char_lvalue;

                if (p->str->info.type == STRING_MUTABLE)
                {
                    /* Remove it from the string's lvalue list. */
                    for (struct protected_char_lvalue **next = &(p->str->u.mutable.char_lvalues);
                         *next != NULL;
                         next = &((*next)->next))
                    {
                        if (*next == p)
                        {
                            *next = p->next;
                            break;
                        }
                    }
                }

                free_mstring(p->str);
                deref_protected_lvalue(p->var);
                xfree(p);

                num_protected_lvalues--;
            }
            break;

        case LVALUE_PROTECTED_RANGE:
            if (--(v->u.protected_range_lvalue->ref) <= 0)
            {
                struct protected_range_lvalue *r;
                r = v->u.protected_range_lvalue;

                if ((r->vec.type == T_STRING || r->vec.type == T_BYTES)
                 && r->vec.u.str->info.type == STRING_MUTABLE)
                {
                    /* Remove it from the string's lvalue list. */
                    for (struct protected_range_lvalue **next = &(r->vec.u.str->u.mutable.range_lvalues);
                         *next != NULL;
                         next = &((*next)->next))
                    {
                        if (*next == r)
                        {
                            *next = r->next;
                            break;
                        }
                    }
                }

                free_svalue(&(r->vec));
                deref_protected_lvalue(r->var);
                xfree(r);

                num_protected_lvalues--;
            }
            break;

        case LVALUE_PROTECTED_MAPENTRY:
            if (--(v->u.protected_mapentry_lvalue->ref) <= 0)
            {
                struct protected_mapentry_lvalue *e = v->u.protected_mapentry_lvalue;

                free_mapping(e->map);
                free_svalue(&(e->key));
                xfree(e);

                num_protected_lvalues--;
            }
            break;

        } /* switch (v->x.lvalue_type) */
        break; /* case T_LVALUE */

    }
} /* int_free_svalue() */

/*-------------------------------------------------------------------------*/

/* Queue element to deserialize the freeing of complex svalues. */
struct fs_queue_s {
    struct fs_queue_s * next;
    svalue_t                   value;
};

typedef struct fs_queue_s fs_queue_t;

static fs_queue_t fs_queue_base;
  /* Static fs_queue_t variable to avoid xallocs for the simple cases.
   */

static fs_queue_t * fs_queue_head = NULL;
static fs_queue_t * fs_queue_tail = NULL;
  /* Double-ended list of deserialized svalues to free.
   */
    
void
free_svalue (svalue_t *v)

/* Free the svalue <v>, which may be of any type, while making sure that
 * complex nested structures are deserialized (to avoid stack overflows).
 * Afterwards, the content of <v> is undefined.
 */

{
    Bool needs_deserializing = MY_FALSE;

    switch (v->type)
    {
    case T_QUOTED_ARRAY:
    case T_POINTER:
        needs_deserializing = (v->u.vec->ref == 1);
        break;

    case T_STRUCT:
        needs_deserializing = (struct_ref(v->u.strct) == 1);
        break;

    case T_MAPPING:
        needs_deserializing = (v->u.map->ref == 1);
        break;

    case T_LVALUE:
        switch (v->x.lvalue_type)
        {
        case LVALUE_PROTECTED:
            needs_deserializing = (v->u.protected_lvalue->ref == 1);
            break;

        case LVALUE_PROTECTED_RANGE:
            needs_deserializing = (v->u.protected_range_lvalue->ref == 1);
            break;

        case LVALUE_PROTECTED_MAPENTRY:
            needs_deserializing = (v->u.protected_mapentry_lvalue->ref == 1);
            break;
        }
        break;
    }

    /* If the value doesn't need de-serializing, it can be
     * be freed immediately.
     */
    if (!needs_deserializing)
    {
        int_free_svalue(v);
        return;
    }

    /* If there are elements in the queue, we are inside the freeing of a
     * complex structure, and this element just needs to be queued up.
     * When out of memory, however, just free it.
     */
    if (fs_queue_head != NULL)
    {
        int save_privilege = malloc_privilege;

        malloc_privilege = MALLOC_SYSTEM;
        fs_queue_t * tmp = xalloc(sizeof(*tmp));
        malloc_privilege = save_privilege;

        if (NULL == tmp)
        {
            int_free_svalue(v);
            return;
        }

        /* Copy the value over, invalidating this one. */
        tmp->next = NULL;
        tmp->value = *v;
        v->type = T_INVALID;

        /* Insert the element into the queue. */
        fs_queue_tail->next = tmp;
        fs_queue_tail = tmp;

        return;
    }

    /* This is the first complex value to be freed - start the queue.
     */
    fs_queue_base.next = NULL;
    fs_queue_base.value = *v;
    v->type = T_INVALID;

    fs_queue_head = fs_queue_tail = &fs_queue_base;

    /* Now loop over the queue, successively freeing the values.
     * If one of the values freed contains complex freeable structures
     * itself, they will be added to the end of the queue and eventually
     * picked up by this loop.
     */
    while (fs_queue_head != NULL)
    {
        fs_queue_t * current = fs_queue_head;

        int_free_svalue(&(fs_queue_head->value));

        fs_queue_head = fs_queue_head->next;
        if (fs_queue_head == NULL)
            fs_queue_tail = NULL;

        if (current != &fs_queue_base)
            xfree(current);
    }
} /* free_svalue() */

/*-------------------------------------------------------------------------*/
static INLINE Bool
_destructed_object_ref (svalue_t *svp)

/* Return TRUE if the svalue in <svp> references a destructed object.
 */

{
    lambda_t *l;
    int type;

    if (svp->type != T_OBJECT && svp->type != T_CLOSURE)
        return MY_FALSE;

    if (svp->type == T_OBJECT || !CLOSURE_MALLOCED(type = svp->x.closure_type))
        return (svp->u.ob->flags & O_DESTRUCTED) ? MY_TRUE : MY_FALSE;

    /* Lambda closure */

    l = svp->u.lambda;

    if (CLOSURE_HAS_CODE(type) && type == CLOSURE_UNBOUND_LAMBDA)
        return MY_FALSE;

    if (type == CLOSURE_LFUN
     && (l->function.lfun.ob->flags & O_DESTRUCTED))
        return MY_TRUE;

    return (l->ob->flags & O_DESTRUCTED) ? MY_TRUE : MY_FALSE;

} /* _destructed_object_ref() */

Bool destructed_object_ref (svalue_t *v) { return _destructed_object_ref(v); }

#define destructed_object_ref(v) _destructed_object_ref(v)

/*-------------------------------------------------------------------------*/
static INLINE Bool
inl_object_ref (svalue_t *svp, object_t *obj)

/* Return TRUE if <svp> references object <obj> (destructed or alive),
 * return FALSE if it doesn't.
 */

{
    lambda_t *l;
    int type;

    if (svp->type != T_OBJECT && svp->type != T_CLOSURE)
        return MY_FALSE;

    if (svp->type == T_OBJECT || !CLOSURE_MALLOCED(type = svp->x.closure_type))
        return svp->u.ob == obj;

    /* Lambda closure */

    l = svp->u.lambda;

    if (CLOSURE_HAS_CODE(type) && type == CLOSURE_UNBOUND_LAMBDA)
        return MY_FALSE;

    if (type == CLOSURE_LFUN && l->function.lfun.ob == obj)
        return MY_TRUE;

    return l->ob == obj;

} /* inl_object_ref() */
// exporting the function als object_ref() is currently not necessary, because
// it is only used in this file.
#define object_ref(v,o) inl_object_ref(v,o)

/*-------------------------------------------------------------------------*/
static INLINE void
internal_assign_svalue_no_free (svalue_t *to, svalue_t *from)

/* Put a duplicate of svalue <from> into svalue <to>, meaning that the original
 * value is either copied when appropriate, or its refcount is increased.
 * <to> is considered empty at the time of call.
 *
 * If <from> is a destructed object, <to> is set to the number 0 but
 * <from> is left unchanged.
 *
 * <from> may never be an unprotected lvalue.
 */

{
#ifdef DEBUG
    if (from == 0)
        fatal("Null pointer to assign_svalue().\n");
#endif

    /* Copy all the data */
    *to = *from;

    /* Now create duplicates resp. increment refcounts where necessary */

    switch(from->type)
    {
    case T_STRING:
    case T_BYTES:
        (void)ref_mstring(from->u.str);
        break;

    case T_OBJECT:
        {
          object_t *ob = to->u.ob;
          if ( !(ob->flags & O_DESTRUCTED) )
              (void)ref_object(ob, "ass to var");
          else
              put_number(to, 0);

          break;
        }
        break;

    case T_QUOTED_ARRAY:
    case T_POINTER:
        (void)ref_array(to->u.vec);
        break;

    case T_STRUCT:
        (void)ref_struct(to->u.strct);
        break;

    case T_SYMBOL:
        (void)ref_mstring(to->u.str);
        break;

    case T_CLOSURE:
        addref_closure(to, "ass to var");
        break;

    case T_MAPPING:
        (void)ref_mapping(to->u.map);
        break;

    case T_LVALUE:
        switch(to->x.lvalue_type)
        {
        default:
            fatal("(assign_svalue_no_free) Illegal lvalue %p type %d\n", to, to->x.lvalue_type);
            /* NOTREACHED */
            break;

        case LVALUE_PROTECTED:
            to->u.protected_lvalue->ref++;
            break;

        case LVALUE_PROTECTED_CHAR:
            to->u.protected_char_lvalue->ref++;
            break;

        case LVALUE_PROTECTED_RANGE:
            to->u.protected_range_lvalue->ref++;
            break;

        case LVALUE_PROTECTED_MAPENTRY:
            to->u.protected_mapentry_lvalue->ref++;
            break;

        } /* switch */
        break;
    }

} /* internal_assign_svalue_no_free() */

/*-------------------------------------------------------------------------*/
static INLINE void
inl_copy_svalue_no_free (svalue_t *to, svalue_t *from)

/* Put a duplicate of svalue <from> into svalue <to>, meaning that the original
 * value is either copied when appropriate, or its refcount is increased.
 * In particular, if <from> is a mapping (which must not contain destructed
 * objects!) or array, a shallow copy is created.
 * <to> is considered empty at the time of call.
 *
 * If <from> is a destructed object, <to> is set to the number 0 but
 * <from> is left unchanged.
 *
 * <from> may never be an unprotected lvalue.
 */

{
    /* For arrays and mappings, create a shallow copy */
    if (from->type == T_MAPPING)
    {
        mapping_t *old, *new;
        old = from->u.map;

        DYN_MAPPING_COST(MAP_SIZE(old));
        new = copy_mapping(old);
        if (!new)
            errorf("Out of memory: mapping[%"PRIdPINT"] for copy.\n"
                 , MAP_SIZE(old));
        put_mapping(to, new);
    }
    else if (from->type == T_POINTER
          || from->type == T_QUOTED_ARRAY)
    {
        vector_t *old, *new;
        size_t size, i;

        old = from->u.vec;
        size = VEC_SIZE(old);
        if (old != &null_vector)
        {
            DYN_ARRAY_COST(size);
            new = allocate_uninit_array((int)size);
            if (!new)
                errorf("Out of memory: array[%zu] for copy.\n"
                     , size);
            for (i = 0; i < size; i++)
                assign_svalue_no_free( &new->item[i]
                                     , &old->item[i]);
            *to = *from;
            to->u.vec = new;
        }
        else
            put_ref_array(to, &null_vector);
    }
    else
        internal_assign_svalue_no_free(to, from);
} /* inl_copy_svalue_no_free() */

void copy_svalue_no_free (svalue_t *to, svalue_t *from)
{ inl_copy_svalue_no_free(to,from); }

#define copy_svalue_no_free(to,from) inl_copy_svalue_no_free(to,from)

/*-------------------------------------------------------------------------*/
static INLINE void
internal_assign_rvalue_no_free ( svalue_t *to, svalue_t *from )

/* Put a duplicate of svalue <from> into svalue <to>, meaning that the original
 * value is either copied when appropriate, or its refcount is increased.
 * <to> is considered empty at the time of call.
 *
 * If <from> is a protected lvalue, it will be dereferenced to get
 * the real value. (It is not allowed to be an unprotected lvalue.)
 * So afterwards <to> is guaranteed not to be an lvalue.
 * If that value is a destructed object, 0 is assigned.
 */

{
    while (from->type == T_LVALUE)
    {
        switch (from->x.lvalue_type)
        {
        default:
            fatal("(assign_rvalue_no_free) Illegal lvalue %p type %d\n", from, from->x.lvalue_type);
            /* NOTREACHED */
            break;

        case LVALUE_PROTECTED:
            internal_assign_svalue_no_free(to, &(from->u.protected_lvalue->val));
            break;

        case LVALUE_PROTECTED_CHAR:
            {
                p_int ch = read_protected_char(from->u.protected_char_lvalue);
                put_number(to, ch);
                break;
            }

        case LVALUE_PROTECTED_RANGE:
            {
                struct protected_range_lvalue *r = from->u.protected_range_lvalue;
                svalue_t *vec = &(r->vec);

                switch (vec->type)
                {
                case T_POINTER:
                    {
                        vector_t *slice = slice_array(vec->u.vec, r->index1, r->index2 - 1);
                        if (slice == NULL)
                            errorf("Out of memory: array[%"PRIdMPINT"..%"PRIdMPINT"].\n", r->index1, r->index2 - 1);
                        else
                            put_array(to, slice);
                        break;
                    }

                case T_STRING:
                case T_BYTES:
                    {
                        string_t *slice;
                        if (r->index2 <= r->index1)
                            slice = ref_mstring(vec->type == T_STRING ? STR_EMPTY : empty_byte_string);
                        else
                            slice = mstr_extract(vec->u.str, r->index1, r->index2 - 1);
                        if (slice == NULL)
                            errorf("Out of memory: string[%"PRIdMPINT"..%"PRIdMPINT"].\n", r->index1, r->index2 - 1);
                        else
                        {
                            to->type = vec->type;
                            to->u.str = slice;
                        }
                        break;
                    }

                default:
                    fatal("(assign_rvalue_no_free) Illegal range %p type %d\n", r, vec->type);
                    /* NOTREACHED */
                    break;
                }
            }
            break;

        case LVALUE_PROTECTED_MAPENTRY:
            {
                struct protected_mapentry_lvalue *e = from->u.protected_mapentry_lvalue;
                svalue_t *val = get_map_value(e->map, &(e->key));

                if (val == &const0)
                    from = val;
                else
                {
                    /* Entry exists, change the lvalue. */
                    svalue_t temp = *from;
                    assign_protected_lvalue_no_free(from, val + e->index);
                    free_svalue(&(temp));
                }

                /* Handle from again. */
                continue;
            }
            break;

        } /* switch (from->x.lvalue_type) */

        break;
    }

    if (from->type != T_LVALUE)
    {
        /* <from> is not an lvalue, so we can try to make
         * it constant again...
         */
        if(from->type == T_STRING || from->type == T_BYTES)
            try_make_constant(from->u.str);

        internal_assign_svalue_no_free(to, from);
    }

    /* If this was a mutable string, isolate it. */
    if(to->type == T_STRING || to->type == T_BYTES)
    {
        string_t *str = make_constant(to->u.str);
        if (str)
            to->u.str = str;
    }

} /* internal_assign_rvalue_no_free() */

/*-------------------------------------------------------------------------*/
INLINE void
normalize_svalue (svalue_t *svp, bool collapse_lvalues)

/* Checks whether <svp> is a protected lvalue (unprotected lvalues are not
 * allowed here). If so, the normalizing steps are applied:
 *
 * 1. If it is a simple protected lvalue pointing to a protected range
 *    lvalue it is replaced by that range lvalue.
 * 2. If this lvalue holds the only reference to the underlying svalue
 *    it is replaced by the svalue (or a range/char of it).
 *    This is done only if <collapse_lvalues> is true.
 * 3. If this is a protected range lvalue, check whether the referenced
 *    variable is still valid. Otherwise replace it with NULL.
 * 4. If this is a mapping entry lvalue, check whether the entry
 *    appeared in the meanwhile and replace it with a ordinary lvalue.
 *
 * If <svp> is a reference to a destructed object, it will be replaced
 * by 0. In all other cases this is a no-op.
 */

{
    while (svp->type == T_LVALUE)
    {
        switch(svp->x.lvalue_type)
        {
            default:
                fatal("(normalize_lvalue) Illegal lvalue %p type %d\n", svp, svp->x.lvalue_type);
                /* NOTREACHED */
                break;

            case LVALUE_PROTECTED:
            {
                svalue_t *dest = &(svp->u.protected_lvalue->val);

                if (dest->type == T_LVALUE
                 || (collapse_lvalues && svp->u.protected_lvalue->ref == 1))
                {
                    svalue_t temp = *svp;
                    internal_assign_svalue_no_free(svp, dest);
                    free_svalue(&temp);
                    continue;
                }

                if (destructed_object_ref(dest))
                {
                    free_svalue(dest);
                    internal_assign_svalue_no_free(dest, &const0);
                    break;
                }
                break;
            }

            case LVALUE_PROTECTED_CHAR:
                if (collapse_lvalues && svp->u.protected_char_lvalue->ref == 1 && svp->u.protected_char_lvalue->str->info.ref == 1)
                {
                    svalue_t temp = *svp;
                    internal_assign_rvalue_no_free(svp, &temp);
                    free_svalue(&temp);
                    continue;
                }
                break;

            case LVALUE_PROTECTED_RANGE:
            {
                struct protected_range_lvalue *r = svp->u.protected_range_lvalue;

                if (collapse_lvalues
                 && r->ref == 1
                 && (r->var == NULL || r->var->ref == 1))
                {
                    svalue_t temp = *svp;
                    internal_assign_rvalue_no_free(svp, &temp);
                    free_svalue(&temp);
                    continue;
                }

                if (r->var != NULL
                 && (r->vec.type != r->var->val.type
                  || (r->vec.type == T_POINTER && r->vec.u.vec != r->var->val.u.vec)
                  || (r->vec.type == T_STRING  && r->vec.u.str != r->var->val.u.str)
                  || (r->vec.type == T_BYTES   && r->vec.u.str != r->var->val.u.str)))
                {
                    deref_protected_lvalue(r->var);
                    r->var = NULL;
                }
                break;
            }

            case LVALUE_PROTECTED_MAPENTRY:
            {
                struct protected_mapentry_lvalue *e = svp->u.protected_mapentry_lvalue;
                svalue_t *val = get_map_value(e->map, &(e->key));

                if (val == &const0)
                {
                    /* Still no entry in the mapping. Keep it. */
                    break;
                }
                else
                {
                    /* This key exists now, we can replace <lv>
                     * with an lvalue into that.
                     */
                    svalue_t temp = *svp;
                    assign_protected_lvalue_no_free(svp, val + e->index);
                    free_svalue(&temp);
                    continue;
                }
            }
        }
        break;
    } /* switch */

    if (destructed_object_ref(svp))
    {
        free_svalue(svp);
        internal_assign_svalue_no_free(svp, &const0);
    }
} /* normalize_lvalue() */

/*-------------------------------------------------------------------------*/
static INLINE void
inl_assign_svalue_no_free (svalue_t *to, svalue_t *from)

/* Put a duplicate of svalue <from> into svalue <to>, meaning that the original
 * value is either copied when appropriate, or its refcount is increased.
 * <to> is considered empty at the time of call.
 * <from> may point to a variable or vector element, so it might contain
 * a destructed object. In that case, <from> and <to> are set to
 * svalue-number 0.
 *
 * <from> may never be an unprotected lvalue.
 */

{
    normalize_svalue(from, false);
    internal_assign_svalue_no_free(to, from);
} /* inl_assign_svalue_no_free() */

void assign_svalue_no_free (svalue_t *to, svalue_t *from)
{ inl_assign_svalue_no_free(to,from); }

#define assign_svalue_no_free(to,from) inl_assign_svalue_no_free(to,from)

/*-------------------------------------------------------------------------*/
static INLINE void
inl_assign_rvalue_no_free (svalue_t *to, svalue_t *from)

/* Put a duplicate of svalue <from> into svalue <to>, meaning that the original
 * value is either copied when appropriate, or its refcount is increased.
 * <to> is considered empty at the time of call.
 *
 * If <from> is a protected lvalue, it will be dereferenced to get
 * the real value. (It is not allowed to be an unprotected lvalue.)
 * So afterwards <to> is guaranteed not to be an lvalue.
 * If that value is a destructed object, 0 is assigned.
 * In that case also <from> is set to svalue-number 0.
 */

{
    normalize_svalue(from, true);
    internal_assign_rvalue_no_free(to, from);
} /* inl_assign_rvalue_no_free() */

void assign_rvalue_no_free (svalue_t *to, svalue_t *from)
{ inl_assign_rvalue_no_free(to,from); }

#define assign_rvalue_no_free(to,from) inl_assign_rvalue_no_free(to,from)

/*-------------------------------------------------------------------------*/
void assign_rvalue_no_free_no_collapse(svalue_t *to, svalue_t *from)

/* Same as assign_rvalue_no_free(), but keeps the lvalues in <to>,
 * even if it is the last reference.
 */
{
    normalize_svalue(from, false);
    internal_assign_rvalue_no_free(to, from);
} /* assign_rvalue_no_free_no_collapse() */

/*-------------------------------------------------------------------------*/
static INLINE svalue_t *
inl_get_rvalue (svalue_t *v, bool *last_reference, bool collapse_lvalues)

/* This function handles unravelling of protected lvalues.
 * (<v> may not be an unprotected lvalue.)
 *
 * If <v> is a normal lvalue, get the base rvalue its pointing at.
 * If <v> is a range lvalue, then NULL is returned.
 * (In that case use assign_rvalue_no_free to store a copy of the range.)
 * If <v> is a char lvalue, the value of that character is stored in a
 * local svalue and a reference to this svalue is returned.
 * (May be overwritten with a subsequent call to this function.)
 * If <v> is not an lvalue at all, it is returned.
 *
 * If <last_reference> is not NULL, it will be set to true
 * if <v> is a protected lvalue with just one reference
 * or not an lvalue at all. (Otherwise it will stay unchanged.)
 *
 * This function normalizes the lvalue first. <collapse_lvalues>
 * is passed to normalize_svalue.
 */

{
    normalize_svalue(v, collapse_lvalues);

    if (v->type == T_LVALUE)
    {
        switch (v->x.lvalue_type)
        {
            default:
                fatal("(get_rvalue) Illegal lvalue %p type %d\n", v, v->x.lvalue_type);
                /* NOTREACHED */
                break;

            case LVALUE_PROTECTED:
                if (last_reference != NULL && v->u.protected_lvalue->ref == 1)
                    *last_reference = true;

                return &(v->u.protected_lvalue->val);

            case LVALUE_PROTECTED_CHAR:
            {
                if (last_reference != NULL && v->u.protected_char_lvalue->ref == 1)
                    *last_reference = true;

                static svalue_t charval = { T_NUMBER };
                charval.u.number = read_protected_char(v->u.protected_char_lvalue);

                return &charval;
            }

            case LVALUE_PROTECTED_RANGE:
                if (last_reference != NULL && v->u.protected_range_lvalue->ref == 1)
                    *last_reference = true;

                return NULL;

            case LVALUE_PROTECTED_MAPENTRY:
            {
                /* When we are here, there is not a corresponding entry
                 * in the mapping. Otherwise the normalization would
                 * have changed the lvalue to LVALUE_PROTECTED.
                 */
                if (last_reference != NULL && v->u.protected_mapentry_lvalue->ref == 1)
                    *last_reference = true;

                return &const0;
            }
        } /* switch (v->x.lvalue_type) */
    }
    else
    {
        if(last_reference != NULL)
            *last_reference = true;
        return v;
    }
} /* inl_get_rvalue() */

svalue_t * get_rvalue (svalue_t *v, bool *last_reference)
{ return inl_get_rvalue(v, last_reference, true); }
svalue_t * get_rvalue_no_collapse (svalue_t *v, bool *last_reference)
{ return inl_get_rvalue(v, last_reference, false); }

#define get_rvalue(v, last_reference) inl_get_rvalue(v, last_reference, true)
#define get_rvalue_no_collapse(v, last_reference) inl_get_rvalue(v, last_reference, false)

/*-------------------------------------------------------------------------*/
static bool
link_protected_lvalue (svalue_t *dest, svalue_t *lv)

/* Link both lvalues together, so that they represent a single value
 * later on. Both, <dest> and <lv>, must be protected lvalues.
 * It is assumed, that both are normalized. The resulting value
 * (where both point to at the end) is the content of <lv>.
 *
 * The function returns true on success, false otherwise
 * (when both lvalues are incompatible).
 */

{
    assert(dest->type == T_LVALUE);
    assert(lv->type == T_LVALUE);

    switch (dest->x.lvalue_type)
    {
        default:
            fatal("(link_protected_lvalue) Illegal lvalue %p type %d\n", dest, dest->x.lvalue_type);
            /* NOTREACHED */
            break;

        case LVALUE_PROTECTED:
            /* Are they the same? */
            if (lv->x.lvalue_type == LVALUE_PROTECTED
             && lv->u.protected_lvalue == dest->u.protected_lvalue)
                 return true;

            /* This is pretty easy, replace <dest> with <lv>.
             * For all other svalues that reference <dest->u.protected_lvalue>
             * we need to put a reference to <lv> also there.
             */
            if (dest->u.protected_lvalue->ref != 1)
            {
                /* Should be normalized. */
                assert(dest->u.protected_lvalue->val.type != T_LVALUE);

                free_svalue(&(dest->u.protected_lvalue->val));
                internal_assign_svalue_no_free(&(dest->u.protected_lvalue->val), lv);
            }

            free_svalue(dest);
            internal_assign_svalue_no_free(dest, lv);
            return true;

        case LVALUE_PROTECTED_CHAR:
            switch (lv->x.lvalue_type)
            {
                default:
                    fatal("(link_protected_lvalue) Illegal lvalue %p type %d\n", lv, lv->x.lvalue_type);
                    /* NOTREACHED */
                    break;

                case LVALUE_PROTECTED:
                    /* We do this the other way around,
                     * make <lv> into a protected_char_lvalue,
                     * but do the assignment first.
                     */
                    if (lv->u.protected_lvalue->val.type != T_NUMBER)
                        return false;

                    assign_protected_char(dest->u.protected_char_lvalue, lv->u.protected_lvalue->val.u.number);
                    return link_protected_lvalue(lv, dest);

                case LVALUE_PROTECTED_CHAR:
                    /* Are they the same? */
                    if (lv->u.protected_char_lvalue == dest->u.protected_char_lvalue)
                         return true;

                    /* TODO: We would need to link them together, there is no other way,
                     * because we would need to update both chars every time there
                     * is an assignment.
                     */
                    return false; /* For now. */

                case LVALUE_PROTECTED_RANGE:
                    /* char and range are incompatible. */
                    return false;

                case LVALUE_PROTECTED_MAPENTRY:
                    /* A normalized map entry is always a non-existent entry. */
                    assign_protected_char(dest->u.protected_char_lvalue, 0);
                    return true;

            } /* switch (lv->x.lvalue_type) */
            break;

        case LVALUE_PROTECTED_RANGE:
            switch (lv->x.lvalue_type)
            {
                default:
                    fatal("(link_protected_lvalue) Illegal lvalue %p type %d\n", lv, lv->x.lvalue_type);
                    /* NOTREACHED */
                    break;

                case LVALUE_PROTECTED:
                {
                    /* So hopefully this points to an array/string.
                     * Then we do the assignment and link them the other
                     * way around (making <lv> a protected_range_lvalue).
                     */
                    struct protected_range_lvalue *prot_range = dest->u.protected_range_lvalue;
                    struct protected_lvalue *prot_lval = lv->u.protected_lvalue;

                    if (prot_lval->val.type != prot_range->vec.type)
                        return false;

                    switch(prot_range->vec.type)
                    {
                        case T_POINTER:
                            ref_array(prot_lval->val.u.vec); /* transfer_...() will free it once */
                            transfer_protected_pointer_range(prot_range, &(prot_lval->val));
                            break;

                        case T_STRING:
                        case T_BYTES:
                            assign_protected_string_range(prot_range, &(prot_lval->val), MY_FALSE);
                            break;

                        default:
                            fatal("(link_protected_lvalue) Illegal range %p type %d\n", prot_range, prot_range->vec.type);
                            /* NOTREACHED */
                            break;
                    }
                    return link_protected_lvalue(lv, dest);
                }

                case LVALUE_PROTECTED_CHAR:
                    /* char and range are incompatible. */
                    return false;

                case LVALUE_PROTECTED_RANGE:
                {
                    struct protected_range_lvalue *lvrange = lv->u.protected_range_lvalue;
                    struct protected_range_lvalue *destrange = dest->u.protected_range_lvalue;

                     /* Are they the same? */
                    if (lvrange == destrange)
                          return true;

                    /* We can't process different types. */
                    if (lvrange->vec.type != destrange->vec.type)
                        return false;

                    if (lvrange->vec.type == T_POINTER)
                    {
                        /* For vectors we need to handle also the elements. */
                        if (lvrange->index2 - lvrange->index1 != destrange->index2 - destrange->index1)
                        {
                            /* Let's make them the same size, first.
                             * We need to update all ranges in the linked list of dest.
                             */
                            vector_t *oldvec = destrange->vec.u.vec;
                            vector_t *vec = allocate_array(VEC_SIZE(oldvec) + lvrange->index2 - lvrange->index1 + destrange->index1 - destrange->index2);
                            svalue_t *srcitem = oldvec->item;
                            svalue_t *dstitem = vec->item;

                            for (int i = destrange->index1; i > 0; i--)
                                assign_svalue_no_free(dstitem++, srcitem++);

                            srcitem = lvrange->vec.u.vec->item + lvrange->index1;
                            for (int i = lvrange->index2 - lvrange->index1; i > 0; i--)
                                assign_protected_lvalue_no_free(dstitem++, srcitem++);

                            srcitem = oldvec->item + destrange->index2;
                            for (int i = VEC_SIZE(oldvec) - destrange->index2; i > 0; i--)
                                assign_svalue_no_free(dstitem++, srcitem++);

                            if (destrange->var != NULL
                             && destrange->var->val.type == T_POINTER
                             && destrange->var->val.u.vec == oldvec)
                            {
                                destrange->var->val.u.vec = ref_array(vec);
                                free_array(oldvec);
                            }
                            destrange->index2 = destrange->index1 + lvrange->index2 - lvrange->index1;
                            destrange->vec.u.vec = vec;
                            free_array(oldvec);
                        }
                        else
                        {
                            /* Link the elements together. */
                            svalue_t *srcitem = lvrange->vec.u.vec->item + lvrange->index1;
                            svalue_t *dstitem = destrange->vec.u.vec->item + destrange->index1;

                            for (int i = lvrange->index2 - lvrange->index1; i > 0; i--)
                                assign_protected_lvalue(dstitem++, srcitem++);
                        }
                    }
                    else
                    {
                        /* We can only do an assignment here. */
                        svalue_t temp;
                        internal_assign_rvalue_no_free(&temp, lv);
                        assign_protected_string_range(destrange, &temp, true);
                    }

                    return true;
                }

                case LVALUE_PROTECTED_MAPENTRY:
                    /* range and non-existent entry are incompatible. */
                    return false;

            } /* switch (lv->x.lvalue_type) */
            break;

        case LVALUE_PROTECTED_MAPENTRY:
        {
            /* We create the entry and assign <lv>. */
            struct protected_mapentry_lvalue *e = dest->u.protected_mapentry_lvalue;
            svalue_t *val = get_map_lvalue(e->map, &(e->key));
            svalue_t temp = *dest;

            /* Copy the lvalue into mapping. */
            assert(val->type == T_NUMBER && val->u.number == 0);
            internal_assign_svalue_no_free(val + e->index, lv);
            internal_assign_svalue_no_free(dest, lv);
            free_svalue(&temp);
            break;
        }

    } /* switch (dest->x.lvalue_type) */

    return false; /* NOTREACHED */

} /* link_protected_lvalue */

/*-------------------------------------------------------------------------*/
void
assign_svalue (svalue_t *dest, svalue_t *v)

/* Put a duplicate of svalue <v> into svalue <dest>, meaning that the
 * original value is either copied when appropriate, or its refcount is
 * increased.
 *
 * <dest> is considered a valid svalue and therefore freed before the
 * assignment. Structured values will necessiate doing the assignment before
 * the actual deallocation, otherwise recursive structures could cause crashs.
 * One nasty example is
 *    a = ( ({((a=({0})),(a[0]=a)),(a=0)})[0] = query_verb() );
 * which used to corrupt the shared string table, namely the entry for
 * the verb in variable a if its length uses a memory block of
 * the same length as an array of size 2.
 *
 * If <dest> is a lvalue, <v> will be assigned to the svalue referenced
 * to by <dest>.
 *
 * <v> may never be an unprotected lvalue.
 */

{
    normalize_svalue(v, false);

    /* First check whether we assign to an lvalue. */
    while (dest->type == T_LVALUE)
    {
        /* If the final svalue in dest is one of these lvalues,
         * the assignment is done right here and now.
         */
        switch(dest->x.lvalue_type)
        {
        default:
            fatal("(assign_svalue) Illegal lvalue %p type %d\n", dest, dest->x.lvalue_type);
            /* NOTREACHED */
            break;

        case LVALUE_UNPROTECTED:
            dest = dest->u.lvalue;
            break;

        case LVALUE_UNPROTECTED_CHAR:
            if (v->type == T_NUMBER)
                assign_unprotected_char(v->u.number);
            return;

        case LVALUE_UNPROTECTED_RANGE:
            switch (current_unprotected_range.vec->type)
            {
            case T_POINTER:
                if (v->type == T_POINTER)
                {
                    (void)ref_array(v->u.vec); /* transfer_...() will free it once */
                    transfer_pointer_range(v);
                }
                return;

            case T_STRING:
            case T_BYTES:
                assign_string_range(v, MY_FALSE);
                return;

            default:
                fatal("(assign_svalue) Illegal type in protected range lvalue: %s, expected array or string.\n",
                   typename(current_unprotected_range.vec->type));
                /* NOTREACHED */
                return;
            } /* switch() */
            /* NOTREACHED */
            return;

        case LVALUE_UNPROTECTED_MAPENTRY:
            {
                /* We create the entry to assign to. */
                svalue_t *val = get_map_lvalue(current_unprotected_mapentry.map, &(current_unprotected_mapentry.key));

                /* The entry should not have existed. */
                assert(val->type == T_NUMBER && val->u.number == 0);
                dest = val + current_unprotected_mapentry.index;

                free_svalue(&(current_unprotected_mapentry.key));
                break;
            }

        case LVALUE_PROTECTED:
            if (v->type == T_LVALUE)
            {
                normalize_svalue(dest, false);
                link_protected_lvalue(dest, v);
                return;
            }
            else
                dest = &(dest->u.protected_lvalue->val);
            break;

        case LVALUE_PROTECTED_CHAR:
            if (v->type == T_LVALUE)
            {
                normalize_svalue(dest, false);
                link_protected_lvalue(dest, v);
            }
            else if (v->type == T_NUMBER)
                assign_protected_char(dest->u.protected_char_lvalue, v->u.number);
            return;

        case LVALUE_PROTECTED_RANGE:
            if (v->type == T_LVALUE)
            {
                normalize_svalue(dest, false);
                link_protected_lvalue(dest, v);
                return;
            }
            else
            {
                struct protected_range_lvalue *r;

                r = dest->u.protected_range_lvalue;

                switch(r->vec.type)
                {
                    case T_POINTER:
                        if (v->type == T_POINTER)
                        {
                            (void)ref_array(v->u.vec); /* transfer_...() will free it once */
                            transfer_protected_pointer_range(r, v);
                        }
                        /* TODO: Else throw an error? */
                        return;

                    case T_STRING:
                    case T_BYTES:
                        assign_protected_string_range(r, v, MY_FALSE);
                        return;

                    default:
                        fatal("(assign_svalue) Illegal range %p type %d\n", r, r->vec.type);
                        /* NOTREACHED */
                        break;
                }
                /* NOTREACHED */
                return;
            }

        case LVALUE_PROTECTED_MAPENTRY:
            if (v->type == T_LVALUE)
            {
                normalize_svalue(dest, false);
                link_protected_lvalue(dest, v);
                return;
            }
            else
            {
                /* Let's create the entry if it doesn't exist already. */
                struct protected_mapentry_lvalue *e = dest->u.protected_mapentry_lvalue;
                dest = get_map_lvalue(e->map, &(e->key)) + e->index;
                continue;
            }

        } /* switch() */

        break;
    }

    /* Now free the <dest> svalue, and assign the new value.
     */

    free_svalue(dest);
    internal_assign_svalue_no_free(dest, v);

} /* assign_svalue() */

/*-------------------------------------------------------------------------*/
static INLINE void
inl_transfer_svalue_no_free (svalue_t *dest, svalue_t *v)

/* Move the value <v> into <dest>.
 *
 * <dest> is assumed to be invalid before the call, <v> is invalid after.
 */

{
    /* Copy the data */
    *dest = *v;
} /* inl_transfer_svalue_no_free() */

void transfer_svalue_no_free (svalue_t *dest, svalue_t *v)
{  inl_transfer_svalue_no_free(dest,v); }

#define transfer_svalue_no_free(dest,v) inl_transfer_svalue_no_free(dest,v)

/*-------------------------------------------------------------------------*/
static INLINE void
inl_transfer_rvalue_no_free (svalue_t *dest, svalue_t *v)

/* Move the value <v> into <dest>.
 *
 * <dest> is assumed to be invalid before the call, <v> is invalid after.
 *
 * If <v> is a protected lvalue, it will be dereferenced to get
 * the real value. (It is not allowed to be an unprotected lvalue.)
 * So afterwards <dest> is guaranteed not to be an lvalue.
 */

{
    if (v->type == T_LVALUE
     || ((v->type == T_STRING || v->type == T_BYTES) && mstr_mutable(v->u.str)))
    {
        assign_rvalue_no_free(dest, v);
        free_svalue(v);
    }
    else
        transfer_svalue_no_free(dest, v);

} /* inl_transfer_rvalue_no_free() */

void transfer_rvalue_no_free (svalue_t *dest, svalue_t *v)
{  inl_transfer_rvalue_no_free(dest,v); }

#define transfer_rvalue_no_free(dest,v) inl_transfer_rvalue_no_free(dest,v)

/*-------------------------------------------------------------------------*/
static INLINE void
inl_transfer_svalue (svalue_t *dest, svalue_t *v)

/* Move svalue <v> into svalue <dest>.
 *
 * <dest> is considered a valid svalue and therefore freed before the
 * assignment. <v> will be invalid after the call.
 *
 * If <dest> is a lvalue, <v> will be moved into the svalue referenced
 * to by <dest>.
 *
 * TODO: Test if copying this function into F_VOID_ASSIGN case speeds up
 * TODO:: the interpreter.
 */

{
    normalize_svalue(v, false);

    /* First check whether we assign to an lvalue. */
    while (dest->type == T_LVALUE)
    {
        /* If the final svalue in dest is one of these lvalues,
         * the assignment is done right here and now.
         */
        switch(dest->x.lvalue_type)
        {
        default:
            fatal("(transfer_svalue) Illegal lvalue %p type %d\n", dest, dest->x.lvalue_type);
            /* NOTREACHED */
            break;

        case LVALUE_UNPROTECTED:
            dest = dest->u.lvalue;
            break;

        case LVALUE_UNPROTECTED_CHAR:
            if (v->type == T_NUMBER)
                assign_unprotected_char(v->u.number);
            else
                free_svalue(v);
            return;

        case LVALUE_UNPROTECTED_RANGE:
            switch (current_unprotected_range.vec->type)
            {
            case T_POINTER:
                if (v->type == T_POINTER)
                    transfer_pointer_range(v);
                else
                    free_svalue(v);
                return;

            case T_STRING:
            case T_BYTES:
                assign_string_range(v, MY_TRUE);
                return;

            default:
                fatal("(transfer_svalue) Illegal type in protected range lvalue: %s, expected array or string.\n",
                   typename(current_unprotected_range.vec->type));
                /* NOTREACHED */
                return;
            } /* switch() */
            /* NOTREACHED */
            return;

        case LVALUE_UNPROTECTED_MAPENTRY:
            {
                /* We create the entry to transfer to. */
                svalue_t *val = get_map_lvalue(current_unprotected_mapentry.map, &(current_unprotected_mapentry.key));

                /* The entry should not have existed. */
                assert(val->type == T_NUMBER && val->u.number == 0);
                dest = val + current_unprotected_mapentry.index;

                free_svalue(&(current_unprotected_mapentry.key));
                break;
            }

        case LVALUE_PROTECTED:
            if (v->type == T_LVALUE)
            {
                normalize_svalue(dest, false);
                link_protected_lvalue(dest, v);
                free_svalue(v);
                return;
            }
            else
                dest = &(dest->u.protected_lvalue->val);
            break;

        case LVALUE_PROTECTED_CHAR:
            if (v->type == T_LVALUE)
            {
                normalize_svalue(dest, false);
                link_protected_lvalue(dest, v);
                free_svalue(v);
                return;
            }
            else if (v->type == T_NUMBER)
                assign_protected_char(dest->u.protected_char_lvalue, v->u.number);
            else
                free_svalue(v);
            return;

        case LVALUE_PROTECTED_RANGE:
            if (v->type == T_LVALUE)
            {
                normalize_svalue(dest, false);
                link_protected_lvalue(dest, v);
                free_svalue(v);
                return;
            }
            else
            {
                struct protected_range_lvalue *r;

                r = dest->u.protected_range_lvalue;

                switch(r->vec.type)
                {
                    case T_POINTER:
                        if (v->type == T_POINTER)
                            transfer_protected_pointer_range(r, v);
                       else
                            free_svalue(v);
                            /* TODO: Else throw an error? */
                        return;

                    case T_STRING:
                    case T_BYTES:
                        assign_protected_string_range(r, v, MY_TRUE);
                        return;

                    default:
                        fatal("(transfer_svalue) Illegal range %p type %d\n", r, r->vec.type);
                        /* NOTREACHED */
                        break;
                }
                /* NOTREACHED */
                return;
            }

        case LVALUE_PROTECTED_MAPENTRY:
            if (v->type == T_LVALUE)
            {
                normalize_svalue(dest, false);
                link_protected_lvalue(dest, v);
                free_svalue(v);
                return;
            }
            else
            {
                struct protected_mapentry_lvalue *e = dest->u.protected_mapentry_lvalue;
                dest = get_map_lvalue(e->map, &(e->key)) + e->index;
                continue;
            }
        } /* switch() */

        break;
    }

    /* Free the <dest> svalue and transfer <v>. */
    free_svalue(dest);
    transfer_svalue_no_free(dest, v);

} /* inl_transfer_svalue() */

void transfer_svalue (svalue_t *dest, svalue_t *v)
{  inl_transfer_svalue(dest,v); }

#define transfer_svalue(dest,v) inl_transfer_svalue(dest,v)

/*-------------------------------------------------------------------------*/
static void
transfer_pointer_range (svalue_t *source)

/* Transfer the vector <source> to the vector range defined by
 * <current_unprotected_range>, modifying the target vector therein
 * accordingly. <source> is freed once in the call.
 *
 * If <source> is not a vector, it is just freed.
 */

{
    if (source->type == T_POINTER)
    {
        vector_t *sv;      /* Source vector (from source) */
        vector_t *dv;      /* Destination vector (from current_unprotected_range) */
        vector_t *rv;      /* Result vector */
        mp_int dsize;           /* Size of destination vector */
        mp_int ssize;           /* Size of source vector */
        mp_int index1, index2;  /* First and last index of destination range */
        mp_int i;

        assert(current_unprotected_range.vec->type == T_POINTER);

        /* Setup the variables */
        index1 = current_unprotected_range.index1;
        index2 = current_unprotected_range.index2;
        dv = current_unprotected_range.vec->u.vec;
        sv = source->u.vec;
        dsize = (mp_int)VEC_SIZE(dv);
        ssize = (mp_int)VEC_SIZE(sv);

#ifdef NO_NEGATIVE_RANGES
        if (index1 > index2)
            errorf("Illegal range [%"PRIdMPINT"..%"PRIdMPINT
                   "] for assignment.\n", index1, index2-1
                 );
#endif /* NO_NEGATIVE_RANGES */

        if (ssize + index1 - index2 == 0)
        {
            /* <source> fits exactly into the target range */

            svalue_t *s, *d;  /* Copy source and destination */

            s = sv->item;
            d = dv->item + index1;

            ref_array(dv); /* protect against recursive refs during the copy */

            /* If there is just one ref to the source, use the faster
             * transfer instead of the slow assign for the copy.
             */
            if (sv->ref == 1)
            {
                for (i = ssize; --i >= 0; )
                {
                    transfer_svalue(d++, s++);
                }
                free_empty_vector(sv);
            }
            else /* sv->ref > 1 */
            {
                for (i = ssize; --i >= 0; )
                {
                    assign_svalue(d++, s++);
                }

                free_array(sv);
                  /* deref_array() is not enough, because in situations
                   * where one d == sv, eg
                   *    arr = ({ ({ 1 }) });
                   *    arr[0..0] = arr[0];
                   * sv would be left behind with 0 refs but unfreed.
                   */
            }

            free_array(dv); /* Undo the ref_array() above */
        }
        else
        {
            /* Create a new vector */

            svalue_t *s, *d; /* Copy source and destination */

            rv = allocate_array(dsize + ssize + index1 - index2);
            current_unprotected_range.vec->u.vec = rv;
            s = dv->item;
            d = rv->item;

            for (i = index1; --i >= 0; )
            {
                assign_svalue_no_free(d++, s++);
            }

            s = sv->item;
            for (i = ssize; --i >= 0; )
            {
                assign_svalue_no_free(d++, s++);
            }
            free_array(sv);

            s = dv->item + index2;
            for (i = dsize - index2; --i >= 0; )
            {
                assign_svalue_no_free(d++, s++);
            }

            /* Update any variable holding that vector. */
            if (current_unprotected_range.var != NULL
             && current_unprotected_range.var->val.type == T_POINTER
             && current_unprotected_range.var->val.u.vec == dv)
            {
               current_unprotected_range.var->val.u.vec = ref_array(rv);
               free_array(dv);
            }

            free_array(dv); /* this can make the lvalue invalid to use */
        }
    }
    else
        /* Not a pointer: just free it */
        free_svalue(source);

} /* transfer_pointer_range() */

/*-------------------------------------------------------------------------*/
static void
transfer_protected_pointer_range ( struct protected_range_lvalue *dest
                                 , svalue_t *source)

/* Transfer the vector <source> to the vector range defined by
 * <dest>, modifying the target vector in <dest>
 * accordingly. <source> is freed once in the call.
 *
 * If <source> is not a vector, it is just freed.
 */

{
    if (source->type == T_POINTER)
    {
        vector_t *sv;      /* Source vector (from source) */
        vector_t *dv;      /* Dest vector (from dest) */
        vector_t *rv;      /* Result vector */
        mp_int dsize;           /* Size of the dest vector */
        mp_int ssize;           /* Size of the source vector */
        mp_int index1, index2;  /* Target range indices */
        mp_int i;

        assert(dest->vec.type == T_POINTER);

        /* Setup the variables */
        index1 = dest->index1;
        index2 = dest->index2;
        dv = dest->vec.u.vec;
        sv = source->u.vec;
        dsize = (mp_int)VEC_SIZE(dv);
        ssize = (mp_int)VEC_SIZE(sv);

#ifdef NO_NEGATIVE_RANGES
        if (index1 > index2)
            errorf("Illegal range [%"PRIdMPINT"..%"PRIdMPINT
                   "] for assignment.\n", index1, index2-1
                 );
#endif /* NO_NEGATIVE_RANGES */

        if (ssize + index1 - index2 == 0)
        {
            /* <source> fits exactly into the target range */

            svalue_t *s, *d; /* Copy source and destination */

            s = sv->item;
            d = dv->item + index1;

            /* If there is just one ref to the source, use the faster
             * transfer instead of the slow assign for the copy.
             */
            if (sv->ref == 1)
            {
                for (i = ssize; --i >= 0; )
                {
                    transfer_svalue(d++, s++);
                }
                free_empty_vector(sv);
            }
            else /* sv->ref > 1 */
            {
                for (i = ssize; --i >= 0; )
                {
                    assign_svalue(d++, s++);
                }

                deref_array(sv);
                /* The if() above effectively did the 'free_svalue(source)' */
            }
        }
        else
        {
            /* Create a new vector */

            svalue_t *s, *d;  /* Copy source and destination */

            rv = allocate_array(dsize + ssize + index1 - index2);

            s = dv->item;
            d = rv->item;
            for (i = index1; --i >= 0; )
            {
                assign_svalue_no_free(d++, s++);
            }

            s = sv->item;
            for (i = ssize; --i >= 0; )
            {
                assign_svalue_no_free(d++, s++);
            }
            free_array(sv);

            s = dv->item + index2;
            for (i = dsize - index2; --i >= 0; )
            {
                assign_svalue_no_free(d++, s++);
            }

            dest->index2 = (int)(index1 + ssize);
            dest->vec.u.vec = rv;

            if (dest->var != NULL
             && dest->var->val.type == T_POINTER
             && dest->var->val.u.vec == dv)
            {
                /* The variable still points to
                 * the vector, so update it.
                 */
               dest->var->val.u.vec = ref_array(rv);
               free_array(dv);
            }

            free_array(dv); /* this can make the lvalue invalid to use */
        }
    }
    else
    {
        /* Not a pointer */
        free_svalue(source);
    }

} /* transfer_protected_pointer_range() */

/*-------------------------------------------------------------------------*/
static INLINE mp_int
get_updated_utf8_string_lvalue_index (mp_int lv_index, string_t *str, mp_int index1, mp_int index2, char *replacement, mp_int length)

/* Calculate the new value for the index <lv_index> into <str> before the text
 * between <index1> and <index2> is replaced by <length> bytes of <replacement>.
 * <str> is assumed to be of STRING_UTF8 type.
 */

{
    mp_int lv_chpos;
    bool error = false;

    if (lv_index >= index2)
        return index1 + length + lv_index - index2;

    if (lv_index <= index1)
        return lv_index;

    lv_chpos = byte_to_char_index(get_txt(str) + index1, lv_index - index1, &error);
    return index1 + char_to_byte_index(replacement, length, lv_chpos, &error);
} /* get_updated_utf8_string_lvalue_index() */

/*-------------------------------------------------------------------------*/
static INLINE mp_int
get_updated_byte_string_lvalue_index (mp_int lv_index, mp_int index1, mp_int index2, mp_int length)

/* Calculate the new value for the index <lv_index> into a string before the text
 * between <index1> and <index2> is replaced by <length> bytes of another string.
 * The strings are assumed to be of STRING_BYTES or STRING_ASCII type.
 */

{
    if (lv_index >= index2)
        return index1 + length + lv_index - index2;

    if (lv_index >= index1 + length)
        return index1 + length;

    return lv_index;
} /* get_updated_utf8_string_lvalue_index() */

/*-------------------------------------------------------------------------*/
static void
update_string_lvalue_indices (string_t *oldstr, string_t *newstr, mp_int index1, mp_int index2, char *replacement, mp_int length)

/* Updates all indices and references in the lvalues from <oldstr> to <newstr>,
 * where the text between <index1> and <index2> (byte offsets) is replaced
 * by <length> bytes of <replacement>. If <oldstr> and <newstr> is the same,
 * this function shall be called before the replacement.
 */

{
    /* while(), so we can jump out with break. */
    bool is_utf8 = (oldstr->info.unicode == STRING_UTF8 || newstr->info.unicode == STRING_UTF8);
    ph_int vartype = (oldstr->info.unicode == STRING_BYTES) ? T_BYTES : T_STRING;

    assert(oldstr->info.type == STRING_MUTABLE);
    assert(newstr->info.type == STRING_MUTABLE);

    /* Nothing to do, when the length didn't change
     * (and we don't have to deal with different encoded character lengths).
     */
    if (!is_utf8 && length == index2 - index1 && oldstr == newstr)
        return;

    ref_mstring(oldstr); /* Keep the string alive till we are finished. */

    for (struct protected_char_lvalue* lv = oldstr->u.mutable.char_lvalues;
         lv != NULL;
         lv = lv->next)
    {
        if (lv->str != oldstr)
            continue;

        if (is_utf8)
            lv->charp = get_txt(newstr) + get_updated_utf8_string_lvalue_index(lv->charp - get_txt(oldstr), oldstr, index1, index2, replacement, length);
        else
            lv->charp = get_txt(newstr) + get_updated_byte_string_lvalue_index(lv->charp - get_txt(oldstr), index1, index2, length);

        if (oldstr != newstr)
        {
            free_mstring(oldstr);
            lv->str = ref_mstring(newstr);

            if (lv->var != NULL
             && lv->var->val.type == vartype
             && lv->var->val.u.str == oldstr)
            {
                free_mstring(oldstr);
                lv->var->val.u.str = ref_mstring(newstr);
            }

            if (lv->next == NULL)
                lv->next = newstr->u.mutable.char_lvalues;
        }
    }

    for (struct protected_range_lvalue* lv = oldstr->u.mutable.range_lvalues;
         lv != NULL;
         lv = lv->next)
    {
        if (lv->vec.u.str != oldstr)
            continue;

        if (is_utf8)
        {
            lv->index1 = get_updated_utf8_string_lvalue_index(lv->index1, oldstr, index1, index2, replacement, length);
            lv->index2 = get_updated_utf8_string_lvalue_index(lv->index2, oldstr, index1, index2, replacement, length);
        }
        else
        {
            lv->index1 = get_updated_byte_string_lvalue_index(lv->index1, index1, index2, length);
            lv->index2 = get_updated_byte_string_lvalue_index(lv->index2, index1, index2, length);
        }

        if (oldstr != newstr)
        {
            free_mstring(oldstr);
            lv->vec.u.str = ref_mstring(newstr);

            if (lv->var != NULL
             && lv->var->val.type == vartype
             && lv->var->val.u.str == oldstr)
            {
                free_mstring(oldstr);
                lv->var->val.u.str = ref_mstring(newstr);
            }

            if (lv->next == NULL)
                lv->next = newstr->u.mutable.range_lvalues;
        }
    }

    if (oldstr != newstr)
    {
        newstr->u.mutable.char_lvalues = oldstr->u.mutable.char_lvalues;
        newstr->u.mutable.range_lvalues = oldstr->u.mutable.range_lvalues;
        oldstr->u.mutable.char_lvalues = NULL;
        oldstr->u.mutable.range_lvalues = NULL;
    }

    free_mstring(oldstr);

} /* update_string_lvalue_indices() */

/*-------------------------------------------------------------------------*/
static void
assign_string_range (svalue_t *source, Bool do_free)

/* Transfer the string <source> to the string range defined by
 * <current_unprotected_range>, modifying the target string therein
 * accordingly. If <do_free> is TRUE, <source> is freed once in the call.
 *
 * If <source> is not a string, it is just freed resp. ignored.
 */

{
    if (source->type == current_unprotected_range.vec->type)
    {
        svalue_t *dsvp;          /* destination svalue (from current_unprotected_range) */
        string_t *ds;            /* destination string (from dsvp) */
        string_t *ss;            /* source string (from source) */
        string_t *rs;            /* result string */
        mp_int dsize;            /* size of destination string */
        mp_int ssize;            /* size of source string */
        mp_int index1, index2;   /* range indices */
        bool free_ds = false;    /* Wether we need to free the reference to <ds>. */

        /* Set variables */
        index1 = current_unprotected_range.index1;
        index2 = current_unprotected_range.index2;
        dsvp = current_unprotected_range.vec;
        ds = dsvp->u.str;
        ss = source->u.str;
        dsize = (mp_int)mstrsize(ds);
        ssize = (mp_int)mstrsize(ss);

#ifdef NO_NEGATIVE_RANGES
        if (index1 > index2)
            errorf("Illegal range [%"PRIdMPINT"..%"PRIdMPINT
                   "] for assignment.\n", index1, index2-1
                 );
#endif /* NO_NEGATIVE_RANGES */

        if (ssize != index2 - index1)
        {
            /* Create the new string */
            rs = alloc_mstring((size_t)(dsize + ssize + index1 - index2));
            if (rs)
                rs = make_mutable(rs);
            if (!rs)
            {
                /* We don't pop the stack here --> don't free source */
                outofmem((dsize + ssize + index1 - index2), "new string");
            }

            if (index1)
                memcpy(get_txt(rs), get_txt(ds), (size_t)index1);

            if (dsize > index2)
                memcpy( get_txt(rs) + index1 + ssize, get_txt(ds) + index2
                      , (size_t)(dsize - index2));

            /* Assign the new string in place of the old */
            dsvp->u.str = rs;
            /* Free <ds> later, because we need it one more time. */
            free_ds = true;

            /* Update the original variable. */
            if (current_unprotected_range.var != NULL
             && current_unprotected_range.var->val.type == source->type
             && current_unprotected_range.var->val.u.str == ds)
            {
                free_mstring(ds);
                current_unprotected_range.var->val.u.str = ref_mstring(rs);
            }
        }
        else
            rs = ds;

        update_string_lvalue_indices(ds, rs, index1, index2, get_txt(ss), ssize);
        if (free_ds)
            free_mstring(ds);

        if (ssize)
            memcpy(get_txt(rs) + index1, get_txt(ss), (size_t)ssize);

        if (ss->info.unicode != STRING_ASCII)
            rs->info.unicode = ss->info.unicode;
        else
            rs->info.unicode = is_ascii(get_txt(rs), mstrsize(rs)) ? STRING_ASCII : STRING_UTF8;

        if (do_free)
            free_string_svalue(source);
    }
    else
    {
        /* Not a string: just free it */
        if (do_free)
            free_svalue(source);
    }
} /* assign_string_range() */

/*-------------------------------------------------------------------------*/
static void
assign_protected_string_range ( struct protected_range_lvalue *dest
                              , svalue_t *source
                              , Bool do_free
                              )

/* Transfer the string <source> to the string range defined by
 * <dest>, modifying the target string in dest
 * accordingly.
 *
 * If <do_free> is TRUE, <source> and the protector <dest> are freed once
 * in the call.
 *
 * If <source> is not a string, it is just freed resp. ignored.
 */

{
    if (source->type == dest->vec.type)
    {
        string_t *ds;            /* destination string (from dsvp) */
        string_t *ss;            /* source string (from source) */
        string_t *rs;            /* result string */
        mp_int dsize;            /* size of destination string */
        mp_int ssize;            /* size of source string */
        mp_int index1, index2;   /* range indices */

        assert(dest->vec.type == T_STRING || dest->vec.type == T_BYTES);

        /* Set variables */
        index1 = dest->index1;
        index2 = dest->index2;
        ds = dest->vec.u.str;
        ss = source->u.str;
        dsize = (mp_int)mstrsize(ds);
        ssize = (mp_int)mstrsize(ss);

#ifdef NO_NEGATIVE_RANGES
        if (index1 > index2)
            errorf("Illegal range [%"PRIdMPINT"..%"PRIdMPINT
                   "] for assignment.\n", index1, index2-1
                 );
#endif /* NO_NEGATIVE_RANGES */

        if (ssize != index2 - index1)
        {
            /* Create a new string */
            size_t rsize = dsize + ssize + index1 - index2;
            rs = alloc_mstring(rsize);
            if (rs)
                rs = make_mutable(rs);
            if (!rs)
            {
                outofmem(rsize, "new string");
            }

            if (index1)
                memcpy(get_txt(rs), get_txt(ds), (size_t)index1);

            if (dsize > index2)
                memcpy( get_txt(rs) + index1 + ssize, get_txt(ds) + index2
                      , (size_t)(dsize - index2));
        }
        else
            rs = ref_mstring(ds);

        update_string_lvalue_indices(ds, rs, index1, index2, get_txt(ss), ssize);

        if (ssize)
            memcpy(get_txt(rs) + index1, get_txt(ss), (size_t)ssize);

        if (ss->info.unicode != STRING_ASCII)
            rs->info.unicode = ss->info.unicode;
        else
            rs->info.unicode = is_ascii(get_txt(rs), mstrsize(rs)) ? STRING_ASCII : STRING_UTF8;

        free_mstring(rs);

        if (do_free)
            free_string_svalue(source);
    }
    else
    {
        /* Not a string: just free it */
        if (do_free)
            free_svalue(source);
    }
} /* transfer_protected_string_range() */

/*-------------------------------------------------------------------------*/
static p_int
read_unprotected_char ()

/* Read the character pointed to by <current_unprotected_char>.
 */

{
    string_t *str = current_unprotected_char.str;

    if (str->info.unicode == STRING_UTF8)
    {
        p_int ch = 0;
        ssize_t result = utf8_to_unicode(current_unprotected_char.charp, get_txt(str) + mstrsize(str) - current_unprotected_char.charp, &ch);

        if (!result)
            errorf("Invalid character in string.\n");

        return ch;
    }
    else
        return *(unsigned char*)current_unprotected_char.charp;
} /* read_unprotected_char() */

/*-------------------------------------------------------------------------*/
static p_int
read_protected_char (struct protected_char_lvalue *src)

/* Read the character pointed to by <src>.
 */

{
    if (src->str->info.unicode == STRING_UTF8)
    {
        p_int ch = 0;
        ssize_t result = utf8_to_unicode(src->charp, get_txt(src->str) + mstrsize(src->str) - src->charp, &ch);
        if (!result)
            errorf("Invalid character in string.\n");

        return ch;
    }
    else
        return *(unsigned char*)src->charp;
} /* read_protected_char() */

/*-------------------------------------------------------------------------*/
static p_int
assign_unprotected_char (p_int ch)

/* Change the character pointed to by <current_unprotected_char> to <ch>,
 * modifying the target string accordingly. Returns the value actually
 * written.
 */

{
    string_t *ds;               /* destination string (from dsvp) */

    ds = current_unprotected_char.str;
    if (ds->info.unicode == STRING_BYTES ||
        (ds->info.unicode == STRING_ASCII && ch >= 0 && ch < 128))
    {
        /* This is easy, just change the byte. */
        *current_unprotected_char.charp = ch;
        return *(unsigned char*)current_unprotected_char.charp;
    }
    else
    {
        char buf[4];            /* buffer for the encoded character. */
        string_t *rs;           /* result string */

        size_t offset;          /* byte offset or the character in the string. */
        size_t dsize;           /* size of the source string. */
        size_t osize;           /* size of the replaced character. */
        size_t nsize;           /* size of the new character. */
        bool error = false;     /* error flag when reading. */
        bool free_ds = false;   /* Wether we need to free the reference to <ds>. */

        offset = current_unprotected_char.charp - get_txt(ds);
        dsize = mstrsize(ds);
        osize = char_to_byte_index(current_unprotected_char.charp, dsize - offset, 1, &error);
        nsize = unicode_to_utf8(ch, buf);

        if (nsize != osize)
        {
            /* Create the new string */
            rs = alloc_mstring(dsize + nsize - osize);
            if (rs)
                rs = make_mutable(rs);
            if (!rs)
            {
                /* We don't pop the stack here --> don't free source */
                outofmem((dsize + nsize - osize), "new string");
            }

            if (offset)
                memcpy(get_txt(rs), get_txt(ds), offset);

            if (dsize > offset + osize)
                memcpy(get_txt(rs) + offset + nsize, get_txt(ds) + offset + osize, dsize - offset - osize);

            if (nsize > 1)
                rs->info.unicode = STRING_UTF8;
            else
                rs->info.unicode = (is_ascii(get_txt(rs), offset) && is_ascii(get_txt(rs) + offset + nsize, dsize - offset - osize)) ? STRING_ASCII : STRING_UTF8;

            current_unprotected_char.str = rs;

            /* Update the original variable. */
            if (current_unprotected_char.var != NULL
             && current_unprotected_char.var->val.type == ((rs->info.unicode == STRING_BYTES) ? T_BYTES : T_STRING)
             && current_unprotected_char.var->val.u.str == ds)
            {
                free_ds = true;
                current_unprotected_char.var->val.u.str = rs;
            }
            else
            {
                /* Keep the string alive. */
                free_svalue(&indexing_quickfix);
                if (rs->info.unicode == STRING_BYTES)
                    put_bytes(&indexing_quickfix, rs);
                else
                    put_string(&indexing_quickfix, rs);
            }
        }
        else
            rs = ds;

        update_string_lvalue_indices(ds, rs, offset, offset + osize, buf, nsize);
        if (free_ds)
            free_mstring(ds);

        if (nsize)
            memcpy(get_txt(rs) + offset, buf, nsize);

        if (!osize)
            return 0;

        return ch;
    }
} /* assign_unprotected_char() */

/*-------------------------------------------------------------------------*/
static p_int
assign_protected_char (struct protected_char_lvalue *dest, p_int ch)

/* Change the character pointed to by <dest> to <ch>,
 * modifying the target string accordingly.
 */

{
    string_t *ds;               /* destination string (from dest) */

    ds = dest->str;

    if (ds->info.unicode == STRING_BYTES ||
        (ds->info.unicode == STRING_ASCII && ch >= 0 && ch < 128))
    {
        /* This is easy, just change the byte. */
        *dest->charp = ch;
        return *(unsigned char*)dest->charp;
    }
    else
    {
        char buf[4];            /* buffer for the encoded character. */
        string_t *rs;           /* result string */

        size_t offset;          /* byte offset or the character in the string. */
        size_t dsize;           /* size of the source string. */
        size_t osize;           /* size of the replaced character. */
        size_t nsize;           /* size of the new character. */
        bool error = false;     /* error flag when reading. */

        offset = dest->charp - get_txt(ds);
        dsize = mstrsize(ds);
        osize = char_to_byte_index(dest->charp, dsize - offset, 1, &error);
        nsize = unicode_to_utf8(ch, buf);

        if (nsize != osize)
        {
            /* Create the new string */
            rs = alloc_mstring(dsize + nsize - osize);
            if (rs)
                rs = make_mutable(rs);
            if (!rs)
            {
                /* We don't pop the stack here --> don't free source */
                outofmem((dsize + nsize - osize), "new string");
            }

            if (offset)
                memcpy(get_txt(rs), get_txt(ds), offset);

            if (dsize > offset + osize)
                memcpy(get_txt(rs) + offset + nsize, get_txt(ds) + offset + osize, dsize - offset - osize);

            if (nsize > 1)
                rs->info.unicode = STRING_UTF8;
            else
                rs->info.unicode = (is_ascii(get_txt(rs), offset) && is_ascii(get_txt(rs) + offset + nsize, dsize - offset - osize)) ? STRING_ASCII : STRING_UTF8;
        }
        else
            rs = ref_mstring(ds);

        update_string_lvalue_indices(ds, rs, offset, offset + osize, buf, nsize);

        if (nsize)
            memcpy(get_txt(rs) + offset, buf, nsize);

        free_mstring(rs);

        if (!osize)
            return 0;

        return ch;
    }
} /* assign_protected_char() */
/*-------------------------------------------------------------------------*/
static void
add_number_to_lvalue (char* op, svalue_t *dest, int i, svalue_t *pre, svalue_t *post)

/* Add the number <i> to the (PROTECTED_)LVALUE <dest>.
 * If <pre> is not null, the <dest> value before the addition is copied
 * into it.
 * If <post> is not null, the <dest> value after the addition is copied
 * into it.
 * Both <pre> and <post> are supposed to be empty svalues when given.
 *
 * If <dest> is of the wrong type, an error is generated.
 *
 * <op> is the name of the operation for the error messages.
 */

{
    /* Handle the lvalue. */
    if (dest->type != T_LVALUE)
    {
        fatal("(add_number_to_lvalue) Illegal svalue %p type %d\n", dest, dest->type);
        /* NOTREACHED */
        return;
    }

    do
    {
        switch (dest->x.lvalue_type)
        {
            default:
                errorf("Bad arg to %s: got '%s', expected numeric type.\n", op, typename(dest->type));
                break;

            case LVALUE_UNPROTECTED:
                dest = dest->u.lvalue;
                break;

            case LVALUE_UNPROTECTED_CHAR:
            {
                p_int val = read_unprotected_char();
                if (pre) put_number(pre, val);

                val = assign_unprotected_char(val + i);

                if (post) put_number(post, val);
                return;
            }

            case LVALUE_UNPROTECTED_MAPENTRY:
                dest = get_map_lvalue(current_unprotected_mapentry.map, &(current_unprotected_mapentry.key)) + current_unprotected_mapentry.index;
                free_svalue(&(current_unprotected_mapentry.key));
                break;

            case LVALUE_PROTECTED:
                dest = &(dest->u.protected_lvalue->val);
                break;

            case LVALUE_PROTECTED_CHAR:
            {
                struct protected_char_lvalue* lv = dest->u.protected_char_lvalue;

                p_int val = read_protected_char(lv);
                if (pre) put_number(pre, val);

                val = assign_protected_char(lv, val + i);

                if (post) put_number(post, val);
                return;
            }

            case LVALUE_PROTECTED_MAPENTRY:
            {
                struct protected_mapentry_lvalue *e = dest->u.protected_mapentry_lvalue;
                dest = get_map_lvalue(e->map, &(e->key)) + e->index;
                continue;
            }
        } /* switch() */

        break;

    } while (dest->type == T_LVALUE);

    /* Now increment the non-LVALUE */
    switch (dest->type)
    {
    default:
        errorf("Bad arg to %s: got '%s', expected numeric type.\n", op, typename(dest->type));
        break;

    case T_NUMBER:
        if ((i>0)
           ?(dest->u.number > PINT_MAX-i)
           :(dest->u.number < PINT_MIN-i))
        {
            errorf("Numeric overflow: (%"PRIdPINT")%s\n",
                   dest->u.number, op);
            /* NOTREACHED */
            break;
        }

        if (pre) put_number(pre, dest->u.number);
        dest->u.number += i;
        if (post) put_number(post, dest->u.number);
        break;

    case T_FLOAT:
      {
        STORE_DOUBLE_USED
        double d;

        d = READ_DOUBLE(dest);

        if (pre)
        {
            pre->type = T_FLOAT;
            STORE_DOUBLE(pre, d);
        }

        d += (double)i;
        if (d < (-DBL_MAX) || d > DBL_MAX)
            errorf("Numeric overflow: (%g)%s\n", READ_DOUBLE(dest), op);

        STORE_DOUBLE(dest, d);

        if (post)
        {
            post->type = T_FLOAT;
            STORE_DOUBLE(post, d);
        }
        break;
      }
    } /* switch() */
} /* add_number_to_lvalue() */

/*-------------------------------------------------------------------------*/
static vector_t *
inter_add_array (vector_t *q, vector_t **vpp)

/* Append array <q> to array *<vpp>. Both <q> and *<vpp> are freed,
 * the result vector (just one ref) is assigned to *<vpp> and also returned.
 *
 * <inter_sp> is supposed to point at the two vectors and will be decremented
 * by 2.
 */

{
    vector_t *p;       /* The second summand vector */
    mp_int cnt;
    vector_t *r;       /* Result vector */
    svalue_t *s, *d;   /* Pointers for copying: src and dest */
    size_t p_size, q_size;  /* Sizes of p and q */

    p = *vpp;

    /* *vpp could be in the summands, thus don't free p / q before
     * assigning.
     * On the other hand, with an uninitialized array, we musn't assign
     * before the copying is done.
     */

    p_size = VEC_SIZE(p);
    q_size = VEC_SIZE(q);
    s = p->item;

    /* Check the result size for legality - this leaves the code below
     * to deal just with out of memory conditions.
     */

    if (max_array_size && p_size + q_size > max_array_size)
    {
        errorf("Illegal array size: %zu.\n", (p_size + q_size));
    }

    /* The optimized array-adding will transfer elements around, rendering
     * the arrays on the stack inconsistent. Thus any out-of-memory
     * error must not attempt to free them - leaking them is the lesser
     * evil in this situation.
     */
    inter_sp -= 2;

    /* Out of memory might result in some memory leaks. Better that freeing
     * arrays with 0 ref count, or indigestion in garbage_collection() .
     * It will simply give some more debugging output...
     */

    /* Allocate the result vector and copy p into it.
     */
    if (!(p->ref-1))
    {
        /* p will be deallocated completely - try to optimize a bit */

        /* We try to expand the existing memory for p (without moving)
         * instead of allocating a completely new vector.
         */
        d = malloc_increment_size(p, q_size * sizeof(svalue_t));
        if ( NULL != d)
        {
            /* We got the additional memory */
            r = p;
            r->ref = 1;
            r->size = p_size + q_size;

            r->user->size_array -= p_size;
            r->user = current_object->user;
            r->user->size_array += p_size + q_size;
        } else
        /* Just allocate a new vector and memcopy p into it. */
        {
            r = allocate_uninit_array((p_int)(p_size + q_size));
            deref_array(p);
            d = r->item;
            for (cnt = (mp_int)p_size; --cnt >= 0; )
            {
                *d++ = *s++;
            }
        }
    }
    else
    {
        /* p must survive: allocate a new vector and assign the values
         * from p.
         */
        r = allocate_uninit_array((p_int)(p_size + q_size));
        deref_array(p);
        d = r->item;
        for (cnt = (mp_int)p_size; --cnt >= 0; ) {
            assign_rvalue_no_free (d++, s++);
        }
    }

    /* Here 'd' points to the first item to set */

    /* Add the values from q. Again, try to optimize */
    s = q->item;
    if (q->ref == 1)
    {
        for (cnt = (mp_int)q_size; --cnt >= 0; )
        {
            if (destructed_object_ref(s))
            {
                assign_svalue(s, &const0);
            }
            *d++ = *s++;
        }
        *vpp = r;
        free_empty_vector(q);
    }
    else /* q->ref > 1 */
    {
        for (cnt = (mp_int)q_size; --cnt >= 0; ) {
            assign_rvalue_no_free (d++, s++);
        }
        *vpp = r;

        deref_array(q);
    }

    if (!p->ref && p != q)
        free_empty_vector(p);

    return r;
} /* inter_add_array() */


/*=========================================================================*/

/*                           S T A C K                                     */

/*-------------------------------------------------------------------------*/
/* The following functions handle the pushing and popping of the
 * interpreter stack. Often functions appear in two versions: one version
 * using the global variable <inter_sp>, the other version receiving and
 * returning the old/new stack pointer as argument and result.
 *
 * Obviously, the former version can be easily called from outside the
 * interpreter, while the latter allows better optimization.
 *
 * To make things even more complicated, some of the 'slower' functions
 * are redefined with preprocessor macros to use the faster function - this
 * is meant to make the code in this module faster, but relies on certain
 * naming conventions (e.g. that 'sp' is always the local copy of the
 * stack pointer).
 *
 * TODO: Streamline the functions, given them macros as fast alternative
 * TODO:: publish them all in interpret.h and enforce their use.
 *-------------------------------------------------------------------------
 * The functions are:
 *
 * put_c_string (sp, p)
 *     Convert the C-String <p> into a mstring and put it into <sp>.
 * push_svalue(v), push_svalue_block(num,v):
 *     Push one or more svalues onto the stack.
 * push_rvalue(v):
 *     Push one rvalue onto the stack.
 * pop_stack(), _drop_n_elems(n,sp):
 *     Pop (free) elements from the stack.
 * stack_overflow(sp,fp,pc):
 *     Handle a stack overflow.
 * push_referenced_mapping(m):
 *     Push a mapping onto the stack.
 * push_error_handler(h)
 *     Push an errorhandler entry onto the stack.
 */

/*-------------------------------------------------------------------------*/
void
put_c_string (svalue_t *sp, const char *p)

/* Put a copy of the C string *<p> into <sp>.
 */

{
    string_t * str;

    memsafe(str = new_unicode_mstring(p), strlen(p), "string");
    put_string(sp, str);
} /* put_c_string() */

/*-------------------------------------------------------------------------*/
void
put_c_n_string (svalue_t *sp, const char *p, size_t len)

/* Put a copy of first <len> characters of the C string *<p> into <sp>.
 */

{
    string_t * str;

    memsafe(str = new_n_unicode_mstring(p, len), len, "string");
    put_string(sp, str);
} /* put_c_n_string() */

/*-------------------------------------------------------------------------*/
void
put_bytes_buf (svalue_t *sp, const void *p, size_t len)

/* Put a copy of first <len> bytes of the buffer <*p> into <sp>.
 */

{
    string_t * str;

    memsafe(str = new_n_mstring((const char*)p, len, STRING_BYTES), len, "bytes");
    put_bytes(sp, str);
} /* put_bytes_buf() */

/*-------------------------------------------------------------------------*/
void
push_svalue (svalue_t *v)

/* Push the svalue <v> onto the stack as defined by <inter_sp>.
 * Same semantic as assign_svalue_no_free().
 */

{
    assign_svalue_no_free(++inter_sp, v);
}

/*-------------------------------------------------------------------------*/
void
push_rvalue (svalue_t *v)

/* Push the svalue <v> as an rvalue onto the stack as defined by <inter_sp>.
 * Same semantic as assign_rvalue_no_free().
 */

{
    assign_rvalue_no_free(++inter_sp, v);
}

/*-------------------------------------------------------------------------*/
void
push_svalue_block (int num, svalue_t *v)

/* Push all <num> svalues starting at <v> onto the stack as defined by
 * <inter_sp>. Same semantic as assign_svalue_no_free().
 */

{
    svalue_t *w;

    for (w = inter_sp; --num >= 0; v++)
    {
        w++;
        assign_svalue_no_free(w, v);
    }
    inter_sp = w;
}

/*-------------------------------------------------------------------------*/
static INLINE void
_pop_stack (void)

/* Pop the topmost element from the stack as defined by <inter_sp>,
 * using free_svalue().
 */

{
#ifdef DEBUG
    if (inter_sp < VALUE_STACK)
        fatal("VM Stack underflow: %"PRIdMPINT" too low.\n", 
              (mp_int)(VALUE_STACK - inter_sp));
#endif
    free_svalue(inter_sp--);
}

void pop_stack (void) { _pop_stack(); }

/*-------------------------------------------------------------------------*/
static INLINE svalue_t *
_pop_n_elems (int n, svalue_t *sp)

/* Pop the <n> topmost elements from the stack, currently ending at <sp>,
 * and return the new stackpointer.
 * The elements are freed using free_svalue().
 */

{
#ifdef DEBUG
    if (n < 0)
        fatal("pop_n_elems: %d elements.\n", n);
#endif
    for (; --n >= 0; )
    {
        free_svalue(sp--);
    }
    return sp;
}

svalue_t * pop_n_elems (int n, svalue_t *sp)
{ return _pop_n_elems(n, sp); }

/*-------------------------------------------------------------------------*/
static void stack_overflow (svalue_t *sp, svalue_t *fp, bytecode_p pc)
                              NORETURN;
static void
stack_overflow (svalue_t *sp, svalue_t *fp, bytecode_p pc)

/* Recover from a stack overflow by popping all the elements between the
 * current stack end <sp> and the begin of the frame <fp>.
 * The function then assigns the new <sp> == <fp> and the <pc> to the
 * corresponding inter_xx variables and generates an error.
 */

{
    if (sp >= &VALUE_STACK[SIZEOF_STACK])
        fatal("Fatal stack overflow: %"PRIdMPINT" too high.\n"
             , (mp_int)(sp - &VALUE_STACK[SIZEOF_STACK])
             );
    sp = _pop_n_elems(sp-fp, sp);
    ERROR("stack overflow\n");
}

/*-------------------------------------------------------------------------*/
void
push_referenced_mapping (mapping_t *m)

/* Push mapping <m> onto the stack as defined by <inter_sp>.
 * The refs of <m> are _not_ incremented.
 */

{
    inter_sp++;
    put_mapping(inter_sp, m);
}

/*-------------------------------------------------------------------------*/
svalue_t *
push_error_handler(void (*errorhandler)(error_handler_t *), error_handler_t *arg)

/* Push the <errorhandler>() with the argument <arg> as error handler
 * onto the stack.
 * This means that a new T_ERROR_HANDLER is created on the stack, pointing
 * to <arg>. <arg> is used to store a pointer to <errorhandler>.
 * Returns new inter_sp.
 */

{
    arg->fun = errorhandler;

    inter_sp++;
    inter_sp->type = T_ERROR_HANDLER;
    inter_sp->u.error_handler = arg;
    return inter_sp;
} /* push_error_handler() */

/*-------------------------------------------------------------------------*/
/* Fast version of several functions, must come last so to not disturb
 * the actual definitions:
 */

#define pop_stack()             free_svalue(sp--)
#define pop_n_elems(n)          (sp = _pop_n_elems((n), sp))

/*=========================================================================*/

/*                          I N D E X I N G                                */

/*-------------------------------------------------------------------------*/
/* The following functions are concerned with the indexing of single
 * elements and ranges of strings, vectors and mappings, both as rvalue
 * and lvalue.
 *
 * Most of the functions are just the implementations of the corresponding
 * machine operators and are called just from the interpreter switch().
 * The actual arguments are pulled from the vm stack and the results pushed;
 * the functions receive the current stackpointer and programcounter as
 * function call parameters. The program counter is usally only used to
 * update <inter_pc> in case of errors. Result of the call is the new
 * stackpointer pointing to the result on the machine stack.
 *
 * The layout is the same for all function. The topmost element on the
 * stack are the indices. Below that is the indexed value. The later
 * can be a protected lvalue. The function differ in the type of index
 * (normal, range, map) and whether they shall return an lvalue or
 * rvalue.
 *-------------------------------------------------------------------------
 * The functions (in a LPCish notation) are:
 *
 *   push_index_lvalue(vector|string|mapping|struct v, int|mixed i)
 *     Return &(v[i]), unprotected. (Supports all single-index
 *     types: &(v[<i]), &(v[>i]), &(s->i).)
 *
 *   push_map_index_lvalue(mapping m, mixed key, int i)
 *     Return &(m[key,i]), unprotected.
 *
 *   push_range_lvalue(vector|string v, int i1, int i2)
 *     Return &(v[i1..i2]), unprotected, using current_unprotected_range.
 *     Supports all range-index types.
 *
 *   push_index_value(vector|string|mapping|struct v, int|mixed i)
 *     Return v[i]
 *
 *   push_range_value(vector|string v, int i1, int i2)
 *     Return v[i1..i2]
 */

enum index_type { REGULAR_INDEX, REVERSE_INDEX, ARITHMETIC_INDEX };

/*-------------------------------------------------------------------------*/
static INLINE svalue_t *
get_vector_item_extended (vector_t * vec, mp_int size, mp_int offset, svalue_t * i, svalue_t *sp, bytecode_p pc, enum index_type itype)

/* Index vector <vec> starting at <offset> and having size <size>
 * with index <i> and return the pointer to the indexed item.
 * If the index is invalid, throw an error.
 */

{
    p_int ind;
    svalue_t * item;

    if (i->type != T_NUMBER)
    {
        ERRORF(("Illegal index for []: got %s, expected number.\n"
               , typename(i->type)
               ));
        return NULL;
    }

    ind = i->u.number;
    switch (itype)
    {
        case REGULAR_INDEX:
            if (ind < 0)
            {
                ERROR("Illegal index for []: not a positive number.\n");
                /* NOTREACHED */
                return NULL;
            }
            if (ind >= size)
            {
                ERRORF(("Index for [] out of bounds: %"PRIdPINT
                        ", vector size: %"PRIdPINT"\n"
                       , ind, size));
                /* NOTREACHED */
                return NULL;
            }
            break;

        case REVERSE_INDEX:
            if (ind < 0)
            {
                ERROR("Illegal index for [<]: not a positive number.\n");
                return NULL;
            }

            if (ind > size || ind <= 0)
            {
                ERRORF(("Index out of bounds for [<]: %"PRIdPINT", vector size: %"
                        PRIdPINT".\n", i->u.number, size));
                return NULL;
            }

            ind = size - ind;
            break;

        case ARITHMETIC_INDEX:
            if (ind < 0)
                ind = size + ind;

            if (ind < 0 || ind >= size)
            {
                ERRORF(("Index out of bounds for [>]: %"PRIdPINT", vector size: %"
                        PRIdPINT".\n"
                       , i->u.number, size));
                return NULL;
            }
            break;
    }

    /* Compute the indexed element */
    item = &vec->item[ind + offset];
    if (destructed_object_ref(item))
    {
        free_svalue(item);
        put_number(item, 0);
    }

    return item;
} /* get_vector_item_extended() */

/*-------------------------------------------------------------------------*/
static INLINE svalue_t *
get_vector_item (vector_t * vec, svalue_t * i, svalue_t *sp, bytecode_p pc, enum index_type itype)

/* Index vector <vec> with index <i> and return the pointer
 * to the indexed item.
 * If the index is invalid, throw an error.
 */
{
    return get_vector_item_extended(vec, VEC_SIZE(vec), 0, i, sp, pc, itype);
} /* get_vector_item() */

/*-------------------------------------------------------------------------*/
static INLINE svalue_t *
get_vector_range_item (vector_t * vec, mp_int index1, mp_int index2, svalue_t * i, svalue_t *sp, bytecode_p pc, enum index_type itype)

/* Index the vector range <vec>[<index1>..<index2>] with index <i> and
 * return the pointer to the indexed item.
 * If the index is invalid, throw an error.
 */

{
    return get_vector_item_extended(vec, index2 - index1, index1, i, sp, pc, itype);
} /* get_vector_range_item() */

/*-------------------------------------------------------------------------*/
static INLINE char *
get_string_item_extended ( svalue_t * svp, mp_int size, mp_int offset
                         , svalue_t * i, Bool make_mutable
                         , Bool allow_one_past
                         , svalue_t *sp, bytecode_p pc
                         , enum index_type itype)

/* Index string <svp> starting at <offset> bytes and having size <size> bytes
 * with index <i> and return the pointer to the indexed item.
 * If <make_mutable> is TRUE, <svp> is made a mutable string
 * with just one reference.
 * If <allow_one_past> is TRUE, indexing one past the official end
 * of the string for retrieval purposes is ok. TODO: Remove this.
 * If the index is invalid, throw an error.
 */

{
    static char nullchar = 0; /* We use that for allowed index at the end. */
    mp_int ind, length;

    if (i->type != T_NUMBER)
    {
        ERRORF(("Illegal index for []: got %s, expected number.\n"
               , typename(i->type)
               ));
        return NULL;
    }
    ind = i->u.number;

    /* For UTF8 strings determine the real length. */
    if (svp->u.str->info.unicode == STRING_UTF8)
    {
        bool error = false;
        length = byte_to_char_index(get_txt(svp->u.str) + offset, size, &error);
        if (error)
            ERRORF(("Invalid character in string at index %zd.\n", length));
    }
    else
        length = size;

    switch (itype)
    {
        case REGULAR_INDEX:
            if (ind < 0)
            {
                ERROR("Illegal index for []: not a positive number.\n");
                /* NOTREACHED */
                return NULL;
            }
            if (ind > length )
            {
                ERRORF(("Index out of bounds for []: %"PRIdMPINT
                        ", string length: %zu.\n"
                       , ind, length));
                /* NOTREACHED */
                return NULL;
            }
            if (ind == length)
            {
                if (!allow_one_past)
                {
                    ERRORF(("Index out of bounds for []: %"PRIdMPINT
                            ", string length: %zu.\n"
                           , ind, length));
                    return NULL;
                }
                else if (!runtime_no_warn_deprecated)
                {
                    warnf( "Warning: Indexing past string end is deprecated: "
                           "index %"PRIdMPINT", string length: %zu.\n"
                         , ind, length
                         );
                    return &nullchar;
                }
            }
            break;

        case REVERSE_INDEX:
            if (ind < 0)
            {
                ERROR("Illegal index for [<]: not a positive number.\n");
                return NULL;
            }

            /* Compute the real index. Allow ""[<1]. */
            ind = length - ind;
            if (!length && ind == -1)
                ind = 0;

            if ( ind < 0
             ||  ind > length
               )
            {
                ERRORF(("Index out of bounds for [<]: %"PRIdPINT
                        ", string length: %zu\n"
                       , i->u.number, length));
                return NULL;
            }

            if (ind == length)
            {
                if (!allow_one_past)
                {
                    ERRORF(("Index out of bounds for [<]: %"PRIdMPINT
                            ", string length: %zu.\n"
                           , ind, length));
                    return NULL;
                }
                else if (!runtime_no_warn_deprecated)
                {
                    warnf( "Warning: Indexing past string end is deprecated: "
                           "index %"PRIdMPINT", string length: %zu.\n"
                         , ind, length
                         );
                    return &nullchar;
                }
            }
            break;

        case ARITHMETIC_INDEX:
            if (0 > ind)
            {
                /* Compute the real index. Allow ""[<1]. */
                ind = length + ind;
                if (!length && ind == -1)
                    ind = 0;
            }
            if (ind < 0 || ind > length)
            {
                ERRORF(("Index out of bounds for [>]: %"PRIdPINT
                        ", string length: %zu\n"
                       , i->u.number, length));
                return NULL;
            }

            if (ind == length)
            {
                if (!allow_one_past)
                {
                    ERRORF(("Index out of bounds for [>]: %"PRIdMPINT
                            ", string length: %zu.\n"
                           , ind, length));
                    return NULL;
                }
                else if (!runtime_no_warn_deprecated)
                {
                    warnf( "Warning: Indexing past string end is deprecated: "
                           "index %"PRIdMPINT", string length: %zu.\n"
                         , ind, length
                         );
                    return &nullchar;
                }
            }
            break;
    }

    /* The basic idea here was to to create a new copy of the string only
     * if the string is not singular (aka !mstr_singular(svp->u.str)).
     * Unfortunately local variable lvalues are pushed without counting
     * the additional reference, so we now have to play it safe and
     * duplicate the string whenever requested.
     */
    if (make_mutable)
    {
        string_t *p;

        memsafe(p = make_mutable(svp->u.str), mstrsize(svp->u.str)
               , "modifiable string");
        svp->u.str = p;
    }

    /* And now convert the character index back to byte index. */
    if (svp->u.str->info.unicode == STRING_UTF8)
    {
        bool error = false;
        ind = char_to_byte_index(get_txt(svp->u.str) + offset, size, ind, &error);
        if (error)
            ERRORF(("Invalid character in string at byte %zd.\n", ind));
    }

    return &(get_txt(svp->u.str)[ind + offset]);
} /* get_string_item_extended() */

/*-------------------------------------------------------------------------*/
static INLINE char *
get_string_item ( svalue_t * svp, svalue_t * i, Bool make_mutable
                , Bool allow_one_past
                , svalue_t *sp, bytecode_p pc
                , enum index_type itype)

/* Index string <svp> with index <i> and return the pointer to the
 * indexed item.
 * If <make_mutable> is TRUE, <svp> is made an untabled string
 * with just one reference.
 * If <allow_one_past> is TRUE, indexing one past the official end
 * of the string for retrieval purposes is ok. TODO: Remove this.
 * If the index is invalid, throw an error.
 */

{
    return get_string_item_extended(svp, (mp_int)mstrsize(svp->u.str), 0,
        i, make_mutable, allow_one_past, sp, pc, itype);
} /* get_string_item() */

/*-------------------------------------------------------------------------*/
static INLINE char *
get_string_range_item ( svalue_t * svp, mp_int index1, mp_int index2
                      , svalue_t * i, Bool make_mutable
                      , Bool allow_one_past
                      , svalue_t *sp, bytecode_p pc
                      , enum index_type itype)

/* Index string range <svp>[<index1>..<index2>] and
 * return the pointer to the indexed item.
 * If <make_mutable> is TRUE, <svp> is made an untabled string
 * with just one reference.
 * If <allow_one_past> is TRUE, indexing one past the official end
 * of the string for retrieval purposes is ok. TODO: Remove this.
 * If the index is invalid, throw an error.
 */

{
    return get_string_item_extended(svp, index2 - index1, index1,
        i, make_mutable, allow_one_past, sp, pc, itype);
} /* get_string_range_item() */

/*-------------------------------------------------------------------------*/
static INLINE svalue_t *
check_struct_op (svalue_t * sp, bytecode_p pc, bool * ignore_error)

/* On the stack are the arguments for a struct indexing operation.
 * In particular: sp[0]:  the struct type index <idx>
 *                sp[-2]: the struct value to idx.
 *                sp[-1]: the member index
 *
 * Check the validity of the indexing operation and thrown an error
 * if invalid.
 *
 * <idx> gives the index of the expected struct type - the
 * operator accepts a struct of this type, or any of its children.
 * An negative <idx> accepts any struct.
 *
 * On success, the <idx> svalue is removed from the stack and the
 * new stack pointer is returned. <ignore_error> is set to true,
 * when the operation was changed to a runtime lookup due to a
 * different struct definition with the same name (and thus read
 * access to non-existing members shall return 0 instead of an error).
 */

{
    short s_index;
    svalue_t * svp, * isvp;

    if (ignore_error)
        *ignore_error = false;

    isvp = get_rvalue(sp, NULL);

    /* These two errors can happen with careless funcall(#'->)s */
    if (!isvp || isvp->type != T_NUMBER)
        ERRORF(("Illegal struct index value: %s, expected a number.\n"
               , typename((isvp ? isvp : sp)->type)
              ));
    if (isvp->u.number >= 0
     && isvp->u.number >= current_prog->num_structs)
    {
        ERRORF(("Too big struct index: %"PRIdPINT", max %hu\n"
               , isvp->u.number, current_prog->num_structs
              ));
    }

    /* Get the struct type index */
    s_index = (short)isvp->u.number;

    /* Get the struct value itself */
    svp = get_rvalue(sp-2, NULL);
    if (!svp || svp->type != T_STRUCT)
    {
        ERRORF(("Illegal type to struct->(): %s, expected struct.\n"
               , typename(svp ? svp->type : (sp-2)->type)
              ));
        /* NOTREACHED */
    }

    /* Check if the struct on the stack is of the correct type */
    if (s_index >= 0)
    {
        struct_type_t * pExpected = current_prog->struct_defs[s_index].type;
        struct_type_t * pType = svp->u.strct->type;

        if (struct_baseof(pExpected, pType))
        {
            /* Fine, they match! */
        }
        else if (struct_baseof_name(pExpected->name, pType))
        {
            /* Okay, that's another version of this struct.
             * We have to change the index to lookup by name.
             */

            svalue_t * member_idx = get_rvalue(sp-1, NULL);
            if (member_idx && member_idx->type == T_NUMBER)
            {
                /* Put the name of the member instead of the index. */
                p_int m_index = member_idx->u.number;

                free_svalue(sp-1);
                put_ref_symbol(sp-1, pExpected->member[m_index].name, 1);
            }
            /* else it is already a lookup by name. */

            /* However in both cases we need to ignore lookup
             * errors, because the struct definition changed.
             */
            if (ignore_error)
                *ignore_error = true;
        }
        else
        {
            /* An incompatible struct... */
            string_t * got_name, * exp_name;

            got_name = struct_unique_name(svp->u.strct);
            if (!got_name)
                got_name = struct_name(svp->u.strct);

            exp_name = struct_t_unique_name(pExpected);
            if (!exp_name)
                exp_name = struct_t_name(pExpected);

            ERRORF(("Illegal type to struct->(): struct %s, "
                    "expected struct %s.\n"
                   , get_txt(got_name)
                   , get_txt(exp_name)
                  ));
        }
    }

    /* Remove the type index entry from the stack */
    free_svalue(sp);
    return sp-1;
} /* check_struct_op() */

/*-------------------------------------------------------------------------*/
static INLINE svalue_t *
get_struct_item (struct_t * st, svalue_t * i, svalue_t *sp, bytecode_p pc, bool ignore_error)

/* Index struct <st> with index <i> and return the pointer to the
 * indexed item.
 * If the index is invalid throw an error, unless <ignore_error> is set, then return 0.
 */

{
    p_int ind;
    svalue_t * item;

    if (i->type == T_SYMBOL || i->type == T_STRING)
    {
        ind = struct_find_member(st->type, i->u.str);
        if (ind < 0)
        {
            if (ignore_error)
            {
                free_svalue(&struct_member_temporary);
                put_number(&struct_member_temporary, 0);
                return &struct_member_temporary;
            }

            ERRORF(("Illegal struct '%s' member: '%s' not found.\n"
                   , get_txt(struct_name(st))
                   , get_txt(i->u.str)
                   ));
            /* NOTREACHED */
            return NULL;
        }
    }
    else if (i->type != T_NUMBER)
    {
        ERRORF(("Illegal struct '%s' member: got %s, "
                "expected number/string/symbol.\n"
               , get_txt(struct_name(st))
               , typename(i->type)
               ));
        return NULL;
    }
    else
    {
        ind = i->u.number;
        if (ind < 0)
        {
            ERRORF(("Illegal struct '%s' member: not a positive number.\n"
                   , get_txt(struct_name(st))
                  ));
            /* NOTREACHED */
            return NULL;
        }
        if (ind >= struct_size(st))
        {
            if (ignore_error)
            {
                free_svalue(&struct_member_temporary);
                put_number(&struct_member_temporary, 0);
                return &struct_member_temporary;
            }

            ERRORF(("Illegal struct '%s' member: out of bounds: "
                    "%"PRIdPINT", struct sized: %lu.\n"
                   , get_txt(struct_name(st))
                   , ind
                   , (unsigned long)struct_size(st)
                  ));
            /* NOTREACHED */
            return NULL;
        }
    }

    /* Compute the indexed element */
    item = &st->member[ind];
    if (destructed_object_ref(item))
    {
        free_svalue(item);
        put_number(item, 0);
    }

    return item;
} /* get_struct_item() */

/*-------------------------------------------------------------------------*/
static INLINE void
assign_var_lvalue_no_free (svalue_t *dest, svalue_t *src)

/* Put an unprotected lvalue to <sp> into <dest>.
 * <dest> is considered empty at the time of call.
 * If <src> is already a (protected) lvalue, ignore that.
 */
{
    dest->type = T_LVALUE;
    dest->x.lvalue_type = LVALUE_UNPROTECTED;
    dest->u.lvalue = src;
} /* assign_var_lvalue_no_free() */

/*-------------------------------------------------------------------------*/
static INLINE void
assign_lvalue_no_free (svalue_t *dest, svalue_t *src)

/* Put an unprotected lvalue to <sp> into <dest>.
 * <dest> is considered empty at the time of call.
 * If <src> is already a (protected) lvalue, use that.
 */
{
    if (src->type == T_LVALUE)
    {
        assign_svalue_no_free(dest, src);
    }
    else
    {
        assign_var_lvalue_no_free(dest, src);
    }
} /* assign_lvalue_no_free() */

/*-------------------------------------------------------------------------*/
static INLINE void
assign_char_lvalue_no_free (svalue_t *dest, struct protected_lvalue *var, string_t* src, char *cp)

/* Put an unprotected char lvalue to <cp> into <dest>.
 * <dest> is considered empty at the time of call.
 */
{
    assert(src->info.type == STRING_MUTABLE);

    dest->type = T_LVALUE;
    dest->x.lvalue_type = LVALUE_UNPROTECTED_CHAR;

    current_unprotected_char.str = src;
    current_unprotected_char.charp = cp;
    current_unprotected_char.var = var;
} /* assign_char_lvalue_no_free() */

/*-------------------------------------------------------------------------*/
static INLINE void
assign_mapentry_lvalue_no_free (svalue_t *dest, mapping_t *map, svalue_t *key, int index)

/* Put an unprotected mapentry lvalue for map[key,index] into <dest>.
 * <dest> is considered empty at the time of call.
 */

{
    dest->type = T_LVALUE;
    dest->x.lvalue_type = LVALUE_UNPROTECTED_MAPENTRY;

    current_unprotected_mapentry.map = map;
    free_svalue(&(current_unprotected_mapentry.key));
    internal_assign_svalue_no_free(&(current_unprotected_mapentry.key), key);
    current_unprotected_mapentry.index = index;
} /* assign_protected_mapentry_lvalue_no_free() */

/*-------------------------------------------------------------------------*/
INLINE void
assign_protected_lvalue_no_free (svalue_t *dest, svalue_t *src)

/* Put a protected lvalue to <src> into <dest>.
 * <dest> is considered empty at the time of call.
 * <src> is not allowed to be an unprotected lvalue.
 */
{
    if (src->type == T_LVALUE)
    {
        assign_svalue_no_free(dest, src);
    }
    else
    {
        struct protected_lvalue *lval;

        memsafe(lval = xalloc(sizeof(*lval)), sizeof(*lval)
               , "protected lvalue");

        lval->ref = 2;
        lval->val = *src;

        dest->type = src->type = T_LVALUE;
        dest->x.lvalue_type = src->x.lvalue_type = LVALUE_PROTECTED;
        dest->u.protected_lvalue = src->u.protected_lvalue = lval;

        num_protected_lvalues++;
    }
} /* assign_protected_lvalue_no_free() */

/*-------------------------------------------------------------------------*/
INLINE void
assign_protected_char_lvalue_no_free (svalue_t *dest, struct protected_lvalue *var, string_t *src, char *charp)

/* Put a protected char lvalue to <cp> into <dest>.
 * <dest> is considered empty at the time of call.
 */
{
    struct protected_char_lvalue *lval;

    assert(src->info.type == STRING_MUTABLE);

    memsafe(lval = xalloc(sizeof(*lval)), sizeof(*lval)
           , "protected char lvalue");

    lval->ref = 1;
    lval->str = ref_mstring(src);
    lval->charp = charp;
    lval->var = var;
    lval->next = src->u.mutable.char_lvalues;

    if (var)
        var->ref++;
    src->u.mutable.char_lvalues = lval;

    dest->type = T_LVALUE;
    dest->x.lvalue_type = LVALUE_PROTECTED_CHAR;
    dest->u.protected_char_lvalue = lval;

    num_protected_lvalues++;
} /* assign_protected_char_lvalue_no_free() */

/*-------------------------------------------------------------------------*/
INLINE void
assign_protected_range_lvalue_no_free (svalue_t *dest, struct protected_lvalue *var, svalue_t *vec, mp_int index1, mp_int index2)

/* Put a protected range lvalue to <src>[index1..index2-1] into <dest>.
 * <dest> is considered empty at the time of call.
 */
{
    struct protected_range_lvalue *lval;

    memsafe(lval = xalloc(sizeof(*lval)), sizeof(*lval)
           , "protected range lvalue");

    lval->ref = 1;
    lval->index1 = index1;
    lval->index2 = index2;

    if (vec->type == T_STRING || vec->type == T_BYTES)
    {
        assert(vec->u.str->info.type == STRING_MUTABLE);

        lval->next = vec->u.str->u.mutable.range_lvalues;
        vec->u.str->u.mutable.range_lvalues = lval;
    }
    else
        lval->next = NULL;

    /* Save the vector first. */
    assign_svalue_no_free(&(lval->vec), vec);

    lval->var = var;
    if (var)
        var->ref++;

    dest->type = T_LVALUE;
    dest->x.lvalue_type = LVALUE_PROTECTED_RANGE;
    dest->u.protected_range_lvalue = lval;

    num_protected_lvalues++;
} /* assign_protected_range_lvalue_no_free() */

/*-------------------------------------------------------------------------*/
INLINE void
assign_protected_lvalue (svalue_t *dest, svalue_t *src)

/* Put a protected lvalue to <src> into <dest>.
 * <dest> is considered a valid svalue and therefore freed
 * before the assignment, unless it is already an lvalue.
 * <src> is not allowed to be an unprotected lvalue.
 */
{
    if (src->type == T_LVALUE)
    {
        assign_svalue(dest, src);
    }
    else if (dest->type == T_LVALUE)
    {
        /* Do the assignment and reversely put the
         * lvalue into <src>.
         */
        assign_svalue(dest, src);
        free_svalue(src);
        assign_protected_lvalue_no_free(src, dest);
    }
    else
    {
        free_svalue(dest);
        assign_protected_lvalue_no_free(dest, src);
    }
} /* assign_protected_lvalue_no() */

/*-------------------------------------------------------------------------*/
void
assign_protected_mapentry_lvalue_no_free (svalue_t *dest, mapping_t *map, svalue_t *key, int index)

/* Put a protected mapentry lvalue &(map[key,index]) into <dest>.
 * <dest> is considered empty at the time of call.
 */

{
    struct protected_mapentry_lvalue *lval;

    memsafe(lval = xalloc(sizeof(*lval)), sizeof(*lval)
           , "protected mapentry lvalue");

    lval->ref = 1;
    lval->map = ref_mapping(map);
    internal_assign_svalue_no_free(&(lval->key), key);
    lval->index = index;

    dest->type = T_LVALUE;
    dest->x.lvalue_type = LVALUE_PROTECTED_MAPENTRY;
    dest->u.protected_mapentry_lvalue = lval;

    num_protected_lvalues++;
} /* assign_protected_mapentry_lvalue_no_free() */

/*-------------------------------------------------------------------------*/
static INLINE svalue_t *
push_protected_lvalue (svalue_t *sp)

/* Convert the value sp[0] into a protected lvalue.
 * It must be an lvalue (protected or unprotected).
 * If its already protected, then this function is a no-op.
 */
{
    if (sp->type != T_LVALUE)
    {
        fatal("(push_protected_lvalue) Expected lvalue, "
              "received svalue type (%"PRIdPHINT":%"PRIdPHINT")\n"
            , sp->type, sp->x.generic);
        /* NOTREACHED */
        return sp;
    }

    /* If there is an unprotected lvalue on <sp>,
     * then we can just overwrite it, because
     * it doesn't hold any counted references.
     */
    switch (sp->x.lvalue_type)
    {
        default:
            fatal("(push_protected_lvalue) Illegal lvalue %p type %d\n", sp, sp->x.lvalue_type);
            /* NOTREACHED */
            break;

        case LVALUE_UNPROTECTED:
            assign_protected_lvalue_no_free(sp, sp->u.lvalue);
            break;

        case LVALUE_UNPROTECTED_CHAR:
            assign_protected_char_lvalue_no_free(sp,
                current_unprotected_char.var,
                current_unprotected_char.str, current_unprotected_char.charp);
            break;

        case LVALUE_UNPROTECTED_RANGE:
            assign_protected_range_lvalue_no_free(sp,
                current_unprotected_range.var, current_unprotected_range.vec,
                current_unprotected_range.index1, current_unprotected_range.index2);
            break;

        case LVALUE_UNPROTECTED_MAPENTRY:
            assign_protected_mapentry_lvalue_no_free(sp,
                current_unprotected_mapentry.map,
                &(current_unprotected_mapentry.key),
                current_unprotected_mapentry.index);
            free_svalue(&(current_unprotected_mapentry.key));
            break;

        case LVALUE_PROTECTED:
        case LVALUE_PROTECTED_CHAR:
        case LVALUE_PROTECTED_RANGE:
        case LVALUE_PROTECTED_MAPENTRY:
            NOOP;
            break;
    }

    return sp;
} /* push_protected_lvalue() */

/*-------------------------------------------------------------------------*/
static INLINE svalue_t *
get_unprotected_lvalue (svalue_t *lval)

/* Generate an unprotected lvalue from *lval.
 * It must be an lvalue (protected or unprotected).
 * If its already unprotected, then this function is a no-op.
 * The svalue *lval is not modified.
 *
 * The resulting unprotected lvalue shouldn't be used
 * to make a protected one from it (again), because it
 * would lead to lvalue chains (which we don't expect anymore).
 */
{
    /* If we must convert, then use that static variable.
     * It's safe, because it carries no counted references
     * and there is only one unprotected lvalue at a time.
     */
    static svalue_t unprotected_lvalue;

    if (lval->type != T_LVALUE)
    {
        fatal("(get_unprotected_lvalue) Expected lvalue, "
              "received svalue type (%"PRIdPHINT":%"PRIdPHINT")\n"
            , lval->type, lval->x.generic);
        /* NOTREACHED */
        return lval;
    }

    /* If there is an unprotected lvalue on <lval>,
     * then we can just overwrite it, because
     * it doesn't hold any counted references.
     */
    switch (lval->x.lvalue_type)
    {
        default:
            fatal("(push_protected_lvalue) Illegal lvalue %p type %d\n", lval, lval->x.lvalue_type);
            /* NOTREACHED */
            return NULL;

        case LVALUE_UNPROTECTED:
        case LVALUE_UNPROTECTED_CHAR:
        case LVALUE_UNPROTECTED_RANGE:
        case LVALUE_UNPROTECTED_MAPENTRY:
            return lval;

        case LVALUE_PROTECTED:
            assign_lvalue_no_free(&unprotected_lvalue, &(lval->u.protected_lvalue->val));
            return &unprotected_lvalue;

        case LVALUE_PROTECTED_CHAR:
            assign_char_lvalue_no_free(&unprotected_lvalue, lval->u.protected_char_lvalue->var, lval->u.protected_char_lvalue->str, lval->u.protected_char_lvalue->charp);
            return &unprotected_lvalue;

        case LVALUE_PROTECTED_RANGE:
            unprotected_lvalue.type = T_LVALUE;
            unprotected_lvalue.x.lvalue_type = LVALUE_UNPROTECTED_RANGE;
            current_unprotected_range.var = lval->u.protected_range_lvalue->var;
            current_unprotected_range.vec = &(lval->u.protected_range_lvalue->vec);
            current_unprotected_range.index1 = lval->u.protected_range_lvalue->index1;
            current_unprotected_range.index2 = lval->u.protected_range_lvalue->index2;
            return &unprotected_lvalue;

        case LVALUE_PROTECTED_MAPENTRY:
        {
            struct protected_mapentry_lvalue *e = lval->u.protected_mapentry_lvalue;

            unprotected_lvalue.type = T_LVALUE;
            unprotected_lvalue.x.lvalue_type = LVALUE_UNPROTECTED_MAPENTRY;
            current_unprotected_mapentry.map = e->map;
            free_svalue(&(current_unprotected_mapentry.key));
            internal_assign_svalue_no_free(&(current_unprotected_mapentry.key), &(e->key));
            current_unprotected_mapentry.index = e->index;
            return &unprotected_lvalue;
        }
    }
} /* get_unprotected_lvalue() */

/*-------------------------------------------------------------------------*/
static INLINE svalue_t *
push_index_lvalue (svalue_t *sp, bytecode_p pc, enum index_type itype, bool reseating, bool ignore_error)

/* Operator F_INDEX_LVALUE    (string|vector v=sp[-1], int   i=sp[0])
 *          F_RINDEX_LVALUE   (string|vector v=sp[-1], int   i=sp[0])
 *          F_AINDEX_LVALUE   (string|vector v=sp[-1], int   i=sp[0])
 *          F_INDEX_LVALUE    (mapping       v=sp[-1], mixed i=sp[0])
 *          F_S_INDEX_LVALUE  (struct        v=sp[-1], mixed i=sp[0])
 *          F_SX_INDEX_LVALUE (struct        v=sp[-1], mixed i=sp[0])
 *          F_INDEX_VLVALUE   (string|vector v=sp[-1], int   i=sp[0])
 *          F_RINDEX_VLVALUE  (string|vector v=sp[-1], int   i=sp[0])
 *          F_AINDEX_VLVALUE  (string|vector v=sp[-1], int   i=sp[0])
 *          F_INDEX_VLVALUE   (mapping       v=sp[-1], mixed i=sp[0])
 *          F_S_INDEX_VLVALUE (struct        v=sp[-1], mixed i=sp[0])
 *          F_SX_INDEX_VLVALUE(struct        v=sp[-1], mixed i=sp[0])
 *
 * Compute the index &(v[i]) of lrvalue <v> and push it onto the stack
 * as an lvalue.
 * If <v> is a string-lvalue, it is made a malloced string if necessary,
 * and the pushed result will be a lvalue pointing to a CHAR_LVALUE stored
 * in <current_unprotected_char>.
 * <ignore_error> will just be passed to get_struct_item() and results in
 * making an lvalue for a tempoary, when a struct member is not found.
 */

{
    svalue_t *vec;   /* the vector/mapping */
    svalue_t *idx;   /* the index */
    svalue_t *item;  /* the resulting item (for non-string values) */
    bool      last_reference = false; /* Whether the value has
                                       * only one reference left.
                                       */

    /* get the arguments */
    vec = get_rvalue_no_collapse(sp - 1, &last_reference);
    idx = sp;

    if (vec == NULL) /* It's a protected range lvalue */
    {
        struct protected_range_lvalue *r;

        vec = sp - 1;
        assert(vec->type == T_LVALUE && vec->x.lvalue_type == LVALUE_PROTECTED_RANGE);
        r = vec->u.protected_range_lvalue;

        switch (r->vec.type)
        {
            case T_POINTER:
            {
                item = get_vector_range_item(r->vec.u.vec, r->index1, r->index2, idx, sp, pc, itype);

                if (last_reference)
                    last_reference = (r->vec.u.vec->ref == 1);
                break;
            }

            case T_STRING:
            case T_BYTES:
            {
                svalue_t temp;
                string_t * orig = r->vec.u.str;
                char * cp = get_string_range_item(&(r->vec), r->index1, r->index2, idx, MY_TRUE, MY_FALSE, sp, pc, itype);

                /* Update the original variable. */
                if (r->vec.u.str != orig
                 && r->var != NULL
                 && r->var->val.type == r->vec.type
                 && r->var->val.u.str == orig)
                {
                    free_mstring(orig);
                    r->var->val.u.str = ref_mstring(r->vec.u.str);
                }

                if (last_reference && vec->u.str->info.ref == 1)
                {
                    /* The string will go away, store the number. */
                    free_svalue(&indexing_quickfix);
                    put_number(&indexing_quickfix, *(unsigned char*)cp);

                    free_svalue(sp--);
                    free_svalue(sp);

                    assign_lvalue_no_free(sp, &indexing_quickfix);
                    return sp;
                }

                assign_char_lvalue_no_free(&temp, r->var, r->vec.u.str, cp);
                free_svalue(sp--);
                free_svalue(sp);
                *sp = temp;

                return sp;
            }

            default:
                fatal("(index_lvalue)Illegal type for range lvalue '%s'.\n", typename(r->vec.type));
        }
    }
    else
    {
        switch (vec->type)
        {
            /* Index an array.
             */
            case T_POINTER:
                item = get_vector_item(vec->u.vec, idx, sp, pc, itype);

                if (last_reference)
                    last_reference = (vec->u.vec->ref == 1);
                break;

            /* Index a string.
             */
            case T_STRING:
            case T_BYTES:
            {
                char * cp;
                struct protected_lvalue *var = NULL;

                if (reseating)
                    errorf("(index_vlvalue)Indexing on illegal type '%s'.\n", typename(vec->type));

                cp = get_string_item(vec, idx, /* make_mutable: */ MY_TRUE
                                    , /* allow_one_past: */ MY_FALSE
                                    , sp, pc, itype);

                if (sp[-1].type == T_LVALUE)
                {
                    assert(sp[-1].x.lvalue_type == LVALUE_PROTECTED);
                    var = sp[-1].u.protected_lvalue;
                }

                if (last_reference && vec->u.str->info.ref == 1)
                {
                    /* The string will go away, store the number. */
                    free_svalue(&indexing_quickfix);
                    put_number(&indexing_quickfix, *(unsigned char*)cp);

                    free_svalue(sp--);
                    free_svalue(sp);

                    assign_lvalue_no_free(sp, &indexing_quickfix);
                    return sp;
                }

                /* Remove the arguments and create and push the result. */
                free_svalue(sp--);
                free_svalue(sp);

                assign_char_lvalue_no_free(sp, var, vec->u.str, cp);
                return sp;
            }

            /* Index a struct.
             */
            case T_STRUCT:
                if (itype != REGULAR_INDEX)
                {
                    inter_sp = sp;
                    inter_pc = pc;
                    errorf("(index_lvalue)Indexing on illegal type '%s'.\n", typename(vec->type));
                    return NULL;
                }

                item = get_struct_item(vec->u.strct, idx, sp, pc, ignore_error);

                if (last_reference)
                    last_reference = (vec->u.strct->ref == 1);
                break;

            /* Index a mapping.
             */
            case T_MAPPING:
            {
                mapping_t *m;

                if (itype != REGULAR_INDEX)
                {
                    inter_sp = sp;
                    inter_pc = pc;
                    errorf("(index_lvalue)Indexing on illegal type '%s'.\n", typename(vec->type));
                    return NULL;
                }

                m = vec->u.map;
                if (!m->num_values)
                {
                    ERROR("Indexing a mapping of width 0.\n");
                    return NULL;
                }

                /* Compute the element */
                item = get_map_value(m, idx);
                if (last_reference)
                    last_reference = (m->ref == 1);

                if (item == &const0 && !last_reference)
                {
                    /* There is no entry, yet. We create a mapentry lvalue. */
                    free_svalue(sp-1);
                    assign_mapentry_lvalue_no_free(sp-1, m, idx, 0);

                    free_svalue(sp--);

                    return sp;
                }

                /* We either create the lvalue to the existing entry
                 * or to const0, which will be transfered to indexing_quickfix.
                 */
                break;
            }

            default:
                /* Illegal type to index. */
                inter_sp = sp;
                inter_pc = pc;
                errorf("(index_lvalue)Indexing on illegal type '%s'.\n", typename(vec->type));
                return NULL;
        }
    }

    /* We should now have our result in <item>. */
    if (last_reference)
    {
        /* Rescue the indexed item as st will go away */
        free_svalue(&indexing_quickfix);
        assign_svalue_no_free(&indexing_quickfix, item);
        item = &indexing_quickfix;
    }

    /* Remove the arguments and push the result */
    free_svalue(sp--);
    free_svalue(sp);

    if (reseating)
        assign_var_lvalue_no_free(sp, item);
    else
        assign_lvalue_no_free(sp, item);
    return sp;
} /* push_index_lvalue() */


/*-------------------------------------------------------------------------*/
static INLINE svalue_t *
push_map_index_lvalue (svalue_t *sp, bytecode_p pc, bool reseating)

/* Operator F_MAP_INDEX_LVALUE(  mapping m=sp[-2]
 *                             , mixed i=sp[-1], int j=sp[0])
 *          F_MAP_INDEX_VLVALUE( mapping m=sp[-2]
 *                             , mixed i=sp[-1], int j=sp[0])
 *
 * Compute the lvalue &(m[i,j]) and push it into the stack. If v has
 * just one ref left, the indexed item is stored in indexing_quickfix
 * and the lvalue refers to that variable.
 */

{
    svalue_t *data;
    mapping_t *m;
    mp_int n;

    if (sp[-2].type != T_MAPPING)
    {
        ERRORF(("(lvalue) Indexing on illegal type: %s, expected mapping.\n"
               , typename(sp[-2].type)
              ));
    }
    if (sp[0].type != T_NUMBER)
    {
        ERRORF(("Illegal sub-index type: %s, expected number.\n"
               , typename(sp[0].type)
              ));
    }

    m = sp[-2].u.map;
    n = sp->u.number;
    if (n < 0 || n >= m->num_values)
    {
        ERRORF(("Illegal sub-index %"PRIdMPINT", mapping width is %"
                PRIdPINT".\n", n, m->num_values));
    }

    sp--; /* the key */
    data = get_map_value(m, sp);

    if (data == &const0)
    {
        if (m->ref == 1)
        {
            /* The entry doesn't exist, but the mapping also going away. */
            pop_stack();

            free_svalue(&indexing_quickfix);
            assign_svalue_no_free(&indexing_quickfix, data);
            assign_var_lvalue_no_free(sp, &indexing_quickfix);
        }
        else
        {
            assign_mapentry_lvalue_no_free(sp-1, m, sp, n);

            pop_stack();
        }
    }
    else
    {
        pop_stack(); /* We don't need the key anymore. */

        if (m->ref == 1)
        {
            free_svalue(&indexing_quickfix);
            assign_svalue_no_free(&indexing_quickfix, data + n);
            if (reseating)
                assign_var_lvalue_no_free(sp, &indexing_quickfix);
            else
                assign_lvalue_no_free(sp, &indexing_quickfix);
        }
        else
        {
            if (reseating)
                assign_var_lvalue_no_free(sp, data + n);
            else
                assign_lvalue_no_free(sp, data + n);
        }
    }
    free_mapping(m);

    return sp;
} /* push_map_index_lvalue() */


/*-------------------------------------------------------------------------*/
/* Code values used by push_range_lvalue() and push_range_value()
 */

enum range_type
{
    NN_RANGE = 0,
    RN_RANGE = 1 << 0,
    AN_RANGE = 2 << 0,
    NR_RANGE = 1 << 2,
    NA_RANGE = 2 << 2,

    RR_RANGE = (RN_RANGE|NR_RANGE),
    RA_RANGE = (RN_RANGE|NA_RANGE),
    AR_RANGE = (AN_RANGE|NR_RANGE),
    AA_RANGE = (AN_RANGE|NA_RANGE),

    NX_MASK  = 3 << 0,
    XN_MASK  = 3 << 2,
};

static svalue_t *
push_range_lvalue (enum range_type code, svalue_t *sp, bytecode_p pc)

/* Operator F_RANGE_LVALUE (string|vector &v=sp[-2], int i1=sp[-1], i2=sp[0])
 * and the operators F_*_RANGE_LVALUE.
 *
 * Compute the range &(v[i1..i2]) of lvalue <v> and push it into the stack.
 * The value pushed is a lvalue pointing to <special_lvalue>. <special_lvalue>
 * then is the POINTER_RANGE_- resp. STRING_RANGE_LVALUE.
 *
 * <code> is a four-bit flag determining whether the indexes are counted
 * from the beginning ('[i1..' and '..i2]'), the end of the vector
 * or string ('[<i1..' and '..<i2]'), or depending on the sign of the
 * index either from the beginning or end ('[>i1..' and '..>i2]').
 * <code>&NX_MASK determines the mode for the lower index (NN_RANGE,
 * RN_RANGE or AN_RANGE), <code>&XN_MASK the upper index (NN_RANGE,
 * NR_RANGE or NA_RANGE).
 */

{
    svalue_t       *vec;          /* the indexed vector or string */
    mp_int         ind1, ind2;    /* Lower and upper range index */
    mp_int         size;          /* size of <vec> in elements for arrays
                                     resp. in bytes for strings */
    mp_int         length;        /* size of <vec> in elements */
    mp_int         offset;        /* offset into <vec> */
    bool last_reference = false;  /* Whether the value has
                                   * only one reference left.
                                   */
    struct protected_lvalue *var; /* The variable holding the vector */

    /* Get the vector */
    vec = get_rvalue_no_collapse(sp - 2, &last_reference);

    /* Get the indices first. */
    if (sp[-1].type != T_NUMBER)
        ERRORF(("Bad type '%s' of start interval to [..] range.\n"
               , typename(sp[-1].type)
               ));
    if (sp[0].type != T_NUMBER)
        ERRORF(("Bad type '%s' of end interval to [..] range.\n"
               , typename(sp[0].type)
               ));
    ind1 = sp[-1].u.number;
    ind2 = sp[0].u.number;

    /* Now get to the vector */
    if (vec == NULL)
    {
        /* It must be a protected range. */
        struct protected_range_lvalue *r;

        vec = sp - 2;
        assert(vec->type == T_LVALUE && vec->x.lvalue_type == LVALUE_PROTECTED_RANGE);
        r = vec->u.protected_range_lvalue;

        vec = &(r->vec);
        offset = r->index1;
        size = r->index2 - r->index1;
        var = r->var;

        if (vec->type != T_POINTER && vec->type != T_STRING && vec->type != T_BYTES)
            fatal("(range_lvalue)Illegal type for range lvalue '%s'.\n", typename(vec->type));
    }
    else
    {
        if (sp[-2].type == T_LVALUE && vec->type != T_NUMBER)
        {
            /* The assertion will not hold if vec is a number,
             * then the lvalue could have been a char or mapentry lvalue.
             */
            assert(sp[-2].x.lvalue_type == LVALUE_PROTECTED);
            var = sp[-2].u.protected_lvalue;
        }
        else
        {
            var = NULL;
            // We're referencing an svalue_t on the stack...
            last_reference = true;
        }

        switch (vec->type)
        {
            case T_POINTER:
                length = size = VEC_SIZE(vec->u.vec);
                offset = 0;
                break;

            case T_STRING:
            case T_BYTES:
            {
                string_t *str;
                memsafe(str = make_mutable(vec->u.str), mstrsize(vec->u.str), "modifiable string");
                vec->u.str = str;

                size = mstrsize(str);
                offset = 0;
                break;
            }

            default:
                ERRORF(("Bad argument to [..] operand: got %s, "
                        "expected string/array.\n", typename(vec->type))
                        );
        }
    }

    /* Convert size in bytes to characters. */
    if (vec->type == T_STRING && vec->u.str->info.unicode == STRING_UTF8)
    {
        bool error = false;
        length = byte_to_char_index(get_txt(vec->u.str) + offset, size, &error);
        if (error)
            ERRORF(("Invalid character in string at index %zd.\n", length));
    }
    else
        length = size;

    if (var && var->ref == 1)
        var = NULL;

    if (last_reference)
    {
        /* Rescue the vector as it will go away */
        svalue_t temp = indexing_quickfix;
        assign_svalue_no_free(&indexing_quickfix, vec);
        free_svalue(&temp);

        vec = &indexing_quickfix;
    }

    switch (code & NX_MASK)
    {
        case NN_RANGE:
            // Nothing to do.
            break;

        case RN_RANGE:
            ind1 = length - ind1;
            break;

        case AN_RANGE:
            if (ind1 < 0)
                ind1 = length + ind1;
            break;

        default:
            fatal("(range_value)Illegal index type %d.\n", code);
            break;
    }

    switch (code & XN_MASK)
    {
        case NN_RANGE:
            // Nothing to do.
            break;

        case NR_RANGE:
            ind2 = length - ind2;
            break;

        case NA_RANGE:
            if (ind2 < 0)
                ind2 = length + ind2;
            break;

        default:
            fatal("(range_value)Illegal index type %d.\n", code);
            break;
    }

    if (ind1 < 0 || ind1 > length)
    {
        /* Appending (ind1 == length) is allowed */
        inter_pc = pc;
        inter_sp = sp;
        if (ind2 < -1 || ind2 >= length)
            errorf( "Warning: Out-of-bounds range limits: [%"
                    PRIdMPINT"..%"PRIdMPINT"], length %"PRIdMPINT".\n"
                  , ind1, ind2, length);
        else
            errorf( "Warning: Out-of-bounds lower range limits: %"
                    PRIdMPINT", length %"PRIdMPINT".\n"
                  , ind1, length);
    }
    else if (ind2 < -1 || ind2 >= length)
    {
        inter_pc = pc;
        inter_sp = sp;
        errorf( "Warning: Out-of-bounds upper range limits: %"
                PRIdMPINT", length %"PRIdMPINT".\n"
              , ind2, length);
    }
    else if (ind1 > ind2 + 1)
    {
        inter_pc = pc;
        inter_sp = sp;
        errorf( "Warning: Ranges of negative size: %"PRIdMPINT
                "..%"PRIdMPINT".\n"
              , ind1, ind2);
    }

    /* And now convert the character indices back to byte index. */
    if (vec->type == T_STRING && vec->u.str->info.unicode == STRING_UTF8)
    {
        bool error = false;
        ind1 = char_to_byte_index(get_txt(vec->u.str) + offset, size, ind1, &error);
        if (error)
            ERRORF(("Invalid character in string at byte %zd.\n", ind1));
        ind2 = char_to_byte_index(get_txt(vec->u.str) + offset, size, ind2 + 1, &error) - 1;
        if (error)
            ERRORF(("Invalid character in string at byte %zd.\n", ind2));
    }

    /* Save information to the current_unprotected_range structure.
     */
    current_unprotected_range.var = var;
    current_unprotected_range.vec = vec;
    current_unprotected_range.index1 = offset+ind1;
    current_unprotected_range.index2 = offset+ind2+1;

    /* Drop the arguments and return the result. */
    pop_n_elems(3);
    sp++;

    sp->type = T_LVALUE;
    sp->x.lvalue_type = LVALUE_UNPROTECTED_RANGE;

    return sp;
} /* push_range_lvalue() */

/*-------------------------------------------------------------------------*/
static INLINE svalue_t *
push_index_value (svalue_t *sp, bytecode_p pc, bool ignore_error, enum index_type itype)

/* Operator F_INDEX   (string|vector v=sp[-1], int        i=sp[0])
 *          F_RINDEX  (string|vector v=sp[-1], int        i=sp[0])
 *          F_AINDEX  (string|vector v=sp[-1], int        i=sp[0])
 *          F_INDEX   (mapping       v=sp[-1], mixed      i=sp[0])
 *          F_S_INDEX (struct        v=sp[-1], string|int i=sp[0])
 *          F_SX_INDEX(struct        v=sp[-1], string|int i=sp[0])
 *
 * Compute the value (v[i]) and push it onto the stack.
 * If the value would be a destructed object, 0 is pushed onto the stack
 * and the ref to the object is removed from the vector/mapping.
 * <ignore_error> will just be passed to get_struct_item() and
 * results in returning 0, when a struct member is not found.
 */

{
    svalue_t *vec;    /* the indexed value */
    svalue_t *idx;    /* the index */
    svalue_t *item;   /* the item in the structure */
    svalue_t  result; /* a copy of that item */

    /* get the arguments */
    vec = get_rvalue(sp - 1, NULL);
    idx = sp;

    if (vec == NULL) /* It's a protected range lvalue */
    {
        struct protected_range_lvalue *r;

        vec = sp - 1;
        assert(vec->type == T_LVALUE && vec->x.lvalue_type == LVALUE_PROTECTED_RANGE);
        r = vec->u.protected_range_lvalue;

        switch (r->vec.type)
        {
            case T_POINTER:
            {
                item = get_vector_range_item(r->vec.u.vec, r->index1, r->index2, idx, sp, pc, itype);
                break;
            }

            case T_STRING:
            case T_BYTES:
            {
                char *cp = get_string_range_item(&(r->vec), r->index1, r->index2, idx, MY_FALSE, MY_TRUE, sp, pc, itype);
                p_int c;

                if (r->vec.u.str->info.unicode == STRING_UTF8)
                {
                    if (!utf8_to_unicode(cp, 4, &c))
                    {
                        inter_sp = sp;
                        inter_pc = pc;
                        errorf("(index_value)Invalid character in string.\n");
                        return NULL;
                    }
                }
                else
                    c = *((unsigned char*)cp);

                free_svalue(sp--);
                free_svalue(sp);

                put_number(sp, c);
                return sp;
            }

            default:
                fatal("(index_value)Illegal type for range lvalue '%s'.\n", typename(r->vec.type));
        }
    }
    else
    {
        switch (vec->type)
        {
            /* Index an array.
             */
            case T_POINTER:
                item = get_vector_item(vec->u.vec, idx, sp, pc, itype);
                break;

            /* Index a string or byte sequence.
             */
            case T_STRING:
            case T_BYTES:
            {
                char *cp = get_string_item(vec, idx, /* make_mutable: */ MY_FALSE
                                    , /* allow_one_past: */ MY_TRUE
                                    , sp, pc, itype);
                p_int c;

                if (vec->u.str->info.unicode == STRING_UTF8)
                {
                    if (!utf8_to_unicode(cp, 4, &c))
                    {
                        inter_sp = sp;
                        inter_pc = pc;
                        errorf("(index_value)Invalid character in string.\n");
                        return NULL;
                    }
                }
                else
                    c = *((unsigned char*)cp);

                /* Remove the arguments and create and push the result. */
                free_svalue(sp--);
                free_svalue(sp);

                put_number(sp, c);
                return sp;
            }

            /* Index a struct.
             */
            case T_STRUCT:
                if (itype != REGULAR_INDEX)
                {
                    inter_sp = sp;
                    inter_pc = pc;
                    errorf("(index_value)Indexing on illegal type '%s'.\n", typename(vec->type));
                    return NULL;
                }

                item = get_struct_item(vec->u.strct, idx, sp, pc, ignore_error);
                break;

            /* Index a mapping.
             */
            case T_MAPPING:
            {
                mapping_t *m;

                if (itype != REGULAR_INDEX)
                {
                    inter_sp = sp;
                    inter_pc = pc;
                    errorf("(index_value)Indexing on illegal type '%s'.\n", typename(vec->type));
                    return NULL;
                }

                m = vec->u.map;
                if (!m->num_values)
                {
                    ERROR("Indexing a mapping of width 0.\n");
                    return NULL;
                }

                /* Compute the element */
                item = get_map_value(m, idx);
                break;
            }

            default:
                /* Illegal type to index. */
                inter_sp = sp;
                inter_pc = pc;
                errorf("(index_value)Indexing on illegal type '%s'.\n", typename(vec->type));
                return NULL;
        }
    }

    /* Copy item first into result, because it can vanish
     * when we're freeing the values on the stack.
     */
    assign_rvalue_no_free(&result, item);

    /* Remove the arguments and push the result */
    free_svalue(sp--);
    free_svalue(sp);

    transfer_svalue_no_free(sp, &result);
    return sp;
} /* push_index_value() */

/*-------------------------------------------------------------------------*/
static INLINE svalue_t *
push_range_value (enum range_type code, svalue_t *sp, bytecode_p pc)

/* Operator F_RANGE (string|vector &v=sp[-2], int i1=sp[-1], i2=sp[0])
 * and the operators F_*_RANGE.
 *
 * Compute the value v[i1..i2] and push it into the stack.
 *
 * <code> is a four-bit flag determining whether the indexes are counted
 * from the beginning ('[i1..' and '..i2]'), the end of the vector
 * or string ('[<i1..' and '..<i2]'), or depending on the sign of the
 * index either from the beginning or end ('[>i1..' and '..>i2]').
 * <code>&NX_MASK determines the mode for the lower index (NN_RANGE,
 * RN_RANGE or AN_RANGE), <code>&XN_MASK the upper index (NN_RANGE,
 * NR_RANGE or NA_RANGE).
 */

{
    svalue_t      *vec;         /* the indexed vector or string */
    mp_int         ind1, ind2;  /* Lower and upper range index */
    mp_int         size;        /* size of <vec> in elements in arrays
                                   resp. bytes in strings. */
    mp_int         length;      /* size of <vec> in elements. */
    mp_int         offset;      /* offset into <vec> */

    /* get the arguments */
    vec = get_rvalue(sp - 2, NULL);

    if (sp[-1].type != T_NUMBER)
        ERRORF(("Bad type '%s' of start interval to [..] range.\n"
               , typename(sp[-1].type)
               ));
    if (sp[0].type != T_NUMBER)
        ERRORF(("Bad type '%s' of end interval to [..] range.\n"
               , typename(sp[0].type)
               ));
    ind1 = sp[-1].u.number;
    ind2 = sp[0].u.number;

    if (vec == NULL)
    {
        /* It must be a protected range. */
        struct protected_range_lvalue *r;

        vec = sp - 2;
        assert(vec->type == T_LVALUE && vec->x.lvalue_type == LVALUE_PROTECTED_RANGE);
        r = vec->u.protected_range_lvalue;

        vec = &(r->vec);
        offset = r->index1;
        size = r->index2 - r->index1;

        if (vec->type != T_POINTER && vec->type != T_STRING && vec->type != T_BYTES)
            fatal("(range_value)Illegal type for range lvalue '%s'.\n", typename(vec->type));
    }
    else
    {
        switch (vec->type)
        {
            case T_POINTER:
                size = VEC_SIZE(vec->u.vec);
                offset = 0;
                break;

            case T_STRING:
            case T_BYTES:
                size = mstrsize(vec->u.str);
                offset = 0;
                break;

            default:
                ERRORF(("Bad argument to [..] operand: got %s, "
                        "expected string/array.\n", typename(vec->type))
                        );
        }
    }

    /* Convert size in bytes to characters. */
    if (vec->type == T_STRING && vec->u.str->info.unicode == STRING_UTF8)
    {
        bool error = false;
        length = byte_to_char_index(get_txt(vec->u.str) + offset, size, &error);
        if (error)
            ERRORF(("Invalid character in string at index %zd.\n", length));
    }
    else
        length = size;

    switch (code & NX_MASK)
    {
        case NN_RANGE:
            // Nothing to do.
            break;

        case RN_RANGE:
            ind1 = length - ind1;
            break;

        case AN_RANGE:
            if (ind1 < 0)
                ind1 = length + ind1;
            break;

        default:
            fatal("(range_value)Illegal index type %d.\n", code);
            break;
    }

    switch (code & XN_MASK)
    {
        case NN_RANGE:
            // Nothing to do.
            break;

        case NR_RANGE:
            ind2 = length - ind2;
            break;

        case NA_RANGE:
            if (ind2 < 0)
                ind2 = length + ind2;
            break;

        default:
            fatal("(range_value)Illegal index type %d.\n", code);
            break;
    }

    if (runtime_array_range_check && vec->type == T_POINTER)
    {
        if (ind1 < 0 || ind1 >= length)
        {
            if (ind2 < 0 || ind2 >= length)
                WARNF(("Warning: Out-of-bounds range limits: [%"
                       PRIdMPINT"..%"PRIdMPINT"], length %"PRIdMPINT".\n"
                      , ind1, ind2, length));
            else
                WARNF(("Warning: Out-of-bounds lower range limits: %"
                       PRIdMPINT", length %"PRIdMPINT".\n"
                      , ind1, length));
        }
        else if (ind2 < 0 || ind2 >= length)
        {
            WARNF(("Warning: Out-of-bounds upper range limits: %"
                   PRIdMPINT", length %"PRIdMPINT".\n"
                  , ind2, length));
        }
        else if (ind1 > ind2 + 1)
        {
            WARNF(("Warning: Ranges of negative length: %"PRIdMPINT
                   "..%"PRIdMPINT".\n"
                  , ind1, ind2));
        }
    }

    /* Make sure, ind1 is greater then zero. */
    if (ind1 < 0)
        ind1 = 0;

    /* Make sure, ind2 is lower then length. */
    if (ind2 >= length)
        ind2 = length-1;

    switch (vec->type)
    {
        case T_POINTER:
        {
            vector_t* v = slice_array(vec->u.vec, offset + ind1, offset + ind2);

            pop_n_elems(3);

            if (v)
                push_array(sp, v);
            else
                push_number(sp, 0);
            return sp;
        }

        case T_STRING:
        case T_BYTES:
        {
            string_t *str;
            ph_int stringtype = vec->type;

            if (ind2 < ind1)
                str = ref_mstring(vec->type == T_STRING ? STR_EMPTY : empty_byte_string);
            else
            {
                /* And now convert the character indices back to byte index. */
                if (vec->u.str->info.unicode == STRING_UTF8)
                {
                    bool error = false;
                    ind1 = char_to_byte_index(get_txt(vec->u.str) + offset, size, ind1, &error);
                    if (error)
                        ERRORF(("Invalid character in string at byte %zd.\n", ind1));
                    ind2 = char_to_byte_index(get_txt(vec->u.str) + offset, size, ind2 + 1, &error) - 1;
                    if (error)
                        ERRORF(("Invalid character in string at byte %zd.\n", ind2));
                }

                str = mstr_extract(vec->u.str, offset + ind1, offset + ind2);
            }

            pop_n_elems(3);

            if (str)
            {
                sp++;
                sp->type = stringtype;
                sp->u.str = str;
            }
            else
                push_number(sp, 0);
            return sp;
        }

        default:
            fatal("(range_value)Illegal type for range lvalue '%s'.\n", typename(vec->type));
    }
}

/*=========================================================================*/
/*-------------------------------------------------------------------------*/
void
m_indices_filter ( svalue_t *key
                 , svalue_t *data UNUSED
                 , void *extra /* is a svalue_t ** */ )

/* Filter function used by mapping:m_indices() to implement the
 * m_indices() efun. It is here take advantage of the inline expansion
 * of assign_svalue_no_free().
 *
 * <key> points to a key in a mapping, <extra> points to a svalue_t*
 * pointing to a storage place. *key is assigned to **extra, *extra is
 * incremented afterwards.
 */

{
#ifdef __MWERKS__
#    pragma unused(data)
#endif
    svalue_t **svpp = (svalue_t **)extra;

    assign_svalue_no_free( (*svpp)++, key );
} /* m_indices_filter() */

/*-------------------------------------------------------------------------*/
void m_values_filter ( svalue_t *key UNUSED
                     , svalue_t *data
                     , void *extra /* is a struct mvf_info * */ )

/* Filter function used by efun m_values().
 *
 * <data> points to a data entry in a mapping, <extra> points to
 * a struct mvf_info describing the amount of data to copy, and the
 * target place. The <data> is copied to where <extra> points and <*extra>
 * is updated.
 */

{
#ifdef __MWERKS__
#    pragma unused(key)
#endif
    struct mvf_info * vip = (struct mvf_info *)extra;

    assign_rvalue_no_free( vip->svp++, data + vip->num);
} /* m_values_filter() */

/*-------------------------------------------------------------------------*/
void
m_unmake_filter ( svalue_t *key
                , svalue_t *data
                , void *extra)

/* Filter function used by efun unmkmapping().
 *
 * <key>/<data> point to key and data entry in a mapping, <extra> points to
 * a struct mvf_info describing the amount of data to copy, and the
 * target place. The <keu> and <data> is copied to where <extra> points
 * and <*extra> is updated.
 */

{
    struct mvf_info * vip = (struct mvf_info *)extra;
    int i;

    assign_svalue_no_free(vip->svp->u.vec->item + vip->num, key);
    for (i = 0; i < vip->width; i++)
        assign_rvalue_no_free(vip->svp[i+1].u.vec->item + vip->num, data+i);
    vip->num++;
} /* m_unmake_filter() */

/*-------------------------------------------------------------------------*/
static INLINE svalue_t *
find_value (int num)

/* Return the address of object-global variable number <num> in the
 * current variable block.
 *
 * <num> is the index of the variable in the current object's variable
 * array.
 */

{
    /* Make sure that we are not calling from a set_this_object()
     * context.
     */
    if (is_sto_context())
    {
        errorf("find_value: Can't execute with "
              "set_this_object() in effect.\n"
             );
    }

#ifdef DEBUG
    if (num >= current_prog->num_variables - (variable_index_offset ? current_prog->num_virtual_variables : 0))
    {
        fatal("Illegal variable access %d(%d).\n",
            num, current_prog->num_variables);
    }
#endif

    return &current_variables[num];
} /* find_value() */

/*-------------------------------------------------------------------------*/
static INLINE svalue_t *
find_virtual_value (int num)

/* For the virtually inherited variable <num> (given as index within
 * the current program's variable block) return the address of the actual
 * variable.
 *
 * If the program for this variable was inherited more than one time,
 * this function returns the address of the corresponding variable svalue
 * of the very first inheritance. If the program was inherited just once,
 * this function is identical to find_value().
 *
 * TODO: It would be nicer if the driver would 'know' here which inherit
 * TODO:: to use, either by giving the inherit index in the code, or
 * TODO:: by putting a reference to the base instance in the struct
 * TODO:: inherit.
 */

{
    return &current_object->variables[translate_virtual_variable_index(num)];
} /* find_virtual_value() */

/*-------------------------------------------------------------------------*/
int
translate_virtual_variable_index (int num)

/* For the virtually inherited variable <num> (given as index within
 * the current program's variable block) return its index in the
 * current_object's variable block.
 *
 * Because virtually inherited programs might have another offset
 * in the current_object then in current_prog (because all virtually
 * inherited variable blocks are put at the beginn of the final
 * variable block before any regular variables), we have to look
 * up the inherit of <num> in the current_object and translate
 * the variable index accordingly.
 */

{
    inherit_t *inheritp;
    program_t *progp;

    /* Make sure that we are not calling from a set_this_object()
     * context.
     */
    if (is_sto_context())
    {
        errorf("translate_virtual_variable_index: Can't execute with "
              "set_this_object() in effect.\n"
             );
    }

    /* Set inheritp to the inherited program which defines variable <num>
     */
    inheritp = current_prog->inherit;
    while
      (   inheritp->inherit_type == INHERIT_TYPE_NORMAL
       || inheritp->variable_index_offset + inheritp->prog->num_variables - inheritp->prog->num_virtual_variables <= num
       || inheritp->variable_index_offset > num)
    {
        inheritp++;
    }

    /* Get the index of the variable within the inherited program.
     */
    num = num - inheritp->variable_index_offset;

    /* Set inheritp to the first instance of this inherited program.
     * We need a virtual inherited instance, so either
     * INHERIT_TYPE_EXTRA or INHERIT_TYPE_VIRTUAL,
     * but not INHERIT_TYPE_NORMAL.
     */
    progp = inheritp->prog;

    for (inheritp = current_object->prog->inherit
       ; inheritp->prog != progp || inheritp->inherit_type == INHERIT_TYPE_NORMAL
       ; inheritp++) NOOP;

    /* Handle obsoleted inherited programs */
    while (inheritp->inherit_type & INHERIT_TYPE_MAPPED)
    {
        inherit_t *new_inheritp = current_object->prog->inherit + inheritp->updated_inherit;
        num = current_object->prog->update_index_map[num + inheritp->variable_map_offset];

        if (num >= new_inheritp->prog->num_variables - new_inheritp->prog->num_virtual_variables)
        {
            /* Dangling variable. We'll stay in that variable block. */
            num -= new_inheritp->prog->num_variables - new_inheritp->prog->num_virtual_variables;
            break;
        }

        inheritp = new_inheritp;
    }

    /* Compute the actual variable address */

    num = num + inheritp->variable_index_offset;

#ifdef DEBUG
    if (!current_object->variables
     || num >= current_object->prog->num_variables
       )
    {
        if (num)
            fatal("%s Fatal: translate_virtual_variable_index() on object %p '%s' "
                  "w/o variables, num %d\n"
                 , time_stamp(), current_object, get_txt(current_object->name)
                 , num);
        else
            errorf("%s Error: translate_virtual_variable_index() on object %p '%s' "
                  "w/o variables, num %d\n"
                 , time_stamp(), current_object, get_txt(current_object->name)
                 , num);
    }
#endif

    return num;
} /* translate_virtual_variable_index() */

/*=========================================================================*/

/*                  T Y P E S   A N D   E R R O R S                        */

/*-------------------------------------------------------------------------*/
/* The following functions deal with the readable display of LPC runtime
 * types, and of errors in general.
 *
 *   typename(type) : Return a descriptive string for a type.
 *   efun_arg_typename(type) : Return a descriptive string for the bit-
 *                    encoded type information of an efun.
 *   complete_instruction(instr) : Return the name of the given instruction,
 *                    resp. of the instruction found a the given negative
 *                    offset.
 *   raise_bad_arg(instr, arg) : Argument no. <arg> for the instruction
 *                    was bad.
 *   vefun_bad_arg(arg,sp) : Argument no. <arg> for the current vefun
 *                    was bad. Also restore inter_sp from sp.
 *   raise_arg_error(instr, arg, expected, got) : (internal) The argument
 *                    no. <arg> to the instruction did not have the
 *                    <expected> type (bit-encoded), but instead <got>
 *                    (the LPC type tag).
 *   (v)efun_gen_arg_error(arg, got, sp): Argument no. <arg> to the current
 *                    tabled (v)efun had the wrong type <got>. inter_sp is
 *                    restored from <sp>.
 *   (v)efun_arg_error(arg, expected, got, sp): Argument no. <arg> to the
 *                    current tabled (v)efun had the wrong type <got> (LPC
 *                    type tag), not the type <expected> (LPC type tag).
 *                    inter_sp is restored from <sp>.
 *   (v)efun_exp_arg_error(arg, expected, got, sp): Argument no. <arg> to the
 *                    current tabled (v)efun had the wrong type <got> (LPC
 *                    type tag), not the type <expected> (bit-encoded).
 *                    inter_sp is restored from <sp>.
 *   code_arg_error(arg, expected, got, pc, sp): Argument no. <arg> to the
 *                    current one-byte instruction had the wrong type <got>
 *                    (LPC type tag), not the type <expected> (LPC type tag).
 *                    inter_sp is restored from <sp>, inter_pc from <pc>.
 *   code_exp_arg_error(arg, expected, got, pc, sp): Argument no. <arg> to the
 *                    current one-byte instruction had the wrong type <got>
 *                    (LPC type tag), not the type <expected> (bit-encoded).
 *                    inter_sp is restored from <sp>, inter_pc from <pc>.
 *   op_arg_error(arg, expected, got, pc, sp): Argument no. <arg> to the
 *                    current one-byte operator had the wrong type <got>
 *                    (LPC type tag), not the type <expected> (LPC type tag).
 *                    inter_sp is restored from <sp>, inter_pc from <pc>.
 *   op_exp_arg_error(arg, expected, got, pc, sp): Argument no. <arg> to the
 *                    current one-byte operator had the wrong type <got>
 *                    (LPC type tag), not the type <expected> (bit-encoded).
 *                    inter_sp is restored from <sp>, inter_pc from <pc>.
 *
 *     The difference between code_... and op_... is that the op_...
 *     will use 'right' and 'left' for the argument names.
 *
 *   test_efun_args(instr, args, argp): (internal) Test the types for
 *                    <args> arguments for the given instruction, starting
 *                    at <argp> against the expected types according to
 *                    efun_lpc_types[].
 */

/*-------------------------------------------------------------------------*/
static INLINE const char *
typename_inline (int type)

/* Translate the svalue <type> into a readable string.
 */

{
    type &= ~(T_MOD_SWAPPED);

    if (type < 0
     || (size_t)type >= sizeof(svalue_typename)/sizeof(svalue_typename[0])
       )
        fatal("Unknown typevalue %d\n", type);

    return svalue_typename[type];
} /* typename_inline() */

const char * typename (int type) { return typename_inline(type); }

#define typename(type) typename_inline(type)

/*-------------------------------------------------------------------------*/
const char *
efun_arg_typename (long type)

/* Translate the bit-encoded efun argument <type> into a readable
 * string and return it. The type encoding is the one used in
 * efun_lpc_types[].
 * Result is a pointer to a static buffer.
 * TODO: this function should use snprintf() for preventing buffer overflows,
 * TODO::especially changing svalue_typename is otherwise risky.
 */

{
    static char result[400];
    int numtypes, i;
  
    /* TODO: better write into result and return the static buffer */
    if (type == TF_ANYTYPE)
        return "mixed";
  
    result[0] = '\0';
    numtypes = sizeof(svalue_typename)/sizeof(svalue_typename[0]);

    for (i = 0; i < numtypes; i++)
    {
        if ((1 << i) & type)
        {
            if (result[0] != '\0')
                strcat(result, "/");
            strcat(result, typename(i));
        }
        type &=~(1 << i);
    }

    if (type != 0)
    {
        char tmp[100];
        if (result[0] != '\0')
            strcat(result, "/");
        sprintf(tmp, "unknown %lx", type);
        strcat(result, tmp);
    }
    return (const char *)result;
} /* efun_arg_typename() */

/*-------------------------------------------------------------------------*/
static INLINE int
complete_instruction (int instr)

/* If <instr> is negative, read the current instruction from
 * inter_pc - <instr> and return it; otherwise return <instr> itself.
 */

{
    if (instr < 0)
    {
        /* Find and decode the actual instruction at the given offset */
        bytecode_p pc = inter_pc + instr;

        instr = *pc;
        switch(instr)
        {
        case F_EFUN0:  instr = pc[1] + EFUN0_OFFSET; break;
        case F_EFUN1:  instr = pc[1] + EFUN1_OFFSET; break;
        case F_EFUN2:  instr = pc[1] + EFUN2_OFFSET; break;
        case F_EFUN3:  instr = pc[1] + EFUN3_OFFSET; break;
        case F_EFUN4:  instr = pc[1] + EFUN4_OFFSET; break;
        case F_EFUNV:  instr = pc[1] + EFUNV_OFFSET; break;
        default:
            /* This is the instruction code we need */
            NOOP;
            break;
        }
    }

    return instr;
} /* complete_instruction() */

/*-------------------------------------------------------------------------*/
static INLINE void
raise_bad_arg (int instr, int arg)
  NORETURN;

static INLINE void
raise_bad_arg (int instr, int arg)

/* The argument <arg> to <instr> was unusable for some reason.
 * If <instr> is negative, the instruction code is read from
 * inter_pc - <instr>; otherwise it is the instruction code itself.
 *
 * inter_sp and inter_pc are assumed to be correct.
 * Raise a proper error.
 */

{
    instr = complete_instruction(instr);

    errorf("Bad argument %d to %s().\n", arg, get_f_name(instr));
    /* NOTREACHED */
} /* raise_bad_arg() */

/*-------------------------------------------------------------------------*/
void
vefun_bad_arg (int arg, svalue_t *sp)

/* The argument <arg> to the current tabled vefun was unusable.
 * inter_pc is assumed to be correct, inter_sp will be set from <sp>.
 */

{
    inter_sp = sp;
    raise_bad_arg(-2, arg);
    /* NOTREACHED */
} /* vefun_bad_arg() */

/*-------------------------------------------------------------------------*/
static INLINE void
raise_arg_error (int instr, int arg, long expected, int got)
  NORETURN;

static INLINE void
raise_arg_error (int instr, int arg, long expected, int got)

/* The argument <arg> to <instr> had the wrong type: expected was the
 * type <expected> (bit-encoded as in the efun_lpc_types[]), but
 * it got the type <got> (the svalue type tag).
 * If <instr> is negative, the instruction code is read from
 * inter_pc - <instr>; otherwise it is the instruction code itself.
 *
 * If <expected> is 0, the expected type is read from the
 * instrs[] table.
 *
 * inter_sp and inter_pc are assumed to be correct.
 * Raise a proper error.
 */

{
    instr = complete_instruction(instr);

    if (!expected)
        expected = efun_lpc_types[instrs[instr].lpc_arg_index];

    errorf("Bad arg %d to %s(): got '%s', expected '%s'.\n"
         , arg, get_f_name(instr), typename(got), efun_arg_typename(expected)
         );
    /* NOTREACHED */
} /* raise_arg_error() */

/*-------------------------------------------------------------------------*/
void
efun_gen_arg_error (int arg, int got, svalue_t *sp)

/* The argument <arg> to the current tabled efun had the wrong type <got>.
 * inter_pc is assumed to be correct, inter_sp will be set from <sp>.
 */

{
    inter_sp = sp;
    raise_arg_error(-2, arg, 0, got);
    /* NOTREACHED */
} /* efun_gen_arg_error() */

/*-------------------------------------------------------------------------*/
void
vefun_gen_arg_error (int arg, int got, svalue_t *sp)

/* The argument <arg> to the current tabled vefun had the wrong type <got>.
 * inter_pc is assumed to be correct, inter_sp will be set from <sp>.
 */

{
    inter_sp = sp;
    raise_arg_error(-2, arg, 0, got);
    /* NOTREACHED */
} /* vefun_gen_arg_error() */

/*-------------------------------------------------------------------------*/
void
efun_arg_error (int arg, int expected, int got, svalue_t *sp)

/* The argument <arg> to the current tabled efun had the wrong type:
 * expected was the type <expected>, but it got the type <got>
 * (both values are the svalue type tag).
 * inter_pc is assumed to be correct, inter_sp will be set from <sp>.
 */

{
    inter_sp = sp;
    raise_arg_error(-2, arg, 1 << expected, got);
    /* NOTREACHED */
} /* efun_arg_error() */

/*-------------------------------------------------------------------------*/
void
efun_exp_arg_error (int arg, long expected, int got, svalue_t *sp)

/* The argument <arg> to the current tabled efun had the wrong type:
 * expected was the type <expected> (given as bitflags), but it got the type
 * <got> (given as svalue type tag).
 * inter_pc is assumed to be correct, inter_sp will be set from <sp>.
 */

{
    inter_sp = sp;
    raise_arg_error(-2, arg, expected, got);
    /* NOTREACHED */
} /* efun_arg_error() */

/*-------------------------------------------------------------------------*/
void
vefun_arg_error (int arg, int expected, int got, svalue_t *sp)

/* The argument <arg> to the current tabled vefun had the wrong type:
 * expected was the type <expected>, but it got the type <got>
 * (both values are the svalue type tag).
 * inter_pc is assumed to be correct, inter_sp will be set from <sp>.
 */

{
    inter_sp = sp;
    raise_arg_error(-2, arg, 1 << expected, got);
    /* NOTREACHED */
} /* vefun_arg_error() */

/*-------------------------------------------------------------------------*/
void
vefun_exp_arg_error (int arg, long expected, int got, svalue_t *sp)

/* The argument <arg> to the current tabled vefun had the wrong type:
 * expected was the type <expected> (in the bit-encoded format), but
 * it got the type <got> (the svalue type tag).
 * inter_pc is assumed to be correct, inter_sp will be set from <sp>.
 */

{
    inter_sp = sp;
    raise_arg_error(-2, arg, expected, got);
    /* NOTREACHED */
} /* vefun_exp_arg_error() */

/*-------------------------------------------------------------------------*/
static INLINE void
code_exp_arg_error (int arg, long expected, int got, bytecode_p pc, svalue_t *sp)
  NORETURN;

static INLINE void
code_exp_arg_error (int arg, long expected, int got, bytecode_p pc, svalue_t *sp)

/* The argument <arg> to the current one-byte instruction had the wrong type:
 * expected was the type <expected> (in bit-flag encoding), but it got the
 * type <got> (the svalue type tag).
 * inter_pc will be set from <pc>, inter_sp will be set from <sp>.
 */

{
    int instr;

    inter_sp = sp;
    inter_pc = pc;

    instr = complete_instruction(-1);

    errorf("Bad arg %d to %s: got '%s', expected '%s'.\n"
         , arg, get_f_name(instr), typename(got), efun_arg_typename(expected)
         );
    /* NOTREACHED */
} /* code_exp_arg_error() */

/*-------------------------------------------------------------------------*/
static INLINE void
code_arg_error (int arg, int expected, int got, bytecode_p pc, svalue_t *sp)
  NORETURN;

static INLINE void
code_arg_error (int arg, int expected, int got, bytecode_p pc, svalue_t *sp)

/* The argument <arg> to the current one-byte instruction had the wrong type:
 * expected was the type <expected>, but it got the type <got>
 * (both values are the svalue type tag).
 * inter_pc will be set from <pc>, inter_sp will be set from <sp>.
 */

{
    code_exp_arg_error(arg, 1 << expected, got, pc,sp);
    /* NOTREACHED */
} /* code_arg_error() */

/*-------------------------------------------------------------------------*/
static INLINE void
op_exp_arg_error (int arg, long expected, int got, bytecode_p pc, svalue_t *sp)
  NORETURN;

static INLINE void
op_exp_arg_error (int arg, long expected, int got, bytecode_p pc, svalue_t *sp)

/* The argument <arg> to the current one-byte operator had the wrong type:
 * expected was the type <expected> (bit-encoded as in efun_lpc_types[]),
 * but it got the type <got> (the svalue type tag).
 * inter_pc will be set from <pc>, inter_sp will be set from <sp>.
 *
 * This function is to be used with binary operators like + or *; the
 * error message will say 'left' and 'right' instead of 'arg 1' or 'arg 2'.
 */

{
    int instr;

    inter_sp = sp;
    inter_pc = pc;

    instr = complete_instruction(-1);

    errorf("Bad %s arg to %s: got '%s', expected '%s'.\n"
         , arg == 1 ? "left" : "right"
         , get_f_name(instr), typename(got), efun_arg_typename(expected)
         );
    /* NOTREACHED */
} /* op_exp_arg_error() */

/*-------------------------------------------------------------------------*/
static INLINE void
op_arg_error (int arg, int expected, int got, bytecode_p pc, svalue_t *sp)
  NORETURN;

static INLINE void
op_arg_error (int arg, int expected, int got, bytecode_p pc, svalue_t *sp)

/* The argument <arg> to the current one-byte operator had the wrong type:
 * expected was the type <expected>, but it got the type <got>
 * (both values are the svalue type tag).
 * inter_pc will be set from <pc>, inter_sp will be set from <sp>.
 *
 * This function is to be used with binary operators like + or *; the
 * error message will say 'left' and 'right' instead of 'arg 1' or 'arg 2'.
 */

{
    op_exp_arg_error(arg, 1 << expected, got, pc, sp);
    /* NOTREACHED */
} /* op_arg_error() */

/*-------------------------------------------------------------------------*/
static INLINE void
test_efun_args (int instr, int args, svalue_t *argp)

/* Test the types of the <args> arguments for (v)efun <instr> starting at
 * <argp> for their correct types according to efun_lpc_types[].
 * Raise an error if they aren't correct (requires inter_pc and inter_sp
 * to be valid).
 */

{
    int i;
    long * typep;

    typep = &(efun_lpc_types[instrs[instr].lpc_arg_index]);
    for (i = 1; i <= args; i++, argp++)
    {
        long exp_type = *typep;
        long act_type = argp->type;

        if (!exp_type && instrs[instr].max_arg < 0)
            exp_type = typep[-1];
        else
            typep++;

        if (argp->type == T_NUMBER && !argp->u.number && (exp_type & TF_NULL))
            continue;

        if (!(exp_type & (1 << act_type)))
            raise_arg_error(instr, i, exp_type, act_type);
    }
} /* test_efun_args() */

/*-------------------------------------------------------------------------*/
static INLINE Bool
check_rtt_compatibility_inl(lpctype_t *formaltype, svalue_t *svp, lpctype_t **svptype)
// This function checks if <formal_type> and the svalue pointed to by <svp>
// are compatible (that means, it is allowed to assign *svp to an LPC variable
// having the type described by <formal_type>. The function handles lvalues,
// but the lvalue property is ignored for assessing the compatibility.
// returns MY_TRUE if *<svp> is compatible to <formal_type>, MY_FALSE otherwise.
// If <svptype> is not NULL, the function stores the type of the value there,
// which is useful for error messages. (The caller must free it afterwards.)
{
    lpctype_t *valuetype = NULL;
    svalue_t *bsvp = get_rvalue_no_collapse(svp, NULL);

    if (bsvp == NULL)
    {
        assert(svp->type == T_LVALUE && svp->x.lvalue_type == LVALUE_PROTECTED_RANGE);
        bsvp = &(svp->u.protected_range_lvalue->vec);
    }

    // No type saved? Anything is possible...
    if (!formaltype)
    {
        if (svptype)
            *svptype = lpctype_unknown;
        return MY_TRUE;
    }

    // Shortcut, mixed accepts anything.
    if (formaltype == lpctype_unknown || formaltype == lpctype_mixed)
    {
        if (svptype)
            *svptype = formaltype;
        return MY_TRUE;
    }

    // Now, let's determine the compiler type corresponding to svp.
    switch(bsvp->type)
    {
        // These should anyway not happen...
        case T_INVALID:
        case T_CALLBACK:
        case T_ERROR_HANDLER:
        case T_BREAK_ADDR:
        case T_LVALUE:
            if (svptype)
                *svptype = lpctype_unknown;
            return MY_FALSE;

        case T_STRING:
            valuetype = lpctype_string;
            break;

        case T_BYTES:
            valuetype = lpctype_bytes;
            break;

        case T_POINTER:
        {
            // We have to look into it...
            // (int|string)* is different from int*|string*.
            // In the later case we have to check both cases independently.
            lpctype_t* head = formaltype;
            lpctype_t* svpelement = NULL;

            while (true)
            {
                // Walk through all possibilities of <formaltype>.
                lpctype_t *member = head->t_class == TCLASS_UNION ? head->t_union.member : head;
                if (member->t_class == TCLASS_ARRAY)
                {
                    lpctype_t *element = member->t_array.element;
                    vector_t *vec = bsvp->u.vec;
                    size_t i, size = VEC_SIZE(vec);
                    Bool correct = MY_TRUE;

                    for (i=0; i < size; i++)
                    {
                        lpctype_t *svpresult;

                        if(!check_rtt_compatibility_inl(element, vec->item + i, svptype ? &svpresult : NULL))
                            correct = MY_FALSE;

                        // mixed is returned when the element is '0'.
                        // Ignore that when determining the svalue's type.
                        // We also don't need to call free_lpctype() for it.
                        if (svptype && svpresult != lpctype_mixed)
                        {
                            lpctype_t *oldval = svpelement;
                            svpelement = get_union_type(svpelement, svpresult);
                            free_lpctype(oldval);
                            free_lpctype(svpresult);
                        }

                        // Abort if this element is already incompatible
                        // and we don't have to determine the full type.
                        if (!correct && !svptype)
                            break;
                    }

                    if (correct)
                    {
                        if (svptype)
                        {
                            // svpelement may be NULL if the array is empty or contains zeroes.
                            *svptype = get_array_type(svpelement ? svpelement : lpctype_mixed);
                            free_lpctype(svpelement);
                        }
                        return MY_TRUE;
                    }
                }

                if (head->t_class == TCLASS_UNION)
                    head = head->t_union.head;
                else
                {
                    if (svptype)
                    {
                        *svptype = get_array_type(svpelement ? svpelement : lpctype_mixed);
                        free_lpctype(svpelement);
                    }

                    return MY_FALSE; // No valid type found.
                }
            }
        }

        case T_QUOTED_ARRAY:
            valuetype = lpctype_quoted_array;
            break;

        case T_MAPPING:
            valuetype = lpctype_mapping;
            break;

        case T_CLOSURE:
            valuetype = lpctype_closure;
            break;

        case T_SYMBOL:
            valuetype = lpctype_symbol;
            break;

        case T_NUMBER:
            // Zero (0) is considered to be an element of any type.
            if (bsvp->u.number == 0)
            {
                if (svptype)
                    *svptype = lpctype_mixed;
                return MY_TRUE;
            }

            valuetype = lpctype_int;
            break;

        case T_FLOAT:
            valuetype = lpctype_float;
            break;

        case T_OBJECT:
            valuetype = lpctype_object;
            break;

        case T_STRUCT:
            valuetype = get_struct_type(bsvp->u.strct->type);
            break;
    }

    if (valuetype)
    {
        Bool result = lpctype_contains(valuetype, formaltype);

        if (svptype)
            *svptype = ref_lpctype(valuetype);

        clean_struct_type(valuetype);
        free_lpctype(valuetype);

        return result;
    }

    if (svptype)
        *svptype = lpctype_unknown;

    // Fall-through
    return MY_FALSE;
} // check_rtt_compatibility()

Bool
check_rtt_compatibility(lpctype_t *formaltype, svalue_t *svp) 
{
    return check_rtt_compatibility_inl(formaltype, svp, NULL);
}
#define check_rtt_compatibility(ft, svp) check_rtt_compatibility_inl(ft, svp, NULL)

lpctype_t*
get_rtt_type(lpctype_t *formaltype, svalue_t *svp)
{
    lpctype_t *result;
    check_rtt_compatibility_inl(formaltype, svp, &result);
    return result;
}

/*-------------------------------------------------------------------------*/
static INLINE void
check_function_args(int fx, program_t *progp, bytecode_p funstart)
/* Check the argument types of the function <fx> in <progp>.
 * The proper number of arguments must be on the stack, so this must be called
 * after setup_new_frame2().
 * <inter_sp> is assumed to be the current top of the stack, <funstart> the 
 * pointer to number_formal_args in the function header.
 * If runtime checks are requested for the program <progp> (pragma rtt_checks)
 * and there are type information for the arguments, this function will check
 * the arguments on the stack for compatibility and call errorf() if not.
 */
{
    // Runtime type checks: check if rtt_checks are requested and we have any
    // argument type information.
    if (progp->flags & P_RTT_CHECKS 
        && progp->type_start && progp->type_start[fx] != INDEX_START_NONE)
    {
        // check for the correct argument types
        lpctype_t **arg_type = progp->argument_types + progp->type_start[fx];
        svalue_t *firstarg = inter_sp - csp->num_local_variables + 1;
        function_t *header = current_prog->function_headers + FUNCTION_HEADER_INDEX(funstart);

        int formal_args = header->num_arg;
        int i = 0;
        while (i < formal_args)
        {
            // do the types match (in case of structs also the structure)
            // or is the formal argument of type TYPE_ANY (mixed)?
            if (!check_rtt_compatibility(arg_type[i], firstarg+i))
            {
                // How many control stack frames to remove.
                int num_csf = 0;

                // Determine the lpctype of arg_type[i] for a better error message.
                static char buff[512];
                lpctype_t *realtype = get_rtt_type(arg_type[i], firstarg+i);
                get_lpctype_name_buf(realtype, buff, sizeof(buff));
                free_lpctype(realtype);

                // raise error
                // At this point, a control frame was already created for this call.
                // To attribute error to caller, pop that one from the control stack.
                // But there are some special cases to take care of...
                if (csp > CONTROL_STACK + 1)
                {
                    // at least 3 frames on the stack. We can get rid of at least one of them.
                    num_csf++;
                    // int_call_lambda() pushes a dummy control frame with funstart==0. This
                    // has to removed as well if existing.
                    // (Assumption: there are not other control stack frames with funstart==0.)
                    if (!csp[-1].funstart)
                        num_csf++;
                }
                else if (csp == CONTROL_STACK + 1)
                {
                    // exactly 2 frames on the stack. We might pop one, but only if the
                    // bottom (remaining) frame is NOT a dummy control frame from
                    // int_call_lambda(). If it is the dummy frame, we would be at the 
                    // start of a top-level evaluation and both frames have to remain.
                    // In that case, we have to set inter_pc to a valid value.
                    if (CONTROL_STACK->funstart)
                        num_csf++;
                    else
                        inter_pc = funstart;
                }
                else
                {
                    // this is a top-level call. This one frame must stay. But we have to set 
                    // inter_pc because it might be either 0 because no code was evaluated yet 
                    // or it is a left-over from last execution.
                    inter_pc = funstart;
                }

                if (progp->flags & P_WARN_RTT_CHECKS)
                {
                    // We need the stack frames to continue execution.
                    // But now they are in the way of a good error message...
                    struct control_stack saved_csf[3];
                    assert(num_csf < 3);

                    push_control_stack(inter_sp, inter_pc, inter_fp, inter_context);
                    for (int j = 0; j <= num_csf; j++)
                        saved_csf[j] = *(csp--);
                    csp++;

                    // This one we saved, but also pull back into reality.
                    pop_control_stack();

                    // warnf will return (errors are caught).
                    warnf("Bad arg %d to %s(): got '%s', expected '%s'.\n"
                           , i+1, get_txt(header->name), buff,
                           get_lpctype_name(arg_type[i]));

                    for (int j = num_csf; j >= 0; j--)
                        *(++csp) = saved_csf[j];

                    pop_control_stack();
                }
                else
                {
                    for (int j = 0; j < num_csf; j++)
                        pop_control_stack();

                    errorf("Bad arg %d to %s(): got '%s', expected '%s'.\n"
                           , i+1, get_txt(header->name), buff,
                           get_lpctype_name(arg_type[i]));
                }
            }
            ++i;
        }
    }
} // check_function_args()

/*-------------------------------------------------------------------------*/
/* general errorhandler */
static void
generic_error_handler( error_handler_t * arg)
/* The error handler: free the allocated buffer and the errorhandler structure.
 * Note: it is static, but the compiler will have to emit a function and 
 * symbol for this because the address of the function is taken and it is 
 * therefore not suitable to be inlined.
 */
{
  mem_error_handler_t *handler = (mem_error_handler_t *)arg;
  if (handler->buff)
    xfree(handler->buff);
  xfree(handler);
} /* general_error_handler() */

/*-------------------------------------------------------------------------*/
void *
xalloc_with_error_handler(size_t size)
/* Allocates <size> bytes from the heap. Additionally an error handler is
 * pushed onto the value stack so that the requested memory is safely freed,
 * either by manually freeing the svalue on the stack or during stack 
 * unwinding during errorf().
 * inter_sp has to point to the top-of-stack before calling and is updated to
 * point to the error handler svalue (new top-of-stack)!
 */
{
  void *buffer;
  mem_error_handler_t *handler;
  /* get the memory for the handler first and fail if out-of-memory */
  handler = xalloc(sizeof(*handler));
  if (!handler)
  {
    return NULL;
  }
  /* then get the requested memory - upon error de-allocate the handler */
  buffer = xalloc(size);
  if (!buffer)
  {
    xfree(handler);
    return NULL;
  }
  handler->buff = buffer;
  /* now push error handler onto the value stack */
  push_error_handler(generic_error_handler, &(handler->head));
  return buffer;
} /* alloc_with_error_handler */


/*=========================================================================*/
/*-------------------------------------------------------------------------*/
Bool
privilege_violation (string_t *what, svalue_t *arg, svalue_t *sp)

/* Call the mudlib to check for a privilege violation:
 *
 *   master->privilege_violation(what, current_object, arg)
 *
 * where <what> describes the type of the violation (uncounted string ref),
 * and <where> is the data used in the violation.
 * <sp> is the current stack setting.
 *
 * If the apply returns a positive number, the privilege is granted and
 * the function returns TRUE.
 * If the apply returns 0, the privilege is gently denied and the function
 * returns FALSE.
 * If the apply returns something else, or if the lfun doesn't exist,
 * an error is raised.
 *
 * If the current_object is the master or simul_efun object, this function
 * immediately returns TRUE.
 *
 * <inter_sp> is updated to <sp>, <inter_pc> is assumed to be correct.
 */

{
    return privilege_violation2(what, arg, NULL, sp);
} /* privilege_violation() */

/*-------------------------------------------------------------------------*/
Bool
privilege_violation2 ( string_t *what, svalue_t *arg, svalue_t *arg2
                     , svalue_t *sp)

/* Call the mudlib to check for a privilege violation:
 *
 *   master->privilege_violation(what, current_object, arg, arg2)
 *
 * where <what> describes the type of the violation (uncounted string ref),
 * and <arg>, <arg2> is the data used in the violation.
 * <sp> is the current stack setting.
 *
 * <arg2> may be NULL and is then ignored.
 *
 * If the apply returns a positive number, the privilege is granted and
 * the function returns TRUE.
 * If the apply returns 0, the privilege is gently denied and the function
 * returns FALSE.
 * If the apply returns something else, or if the lfun doesn't exist,
 * an error is raised.
 *
 * If the current_object is the master or simul_efun object, this function
 * immediately returns TRUE.
 *
 * <inter_sp> is updated to <sp>, <inter_pc> is assumed to be correct.
 */

{
    svalue_t *svp;
    int num_arg = 3;

    /* Trusted objects */
    if (current_object == master_ob) return MY_TRUE;
    if (current_object == simul_efun_object) return MY_TRUE;

    /* Setup and call the lfun */
    push_ref_string(sp, what);
    push_ref_valid_object(sp, current_object, "privilege violation");
    sp++;
    assign_svalue_no_free(sp, arg);
    if (arg2 != NULL)
    {
        sp++;
        assign_svalue_no_free(sp, arg2);
        num_arg++;
    }
    inter_sp = sp;
    svp = apply_master(STR_PRIVILEGE, num_arg);

    /* Is there a lfun to call? */
    if (!svp || svp->type != T_NUMBER || svp->u.number < 0)
    {
        inter_sp = sp-num_arg;
        errorf("privilege violation: %s\n", get_txt(what));
        /* TODO: Print full args and types */
    }

    /* Return the result */
    return svp->u.number > 0;
} /* privilege_violation2() */

/*-------------------------------------------------------------------------*/
Bool
privilege_violation4 ( string_t *what,    object_t *whom
                     , string_t *how_str, int how_num
                     , svalue_t *sp)

/* Call the mudlib to check for a privilege violation:
 *
 *   !whom:
 *       master->privilege_violation(what, current_object, how_str, how_num)
 *   whom && how_str:
 *       master->privilege_violation(what, current_object, whom, how_str)
 *   whom && !how_str:
 *       master->privilege_violation(what, current_object, whom, how_num)
 *
 * where <what> describes the type of the violation, and <whom>/<how_str>/
 * <how_num> are data used in the violation. <sp> is the current stack setting.
 * All strings are not counted.
 *
 * If the apply returns a positive number, the privilege is granted and
 * the function returns TRUE.
 * If the apply returns 0, the privilege is gently denied and the function
 * returns FALSE.
 * If the apply returns something else, or if the lfun doesn't exist,
 * an error is raised.
 *
 * If the current_object is the master or simul_efun object, this function
 * immediately returns TRUE.
 *
 * If the lfun doesn't exist, or returns anything else but a positive
 * number, an error is raised.
 *
 * <inter_sp> is updated to <sp>, <inter_pc> is assumed to be correct.
 */

{
    svalue_t *svp;

    /* Trust these objects */
    if (current_object == master_ob) return MY_TRUE;
    if (current_object == simul_efun_object) return MY_TRUE;

    /* Set up the lfun call */

    push_ref_string(sp, what);
    push_ref_valid_object(sp, current_object, "privilege_violation");
    if (!whom)
    {
        if (how_str)
            push_ref_string(sp, how_str);
        else
            push_number(sp, 0);
        push_number(sp, how_num);
    }
    else
    {
        push_ref_object(sp, whom, "privilege_violation");
        if (how_str)
            push_ref_string(sp, how_str);
        else
            push_number(sp, how_num);
    }
    inter_sp = sp;
    svp = apply_master(STR_PRIVILEGE, 4);

    /* Was it the proper lfun to call? */
    if (!svp || svp->type != T_NUMBER || svp->u.number < 0)
    {
        inter_sp = sp-4;
        errorf("privilege violation : %s\n", get_txt(what));
        /* TODO: Print full args and types */
    }

    /* Return the result */
    return svp->u.number > 0;
} /* privilege_violation4() */

/*-------------------------------------------------------------------------*/
Bool
privilege_violation_n ( string_t *what, object_t *whom, svalue_t *sp, int num_arg)

/* Call the mudlib to check for a privilege violation:
 *
 *   master->privilege_violation(what, current_object, whom,
 *                               sp[-num_arg+1], ...., sp)
 *
 * where <what> describes the type of the violation, and <whom> and the last
 * <num_arg> values of the stack are data used in the violation. <sp> is 
 * also the current stack setting. All strings are not counted.
 *
 * If the apply returns a positive number, the privilege is granted and
 * the function returns TRUE.
 * If the apply returns 0, the privilege is gently denied and the function
 * returns FALSE.
 * If the apply returns something else, or if the lfun doesn't exist,
 * an error is raised.
 *
 * If the current_object is the master or simul_efun object, this function
 * immediately returns TRUE.
 *
 * If the lfun doesn't exist, or returns anything else but a positive
 * number, an error is raised.
 *
 * <inter_sp> is updated to <sp>, <inter_pc> is assumed to be correct.
 */

{
    svalue_t *svp, *arg;
    int num;

    /* Trust these objects */
    if (current_object == master_ob) return MY_TRUE;
    if (current_object == simul_efun_object) return MY_TRUE;

    /* Set up the lfun call */

    arg = sp + 1 - num_arg;

    push_ref_string(sp, what);
    push_ref_valid_object(sp, current_object, "privilege_violation");
    if (!whom)
    {
        push_number(sp, 0);
    }
    else
    {
        push_ref_object(sp, whom, "privilege_violation");
    }

    for (num = num_arg; num--; arg++)
    {
        sp++;
        assign_svalue_no_free(sp,  arg);
    }

    inter_sp = sp;
    svp = apply_master(STR_PRIVILEGE, 3 + num_arg);

    /* Was it the proper lfun to call? */
    if (!svp || svp->type != T_NUMBER || svp->u.number < 0)
    {
        inter_sp = sp - 3 - num_arg;
        errorf("privilege violation : %s\n", get_txt(what));
        /* TODO: Print full args and types */
    }

    /* Return the result */
    return svp->u.number > 0;
} /* privilege_violation_n() */

/*-------------------------------------------------------------------------*/
static Bool
trace_test (int b)

/* Test if tracing of the given option(s) <b> is allowed right now.
 * The function tests the options <b> against what the current interactive
 * requested, and if a trace_prefix is given, if the prefix matches the
 * name of the current object.
 */

{
    interactive_t *ip;

    return current_interactive
        && O_SET_INTERACTIVE(ip, current_interactive)
        && (ip->trace_level & b)
        && (ip->trace_prefix == NULL
            || (current_object
                && mstrprefixed(ip->trace_prefix, current_object->name)))
    ;
} /* trace_test() */

/*-------------------------------------------------------------------------*/
static void
do_trace (char *msg, char *fname, char *post)

/* If not in a heartbeat, or if heartbeat tracing is allowed, generate
 * a tracemessage of the form '<tracedepth> <msg> <objname> <fname> <post>'
 * and print it to the player using add_message().
 *
 * Don't do anything if the current command_giver is not interactive.
 *
 * <obj_name> is filled in only if TRACE_OBJNAME is requested, else
 * the empty string is used.
 */

{
    char buf[10000];
    char *objname;

    if (!TRACEHB())
        return;
    objname = TRACETST(TRACE_OBJNAME)
              ? (current_object && current_object->name
                   ? get_txt(current_object->name)
                   : "?")
              : "";
    sprintf(buf, "*** %d %*s %s %s %s%s", tracedepth, tracedepth, ""
               , msg, objname, fname, post);
    add_message("%s", buf);
#ifdef DEBUG
    add_message_flush();
#endif
} /* do_trace() */

/*-------------------------------------------------------------------------*/
static void
do_trace_call (bytecode_p funstart, Bool is_lambda)

/* Trace a call to the function starting at <funstart>.
 */

{
    if (!++traceing_recursion || !TRACE_IS_INTERACTIVE()) /* Do not recurse! */
    {
        int num_args;
        int save_var_ix_offset = variable_index_offset;
          /* TODO: Might be clobbered, but where? */

        /* Trace the function itself */
        if (is_lambda)
        {
            lambda_t *l = current_lambda.u.lambda;
            if (current_lambda.x.closure_type == CLOSURE_BOUND_LAMBDA)
                l = l->function.lambda;

            do_trace("Call direct ", "lambda-closure", " ");
            num_args = l->function.code.num_arg;
        }
        else
        {
            function_t *header = current_prog->function_headers + FUNCTION_HEADER_INDEX(funstart);
            do_trace("Call direct ", get_txt(header->name), " ");
            num_args = header->num_arg;
        }

        /* If requested, also trace the arguments */
        if (TRACEHB())
        {
            if (TRACETST(TRACE_ARGS))
            {
                int i;
                svalue_t *svp;

                add_message(" with %d arguments: ", num_args);
                svp = inter_fp;
                for (i = num_args; --i >= 0; )
                {
                    print_svalue(svp++);
                    add_message(" ");
                }
            }
            add_message("\n");
        }
        variable_index_offset = save_var_ix_offset;
    }
    traceing_recursion--;
} /* do_trace_call() */

/*-------------------------------------------------------------------------*/
static void
do_trace_return (svalue_t *sp)

/* Trace the return from a function call; <sp> is the current stack pointer,
 * pointing to the result.
 */

{
    if (!++traceing_recursion || !TRACE_IS_INTERACTIVE())
    {
        if (trace_test(TRACE_RETURN))
        {
            inter_sp = sp;
            do_trace("Return", "", "");
            if (TRACEHB()) {
                if (TRACETST(TRACE_ARGS)) {
                    add_message(" with value: ");
                    print_svalue(sp);
                }
                add_message("\n");
            }
        }
    }
    traceing_recursion--;

    /* If requested, (re)activate TRACE_EXEC */
    SET_TRACE_EXEC();
}

/*-------------------------------------------------------------------------*/
struct longjump_s *
push_error_context (svalue_t *sp, int catch_flags)

/* Create a catch recovery context, using <sp> as the stackpointer to save,
 * link it into the recovery stack and return the longjmp context struct.
 * The actual type of the catch context is determined by the <catch_flags>.
 */

{
    struct catch_context *p;

    p = xalloc (sizeof *p);
    p->save_sp = sp;
    p->save_csp = csp;
    p->save_command_giver = command_giver;
    p->recovery_info.rt.last = rt_context;
    p->recovery_info.rt.type = ERROR_RECOVERY_CATCH;
    p->recovery_info.flags = catch_flags;
    p->catch_value.type = T_INVALID;
    rt_context = (rt_context_t *)&p->recovery_info.rt;
    return &p->recovery_info.con;
} /* push_error_context() */

/*-------------------------------------------------------------------------*/
void
pop_error_context (void)

/* Pop and discard the top entry in the error recovery stack, assuming
 * that it's a catch recovery entry.
 *
 * This function is called when the catch() completed normally.
 */

{
    struct catch_context *p;

    p = (struct catch_context *)rt_context;

#ifdef DEBUG
    if (!ERROR_RECOVERY_CAUGHT(p->recovery_info.rt.type))
        fatal("Catch: runtime stack underflow");
    if (csp != p->save_csp-1)
        fatal("Catch: Lost track of csp");
    /* Note: the command_giver might have changed (with the exec() efun),
     * so testing it is of no use.
     */
#endif
    rt_context = p->recovery_info.rt.last;
    xfree(p);
} /* pop_error_context() */

/*-------------------------------------------------------------------------*/
svalue_t *
pull_error_context (svalue_t *sp, svalue_t *msg)

/* Restore the context saved by a catch() after a throw() or runtime error
 * occurred. <sp> is the current stackpointer and is used to pop the elements
 * pushed since the catch().
 *
 * The function pops the topmost recovery entry, which must be the catch
 * recovery entry, restores the important global variables and returns
 * the saved stack pointer.
 *
 * If <msg> is not NULL the caught error message is put there.
 */

{
    struct catch_context *p;
    struct control_stack *csp2;

    p = (struct catch_context *)rt_context;

    if (!ERROR_RECOVERY_CAUGHT(p->recovery_info.rt.type))
        fatal("Catch: runtime stack underflow");

    /* If there was a call_other() or similar, previous_ob and current_object
     * must be restored. For this, find the control frame where the call
     * occurred and get the proper values from there.
     */
    csp2 = p->save_csp;
    while (++csp2 <= csp)
    {
        if (csp2->extern_call)
        {
            previous_ob = csp2->prev_ob;
            current_object = csp2->ob;
            break;
        }
    }

    /* If there was a lambda call, we have to restore current_lambda */
    for (csp2 = csp; csp2 >p->save_csp; csp2--)
    {
        if (current_lambda.type == T_CLOSURE)
            free_closure(&current_lambda);
        current_lambda = csp2->lambda;
    }

    /* Restore the global variables and the evaluator stack */
    csp = p->save_csp;
    pop_n_elems(sp - p->save_sp);
    command_giver = p->save_command_giver;
    
    /* Save the error message */
    if (msg)
        transfer_svalue_no_free(msg, &p->catch_value);
    else
        free_svalue(&p->catch_value);

    /* Remove the context from the context stack */
    rt_context = p->recovery_info.rt.last;
    xfree(p);

    return sp;
} /* pull_error_context() */

/*-------------------------------------------------------------------------*/
void
transfer_error_message (svalue_t *v, rt_context_t *rt)
 /* Saves the message <v> in the error context <rt> assuming that
  * it's a catch recovery context. <v> is freed afterwards.
  */
{
    struct catch_context *p;

    p = (struct catch_context *)rt;
    transfer_svalue_no_free(&p->catch_value, v);
}

/*-------------------------------------------------------------------------*/
void
push_control_stack ( svalue_t   *sp
                   , bytecode_p  pc
                   , svalue_t   *fp
                   , svalue_t   *context
                   )

/* Push the current execution context onto the control stack.
 * On stack overflow, raise a 'too deep recursion' error.
 */

{

    /* Check for overflow */
    if (csp >= &CONTROL_STACK[MAX_USER_TRACE-1])
    {
        if (!num_error || csp == &CONTROL_STACK[MAX_TRACE-1])
        {
            ERRORF(("Too deep recursion: depth %"PRIdMPINT
                    ", limit %d user/%d max.\n"
                   , (mp_int)(csp - CONTROL_STACK + 1)
                   , MAX_USER_TRACE, MAX_TRACE));
        }
    }

    /* Move csp to the next entry and fill it with the current context
     */
    csp++;

    /* csp->funstart  has to be set later, it is used only for tracebacks. */
    csp->fp = fp;
    csp->context = context;
    csp->prog = current_prog;
    csp->lambda = current_lambda; put_number(&current_lambda, 0);
    /* csp->extern_call = MY_FALSE; It is set by eval_instruction() */
    csp->catch_call = MY_FALSE;
    csp->pc = pc;
    csp->function_index_offset = function_index_offset;
    csp->variable_index_offset = variable_index_offset;
    csp->current_variables = current_variables;
    csp->break_sp = break_sp;
#ifdef EVAL_COST_TRACE
    csp->eval_cost = eval_cost;
#endif
} /* push_control_stack() */

/*-------------------------------------------------------------------------*/
void
pop_control_stack (void)

/* Pop the last entry from the control stack and restore the execution
 * context from it - except for extern_call of which the old value will
 * be used immediately after the pop.
 */

{
#ifdef DEBUG
    if (csp < CONTROL_STACK)
        fatal("Popped out of the control stack");
#endif

    if ( NULL != (current_prog = csp->prog) ) /* is 0 when we reach the bottom */
    {
        current_strings = current_prog->strings;
    }
    if (current_lambda.type == T_CLOSURE)
        free_closure(&current_lambda);
    current_lambda = csp->lambda;
    inter_pc = csp->pc;
    inter_fp = csp->fp;
    inter_context = csp->context;
    function_index_offset = csp->function_index_offset;
    variable_index_offset = csp->variable_index_offset;
    current_variables     = csp->current_variables;
    break_sp = csp->break_sp;
    csp--;
} /* pop_control_stack() */

/*-------------------------------------------------------------------------*/
inherit_t *
adjust_variable_offsets ( const inherit_t * inheritp
                        , const program_t * prog
                        , const object_t  * obj
                        )

/* If we do an explicit call into a virtually inherited base class we
 * have to find the first instance of the inherited variables.
 * This cannot be done at compile time because it depends on the
 * _object_ (i.e. the runtime environment) in which the program
 * is running.
 *
 * <inheritp> is the intended target for the call, <prog> is the
 * currently running program, <obj> is the currently used object.
 * The result is either NULL if no adjustment is required (then the caller
 * has to use the original <inheritp> passed in), or the pointer to the
 * inheritance structure to be used.
 *
 * TODO: A better compiler might do some backpatching and at least
 * TODO:: leave hints where the variables are, so that we can omit
 * TODO:: the explicit search. Or some load-time patching.
 */
{
    inherit_t * inh = NULL;

    if (prog != obj->prog
     && inheritp->inherit_type != INHERIT_TYPE_NORMAL
       )
    {
        /* Now search for the first virtual inheritance of the program
         * in the inherit list of the topmost program.
         * Don't get confused by normal inherits, though.
         */

        int i = obj->prog->num_inherited;
        inh = obj->prog->inherit;

        while (i)
        {
            if (inh->prog == inheritp->prog
             && inh->inherit_type != INHERIT_TYPE_NORMAL
               )
                break;
            inh++;
            i--;
        }

        /* i should always be != 0 here, with inh pointing the the
         * inherit structure we're looking for.
         */

#ifdef DEBUG
        if (!i)
        {
            char *ts;
            ts = time_stamp();
            fprintf(stderr,
                    "%s Adjusting variable offsets because of virtual "
                        "inheritance for call\n"
                    "%s from %s into %s (topmost program %s) FAILED.\n"
                    "%s Please check the inherit tree and report it.\n"
                   , ts, ts
                   , get_txt(prog->name)
                   , get_txt(inheritp->prog->name)
                   , get_txt(obj->prog->name)
                   , ts);
            inh = NULL;
        }
#endif
    }

    return inh;
} /* adjust_variable_offsets() */

/*-------------------------------------------------------------------------*/
static inherit_t *
setup_inherited_call (unsigned short inhIndex, unsigned short *fx)

/* Setup the global variables for a call to an explicitly inherited
 * function, inherited from <inhIndex>. Result is the pointer to the
 * inherit structure.
 */

{
    inherit_t * inheritp = &current_prog->inherit[inhIndex];

#ifdef DEBUG
    if (inhIndex >= current_prog->num_inherited)
        errorf("(setup_inherited_call): inhIndex %ld > number of inherits %ld "
              "in program '%s'\n"
             , (long)inhIndex
             , (long)current_prog->num_inherited
             , get_txt(current_prog->name)
             );
#endif

    /* If we do an explicit call into a virtually inherited base class we
     * have to find the first instance of the inherited variables.
     * This cannot be done at compile time because it depends on the
     * _object_ (i.e. the runtime environment) in which current_prog
     * is running.
     */
    {
        inherit_t * inh;

        inh = adjust_variable_offsets(inheritp, current_prog, current_object);
        if (inh)
        {
            /* Found a virtual base class, so un-adjust the offsets. */
            inheritp = inh;
            current_variables = current_object->variables;
            variable_index_offset = 0;
            function_index_offset = 0;

            /* Check for obsoleted inherited programs. */
            while (inheritp->inherit_type & INHERIT_TYPE_MAPPED)
            {
                unsigned short prevfx = *fx;

                *fx = current_object->prog->update_index_map[prevfx + inheritp->function_map_offset];
                if (*fx == USHRT_MAX)
                {
                    /* There was no corresponding function in the new program.
                     * If this was the __INIT function we'll ignore it.
                     * Otherwise throw an error.
                     */
                    function_t* fun = get_function_header(inheritp->prog, prevfx);
                    if (mstreq(fun->name, STR_VARINIT))
                        return NULL;
                    else
                        errorf("Dangling function call to '%s' in program '%s'.\n", get_txt(fun->name), get_txt(inheritp->prog->name));
                }
                inheritp = current_object->prog->inherit + inheritp->updated_inherit;
            }
        }
        else if(inheritp->inherit_type != INHERIT_TYPE_NORMAL)
        {
            /* Virtual inherit, but we're at the top level.
             * So nothing to correct, just reset the variable offsets,
             * because they point to the non-virtual variables.
             */

            current_variables = current_object->variables;
            variable_index_offset = 0;
            assert(function_index_offset == 0); /* We should be at the topmost program. */
        }
    }

    /* Set the current program to the inherited program so that the
     * caller can search for the function.
     */
    current_prog = inheritp->prog;

    return inheritp;
} /* setup_inherited_call() */

/*-------------------------------------------------------------------------*/
static INLINE funflag_t
setup_new_frame1 (int fx, int fun_ix_offs, int var_ix_offs)

/* Setup current_prog, function_ and variable_index_offset for a call
 * to function index <fx> in the current program.
 *
 * <fun_ix_offs> and <var_ix_offs> are offsets to be added to the
 * functions given offsets - this is necessary when <fx> is given relative
 * to some inherited program and needs to be adjusted for the topmost
 * program.
 *
 * Return the 'flags' for the function.
 */

{
    program_t *progp;
    funflag_t flags;

    progp = current_prog;

    flags = progp->functions[fx];

    /* Handle a cross-define.
     * This is a rather rare occasion and usually happens only with functions
     * like heart_beat() which are called by function index and not by name.
     * This index, determined at compile time, might point to the
     * cross-defined function entry.
     */
    if (flags & NAME_CROSS_DEFINED)
    {
        fx += CROSSDEF_NAME_OFFSET(flags);
        flags = progp->functions[fx];
    }

    /* If the function is inherited, find the real function definition
     * and adjust the offsets to point to its code and variables.
     * This is an iteration walking along the inherit chain.
     */
    fun_ix_offs += fx;
    while (flags & NAME_INHERITED)
    {
        inherit_t *inheritp;

        inheritp = &progp->inherit[flags & INHERIT_MASK];
        assert(!(inheritp->inherit_type & INHERIT_TYPE_MAPPED));

        if(inheritp->inherit_type != INHERIT_TYPE_NORMAL)
        {
            /* We must be at the top-level for virtual inherits. */
            assert(var_ix_offs == progp->num_virtual_variables);

            var_ix_offs = 0;
        }

        progp = inheritp->prog;
        fx -= inheritp->function_index_offset;
        var_ix_offs += inheritp->variable_index_offset;
          /* Remember here that function offset is relative to current_prog,
           * but variable_offset is relative to current_object.
           */
        flags = progp->functions[fx];
    }
    /* fx is now the 'pure' function index without any offsets */

    /* Setup the variables and return */
    current_prog = progp;
    function_index_offset = fun_ix_offs - fx;
    variable_index_offset = var_ix_offs;

    return flags;
} /* setup_new_frame1() */

/*-------------------------------------------------------------------------*/
static INLINE svalue_t *
setup_new_frame2 (bytecode_p funstart, svalue_t *sp
                 , Bool is_lambda)

/* Before calling the function at <funstart>, massage the data on the
 * stack ending at <sp> to match the formal argumentlist of the function
 * (excessive args are removed, missing args are provided as 0),
 * and allocate the local variables on the stack.
 *
 * <is_lambda> has to be TRUE if the function is a lambda closure.
 * This information is needed for proper tracing.
 *
 * csp->num_local_variables is supposed to hold the number of actual
 * arguments on the stack.
 *
 * Result is the new stackpointer, the framepointer <inter_fp>, the
 * program counter <inter_pc>, csp->num_local_variables and
 * <break_sp> are set up.
 * The context pointer <inter_context> is cleared.
 */

{
    int i;        /* Difference between number of formal and actual args;
                   * Number of (uninitialized) local variables
                   */
    int num_arg;  /* Number of formal args */
    int num_vars; /* Number of local variables */
    bool has_varargs; /* Function has an varargs parameter. */

    /* Setup the frame pointer */
    inter_fp = sp - csp->num_local_variables + 1;

    /* By default there is no context */
    inter_context = NULL;

    /* (Re)move excessive arguments.  */
    if (is_lambda)
    {
        lambda_t *l = current_lambda.u.lambda;
        if (current_lambda.x.closure_type == CLOSURE_BOUND_LAMBDA)
            l = l->function.lambda;

        num_arg = l->function.code.num_arg;
        num_vars = l->function.code.num_locals;
        has_varargs = false;

        inter_pc = funstart;
    }
    else
    {
        int num_opt_arg; /* Number of optional formal arguments */

        function_t *header = current_prog->function_headers + FUNCTION_HEADER_INDEX(funstart);
        num_arg = header->num_arg;
        num_opt_arg = header->num_opt_arg;
        num_vars = header->num_locals;
        has_varargs = (header->flags & TYPE_MOD_XVARARGS) != 0;

        inter_pc = funstart;
        if (num_opt_arg)
        {
            /* Modify inter_pc, depending on how many
             * arguments are missing. */
            int missing = num_arg - csp->num_local_variables;

            if (has_varargs)
                missing--;
            if (missing < 0)
                missing = 0;
            else if (missing > num_opt_arg)
                missing = num_opt_arg;

            /* Skip the initial jump table. */
            inter_pc += num_opt_arg * sizeof(unsigned short);
            if (missing < num_opt_arg)
                inter_pc += get_short(funstart + sizeof(unsigned short) * (num_opt_arg - missing - 1));
        }
    }

    if (has_varargs)
    {
        /* Function has a 'varargs' argument */
        if ((i = csp->num_local_variables - num_arg + 1) < 0)
        {
            /* More formal than actual parameters. */

            csp->num_local_variables = num_arg;

            /* First, fill in zero for the rest... */
            do {
                *++sp = const0;
            } while (++i);

            /* ...and an empty array for the varargs portion */
            ++sp;
            put_array(sp, allocate_uninit_array(0));

            /* At this point i == 0, because all args are there. */
        }
        else
        {
            /* More actual than formal parameters */

            vector_t *v;

            csp->num_local_variables = num_arg;

            /* Move the extra args into an array and put that
             * onto the stack
             */
            v = allocate_uninit_array(i);
            while (--i >= 0)
                v->item[i] = *sp--;

            ++sp;
            put_array(sp, v);

            i = 0; /* All arguments are there now. */
        }
    }
    else if ((i = csp->num_local_variables - num_arg) > 0)
    {
        /* Function takes a fixed number of arguments,
         * but more actual than formal args.
         */

        /* Pop the extraneous args */
        do {
            free_svalue(sp--);
            csp->num_local_variables--;
        } while(--i);

        /* At this point i == 0, because all args are there. */
    }
    /* else
     *
     * Enough or too little arguments supplied to a fixed-args
     * function: initialize the missing args and the locals
     * in one swoop (i = minus the number of missing arguments)
     *
     * For all other cases i == 0, so we're just initializing
     * the local vars here.
     */

    if ( 0 != (i = num_vars - i) )
    {
        csp->num_local_variables += i;
        do {
            *++sp = const0;
        } while (--i);
    }

    /* Check for stack overflow. Since the actual stack size is
     * larger than EVALUATOR_STACK_SIZE, this check at the
     * end should be sufficient. If not, stack_overflow() will
     * generate a fatal error and we have to resize.
     */
    if ( sp >= &VALUE_STACK[EVALUATOR_STACK_SIZE] )
        stack_overflow(sp, csp->fp, funstart);

    /* Count the call depth for traces and handle tracing */
    tracedepth++;
    if (TRACEP(TRACE_CALL) && TRACE_IS_INTERACTIVE())
    {
      inter_sp = sp;
      do_trace_call(funstart, is_lambda);
    }

    /* Initialize the break stack, pointing to the entry above
     * the first available svalue.
     */
    break_sp = sp+1;
    
    return sp;
} /* setup_new_frame2() */

/*-------------------------------------------------------------------------*/
static void
setup_new_frame (int fx, program_t *inhProg)

/* Setup a call for function <fx> in the current program.
 * If <inhProg> is not NULL, it is the program of the inherited function
 * to call.
 * Result are the flags for the function. Global csp->funstart is set
 * to the start of the function bytecode.
 */

{
    funflag_t flags;

    /* We must be at the topmost level in the inherit hierarchy. */
    assert(current_prog == current_object->prog);

    if (inhProg)
    {
        program_t *progp;
        int       fun_ix_offs;
        int       var_ix_offs;

        progp = current_prog;
        fun_ix_offs = 0;
        var_ix_offs = progp->num_virtual_variables;

        while (progp != inhProg)
        {
            inherit_t      *inheritp, *inh;
            
#ifdef DEBUG
            if (!progp->num_inherited)
                errorf("(setup_new_frame): Couldn't find program '%s' "
                       "in program '%s' with function index %ld. "
                       "Found program '%s' instead.\n"
                     , get_txt(inhProg->name)
                     , get_txt(current_prog->name)
                     , (long) fx
                     , get_txt(progp->name)
                     );
#endif
            inheritp = search_function_inherit(progp, fx);
            fx -= inheritp->function_index_offset;

            inh = adjust_variable_offsets(inheritp, progp, current_object);
            if (inh)
            {
                /* Virtual base class. Reset offsets. */
                inheritp = inh;
                fun_ix_offs = 0;
                var_ix_offs = 0;

                /* Check for obsoleted inherited programs. */
                while (inheritp->inherit_type & INHERIT_TYPE_MAPPED)
                {
                    fx = current_object->prog->update_index_map[fx + inheritp->function_map_offset];
                    if (fx == USHRT_MAX)
                        errorf("Dangling function call in program '%s'.\n", get_txt(inheritp->prog->name));
                    inheritp = current_object->prog->inherit + inheritp->updated_inherit;
                }
            }

            fun_ix_offs += inheritp->function_index_offset;
            var_ix_offs += inheritp->variable_index_offset;
            progp = inheritp->prog;

#ifdef DEBUG
            if (fx >= progp->num_functions)
                errorf("(setup_new_frame): fx %ld > number of "
                       "functions %ld in program '%s'\n"
                     , (long) fx
                     , (long) progp->num_functions
                     , get_txt(progp->name)
                     );
#endif
        }
        
        current_prog = inhProg;

        flags = setup_new_frame1(fx, fun_ix_offs, var_ix_offs);
    }
    else
        flags = setup_new_frame1(fx, 0, current_prog->num_virtual_variables);

    /* Setting csp->funstart is not just convenient, but also
     * required for proper error handling in setup_new_frame2()
     */
    csp->funstart = current_prog->program + (flags & FUNSTART_MASK);

    inter_sp = setup_new_frame2(csp->funstart, inter_sp, MY_FALSE);
#ifdef DEBUG
    if (!current_object->variables && variable_index_offset)
        fatal("%s Fatal: new frame for object %p '%s' w/o variables, "
              "but offset %d\n"
             , time_stamp(), current_object, get_txt(current_object->name)
             , variable_index_offset);
#endif
    current_variables = current_object->variables;
    if (current_variables)
        current_variables += variable_index_offset;
    current_strings = current_prog->strings;

} /* setup_new_frame() */

/*-------------------------------------------------------------------------*/
void
reset_machine (Bool first)

/* Reset the virtual machine. <first> is true on the very first call
 * (the cold boot, so to speak). Subsequent calls pass <first> as false
 * and this way make sure that all values currently on the stack
 * are properly removed.
 */

{
    traceing_recursion = -1;
    if (first)
    {
        csp = CONTROL_STACK - 1;
        inter_sp = VALUE_STACK - 1;
        tracedepth = 0;
        put_number(&current_lambda, 0);
    }
    else
    {
        inter_sp = _pop_n_elems(inter_sp - VALUE_STACK + 1, inter_sp);
        if (current_lambda.type == T_CLOSURE)
            free_closure(&current_lambda);
        put_number(&current_lambda, 0);
        while (csp >= CONTROL_STACK)
        {
            if (csp->lambda.type == T_CLOSURE)
                free_closure(&csp->lambda);
            csp--;
        }
    }
} /* reset_machine() */

/*-------------------------------------------------------------------------*/
#ifdef DEBUG
int
check_state (void)

/* Check the virtual machine for consistency. Return 0 when it is, else
 * print a debug message and return an error code.
 *
 * As this function can be costly, it is by default not called from
 * the backend loop.
 */

{
    int rc;

    rc = 0;

    if (rt_context->type != ERROR_RECOVERY_BACKEND) {
        debug_message("%s rt_context stack inconsistent: type %d instead of %d\n"
                     , time_stamp(), rt_context->type, ERROR_RECOVERY_BACKEND);
        printf("%s rt_context stack inconsistent: type %d instead of %d\n"
              , time_stamp(), rt_context->type, ERROR_RECOVERY_BACKEND);
        if (!rc) rc = 1;
    }
    if (csp != CONTROL_STACK - 1) {
        debug_message("%s csp inconsistent: %p instead of %p\n"
                     , time_stamp(), csp, CONTROL_STACK-1);
        printf("%s csp inconsistent: %p instead of %p\n"
              , time_stamp(), csp, CONTROL_STACK-1);
        if (!rc) rc = 2;
    }
    if (inter_sp != VALUE_STACK - 1) {
        debug_message("%s sp inconsistent: %p instead of %p\n"
                     , time_stamp(), inter_sp, VALUE_STACK - 1);
        printf("%s sp inconsistent: %p instead of %p\n"
              , time_stamp(), inter_sp, VALUE_STACK - 1);
        if (!rc) rc = 3;
    }

    return rc;
} /* check_state() */
#endif

/*-------------------------------------------------------------------------*/
void
free_interpreter_temporaries (void)

/* Free all svalue the interpreter holds in global variables.
 * Usually the values are freed whenever a new value is stored, but
 * this function allows e.g. the garbage collector to free them all
 * at once.
#ifdef TRACE_CODE
 * The function also cleans out all destructed objects from the
 * instruction trace.
#endif
 */

{
    free_svalue(&indexing_quickfix);
    indexing_quickfix.type = T_NUMBER;
    free_svalue(&apply_return_value);
    apply_return_value.type = T_NUMBER;
    free_svalue(&struct_member_temporary);
    struct_member_temporary.type = T_NUMBER;
    free_svalue(&current_unprotected_mapentry.key);

#ifdef TRACE_CODE
    {
        int i;

        for (i = TOTAL_TRACE_LENGTH; --i >= 0; )
        {
            object_t *ob;

            if (NULL != (ob = previous_objects[i])
             && ob->flags & O_DESTRUCTED
               )
            {
                free_object(ob, "free_interpreter_temporaries");
                previous_objects[i] = NULL;
                previous_instruction[i] = 0;
            }
        }
    }
#endif

} /* free_interpreter_temporaries() */

/*-------------------------------------------------------------------------*/
void
remove_object_from_stack (object_t *ob)

/* Object <ob> was/will be destructed, so remove all references from
 * to it from the stack, including references through closures.
 */

{
    svalue_t *svp;

    for (svp = VALUE_STACK; svp <= inter_sp; svp++)
    {
        if (object_ref(svp, ob))
        {
            free_svalue(svp);
            put_number(svp, 0);
        }
    } /* foreach svp in stack */
} /* remove_object_from_stack() */

/*-------------------------------------------------------------------------*/
static INLINE void
put_default_argument (svalue_t *sp, int instruction)

/* Evaluate <instruction> and put it's result into *<sp>.
 * This function is used to generate default arguments for efuns at runtime,
 * and therefor implements just the instructions F_CONST0, F_CONST1,
 * F_NCONST1, F_TIME, F_THIS_OBJECT, and F_THIS_PLAYER.
 */

{
    switch(instruction)
    {
    case F_CONST0:
        put_number(sp, 0);
        break;

    case F_CONST1:
        put_number(sp, 1);
        break;

    case F_NCONST1:
        put_number(sp, -1);
        break;

    case F_TIME:
        put_number(sp, current_time);
        break;

    case F_THIS_OBJECT:
        if (current_object->flags & O_DESTRUCTED)
        {
            put_number(sp, 0);
            break;
        }
        put_ref_object(sp, current_object, "default: this_object");
        break;

    case F_THIS_PLAYER:
        if (command_giver && !(command_giver->flags & O_DESTRUCTED))
            put_ref_object(sp, command_giver, "default: this_player");
        else
            put_number(sp, 0);
        break;

    default:
        fatal("Unimplemented runtime default argument '%s' to %s().\n"
             , get_f_name(instruction), get_f_name(complete_instruction(-2))
             );
        break;
    }
} /* put_default_argument() */

/*-------------------------------------------------------------------------*/
Bool
eval_instruction (bytecode_p first_instruction
                 , svalue_t *initial_sp)

/* Evaluate the code starting at <first_instruction>, using <inital_sp>
 * as the stack pointer.
 *
 * All other variables like current_prog must be setup before the call.
 * The function will return upon encountering a F_RETURN instruction
 * for which .extern_call or .catch_call is true, or upon encountering
 * a F_END_CATCH instruction.
 *
 * The result will state the reason for returning: FALSE for F_RETURN,
 * and TRUE for F_END_CATCH.
 *
 * This also means that for every intra-object call eval_instruction()
 * is called recursively.
 *
 * There must not be destructed objects on the stack. The destruct_object()
 * function will automatically remove all occurrences. The effect is that
 * all called efuns know that they won't have destructed objects as
 * arguments.
 *
 * All instructions/functions callable from LPC must return a value or be
 * declared void. This does not apply to internal control codes like F_JUMP.
 */

{
    bytecode_p     pc;  /* Current program pointer */
    register svalue_t *fp;  /* Current frame pointer */
    register svalue_t *sp;  /* Current stack pointer */
      /* For speed reasons, these variables shadow their global counterparts,
       * allowing more optimisations.
       * gcc feels better about setjmp() when variables are declared register.
       * Still we might get 'variable foo might be clobbered' warnings, but
       * declaring them as volatile would degrade optimization, so we don't.
       */
    int num_arg;      /* Number of arguments given to the current instr */
    int instruction;  /* The current instruction code */
    int full_instr;   /* The full instruction code; including any additional
                       * code bytes (e.g. for efuns)
                       */
#ifdef DEBUG
    svalue_t *expected_stack; /* Expected stack at the instr end */
#endif

    svalue_t *ap;
      /* Argument frame pointer: pointer to first outgoing argument to be
       * passed to called function.
       */
    Bool use_ap;
      /* TRUE if the next simul_efun/efun call is to determine the number of
       * arguments from the current *ap value. This variable is static in order
       * to survive longjmp()s, its actual scope is just within one execution
       * of eval_instruction().
       */


    /* Handy macros:
     *
     *   GET_NUM_ARG: Get the number of arguments, resp. check if the
     *                number was read correctly.
     *
     *   RAISE_ARG_ERROR(arg,expected,got),
     *   OP_ARG_ERROR(arg,expected,got):
     *                Argument <arg> had type <got> (LPC type tag), not
     *                type <expected> (bit-encoded).
     *
     *   BAD_ARG_ERROR(arg,expected,got),
     *   BAD_OP_ARG(arg,expected,got):
     *                Argument <arg> had type <got> (LPC type tag), not
     *                type <expected> (LPC type tag).
     *
     *   TYPE_TEST1/2/3/4(arg, t): Test argument <arg> of a one-byte
     *                instruction if it has type <t> (LPC type tag).
     *                The 1/2/3/4 is the number of the argument.
     *   TYPE_TEST_LEFT(arg, t), TYPE_TEST_RIGHT(arg, t): Test the
     *                argument <arg> if it has type <t> (LPC type tag).
     *                It is either the left or the right argument to a
     *                one-byte operator.
     *   TYPE_TEST_EXP_LEFT(arg, t), TYPE_TEST_EXP_RIGHT(arg, t): Test the
     *                argument <arg> if it has type <t> (bit-encoded).
     *                It is either the left or the right argument to a
     *                one-byte operator.
     *
     */

#   ifdef DEBUG
#       define GET_NUM_ARG \
            if (num_arg != GET_UINT8(pc-1)) {\
                fatal("Argument count error for %s: %d vs. %d.\n", get_f_name(instruction), num_arg, GET_UINT8(pc-1));}
        /* The macro catches two faults: getting num_arg for instructions
         * which don't take arguments, and getting num_arg after incrementing
         * the pc too far.
         */
#   else /* DEBUG */
#       define GET_NUM_ARG num_arg = GET_UINT8(pc); inter_pc = ++pc;
#   endif /* DEBUG */
      /* Get and/or test the number of arguments.
       */

#   define ARG_ERROR_TEMPL(fun, arg, expected, got) \
       do {\
           inter_sp = sp; \
           inter_pc = pc; \
           fun(instruction, arg, expected, got); \
       }while(0)

#   define RAISE_ARG_ERROR(arg, expected, got) \
       ARG_ERROR_TEMPL(raise_arg_error, arg, expected, got)

#   define BAD_ARG_ERROR(arg, expected, got) \
       ARG_ERROR_TEMPL(raise_arg_error, arg, 1 << expected, got)

#   define OP_ARG_ERROR_TEMPL(fun, arg, expected, got) \
       do {\
           inter_sp = sp; \
           inter_pc = pc; \
           fun(arg, expected, got, pc, sp); \
       }while(0)

#   define BAD_OP_ARG(arg, expected, got) \
       OP_ARG_ERROR_TEMPL(op_arg_error, arg, expected, got)

#   define OP_ARG_ERROR(arg, expected, got) \
       OP_ARG_ERROR_TEMPL(op_exp_arg_error, arg, expected, got)

#   define TYPE_TEST_TEMPL(num, arg, t) \
        if ( (arg)->type != t ) code_arg_error(num, t, (arg)->type, pc, sp); else NOOP;
#   define OP_TYPE_TEST_TEMPL(num, arg, t) \
        if ( (arg)->type != t ) op_arg_error(num, t, (arg)->type, pc, sp); else NOOP;
#   define EXP_TYPE_TEST_TEMPL(num, arg, t) \
        if (!( (1 << (arg)->type) & (t)) ) op_exp_arg_error(num, (t), (arg)->type, pc, sp); else NOOP;

#   define TYPE_TEST1(arg, t) TYPE_TEST_TEMPL(1, arg, t)
#   define TYPE_TEST2(arg, t) TYPE_TEST_TEMPL(2, arg, t)
#   define TYPE_TEST3(arg, t) TYPE_TEST_TEMPL(3, arg, t)
#   define TYPE_TEST4(arg, t) TYPE_TEST_TEMPL(4, arg, t)

#   define TYPE_TEST_LEFT(arg, t)  OP_TYPE_TEST_TEMPL(1, arg, t)
#   define TYPE_TEST_RIGHT(arg, t) OP_TYPE_TEST_TEMPL(2, arg, t)

#   define TYPE_TEST_EXP_LEFT(arg, t)  EXP_TYPE_TEST_TEMPL(1, arg, t)
#   define TYPE_TEST_EXP_RIGHT(arg, t) EXP_TYPE_TEST_TEMPL(2, arg, t)
      /* Test the type of a certain argument.
       */

#   ifdef MARK
#        define CASE(x) case (x): MARK(x);
#   else
#        define CASE(x) case (x):
#   endif
      /* Macro to build the case: labels for the evaluator switch.
       * 'MARK' adds profiling support.
       */

#   define push_ref_prog_string(idx) \
        if (current_strings[idx]->info.unicode == STRING_BYTES) \
            push_ref_bytes(sp, current_strings[idx]);           \
        else                                                    \
            push_ref_string(sp, current_strings[idx]);
      /* Macro to put a program string <idx> onto the stack. */


    /* Setup the variables.
     * The next F_RETURN at this level will return out of eval_instruction().
     */
    if (!csp->catch_call)
        csp->extern_call = MY_TRUE;
    sp = initial_sp;
    pc = first_instruction;
    fp = inter_fp;
    ap = inter_fp; /* so that call_lambda() can call us for efun closures */
    use_ap = MY_FALSE;
    runtime_no_warn_deprecated = MY_FALSE;
    runtime_array_range_check = MY_FALSE;
    SET_TRACE_EXEC();

    /* ------ The evaluation loop ------ */

again:
    /* Get the next instruction and increment the pc */

    full_instr = instruction = LOAD_CODE(pc);
    if (full_instr == F_EFUN0)
        full_instr = GET_CODE(pc) + EFUN0_OFFSET;
    else if (full_instr == F_EFUN1)
        full_instr = GET_CODE(pc) + EFUN1_OFFSET;
    else if (full_instr == F_EFUN2)
        full_instr = GET_CODE(pc) + EFUN2_OFFSET;
    else if (full_instr == F_EFUN3)
        full_instr = GET_CODE(pc) + EFUN3_OFFSET;
    else if (full_instr == F_EFUN4)
        full_instr = GET_CODE(pc) + EFUN4_OFFSET;
    else if (full_instr == F_EFUNV)
        full_instr = GET_CODE(pc) + EFUNV_OFFSET;

#if 0
    if (full_instr != instruction)
        printf("DEBUG: %p (%p): %3d %s %s\n"
              , pc-1, sp
              , full_instr, get_f_name(instruction), get_f_name(full_instr));
    else
        printf("DEBUG: %p (%p): %3d %s\n"
              , pc-1, sp
              , full_instr, get_f_name(full_instr));
    fflush(stdout);
#endif

#   ifdef TRACE_CODE
        /* Store some vitals in the trace buffer */

#       if TOTAL_TRACE_LENGTH & TOTAL_TRACE_LENGTH-1
            if (++last == TOTAL_TRACE_LENGTH)
                last = 0;
#       else
            last = (last+1) & (TOTAL_TRACE_LENGTH-1);
#       endif
        previous_instruction[last] = instruction;
        previous_pc[last] = pc-1;
        stack_size[last] = sp - fp - csp->num_local_variables;
        abs_stack_size[last] = sp - VALUE_STACK;
        if (previous_objects[last])
        {
            /* Need to free the previously stored object */
            free_object(previous_objects[last], "TRACE_CODE");
        }
        previous_objects[last] = ref_object(current_object, "TRACE_CODE");
        previous_programs[last] = current_prog;
#   endif  /* ifdef TRACE_CODE */

#   ifdef MALLOC_LPC_TRACE
        inter_pc = pc;
#   endif

#   ifdef OPCPROF
        opcount[full_instr]++;
#   endif

    /* If requested, trace the instruction.
     * Print the name of the instruction, but guard against recursions.
     */
    if (trace_exec_active && TRACE_EXEC_P() && TRACE_IS_INTERACTIVE())
    {
        if (!++traceing_recursion)
        {
            inter_sp = sp;
            do_trace("Exec ", get_f_name(full_instr), "\n");
            instruction = EXTRACT_UCHAR(pc-1);
        }
        traceing_recursion--;
    }

    /* Test the evaluation cost.
     * eval_cost < 0 signify a wrap-around - unlikely, but with these crazy
     * wizards everything is possible.
     */
    if (add_eval_cost(1))
    {
        rt_context_t * context;

        /* Evaluation too long. Restore some globals and throw
         * an error.
         */

        printf("%s eval_cost too big %ld\n", time_stamp(), (long)eval_cost);

        assign_eval_cost_inl();

        /* If the error isn't caught, reset the eval costs */
        for (context = rt_context
            ; !ERROR_RECOVERY_CONTEXT(context->type)
            ; context = context->last
            ) NOOP;
        if (context->type <= ERROR_RECOVERY_BACKEND)
        {
            CLEAR_EVAL_COST;
            RESET_LIMITS;
        }

        inter_pc = pc;
        inter_fp = fp;
        ERROR("Too long evaluation. Execution aborted.\n");
    }

#if defined(DEBUG)

    /* Get the expected number of arguments and determined the expected
     * stack setting.
     * Note that the code deliberately looks at instruction and not
     * full_instr, as all multibyte instructions do not store the number
     * of arguments in code.
     */
    if (instrs[instruction].min_arg != instrs[instruction].max_arg
     && instruction != F_CALL_OTHER
     && instruction != F_CALL_DIRECT
       )
    {
        num_arg = GET_UINT8(pc);
        pc++;
    }
    else
    {
        /* Safety measure. It is supposed that the evaluator knows
         * the number of arguments.
         */
        num_arg = -1;
    }

    if (num_arg != -1 && !use_ap)
    {
        expected_stack = sp - num_arg +
            ( instrs[full_instr].ret_type == lpctype_void ? 0 : 1 );
    }
    else if (use_ap)
    {
        expected_stack = ap -
            ( instrs[full_instr].ret_type == lpctype_void ? 1 : 0 );
    }
    else
    {
        expected_stack = NULL;
    }
#endif /* DEBUG */

    /* If an argument frame is in effect, check the number of arguments
     * for unprefixed efuns, the compiler didn't check them in this case.
     * We have an argument frame when use_ap is set or the efun doesn't have
     * a fixed number of arguments.
     */
    if (instrs[instruction].Default != -1
     && (use_ap || instrs[instruction].min_arg != instrs[instruction].max_arg))
    {
        int numarg = sp - ap + 1;

        if (numarg < instrs[instruction].min_arg)
            ERRORF(("Not enough args for %s: got %d, expected %d.\n"
                   , instrs[instruction].name
                   , numarg, instrs[instruction].min_arg));
        if (numarg > instrs[instruction].max_arg && instrs[instruction].max_arg >= 0)
            ERRORF(("Too many args for %s: got %d, expected %d.\n"
                   , instrs[instruction].name
                   , numarg, instrs[instruction].min_arg));

        use_ap = false;
    }

    /* The monster switch to execute the instruction.
     * The order of the cases is held (mostly) in the order
     * the instructions appear in func_spec.
     */
    inter_sp = sp;
    inter_pc = pc;
      /* TODO: This continual update is crude, but circumvents a lot
       * TODO:: of situations where an error is thrown but inter_sp
       * TODO:: is invalid (heck, every assign_svalue() could cause that). In
       * TODO:: the long run, we should do this only for efuns (which are by
       * TODO:: then hopefully all tabled).
       */
    switch(instruction)
    {
    default:
        fatal("Undefined instruction '%s' (%d)\n", get_f_name(instruction),
              instruction);
        /* NOTREACHED */
        return MY_FALSE; /* hint for data flow analysis */

#ifdef F_ILLEGAL
    CASE(F_ILLEGAL);                /* --- illegal             --- */
        inter_pc = pc;
        fatal("'illegal' instruction encountered.\n");
        /* NOTREACHED */
#endif /* F_ILLEGAL */

    CASE(F_UNDEF);                  /* --- undef               --- */
      {
        /* Catch-all instructions for declared but not implemented
         * (defined) functions. Usually used by the compiler to
         * handle prototypes (in that case it is the first and only
         * instruction of the generated stub), it is also inserted
         * into lambda closures when they referenced a function
         * that went missing because of a replace_program.
         *
         * Note: this instruction MUST be the first in the function.
         */

        string_t *name;

        /* pc has already been incremented */
        if (pc > current_prog->program && pc <= PROGRAM_END(*current_prog))
        {
            /* Copy the function name pointer into name.
             */
            name = current_prog->function_headers[FUNCTION_HEADER_INDEX(pc-1)].name;
        }
        else
        {
            /* It is a lambda closure after a replace_program. */
            name = STR_DANGLING_LAMBDA;
        }
        ERRORF(("Undefined function: %s\n", get_txt(name)));
      }

    CASE(F_EFUN0);                  /* --- efun0 <code>        --- */
    {
        /* Call the tabled efun EFUN0_OFFSET + <code>, where <code> is
         * a uint8.
         * The efun takes no argument.
         */

        int code;

        /* Check the number of arguments on the stack */
        if (use_ap)
        {
            int numarg = sp - ap + 1;

            if (numarg < 0)
                ERRORF(("Not enough args for %s: got %d, expected none.\n"
                       , instrs[instruction].name, numarg));
            if (numarg > 0)
                ERRORF(("Too many args for %s: got %d, expected none.\n"
                       , instrs[instruction].name, numarg));
            use_ap = MY_FALSE;
        }

        code = LOAD_UINT8(pc);
#ifdef TRACE_CODE
        previous_instruction[last] = code + EFUN0_OFFSET;
#endif
#ifdef OPCPROF
        opcount[code+EFUN0_OFFSET]++;
#endif
        inter_sp = sp;
        inter_pc = pc;
        assign_eval_cost_inl();
        sp = (*efun_table[code+EFUN0_OFFSET-TEFUN_OFFSET])(sp);
#ifdef CHECK_OBJECT_REF
        check_all_object_shadows();
#endif /* CHECK_OBJECT_REF */
        break;
    }

    CASE(F_EFUN1);                  /* --- efun1 <code>        --- */
    {
        /* Call the tabled efun EFUN1_OFFSET + <code>, where <code> is
         * a uint8.
         * The efun takes one argument.
         */

        int code;

        code = LOAD_UINT8(pc);
        instruction = code + EFUN1_OFFSET;

        /* Correct then number of arguments on the stack */
        if (use_ap)
        {
            int numarg = sp - ap + 1;
            int def;

            if (numarg == 0 && (def = instrs[instruction].Default) != 0)
            {
                put_default_argument(++sp, def);
                numarg++;
            }

            if (numarg < 1)
                ERRORF(("Not enough args for %s: got %d, expected 1.\n"
                       , instrs[instruction].name, numarg));
            if (numarg > 1)
                ERRORF(("Too many args for %s: got %d, expected 1.\n"
                       , instrs[instruction].name, numarg));
            use_ap = MY_FALSE;
        }

#ifdef TRACE_CODE
        previous_instruction[last] = instruction;
#endif
#ifdef OPCPROF
        opcount[instruction]++;
#endif
        inter_sp = sp;
        inter_pc = pc;
        assign_eval_cost_inl();
        test_efun_args(instruction, 1, sp);
        sp = (*efun_table[instruction-TEFUN_OFFSET])(sp);
#ifdef CHECK_OBJECT_REF
        check_all_object_shadows();
#endif /* CHECK_OBJECT_REF */
        break;
    }

    CASE(F_EFUN2);                  /* --- efun2 <code>        --- */
    {
        /* Call the tabled efun EFUN2_OFFSET + <code>, where <code> is
         * a uint8.
         * The efun takes two arguments.
         */

        int code;

        code = LOAD_UINT8(pc);
        instruction = code + EFUN2_OFFSET;

        /* Correct then number of arguments on the stack */
        if (use_ap)
        {
            int numarg = sp - ap + 1;
            int def;

            if (numarg == 1 && (def = instrs[instruction].Default) != 0)
            {
                put_default_argument(++sp, def);
                numarg++;
            }

            if (numarg < 2)
                ERRORF(("Not enough args for %s: got %d, expected 2.\n"
                       , instrs[instruction].name, numarg));
            if (numarg > 2)
                ERRORF(("Too many args for %s: got %d, expected 2.\n"
                       , instrs[instruction].name, numarg));
            use_ap = MY_FALSE;
        }

#ifdef TRACE_CODE
        previous_instruction[last] = instruction;
#endif
#ifdef OPCPROF
        opcount[instruction]++;
#endif
        inter_sp = sp;
        inter_pc = pc;
        assign_eval_cost_inl();
        test_efun_args(instruction, 2, sp-1);
        sp = (*efun_table[instruction-TEFUN_OFFSET])(sp);
#ifdef CHECK_OBJECT_REF
        check_all_object_shadows();
#endif /* CHECK_OBJECT_REF */
        break;
    }

    CASE(F_EFUN3);                  /* --- efun3 <code>        --- */
    {
        /* Call the tabled efun EFUN3_OFFSET + <code>, where <code> is
         * a uint8.
         * The efun takes three arguments.
         */

        int code;

        code = LOAD_UINT8(pc);
        instruction = code + EFUN3_OFFSET;

        /* Correct then number of arguments on the stack */
        if (use_ap)
        {
            int numarg = sp - ap + 1;
            int def;

            if (numarg == 2 && (def = instrs[instruction].Default) != 0)
            {
                put_default_argument(++sp, def);
                numarg++;
            }

            if (numarg < 3)
                ERRORF(("Not enough args for %s: got %d, expected 3.\n"
                       , instrs[instruction].name, numarg));
            if (numarg > 3)
                ERRORF(("Too many args for %s: got %d, expected 3.\n"
                       , instrs[instruction].name, numarg));
            use_ap = MY_FALSE;
        }


#ifdef TRACE_CODE
        previous_instruction[last] = instruction;
#endif
#ifdef OPCPROF
        opcount[instruction]++;
#endif
        inter_sp = sp;
        inter_pc = pc;
        assign_eval_cost_inl();
        test_efun_args(instruction, 3, sp-2);
        sp = (*efun_table[instruction-TEFUN_OFFSET])(sp);
#ifdef CHECK_OBJECT_REF
        check_all_object_shadows();
#endif /* CHECK_OBJECT_REF */
        break;
    }

    CASE(F_EFUN4);                  /* --- efun4 <code>        --- */
    {
        /* Call the tabled efun EFUN4_OFFSET + <code>, where <code> is
         * a uint8.
         * The efun takes four arguments.
         */

        int code;

        code = LOAD_UINT8(pc);
        instruction = code + EFUN4_OFFSET;

        /* Correct then number of arguments on the stack */
        if (use_ap)
        {
            int numarg = sp - ap + 1;
            int def;

            if (numarg == 3 && (def = instrs[instruction].Default) != 0)
            {
                put_default_argument(++sp, def);
                numarg++;
            }

            if (numarg < 4)
                ERRORF(("Not enough args for %s: got %d, expected 4.\n"
                       , instrs[instruction].name, numarg));
            if (numarg > 4)
                ERRORF(("Too many args for %s: got %d, expected 4.\n"
                       , instrs[instruction].name, numarg));
            use_ap = MY_FALSE;
        }

#ifdef TRACE_CODE
        previous_instruction[last] = instruction;
#endif
#ifdef OPCPROF
        opcount[instruction]++;
#endif
        inter_sp = sp;
        inter_pc = pc;
        assign_eval_cost_inl();
        test_efun_args(instruction, 4, sp-3);
        sp = (*efun_table[instruction-TEFUN_OFFSET])(sp);
#ifdef CHECK_OBJECT_REF
        check_all_object_shadows();
#endif /* CHECK_OBJECT_REF */
        break;
    }

    CASE(F_EFUNV);                  /* --- efunv <code>        --- */
    {
        /* Call the tabled efun EFUNV_OFFSET + <code>, where <code> is
         * a uint8, with the number of arguments determined through the
         * ap pointer.
         * The number of arguments accepted by the efun is given by the
         * .min_arg and .max_arg entries in the instrs[] table.
         */

        int code;
        int min_arg, max_arg, numarg;

        code = LOAD_UINT8(pc);
        instruction = code + EFUNV_OFFSET;

        numarg = sp - ap + 1;
        use_ap = MY_FALSE;

#ifdef TRACE_CODE
        previous_instruction[last] = instruction;
#endif
#ifdef OPCPROF
        opcount[instruction]++;
#endif

        inter_sp = sp;
        inter_pc = pc;
        assign_eval_cost_inl();

        min_arg = instrs[instruction].min_arg;
        max_arg = instrs[instruction].max_arg;

        if (numarg < min_arg)
            ERRORF(("Not enough args for %s: got %d, expected %d.\n"
                   , instrs[instruction].name, numarg, min_arg));
        if (max_arg >= 0 && numarg > max_arg)
            ERRORF(("Too many args for %s: got %d, expected %d.\n"
                   , instrs[instruction].name, numarg, max_arg));

        test_efun_args(instruction, numarg, sp-numarg+1);
        sp = (*vefun_table[instruction-EFUNV_OFFSET])(sp, numarg);
#ifdef CHECK_OBJECT_REF
        check_all_object_shadows();
#endif /* CHECK_OBJECT_REF */
        break;
    }

    /* --- Predefined functions with counterparts in LPC --- */

    CASE(F_IDENTIFIER);             /* --- identifier <var_ix> --- */
        /* Push value of object variable <var_ix>.
         * It is possible that it is a variable that points to
         * a destructed object. In that case, it has to be replaced by 0.
         *
         * <var_ix> is a uint8.
         */
        sp++;
        assign_rvalue_no_free(sp, find_value((int)(LOAD_UINT8(pc))) );
        break;

    CASE(F_STRING);                /* --- string <ix>          --- */
    {
        /* Push the string current_strings[<ix>] onto the stack,
         * <ix> being a (16-Bit) ushort, stored low byte first.
         * See also the F_CSTRINGx functions.
         */
        unsigned short string_number;

        LOAD_SHORT(string_number, pc);
        push_ref_prog_string(string_number);
        break;
    }

    CASE(F_CSTRING3);               /* --- cstring3 <ix>       --- */
    {
        /* Push the string current_strings[0x3<ix>] onto the stack.
         * <ix> is a 8-Bit uint.
         */
        unsigned int ix = LOAD_UINT8(pc);
        push_ref_prog_string(ix+0x300);
        break;
    }

    CASE(F_CSTRING2);               /* --- cstring2 <ix>       --- */
    {
        /* Push the string current_strings[0x2<ix>] onto the stack.
         * <ix> is a 8-Bit uint.
         */
        unsigned int ix = LOAD_UINT8(pc);
        push_ref_prog_string(ix+0x200);
        break;
    }

    CASE(F_CSTRING1);               /* --- cstring1 <ix>       --- */
    {
        /* Push the string current_strings[0x1<ix>] onto the stack.
         * <ix> is a 8-Bit uint.
         */
        unsigned int ix = LOAD_UINT8(pc);
        push_ref_prog_string(ix+0x100);
        break;
    }

    CASE(F_CSTRING0);               /* --- cstring0 <ix>       --- */
    {
        /* Push the string current_strings[0x0<ix>] onto the stack.
         * <ix> is a 8-Bit uint.
         */
        unsigned int ix = LOAD_UINT8(pc);
        push_ref_prog_string(ix);
        break;
    }

    CASE(F_NUMBER);                 /* --- number <num>        --- */
    {
        /* Push the number <num> onto the stack.
         * <num> is a p_int stored in the host format.
         * See also the F_CONSTx functions.
         * TODO: It should be rewritten to use the LOAD_ macros (but
         * TODO:: then the compiler needs to use them, too.
         */
        sp++;
        sp->type = T_NUMBER;
        memcpy(&sp->u.number, pc, sizeof sp->u.number);
        pc += sizeof sp->u.number;
        break;
    }

    CASE(F_CONST0);                 /* --- const0              --- */
        /* Push the number 0 onto the stack.
         */
        push_number(sp, 0);
        break;

    CASE(F_CONST1);                 /* --- const1              --- */
        /* Push the number 1 onto the stack.
         */
        push_number(sp, 1);
        break;

    CASE(F_NCONST1);                /* --- nconst1             --- */
        /* Push the number -1 onto the stack.
         */
        push_number(sp, -1);
        break;

    CASE(F_CLIT);                   /* --- clit <num>          --- */
    {
        /* Push the number <num> onto the stack.
         * <num> is a 8-Bit uint.
         */
        push_number(sp, (p_int)LOAD_UINT8(pc));
        break;
    }

    CASE(F_NCLIT);                  /* --- nclit <num>         --- */
    {
        /* Push the number -<num> onto the stack.
         * <num> is a 8-Bit uint.
         */
        push_number(sp, -(p_int)LOAD_UINT8(pc));
        break;
    }

    CASE(F_FCONST0);                /* --- fconst0             --- */
    {
        /* Push the float 0.0 onto the stack.
         * The binary format is the one determined by STORE_DOUBLE in
         * datatypes.h
         */

        sp++;
        sp->type = T_FLOAT;
        STORE_DOUBLE(sp, 0.0);
        break;
    }

    CASE(F_FLOAT);                  /* --- float <mant> <exp>  --- */
                                    /* --- float double  --- */
    {
        /* Push a float onto the stack. The binary format is the one determined
         * by STORE_DOUBLE in svalue.h.
         * The float is either build from <mant> (4 bytes) and <exp> (2 bytes)
         * or a double (8 bytes) is directly read from the bytecode.
         */

        sp++;
        sp->type = T_FLOAT;
#ifdef FLOAT_FORMAT_2
        STORE_DOUBLE(sp,load_double(&pc));
#else
        // TODO: It would be reasonable to use JOIN_DOUBLE + STORE_DOUBLE here.
        int32_t mantissa = load_uint32(&pc);
        int16_t exponent = load_uint16(&pc);
        sp->u.mantissa = mantissa;
        sp->x.exponent = exponent;
#endif // FLOAT_FORMAT_2
        break;
    }

    CASE(F_CLOSURE);            /* --- closure <ix> <inhIndex> --- */
    CASE(F_CONTEXT_CLOSURE); /* --- context_closure <ix> <vix> <num_ex> <num_im> --- */
    {
        /* Push the closure value <ix> and <inhIndex> onto the stack.
         * Both <ix> and <inhIndex> are uint16, stored low byte first.
         *
         * For <ix>:
         * Values 0xf000..0xffff are efun and simul-efun symbols, the others
         * are operators and literals (0xf000 == CLOSURE_EFUN_OFFS)
         * Simul-efun symbols (0xf800..0xffff) and true efun symbolx (0xf000..
         * 0xf7ff for which instrs[].Default >= 0) are made signed and stored
         * as they are. (0xf800 == CLOSURE_SIMUL_EFUN_OFFS)
         * Operator symbols (0xf000..0xf7ff for which instrs[].Default == -1)
         * are moved into their 0xe800..0xefff range, then made signed and
         * stored.
#ifdef USE_PYTHON
         * Values 0xe800..0xeffff are python-defined efun symbols.
#endif
         *
         * For <inhIndex>:
         * If not 0 for lfun closures, it is the (inheritance index + 1)
         * of the directly referenced inherited function.
         *
         * If it is a context closure, the context is sized to 
         * uint16 <num_ex>+<num_in> values, uint16 <num_ex> values
         * are taken from the local variables beginning at <vix>,
         * uint16 <num_im> values are taken from the stack.
         */

        /* TODO: uint16 */ unsigned short tmp_ushort;
        /* TODO: int32 */ int ix;
        /* TODO: uint16 */ unsigned short inhIndex;
        unsigned short explicit_context_size, implicit_context_size;
        svalue_t * explicit_context;

        inhIndex = 0;
        explicit_context_size = implicit_context_size = 0;
        LOAD_SHORT(tmp_ushort, pc);
        if (instruction == F_CONTEXT_CLOSURE)
        {
            explicit_context = fp + LOAD_UINT8(pc);
            LOAD_SHORT(explicit_context_size, pc);
            LOAD_SHORT(implicit_context_size, pc);
        }
        else
        {
            explicit_context = NULL; /* Makes the compiler happy. */
            LOAD_SHORT(inhIndex, pc);
        }

        ix = tmp_ushort;
#ifdef USE_PYTHON
        if (ix < CLOSURE_PYTHON_EFUN_OFFS)
#else
        if (ix < CLOSURE_EFUN_OFFS)
#endif
        {
            inter_sp = sp;
            inter_pc = pc;
            sp++;
            closure_literal(sp, ix, inhIndex, explicit_context_size + implicit_context_size);
            /* If out of memory, this will set sp to svalue-0 and
             * throw an error.
             */

#ifdef DEBUG
            if (instruction == F_CONTEXT_CLOSURE
             && sp->x.closure_type != CLOSURE_LFUN
               )
                fatal("(eval_instruction) context_closure used for non-lfun "
                      "closure type %d.\n", sp->x.closure_type);
#endif
            /* Now copy the context values */
            if (explicit_context_size != 0)
            {
                unsigned short i;
                svalue_t * context = sp->u.lambda->context;

                for (i = 0; i < explicit_context_size; i++)
                {
                    transfer_svalue_no_free(context+i, explicit_context+i);

                    /* Set it to T_INVALID, as it is still a variable of
                     * the function frame and will be freed on return.
                     */
                    explicit_context[i].type = T_INVALID;
                }
            }

            if (implicit_context_size != 0)
            {
                unsigned short i;
                svalue_t * arg = sp - implicit_context_size;
                svalue_t * context = sp->u.lambda->context + explicit_context_size;

                for (i = 0; i < implicit_context_size; i++)
                    transfer_svalue_no_free(context+i, arg+i);

                /* Now move the created closure to the new top of the stack */
                *arg = *sp;
                inter_sp = sp = arg;
            }
        }
        else
        {
#ifdef DEBUG
            if (instruction == F_CONTEXT_CLOSURE)
                fatal("(eval_instruction) context_closure used for non-lfun.\n");
#endif
            sp++;
            sp->type = T_CLOSURE;
            sp->u.ob = ref_object(current_object, "closure");
            if (ix >= CLOSURE_SIMUL_EFUN_OFFS)
            {
                /* Sefun closure */
                sp->x.closure_type = (short)ix;
            }
#ifdef USE_PYTHON
            else if (ix < CLOSURE_EFUN_OFFS)
            {
                /* python-defined efun closure */
                sp->x.closure_type = (short)ix;
            }
#endif
            else
            {
                /* Efun or operator closure */
                if (!runtime_no_warn_deprecated
                 && instrs[ix - CLOSURE_EFUN_OFFS].deprecated != NULL)
                {
                    WARNF(("Warning: %s() is deprecated: %s\n"
                          , instrs[ix - CLOSURE_EFUN_OFFS].name
                          , instrs[ix - CLOSURE_EFUN_OFFS].deprecated
                         ));
                }

                sp->x.closure_type
                  = (short)(  instrs[ix - CLOSURE_EFUN_OFFS].Default == -1
                            ? ix + CLOSURE_OPERATOR-CLOSURE_EFUN
                            : ix);
            }
        }
        break;
    }

    CASE(F_SYMBOL);                 /* --- symbol <ix> <num>   --- */
    {
        /* Push a symbol of current_strings[<ix>] with <num> quotes
         * onto the stack.
         * <ix> is a uint16, stored low byte first. <num> is a uint8.
         */

        /* TODO: uint16 */ unsigned short string_number;

        LOAD_SHORT(string_number, pc);

        sp++;
        sp->type = T_SYMBOL;
        sp->x.quotes = LOAD_UINT8(pc);
        sp->u.str = ref_mstring(current_strings[string_number]);
        break;
    }

    CASE(F_DEFAULT_RETURN);         /* --- default_return      --- */
        /* Inserted at the end of value-returning function, this instruction
         * provides a default 'return 0' in case the programmer forgot about
         * it. The instruction also prints a warning so that the code can be
         * corrected.
         */
        warnf("Missing 'return <value>' statement.\n");

        /* Warn only once per missing return and program. */
        PUT_UINT8(pc-1, F_RETURN0);
        /* FALLTHROUGH */

    CASE(F_RETURN0);                /* --- return0             --- */
        /* Return from the function with result value 0.
         */
        push_number(sp, 0);
        /* FALLTHROUGH */

    CASE(F_RETURN);                 /* --- return              --- */
    {
        /* Return from the function with the result topmost on the stack.
         * If this is an .extern_call, eval_instruction()
         * is left here.
         */

        svalue_t *pResult;  /* Return value on stack */
        svalue_t *efp = fp+csp->num_local_variables; /* Expected end of frame */

        pResult = sp;

        /* Remove any intermediate error contexts */
        while (csp->catch_call)
        {
            pop_control_stack();
            pop_error_context();
        }

        /* The caller might have a yet-unterminated SAVE_ARG_FRAME in
         * effect (this can happen in lambda closures, when the subclosure
         * to compute an efun argument executes a #'return) - undo them.
         */
        
        while (ap && ap > efp)
        {
            while (sp > ap)
                free_svalue(--sp);
            sp = ap-1;
            ap = sp->u.lvalue;
        }

        /* Deallocate frame, but not the result value.
         */
#ifdef DEBUG
        if (efp > sp)
            fatal("Bad stack at F_RETURN, %"PRIdMPINT" values too low\n"
                 , (mp_int)(efp - sp));
        /* In lambda closures the F_RETURN can occur anywhere, even in the
         * middle of an expression, so we allow residual values on the stack
         * then. The loop below will take care of them anyway.
         */
        else if (efp < sp && (current_lambda.type != T_CLOSURE
              || (current_lambda.x.closure_type != CLOSURE_LAMBDA
               && current_lambda.x.closure_type != CLOSURE_BOUND_LAMBDA
               && current_lambda.x.closure_type != CLOSURE_UNBOUND_LAMBDA)))
            fatal("Bad stack at F_RETURN, %"PRIdMPINT" values too high\n"
                 , (mp_int)(sp - efp));
#endif
        while (sp != fp)
        {
            free_svalue(--sp);
        }
        *sp = *pResult;

        // check return type. Runtime type checks have to be enabled by the 
        // program, we must not return from a Lambda (only CLOSURE_LFUN has
        // type data) or efun- or operator closure.
        // (IMHO funstart == SIMUL_EFUN_FUNSTART can't happen here.)
        if (current_prog && current_prog->flags & P_RTT_CHECKS
            && !(current_lambda.type == T_CLOSURE && current_lambda.x.closure_type != CLOSURE_LFUN)
            && csp->funstart != EFUN_FUNSTART)
        {
            function_t *header = current_prog->function_headers + FUNCTION_HEADER_INDEX(csp->funstart);

            if (!check_rtt_compatibility(header->type, sp))
            {
                static char buff[512];
                lpctype_t *realtype = get_rtt_type(header->type, sp);
                get_lpctype_name_buf(realtype, buff, sizeof(buff));
                free_lpctype(realtype);

                inter_sp = sp;
                if (current_prog->flags & P_WARN_RTT_CHECKS)
                    warnf("Bad return type in %s(): got '%s', expected '%s'.\n",
                       get_txt(header->name), buff,
                       get_lpctype_name(header->type));
                else
                    errorf("Bad return type in %s(): got '%s', expected '%s'.\n",
                       get_txt(header->name), buff,
                       get_lpctype_name(header->type));
            }
        }
        
        /* Restore the previous execution context */
        current_prog = csp->prog;
        if ( NULL != current_prog ) /* is 0 when we reach the bottom */
        {
            current_strings = current_prog->strings;
        }

        function_index_offset = csp->function_index_offset;
        variable_index_offset = csp->variable_index_offset;
        current_variables     = csp->current_variables;
        break_sp = csp->break_sp;
        inter_context = csp->context;
        if (current_lambda.type == T_CLOSURE)
            free_closure(&current_lambda);
        current_lambda = csp->lambda;

        tracedepth--; /* We leave this level */

        if (csp->extern_call)
        {
            /* eval_instruction() must be left - setup the globals */

            assign_eval_cost_inl();
            current_object = csp->ob;
            previous_ob = csp->prev_ob;
            inter_pc = csp->pc;
            inter_fp = csp->fp;

            if (trace_level)
            {
                do_trace_return(sp);
                if (csp == CONTROL_STACK - 2)
                    /* TODO: This can't be legal according to ISO C */
                    traceing_recursion = -1;
            }
            csp--;
            inter_sp = sp;
#ifdef CHECK_OBJECT_REF
            check_all_object_shadows();
#endif /* CHECK_OBJECT_REF */
            return MY_FALSE;
        }

        /* We stay in eval_instruction() */

        if (trace_level)
            do_trace_return(sp);
        pc = csp->pc;
        fp = csp->fp;
        csp--;
        break;
    }

    CASE(F_BREAK);                  /* --- break               --- */
    {
        /* Break out of a switch() by pulling the continuation address
         * from the break stack.
         */

        pc = break_sp->u.break_addr;
        break_sp++;
        break;
    }

    CASE(F_SWITCH);            /* --- switch <lots of data...> --- */
    {
        /* The switch()-Statement: pop the topmost value from the stack,
         * search it in the given case values and set the pc to the
         * associated code. Also push the address of the next instruction
         * as break address onto the break stack.
         *
         * The compiler makes sure that there is always a 'default' case
         * and that all execution paths eventually execute a F_BREAK.
         *
         * The layout created by the LPC compiler is this:
         *
         *     switch b1 a2 b2 [b3 [b4] ]
         *            instructions (sans the first byte 'i0')...
         *            l[]
         *            [c0 [c1]]
         *            a0 a1 i0
         *            v*n
         *            o*n
         *            [d0]
         *
         * b1 & 0x03 is 0, marking this switch statement as unaligned.
         * Since for an efficient search the tables v*n and o*n must be
         * 4-Byte aligned (TODO: on some machines 8-Byte), the interpreter
         * will on first execution of such a switch align it (using
         * closure:align_switch()) by arranging the bytes a0..a2 around
         * the tables. The aligned layout is this:
         *
         *     switch b1 b2 [b3 [b4] ]
         *            instructions...
         *            l[]
         *            [c0 [c1]]            <-- p0 = pc + offset
         *            a0..
         *            v[]                  <-- tabstart
         *            o[]                  <-- end_tab = pc + offset + tablen
         *            ..a2                 <-- p1
         *            [d0]
         *
         *  b1 (bits 1..0) = len: the length in bytes needed to store
         *        'offset', 'tablen', 'default offset', 'o*n' and the
         *        length of lookup tables for table ranges.
         *  b1 (bits 7..2) = tablen lo
         *  c0 = tablen mid (optional)
         *  c1 = tablen hi  (optional)
         *  b2 = offset lo
         *  b3 = offset med (optional)
         *  b4 = offset hi  (optional)
         *  a0, a1 = default-case offset lo and med in host byte order
         *  d0     = default-case offset hi (optional)
         *  a2 'type' (bits 0..4): start position for search (used to index
         *                         a table with the real offsets)
         *            (bit  5)   : 0: numeric switch , 1: string switch
         *            (bits 6..7): in an unaligned switch, the true value
         *                         of <len> (b1, bits 1..0).
         *  l[]: range lookup table: each <len> bytes, network byte order
         *       (numeric switch only)
         *  v[]: case values, string_t* or p_int, host byte order
         *  o[]: case offsets : each <len> bytes, network byte order
         *
         * The case value table v[] holds (sorted numerically) all values
         * appearing in the case statements, both singular values and range
         * bounds. Range bound values (which are inclusive) always appear
         * next to each other.
         *
         * The offset table o[] holds the associated offset with
         * this interpretation:
         *
         *   singular cases: jump destination offsets relative to pc.
         *
         *   range cases:    the 'offset' for the lower bound is 1, the
         *                   offset for the upper bound gives the jump
         *                   destination relative to pc.
         *
         *   lookup ranges:  the 'offset' for the lower bound is 0, the
         *                   offset for the upper bound is an offset
         *                   pointing into the lookup table.
         *                   The real jump offset is then
         *                     l[o[i] + <value> - lower-bound].
         *
         *   The lookup ranges are used for an efficient implementation of
         *   sparse ranges like 'case 0: case 2: case 5: ...'.
         *
         *   TODO: This code still makes too many un-macro'ed mem accesses.
         */

        Bool useDefault; /* TRUE: Immediately jump to the default case */
        mp_int offset;  /* Length of instruction and range-table area */
        mp_int def_offs;  /* Offset to code for the 'default' case */
        int tablen; /* Number of single case entries, multiplied by 4 */
        int len;    /* Number of bytes per offset/length value (1..3) */
        int type;   /* Start position for search */
        static int32 off_tab[] = {
                0*sizeof(char*), 0x00001*sizeof(char*), 0x00003*sizeof(char*),
          0x00007*sizeof(char*), 0x0000f*sizeof(char*), 0x0001f*sizeof(char*),
          0x0003f*sizeof(char*), 0x0007f*sizeof(char*), 0x000ff*sizeof(char*),
          0x001ff*sizeof(char*), 0x003ff*sizeof(char*), 0x007ff*sizeof(char*),
          0x00fff*sizeof(char*), 0x01fff*sizeof(char*), 0x03fff*sizeof(char*),
          0x07fff*sizeof(char*), 0x0ffff*sizeof(char*), 0x1ffff*sizeof(char*),
          0x3ffff*sizeof(char*), 0x7ffff*sizeof(char*)
        };
          /* Start offsets for the binary search for different table sizes.
           * This table is indexed by <type> & 0x1f, and the compiler choses
           * the start position to be the first power of 2 which is at least
           * half the table size. This way the search algorithm only needs
           * to check for the upper table end.
           * TODO: Is the choice really so?
           */
        bytecode_p p0;
          /* Points after the range lookup tables (initially). */
        bytecode_p p1;
          /* Points to the table of offsets. */
        bytecode_p tabstart;
          /* Points to the 'v*n' table of cases */
        bytecode_p end_tab;
          /* Points to the 'o*n' table of offsets for the cases */
        bytecode_p break_addr;
          /* Address of the first bytecode after the switch, will be pushed
           * onto the break stack.
           */
        mp_int s;
          /* Search value for the lookup, derived from the stack value.
           * It is either u.number or the numeric value of u.string.
           */
        /* TODO: opcode? */ unsigned char *l;
          /* Current search pointer into the value table v[] */
        mp_int r;
          /* Current value retrieved from *<l> */
        mp_int d;
          /* Half the distance between <l> and the current upper resp. lower
           * bound of the search partition
           */
        /* TODO: opcode? */ unsigned char *p2;
          /* For a found case, the pointer into o[] */
        mp_int o0, o1;
          /* The offsets read from *(p2-1) and *p2, resp. *p2 and *(p2+1) */
        int i; /* Temporary */

        /* Extract the basic tablen and len */
        tablen = EXTRACT_UCHAR(pc);
        if ( !(len = tablen & SWITCH_VALUELEN) )
        {
            /* Oops, first lets align the switch */
            align_switch(pc);
            tablen = EXTRACT_UCHAR(pc);
            len = tablen & SWITCH_VALUELEN;
        }
        tablen &= ~SWITCH_VALUELEN;
        /* SWITCH_TABLEN_SHIFT is 2, so don't need to do
         *   tablen = (tablen >> SWITCH_TABLEN_SHIFT) * 4
         */

        /* Get the offset, aka the length of instruction and range table
         * part, and let p0 point after them.
         */
        offset = EXTRACT_UCHAR(pc+1);
        if (len > 1)
        {
            offset += EXTRACT_UCHAR(pc+2) << 8;
            if (len > 2)
            {
                offset += EXTRACT_UCHAR(pc+3) << 16;
            }
        }
        p0 = pc + offset;

        /* Get the full tablen, aka the number of single case entries,
         * and set p1 to point _after_ the offset table 'o*n'.
         * The computed formula is
         *
         *   p1 = p0 + tablen * sizeof(char*) + tablen * len * sizeof(char)
         *               (length of v*n)           (length of o*n)
         *
         * with the code taking into account that the _variable_ tablen
         * already comes as 'tablen * sizeof(char*)'.
         */
        if (len > 1)
        {
            tablen += *(unsigned char *)(p0++) << 8;
            if (len > 2)
            {
                tablen += *(unsigned char *)(p0++) << 16;
#if SIZEOF_CHAR_P == 4
                p1 = (unsigned char *)(p0 + (tablen << 1) - (tablen >> 2));
            }
            else
            {
                p1 = (unsigned char *)(p0 + tablen + (tablen >> 1));
#else
                p1 = (unsigned char *)(p0 + tablen + tablen*3/sizeof(p_int) );
            }
            else
            {
                p1 = (unsigned char *)(p0 + tablen + tablen*2/sizeof(p_int) );
#endif
            }
        }
        else
        {
            p1 = (unsigned char *)(p0 + tablen + tablen / sizeof(p_int) );
        }

        /* Gather the 'default offset' and the 'type' from the alignment
         * bytes before v[] (pointer to by p0) and the bytes after
         * o[] (pointed to by p1).
         * Set 'tabstart' to the real start of 'v*n'.
         * Set 'break_addr' to the first instruction after the switch.
         */

        {
            int a, b;
            union { unsigned char b[sizeof(p_int)-1]; unsigned short s; } abuf;
              /* TODO: Assumes sizeof(p_int)-1 >= sizeof(short) */
              /* TODO: Assumes sizeof(p_int) == 4 */
              /* TODO: Assumes sizeof(short) == 2 */

            /* Gather the bytes a0..a2 into abuf.b[] */
            b = (int)(((p_int)p0-1) & sizeof abuf.b);
              /* The number of a-bytes after 'o*n' */
            memcpy((char *)abuf.b, p0, sizeof abuf.b);
            a = (int)(sizeof abuf.b - b);
              /* The number of remaining bytes */
            memcpy((char *)(abuf.b + a), (char *)(p1 + a), (size_t)b);
            def_offs = abuf.s;
            type = abuf.b[2];
            if (len > 2)
            {
                def_offs += p1[sizeof(p_int)-1] << 16;
                break_addr = p1 + sizeof(p_int);
            }
            else
            {
                break_addr = p1 + sizeof(p_int)-1;
            }
            tabstart = p0 + a;
        }

        /* Set 'end_tab' to point to the 'o*n' table,
         * push the break address onto the break stack.
         */
        end_tab  = tabstart + tablen;
        break_sp--;
        break_sp->type = T_BREAK_ADDR;
        break_sp->u.break_addr = break_addr;

        /* Get the search value from the argument passed on the
         * stack. This also does the type checking.
         */
        useDefault = MY_FALSE;
        if (type & SWITCH_TYPE)
        {
            /* String switch */

            if ( sp->type == T_NUMBER && !sp->u.number )
            {
                /* Special case: uninitialized string '0'.
                 * Use a magic value for this one.
                 */
                s = (mp_int)ZERO_AS_STR_CASE_LABEL;
            }
            else if ( sp->type == T_STRING || sp->type == T_BYTES)
            {
                /* The case strings in the program shared, so whatever
                 * string we get on the stack, it must at least have
                 * a shared twin to be sensible. Get the address of
                 * that twin.
                 */
                s = (mp_int)find_tabled(sp->u.str);
            }
            else
            {
                /* Non-string value for string switch: use default */
                useDefault = MY_TRUE;
                s = 0;
            }
        }
        else if (sp->type == T_NUMBER)
        {
            /* Numeric switch and number given */
            s = sp->u.number;
        }
        else
        {
            /* Non-number value for numeric switch: use default */
            useDefault = MY_TRUE;
            s = 0;
        }
        pop_stack();

        if (useDefault)
        {
            o1 = def_offs;
        }
        else
        {
            /* Setup the binary search:
             *   l points roughly into the middle of the table,
             *   d is 1/4 of the (assumed) total size of the table
             */
            i = type & SWITCH_START;
            l = tabstart + off_tab[i];
            d = (mp_int)((off_tab[i]+sizeof(p_int)) >> 1 & ~(sizeof(p_int)-1));
              /* '+sizeof()' to make the off_tab[] value even and non-0 */

            /* Binary search for the value <s> in the table, starting at
             * position <l> and first subdivision size <d>.
             * The algorithm runs until <d> falls below the size of a case value
             * (sizeof(p_int)).
             *
             * After the loop terminates, o1 will be the jump offset relative
             * to the pc, which might be the 'default' offset if the value <s>
             * was not found.
             */
            for(;;)
            {
                r = *(p_int*)l; /* Get the case value */

                if (s < r)
                {

                    /* --- s < r --- */

                    if (d < (mp_int)sizeof(p_int))
                    {
                        if (!d)
                        {
                            /* End of search: s not found.
                             *
                             * Set p2 to the offset matching <l> and retrieve
                             * o0 and o1 from there.
                             *
                             * s might still be in a range, then <l>/<p2> point to
                             * the entries for the upper bound.
                             */
                            p2 =   tabstart + tablen
                                 + ((p_int*)l - (p_int*)tabstart)*len;
                            o0 = EXTRACT_UCHAR(p2-1);
                            o1 = EXTRACT_UCHAR(p2);
                            if (len > 1)
                            {
                                o0 += EXTRACT_UCHAR(p2-2) << 8;
                                o1 = EXTRACT_UCHAR(p2+1) + (o1 << 8);
                                if (len > 2)
                                {
                                    o0 += EXTRACT_UCHAR(p2-3) << 16;
                                    o1 = EXTRACT_UCHAR(p2+2) + (o1 << 8);
                                }
                            }
                            /* Because the pre-table alignment area is in the
                             * indexing underflow memory region, we can't make
                             * useful predictions on the peeked o0 value in case
                             * of underflow.
                             */

                            /* Test for a range */

                            if (o0 <= 1 && l > tabstart)
                            {
                                /* No indexing underflow: test if s is in range */

                                r = ((p_int*)l)[-1]; /* the lower bound */
                                if (s >= r)
                                {
                                    /* s is in the range */
                                    if (!o0)
                                    {
                                        /* Look up the real jump offset */
                                        l = pc + o1 + (s-r) * len;
                                        o1 = 0;
                                        i = len;
                                        do {
                                            o1 = (o1 << 8) + *l++;
                                        } while (--i);
                                        break;
                                    }
                                    /* o1 holds jump destination */
                                    break;
                                }
                                /* s is not in the range */
                            }

                            /* <s> not found at all: use 'default' address */
                            o1 = def_offs;

                            /* o1 holds jump destination */
                            break;
                        } /* if (!d) */

                        /* Here is 0 < d < sizeof(p_int).
                         * Set d = 0 and finish the loop in the next
                         * iteration.
                         * TODO: Why the delay?
                         */
                        d = 0;
                    }
                    else
                    {
                        /* Move <l> down and half the partition size <d>. */
                        l -= d;
                        d >>= 1;
                    }
                }
                else if (s > r)
                {

                    /* --- s > r --- */

                    if (d < (mp_int)sizeof(p_int))
                    {
                        if (!d)
                        {
                            /* End of search: s not found.
                             *
                             * Set p2 to the offset matching <l> and retrieve
                             * o0 and o1 from there.
                             *
                             * s might still be in a range, then <l> points to
                             * the entry of the lower bound, and <p2> is set to
                             * the entry for the upper bound.
                             */
                            p2 = tabstart + tablen
                                 + (((p_int*)l - (p_int*)tabstart) + 1)*len;
                            o0 = EXTRACT_UCHAR(p2-1);
                            o1 = EXTRACT_UCHAR(p2);
                            if (len > 1)
                            {
                                o0 += EXTRACT_UCHAR(p2-2) << 8;
                                o1 = EXTRACT_UCHAR(p2+1) + (o1 << 8);
                                if (len > 2)
                                {
                                    o0 += EXTRACT_UCHAR(p2-3) << 16;
                                    o1 = EXTRACT_UCHAR(p2+2) + (o1 << 8);
                                }
                            }

                            /* Test for a range */

                            if (o0 <= 1)
                            {
                                /* It is a range. */

                                if (s <= ((p_int*)l)[1])
                                {
                                    /* s is in the range, and r is already correct
                                     * (ie the upper bound)
                                     */
                                    if (!o0)
                                    {
                                        /* Lookup the real jump offset */
                                        l = pc + o1 + (s-r) * len;
                                        o1 = 0;
                                        i = len;
                                        do {
                                            o1 = (o1 << 8) + *l++;
                                        } while (--i);
                                        break;
                                    }
                                    /* o1 holds jump destination */
                                    break;
                                }
                                /* s is not in the range */
                            }

                            /* <s> not found at all: use 'default' address */
                            o1 = def_offs;

                            /* o1 holds jump destination */
                            break;
                        } /* !d */

                        /* Here is 0 < d < sizeof(p_int).
                         * Set d = 0 and finish the loop in the next
                         * iteration.
                         * TODO: Why the delay?
                         */
                        d = 0;
                    }
                    else
                    {
                        /* Move <l> up, and half the partition size <d>
                         * If this would push l beyond the table, repeat the
                         * steps 'move <l> down and half the partition size'
                         * until <l> is within the table again.
                         */

                        l += d;
                        while (l >= end_tab)
                        {
                            d >>= 1;
                            if (d <= (mp_int)sizeof(p_int)/2)
                            {
                                /* We can't move l further - finish the loop */
                                l -= sizeof(p_int);
                                d = 0;
                                break;
                            }
                            l -= d;
                        }
                        d >>= 1;
                    }
                }
                else
                {
                    /* --- s == r --- */

                    /* End of search: s found.
                     *
                     * Set p2 to the offset matching <l> and retrieve
                     * o0 and o1 from there.
                     *
                     * We don't distinguish between a singular case match
                     * and a match with an upper range bound, but we have
                     * to take extra steps in case <s> matched a lower range
                     * bound. In that light, o0 need not be an exact value.
                     */
                    p2 = tabstart + tablen + ((p_int*)l - (p_int*)tabstart)*len;
                    o0 = EXTRACT_UCHAR(p2-1);
                    o1 = EXTRACT_UCHAR(p2);
                    if (len > 1)
                    {
                        o0 |= EXTRACT_UCHAR(p2-2);
                        o1 = EXTRACT_UCHAR(p2+1) + (o1 << 8);
                        if (len > 2)
                        {
                            o0 |= EXTRACT_UCHAR(p2-3);
                            o1 = EXTRACT_UCHAR(p2+2) + (o1 << 8);
                        }
                    }

                    /* Test if <s> matched the end of a range with a lookup table.
                     */
                    /* TODO: Does this mean that the compiler never creates
                     * TODO:: an ordinary range at the beginning of v[]?
                     */
                    if (!o0 && l > tabstart)
                    {
                        r = ((p_int*)l)[-1]; /* the lower bound */
                        l = pc + o1 + (s-r) * len;
                        o1 = 0;
                        i = len;
                        do
                        {
                            o1 = (o1 << 8) + *l++;
                        } while (--i);
                        /* o1 holds jump destination */
                        break;
                    }

                    /* Test if <s> matched the start of a range */
                    if (o1 <= 1)
                    {
                        /* Yup. Realign p2 and reget o1 */
                        p2 += len;

                        /* Set l to point to the jump offset */
                        if (o1)
                        {
                            /* start of ordinary range */
                            l = p2;
                        }
                        else
                        {
                            /* start of range with lookup table */
                            i = len;
                            do {
                                o1 = (o1 << 8) + *p2++;
                            } while (--i);
                            l = pc + o1;
                        }

                        /* Get the jump offset from where <l> points */
                        o1 = 0;
                        i = len;
                        do {
                            o1 = (o1 << 8) + *l++;
                        } while (--i);

                        /* o1 holds jump destination */
                        break;
                    }

                    /* At this point, s was a match with a singular case, and
                     * o1 already holds the jump destination.
                     */
                    break;
                }
            } /* binary search */
        } /* if (useDefault) */

        /* o1 is now the offset to jump to. */
        pc += o1;
        break;
    }

    CASE(F_LOCAL);                  /* --- local <ix>          --- */

        /* Fetch the value of local variable <ix> and push it
         * onto the stack.
         */
        sp++;
        assign_rvalue_no_free(sp, fp + LOAD_UINT8(pc));
        break;

    CASE(F_CATCH);       /* --- catch <flags> <offset> <guarded code> --- */
    {
        /* catch(...instructions...)
         *
         * Execute the instructions (max. uint8 <offset> bytes) following the
         * catch statement. If an error occurs, or a throw() is executed,
         * catch that exception, push the <catch_value> (a global var)
         * onto the stack and continue execution at instruction
         * <pc>+1+<offset>.
         *
         * The attributes of the catch are given as uint8 <flags>.
         * If CATCH_FLAG_RESERVE is set, the top most stack value denotes
         * the eval cost to reserve for the catch handling - it is removed
         * from the stack before continuing.
         *
         * The implementation is such that a control-stack entry is created
         * as if the instructions following catch are called as a subroutine
         * from <pc>+1+<offset>. Additionally an appropriate error context
         * is pushed. This way the error handling will have the VM 'return'
         * to the right place automatically.
         *
         * The last instruction of the guarded code is F_END_CATCH which
         * will clean up the control and error stack.
         *
         * If the actual guarded code is longer than 256 Bytes, the compiler
         * will generate appropriate branches:
         *
         *                  catch 2
         *                  branch guarded_code
         *                  branch continuation
         *    guarded_code: ...
         */

        uint offset;
        int  flags;
        int32 reserve_cost = CATCH_RESERVED_COST;

        /* Get the flags */
        flags = LOAD_UINT8(pc);

        if (flags & CATCH_FLAG_RESERVE)
        {
            if (sp->type != T_NUMBER)
            {
                ERRORF(("Illegal 'reserve' type for catch(): got %s, expected number.\n"
                       , typename(sp->type)
                       ));
            }

            if (sp->u.number <= 0)
            {
                ERRORF(("Illegal 'reserve' value for catch(): got %"PRIdPINT
                        ", expected a positive value.\n"
                       , sp->u.number
                       ));
            }

            reserve_cost = sp->u.number;
            sp--;
        }
        /* Get the offset to the next instruction after the CATCH statement.
         */
        offset = LOAD_UINT8(pc);

        /* Save the important variables in their global locations */
        inter_pc = pc;
        inter_sp = sp;
        inter_fp = fp;

        /* Perform the catch() */
        if (!catch_instruction(flags, offset
#ifndef __INTEL_COMPILER
                              , (volatile svalue_t ** volatile) &inter_sp
#else
                              , (svalue_t ** volatile) &inter_sp
#endif
                              , inter_pc, inter_fp
                              , reserve_cost
                              , inter_context
                              )
           )
        {
#ifdef CHECK_OBJECT_REF
            check_all_object_shadows();
#endif /* CHECK_OBJECT_REF */
            return MY_FALSE; /* Guarded code terminated with 'return' itself */
        }

        /* Restore the important variables */
        pc = inter_pc;
        sp = inter_sp;
        fp = inter_fp;

        /* Not really necessary, but tells gcc to complain less */
        ap = NULL; /* Will be restored with a restore_arg_frame */
        use_ap = MY_FALSE;
        instruction = F_CATCH;
        num_arg = -1;
#ifdef DEBUG
        expected_stack = NULL;
#endif
        break;
    }

    CASE(F_INC);                    /* --- inc                 --- */
    {
        /* void inc (mixed & sp[0])
         *
         * Increment the (numeric) value designed by the lvalue on top
         * of the stack, then remove the lvalue from the stack.
         */
        TYPE_TEST1(sp, T_LVALUE);

        inter_sp = sp;
        add_number_to_lvalue("++", sp, 1, NULL, NULL);
        pop_stack();
        break;
    }

    CASE(F_DEC);                    /* --- dec                 --- */
    {
        /* void dec (mixed & sp[0])
         *
         * Decrement the (numeric) value designed by the lvalue on top
         * of the stack, then remove the lvalue from the stack.
         */

        TYPE_TEST1(sp, T_LVALUE);

        inter_sp = sp;
        add_number_to_lvalue("--", sp, -1, NULL, NULL);
        pop_stack();
        break;
    }

    CASE(F_POST_INC);               /* --- post_inc            --- */
    {
        /* mixed post_inc (mixed & sp[0])
         *
         * Increment the numeric value designated by the lvalue on top
         * of the stack, and replace the stack entry with the value
         * before the increment.
         */

        svalue_t result;

        TYPE_TEST1(sp, T_LVALUE);

        inter_sp = sp;
        add_number_to_lvalue("++", sp, 1, &result, NULL);
        free_svalue(sp);
        transfer_svalue_no_free(sp, &result);
        break;
    }

    CASE(F_POST_DEC);               /* --- post_dec            --- */
    {
        /* mixed post_dec (mixed & sp[0])
         *
         * Decrement the numeric value designated by the lvalue on top
         * of the stack, and replace the stack entry with the value
         * before the decrement.
         */

        svalue_t result;

        TYPE_TEST1(sp, T_LVALUE);

        inter_sp = sp;
        add_number_to_lvalue("--", sp, -1, &result, NULL);
        free_svalue(sp);
        transfer_svalue_no_free(sp, &result);
        break;
    }

    CASE(F_PRE_INC);                /* --- pre_inc             --- */
    {
        /* mixed pre_inc (mixed & sp[0])
         *
         * Increment the numeric value designated by the lvalue on top
         * of the stack, and replace the stack entry with the incremented
         * value.
         */

        svalue_t result;

        TYPE_TEST1(sp, T_LVALUE);

        inter_sp = sp;
        add_number_to_lvalue("++", sp, 1, NULL, &result);
        free_svalue(sp);
        transfer_svalue_no_free(sp, &result);
        break;
    }

    CASE(F_PRE_DEC);                /* --- pre_dec             --- */
    {
        /* mixed pre_dec (mixed & sp[0])
         *
         * Decrement the numeric value designated by the lvalue on top
         * of the stack, and replace the stack entry with the decremented
         * value.
         */

        svalue_t result;

        TYPE_TEST1(sp, T_LVALUE);

        inter_sp = sp;
        add_number_to_lvalue("--", sp, -1, NULL, &result);
        free_svalue(sp);
        transfer_svalue_no_free(sp, &result);
        break;
    }

    CASE(F_LAND);                   /* --- land <offset>       --- */
    {
        /* If sp[0] is the number 0, leave it on the stack (as result)
         * and branch by <offset>.
         * Otherwise, pop the value and just continue.
         */

        if (sp->type == T_NUMBER)
        {
            if (sp->u.number == 0)
            {
                uint offset = LOAD_UINT8(pc);
                pc += offset;
                break;
            }
            /* No need to explicitly free_svalue(), it's just a number */
        }
        else
        {
            free_svalue(sp);
        }
        sp--;
        pc++;
        break;
    }

    CASE(F_LOR);                    /* --- lor <offset>        --- */
    {
        /* If sp[0] is not the number 0, leave it on the stack (as result)
         * and branch by <offset>.
         * Otherwise, pop the value and just continue.
         */

        if (sp->type == T_NUMBER && sp->u.number == 0)
            sp--; /* think 'free_svalue(sp--)' here... */
        else
            pc += GET_UINT8(pc);
        pc++;
        break;
    }

    CASE(F_ASSIGN);                 /* --- assign              --- */
    {
        /* Assign the value sp[-1] to the value designated by lvalue sp[0].
         * The assigned value sp[-1] remains on the stack as result
         * (ie. the assign yields a rvalue).
         *
         * Make sure that complex destinations like arrays are not freed
         * before the assignment is complete - see the comments to
         * assign_svalue().
         */

#ifdef DEBUG
        if (sp->type != T_LVALUE)
            FATALF(("Bad left arg to F_ASSIGN: got '%s', expected 'lvalue'.\n"
                   , typename(sp->type)
                   ));
#endif
        assign_svalue(sp, sp-1);
        pop_stack();
        break;
    }

    CASE(F_VOID_ASSIGN);            /* --- void_assign         --- */
    {
        /* Assign the value sp[-1] to the value designated by lvalue sp[0],
         * then remove both values from the stack.
         *
         * Make sure that complex destinations like arrays are not freed
         * before the assignment is complete - see the comments to
         * assign_svalue().
         */

#ifdef DEBUG
        if (sp->type != T_LVALUE)
            FATALF(("Bad left arg to F_VOID_ASSIGN: got '%s', expected 'lvalue'.\n"
                   , typename(sp->type)
                   ));
#endif
        transfer_svalue(sp, sp-1);
        pop_stack();
        sp--;
        break;
    }

    CASE(F_ADD);                    /* --- add                 --- */
        /* Add sp[0] to sp[-1] (the order is important), pop both
         * summands from the stack and push the result.
         *
         * Possible type combinations:
         *   string      + (string,int,float) -> string
         *   (int,float) + string             -> string
         *   bytes       + bytes              -> bytes
         *   int         + int                -> int
         *   float       + (int,float)        -> float
         *   int         + float              -> float
         *   vector      + vector             -> vector
         *   mapping     + mapping            -> mapping
         */

        switch ( sp[-1].type )
        {

        case T_BYTES:
            /* Just prevent adding numbers to them... */
            TYPE_TEST_RIGHT(sp, T_BYTES);
            /* FALLTHROUGH */

        case T_STRING:
            inter_pc = pc;
            inter_sp = sp;
            switch ( sp->type )
            {
            case T_STRING:
            case T_BYTES:
              {
                string_t *left, *right, *res;

                TYPE_TEST_RIGHT(sp, sp[-1].type);

                left = (sp-1)->u.str;
                right = sp->u.str;

                DYN_STRING_COST(mstrsize(left) + mstrsize(right))
                res = mstr_add(left, right);
                if (!res)
                    ERRORF(("Out of memory (%zu bytes)\n"
                           , mstrsize(left) + mstrsize(right)
                           ));
                free_string_svalue(sp);
                sp--;
                free_string_svalue(sp);
                sp->u.str = res;
                break;
              }

            case T_NUMBER:
              {
                string_t *left, *res;
                char buff[80];
                size_t len;

                left = (sp-1)->u.str;
                buff[sizeof(buff)-1] = '\0';
                sprintf(buff, "%"PRIdPINT, sp->u.number);
                if (buff[sizeof(buff)-1] != '\0')
                    FATAL("Buffer overflow in F_ADD: int number too big.\n");
                len = mstrsize(left)+strlen(buff);
                DYN_STRING_COST(len)
                res = mstr_add_txt(left, buff, strlen(buff));
                if (!res)
                    ERRORF(("Out of memory (%zu bytes)\n", len ));
                pop_n_elems(2);
                push_string(sp, res);
                break;
              }

            case T_FLOAT:
              {
                char buff[160];
                string_t *left, *res;
                size_t len;

                left = (sp-1)->u.str;
                buff[sizeof(buff)-1] = '\0';
                sprintf(buff, "%g", READ_DOUBLE( sp ) );
                if (buff[sizeof(buff)-1] != '\0')
                    FATAL("Buffer overflow in F_ADD: float number too big.\n");
                len = mstrsize(left)+strlen(buff);
                DYN_STRING_COST(len)
                res = mstr_add_txt(left, buff, strlen(buff));
                if (!res)
                    ERRORF(("Out of memory (%zu bytes)\n", len));
                sp--;
                free_string_svalue(sp);
                put_string(sp, res);
                break;
              }

            default:
                OP_ARG_ERROR(2, TF_STRING|TF_FLOAT|TF_NUMBER, sp->type);
                /* NOTREACHED */
            }
            break;
            /* End of case T_STRING */

          case T_NUMBER:
            switch ( sp->type )
            {
            case T_STRING:
              {
                char buff[80];
                string_t *right, *res;
                size_t len;

                right = sp->u.str;
                buff[sizeof(buff)-1] = '\0';
                sprintf(buff, "%"PRIdPINT, (sp-1)->u.number);
                if (buff[sizeof(buff)-1] != '\0')
                    FATAL("Buffer overflow in F_ADD: int number too big.\n");
                len = mstrsize(right)+strlen(buff);
                DYN_STRING_COST(len)
                res = mstr_add_to_txt(buff, strlen(buff), right);
                if (!res)
                    ERRORF(("Out of memory (%zu bytes)\n", len));
                free_string_svalue(sp);
                sp--;
                /* Overwrite the number at sp */
                put_string(sp, res);
                break;
              }

            case T_NUMBER:
              {
                p_int i;
                p_int right = sp->u.number;
                p_int left = (sp-1)->u.number;

                if ((left >= 0 && right >= 0 && PINT_MAX - left < right)
                 || (left < 0 && right < 0 && PINT_MIN - left > right)
                   )
                {
                    ERRORF(("Numeric overflow: %"PRIdPINT" + %"PRIdPINT"\n"
                           , left, right));
                    /* NOTREACHED */
                    break;
                }
                i = left + right;
                sp--;
                sp->u.number = i;
                break;
              }

            case T_FLOAT:
              {
                STORE_DOUBLE_USED
                double sum;

                sum = (double)((sp-1)->u.number) + READ_DOUBLE(sp);
                if (sum < (-DBL_MAX) || sum > DBL_MAX)
                    ERRORF(("Numeric overflow: %"PRIdPINT" + %g\n"
                           , (sp-1)->u.number, READ_DOUBLE(sp)));
                STORE_DOUBLE(sp-1, sum);
                sp--;
                sp->type = T_FLOAT;
                break;
              }

            default:
                OP_ARG_ERROR(2, TF_STRING|TF_FLOAT|TF_NUMBER, sp->type);
                /* NOTREACHED */
            }
            break;
            /* End of case T_NUMBER */

        case T_FLOAT:
          {
            STORE_DOUBLE_USED
            double sum;

            if (sp->type == T_FLOAT)
            {
                sum = READ_DOUBLE(sp-1) + READ_DOUBLE(sp);
                if (sum < (-DBL_MAX) || sum > DBL_MAX)
                    ERRORF(("Numeric overflow: %g + %g\n"
                           , READ_DOUBLE(sp-1), READ_DOUBLE(sp)));
                STORE_DOUBLE(sp-1, sum);
                sp--;
                break;
            }
            if (sp->type == T_NUMBER)
            {
                sum = READ_DOUBLE(sp-1) + (double)(sp->u.number);
                if (sum < (-DBL_MAX) || sum > DBL_MAX)
                    ERRORF(("Numeric overflow: %g + %"PRIdPINT"\n"
                           , READ_DOUBLE(sp-1), sp->u.number));
                STORE_DOUBLE(sp-1, sum);
                sp--;
                break;
            }
            if (sp->type == T_STRING)
            {
                char buff[160];
                string_t *right, *res;
                size_t len;

                right = sp->u.str;
                buff[sizeof(buff)-1] = '\0';
                sprintf(buff, "%g", READ_DOUBLE(sp-1) );
                if (buff[sizeof(buff)-1] != '\0')
                    FATAL("Buffer overflow in F_ADD: float number too big.\n");
                len = mstrsize(right)+strlen(buff);
                DYN_STRING_COST(len)
                res = mstr_add_to_txt(buff, strlen(buff), right);
                if (!res)
                    ERRORF(("Out of memory (%zu bytes)\n", len));
                free_string_svalue(sp);
                sp--;
                /* Overwrite the number at sp */
                put_string(sp, res);
                break;
            }
            OP_ARG_ERROR(2, TF_STRING|TF_FLOAT|TF_NUMBER, sp->type);
            /* NOTREACHED */
          }
          /* End of case T_FLOAT */

        case T_POINTER:
          {
            TYPE_TEST_RIGHT(sp, T_POINTER);
            inter_sp = sp;
            inter_pc = pc;
            DYN_ARRAY_COST(VEC_SIZE(sp->u.vec)+VEC_SIZE(sp[-1].u.vec));
            inter_add_array(sp->u.vec, &(sp-1)->u.vec);
            sp--;
            break;
          }

        case T_MAPPING:
          {
            mapping_t *m;

            TYPE_TEST_RIGHT(sp, T_MAPPING);
            inter_pc = pc;
            inter_sp = sp;
            m = add_mapping((sp-1)->u.map,sp->u.map);
            if (!m) {
                ERROR("Out of memory.\n");
            }
            pop_n_elems(2);
            push_mapping(sp, m);
            if ((max_mapping_size && MAP_TOTAL_SIZE(m) > (p_int)max_mapping_size)
             || (max_mapping_keys && MAP_SIZE(m) > (p_int)max_mapping_keys)
               )
            {
                // very unlikely that there are destructed keys... But anyway, this is a 
                // rare error condition.
                check_map_for_destr_keys(m);
                if (max_mapping_size && MAP_TOTAL_SIZE(m) > (p_int)max_mapping_size)
                    ERRORF(("Illegal mapping size: %"PRIdPINT
                            " elements (%"PRIdPINT" x %"PRIdPINT")\n"
                           , MAP_TOTAL_SIZE(m), MAP_SIZE(m), m->num_values));
              
                if (max_mapping_keys && MAP_SIZE(m) > (p_int)max_mapping_keys)
                    ERRORF(("Illegal mapping size: %"PRIdPINT" entries\n", 
                            MAP_SIZE(m)));
            }
            break;
          }

        default:
            OP_ARG_ERROR(1, TF_POINTER|TF_MAPPING|TF_STRING|TF_BYTES|TF_FLOAT|TF_NUMBER
                          , sp[-1].type);
            /* NOTREACHED */
        }

        break;

    CASE(F_SUBTRACT);               /* --- subtract            --- */
    {
        /* Subtract sp[0] from sp[-1] (the order is important), pop both
         * arguments from the stack and push the result.
         *
         * Possible type combinations:
         *   int         - int                -> int
         *   float       - (int,float)        -> float
         *   int         - float              -> float
         *   string      - string             -> string
         *   bytes       - bytes              -> bytes
         *   vector      - vector             -> vector
         *   vector      - mapping            -> vector
         *   mapping     - mapping            -> mapping
         *   mapping     - vector             -> mapping
         */

        p_int i;

        if ((sp-1)->type == T_NUMBER)
        {
            if (sp->type == T_NUMBER)
            {
                p_int left = (sp-1)->u.number;
                p_int right = sp->u.number;

                if ((left >= 0 && right < 0 && PINT_MAX + right < left)
                 || (left < 0 && right >= 0 && PINT_MIN + right > left)
                   )
                {
                    ERRORF(("Numeric overflow: %"PRIdPINT" - %"PRIdPINT"\n"
                           , left, right));
                    /* NOTREACHED */
                    break;
                }

                i = left - right;
                sp--;
                sp->u.number = i;
                break;
            }
            if (sp->type == T_FLOAT)
            {
                STORE_DOUBLE_USED
                double diff;

                diff = (double)((sp-1)->u.number) - READ_DOUBLE(sp);
                if (diff < (-DBL_MAX) || diff > DBL_MAX)
                    ERRORF(("Numeric overflow: %"PRIdPINT" - %g\n"
                           , (sp-1)->u.number, READ_DOUBLE(sp)));
                sp--;
                STORE_DOUBLE(sp, diff);
                sp->type = T_FLOAT;
                break;
            }
            OP_ARG_ERROR(2, TF_FLOAT|TF_NUMBER, sp->type);
            /* NOTREACHED */
        }
        else if ((sp-1)->type == T_FLOAT)
        {
            STORE_DOUBLE_USED
            double diff;

            if (sp->type == T_FLOAT)
            {
                diff = READ_DOUBLE(sp-1) - READ_DOUBLE(sp);
                if (diff < (-DBL_MAX) || diff > DBL_MAX)
                    ERRORF(("Numeric overflow: %g - %g\n"
                           , READ_DOUBLE(sp-1), READ_DOUBLE(sp)));
                sp--;
                STORE_DOUBLE(sp, diff);
                break;
            }
            if (sp->type == T_NUMBER)
            {
                diff = READ_DOUBLE(sp-1) - (double)(sp->u.number);
                if (diff < (-DBL_MAX) || diff > DBL_MAX)
                    ERRORF(("Numeric overflow: %g - %"PRIdPINT"\n"
                           , READ_DOUBLE(sp-1), sp->u.number));
                sp--;
                STORE_DOUBLE(sp, diff);
                break;
            }
            OP_ARG_ERROR(2, TF_FLOAT|TF_NUMBER, sp->type);
            /* NOTREACHED */
        }
        else if ((sp-1)->type == T_POINTER)
        {
            if (sp->type == T_POINTER)
            {
                vector_t *v = sp->u.vec;
                if (v->ref > 1)
                {
                    deref_array(v);
                    v = slice_array(v, 0, (mp_int)VEC_SIZE(v) - 1 );
                }
                sp--;
                /* subtract_array already takes care of destructed objects */
                sp->u.vec = subtract_array(sp->u.vec, v);
                break;
            }
            if (sp->type == T_MAPPING)
            {
                sp[-1].u.vec = map_intersect_array(sp[-1].u.vec, sp->u.map, true);
                sp--;
                break;
            }
            OP_ARG_ERROR(2, TF_POINTER|TF_MAPPING, sp->type);
            /* NOTREACHED */
        }
        else if ((sp-1)->type == T_MAPPING)
        {
            if (sp->type == T_MAPPING)
            {
                mapping_t *m;

                m = subtract_mapping(sp[-1].u.map, sp->u.map);
                free_mapping(sp->u.map);
                sp--;
                free_mapping(sp->u.map);
                sp->u.map = m;
                break;
            }
            if (sp->type == T_POINTER)
            {
                mapping_t *m = copy_mapping(sp[-1].u.map);
                map_subtract_eq_array(m, sp->u.vec);
                free_array(sp->u.vec);
                sp--;
                free_mapping(sp->u.map);
                sp->u.map = m;
                break;
            }
            OP_ARG_ERROR(2, TF_POINTER|TF_MAPPING, sp->type);
        }
        else if ((sp-1)->type == T_STRING || (sp-1)->type == T_BYTES)
        {
            string_t * result;

            TYPE_TEST_RIGHT(sp, (sp-1)->type);

            inter_sp = sp;
            result = intersect_strings((sp-1)->u.str, sp->u.str, MY_TRUE);
            free_string_svalue(sp);
            sp--;
            free_string_svalue(sp);
            sp->u.str = result;
            break;
        }

        OP_ARG_ERROR(1, TF_POINTER|TF_MAPPING|TF_STRING|TF_BYTES|TF_FLOAT|TF_NUMBER
                      , sp[-1].type);
        /* NOTREACHED */
    }

    CASE(F_MULTIPLY);               /* --- multiply            --- */
    {
        /* Multiply sp[-1] by sp[0] pop both arguments from the stack
         * and push the result.
         * TODO: Could be extended to cover mappings.
         * TODO:: array/string multiplied by element === implode.
         *
         * Possible type combinations:
         *   int         * int                -> int
         *   float       * (int,float)        -> float
         *   int         * float              -> float
         *   string      * int                -> string
         *   int         * string             -> string
         *   bytes       * int                -> bytes
         *   int         * bytes              -> bytes
         *   array       * int                -> array
         *   int         * array              -> array
         */

        p_int i;

        switch ( sp[-1].type )
        {
        case T_NUMBER:
            if (sp->type == T_NUMBER)
            {
                p_int left = (sp-1)->u.number;
                p_int right = sp->u.number;

                if (left > 0 && right > 0)
                {
                    if ((left != 0 && PINT_MAX / left < right)
                     || (right != 0 && PINT_MAX / right < left)
                       )
                    {
                        ERRORF(("Numeric overflow: %"PRIdPINT" * %"PRIdPINT"\n"
                               , left, right));
                        /* NOTREACHED */
                        break;
                    }
                }
                else if (left < 0 && right < 0)
                {
                    if ((left != 0 && PINT_MAX / left > right)
                     || (right != 0 && PINT_MAX / right > left)
                       )
                    {
                        ERRORF(("Numeric overflow: %"PRIdPINT
                                " * %"PRIdPINT"\n"
                               , left, right));
                        /* NOTREACHED */
                        break;
                    }
                }
                else if (left != 0 && right != 0)
                {
                    if ((left > 0 && PINT_MIN / left > right)
                     || (right > 0 && PINT_MIN / right > left)
                       )
                    {
                        ERRORF(("Numeric overflow: %"PRIdPINT
                                " * %"PRIdPINT"\n"
                               , left, right));
                        /* NOTREACHED */
                        break;
                    }
                }
                i = left * right;
                sp--;
                sp->u.number = i;
                break;
            }
            if (sp->type == T_FLOAT)
            {
                STORE_DOUBLE_USED
                double product;

                product = (sp-1)->u.number * READ_DOUBLE(sp);
                if (product < (-DBL_MAX) || product > DBL_MAX)
                    ERRORF(("Numeric overflow: %"PRIdPINT" * %g\n"
                           , (sp-1)->u.number, READ_DOUBLE(sp)));
                sp--;
                STORE_DOUBLE(sp, product);
                sp->type = T_FLOAT;
                break;
            }
            if (sp->type == T_STRING || sp->type == T_BYTES)
            {
                string_t * result;
                size_t slen;
                ph_int stringtype = sp->type;

                if (sp[-1].u.number < 0)
                    ERROR("Bad right arg to *: negative number.\n");

                slen = mstrsize(sp->u.str);
                if (slen > (size_t)PINT_MAX
                 || (   slen != 0
                     && PINT_MAX / (p_int)slen < sp[-1].u.number)
                 || (   sp[-1].u.number != 0
                     && PINT_MAX / sp[-1].u.number < (p_int)slen)
                   )
                    ERRORF(("Result string too long (%zu * %"PRIdPINT").\n"
                           , slen, sp[-1].u.number
                           ));

                result = mstr_repeat(sp->u.str, (size_t)sp[-1].u.number);
                if (!result)
                    ERRORF(("Out of memory (%"PRIdPINT" bytes).\n"
                           , (p_int)mstrsize(sp->u.str) * sp[-1].u.number));

                DYN_STRING_COST(mstrsize(result))
                free_svalue(sp);
                sp--;
                /* No free_svalue(sp): it's just a number */
                sp->type = stringtype;
                sp->u.str = result;
                break;
            }
            if (sp->type == T_POINTER)
            {
                vector_t *result;
                mp_int reslen;
                size_t len;

                if (sp[-1].u.number < 0)
                    ERROR("Bad right arg to *: negative number.\n");

                inter_sp = sp;
                inter_pc = pc;
                len = VEC_SIZE(sp->u.vec);
                reslen = sp[-1].u.number * (mp_int)len;
                result = allocate_uninit_array(reslen);
                DYN_ARRAY_COST(reslen);

                if (sp[-1].u.number > 0 && len)
                {
                    size_t left;
                    svalue_t *from, *to;

                    /* Seed result[] with one copy of the array.
                     */
                    for ( from = sp->u.vec->item, to = result->item, left = len
                        ; left
                        ; from++, to++, left--)
                    {
                        assign_svalue_no_free(to, from);
                    } /* for() seed */

                    /* Now fill the remainder of the vector with
                     * the values already copied in there.
                     */
                    for (from = result->item, left = reslen - len
                        ; left
                        ; to++, from++, left--
                        )
                        assign_svalue_no_free(to, from);
                } /* if (len) */

                free_svalue(sp);
                sp--;
                /* No free_svalue(sp): it's just a number */
                put_array(sp, result);
                break;
            }
            OP_ARG_ERROR(2, TF_POINTER|TF_STRING|TF_BYTES|TF_FLOAT|TF_NUMBER
                          , sp->type);
            /* NOTREACHED */
        case T_FLOAT:
          {
            STORE_DOUBLE_USED
            double product;

            if (sp->type == T_FLOAT)
            {
                product = READ_DOUBLE(sp-1) * READ_DOUBLE(sp);
                if (product < (-DBL_MAX) || product > DBL_MAX)
                    ERRORF(("Numeric overflow: %g * %g\n"
                           , READ_DOUBLE(sp-1), READ_DOUBLE(sp)));
                STORE_DOUBLE(sp-1, product);
                sp--;
                break;
            }
            if (sp->type == T_NUMBER)
            {
                product = READ_DOUBLE(sp-1) * sp->u.number;
                if (product < (-DBL_MAX) || product > DBL_MAX)
                    ERRORF(("Numeric overflow: %g * %"PRIdPINT"\n"
                           , READ_DOUBLE(sp-1), sp->u.number));
                STORE_DOUBLE(sp-1, product);
                sp--;
                break;
            }
            OP_ARG_ERROR(2, TF_FLOAT|TF_NUMBER, sp->type);
            /* NOTREACHED */
          }
        case T_STRING:
        case T_BYTES:
          {
            if (sp->type == T_NUMBER)
            {
                string_t * result;
                size_t slen;

                if (sp->u.number < 0)
                    ERROR("Bad left arg to *: negative number.\n");

                slen = mstrsize(sp[-1].u.str);
                if (slen > (size_t)PINT_MAX
                 || (   slen != 0
                     && PINT_MAX / (p_int)slen < sp->u.number)
                 || (   sp->u.number != 0
                     && PINT_MAX / sp->u.number < (p_int)slen)
                   )
                    ERRORF(("Result string too long (%"PRIdPINT" * %zu).\n"
                           , sp->u.number, slen));

                result = mstr_repeat(sp[-1].u.str, (size_t)sp->u.number);
                if (!result)
                    ERRORF(("Out of memory (%"PRIdMPINT" bytes).\n"
                           , (mp_int)mstrsize(sp[-1].u.str) * sp->u.number));

                DYN_STRING_COST(mstrsize(result))

                /* No free_svalue(sp): it's just a number */
                sp--;
                free_string_svalue(sp);
                sp->u.str = result;
                break;
            }
            BAD_OP_ARG(2, T_NUMBER, sp->type);
            /* NOTREACHED */
          }
        case T_POINTER:
          {
            if (sp->type == T_NUMBER)
            {
                vector_t *result;
                mp_int reslen;
                size_t len;

                if (sp->u.number < 0)
                    ERROR("Bad left arg to *: negative number.\n");

                inter_sp = sp;
                inter_pc = pc;
                len = VEC_SIZE(sp[-1].u.vec);
                reslen = sp->u.number * (mp_int)len;
                result = allocate_uninit_array(reslen);

                if (sp->u.number > 0 && len)
                {
                    size_t left;
                    svalue_t *from, *to;

                    /* Seed result[] with one copy of the array.
                     */
                    for ( from = sp[-1].u.vec->item, to = result->item, left = len
                        ; left
                        ; from++, to++, left--)
                    {
                        assign_svalue_no_free(to, from);
                    } /* for() seed */

                    /* Now fill the remainder of the vector with
                     * the values already copied in there.
                     */
                    for (from = result->item, left = reslen - len
                        ; left
                        ; to++, from++, left--
                        )
                        assign_svalue_no_free(to, from);
                } /* if (len) */

                /* No free_svalue(sp): it's just a number */
                sp--;
                free_svalue(sp);
                put_array(sp, result);
                break;
              }
            BAD_OP_ARG(2, T_NUMBER, sp->type);
            /* NOTREACHED */
          }
        default:
            OP_ARG_ERROR(1, TF_POINTER|TF_STRING|TF_BYTES|TF_FLOAT|TF_NUMBER
                          , sp[-1].type);
            /* NOTREACHED */
        }
        break;
    }

    CASE(F_DIVIDE);                 /* --- divide              --- */
    {
        /* Divide sp[-1] by sp[0] pop both arguments from the stack
         * and push the result.
         * TODO: Could be extended to cover arrays and mappings.
         * TODO:: array/string divided by element === explode.
         *
         * Possible type combinations:
         *   int         / int                -> int
         *   float       / (int,float)        -> float
         *   int         / float              -> float
         */

        p_int i;

        if ((sp-1)->type == T_NUMBER)
        {
            if (sp->type == T_NUMBER) {
                if (sp->u.number == 0)
                    ERROR("Division by zero\n");
                if ((sp-1)->u.number == PINT_MIN && sp->u.number == -1)
                    ERRORF(("Numeric overflow: %"PRIdPINT" / -1\n"
                           , (sp-1)->u.number
                           ));
                i = (sp-1)->u.number / sp->u.number;
                sp--;
                sp->u.number = i;
                break;
            }
            if (sp->type == T_FLOAT)
            {
                double dtmp;
                STORE_DOUBLE_USED

                dtmp = READ_DOUBLE( sp );
                if (dtmp == 0.)
                    ERROR("Division by zero\n");
                sp--;
                dtmp = (double)sp->u.number / dtmp;
                if (dtmp < (-DBL_MAX) || dtmp > DBL_MAX)
                    ERRORF(("Numeric overflow: %"PRIdPINT" / %g\n"
                           , (sp)->u.number, READ_DOUBLE(sp+1)));
                STORE_DOUBLE(sp, dtmp);
                sp->type = T_FLOAT;
                break;
            }
            OP_ARG_ERROR(2, TF_FLOAT|TF_NUMBER, sp->type);
            /* NOTREACHED */
        }
        else if ((sp-1)->type == T_FLOAT)
        {
            double dtmp;
            STORE_DOUBLE_USED

            if (sp->type == T_FLOAT)
            {
                dtmp = READ_DOUBLE( sp );
                if (dtmp == 0.) {
                    ERROR("Division by zero\n");
                    return MY_FALSE;
                }
                sp--;
                dtmp = READ_DOUBLE(sp) / dtmp;
                if (dtmp < (-DBL_MAX) || dtmp > DBL_MAX)
                    ERRORF(("Numeric overflow: %g / %g\n"
                           , READ_DOUBLE(sp), READ_DOUBLE(sp+1)));
                STORE_DOUBLE(sp, dtmp);
                break;
            }
            if (sp->type == T_NUMBER)
            {
                if (sp->u.number == 0) {
                    ERROR("Division by zero\n");
                    return MY_FALSE;
                }
                dtmp = (double)sp->u.number;
                sp--;
                dtmp = READ_DOUBLE(sp) / dtmp;
                if (dtmp < (-DBL_MAX) || dtmp > DBL_MAX)
                    ERRORF(("Numeric overflow: %g / %"PRIdPINT"\n"
                           , READ_DOUBLE(sp), (sp+1)->u.number));
                STORE_DOUBLE(sp, dtmp);
                break;
            }
            OP_ARG_ERROR(2, TF_FLOAT|TF_NUMBER, sp->type);
            /* NOTREACHED */
        }
        OP_ARG_ERROR(1, TF_FLOAT|TF_NUMBER, sp[-1].type);
        /* NOTREACHED */
        break;
    }

    CASE(F_MOD);                    /* --- mod                 --- */
    {
        /* Compute sp[-1] modulus sp[0] pop both arguments from the stack
         * and push the result.
         * TODO: Could be extended to cover floats(!), arrays and mappings.
         * TODO: Define properly and add the rem operation.
         *
         * Possible type combinations:
         *   int         % int                -> int
         */

        p_int i;

        TYPE_TEST_LEFT((sp-1), T_NUMBER);
        TYPE_TEST_RIGHT(sp, T_NUMBER);
        if (sp->u.number == 0)
        {
            ERROR("Modulus by zero.\n");
            break;
        }
        else
            i = (sp-1)->u.number % sp->u.number;
        sp--;
        sp->u.number = i;
        break;
    }

    CASE(F_GT);                     /* --- gt                  --- */
    {
        /* Test if sp[-1] > sp[0]. If yes, push 1 onto the stack,
         * else 0 (of course after popping both arguments).
         *
         * Comparable types are int, string and float, each only
         * to its own type.
         */

        int i;

        if (((sp-1)->type == T_STRING && sp->type == T_STRING)
         || ((sp-1)->type == T_BYTES  && sp->type == T_BYTES))
        {
            i = mstrcmp((sp-1)->u.str, sp->u.str) > 0;
            free_string_svalue(sp);
            sp--;
            free_string_svalue(sp);
            put_number(sp, i);
            break;
        }

        if ((sp-1)->type == T_NUMBER && sp->type == T_NUMBER)
        {
            i = (sp-1)->u.number > sp->u.number;
            sp--;
            sp->u.number = i;
            break;
        }

        if ((sp-1)->type == T_FLOAT && sp->type == T_FLOAT)
        {
            i = READ_DOUBLE( sp-1 ) > READ_DOUBLE( sp );
            sp--;
            put_number(sp, i);
            break;
        }

        if ((sp-1)->type == T_NUMBER && sp->type == T_FLOAT)
        {
            i = (double)((sp-1)->u.number) > READ_DOUBLE( sp );
            sp--;
            put_number(sp, i);
            break;
        }

        if ((sp-1)->type == T_FLOAT && sp->type == T_NUMBER)
        {
            i = READ_DOUBLE( sp-1 ) > (double)(sp->u.number);
            sp--;
            put_number(sp, i);
            break;
        }

        TYPE_TEST_EXP_LEFT((sp-1), TF_NUMBER|TF_STRING|TF_BYTES|TF_FLOAT);
        TYPE_TEST_EXP_RIGHT(sp, TF_NUMBER|TF_STRING|TF_BYTES|TF_FLOAT);
        ERRORF(("Arguments to > don't match: %s vs %s\n"
               , typename(sp[-1].type), typename(sp->type)
               ));
    }

    CASE(F_GE);                     /* --- ge                  --- */
    {
        /* Test if sp[-1] >= sp[0]. If yes, push 1 onto the stack,
         * else 0 (of course after popping both arguments).
         *
         * Comparable types are int, string and float, each only
         * to its own type.
         */

        int i;

        if (((sp-1)->type == T_STRING && sp->type == T_STRING)
         || ((sp-1)->type == T_BYTES  && sp->type == T_BYTES))
        {
            i = mstrcmp((sp-1)->u.str, sp->u.str) >= 0;
            free_string_svalue(sp);
            sp--;
            free_string_svalue(sp);
            put_number(sp, i);
            break;
        }

        if ((sp-1)->type == T_NUMBER && sp->type == T_NUMBER)
        {
            i = (sp-1)->u.number >= sp->u.number;
            sp--;
            sp->u.number = i;
            break;
        }

        if ((sp-1)->type == T_FLOAT && sp->type == T_FLOAT)
        {
            i = READ_DOUBLE( sp-1 ) >= READ_DOUBLE( sp );
            sp--;
            put_number(sp, i);
            break;
        }

        if ((sp-1)->type == T_NUMBER && sp->type == T_FLOAT)
        {
            i = (double)((sp-1)->u.number) >= READ_DOUBLE( sp );
            sp--;
            put_number(sp, i);
            break;
        }

        if ((sp-1)->type == T_FLOAT && sp->type == T_NUMBER)
        {
            i = READ_DOUBLE( sp-1 ) >= (double)(sp->u.number);
            sp--;
            put_number(sp, i);
            break;
        }

        TYPE_TEST_EXP_LEFT((sp-1), TF_NUMBER|TF_STRING|TF_BYTES|TF_FLOAT);
        TYPE_TEST_EXP_RIGHT(sp, TF_NUMBER|TF_STRING|TF_BYTES|TF_FLOAT);
        ERRORF(("Arguments to >= don't match: %s vs %s\n"
               , typename(sp[-1].type), typename(sp->type)
               ));
    }

    CASE(F_LT);                     /* --- lt                  --- */
    {
        /* Test if sp[-1] < sp[0]. If yes, push 1 onto the stack,
         * else 0 (of course after popping both arguments).
         *
         * Comparable types are int, string and float, each only
         * to its own type.
         */

        int i;

        if (((sp-1)->type == T_STRING && sp->type == T_STRING)
         || ((sp-1)->type == T_BYTES  && sp->type == T_BYTES))
        {
            i = mstrcmp((sp-1)->u.str, sp->u.str) < 0;
            free_string_svalue(sp);
            sp--;
            free_string_svalue(sp);
            put_number(sp, i);
            break;
        }

        if ((sp-1)->type == T_NUMBER && sp->type == T_NUMBER)
        {
            i = (sp-1)->u.number < sp->u.number;
            sp--;
            sp->u.number = i;
            break;
        }

        if ((sp-1)->type == T_FLOAT && sp->type == T_FLOAT)
        {
            i = READ_DOUBLE( sp-1 ) < READ_DOUBLE( sp );
            sp--;
            put_number(sp, i);
            break;
        }

        if ((sp-1)->type == T_NUMBER && sp->type == T_FLOAT)
        {
            i = (double)((sp-1)->u.number) < READ_DOUBLE( sp );
            sp--;
            put_number(sp, i);
            break;
        }

        if ((sp-1)->type == T_FLOAT && sp->type == T_NUMBER)
        {
            i = READ_DOUBLE( sp-1 ) < (double)(sp->u.number);
            sp--;
            put_number(sp, i);
            break;
        }

        TYPE_TEST_EXP_LEFT((sp-1), TF_NUMBER|TF_STRING|TF_BYTES|TF_FLOAT);
        TYPE_TEST_EXP_RIGHT(sp, TF_NUMBER|TF_STRING|TF_BYTES|TF_FLOAT);
        ERRORF(("Arguments to < don't match: %s vs %s\n"
               , typename(sp[-1].type), typename(sp->type)
               ));
    }

    CASE(F_LE);                     /* --- le                  --- */
    {
        /* Test if sp[-1] <= sp[0]. If yes, push 1 onto the stack,
         * else 0 (of course after popping both arguments).
         *
         * Comparable types are int, string and float, each only
         * to its own type.
         */

        int i;

        if (((sp-1)->type == T_STRING && sp->type == T_STRING)
         || ((sp-1)->type == T_BYTES  && sp->type == T_BYTES))
        {
            i = mstrcmp((sp-1)->u.str, sp->u.str) <= 0;
            free_string_svalue(sp);
            sp--;
            free_string_svalue(sp);
            put_number(sp, i);
            break;
        }

        if ((sp-1)->type == T_NUMBER && sp->type == T_NUMBER)
        {
            i = (sp-1)->u.number <= sp->u.number;
            sp--;
            sp->u.number = i;
            break;
        }

        if ((sp-1)->type == T_FLOAT && sp->type == T_FLOAT)
        {
            i = READ_DOUBLE( sp-1 ) <= READ_DOUBLE( sp );
            sp--;
            put_number(sp, i);
            break;
        }

        if ((sp-1)->type == T_NUMBER && sp->type == T_FLOAT)
        {
            i = (double)((sp-1)->u.number) <= READ_DOUBLE( sp );
            sp--;
            put_number(sp, i);
            break;
        }

        if ((sp-1)->type == T_FLOAT && sp->type == T_NUMBER)
        {
            i = READ_DOUBLE( sp-1 ) <= (double)(sp->u.number);
            sp--;
            put_number(sp, i);
            break;
        }

        TYPE_TEST_EXP_LEFT((sp-1), TF_NUMBER|TF_STRING|TF_BYTES|TF_FLOAT);
        TYPE_TEST_EXP_RIGHT(sp, TF_NUMBER|TF_STRING|TF_BYTES|TF_FLOAT);
        ERRORF(("Arguments to <= don't match: %s vs %s\n"
               , typename(sp[-1].type), typename(sp->type)
               ));
    }

    CASE(F_EQ);                     /* --- eq                  --- */
    {
        /* Test if sp[-1] == sp[0]. If yes, push 1 onto the stack,
         * else 0 (of course after popping both arguments).
         *
         * Comparable types are all types, each to its own. Comparisons
         * between distinct types (except between int and float) always
         * yield 'unequal'.
         * Vectors and mappings are compared by ref only.
         */

        int i = 0;

        if ((sp-1)->type == T_NUMBER && sp->type == T_FLOAT)
        {
            i = (double)((sp-1)->u.number) == READ_DOUBLE( sp );
        }
        else if ((sp-1)->type == T_FLOAT && sp->type == T_NUMBER)
        {
            i = READ_DOUBLE( sp-1 ) == (double)(sp->u.number);
        }
        else if ((sp-1)->type != sp->type)
        {
            i = 0;
        }
        else /* type are equal */
        {
            switch(sp->type)
            {
            case T_NUMBER:
                i = (sp-1)->u.number == sp->u.number;
                break;
            case T_POINTER:
                i = (sp-1)->u.vec == sp->u.vec;
                break;
            case T_STRUCT:
                i = (sp-1)->u.strct == sp->u.strct;
                if (!i && struct_size((sp-1)->u.strct) == 0
                       && struct_size(sp->u.strct) == 0
                   )
                {
                    i = 1;
                }
                break;
            case T_STRING:
            case T_BYTES:
                i = mstreq((sp-1)->u.str, sp->u.str);
                break;
            case T_OBJECT:
                i = (sp-1)->u.ob == sp->u.ob;
                break;
            case T_FLOAT:
                i = READ_DOUBLE( sp-1 ) == READ_DOUBLE( sp );
                break;

            case T_CLOSURE:
                i = closure_eq(sp-1, sp);
                break;

            case T_SYMBOL:
            case T_QUOTED_ARRAY:
                i = (sp-1)->u.generic  == sp->u.generic &&
                    (sp-1)->x.generic == sp->x.generic;
                break;
            case T_MAPPING:
                i = (sp-1)->u.map == sp->u.map;
                break;
            default:
                if (sp->type == T_LVALUE)
                    errorf("Reference passed to ==\n");
                FATALF(("Illegal type '%s' to ==\n",typename(sp->type)));
                /* NOTREACHED */
                return MY_FALSE;
            }
        }

        pop_stack();
        free_svalue(sp);
        put_number(sp, i);
        break;
    }

    CASE(F_NE);                     /* --- ne                  --- */
    {
        /* Test if sp[-1] != sp[0]. If yes, push 1 onto the stack,
         * else 0 (of course after popping both arguments).
         *
         * Comparable types are all types, each to its own. Comparisons
         * between distinct types (except between int and float) always
         * yield 'unequal'.
         * Vectors and mappings are compared by ref only.
         */

        int i = 0;

        if ((sp-1)->type == T_NUMBER && sp->type == T_FLOAT)
        {
            i = (double)((sp-1)->u.number) != READ_DOUBLE( sp );
        }
        else if ((sp-1)->type == T_FLOAT && sp->type == T_NUMBER)
        {
            i = READ_DOUBLE( sp-1 ) != (double)(sp->u.number);
        }
        else if ((sp-1)->type != sp->type)
        {
            i = 1;
        }
        else /* type are equal */
        {
            switch(sp->type)
            {
            case T_NUMBER:
                i = (sp-1)->u.number != sp->u.number;
                break;
            case T_STRING:
            case T_BYTES:
                i = !mstreq((sp-1)->u.str, sp->u.str);
                break;
            case T_POINTER:
                i = (sp-1)->u.vec != sp->u.vec;
                break;
            case T_STRUCT:
                i = (sp-1)->u.strct != sp->u.strct;
                break;
            case T_OBJECT:
                i = (sp-1)->u.ob != sp->u.ob;
                break;
            case T_FLOAT:
                i = READ_DOUBLE( sp-1 ) != READ_DOUBLE( sp );
                break;

            case T_CLOSURE:
                i = !closure_eq(sp-1, sp);
                break;

            case T_SYMBOL:
            case T_QUOTED_ARRAY:
                i = (sp-1)->u.generic  != sp->u.generic ||
                    (sp-1)->x.generic != sp->x.generic;
                break;
            case T_MAPPING:
                i = (sp-1)->u.map != sp->u.map;
                break;
            default:
                if (sp->type == T_LVALUE)
                    errorf("Reference passed to !=\n");
                FATALF(("Illegal type '%s' to !=\n",typename(sp->type)));
                /* NOTREACHED */
                return MY_FALSE;
            }
        }

        pop_stack();
        free_svalue(sp);
        put_number(sp, i);
        break;
    }

    CASE(F_IN);                     /* --- in                  --- */
    {
        /* Do a membership test for sp[-1] in sp[0].
         * Leave 1 for success, 0 for failure on the stack.
         */
        int result = 0;
        svalue_t *container = get_rvalue(sp, NULL);
        struct protected_range_lvalue *container_range = NULL;
        svalue_t *item = get_rvalue(sp-1, NULL);

        if (item == NULL)
            item = sp-1;

        if (container == NULL)
        {
            /* string or pointer range. */
            container_range = sp->u.protected_range_lvalue;
            container = &(container_range->vec);
        }

        switch (container->type)
        {
            case T_POINTER:
            {
                vector_t *vec = container->u.vec;
                p_int start = container_range == NULL ? 0 : container_range->index1;
                p_int count = container_range == NULL ? VEC_SIZE(vec) : (container_range->index2 - container_range->index1);

                for (svalue_t *entry = container->u.vec->item + start; count != 0; entry++, count--)
                {
                    if (rvalue_eq(item, entry) == 0)
                    {
                        result = 1;
                        break;
                    }
                }
                break;
            }

            case T_MAPPING:
                result = get_map_value(container->u.map, item) != &const0;
                break;

            case T_STRING:
            case T_BYTES:
            {
                string_t *str = container->u.str;
                p_int start = container_range == NULL ? 0 : container_range->index1;
                p_int len = container_range == NULL ? mstrsize(str) : (container_range->index2 - container_range->index1);
                struct protected_range_lvalue *item_range = NULL;

                if (item->type == T_LVALUE)
                {
                    item_range = item->u.protected_range_lvalue;
                    item = &(item_range->vec);
                }

                switch (item->type)
                {
                    case T_NUMBER:
                        if (str->info.unicode == STRING_UTF8)
                        {
                            char* s = get_txt(str) + start;
                            p_int ch = item->u.number;

                            while (len > 0)
                            {
                                p_int elem;
                                size_t elemlen = utf8_to_unicode(s, len, &elem);
                                if (!elemlen)
                                    break;
                                if (elem == ch)
                                {
                                    result = 1;
                                    break;
                                }

                                s += elemlen;
                                len -= elemlen;
                            }
                        }
                        else if (item->u.number & ~0xff)
                            result = 0;
                        else
                            result = memchr(get_txt(str) + start, item->u.number, len) != NULL;
                        break;

                    case T_STRING:
                    case T_BYTES:
                        if (item->type == container->type)
                        {
                            p_int itemlen = item_range == NULL ? mstrsize(item->u.str) : (item_range->index2 - item_range->index1);
                            const char* found = mstring_mstr_n_str(str, start
                                                                 , get_txt(item->u.str) + (item_range == NULL ? 0 : item_range->index1)
                                                                 , itemlen);
                            result = found != NULL
                                  && found - get_txt(str) + itemlen <= start + len;
                            break;
                        }
                        /* else FALLTHROUGH */
                    default:
                        OP_ARG_ERROR(1, TF_NUMBER|(container->type == T_STRING ? TF_STRING : TF_BYTES), item->type);
                }
                break;
            }

            default:
                OP_ARG_ERROR(2, TF_POINTER|TF_MAPPING|TF_STRING|TF_BYTES, sp->type);
                /* NOTREACHED */
        }

        pop_stack();
        free_svalue(sp);
        put_number(sp, result);
        break;
    }

    CASE(F_COMPL);                  /* --- compl               --- */
        /* Compute the binary complement of number sp[0] and leave
         * that on the stack.
         */
        TYPE_TEST1(sp, T_NUMBER);
        sp->u.number = ~ sp->u.number;
        break;

    CASE(F_AND);                    /* --- and                 --- */
    {
        /* Compute the intersection of sp[-1] and sp[0] and leave
         * the result on the stack.
         *
         * Possible type combinations:
         *   int    & int    -> int
         *   string & string -> string
         *   bytes  & bytes  -> bytes
         *   vector & vector -> vector
         *   vector & mapping  -> vector
         *   mapping & vector  -> mapping
         *   mapping & mapping -> mapping
         *
         */

        if (sp->type == T_POINTER && (sp-1)->type == T_POINTER)
        {
            inter_sp = sp - 2;
            (sp-1)->u.vec = intersect_array((sp-1)->u.vec, sp->u.vec);
            sp--;
            break;
        }

        if (sp[-1].type == T_POINTER
         && sp->type == T_MAPPING)
        {
            inter_sp = sp - 2;
            (sp-1)->u.vec = map_intersect_array(sp[-1].u.vec, sp->u.map, false);
            sp--;
            break;
        }

        if ((sp->type == T_STRING && (sp-1)->type == T_STRING)
         || (sp->type == T_BYTES  && (sp-1)->type == T_BYTES))
        {
            string_t * result;

            inter_sp = sp;
            result = intersect_strings(sp[-1].u.str, sp->u.str, MY_FALSE);
            free_string_svalue(sp-1);
            free_string_svalue(sp);
            sp--;
            sp->u.str = result;
            break;
        }

        if (sp->type == T_NUMBER && (sp-1)->type == T_NUMBER)
        {
            p_int i = (sp-1)->u.number & sp->u.number;
            sp--;
            sp->u.number = i;
            break;
        }

        if (sp[-1].type == T_MAPPING
         && (sp->type == T_POINTER || sp->type == T_MAPPING))
        {
            inter_sp = sp - 2;
            (sp-1)->u.map = map_intersect(sp[-1].u.map, sp);
            sp--;
            break;
        }

        TYPE_TEST_EXP_LEFT((sp-1), TF_NUMBER|TF_STRING|TF_BYTES|TF_POINTER|TF_MAPPING);
        TYPE_TEST_EXP_RIGHT(sp, TF_NUMBER|TF_STRING|TF_BYTES|TF_POINTER|TF_MAPPING);
        ERRORF(("Arguments to & don't match: %s vs %s\n"
               , typename(sp[-1].type), typename(sp->type)
               ));

    }

    CASE(F_OR);                     /* --- or                  --- */
    {
        /* Compute the binary-or of sp[-1] and sp[0] and leave
         * the result on the stack.
         *
         * Possible type combinations:
         *   int    | int    -> int
         *   array  | array  -> array
         *
         * TODO: Extend this to mappings.
         */

        TYPE_TEST_EXP_LEFT((sp-1), TF_NUMBER|TF_POINTER);
        if ((sp-1)->type == T_NUMBER)
        {
            TYPE_TEST_RIGHT(sp, T_NUMBER);
            p_int i = (sp-1)->u.number | sp->u.number;
            sp--;
            sp->u.number = i;
        }
        else if ((sp-1)->type == T_POINTER)
        {
            TYPE_TEST_RIGHT(sp, T_POINTER);
            inter_sp = sp;
            inter_pc = pc;
            sp--;
            sp->u.vec = join_array(sp->u.vec, (sp+1)->u.vec);
        }

        break;
    }

    CASE(F_XOR);                    /* --- xor                 --- */
    {
        /* Compute the binary-xor of sp[-1] and sp[0] and leave
         * the result on the stack.
         *
         * Possible type combinations:
         *   int ^ int    -> int
         *   array ^ array  -> array
         *
         * TODO: Extend this to mappings.
         */

        TYPE_TEST_EXP_LEFT((sp-1), TF_NUMBER|TF_POINTER);
        if ((sp-1)->type == T_NUMBER)
        {
            TYPE_TEST_RIGHT(sp, T_NUMBER);
            p_int i = (sp-1)->u.number ^ sp->u.number;
            sp--;
            sp->u.number = i;
        }
        else if ((sp-1)->type == T_POINTER)
        {
            TYPE_TEST_RIGHT(sp, T_POINTER);
            sp--;
            sp->u.vec = symmetric_diff_array(sp->u.vec, (sp+1)->u.vec);
        }

        break;
    }

    CASE(F_LSH);                    /* --- lsh                 --- */
    {
        /* Shift number sp[-1] left by sp[0] bits and leave
         * the result on the stack.
         *
         * Possible type combinations:
         *   int << int    -> int
         *
         * TODO: Extend this to vectors and mappings.
         * TODO: Implement an arithmetic shift.
         */

        TYPE_TEST_LEFT((sp-1), T_NUMBER);
        TYPE_TEST_RIGHT(sp, T_NUMBER);

        p_uint shift = sp->u.number;
        sp--;
        sp->u.number = shift > MAX_SHIFT ? 0 : sp->u.number << shift;
        break;
    }

    CASE(F_RSH);                    /* --- rsh                 --- */
    {
        /* Arithmetically shift number sp[-1] right by sp[0] bits and leave
         * the result on the stack.
         *
         * Possible type combinations:
         *   int >> int    -> int
         *
         * TODO: Extend this to vectors and mappings.
         */

        TYPE_TEST_LEFT((sp-1), T_NUMBER);
        TYPE_TEST_RIGHT(sp, T_NUMBER);

        p_uint shift = sp->u.number;
        sp--;
        if (shift <= MAX_SHIFT)
            sp->u.number >>= shift;
        else if (sp->u.number >= 0)
            sp->u.number = 0;
        else
            sp->u.number = -1;
        break;
    }

    CASE(F_RSHL);                   /* --- rshl                --- */
    {
        /* Logically shift number sp[-1] right by sp[0] bits and leave
         * the result on the stack.
         *
         * Possible type combinations:
         *   int >>> int    -> int
         *
         * TODO: Extend this to vectors and mappings.
         */

        TYPE_TEST_LEFT((sp-1), T_NUMBER);
        TYPE_TEST_RIGHT(sp, T_NUMBER);

        p_uint shift = sp->u.number;
        sp--;
        if (shift > MAX_SHIFT)
            sp->u.number = 0;
        else
            sp->u.number = (p_uint)sp->u.number >> shift;
        break;
    }

    CASE(F_NOT);                    /* --- not                 --- */
        /* Compute the logical negation of sp[0] and put it onto the stack.
         * Every value != 0 is replaced by 0, just number 0 is replaced by 1.
         */

        if (sp->type == T_NUMBER)
        {
            if (sp->u.number == 0)
            {
                sp->u.number = 1;
                break;
            }
        } else
            free_svalue(sp);
        put_number(sp, 0);
        break;

    CASE(F_NX_RANGE);               /* --- nx_range            --- */
        /* Push '1' onto the stack to make up for the missing
         * upper range bound, then fall through to the normal
         * range handling.
         */
        sp++;
        put_number(sp, 1);
        /* FALLTHROUGH */
    CASE(F_NR_RANGE);               /* --- nr_range            --- */
        sp = push_range_value(NR_RANGE, sp, pc);
        break;

    CASE(F_RX_RANGE);               /* --- rx_range            --- */
        sp++;
        put_number(sp, 1);
        /* FALLTHROUGH */
    CASE(F_RR_RANGE);               /* --- rr_range            --- */
        sp = push_range_value(RR_RANGE, sp, pc);
        break;

    CASE(F_AX_RANGE);               /* --- ax_range            --- */
        sp++;
        put_number(sp, 1);
        /* FALLTHROUGH */
    CASE(F_AR_RANGE);               /* --- ar_range            --- */
        sp = push_range_value(AR_RANGE, sp, pc);
        break;

    CASE(F_RANGE);                  /* --- range               --- */
        sp = push_range_value(NN_RANGE, sp, pc);
        break;

    CASE(F_RN_RANGE);               /* --- rn_range            --- */
        sp = push_range_value(RN_RANGE, sp, pc);
        break;

    CASE(F_NA_RANGE);               /* --- na_range            --- */
        sp = push_range_value(NA_RANGE, sp, pc);
        break;

    CASE(F_AN_RANGE);               /* --- an_range            --- */
        sp = push_range_value(AN_RANGE, sp, pc);
        break;

    CASE(F_RA_RANGE);               /* --- ra_range            --- */
        sp = push_range_value(RA_RANGE, sp, pc);
        break;

    CASE(F_AA_RANGE);               /* --- aa_range            --- */
        sp = push_range_value(AA_RANGE, sp, pc);
        break;

    CASE(F_ADD_EQ);                 /* --- add_eq              --- */
    CASE(F_VOID_ADD_EQ);            /* --- void_add_eq         --- */
    {
        /* Add sp[-1] to the value designated by lvalue sp[0] (the order
         * is important) and assign the result to sp[0].
         * For F_ADD_EQ, the result is also left on the stack.
         *
         * Possible type combinations:
         *   string       + (string,int,float) -> string
         *   bytes        + bytes              -> bytes
         *   int          + string             -> string
         *   int          + int                -> int
         *   int          + float              -> float
         *   float        + (float,int)        -> float
         *   vector       + vector             -> vector
         *   mapping      + mapping            -> mapping
         * TODO: This type mapping should be documented in 2-dim-arrays,
         * TODO:: one each for F_ADD_EQ, F_MULT_EQ, etc. This would
         * TODO:: also make the checks in the compiler simpler.
         */

        short type2;         /* type and value of sp[-1] */
        union u u2;
        svalue_t *argp; /* the actual value of sp[0] */
        svalue_t *lval;

        type2 = sp[-1].type;
        u2 = sp[-1].u;

#ifdef DEBUG
        TYPE_TEST_LEFT(sp, T_LVALUE);
#endif

        /* Set argp to the actual value designated by sp[0].
         * We handle special lvalues right away.
         */
        argp = NULL;
        lval = get_unprotected_lvalue(sp);

        switch (lval->x.lvalue_type)
        {
            default:
                fatal("(F_ADD_EQ) Illegal lvalue %p type %d\n", lval, lval->x.lvalue_type);
                /* NOTREACHED */
                break;

            case LVALUE_UNPROTECTED:
                argp = lval->u.lvalue;
                break;

            case LVALUE_UNPROTECTED_CHAR:
                if (type2 == T_NUMBER)
                {
                    p_int left = read_unprotected_char();
                    p_int right = u2.number;

                    if ((left >= 0 && right >= 0 && PINT_MAX - left < right)
                     || (left < 0 && right < 0 && PINT_MIN - left > right)
                       )
                    {
                        ERRORF(("Numeric overflow: %"PRIdPINT" += %"PRIdPINT"\n"
                               , left, right));
                        /* NOTREACHED */
                        break;
                    }

                    left = assign_unprotected_char(left + right);
                    pop_stack();

                    if (instruction == F_VOID_ADD_EQ)
                        pop_stack();
                    else
                        sp->u.number = left; /* It is already a T_NUMBER. */
                }
                else
                {
                    OP_ARG_ERROR(2, TF_NUMBER, type2);
                    /* NOTREACHED */
                }
                break;

            case LVALUE_UNPROTECTED_RANGE:
                ERRORF(("Bad argument to +=: Addition to a range is not implemented.\n"));
                break; /* NOTREACHED */

            case LVALUE_UNPROTECTED_MAPENTRY:
                argp = get_map_lvalue(current_unprotected_mapentry.map, &(current_unprotected_mapentry.key)) + current_unprotected_mapentry.index;
                free_svalue(&(current_unprotected_mapentry.key));
                break;
        }

        if(argp == NULL) /* Already handled. */
            break;

        /* Now do it */
        switch(argp->type)
        {

        case T_BYTES:   /* Adding to a byte sequence */
            /* Just prevent adding numbers to them... */
            TYPE_TEST_RIGHT(sp-1, T_BYTES);
            /* FALLTHROUGH */

        case T_STRING:  /* Adding to a string */
          {
            string_t *new_string;

            /* Perform the addition, creating new_string */
            if (type2 == T_STRING || type2 == T_BYTES)
            {
                string_t *left, *right;
                size_t len;

                TYPE_TEST_RIGHT(sp-1, argp->type);

                left = argp->u.str;
                right = (sp-1)->u.str;

                len = mstrsize(left) + mstrsize(right);
                DYN_STRING_COST(len)
                new_string = mstr_add(left, right);
                if (!new_string)
                    ERRORF(("Out of memory (%zu bytes)\n", len));
            }
            else if (type2 == T_NUMBER)
            {
                char buff[80];
                size_t len;

                buff[sizeof(buff)-1] = '\0';
                sprintf(buff, "%ld", (long)u2.number);
                if (buff[sizeof(buff)-1] != '\0')
                    FATAL("Buffer overflow in F_ADD_EQ: int number too big.\n");
                len = mstrsize(argp->u.str)+strlen(buff);
                DYN_STRING_COST(len)
                new_string = mstr_add_txt(argp->u.str, buff, strlen(buff));
                if (!new_string)
                    ERRORF(("Out of memory (%lu bytes)\n"
                           , (unsigned long) len
                           ));
            }
            else if (type2 == T_FLOAT)
            {
                char buff[160];
                size_t len;

                buff[sizeof(buff)-1] = '\0';
                sprintf(buff, "%g", READ_DOUBLE(sp-1) );
                if (buff[sizeof(buff)-1] != '\0')
                    FATAL("Buffer overflow in F_ADD_EQ: float number too big.\n");
                len = mstrsize(argp->u.str) + strlen(buff);
                DYN_STRING_COST(len)
                new_string = mstr_add_txt(argp->u.str, buff, strlen(buff));
                if (!new_string)
                    ERRORF(("Out of memory (%zu bytes).\n", len));
            }
            else
            {
                OP_ARG_ERROR(2, TF_STRING|TF_FLOAT|TF_NUMBER, type2);
                /* NOTREACHED */
            }

            /* Replace *argp by the new string */
            free_string_svalue(argp);
            argp->u.str = new_string;
            break;
          }

        case T_NUMBER:  /* Add to a number */
            if (type2 == T_NUMBER)
            {
                p_int left = argp->u.number;
                p_int right = u2.number;

                if ((left >= 0 && right >= 0 && PINT_MAX - left < right)
                 || (left < 0 && right < 0 && PINT_MIN - left > right)
                   )
                {
                    ERRORF(("Numeric overflow: %"PRIdPINT" += %"PRIdPINT"\n"
                           , left, right));
                    /* NOTREACHED */
                    break;
                }

                argp->u.number += u2.number;
            }
            else if (type2 == T_FLOAT)
            {
                STORE_DOUBLE_USED
                double sum;

                sum = (double)(argp->u.number) + READ_DOUBLE(sp-1);
                if (sum < (-DBL_MAX) || sum > DBL_MAX)
                    ERRORF(("Numeric overflow: %"PRIdPINT" + %g\n"
                           , argp->u.number, READ_DOUBLE(sp-1)));
                argp->type = T_FLOAT;
                STORE_DOUBLE(argp, sum);
            }
            else if (type2 == T_STRING)
            {
                char buff[80];
                string_t *right, *res;
                size_t len;

                right = (sp-1)->u.str;
                buff[sizeof(buff)-1] = '\0';
                sprintf(buff, "%"PRIdPINT, argp->u.number);
                if (buff[sizeof(buff)-1] != '\0')
                    FATAL("Buffer overflow in F_ADD_EQ: int number too big.\n");
                len = mstrsize(right)+strlen(buff);
                DYN_STRING_COST(len)
                res = mstr_add_to_txt(buff, strlen(buff), right);
                if (!res)
                    ERRORF(("Out of memory (%zu bytes)\n", len));

                /* Overwrite the number in argp */
                put_string(argp, res);
            }
            else
            {
                OP_ARG_ERROR(2, TF_NUMBER, type2);
                /* NOTREACHED */
            }
            break;

        case T_MAPPING:  /* Add to a mapping */
            if (type2 != T_MAPPING)
            {
                OP_ARG_ERROR(2, TF_MAPPING, type2);
                /* NOTREACHED */
            }
            else
            {
                add_to_mapping(argp->u.map, u2.map);
                if ((max_mapping_size && MAP_TOTAL_SIZE(argp->u.map) > (p_int)max_mapping_size)
                 || (max_mapping_keys && MAP_SIZE(argp->u.map) > (p_int)max_mapping_keys)
                  )
                {
                    check_map_for_destr_keys(argp->u.map);
                    if (max_mapping_size && MAP_TOTAL_SIZE(argp->u.map) > (p_int)max_mapping_size)
                        ERRORF(("Illegal mapping size: %"PRIdMPINT" elements "
                                "(%"PRIdPINT" x %"PRIdPINT")\n"
                               , (mp_int)MAP_TOTAL_SIZE(argp->u.map)
                               , MAP_SIZE(argp->u.map)
                               , argp->u.map->num_values));
                    if (max_mapping_keys && MAP_SIZE(argp->u.map) > (p_int)max_mapping_keys)
                        ERRORF(("Illegal mapping size: %"PRIdPINT" entries\n"
                               , MAP_SIZE(argp->u.map)
                              ));
                }
            }
            break;

        case T_POINTER:  /* Add to an array */
            if (type2 != T_POINTER)
            {
                OP_ARG_ERROR(2, TF_POINTER, type2);
                /* NOTREACHED */
            }
            else
            {
                inter_sp = sp;
                inter_pc = pc;
                DYN_ARRAY_COST(VEC_SIZE(u2.vec)+VEC_SIZE(argp->u.vec));
                inter_add_array(u2.vec, &argp->u.vec);

                /* The reference of sp[-1] is gone. */
                sp[-1].type = T_INVALID;
            }
            break;

        case T_FLOAT:  /* Add to a float */
            if (type2 == T_FLOAT)
            {
                STORE_DOUBLE_USED
                double d;

                   /* don't use the address of u2, this would prevent putting
                    * it in a register
                    */
                d = READ_DOUBLE(argp) + READ_DOUBLE(sp-1);
                if (d < (-DBL_MAX) || d > DBL_MAX)
                    ERRORF(("Numeric overflow: %g + %g\n"
                           , READ_DOUBLE(argp), READ_DOUBLE(sp-1)));
                STORE_DOUBLE(argp, d);
            }
            else if (type2 == T_NUMBER)
            {
                STORE_DOUBLE_USED
                double d;

                d = READ_DOUBLE(argp) + (double)sp[-1].u.number;
                if (d < (-DBL_MAX) || d > DBL_MAX)
                    ERRORF(("Numeric overflow: %g + %"PRIdPINT"\n"
                           , READ_DOUBLE(argp), (sp-1)->u.number));
                STORE_DOUBLE(argp, d);
            }
            else
            {
                OP_ARG_ERROR(2, TF_FLOAT|TF_NUMBER, type2);
                /* NOTREACHED */
            }
            break;

        default:
            OP_ARG_ERROR(1, TF_STRING|TF_BYTES|TF_FLOAT|TF_MAPPING|TF_POINTER|TF_NUMBER
                        , argp->type);
            /* NOTREACHED */
        } /* end of switch */

        /* Remove the arguments from the stack. */
        pop_n_elems(2);

        /* If the instruction is F_ADD_EQ, leave the result on the stack */
        if (instruction != F_VOID_ADD_EQ)
        {
            sp++;
            assign_svalue_no_free(sp, argp);
        }
        break;
    }

    CASE(F_SUB_EQ);                 /* --- sub_eq              --- */
    {
        /* Subtract sp[-1] from the value designated by lvalue sp[0] (the
         * order is important), assign the result to sp[0] and also leave
         * it on the stack.
         * Ex. v1 -= v2;
         *
         * Possible type combinations:
         *   int         - int                -> int
         *   float       - (float,int)        -> float
         *   int         - float              -> float
         *   string      - string             -> string
         *   bytes       - bytes              -> bytes
         *   vector      - vector             -> vector
         *   vector      - mapping            -> vector
         *   mapping     - mapping            -> mapping
         *   mapping     - vector             -> mapping
         */

        short type2;         /* type and value of sp[-1] */
        union u u2;
        svalue_t *argp; /* the actual value of sp[0] */
        svalue_t *lval;

        type2 = sp[-1].type;
        u2 = sp[-1].u;

#ifdef DEBUG
        TYPE_TEST_LEFT(sp, T_LVALUE);
#endif

        /* Set argp to the actual value designated by sp[0].
         * We handle special lvalues right away.
         */
        argp = NULL;
        lval = get_unprotected_lvalue(sp);

        switch (lval->x.lvalue_type)
        {
            default:
                fatal("(F_SUB_EQ) Illegal lvalue %p type %d\n", lval, lval->x.lvalue_type);
                /* NOTREACHED */
                break;

            case LVALUE_UNPROTECTED:
                argp = lval->u.lvalue;
                break;

            case LVALUE_UNPROTECTED_CHAR:
                if (type2 == T_NUMBER)
                {
                    p_int left = read_unprotected_char();
                    p_int right = u2.number;

                    if ((left >= 0 && right < 0 && PINT_MAX + right < left)
                     || (left < 0 && right >= 0 && PINT_MIN + right > left)
                       )
                    {
                        ERRORF(("Numeric overflow: %"PRIdPINT" -= %"PRIdPINT"\n"
                               , left, right));
                        /* NOTREACHED */
                        break;
                    }

                    left = assign_unprotected_char(left - right);
                    pop_stack();
                    sp->u.number = left; /* It is already a T_NUMBER. */
                }
                else
                {
                    OP_ARG_ERROR(2, TF_NUMBER, type2);
                    /* NOTREACHED */
                }
                break;

            case LVALUE_UNPROTECTED_RANGE:
                ERRORF(("Bad argument to -=: Subtraction from a range is not implemented.\n"));
                break; /* NOTREACHED */

            case LVALUE_UNPROTECTED_MAPENTRY:
                argp = get_map_lvalue(current_unprotected_mapentry.map, &(current_unprotected_mapentry.key)) + current_unprotected_mapentry.index;
                free_svalue(&(current_unprotected_mapentry.key));
                break;
        }

        if(argp == NULL) /* Already handled. */
            break;

        /* Now do it */
        switch (argp->type)
        {
        case T_NUMBER:  /* Subtract from a number */
            if (type2 == T_NUMBER)
            {
                p_int left = argp->u.number;
                p_int right = u2.number;

                if ((left >= 0 && right < 0 && PINT_MAX + right < left)
                 || (left < 0 && right >= 0 && PINT_MIN + right > left)
                   )
                {
                    ERRORF(("Numeric overflow: %"PRIdPINT" -= %"PRIdPINT"\n"
                           , left, right));
                    /* NOTREACHED */
                    break;
                }
                argp->u.number -= u2.number;
            }
            else if (type2 == T_FLOAT)
            {
                STORE_DOUBLE_USED
                double diff;

                diff = (double)(argp->u.number) - READ_DOUBLE(sp-1);
                if (diff < (-DBL_MAX) || diff > DBL_MAX)
                    ERRORF(("Numeric overflow: %"PRIdPINT" - %g\n"
                           , argp->u.number, READ_DOUBLE(sp-1)));
                argp->type = T_FLOAT;
                STORE_DOUBLE(argp, diff);
            }
            else
            {
                /* type2 of the wrong type */
                OP_ARG_ERROR(2, TF_NUMBER|TF_FLOAT, type2);
                /* NOTREACHED */
            }
            break;

        case T_STRING:   /* Subtract from a string */
        case T_BYTES:    /* Subtract from a byte sequence */
            if (type2 == argp->type)
            {
                string_t * result;

                inter_sp = sp;
                result = intersect_strings(argp->u.str, (sp-1)->u.str, MY_TRUE);
                free_string_svalue(argp);
                argp->u.str = result;
            }
            else
            {
                BAD_OP_ARG(2, argp->type, type2);
                /* NOTREACHED */
            }
            break;

        case T_POINTER:  /* Subtract from an array */
            if (type2 == T_POINTER)
            {
                vector_t *v;
                v = u2.vec;

                /* Duplicate the minuend array if necessary, as
                 * the subtraction will change and free it
                 */
                if (v->ref > 1)
                {
                    deref_array(v);
                    v = slice_array(v, 0, (mp_int)VEC_SIZE(v)-1 );
                }
                argp->u.vec = subtract_array(argp->u.vec, v);
                sp[-1].type = T_INVALID; /* subtract_array used that reference. */
            }
            else if (type2 == T_MAPPING)
            {
                argp->u.vec = map_intersect_array(argp->u.vec, u2.map, true);
                sp[-1].type = T_INVALID; /* map_intersect_array freed that alreay. */
            }
            else
            {
                OP_ARG_ERROR(2, TF_POINTER, type2);
                /* NOTREACHED */
            }
            break;

        case T_FLOAT:  /* Subtract from a float */
            if (type2 == T_FLOAT)
            {
                STORE_DOUBLE_USED
                double d;

                /* don't use the address of u2, this would prevent putting it
                 * in a register
                 */
                d = READ_DOUBLE(argp) - READ_DOUBLE(sp-1);
                if (d < (-DBL_MAX) || d > DBL_MAX)
                    ERRORF(("Numeric overflow: %g + %g\n"
                           , READ_DOUBLE(argp), READ_DOUBLE(sp-1)));
                STORE_DOUBLE(argp, d);
            }
            else if (type2 == T_NUMBER)
            {
                STORE_DOUBLE_USED
                double d;

                d = READ_DOUBLE(argp) - (double)sp[-1].u.number;
                if (d < (-DBL_MAX) || d > DBL_MAX)
                    ERRORF(("Numeric overflow: %g + %"PRIdPINT"\n"
                           , READ_DOUBLE(argp), sp[-1].u.number));
                STORE_DOUBLE(argp, d);
            }
            else
            {
                OP_ARG_ERROR(2, TF_FLOAT|TF_NUMBER, type2);
                /* NOTREACHED */
            }
            break;

        case T_MAPPING:  /* Subtract from a mapping */
            if (type2 == T_MAPPING)
            {
                mapping_t *m;

                m = sp[-1].u.map;

                /* Test for the special case 'm - m'
                 * Note: do not return a new empty mapping, argp->u.map must be changed.
                 */
                if (m == argp->u.map)
                {
                    /* m->ref is > 1, because the content of the lvalue is
                     * associated with a ref
                     */
                    deref_mapping(m);
                    m = copy_mapping(m);
                }

                walk_mapping(m, sub_from_mapping_filter, argp->u.map);
                free_mapping(m);
                sp[-1].type = T_INVALID;
            }
            else if (type2 == T_POINTER)
            {
                map_subtract_eq_array(argp->u.map, u2.vec);
                free_array(u2.vec);
                sp[-1].type = T_INVALID;
            }
            else
            {
                OP_ARG_ERROR(2, TF_MAPPING, type2);
                /* NOTREACHED */
            }
            break;

        default:
            OP_ARG_ERROR(1, TF_STRING|TF_BYTES|TF_FLOAT|TF_MAPPING|TF_POINTER|TF_NUMBER
                        , argp->type);
            /* NOTREACHED */
        } /* end of switch */

        pop_n_elems(2);
        assign_svalue_no_free(++sp, argp);
        break;
    }

    CASE(F_MULT_EQ);                /* --- mult_eq             --- */
    {
        /* Multiply sp[-1] to the value designated by lvalue sp[0],
         * assign the result to sp[0] and also leave it on the stack.
         *
         * Possible type combinations:
         *   int         * int                -> int
         *   float       * (float,int)        -> float
         *   int         * float              -> float
         *   string      * int                -> string
         *   bytes       * int                -> bytes
         *   array       * int                -> array
         *
         * TODO: Extend this to mappings.
         */

        svalue_t *argp;
        svalue_t *lval;

#ifdef DEBUG
        TYPE_TEST_LEFT(sp, T_LVALUE);
#endif

        /* Set argp to the actual value designated by sp[0].
         * We handle special lvalues right away.
         */
        argp = NULL;
        lval = get_unprotected_lvalue(sp);

        switch (lval->x.lvalue_type)
        {
            default:
                fatal("(F_MULT_EQ) Illegal lvalue %p type %d\n", lval, lval->x.lvalue_type);
                /* NOTREACHED */
                break;

            case LVALUE_UNPROTECTED:
                argp = lval->u.lvalue;
                break;

            case LVALUE_UNPROTECTED_CHAR:
                if (sp[-1].type == T_NUMBER)
                {
                    p_int left = read_unprotected_char();
                    p_int right = sp[-1].u.number;

                    if (left > 0 && right > 0)
                    {
                        if ((left != 0 && PINT_MAX / left < right)
                         || (right != 0 && PINT_MAX / right < left)
                           )
                        {
                            ERRORF(("Numeric overflow: %"PRIdPINT" *= %"
                                    PRIdPINT"\n", left, right));
                            /* NOTREACHED */
                            break;
                        }
                    }
                    else if (left < 0 && right < 0)
                    {
                        if ((left != 0 && PINT_MAX / left > right)
                         || (right != 0 && PINT_MAX / right > left)
                           )
                        {
                            ERRORF(("Numeric overflow: %"PRIdPINT" *= %"
                                    PRIdPINT"\n", left, right));
                            /* NOTREACHED */
                            break;
                        }
                    }
                    else if (left != 0 && right != 0)
                    {
                        if ((left > 0 && PINT_MIN / left > right)
                         || (right > 0 && PINT_MIN / right > left)
                           )
                        {
                            ERRORF(("Numeric overflow: %"PRIdPINT" *= %"
                                    PRIdPINT"\n", left, right));
                            /* NOTREACHED */
                            break;
                        }
                    }

                    left = assign_unprotected_char(left * right);
                    pop_stack();
                    sp->u.number = left; /* It is already a T_NUMBER. */
                }
                else
                {
                    OP_ARG_ERROR(2, TF_NUMBER, sp[-1].type);
                    /* NOTREACHED */
                }
                break;

            case LVALUE_UNPROTECTED_RANGE:
                ERRORF(("Bad argument to *=: Multiplicating a range is not implemented.\n"));
                break; /* NOTREACHED */

            case LVALUE_UNPROTECTED_MAPENTRY:
                argp = get_map_lvalue(current_unprotected_mapentry.map, &(current_unprotected_mapentry.key)) + current_unprotected_mapentry.index;
                free_svalue(&(current_unprotected_mapentry.key));
                break;
        }

        if(argp == NULL) /* Already handled. */
            break;

        /* Now do it */
        switch (argp->type)
        {
        case T_NUMBER:
            if (sp[-1].type == T_NUMBER)
            {
                p_int left = argp->u.number;
                p_int right = sp[-1].u.number;

                if (left > 0 && right > 0)
                {
                    if ((left != 0 && PINT_MAX / left < right)
                     || (right != 0 && PINT_MAX / right < left)
                       )
                    {
                        ERRORF(("Numeric overflow: %"PRIdPINT" *= %"
                                PRIdPINT"\n"
                               , left, right));
                        /* NOTREACHED */
                        break;
                    }
                }
                else if (left < 0 && right < 0)
                {
                    if ((left != 0 && PINT_MAX / left > right)
                     || (right != 0 && PINT_MAX / right > left)
                       )
                    {
                        ERRORF(("Numeric overflow: %"PRIdPINT" *= %"
                                PRIdPINT"\n"
                               , left, right));
                        /* NOTREACHED */
                        break;
                    }
                }
                else if (left != 0 && right != 0)
                {
                    if ((left > 0 && PINT_MIN / left > right)
                     || (right > 0 && PINT_MIN / right > left)
                       )
                    {
                        ERRORF(("Numeric overflow: %"PRIdPINT" *= %"
                                PRIdPINT"\n"
                               , left, right));
                        /* NOTREACHED */
                        break;
                    }
                }
                argp->u.number *= sp[-1].u.number;
            } /* type2 == T_NUMBER */
            else if (sp[-1].type == T_FLOAT)
            {
                STORE_DOUBLE_USED
                double product;

                product = argp->u.number * READ_DOUBLE(sp-1);
                if (product < (-DBL_MAX) || product > DBL_MAX)
                    ERRORF(("Numeric overflow: %"PRIdPINT" * %g\n"
                           , argp->u.number, READ_DOUBLE(sp-1)));
                argp->type = T_FLOAT;
                STORE_DOUBLE(argp, product);
            }
            else
            {
                /* Unsupported type2 */
                OP_ARG_ERROR(2, TF_NUMBER|TF_FLOAT, sp[-1].type);
                /* NOTREACHED */
            }
            break;

        case T_FLOAT:
            if (sp[-1].type == T_FLOAT)
            {
                STORE_DOUBLE_USED
                double d;

                d = READ_DOUBLE(argp) * READ_DOUBLE(sp-1);
                if (d < (-DBL_MAX) || d > DBL_MAX)
                    ERRORF(("Numeric overflow: %g * %g\n"
                           , READ_DOUBLE(argp), READ_DOUBLE(sp-1)));
                STORE_DOUBLE(argp, d);
            }
            else if (sp[-1].type == T_NUMBER)
            {
                STORE_DOUBLE_USED
                double d;

                d = READ_DOUBLE(argp) * (double)sp[-1].u.number;
                if (d < (-DBL_MAX) || d > DBL_MAX)
                    ERRORF(("Numeric overflow: %g * %"PRIdPINT"\n"
                           , READ_DOUBLE(argp), sp[-1].u.number));
                STORE_DOUBLE(argp, d);
            }
            else
            {
                OP_ARG_ERROR(2, TF_NUMBER|TF_FLOAT, sp[-1].type);
                /* NOTREACHED */
            }
            break;

        case T_STRING:
        case T_BYTES:
            if (sp[-1].type == T_NUMBER)
            {
                string_t * result;
                size_t reslen;
                size_t len;

                if (sp[-1].u.number < 0)
                {
                    ERROR("Bad right arg to *=: negative number\n");
                    /* NOTREACHED */
                }

                len = mstrsize(argp->u.str);

                if (len > (size_t)PINT_MAX
                 || (   len != 0
                     && PINT_MAX / (p_int)len < sp[-1].u.number)
                 || (   sp[-1].u.number != 0
                     && PINT_MAX / sp[-1].u.number < (p_int)len)
                   )
                    ERRORF(("Result string too long (%"PRIdPINT" * %zu).\n"
                           , sp[-1].u.number, len
                           ));

                reslen = (size_t)sp[-1].u.number * len;
                result = mstr_repeat(argp->u.str, (size_t)sp[-1].u.number);
                if (!result)
                    ERRORF(("Out of memory (%zu bytes).\n", reslen));

                DYN_STRING_COST(reslen)

                free_string_svalue(argp);
                argp->u.str = result;
            }
            else
            {
                OP_ARG_ERROR(2, TF_NUMBER, sp[-1].type);
                /* NOTREACHED */
            }
            break;

        case T_POINTER:
            if (sp[-1].type == T_NUMBER)
            {
                vector_t *result;
                mp_int reslen;
                p_uint len;

                if (sp[-1].u.number < 0)
                {
                    ERROR("Bad right arg to *=: negative number\n");
                    /* NOTREACHED */
                }

                inter_sp = sp;
                inter_pc = pc;
                len = VEC_SIZE(argp->u.vec);
                reslen = sp[-1].u.number * (mp_int)len;
                result = allocate_uninit_array(reslen);
                DYN_ARRAY_COST(reslen);

                if (sp[-1].u.number > 0 && len)
                {
                    p_uint left;
                    svalue_t *from, *to;

                    /* Seed result[] with one copy of the array.
                     */
                    for ( from = argp->u.vec->item, to = result->item, left = len
                        ; left
                        ; from++, to++, left--)
                    {
                        assign_svalue_no_free(to, from);
                    } /* for() seed */

                    /* Now fill the remainder of the vector with
                     * the values already copied in there.
                     */
                    for (from = result->item, left = reslen - len
                        ; left
                        ; to++, from++, left--
                        )
                        assign_svalue_no_free(to, from);
                } /* if (len) */

                free_svalue(argp);
                put_array(argp, result);
            }
            else
            {
                OP_ARG_ERROR(2, TF_NUMBER, sp[-1].type);
                /* NOTREACHED */
            }
            break;

        default:
            OP_ARG_ERROR(1, TF_STRING|TF_BYTES|TF_FLOAT|TF_POINTER|TF_NUMBER
                        , argp->type);
            /* NOTREACHED */
            break;
        }

        pop_n_elems(2);
        assign_svalue_no_free(++sp, argp);
        break;
    }

    CASE(F_DIV_EQ);                 /* --- div_eq              --- */
    {
        /* Divide the value designated by lvalue sp[0] by sp[-1],
         * assign the result to sp[0] and also leave it on the stack.
         *
         * Possible type combinations:
         *   int         / int                -> int
         *   float       / (float,int)        -> float
         *   int         - float              -> float
         *
         * TODO: Extend this to arrays and mappings.
         */

        svalue_t *argp;
        svalue_t *lval;

#ifdef DEBUG
        TYPE_TEST_LEFT(sp, T_LVALUE);
#endif

        /* Set argp to the actual value designated by sp[0].
         * We handle special lvalues right away.
         */
        argp = NULL;
        lval = get_unprotected_lvalue(sp);

        switch (lval->x.lvalue_type)
        {
            default:
                fatal("(F_DIV_EQ) Illegal lvalue %p type %d\n", lval, lval->x.lvalue_type);
                /* NOTREACHED */
                break;

            case LVALUE_UNPROTECTED:
                argp = lval->u.lvalue;
                break;

            case LVALUE_UNPROTECTED_CHAR:
                if (sp[-1].type == T_NUMBER)
                {
                    p_int val = read_unprotected_char();

                    if (sp[-1].u.number == 0)
                        ERROR("Division by zero\n");

                    val = assign_unprotected_char(val / sp[-1].u.number);
                    sp[-1].u.number = val;
                    pop_stack();
                }
                else
                {
                    OP_ARG_ERROR(2, TF_NUMBER, sp[-1].type);
                    /* NOTREACHED */
                }
                break;

            case LVALUE_UNPROTECTED_RANGE:
                OP_ARG_ERROR(2, TF_FLOAT|TF_NUMBER, T_POINTER);
                break; /* NOTREACHED */

            case LVALUE_UNPROTECTED_MAPENTRY:
                argp = get_map_lvalue(current_unprotected_mapentry.map, &(current_unprotected_mapentry.key)) + current_unprotected_mapentry.index;
                free_svalue(&(current_unprotected_mapentry.key));
                break;
        }

        if(argp == NULL) /* Already handled. */
            break;

        /* Now do it */
        switch (argp->type)
        {
        case T_NUMBER:
            if (sp[-1].type == T_NUMBER)
            {
                if (sp[-1].u.number == 0)
                    ERROR("Division by zero\n");
                if (argp->u.number == PINT_MIN && sp[-1].u.number == -1)
                    ERRORF(("Numeric overflow: %"PRIdPINT" / -1\n"
                           , argp->u.number
                           ));
                argp->u.number /= sp[-1].u.number;
            }
            else if (sp[-1].type == T_FLOAT)
            {
                double dtmp;
                STORE_DOUBLE_USED

                dtmp = READ_DOUBLE( sp-1 );
                if (dtmp == 0.)
                    ERROR("Division by zero\n");
                dtmp = (double)argp->u.number / dtmp;
                if (dtmp < (-DBL_MAX) || dtmp > DBL_MAX)
                    ERRORF(("Numeric overflow: %"PRIdPINT" / %g\n"
                           , argp->u.number, READ_DOUBLE(sp-1)));
                argp->type = T_FLOAT;
                STORE_DOUBLE(argp, dtmp);
            }
            else
            {
                /* Unsupported type2 */
                OP_ARG_ERROR(2, TF_NUMBER|TF_FLOAT, sp[-1].type);
                /* NOTREACHED */
            }
            break;

        case T_FLOAT:
            if (sp[-1].type == T_FLOAT)
            {
                STORE_DOUBLE_USED
                double d;

                d = READ_DOUBLE(sp-1);
                if (d == 0.0)
                    ERROR("Division by zero\n");
                d = READ_DOUBLE(argp) / d;
                if (d < (-DBL_MAX) || d > DBL_MAX)
                    ERRORF(("Numeric overflow: %g / %g\n"
                           , READ_DOUBLE(argp), READ_DOUBLE(sp-1)));
                STORE_DOUBLE(argp, d);
            }
            else if (sp[-1].type == T_NUMBER)
            {
                STORE_DOUBLE_USED
                double d;
                p_int i;

                i = sp[-1].u.number;
                if (i == 0)
                    ERROR("Division by zero\n");
                d = READ_DOUBLE(argp) / (double)i;
                if (d < (-DBL_MAX) || d > DBL_MAX)
                    ERRORF(("Numeric overflow: %g / %"PRIdPINT"\n"
                           , READ_DOUBLE(argp), sp[-1].u.number));
                STORE_DOUBLE(argp, d);
            }
            else
            {
                OP_ARG_ERROR(2, TF_NUMBER|TF_FLOAT, sp[-1].type);
                /* NOTREACHED */
            }
            break;

        default:
            OP_ARG_ERROR(1, TF_FLOAT|TF_NUMBER, argp->type);
            /* NOTREACHED */
        }

        pop_n_elems(2);
        assign_svalue_no_free(++sp, argp);
        break;
    }

    CASE(F_MOD_EQ);                 /* --- mod_eq              --- */
    {
        /* Compute the modulus of the value designated by lvalue sp[0]
         * divided by sp[-1], assign the result to sp[0] and also
         * leave it on the stack.
         *
         * Possible type combinations:
         *   int         % int                -> int
         *
         * TODO: Extend this to arrays and mappings.
         * TODO: Implement the other remainder function.
         */

        svalue_t *argp;
        svalue_t *lval;

#ifdef DEBUG
        TYPE_TEST_LEFT(sp, T_LVALUE);
#endif

        /* Set argp to the actual value designated by sp[0].
         * We handle special lvalues right away.
         */
        argp = NULL;
        lval = get_unprotected_lvalue(sp);

        switch (lval->x.lvalue_type)
        {
            default:
                fatal("(F_MOD_EQ) Illegal lvalue %p type %d\n", lval, lval->x.lvalue_type);
                /* NOTREACHED */
                break;

            case LVALUE_UNPROTECTED:
                argp = lval->u.lvalue;
                break;

            case LVALUE_UNPROTECTED_CHAR:
                if (sp[-1].type == T_NUMBER)
                {
                    p_int val = read_unprotected_char();

                    if (sp[-1].u.number == 0)
                        ERROR("Division by zero\n");

                    val = assign_unprotected_char(val % sp[-1].u.number);
                    sp[-1].u.number = val;
                    pop_stack();
                }
                else
                {
                    OP_ARG_ERROR(2, TF_NUMBER, sp[-1].type);
                    /* NOTREACHED */
                }
                break;

            case LVALUE_UNPROTECTED_RANGE:
                OP_ARG_ERROR(2, TF_NUMBER, T_POINTER);
                break; /* NOTREACHED */

            case LVALUE_UNPROTECTED_MAPENTRY:
                argp = get_map_lvalue(current_unprotected_mapentry.map, &(current_unprotected_mapentry.key)) + current_unprotected_mapentry.index;
                free_svalue(&(current_unprotected_mapentry.key));
                break;
        }

        if(argp == NULL) /* Already handled. */
            break;

        /* Now do it */
        switch (argp->type)
        {
        case T_NUMBER:
            if (sp[-1].type == T_NUMBER)
            {
                if (sp[-1].u.number == 0)
                    ERROR("Division by zero\n");
                argp->u.number %= sp[-1].u.number;
            }
            else
            {
                OP_ARG_ERROR(2, TF_NUMBER, sp[-1].type);
                /* NOTREACHED */
            }
            break;

        default:
            OP_ARG_ERROR(1, TF_NUMBER, argp->type);
            /* NOTREACHED */
        }

        pop_n_elems(2);
        assign_svalue_no_free(++sp, argp);
        break;
    }

    CASE(F_AND_EQ);                 /* --- and_eq              --- */
    {
        /* Intersect the value designated by lvalue sp[0] with sp[-1],
         * assign the result to sp[0] and also leave it on the stack.
         *
         * Possible type combinations:
         *   int    & int    -> int
         *   string & string -> string
         *   bytes  & bytes  -> bytes
         *   array  & array  -> array
         *   array  & mapping -> array
         *   mapping & array -> mapping
         *   mapping & mapping -> mapping
         */

        svalue_t *argp;
        svalue_t *lval;

#ifdef DEBUG
        TYPE_TEST_LEFT(sp, T_LVALUE);
#endif

        /* Set argp to the actual value designated by sp[0].
         * We handle special lvalues right away.
         */
        argp = NULL;
        lval = get_unprotected_lvalue(sp);

        switch (lval->x.lvalue_type)
        {
            default:
                fatal("(F_AND_EQ) Illegal lvalue %p type %d\n", lval, lval->x.lvalue_type);
                /* NOTREACHED */
                break;

            case LVALUE_UNPROTECTED:
                argp = lval->u.lvalue;
                break;

            case LVALUE_UNPROTECTED_CHAR:
                if (sp[-1].type == T_NUMBER)
                {
                    p_int val = read_unprotected_char();

                    val = assign_unprotected_char(val & sp[-1].u.number);
                    sp[-1].u.number = val;
                    pop_stack();
                }
                else
                {
                    OP_ARG_ERROR(2, TF_NUMBER, sp[-1].type);
                    /* NOTREACHED */
                }
                break;

            case LVALUE_UNPROTECTED_RANGE:
                ERRORF(("Bad argument to &=: Intersecting a range is not implemented.\n"));
                break; /* NOTREACHED */

            case LVALUE_UNPROTECTED_MAPENTRY:
                argp = get_map_lvalue(current_unprotected_mapentry.map, &(current_unprotected_mapentry.key)) + current_unprotected_mapentry.index;
                free_svalue(&(current_unprotected_mapentry.key));
                break;
        }

        if(argp == NULL) /* Already handled. */
            break;

        /* Now do it */
        switch (argp->type)
        {
        case T_NUMBER:  /* Intersect a number */
            if (sp[-1].type == T_NUMBER)
            {
                argp->u.number &= sp[-1].u.number;
            }
            else
            {
                OP_ARG_ERROR(2, TF_NUMBER, sp[-1].type);
                /* NOTREACHED */
            }
            break;

        case T_POINTER:
            /* Intersect an array */
            if (sp[-1].type == T_POINTER)
            {
                vector_t *vec1, *vec2;

                vec1 = argp->u.vec;
                vec2 = sp[-1].u.vec;

                /* intersect_array will free both arguments (also on errors).
                 * With sp[-1] we can live without, the other we need.
                 */
                ref_array(vec1);
                sp[-1].type = T_NUMBER;

                inter_sp = sp;
                argp->u.vec = intersect_array(vec1, vec2);

                free_array(vec1);
            }
            else if (sp[-1].type == T_MAPPING)
            {
                vector_t *vec;
                mapping_t * map;

                vec = argp->u.vec;
                map = sp[-1].u.map;

                /* map_intersect_array frees both,
                 * so taking an extra ref of the lvalue.
                 */
                ref_array(vec);
                sp[-1].type = T_NUMBER;

                inter_sp = sp;
                argp->u.vec = map_intersect_array(vec, map, false);
                free_array(vec);
            }
            else
            {
                OP_ARG_ERROR(2, TF_POINTER|TF_MAPPING, sp[-1].type);
                /* NOTREACHED */
            }
            break;

        case T_MAPPING:
            /* Intersect a mapping */
            if (sp[-1].type == T_POINTER || sp[-1].type == T_MAPPING)
            {
                 inter_sp = sp;
                 argp->u.map = map_intersect(argp->u.map, sp-1);
                 sp[-1].type = T_NUMBER; /* freed by map_intersect */
            }
            else
            {
                OP_ARG_ERROR(2, TF_MAPPING|TF_POINTER, sp[-1].type);
                /* NOTREACHED */
            }
            break;

        case T_STRING:
        case T_BYTES:
            if (sp[-1].type == argp->type)
            {
                string_t * result;

                inter_sp = sp;
                result = intersect_strings(argp->u.str, (sp-1)->u.str, MY_FALSE);

                free_string_svalue(argp);
                argp->u.str = result;
            }
            else
            {
                BAD_OP_ARG(2, argp->type, sp[-1].type);
                /* NOTREACHED */
            }
            break;
        default:
            OP_ARG_ERROR(1, TF_NUMBER|TF_STRING|TF_BYTES|TF_POINTER, argp->type);
            /* NOTREACHED */
        }

        pop_n_elems(2);
        assign_svalue_no_free(++sp, argp);
        break;
    }

    CASE(F_OR_EQ);                  /* --- or_eq               --- */
    {
        /* Binary-Or the value designated by lvalue sp[0] with sp[-1],
         * assign the result to sp[0] and also leave it on the stack.
         *
         * Possible type combinations:
         *   int   | int   -> int
         *   array | array -> array
         */

        svalue_t *argp;
        svalue_t *lval;

#ifdef DEBUG
        TYPE_TEST_LEFT(sp, T_LVALUE);
#endif

        /* Set argp to the actual value designated by sp[0].
         * We handle special lvalues right away.
         */
        argp = NULL;
        lval = get_unprotected_lvalue(sp);

        switch (lval->x.lvalue_type)
        {
            default:
                fatal("(F_OR_EQ) Illegal lvalue %p type %d\n", lval, lval->x.lvalue_type);
                /* NOTREACHED */
                break;

            case LVALUE_UNPROTECTED:
                argp = lval->u.lvalue;
                break;

            case LVALUE_UNPROTECTED_CHAR:
                if (sp[-1].type == T_NUMBER)
                {
                    p_int val = read_unprotected_char();
                    val = assign_unprotected_char(val | sp[-1].u.number);
                    sp[-1].u.number = val;
                    pop_stack();
                }
                else
                {
                    OP_ARG_ERROR(2, TF_NUMBER, sp[-1].type);
                    /* NOTREACHED */
                }
                break;

            case LVALUE_UNPROTECTED_RANGE:
                ERRORF(("Bad argument to |=: Joining a range is not implemented.\n"));
                break; /* NOTREACHED */

            case LVALUE_UNPROTECTED_MAPENTRY:
                argp = get_map_lvalue(current_unprotected_mapentry.map, &(current_unprotected_mapentry.key)) + current_unprotected_mapentry.index;
                free_svalue(&(current_unprotected_mapentry.key));
                break;
        }

        if(argp == NULL) /* Already handled. */
            break;

        /* Now do it */
        switch (argp->type)
        {
        case T_NUMBER:
            if (sp[-1].type == T_NUMBER)
            {
                argp->u.number |= sp[-1].u.number;
            }
            else
            {
                OP_ARG_ERROR(2, TF_NUMBER, sp[-1].type);
                /* NOTREACHED */
            }
            break;

        case T_POINTER:
            /* Join an array */
            if (sp[-1].type == T_POINTER)
            {
                vector_t *vec1, *vec2;

                inter_sp = sp;
                inter_pc = pc;

                vec1 = argp->u.vec;
                vec2 = sp[-1].u.vec;

                /* The new vec may be one of the original vec1 or vec2 */
                argp->u.vec = join_array(vec1, vec2);
                sp[-1].type = T_NUMBER; /* freed by join_array. */
            }
            else
            {
                OP_ARG_ERROR(2, TF_POINTER, sp[-1].type);
                /* NOTREACHED */
            }
            break;

        default:
            OP_ARG_ERROR(1, TF_NUMBER|TF_POINTER, argp->type);
            /* NOTREACHED */
        }

        pop_n_elems(2);
        assign_svalue_no_free(++sp, argp);
        break;
    }

    CASE(F_XOR_EQ);                 /* --- xor_eq              --- */
    {
        /* Binary-XOr the value designated by lvalue sp[0] with sp[-1],
         * assign the result to sp[0] and also leave it on the stack.
         *
         * Possible type combinations:
         *   int   ^ int   -> int
         *   array ^ array -> array
         *
         * TODO: Extend this to mappings.
         */

        svalue_t *argp;
        svalue_t *lval;

#ifdef DEBUG
        TYPE_TEST_LEFT(sp, T_LVALUE);
#endif

        /* Set argp to the actual value designated by sp[0].
         * We handle special lvalues right away.
         */
        argp = NULL;
        lval = get_unprotected_lvalue(sp);

        switch (lval->x.lvalue_type)
        {
            default:
                fatal("(F_XOR_EQ) Illegal lvalue %p type %d\n", lval, lval->x.lvalue_type);
                /* NOTREACHED */
                break;

            case LVALUE_UNPROTECTED:
                argp = lval->u.lvalue;
                break;

            case LVALUE_UNPROTECTED_CHAR:
                if (sp[-1].type == T_NUMBER)
                {
                    p_int val = read_unprotected_char();
                    val = assign_unprotected_char(val ^ sp[-1].u.number);
                    sp[-1].u.number = val;
                    pop_stack();
                }
                else
                {
                    OP_ARG_ERROR(2, TF_NUMBER, sp[-1].type);
                    /* NOTREACHED */
                }
                break;

            case LVALUE_UNPROTECTED_RANGE:
                ERRORF(("Bad argument to ^=: Symmetric difference with a range is not implemented.\n"));
                break; /* NOTREACHED */

            case LVALUE_UNPROTECTED_MAPENTRY:
                argp = get_map_lvalue(current_unprotected_mapentry.map, &(current_unprotected_mapentry.key)) + current_unprotected_mapentry.index;
                free_svalue(&(current_unprotected_mapentry.key));
                break;
        }

        if(argp == NULL) /* Already handled. */
            break;

        /* Now do it */
        switch (argp->type)
        {
        case T_NUMBER:
            if (sp[-1].type == T_NUMBER)
            {
                argp->u.number ^= sp[-1].u.number;
            }
            else
            {
                OP_ARG_ERROR(2, TF_NUMBER, sp[-1].type);
                /* NOTREACHED */
            }
            break;

        case T_POINTER:
            /* Symm-diff an array */
            if (sp[-1].type == T_POINTER)
            {
                vector_t *vec1, *vec2;

                inter_sp = sp;

                vec1 = argp->u.vec;
                vec2 = sp[-1].u.vec;

                argp->u.vec = symmetric_diff_array(vec1, vec2);
                sp[-1].type = T_NUMBER; /* freed by symmetric_diff_array */
            }
            else
            {
                OP_ARG_ERROR(2, TF_POINTER, sp[-1].type);
                /* NOTREACHED */
            }
            break;

        default:
            OP_ARG_ERROR(1, TF_NUMBER|TF_POINTER, argp->type);
            /* NOTREACHED */
        }

        pop_n_elems(2);
        assign_svalue_no_free(++sp, argp);
        break;
    }

    CASE(F_LSH_EQ);                 /* --- lsh_eq              --- */
    {
        /* Shift the value designated by lvalue sp[0] left by sp[-1],
         * assign the result to sp[0] and also leave it on the stack.
         *
         * Possible type combinations:
         *   int        << int                -> int
         *
         * TODO: Implement an arithmetic shift.
         */

        p_uint shift = sp[-1].u.number;
        svalue_t *argp;
        svalue_t *lval;

#ifdef DEBUG
        TYPE_TEST_LEFT(sp, T_LVALUE);
#endif

        if (shift > MAX_SHIFT)
            shift = MAX_SHIFT;

        /* Set argp to the actual value designated by sp[0].
         * We handle special lvalues right away.
         */
        argp = NULL;
        lval = get_unprotected_lvalue(sp);

        switch (lval->x.lvalue_type)
        {
            default:
                fatal("(F_LSH_EQ) Illegal lvalue %p type %d\n", lval, lval->x.lvalue_type);
                /* NOTREACHED */
                break;

            case LVALUE_UNPROTECTED:
                argp = lval->u.lvalue;
                break;

            case LVALUE_UNPROTECTED_CHAR:
                if (sp[-1].type == T_NUMBER)
                {
                    p_int val = read_unprotected_char();
                    val = assign_unprotected_char(shift > MAX_SHIFT ? 0 : (val << shift));
                    sp[-1].u.number = val;
                    pop_stack();
                }
                else
                {
                    OP_ARG_ERROR(2, TF_NUMBER, sp[-1].type);
                    /* NOTREACHED */
                }
                break;

            case LVALUE_UNPROTECTED_RANGE:
                OP_ARG_ERROR(2, TF_NUMBER, T_POINTER);
                break; /* NOTREACHED */

            case LVALUE_UNPROTECTED_MAPENTRY:
                argp = get_map_lvalue(current_unprotected_mapentry.map, &(current_unprotected_mapentry.key)) + current_unprotected_mapentry.index;
                free_svalue(&(current_unprotected_mapentry.key));
                break;
        }

        if(argp == NULL) /* Already handled. */
            break;

        /* Now do it */
        switch (argp->type)
        {
        case T_NUMBER:
            if (sp[-1].type == T_NUMBER)
            {
                if (shift > MAX_SHIFT)
                    argp->u.number = 0;
                else
                    argp->u.number <<= shift;
            }
            else
            {
                OP_ARG_ERROR(2, TF_NUMBER, sp[-1].type);
                /* NOTREACHED */
            }
            break;

        default:
            OP_ARG_ERROR(1, TF_NUMBER, argp->type);
            /* NOTREACHED */
        }

        pop_n_elems(2);
        assign_svalue_no_free(++sp, argp);
        break;
    }

    CASE(F_RSH_EQ);                 /* --- rsh_eq              --- */
    {
        /* Arithmetically shift the value designated by lvalue sp[0] right by
         * sp[-1], assign the result to sp[0] and also leave it on the stack.
         *
         * Possible type combinations:
         *   int        >> int                -> int
         */

        p_uint shift = sp[-1].u.number;
        svalue_t *argp;
        svalue_t *lval;

#ifdef DEBUG
        TYPE_TEST_LEFT(sp, T_LVALUE);
#endif

        if (shift > MAX_SHIFT)
            shift = MAX_SHIFT + 1;

        /* Set argp to the actual value designated by sp[0].
         * We handle special lvalues right away.
         */
        argp = NULL;
        lval = get_unprotected_lvalue(sp);

        switch (lval->x.lvalue_type)
        {
            default:
                fatal("(F_RSH_EQ) Illegal lvalue %p type %d\n", lval, lval->x.lvalue_type);
                /* NOTREACHED */
                break;

            case LVALUE_UNPROTECTED:
                argp = lval->u.lvalue;
                break;

            case LVALUE_UNPROTECTED_CHAR:
                if (sp[-1].type == T_NUMBER)
                {
                    p_int val = read_unprotected_char();
                    val = assign_unprotected_char(shift > MAX_SHIFT ? 0 : (val >> shift));
                    sp[-1].u.number = val;
                    pop_stack();
                }
                else
                {
                    OP_ARG_ERROR(2, TF_NUMBER, sp[-1].type);
                    /* NOTREACHED */
                }
                break;

            case LVALUE_UNPROTECTED_RANGE:
                OP_ARG_ERROR(2, TF_NUMBER, T_POINTER);
                break; /* NOTREACHED */

            case LVALUE_UNPROTECTED_MAPENTRY:
                argp = get_map_lvalue(current_unprotected_mapentry.map, &(current_unprotected_mapentry.key)) + current_unprotected_mapentry.index;
                free_svalue(&(current_unprotected_mapentry.key));
                break;
        }

        if(argp == NULL) /* Already handled. */
            break;

        /* Now do it */
        switch (argp->type)
        {
        case T_NUMBER:
            if (sp[-1].type == T_NUMBER)
            {
                if (shift > MAX_SHIFT)
                    argp->u.number = 0;
                else
                    argp->u.number >>= shift;
            }
            else
            {
                OP_ARG_ERROR(2, TF_NUMBER, sp[-1].type);
                /* NOTREACHED */
            }
            break;

        default:
            OP_ARG_ERROR(1, TF_NUMBER, argp->type);
            /* NOTREACHED */
        }

        pop_n_elems(2);
        assign_svalue_no_free(++sp, argp);
        break;
    }

    CASE(F_RSHL_EQ);               /* --- rshl_eq              --- */
    {
        /* Logically shift the value designated by lvalue sp[0] right by
         * sp[-1], assign the result to sp[0] and also leave it on the stack.
         *
         * Possible type combinations:
         *   int        >>> int                -> int
         */

        p_uint shift = sp[-1].u.number;
        svalue_t *argp;
        svalue_t *lval;

#ifdef DEBUG
        TYPE_TEST_LEFT(sp, T_LVALUE);
#endif

        /* Set argp to the actual value designated by sp[0].
         * We handle special lvalues right away.
         */
        argp = NULL;
        lval = get_unprotected_lvalue(sp);

        switch (lval->x.lvalue_type)
        {
            default:
                fatal("(F_RSHL_EQ) Illegal lvalue %p type %d\n", lval, lval->x.lvalue_type);
                /* NOTREACHED */
                break;

            case LVALUE_UNPROTECTED:
                argp = lval->u.lvalue;
                break;

            case LVALUE_UNPROTECTED_CHAR:
                if (sp[-1].type == T_NUMBER)
                {
                    /* We treat the chars as unsigned, so
                     * no difference here to F_RSH_EQ.
                     */
                    p_int val = read_unprotected_char();
                    val = assign_unprotected_char(shift > MAX_SHIFT ? 0 : (val >> shift));
                    sp[-1].u.number = val;
                    pop_stack();
                }
                else
                {
                    OP_ARG_ERROR(2, TF_NUMBER, sp[-1].type);
                    /* NOTREACHED */
                }
                break;

            case LVALUE_UNPROTECTED_RANGE:
                OP_ARG_ERROR(2, TF_NUMBER, T_POINTER);
                break; /* NOTREACHED */

            case LVALUE_UNPROTECTED_MAPENTRY:
                argp = get_map_lvalue(current_unprotected_mapentry.map, &(current_unprotected_mapentry.key)) + current_unprotected_mapentry.index;
                free_svalue(&(current_unprotected_mapentry.key));
                break;
        }

        if(argp == NULL) /* Already handled. */
            break;

        /* Now do it */
        switch (argp->type)
        {
        case T_NUMBER:
            if (sp[-1].type == T_NUMBER)
            {
                if (shift > MAX_SHIFT)
                    argp->u.number = 0;
                else
                    argp->u.number = (p_uint)argp->u.number >> shift;
            }
            else
            {
                OP_ARG_ERROR(2, TF_NUMBER, sp[-1].type);
                /* NOTREACHED */
            }
            break;

        default:
            OP_ARG_ERROR(1, TF_NUMBER, argp->type);
            /* NOTREACHED */
        }

        pop_n_elems(2);
        assign_svalue_no_free(++sp, argp);
        break;
    }

    /* --- Machine internal instructions --- */

    CASE(F_POP_VALUE);              /* --- pop_value           --- */
        /* Pop the topmost value from the stack (freeing it).
         * Simple, huh?
         */
        pop_stack();
        break;

    CASE(F_POP_SECOND);             /* --- pop_second          --- */
        /* Pop the value under the topmost value and put the
         * topmost value there.
         */
        free_svalue(--sp);
        *sp = sp[1];
        break;

    CASE(F_DUP);                    /* --- dup                 --- */
        /* Push a duplicate of sp[0] onto the stack.
         */
        sp++;
        assign_svalue_no_free(sp, sp-1);
        break;

    CASE(F_LDUP);                   /* --- ldup                --- */
      {
        /* Push a duplicate of sp[0] onto the stack.
         * If sp[0] is an lvalue, it is derefenced first.
         */
        sp++;
        normalize_svalue(sp-1, false);
        internal_assign_rvalue_no_free(sp, sp-1);
        break;
      }

    CASE(F_SWAP_VALUES);            /* --- swap_values         --- */
      {
        /* Swap sp[0] and sp[-1] on the stack.
         */
        svalue_t sv = sp[0];
        sp[0] = sp[-1];
        sp[-1] = sv;
        break;
      }

    CASE(F_CLEAR_LOCALS);    /* --- clear_locals <first> <num> --- */
      {
        /* Set the local variables <first> .. <first>+<num>-1 back
         * to svalue-0. This is used to initalize local variables
         * of nested scopes.
         */
        int first, num;
        svalue_t *plocal;

        first = LOAD_UINT8(pc);
        num = LOAD_UINT8(pc);

        for (plocal = fp+first; num > 0; num--, plocal++)
        {
            free_svalue(plocal);
            *plocal = const0;
        }
        break;
      }

    CASE(F_SAVE_ARG_FRAME);         /* --- save_arg_frame      --- */
      {
        /* Save the current value of ap on the stack and set ap to
         * the next stack entry.
         */

        ++sp;
        sp->type = T_INVALID;
        sp->u.lvalue = ap;
        ap = sp+1;
        break;
      }

    CASE(F_RESTORE_ARG_FRAME);      /* --- restore_arg_frame   --- */
      {
        /* While sp points at a function result, restore the value
         * of ap from sp[-1]; then move the result down there.
         */

        ap = sp[-1].u.lvalue;
        sp[-1] = sp[0];
        sp--;
        break;
      }

    CASE(F_USE_ARG_FRAME);          /* --- use_arg_frame       --- */
      {
        /* Used as a prefix (and only as a prefix) to instructions which
         * usually know or take the number of arguments from the bytecode.
         * With this prefix, the instruction uses the difference between
         * sp and ap as the real number of arguments.
         *
         * use_arg_frame is recognized by: simul_efun, efun{0,1,2,3,4,v}.
         */

#ifdef DEBUG
        if (use_ap)
            fatal("Previous use_arg_frame hasn't been consumed.\n");
#endif
        use_ap = MY_TRUE;
        break;
      }

    CASE(F_FLATTEN_XARG);           /* --- flatten_xarg        --- */
      {
        /* Take the value at sp and if it is an array, put
         * the array's contents onto the stack in its place. Other values stay
         * as they are.
         * This code is used in conjunction with save/restore/use_arg_frame
         * to implement flexible varargs.
         */
        sp += expand_argument(sp) - 1;
        break;
      }

    CASE(F_FBRANCH);                /* --- fbranch <offset>    --- */
    {
        /* Jump by (32-Bit) long <offset> bytes.
         * The <offset> is counted from its first byte (TODO: Ugh).
         */

        pc += get_bc_offset(pc);
        break;
    }

    CASE(F_LBRANCH);                /* --- lbranch <offset>    --- */
    {
        /* Jump by (16-Bit) short <offset> bytes.
         * The <offset> is counted from its first byte (TODO: Ugh).
         */

        pc += get_bc_shortoffset(pc);
        break;
    }

    CASE(F_LBRANCH_WHEN_ZERO); /* --- lbranch_when_zero <offset> --- */
    {
        /* Jump by (16-Bit) short <offset> bytes if sp[0] is number 0.
         * The <offset> is counted from its first byte (TODO: Ugh).
         * sp[0] is popped from the stack.
         */

        if (sp->type == T_NUMBER && sp->u.number == 0)
        {
            pc += get_bc_shortoffset(pc);
            sp--;
            break;
        }
        pc += sizeof(bc_shortoffset_t);
        pop_stack();
        break;
    }

    CASE(F_LBRANCH_WHEN_NON_ZERO); /* --- lbranch_when_non_zero <offset> --- */
    {
        /* Jump by (16-Bit) short <offset> bytes if sp[0] is not number 0.
         * The <offset> is counted from its first byte (TODO: Ugh).
         * sp[0] is popped from the stack.
         */

        if (sp->type != T_NUMBER || sp->u.number != 0)
        {
            pc += get_bc_shortoffset(pc);
            pop_stack();
            break;
        }
        pc += sizeof(bc_shortoffset_t);
        sp--;
        break;
    }

    CASE(F_BRANCH);                 /* --- branch <offset>     --- */
    {
        /* Jump forward by uint8 <offset> bytes.
         * The <offset> is counted from the next instruction.
         */

        pc += get_uint8(pc) + sizeof(bytecode_t);
        break;
    }

    CASE(F_BRANCH_WHEN_ZERO); /* --- branch_when_zero <offset> --- */
    {
        /* Jump forward by uint8 <offset> bytes if sp[0] is number 0.
         * The <offset> is counted from the next instruction.
         * sp[0] is popped from the stack.
         */

        if (sp->type == T_NUMBER)
        {
            if (sp->u.number == 0)
            {
                sp--;
                pc += GET_UINT8(pc) + sizeof(bytecode_t);
                break;
            }
            sp--;
            pc += sizeof(uint8_t);
            break;
        }
        else
        {
            free_svalue(sp);
            sp--;
            pc += sizeof(uint8_t);
            break;
        }
    }

    CASE(F_BRANCH_WHEN_NON_ZERO); /* --- branch_when_non_zero <offset> --- */
    {
        /* Jump forward by uint8 <offset> bytes if sp[0] is not number 0.
         * The <offset> is counted from the next instruction.
         * sp[0] is popped from the stack.
         */

        if (sp->type == T_NUMBER)
        {
            if (sp->u.number == 0)
            {
                sp--;
                pc += sizeof(uint8_t);
                break;
            }
        }
        else
        {
            free_svalue(sp);
        }
        sp--;
        pc += GET_UINT8(pc) + sizeof(bytecode_t);
        break;
    }

    CASE(F_BBRANCH_WHEN_ZERO);  /* --- bbranch_when_zero <offset> --- */
    {
        /* Jump backward by uint8 <offset> bytes if sp[0] is number 0.
         * The <offset> is counted from its first byte (TODO: Ugh).
         * sp[0] is popped from the stack.
         */

        if (sp->type == T_NUMBER && sp->u.number == 0)
        {
            sp--;
            pc -= GET_UINT8(pc);
            break;
        }
        pc += sizeof(bytecode_t);
        pop_stack();
        break;
    }
    CASE(F_BBRANCH_WHEN_NON_ZERO); /* --- branch_when_non_zero <offset> --- */
    {
        /* Jump backward by uint8 <offset> bytes if sp[0] is not number 0.
         * The <offset> is counted from its first byte (TODO: Ugh).
         * sp[0] is popped from the stack.
         */

        if (sp->type == T_NUMBER)
        {
            if (sp->u.number == 0)
            {
                pc += sizeof(bytecode_t);
                sp--;
                break;
            }
        }
        else
            free_svalue(sp);
        sp--;
        pc -= GET_UINT8(pc);
        break;
    }

    CASE(F_CALL_FUNCTION)         /* --- call_function <index> --- */
    {
        /* Call the function <index> with the arguments on the stack.
         * <index> is a (16-Bit) unsigned short, giving the index within
         * the programs function table. The number of arguments is determined
         * through the ap pointer.
         *
         * Since the function may be redefined through inheritance, the
         * function must be searched in the current_objects program, which
         * might not be the current_program.
         */

        unsigned short prog_func_index;   /* function index within current program */
        unsigned short obj_func_index;
          /* function index within the current object's program.
           * This way local function may be redefined through inheritance.
           */
        funflag_t  flags;     /* the function flags */
        bytecode_p  funstart;  /* the actual function (code) */
        
        /* Make sure that we are not calling from a set_this_object()
         * context.
         */
        if (is_sto_context())
        {
            ERROR("call_function: Can't execute with "
                  "set_this_object() in effect.\n"
                 );
        }

        /* Get the function's index */
        LOAD_SHORT(prog_func_index, pc);
        obj_func_index = (unsigned short)(prog_func_index + function_index_offset);

        /* Find the function in the function table. As the function may have
         * been redefined by inheritance, we must look in the last table,
         * which is pointed to by current_object.
         */

        if (obj_func_index >= current_object->prog->num_functions)
        {
            fatal("call_function: "
                  "Illegal function index: within object %hu (within program %hu), "
                  "%d functions - current object %s\n"
                 , obj_func_index, prog_func_index
                 , current_object->prog->num_functions
                 , get_txt(current_object->name)
                 );
        }

        /* NOT current_prog, which can be an inherited object. */
        flags = current_object->prog->functions[obj_func_index];

        /* If the function was cross-defined, get the real offset */
        if (flags & NAME_CROSS_DEFINED)
        {
            obj_func_index += CROSSDEF_NAME_OFFSET(flags);
        }

        /* Save all important global stack machine registers */
        push_control_stack(sp, pc, fp, inter_context);

        /* Set the current program back to the objects program _after_
         * the control stack push, since here is where we search for
         * the function.
         */
        current_prog = current_object->prog;

        /* Search for the function definition and determine the offsets.
         */
        csp->num_local_variables = sp - ap + 1;
        flags = setup_new_frame1(obj_func_index, 0, current_prog->num_virtual_variables);
        funstart = current_prog->program + (flags & FUNSTART_MASK);
        csp->funstart = funstart;

        /* Setup the stack, arguments and local vars */
        sp = setup_new_frame2(funstart, sp, MY_FALSE);
        inter_sp = sp;

        // check the argument types
        check_function_args(obj_func_index - function_index_offset, current_prog, funstart);

        /* Finish the setup */

#ifdef DEBUG
        if (!current_object->variables && variable_index_offset)
            fatal("%s Fatal: call function for object %p '%s' w/o variables, "
                  "but offset %d\n"
                 , time_stamp(), current_object, get_txt(current_object->name)
                 , variable_index_offset);
#endif
        current_variables = current_object->variables;
        if (current_variables)
            current_variables += variable_index_offset;
        current_strings = current_prog->strings;
        fp = inter_fp;
        pc = inter_pc;
        csp->extern_call = MY_FALSE;

        break;
    }

                   /* --- call_inherited        <prog> <index> --- */
                   /* --- call_inherited_noargs <prog> <index> --- */
    CASE(F_CALL_INHERITED);
    CASE(F_CALL_INHERITED_NOARGS);
    {
        /* Call the (inherited) function <index> in program <prog> with
         * the arguments on the stack; or for the _noargs code, with no
         * arguments.
         *
         * <index> is a (16-Bit) unsigned short, giving the index within
         * the programs function table.
         * <prog> is a (16-Bit) unsigned short, giving the index within
         * the current programs inherit table.
         *
         * The number of arguments, if needed, is determined through the
         * ap pointer.
         *
         * The _noargs code is used to implement wildcarded
         * super calls, which take no argument, but store their results
         * above the ap. Without this extra bytecode, the normal argument
         * massaging would remove the intermediate results.
         */

        unsigned short prog_index;  /* Index within the inherit table */
        unsigned short func_index;  /* Index within the function table */
        funflag_t flags;            /* the functions flags */
        bytecode_p funstart;        /* the actual function (code) */
        inherit_t *inheritp;        /* the inheritance descriptor */

        /* Make sure that we are not calling from a set_this_object()
         * context.
         */
        if (is_sto_context())
        {
            ERROR("call_inherited: Can't execute with "
                  "set_this_object() in effect.\n"
                 );
        }

        /* Get the program and function index, and determine the
         * inheritance descriptor
         */
        LOAD_SHORT(prog_index, pc);
        LOAD_SHORT(func_index, pc);

#ifdef DEBUG
        inheritp = &current_prog->inherit[prog_index];
        if (func_index >= inheritp->prog->num_functions)
        {
            fatal("call_inherited: Illegal function index: "
                  "program %d, func %d, %d functions\n"
                 , prog_index, func_index, inheritp->prog->num_functions);
        }
#endif

        /* Save all important global stack machine registers */
        push_control_stack(sp, pc, fp, inter_context);

        inheritp = setup_inherited_call(prog_index, &func_index);
        if (!inheritp)
        {
            /* Not found anymore and no error thrown.
             * This can happen for vanished __INIT functions.
             */
            pop_control_stack();
            push_number(sp, 0);
            break;
        }

        /* Search for the function definition and determine the offsets.
         */
        if (instruction != F_CALL_INHERITED_NOARGS)
            csp->num_local_variables = sp - ap + 1;
        else
            csp->num_local_variables = 0;
        flags = setup_new_frame1(
          func_index,
          function_index_offset + inheritp->function_index_offset,
          variable_index_offset + inheritp->variable_index_offset
        );
        funstart = current_prog->program + (flags & FUNSTART_MASK);
        csp->funstart = funstart;

        /* Setup the stack, arguments and local vars */
        sp = setup_new_frame2(funstart, sp, MY_FALSE);
        inter_sp = sp;

        // check the arguments
        check_function_args(current_prog->function_headers[FUNCTION_HEADER_INDEX(funstart)].offset.fx, current_prog, funstart);

        /* Finish the setup */
        fp = inter_fp;
        pc = inter_pc;
        current_variables = current_object->variables;
        if (current_variables)
            current_variables += variable_index_offset;
        current_strings = current_prog->strings;
        csp->extern_call = MY_FALSE;
        break;
    }

    CASE(F_CALL_CLOSURE); /* --- call_closure --- */
        /* Call the closure on the stack with the arguments on the stack.
         * Just like funcall(), but as an internal call.
         * We leave the closure an the stack for a following F_POP_SECOND
         * to clear it up. This instruction is only used by lambda closures.
         */
    {
        num_arg = sp - ap;

        if (ap->type == T_CLOSURE)
        {
            inter_sp = sp;

            /* No external calls may be done when this object is
             * destructed.
             */
            if (current_object->flags & O_DESTRUCTED)
            {
                sp = _pop_n_elems(num_arg, sp);
                push_number(sp, 0);
                inter_sp = sp;
                warnf("Call from destructed object '%s' ignored.\n"
                     , get_txt(current_object->name));
                return sp;
            }

            /* Call the closure and push the result.
             * Note that the closure might destruct itself.
             */
            inter_pc = pc;

            int_call_lambda(ap, num_arg, MY_FALSE);

            pc = inter_pc;
            sp = inter_sp;
            fp = inter_fp;
        }
        else
        {
            /* Not a closure: pop the excess args and return <cl>
             * as result.
             */

            sp = _pop_n_elems(num_arg, sp);
            push_number(sp, 0);
        }

        break;
    }

    CASE(F_CONTEXT_IDENTIFIER);  /* --- context_identifier <var_ix> --- */
        /* Push value of context variable <var_ix>.
         * It is possible that it is a variable that points to
         * a destructed object. In that case, it has to be replaced by 0.
         *
         * <var_ix> is a uint8.
         */

        if (inter_context == NULL)
            errorf("(eval_instruction) context_identifier: "
                  "inter_context is NULL\n");
            /* May happen if somebody does a funcall(symbol_function())
             * on the lfun of an context closure.
             */

        sp++;
        assign_rvalue_no_free(sp, inter_context+LOAD_UINT8(pc));
        break;

                               /* --- context_identifier16 <var_ix> --- */
    CASE(F_CONTEXT_IDENTIFIER16);
      {
        /* Push value of context variable <var_ix>.
         * It is possible that it is a variable that points to
         * a destructed object. In that case, it has to be replaced by 0.
         *
         * <var_ix> is a (16-Bit) unsigned short.
         */
        unsigned short var_index;


        if (inter_context == NULL)
            errorf("(eval_instruction) context_identifier16: "
                  "inter_context is NULL\n");
            /* May happen if somebody does a funcall(symbol_function())
             * on the lfun of an context closure.
             */

        LOAD_SHORT(var_index, pc);
        sp++;
        assign_rvalue_no_free(sp, inter_context+var_index);
        break;
     }

    CASE(F_PUSH_CONTEXT_LVALUE);   /* --- push_context_lvalue <num> --- */
        /* Push an lvalue onto the stack pointing to context variable <num>.
         *
         * <num> is an uint8.
         */

        if (inter_context == NULL)
            errorf("(eval_instruction) context_identifier: "
                  "inter_context is NULL\n");
            /* May happen if somebody does a funcall(symbol_function())
             * on the lfun of an context closure.
             */

        sp++;
        assign_lvalue_no_free(sp, inter_context + LOAD_UINT8(pc));
        break;

                                 /* --- push_context16_lvalue <num> --- */
    CASE(F_PUSH_CONTEXT16_LVALUE);
      {
        /* Push an lvalue onto the stack pointing to context variable <num>.
         *
         * <num> is an (16-Bit) unsigned short.
         */
        unsigned short var_index;


        if (inter_context == NULL)
            errorf("(eval_instruction) context_identifier: "
                  "inter_context is NULL\n");
            /* May happen if somebody does a funcall(symbol_function())
             * on the lfun of an context closure.
             */

        LOAD_SHORT(var_index, pc);
        sp++;
        assign_lvalue_no_free(sp, inter_context + var_index);
        break;
      }

    CASE(F_PUSH_IDENTIFIER_LVALUE);  /* --- push_identifier_lvalue <num> --- */
        /* Push an lvalue onto the stack pointing to object-global variable
         * <num>.
         *
         * <num> is an uint8 and used as index in the current objects
         * variable table.
         */
        sp++;
        assign_lvalue_no_free(sp, find_value((int)(LOAD_UINT8(pc) )));
        break;

    CASE(F_VIRTUAL_VARIABLE);         /* --- virtual_variable <num> --- */
        /* Push the virtual object-global variable <num> onto the stack.
         * It is possible that it is a variable that points to
         * a destructed object. In that case, it has to be replaced by 0.
         *
         * <num> is an uint8 and used as index in the current objects
         * variable table.
         */
        sp++;
        assign_rvalue_no_free(sp, find_virtual_value((int)(LOAD_UINT8(pc))));
        break;

                          /* --- push_virtual_variable_lvalue <num> --- */
    CASE(F_PUSH_VIRTUAL_VARIABLE_LVALUE);
        /* Push an lvalue onto the stack pointing to virtual object-global
         * variable <num>.
         *
         * <num> is an uint8 and used as index in the current objects
         * variable table.
         */
        sp++;
        assign_lvalue_no_free(sp, find_virtual_value((int)(LOAD_UINT8(pc) )));
        break;

    CASE(F_IDENTIFIER16);         /* --- identifier16 <var_ix> --- */
    {
        /* Push value of object variable <var_ix>.
         * It is possible that it is a variable that points to
         * a destructed object. In that case, it has to be replaced by 0.
         *
         * <var_ix> is a (16-Bit) unsigned short.
         */
        unsigned short var_index;

        LOAD_SHORT(var_index, pc);
        sp++;
        assign_rvalue_no_free(sp, find_value((int)var_index));
        break;
    }

                       /* --- push_identifier16_lvalue <var_ix> --- */
    CASE(F_PUSH_IDENTIFIER16_LVALUE);
    {
        /* Push an lvalue onto the stack pointing to object-global variable
         * <num>.
         *
         * <num> is an uint8 and used as index in the current objects
         * variable table.
         */
        unsigned short var_index;

        LOAD_SHORT(var_index, pc);
        sp++;
        assign_lvalue_no_free(sp, find_value((int)var_index));
        break;
    }
                         /* --- push_local_variable_lvalue <num> --- */
    CASE(F_PUSH_LOCAL_VARIABLE_LVALUE);
        /* Push an lvalue onto the stack pointing to local variable <num>.
         *
         * <num> is an uint8 and used as index onto the framepointer.
         */
        sp++;
        assign_lvalue_no_free(sp, fp + LOAD_UINT8(pc));
        break;

    CASE(F_S_INDEX_LVALUE);         /* --- s_index_lvalue     --- */
    CASE(F_SX_INDEX_LVALUE);        /* --- sx_index_lvalue    --- */
        /* Op. (struct v=sp[-2], mixed i=sp[-1], short idx=sp[0])
         *
         * Compute the index &(v[i]) of rvalue <v> and push it into the
         * stack as an lvalue.
         *
         * <idx> gives the index of the expected struct type - the
         * operator accepts a struct of this type, or any of its children.
         * An negative <idx> accepts any struct.
         *
         * F_SX_INDEX_LVALUE is the relaxed variant that will not throw
         * an error if the member is not found. Instead an lvalue to
         * a temporary will be put on the stack.
         */

        sp = check_struct_op(sp, pc, NULL);
        sp = push_index_lvalue(sp, pc, REGULAR_INDEX, false, instruction == F_SX_INDEX_LVALUE);
        break;

    CASE(F_MAP_INDEX_LVALUE);       /* --- map_index_lvalue   --- */
        /* Operator F_MAP_INDEX_LVALUE( mapping m=sp[-2]
         *                            , mixed i=sp[-1], int j=sp[0])
         *
         * Compute the lvalue &(m[i,j]) and push it into the stack. If v has
         * just one ref left, the indexed item is stored in indexing_quickfix
         * and the lvalue refers to that variable.
         */
        sp = push_map_index_lvalue(sp, pc, false);
        break;

    CASE(F_INDEX_LVALUE);           /* --- index_lvalue       --- */
        /* Operator F_INDEX_LVALUE (string|vector &v=sp[-1], int   i=sp[0])
         *          F_INDEX_LVALUE (mapping       &v=sp[-1], mixed i=sp[0])
         *
         * Compute the index &(v[i]) of lvalue <v> and push it into the stack.
         * The computed index is a lvalue itself.  If <v> is a string-lvalue,
         * it is made a malloced string if necessary, and the pushed result
         * will be a lvalue pointing to a CHAR_LVALUE stored in
         * <special_lvalue>.
         */

        {
            svalue_t * svp = get_rvalue_no_collapse(sp-1, NULL);
            if (svp && svp->type == T_STRUCT)
            {
                ERRORF(("Illegal type to []: %s lvalue, "
                        "expected string/mapping/vector lvalue.\n"
                       , typename(svp->type)
                      ));
                /* NOTREACHED */
            }
        }
        sp = push_index_lvalue(sp, pc, REGULAR_INDEX, false, false);
        break;

    CASE(F_RINDEX_LVALUE);          /* --- rindex_lvalue      --- */
        /* Operator F_RINDEX_LVALUE (string|vector &v=sp[-1], int   i=sp[0])
         *
         * Compute the index &(v[<i]) of lvalue <v> and push it into the
         * stack. The computed index is a lvalue itself.
         * If <v> is a string-lvalue, it is made a malloced string if
         * necessary, and the pushed result will be a lvalue pointing to a
         * CHAR_LVALUE stored in <special_lvalue>.
         */

        sp = push_index_lvalue(sp, pc, REVERSE_INDEX, false, false);
        break;

    CASE(F_AINDEX_LVALUE);          /* --- aindex_lvalue      --- */
        /* Operator F_AINDEX_LVALUE (string|vector &v=sp[-1], int   i=sp[0])
         *
         * Compute the index &(v[>i]) of lvalue <v> and push it into the
         * stack. The computed index is a lvalue itself.
         * If <v> is a string-lvalue, it is made a malloced string if
         * necessary, and the pushed result will be a lvalue pointing to a
         * CHAR_LVALUE stored in <special_lvalue>.
         */

        sp = push_index_lvalue(sp, pc, ARITHMETIC_INDEX, false, false);
        break;

    CASE(F_S_INDEX);                /* --- s_index            --- */
    CASE(F_SX_INDEX);               /* --- sx_index           --- */
    {
        /* Operator F_S_INDEX (struct v=sp[-2], mixed i=sp[-1], short idx=sp[0])
         * Operator F_SX_INDEX (struct v=sp[-2], mixed i=sp[-1], short idx=sp[0])
         *
         * Compute the value (v->i) and push it onto the stack.  If the value
         * would be a destructed object, 0 is pushed onto the stack and the
         * ref to the object is removed from the struct.
         *
         * <idx> gives the index of the expected struct type - the
         * operator accepts a struct of this type, or any of its children.
         * An negative <idx> accepts any struct.
         *
         * F_SX_INDEX is similar to F_S_INDEX, but it will not throw an
         * error if the member doesn't exist.
         */

        bool ignore_error = false;
        sp = check_struct_op(sp, pc, &ignore_error);

        if (instruction == F_SX_INDEX)
            ignore_error = true;

        sp = push_index_value(sp, pc, ignore_error, REGULAR_INDEX);
        break;
    }

    CASE(F_INDEX);                  /* --- index              --- */
        /* Operator F_INDEX (string|vector v=sp[-1], int   i=sp[0])
         *          F_INDEX (mapping       v=sp[-1], mixed i=sp[0])
         *
         * Compute the value (v[i]) and push it onto the stack.  If the value
         * would be a destructed object, 0 is pushed onto the stack and the
         * ref to the object is removed from the vector/mapping.
         *
         * Mapping indices may use <indexing_quickfix> for temporary storage.
         */

        if ((sp-1)->type == T_STRUCT)
        {
            ERRORF(("Illegal type to []: %s, expected string/vector/mapping.\n"
                   , typename((sp-1)->type)
                  ));
            /* NOTREACHED */
        }
        sp = push_index_value(sp, pc, false, REGULAR_INDEX);
        break;

    CASE(F_RINDEX);                 /* --- rindex              --- */
        /* Operator F_RINDEX (string|vector v=sp[-1], int   i=sp[0])
         *
         * Compute the value (v[<i]) and push it onto the stack.  If the value
         * would be a destructed object, 0 is pushed onto the stack and the
         * ref to the object is removed from the vector/mapping.
         */

        sp = push_index_value(sp, pc, false, REVERSE_INDEX);
        break;

    CASE(F_AINDEX);                 /* --- aindex              --- */
        /* Operator F_AINDEX (string|vector v=sp[-1], int   i=sp[0])
         *
         * Compute the value (v[>i]) and push it onto the stack.  If the value
         * would be a destructed object, 0 is pushed onto the stack and the
         * ref to the object is removed from the vector/mapping.
         */

        sp = push_index_value(sp, pc, false, ARITHMETIC_INDEX);
        break;

    CASE(F_RANGE_LVALUE);           /* --- range_lvalue        --- */
        /* Operator F_RANGE_LVALUE (string|vector &v=sp[-2]
         *                         , int i1=sp[-1], i2=sp[0])
         *
         * Compute the range &(v[i1..i2]) of lvalue <v> and push it into the
         * stack.  The value pushed is a lvalue pointing to <special_lvalue>.
         * <special_lvalue> then is the POINTER_RANGE_- resp.
         * STRING_RANGE_LVALUE.
         *
         * TODO: Four different instructions for this? A single instruction plus
         * TODO:: argument would be as well.
         */

        sp = push_range_lvalue(NN_RANGE, sp, pc);
        break;

    CASE(F_NR_RANGE_LVALUE);           /* --- nr_range_lvalue     --- */
        /* Operator F_NR_RANGE_LVALUE (string|vector &v=sp[-2]
         *                         , int i1=sp[-1], i2=sp[0])
         *
         * Compute the range &(v[i1..<i2]) of lvalue <v> and push it into the
         * stack.  The value pushed is a lvalue pointing to <special_lvalue>.
         * <special_lvalue> then is the POINTER_RANGE_- resp.
         * STRING_RANGE_LVALUE.
         */

        sp = push_range_lvalue(NR_RANGE, sp, pc);
        break;

    CASE(F_RN_RANGE_LVALUE);           /* --- rn_range_lvalue     --- */
        /* Operator F_RN_RANGE_LVALUE (string|vector &v=sp[-2]
         *                         , int i1=sp[-1], i2=sp[0])
         *
         * Compute the range &(v[<i1..i2]) of lvalue <v> and push it into the
         * stack.  The value pushed is a lvalue pointing to <special_lvalue>.
         * <special_lvalue> then is the POINTER_RANGE_- resp.
         * STRING_RANGE_LVALUE.
         */

        sp = push_range_lvalue(RN_RANGE, sp, pc);
        break;

    CASE(F_RR_RANGE_LVALUE);           /* --- rr_range_lvalue     --- */
        /* Operator F_RR_RANGE_LVALUE (string|vector &v=sp[-2]
         *                         , int i1=sp[-1], i2=sp[0])
         *
         * Compute the range &(v[<i1..<i2]) of lvalue <v> and push it into the
         * stack.  The value pushed is a lvalue pointing to <special_lvalue>.
         * <special_lvalue> then is the POINTER_RANGE_- resp.
         * STRING_RANGE_LVALUE.
         */

        sp = push_range_lvalue(RR_RANGE, sp, pc);
        break;

    CASE(F_NA_RANGE_LVALUE);           /* --- na_range_lvalue     --- */
        /* Operator F_NA_RANGE_LVALUE (string|vector &v=sp[-2]
         *                         , int i1=sp[-1], i2=sp[0])
         *
         * Compute the range &(v[i1..>i2]) of lvalue <v> and push it into the
         * stack.  The value pushed is a lvalue pointing to <special_lvalue>.
         * <special_lvalue> then is the POINTER_RANGE_- resp.
         * STRING_RANGE_LVALUE.
         */

        sp = push_range_lvalue(NA_RANGE, sp, pc);
        break;

    CASE(F_AN_RANGE_LVALUE);           /* --- an_range_lvalue     --- */
        /* Operator F_AN_RANGE_LVALUE (string|vector &v=sp[-2]
         *                         , int i1=sp[-1], i2=sp[0])
         *
         * Compute the range &(v[>i1..i2]) of lvalue <v> and push it into the
         * stack.  The value pushed is a lvalue pointing to <special_lvalue>.
         * <special_lvalue> then is the POINTER_RANGE_- resp.
         * STRING_RANGE_LVALUE.
         */

        sp = push_range_lvalue(AN_RANGE, sp, pc);
        break;

    CASE(F_RA_RANGE_LVALUE);           /* --- ra_range_lvalue     --- */
        /* Operator F_RA_RANGE_LVALUE (string|vector &v=sp[-2]
         *                         , int i1=sp[-1], i2=sp[0])
         *
         * Compute the range &(v[<i1..>i2]) of lvalue <v> and push it into the
         * stack.  The value pushed is a lvalue pointing to <special_lvalue>.
         * <special_lvalue> then is the POINTER_RANGE_- resp.
         * STRING_RANGE_LVALUE.
         */

        sp = push_range_lvalue(RA_RANGE, sp, pc);
        break;

    CASE(F_AR_RANGE_LVALUE);           /* --- ar_range_lvalue     --- */
        /* Operator F_AR_RANGE_LVALUE (string|vector &v=sp[-2]
         *                         , int i1=sp[-1], i2=sp[0])
         *
         * Compute the range &(v[>i1..<i2]) of lvalue <v> and push it into the
         * stack.  The value pushed is a lvalue pointing to <special_lvalue>.
         * <special_lvalue> then is the POINTER_RANGE_- resp.
         * STRING_RANGE_LVALUE.
         */

        sp = push_range_lvalue(AR_RANGE, sp, pc);
        break;

    CASE(F_AA_RANGE_LVALUE);           /* --- aa_range_lvalue     --- */
        /* Operator F_AA_RANGE_LVALUE (string|vector &v=sp[-2]
         *                         , int i1=sp[-1], i2=sp[0])
         *
         * Compute the range &(v[>i1..>i2]) of lvalue <v> and push it into the
         * stack.  The value pushed is a lvalue pointing to <special_lvalue>.
         * <special_lvalue> then is the POINTER_RANGE_- resp.
         * STRING_RANGE_LVALUE.
         */

        sp = push_range_lvalue(AA_RANGE, sp, pc);
        break;

    CASE(F_NX_RANGE_LVALUE);           /* --- nx_range_lvalue     --- */
        /* Operator F_NX_RANGE_LVALUE (string|vector &v=sp[-1]
         *                            , int i1=sp[0])
         *
         * Compute the range &(v[i1..]) of lvalue <v> and push it into the
         * stack.  The value pushed is a lvalue pointing to <special_lvalue>.
         * <special_lvalue> then is the POINTER_RANGE_- resp.
         * STRING_RANGE_LVALUE.
         *
         * We implement this by pushing '1' onto the stack and then
         * call F_NR_RANGE_LVALUE, effectively computing &(v[i1..<1]).
         */

        push_number(sp, 1);  /* 'Push' the 1 for the upper bound */
        sp = push_range_lvalue(NR_RANGE, sp, pc);
        break;

    CASE(F_RX_RANGE_LVALUE);           /* --- rx_range_lvalue     --- */
        /* Operator F_RX_RANGE_LVALUE (string|vector &v=sp[-1]
         *                            , int i1=sp[0])
         *
         * Compute the range &(v[<i1..]) of lvalue <v> and push it into the
         * stack.  The value pushed is a lvalue pointing to <special_lvalue>.
         * <special_lvalue> then is the POINTER_RANGE_- resp.
         * STRING_RANGE_LVALUE.
         *
         * We implement this by pushing '1' onto the stack and then
         * call F_RR_RANGE_LVALUE, effectively computing &(v[<i1..<1]).
         */

        push_number(sp, 1);  /* 'Push' the 1 for the upper bound */
        sp = push_range_lvalue(RR_RANGE, sp, pc);
        break;

    CASE(F_AX_RANGE_LVALUE);           /* --- ax_range_lvalue     --- */
        /* Operator F_AX_RANGE_LVALUE (string|vector &v=sp[-1]
         *                            , int i1=sp[0])
         *
         * Compute the range &(v[>i1..]) of lvalue <v> and push it into the
         * stack.  The value pushed is a lvalue pointing to <special_lvalue>.
         * <special_lvalue> then is the POINTER_RANGE_- resp.
         * STRING_RANGE_LVALUE.
         *
         * We implement this by pushing '1' onto the stack and then
         * call F_AR_RANGE_LVALUE, effectively computing &(v[>i1..<1]).
         */

        push_number(sp, 1);  /* 'Push' the 1 for the upper bound */
        sp = push_range_lvalue(AR_RANGE, sp, pc);
        break;

    CASE(F_MAKE_PROTECTED);
        /* Operator &(ref=sp[0])
         *
         * Convert the lvalue on the stack into a protected lvalue.
         * If <v> happens the be a protected lvalue already, then
         * do nothing. (Can happen when creating a reference to
         * a variable that already is a protected lvalue.)
         */

        inter_pc = pc;
        sp = push_protected_lvalue(sp);

        /* Now we don't need the quickfix anymore. */
        free_svalue(&indexing_quickfix);
        break;

    CASE(F_MAKE_RVALUE);
    {
        /* Operator val=sp[0]
         *
         * If there is a protected lvalue on the stack,
         * put its value on the stack instead. If it is not
         * a protected lvalue, leave it. (Unprotected
         * lvalues are forbidden here.)
         */
        svalue_t val = sp[0];

        inter_pc = pc;
        transfer_rvalue_no_free(sp, &val);
        break;
    }

                          /* --- push_virtual_variable_vlvalue <num> --- */
    CASE(F_PUSH_VIRTUAL_VARIABLE_VLVALUE);
        /* Push a var-lvalue onto the stack pointing to virtual object-global
         * variable <num> (ignoring any existing lvalues in the variable).
         *
         * <num> is an uint8 and used as index in the current objects
         * variable table.
         */
        sp++;
        assign_var_lvalue_no_free(sp, find_virtual_value((int)(LOAD_UINT8(pc) )));
        break;

    CASE(F_PUSH_IDENTIFIER_VLVALUE);  /* --- push_identifier_vlvalue <num> --- */
        /* Push a variable-lvalue onto the stack pointing to object-global
         * variable <num> (ignoring any existing lvalues in the variable).
         *
         * <num> is an uint8 and used as index in the current objects
         * variable table.
         */
        sp++;
        assign_var_lvalue_no_free(sp, find_value((int)(LOAD_UINT8(pc) )));
        break;

                       /* --- push_identifier16_vlvalue <var_ix> --- */
    CASE(F_PUSH_IDENTIFIER16_VLVALUE);
    {
        /* Push a variable-lvalue onto the stack pointing to object-global
         * variable <num> (ignoring any existing lvalues in the variable).
         *
         * <num> is an uint8 and used as index in the current objects
         * variable table.
         */
        unsigned short var_index;

        LOAD_SHORT(var_index, pc);
        sp++;
        assign_var_lvalue_no_free(sp, find_value((int)var_index));
        break;
    }

    CASE(F_PUSH_CONTEXT_VLVALUE);   /* --- push_context_vlvalue <num> --- */
        /* Push a variable-lvalue onto the stack pointing to context
         * variable <num> (ignoring any existing lvalues in the variable).
         *
         * <num> is an uint8.
         */

        if (inter_context == NULL)
            errorf("(eval_instruction) context_identifier: "
                  "inter_context is NULL\n");
            /* May happen if somebody does a funcall(symbol_function())
             * on the lfun of an context closure.
             */

        sp++;
        assign_var_lvalue_no_free(sp, inter_context + LOAD_UINT8(pc));
        break;

                         /* --- push_local_variable_vlvalue <num> --- */
    CASE(F_PUSH_LOCAL_VARIABLE_VLVALUE);
        /* Push a variable-lvalue onto the stack pointing to local
         * variable <num> (ignoring any existing lvalues in the variable).
         *
         * <num> is an uint8 and used as index onto the framepointer.
         */
        sp++;
        assign_var_lvalue_no_free(sp, fp + LOAD_UINT8(pc));
        break;

    CASE(F_INDEX_VLVALUE);          /* --- index_vlvalue       --- */
        /* Operator F_INDEX (vector  v=sp[-1], int   i=sp[0])
         *          F_INDEX (mapping v=sp[-1], mixed i=sp[0])
         *
         * Compute the value (v[i]) and push it onto the stack.
         * This'll create an unprotected lvalue directly to the entry,
         * so the entry can be changed without respecting any existing
         * lvalue references in that entry.
         */

        if ((sp-1)->type == T_STRUCT)
        {
            ERRORF(("Illegal type to []: %s, expected string/vector/mapping.\n"
                   , typename((sp-1)->type)
                  ));
            /* NOTREACHED */
        }
        sp = push_index_lvalue(sp, pc, REGULAR_INDEX, true, false);
        break;

    CASE(F_RINDEX_VLVALUE);         /* --- rindex_vlvalue      --- */
        /* Operator F_RINDEX_VLVALUE (string|vector &v=sp[-1], int   i=sp[0])
         *
         * Compute the index &(v[<i]) of lvalue <v> and push it onto the stack.
         * This'll create an unprotected lvalue directly to the entry,
         * so the entry can be changed without respecting any existing
         * lvalue references in that entry.
         */

        sp = push_index_lvalue(sp, pc, REVERSE_INDEX, true, false);
        break;

    CASE(F_AINDEX_VLVALUE);         /* --- aindex_vlvalue      --- */
        /* Operator F_AINDEX_VLVALUE (string|vector &v=sp[-1], int   i=sp[0])
         *
         * Compute the index &(v[>i]) of lvalue <v> and push it onto the stack.
         * This'll create an unprotected lvalue directly to the entry,
         * so the entry can be changed without respecting any existing
         * stack. The computed index is a lvalue itself.
         */

        sp = push_index_lvalue(sp, pc, ARITHMETIC_INDEX, true, false);
        break;

    CASE(F_S_INDEX_VLVALUE);        /* --- s_index_vlvalue     --- */
    CASE(F_SX_INDEX_VLVALUE);       /* --- sx_index_vlvalue    --- */
        /* Op. (struct v=sp[-2], mixed i=sp[-1], short idx=sp[0])
         *
         * Compute the index &(v[i]) of rvalue <v> and push it into the
         * stack as an lvalue.
         *
         * <idx> gives the index of the expected struct type - the
         * operator accepts a struct of this type, or any of its children.
         * An negative <idx> accepts any struct.
         *
         * This'll create an unprotected lvalue directly to the entry,
         * so the entry can be changed without respecting any existing
         * stack. The computed index is a lvalue itself.
         *
         * F_SX_INDEX_VLVALUE is the relaxed variant that will create
         * an lvalue to a tempoary if the struct member is not found.
         */

        sp = check_struct_op(sp, pc, NULL);
        sp = push_index_lvalue(sp, pc, REGULAR_INDEX, true, instruction == F_SX_INDEX_VLVALUE);
        break;

    CASE(F_MAP_INDEX_VLVALUE);      /* --- map_index_vlvalue   --- */
        /* Operator F_MAP_INDEX_VLVALUE( mapping m=sp[-2]
         *                             , mixed i=sp[-1], int j=sp[0])
         *
         * Compute the lvalue &(m[i,j]) and push it into the stack. If v has
         * just one ref left, the indexed item is stored in indexing_quickfix
         * and the lvalue refers to that variable.
         *
         * This'll create an unprotected lvalue directly to the entry,
         * so the entry can be changed without respecting any existing
         * stack. The computed index is a lvalue itself.
         */
        sp = push_map_index_lvalue(sp, pc, true);
        break;


    CASE(F_SIMUL_EFUN);             /* --- simul_efun <code>   --- */
    {
        /* Call the simul_efun <code> with the arguments on the stack.
         * If the simul_efun takes a variable number of arguments, or
         * if use_ap is TRUE, then the number of arguments is determined
         * through the ap pointer; otherwise the code assumes that the
         * compiler left the proper number of arguments on the stack.
         *
         * <code> is an ushort and indexes the function list *simul_efunp.
         */

        unsigned short      code;      /* the function index */
        bytecode_p          funstart;  /* the actual function */
        object_t           *ob;        /* the simul_efun object */
        int                 def_narg;  /* expected number of arguments */
        simul_efun_table_t *entry;

        assign_eval_cost_inl();  /* we're changing objects */

        /* Get the sefun code and the number of arguments on the stack */
        LOAD_SHORT(code, pc);
        def_narg = simul_efunp[code].num_arg;

        if (use_ap
         || (simul_efunp[code].flags & (TYPE_MOD_VARARGS|TYPE_MOD_XVARARGS))
         || simul_efunp[code].num_opt_arg
           )
        {
            use_ap = MY_FALSE;  /* Reset the flag */
            num_arg = sp - ap + 1;
        }
        else
            num_arg = def_narg;

        /* Correct the number of arguments on the stack */
        if (num_arg != def_narg && !(simul_efunp[code].flags & TYPE_MOD_VARARGS))
        {
            /* If it's an XVARARGS, we don't require the last argument. */
            if (simul_efunp[code].flags & TYPE_MOD_XVARARGS)
                def_narg--;

            /* Add eventually missing arguments */
            while (num_arg < def_narg - simul_efunp[code].num_opt_arg)
            {
                sp++;
                put_number(sp, 0);
                num_arg++;
            }

            /* Remove extraneous arguments */
            if (!(simul_efunp[code].flags & TYPE_MOD_XVARARGS))
            {
                while (num_arg > def_narg)
                {
                    free_svalue(sp--);
                    num_arg--;
                }
            }
        }

        /* No external calls may be done when this object is destructed.
         */
        if (current_object->flags & O_DESTRUCTED)
        {
            pop_n_elems(num_arg);
            push_number(sp, 0);
            WARNF(("Call from destructed object '%s' ignored.\n"
                  , get_txt(current_object->name)));
            break;
        }

        /* Make sure the simul_efun object exists; loading it when
         * necessary.
         */
        if ( !(ob = simul_efun_object) )
        {
            inter_sp = sp;
            inter_pc = pc;
            if (!assert_simul_efun_object()
             || !(ob = simul_efun_object)
               )
            {
                errorf("Couldn't load simul_efun object.\n");
            }
        }

        /* Get the function code information */
        entry = &simul_efun_table[code];

        if ( NULL != (funstart = entry->funstart) )
        {
            /* The entry is valid: call the sefun by recursing into
             * eval_instruction(), so we can get the result from the
             * stack.
             * We recurse because some simul_efuns are called with
             * F_CALL_DIRECT, and the functions should not be able
             * to see any difference.
             */
            program_t *prog;
            svalue_t *new_sp;
            push_control_stack(sp, pc, fp, inter_context);
            csp->ob = current_object;
            csp->prev_ob = previous_ob;
            csp->funstart = funstart;
            csp->num_local_variables = num_arg;
            current_prog = prog = entry->program;
            function_index_offset = entry->function_index_offset;
#ifdef DEBUG
            if (!ob->variables && entry->variable_index_offset)
                fatal("%s Fatal: call sefun for object %p '%s' w/o variables, "
                      "but offset %"PRIdPINT"\n"
                     , time_stamp(), ob, get_txt(ob->name)
                     , (entry->variable_index_offset));
#endif
            current_variables = ob->variables;
            if (current_variables)
                current_variables += entry->variable_index_offset;
            new_sp = setup_new_frame2(funstart, sp, MY_FALSE);
            inter_sp = new_sp;
            check_function_args(prog->function_headers[FUNCTION_HEADER_INDEX(funstart)].offset.fx, current_prog, funstart);

            /* The simul_efun object should not use simul_efuns itself... */
            previous_ob = current_object;
            current_object = ob;
            current_strings = prog->strings;
            eval_instruction(inter_pc, new_sp);
            sp -= num_arg - 1;
            /*
             * The result of the function call is on the stack.
             */
            break;
        }

        /* At this point the simul_efun was discarded meanwhile and
         * not recreated.
         * Call the function the old fashioned way with apply() in case it
         * exists in a slightly different form.
         */
        inter_sp = sp;
        inter_pc = pc;
        call_simul_efun(code, ob, num_arg);
        sp = inter_sp;
        /*
         * The result of the function call is on the stack.
         */
        break;
    }

#ifdef USE_PYTHON
    CASE(F_PYTHON_EFUN);             /* --- python_efun <code>   --- */
    {
        /* Call the python-defined efun <code> with the arguments on the stack.
         * The number of arguments is determined through the ap pointer.
         *
         * <code> is an ushort and indexes the python_efun_table.
         */

        unsigned short      code;      /* the function index */

        LOAD_SHORT(code, pc);
        num_arg = sp - ap + 1;
        use_ap = MY_FALSE;

        inter_sp = sp;
        inter_pc = pc;

        call_python_efun(code, num_arg);
        sp = inter_sp;
        /*
         * The result of the function call is on the stack.
         */
        break;
    }
#endif /* USE_PYTHON */

    CASE(F_AGGREGATE);              /* --- aggregate <size>    --- */
    {
        /* Create an array ({ sp[<-size>+1], ..., sp[0] }), remove the
         * single values from the stack and leave the array as result.
         *
         * <size> is a (16-Bit) unsigned short.
         *
         * TODO: It is tempting to introduce flat 'literal arrays',
         * TODO:: which can be copied quickly and just need a few
         * TODO:: slots to filled in, if any.
         */

        int i;
        vector_t *v;
        unsigned short num;
        svalue_t *value, *item;

        /* Get the size */
        LOAD_SHORT(num, pc);

        /* Allocate the array */
        i = num;
        v = allocate_uninit_array(i);

        /* Set sp and value to the first single value on the stack */
        sp = value = sp - i + 1;

        /* Move the single values into the array.
         * Volatile strings are made shared during this.
         */
        item = v->item;
        while (--i >= 0)
            transfer_svalue_no_free(item++, value++);

        /* Leave the array on the stack (ref count is already ok) */
        put_array(sp, v);
        break;
    }

    CASE(F_M_AGGREGATE);     /* --- m_aggregate <size> <width> --- */
    CASE(F_M_CAGGREGATE);   /* --- m_caggregate <size> <width> --- */
    {
        /* Create a mapping from the <size>*<width> single values on the
         * stack, remove the single values and leave the mapping as result.
         * Starting at the lowest entry (sp[-(<size>*<width>)]), the values
         * are laid out in <key>:<data 1>...<data <width>> order.
         * Keys may appear several times.
         *
         * m_aggregate: <size> and <width> are (16-Bit) unsigned shorts.
         * m_caggregate: <size> and <width> are uint8.
         *
         * TODO: It is tempting to introduce flat 'literal mappings',
         * TODO:: which can be copied quickly and just need a few
         * TODO:: slots to filled in, if any.
         */
        int i, j;
        mapping_t *m;
        svalue_t *data;
        int num_values;
        svalue_t *value;

        /* Get the size and width from the code.
         */
        if (instruction == F_M_CAGGREGATE)
        {
            i = LOAD_UINT8(pc);
            num_values = LOAD_UINT8(pc);
        }
        else
        {
            unsigned short num[2];

            LOAD_SHORT(num[0], pc);
            LOAD_SHORT(num[1], pc);
            i = num[0];
            num_values = num[1];
        }

        if (max_mapping_size && (p_uint)i * (1+num_values) > (p_uint)max_mapping_size)
            ERRORF(("Illegal mapping size: %"PRIuPINT" elements (%u x %u)\n"
                   , ((p_uint)i * (1+num_values)), i, num_values));
        if (max_mapping_keys && (p_uint)i > (p_uint)max_mapping_keys)
            ERRORF(("Illegal mapping size: %u entries\n", i));

        /* Get the mapping */
        m = allocate_mapping(i, num_values);
        if (!m)
            ERROR("Out of memory\n");

        /* Set sp and value to the first single value on the stack.
         */
        sp = value = sp - (i * (num_values+1)) + 1;
        while (--i >= 0)
        {
            /* Create/reget the mapping entry */
            data = get_map_lvalue_unchecked(m, value);
            if (!data)
            {
                outofmemory("literal mapping");
                /* NOTREACHED */
                return MY_FALSE;
            }
            free_svalue(value++);
            for (j = num_values; --j >= 0;)
            {
                /* Copy over the entry data */
                if (data->type != T_NUMBER)
                    free_svalue(data);
                transfer_svalue_no_free(data++, value++);
            }
        }

        /* Put the mapping onto the stack */
        put_mapping(sp, m);
        break;
    }

    CASE(F_S_AGGREGATE);
                        /* --- s_aggregate <idx> <num> --- */
    CASE(F_S_M_AGGREGATE);
           /* --- s_m_aggregate <idx> <num> <index>... --- */
    {
        /* Create a struct from the <num> values currently on the
         * stack. The struct can be found at short <idx> in
         * program.struct_defs[]. If <idx> is negative, the <num>+1th
         * value on the stack is a struct of the type to be generated
         * (F_S_AGGREGATE only).
         * For F_S_AGGREGATE, the values on the stack are to be assigned
         * to the struct members in ascending order.
         * For F_S_M_AGGREGATE, the <index>... values give for each
         * value on the stack into which struct member the value has to go.
         * This list of indices is given in reverse order, that is the
         * index for the topmost stack value comes first.
         */
        struct_t * st;
        short idx;
        int num_values;
        Bool has_template;
        svalue_t * svp;

        idx = load_short(&pc);
        num_values = load_uint8(&pc);
        has_template = MY_FALSE;

        if (idx < 0 && instruction == F_S_AGGREGATE)
        {
            struct_type_t *pType;

            if ((sp - num_values)->type != T_STRUCT)
            {
                ERRORF(("Bad template arg to #'(<: got %s, expected struct\n"
                       , typename((sp - num_values)->type)
                      ));
                /* NOTREACHED */
            }

            pType = (sp - num_values)->u.strct->type;

            if (num_values > struct_t_size(pType))
            {
                ERRORF(("Too many initializers for struct %s: "
                        "%ld, expected %ld\n"
                       , get_txt(struct_t_name(pType))
                       , (long)num_values
                       , (long)struct_t_size(pType)
                      ));
                /* NOTREACHED */
            }
            has_template = MY_TRUE;
            st = struct_new(pType);
        }
        else
        {
            st = struct_new(current_prog->struct_defs[idx].type);
        }
        if  (!st)
            ERROR("Out of memory!\n");

        if (instruction == F_S_AGGREGATE)
        {
            /* Easy way: just move all the values into the struct.
             * This allows for having less initializers than members.
             */

            for ( svp = st->member + num_values - 1
                ; num_values > 0
                ; num_values--, svp--, sp--
                )
            {
                *svp = *sp;
            }
        }
        else
        {
            /* Complex way: assign using the indices */
            int ix;

            for ( ; num_values > 0 ; num_values--, sp--)
            {
                ix = load_uint8(&pc);
                st->member[ix] = *sp;
            }
        }

        /* If necessary, remove the template struct */
        if (has_template)
        {
            free_svalue(sp); sp--;
        }

        /* Put the struct onto the stack */
        sp++;
        put_struct(sp, st);

        break;
   }

    CASE(F_PREVIOUS_OBJECT0);       /* --- previous_object0    --- */
        /* EFUN previous_object(void)
         *
         * Push the previous_object onto the stack, if existing and
         * not destructed.
         *
         * The compiler generates this code when it sees the previous_object()
         * efun used with no arguments.
         *
         * (Reminder: the efun previous_object(int) has a different meaning.)
         * TODO: How do other driver handle this?
         */
        if (previous_ob == 0 || (previous_ob->flags & O_DESTRUCTED))
            push_number(sp, 0);
        else
            push_ref_object(sp, previous_ob, "previous_object0");
        break;

    CASE(F_LAMBDA_CCONSTANT);    /* --- lambda_cconstant <num> --- */
    {
        /* Push the constant value <num> of this lambda closure onto
         * the stack.
         *
         * The values are stored in an svalue[] before the actual
         * function code and uint8 <num> is used to index that array
         * from the end.
         */
        uint8_t ix;
        svalue_t * cstart;

        /* Get the value index */
        ix = load_uint8(&pc);

        /* Get the pointer to the last constant value */
        cstart = (svalue_t *)((char *)(csp->funstart)
                                   - LAMBDA_VALUE_OFFSET);
        sp++;
        assign_svalue_no_free(sp, cstart - ix);
        break;
    }

    CASE(F_LAMBDA_CONSTANT);     /* --- lambda_constant <num> --- */
    {
        /* Push the constant value <num> of this lambda closure onto
         * the stack.
         *
         * The values are stored in an svalue[] before the actual
         * function code and (16-Bit) ushort <num> is used to index
         * that array from the end.
         */
        unsigned short ix;
        svalue_t * cstart;

        /* Get the value index */
        ix = load_short(&pc);

        /* Get the pointer to the last constant value */
        cstart = (svalue_t *)((char *)(csp->funstart)
                                   - LAMBDA_VALUE_OFFSET);
        sp++;
        assign_svalue_no_free(sp, cstart - ix);
        break;
    }

    CASE(F_MAP_INDEX);              /* --- map_index           --- */
    {
        /* Operator F_MAP_INDEX( mapping m=sp[-2], mixed i=sp[-1], int j=sp[0])
         *
         * Compute m[i,j] and push it onto the stack.
         */

        mapping_t *m;
        mp_int n;
        svalue_t *data;

        if (sp[-2].type != T_MAPPING)
        {
            ERRORF(("(value) Indexing on illegal type: %s, expected mapping.\n"
                   , typename(sp[-2].type)
                  ));
        }
        if (sp[0].type != T_NUMBER)
        {
            ERRORF(("Illegal sub-index type: %s, expected number.\n"
                   , typename(sp[0].type)
                  ));
        }

        m = sp[-2].u.map;
        n = sp->u.number;

        if (n < 0 || n >= m->num_values)
        {
            ERRORF(("Illegal sub-index %"PRIdMPINT", mapping width is %"
                    PRIdPINT".\n", n, m->num_values));
        }

        sp--; /* the key */

        data = get_map_value(m, sp);
        pop_stack();

        if (data == &const0)
        {
            put_number(sp, 0);
        }
        else
        {
            assign_rvalue_no_free(sp, data + n);
        }
        free_mapping(m);
        break;
    }

    CASE(F_FOREACH);       /* --- foreach     <nargs> <offset> --- */
    CASE(F_FOREACH_REF);   /* --- foreach_ref <nargs> <offset> --- */
    CASE(F_FOREACH_RANGE); /* --- foreach_range <nargs> <offset> --- */
    {
        /* Initialize a foreach() loop. On the stack are <nargs>-1
         * lvalues where the (l)value(s) are to be stored.
         * These lvalues are always unprotected lvalues (it's safe
         * here, because they point only to local or global variables).
         *
         * The last value on the stack is the (l)value to loop over.
         * (Do not confuse <nargs> with the normal NUM_ARG!).
         *
         * ushort <offset> is the distance to the FOREACH_NEXT
         * instruction follwing the codeblock after the instruction,
         * counted from the byte following this instruction.
         *
         * The instruction pushes two or three more values onto
         * the stack to store its internal status.
         *
         *   sp[0]  -> number 'next':  index of the next value to assign (0).
         *             x.generic:      0: FOREACH, 1: FOREACH_REF
         *                             2: FOREACH_RANGE with one extra loop
         *                                (this falls back to FOREACH after
         *                                 the first encounter of
         *                                 FOREACH_NEXT).
         *   sp[-1] -> number 'count': number of values left to loop over.
         *             x.generic:      <nargs>, or -<nargs> if the value
         *                             is mapping or a string lvalue.
         *   sp[-2] -> array 'm_indices': if the value is a mapping, this
         *                             is the array with the indices.
         *             string lvalue: if the value is a string used with
         *                            FOREACH_REF, this is a range lvalue.
         *
         * When iterating over a string reference, the string might change
         * its length due to UTF-8 encoding. To keep track of the length
         * and the current position we store a protected range lvalue
         * from the current position to the end. This will be updated
         * accordingly.
         *
         * After pushing the values onto the stack, the instruction
         * branches to the FOREACH_NEXT instruction to start the first
         * iteration.
         */

        int8_t nargs;
        p_int count, start;
        unsigned short offset;
        Bool gen_refs, use_range, do_extra_loop;
        bool last_reference = false;
        svalue_t * arg;

        gen_refs = (instruction == F_FOREACH_REF);
        use_range = (instruction == F_FOREACH_RANGE);
        do_extra_loop = MY_FALSE;
        start = 0;
        count = -1; /* -1 meaning uninitialized. */

        nargs = load_uint8(&pc);
        LOAD_SHORT(offset, pc);

        /* Unravel the lvalue chain (if any) to get to the actual value
         * to loop over.
         */
        if (gen_refs)
        {
            svalue_t argval;
            struct protected_lvalue* argvar = NULL;

            if (sp->type != T_LVALUE)
                ERRORF(("foreach() got a %s, expected a &(string/array/mapping).\n"
                       , typename(sp->type)
                       ));

            arg = get_rvalue_no_collapse(sp, &last_reference);
            if (arg && arg->type == T_NUMBER)
                ERROR("foreach() got a &number, expected a (&)string/array/mapping/struct or number.\n");

            if (arg == NULL)
            {
                /* This is a range lvalue, take that string/vector
                 * But set start and count accordingly.
                 */
                struct protected_range_lvalue *r;

                assert(sp->type == T_LVALUE && sp->x.lvalue_type == LVALUE_PROTECTED_RANGE);
                r = sp->u.protected_range_lvalue;

                arg = &(r->vec);

                start = r->index1;
                count = r->index2 - r->index1;
                if (count < 0)
                    count = 0;

                /* Remember the variable, if it is a string,
                 * because we might need to update it.
                 */
                if ((arg->type == T_STRING || arg->type == T_BYTES)
                 && r->var != NULL
                 && r->var->val.type == arg->type
                 && r->var->val.u.str == arg->u.str )
                    argvar = r->var;
            }
            else if (sp->x.lvalue_type == LVALUE_PROTECTED
                  && (arg->type == T_STRING || arg->type == T_BYTES))
                argvar = sp->u.protected_lvalue;

            if (arg->type == T_STRING || arg->type == T_BYTES)
            {
                /* If the string is tabled, i.e. not changeable, or has more
                 * than one reference, allocate a new copy which can be
                 * changed safely.
                 */
                string_t *str;
                memsafe(str = make_mutable(arg->u.str), mstrsize(arg->u.str)
                       , "modifiable string");

                if (arg->u.str != str)
                {
                    arg->u.str = str;
                    if (argvar != NULL && argvar->val.u.str != str)
                    {
                        free_mstring(argvar->val.u.str);
                        argvar->val.u.str = ref_mstring(str);
                    }
                }

                if (argvar)
                    argvar->ref++;
            }

            /* Replace the lvalue on the stack by the value
             * itself, we don't need the lvalue anymore.
             *
             * If this was the last reference,
             * we need to copy it before overwriting <sp>.
             */
            assign_svalue_no_free(&argval, arg);
            free_svalue(sp);
            transfer_svalue_no_free(sp, &argval);
            arg = sp;

            if (argvar)
            {
                /* Remember that one for later updates. */
                argval.type = T_LVALUE;
                argval.x.lvalue_type = LVALUE_PROTECTED;
                argval.u.protected_lvalue = argvar;
            }
            else if (arg->type == T_STRING || arg->type == T_BYTES)
            {
                /* We need the protected lvalue
                 * to keep track of the string.
                 */
                assign_svalue_no_free(&argval, sp);
                assign_protected_lvalue_no_free(sp+1, &argval);
                free_svalue(sp+1);
                argvar = argval.u.protected_lvalue;
            }

            if (argvar)
            {
                assign_protected_range_lvalue_no_free(sp+1, argvar, sp, start, count < 0 ? mstrsize(sp->u.str) : (start + count));
                free_svalue(&argval);
                sp++;

                nargs = -nargs;
            }
        }
        else if (use_range)
        {
            if (sp->type != T_NUMBER)
                ERRORF(("foreach() got a %s, requires a number for upper range bound.\n"
                       , typename(sp->type)
                       ));
        }

        arg = (nargs < 0) ? sp-1 : sp;

        if (arg->type != T_STRING
         && arg->type != T_BYTES
         && arg->type != T_POINTER
         && arg->type != T_NUMBER
         && arg->type != T_STRUCT
         && arg->type != T_MAPPING)
            ERRORF(("foreach() got a %s, expected a (&)string/bytes/array/mapping/struct or number.\n"
                   , typename(sp->type)
                   ));

        /* Find out how many variables we require */

        if (arg->type == T_NUMBER)
        {
            count = arg->u.number;
            if (count < 0 && !use_range)
                ERRORF(("foreach() got a %"PRIdPINT", expected a non-negative "
                        "number.", count));
        }
        else if (arg->type == T_STRING || arg->type == T_BYTES)
        {
            if (count < 0)
                count = mstrsize(arg->u.str);
        }
        else if (arg->type == T_POINTER)
        {
            check_for_destr(arg->u.vec);
            if (count < 0)
                count = VEC_SIZE(arg->u.vec);
        }
        else if (arg->type == T_STRUCT)
        {
            struct_check_for_destr(arg->u.strct);
            count = struct_size(arg->u.strct);
        }
        else
        {
            mapping_t *m;
            vector_t  *indices;

            m = arg->u.map;
            indices = m_indices(m);

            /* after m_indices(), else we'd count destructed entries */
            count = MAP_SIZE(m);

            /* Push the indices array and remember the fact in nargs.
             */
            sp++;
            put_array(sp, indices);
            nargs = -nargs;
        }

        /* If this is a range foreach, drop the upper bound svalue
         * from the stack and calculate the actual number of steps, and
         * get the lower bound svalue to be used as starting index.
         * Since this lower bound svalue is an integer as well, we can
         * then pretend to execute a normal foreach over an integer.
         */
        if (use_range)
        {
            free_svalue(sp); sp--;
            if (sp->type != T_NUMBER)
                ERRORF(("foreach() got a %s, expected a number for lower range bound.\n"
                       , typename(sp->type)
                       ));
            start = sp->u.number;
            if (count < start)
                count = 0;
            else
            {
                count = count - sp->u.number + 1;
                if (!count)
                {
                    /* Range is __INT_MIN_..__INT_MAX__: for this
                     * we need to make one more loop than we can count.
                     */
                    do_extra_loop = MY_TRUE;
                }
            }
        }

        /* Push the count and the starting index */
        push_number(sp, count); sp->x.generic = nargs;
        push_number(sp, start); sp->x.generic = do_extra_loop
                                                ? 2
                                                : (gen_refs ? 1 : 0);

#ifdef DEBUG
        /* The <nargs> lvalues and our temporaries act as hidden
         * local variables. We therefore adapt the variable count
         * so that a F_RETURN won't complain.
         */
        if (nargs >= 0)
            csp->num_local_variables += 2 + nargs;
        else
            csp->num_local_variables += 3 + (-nargs);
#endif

        /* Now branch to the FOREACH_NEXT */
        pc += offset;

        break;
    }

    CASE(F_FOREACH_NEXT);         /* --- foreach_next <typeidx> <offset> --- */
    {
        /* Start the next (resp. the first) iteration of a foreach()
         * loop. ushort <offset> is the distance to branch back to the
         * loop body, counted from the first byte of the next instruction.
         * For the stack layout, see F_FOREACH.
         *
         * ushort <typeidx> indicates the offset into the argument types
         * for the lvalue's type (<nargs>-1 entries). If it is USHRT_MAX
         * there should be no typecheck. (For F_FOREACH_RANGE this is
         * always the case, because we guarantee integers there.)
         */

        unsigned short typeidx, offset;
        p_int     ix;
        svalue_t *lvalue;  /* Pointer to the first lvalue */
        Bool      gen_refs;

        LOAD_SHORT(typeidx, pc);
        LOAD_SHORT(offset, pc);

        ix = sp->u.number;
        if (sp->x.generic == 2)
        {
            sp->x.generic = 0;
            /* FOREACH_RANGE with extra loop: count is 0,
             * but we must not end the loop here, instead
             * make a round through the full number range.
             */
        }
        else
        {
            /* Is there something left to iterate? */
            if (0 == sp[-1].u.number)
                break; /* Nope */

        }
        sp->u.number++;    /* next number */
        sp[-1].u.number--; /* decrement loop count */

        gen_refs = sp->x.generic;

        if (sp[-1].x.generic < 0 && sp[-2].type == T_POINTER)
        {
            /* We loop over a mapping */

            mapping_t *m;
            vector_t  *indices;
            svalue_t  *values;
            p_int      left;

            lvalue = sp + sp[-1].x.generic - 2;

            m = sp[-3].u.map;
            indices = sp[-2].u.vec;

            values = get_map_value(m, indices->item+ix);
            if (values == &const0)
            {
                /* Whoops, the entry has vanished.
                 * Start over with this instruction again, the
                 * index on the stack has been incremented already.
                 */
                pc -= 5;
                break;
            }

            /* Get the number of values we have to assign (in addition to the key). */
            left = -(sp[-1].x.generic) - 2;
            if (left > m->num_values)
                left = m->num_values;

            if (typeidx != USHRT_MAX && current_prog->argument_types)
            {
                /* Do runtime type checks. */
                for (int i = 0; i <= left; i++)
                {
                    lpctype_t* exptype = current_prog->argument_types[typeidx + i];
                    svalue_t * val = i ? (values + i - 1) : (indices->item + ix);
                    if (!check_rtt_compatibility(exptype, val))
                    {
                        char buf[512];
                        lpctype_t *realtype = get_rtt_type(exptype, val);
                        get_lpctype_name_buf(realtype, buf, sizeof(buf));
                        free_lpctype(realtype);

                        if (current_prog->flags & P_WARN_RTT_CHECKS)
                            warnf("Bad type for variable %d in foreach: got '%s', expected '%s'.\n",
                               i+1, buf, get_lpctype_name(exptype));
                        else
                            errorf("Bad type for variable %d in foreach: got '%s', expected '%s'.\n",
                               i+1, buf, get_lpctype_name(exptype));
                    }
                }
            }

            /* Assign the index we used */
            {
#ifdef DEBUG
                if (lvalue->type != T_LVALUE || lvalue->x.lvalue_type != LVALUE_UNPROTECTED)
                    fatal("Bad argument to foreach(): not a lvalue\n");
                    /* TODO: Give type and value */
#endif
                assign_svalue(lvalue, indices->item+ix);

                lvalue++;
            }

            /* Loop over the values and assign them */
            for ( ; left > 0; left--, lvalue++, values++)
            {
#ifdef DEBUG
                if (lvalue->type != T_LVALUE || lvalue->x.lvalue_type != LVALUE_UNPROTECTED)
                    fatal("Bad argument to foreach(): not a lvalue\n");
                    /* TODO: Give type and value */
#endif
                if (!gen_refs)
                {
                    assign_svalue(lvalue, values);
                }
                else
                {
                    svalue_t * dest = lvalue->u.lvalue;

                    free_svalue(dest);
                    assign_protected_lvalue_no_free(dest, values);
                }
            }

            /* Ta-Da! */
        }
        else
        {
            svalue_t *arg;
            svalue_t* val = NULL;

            if (sp[-1].x.generic < 0)
            {
                lvalue = sp + sp[-1].x.generic - 2;
                arg = sp - 3;
            }
            else
            {
                lvalue = sp - sp[-1].x.generic - 1;
                arg = sp - 2;
            }
#ifdef DEBUG
            if (lvalue->type != T_LVALUE || lvalue->x.lvalue_type != LVALUE_UNPROTECTED)
                fatal("Bad argument to foreach(): not a lvalue\n");
                /* TODO: Give type and value */
#endif

            /* Determine the value to assign. */
            switch (arg->type)
            {
                case T_NUMBER:
                {
                    static svalue_t num = { T_NUMBER };
                    num.u.number = ix;
                    val = &num;
                    break;
                }

                case T_STRING:
                case T_BYTES:
                    val = &const1; /* We do that later. */
                    break;

                case T_POINTER:
                    if (ix >= VEC_SIZE(arg->u.vec))
                        break;
                        /* Oops, this array shrunk while we're looping over it.
                         * We stop processing and continue with the following
                         * FOREACH_END instruction.
                         */

                    val = arg->u.vec->item+ix;
                    break;

                case T_STRUCT:
                    if (ix >= struct_size(arg->u.strct))
                        break;
                        /* Oops, somehow the struct managed to shring while
                         * we're looping over it.
                         * We stop processing and continue with the following
                         * FOREACH_END instruction.
                         */

                    val = arg->u.strct->member+ix;
                    break;

                default:
                  fatal("foreach() requires a string, array, struct or mapping.\n");
                  /* If this happens, the check in F_FOREACH failed. */
                  break;
            }

            if (!val)
                break;

            if (typeidx != USHRT_MAX && current_prog->argument_types)
            {
                /* Do runtime type checks. */
                lpctype_t* exptype = current_prog->argument_types[typeidx];
                if (!check_rtt_compatibility(exptype, val))
                {
                    char buf[512];
                    lpctype_t *realtype = get_rtt_type(exptype, val);
                    get_lpctype_name_buf(realtype, buf, sizeof(buf));
                    free_lpctype(realtype);

                    if (current_prog->flags & P_WARN_RTT_CHECKS)
                        warnf("Bad type for variable 1 in foreach: got '%s', expected '%s'.\n",
                           buf, get_lpctype_name(exptype));
                    else
                        errorf("Bad type for variable 1 in foreach: got '%s', expected '%s'.\n",
                           buf, get_lpctype_name(exptype));
                }
            }

            lvalue = lvalue->u.lvalue;

            if (arg->type == T_STRING || arg->type == T_BYTES)
            {
                size_t chlen;

                free_svalue(lvalue);
                if (!gen_refs)
                {
                    string_t * str = arg->u.str;
                    p_int ch = 0;

                    if (str->info.unicode == STRING_UTF8)
                        chlen = utf8_to_unicode(get_txt(str) + ix, sp[-1].u.number + 1, &ch);
                    else
                    {
                        chlen = 1;
                        ch = ((unsigned char*)get_txt(str))[ix];
                    }

                    put_number(lvalue, ch);

                    if (chlen > 1)
                    {
                        sp->u.number += chlen - 1;
                        sp[-1].u.number -= chlen - 1;
                    }
                }
                else
                {
                    struct protected_range_lvalue* lv;
                    string_t *str;
                    bool error = false;

                    assert(sp[-1].x.generic < 0);
                    assert(sp[-2].type == T_LVALUE && sp[-2].x.lvalue_type == LVALUE_PROTECTED_RANGE);

                    /* We use the count from the range lvalue. */
                    lv = sp[-2].u.protected_range_lvalue;
                    ix = lv->index1;
                    str = lv->vec.u.str;

                    if (str->info.unicode == STRING_UTF8)
                        chlen = char_to_byte_index(get_txt(str) + ix, lv->index2 - ix, 1, &error);
                    else
                        chlen = 1;

                    assign_protected_char_lvalue_no_free(lvalue, lv->var, str, get_txt(str) + ix);

                    lv->index1 = ix += (chlen ? chlen : 1);

                    /* But we update the count on the stack for the end check. */
                    sp->u.number = ix;
                    sp[-1].u.number = lv->index2 - lv->index1;
                }

            }
            else
            {
                free_svalue(lvalue);

                if (!gen_refs)
                {
                    assign_svalue_no_free(lvalue, val);
                }
                else
                {
                    assign_protected_lvalue_no_free(lvalue, val);
                }
            }
        }

        /* All that is left is to branch back. */
        pc -= offset;
        break;
    }

    CASE(F_FOREACH_END);            /* --- foreach_end         --- */
    {
        /* The foreach() loop ended or was terminated by a break.
         * All there's left to do is cleaning up the stack.
         */

        int nargs;

        nargs = sp[-1].x.generic;

        if (nargs < 0)
            nargs = (-nargs) + 3;
        else
            nargs = nargs + 2;

        pop_n_elems(nargs);

#ifdef DEBUG
        /* The <nargs> lvalues and our temporaries acted as hidden
         * local variables. We now count back the variable count
         * so that a F_RETURN won't complain.
         */
        csp->num_local_variables -= nargs;
#endif

        break;
    }

    CASE(F_END_CATCH);                  /* --- end_catch       --- */
        /* For a catch(...guarded code...) statement, the compiler
         * generates a F_END_CATCH as last instruction of the
         * guarded code.
         *
         * Executed when no error occurred, it returns into
         * catch_instruction() to clean up the
         * error recovery information pushed by the F_CATCH
         * and leave a 0 on the stack.
         *
         * dump_trace() checks for this bytecode, but accepts a normal
         * instruction as well as an escaped instruction.
         */

        return MY_TRUE;
        break;

                          /* --- breakn_continue <num> <offset> ---*/
    CASE(F_BREAKN_CONTINUE);
        /* Implement the 'continue;' statement from within
         * a nested surrounding structure.
         *
         * Pop <num>+1 (uint8) break-levels from the break stack
         * and jump by (32-Bit) long <offset> bytes, counted from the
         * first by of <offset>
         */

        break_sp += LOAD_UINT8(pc);
        /* FALLTHROUGH */

    CASE(F_BREAK_CONTINUE);       /* --- break_continue <offset> ---*/
    {
        /* Implement the 'continue;' statement for the immediate
         * surrounding structure.
         *
         * Pop one break-level from the break stack and jump
         * by (32-Bit) bc_offset_t <offset> bytes, counted from the
         * first byte of <offset>.
         *
         */
        break_sp++;
        pc += get_bc_offset(pc);
        break;
    }

#ifdef F_JUMP
    CASE(F_JUMP);                   /* --- jump <dest>         --- */
    {
        /* Jump to the address <dest> (absolute jump, bc_offset_t).
         */

        pc = current_prog->program + get_bc_offset(pc);
        break;
    }
#endif /* F_JUMP */

    CASE(F_NO_WARN_DEPRECATED);     /* --- no_warn_deprecated  --- */
    {
        /* Set the runtime_no_warn_deprecated flag for the next
         * instruction.
         */

        runtime_no_warn_deprecated = MY_TRUE;
        break;
    }

    CASE(F_ARRAY_RANGE_CHECK);       /* --- array_range_check  --- */
    {
        /* Set the runtime_array_range_check flag for the next
         * instruction.
         */

        runtime_array_range_check = MY_TRUE;
        break;
    }

    CASE(F_ARRAY0);                 /* --- array0 <size>  --- */
    {
        /* Push a zero-initialized array on the stack.
         */

        unsigned short size;
        vector_t *v;

        LOAD_SHORT(size, pc);
        v = allocate_array(size);
        push_array(sp, v);
        break;
    }

    CASE(F_MOVE_VALUE);             /* --- move_value <offset>  --- */
    {
        /* Move sp[0] to sp[-offset-1] and the values sp[-1] to sp[-offset-1]
         * one position up. This opcode also takes care of at most one
         * argument frame.
         */

        int i, offset = LOAD_UINT8(pc);
        svalue_t sv = sp[0];

        for (i = 0; i >= -offset; i--)
            sp[i] = sp[i-1];
        sp[i] = sv;

        if (ap == sp+1) // Argument frame moved?
            ap = sp+i+1;
        else if (ap > sp+i)
            ap++;
        break;
    }

    CASE(F_DUP_N);                  /* --- dup_n <offset> <num>  --- */
    {
        /* Copy <num> elements (ignoring the topmost <offset> elements)
         * of the top of stack on top of the stack.
         */

        int offset, num;

        offset = LOAD_UINT8( pc);
        num = LOAD_UINT8(pc);

        inter_sp = sp;
        push_svalue_block(num, sp-offset-num+1);
        sp = inter_sp;
        break;
    }

    CASE(F_POP_N);                  /* --- pop_n <num>  --- */
    {
        /* Pop the <num> topmost elements off the stack.
         */

        pop_n_elems(LOAD_UINT8(pc));
        break;
    }

    CASE(F_PUT_ARRAY_ELEMENT); /* --- put_array_element <offset> <ix>  --- */
    {
        /* Remove the top value off the stack und put it into the array
         * at sp[-offset-1] at index <ix>.
         */

        unsigned short offset;
        int ix;

        LOAD_SHORT(offset, pc);
        ix = LOAD_UINT8(pc);

#ifdef DEBUG
        if (sp[-offset-1].type != T_POINTER)
            FATALF(("Illegal type '%s' for F_PUT_ARRAY_ELEMENT.\n"
                   , typename(sp[-offset-1].type)
                   ));
#endif

        transfer_svalue_no_free(sp[-offset-1].u.vec->item+ix, sp);
        sp--;
        break;
    }

    CASE(F_TYPE_CHECK);             /* --- type_check <op> <ix> --- */
    {
        /* Check the top value off the stack against the type
         * at prog->argument_types[<ix>]. Raise an error if
         * it doesn't match. Do nothing otherwise.
         * <op> contains a value of enum type_check_operation to
         * give a specific error message.
         */

        unsigned short ix, op = LOAD_UINT8(pc);
        lpctype_t* exptype;

        LOAD_SHORT(ix, pc);

        /* Types were saved? */
        if (!current_prog->argument_types)
            break;

        exptype = current_prog->argument_types[ix];
        if (!check_rtt_compatibility(exptype, sp))
        {
            static char buff[512];
            const char* op_str;
            lpctype_t *realtype = get_rtt_type(exptype, sp);
            get_lpctype_name_buf(realtype, buff, sizeof(buff));
            free_lpctype(realtype);

            inter_sp = sp;
            switch (op)
            {
                case TYPECHECK_ASSIGNMENT:
                    op_str = "assignment";
                    break;

                case TYPECHECK_VAR_INIT:
                    op_str = "variable initialization";
                    break;

                case TYPECHECK_CAST:
                    op_str = "type cast";
                    break;

                case TYPECHECK_DECL_CAST:
                    op_str = "declarative type cast";
                    break;

                default:
                    op_str = "unknown operation";
                    break;
            }
            if (current_prog->flags & P_WARN_RTT_CHECKS)
                warnf("Bad type for %s: got '%s', expected '%s'.\n",
                   op_str, buff, get_lpctype_name(exptype));
            else
                errorf("Bad type for %s: got '%s', expected '%s'.\n",
                   op_str, buff, get_lpctype_name(exptype));
        }

        break;
    }

    /* --- Efuns: Miscellaneous --- */

    CASE(F_CLONEP);                 /* --- clonep              --- */
    {
        /* EFUN clonep()
         *
         *   int clonep()
         *   int clonep (object obj)
         *   int clonep (string obj)
         *
         * The efun returns 1 if <obj> is a clone, and 0 if it is not.
         * The <obj> can be given as the object itself, or by its name.
         * If <obj> is omitted, the current object is tested.
         * Arguments of other types return 0.
         */

        int i;

        if (sp->type == T_OBJECT)
        {
            i = (sp->u.ob->flags & O_CLONE);
        }
        else if (sp->type == T_STRING)
        {
            object_t *o;

            o = find_object(sp->u.str);
            if (!o)
                ERRORF(("No such object '%s'.\n", get_txt(sp->u.str)));
            i = o->flags & O_CLONE;
        }
        else
            i = 0;
        free_svalue(sp);
        put_number(sp, i ? 1 : 0);
        break;
    }

    CASE(F_CLOSUREP);               /* --- closurep            --- */
    {
        /* EFUN closurep()
         *
         *   int closurep(mixed)
         *
         * Returns 1 if the argument is a closure.
         */

        int i;

        i = sp->type == T_CLOSURE;
        free_svalue(sp);
        put_number(sp, i);
        break;
    }

    CASE(F_FLOATP);                 /* --- floatp              --- */
    {
        /* EFUN floatp()
         *
         *   int floatp(mixed)
         *
         * Returns 1 if the argument is a float.
         */

        int i;

        i = sp->type == T_FLOAT;
        free_svalue(sp);
        put_number(sp, i);
        break;
    }

    CASE(F_INTP);                   /* --- intp                --- */
    {
        /* EFUN intp()
         *
         *   int intp(mixed)
         *
         * Returns 1 if the argument is an integer.
         */

        int i;

        i = sp->type == T_NUMBER;
        free_svalue(sp);
        put_number(sp, i);
        break;
    }

    CASE(F_MAPPINGP);               /* --- mappingp            --- */
    {
        /* EFUN mappingp()
         *
         *   int mappingp(mixed)
         *
         * Returns 1 if the argument is a mapping.
         */

        int i;

        i = sp->type == T_MAPPING;
        free_svalue(sp);
        put_number(sp, i);
        break;
    }

    CASE(F_OBJECTP);                /* --- objectp              --- */
    {
        /* EFUN objectp()
         *
         *   int objectp(mixed)
         *
         * Returns 1 if the argument is an object.
         */

        int i;

        i = sp->type == T_OBJECT;
        free_svalue(sp);
        put_number(sp, i);
        break;
    }

    CASE(F_POINTERP);               /* --- pointerp            --- */
    {
        /* EFUN pointerp()
         *
         *   int pointerp(mixed)
         *
         * Returns 1 if the argument is an array.
         */

        int i;

        i = sp->type == T_POINTER;
        free_svalue(sp);
        put_number(sp, i);
        break;
    }

    CASE(F_REFERENCEP);                /* --- referencep      --- */
      {
        /* EFUN referencep()
         *
         *   int referencep(mixed arg)
         *
         * Returns true if arg was passed by reference to the current
         * function, instead of the usual call-by-value.
         */

        int i = 0;
        if (sp->type == T_LVALUE)
        {
            /* It must be an protected lvalue with at least 3 references:
             *  - one on the stack,
             *  - one on the local variable of the caller to this efun,
             *  - one on whatever variable of the caller of the caller
             *    (whatever the local variable references).
             */
            switch (sp->x.lvalue_type)
            {
                default:
                    fatal("Illegal lvalue %p type %d as arg 1 to referencep().\n", sp, sp->x.lvalue_type);
                    /* NOTREACHED */
                    break;

                case LVALUE_PROTECTED:
                    i = (sp->u.protected_lvalue->ref > 2);
                    break;

                case LVALUE_PROTECTED_CHAR:
                    i = (sp->u.protected_char_lvalue->ref > 2);
                    break;

                case LVALUE_PROTECTED_RANGE:
                    i = (sp->u.protected_range_lvalue->ref > 2);
                    break;

                case LVALUE_PROTECTED_MAPENTRY:
                    i = (sp->u.protected_mapentry_lvalue->ref > 2);
                    break;
            } /* switch (sp->x.lvalue_type) */
        }
        free_svalue(sp);
        put_number(sp, i);
        break;
      }

    CASE(F_STRINGP);                /* --- stringp             --- */
    {
        /* EFUN stringp()
         *
         *   int stringp(mixed)
         *
         * Returns 1 if the argument is a string.
         */

        int i;

        i = sp->type == T_STRING;
        free_svalue(sp);
        put_number(sp, i);
        break;
    }

    CASE(F_BYTESP);                 /* --- bytesp              --- */
    {
        /* EFUN bytesp()
         *
         *   int bytesp(mixed)
         *
         * Returns 1 if the argument are bytes.
         */

        int i;

        i = sp->type == T_BYTES;
        free_svalue(sp);
        put_number(sp, i);
        break;
    }

    CASE(F_STRUCTP);                /* --- structp             --- */
    {
        /* EFUN structp()
         *
         *   int structp(mixed)
         *
         * Returns 1 if the argument is a struct.
         */

        int i;

        i = sp->type == T_STRUCT;
        free_svalue(sp);
        put_number(sp, i);
        break;
    }

    CASE(F_SYMBOLP);                /* --- symbolp             --- */
    {
        /* EFUN symbolp()
         *
         *   int symbolp(mixed)
         *
         * Returns 1 if the argument is a symbol.
         */

        int i;

        i = sp->type == T_SYMBOL;
        free_svalue(sp);
        put_number(sp, i);
        break;
    }

    CASE(F_TYPEOF);                    /* --- typeof          --- */
      {
        /* EFUN typeof()
         *
         *   int typeof(mixed)
         *
         * Returns a code for the type of the argument, as defined in
         * <sys/lpctypes.h>
         */

        mp_int i = sp->type;
        free_svalue(sp);
        put_number(sp, i);
        break;
      }

    CASE(F_NEGATE);                 /* --- negate              --- */
        /* EFUN negate()
         *
         *   int|float negate(int|float arg)
         *
         * Negate the value <arg> and leave it on the stack.
         * Calls to this efun are mainly generated by the compiler when
         * it sees the unary '-' used.
         */

        if (sp->type == T_NUMBER)
        {
            if (sp->u.number == PINT_MIN)
                ERRORF(("Numeric overflow: - %"PRIdPINT"\n", sp->u.number));
            sp->u.number = - sp->u.number;
            break;
        }
        else if (sp->type == T_FLOAT)
        {
            STORE_DOUBLE_USED
            double d;

            d = -READ_DOUBLE(sp);
            if (d < (-DBL_MAX) || d > DBL_MAX)
                ERRORF(("Numeric overflow: -(%g)\n", READ_DOUBLE(sp)));
            STORE_DOUBLE(sp,d);
            break;
        }
        ERRORF(("Bad arg to unary minus: got %s, expected number/float\n"
               , typename(sp->type)
               ));

    CASE(F_RAISE_ERROR);               /* --- raise_error     --- */
      {
        /* EFUN raise_error()
         *
         *   void raise_error(string arg)
         *
         * Abort execution. If the current program execution was initiated
         * by catch(), that catch expression will return arg as error
         * code, else the arg will printed as error message. This
         * is very similar to throw(), but while throw() is intended to be
         * called inside catch(), raise_error() can be called
         * anywhere.
         */

        TYPE_TEST1(sp, T_STRING);

        ERRORF(("%s", get_txt(sp->u.str)));
      }

    CASE(F_THROW);                  /* --- throw               --- */
        /* EFUN throw()
         *
         *   void throw(mixed arg)
         *
         * Abort execution. If the current program execution was initiated by
         * catch(), that catch expression will return arg as error code.
         */

        assign_eval_cost_inl();
        inter_sp = --sp;
        inter_pc = pc;
        throw_error(sp+1); /* do the longjump, with extra checks... */
        break;

    /* --- Efuns: Arrays and Mappings --- */

    CASE(F_SIZEOF);                 /* --- sizeof              --- */
    {
        /* EFUN sizeof()
         *
         *   int sizeof(mixed arr)
         *
         * Returns the number of elements of an array, the number of
         * keys in a mapping, or the number of characters in a string.
         *
         * As a special case, the number 0 can be passed, and the function
         * will return 0.
         */

        p_int i;

        if (sp->type == T_STRING || sp->type == T_BYTES)
        {
            i = mstrsize(sp->u.str);
            if (sp->u.str->info.unicode == STRING_UTF8)
            {
                bool error = false;
                i = byte_to_char_index(get_txt(sp->u.str), i, &error);
                if (error)
                    ERRORF(("Invalid character in string at index %zd.\n", i));
            }
            free_svalue(sp);
            put_number(sp, i);
            break;
        }

        if (sp->type == T_POINTER)
        {
            i = VEC_SIZE(sp->u.vec);
            free_svalue(sp);
            put_number(sp, i);
            break;
        }

        if (sp->type == T_STRUCT)
        {
            i = struct_size(sp->u.strct);
            free_svalue(sp);
            put_number(sp, i);
            break;
        }

        if (sp->type == T_MAPPING)
        {
            mapping_t *m = sp->u.map;
            check_map_for_destr_keys(m); /* Don't count the destructed keys! */
            i = MAP_SIZE(m);
            free_svalue(sp);
            put_number(sp, i);
            break;
        }

        if (sp->type == T_NUMBER && sp->u.number == 0)
            break;

        RAISE_ARG_ERROR(1, TF_STRING|TF_BYTES|TF_STRUCT|TF_MAPPING|TF_POINTER, sp->type);
        /* NOTREACHED */
    }

    /* --- Efuns: Functions and Closures --- */

    CASE(F_EXTERN_CALL);               /* --- extern_call     --- */
      {
        /* EFUN extern_call()
         *
         *   int extern_call();
         *
         * Returns zero, if the function that is currently being executed
         * was called by a local call, non-zero for call_other(), driver
         * applies, closure calls, etc. Currently the only return value
         * for them is 1, but later the various methods may be
         * distinguished by means of the return value.
         */


        struct control_stack * pt = csp;

        while (pt->catch_call) pt--;
        push_number(sp, (pt->extern_call & ~CS_PRETEND) ? 1 : 0);
        break;
      }

    /* --- Efuns: Objects --- */

    CASE(F_MASTER);                 /* --- master              --- */
      {
        /* EFUN master()
         *
         *   object master(int dont_load)
         *
         * Return the master object. If <dont_load> is false, the
         * function first makes sure that the master object exists.
         * If <dont_load> is true, the function just returns the current
         * master object, or 0 if the current master has been destructed.
         */

        TYPE_TEST1(sp, T_NUMBER)

        if (! sp->u.number)
            assert_master_ob_loaded();

        free_svalue(sp);

        if (master_ob)
            put_ref_object(sp, master_ob, "master");
        else
            put_number(sp, 0);
        break;
     }

    CASE(F_THIS_INTERACTIVE);       /* --- this_interactive    --- */
        /* EFUN this_interactive()
         *
         *   object this_interactive(void)
         *
         * this_interactive() returns the current interactive object, if
         * any, i.e. the one who "hit the RETURN key".
         */

        if (current_interactive
         && !(current_interactive->flags & O_DESTRUCTED))
            push_ref_object(sp, current_interactive, "this_interactive");
        else
            push_number(sp, 0);
        break;

    CASE(F_THIS_OBJECT);            /* --- this_object         --- */
        /* EFUN this_object()
         *
         *   object this_object(void)
         *
         * Return the object pointer for this object.
         */

        if (current_object->flags & O_DESTRUCTED)
        {
            push_number(sp, 0);
            break;
        }
        push_ref_object(sp, current_object, "this_object");
        break;

    /* --- Efuns: Verbs and Commands --- */

    CASE(F_THIS_PLAYER);            /* --- this_player         --- */
        /* EFUN this_player()
         *
         *   object this_player(void)
         *
         * Return the current command giver.  This can be an interactive
         * user or a living object like a npc.
         *
         * If called from inside the heart_beat() of a not living object
         * 0 will be returned.
         */

        if (command_giver && !(command_giver->flags & O_DESTRUCTED))
            push_ref_object(sp, command_giver, "this_player");
        else
            push_number(sp, 0);
        break;

    /* --- Optional Efuns: Technical --- */

#ifdef F_BREAK_POINT
    CASE(F_BREAK_POINT);            /* --- break_point         --- */
        /* EFUN break_point()
         *
         *   void break_point()
         *
         * This function is for system internal use and should never be called
         * by user objects. It is supposed to check the stack integrity and
         * aborts the driver when it detects corruption.
         *
         */

        if (sp - fp - csp->num_local_variables + 1 != 0)
            fatal("Bad stack pointer.\n");
        break;
#endif

#ifdef F_SWAP
    CASE(F_SWAP);                      /* --- swap            --- */
      {
        /* EFUN swap()
         *
         *   void swap(object obj, int parts = 0)
         *
         * Swap out an object. This efun is only used for system internal
         * debugging and can cause a crash.
         *
         * <parts> is a bitmask:
         *  0x01: Swap program
         *  0x02: Swap variables
         * The default (also when 0 given) is both (3).
         */

        object_t *ob;
        int flags;

        /* Test the arguments */
        if (sp[-1].type != T_OBJECT)
            RAISE_ARG_ERROR(1, TF_OBJECT, sp[-1].type);
        if (sp[0].type != T_NUMBER)
            RAISE_ARG_ERROR(1, TF_NUMBER, sp[0].type);

        ob = sp[-1].u.ob;
        flags = sp[0].u.number;
        if (!flags)
            flags = 3;

        if (ob != current_object
         && !(ob->flags & O_DESTRUCTED)
          ) /* should also check csp */
        {
            if ((flags&1) && !O_PROG_SWAPPED(ob))
                (void)swap_program(ob);
            if ((flags&2) && !O_VAR_SWAPPED(ob))
                (void)swap_variables(ob);
        }
        sp--; /* T_NUMBER */
        free_svalue(sp--);
        break;
      }
#endif

    } /* end of the monumental switch */

    /* Instruction executed */

    /* Reset the no-warn-deprecated flag */
    if (instruction != F_NO_WARN_DEPRECATED)
        runtime_no_warn_deprecated = MY_FALSE;

    /* Reset the no-warn-deprecated flag */
    if (instruction != F_ARRAY_RANGE_CHECK)
        runtime_array_range_check = MY_FALSE;

    /* Even intermediate results could exceed the stack size.
     * We better check for that.
     */
    if (sp - VALUE_STACK == SIZEOF_STACK - 1)
    {
        /* sp ist just at then end of the stack area */
        stack_overflow(sp, fp, pc);
    }
    else if ((mp_int)(sp - VALUE_STACK) > (mp_int)(SIZEOF_STACK - 1))
    {
        /* When we come here, we already overwrote the bounds
         * of the stack :-(
         */
        fatal("Fatal stack overflow: %"PRIdMPINT" too high\n"
             , (mp_int)(sp - VALUE_STACK - (SIZEOF_STACK - 1))
             );
    }

#ifdef DEBUG
    if (expected_stack && expected_stack != sp)
    {
        fatal( "Bad stack after evaluation.\n"
               "sp: %p expected: %p\n"
               "Instruction %d(%s), num arg %d\n"
             , sp, expected_stack
             , instruction, get_f_name(instruction), num_arg);
    }

    if (sp < fp + csp->num_local_variables - 1)
    {
        fatal( "Bad stack after evaluation.\n"
               "sp: %p minimum expected: %p\n"
               "Instruction %d(%s), num arg %d\n"
             , sp, (fp + csp->num_local_variables - 1)
             , instruction, get_f_name(instruction), num_arg);
    }
#endif /* DEBUG */

    // Did we receive a SIGPROF signal and should dump a trace into the debuglog?
    if (received_prof_signal)
    {
        received_prof_signal = MY_FALSE;
        char     *ts = time_stamp();
        debug_message("%s Received profiling signal, evaluation time > %ld.%06lds\n",
                      ts, (long)profiling_timevalue.tv_sec, (long)profiling_timevalue.tv_usec);
        printf("%s Received profiling signal, evaluation time > %ld.%06lds\n",
                      ts, (long)profiling_timevalue.tv_sec, (long)profiling_timevalue.tv_usec);
        // dump stack trace and continue execution
        inter_pc = pc;
        dump_trace(MY_FALSE, NULL, NULL);
        debug_message("%s ... execution continues.\n", ts);
        printf("%s ... execution continues.\n", ts);
    }
    
    // Did we allocate too much memory in this execution/evaluation thread?
    if (max_memory
        && xalloc_used() - used_memory_at_eval_start > max_memory)
    {
        ERRORF(("Additional memory allocation in this execution thread "
                "exceeded limit: %"PRIdPINT", allowed: %"PRIdPINT".\n",
                xalloc_used() - used_memory_at_eval_start,
                max_memory));
    }

    // Was there a signal to terminate this thread?
    if (interrupt_execution)
    {
        /* Reset the flag, so the error handler can execute. */
        interrupt_execution = false;
        ERRORF(("Interrupt by external signal.\n"));
    }

    /* Execute the next instruction */

    goto again;

    /* Get rid of the handy but highly local macros */
#   undef GET_NUM_ARG
#   undef RAISE_ARG_ERROR
#   undef BAD_ARG_ERROR
#   undef OP_ARG_ERROR
#   undef BAD_OP_ARG
#   undef TYPE_TEST1
#   undef TYPE_TEST2
#   undef TYPE_TEST3
#   undef TYPE_TEST4
#   undef TYPE_TEST_LEFT
#   undef TYPE_TEST_RIGHT
#   undef TYPE_TEST_EXP_LEFT
#   undef TYPE_TEST_EXP_RIGHT
#   undef CASE
#   undef ARG_ERROR_TEMPL
#   undef OP_ARG_ERROR_TEMPL
#   undef TYPE_TEST_TEMPL
#   undef OP_TYPE_TEST_TEMPL
#   undef EXP_TYPE_TEST_TEMPL

} /* eval_instruction() */

/*-------------------------------------------------------------------------*/
static Bool
apply_low ( string_t *fun, object_t *ob, int num_arg
          , Bool b_ign_prot)

/* The low-level implementation of function calls.
 *
 * Call function <fun> in <ob>ject with <num_arg> arguments pushed
 * onto the stack (<inter_sp> points to the last one). static and protected
 * functions can't be called from the outside unless <b_ign_prot> is true.
 * apply_low() takes care of calling shadows where necessary.
 *
 * When apply_low() returns true, the call was successful, the arguments
 * one the stack have been popped and replaced with the result. But note
 * that <ob> might have been destructed during the call.
 *
 * If apply_low() returns false, the function was not found and the arguments
 * must be removed by the caller. One reason for failure can be an attempt
 * to call an inherited function '::foo' with this function.
 *
 * To speed up the calls, apply_low() maintains a cache of earlier calls, both
 * hits and misses.
 *
 * The function call will swap in the object and also unset its reset status.
 */

{
    program_t *progp;
    struct control_stack *save_csp;
    p_int ix;

    /* This object will now be used, and is thus a target for
     * reset later on (when time due).
     */
    ob->flags &= ~O_RESET_STATE;

#ifdef DEBUG
    if (num_error > 2) {
        fatal("apply_low with too many errors.\n");
        goto failure;
    }
#endif

    /* If there is a chain of objects shadowing, start with the first
     * of these.
     */
    if (ob->flags & O_SHADOW)
    {
        object_t *shadow;

        while (NULL != (shadow = O_GET_SHADOW(ob)->shadowed_by)
            && shadow != current_object)
        {
            ob = shadow;
        }
    }

retry_for_shadow:

    ob->time_of_ref = current_time;

    /* Load the object from swap */
    if (ob->flags & O_SWAPPED)
    {
        if (load_ob_from_swap(ob) < 0)
            errorf("Out of memory\n");
    }

    progp = ob->prog;

#ifdef DEBUG
    if (ob->flags & O_DESTRUCTED)
        fatal("apply() on destructed object '%s' function '%s'\n"
             , ob->name != NULL ? get_txt(ob->name) : "<null>"
             , fun != NULL ? get_txt(fun) : "<null>"
             );
#endif

    /* Get the function name as a shared (directly tabled) string.
     * Since function names are always tabled, such a string must exist
     * if the function exists.
     */
    if (!mstr_tabled(fun))
    {
        fun = find_tabled(fun);
        if (!fun)
            goto failure2;
    }
    /* fun is now guaranteed to be a shared string */

    /* Get the hashed index into the cache */
    ix =
      ( progp->id_number ^ (p_int)fun ^ ( (p_int)fun >> APPLY_CACHE_BITS ) )
         & (CACHE_SIZE-1);

    /* Check if we have an entry for this function call */
    if (cache[ix].id == progp->id_number
     && (cache[ix].name == fun || mstreq(cache[ix].name, fun))
       )
    {
        /* We have found a matching entry in the cache. The contents have
         * to match, not only the pointers, because cache entries for
         * functions not existant in _this_ object <ob> are stored as
         * separately allocated copy, not as another ref to the shared
         * string. Yet they shall be found here.
         */
#ifdef APPLY_CACHE_STAT
        apply_cache_hit++;
#endif
        if (cache[ix].progp
          /* Static functions may not be called from outside.
           * Protected functions not even from the inside
           * And undefined functions are never found by name.
           */
          && !(cache[ix].flags &
                 ((!b_ign_prot && current_object != ob ? TYPE_MOD_STATIC    : 0)
                 |(!b_ign_prot                         ? TYPE_MOD_PROTECTED : 0)
                 | NAME_UNDEFINED
                 )
             )
           )
        {
            /* the cache will tell us in wich program the function is, and
             * where.
             */
            bytecode_p funstart;
            
            // check for deprecated functions before pushing a new control stack frame.
            if (cache[ix].flags & TYPE_MOD_DEPRECATED)
                warnf("Callother to deprecated function \'%s\' in object %s (%s).\n",
                      get_txt(fun), get_txt(ob->name), get_txt(ob->prog->name));

            push_control_stack(inter_sp, inter_pc, inter_fp, inter_context);
            csp->ob = current_object;
            csp->prev_ob = previous_ob;
            csp->num_local_variables = num_arg;
            csp->funstart = funstart = cache[ix].funstart;
            current_prog = cache[ix].progp;
            current_strings = current_prog->strings;
            function_index_offset = cache[ix].function_index_offset;
            variable_index_offset = cache[ix].variable_index_offset;
#ifdef DEBUG
            if (!ob->variables && cache[ix].variable_index_offset)
                fatal("%s Fatal: apply (cached) for object %p '%s' "
                      "w/o variables, but offset %d\n"
                     , time_stamp(), ob, get_txt(ob->name)
                     , cache[ix].variable_index_offset);
#endif
            current_variables = ob->variables;
            if (current_variables)
                current_variables += variable_index_offset;
            inter_sp = setup_new_frame2(funstart, inter_sp, MY_FALSE);
                        
            // check argument types
            check_function_args(cache[ix].progp->function_headers[FUNCTION_HEADER_INDEX(funstart)].offset.fx, cache[ix].progp, funstart);
            
            previous_ob = current_object;
            current_object = ob;
            save_csp = csp;
            eval_instruction(inter_pc, inter_sp);
#ifdef DEBUG
            if (save_csp-1 != csp)
                fatal("Bad csp after execution in apply_low\n");
#endif
            /* Arguments and local variables are now removed. One
             * resulting value is always returned on the stack.
             */
            return MY_TRUE;
        }

        /* when we come here, the cache has told us that the function isn't
         * defined in the object
         */
    }
    else
    {
        /* we have to search the function */

#ifdef APPLY_CACHE_STAT
        apply_cache_miss++;
#endif

        if ( NULL != fun)
        {
            int fx;

            /* Yup, fun is a function _somewhere_ */

            eval_cost++;
            total_evalcost++;
            fx = find_function(fun, progp);
            if (fx >= 0)
            {
                /* Found the function - setup the control stack and
                 * create a new cache entry.
                 */

                funflag_t flags;
                bytecode_p funstart;

                // check for deprecated functions before pushing a new control stack frame.
                if (progp->functions[fx] & TYPE_MOD_DEPRECATED)
                    warnf("Callother to deprecated function \'%s\' in object %s (%s).\n",
                          get_txt(fun), get_txt(ob->name), get_txt(ob->prog->name));
                push_control_stack(inter_sp, inter_pc, inter_fp, inter_context);
                  /* if an error occurs here, it won't leave the cache in an
                   * inconsistent state.
                   */
                csp->ob = current_object;
                csp->prev_ob = previous_ob;
                if (cache[ix].name)
                    free_mstring(cache[ix].name);

                cache[ix].id = progp->id_number;
                cache[ix].name = ref_mstring(fun);

                csp->num_local_variables = num_arg;
                current_prog = progp;
                flags = setup_new_frame1(fx, 0, current_prog->num_virtual_variables);
                
                current_strings = current_prog->strings;

                cache[ix].progp = current_prog;
                cache[ix].function_index_offset = function_index_offset;
                cache[ix].variable_index_offset = variable_index_offset;

#ifdef DEBUG
                if (!ob->variables && variable_index_offset)
                    fatal("%s Fatal: apply for object %p '%s' w/o variables, "
                          "but offset %d\n"
                         , time_stamp(), ob, get_txt(ob->name)
                         , variable_index_offset);
#endif
                current_variables = ob->variables;
                if (current_variables)
                    current_variables += variable_index_offset;
                funstart = current_prog->program + (flags & FUNSTART_MASK);

                cache[ix].funstart = funstart;
                cache[ix].flags = (progp->functions[fx]
                                   & (TYPE_MOD_STATIC|TYPE_MOD_PROTECTED|TYPE_MOD_DEPRECATED))
                                | (GET_CODE(funstart) == F_UNDEF ? NAME_UNDEFINED : 0);

                /* Static functions may not be called from outside,
                 * Protected functions not even from the inside.
                 * And undefined functions are never found by name.
                 */
                if (cache[ix].flags &
                     ((!b_ign_prot && current_object != ob ? TYPE_MOD_STATIC    : 0)
                     |(!b_ign_prot                         ? TYPE_MOD_PROTECTED : 0)
                     | NAME_UNDEFINED
                     )
                   )
                {
                    /* Not found */

                    previous_ob = csp->prev_ob;
                    current_object = csp->ob;
                    pop_control_stack();
                    if (ob->flags & O_SHADOW && O_GET_SHADOW(ob)->shadowing)
                    {
                        /* This is an object shadowing another. The function
                         * was not found, but can maybe be found in the object
                         * we are shadowing.
                         */
                        ob = O_GET_SHADOW(ob)->shadowing;
                        goto retry_for_shadow;
                    }
                    else
                        goto failure;
                }
                csp->funstart = funstart;
                inter_sp = setup_new_frame2(funstart, inter_sp, MY_FALSE);

                // check argument types
                check_function_args(fx - function_index_offset, current_prog, funstart);

                previous_ob = current_object;
                current_object = ob;
                save_csp = csp;
                eval_instruction(inter_pc, inter_sp);
#ifdef DEBUG
                if (save_csp-1 != csp)
                    fatal("Bad csp after execution in apply_low\n");
#endif
                /* Arguments and local variables are now removed. One
                 * resulting value is always returned on the stack.
                 */
                return MY_TRUE;
            } /* end if (fx >= 0) */
        } /* end if(fun) */

        /* We have to mark this function as non-existant in this object. */

        if (cache[ix].name)
            free_mstring(cache[ix].name);

        cache[ix].id = progp->id_number;
        cache[ix].name = ref_mstring(fun);
        cache[ix].progp = NULL;
    }

    /* At this point, the function was not found in the object. But
     * maybe this object is a shadow and we find the function in the
     * shadowed object.
     */

    if (ob->flags & O_SHADOW && O_GET_SHADOW(ob)->shadowing)
    {
        ob = O_GET_SHADOW(ob)->shadowing;
        goto retry_for_shadow;
    }

failure:
    if (get_txt(fun)[0] == ':')
        errorf("Illegal function call\n");

failure2:
    /* Failure. Deallocate stack. */
    return MY_FALSE;
} /* apply_low() */

/*-------------------------------------------------------------------------*/
static int
int_apply (string_t *fun, object_t *ob, int num_arg
          , Bool b_ign_prot, Bool b_use_default
          )

/* The wrapper around apply_low() to handle default methods.
 *
 * Call function <fun> in <ob>ject with <num_arg> arguments pushed
 * onto the stack (<inter_sp> points to the last one). static and protected
 * functions can't be called from the outside unless <b_ign_prot> is true.
 * int_apply() takes care of calling shadows where necessary.
 * If <b_use_default> is true and the function call can't be resolved,
 * the function will try to call the default method if one is defined.
 *
 * Results:
 *   APPLY_NOT_FOUND (0): The function was not found (and neither a default
 *       lfun, if allowed). If <b_use_default> was TRUE, the arguments
 *       have already been removed, otherwise the arguments must be
 *       removed by the caller.
 *       One eason for failure can be an attempt to call an inherited
 *       function '::foo' with this function.
 *
 *   APPLY_FOUND: The function was found, and the arguments on the stack
 *       have been popped and replaced with the result. But note
 *       that <ob> might have been destructed during the call.
 *
 *   APPLY_DEFAULT_FOUND: The function was not found, but the call to the
 *       default function succeeded and the arguments on the stack
 *       have been popped and replaced with the result. But note
 *       that <ob> might have been destructed during the call.
 *
 * The function call will swap in the object and also unset its reset status.
 */

{
    if (apply_low(fun, ob, num_arg, b_ign_prot))
        return APPLY_FOUND;

    if (b_use_default)
    {
        /* Check if there is a hook */
        svalue_t * hook = driver_hook + H_DEFAULT_METHOD;

        if (hook->type == T_STRING || hook->type == T_CLOSURE)
        {
            /* We got a default method hook.
             * Now we have to rearrange the stack contents to
             * make space for four more values.
             */
            svalue_t * argp;
            int num_extra = (hook->type == T_STRING) ? 3 : 4;
            int i, rc;

            argp = inter_sp - num_arg + 1;
            for (i = 0; i < num_arg; i++)
                inter_sp[-i+num_extra] = inter_sp[-i];
            inter_sp += num_extra;

            /* The first and second position on the stack will
             * be the return lvalue. The first one is for us,
             * the second one will be for the function call.
             */
            *argp = const0;
            assign_protected_lvalue_no_free(argp+1, argp);

            /* Add the three new arguments: &result, ob, fun
             * to the arguments on the stack.
             */
            if (hook->type == T_CLOSURE)
            {
                put_ref_object(argp+2, ob, "int_apply");
                put_ref_string(argp+3, fun);
            }
            else
                put_ref_string(argp+2, fun);

            /* Call the function */
            if (hook->type == T_STRING)
            {
                rc = apply_low(hook->u.str, ob, num_arg+num_extra-1, b_ign_prot);
            }
            else /* hook->type == T_CLOSURE */
            {
                int_call_lambda(hook, num_arg+num_extra-1, MY_TRUE);
                rc = 1; /* This call obviously succeeds */
            }

            /* Evaluate the result and clean up the stack */
            if (!rc)
            {
                /* Can happen only for T_STRING hooks: Function not found,
                 * but caller expects a clean stack.
                 */
                inter_sp = _pop_n_elems(num_arg+num_extra, inter_sp);
                rc = APPLY_NOT_FOUND;
            }
            else if (inter_sp->type == T_NUMBER
             && inter_sp->u.number == 0)
            {
                /* Default method found, but it denied executing the call.
                 */
                inter_sp--;
                _pop_stack(); /* Remove the result. */
                rc = APPLY_NOT_FOUND;
            }
            else
            {
                /* Default method found and executed.
                 * Remove the call result, then the result value
                 * will be the last remaining value on the stack.
                 */
                _pop_stack();

                /* Remove the lvalue. */
                normalize_svalue(inter_sp, true);
                rc = APPLY_DEFAULT_FOUND;
            }

            /* rc is now the return value from int_apply(), and
             * the result, if any, is on the stack.
             */

            return rc;
        } /* if (hook is STRING or CLOSURE) */

        /* If we come here, there was no suitable default hook to
         * call - remove the arguments.
         */
        inter_sp = _pop_n_elems(num_arg, inter_sp);
    }
    return APPLY_NOT_FOUND;
} /* int_apply() */

/*-------------------------------------------------------------------------*/
void
push_apply_value (void)

/* Push the current <apply_return_value> onto the stack, <apply_return_value>
 * itself free afterwards.
 */

{
    *++inter_sp = apply_return_value;
    apply_return_value.type = T_NUMBER;
}

/*-------------------------------------------------------------------------*/
void
pop_apply_value (void)

/* Pop the current value on the stack into <apply_return_value>, after
 * freeing the latter of course.
 */

{
    free_svalue(&apply_return_value);
    apply_return_value = *inter_sp--;
}

/*-------------------------------------------------------------------------*/
svalue_t *
sapply_int (string_t *fun, object_t *ob, int num_arg
           , Bool b_find_static, Bool b_use_default)

/* Call function <fun> in <ob>ject with <num_arg> arguments pushed
 * onto the stack (<inter_sp> points to the last one). static and protected
 * functions can't be called from the outside unless <b_find_static> is true.
 * sapply() takes care of calling shadows where necessary.
 * If <b_use_default> is true, an unresolved apply may be redirected to
 * a default lfun.
 *
 * sapply() returns a pointer to the function result when the call was
 * successfull, or NULL on failure. The arguments are popped in any case.
 * The result pointer, if returned, points to a static area which will be
 * overwritten with the next sapply().
 *
 * The function call will swap in the object and also unset its reset status.
 *
 * interpret.h defines the macro sapply(fun,ob,num_arg) for the most
 * common call with b_find_static passed as false.
 */

{
#ifdef DEBUG
    svalue_t *expected_sp;
#endif

    /* Handle tracing */
    if (TRACEP(TRACE_APPLY) && TRACE_IS_INTERACTIVE())
    {
        if (!++traceing_recursion)
        {
            do_trace("Apply", "", "\n");
        }
        traceing_recursion--;
    }

#ifdef DEBUG
    expected_sp = inter_sp - num_arg;
#endif

    /* Do the call */
    if (!int_apply(fun, ob, num_arg, b_find_static, b_use_default))
    {
        if (!b_use_default) /* int_apply() did not clean up the stack */
            inter_sp = _pop_n_elems(num_arg, inter_sp);
        return NULL;
    }
    transfer_svalue(&apply_return_value, inter_sp);
    inter_sp--;

#ifdef DEBUG
    if (expected_sp != inter_sp)
        fatal("Corrupt stack pointer: expected %p, got %p.\n"
             , expected_sp, inter_sp);
#endif

    return &apply_return_value;
} /* sapply_int() */

/*-------------------------------------------------------------------------*/
svalue_t *
apply (string_t *fun, object_t *ob, int num_arg)

/* Call function <fun> in <ob>ject with <num_arg> arguments pushed
 * onto the stack (<inter_sp> points to the last one). static and protected
 * functions can't be called from the outside.
 * apply() takes care of calling shadows where necessary.
 *
 * apply() returns a pointer to the function result when the call was
 * successfull, or NULL on failure. The arguments are popped in any case.
 * The result pointer, if returned, points to a static area which will be
 * overwritten with the next apply().
 *
 * The function call will swap in the object and also unset its reset status.
 *
 * The big difference between apply() and sapply() is that apply() sets
 * the tracedepth to 0 before calling the function.
 */

{
    tracedepth = 0;
    return sapply_int(fun, ob, num_arg, MY_FALSE, MY_TRUE);
} /* apply() */

/*-------------------------------------------------------------------------*/
void
secure_apply_error ( svalue_t *save_sp, struct control_stack *save_csp
                   , Bool external)

/* Recover from an error during a secure apply. <save_sp> and <save_csp>
 * are the saved evaluator stack and control stack pointers, saving the
 * state from when secure_apply() was entered.
 *
 * The function pops all the arguments for the call from the stack, and
 * then calls runtime_error() in the master object with the necessary
 * information, unless it is a triple fault - in that case only a
 * debug_message() is generated.
 *
 * If <external> is TRUE, the eval costs and limits will be reset
 * before runtime_error() is called. This is used for top-level master
 * applies which should behave like normal function calls in the error
 * handling.
 */

{
    if (csp != save_csp)
    {
        /* Could be error before push.
         * We have to unroll the control stack in case it references
         * lambda closures.
         */

        while (csp > save_csp+1)
            pop_control_stack();

        previous_ob = csp->prev_ob;
        current_object = csp->ob;
        pop_control_stack();
    }

    if (inter_sp > save_sp)
        inter_sp = _pop_n_elems (inter_sp - save_sp, inter_sp);
        /* Note: On a stack overflow, the stack_overflow() routine
         * already removed the values from the stack
         */

    if (num_error == 3)
    {
        if (!out_of_memory)
        {
            debug_message("%s Master failure: %s", time_stamp()
                         , get_txt(current_error));
            free_mstring(current_error);
            free_mstring(current_error_file);
            free_mstring(current_error_object_name);
            if (current_error_trace)
            {
                free_array(current_error_trace);
                current_error_trace = NULL;
            }
            if (current_error_trace_string)
            {
                free_mstring(current_error_trace_string);
                current_error_trace_string = NULL;
            }
            if (uncaught_error_trace)
            {
                free_array(uncaught_error_trace);
                uncaught_error_trace = NULL;
            }
            if (uncaught_error_trace_string)
            {
                free_mstring(uncaught_error_trace_string);
                uncaught_error_trace_string = NULL;
            }
        }
    }
    else if (!out_of_memory)
    {
        int a;
        object_t *save_cmd;

        push_string(inter_sp, current_error);
        a = 1;
        if (current_error_file)
        {
            push_string(inter_sp, current_error_file);
            push_string(inter_sp, current_error_object_name);
            push_number(inter_sp, current_error_line_number);
            a += 3;
        }

        if (current_heart_beat)
        {
            /* Heartbeat error: turn off the heartbeat in the object
             * and also pass it to RUNTIME_ERROR.
             */

            object_t *culprit;

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
            if (!current_error_file)
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

        if (external)
        {
            mark_end_evaluation();
            CLEAR_EVAL_COST;
            RESET_LIMITS;
        }

        save_cmd = command_giver;
        apply_master(STR_RUNTIME, a);
        command_giver = save_cmd;
        /* STR_RUNTIME freed all the current_ variables, except
         * current_error_trace.
         */
    }
    num_error--;

} /* secure_apply_error() */

/*-------------------------------------------------------------------------*/
svalue_t *
secure_apply_ob (string_t *fun, object_t *ob, int num_arg, Bool external)

/* Aliases:
 *   secure_apply(fun, ob, num_arg) == secure_apply_ob(fun, ob, num_arg, FALSE)
 *   secure_callback(fun, ob, num_arg) == secure_apply_ob(fun, ob, num_arg, TRUE)
 *
 * Call function <fun> in <ob>ject with <num_arg> arguments pushed
 * onto the stack (<inter_sp> points to the last one). static and protected
 * functions can't be called from the outside.
 * secure_apply_ob() takes care of calling shadows where necessary.
 *
 * If <external> is TRUE, it means that this call is due to some external
 * event (like an ERQ message) instead of being caused by a running program.
 * The effect of this flag is that the error handling is like for a normal
 * function call (clearing the eval costs before calling runtime_error()).
 *
 * secure_apply_ob() returns a pointer to the function result when the call
 * was successfull, or NULL on failure. The arguments are popped in any case.
 * The result pointer, if returned, points to a static area which will be
 * overwritten with the next secure_apply_ob().
 *
 * The function call will swap in the object and also unset its reset status.
 *
 * Errors during the execution are caught (this is the big difference
 * to sapply()/apply()) and cause secure_apply_ob() to return NULL.
 */

{
    struct error_recovery_info error_recovery_info;
    svalue_t *save_sp;
    struct control_stack *save_csp;
    svalue_t *result;

    if (ob->flags & O_DESTRUCTED)
        return NULL;

    error_recovery_info.rt.last = rt_context;
    error_recovery_info.rt.type = ERROR_RECOVERY_APPLY;
    rt_context = (rt_context_t *)&error_recovery_info.rt;
    save_sp = inter_sp;
    save_csp = csp;
    if (setjmp(error_recovery_info.con.text))
    {
        secure_apply_error(save_sp - num_arg, save_csp, external);
        result = NULL;
    }
    else
    {
        if (external)
            mark_start_evaluation();
        result = sapply(fun, ob, num_arg);
        if (external)
            mark_end_evaluation();
    }
    rt_context = error_recovery_info.rt.last;
    return result;
} /* secure_apply_ob() */

/*-------------------------------------------------------------------------*/
svalue_t *
apply_master_ob (string_t *fun, int num_arg, Bool external)

/* Aliases:
 *   apply_master(fun, num_arg) == apply_master_ob(fun, num_arg, FALSE)
 *   callback_master(fun, num_arg) == apply_master_ob(fun, num_arg, TRUE)
 *
 * Call function <fun> in the master object with <num_arg> arguments pushed
 * onto the stack (<inter_sp> points to the last one). static and protected
 * functions can be called from the outside. The function takes care
 * of calling shadows where necessary.
 *
 * If <external> is TRUE, it means that this call is due to some external
 * event (like an ERQ message) instead of being caused by a running program.
 * The effect of this flag is that the error handling is like for a normal
 * function call (clearing the eval costs before calling runtime_error()).
 *
 * apply_master_object() returns a pointer to the function result when the
 * call was successfull, or NULL on failure. The arguments are popped in
 * any case.
 * The result pointer, if returned, points to a static area which will be
 * overwritten with the next apply_master_object().
 *
 * The function makes sure that there is a master object to be called. If
 * necessary, a new one is compiled or, failing that, an old one is
 * reactivated.
 *
 * Errors during the execution are caught and cause the function to
 * return NULL.
 *
 * The function operates on an execution tick reserve of MASTER_RESERVED_COST
 * which is used then the normal evaluation cost is already too high.
 */

{
    static int eval_cost_reserve = MASTER_RESERVED_COST;
      /* Available eval_cost reserver. If needed, the reserve is halved
       * for the duration of the apply to establish a protection against
       * an endless recursion of master calls.
       */

    volatile Bool reserve_used = MY_FALSE;

    struct error_recovery_info error_recovery_info;
    svalue_t *save_sp;
    struct control_stack *save_csp;
    svalue_t *result;

    /* Get the master object. */
    assert_master_ob_loaded();

    /* Tap into the eval_cost reserve if the end is near */
    if (   (max_eval_cost && eval_cost > max_eval_cost - MASTER_RESERVED_COST)
        && eval_cost_reserve > 1)
    {
        eval_cost -= eval_cost_reserve;
        assigned_eval_cost -= eval_cost_reserve;
        eval_cost_reserve >>= 1;
        reserve_used = MY_TRUE;
    }

    /* Setup the the error recovery and call the function */
    error_recovery_info.rt.last = rt_context;
    error_recovery_info.rt.type = ERROR_RECOVERY_APPLY;
    rt_context = (rt_context_t *)&error_recovery_info.rt;
    save_sp = inter_sp;
    save_csp = csp;
    if (setjmp(error_recovery_info.con.text))
    {
        secure_apply_error(save_sp - num_arg, save_csp, external);
        printf("%s Error in master_ob->%s()\n", time_stamp(), get_txt(fun));
        debug_message("%s Error in master_ob->%s()\n", time_stamp(), get_txt(fun));
        result = NULL;
    }
    else
    {
        if (external)
            mark_start_evaluation();
        result = sapply_int(fun, master_ob, num_arg, MY_TRUE, MY_FALSE);
        if (external)
            mark_end_evaluation();
    }

    /* Free the reserve if we used it */
    if (reserve_used)
    {
        eval_cost_reserve <<= 1;
        assigned_eval_cost = eval_cost += eval_cost_reserve;
    }
    rt_context = error_recovery_info.rt.last;

    return result;
} /* apply_master_ob() */

/*-------------------------------------------------------------------------*/
void
assert_master_ob_loaded (void)

/* Make sure that there is a master object <master_ob>.
 * If necessary, a new master is compiled, or, failing that, an old
 * destructed one is reactivated. If everything fails, the driver exits.
 *
 * Note that the function may be called recursively:
 *  - While calling a master function from yyparse() (e.g. log_error()),
 *    the master self-destructs and then causes an error.
 *  - Another possibility is that some driver hook invokes some
 *    function that uses apply_master_ob().
 *  - The master object might have been reloaded without noticing that
 *    it is the master. This could happen when there already was a call to
 *    assert_master_ob_loaded(), clearing master_ob, and the master
 *    inherits itself. Partial working self-inheritance is possible if
 *    the H_INCLUDE_DIRS hook does something strange.
 */

{
    static Bool inside = MY_FALSE;
      /* Flag to notice recursive calls */

    static object_t *destructed_master_ob = NULL;
      /* Old, destructed master object */

    int i;

    if (!master_ob || master_ob->flags & O_DESTRUCTED)
    {
        /* The master object has been destructed. Free our reference,
         * and load a new one.
         */
        if (inside || !master_ob)
        {
            object_t *ob;
            object_t *prev;
            Bool newly_removed = MY_FALSE;
              /* TRUE if the old master was on the list of newly
               * destructed objects. That is important to know
               * because then it still has all its variables.
               */

            /* A recursive call while loading the master, or there
             * was no master to begin with.
             * If there is a destructed master, reactivate that
             * one, else stop the driver.
             */

            if (!destructed_master_ob)
            {
                add_message("Failed to load master object '%s'!\n"
                           , master_name);
                // fatal() may call us again. But fatal() and this function
                // are secured against recursion so it should be safe to call
                // it from here (otherwise we would not get a core dump...).
                fatal("Failed to load master object '%s'!\n",
                      master_name);
            }

            /* If we come here, we had a destructed master and failed
             * to load a new one. Now try to reactivate the
             * old one again.
             *
             * We don't have to reactivate any destructed inherits, though:
             * as long as the master references their programs, that's all
             * we need.
             */

            /* First, make sure that there is no half-done object
             * using the masters name.
             */
            if ( NULL != (ob = find_object(master_name_str)) )
            {
                destruct(ob);
            }

            /* Get the destructed master */
            ob = destructed_master_ob;
            destructed_master_ob = NULL;

            /* Remove the destructed master from the list
             * of newly destructed objects or destructed objects.
             */
            if (newly_destructed_objs != NULL)
            {
                if (ob == newly_destructed_objs)
                {
                    newly_destructed_objs = ob->next_all;
                    newly_removed = MY_TRUE;
                    num_newly_destructed--;
#ifdef CHECK_OBJECT_REF
                    {
                        object_shadow_t * sh = newly_destructed_obj_shadows;
                        newly_destructed_obj_shadows = sh->next;
                        xfree(sh);
                    }
#endif /* CHECK_OBJECT_REF */
                }
                else
                {
#ifdef CHECK_OBJECT_REF
                    object_shadow_t *sprev;
#endif /* CHECK_OBJECT_REF */
                    for ( prev = newly_destructed_objs
#ifdef CHECK_OBJECT_REF
                        , sprev = newly_destructed_obj_shadows
#endif /* CHECK_OBJECT_REF */
                        ; prev && prev->next_all != ob
                        ; prev = prev->next_all
#ifdef CHECK_OBJECT_REF
                        , sprev = sprev->next
#endif /* CHECK_OBJECT_REF */
                        ) NOOP;
                    if (prev)
                    {
                        prev->next_all = ob->next_all;
                        newly_removed = MY_TRUE;
                        num_newly_destructed--;
#ifdef CHECK_OBJECT_REF
                        {
                            object_shadow_t *sh = sprev->next;
                            sprev->next = sh->next;
                            xfree(sh);
                        }
#endif /* CHECK_OBJECT_REF */
                    }
                }
            }
            if (!newly_removed && destructed_objs != NULL)
            {
                if (ob == destructed_objs)
                {
                    destructed_objs = ob->next_all;
                    if (destructed_objs)
                        destructed_objs->prev_all = NULL;
                    num_destructed--;
#ifdef CHECK_OBJECT_REF
                    {
                        object_shadow_t * sh = destructed_obj_shadows;
                        destructed_obj_shadows = sh->next;
                        xfree(sh);
                    }
#endif /* CHECK_OBJECT_REF */
                }
                else
                {
#ifdef CHECK_OBJECT_REF
                    object_shadow_t *sprev;
#endif /* CHECK_OBJECT_REF */
                    for ( prev = destructed_objs
#ifdef CHECK_OBJECT_REF
                        , sprev = destructed_obj_shadows
#endif /* CHECK_OBJECT_REF */
                        ; prev && prev->next_all != ob
                        ; prev = prev->next_all
#ifdef CHECK_OBJECT_REF
                        , sprev = sprev->next
#endif /* CHECK_OBJECT_REF */
                        ) NOOP;
                    if (prev)
                    {
                        prev->next_all = ob->next_all;
                        if (prev->next_all)
                            prev->next_all->prev_all = prev;
                        num_destructed--;
#ifdef CHECK_OBJECT_REF
                        {
                            object_shadow_t *sh = sprev->next;
                            sprev->next = sh->next;
                            xfree(sh);
                        }
#endif /* CHECK_OBJECT_REF */
                    }
                }
            }
            ob->flags &= ~O_DESTRUCTED;

            /* Restore the old masters variable space.
             * Remember: as long as the objects are in the 'newly destructed'
             * list, they still have all variables.
             */
            if (!newly_removed && ob->prog->num_variables)
            {
                int save_privilege = malloc_privilege;
                int j;
                svalue_t *v;

                malloc_privilege = MALLOC_SYSTEM;
                ob->variables = v = (svalue_t *)
                    xalloc(sizeof *v * ob->prog->num_variables);
                malloc_privilege = save_privilege;
                for (j = ob->prog->num_variables; --j >= 0; )
                    *v++ = const0;
            }

            /* Reenter the object into the various lists */
            enter_object_hash(ob);
            ob->next_all = obj_list;
            ob->prev_all = NULL;
            if (obj_list)
                obj_list->prev_all = ob;
            obj_list = ob;
            if (!obj_list_end)
                obj_list_end = ob;
            num_listed_objs++;
            ob->super = NULL;
            ob->contains = NULL;
            ob->next_inv = NULL;

            /* Reactivate the old master */
            master_ob = ref_object(ob, "assert_master_ob_loaded");
            if (current_object == &dummy_current_object_for_loads)
                current_object = master_ob;
            push_number(inter_sp, newly_removed);
            sapply_int(STR_REACTIVATE, ob, 1, MY_TRUE, MY_FALSE);
            push_number(inter_sp, 2 - (newly_removed ? 1 : 0));
            sapply_int(STR_INAUGURATE, ob, 1, MY_TRUE, MY_FALSE);
            fprintf(stderr, "%s Old master reactivated.\n", time_stamp());
            inside = MY_FALSE;
            return;

        } /* if (inside || !master_obj) */

        /* A normal call to assert_master_ob_loaded: just load a new one */

        fprintf(stderr, "%s assert_master_ob_loaded: Reloading master '%s'\n"
               , time_stamp(), master_name);
        destructed_master_ob = master_ob;

        /* Clear the pointer, in case the load failed.
         */
        master_ob = NULL;
        inside = MY_TRUE;
        if (!current_object)
        {
            current_object = &dummy_current_object_for_loads;
        }

        /* Free the driver hooks.
         */
        for (i = NUM_DRIVER_HOOKS; i--;)
        {
            assign_svalue(driver_hook+i, &const0);
        }

        init_telopts();

        master_ob = get_object(master_name_str);
        if (current_object == &dummy_current_object_for_loads)
        {
            /* This might be due to the above assignment, or to setting
             * it in the backend.
             */
            current_object = master_ob;
        }

        initialize_master_uid();
        push_number(inter_sp, 3);
        apply_master(STR_INAUGURATE, 1);
        assert_master_ob_loaded();
          /* ...in case inaugurate_master() destructed this object again */
        inside = MY_FALSE;
        ref_object(master_ob, "assert_master_ob_loaded");

        if (destructed_master_ob)
            free_object(destructed_master_ob, "assert_master_ob_loaded");

        fprintf(stderr, "%s Reloading done.\n", time_stamp());
    }

    /* Master exists. Nothing to see here, move along... */

} /* assert_master_ob_loaded() */

/*-------------------------------------------------------------------------*/
void
int_call_lambda (svalue_t *lsvp, int num_arg, Bool external)

/* Call the closure <lsvp> with <num_arg> arguments on the stack. On
 * success, the arguments are replaced with the result, else an errorf()
 * is generated.
 *
 * If <external> is TRUE, the eval_instruction is called to execute the
 * closure. Otherwise inter_pc is just set and int_call_lambda returns
 * (this is only valid for non-alien lfun or lambda closures).
 */

{
#  define CLEAN_CSP \
        previous_ob = csp->prev_ob; \
        current_object = csp->ob; \
        pop_control_stack();
  /* Macro to undo all the call preparations in case the closure
   * can't be called after all.
   */

    svalue_t *sp;
    lambda_t *l = lsvp->u.lambda;

    sp = inter_sp;

    /* Basic setup for the new control frame.
     * If the closure can't be called, all this has to be undone
     * using the macro CLEAN_CSP.
     */
    push_control_stack(sp, inter_pc, inter_fp, inter_context);
    csp->ob = current_object;
    csp->prev_ob = previous_ob;
    csp->num_local_variables = num_arg;
    previous_ob = current_object;

    switch(lsvp->x.closure_type)
    {

    case CLOSURE_LFUN:  /* --- lfun closure --- */
      {
        Bool      extra_frame;

        /* Can't call from a destructed object */
        if (l->ob->flags & O_DESTRUCTED)
        {
            /* inter_sp == sp */
            CLEAN_CSP
            pop_n_elems(num_arg);
            push_number(sp, 0);
            inter_sp = sp;
            return;
        }

        /* Reference the bound and the originating object */
        l->ob->time_of_ref = current_time;
        l->function.lfun.ob->time_of_ref = current_time;
        l->function.lfun.ob->flags &= ~O_RESET_STATE;

        current_object = l->ob;

        /* Can't call a function in a destructed object */
        if (l->function.lfun.ob->flags & O_DESTRUCTED)
        {
            /* inter_sp == sp */
            CLEAN_CSP
            pop_n_elems(num_arg);
            push_number(sp, 0);
            inter_sp = sp;
            return;
        }

        /* Make the objects resident */
        if ( (   current_object->flags & O_SWAPPED
              && load_ob_from_swap(current_object) < 0)
         ||  (   l->function.lfun.ob->flags & O_SWAPPED
              && load_ob_from_swap(l->function.lfun.ob) < 0)
           )
        {
            /* inter_sp == sp */
            CLEAN_CSP
            errorf("Out of memory\n");
            /* NOTREACHED */
            return;
        }

#ifdef DEBUG
        if (l->function.lfun.index >= l->function.lfun.ob->prog->num_functions)
            fatal("Calling non-existing lfun closure #%hu in program '%s' "
                  "with %hu functions.\n"
                 , l->function.lfun.index
                 , get_txt(l->function.lfun.ob->prog->name)
                 , l->function.lfun.ob->prog->num_functions
                );
#endif
          
        /* If the object creating the closure wasn't the one in which
         * it will be executed, we need to record the fact in a second
         * 'dummy' control frame. If we didn't, major security holes
         * open up.
         */

        if (l->ob != l->function.lfun.ob)
        {
            extra_frame = MY_TRUE;
            csp->extern_call = MY_TRUE;
            csp->funstart = NULL;
            push_control_stack(sp, 0, inter_fp, inter_context);
            csp->ob = current_object;
            csp->prev_ob = previous_ob;
            csp->num_local_variables = num_arg;
            previous_ob = current_object;
            external = MY_TRUE;
        }
        else
            extra_frame = MY_FALSE;

        /* Finish the setup of the control frame.
         * This is a real inter-object call.
         */
        csp->extern_call = external;
        current_object = l->function.lfun.ob;
        current_prog = current_object->prog;
        /* inter_sp == sp */
        setup_new_frame(l->function.lfun.index, l->function.lfun.inhProg);
          
        // check arguments
        check_function_args(current_prog->function_headers[FUNCTION_HEADER_INDEX(csp->funstart)].offset.fx, current_prog, csp->funstart);
        if (l->function.lfun.context_size > 0)
            inter_context = l->context;
        if (external)
            eval_instruction(inter_pc, inter_sp);

        /* If l->ob selfdestructs during the call, l might have been
         * deallocated at this point!
         */

        /* If necessary, remove the second control frame */
        if (extra_frame)
        {
            current_object = csp->ob;
            previous_ob = csp->prev_ob;
            pop_control_stack();
        }

        /* The result is on the stack (inter_sp) */
        return;
      }

    case CLOSURE_IDENTIFIER:  /* --- variable closure --- */
      {
        short i; /* the signed variant of lambda_t->function.index */

        CLEAN_CSP  /* no call will be done */

        /* Ignore any arguments passed to a variable closure. */
        pop_n_elems(num_arg);

        /* Don't use variables in a destructed object */
        if (l->ob->flags & O_DESTRUCTED)
        {
            push_number(sp, 0);
            inter_sp = sp;
            return;
        }

        /* Make the object resident */
        if (   (l->ob->flags & O_SWAPPED)
             && load_ob_from_swap(l->ob) < 0
           )
        {
            errorf("Out of memory.\n");
            /* NOTREACHED */
            return;
        }

        /* Do we have the variable? */
        if ( (i = (short)l->function.var_index) < 0)
        {
            errorf("Variable not inherited\n");
            /* NOTREACHED */
            return;
        }

        l->ob->time_of_ref = current_time;
#ifdef DEBUG
        if (!l->ob->variables)
            fatal("%s Fatal: call_lambda on variable for object %p '%s' "
                  "w/o variables, index %d\n"
                 , time_stamp(), l->ob, get_txt(l->ob->name), i);
#endif
        assign_svalue_no_free(++sp, &l->ob->variables[i]);
        inter_sp = sp;
        return;
      }

    case CLOSURE_PRELIMINARY:
        /* no valid current_object: fall out of the switch
         * and let the error handling clean up the control
         * stack.
         */
        break;

    case CLOSURE_BOUND_LAMBDA:  /* --- bound lambda closure --- */
      {
        lambda_t *l2;

        /* Deref the closure and then treat the resulting unbound
         * lambda like a normal lambda
         */
        l2 = l->function.lambda;
        l2->ob = l->ob;
        l = l2;
      }
      /* FALLTHROUGH */

    case CLOSURE_UNBOUND_LAMBDA:
      if (lsvp->x.closure_type == CLOSURE_UNBOUND_LAMBDA)
      {
          if (external)
              break;

          /* Internal call of an unbound closure.
           * Bind it on the fly.
           */
          l->ob = current_object;
      }
      /* FALLTHROUGH */

    case CLOSURE_LAMBDA:
      {
        bytecode_p funstart;

        /* Can't call from a destructed object */
        if (l->ob->flags & O_DESTRUCTED)
        {
            /* inter_sp == sp */
            CLEAN_CSP
            pop_n_elems(num_arg);
            push_number(sp, 0);
            inter_sp = sp;
            return;
        }

        current_object = l->ob;

        /* Make the object resident */
        if (current_object->flags & O_SWAPPED
         && load_ob_from_swap(current_object) < 0)
        {
            /* inter_sp == sp */
            CLEAN_CSP
            errorf("Out of memory\n");
            /* NOTREACHED */
            return;
        }

        /* Reference the object */
        current_object->time_of_ref = current_time;
        current_object->flags &= ~O_RESET_STATE;

        /* Finish the setup */

        current_prog = current_object->prog;
        current_lambda = *lsvp; addref_closure(lsvp, "call_lambda()");
        variable_index_offset = 0;
        function_index_offset = 0;
        funstart = l->function.code.program;
        csp->funstart = funstart;
        csp->extern_call = external;
        sp = setup_new_frame2(funstart, sp, MY_TRUE);

        current_variables = current_object->variables;
        current_strings = current_prog->strings;
        if (external)
            eval_instruction(inter_pc, sp);
        else
            inter_sp = sp;

        /* The result is on the stack (inter_sp). */
        return;
      }

    default: /* --- efun-, simul efun-, operator closure */
      {
        int i;  /* the closure type */

        current_object = lsvp->u.ob;
        /* Can't call from a destructed object */
        if (current_object->flags & O_DESTRUCTED)
        {
            /* inter_sp == sp */
            CLEAN_CSP
            pop_n_elems(num_arg);
            push_number(sp, 0);
            inter_sp = sp;
            return;
        }

        /* Make the object resident */
        if (current_object->flags & O_SWAPPED
         && load_ob_from_swap(current_object) < 0)
        {
            /* inter_sp == sp */
            CLEAN_CSP
            errorf("Out of memory\n");
            /* NOTREACHED */
            return;
        }

        /* Reference the object */
        current_object->time_of_ref = current_time;

        i = lsvp->x.closure_type;
        if (i < CLOSURE_SIMUL_EFUN)
        {
            /* It's an operator or efun */

            if (i == CLOSURE_EFUN + F_UNDEF)
            {
                /* The closure was discovered to be bound to a destructed
                 * object and thus disabled.
                 * This situation should no longer happen - in all situations
                 * the closure should be zeroed out.
                 */
                CLEAN_CSP
                pop_n_elems(num_arg);
                push_number(sp, 0);
                inter_sp = sp;
                return;
            }

#ifdef USE_PYTHON
            if (i >= CLOSURE_PYTHON_EFUN && i < CLOSURE_EFUN)
            {
                inter_pc = csp->funstart = PYTHON_EFUN_FUNSTART;
                csp->instruction = i - CLOSURE_PYTHON_EFUN;
                csp->num_local_variables = 0;

                call_python_efun(i - CLOSURE_PYTHON_EFUN, num_arg);
                CLEAN_CSP
                return;
            }
#endif

            i -= CLOSURE_EFUN;
              /* Efuns have now a positive value, operators a negative one.
               */

            if (i >= 0
             || instrs[i -= CLOSURE_OPERATOR-CLOSURE_EFUN].min_arg)
            {
                /* To call an operator or efun, we have to construct
                 * a small piece of program with this instruction.
                 */
                bytecode_t code[9];    /* the code fragment */
                bytecode_p p;          /* the code pointer */

                int min, max, def;

                min = instrs[i].min_arg;
                max = instrs[i].max_arg;
                p = code;

                /* Fix up the number of arguments passed */
                if (num_arg < min)
                {
                    /* Add some arguments */

                    int f;

                    if (num_arg == min-1
                     && 0 != (def = instrs[i].Default) && def != -1)
                    {
                        /* We lack one argument for which a default
                         * is provided.
                         */
                        if (instrs[def].prefix)
                            *p++ = instrs[def].prefix;
                        *p++ = instrs[def].opcode;
                        max--;
                        min--;
                    }
                    else
                    {
                        /* Maybe there is a fitting replacement efun */
                        f = proxy_efun(i, num_arg);
                        if (f >= 0)
                            /* Yup, use that one */
                            i = f;
                        else
                        {
                            /* Nope. */
                            csp->extern_call = MY_TRUE;
                            inter_pc = csp->funstart = EFUN_FUNSTART;
                            csp->instruction = i;
                            errorf("Too few arguments to %s\n", instrs[i].name);
                        }
                    }
                }
                else if (num_arg > 0xff || (num_arg > max && max != -1))
                {
                    csp->extern_call = MY_TRUE;
                    inter_pc = csp->funstart = EFUN_FUNSTART;
                    csp->instruction = i;
                    errorf("Too many arguments to %s\n", instrs[i].name);
                }

                /* Store the instruction code */
                if (instrs[i].prefix)
                    *p++ = instrs[i].prefix;
                *p++ = instrs[i].opcode;

                /* And finally the return instruction */
                if ( instrs[i].ret_type == lpctype_void )
                    *p++ = F_RETURN0;
                else
                    *p++ = F_RETURN;

                csp->instruction = i;
                csp->funstart = EFUN_FUNSTART;
                csp->num_local_variables = 0;
                inter_fp = sp - num_arg + 1;
                inter_context = NULL;
                tracedepth++; /* Counteract the F_RETURN */
                eval_instruction(code, sp);
                /* The result is on the stack (inter_sp) */
                return;
            }
            else
            {
                /* It is an operator or syntactic marker: fall through
                 * to uncallable closure type.
                 */
                break;
            }
        }
        else
        {
            /* simul_efun */
            object_t *ob;

            /* Mark the call as sefun closure */
            inter_pc = csp->funstart = SIMUL_EFUN_FUNSTART;

            /* Get the simul_efun object */
            if ( !(ob = simul_efun_object) )
            {
                /* inter_sp == sp */
                if (!assert_simul_efun_object()
                 || !(ob = simul_efun_object)
                   )
                {
                    csp->extern_call = MY_TRUE;
                    errorf("Couldn't load simul_efun object\n");
                    /* NOTREACHED */
                    return;
                }
            }
            call_simul_efun(i - CLOSURE_SIMUL_EFUN, ob, num_arg);
            CLEAN_CSP
        }
        /* The result is on the stack (inter_sp) */
        return;
      }

    }

    CLEAN_CSP
    errorf("Uncallable closure\n");
    /* NOTREACHED */
    return;

#   undef CLEAN_CSP
} /* int_call_lambda() */

/*-------------------------------------------------------------------------*/
svalue_t *
secure_call_lambda (svalue_t *closure, int num_arg, Bool external)

/* Aliases:
 *   secure_apply_lambda(fun, num_arg)
 *     == secure_call_lambda(fun, num_arg, FALSE)
 *   secure_callback_lambda(fun, num_arg)
 *     == secure_call_lambda(fun, num_arg, TRUE)
 *
 * Call the closure <closure> with <num_arg> arguments on the stack.
 * On success, the functions returns a pointer to the result in the
 * global apply_return_value, on failure it returns NULL. The arguments are
 * removed in either case.
 *
 * If <external> is TRUE, it means that this call is due to some external
 * event (like an ERQ message) instead of being caused by a running program.
 * The effect of this flag is that the error handling is like for a normal
 * function call (clearing the eval costs before calling runtime_error()).
 *
 * This error recovery is the difference to call_lambda().
 */

{
    struct error_recovery_info error_recovery_info;
    svalue_t *save_sp;
    struct control_stack *save_csp;
    svalue_t *result;

    error_recovery_info.rt.last = rt_context;
    error_recovery_info.rt.type = ERROR_RECOVERY_APPLY;
    rt_context = (rt_context_t *)&error_recovery_info.rt;
    save_sp = inter_sp;
    save_csp = csp;

    if (setjmp(error_recovery_info.con.text))
    {
        secure_apply_error(save_sp - num_arg, save_csp, external);
        result = NULL;
    }
    else
    {
        if (external)
            mark_start_evaluation();
        call_lambda(closure, num_arg);
        transfer_svalue((result = &apply_return_value), inter_sp);
        inter_sp--;
        if (external)
            mark_end_evaluation();
    }
    rt_context = error_recovery_info.rt.last;
    return result;
} /* secure_call_lambda() */

/*-------------------------------------------------------------------------*/
static void
call_simul_efun (unsigned int code, object_t *ob, int num_arg)

/* Call the simul_efun <code> in the sefun object <ob> with <num_arg>
 * arguments on the stack. If it can't be found in the <ob>ject, the
 * function queries the auxiliary sefun objects in <simul_efun_vector>.
 *
 * The function is looked up in the objects by name because its original
 * entry in the simul_efun_table[] has been marked as "discarded".
 *
 * Leave the result on the stack on return.
 */

{
    string_t *function_name;

    function_name = simul_efunp[code].name;

    /* First, try calling the function in the given object */
    if (!int_apply(function_name, ob, num_arg, MY_FALSE, MY_FALSE))
    {
        /* Function not found: try the alternative sefun objects */
        if (simul_efun_vector)
        {
            p_int i;
            svalue_t *v;

            i = VEC_SIZE(simul_efun_vector);
            for (v = simul_efun_vector->item+1 ; ; v++)
            {
                if (--i <= 0 || v->type != T_STRING)
                {
                    errorf("Calling a vanished simul_efun\n");
                    return;
                }
                if ( !(ob = get_object(v->u.str)) )
                    continue;
                if (int_apply(function_name, ob, num_arg, MY_FALSE, MY_FALSE))
                    return;
            }
            return;
        }
        errorf("Calling a vanished simul_efun\n");
        return;
    }
    /*
     * The result of the function call is on the stack.
     */
} /* call_simul_efun() */

/*-------------------------------------------------------------------------*/
void
call_function (program_t *progp, int fx)

/* Call the function <fx> in program <progp> for the current_object.
 * This is done with no frame set up. No arguments are passed,
 * returned values are removed.
 *
 * Right now this function is used just for heartbeats, and the
 * way of calling prevents shadows from being called.
 */

{
    push_control_stack(inter_sp, inter_pc, inter_fp, inter_context);
    csp->ob = current_object;
    csp->prev_ob = previous_ob;
#ifdef DEBUG
    if (csp != CONTROL_STACK)
        fatal("call_function with bad csp\n");
#endif
    csp->num_local_variables = 0;
    current_prog = progp;
    setup_new_frame(fx, NULL);
    previous_ob = current_object;
    tracedepth = 0;
    eval_instruction(inter_pc, inter_sp);
    free_svalue(inter_sp--);  /* Throw away the returned result */
} /* call_function() */

/*-------------------------------------------------------------------------*/
void
call_function_args (object_t* ob, int fx, int num_arg)

/* Call the function <fx> in object <ob> with <num_arg> arguments.
 * The return value will be left on the stack.
 *
 * The function is called directly, no shadows will be considered.
 */

{
    ob->flags &= ~O_RESET_STATE;
    ob->time_of_ref = current_time;
    if (ob->flags & O_SWAPPED)
    {
        if (load_ob_from_swap(ob) < 0)
            errorf("Out of memory\n");
    }

    push_control_stack(inter_sp, inter_pc, inter_fp, inter_context);
    csp->ob = current_object;
    csp->prev_ob = previous_ob;
    csp->num_local_variables = num_arg;

    previous_ob = current_object;
    current_object = ob;
    current_prog = ob->prog;
    setup_new_frame(fx, NULL);

    /* check_function_args might remove frames from the control stack without
     * restoring the current_object.
     */
    previous_ob = csp->prev_ob;
    current_object = csp->ob;
    check_function_args(current_prog->function_headers[FUNCTION_HEADER_INDEX(csp->funstart)].offset.fx, current_prog, csp->funstart);
    previous_ob = current_object;
    current_object = ob;

    eval_instruction(inter_pc, inter_sp);
} /* call_function_args() */

/*-------------------------------------------------------------------------*/
int
get_line_number (bytecode_p p, program_t *progp, string_t **namep)

/* Look up the line number for address <p> within the program <progp>.
 * Result is the line number, and *<namep> is set to the name of the
 * source resp. include file.
 *
 * If the code was generated from an included file, and if the name lengths
 * allow it, the returned name is "<program name> (<include filename>)".
 * In this case, the returned *<namep> points to an untabled string.
 *
 * In either case, the string returned in *<namep> has one reference
 * added.
 *
 * TODO: (an old comment which might no longer be true): This can be done
 * TODO:: much more efficiently, but that change has low priority.)
 */
{
    /* Datastructure to keep track of included files */
    struct incinfo
    {
        string_t *name;         /* Name of parent file */
        struct incinfo *super;  /* Pointer to parent entry */
        int super_line;         /* Line number within parent file */
    };

    p_int offset;          /* (Remaining) program offset to resolve */
    int i;                 /* Current line number */
    include_t *includes;   /* Pointer to the next include info */
    struct incinfo *inctop = NULL;  /* The include information stack. */
    int relocated_from = 0;
    int relocated_to = -1;
    Bool used_system_mem;
      /* TRUE if the line numbers needed SYSTEM privilege to be swapped in,
       * because this means that afterwards they need to be deallocated
       * again.
       */

    if (!progp || !p)
    {
        *namep = ref_mstring(STR_UNDEFINED);
        return 0;
    }

    used_system_mem = MY_FALSE;

    /* Get the line numbers */
    if (!progp->line_numbers)
    {
        if (!load_line_numbers_from_swap(progp))
        {
            /* Uhhmm, out of memory - try to pull some rank */

            int save_privilege;
            Bool rc;

            used_system_mem = MY_TRUE;
            save_privilege = malloc_privilege;
            malloc_privilege = MALLOC_SYSTEM;
            rc = load_line_numbers_from_swap(progp);
            malloc_privilege = save_privilege;
            if (!rc)
            {
                *namep = ref_mstring(STR_UNDEFINED);
                return 0;
            }
        }
    }

    /* Get the offset within the program */
    offset = (p - progp->program);
    if (p < progp->program || p > PROGRAM_END(*progp))
    {
        printf("%s get_line_number(): Illegal offset %"PRIdPINT" in object %s\n"
              , time_stamp(), offset, get_txt(progp->name));
        debug_message("%s get_line_number(): Illegal offset %"PRIdPINT
                      " in object %s\n",
                      time_stamp(), offset, get_txt(progp->name));
        *namep = ref_mstring(STR_UNDEFINED);
        return 0;
    }

    includes = progp->includes;

    /* Decode the line number information until the line number
     * for offset is found. We do this by reading the line byte codes,
     * counting up the line number <i> while decrementing the <offset>.
     * If the offset becomes <= 0, we found the line.
     */
    for (i = 0, p = progp->line_numbers->line_numbers; ; )
    {
        int o;

        o = GET_CODE(p);

        if (o <= 63)  /* 0x00..0x3F */
        {
            if (o >= LI_MAXOFFSET)  /* 0x3b..0x3f */
            {
                if (o != LI_MAXOFFSET)
                {
                    switch (o)
                    {

                    case LI_BACK:
                      {
                        unsigned int off;

                        p++;
                        off = GET_CODE(p);
                        i -= off+1;
                        break;
                      }

                    case LI_INCLUDE:
                      {
                        /* Included file: push the information */

                        struct incinfo *inc_new;

                        /* Find the next include which generated code.
                         * We know that there is one.
                         */
                        while (includes->depth < 0) includes++;

                        i++;
                        inc_new = xalloc(sizeof *inc_new);
                        /* TODO: What if this fails? */
                        inc_new->name = includes->filename;
                        includes++;
                        inc_new->super = inctop;
                        inc_new->super_line = i;
                        inctop = inc_new;
                        i = 0;
                        break;
                      }

                    case LI_INCLUDE_END:
                      {
                        /* End of include: retrieve old position */

                        struct incinfo *inc_old;

                        inc_old = inctop;
                        i = inc_old->super_line;
                        inctop = inc_old->super;
                        xfree(inc_old );
                        break;
                      }

                    case LI_L_RELOCATED:
                      {
                        int h, l;

                        p++;
                        h = GET_CODE(p);
                        p++;
                        l = GET_CODE(p);
                        i -= 2;
                        relocated_to = i;
                        relocated_from = relocated_to - ((h << 8) + l);
                        p++; /* skip trailing LI_L_RELOCATED */
                        break;
                      }
                    }
                }
                else /* 0x3c */
                {
                    offset -= o;
                }
            }
            else  /* 0x00..0x3b */
            {
                offset -= o;
                i++;
                if (offset <= 0)
                    break;
            }
        }
        else if (o <= 127)  /* 0x40..0x7f */
        {
            /* Simple entry: count offset and lines */
            offset -= (o&7) + 1;
            i += (o>>3) - 6;
            if (offset <= 0)
                break;
        }
        else if (o >= 256-LI_MAXEMPTY)  /* 0xE0 .. 0xFF */
        {
            i += 256-o;
        }
        else /* 0x80 .. 0xDF */
        {
            i -= 2;
            relocated_from = (relocated_to = i) - (o - LI_RELOCATED);
        }

        /* Get the next line number bytecode */
        p++;
    } /* line number search */

    if (i == relocated_to + 1)
        i = relocated_from + 1;
        /* Perform the announced relocation */

    /* Here, i is the line number, and if inctop is not NULL, the
     * code originates from the included file pointed to by inctop.
     * In either case, set *<namep> to the pointer to the name
     * of the file.
     */

    if (inctop)
    {
        /* The code was included */

        string_t * namebuf;

        namebuf = alloc_mstring(mstrsize(inctop->name) + mstrsize(progp->name)
                                                       + 3);
        if (namebuf)
        {
            sprintf(get_txt(namebuf), "%s (%s)"
                           , get_txt(progp->name), get_txt(inctop->name));
            *namep = namebuf;
        }
        else
        {
            /* No memory for the new string - improvise */
            *namep = ref_mstring(inctop->name);
        }

        /* Free the include stack structures */
        do {
            struct incinfo *inc_old;

            inc_old = inctop;
            inctop = inc_old->super;
            xfree(inc_old);
        } while (inctop);
    }
    else
    {
        /* Normal code */

        *namep = ref_mstring(progp->name);
    }

    if (used_system_mem)
    {
        /* We used SYSTEM priviledged memory - now we have to return it.
         */

        total_prog_block_size -= progp->line_numbers->size;
        total_bytes_unswapped -= progp->line_numbers->size;
        xfree(progp->line_numbers);
        progp->line_numbers = NULL;
        reallocate_reserved_areas();
    }

    /* Return the line number */
    return i;
} /* get_line_number() */

/*-------------------------------------------------------------------------*/
int
get_line_number_if_any (string_t **name)

/* Look up the line number for the current execution address.
 * Result is the line number, and *<name> is set to the name of the
 * source resp. include file.
 *
 * The function recognizes sefun and lambda closures, the latter return
 * the approximate position offset of the offending instruction within
 * the closure.
 *
 * *<name> may point to an untabled string; and in any case has its
 * own reference.
 */

{
    if (csp >= &CONTROL_STACK[0] && csp->funstart == SIMUL_EFUN_FUNSTART)
    {
        *name = ref_mstring(STR_SEFUN_CLOSURE);
        return 0;
    }

    if (csp >= &CONTROL_STACK[0] && csp->funstart == EFUN_FUNSTART)
    {
        static char buf[256];
        char *iname;

        iname = instrs[csp->instruction].name;
        if (iname)
        {
            buf[sizeof buf - 1] = '\0';
            buf[0] = '#';
            buf[1] = '\'';
            strcpy(buf+2, iname);
            if (buf[sizeof buf - 1] != '\0')
                fatal("interpret:get_line_number_if_any(): "
                      "buffer overflow.\n");
            memsafe(*name = new_unicode_mstring(buf), strlen(buf), "instruction name");
        }
        else
            *name = ref_mstring(STR_EFUN_CLOSURE);

        return 0;
    }

#ifdef USE_PYTHON
    if (csp >= &CONTROL_STACK[0] && csp->funstart == PYTHON_EFUN_FUNSTART)
    {
        static char buf[256] = "#'";
        const char *iname = closure_python_efun_to_string(csp->instruction + CLOSURE_PYTHON_EFUN);

        strncpy(buf + 2, iname, sizeof(buf) - 2);
        buf[sizeof(buf)-1] = 0;
        memsafe(*name = new_unicode_mstring(buf), strlen(buf), "python efun name");

        return 0;
    }
#endif

    if (current_prog)
    {
        if (csp->funstart < current_prog->program
         || csp->funstart > PROGRAM_END(*current_prog))
        {
            static char name_buffer[32];
            string_t * location, *tmp;
            lambda_t * l;

            sprintf(name_buffer, "<lambda %6p>", csp->funstart);
            memsafe(*name = new_mstring(name_buffer, STRING_ASCII), strlen(name_buffer)
                   , "lambda name");
            /* Find the beginning of the lambda structure.*/
            l = (lambda_t *)( (PTRTYPE)(csp->funstart)
                             - offsetof(lambda_t, function.code.program));

            location = closure_location(l);

            tmp = mstr_add(*name, location);
            if (tmp)
            {
                free_mstring(*name);
                *name = tmp;
            }
            free_mstring(location);
            return inter_pc - csp->funstart - 2;
        }
        return get_line_number(inter_pc, current_prog, name);
    }
  
    *name = ref_mstring(STR_EMPTY);
    return 0;
} /* get_line_number_if_any() */

/*-------------------------------------------------------------------------*/
string_t *
collect_trace (strbuf_t * sbuf, vector_t ** rvec )

/* Collect the traceback for the current (resp. last) function call, starting
 * from the first frame.
 *
 * If <sbuf> is not NULL, traceback is written in readable form into the
 * stringbuffer <sbuf>.
 *
 * If <rvec> is not NULL, the traceback is returned in a newly created array
 * which pointer is put into *<rvec>. For the format of the array, see
 * efun driver_info().
 *
 * If a heart_beat() is involved, return an uncounted pointer to the name of
 * the object that had it, otherwise return NULL.
 */

{
    struct control_stack *p;  /* Control frame under inspection */
    string_t *ret = NULL;     /* Uncounted ref to object name */
    bytecode_p pc = inter_pc;
    int line = 0;
    string_t *name;           /* Uncounted ref to function name */
    string_t *file;           /* Counted ref to most recent file name */
    object_t *ob = NULL;
    bytecode_p last_catch = NULL;  /* Last found catch */

    /* Temporary structure to hold the tracedata before it is condensed
     * into the result array.
     */
    struct traceentry {
        vector_t          * vec;
        struct traceentry * next;
    } *first_entry, *last_entry;
    size_t num_entries;

#ifdef EVAL_COST_TRACE
#define PUT_EVAL_COST(var, cost) \
        put_number(var->vec->item+TRACE_EVALCOST, cost);
#else
#define PUT_EVAL_COST(var, cost)
#endif

#define NEW_ENTRY(var, type, progname, cost) \
        struct traceentry * var; \
        var = alloca(sizeof(*var)); \
        if (!var) \
            errorf("Stack overflow in collect_trace()"); \
        var->vec = allocate_array_unlimited(TRACE_MAX); \
        var->next = NULL; \
        if (!first_entry) \
            first_entry = last_entry = var; \
        else { \
            last_entry->next = var; \
            last_entry = var; \
        } \
        num_entries++; \
        put_number(var->vec->item+TRACE_TYPE, type); \
        put_ref_string(var->vec->item+TRACE_PROGRAM, progname); \
        put_ref_string(entry->vec->item+TRACE_OBJECT, ob->name); \
	PUT_EVAL_COST(var, cost)

#define PUT_LOC(entry, val) \
        put_number(entry->vec->item+TRACE_LOC, (p_int)(val))

    first_entry = last_entry = NULL;
    num_entries = 0;

    if (!current_prog)
    {
        if (sbuf)
           strbuf_addf(sbuf, "%s\n", get_txt(STR_NO_PROG_TRACE));
        if (rvec)
        {
            vector_t * vec;

            vec = allocate_array_unlimited(1);
            put_ref_string(vec->item, STR_NO_PROG_TRACE);
            *rvec = vec;
        }
        return NULL;
    }

    if (csp < &CONTROL_STACK[0])
    {
        if (sbuf)
           strbuf_addf(sbuf, "%s\n", get_txt(STR_NO_TRACE));
        if (rvec)
        {
            vector_t * vec;

            vec = allocate_array_unlimited(1);
            put_ref_string(vec->item, STR_NO_TRACE);
            *rvec = vec;
        }
        return NULL;
    }

    /* Loop through the call stack.
     * The organisation of the control stack results in the information
     * for this frame (p[0]) being stored in the next (p[1]).
     * Confused now? Good.
     */
    file = ref_mstring(STR_EMPTY);
    p = &CONTROL_STACK[0];
    do {
        bytecode_p  dump_pc;  /* the frame's pc */
        program_t  *prog;     /* the frame's program */
#ifdef EVAL_COST_TRACE
        int32       dump_eval_cost; /* The eval cost at that frame. */
#endif

        /* Note: Under certain circumstances the value of file carried over
         * from the previous iteration is reused in this one.
         */

        if (p->extern_call)
        {
            /* Find the next extern_call and set <ob> to the
             * then-current object for all the coming frames.
             */
            struct control_stack *q = p;
            for (;;) {
                if (++q > csp)
                {
                    ob = current_object;
                    break;
                }
                if (q->extern_call)
                {
                    ob = q->ob;
                    break;
                }
            }
            last_catch = NULL;
        }

        /* Retrieve pc and program from the stack */
        if (p == csp)
        {
            dump_pc = pc;
            prog = current_prog;
#ifdef EVAL_COST_TRACE
            dump_eval_cost = eval_cost;
#endif
        }
        else
        {
            dump_pc = p[1].pc;
            prog = p[1].prog;
#ifdef EVAL_COST_TRACE
            dump_eval_cost = p[1].eval_cost;
#endif
        }

        /* Use some heuristics first to see if it could possibly be a CATCH.
         * The pc should point at a F_END_CATCH instruction, or at a LBRANCH
         * to that instruction.
         */
        if (p > &CONTROL_STACK[0] && p->funstart == p[-1].funstart)
        {
            bytecode_p pc2 = p->pc;

            if (!pc2)
                goto not_catch;  /* shouldn't happen... */

            if (GET_CODE(pc2) == F_LBRANCH)
            {
                short offset;
                pc2++;
                GET_SHORT(offset, pc2);
                if (offset <= 0)
                    goto not_catch;
                pc2 += offset;
            }

            if (pc2 - p->funstart < 1)
                goto not_catch;

            if (GET_CODE(pc2-1) != F_END_CATCH)
            {
                goto not_catch;
            }

            if (last_catch == pc2)
                goto not_catch;
            last_catch = pc2;
            name = STR_CATCH;
            if (file)
                free_mstring(file);
            file = NULL;
            line = 0;
            goto name_computed;
        }

not_catch:  /* The frame does not point at a catch here */

        /* Efun symbol? */
        if (!prog || !dump_pc)
        {
            /* TODO: See comments in call_lambda(): this code should never be reached.
             * TODO: this is reached - when handling the dummy control frame int_call_lambda() pushes.
             * TODO::But this handling might be changed, because it is not an efun symbol?
             */
            if (sbuf)
#ifndef EVAL_COST_TRACE
                strbuf_addf(sbuf, "<function symbol> in '%20s' ('%20s')\n"
#else
                strbuf_addf(sbuf, "%8d <function symbol> in '%20s' ('%20s')\n"
                           , dump_eval_cost
#endif
                           , get_txt(ob->prog->name), get_txt(ob->name));
            if (rvec)
            {
                NEW_ENTRY(entry, TRACE_TYPE_SYMBOL, ob->prog->name, dump_eval_cost);
            }
            continue;
        }

        /* simul_efun closure? */
        if (p[0].funstart == SIMUL_EFUN_FUNSTART)
        {
            if (sbuf)
                strbuf_addf( sbuf
#ifndef EVAL_COST_TRACE
                           , "<simul_efun closure> bound to '%20s' ('%20s')\n"
#else
                           , "%8d <simul_efun closure> bound to '%20s' ('%20s')\n"
                           , dump_eval_cost
#endif
                           , get_txt(ob->prog->name), get_txt(ob->name));
            if (rvec)
            {
                NEW_ENTRY(entry, TRACE_TYPE_SEFUN, ob->prog->name, dump_eval_cost);
            }
            continue;
        }

        /* efun closure? */
        if (p[0].funstart == EFUN_FUNSTART)
        {
            char * iname;

            iname = instrs[p[0].instruction].name;
            if (iname)
            {
                if (sbuf)
#ifndef EVAL_COST_TRACE
                    strbuf_addf(sbuf, "#\'%-14s for '%20s' ('%20s')\n"
#else
                    strbuf_addf(sbuf, "%8d #\'%-14s for '%20s' ('%20s')\n"
                               , dump_eval_cost
#endif
                               , iname, get_txt(ob->prog->name)
                               , get_txt(ob->name));
                if (rvec)
                {
                    string_t *tmp;

                    NEW_ENTRY(entry, TRACE_TYPE_EFUN, ob->prog->name, dump_eval_cost);
                    memsafe(tmp = new_mstring(iname, STRING_ASCII), strlen(iname)
                           , "instruction name");
                    put_string(entry->vec->item+TRACE_NAME, tmp);
                }
            }
            else
            {
                if (sbuf)
#ifndef EVAL_COST_TRACE
                    strbuf_addf( sbuf, "<efun closure %d> for '%20s' ('%20s')\n"
#else
                    strbuf_addf( sbuf, "%8d <efun closure %d> for '%20s' ('%20s')\n"
                               , dump_eval_cost
#endif
                               , p[0].instruction, get_txt(ob->prog->name)
                               , get_txt(ob->name));
                if (rvec)
                {
                    NEW_ENTRY(entry, TRACE_TYPE_EFUN, ob->prog->name, dump_eval_cost);
                    put_number(entry->vec->item+TRACE_NAME, p[0].instruction);
                }
            }
            continue;
        }

        /* Lambda closure? */
        if (p[0].funstart < prog->program
         || p[0].funstart > PROGRAM_END(*prog))
        {
            if (sbuf)
                strbuf_addf( sbuf
#ifndef EVAL_COST_TRACE
                           , "<lambda 0x%6lx> in '%20s' ('%20s') offset %ld\n"
#else
                           , "%8d <lambda 0x%6lx> in '%20s' ('%20s') offset %ld\n"
                           , dump_eval_cost
#endif
                           , (long)p[0].funstart
                           , get_txt(ob->prog->name)
                           , get_txt(ob->name)
                           , (long)(dump_pc - p[0].funstart)
                           );
            if (rvec)
            {
                NEW_ENTRY(entry, TRACE_TYPE_LAMBDA, ob->prog->name, dump_eval_cost);
                put_number(entry->vec->item+TRACE_NAME, (p_int)p[0].funstart);
                PUT_LOC(entry, (dump_pc - p[0].funstart));
            }
            continue;
        }

        /* Nothing of the above: a normal program */
        if (file)
            free_mstring(file);
        line = get_line_number(dump_pc, prog, &file);
        name = prog->function_headers[FUNCTION_HEADER_INDEX(p[0].funstart)].name;

name_computed: /* Jump target from the catch detection */

        /* Print the name and line */

        if (mstreq(name, STR_HEART_BEAT) && p != csp)
            ret = p->extern_call ? (p->ob ? p->ob->name : NULL) : ob->name;

        if (sbuf)
        {
            if (file != NULL)
#ifndef EVAL_COST_TRACE
                strbuf_addf(sbuf, "'%15s' in '%20s' ('%20s') line %d\n"
#else
                strbuf_addf(sbuf, "%8d '%15s' in '%20s' ('%20s') line %d\n"
                           , dump_eval_cost
#endif
                           , get_txt(name), get_txt(file)
                           , get_txt(ob->name), line);
            else
#ifndef EVAL_COST_TRACE
                strbuf_addf(sbuf, "'%15s' in %22s ('%20s')\n"
#else
                strbuf_addf(sbuf, "%8d '%15s' in %22s ('%20s')\n"
                           , dump_eval_cost
#endif
                           , get_txt(name), "", get_txt(ob->name));
        }

        if (rvec)
        {
            NEW_ENTRY(entry, TRACE_TYPE_LFUN, file != NULL ? file : STR_EMPTY, dump_eval_cost);
            put_ref_string(entry->vec->item+TRACE_NAME, name);
            PUT_LOC(entry, line);
        }

    } while (++p <= csp);

    if (file)
        free_mstring(file);


    /* Condense the singular entries into the result array */
    if (rvec)
    {
        vector_t * vec;
        size_t ix;

        vec = allocate_array_unlimited(num_entries+1);

        if (ret)
            put_ref_string(vec->item, ret);

        for (ix = 1; first_entry != NULL; ix++, first_entry = first_entry->next)
        {
            put_array(vec->item+ix, first_entry->vec);
        }

        *rvec = vec;
    }

    /* Done */
    return ret;

#undef NEW_ENTRY
#undef PUT_LOC

} /* collect_trace() */

/*-------------------------------------------------------------------------*/
string_t *
dump_trace (Bool how, vector_t ** rvec, string_t ** rstr)

/* Write out a traceback, starting from the first frame. If a heart_beat()
 * is involved, return (uncounted) the name of the object that had it.
 *
 * If <how> is FALSE (the normal case), the trace is written with
 * debug_message() only. If <how> is TRUE (used for internal errors), the
 * trace is also written to stdout.
 *
 * If TRACE_CODE is defined and <how> is true, the last executed
 * instructions are printed, too.
 *
 * If <rvec> is not NULL, the traceback is returned in a newly created array
 * which pointer is put into *<rvec>. For the format of the array, see
 * efun driver_info().
 * If <rstr> is not NULL, the traceback is put as a string into *<rstr>.
 */

{
    strbuf_t sbuf;
    string_t *hb_obj_name;

    strbuf_zero(&sbuf);
    hb_obj_name = collect_trace(&sbuf, rvec);

    /* Print the last instructions if required */
#ifdef TRACE_CODE
    if (how) {
        /* TODO: This number of instructions should be a runtime arg */
#ifdef DEBUG
        (void)last_instructions(200, MY_TRUE, NULL);
        if (inter_pc)
            printf("%6p: %3d %3d %3d %3d %3d %3d %3d %3d\n"
                  , inter_pc
                  , inter_pc[0], inter_pc[1], inter_pc[2], inter_pc[3]
                  , inter_pc[4], inter_pc[5], inter_pc[6], inter_pc[7] );
        else
            printf("No program counter.\n");
#else  /* DEBUG */
        last_instructions(20, MY_TRUE, NULL);
#endif /* DEBUG */
    }
#endif /* TRACE_CODE */

    /* Print the trace */
    if (how)
        fputs(sbuf.buf, stdout);
    debug_message("%s", sbuf.buf);
    if (rstr)
        *rstr = new_unicode_mstring(sbuf.buf);

    /* Cleanup and return */
    strbuf_free(&sbuf);

    return hb_obj_name;
} /* dump_trace() */

/*-------------------------------------------------------------------------*/
void
invalidate_apply_low_cache (void)

/* Called in the (unlikely) case that all programs had to be renumbered,
 * this invalidates the call cache.
 */

{
    int i;
  
    for (i = 0; i < CACHE_SIZE; i++)
    {
        cache[i].id = 0;
        if (cache[i].name)
        {
            free_mstring(cache[i].name);
            cache[i].name = NULL;
        }
    }
}


/*-------------------------------------------------------------------------*/
size_t
interpreter_overhead (void)

/* Return the amount of memory allocated for the interpreter.
 * Right now, there is none.
 */

{
    size_t sum;

    sum = 0;

    return sum;
} /* interpreter_overhead() */


#ifdef GC_SUPPORT

/*-------------------------------------------------------------------------*/
void
clear_interpreter_refs (void)

/* GC Support: Clear the interpreter references.
 */

{
#ifdef TRACE_CODE
    {
        int i;

        for (i = TOTAL_TRACE_LENGTH; --i >= 0; )
        {
            object_t *ob;

            if (NULL != (ob = previous_objects[i])
             && ob->flags & O_DESTRUCTED
             && ob->ref
               )
            {
                ob->ref = 0;
                ob->prog->ref = 0;
                clear_program_ref(ob->prog, MY_FALSE);
            }
        }
    }
#endif
} /* clear_interpreter_refs() */

/*-------------------------------------------------------------------------*/
void
count_interpreter_refs (void)

/* GC Support: Count/mark all interpreter held structures.
 */

{
    int i;

    for (i = CACHE_SIZE; --i>= 0; ) {
        if (cache[i].name)
            count_ref_from_string(cache[i].name);
    }
#ifdef TRACE_CODE
    for (i = TOTAL_TRACE_LENGTH; --i >= 0; )
    {
        object_t *ob;

        if ( NULL != (ob = previous_objects[i]) )
        {
            if (ob->flags & O_DESTRUCTED)
            {
                previous_objects[i] = NULL;
                previous_instruction[i] = 0;
                reference_destructed_object(ob);
            }
            else
            {
                ob->ref++;
            }
        }
    }
#endif
}
/*-------------------------------------------------------------------------*/

#endif /* GC_SUPPORT */

/*=========================================================================*/

/*                            D E B U G G I N G                            */

/*-------------------------------------------------------------------------*/
#ifdef OPCPROF
Bool
opcdump (string_t * fname)

/* Print the usage statistics for the opcodes into the file <fname>.
 * Return TRUE on success, FALSE if <fname> can't be written.
 */

{
    int i;
    FILE *f;
    char *native;

    fname = check_valid_path(fname, current_object, STR_OPCDUMP, MY_TRUE);
    if (!fname)
        return MY_FALSE;

    native = convert_path_str_to_native_or_throw(fname);
    f = fopen(native, "w");
    if (!f)
        return MY_FALSE;
    FCOUNT_WRITE(native);


    for(i = 0; i < MAXOPC; i++)
    {
        if (opcount[i])
#ifdef VERBOSE_OPCPROF
            fprintf(f,"%d: \"%-16s\" %6d\n",i, get_f_name(i), opcount[i]);
#else
            fprintf(f,"%d: %d\n", i, opcount[i]);
#endif
    }
    fclose(f);

    return MY_TRUE;
}
#endif /* OPCPROF */


#ifdef TRACE_CODE

/*-------------------------------------------------------------------------*/
static char *
get_arg (int a)

/* Return the argument for the instruction at previous_pc[<a>] as a string.
 * If there is no argument, return "".
 *
 * Helper function for last_instructions().
 */

{
    static char buff[12];
    bytecode_p from, to;
    int b;

    b = (a+1) % TOTAL_TRACE_LENGTH;
    from = previous_pc[a];
    to = previous_pc[b];

    if (to - from < 2)
        return "";

    if (to - from == 2)
    {
        snprintf(buff, sizeof(buff), "%d", GET_CODE(from+1));
        return buff;
    }

    if (to - from == 3)
    {
        short arg;

        GET_SHORT(arg, from+1);
        snprintf(buff, sizeof(buff), "%hd", arg);
        return buff;
    }

    if (to - from == 5)
    {
        int32 arg = get_uint32(from+1);

        snprintf(buff, sizeof(buff), "%"PRId32, arg);
        return buff;
    }

    return "";
} /* get_arg() */

/*-------------------------------------------------------------------------*/
static void
last_instr_output (char *str, svalue_t **svpp)

/* <svpp> == NULL: print string <str>
 * <svpp> != NULL: store a copy of <str> as string-svalue to *<svpp>, then
 *                 increment *<svpp>
 *
 * Helper function to last_instructions() to either print strings for
 * a tracedump, or to push them onto the evaluator stack for the efun
 * last_instructions().
 */

{
    if (svpp)
    {
        string_t *s;
        memsafe(s = new_unicode_mstring(str), strlen(str), "copy of instruction name");
        put_string((*svpp), s);
        (*svpp)++;
    }
    else
    {
        fputs(str, stdout);
        putc('\n', stdout);
    }
} /* last_instr_output() */

/*-------------------------------------------------------------------------*/
static Bool
program_referenced (program_t *prog, program_t *prog2)

/* Return TRUE if <prog2> inherits <prog>.
 *
 * Auxiliary function to last_instructions().
 */

{
    inherit_t *inh;
    int i;

    if (prog == prog2)
        return MY_TRUE;

    /* If a prog2 is swapped out, it can't have prog inherited
     * and swapped in.
     */
    if (P_PROG_SWAPPED(prog2))
        return MY_FALSE;

    /* Recursively test the inherits */
    for (i = prog2->num_inherited, inh = prog2->inherit; --i >= 0; inh++)
    {
        if (program_referenced(prog, inh->prog))
            return MY_TRUE;
    }

    return MY_FALSE;
}

/*-------------------------------------------------------------------------*/
static Bool
program_exists (program_t *prog, object_t *guess)

/* Test if <prog> exists - either by itself or as inherited program.
 * Start testing with the program of <guess>, if it is not there,
 * test all objects in the list.
 *
 * Auxiliary function to last_instructions().
 */

{
    if (program_referenced(prog, guess->prog))
        return MY_TRUE;

    for (guess = obj_list; guess; guess = guess->next_all)
    {
#ifdef DEBUG
        if (guess->flags & O_DESTRUCTED)  /* TODO: Can't happen */
            continue;
#endif
        if (program_referenced(prog, guess->prog))
            return MY_TRUE;
    }

    return MY_FALSE;
}

/*-------------------------------------------------------------------------*/
int
last_instructions (int length, Bool verbose, svalue_t **svpp)

/* 'Print' a dump of the <length> last instructions. If <svpp> is NULL,
 * all the data is printed, else *<svpp> points to the evaluator stack
 * and all the 'printed' lines are pushed onto the stack using *<svpp>
 * as pointer.
 *
 * If <verbose> is true, more information is printed.
 *
 * Return the index for the last executed instruction.
 *
 * This function is called from dump_trace() and f_last_instructions().
 */

{
    int i;
    object_t *old_obj;
    char buf[400];
    string_t *old_file;
    int old_line, line = 0;

    old_obj = NULL;
    old_file = NULL;
    old_line = 0;
    i = (last - length + TOTAL_TRACE_LENGTH) % TOTAL_TRACE_LENGTH;

    /* Walk through the instructions.
     * Instructions with value 0 are not used yet, or have been
     * removed while cleaning up destructed objects.
     */
    do {
        i = (i + 1) % TOTAL_TRACE_LENGTH;
        if (previous_instruction[i] != 0)
        {
            if (verbose)
            {
                string_t *file;
                program_t *ppr;
                bytecode_p ppc;

                ppr = previous_programs[i];
                ppc = previous_pc[i]+1;
                if (!program_exists(ppr, previous_objects[i]))
                {
                    file = ref_mstring(STR_PROG_DEALLOCATED);
                    line = 0;
                }
                else if (ppc < ppr->program || ppc > PROGRAM_END(*ppr))
                {
                    file = ref_mstring(STR_UNKNOWN_LAMBDA);
                    line = 0;
                }
                else
                {
                    line = get_line_number(ppc, ppr, &file);
                }

                if (previous_objects[i] != old_obj
                 || (old_file && !mstreq(file, old_file))
                   )
                {
                    snprintf(buf, sizeof(buf), "%.170s %.160s line %d"
                               , get_txt(previous_objects[i]->name)
                               , get_txt(file), line
                    );
                    last_instr_output(buf, svpp);
                    old_obj = previous_objects[i];
                    if (old_file)
                        free_mstring(old_file);
                    old_file = ref_mstring(file);
                }

                if (file)
                    free_mstring(file);
            }
            snprintf(buf, sizeof(buf)-40, "%6p: %3d %8s %-26s (%td:%3td)"
                   , previous_pc[i]
                   , previous_instruction[i] /* instrs.h has these numbers */
                   , get_arg(i)
                   , get_f_name(previous_instruction[i])
                   , (stack_size[i] + 1)
                   , (abs_stack_size[i])
            );
            if (verbose && line != old_line)
                snprintf(buf + strlen(buf), 40, "\tline %d", old_line = line);
            last_instr_output(buf, svpp);
        }
    } while (i != last);

    if (old_file)
        free_mstring(old_file);

    return last;
} /* last_instructions() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_last_instructions (svalue_t *sp)

/* EFUN last_instructions()
 *
 *   string *last_instructions (int length, int verbose)
 *
 * Return an array showing the 'length' last executed
 * instructions in disassembled form. If 'verbose' is non-zero
 * (the default), line number information are also included.
 * Each string is built as this:
 *
 *   Opcode-Address: Opcode Operand Mnemonic (Stackdepth) Linenumber
 *
 * The Stackdepth information consists of two numbers <rel>:<abs>:
 * <rel> is the relative stack usage in this function, <abs> is the
 * absolute stack usage.
 *
 * The linenumber information is appended if requested and a new
 * source line is reached. Also, calls between objects produce a
 *
 *   Objectname Programname Linenumber
 *
 * entry in the resulting array (in verbose mode only).
 *
 * There is a preconfigured upper limit for the backtrace.
 */

{
    vector_t *v, *v2;
    mp_int num_instr, size;
    svalue_t *svp;

    /* Test the arguments */
    num_instr = sp[-1].u.number;
    if (num_instr <= 0)
        errorf("Illegal number of instructions: %"PRIdMPINT".\n", num_instr);

    sp--;
    inter_sp = sp; /* Out of memory possible */
    if (num_instr > TOTAL_TRACE_LENGTH)
        num_instr = TOTAL_TRACE_LENGTH;

    /* Allocate the result vector */
    size = sp[1].u.number ? num_instr << 1 : num_instr;
    v = allocate_array(size);

    /* Enter the vector into the stack for now, so that it will be
     * freed when an out of memory error occurs.
     */
    put_array(sp, v);
    svp = v->item;
    last_instructions(num_instr, sp[1].u.number != 0, &svp);

    /* If we allocated the vector to big, get a shorter one and copy
     * the data.
     */
    if (svp - v->item < size)
    {
        size = svp - v->item;
        v2 = allocate_array(size);
        memcpy(v2->item, v->item, size * sizeof *svp);
        sp->u.vec = v2;
        free_empty_vector(v);
    }

    return sp;
} /* f_last_instructions() */

/*-------------------------------------------------------------------------*/

#endif /* TRACE_CODE */

/*-------------------------------------------------------------------------*/
int control_stack_depth (void)
  /* Returns the number of frames on the control stack. Can be used to estimate
   * the still available stack depth in recursive code.
   */
{
    return (csp - CONTROL_STACK) + 1; 
} /* control_stack_depth() */

/*-------------------------------------------------------------------------*/
static INLINE int
caller_stack_depth(void)
/* static helper function for f_caller_stack_depth() and f_caller_stack() for
 * calculating the stack depth. It is a separate function because the code
 * is used at two places and the compiler will probably inline it anyway.
 */
{
  int depth;
  Bool done;
  struct control_stack *p;
  
  /* Determine the depth of the call stack */
  p = csp;
  for (depth = 0, done = MY_FALSE; ; depth++)
  {
    do {
      if (p == CONTROL_STACK)
      {
        done = MY_TRUE;
        break;
      }
    } while ( !(--p)[1].extern_call );
    if (done)
      break;
  }
  
  return depth;
} /* caller_stack_depth() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_caller_stack_depth (svalue_t *sp)
/* EFUN caller_stack_depth()
 *
 *   int caller_stack_depth(void)
 *
 * Returns the number of previous objects on the stack. This
 * can be used for security checks.
 */

{
    push_number(sp, caller_stack_depth());

    return sp;
} /* f_caller_stack_depth() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_caller_stack (svalue_t *sp)

/* EFUN caller_stack()
 *
 *   object *caller_stack()
 *   object *caller_stack(int add_interactive)
 *
 * Returns an array of the previous_object()s who caused the
 * call_other() to this_object().  previous_object(i) equals
 * caller_stack()[i].

 * If you pass the optional argument <add_interactive> (as true
 * value), this_interactive() (or 0 if not existing) is appended
 * to the array.
 */

{
    int depth, i;
    Bool done;
    struct control_stack *p;
    vector_t *v;
    svalue_t *svp;

    /* Determine the depth of the call stack */
    depth = caller_stack_depth();
  
    /* Allocate and fill in the result array */
    v = allocate_uninit_array(depth + (sp->u.number ? 1 : 0));
    p = csp;
    for (i = 0, svp = v->item, done = MY_FALSE; i < depth; i++, svp++)
    {
        object_t *prev;
        do {
            if (p == CONTROL_STACK)
            {
                done = MY_TRUE;
                break;
            }
        } while ( !(--p)[1].extern_call);

        /* Break if end of stack */
        if (done)
            break;

        /* Get 'the' calling object */
        if (p[1].extern_call & CS_PRETEND)
            prev = p[1].pretend_to_be;
        else
            prev = p[1].ob;

        /* Enter it into the array */
        if (prev == NULL || prev->flags & O_DESTRUCTED)
            put_number(svp, 0);
        else
            put_ref_object(svp, prev, "caller_stack");
    }

#ifdef DEBUG
    if (i < depth)
    {
        errorf("Computed stack depth to %d, but found only %d objects\n"
             , depth, i);
        /* NOTREACHED */
        return sp;
    }
#endif

    /* If so desired, add the interactive object */
    if (sp->u.number)
    {
        if ( current_interactive
         && !(current_interactive->flags & O_DESTRUCTED))
        {
            put_ref_object(svp, current_interactive, "caller_stack");
        }
        else
            put_number(svp, 0);
    }

    /* Assign the result and return */
    put_array(sp, v);

    return sp;
} /* f_caller_stack() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_previous_object (svalue_t *sp)

/* EFUN previous_object()
 *
 *   object previous_object(int i)
 *
 * Follow back the last <i> call_other()s and return the calling
 * object (i.e. previous_object(2) returns the caller of the
 * caller). It must hold 1 <= i < caller_stack_depth().
 * Any value of i < 1 is treated as i == 1.
 *
 * There is an important special case: in functions called by the
 * gamedriver in reaction to some external event (e.g. commands
 * added by add_action), previous_object() will return
 * this_object(), but previous_object(1) will return 0.
 */

{
    int i;
    struct control_stack *p;
    object_t *prev_ob;

    /* Test the arguments */
    i = sp->u.number;
    if (i > MAX_TRACE) {
        sp->u.number = 0;
        return sp;
    }

    /* Set p back to the <i>th extern call */
    p = csp;
    do {
        do {
            if (p == CONTROL_STACK) {
                sp->u.number = 0;
                return sp;
            }
        } while ( !(--p)[1].extern_call );
    } while (--i >= 0);

    /* Determine the object and push it */
    if (p[1].extern_call & CS_PRETEND)
        prev_ob = p[1].pretend_to_be;
    else
        prev_ob = p[1].ob;

    if (!prev_ob || prev_ob->flags & O_DESTRUCTED)
        sp->u.number = 0;
    else
        put_ref_object(sp, prev_ob, "previous_object");

    return sp;
} /* f_previous_object() */



#ifdef DEBUG

/*-------------------------------------------------------------------------*/
void
count_inherits (program_t *progp)

/* Check Refcounts: Increment the extra_ref of all programs inherited
 * by <progp>. If one of those programs has not been visited yet,
 * its extra_ref is set to 1 and this function is called recursively.
 *
 * If check_..._search_prog is set and equal to one of the inherited
 * programs, a notice is printed.
 */

{
    int i;
    program_t *progp2;

    /* Clones will not add to the ref count of inherited progs */
    for (i = 0; i < progp->num_inherited; i++)
    {
        progp2 = progp->inherit[i].prog;
        progp2->extra_ref++;
        if (progp2 == check_a_lot_ref_counts_search_prog)
            printf("%s Found prog, inherited by %s, new total ref %"
                   PRIdPINT"\n",
                   time_stamp(), get_txt(progp->name), progp2->ref);
        if (NULL == register_pointer(ptable, progp2))
        {
            continue;
        }
        progp2->extra_ref = 1;
        if (progp2->blueprint)
        {
            count_extra_ref_in_object(progp2->blueprint);
        }
        count_inherits(progp2);
    }
} /* count_inherits() */

/*-------------------------------------------------------------------------*/
static void
count_extra_ref_in_mapping_filter ( svalue_t *key, svalue_t *data
                                  , void * extra)

/* Count the extra refs for <key> and the associated <data>. <extra>
 * is a mp_int giving the number of data values.
 */

{
    count_extra_ref_in_vector(key, 1);
    count_extra_ref_in_vector(data, (size_t)extra);
}

/*-------------------------------------------------------------------------*/
static void
check_extra_ref_in_mapping_filter (svalue_t *key, svalue_t *data
                                  , void * extra)

/* Check the extra refs for <key> and the associated <data>. <extra>
 * is a mp_int giving the number of data values.
 */

{
    check_extra_ref_in_vector(key, 1);
    check_extra_ref_in_vector(data, (size_t)extra);
}

static void
count_extra_ref_in_prog (program_t *prog)
/* Count extra refs for <prog>.
 */
{
    if (NULL != register_pointer(ptable, prog))
    {
        prog->extra_ref = 1;
        if (prog->blueprint)
        {
            count_extra_ref_in_object(prog->blueprint);
        }
        count_inherits(prog);
    }
}

/*-------------------------------------------------------------------------*/
void
count_extra_ref_in_object (object_t *ob)

/* Count the extra refs for object <ob>. If the object has been visited
 * before, extra_ref is just incremented. Otherwise, extra_ref is
 * set to 1 and all depending refs are counted.
 *
 * If check_..._search_prog is set and matches the objects program,
 * a notice is printed.
 */

{
    int was_swapped = MY_FALSE;

    ob->extra_ref++;
    if ( NULL == register_pointer(ptable, ob) )
    {
        return;
    }

    ob->extra_ref = 1;
    if ( !O_PROG_SWAPPED(ob) )
    {
        ob->prog->extra_ref++;
        if (ob->prog == check_a_lot_ref_counts_search_prog)
            printf("%s Found program for object %s\n", time_stamp()
                  , get_txt(ob->name));
    }

    /* Clones will not add to the ref count of inherited progs */
    if (O_PROG_SWAPPED(ob))
    {
         if (load_ob_from_swap(ob) < 0)
            debug_message( "%s check-refcounts: Program for '%s' can't be "
                           "swapped in - extra refcounts may be wrong.\n"
                         , time_stamp(), get_txt(ob->name));
         else
             was_swapped = MY_TRUE;
    }

    if (!O_PROG_SWAPPED(ob))
    {
        count_extra_ref_in_prog(ob->prog);
    }

    if (was_swapped)
        swap_program(ob);
} /* count_extra_ref_in_object() */

/*-------------------------------------------------------------------------*/
static void
count_extra_ref_in_closure (lambda_t *l, ph_int type)

/* Count the extra refs in the closure <l> of type <type>.
 */

{
    if (CLOSURE_HAS_CODE(type))
    {
        /* We need to count the extra_refs in the constant values. */

        mp_int num_values;
        svalue_t *svp;

        svp = (svalue_t *)l;
        num_values = l->function.code.num_values;

        svp -= num_values;
        count_extra_ref_in_vector(svp, (size_t)num_values);
    }
    else
    {
        /* Count the referenced closures and objects */
        if (type == CLOSURE_BOUND_LAMBDA)
        {
            lambda_t *l2 = l->function.lambda;

            if (NULL != register_pointer(ptable, l2) )
                count_extra_ref_in_closure(l2, CLOSURE_UNBOUND_LAMBDA);
        }
        else if (type == CLOSURE_LFUN)
        {
            count_extra_ref_in_object(l->function.lfun.ob);
            if (l->function.lfun.inhProg)
            {
                l->function.lfun.inhProg->extra_ref++;
                count_extra_ref_in_prog(l->function.lfun.inhProg);
            }
        }
    }

    if (type != CLOSURE_UNBOUND_LAMBDA)
    {
        count_extra_ref_in_object(l->ob);
    }
    
    if (l->prog_ob)
    {
        count_extra_ref_in_object(l->prog_ob);
    }
} /* count_extra_ref_in_closure() */

/*-------------------------------------------------------------------------*/
void
count_extra_ref_in_vector (svalue_t *svp, size_t num)

/* Count the extra_refs of all <num> values starting at <svp>.
 */

{
    svalue_t *p;

    if (!svp)
        return;

    for (p = svp; p < svp+num; p++)
    {
        switch(p->type)
        {

        case T_CLOSURE:
            if (CLOSURE_MALLOCED(p->x.closure_type))
            {
                lambda_t *l;

                l = p->u.lambda;
                if ( NULL == register_pointer(ptable, l) )
                    continue;
                count_extra_ref_in_closure(l, p->x.closure_type);
                continue;
            }
            /* FALLTHROUGH */

        case T_OBJECT:
          {
            count_extra_ref_in_object(p->u.ob);
            continue;
          }

        case T_QUOTED_ARRAY:
        case T_POINTER:
            p->u.vec->extra_ref++;
            if (NULL == register_pointer(ptable, p->u.vec) )
                continue;
            p->u.vec->extra_ref = 1;
            count_extra_ref_in_vector(&p->u.vec->item[0], VEC_SIZE(p->u.vec));
            continue;

        case T_STRUCT:
            if (NULL == register_pointer(ptable, p->u.strct) )
                continue;
            count_extra_ref_in_vector(&p->u.strct->member[0], struct_size(p->u.strct));
            continue;

        case T_MAPPING:
            if (NULL == register_pointer(ptable, p->u.map) ) continue;
            walk_mapping(
              p->u.map,
              count_extra_ref_in_mapping_filter,
              (void *)p->u.map->num_values
            );
            continue; /* no extra ref count implemented */
        }
    }
} /* count_extra_ref_in_vector() */

/*-------------------------------------------------------------------------*/
static void
check_extra_ref_in_vector (svalue_t *svp, size_t num)

/* Check the extra_refs of the <num> values starting at <svp>
 */

{
    svalue_t *p;

    if (!svp)
        return;

    for (p = svp; p < svp+num; p++)
    {
        switch(p->type)
        {
        case T_QUOTED_ARRAY:
        case T_POINTER:
            if (NULL == register_pointer(ptable, p->u.vec) )
                continue;
            check_extra_ref_in_vector(&p->u.vec->item[0], VEC_SIZE(p->u.vec));
            p->u.vec->extra_ref = 0;
            continue;

        case T_STRUCT:
            if (NULL == register_pointer(ptable, p->u.strct) )
                continue;
            check_extra_ref_in_vector(&p->u.strct->member[0], struct_size(p->u.strct));
            continue;

        case T_MAPPING:
            if (NULL == register_pointer(ptable, p->u.map) ) continue;
            walk_mapping(
              p->u.map,
              check_extra_ref_in_mapping_filter,
              (void *)((p_int)p->u.map->num_values)
            );
            continue; /* no extra ref count implemented */
        }
    }
} /* check_extra_ref_in_vector() */

/*-------------------------------------------------------------------------*/
void
check_a_lot_ref_counts (program_t *search_prog)

/* Loop through every object and variable in the game and check all
 * reference counts. This will surely take some time and should be
 * used only for debugging.
 *
 * If <search_prog> is set, the function will just count the references
 * and print the information for the given program, if found.
 *
 * The function must be called after removing all destructed objects.
 *
 * TODO: No extra_refs implemented in mappings.
 * TODO: Put this code somewhere else.
 */

{
    object_t *ob;

    check_a_lot_ref_counts_search_prog = search_prog;

    /* Pass 1: Compute the ref counts.
     *
     * The pointer table keeps track of objects already visited,
     * eliminating the need for a separate pass to clear the
     * ref counts.
     */
    ptable = new_pointer_table();
    if (!ptable)
    {
        debug_message("%s Out of memory while checking all refcounts.\n"
                     , time_stamp());
        return;
    }

    /* List of all objects.
     */
    for (ob = obj_list; ob; ob = ob->next_all)
    {
        if (ob->flags & O_DESTRUCTED)
        {
            /* This shouldn't happen 
             * TODO: remove check? enclose in #ifdef DEBUG? */
            debug_message("%s Found destructed object '%s' where it shouldn't "
                          "be.\n", time_stamp(), get_txt(ob->name));
            continue;
        }
        if (O_VAR_SWAPPED(ob))
            load_ob_from_swap(ob);
        count_extra_ref_in_vector(ob->variables, (size_t)ob->extra_num_variables);
        count_extra_ref_in_object(ob);
    }

    if (master_ob)
        master_ob->extra_ref++;

    if (d_flag > 3)
    {
        debug_message("%s obj_list evaluated\n", time_stamp());
    }

    /* The current stack.
     */
    count_extra_ref_in_vector(VALUE_STACK, (size_t)(inter_sp - VALUE_STACK + 1));
    if (d_flag > 3)
    {
        debug_message("%s stack evaluated\n", time_stamp());
    }


    /* Other variables and lists.
     */
    count_extra_ref_from_call_outs();
    count_extra_ref_from_wiz_list();
    count_simul_efun_extra_refs(ptable);
    count_comm_extra_refs();
#ifdef USE_PYTHON
    count_python_extra_refs();
#endif

#ifdef TRACE_CODE
    {
        int j;

        for (j = TOTAL_TRACE_LENGTH; --j >= 0; )
        {
            if ( NULL != (ob = previous_objects[j]) )
            {
                count_extra_ref_in_object(ob);
            }
        }
    }
#endif

    count_extra_ref_in_vector(&indexing_quickfix, 1);
    null_vector.extra_ref++;
    count_extra_ref_in_vector(driver_hook, NUM_DRIVER_HOOKS);

    /* Done with the counting */
    free_pointer_table(ptable);

    /* Was that all for this time? */
    if (search_prog)
        return;

    /* Pass 3: Check the ref counts.
     *
     * The (new) pointer table is used as before.
     */
    ptable = new_pointer_table();
    if (!ptable)
    {
        debug_message("%s Out of memory while checking all refcounts.\n"
                     , time_stamp());
        return;
    }

    for (ob = obj_list; ob; ob = ob->next_all) {
        if (ob->flags & O_DESTRUCTED)  /* shouldn't happen */
            continue;

        if (ob->ref != ob->extra_ref)
        {
             debug_message("%s Bad ref count in object %s: listed %"
                           PRIdPINT" - counted %"PRIdPINT"\n"
                          , time_stamp(), get_txt(ob->name)
                          , ob->ref, ob->extra_ref);
        }
        else if ( !(ob->flags & O_SWAPPED) )
        {
            if (ob->prog->ref != ob->prog->extra_ref)
            {
                /* an inheriting file might be swapped */
                if (time_to_swap + 1 > 0
                 && ob->prog->ref > ob->prog->extra_ref)
                {
                    debug_message("%s high ref count in prog %s: "
                                  "listed %"PRIdPINT" - counted %"PRIdPINT"\n"
                                 , time_stamp()
                                 , get_txt(ob->prog->name), ob->prog->ref
                                 , ob->prog->extra_ref);
                }
                else
                {
                    check_a_lot_ref_counts(ob->prog);
                    debug_message("%s Bad ref count in prog %s: "
                                  "listed %"PRIdPINT" - counted %"PRIdPINT"\n"
                                 , time_stamp()
                                 , get_txt(ob->prog->name)
                                 , ob->prog->ref, ob->prog->extra_ref);
                }
            }
        } /* !SWAPPED */
        check_extra_ref_in_vector(ob->variables, ob->extra_num_variables);
    } /* for */

    check_extra_ref_in_vector(VALUE_STACK, (size_t)(inter_sp - VALUE_STACK + 1));

    free_pointer_table(ptable);
} /* check_a_lot_of_ref_counts() */

/*-------------------------------------------------------------------------*/

#endif /* DEBUG */


/*=========================================================================*/

/*                             E F U N S                                   */

/*-------------------------------------------------------------------------*/
/* (Re)define a couple a macros for the efuns below
 */

#undef ERROR
#define ERROR(s) {inter_sp = sp; errorf(s);}

/*-------------------------------------------------------------------------*/
static int
expand_argument (svalue_t *sp)

/* Expands the argument <sp> on the stack.
 * If <sp> is an array or array range, remove it from the stack
 * and push its elements on the stack. Return the number of
 * elements pushed (return 1, if <sp> stayed unchanged).
 *
 * If it's an array lvalue or array range lvalue, pass its
 * elements also as lvalues. If it's an unprotected (non-array)
 * lvalue, make it protected.
 */

{
    vector_t *vec = NULL;
    svalue_t *val = sp;
    p_int start = 0, size = 0;
    bool make_ref = false;

    while (val->type == T_LVALUE)
    {
        switch(val->x.lvalue_type)
        {
            default:
                fatal("(expand_argument) Illegal lvalue %p type %d\n", val, val->x.lvalue_type);
                /* NOTREACHED */
                break;

            case LVALUE_UNPROTECTED:
            {
                svalue_t *dest = val->u.lvalue;
                if (dest->type != T_POINTER)
                {
                    push_protected_lvalue(val);
                    break;
                }

                vec = dest->u.vec;
                start = 0;
                size = VEC_SIZE(vec);
                make_ref = true;
                break;
            }

            case LVALUE_UNPROTECTED_CHAR:
                push_protected_lvalue(val);
                break;

            case LVALUE_UNPROTECTED_RANGE:
                if (current_unprotected_range.vec->type == T_STRING || current_unprotected_range.vec->type == T_BYTES)
                    break;
                else if (current_unprotected_range.vec->type != T_POINTER)
                {
                    fatal("(expand_argument) Illegal range %p type %d\n", val, current_unprotected_range.vec->type);
                    break;
                }

                vec = current_unprotected_range.vec->u.vec;
                start = current_unprotected_range.index1;
                size = current_unprotected_range.index2 - current_unprotected_range.index1;
                make_ref = true;
                break;

            case LVALUE_UNPROTECTED_MAPENTRY:
                /* Non-existent entry cannot be an array. */
                push_protected_lvalue(val);
                break;

            case LVALUE_PROTECTED:
            {
                struct protected_lvalue *l = val->u.protected_lvalue;

                if (l->val.type != T_POINTER) /* Nothing to do for non-arrays, pass it as-is. */
                    break;

                vec = l->val.u.vec;
                start = 0;
                size = VEC_SIZE(vec);
                make_ref = true;
                break;
            }

            case LVALUE_PROTECTED_CHAR:
                /* Nothing to do... */
                break;

            case LVALUE_PROTECTED_RANGE:
            {
                struct protected_range_lvalue *r = val->u.protected_range_lvalue;

                if (r->vec.type == T_STRING || r->vec.type == T_BYTES)
                    break;
                else if (r->vec.type != T_POINTER)
                {
                    fatal("(expand_argument) Illegal range %p type %d\n", r, r->vec.type);
                    break;
                }

                vec = r->vec.u.vec;
                start = r->index1;
                size = r->index2 - r->index1;
                make_ref = true;
                break;
            }

            case LVALUE_PROTECTED_MAPENTRY:
            {
                /* Let's see, whether an array appeared at that entry. */
                struct protected_mapentry_lvalue *e = val->u.protected_mapentry_lvalue;
                svalue_t *entry = get_map_value(e->map, &(e->key));

                if (entry == &const0)
                    break;
                if (entry[e->index].type == T_LVALUE || entry[e->index].type == T_POINTER)
                {
                    val = entry + e->index;
                    continue;
                }
                break;
            }
        } /* switch */
        break;
    }

    if (val->type == T_POINTER)
    {
        vec = val->u.vec;
        start = 0;
        size = VEC_SIZE(vec);
        make_ref = false;
    }

    if (vec != NULL)
    {
        /* The last argument is an array: flatten it */
        svalue_t *svp;  /* pointer into the array */
        int i = size;

        /* Check if we would overflowing the stack. */
        if (size + (sp - VALUE_STACK) >= EVALUATOR_STACK_SIZE)
            errorf("VM Stack overflow: %zu too high.\n",
                 (size_t)(size + (sp - VALUE_STACK) - EVALUATOR_STACK_SIZE) );

        /* Push the array elements onto the stack, overwriting the
         * array value itself.
         */
        ref_array(vec);
        free_svalue(sp--);

        if (deref_array(vec))
        {
            /* The array continues to live, copy the values. */
            for (svp = vec->item + start; --i >= 0; svp++)
            {
                if (make_ref)
                    assign_protected_lvalue_no_free(++sp, svp);
                else
                    assign_svalue_no_free(++sp, svp);
            }
        }
        else
        {
            /* The array will be freed, so use a faster function */
            for (svp = vec->item + start; --i >= 0; svp++)
            {
                /* For references, we keep them, but we don't
                 * create one here, because the array will go...
                 */
                if (make_ref)
                    transfer_svalue_no_free(++sp, svp);
                else
                    transfer_rvalue_no_free(++sp, svp);
            }
            free_empty_vector(vec);
        }

        return size;
    }
    else
        return 1;

} /* expand_argument() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_apply (svalue_t *sp, int num_arg)

/* EFUN apply()
 *
 *     mixed apply(mixed|closure cl, ...)
 *
 * Call the closure <cl> and pass it all the extra arguments
 * given in the call. If the last argument is an array, it
 * is flattened, ie. passed as a bunch of single arguments.
 *
 * If it's an array lvalue or array range lvalue, pass its
 * elements also as lvalues.
 *
 * TODO: Use the MudOS-Notation '(*f)(...)' as alternative.
 */

{
    svalue_t *args;

    args = sp -num_arg +1;

    if (args->type != T_CLOSURE)
    {
        /* Not a closure: pop the excess args and return <cl>
         * as result.
         */

        while (--num_arg)
            free_svalue(sp--);
        return sp;
    }

    /* Check the target closure type */
    if (args->x.closure_type >= 0 &&
        args->x.closure_type != CLOSURE_LFUN &&
        args->x.closure_type != CLOSURE_LAMBDA &&
        args->x.closure_type != CLOSURE_BOUND_LAMBDA)
            errorf("Uncallable closure in apply().\n");


    /* Expand the last argument. */
    num_arg += expand_argument(sp) - 1;
    sp = args + num_arg - 1;

    /* No external calls may be done when this object is
     * destructed.
     */
    if (current_object->flags & O_DESTRUCTED)
    {
        sp = _pop_n_elems(num_arg, sp);
        push_number(sp, 0);
        inter_sp = sp;
        warnf("Call from destructed object '%s' ignored.\n"
             , get_txt(current_object->name));
        return sp;
    }

    inter_sp = sp;


    /* Call the closure and push the result.
     * Note that the closure might destruct itself.
     */
    call_lambda(args, num_arg - 1);
    sp = args;
    free_svalue(sp);
    *sp = sp[1];

    return sp;
} /* v_apply() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_funcall (svalue_t *sp, int num_arg)

/* EFUN funcall()
 *
 *   mixed funcall(mixed|closure cl, mixed arg ...)
 *
 * Evaluates the closure. The extra args will be passed as args
 * to the closure. If cl is not a closure, it will simply be
 * returned.
 */

{
    svalue_t *args;

    args = sp -num_arg +1;

    if (args->type == T_CLOSURE)
    {
        /* No external calls may be done when this object is
         * destructed.
         */
        if (current_object->flags & O_DESTRUCTED) {
            sp = _pop_n_elems(num_arg, sp);
            push_number(sp, 0);
            inter_sp = sp;
            warnf("Call from destructed object '%s' ignored.\n"
                 , get_txt(current_object->name));
            return sp;
        }

        /* Call the closure and push the result.
         * Note that the closure might destruct itself.
         */
        call_lambda(args, num_arg - 1);
        sp = args;
        free_svalue(sp);
        *sp = sp[1];
    }
    else
    {
        /* Not a closure: pop the excess args and return <cl>
         * as result.
         */

        while (--num_arg)
            free_svalue(sp--);
    }

    return sp;
} /* v_funcall() */

/*-------------------------------------------------------------------------*/
static svalue_t *
int_call_other (bool b_use_default, enum call_other_error_handling error_handling, char* efunname, svalue_t *sp, int num_arg)

/* EFUN call_other(), call_direct(),
 *      call_resolved(), call_direct_resolved(),
 *      call_strict(), call_direct_strict()
 *
 *   unknown call_other(object|object* ob, string fun, mixed arg, ...)
 *   unknown call_direct(object|object* ob, string fun, mixed arg, ...)
 *   int|int* call_resolved(mixed & result, object|object* ob, string func, ...)
 *   int|int* call_direct_resolved(mixed & result, object|object* ob, string func, ...)
 *   unknown call_strict(object|object* ob, string func, ...)
 *   unknown call_direct_strict(object|object* ob, string func, ...)
 *
 * Implementation of calls to other functions. If ob->func() is defined and
 * publicly accessible, any of the optional extra arguments are passed to
 * ob->func(...). If error_handling is CO_RESULT, then the result of that
 * function call is stored in result, which must be passed by reference,
 * otherwise it is returned directly.
 *
 * If the current object is already destructed, or the ob does not
 * exist, or ob does not define a public accessible function named
 * func, then the default method will be executed if b_use_default is true.
 * If b_use_default is false or the default method is not successful,
 * the handling is defined by error_handling. For CO_IGNORE and CO_RESULT
 * 0 is returned, CO_ERROR will raise an error in this case. In a successful
 * call CO_IGNORE and CO_ERROR will return the function result,
 * CA_RESULT will return 1 (or -1 if the default method was called).
 *
 * ob can also be a file_name. If a string is passed for ob, and
 * no object with that name does exist, an error occurs.
 *
 * ob can also be given as an array. In this case the results will
 * be returned as arrays as well. When having CO_ERROR an error will
 * be raised if at least one call failed.
 */

{
    svalue_t *arg;
    object_t *ob;
    int rc, call_num_arg;

    arg = sp - num_arg + 1;
    call_num_arg = num_arg - 2;

    /* Ignore the result lvalue for now, so we can treat all variants alike. */
    if (error_handling == CO_RESULT)
    {
        arg++;
        call_num_arg--;
    }

    /* Test the arguments */
    if (get_txt(arg[1].u.str)[0] == ':')
        errorf("Illegal function name in %s: %s\n",
              get_txt(arg[1].u.str), efunname);

    /* No external calls may be done when this object is
     * destructed.
     */
    if (current_object->flags & O_DESTRUCTED)
    {
        if (error_handling == CO_ERROR)
            errorf("%s() from destructed object.\n", efunname);

        sp = _pop_n_elems(num_arg, sp);
        push_number(sp, 0);
        inter_sp = sp;
        warnf("Call from destructed object '%s' ignored.\n"
             , get_txt(current_object->name));
        return sp;
    }

    if (arg[0].type != T_POINTER)
    {
        /* --- The normal call other to a single object --- */

        if (arg[0].type == T_NUMBER)
        {
            if (error_handling == CO_ERROR)
                errorf("%s() to destructed object.\n", efunname);

            sp = _pop_n_elems(num_arg, sp);
            push_number(sp, 0);
            inter_sp = sp;
            warnf("Call to destructed object ignored.\n");
            return sp;
        }
        else if (arg[0].type == T_OBJECT)
            ob = arg[0].u.ob;
        else /* it's a string */
        {
            ob = get_object(arg[0].u.str);
            if (!ob)
                errorf("%s() failed: can't get object '%s'.\n"
                     , efunname
                     , get_txt(arg[0].u.str));
        }

        /* Handle traceing. */
        if (TRACEP(TRACE_CALL_OTHER) && TRACE_IS_INTERACTIVE())
        {
            if (!++traceing_recursion)
            {
                inter_sp = sp;
                do_trace("Call other ", get_txt(arg[1].u.str), "\n");
            }
            traceing_recursion--;
        }

        /* Send the remaining arguments to the function.
         */
        if (ob == master_ob)
            b_use_default = MY_FALSE;
        rc = int_apply(arg[1].u.str, ob, call_num_arg, MY_FALSE, b_use_default);
        if (rc == APPLY_NOT_FOUND)
        {
            /* Function not found */
            if (error_handling == CO_ERROR)
            {
                errorf("Function %.50s() not found in %s.\n"
                     , get_txt(arg[1].u.str)
                     , get_txt(ob->name));
            }

            if (b_use_default)
                sp -= call_num_arg;
            else
                sp = _pop_n_elems(call_num_arg, sp);
            sp = _pop_n_elems(2, sp);

            if (error_handling == CO_RESULT)
                free_svalue(sp--);
            push_number(sp, 0);
            return sp;
        }

        /* The result of the function call is on the stack. But, so
         * is the function name and object that was called.
         * These have to be removed.
         */
        sp = inter_sp;
        if (error_handling == CO_RESULT)
        {
            transfer_svalue(arg-1, sp--);   /* Copy the function call result */
            sp = _pop_n_elems(2, sp);       /* Remove old arguments to call_solved */
            free_svalue(sp);                /* Free the lvalue */
            put_number(sp, rc == APPLY_FOUND ? 1 : -1);
            return sp;
        }
        else
        {
            _pop_n_elems(2, sp-1);
            transfer_svalue_no_free(arg, sp);
            return arg;
        }
    }
    else /* arg[0].type == T_POINTER */
    {
        /* --- The other call other to an array of objects --- */

        svalue_t *svp, *rcp;
        size_t    size;

        /* The array with the objects will also hold the results.
         * For that, it mustn't be shared, therefore we create a
         * copy if necessary.
         */
        size = VEC_SIZE(arg->u.vec);
        if (arg->u.vec->ref != 1 && size != 0)
        {
            vector_t *vec;
            svalue_t *to;

            vec = allocate_array_unlimited(size);
            if (!vec)
                errorf("Out of memory.\n");
            for (svp = arg->u.vec->item, to = vec->item
                ; size != 0
                ; size--, svp++, to++)
                assign_svalue_no_free(to, svp);
            free_array(arg->u.vec);
            arg->u.vec = vec; /* adopts the reference */
        }

        if (error_handling == CO_RESULT)
        {
            /* For call_resolved we need a second array holding
             * return value.
             */
            vector_t *vec = allocate_array_unlimited(size);
            if (!vec)
                errorf("Out of memory.\n");

            push_array(sp, vec);
            rcp = vec->item;

            inter_sp = sp;
        }
        else
            rcp = arg->u.vec->item;

        /* Now loop over the array of objects and call the function
         * in each of it. For that, the arguments are duly replicated
         * for every call.
         */
        size = VEC_SIZE(arg->u.vec);
        svp = arg->u.vec->item;
        for ( ; size != 0; size--, svp++, rcp++)
        {
            int i;
            bool use_default_for_ob;

            svalue_t * item = get_rvalue(svp, NULL);
            if(!item)
                item = svp;

            assign_eval_cost_inl();
            inter_sp = sp; /* Might be clobbered from previous loop */

            if (item->type == T_OBJECT)
                ob = item->u.ob;
            else if (item->type == T_STRING)
            {
                ob = get_object(item->u.str);
                if (ob == NULL)
                {
                    errorf("%s() failed: can't get object '%s'\n"
                         , efunname
                         , get_txt(item->u.str));
                    /* NOTREACHED */
                    continue;
                }
            }
            else if (item->type == T_NUMBER && item->u.number == 0)
            {
                if (error_handling == CO_ERROR)
                    errorf("%s() to destructed object at index %"PRIdMPINT".\n"
                         , efunname
                         , (mp_int)(svp - arg->u.vec->item));

                /* Write 0 as a result. */
                free_svalue(svp);
                put_number(svp, 0);
                put_number(rcp, 0);
                continue;
            }
            else
                errorf("Bad arg for %s() at index %"PRIdMPINT": "
                       "got %s, expected string/object\n"
                      , efunname
                      , (mp_int)(svp - arg->u.vec->item)
                      , typename(item->type));

            /* Destructed objects yield 0 */
            if (ob->flags & O_DESTRUCTED)
            {
                if (error_handling == CO_ERROR)
                    errorf("%s() to destructed object at index %"PRIdMPINT".\n"
                         , efunname
                         , (mp_int)(svp - arg->u.vec->item));

                free_svalue(item);
                put_number(item, 0);

                free_svalue(svp);
                put_number(svp, 0);
                put_number(rcp, 0);
                continue;
            }

            /* Traceing, if necessary */
            if (TRACEP(TRACE_CALL_OTHER) && TRACE_IS_INTERACTIVE())
            {
                if (!++traceing_recursion)
                {
                    do_trace("Call other ", get_txt(arg[1].u.str), "\n");
                }
                traceing_recursion--;
            }

            /* Duplicate the arguments to pass, increasing sp on
             * the way. Optimizing this for the last pass is
             * dangerous as not every iteration will come here.
             */
            for (i = 0; i < call_num_arg; i++)
                assign_svalue_no_free(++sp, arg+2+i);

            /* Call the function with the remaining args on the stack.
             */
            inter_sp = sp; /* update to new setting */
            use_default_for_ob = b_use_default && (ob != master_ob);
            rc = int_apply(arg[1].u.str, ob, call_num_arg, MY_FALSE, use_default_for_ob);
            if (rc == APPLY_NOT_FOUND)
            {
                /* Function not found. */
                if (error_handling == CO_ERROR)
                {
                    errorf("Function %.50s() not found in %s.\n"
                         , get_txt(arg[1].u.str)
                         , get_txt(ob->name));
                }

                /* Assign 0 as result. */
                if (use_default_for_ob) /* int_apply() removed the args */
                    sp -= call_num_arg;
                else
                    sp = _pop_n_elems(call_num_arg, sp);

                free_svalue(svp);
                put_number(svp, 0);
                put_number(rcp, 0);
            }
            else
            {
                /* Function found - assign the result from the stack */
                sp -= call_num_arg;
                free_svalue(svp);
                transfer_svalue_no_free(svp, sp+1);

                if (error_handling == CO_RESULT)
                    put_number(rcp, rc == APPLY_FOUND ? 1 : -1);
            }
        } /* for (objects in array) */

        if (error_handling == CO_RESULT)
        {
            /* sp points to the efun result array,
             * arg points to the function call result array.
             * between them are function call arguments and
             * the function name, and below arg is the lvalue.
             */

            transfer_svalue(arg-1, arg);           /* results into lvalue.    */
            free_svalue(arg-1);
            transfer_svalue_no_free(arg-1, sp--);  /* efun result.            */
            sp = _pop_n_elems(call_num_arg+1, sp); /* args and function name. */
            assert(sp == arg); sp--;               /* We already transfered.  */
            return sp;
        }
        else
        {
            /* Remove the original function call arguments from the stack.
             */
            sp = _pop_n_elems(call_num_arg, sp);

            /* Calls complete, left on the stack are now the function name
             * and, in arg, the final result.
             */
            free_string_svalue(sp--);
            return sp;
        }
    }

} /* int_call_other() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_call_other (svalue_t *sp, int num_arg)

/* EFUN call_other()
 *
 * This is just a wrapper around the real implementation.
 */

{
    return int_call_other(true, CO_IGNORE, "call_other", sp, num_arg);
} /* v_call_other() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_call_direct (svalue_t *sp, int num_arg)

/* EFUN call_direct_direct()
 *
 * This is just a wrapper around the real implementation.
 */

{
    return int_call_other(false, CO_IGNORE, "call_direct", sp, num_arg);
} /* v_call_direct() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_call_resolved (svalue_t *sp, int num_arg)

/* EFUN call_resolved()
 *
 * This is just a wrapper around the real implementation.
 */

{
    return int_call_other(true, CO_RESULT, "call_resolved", sp, num_arg);
} /* v_call_resolved() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_call_direct_resolved (svalue_t *sp, int num_arg)

/* EFUN call_direct_resolved()
 *
 * This is just a wrapper around the real implementation.
 */

{
    return int_call_other(false, CO_RESULT, "call_direct_resolved", sp, num_arg);
} /* v_call_direct_resolved() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_call_strict (svalue_t *sp, int num_arg)

/* EFUN call_strict()
 *
 * This is just a wrapper around the real implementation.
 */

{
    return int_call_other(true, CO_ERROR, "call_strict", sp, num_arg);
} /* v_call_strict() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_call_direct_strict (svalue_t *sp, int num_arg)

/* EFUN call_direct_strict()
 *
 * This is just a wrapper around the real implementation.
 */

{
    return int_call_other(false, CO_ERROR, "call_direct_strict", sp, num_arg);
} /* v_call_direct_strict() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_get_eval_cost (svalue_t *sp)

/* EFUN get_eval_cost()
 *
 *   int get_eval_cost()
 *
 * Returns the remaining evaluation cost the current
 * execution (the current command) may use up.
 *
 * It starts at a driver given high value (__MAX_EVAL_COST__) and
 * is reduced with each executed statement.
 */

{
    push_number(sp, (max_eval_cost ? max_eval_cost : PINT_MAX) - eval_cost);

    return sp;
} /* f_get_eval_cost() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_set_this_object (svalue_t *sp)

/* EFUN set_this_object()
 *
 *   void set_this_object(object object_to_pretend_to_be);
 *
 * Set this_object() to <object_to_pretend_to_be>. A privilege
 * violation ("set_this_object", this_object(), object_to_be)
 * occurs.
 *
 * It changes the result of this_object() in the using function, and
 * the result of previous_object() in functions called in other
 * objects by call_other(). Its effect will remain till there is a
 * return of an external function call, or another call of
 * set_this_object(). While executing code in the master
 * object's program or the primary simul_efun object's program,
 * set_this_object() is granted even if this_object() is altered by
 * set_this_object(). This does not apply to functions inherited from
 * other programs.
 *
 * Use it with extreme care to avoid inconsistencies.  After a call of
 * set_this_object(), some LPC-constructs might behave in an odd
 * manner, or even crash the system. In particular, using global
 * variables or calling local functions (except by call_other) is
 * illegal.
 *
 * With the current implementation, global variables can be accessed,
 * but this is not guaranteed to work in subsequent versions.
 *
 * Allowed are call_other, map functions, access of local variables
 * (which might hold array pointers to a global array), simple
 * arithmetic and the assignment operators.
 */

{

    if (sp->u.ob != current_object)
    {
        if ((master_ob != NULL && current_variables == master_ob->variables)
         || (simul_efun_object != NULL && current_variables == simul_efun_object->variables)
         || privilege_violation(STR_SET_THIS_OBJECT, sp, sp))
        {
            struct control_stack *p;

            /* Find the 'extern_call' entry in the call stack which
             * determined the current this_object().
             */
            for (p = csp; !p->extern_call; p--) NOOP;

            p->extern_call |= CS_PRETEND;
            p->pretend_to_be = current_object = sp->u.ob;
        }
    }

    free_svalue(sp);
    sp--;
    return sp;
} /* f_set_this_object() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_trace (svalue_t *sp)

/* EFUN trace()
 *
 *   int trace(int traceflags)
 *
 * Sets the trace flags and returns the old trace flags. When
 * tracing is on, a lot of information is printed during
 * execution and too much output can crash your connection or
 * even the whole driver.
 *
 * Tracing is done on a per-connection basis: each interactive(!)
 * user may specifiy its own tracelevel and -prefix. Each gets the
 * traceoutput for just the code executed during the evaluation
 * of the commands he entered.
 *
 * The trace bits are:
 *
 *   TRACE_NOTHING     (  0): stop tracing.
 *
 *   TRACE_CALL        (  1): trace all calls to lfuns.
 *   TRACE_CALL_OTHER  (  2): trace call_others()s.
 *   TRACE_RETURN      (  4): trace function returns.
 *   TRACE_ARGS        (  8): print function arguments and results.
 *   TRACE_EXEC        ( 16): trace all executed instructions.
 *   TRACE_HEART_BEAT  ( 32): trace heartbeat code.
 *   TRACE_APPLY       ( 64): trace driver applies.
 *   TRACE_OBJNAME     (128): print the object names.
 *
 * TRACE_EXEC and TRACE_HEART_BEAT should be avoided as they cause massive
 * output! TRACE_OBJNAME should be avoided when you know what you trace.
 *
 * The master-lfun valid_trace() is called to verify the
 * usage of this efun.
 */

{
    int ot;
    interactive_t *ip;

    ot = -1;

    /* If the command_giver is allowed to do so... */
    if (command_giver
     && O_SET_INTERACTIVE(ip, command_giver))
    {
        svalue_t *arg;

        assign_eval_cost_inl();
        inter_sp = sp;
        push_ref_string(inter_sp, STR_TRACE);
        push_number(inter_sp, sp->u.number);
        arg = apply_master(STR_VALID_TRACE, 2);
        if (arg)
        {
            /* ... then set the new tracelevel */
            if (arg->type != T_NUMBER || arg->u.number != 0)
            {
                ot = ip->trace_level;
                trace_level |=
                  ip->trace_level = sp->u.number;
            }
        }
    }

    /* Return the old level */
    sp->u.number = ot;
    SET_TRACE_EXEC();
    return sp;
} /* f_trace() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_traceprefix (svalue_t *sp)

/* EFUN traceprefix()
 *
 *   string traceprefix(string prefix)
 *   string traceprefix(int dummy)
 *
 * If called with a string, only objects matching this prefix will be traced.
 * The string must not contain a leading "/" because the object names are
 * stored internally without it. If called with a number, the traceprefix will
 * be ignored and all objects will be traced. Returns the last traceprefix or
 * 0 if there wasn't any.
 *
 * The master-lfun valid_trace() is called to verify the usage of this
 * efun.
 */

{
    string_t *old;
    interactive_t *ip;

    old = NULL;

    /* If the command_giver is allowed to do that... */
    if (command_giver
     && O_SET_INTERACTIVE(ip, command_giver))
    {
        svalue_t *arg;

        inter_sp = sp;
        push_ref_string(inter_sp, STR_TRACEPREFIX);
        inter_sp++; assign_svalue_no_free(inter_sp, sp);
        assign_eval_cost_inl();
        arg = apply_master(STR_VALID_TRACE,2);
        if (arg)
        {
            /* ... then so shall it be */
            if (arg && (arg->type != T_NUMBER || arg->u.number))
            {
                old = ip->trace_prefix;
                if (sp->type == T_STRING)
                {
                    ip->trace_prefix = make_tabled_from(sp->u.str);
                      /* tabled for faster comparisons */
                }
                else
                    ip->trace_prefix = NULL;
            }
        }
    }

    free_svalue(sp);

    /* Return the old prefix */
    if (old)
        put_string(sp, old);
    else
        put_number(sp, 0);
    return sp;
} /* f_traceprefix() */

/*-------------------------------------------------------------------------*/
Bool
set_profiling_time_limit(mp_int limit)
/* Sets the profiling time limit to <limit> us.
 * return TRUE on success and FALSE otherwise.
 */
{
    if (limit >= 0)
    {
        profiling_timevalue.tv_sec = limit / 1000000;
        profiling_timevalue.tv_usec = limit % 1000000;
        return MY_TRUE;
    }
    
    return MY_FALSE;
} /* set_memory_limit */

mp_int
get_profiling_time_limit()
/* Gets the profiling time limit in microseconds.
*/
{
    return profiling_timevalue.tv_sec * 1000000 + profiling_timevalue.tv_usec;
} /* get_memory_limit */
                            
/***************************************************************************/
