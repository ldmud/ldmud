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
#ifdef USE_NEW_INLINES
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
#endif
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
 *    the return instruction not needs to know it explicitely.
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
 * TODO: Let all assign_ and transfer_ functions check for destruct objects.
 * TODO:: The speed difference to assign_checked_ and transfer_checked_ is
 * TODO:: not big enough to justify the extra set of functions.
 */

/*-------------------------------------------------------------------------*/

#include "driver.h"
#include "typedefs.h"

#include "my-alloca.h"
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
#include "structs.h"
#include "svalue.h"
#include "swap.h"
#include "switch.h"
#include "wiz_list.h"
#include "xalloc.h"

#include "i-eval_cost.h"

#include "../mudlib/sys/driver_hook.h"
#include "../mudlib/sys/debug_info.h"
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
    fun_hdr_p funstart;
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
   , /* T_QUOTED_ARRAY */  "quoted-array"
   , /* T_STRUCT */  "struct"
   , /* T_CHAR_LVALUE */           "char-lvalue"
   , /* T_STRING_RANGE_LVALUE */   "string-range-lvalue"
   , /* T_POINTER_RANGE_LVALUE */  "array-range-lvalue"
   , /* T_PROTECTED_CHAR_LVALUE */           "prot-char-lvalue"
   , /* T_PROTECTED_STRING_RANGE_LVALUE */   "prot-string-range-lvalue"
   , /* T_PROTECTED_POINTER_RANGE_LVALUE */  "prot-array-range-lvalue"
   , /* T_PROTECTED_LVALUE  */               "prot-lvalue"
   , /* T_PROTECTOR_MAPPING  */              "protector-mapping"
   , /* T_CALLBACK */       "callback-mapping"
   , /* T_ERROR_HANDLER */  "error-handler"
   , /* T_BREAK_ADDR */     "break-address"
   , /* T_NULL */  "null"
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

#ifdef USE_NEW_INLINES
static svalue_t *inter_context;
  /* Contextpointer: pointer to first context variable.
   * May be NULL if no context is available.
   */
#endif /* USE_NEW_INLINES */

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
  /* Index of current program's variable block within the variables
   * of the current object (needed for inheritance).
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

static struct
  {
    svalue_t v;
      /* The target value:
       *   .v.type: T_CHAR_LVALUE
       *   .v.u.charp: the char to modify
       * or
       *   .v.type: T_{POINTER,STRING}_RANGE_LVALUE
       *   .v.u.{vec,string}: the target value holding the range
       *   .index1, .index2, .size: see below
       */
    mp_int index1;  /* First index of the range */
    mp_int index2;  /* Last index of the range plus 1 */
    mp_int size;    /* Current(?) size of the value */
  }
special_lvalue;
  /* When assigning to vector and string ranges or elements, the
   * target information is stored in this structure.
   * TODO: Having one global structure counts as 'ugly'.
   * Used knowingly by: (r)index_lvalue(), transfer_pointer_range(),
   *                    assign_string_range().
   * Used unknowingly by: assign_svalue(), transfer_svalue(),
   *                    add_number_to_lvalue(), F_VOID_ASSIGN.
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

svalue_t last_indexing_protector = { T_NUMBER };
  /* When indexing a protected non-string-lvalue, this variable receives
   * the protecting svalue for the duration of the operation (actually
   * until the next indexing operation (TODO: not nice)).
   * This is necessary because the indexing operation necessarily destroys
   * the protector structure, even though the protection is still needed.
   * Used by: protected_index_lvalue().
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

/*-------------------------------------------------------------------------*/
/* Forward declarations */

enum { APPLY_NOT_FOUND = 0, APPLY_FOUND, APPLY_DEFAULT_FOUND };
static int int_apply(string_t *, object_t *, int, Bool, Bool);
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
RETSIGTYPE
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
 * free_string_svalue(v): free string svalue <v>.
 * free_object_svalue(v): free object svalue <v>.
 * zero_object_svalue(v): replace the object in svalue <v> by number 0.
 * free_svalue(v):        free the svalue <v>.
 * assign_svalue_no_free(to,from): put a copy of <from> into <to>; <to>
 *                        is considered empty.
 * copy_svalue_no_free(to,from): put a shallow value copy of <from> into <to>;
 *                        <to> is considered empty.
 * assign_checked_svalue_no_free(to,from): put a copy of <from> into <to>;
 *                        <to> is considered empty, <from> may be destructed
 *                        object.
 * assign_local_svalue_no_free(to,from): put a copy of local var <from>
 *                        into <to>; <to> is considered empty, <from> may
 *                        be destructed object.
 * static assign_lrvalue_no_free(to,from): like assign_svalue_no_free(),
 *                        but lvalues and strings are handled differently.
 * assign_svalue(dest,v): assign <v> to <dest>, freeing <dest> first.
 *                        Also handles assigns to lvalues.
 * transfer_svalue_no_free(dest,v): move <v> into <dest>; <dest> is
 *                        considered empty.
 * transfer_svalue(dest,v): move <v> into <dest>; freeing <dest> first.
 *                        Also handles transfers to lvalues.
 * static add_number_to_lvalue(dest,i,pre,post): add <i> to lvalue <dest>.
 *
 * In addition there are some helper functions.
 *
 * TODO: All these functions and vars should go into a separate module.
 */

/*-------------------------------------------------------------------------*/

/* --- Protector structures ---
 *
 * Whenever an assignment is made to a single value, or to a range in
 * a string, vector or mapping, the interpreter generates protector
 * structures in place of the usual LVALUE-svalues, which hold:
 *  - a svalue structure referring to the svalue into which the assignment
 *    is done (this structure is always first so that the protector
 *    structures can be used instead of normal svalues),
 *  - the necessary information to store the assigned svalue into its
 *    place in the target holding the value,
 *  - a protective reference to the holding value.
 *
 * All this just to keep LPC statements like 'a = ({ 1 }); a[0] = (a = 0);'
 * from crashing.
 *
 * TODO: A simpler way would be to compute the lhs of an assignment
 * TODO:: after evaluating the rhs - not vice versa as it is now.
 * TODO:: However, passing lvalues and ranges as ref-parameters to functions
 * TODO:: would still be a potential problem.
 */

/* --- struct protected_lvalue: protect a single value
 */
struct protected_lvalue
{
    svalue_t v;
      /* .v.type: T_PROTECTED_LVALUE
       * .v.u.lvalue: the protected value
       */
    svalue_t protector;
      /* additional reference .v.u.lvalue (or its holder) as means of
       * protection
       */
};

/* --- struct protected_char_lvalue: protect a char in a string
 */
struct protected_char_lvalue
{
    svalue_t v;
      /* .v.type: T_PROTECTED_CHAR_LVALUE
       * .v.u.charp: points to the char to access
       */
    svalue_t protector; /* protects .lvalue */
    svalue_t *lvalue;   /* the string containing the char */
    char *start;
      /* must be == get_txt(lvalue->u.str), otherwise the string has been
       * changed and this lvalue is invalid
       */
};

/* --- struct protected_range_lvalue: protect a range in a string or vector
 */
struct protected_range_lvalue {
    svalue_t v;
      /* .v.type: T_PROTECTED_{POINTER,STRING}_RANGE_LVALUE
       * .v.u.{str,vec}: the target value holding the range
       */
    svalue_t protector;  /* protects .lvalue */
    svalue_t *lvalue;    /* the original svalue holding the range */
    int index1, index2;  /* first and last index of the range in .lvalue */
    int size;            /* original size of .lvalue */

    /* On creation, .v.u.{vec,str} == .lvalue->u.{vec,str}.
     * If that condition no longer holds, the target in .v has been changed
     * and the range information (index, size) is no longer valid.
     */
};

/*-------------------------------------------------------------------------*/
/* Forward declarations */

static void transfer_pointer_range(svalue_t *source);
static void transfer_protected_pointer_range(
    struct protected_range_lvalue *dest, svalue_t *source);
static void assign_string_range(svalue_t *source, Bool do_free);
static void assign_protected_string_range(
    struct protected_range_lvalue *dest,svalue_t *source, Bool do_free);

/*-------------------------------------------------------------------------*/
void INLINE
free_string_svalue (svalue_t *v)

/* Free the string svalue <v>; <v> must be of type T_STRING.
 */

{
#ifdef DEBUG
    if (v->type != T_STRING)
    {
        fatal("free_string_svalue(): Expected string, "
              "received svalue type (%d:%hd)\n"
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
              "received svalue type (%d:%hd)\n"
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
static void
free_protector_svalue (svalue_t *v)

/* Free the svalue <v> which contains a protective reference to a vector
 * or to a mapping.
 */

{
    switch (v->type)
    {
      case T_STRUCT:
        free_struct(v->u.strct);
        break;
      case T_POINTER:
        free_array(v->u.vec);
        break;
      case T_MAPPING:
        free_mapping(v->u.map);
        break;
      case T_PROTECTOR_MAPPING:
        free_protector_mapping(v->u.map);
        break;
    }
}

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
        free_mstring(v->u.str);
        break;

    case T_CLOSURE:
        free_closure(v);
        break;

    case T_CALLBACK:
        free_callback(v->u.cb);
        break;

    case T_ERROR_HANDLER:
        v->u.error_handler->fun(v->u.error_handler);
        break;

    case T_BREAK_ADDR:
        NOOP;
        break;

    case T_LVALUE:
        switch (v->u.lvalue->type)
        {
        case T_PROTECTED_LVALUE:
          {
              struct protected_lvalue *p;

              p = v->u.protected_lvalue;
              free_protector_svalue(&p->protector);
              xfree(p);
              break;
          }

        case T_PROTECTED_CHAR_LVALUE:
          {
              struct protected_char_lvalue *p;

              p = v->u.protected_char_lvalue;
              if (p->v.type == T_STRING)
              {
                  free_mstring(p->v.u.str);
              }
              free_protector_svalue(&p->protector);
              xfree(p);
              break;
          }

        case T_PROTECTED_STRING_RANGE_LVALUE:
          {
              struct protected_range_lvalue *p;

              p = v->u.protected_range_lvalue;
              free_mstring(p->v.u.str);
              free_protector_svalue(&p->protector);
              xfree(p);
              break;
          }

        case T_PROTECTED_POINTER_RANGE_LVALUE:
          {
              struct protected_range_lvalue *p;

              p = v->u.protected_range_lvalue;
              free_array(p->v.u.vec);
              free_protector_svalue(&p->protector);
              xfree(p);
              break;
          }

        } /* switch (v->u.lvalue->type) */
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
        fs_queue_t * tmp = xalloc(sizeof(*tmp));

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
check_for_ref_loop (svalue_t * dest)

/* <dest> has just been assigned to - check if this created a reference loop.
 * If yes, free <dest> and throw an error.
 */

{
    if (dest->type == T_LVALUE || dest->type == T_PROTECTED_LVALUE)
    {
        /* rover1 will scan the lvalue chain in two-steps, rover2 will
         * scan it step by step. If there is a loop, the two will eventually
         * meet again.
         */
        svalue_t * rover1, * rover2;

        rover1 = rover2 = dest;
        do {
            if (rover1->type == T_LVALUE || rover1->type == T_PROTECTED_LVALUE)
                rover1 = rover1->u.lvalue;
            else
                break;
            if (rover1->type == T_LVALUE || rover1->type == T_PROTECTED_LVALUE)
                rover1 = rover1->u.lvalue;
            else
                break;
            if (rover2->type == T_LVALUE || rover2->type == T_PROTECTED_LVALUE)
                rover2 = rover2->u.lvalue;
            else
                break;
            if (rover1 == rover2)
            {
                free_svalue(dest);
                errorf("Assignment would create reference loop.\n");
            }
        } while (rover1
             && (rover1->type == T_LVALUE || rover1->type == T_PROTECTED_LVALUE)
             && rover2
             && (rover2->type == T_LVALUE || rover2->type == T_PROTECTED_LVALUE)
                );
    }

} /* check_for_ref_loop() */

/*-------------------------------------------------------------------------*/
static INLINE void
inl_assign_svalue_no_free (svalue_t *to, svalue_t *from)

/* Put a duplicate of svalue <from> into svalue <to>, meaning that the original
 * value is either copied when appropriate, or its refcount is increased.
 * <to> is considered empty at the time of call.
 *
 * If <from> is a destructed object, <to> is set to the number 0 but
 * <from> is left unchanged.
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
    }

    /* Protection against endless reference loops */
    if (to->type == T_LVALUE || to->type == T_PROTECTED_LVALUE)
    {
        check_for_ref_loop(to);
    }
} /* inl_assign_svalue_no_free() */

void assign_svalue_no_free (svalue_t *to, svalue_t *from)
{ inl_assign_svalue_no_free(to,from); }

#define assign_svalue_no_free(to,from) inl_assign_svalue_no_free(to,from)

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
 */

{
    assign_svalue_no_free(to, from);

    /* For arrays and mappings, create a shallow copy */
    if (from->type == T_MAPPING)
    {
        mapping_t *old, *new;

        old = to->u.map;
        if (old->ref != 1)
        {
            DYN_MAPPING_COST(MAP_SIZE(old));
            new = copy_mapping(old);
            if (!new)
                errorf("Out of memory: mapping[%"PRIdPINT"] for copy.\n"
                     , MAP_SIZE(old));
            free_mapping(old);
            to->u.map = new;
        }
    }
    else if (from->type == T_POINTER
          || from->type == T_QUOTED_ARRAY)
    {
        vector_t *old, *new;
        size_t size, i;

        old = to->u.vec;
        size = VEC_SIZE(old);
        if (old->ref != 1 && old != &null_vector)
        {
            DYN_ARRAY_COST(size);
            new = allocate_uninit_array((int)size);
            if (!new)
                errorf("Out of memory: array[%zu] for copy.\n"
                     , size);
            for (i = 0; i < size; i++)
                assign_svalue_no_free( &new->item[i]
                                     , &old->item[i]);
            free_array(old);
            to->u.vec = new;
        }
    }
} /* inl_copy_svalue_no_free() */

void copy_svalue_no_free (svalue_t *to, svalue_t *from)
{ inl_copy_svalue_no_free(to,from); }

#define copy_svalue_no_free(to,from) inl_copy_svalue_no_free(to,from)

/*-------------------------------------------------------------------------*/
static INLINE void
assign_checked_svalue_no_free (svalue_t *to, svalue_t *from)

/* Put a duplicate of svalue <from> into svalue <to>, meaning that the original
 * value is either copied when appropriate, or its refcount is increased.
 * <to> is considered empty at the time of call.
 * <from> may point to a variable or vector element, so it might contain
 * a destructed object. In that case, <from> and <to> are set to
 * svalue-number 0.
 */

{
    switch (from->type)
    {
    case T_STRING:
        (void)ref_mstring(from->u.str);
        break;

    case T_OBJECT:
      {
        object_t *ob = from->u.ob;
        if ( !(ob->flags & O_DESTRUCTED) ) {
            ref_object(ob, "ass to var");
            break;
        }
        zero_object_svalue(from);
        break;
      }

    case T_QUOTED_ARRAY:
    case T_POINTER:
        (void)ref_array(from->u.vec);
        break;

    case T_STRUCT:
        (void)ref_struct(from->u.strct);
        break;

    case T_SYMBOL:
        (void)ref_mstring(from->u.str);
        break;

    case T_CLOSURE:
        if (!destructed_object_ref(from))
            addref_closure(from, "ass to var");
        else
            assign_svalue(from, &const0);
        break;

    case T_MAPPING:
        (void)ref_mapping(from->u.map);
        break;
    }
    *to = *from;

    /* Protection against endless reference loops */
    if (to->type == T_LVALUE || to->type == T_PROTECTED_LVALUE)
    {
        check_for_ref_loop(to);
    }
} /* assign_checked_svalue_no_free() */

/*-------------------------------------------------------------------------*/
static INLINE void
assign_local_svalue_no_free ( svalue_t *to, svalue_t *from )

/* Put a duplicate of svalue <from> into svalue <to>, meaning that the original
 * value is either copied when appropriate, or its refcount is increased.
 * <to> is considered empty at the time of call.
 *
 * <from> is meant to point to a local variable, which might be an arg
 * to the current lfun.
 * If <from> is a lvalue, the chain is unraveled and the final non-lvalue
 * is assigned. If that value is a destructed object, 0 is assigned.
 */

{
assign_from_lvalue:
    switch (from->type)
    {
      case T_STRING:
        (void)ref_mstring(from->u.str);
        break;
      case T_OBJECT:
        (void)ref_object(from->u.ob, "assign_local_lvalue_no_free");
        break;
      case T_QUOTED_ARRAY:
      case T_POINTER:
        (void)ref_array(from->u.vec);
        break;
      case T_STRUCT:
        (void)ref_struct(from->u.strct);
        break;
      case T_SYMBOL:
        (void)ref_mstring(from->u.str);
        break;
      case T_CLOSURE:
        addref_closure(from, "ass to var");
        break;
      case T_MAPPING:
        (void)ref_mapping(from->u.map);
        break;
      case T_LVALUE:
      case T_PROTECTED_LVALUE:
        from = from->u.lvalue;
        if (destructed_object_ref(from)) {
            assign_svalue(from, &const0);
            break;
        }
        goto assign_from_lvalue;
      case T_PROTECTED_CHAR_LVALUE:
        put_number(to, *from->u.charp);
        return;
    }
    *to = *from;

    /* Protection against endless reference loops */
    if (to->type == T_LVALUE || to->type == T_PROTECTED_LVALUE)
    {
        check_for_ref_loop(to);
    }
} /* assign_local_svalue_no_free() */

/*-------------------------------------------------------------------------*/
static INLINE
void assign_lrvalue_no_free (svalue_t *to, svalue_t *from)

/* Put a duplicate of svalue <from> into svalue <to>, meaning that the original
 * value is either copied when appropriate, or its refcount is increased.
 * <to> is considered empty at the time of call.
 *
 * This function differs from assign_svalue_no_free() in the handling of
 * two types:
 *  - if <from> is an unshared string, the string is made shared and
 *    both <to> and <from> are changed to use the shared string.
 *  - if <from> is a lvalue, <to>.u.lvalue is set to point to <from>.
 *    This is necessary when pushing references onto the stack - if
 *    assign_svalue_no_free() were used, the first free_svalue() would undo
 *    the whole lvalue indirection, even though there were still other lvalue
 *    entries in the stack for the same svalue.
 *    TODO: An alternative would be use a special struct lvalue {} with a
 *    refcount.
 */

{
#ifdef DEBUG
    if (from == 0)
        fatal("Null pointer to assign_lrvalue_no_free().\n");
#endif

    /* Copy the data */
    *to = *from;

    /* Now adapt the refcounts or similar */

    switch(from->type)
    {
    case T_STRING:
        (void)ref_mstring(to->u.str);
        break;

    case T_OBJECT:
        (void)ref_object(to->u.ob, "ass to var");
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
        if (!destructed_object_ref(to))
            addref_closure(to, "ass to var");
        else
            put_number(to, 0);
        break;

    case T_MAPPING:
        (void)ref_mapping(to->u.map);
        break;

    case T_LVALUE:
        to->u.lvalue = from;
        break;
    }

    /* Protection against endless reference loops */
    if (to->type == T_LVALUE || to->type == T_PROTECTED_LVALUE)
    {
        check_for_ref_loop(to);
    }
} /* assign_lrvalue_no_free() */

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
 */

{
    /* Free the <dest> svalue.
     * If <dest> is a (protected) lvalue, the loop will traverse the lvalue
     * chain until the actual svalue is found.
     * If a T_xxx_LVALUE is found, the assignment will be done here
     * immediately.
     */

    for (;;) {
        switch(dest->type)
        {
        case T_LVALUE:
        case T_PROTECTED_LVALUE:
            dest = dest->u.lvalue;
            continue;

        case T_STRING:
            free_mstring(dest->u.str);
            break;

        case T_OBJECT:
          {
            object_t *ob = dest->u.ob;
            free_object(ob, "assign_svalue");
            break;
          }

        case T_QUOTED_ARRAY:
        case T_POINTER:
          {
            vector_t *vec = dest->u.vec;
            assign_svalue_no_free(dest, v);
              /* TODO: leaks vec if out of memory */
            free_array(vec);
            return;
          }

        case T_STRUCT:
          {
            struct_t *strct = dest->u.strct;
            assign_svalue_no_free(dest, v);
              /* TODO: leaks strct if out of memory */
            free_struct(strct);
            return;
          }

        case T_MAPPING:
          {
            mapping_t *map = dest->u.map;
            assign_svalue_no_free(dest, v); /* leaks map if out of memory */
            free_mapping(map);
            return;
          }

        case T_SYMBOL:
            free_mstring(dest->u.str);
            break;

        case T_CLOSURE:
            free_closure(dest);
            break;

        /* If the final svalue in dest is one of these lvalues,
         * the assignment is done right here and now.
         * Note that 'dest' in some cases points to a protector structure.
         */

        case T_CHAR_LVALUE:
            if (v->type == T_NUMBER)
                *dest->u.charp = (char)v->u.number;
            return;

        case T_PROTECTED_CHAR_LVALUE:
          {
            struct protected_char_lvalue *p;

            p = (struct protected_char_lvalue *)dest;
            if (p->lvalue->type == T_STRING
             && get_txt(p->lvalue->u.str) == p->start)
            {
                if (v->type == T_NUMBER)
                    *p->v.u.charp = (char)v->u.number;
            }
            return;
          }

        case T_POINTER_RANGE_LVALUE:
            if (v->type == T_POINTER)
            {
                (void)ref_array(v->u.vec); /* transfer_...() will free it once */
                transfer_pointer_range(v);
            }
            return;

        case T_PROTECTED_POINTER_RANGE_LVALUE:
            if (v->type == T_POINTER)
            {
                (void)ref_array(v->u.vec); /* transfer_...() will free it once */
                transfer_protected_pointer_range(
                  (struct protected_range_lvalue *)dest, v
                );
            }
            return;

        case T_STRING_RANGE_LVALUE:
            assign_string_range(v, MY_FALSE);
            return;

        case T_PROTECTED_STRING_RANGE_LVALUE:
            assign_protected_string_range(
                  (struct protected_range_lvalue *)dest, v, MY_FALSE
            );
            return;

        } /* switch() */

        /* No more lvalues to follow, old value freed: do the assign next */
        break;
    } /* end for */

    /* Now assign the value to the now-invalid <dest> */
    assign_svalue_no_free(dest, v);
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

    /* Protection against endless reference loops */
    if (dest->type == T_LVALUE || dest->type == T_PROTECTED_LVALUE)
    {
        v->type = T_INVALID;
        check_for_ref_loop(dest);
    }
} /* inl_transfer_svalue_no_free() */

void transfer_svalue_no_free (svalue_t *dest, svalue_t *v)
{  inl_transfer_svalue_no_free(dest,v); }

#define transfer_svalue_no_free(dest,v) inl_transfer_svalue_no_free(dest,v)

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
    /* Unravel the T_LVALUE chain, if any. */
    while (dest->type == T_LVALUE || dest->type == T_PROTECTED_LVALUE)
        dest = dest->u.lvalue;

    /* Free the <dest> svalue.
     * If a T_xxx_LVALUE is found, the transfer will be done here
     * immediately.
     */

    for(;;)
    {
        switch (dest->type)
        {
        case T_STRING:
            free_mstring(dest->u.str);
            break;

        case T_OBJECT:
          {
            object_t *ob = dest->u.ob;
            free_object(ob, "transfer_svalue");
            break;
          }

        case T_QUOTED_ARRAY:
        case T_POINTER:
            free_array(dest->u.vec);
            break;

        case T_STRUCT:
            free_struct(dest->u.strct);
            break;

        case T_SYMBOL:
            free_mstring(dest->u.str);
            break;

        case T_CLOSURE:
            free_closure(dest);
            break;

        case T_MAPPING:
            free_mapping(dest->u.map);
            break;

        /* If the final svalue in dest is one of these lvalues,
         * the assignment is done right here and now.
         * Note that 'dest' in some cases points to a protector structure.
         */

        case T_CHAR_LVALUE:
            if (v->type == T_NUMBER)
            {
                *dest->u.charp = (char)v->u.number;
            }
            else
                free_svalue(v);
            return;

        case T_PROTECTED_CHAR_LVALUE:
          {
            struct protected_char_lvalue *p;

            p = (struct protected_char_lvalue *)dest;
            if (p->lvalue->type == T_STRING
             && get_txt(p->lvalue->u.str) == p->start)
            {
                if (v->type == T_NUMBER)
                {
                    *p->v.u.charp = (char)v->u.number;
                    return;
                }
            }
            free_svalue(v);
            return;
          }

        case T_POINTER_RANGE_LVALUE:
            transfer_pointer_range(v);
            return;

        case T_PROTECTED_POINTER_RANGE_LVALUE:
            transfer_protected_pointer_range(
              (struct protected_range_lvalue *)dest, v
            );
            return;

        case T_STRING_RANGE_LVALUE:
            assign_string_range(v, MY_TRUE);
            return;

        case T_PROTECTED_STRING_RANGE_LVALUE:
            assign_protected_string_range(
              (struct protected_range_lvalue *)dest, v, MY_TRUE
            );
            return;
        } /* end switch */

        /* No more lvalues to follow, old value freed: do the assign next */
        break;
    } /* end for */

    /* Transfer the value */
    *dest = *v;

    /* Protection against endless reference loops */
    if (dest->type == T_LVALUE || dest->type == T_PROTECTED_LVALUE)
    {
        v->type = T_INVALID;
        check_for_ref_loop(dest);
    }

} /* inl_transfer_svalue() */

void transfer_svalue (svalue_t *dest, svalue_t *v)
{  inl_transfer_svalue(dest,v); }

#define transfer_svalue(dest,v) inl_transfer_svalue(dest,v)

/*-------------------------------------------------------------------------*/
static void
transfer_pointer_range (svalue_t *source)

/* Transfer the vector <source> to the vector range defined by
 * <special_lvalue>, modifying the target vector in special_lvalue
 * accordingly. <source> is freed once in the call.
 *
 * If <source> is not a vector, it is just freed.
 */

{
    if (source->type == T_POINTER)
    {
        vector_t *sv;      /* Source vector (from source) */
        vector_t *dv;      /* Destination vector (from special_lvalue) */
        vector_t *rv;      /* Result vector */
        mp_int dsize;           /* Size of destination vector */
        mp_int ssize;           /* Size of source vector */
        mp_int index1, index2;  /* First and last index of destination range */
        mp_int i;

        /* Setup the variables */
        dsize = special_lvalue.size;
        index1 = special_lvalue.index1;
        index2 = special_lvalue.index2;
        dv = special_lvalue.v.u.lvalue->u.vec;
        sv = source->u.vec;
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
            special_lvalue.v.u.lvalue->u.vec = rv;
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
    if (source->type == T_POINTER && dest->v.u.vec == dest->lvalue->u.vec)
    {
        vector_t *sv;      /* Source vector (from source) */
        vector_t *dv;      /* Dest vector (from dest) */
        vector_t *rv;      /* Result vector */
        mp_int dsize;           /* Size of the dest vector */
        mp_int ssize;           /* Size of the source vector */
        mp_int index1, index2;  /* Target range indices */
        mp_int i;

        /* Setup the variables */
        dsize = dest->size;
        index1 = dest->index1;
        index2 = dest->index2;
        dv = dest->v.u.vec;
        sv = source->u.vec;
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
            dest->lvalue->u.vec = rv;

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

            free_array(dv); /* this can make the lvalue invalid to use */
        }
    }
    else
    {
        /* Not a pointer, or the protected range has changed in size before:
         * just free it
         */
        free_svalue(source);
    }

} /* transfer_protected_pointer_range() */

/*-------------------------------------------------------------------------*/
static void
assign_string_range (svalue_t *source, Bool do_free)

/* Transfer the string <source> to the string range defined by
 * <special_lvalue>, modifying the target string in special_lvalue
 * accordingly. If <do_free> is TRUE, <source> is freed once in the call.
 *
 * If <source> is not a string, it is just freed resp. ignored.
 */

{
    if (source->type == T_STRING)
    {
        svalue_t *dsvp;     /* destination svalue (from special_lvalue) */
        string_t *ds;            /* destination string (from dsvp) */
        string_t *ss;            /* source string (from source) */
        string_t *rs;            /* result string */
        mp_int dsize;            /* size of destination string */
        mp_int ssize;            /* size of source string */
        mp_int index1, index2;   /* range indices */

        /* Set variables */
        dsize = special_lvalue.size;
        index1 = special_lvalue.index1;
        index2 = special_lvalue.index2;
        dsvp = special_lvalue.v.u.lvalue;
        ds = dsvp->u.str;
        ss = source->u.str;
        ssize = (mp_int)mstrsize(ss);

#ifdef NO_NEGATIVE_RANGES
        if (index1 > index2)
            errorf("Illegal range [%"PRIdMPINT"..%"PRIdMPINT
                   "] for assignment.\n", index1, index2-1
                 );
#endif /* NO_NEGATIVE_RANGES */

        /* Create the new string */
        rs = alloc_mstring((size_t)(dsize + ssize + index1 - index2));
        if (!rs)
        {
            /* We don't pop the stack here --> don't free source */
            outofmem((dsize + ssize + index1 - index2), "new string");
        }

        if (index1)
            memcpy(get_txt(rs), get_txt(ds), (size_t)index1);
        if (ssize)
            memcpy(get_txt(rs) + index1, get_txt(ss), (size_t)ssize);
        if (dsize > index2)
            memcpy( get_txt(rs) + index1 + ssize, get_txt(ds) + index2
                  , (size_t)(dsize - index2));

        /* Assign the new string in place of the old */
        free_string_svalue(dsvp);
        dsvp->u.str = rs;

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
    if (source->type == T_STRING)
    {
        svalue_t *dsvp;     /* destination value (from dest) */
        string_t *ds;            /* destination string (from dsvp) */
        string_t *ss;            /* source string (from source) */
        string_t *rs;            /* result string */
        mp_int dsize;            /* size of destination string */
        mp_int ssize;            /* size of source string */
        mp_int rsize;            /* size of result string */
        mp_int index1, index2;   /* range indices */

        /* Set variables */
        dsize = dest->size;
        index1 = dest->index1;
        index2 = dest->index2;
        dsvp = dest->lvalue;
        ds = dest->v.u.str;

#ifdef NO_NEGATIVE_RANGES
        if (index1 > index2)
            errorf("Illegal range [%"PRIdMPINT"..%"PRIdMPINT
                   "] for assignment.\n", index1, index2-1
                 );
#endif /* NO_NEGATIVE_RANGES */

        /* If the lvalue is no longer valid, free it */
        if (dsvp->u.str != ds)
        {
            if (do_free)
                free_svalue(source);
            return;
        }

        /* Create a new string */
        ss = source->u.str;
        ssize = (mp_int)mstrsize(ss);
        rsize = dsize + ssize + index1 - index2;
        rs = alloc_mstring((size_t)rsize);
        if (!rs)
        {
            outofmem((dsize + ssize + index1 - index2), "new string");
        }

        if (index1)
            memcpy(get_txt(rs), get_txt(ds), (size_t)index1);
        if (ssize)
            memcpy(get_txt(rs) + index1, get_txt(ss), (size_t)ssize);
        dest->index2 = (int)(index1 + ssize);
        if (dsize > index2)
            memcpy( get_txt(rs) + dest->index2, get_txt(ds) + index2
                  , (size_t)(dsize - index2));
        dest->size = rsize;
        free_mstring(ds);
        free_mstring(ds);
        /* we will have two references to rs */
        ref_mstring(rs);
        dest->v.u.str = dsvp->u.str = rs;

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
static void
add_number_to_lvalue (svalue_t *dest, int i, svalue_t *pre, svalue_t *post)

/* Add the number <i> to the (PROTECTED_)LVALUE <dest>.
 * If <pre> is not null, the <dest> value before the addition is copied
 * into it.
 * If <post> is not null, the <dest> value after the addition is copied
 * into it.
 * Both <pre> and <post> are supposed to be empty svalues when given.
 *
 * If <dest> is of the wrong type, an error is generated.
 */

{
    /* Deref the T_(PROTECTED_)LVALUES */
    do
        dest = dest->u.lvalue;
    while (dest->type == T_LVALUE || dest->type == T_PROTECTED_LVALUE);

    /* Now increment the non-LVALUE */
    switch (dest->type)
    {
    default:
        errorf("Reference to bad type %s to ++/--\n", typename(dest->type));
        break;

    case T_NUMBER:
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
        STORE_DOUBLE(dest, d);

        if (post)
        {
            post->type = T_FLOAT;
            STORE_DOUBLE(post, d);
        }
        break;
      }

    case T_PROTECTED_LVALUE:
        add_number_to_lvalue(dest, i, pre, post);
        break;

    case T_CHAR_LVALUE:
        if (pre) put_number(pre, (unsigned char)*dest->u.charp);
        *(dest->u.charp) += i;
        if (post) put_number(post, (unsigned char)*dest->u.charp);
        break;

    case T_PROTECTED_CHAR_LVALUE:
      {
        struct protected_char_lvalue *p;

        p = (struct protected_char_lvalue *)dest;
        if (p->lvalue->type == T_STRING
         && get_txt(p->lvalue->u.str) == p->start)
        {
            if (pre) put_number(pre, (unsigned char)*(p->v.u.charp));
            i = (unsigned char)(*(p->v.u.charp) += i);
            if (post) put_number(post, i);
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
            assign_checked_svalue_no_free (d++, s++);
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
            assign_checked_svalue_no_free (d++, s++);
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

    memsafe(str = new_mstring(p), strlen(p), "string");
    put_string(sp, str);
} /* put_c_string() */

/*-------------------------------------------------------------------------*/
void
put_c_n_string (svalue_t *sp, const char *p, size_t len)

/* Put a copy of first <len> characters of the C string *<p> into <sp>.
 */

{
    string_t * str;

    memsafe(str = new_n_mstring(p, len), len, "string");
    put_string(sp, str);
} /* put_c_n_string() */

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
push_svalue_block (int num, svalue_t *v)

/* Push all <num> svalues starting at <v> onto the stack as defined by
 * <inter_sp>. Same semantic as assign_svalue_no_free().
 */

{
    svalue_t *w;

    for (w = inter_sp; --num >= 0; v++)
    {
        w++;
        assign_lrvalue_no_free(w, v);
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
 * This means that a new T_LVALUE is created on the stack, pointing
 * to <arg>. <arg> itself is setup to be a T_ERROR_HANDLER value.
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
 * Some typical layouts:
 *
 *     (LVALUE) -> indexed svalue from vector/mapping
 *                 (might be copied into indexing_quickfix)
 *
 *       by: push_(r)indexed_lvalue()
 *           (r)index_lvalue()
 *
 *
 *     (LVALUE) -> (CHAR_LVALUE)
 *                 special_lvalue.u.charp -> character in untabled string
 *
 *       by: (r)index_lvalue() on string lvalues
 *
 *
 *     (LVALUE) -> (PROTECTED_LVALUE) -> indexed svalue in vector/mapping
 *                 protector          -> vector/mapping
 *       by: push_protected_(r)indexed_lvalue()
 *           push_protected_indexed_map_lvalue()
 *           protected_(r)index_lvalue()
 *
 *
 *     (LVALUE) -> (PROTECTED_CHAR_LVALUE)
 *                    .lvalue -> untabled string svalue
 *                    .charp -> indexed character in untabled string
 *                    .start  -> first character of actual string text
 *                    .protector: T_INVALID or the string's .protector value
 *                                if the string itself is result of a protected
 *                                lvalue index.
 *
 *       by: protected_(r)index_lvalue() on string lvalue
 *
 *
 *     (LVALUE) -> (T_{STRING,POINTER}_RANGE_LVALUE)
 *                   special_lvalue.v   -> indexed-on string/vector
 *                   special_lvalue.size:  size of the string/vector
 *                                 .ind1:  lower index
 *                                 .ind2:  upper index
 *
 *       by: range_lvalue()
 *
 *
 *     (LVALUE) -> (T_PROTECTED_{STRING,POINTER}_RANGE_LVALUE)
 *                   .v   :   indexed-on string/vector
 *                   .lvalue -> svalue of indexed-on string/vector
 *                   .size:  size of the string/vector
 *                   .ind1:  lower index
 *                   .ind2:  upper index
 *                   .protector: the protector of the initial lvalue, if any.
 *
 *       by: protected_range_lvalue()
 *
 *
 * TODO: A lot of the functions differ only in minute details - test how
 * TODO:: much time merging the functions (and adding if()s for the
 * TODO:: differences) really costs.
 *-------------------------------------------------------------------------
 * The functions (in a LPCish notation) are:
 *
 *   push_indexed_lvalue(vector|mapping v, int|mixed i)
 *     Return &(v[i]), unprotected.
 *   push_rindexed_lvalue(vector v, int i)
 *     Return &(v[<i]), unprotected.
 *   push_aindexed_lvalue(vector v, int i)
 *     Return &(v[>i]), unprotected.
 *   push_protected_indexed_lvalue(vector|mapping v, int|mixed i)
 *     Return &(v[i]), protected.
 *   push_protected_rindexed_lvalue(vector v, int i)
 *     Return &(v[<i]), protected.
 *   push_protected_aindexed_lvalue(vector v, int i)
 *     Return &(v[>i]), protected.
 *   push_protected_indexed_map_lvalue(mapping m, mixed i, int j)
 *     Return &(m[i:j]), protected.
 *   index_lvalue(vector|mapping|string & v, int|mixed i)
 *     Return &(*v[i]), unprotected, using special_lvalue.
 *   rindex_lvalue(vector|string & v, int i)
 *     Return &(*v[<i]), unprotected, using special_lvalue.
 *   aindex_lvalue(vector|string & v, int i)
 *     Return &(*v[>i]), unprotected, using special_lvalue.
 *   protected_index_lvalue(vector|mapping|string & v, int|mixed i)
 *     Return &(*v[i]), protected.
 *   protected_rindex_lvalue(vector|string & v, int i)
 *     Return &(*v[<i]), protected.
 *   protected_aindex_lvalue(vector|string & v, int i)
 *     Return &(*v[>i]), protected.
 *   range_lvalue(vector|string & v, int i2, int i1)
 *     Return &(*v[i1..i2]), unprotected, using special_lvalue.
 *   protected_range_lvalue(vector|string & v, int i2, int i1)
 *     Return &(*v[i1..i2]), protected.
 *   push_indexed_value(string|vector|mapping|struct v, int|mixed i)
 *     Return v[i].
 *   push_rindexed_value(string|vector v, int i)
 *     Return v[<i].
 *   push_aindexed_value(string|vector v, int i)
 *     Return v[>i].
 */

/*-------------------------------------------------------------------------*/
static INLINE svalue_t *
get_vector_item (vector_t * vec, svalue_t * i, svalue_t *sp, bytecode_p pc)

/* Index vector <vec> with index <i> and return the pointer to the
 * indexed item.
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
    else
    {
        ind = i->u.number;
        if (ind < 0)
        {
            ERROR("Illegal index for []: not a positive number.\n");
            /* NOTREACHED */
            return NULL;
        }
        if (ind >= VEC_SIZE(vec))
        {
            ERRORF(("Index for [] out of bounds: %"PRIdPINT
                    ", vector size: %"PRIdPINT"\n"
                   , ind, VEC_SIZE(vec)));
            /* NOTREACHED */
            return NULL;
        }
    }

    /* Compute the indexed element */
    item = &vec->item[ind];
    if (destructed_object_ref(item))
    {
        free_svalue(item);
        put_number(item, 0);
    }

    return item;
} /* get_vector_item() */

/*-------------------------------------------------------------------------*/
static INLINE svalue_t *
get_vector_r_item (vector_t * vec, svalue_t * i, svalue_t *sp, bytecode_p pc)

/* Reverse-index vector <vec> with index <i> and return the pointer to the
 * indexed item.
 * If the index is invalid, throw an error.
 */

{
    p_int ind;
    svalue_t * item;

    if (i->type != T_NUMBER)
    {
        ERRORF(("Illegal index for [<]: got %s, expected number.\n"
               , typename(i->type)
               ));
        return NULL;
    }
    if ((ind = i->u.number) < 0)
    {
        ERROR("Illegal index for [<]: not a positive number.\n");
        return NULL;
    }
    if ( (ind = VEC_SIZE(vec) - ind) < 0
     ||  ind >= VEC_SIZE(vec)
       )
    {
        ERRORF(("Index out of bounds for [<]: %"PRIdPINT", vector size: %"
                PRIdPINT".\n", i->u.number, VEC_SIZE(vec)));
        return NULL;
    }

    /* Compute the indexed element */
    item = &vec->item[ind];
    if (destructed_object_ref(item))
    {
        free_svalue(item);
        put_number(item, 0);
    }

    return item;
} /* get_vector_r_item() */

/*-------------------------------------------------------------------------*/
static INLINE svalue_t *
get_vector_a_item (vector_t * vec, svalue_t * i, svalue_t *sp, bytecode_p pc)

/* Arithmetic-index vector <vec> with index <i> and return the pointer to the
 * indexed item.
 * If the index is invalid, throw an error.
 */

{
    p_int ind;
    svalue_t * item;

    if (i->type != T_NUMBER)
    {
        ERRORF(("Illegal index for [>]: got %s, expected number.\n"
               , typename(i->type)
               ));
        return NULL;
    }
    if (0 > (ind = i->u.number))
        ind = VEC_SIZE(vec) + ind;
    if (ind < 0 || ind >= VEC_SIZE(vec))
    {
        ERRORF(("Index out of bounds for [>]: %"PRIdPINT", vector size: %"
                PRIdPINT".\n"
               , i->u.number, VEC_SIZE(vec)));
        return NULL;
    }

    /* Compute the indexed element */
    item = &vec->item[ind];
    if (destructed_object_ref(item))
    {
        free_svalue(item);
        put_number(item, 0);
    }

    return item;
} /* get_vector_a_item() */

/*-------------------------------------------------------------------------*/
static INLINE char *
get_string_item ( svalue_t * svp, svalue_t * i, Bool make_singular
                , Bool allow_one_past
                , svalue_t *sp, bytecode_p pc)

/* Index string <svp> with index <i> and return the pointer to the
 * indexed item.
 * If <make_singular> is TRUE, <svp> is made an untabled string
 * with just one reference.
 * If <allow_one_past> is TRUE, indexing one past the official end
 * of the string for retrieval purposes is ok. TODO: Remove this.
 * If the index is invalid, throw an error.
 */

{
    mp_int ind;

    if (i->type != T_NUMBER)
    {
        ERRORF(("Illegal index for []: got %s, expected number.\n"
               , typename(i->type)
               ));
        return NULL;
    }
    else
    {
        ind = i->u.number;
        if (ind < 0)
        {
            ERROR("Illegal index for []: not a positive number.\n");
            return NULL;
        }

        if (ind > (mp_int)mstrsize(svp->u.str) )
        {
            ERRORF(("Index out of bounds for []: %"PRIdMPINT
                    ", string length: %zu.\n"
                   , ind, mstrsize(svp->u.str)));
            return NULL;
        }

        if (ind == (mp_int)mstrsize(svp->u.str))
        {
            if (!allow_one_past)
            {
                ERRORF(("Index out of bounds for []: %"PRIdMPINT
                        ", string length: %zu.\n"
                       , ind, mstrsize(svp->u.str)));
                return NULL;
            }
            else if (!runtime_no_warn_deprecated)
                warnf( "Warning: Indexing past string end is deprecated: "
                       "index %"PRIdMPINT", string length: %zu.\n"
                     , ind, mstrsize(svp->u.str)
                     );
        }
    }

    /* The basic idea here was to to create a new copy of the string only
     * if the string is not singular (aka !mstr_singular(svp->u.str)).
     * Unfortunately local variable lvalues are pushed without counting
     * the additional reference, so we now have to play it safe and
     * duplicate the string whenever requested.
     */
    if (make_singular)
    {
        string_t *p;
        
        memsafe(p = unshare_mstring(svp->u.str), mstrsize(svp->u.str)
               , "modifiable string");
        svp->u.str = p;
    }

    return &(get_txt(svp->u.str)[ind]);
} /* get_string_item() */

/*-------------------------------------------------------------------------*/
static INLINE char *
get_string_r_item (svalue_t * svp, svalue_t * i, Bool make_singular
                  , Bool allow_one_past
                  , svalue_t *sp, bytecode_p pc)

/* Reverse-Index string <svp> with index <i> and return the pointer to the
 * indexed item.
 * If <allow_one_past> is TRUE, indexing one past the official end
 * of the string for retrieval purposes is ok. TODO: Remove this.
 * If <make_singular> is TRUE, <svp> is made an untabled string
 * with just one reference.
 * If the index is invalid, throw an error.
 */

{
    mp_int ind;

    if (i->type != T_NUMBER)
    {
        ERRORF(("Illegal index for [<]: got %s, expected number.\n"
               , typename(i->type)
               ));
        return NULL;
    }
    else
    {
        ind = i->u.number;
        if ((ind = i->u.number) < 0)
        {
            ERROR("Illegal index for [<]: not a positive number.\n");
            return NULL;
        }

        /* Compute the real index. Allow ""[<1]. */
        ind = (mp_int)mstrsize(svp->u.str) - ind;
        if (!mstrsize(svp->u.str) && ind == -1)
            ind = 0;

        if ( ind < 0
         ||  ind > (mp_int)mstrsize(svp->u.str)
           )
        {
            ERRORF(("Index out of bounds for [<]: %"PRIdPINT
                    ", string length: %zu\n"
                   , i->u.number, mstrsize(svp->u.str)));
            return NULL;
        }

        if (ind == (mp_int)mstrsize(svp->u.str))
        {
            if (!allow_one_past)
            {
                ERRORF(("Index out of bounds for [<]: %"PRIdMPINT
                        ", string length: %zu.\n"
                       , ind, mstrsize(svp->u.str)));
                return NULL;
            }
            else if (!runtime_no_warn_deprecated)
                warnf( "Warning: Indexing past string end is deprecated: "
                       "index %"PRIdMPINT", string length: %zu.\n"
                     , ind, mstrsize(svp->u.str)
                     );
        }
    }

    /* The basic idea here was to to create a new copy of the string only
     * if the string is not singular (aka !mstr_singular(svp->u.str)).
     * Unfortunately local variable lvalues are pushed without counting
     * the additional reference, so we now have to play it safe and
     * duplicate the string whenever requested.
     */
    if (make_singular)
    {
        string_t *p;
        
        memsafe(p = unshare_mstring(svp->u.str), mstrsize(svp->u.str)
               , "modifiable string");
        svp->u.str = p;
    }

    return &(get_txt(svp->u.str)[ind]);
} /* get_string_r_item() */

/*-------------------------------------------------------------------------*/
static INLINE char *
get_string_a_item (svalue_t * svp, svalue_t * i, Bool make_singular
                  , Bool allow_one_past
                  , svalue_t *sp, bytecode_p pc)

/* Arithmetic-Index string <svp> with index <i> and return the pointer to the
 * indexed item.
 * If <allow_one_past> is TRUE, indexing one past the official end
 * of the string for retrieval purposes is ok. TODO: Remove this.
 * If <make_singular> is TRUE, <svp> is made an untabled string
 * with just one reference.
 * If the index is invalid, throw an error.
 */

{
    mp_int ind;

    if (i->type != T_NUMBER)
    {
        ERRORF(("Illegal index for [>]: got %s, expected number.\n"
               , typename(i->type)
               ));
        return NULL;
    }
    else
    {
        ind = i->u.number;

        if (0 > ind)
        {
            /* Compute the real index. Allow ""[<1]. */
            ind = (mp_int)mstrsize(svp->u.str) + ind;
            if (!mstrsize(svp->u.str) && ind == -1)
                ind = 0;
        }
        if (ind < 0 || ind > (mp_int)mstrsize(svp->u.str))
        {
            ERRORF(("Index out of bounds for [>]: %"PRIdPINT
                    ", string length: %zu\n"
                   , i->u.number, mstrsize(svp->u.str)));
            return NULL;
        }

        if (ind == (mp_int)mstrsize(svp->u.str))
        {
            if (!allow_one_past)
            {
                ERRORF(("Index out of bounds for [>]: %"PRIdMPINT
                        ", string length: %zu.\n"
                       , ind, mstrsize(svp->u.str)));
                return NULL;
            }
            else if (!runtime_no_warn_deprecated)
                warnf( "Warning: Indexing past string end is deprecated: "
                       "index %"PRIdMPINT", string length: %zu.\n"
                     , ind, mstrsize(svp->u.str)
                     );
        }
    }

    /* The basic idea here was to to create a new copy of the string only
     * if the string is not singular (aka !mstr_singular(svp->u.str)).
     * Unfortunately local variable lvalues are pushed without counting
     * the additional reference, so we now have to play it safe and
     * duplicate the string whenever requested.
     */
    if (make_singular)
    {
        string_t *p;
        
        memsafe(p = unshare_mstring(svp->u.str), mstrsize(svp->u.str)
               , "modifiable string");
        svp->u.str = p;
    }

    return &(get_txt(svp->u.str)[ind]);
} /* get_string_a_item() */

/*-------------------------------------------------------------------------*/
static INLINE svalue_t *
check_struct_op (svalue_t * sp, int off_type, int off_value, bytecode_p pc)

/* On the stack are the arguments for a struct indexing operation.
 * In particular: sp[<off_type>]:  the struct type index <idx>
 *               sp[<off_value>]:    <off_value> <= 0: the struct value to idx.
 *               sp[-<off_value>+1]: <off_value> >  0: the struct Lvalue to idx.
 *
 * Check the validity of the indexing operation and thrown an error
 * if invalid.
 *
 * <idx> gives the index of the expected struct type - the
 * operator accepts a struct of this type, or any of its children.
 * An negative <idx> accepts any struct.
 *
 * On success, the <idx> svalue is removed from the stack and the
 * new stack pointer is returned.
 */

{
    short s_index;
    svalue_t * svp;

    /* These two errors can happen with careless funcall(#'->)s */
    if (sp[off_type].type != T_NUMBER)
        ERRORF(("Illegal struct type value: %s, expected a number.\n"
               , typename(sp[off_type].type)
              ));
    if (sp[off_type].u.number >= 0
     && sp[off_type].u.number >= current_prog->num_structs)
    {
        ERRORF(("Too big struct index: %"PRIdPINT", max %hu\n"
               , sp[off_type].u.number, current_prog->num_structs
              ));
    }

    /* Get the struct type index */
    s_index = (short)sp[off_type].u.number;

    if (off_value <= 0 && sp[off_value].type != T_STRUCT)
    {
        ERRORF(("Illegal type to struct->(): %s, expected struct.\n"
               , typename(sp[off_type].type)
              ));
        /* NOTREACHED */
    }

    /* Get the reference to struct svalue to index */

    if (off_value > 0)
    {
        svp = &sp[-off_value+1];

        if (svp->type != T_LVALUE && svp->type != T_PROTECTED_LVALUE)
        {
            ERRORF(("Illegal type to lvalue struct->(): %s value, "
                    "expected struct lvalue.\n"
                   , typename(svp->type)
                  ));
            /* NOTREACHED */
        }

        while (svp->type == T_LVALUE || svp->type == T_PROTECTED_LVALUE)
            svp = svp->u.lvalue;
        if (svp->type != T_STRUCT)
        {
            if (svp->type == T_NUMBER && !svp->u.number)
                ERRORF(("Illegal type to lvalue struct->(): number 0, "
                        "expected struct.\n"
                      ));
            else
                ERRORF(("Illegal type to lvalue struct->(): %s, "
                        "expected struct.\n"
                       , typename(svp->type)
                      ));
            /* NOTREACHED */
        }
    }
    else
        svp = &sp[off_value];

    /* Check if the struct on the stack is of the correct type */
    if (s_index >= 0)
    {
        struct_type_t * pExpected = current_prog->struct_defs[s_index].type;
        struct_type_t * pType = svp->u.strct->type;

        if (!struct_baseof(pExpected, pType))
        {
            string_t * got_name, * exp_name;

            got_name = struct_unique_name(svp->u.strct);
            if (!got_name)
                got_name = struct_name(svp->u.strct);

            exp_name = struct_t_unique_name(pExpected);
            if (!exp_name)
                exp_name = struct_t_name(pExpected);

            ERRORF(("Illegal type to%s struct->(): struct %s, "
                    "expected struct %s.\n"
                   , off_value > 0 ? " lvalue" : ""
                   , get_txt(got_name)
                   , get_txt(exp_name)
                  ));
        }
    }

    /* Remove the type index entry from the stack */
    if (off_type != 0)
    {
        for ( ; off_type < 0; off_type++)
            sp[off_type] = sp[off_type+1];
    }

    return sp-1;
} /* check_struct_op() */

/*-------------------------------------------------------------------------*/
static INLINE svalue_t *
get_struct_item (struct_t * st, svalue_t * i, svalue_t *sp, bytecode_p pc)

/* Index struct <st> with index <i> and return the pointer to the
 * indexed item.
 * If the index is invalid, throw an error.
 */

{
    p_int ind;
    svalue_t * item;

    if (i->type == T_SYMBOL || i->type == T_STRING)
    {
        ind = struct_find_member(st->type, i->u.str);
        if (ind < 0)
        {
            ERRORF(("Illegal struct '%s'->(): member '%s' not found.\n"
                   , get_txt(struct_name(st))
                   , get_txt(i->u.str)
                   ));
            /* NOTREACHED */
            return NULL;
        }
    }
    else if (i->type != T_NUMBER)
    {
        ERRORF(("Illegal struct '%s'->(): got %s, "
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
            ERRORF(("Illegal struct '%s'->(): not a positive number.\n"
                   , get_txt(struct_name(st))
                  ));
            /* NOTREACHED */
            return NULL;
        }
        if (ind >= struct_size(st))
        {
            ERRORF(("Illegal struct '%s'->: out of bounds: "
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
static INLINE svalue_t *
push_indexed_lvalue (svalue_t *sp, bytecode_p pc)

/* Operator F_PUSH_INDEXED_LVALUE(vector  v=sp[-1], int   i=sp[0])
 * Operator F_PUSH_INDEXED_LVALUE(mapping v=sp[-1], mixed i=sp[0])
 * Operator F_PUSH_INDEXED_S_LVALUE(struct v=sp[-1], mixed i=sp[0])
 *
 * Compute the lvalue &(v[i]) and push it into the stack. If v has just
 * one ref left, the indexed item is stored in indexing_quickfix and the
 * lvalue refers to that variable.
 * TODO: indexing_quickfix could be implemented using protected lvalues.
 */

{
    svalue_t *i;     /* the index value */
    svalue_t *vec;   /* the indexed vector or mapping */

    /* Get the arguments */
    i = sp;
    vec = sp - 1;

    /* Index a vector.
     */
    if (vec->type == T_POINTER)
    {
        svalue_t *item;

        item = get_vector_item(vec->u.vec, i, sp, pc);

        if (vec->u.vec->ref == 1)
        {
            /* Rescue the indexed item as vec will go away */
            assign_svalue (&indexing_quickfix, item);
            item = &indexing_quickfix;
        }

        /* Remove the arguments from the stack */
        sp = vec;
        free_array(vec->u.vec);

        /* Return the result */
        vec->type = T_LVALUE;
        vec->u.lvalue = item;
        return sp;
    }

    /* Index a struct.
     */
    if (vec->type == T_STRUCT)
    {
        struct_t * st = vec->u.strct;
        svalue_t * item;

        item = get_struct_item(st, i, sp, pc);

        if (st->ref == 1)
        {
            /* Rescue the indexed item as st will go away */
            assign_svalue (&indexing_quickfix, item);
            item = &indexing_quickfix;
        }

        /* Remove the arguments from the stack */
        free_svalue(sp); sp--;
        free_struct(st);

        /* Return the result */
        sp->type = T_LVALUE;
        sp->u.lvalue = item;
        return sp;
    }

    /* Index a mapping
     */
    if (vec->type == T_MAPPING)
    {
        mapping_t *m;
        svalue_t *item;

        m = vec->u.map;

        if (!m->num_values)
        {
            ERROR("Indexing a mapping of width 0.\n");
            return NULL;
        }

        /* Compute the indexed element */

        item = get_map_lvalue(m, i);
        if (!item)
        {
            outofmemory("indexed lvalue");
            /* NOTREACHED */
            return NULL;
        }

        if (m->ref == 1)
        {
            /* Rescue the indexed item as vec will go away */
            assign_svalue (&indexing_quickfix, item);
            item = &indexing_quickfix;
        }

        /* Remove the arguments from the stack */
        free_svalue(sp--);
        free_mapping(m);

        /* Return the result */
        vec->type = T_LVALUE;
        vec->u.lvalue = item;
        return sp;
    }

    /* Illegal type to index */
    inter_sp = sp;
    inter_pc = pc;
    errorf("(lvalue1)Indexing on illegal type '%s'.\n", typename(vec->type));
    return sp;
} /* push_indexed_lvalue() */

/*-------------------------------------------------------------------------*/
static INLINE svalue_t *
push_rindexed_lvalue (svalue_t *sp, bytecode_p pc)

/* Operator F_PUSH_RINDEXED_LVALUE(vector v=sp[-1], int i=sp[0])
 *
 * Compute the lvalue &(v[<i]) and push it into the stack. If v has just
 * one ref left, the indexed item is stored in indexing_quickfix and the
 * lvalue refers to that variable.
 */

{
    svalue_t *i;     /* the index value */
    svalue_t *vec;   /* the vector */

    /* Get the arguments */
    i = sp;
    vec = sp - 1;

    /* Index a vector.
     */
    if (vec->type == T_POINTER)
    {
        svalue_t *item;

        item = get_vector_r_item(vec->u.vec, i, sp, pc);

        if (vec->u.vec->ref == 1)
        {
            /* Rescue the indexed item as vec will go away */
            assign_svalue (&indexing_quickfix, item);
            item = &indexing_quickfix;
        }

        /* Remove the arguments from the stack */
        sp = vec;
        free_array(vec->u.vec);

        /* Return the result */
        vec->type = T_LVALUE;
        vec->u.lvalue = item;
        return sp;
    }

    /* Indexing on illegal type */
    inter_sp = sp;
    inter_pc = pc;
    errorf("(lvalue2)Indexing on illegal type '%s'.\n", typename(vec->type));
    return NULL;
} /* push_rindexed_lvalue() */

/*-------------------------------------------------------------------------*/
static INLINE svalue_t *
push_aindexed_lvalue (svalue_t *sp, bytecode_p pc)

/* Operator F_PUSH_AINDEXED_LVALUE(vector v=sp[-1], int i=sp[0])
 *
 * Compute the lvalue &(v[>i]) and push it into the stack. If v has just
 * one ref left, the indexed item is stored in indexing_quickfix and the
 * lvalue refers to that variable.
 */

{
    svalue_t *i;     /* the index value */
    svalue_t *vec;   /* the vector */

    /* Get the arguments */
    i = sp;
    vec = sp - 1;

    /* Index a vector.
     */
    if (vec->type == T_POINTER)
    {
        svalue_t *item;

        item = get_vector_a_item(vec->u.vec, i, sp, pc);

        if (vec->u.vec->ref == 1)
        {
            /* Rescue the indexed item as vec will go away */
            assign_svalue (&indexing_quickfix, item);
            item = &indexing_quickfix;
        }

        /* Remove the arguments from the stack */
        sp = vec;
        free_array(vec->u.vec);

        /* Return the result */
        vec->type = T_LVALUE;
        vec->u.lvalue = item;
        return sp;
    }

    /* Indexing on illegal type */
    inter_sp = sp;
    inter_pc = pc;
    errorf("(lvalue3)Indexing on illegal type '%s'.\n", typename(vec->type));
    return NULL;
} /* push_aindexed_lvalue() */

/*-------------------------------------------------------------------------*/
/* void BUILD_MAP_PROTECTOR(svalue_t *dest, mapping_t *m)
 *
 * Init svalue <dest> to protectively hold mapping <m> in which one entry
 * is about to be used as target for a lvalue.
 *
 * If mapping <m> is dirty, protect its hash_mapping part by incrementing
 * its refcount (and if this is the first call, also initialize the .deleted
 * entry), and by making the svalue a T_PROTECTOR_MAPPING.
 *
 * If <m> is not dirty, not protection is necessary.
 */
#define BUILD_MAP_PROTECTOR(dest, m)     \
{                                        \
    mapping_hash_t *hm;                  \
                                         \
    if ( NULL != (hm = (m)->hash) ) {    \
        if (!hm->ref++)                  \
            hm->deleted = NULL;          \
        dest.type = T_PROTECTOR_MAPPING; \
    } else {                             \
        dest.type = T_MAPPING;           \
    }                                    \
    dest.u.map = m;                      \
}

/*-------------------------------------------------------------------------*/
static INLINE svalue_t *
push_protected_indexed_lvalue (svalue_t *sp, bytecode_p pc)

/* Op. F_PUSH_PROTECTED_INDEXED_LVALUE(vector  v=sp[-1], int   i=sp[0])
 * Op. F_PUSH_PROTECTED_INDEXED_LVALUE(mapping v=sp[-1], mixed i=sp[0])
 * Op. F_PUSH_PROTECTED_INDEXED_S_LVALUE(struct  v=sp[-1], mixed i=sp[0])
 *
 * Compute the lvalue &(v[i]), store it in a struct protected_lvalue, and
 * push the protector as PROTECTED_LVALUE into the stack.
 */

{
    svalue_t           * i;       /* the index */
    svalue_t           * vec;     /* the vector */

    /* Get the arguments */
    i = sp;
    vec = sp - 1;

    /* Index a vector.
     */
    if (vec->type == T_POINTER)
    {
        svalue_t *item;
        struct protected_lvalue * lvalue;

        item = get_vector_item(vec->u.vec, i, sp, pc);

        /* Compute the indexed item and set up the protector */

        lvalue = (struct protected_lvalue *)xalloc(sizeof *lvalue);
        lvalue->v.type = T_PROTECTED_LVALUE;
        lvalue->v.u.lvalue = item;
        put_array(&(lvalue->protector), vec->u.vec);
          /* The one ref to vec is transferred from *vec */

        /* Remove the arguments and return the result */
        sp = vec;
        vec->type = T_LVALUE;
        vec->u.lvalue = &lvalue->v;
        return sp;
    }

    /* Index a struct.
     */
    if (vec->type == T_STRUCT)
    {
        struct_t * st = vec->u.strct;
        svalue_t * item;
        struct protected_lvalue * lvalue;

        item = get_struct_item(st, i, sp, pc);

        /* Item and set up the protector */

        lvalue = (struct protected_lvalue *)xalloc(sizeof *lvalue);
        lvalue->v.type = T_PROTECTED_LVALUE;
        lvalue->v.u.lvalue = item;
        put_struct(&(lvalue->protector), st);
          /* The one ref to st is transferred from *vec */

        /* Remove the arguments and return the result */
        free_svalue(i);
        sp = vec;
        sp->type = T_LVALUE;
        sp->u.lvalue = &lvalue->v;
        return sp;
    }

    /* Index a mapping
     */
    if (vec->type == T_MAPPING)
    {
        mapping_t *m;
        svalue_t *item;
        struct protected_lvalue * lvalue;

        m = vec->u.map;

        if (!m->num_values)
        {
            ERROR("Indexing a mapping of width 0.\n");
            return NULL;
        }

        /* Compute the indexed item and set up the protector */

        item = get_map_lvalue(m, i);
        if (!item)
        {
            outofmemory("indexed lvalue");
            /* NOTREACHED */
            return NULL;
        }

        lvalue = (struct protected_lvalue *)xalloc(sizeof *lvalue);
        lvalue->v.type = T_PROTECTED_LVALUE;
        lvalue->v.u.lvalue = item;
        BUILD_MAP_PROTECTOR(lvalue->protector, m)
          /* The one ref is transferred from the stack */

        /* Remove the arguments and return the result */
        pop_stack();
        vec->type = T_LVALUE;
        vec->u.lvalue = &lvalue->v;
        return sp;
    }

    /* Indexing on illegal type. */

    inter_sp = sp;
    inter_pc = pc;
    errorf("(lvalue4)Indexing on illegal type '%s'.\n", typename(vec->type));
    return NULL;
} /* push_protected_indexed_lvalue() */

/*-------------------------------------------------------------------------*/
static INLINE svalue_t *
push_protected_rindexed_lvalue (svalue_t *sp, bytecode_p pc)

/* Op. F_PUSH_PROTECTED_RINDEXED_LVALUE(vector v=sp[-1], int i=sp[0])
 *
 * Compute the lvalue &(v[<i]), store it in a struct protected_lvalue, and
 * push the protector as PROTECTED_LVALUE into the stack.
 */

{
    svalue_t           * i;       /* the index */
    svalue_t           * vec;     /* the vector */
    struct protected_lvalue * lvalue;  /* the protector */

    /* Get the arguments */

    i = sp;
    vec = sp - 1;

    /* Index a vector
     */
    if (vec->type == T_POINTER)
    {
        svalue_t *item;

        item = get_vector_r_item(vec->u.vec, i, sp, pc);

        /* Set up the protector */

        lvalue = (struct protected_lvalue *)xalloc(sizeof *lvalue);
        lvalue->v.type = T_PROTECTED_LVALUE;
        lvalue->v.u.lvalue = item;
        put_array(&(lvalue->protector), vec->u.vec);
          /* The one ref is transferred from the stack */

        /* Remove arguments and return result */
        sp = vec;
        vec->type = T_LVALUE;
        vec->u.lvalue = &lvalue->v;
        return sp;
    }

    /* Indexing in illegal type */

    inter_sp = sp;
    inter_pc = pc;
    errorf("(lvalue5)Indexing on illegal type '%s'.\n", typename(vec->type));
    return NULL;
} /* push_protected_rindexed_lvalue() */

/*-------------------------------------------------------------------------*/
static INLINE svalue_t *
push_protected_aindexed_lvalue (svalue_t *sp, bytecode_p pc)

/* Op. F_PUSH_PROTECTED_AINDEXED_LVALUE(vector v=sp[-1], int i=sp[0])
 *
 * Compute the lvalue &(v[>i]), store it in a struct protected_lvalue, and
 * push the protector as PROTECTED_LVALUE into the stack.
 */

{
    svalue_t           * i;       /* the index */
    svalue_t           * vec;     /* the vector */
    struct protected_lvalue * lvalue;  /* the protector */

    /* Get the arguments */

    i = sp;
    vec = sp - 1;

    /* Index a vector
     */
    if (vec->type == T_POINTER)
    {
        svalue_t *item;

        item = get_vector_a_item(vec->u.vec, i, sp, pc);

        /* Setup the protector */

        lvalue = (struct protected_lvalue *)xalloc(sizeof *lvalue);
        lvalue->v.type = T_PROTECTED_LVALUE;
        lvalue->v.u.lvalue = item;
        put_array(&(lvalue->protector), vec->u.vec);
          /* The one ref is transferred from the stack */

        /* Remove arguments and return result */
        sp = vec;
        vec->type = T_LVALUE;
        vec->u.lvalue = &lvalue->v;
        return sp;
    }

    /* Indexing in illegal type */

    inter_sp = sp;
    inter_pc = pc;
    errorf("(lvalue6)Indexing on illegal type '%s'.\n", typename(vec->type));
    return NULL;
} /* push_protected_aindexed_lvalue() */

/*-------------------------------------------------------------------------*/
static INLINE svalue_t *
push_protected_indexed_map_lvalue (svalue_t *sp, bytecode_p pc)

/* Op. F_PUSH_PROTECTED_INDEXED_MAP_LVALUE(mapping m=sp[-2], mixed i=sp[-1]
 *                                                         , int   j=sp[0])
 *
 * Compute the lvalue &(m[i:j]), store it in a struct protected_lvalue, and
 * push the protector as PROTECTED_LVALUE into the stack.
 */

{
    svalue_t           * i;       /* the index */
    svalue_t           * vec;     /* the vector */
    svalue_t           * item;    /* the indexed element */
    struct protected_lvalue * lvalue;  /* the protector */

    /* Get the arguments */
    i = sp - 1;
    vec = sp - 2;

    /* Index a mapping.
     */
    if (vec->type == T_MAPPING)
    {
        mapping_t *m;

        m = vec->u.map;
        if (sp->type != T_NUMBER)
        {
            ERRORF(("Illegal subindex for []: got %s, expected number.\n"
                   , typename(sp->type)
                   ));
            return NULL;
        }
        if ((p_uint)sp->u.number >= (p_uint)m->num_values
            /* using uints automagically checks for negative indices */
           )
        {
            ERRORF(("Too big subindex for []: value %"PRIdPINT", width %"
                    PRIdPINT".\n", sp->u.number, m->num_values));
            return NULL;
        }

        /* Compute the indexed element and setup the protector */

        item = get_map_lvalue(m, i);
        if (!item)
        {
            outofmemory("indexed lvalue");
            /* NOTREACHED */
            return NULL;
        }
        item += sp->u.number;

        lvalue = (struct protected_lvalue *)xalloc(sizeof *lvalue);
        lvalue->v.type = T_PROTECTED_LVALUE;
        lvalue->v.u.lvalue = item;
        BUILD_MAP_PROTECTOR(lvalue->protector, m)
          /* The one ref is transferred from the stack */

        /* Remove the arguments and return the result */
        sp--;
        pop_stack();
        vec->type = T_LVALUE;
        vec->u.lvalue = &lvalue->v;
        return sp;
    }

    /* Indexing on illegal type */

    inter_sp = sp;
    inter_pc = pc;
    errorf("(lvalue7)Indexing on illegal type '%s'.\n", typename(vec->type));
    return NULL;
} /* push_protected_indexed_map_lvalue() */

/*-------------------------------------------------------------------------*/
static INLINE svalue_t *
index_lvalue (svalue_t *sp, bytecode_p pc)

/* Operator F_INDEX_LVALUE (string|vector &v=sp[0], int   i=sp[-1])
 *          F_INDEX_LVALUE (mapping       &v=sp[0], mixed i=sp[-1])
 *          F_INDEX_S_LVALUE (struct      &v=sp[0], mixed i=sp[-1])
 *
 * Compute the index &(v[i]) of lvalue <v> and push it into the stack. The
 * computed index is a lvalue itself.
 * If <v> is a string-lvalue, it is made a malloced string if necessary,
 * and the pushed result will be a lvalue pointing to a CHAR_LVALUE stored
 * in <special_lvalue>.
 */

{
    svalue_t *vec;   /* the vector/mapping */
    svalue_t *i;     /* the index */
    short     type;  /* type of <vec> */

    /* get the arguments */
    vec = sp;
    i = sp -1;

    /* Dereference the initial (and possibly more) lvalue-indirection
     */
    do {
        vec = vec->u.lvalue;
        type = vec->type;
    } while (type == T_LVALUE || type == T_PROTECTED_LVALUE);

    /* Index a vector.
     */
    if (type == T_POINTER)
    {
        vector_t *v = vec->u.vec;
        svalue_t *item;

        item = get_vector_item(v, i, sp, pc);

        /* Remove the arguments and push the result */

        sp = i;

        sp->type = T_LVALUE;
        sp->u.lvalue = item;
        return sp;
    }

    /* Index a string.
     */
    if (type == T_STRING)
    {
        char * cp;

        cp = get_string_item(vec, i, /* make_singular: */ MY_TRUE
                            , /* allow_one_past: */ MY_FALSE
                            , sp, pc);

        /* Remove the arguments and create and push the result. */

        sp = i;

        sp->type = T_LVALUE;
        sp->u.lvalue = &special_lvalue.v;
        special_lvalue.v.type = T_CHAR_LVALUE;
        special_lvalue.v.u.charp = cp;
        return sp;
    }

    /* Index a struct.
     */
    if (type == T_STRUCT)
    {
        struct_t * st = vec->u.strct;
        svalue_t * item;

        item = get_struct_item(st, i, sp, pc);
        
        /* Remove the arguments and push the result */

        sp--; /* *sp is a T_LVALUE and can be dropped silently  */
        free_svalue(sp); /* This was 'i' */

        sp->type = T_LVALUE;
        sp->u.lvalue = item;
        return sp;
    }

    /* Index a mapping.
     */
    if (type == T_MAPPING)
    {
        svalue_t *item;
        mapping_t *m;

        m = vec->u.map;
        if (!m->num_values)
        {
            ERROR("Indexing a mapping of width 0.\n");
            return NULL;
        }

        /* Compute the element */
        item = get_map_lvalue(m, i);
        if (!item)
        {
            outofmemory("indexed lvalue");
            /* NOTREACHED */
            return NULL;
        }

        /* Remove the arguments and push the result. */

        sp = i;
        free_svalue(i);
        sp->type = T_LVALUE;
        sp->u.lvalue = item;
        return sp;
    }

    /* Illegal type to index. */

    inter_sp = sp;
    inter_pc = pc;
    errorf("(lvalue8)Indexing on illegal type '%s'.\n", typename(type));
    return NULL;
} /* index_lvalue() */

/*-------------------------------------------------------------------------*/
static INLINE svalue_t *
rindex_lvalue (svalue_t *sp, bytecode_p pc)

/* Operator F_RINDEX_LVALUE (string|vector &v=sp[0], int   i=sp[-1])
 *
 * Compute the index &(v[<i]) of lvalue <v> and push it into the stack. The
 * computed index is a lvalue itself.
 * If <v> is a string-lvalue, it is made a malloced string if necessary,
 * and the pushed result will be a lvalue pointing to a CHAR_LVALUE stored
 * in <special_lvalue>.
 */

{
    svalue_t *vec;   /* the vector/string */
    svalue_t *i;     /* the index */
    short     type;  /* type of <vec> */

    /* get the arguments */
    vec = sp;
    i = sp -1;

    /* Dereference the initial (and possibly more) lvalue-indirection
     */
    do {
        vec = vec->u.lvalue;
        type = vec->type;
    } while (type == T_LVALUE || type == T_PROTECTED_LVALUE);

    /* Index a vector
     */
    if (type == T_POINTER)
    {
        vector_t *v = vec->u.vec;
        svalue_t *item;

        item = get_vector_r_item(v, i, sp, pc);

        /* Remove the arguments and return the result */

        sp = i;
        sp->type = T_LVALUE;
        sp->u.lvalue = item;
        return sp;
    }

    /* Index a string
     */
    if (type == T_STRING)
    {
        char * cp;

        cp = get_string_r_item(vec, i, /* make_singular: */ MY_TRUE
                              , /* allow_one_past: */ MY_FALSE
                              , sp, pc);

        /* Remove the argument and return the result */

        sp = i;
        sp->type = T_LVALUE;
        sp->u.lvalue = &special_lvalue.v;
        special_lvalue.v.type = T_CHAR_LVALUE;
        special_lvalue.v.u.charp = cp;
        return sp;
    }

    /* Indexing on illegal type */

    inter_sp = sp;
    inter_pc = pc;
    errorf("(lvalue9)Indexing on illegal type '%s'.\n", typename(type));
    return NULL;
} /* rindex_lvalue() */

/*-------------------------------------------------------------------------*/
static INLINE svalue_t *
aindex_lvalue (svalue_t *sp, bytecode_p pc)

/* Operator F_AINDEX_LVALUE (string|vector &v=sp[0], int   i=sp[-1])
 *
 * Compute the index &(v[>i]) of lvalue <v> and push it into the stack. The
 * computed index is a lvalue itself.
 * If <v> is a string-lvalue, it is made a malloced string if necessary,
 * and the pushed result will be a lvalue pointing to a CHAR_LVALUE stored
 * in <special_lvalue>.
 */

{
    svalue_t *vec;   /* the vector/string */
    svalue_t *i;     /* the index */
    short     type;  /* type of <vec> */

    /* get the arguments */
    vec = sp;
    i = sp -1;

    /* Dereference the initial (and possibly more) lvalue-indirection
     */
    do {
        vec = vec->u.lvalue;
        type = vec->type;
    } while (type == T_LVALUE || type == T_PROTECTED_LVALUE);

    /* Index a vector
     */
    if (type == T_POINTER)
    {
        vector_t *v = vec->u.vec;
        svalue_t *item;

        item = get_vector_a_item(v, i, sp, pc);

        /* Remove the arguments and return the result */

        sp = i;
        sp->type = T_LVALUE;
        sp->u.lvalue = item;
        return sp;
    }

    /* Index a string
     */
    if (type == T_STRING)
    {
        char * cp;

        cp = get_string_a_item(vec, i, /* make_singular: */ MY_TRUE
                              , /* allow_one_past: */ MY_FALSE
                              , sp, pc);

        /* Remove the argument and return the result */

        sp = i;
        sp->type = T_LVALUE;
        sp->u.lvalue = &special_lvalue.v;
        special_lvalue.v.type = T_CHAR_LVALUE;
        special_lvalue.v.u.charp = cp;
        return sp;
    }

    /* Indexing on illegal type */

    inter_sp = sp;
    inter_pc = pc;
    errorf("(lvalue10)Indexing on illegal type '%s'.\n", typename(type));
    return NULL;
} /* aindex_lvalue() */

/*-------------------------------------------------------------------------*/
static svalue_t *
protected_index_lvalue (svalue_t *sp, bytecode_p pc)

/* Operator F_PROTECTED_INDEX_LVALUE (string|vector &v=sp[0], int   i=sp[-1])
 *          F_PROTECTED_INDEX_LVALUE (mapping       &v=sp[0], mixed i=sp[-1])
 *          F_PROTECTED_INDEX_S_LVALUE (struct      &v=sp[0], mixed i=sp[-1])
 *
 * Compute the index &(*v[i]) of lvalue <v>, wrap it into a protector, and push
 * the reference to the protector as PROTECTED_LVALUE onto the stack.
 *
 * If <v> is a protected non-string-lvalue, the protected_lvalue referenced
 * by <v>.u.lvalue will be deallocated, and the protector itself will be
 * stored in <last_indexing_protector> for the time being.
 *
 * If <v> is a string-lvalue, it is made a malloced string if necessary.
 */

{
    svalue_t *vec;   /* the indexed value */
    svalue_t *i;     /* the index */
    short     type;  /* type of <vec> */

    /* Get arguments */
    vec = sp->u.lvalue;
    i = sp -1;

    /* The loop unravels the (possible) lvalue chain starting at vec.
     * When a non-lvalue is encountered, the indexing takes place
     * the function returns.
     */
    for (;;)
    {
        type = vec->type;

        /* Index a vector.
         */
        if (type == T_POINTER)
        {
            vector_t *v = vec->u.vec;
            struct protected_lvalue *lvalue;
            svalue_t *item;

            item = get_vector_item(v, i, sp, pc);

            /* Drop the arguments */
            sp = i;

            /* Compute and return the result */

            lvalue = (struct protected_lvalue *)xalloc(sizeof *lvalue);
            lvalue->v.type = T_PROTECTED_LVALUE;
            lvalue->v.u.lvalue = item;
            put_ref_array(&(lvalue->protector), v);

            sp->type = T_LVALUE;
            sp->u.lvalue = &lvalue->v;

            return sp;
        }

        /* Index a struct.
         */
        if (type == T_STRUCT)
        {
            struct_t * st = vec->u.strct;
            svalue_t * item;
            struct protected_lvalue *lvalue;

            item = get_struct_item(st, i, sp, pc);

            /* Drop the arguments */
            free_svalue(i);
            sp = i;

            /* Compute and return the result */

            lvalue = (struct protected_lvalue *)xalloc(sizeof *lvalue);
            lvalue->v.type = T_PROTECTED_LVALUE;
            lvalue->v.u.lvalue = item;
            put_ref_struct(&(lvalue->protector), st);

            sp->type = T_LVALUE;
            sp->u.lvalue = &lvalue->v;

            return sp;
        }

        /* Index a string.
         */
        if (type == T_STRING)
        {
            struct protected_char_lvalue *val;
            char * cp;

            cp = get_string_item(vec, i, /* make_singular: */ MY_TRUE
                                , /* allow_one_past: */ MY_FALSE
                                , sp, pc);

            /* Add another reference to the string to keep it alive while
             * we use it.
             */
            (void)ref_mstring(vec->u.str);

            /* Drop the arguments */
            sp = i;

            /* Compute and return the result */

            val = (struct protected_char_lvalue *)xalloc(sizeof *val);
            val->v.type = T_PROTECTED_CHAR_LVALUE;
            val->v.u.charp = cp;
            val->lvalue = vec;
            val->start = get_txt(vec->u.str);
            val->protector.type = T_INVALID;

            sp->type = T_LVALUE;
            sp->u.protected_char_lvalue = val;

            return sp;
        }

        /* Index a mapping.
         */
        if (type == T_MAPPING)
        {
            svalue_t *item;
            struct protected_lvalue *lvalue;
            mapping_t *m;

            m = vec->u.map;
            if (!m->num_values)
            {
                ERROR("Indexing a mapping of width 0.\n");
                return NULL;
            }

            /* Compute the indexed element */
            item = get_map_lvalue(m, i);
            if (!item)
            {
                outofmemory("indexed lvalue");
                /* NOTREACHED */
                return NULL;
            }

            /* Build the protector */
            ref_mapping(m);
            lvalue = (struct protected_lvalue *)xalloc(sizeof *lvalue);
            lvalue->v.type = T_PROTECTED_LVALUE;
            lvalue->v.u.lvalue = item;
            BUILD_MAP_PROTECTOR(lvalue->protector, m)

            /* Drop the arguments and return the result */
            sp = i;
            free_svalue(i);

            sp->type = T_LVALUE;
            sp->u.lvalue = &lvalue->v;

            return sp;
        }

        /* lvalues are just dereferenced.
         */
        if (type == T_LVALUE)
        {
            vec = vec->u.lvalue;
            continue;
        }

        /* Non-string protected lvalues are dereferenced, a protected
         * string lvalue is indexed immediately.
         */
        if (type == T_PROTECTED_LVALUE)
        {
            struct protected_lvalue *lvalue;
            struct protected_char_lvalue *val;
            char * cp;

            lvalue = (struct protected_lvalue *)vec;

            if (lvalue->v.u.lvalue->type != T_STRING)
            {
                /* Deref a non-string protected lvalue.
                 * If this is the lvalue passed to the operator, also free
                 * the protector structure (since its stack space will be
                 * used for the result), but keep the protector itself
                 * in a global variable.
                 */
                if (vec == sp->u.lvalue)
                {
                    free_protector_svalue(&last_indexing_protector);
                    last_indexing_protector = lvalue->protector;
                    vec = lvalue->v.u.lvalue;
                    xfree(lvalue);
                    continue;
                }

                vec = lvalue->v.u.lvalue;
                continue;
            }

            vec = lvalue->v.u.lvalue; /* it's a string... */

            cp = get_string_item(vec, i, /* make_singular: */ MY_TRUE
                                , /* allow_one_past: */ MY_FALSE
                                , sp, pc);

            /* Add another reference to the string to keep it alive while
             * we use it.
             */
            (void)ref_mstring(vec->u.str);

            /* Build the protector */
            val = (struct protected_char_lvalue *)xalloc(sizeof *val);
            val->v.type = T_PROTECTED_CHAR_LVALUE;
            val->v.u.charp = cp;
            val->lvalue = vec;
            val->start = get_txt(vec->u.str);

            /* Drop the arguments and return the result.
             * If this was the lvalue passed to the operator in the
             * first place, adopt the protecting value and free the old
             * operator structure. If not, just don't assign a protecting
             * value.
             */
            if (lvalue == sp->u.protected_lvalue)
            {
                val->protector = lvalue->protector;
                xfree(lvalue);
            }
            else
            {
                val->protector.type = T_INVALID;
            }

            sp = i;
            sp->type = T_LVALUE;
            sp->u.protected_char_lvalue = val;

            return sp;
        }

        /* Indexing on illegal type */
        inter_sp = sp;
        inter_pc = pc;
        errorf("(lvalue11)Indexing on illegal type '%s'.\n", typename(type));
        return NULL;
    } /* for(ever) */

    /* NOTREACHED */
    return NULL;
} /* protected_index_lvalue() */

/*-------------------------------------------------------------------------*/
static INLINE svalue_t *
protected_rindex_lvalue (svalue_t *sp, bytecode_p pc)

/* Operator F_PROTECTED_RINDEX_LVALUE (string|vector &v=sp[0], int   i=sp[-1])
 *
 * Compute the index &(*v[<i]) of lvalue <v>, wrap it into a protector, and
 * push the reference to the protector as PROTECTED_LVALUE onto the stack.
 *
 * If <v> is a protected non-string-lvalue, the protected_lvalue referenced
 * by <v>.u.lvalue will be deallocated, and the protector itself will be
 * stored in <last_indexing_protector> for the time being.
 *
 * If <v> is a string-lvalue, it is made a malloced string if necessary.
 */

{
    svalue_t *vec;   /* the indexed value */
    svalue_t *i;     /* the index */
    short     type;  /* type of <vec> */

    /* Get arguments */
    vec = sp->u.lvalue;
    i = sp -1;

    /* The loop unravels the (possible) lvalue chain starting at vec.
     * When a non-lvalue is encountered, the indexing takes place
     * the function returns.
     */
    for (;;)
    {
        type = vec->type;

        /* Index a vector.
         */
        if (type == T_POINTER)
        {
            vector_t *v = vec->u.vec;
            struct protected_lvalue *lvalue;
            svalue_t *item;

            item = get_vector_r_item(v, i, sp, pc);

            /* Create the protector for the result */

            lvalue = (struct protected_lvalue *)xalloc(sizeof *lvalue);
            lvalue->v.type = T_PROTECTED_LVALUE;
            lvalue->v.u.lvalue = item;
            put_ref_array(&(lvalue->protector), v);

            /* Drop the arguments and return the result */

            sp = i;

            sp->type = T_LVALUE;
            sp->u.lvalue = &lvalue->v;
            return sp;
        }

        /* Index a string.
         */
        if (type == T_STRING)
        {
            struct protected_char_lvalue *val;
            char * cp;

            cp = get_string_r_item(vec, i, /* make_singular: */ MY_TRUE
                                  , /* allow_one_past: */ MY_FALSE
                                  , sp, pc);

            /* Add another reference to the string to keep it alive while
             * we use it.
             */
            (void)ref_mstring(vec->u.str);

            /* Build the protector */
            val = (struct protected_char_lvalue *)xalloc(sizeof *val);
            val->v.type = T_PROTECTED_CHAR_LVALUE;
            val->v.u.charp = cp;
            val->lvalue = vec;
            val->start = get_txt(vec->u.str);
            val->protector.type = T_INVALID;

            /* Drop the arguments and return the result */
            sp = i;

            sp->type = T_LVALUE;
            sp->u.protected_char_lvalue = val;

            return sp;
        }

        /* lvalues are just dereferenced.
         */
        if (type == T_LVALUE)
        {
            vec = vec->u.lvalue;
            continue;
        }

        /* Non-string protected lvalues are dereferenced, a protected
         * string lvalue is indexed immediately.
         */
        if (type == T_PROTECTED_LVALUE)
        {
            struct protected_lvalue *lvalue;
            struct protected_char_lvalue *val;
            char * cp;

            lvalue = (struct protected_lvalue *)vec;

            if (lvalue->v.u.lvalue->type != T_STRING)
            {
                /* Deref a non-string protected lvalue.
                 * If this is the lvalue passed to the operator, also free
                 * the protector structure (since its stack space will be
                 * used for the result), but keep the protector itself
                 * in a global variable.
                 */
                if (vec == sp->u.lvalue)
                {
                    free_protector_svalue(&last_indexing_protector);
                    last_indexing_protector = lvalue->protector;
                    vec = lvalue->v.u.lvalue;
                    xfree(lvalue);
                    continue;
                }
                vec = lvalue->v.u.lvalue;
                continue;
            }

            vec = lvalue->v.u.lvalue; /* it's a string... */
            cp = get_string_r_item(vec, i, /* make_singular: */ MY_TRUE
                                  , /* allow_one_past: */ MY_FALSE
                                  , sp, pc);

            /* Add another reference to the string to keep it alive while
             * we use it.
             */
            (void)ref_mstring(vec->u.str);

            /* Build the protector */
            val = (struct protected_char_lvalue *)xalloc(sizeof *val);
            val->v.type = T_PROTECTED_CHAR_LVALUE;
            val->v.u.charp = cp;
            val->lvalue = vec;
            val->start = get_txt(vec->u.str);

            /* Drop the arguments and return the result.
             * If this was the lvalue passed to the operator in the
             * first place, adopt the protecting value and free the old
             * operator structure. If not, just don't assign a protecting
             * value.
             */
            if (lvalue == sp->u.protected_lvalue)
            {
                val->protector = lvalue->protector;
                xfree(lvalue);
            }
            else
            {
                val->protector.type = T_INVALID;
            }

            sp = i;
            sp->type = T_LVALUE;
            sp->u.protected_char_lvalue = val;

            return sp;
        }

        /* Indexing on illegal type */
        inter_sp = sp;
        inter_pc = pc;
        errorf("(lvalue12)Indexing on illegal type '%s'.\n", typename(type));
        return NULL;
    } /* for(ever) */

    /* NOTREACHED */
    return NULL;
} /* protected_rindex_lvalue() */

/*-------------------------------------------------------------------------*/
static INLINE svalue_t *
protected_aindex_lvalue (svalue_t *sp, bytecode_p pc)

/* Operator F_PROTECTED_AINDEX_LVALUE (string|vector &v=sp[0], int   i=sp[-1])
 *
 * Compute the index &(*v[>i]) of lvalue <v>, wrap it into a protector, and
 * push the reference to the protector as PROTECTED_LVALUE onto the stack.
 *
 * If <v> is a protected non-string-lvalue, the protected_lvalue referenced
 * by <v>.u.lvalue will be deallocated, and the protector itself will be
 * stored in <last_indexing_protector> for the time being.
 *
 * If <v> is a string-lvalue, it is made a malloced string if necessary.
 */

{
    svalue_t *vec;   /* the indexed value */
    svalue_t *i;     /* the index */
    short     type;  /* type of <vec> */

    /* Get arguments */
    vec = sp->u.lvalue;
    i = sp -1;

    /* The loop unravels the (possible) lvalue chain starting at vec.
     * When a non-lvalue is encountered, the indexing takes place
     * the function returns.
     */
    for (;;)
    {
        type = vec->type;

        /* Index a vector.
         */
        if (type == T_POINTER)
        {
            vector_t *v = vec->u.vec;
            struct protected_lvalue *lvalue;
            svalue_t *item;

            item = get_vector_a_item(v, i, sp, pc);

            /* Create the protector for the result */

            lvalue = (struct protected_lvalue *)xalloc(sizeof *lvalue);
            lvalue->v.type = T_PROTECTED_LVALUE;
            lvalue->v.u.lvalue = item;
            put_ref_array(&(lvalue->protector), v);

            /* Drop the arguments and return the result */

            sp = i;

            sp->type = T_LVALUE;
            sp->u.lvalue = &lvalue->v;
            return sp;
        }

        /* Index a string.
         */
        if (type == T_STRING)
        {
            struct protected_char_lvalue *val;
            char * cp;

            cp = get_string_a_item(vec, i, /* make_singular: */ MY_TRUE
                                  , /* allow_one_past: */ MY_FALSE
                                  , sp, pc);

            /* Add another reference to the string to keep it alive while
             * we use it.
             */
            (void)ref_mstring(vec->u.str);

            /* Build the protector */
            val = (struct protected_char_lvalue *)xalloc(sizeof *val);
            val->v.type = T_PROTECTED_CHAR_LVALUE;
            val->v.u.charp = cp;
            val->lvalue = vec;
            val->start = get_txt(vec->u.str);
            val->protector.type = T_INVALID;

            /* Drop the arguments and return the result */
            sp = i;

            sp->type = T_LVALUE;
            sp->u.protected_char_lvalue = val;

            return sp;
        }

        /* lvalues are just dereferenced.
         */
        if (type == T_LVALUE)
        {
            vec = vec->u.lvalue;
            continue;
        }

        /* Non-string protected lvalues are dereferenced, a protected
         * string lvalue is indexed immediately.
         */
        if (type == T_PROTECTED_LVALUE)
        {
            struct protected_lvalue *lvalue;
            struct protected_char_lvalue *val;
            char * cp;

            lvalue = (struct protected_lvalue *)vec;

            if (lvalue->v.u.lvalue->type != T_STRING)
            {
                /* Deref a non-string protected lvalue.
                 * If this is the lvalue passed to the operator, also free
                 * the protector structure (since its stack space will be
                 * used for the result), but keep the protector itself
                 * in a global variable.
                 */
                if (vec == sp->u.lvalue)
                {
                    free_protector_svalue(&last_indexing_protector);
                    last_indexing_protector = lvalue->protector;
                    vec = lvalue->v.u.lvalue;
                    xfree(lvalue);
                    continue;
                }
                vec = lvalue->v.u.lvalue;
                continue;
            }

            vec = lvalue->v.u.lvalue; /* it's a string... */
            cp = get_string_a_item(vec, i, /* make_singular: */ MY_TRUE
                                  , /* allow_one_past: */ MY_FALSE
                                  , sp, pc);

            /* Add another reference to the string to keep it alive while
             * we use it.
             */
            (void)ref_mstring(vec->u.str);

            /* Build the protector */
            val = (struct protected_char_lvalue *)xalloc(sizeof *val);
            val->v.type = T_PROTECTED_CHAR_LVALUE;
            val->v.u.charp = cp;
            val->lvalue = vec;
            val->start = get_txt(vec->u.str);

            /* Drop the arguments and return the result.
             * If this was the lvalue passed to the operator in the
             * first place, adopt the protecting value and free the old
             * operator structure. If not, just don't assign a protecting
             * value.
             */
            if (lvalue == sp->u.protected_lvalue)
            {
                val->protector = lvalue->protector;
                xfree(lvalue);
            }
            else
            {
                val->protector.type = T_INVALID;
            }

            sp = i;
            sp->type = T_LVALUE;
            sp->u.protected_char_lvalue = val;

            return sp;
        }

        /* Indexing on illegal type */
        inter_sp = sp;
        inter_pc = pc;
        errorf("(lvalue13)Indexing on illegal type '%s'.\n", typename(type));
        return NULL;
    } /* for(ever) */

    /* NOTREACHED */
    return NULL;
} /* protected_aindex_lvalue() */

/*-------------------------------------------------------------------------*/

/* Code values used by range_lvalue() and protected_range_lvalue()
 */

#define NN_RANGE (0)
#define RN_RANGE (1 << 0)
#define AN_RANGE (2 << 0)
#define NR_RANGE (1 << 2)
#define NA_RANGE (2 << 2)

#define RR_RANGE (RN_RANGE|NR_RANGE)
#define RA_RANGE (RN_RANGE|NA_RANGE)
#define AR_RANGE (AN_RANGE|NR_RANGE)
#define AA_RANGE (AN_RANGE|NA_RANGE)

#define NX_MASK (3)
#define XN_MASK (3 << 2)

static svalue_t *
range_lvalue (int code, svalue_t *sp)

/* Operator F_RANGE_LVALUE (string|vector &v=sp[0], int i2=sp[-1], i1=sp[-2])
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
    svalue_t *vec;         /* the indexed vector or string */
    svalue_t *i;           /* the index */
    int            ind1, ind2;  /* Lower and upper range index */
    short          type;        /* type of <vec> */
    mp_int         size;        /* size of <vec> in elements */

    /* Get the arguments */
    vec = sp;
    i = sp-1;

#ifdef DEBUG
    if (sp->type != T_LVALUE) {
        inter_sp = sp;
        errorf("wrong type to range_lvalue: got %s, expected lvalue\n"
             , typename(sp->type));
        return NULL;
    }
#endif

    /* Deref the initial, and possibly more, lvalues.
     */
    do {
        vec = vec->u.lvalue;
        type = vec->type;
    } while (type == T_LVALUE || type == T_PROTECTED_LVALUE);

    /* Determine the type of the result, and the input's size.
     */
    switch(type)
    {
    case T_POINTER:
        special_lvalue.v.type = T_POINTER_RANGE_LVALUE;
        size = (mp_int)VEC_SIZE(vec->u.vec);
        break;
    case T_STRING:
        special_lvalue.v.type = T_STRING_RANGE_LVALUE;
        size = (mp_int)mstrsize(vec->u.str);
        break;
    default:
        inter_sp = sp;
        errorf("(lvalue)Range index on illegal type '%s'.\n", typename(type));
        return NULL;
    }

    /* Get and check the upper bound i2 */

    if (i->type != T_NUMBER)
    {
        inter_sp = sp;
        errorf("Illegal upper range index: got '%s', expected 'number'.\n"
             , typename(i->type));
        return NULL;
    }

    if ((code & XN_MASK) == NR_RANGE)
    {
        ind2 = size - i->u.number;
    }
    else if ((code & XN_MASK) == NA_RANGE)
    {
        if (i->u.number < 0)
            ind2 = size + i->u.number;
        else
            ind2 = i->u.number;
    }
    else
    {
        ind2 = i->u.number;
    }


    if (++ind2 < 0 || ind2 > size+1)
    {
        inter_sp = sp;
        errorf("Upper range index out of bounds: %"PRIdPINT
               ", size: %"PRIdMPINT".\n"
             , i->u.number, size);
        return NULL;
    }

    /* Get and check the lower bound i1 */

    if ((--i)->type != T_NUMBER)
    {
        inter_sp = sp;
        errorf("Illegal lower range index: got %s, expected number.\n"
             , typename(i->type));
        return NULL;
    }

    if ((code & NX_MASK) == RN_RANGE)
    {
        ind1 = size - i->u.number;
    }
    else if ((code & NX_MASK) == AN_RANGE)
    {
        if (i->u.number < 0)
            ind1 = size + i->u.number;
        else
            ind1 = i->u.number;
    }
    else
    {
        ind1 = i->u.number;
    }

    if (ind1 < 0 || ind1 > size)
    {   /* Appending (ind1 == size) is allowed */
        inter_sp = sp;
        errorf("Lower range index out of bounds: %"PRIdPINT
               ", size: %"PRIdMPINT".\n"
             , i->u.number, size);
        return NULL;
    }


    /* Check the range for consistency */

    if (ind2 < ind1)
    {
        inter_sp = sp;
        errorf("Range of negative size given: %"PRIdPINT
               "..%"PRIdPINT" .\n"
             , i->u.number, (i+1)->u.number);
        return NULL;
    }

    if (ind1 == size) /* again allow appending */
        ind2 = ind1;
    else if (ind2 > size)
    {
        inter_sp = sp;
        errorf("Upper range index out of bounds: %"PRIdPINT
               ", size: %"PRIdMPINT".\n"
             , (i+1)->u.number, size);
        return NULL;
    }

    /* Finish the special_lvalue structure
     */
    special_lvalue.v.u.lvalue = vec;
    special_lvalue.size = size;
    special_lvalue.index1 = ind1;
    special_lvalue.index2 = ind2;

    /* Drop the arguments and return the result. */

    sp = i;

    sp->type = T_LVALUE;
    sp->u.lvalue = &special_lvalue.v;

    return sp;
} /* range_lvalue() */

/*-------------------------------------------------------------------------*/
static svalue_t *
protected_range_lvalue (int code, svalue_t *sp)

/* Operator F_PROTECTED_RANGE_LVALUE
 *                       (string|vector &v=sp[0], int i2=sp[-1], i1=sp[-2])
 * and the x-operators F_PROTECTED_*_RANGE_LVALUE and
 * F_PROTECTED_LVALUE.
 *
 * Compute the range &(v[i1..i2]) of lvalue <v>, wrap it into a protector,
 * and push the reference to the protector onto the stack.
 *
 * If <v> is a protected lvalue itself, its protecting svalue will be used
 * in the result protector.
 *
 * If <v> is a string-lvalue, it is made a malloced string if necessary.
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
    svalue_t *vec;         /* the indexed vector or string */
    svalue_t *i;           /* the index */
    int            ind1, ind2;  /* Lower and upper range index */
    short          type;        /* type of <vec> */
    mp_int         size;        /* size of <vec> in elements */
    short          lvalue_type; /* Result type */
    svalue_t  protector;   /* Protecting svalue saved from v */
    struct protected_range_lvalue *new_lvalue;
                                /* Result protector structure */

#ifdef DEBUG
    if (sp->type != T_LVALUE)
    {
        inter_sp = sp;
        errorf("wrong type to protected_range_lvalue: got %s, expected lvalue\n"
             , typename(sp->type));
        return NULL;
    }
#endif
    /* Get the arguments, and also remember the protector in case v
     * is a protected lvalue.
     */
    vec = sp->u.lvalue; /* deref initial lvalue */
    i = sp - 1;

    type = vec->type;

    if (type != T_PROTECTED_LVALUE)
        protector.type = T_INVALID;
    else
        protector = ((struct protected_lvalue*)vec)->protector;

    /* Deref any possibly following lvalues
     */
    while (type == T_LVALUE || type == T_PROTECTED_LVALUE)
    {
        vec = vec->u.lvalue;
        type = vec->type;
    }

    /* Determine the type of the result, and the input's size.
     * Also massage the input value a bit.
     */
    switch(type)
    {
    case T_POINTER:
        (void)ref_array(vec->u.vec); /* Count the coming protector */
        lvalue_type = T_PROTECTED_POINTER_RANGE_LVALUE;
        size = (mp_int)VEC_SIZE(vec->u.vec);
        break;

    case T_STRING:
        /* If the string is tabled, i.e. not changeable, or has more than
         * one reference, allocate a new copy which can be changed safely.
         */
        if (!mstr_singular(vec->u.str))
        {
            string_t *p;

            memsafe(p = unshare_mstring(vec->u.str), mstrsize(vec->u.str)
                   , "modifiable string");
            vec->u.str = p;
        }

        /* Add another reference to the string to keep it alive while
         * we use it.
         */
        (void)ref_mstring(vec->u.str);

        lvalue_type = T_PROTECTED_STRING_RANGE_LVALUE;
        size = (mp_int)mstrsize(vec->u.str);
        break;

    default:
        inter_sp = sp;
        errorf("(lvalue)Range index on illegal type '%s'.\n", typename(type));
        return NULL;
    }

    /* Get and check the upper index i2 */

    if (i->type != T_NUMBER)
    {
        inter_sp = sp;
        errorf("Illegal upper range index: got '%s', expected 'number'.\n"
             , typename(i->type));
        return NULL;
    }

    if ((code & XN_MASK) == NR_RANGE)
    {
        ind2 = size - i->u.number;
    }
    else if ((code & XN_MASK) == NA_RANGE)
    {
        if (i->u.number < 0)
            ind2 = size + i->u.number;
        else
            ind2 = i->u.number;
    }
    else
    {
        ind2 = i->u.number;
    }

    if (++ind2 < 0 || ind2 > size) {
        inter_sp = sp;
        errorf("Upper range index out of bounds: %"PRIdPINT
               ", size: %"PRIdMPINT".\n"
             , i->u.number, size);
        return NULL;
    }

    /* Get and check the lower index i1 */

    if ((--i)->type != T_NUMBER)
    {
        inter_sp = sp;
        errorf("Illegal lower range index: got %s, expected number.\n"
             , typename(i->type));
        return NULL;
    }

    if ((code & NX_MASK) == RN_RANGE)
    {
        ind1 = size - i->u.number;
    }
    else if ((code & NX_MASK) == AN_RANGE)
    {
        if (i->u.number < 0)
            ind1 = size + i->u.number;
        else
            ind1 = i->u.number;
    }
    else
    {
        ind1 = i->u.number;
    }

    if (ind1 < 0 || ind1 > size)
    {
        /* Appending (ind1 == size) is allowed */
        inter_sp = sp;
        errorf("Lower range index out of bounds: %"PRIdPINT
               ", size: %"PRIdMPINT".\n"
             , i->u.number, size);
        return NULL;
    }

    /* Build the protector */
    new_lvalue = (struct protected_range_lvalue *)xalloc(sizeof *new_lvalue);
    new_lvalue->v.type = lvalue_type;
    new_lvalue->v.u = vec->u;
    new_lvalue->protector = protector;
    new_lvalue->lvalue = vec;
    new_lvalue->index2 = ind2;
    new_lvalue->index1 = ind1;
    new_lvalue->size = size;

    /* Drop the arguments and return the result */

    sp = i;

    sp->type = T_LVALUE;
    sp->u.protected_range_lvalue = new_lvalue;

    return sp;
} /* protected_range_lvalue() */

/*-------------------------------------------------------------------------*/
static INLINE svalue_t *
push_indexed_value (svalue_t *sp, bytecode_p pc)

/* Operator F_INDEX (string|vector v=sp[-1], int   i=sp[0])
 *          F_INDEX (mapping       v=sp[-1], mixed i=sp[0])
 *          F_S_INDEX (struct      v=sp[-1], string|int i=sp[0])
 *
 * Compute the value (v[i]) and push it onto the stack.
 * If the value would be a destructed object, 0 is pushed onto the stack
 * and the ref to the object is removed from the vector/mapping.
 */

{
    svalue_t *vec;  /* the indexed value */
    svalue_t *i;    /* the index */

    /* Get arguments */
    i = sp;
    vec = sp - 1;

    switch (vec->type)
    {
    case T_STRING:
      {
        int c;

        c = (unsigned char)
            *get_string_item(vec, i, /* make_singular: */ MY_FALSE
                            , /* allow_one_past: */ MY_TRUE
                            , sp, pc);

        /* Drop the args and return the result */

        free_string_svalue(vec);

        sp = vec; /* == sp-1 */
        put_number(sp, c);

        return sp;
      }

    case T_POINTER:
      {
        svalue_t *item;

        item = get_vector_item(vec->u.vec, i, sp, pc);

        /* Drop the arguments */
        sp = vec; /* == sp-1 */

        /* Assign the indexed element to the sp entry holding vec.
         * Decrement the vector ref manually to optimize the case that
         * this is the last ref to the vector.
         */
        if (vec->u.vec->ref == 1)
        {
            svalue_t tmp;

            /* Copy the indexed element into <tmp>
             */
            tmp = *item;

            /* Invalidate the old space of the result value and free
             * the vector.
             */
            item->type = T_INVALID;
            free_array(vec->u.vec);

            /* Return the result */
            *sp = tmp;
            return sp;
        }

        deref_array(vec->u.vec);

        /* The vector continues to live: keep the refcount as it is
         * and just assign the indexed element for the result.
         */
        assign_checked_svalue_no_free(sp, item);
        return sp;
      }

    case T_MAPPING:
      {
        svalue_t item;
        mapping_t *m;

        m = vec->u.map;

        if (!m->num_values)
        {
            inter_sp = sp;
            inter_pc = pc;
            errorf("(value)Indexing a mapping of width 0.\n");
            return NULL;
        }

        /* Get the item.
         * We are getting a copy in case the subsequent free() actions
         * free the mapping and all it's data.
         */
        assign_checked_svalue_no_free(&item, get_map_value(m, i));

        /* Drop the arguments */

        free_svalue(i);
        free_mapping(m);

        /* Return the result */
        sp = vec; /* == sp-1 */
        transfer_svalue_no_free(sp, &item);
        return sp;
      }

    case T_STRUCT:
      {
        struct_t * st = vec->u.strct;
        svalue_t * item;

        item = get_struct_item(st, i, sp, pc);

        /* Drop the 'i' argument */
        free_svalue(sp);
        sp--;

        /* Assign the value */
        assign_svalue_no_free(sp, item);

        /* Drop the struct reference */
        free_struct(st);
        return sp;
      }

    default:
        inter_sp = sp;
        inter_pc = pc;
        errorf("(value)Indexing on illegal type '%s'.\n", typename(vec->type));
        return NULL;
    }

    /* NOTREACHED */
    return NULL;
} /* push_indexed_value() */

/*-------------------------------------------------------------------------*/
static INLINE svalue_t *
push_rindexed_value (svalue_t *sp, bytecode_p pc)

/* Operator F_RINDEX (string|vector v=sp[0], int   i=sp[-1])
 *
 * Compute the value (v[<i]) and push it onto the stack.
 * If the value would be a destructed object, 0 is pushed onto the stack
 * and the ref to the object is removed from the vector/mapping.
 */

{
    svalue_t *vec;  /* the indexed value */
    svalue_t *i;    /* the index */

    /* Get arguments */
    i = sp;
    vec = sp - 1;

    switch (vec->type)
    {
    case T_STRING:
      {
        int c;

        c = (unsigned char)
            *get_string_r_item(vec, i, /* make_singular: */ MY_FALSE
                              , /* allow_one_past: */ MY_TRUE
                              , sp, pc);

        /* Drop the args and return the result */

        free_string_svalue(vec);

        sp = vec; /* == sp-1 */
        put_number(sp, c);
        return sp;
      }

    case T_POINTER:
      {
        svalue_t *item;

        item = get_vector_r_item(vec->u.vec, i, sp, pc);

        /* Drop the arguments */
        sp = vec;

        /* Assign the indexed element to the sp entry holding vec.
         * Decrement the vector ref manually to optimize the case that
         * this is the last ref to the vector.
         */
        if (vec->u.vec->ref == 1)
        {
            svalue_t tmp;

            /* Copy the indexed element into <tmp>
             */
            tmp = *item;

            /* Invalidate the old space of the result value and free
             * the vector.
             */
            item->type = T_INVALID;
            free_array(vec->u.vec);

            /* Return the result */
            *sp = tmp;
            return sp;
        }

        deref_array(vec->u.vec);

        /* The vector continues to live: keep the refcount as it is
         * and just assign the indexed element for the result.
         */
        assign_checked_svalue_no_free(sp, item);
        return sp;
      }

    default:
        inter_sp = sp;
        inter_pc = pc;
        errorf("(lvalue)Range index on illegal type '%s'.\n", typename(vec->type));
        return NULL;
    }

    /* NOTREACHED */
    return NULL;
} /* push_rindexed_value() */

/*-------------------------------------------------------------------------*/
static INLINE svalue_t *
push_aindexed_value (svalue_t *sp, bytecode_p pc)

/* Operator F_AINDEX (string|vector v=sp[0], int   i=sp[-1])
 *
 * Compute the value (v[>i]) and push it onto the stack.
 * If the value would be a destructed object, 0 is pushed onto the stack
 * and the ref to the object is removed from the vector/mapping.
 */

{
    svalue_t *vec;  /* the indexed value */
    svalue_t *i;    /* the index */

    /* Get arguments */
    i = sp;
    vec = sp - 1;

    switch (vec->type)
    {
    case T_STRING:
      {
        int c;

        c = (unsigned char)
            *get_string_a_item(vec, i, /* make_singular: */ MY_FALSE
                              , /* allow_one_past: */ MY_TRUE
                              , sp, pc);

        /* Drop the args and return the result */

        free_string_svalue(vec);

        sp = vec; /* == sp-1 */
        put_number(sp, c);
        return sp;
      }

    case T_POINTER:
      {
        svalue_t *item;

        item = get_vector_a_item(vec->u.vec, i, sp, pc);

        /* Drop the arguments */
        sp = vec;

        /* Assign the indexed element to the sp entry holding vec.
         * Decrement the vector ref manually to optimize the case that
         * this is the last ref to the vector.
         */
        if (vec->u.vec->ref == 1)
        {
            svalue_t tmp;

            /* Copy the indexed element into <tmp>
             */
            tmp = *item;

            /* Invalidate the old space of the result value and free
             * the vector.
             */
            item->type = T_INVALID;
            free_array(vec->u.vec);

            /* Return the result */
            *sp = tmp;
            return sp;
        }

        deref_array(vec->u.vec);

        /* The vector continues to live: keep the refcount as it is
         * and just assign the indexed element for the result.
         */
        assign_checked_svalue_no_free(sp, item);
        return sp;
      }

    default:
        inter_sp = sp;
        inter_pc = pc;
        errorf("(lvalue)Range index on illegal type '%s'.\n", typename(vec->type));
        return NULL;
    }

    /* NOTREACHED */
    return NULL;
} /* push_aindexed_value() */

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

    assign_svalue_no_free( vip->svp++, data + vip->num);
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
        assign_svalue_no_free(vip->svp[i+1].u.vec->item + vip->num, data+i);
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
    if (num >= current_object->prog->num_variables)
    {
        fatal("Illegal variable access %d(%d).\n",
            num, current_object->prog->num_variables);
    }
#endif
    return &current_variables[num];
} /* find_value() */

/*-------------------------------------------------------------------------*/
static INLINE svalue_t *
find_virtual_value (int num)

/* For the virtually inherited variable <num> (given as index within
 * the current object's variable block) return the address of the actual
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
    inherit_t *inheritp;
    program_t *progp;
    char *progpp; /* actually a program_t **, but some compilers... */

    /* Make sure that we are not calling from a set_this_object()
     * context.
     */
    if (is_sto_context())
    {
        errorf("find_virtual_value: Can't execute with "
              "set_this_object() in effect.\n"
             );
    }

    /* Set inheritp to the inherited program which defines variable <num>
     */
    inheritp = current_prog->inherit;
    while
      (   inheritp->variable_index_offset + inheritp->prog->num_variables <= num
       || inheritp->variable_index_offset > num)
    {
        inheritp++;
    }

    /* Get the index of the variable within the inherited program.
     */
    num -= inheritp->variable_index_offset;

    /* Set inheritp to the first instance of this inherited program.
     * A cleaner, but slighly slower way to write the following segment
     * is: for (inheritp = current_object->prog_inherit
     *         ; inheritp->prog != progp
     *         ; inheritp++) NOOP;
     */
    progp = inheritp->prog;
    progpp = (char *)&current_object->prog->inherit->prog;
    while (*(program_t **)progpp != progp)
        progpp += sizeof(inherit_t);
    inheritp = (inherit_t *)
                 (((PTRTYPE)(progpp)) - offsetof(inherit_t, prog));

    /* Compute the actual variable address */

    num += inheritp->variable_index_offset;

#ifdef DEBUG
    if (!current_object->variables
     || num >= current_object->prog->num_variables
       )
    {
        if (num)
            fatal("%s Fatal: find_virtual_value() on object %p '%s' "
                  "w/o variables, num %d\n"
                 , time_stamp(), current_object, get_txt(current_object->name)
                 , num);
        else
            errorf("%s Error: find_virtual_value() on object %p '%s' "
                  "w/o variables, num %d\n"
                 , time_stamp(), current_object, get_txt(current_object->name)
                 , num);
    }
#endif

    return &current_object->variables[num];
      /* TODO: Why not '&current_variables[num]'? */
} /* find_virtual_value() */


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
    long * typep, type;

    typep = &(efun_lpc_types[instrs[instr].lpc_arg_index]);
    for (i = 1; i <= args; i++, typep++, argp++)
    {
        type = *typep;
        if (argp->type == T_NUMBER && !argp->u.number
         && (type & TF_NULL)
           )
            continue;
        if (!(*typep & (1 << argp->type)))
            raise_arg_error(instr, i, *typep, argp->type);
    }
} /* test_efun_args() */

/*-------------------------------------------------------------------------*/
static INLINE svalue_t *
unravel_lvalue_indirection(svalue_t *svp)
// unravels any lvalue indirection if there is one
{
    switch(svp->type)
    {
        // handle lvalues/references.
        case T_LVALUE:
        case T_PROTECTED_LVALUE:
            // trace LVALUE chain to the real svalue.
            while (svp->type == T_LVALUE || svp->type == T_PROTECTED_LVALUE)
                svp = svp->u.lvalue;
            return svp;
            
        // these point directly to the referenced svalue.
        case T_PROTECTED_STRING_RANGE_LVALUE:
        case T_STRING_RANGE_LVALUE:
        case T_PROTECTED_CHAR_LVALUE:
        case T_POINTER_RANGE_LVALUE:
        case T_PROTECTED_POINTER_RANGE_LVALUE:
        case T_PROTECTOR_MAPPING:
            return svp->u.lvalue;
            
    }
    // Fall-through, no lvalue
    return svp;
} // unravel_lvalue_indirection

/*-------------------------------------------------------------------------*/
static INLINE Bool
check_rtt_compatibility_inl(vartype_t formaltype, svalue_t *svp)
// This function checks if <formal_type> and the svalue pointed to by <svp>
// are compatible (that means, it is allowed to assign *svp to an LPC variable
// having the type described by <formal_type>. The function handles lvalues,
// but the lvalue property is ignored for assessing the compatibility.
// returns MY_TRUE if *<svp> is compatible to <formal_type>, MY_FALSE otherwise.
{
    
    svp = unravel_lvalue_indirection(svp);
    
    // mixed accepts anything... Also zero (0) is considered to be an element
    // of any type.
    if ((formaltype.type & PRIMARY_TYPE_MASK) == TYPE_ANY
        || (svp->type == T_NUMBER && svp->u.number == 0))
        return MY_TRUE;
    
    switch(svp->type)
    {
        // These should anyway not happen...
        case T_INVALID:
        case T_CALLBACK:
        case T_ERROR_HANDLER:
        case T_BREAK_ADDR:
            return MY_FALSE;

        case T_STRING:
            if ((formaltype.type & PRIMARY_TYPE_MASK) == TYPE_STRING)
                return MY_TRUE;
            return MY_FALSE;
                        
        case T_POINTER:
        case T_QUOTED_ARRAY:
            // unfortunately, there is no real information about the types of 
            // the vector in the svalue.
            if (formaltype.type & TYPE_MOD_POINTER)
                return MY_TRUE;
            return MY_FALSE;

        case T_MAPPING:
            if ((formaltype.type & PRIMARY_TYPE_MASK) == TYPE_MAPPING)
                return MY_TRUE;
            return MY_FALSE;
        
        case T_CLOSURE:
            if ((formaltype.type & PRIMARY_TYPE_MASK) == TYPE_CLOSURE)
                return MY_TRUE;
            return MY_FALSE;
            
        case T_SYMBOL:
            if ((formaltype.type & PRIMARY_TYPE_MASK) == TYPE_SYMBOL)
                return MY_TRUE;
            return MY_FALSE;
            
        case T_NUMBER:
            if ((formaltype.type & PRIMARY_TYPE_MASK) == TYPE_NUMBER)
                return MY_TRUE;
            return MY_FALSE;
            
        case T_FLOAT:
            if ((formaltype.type & PRIMARY_TYPE_MASK) == TYPE_FLOAT)
                return MY_TRUE;
            return MY_FALSE;
            
        case T_OBJECT:
            if ((formaltype.type & PRIMARY_TYPE_MASK) == TYPE_OBJECT)
                return MY_TRUE;
            return MY_FALSE;
            
        case T_STRUCT:
            if ((formaltype.type & PRIMARY_TYPE_MASK) == TYPE_STRUCT)
            {
                // allow structs having the declared struct as a base.
                if (struct_baseof(formaltype.t_struct, svp->u.strct->type) > 0)
                    return MY_TRUE;
            }
            return MY_FALSE;
        
        // any lvalue indirections must have been unraveled before.
            
    } // switch(svp->type)

    // Fall-through
    return MY_FALSE;
} // check_rtt_compatibility()

Bool
check_rtt_compatibility(vartype_t formaltype, svalue_t *svp) 
{
    return check_rtt_compatibility_inl(formaltype, svp);
}
#define check_rtt_compatibility(ft, svp) check_rtt_compatibility_inl(ft, svp)

/*-------------------------------------------------------------------------*/
static INLINE void
check_function_args(int fx, program_t *progp, fun_hdr_p funstart)
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
        vartype_t *arg_type = progp->argument_types + progp->type_start[fx];
        svalue_t *firstarg = inter_sp - csp->num_local_variables + 1;
        char formal_args = FUNCTION_NUM_ARGS(funstart);
        if (formal_args < 0)
            formal_args &= 0x7f;
        int i = 0;
        while (i < formal_args)
        {
            // do the types match (in case of structs also the structure)
            // or is the formal argument of type TYPE_ANY (mixed)?
            if (!check_rtt_compatibility(arg_type[i], firstarg+i))
            {
                // raise error
                // At this point, a control frame was already created for this call.
                // To attribute error to caller, pop that one from the control stack.
                // But there are some special cases to take care of...
                if (csp > CONTROL_STACK + 1)
                {
                    // at least 3 frames on the stack. We can get rid of at least one of them.
                    pop_control_stack();
                    // int_call_lambda() pushes a dummy control frame with funstart==0. This
                    // has to removed as well if existing.
                    // (Assumption: there are not other control stack frames with funstart==0.)
                    if (!csp->funstart)
                        pop_control_stack();
                }
                else if (csp == CONTROL_STACK + 1)
                {
                    // exactly 2 frames on the stack. We might pop one, but only if the
                    // bottom (remaining) frame is NOT a dummy control frame from
                    // int_call_lambda(). If it is the dummy frame, we would be at the 
                    // start of a top-level evaluation and both frames have to remain.
                    // In that case, we have to set inter_pc to a valid value.
                    if (CONTROL_STACK->funstart)
                        pop_control_stack();
                    else
                        inter_pc = FUNCTION_CODE(funstart);
                }
                else
                {
                    // this is a top-level call. This one frame must stay. But we have to set 
                    // inter_pc because it might be either 0 because no code was evaluated yet 
                    // or it is a left-over from last execution.
                    inter_pc = FUNCTION_CODE(funstart);
                }
                string_t *function_name;
                memcpy(&function_name, FUNCTION_NAMEP(funstart), sizeof function_name);
                fulltype_t ft = { .typeflags = arg_type[i].type, .t_struct = arg_type[i].t_struct };
                // unravel any lvalue indirection (if any)
                svalue_t *svp = unravel_lvalue_indirection(&firstarg[i]);
                errorf("Bad arg %d to %s(): got '%s', expected '%s'.\n"
                       , i+1, get_txt(function_name), typename(svp->type),
                       get_type_name(ft));
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
    add_message(message_flush);
#endif
} /* do_trace() */

/*-------------------------------------------------------------------------*/
static void
do_trace_call (fun_hdr_p funstart, Bool is_lambda)

/* Trace a call to the function starting at <funstart>.
 */

{
    if (!++traceing_recursion || !TRACE_IS_INTERACTIVE()) /* Do not recurse! */
    {
        int save_var_ix_offset = variable_index_offset;
          /* TODO: Might be clobbered, but where? */

        /* Trace the function itself */
        if (is_lambda)
            do_trace("Call direct ", "lambda-closure", " ");
        else
        {
            string_t *name;

            memcpy(&name, FUNCTION_NAMEP(funstart), sizeof name);
            do_trace("Call direct ", get_txt(name), " ");
        }

        /* If requested, also trace the arguments */
        if (TRACEHB())
        {
            if (TRACETST(TRACE_ARGS))
            {
                int i;
                svalue_t *svp;

                add_message(" with %d arguments: "
                           , FUNCTION_NUM_ARGS(funstart) & 0x7f);
                svp = inter_fp;
                for (i = (FUNCTION_NUM_ARGS(funstart) & 0x7f); --i >= 0; )
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
 * occured. <sp> is the current stackpointer and is used to pop the elements
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
     * occured and get the proper values from there.
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
#ifdef USE_NEW_INLINES
    inter_context = csp->context;
#endif /* USE_NEW_INLINES */
    function_index_offset = csp->function_index_offset;
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
 * TODO:: the explicite search. Or some load-time patching.
 */
{
    inherit_t * inh = NULL;

    if (prog != obj->prog
     && inheritp->prog->num_variables
     && (prog->variables[inheritp->variable_index_offset
                         +inheritp->prog->num_variables-1
                        ].type.typeflags & TYPE_MOD_VIRTUAL)
     && !(inheritp->prog->variables[inheritp->prog->num_variables-1
                                   ].type.typeflags & TYPE_MOD_VIRTUAL)
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
             && obj->prog->variables[inh->variable_index_offset
                                     +inh->prog->num_variables-1
                                    ].type.typeflags&TYPE_MOD_VIRTUAL
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
setup_inherited_call (unsigned short inhIndex)

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
            function_index_offset = 0;
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
setup_new_frame2 (fun_hdr_p funstart, svalue_t *sp
                 , Bool allowRefs, Bool is_lambda)

/* Before calling the function at <funstart>, massage the data on the
 * stack ending at <sp> to match the formal argumentlist of the function
 * (excessive args are removed, missing args are provided as 0),
 * and allocate the local variables on the stack.
 *
 * If <allowRefs> is TRUE, references may be passed as extended varargs
 * ('(varargs mixed *)'). Currently this is used only for simul efuns.
 * TODO: Investigate if holding references in arrays is really such a
 * TODO:: a bad thing. Maybe it's just an implementation issue.
 * TODO:: This also affects apply_low() and call_lambda().
 *
 * <is_lambda> has to be TRUE if the function is a lambda closure.
 * This information is needed for proper tracing.
 *
 * csp->num_local_variables is supposed to hold the number of actual
 * arguments on the stack.
 *
 * Result is the new stackpointer, the framepointer <inter_fp>,
 * csp->num_local_variables and <break_sp> are set up.
 * The context pointer <inter_context> is cleared.
 */

{
    int i;        /* Difference between number of formal and actual args;
                   * Number of (uninitialized) local variables
                   */
    int num_arg;  /* Number of formal args */

    /* Setup the frame pointer */
    inter_fp = sp - csp->num_local_variables + 1;

#ifdef USE_NEW_INLINES
    /* By default there is no context */
    inter_context = NULL;
#endif /* USE_NEW_INLINES */

    /* (Re)move excessive arguments.
     * TODO: This code uses that bit7 makes num_arg negative.
     */
    num_arg = FUNCTION_NUM_ARGS(funstart);
    if ((i = csp->num_local_variables - num_arg) > 0)
    {
        /* More actual than formal args, or the function has
         * a 'varargs' argument.
         */

        if (num_arg < 0)
        {
            /* Function has a 'varargs' argument */

            num_arg &= 0x7f;

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
                {
                    if (!allowRefs && sp->type == T_LVALUE)
                        num_arg = -1; /* mark error condition */
                    v->item[i] = *sp--;
                }

                ++sp;
                put_array(sp, v);

                if (num_arg < 0)
                {
                    bytecode_p pc = funstart; /* for the ERROR() macro */

                    ERROR("Varargs argument passed by reference.\n");
                }
            }
        }
        else
        {
            /* Function takes a fixed number of arguments */

            /* Pop the extraneous args */
            do {
                free_svalue(sp--);
                csp->num_local_variables--;
            } while(--i);

        } /* if(varargs or fixedargs) */

        /* Clear the local variables */

        if ( 0 != (i = FUNCTION_NUM_VARS(funstart)) )
        {
            csp->num_local_variables += i;
            do {
                *++sp = const0;
            } while (--i);
        }
    }
    else
    {
        /* Enough or too little arguments supplied to a fixed-args
         * function: initialize the missing args and the locals
         * in one swoop.
         */

        if ( 0 != (i = FUNCTION_NUM_VARS(funstart) - i) )
        {
            csp->num_local_variables += i;
            do {
                *++sp = const0;
            } while (--i);
        }
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

    if (inhProg)
    {
        program_t *progp;
        int       fun_ix_offs;
        int       var_ix_offs;
        
        progp = current_prog;
        fun_ix_offs = 0;
        var_ix_offs = 0;
        
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
        flags = setup_new_frame1(fx, 0, 0);

    /* Setting csp->funstart is not just convenient, but also
     * required for proper error handling in setup_new_frame2()
     */
    csp->funstart = current_prog->program + (flags & FUNSTART_MASK);

    inter_sp = setup_new_frame2(csp->funstart, inter_sp, MY_FALSE, MY_FALSE);
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
    free_protector_svalue(&last_indexing_protector);
    last_indexing_protector.type = T_NUMBER;
    free_svalue(&indexing_quickfix);
    indexing_quickfix.type = T_NUMBER;
    free_svalue(&apply_return_value);
    apply_return_value.type = T_NUMBER;

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
 * function will automatically remove all occurences. The effect is that
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
            ( instrs[full_instr].ret_type.typeflags == TYPE_VOID ? 0 : 1 );
    }
    else if (use_ap)
    {
        expected_stack = ap -
            ( instrs[full_instr].ret_type.typeflags == TYPE_VOID ? 1 : 0 );
    }
    else
    {
        expected_stack = NULL;
    }
#endif /* DEBUG */

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
            memcpy(&name, FUNCTION_NAMEP(FUNCTION_FROM_CODE(pc-1)), sizeof name);
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

        test_efun_args(instruction, max_arg >= 0 ? numarg : min_arg
                      , sp-numarg+1);
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
        assign_checked_svalue_no_free(sp, find_value((int)(LOAD_UINT8(pc))) );
        break;

    CASE(F_STRING);                /* --- string <ix>          --- */
    {
        /* Push the string current_strings[<ix>] onto the stack,
         * <ix> being a (16-Bit) ushort, stored low byte first.
         * See also the F_CSTRINGx functions.
         */
        unsigned short string_number;

        LOAD_SHORT(string_number, pc);
        push_ref_string(sp, current_strings[string_number]);
        break;
    }

    CASE(F_CSTRING3);               /* --- cstring3 <ix>       --- */
    {
        /* Push the string current_strings[0x3<ix>] onto the stack.
         * <ix> is a 8-Bit uint.
         */
        unsigned int ix = LOAD_UINT8(pc);
        push_ref_string(sp, current_strings[ix+0x300]);
        break;
    }

    CASE(F_CSTRING2);               /* --- cstring2 <ix>       --- */
    {
        /* Push the string current_strings[0x2<ix>] onto the stack.
         * <ix> is a 8-Bit uint.
         */
        unsigned int ix = LOAD_UINT8(pc);
        push_ref_string(sp, current_strings[ix+0x200]);
        break;
    }

    CASE(F_CSTRING1);               /* --- cstring1 <ix>       --- */
    {
        /* Push the string current_strings[0x1<ix>] onto the stack.
         * <ix> is a 8-Bit uint.
         */
        unsigned int ix = LOAD_UINT8(pc);
        push_ref_string(sp, current_strings[ix+0x100]);
        break;
    }

    CASE(F_CSTRING0);               /* --- cstring0 <ix>       --- */
    {
        /* Push the string current_strings[0x0<ix>] onto the stack.
         * <ix> is a 8-Bit uint.
         */
        unsigned int ix = LOAD_UINT8(pc);
        push_ref_string(sp, current_strings[ix]);
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
         * TODO: This code makes heavy assumptions about data sizes and
         * TODO:: layout. E.g. there need not be a 16-Bit integral type
         * TODO:: available.
         * TODO: It should be rewritten to use the LOAD_ macros (but
         * TODO:: then the compiler needs to use them, too.
         */

        double zero = 0.0;
        STORE_DOUBLE_USED

        sp++;
        sp->type = T_FLOAT;
        STORE_DOUBLE(sp, zero);
        break;
    }

    CASE(F_FLOAT);                  /* --- float <mant> <exp>  --- */
    {
        /* Push the float build from <mant> (4 bytes) and <exp> (2 bytes)
         * onto the stack. The binary format is the one determined
         * by STORE_DOUBLE in datatypes.h
         * TODO: This code makes heavy assumptions about data sizes and
         * TODO:: layout. E.g. there need not be a 16-Bit integral type
         * TODO:: available.
         * TODO: short doesn't to be a 16 bit wide type (which the float format
         * TODO:: expects). LOAD_INT16 would be nice (change in compiler as well).
         */

        int32_t mantissa;
        int16_t exponent;

        sp++;
        sp->type = T_FLOAT;
        mantissa = load_uint32(&pc);
        exponent = load_uint16(&pc);
        sp->u.mantissa = mantissa;
        sp->x.exponent = exponent;
        break;
    }

    CASE(F_CLOSURE);            /* --- closure <ix> <inhIndex> --- */
#ifdef USE_NEW_INLINES
    CASE(F_CONTEXT_CLOSURE); /* --- context_closure <ix> <vix> <num_ex> <num_im> --- */
#endif /* USE_NEW_INLINES */
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
         *
         * For <inhIndex>:
         * If not 0 for lfun closures, it is the (inheritance index + 1)
         * of the directly referenced inherited function.
#ifdef USE_NEW_INLINES
         *
         * If it is a context closure, the context is sized to 
         * uint16 <num_ex>+<num_in> values, uint16 <num_ex> values
         * are taken from the local variables beginning at <vix>,
         * uint16 <num_im> values are taken from the stack.
#endif
         */

        /* TODO: uint16 */ unsigned short tmp_ushort;
        /* TODO: int32 */ int ix;
        /* TODO: uint16 */ unsigned short inhIndex;
#ifdef USE_NEW_INLINES
        unsigned short explicit_context_size, implicit_context_size;
        svalue_t * explicit_context;
#endif /* USE_NEW_INLINES */

        inhIndex = 0;
#ifdef USE_NEW_INLINES
        explicit_context_size = implicit_context_size = 0;
#endif /* USE_NEW_INLINES */
        LOAD_SHORT(tmp_ushort, pc);
#ifdef USE_NEW_INLINES
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
#else /* USE_NEW_INLINES */
        LOAD_SHORT(inhIndex, pc);
#endif /* USE_NEW_INLINES */

        ix = tmp_ushort;
        if (ix < CLOSURE_EFUN_OFFS)
        {
            sp++;
            inter_sp = sp;
            inter_pc = pc;
#ifndef USE_NEW_INLINES
            closure_literal(sp, ix, inhIndex);
#else /* USE_NEW_INLINES */
            closure_literal(sp, ix, inhIndex, explicit_context_size + implicit_context_size);
#endif /* USE_NEW_INLINES */
            /* If out of memory, this will set sp to svalue-0 and
             * throw an error.
             */

#ifdef USE_NEW_INLINES
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
#endif /* USE_NEW_INLINES */
        }
        else
        {
#ifdef USE_NEW_INLINES
#ifdef DEBUG
            if (instruction == F_CONTEXT_CLOSURE)
                fatal("(eval_instruction) context_closure used for non-lfun.\n");
#endif
#endif /* USE_NEW_INLINES */
            sp++;
            sp->type = T_CLOSURE;
            sp->u.ob = ref_object(current_object, "closure");
            if (ix >= CLOSURE_SIMUL_EFUN_OFFS)
            {
                /* Sefun closure */
                sp->x.closure_type = (short)ix;
            }
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
        else if (efp < sp)
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
            vartype_t rtype = *((vartype_t *)FUNCTION_TYPEP(csp->funstart));
            // functions declared without type may return anything... *sigh*
            if (rtype.type != TYPE_UNKNOWN && !check_rtt_compatibility(rtype, sp))
            {
                string_t *function_name;
                memcpy(&function_name, FUNCTION_NAMEP(csp->funstart), sizeof function_name);
                fulltype_t ft = { .typeflags = rtype.type, .t_struct = rtype.t_struct };
                inter_sp = sp;
                errorf("Bad return type in %s(): got '%s', expected '%s'.\n",
                       get_txt(function_name), typename(sp->type),
                       get_type_name(ft));
            }
        }
        
        /* Restore the previous execution context */
        current_prog = csp->prog;
        if ( NULL != current_prog ) /* is 0 when we reach the bottom */
        {
            current_strings = current_prog->strings;
        }

        function_index_offset = csp->function_index_offset;
        current_variables     = csp->current_variables;
        break_sp = csp->break_sp;
#ifdef USE_NEW_INLINES
        inter_context = csp->context;
#endif /* USE_NEW_INLINES */
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
            else if ( sp->type == T_STRING )
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

    CASE(F_SSCANF);                 /* --- sscanf <numarg>     --- */
    {
        /* EFUN sscanf()
         *
         *   int sscanf(string str, string fmt, mixed var1, mixed var2, ...)
         *
         * Scanf <str> according to <fmt> and store the resultes in var1...
         * The compiler knows that var1... have to be passed as lvalues.
         *
         * Result is the number of variables assigned.
         */
        int i;
        svalue_t *arg;

        num_arg = LOAD_UINT8(pc);
          /* GET_NUM_ARG doesn't work here. Trust me on that. */
        inter_sp = sp;
        inter_pc = pc;
        arg = sp - num_arg + 1;
        if (arg[0].type != T_STRING)
            BAD_ARG_ERROR(1, T_STRING, arg[0].type);
        if (arg[1].type != T_STRING)
            BAD_ARG_ERROR(2, T_STRING, arg[1].type);
        i = e_sscanf(num_arg, sp);
        pop_n_elems(num_arg-1);
        free_svalue(sp);
        put_number(sp, i);
        break;
    }

#ifdef USE_PARSE_COMMAND
    CASE(F_PARSE_COMMAND);      /* --- parse_command <numargs> --- */
    {
        /* EFUN parse_command()
         *
         *   int parse_command(string cmd, object|object* objs
         *                    , string fmt, mixed var1, mixed var2...)
         *
         * Parse the command <cmd> against <objs> and the format <fmt>
         * and assign the parsed values to variables var1....
         * The compiler knows that var1... have to be passed as lvalues.
         *
         * Result is TRUE if the pattern matches, and FALSE if not.
         */
        int i;
        svalue_t *arg;
        string_t *str;

        assign_eval_cost_inl();
        num_arg = LOAD_UINT8(pc);
          /* GET_NUM_ARG doesn't work here either. */
        arg = sp - num_arg + 1;
        if (arg[0].type != T_STRING)
            BAD_ARG_ERROR(1, T_STRING, arg[0].type);
        if (arg[1].type != T_OBJECT && arg[1].type != T_POINTER)
            RAISE_ARG_ERROR(2, TF_OBJECT|TF_POINTER, arg[1].type);
        if (arg[2].type != T_STRING)
            BAD_ARG_ERROR(3, T_STRING, arg[2].type);
        if (arg[1].type == T_POINTER)
            check_for_destr(arg[1].u.vec);

        inter_sp = sp;
        inter_pc = pc;

        str = trim_all_spaces(arg[0].u.str);
        free_mstring(arg[0].u.str);
        arg[0].u.str = str;

        str = trim_all_spaces(arg[2].u.str);
        free_mstring(arg[2].u.str);
        arg[2].u.str = str;

        i = e_parse_command(arg[0].u.str, &arg[1], arg[2].u.str
                           , &arg[3], num_arg-3);
        pop_n_elems(num_arg);        /* Get rid of all arguments */
        push_number(sp, i ? 1 : 0);      /* Push the result value */
        break;
    }
#endif /* USE_PARSE_COMMAND */

    CASE(F_LOCAL);                  /* --- local <ix>          --- */

        /* Fetch the value of local variable <ix> and push it
         * onto the stack.
         */
        sp++;
        assign_local_svalue_no_free(sp, fp + LOAD_UINT8(pc));
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
#ifdef USE_NEW_INLINES
                              , inter_context
#endif /* USE_NEW_INLINES */
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
         * of the stack, then remove the lvalue from the
         * stack (not free()!, this lvalue is just a copy).
         */

        svalue_t *svp;

        /* Get the designated value */
        TYPE_TEST1(sp, T_LVALUE);
        svp = sp->u.lvalue;

        /* Now increment where we can */
        if (svp->type == T_NUMBER)
        {
            if (svp->u.number == PINT_MAX)
            {
                ERRORF(("Numeric overflow: (%"PRIdPINT")++\n", 
                        svp->u.number));
                /* NOTREACHED */
                break;
            }
            svp->u.number++;
            sp--;
            break;
        }
        else if (svp->type == T_FLOAT)
        {
            STORE_DOUBLE_USED
            double d;

            d = READ_DOUBLE(svp) + 1.0;
            if (d < (-DBL_MAX) || d > DBL_MAX)
                ERRORF(("Numeric overflow: (%g)++\n", READ_DOUBLE(svp)));
            sp->type = T_FLOAT;
            STORE_DOUBLE(svp, d);
            sp--;
            break;
        }
        else if (svp->type == T_CHAR_LVALUE)
        {
            (*svp->u.charp)++;
            sp--;
            break;
        }
        else if (svp->type == T_LVALUE
              || svp->type == T_PROTECTED_LVALUE)
        {
            inter_sp = sp;
            add_number_to_lvalue(svp, 1, NULL, NULL);
            sp--;
            break;
        }

        ERRORF(("Bad arg to ++: got '%s', expected numeric type.\n"
               , typename(svp->type)
               ));
        break;
    }

    CASE(F_DEC);                    /* --- dec                 --- */
    {
        /* void dec (mixed & sp[0])
         *
         * Decrement the (numeric) value designed by the lvalue on top
         * of the stack, then remove the lvalue from the
         * stack (not free()!, this lvalue is just a copy).
         */

        svalue_t *svp;

        /* Get the designated value */
        TYPE_TEST1(sp, T_LVALUE);
        svp = sp->u.lvalue;

        /* Now decrement where we can */
        if (svp->type == T_NUMBER)
        {
            if (svp->u.number == PINT_MIN)
            {
                ERRORF(("Numeric overflow: (%"PRIdPINT")--\n", 
                        svp->u.number));
                /* NOTREACHED */
                break;
            }
            svp->u.number--;
            sp--;
            break;
        }
        else if (svp->type == T_FLOAT)
        {
            STORE_DOUBLE_USED
            double d;

            d = READ_DOUBLE(svp) - 1.0;
            if (d < (-DBL_MAX) || d > DBL_MAX)
                ERRORF(("Numeric overflow: (%g)--\n", READ_DOUBLE(svp)));
            sp->type = T_FLOAT;
            STORE_DOUBLE(svp, d);
            sp--;
            break;
        }
        else if (svp->type == T_CHAR_LVALUE)
        {
            (*svp->u.charp)--;
            sp--;
            break;
        }
        else if (svp->type == T_LVALUE
              || svp->type == T_PROTECTED_LVALUE)
        {
            inter_sp = sp;
            add_number_to_lvalue(svp, -1,  NULL, NULL);
            sp--;
            break;
        }

        ERRORF(("Bad arg to --: got '%s', expected numeric type.\n"
               , typename(svp->type)
               ));
        break;
    }

    CASE(F_POST_INC);               /* --- post_inc            --- */
    {
        /* mixed post_inc (mixed & sp[0])
         *
         * Increment the numeric value designated by the lvalue on top
         * of the stack, and replace the stack entry with the value
         * before the increment. The lvalue itself is simply removed, not
         * free()d.
         */

        svalue_t *svp;

        /* Get the designated value */
        TYPE_TEST1(sp, T_LVALUE);
        svp = sp->u.lvalue;

        /* Do the push and increment */
        if (svp->type == T_NUMBER)
        {
            if (svp->u.number == PINT_MAX)
            {
                ERRORF(("Numeric overflow: (%"PRIdPINT")++\n", 
                        svp->u.number));
                /* NOTREACHED */
                break;
            }
            put_number(sp,  svp->u.number++ );
            break;
        }
        else if (svp->type == T_FLOAT)
        {
            STORE_DOUBLE_USED
            double d;

            d = READ_DOUBLE(svp);
            sp->type = T_FLOAT;
            STORE_DOUBLE(sp, d);
            d += 1.0;
            if (d < (-DBL_MAX) || d > DBL_MAX)
                ERRORF(("Numeric overflow: (%g)++\n", READ_DOUBLE(svp)));
            STORE_DOUBLE(svp, d);
            break;
        }
        else if (svp->type == T_CHAR_LVALUE)
        {
            put_number(sp,  (unsigned char)(*svp->u.charp) );
            (*svp->u.charp)++;
            break;
        }
        else if (svp->type == T_LVALUE
              || svp->type == T_PROTECTED_LVALUE)
        {
            inter_sp = sp;
            add_number_to_lvalue(svp, 1, sp, NULL);
            break;
        }

        ERRORF(("Bad arg to ++: got '%s', expected numeric type.\n"
               , typename(svp->type)
               ));
        break;
    }

    CASE(F_POST_DEC);               /* --- post_dec            --- */
    {
        /* mixed post_dec (mixed & sp[0])
         *
         * Decrement the numeric value designated by the lvalue on top
         * of the stack, and replace the stack entry with the value
         * before the decrement. The lvalue itself is simply removed, not
         * free()d.
         */

        svalue_t *svp;

        /* Get the designated value */
        TYPE_TEST1(sp, T_LVALUE);
        svp = sp->u.lvalue;

        /* Do the push and decrement */
        if (svp->type == T_NUMBER)
        {
            if (svp->u.number == PINT_MIN)
            {
                ERRORF(("Numeric overflow: (%"PRIdPINT")--\n", 
                        svp->u.number));
                /* NOTREACHED */
                break;
            }
            put_number(sp,  svp->u.number-- );
            break;
        }
        else if (svp->type == T_FLOAT)
        {
            STORE_DOUBLE_USED
            double d;

            d = READ_DOUBLE(svp);
            sp->type = T_FLOAT;
            STORE_DOUBLE(sp, d);
            d -= 1.0;
            if (d < (-DBL_MAX) || d > DBL_MAX)
                ERRORF(("Numeric overflow: (%g)--\n", READ_DOUBLE(svp)));
            STORE_DOUBLE(svp, d);
            break;
        }
        else if (svp->type == T_CHAR_LVALUE)
        {
            put_number(sp, (unsigned char)(*svp->u.charp) );
            (*svp->u.charp)--;
            break;
        }
        else if (svp->type == T_LVALUE
              || svp->type == T_PROTECTED_LVALUE)
        {
            inter_sp = sp;
            add_number_to_lvalue(svp, -1, sp, NULL);
            break;
        }

        ERRORF(("Bad arg to --: got '%s', expected numeric type.\n"
               , typename(svp->type)
               ));
        break;
    }

    CASE(F_PRE_INC);                /* --- pre_inc             --- */
    {
        /* mixed pre_inc (mixed & sp[0])
         *
         * Increment the numeric value designated by the lvalue on top
         * of the stack, and replace the stack entry with the incremented
         * value. The lvalue itself is simply removed, not free()d.
         */

        svalue_t *svp;

        /* Get the designated value */
        TYPE_TEST1(sp, T_LVALUE);
        svp = sp->u.lvalue;

        /* Do the increment and push */
        if (svp->type == T_NUMBER)
        {
            if (svp->u.number == PINT_MAX)
            {
                ERRORF(("Numeric overflow: ++(%"PRIdPINT")\n", 
                        svp->u.number));
                /* NOTREACHED */
                break;
            }
            put_number(sp,  ++(svp->u.number) );
            break;
        }
        else if (svp->type == T_FLOAT)
        {
            STORE_DOUBLE_USED
            double d;

            d = READ_DOUBLE(svp) + 1.0;
            if (d < (-DBL_MAX) || d > DBL_MAX)
                ERRORF(("Numeric overflow: ++(%g)\n", READ_DOUBLE(svp)));
            sp->type = T_FLOAT;
            STORE_DOUBLE(sp, d);
            STORE_DOUBLE(svp, d);
            break;
        }
        else if (svp->type == T_CHAR_LVALUE)
        {
            ++(*svp->u.charp);
            put_number(sp,  (unsigned char)(*svp->u.charp) );
            break;
        }
        else if (svp->type == T_LVALUE
              || svp->type == T_PROTECTED_LVALUE)
        {
            inter_sp = sp;
            add_number_to_lvalue(svp, 1, NULL, sp);
            break;
        }

        ERRORF(("Bad arg to ++: got '%s', expected numeric type.\n"
               , typename(svp->type)
               ));
        break;
    }

    CASE(F_PRE_DEC);                /* --- pre_dec             --- */
    {
        /* mixed pre_dec (mixed & sp[0])
         *
         * Decrement the numeric value designated by the lvalue on top
         * of the stack, and replace the stack entry with the decremented
         * value. The lvalue itself is simply removed, not free()d.
         */

        svalue_t *svp;

        /* Get the designated value */
        TYPE_TEST1(sp, T_LVALUE);
        svp = sp->u.lvalue;

        /* Do the decrement and push */
        if (svp->type == T_NUMBER)
        {
            if (svp->u.number == PINT_MIN)
            {
                ERRORF(("Numeric overflow: --(%"PRIdPINT")\n", 
                        svp->u.number));
                /* NOTREACHED */
                break;
            }
            put_number(sp,  --(svp->u.number) );
            break;
        }
        else if (svp->type == T_FLOAT)
        {
            STORE_DOUBLE_USED
            double d;

            d = READ_DOUBLE(svp) - 1.0;
            if (d < (-DBL_MAX) || d > DBL_MAX)
                ERRORF(("Numeric overflow: --(%g)\n", READ_DOUBLE(svp)));
            sp->type = T_FLOAT;
            STORE_DOUBLE(sp, d);
            STORE_DOUBLE(svp, d);
            break;
        }
        else if (svp->type == T_CHAR_LVALUE)
        {
            --(*svp->u.charp);
            put_number(sp,  (unsigned char)(*svp->u.charp) );
            break;
        }
        else if (svp->type == T_LVALUE
              || svp->type == T_PROTECTED_LVALUE)
        {
            inter_sp = sp;
            add_number_to_lvalue(svp, -1, NULL, sp);
            break;
        }

        ERRORF(("Bad arg to --: got '%s', expected numeric type.\n"
               , typename(svp->type)
               ));
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
            /* No need to explicitely free_svalue(), it's just a number */
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

        svalue_t *dest;

        /* Get the designated lvalue */
#ifdef DEBUG
        if (sp->type != T_LVALUE)
            FATALF(("Bad left arg to F_ASSIGN: got '%s', expected 'lvalue'.\n"
                   , typename(sp->type)
                   ));
#endif
        dest = sp->u.lvalue;
        assign_svalue(dest, sp-1);
        sp--;
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
        transfer_svalue(sp->u.lvalue, sp-1);
        sp -= 2;
        break;
    }

    CASE(F_ADD);                    /* --- add                 --- */
        /* Add sp[0] to sp[-1] (the order is important), pop both
         * summands from the stack and push the result.
         *
         * Possible type combinations:
         *   string      + (string,int,float) -> string
         *   (int,float) + string             -> string
         *   int         + int                -> int
         *   float       + (int,float)        -> float
         *   int         + float              -> float
         *   vector      + vector             -> vector
         *   mapping     + mapping            -> mapping
         */

        switch ( sp[-1].type )
        {

        case T_STRING:
            inter_pc = pc;
            inter_sp = sp;
            switch ( sp->type )
            {
            case T_STRING:
              {
                string_t *left, *right, *res;

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
                put_string(sp, res);
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
            check_map_for_destr((sp-1)->u.map);
            check_map_for_destr(sp->u.map);
              /* required for add_mapping() */
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
            OP_ARG_ERROR(1, TF_POINTER|TF_MAPPING|TF_STRING|TF_FLOAT|TF_NUMBER
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
         *   vector      - vector             -> vector
         *   mapping     - mapping            -> mapping
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
            vector_t *v;

            TYPE_TEST_RIGHT(sp, T_POINTER);
            v = sp->u.vec;
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
        else if ((sp-1)->type == T_MAPPING)
        {
            mapping_t *m;

            TYPE_TEST_RIGHT(sp, T_MAPPING);
            m = subtract_mapping(sp[-1].u.map, sp->u.map);
            free_mapping(sp->u.map);
            sp--;
            free_mapping(sp->u.map);
            sp->u.map = m;
            break;
        }
        else if ((sp-1)->type == T_STRING)
        {
            string_t * result;

            TYPE_TEST_RIGHT(sp, T_STRING);
            inter_sp = sp;
            result = intersect_strings((sp-1)->u.str, sp->u.str, MY_TRUE);
            free_string_svalue(sp);
            sp--;
            free_string_svalue(sp);
            put_string(sp, result);
            break;
        }

        OP_ARG_ERROR(1, TF_POINTER|TF_MAPPING|TF_STRING|TF_FLOAT|TF_NUMBER
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
            if (sp->type == T_STRING)
            {
                string_t * result;
                size_t slen;

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
                put_string(sp, result);
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
            OP_ARG_ERROR(2, TF_POINTER|TF_STRING|TF_FLOAT|TF_NUMBER
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
                put_string(sp, result);
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
            OP_ARG_ERROR(1, TF_POINTER|TF_STRING|TF_FLOAT|TF_NUMBER
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

        if ((sp-1)->type == T_STRING && sp->type == T_STRING)
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

        TYPE_TEST_EXP_LEFT((sp-1), TF_NUMBER|TF_STRING|TF_FLOAT);
        TYPE_TEST_EXP_RIGHT(sp, TF_NUMBER|TF_STRING|TF_FLOAT);
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

        if ((sp-1)->type == T_STRING && sp->type == T_STRING)
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

        TYPE_TEST_EXP_LEFT((sp-1), TF_NUMBER|TF_STRING|TF_FLOAT);
        TYPE_TEST_EXP_RIGHT(sp, TF_NUMBER|TF_STRING|TF_FLOAT);
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

        if ((sp-1)->type == T_STRING && sp->type == T_STRING)
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

        TYPE_TEST_EXP_LEFT((sp-1), TF_NUMBER|TF_STRING|TF_FLOAT);
        TYPE_TEST_EXP_RIGHT(sp, TF_NUMBER|TF_STRING|TF_FLOAT);
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

        if ((sp-1)->type == T_STRING && sp->type == T_STRING)
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

        TYPE_TEST_EXP_LEFT((sp-1), TF_NUMBER|TF_STRING|TF_FLOAT);
        TYPE_TEST_EXP_RIGHT(sp, TF_NUMBER|TF_STRING|TF_FLOAT);
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
            (sp-1)->u.vec = map_intersect_array(sp[-1].u.vec, sp->u.map);
            sp--;
            break;
        }

        if (sp->type == T_STRING && (sp-1)->type == T_STRING)
        {
            string_t * result;

            inter_sp = sp;
            result = intersect_strings(sp[-1].u.str, sp->u.str, MY_FALSE);
            free_string_svalue(sp-1);
            free_string_svalue(sp);
            put_string(sp-1, result);
            sp--;
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

        TYPE_TEST_EXP_LEFT((sp-1), TF_NUMBER|TF_STRING|TF_POINTER|TF_MAPPING);
        TYPE_TEST_EXP_RIGHT(sp, TF_NUMBER|TF_STRING|TF_POINTER|TF_MAPPING);
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
    CASE(F_RX_RANGE);               /* --- rx_range            --- */
    CASE(F_AX_RANGE);               /* --- ax_range            --- */
        /* Push '1' onto the stack to make up for the missing
         * upper range bound, then fall through to the normal
         * range handling.
         */
        sp++;
        put_number(sp, 1);
        /* FALLTHROUGH */

    CASE(F_RANGE);                  /* --- range               --- */
    CASE(F_NR_RANGE);               /* --- nr_range            --- */
    CASE(F_RN_RANGE);               /* --- rn_range            --- */
    CASE(F_RR_RANGE);               /* --- rr_range            --- */
    CASE(F_NA_RANGE);               /* --- na_range            --- */
    CASE(F_AN_RANGE);               /* --- an_range            --- */
    CASE(F_RA_RANGE);               /* --- ra_range            --- */
    CASE(F_AR_RANGE);               /* --- ar_range            --- */
    CASE(F_AA_RANGE);               /* --- aa_range            --- */
      {
        /* Compute the range sp[-1]..sp[0] from string/array sp[-2]
         * and leave it on the stack.
         * This code also handles the NX/RX/AX_RANGE, pretending that
         * they are NR/RR/AR_RANGEs.
         */

        if (sp[-1].type != T_NUMBER)
            ERRORF(("Bad type '%s' of start interval to [..] range.\n"
                   , typename(sp[-1].type)
                   ));
        if (sp[0].type != T_NUMBER)
            ERRORF(("Bad type '%s' of end interval to [..] range.\n"
                   , typename(sp[0].type)
                   ));

        if (sp[-2].type == T_POINTER)
        {
            /* Slice a range from an array */

            vector_t *v;
            p_int size, i1, i2;

            size = VEC_SIZE(sp[-2].u.vec);

            if (instruction == F_RANGE
             || instruction == F_NR_RANGE
             || instruction == F_NA_RANGE
             || instruction == F_NX_RANGE)
                i1 = sp[-1].u.number;
            else
            if (instruction == F_AN_RANGE
             || instruction == F_AR_RANGE
             || instruction == F_AA_RANGE
             || instruction == F_AX_RANGE)
            {
                if (sp[-1].u.number < 0)
                    i1 = size + sp[-1].u.number;
                else
                    i1 = sp[-1].u.number;
            }
            else
                i1 = size - sp[-1].u.number;

            if (instruction == F_RANGE
             || instruction == F_RN_RANGE
             || instruction == F_AN_RANGE)
                i2 = sp[0].u.number;
            else
            if (instruction == F_NA_RANGE
             || instruction == F_RA_RANGE
             || instruction == F_AA_RANGE)
            {
                if (sp[0].u.number < 0)
                    i2 = size + sp[0].u.number;
                else
                    i2 = sp[0].u.number;
            }
            else
                i2 = size - sp[0].u.number;

            if (runtime_array_range_check)
            {
                if (i1 < 0 || i1 >= size)
                {
                    if (i2 < 0 || i2 >= size)
                        WARNF(("Warning: Out-of-bounds range limits: [%"
                               PRIdPINT"..%"PRIdPINT"], size %"PRIdPINT".\n"
                              , i1, i2, size));
                    else
                        WARNF(("Warning: Out-of-bounds lower range limits: %"
                               PRIdPINT", size %"PRIdPINT".\n"
                              , i1, size));
                }
                else if (i2 < 0 || i2 >= size)
                {
                    WARNF(("Warning: Out-of-bounds upper range limits: %"
                           PRIdPINT", size %"PRIdPINT".\n"
                          , i2, size));
                }
                else if (i1 > i2)
                {
                    WARNF(("Warning: Ranges of negative size: %"PRIdPINT
                           "..%"PRIdPINT".\n"
                          , i1, i2));
                }
            }

            if (i2 >= size)
            {
                i2 = size - 1;
            }

            pop_stack();
            pop_stack();

            v = slice_array(sp->u.vec, i1, i2);

            free_array(sp->u.vec);
            if (v)
            {
                sp->u.vec = v;
            }
            else
            {
                put_number(sp, 0);
            }
        }
        else if (sp[-2].type == T_STRING)
        {
            /* Slice a range from string */

            p_int len, from, to;
            string_t *res;

            len = mstrsize(sp[-2].u.str);
            if (instruction == F_RANGE
             || instruction == F_NR_RANGE
             || instruction == F_NX_RANGE
             || instruction == F_NA_RANGE)
                from = sp[-1].u.number;
            else
            if (instruction == F_AN_RANGE
             || instruction == F_AR_RANGE
             || instruction == F_AX_RANGE
             || instruction == F_AA_RANGE)
            {
                if (sp[-1].u.number < 0)
                    from = len + sp[-1].u.number;
                else
                    from = sp[-1].u.number;
            }
            else
                from = len - sp[-1].u.number;
            if (from < 0)
            {
                from = 0;
            }

            if (instruction == F_RANGE
             || instruction == F_RN_RANGE
             || instruction == F_AN_RANGE)
                to = sp[0].u.number;
            else
            if (instruction == F_NA_RANGE
             || instruction == F_RA_RANGE
             || instruction == F_AA_RANGE)
            {
                if (sp[0].u.number < 0)
                    to = len + sp[0].u.number;
                else
                    to = sp[0].u.number;
            }
            else
                to = len - sp[0].u.number;
            if (to >= len)
                to = len-1;

            if (to < from)
            {
                pop_n_elems(3);
                push_ref_string(sp, STR_EMPTY);
                break;
            }

            if (to == len-1)
            {
                res = mstr_extract(sp[-2].u.str, from, -1);
            }
            else
            {
                res = mstr_extract(sp[-2].u.str, from, to);
            }

            if (res == NULL)
            {
                ERRORF(("Out of memory (%"PRIdPINT" bytes).\n", to-from+1));
            }
            pop_n_elems(3);
            push_string(sp, res);
        }
        else
        {
            ERRORF(("Bad argument to [..] operand: got %s, "
                    "expected string/array.\n", typename(sp[-2].type)
                    ));
        }
        break;
      }

    CASE(F_ADD_EQ);                 /* --- add_eq              --- */
    CASE(F_VOID_ADD_EQ);            /* --- void_add_eq         --- */
    {
        /* Add sp[-1] to the value designated by lvalue sp[0] (the order
         * is important) and assign the result to sp[0].
         * For F_ADD_EQ, the result is also left on the stack.
         *
         * Possible type combinations:
         *   string       + (string,int,float) -> string
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

        type2 = sp[-1].type;
        u2 = sp[-1].u;

#ifdef DEBUG
        TYPE_TEST_LEFT(sp, T_LVALUE);
#endif

        /* Set argp to the actual value designated by sp[0] */
        for ( argp = sp->u.lvalue
            ; T_LVALUE == argp->type || T_PROTECTED_LVALUE == argp->type
            ; argp = argp->u.lvalue)
            NOOP;

        /* Now do it */
        switch(argp->type)
        {

        case T_STRING:  /* Adding to a string */
          {
            string_t *new_string;

            /* Perform the addition, creating new_string */
            if (type2 == T_STRING)
            {
                string_t *left, *right;
                size_t len;

                left = argp->u.str;
                right = (sp-1)->u.str;

                len = mstrsize(left) + mstrsize(right);
                DYN_STRING_COST(len)
                new_string = mstr_add(left, right);
                if (!new_string)
                    ERRORF(("Out of memory (%zu bytes)\n", len));
                free_string_svalue(sp-1);
                sp -= 2;
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
                sp -= 2;
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
                sp -= 2;
            }
            else
            {
                OP_ARG_ERROR(2, TF_STRING|TF_FLOAT|TF_NUMBER, type2);
                /* NOTREACHED */
            }

            /* Replace *argp by the new string */
            free_string_svalue(argp);
            put_string(argp, new_string);
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

                if (instruction == F_VOID_ADD_EQ)
                {
                    argp->u.number += u2.number;
                    sp -= 2;
                    goto again;
                }
                (--sp)->u.number = argp->u.number += u2.number;
                goto again;
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
                if (instruction == F_VOID_ADD_EQ)
                {
                    sp -= 2;
                    goto again;
                }

                --sp;
                sp->type = T_FLOAT;
                STORE_DOUBLE(sp, sum);
                goto again;
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
                free_string_svalue(sp-1);

                /* Overwrite the number in argp */
                put_string(argp, res);

                if (instruction == F_VOID_ADD_EQ)
                {
                    sp -= 2;
                    goto again;
                }

                --sp;
                put_ref_string(sp, res);

                goto again;
            }
            else
            {
                OP_ARG_ERROR(2, TF_NUMBER, type2);
                /* NOTREACHED */
            }
            break;

        case T_CHAR_LVALUE:  /* Add to a character in a string */
            if (type2 == T_NUMBER)
            {
                p_int left = (unsigned char)*argp->u.charp;
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

                if (instruction == F_VOID_ADD_EQ)
                {
                    *argp->u.charp += u2.number;
                    sp -= 2;
                    goto again;
                }
                (--sp)->u.number = (unsigned char)(*argp->u.charp += u2.number);
                goto again;
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
                check_map_for_destr(u2.map);
                add_to_mapping(argp->u.map, u2.map);
                sp -= 2;
                free_mapping(u2.map);
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
                vector_t *v;

                inter_sp = sp;
                inter_pc = pc;
                DYN_ARRAY_COST(VEC_SIZE(u2.vec)+VEC_SIZE(argp->u.vec));
                v = inter_add_array(u2.vec, &argp->u.vec);
                if (instruction == F_VOID_ADD_EQ)
                {
                    sp -= 2;
                    goto again;
                }
                sp--;
                sp->u.vec = ref_array(v);
                goto again;
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
                sp -= 2;
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
                sp -= 2;
            }
            else
            {
                OP_ARG_ERROR(2, TF_FLOAT|TF_NUMBER, type2);
                /* NOTREACHED */
            }
            break;

        default:
            OP_ARG_ERROR(1, TF_STRING|TF_FLOAT|TF_MAPPING|TF_POINTER|TF_NUMBER
                        , argp->type);
            /* NOTREACHED */
        } /* end of switch */

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
         *   vector      - vector             -> vector
         *   mapping     - mapping            -> mapping
         */

        short type2;         /* type and value of sp[-1] */
        union u u2;
        svalue_t *argp; /* the actual value of sp[0] */

        type2 = sp[-1].type;
        u2 = sp[-1].u;

#ifdef DEBUG
        TYPE_TEST_LEFT(sp, T_LVALUE);
#endif

        /* Set argp to the actual value designated by sp[0] */
        for ( argp = sp->u.lvalue
            ; T_LVALUE == argp->type || T_PROTECTED_LVALUE == argp->type
            ; argp = argp->u.lvalue)
            NOOP;

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
                sp--;
                sp->u.number = argp->u.number -= u2.number;
                break;
            }

            if (type2 == T_FLOAT)
            {
                STORE_DOUBLE_USED
                double diff;

                sp--;
                diff = (double)(argp->u.number) - READ_DOUBLE(sp);
                if (diff < (-DBL_MAX) || diff > DBL_MAX)
                    ERRORF(("Numeric overflow: %"PRIdPINT" - %g\n"
                           , argp->u.number, READ_DOUBLE(sp)));
                STORE_DOUBLE(sp, diff);
                sp->type = T_FLOAT;
                assign_svalue_no_free(argp, sp);
                break;
            }

            /* type2 of the wrong type */
            OP_ARG_ERROR(2, TF_NUMBER|TF_FLOAT, type2);
            /* NOTREACHED */
            break;

        case T_CHAR_LVALUE:  /* Subtract from a char in a string */
            if (type2 != T_NUMBER)
            {
                OP_ARG_ERROR(2, TF_NUMBER, type2);
                /* NOTREACHED */
            }

            {
                p_int left = (unsigned char)*argp->u.charp;
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
            }

            sp--;
            sp->u.number = (unsigned char)(*argp->u.charp -= u2.number);
            break;

        case T_STRING:   /* Subtract from a string */
        {
            string_t * result;

            if (type2 != T_STRING)
            {
                OP_ARG_ERROR(2, TF_STRING, type2);
                /* NOTREACHED */
            }

            inter_sp = sp;
            result = intersect_strings(argp->u.str, (sp-1)->u.str, MY_TRUE);
            free_string_svalue(argp);
            put_string(argp, result);
            free_svalue(sp);
            sp--;
            free_string_svalue(sp);
            put_ref_string(sp, result);
            break;
        }

        case T_POINTER:  /* Subtract from an array */
          {
            vector_t *v, *v_old;

            if (type2 != T_POINTER)
            {
                OP_ARG_ERROR(2, TF_POINTER, type2);
                /* NOTREACHED */
            }

            v = u2.vec;

            /* Duplicate the minuend array if necessary, as
             * the subtraction will change and free it
             */
            if (v->ref > 1)
            {
                deref_array(v);
                v = slice_array(v, 0, (mp_int)VEC_SIZE(v)-1 );
            }
            sp--;
            v_old = argp->u.vec;
            v = subtract_array(v_old, v);
            argp->u.vec = v;
            put_ref_array(sp, v);
            break;
          }

        case T_FLOAT:  /* Subtract from a float */
            if (type2 == T_FLOAT)
            {
                STORE_DOUBLE_USED
                double d;

                /* don't use the address of u2, this would prevent putting it
                 * in a register
                 */
                sp--;
                d = READ_DOUBLE(argp) - READ_DOUBLE(sp);
                if (d < (-DBL_MAX) || d > DBL_MAX)
                    ERRORF(("Numeric overflow: %g + %g\n"
                           , READ_DOUBLE(argp), READ_DOUBLE(sp)));
                STORE_DOUBLE(argp, d);
                *sp = *argp;
            }
            else if (type2 == T_NUMBER)
            {
                STORE_DOUBLE_USED
                double d;

                sp--;
                d = READ_DOUBLE(argp) - (double)sp->u.number;
                if (d < (-DBL_MAX) || d > DBL_MAX)
                    ERRORF(("Numeric overflow: %g + %"PRIdPINT"\n"
                           , READ_DOUBLE(argp), sp->u.number));
                STORE_DOUBLE(argp, d);
                *sp = *argp;
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

                sp--;
                m = sp->u.map;
                check_map_for_destr(m);

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
                sp->u.map = ref_mapping(argp->u.map);
            }
            else if (type2 == T_MAPPING && sp[-1].u.map->num_values)
            {
                ERROR("Bad right arg to -=: mapping has values.\n");
                /* NOTREACHED */
            }
            else
            {
                OP_ARG_ERROR(2, TF_MAPPING, type2);
                /* NOTREACHED */
            }
            break;

        default:
            OP_ARG_ERROR(1, TF_STRING|TF_FLOAT|TF_MAPPING|TF_POINTER|TF_NUMBER
                        , argp->type);
            /* NOTREACHED */
        } /* end of switch */
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
         *   array       * int                -> array
         *
         * TODO: Extend this to mappings.
         */

        svalue_t *argp;

#ifdef DEBUG
        TYPE_TEST_LEFT(sp, T_LVALUE);
#endif

        /* Set argp to the actual value designated by sp[0] */
        for ( argp = sp->u.lvalue
            ; T_LVALUE == argp->type || T_PROTECTED_LVALUE == argp->type
            ; argp = argp->u.lvalue)
            NOOP;

        /* Now do it */
        if (argp->type == T_NUMBER)
        {
            sp--;
            if (sp->type == T_NUMBER)
            {
                p_int left = argp->u.number;
                p_int right = sp->u.number;

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
                sp->u.number = argp->u.number *= sp->u.number;
                break;
            } /* type2 == T_NUMBER */

            if (sp->type == T_FLOAT)
            {
                STORE_DOUBLE_USED
                double product;

                product = argp->u.number * READ_DOUBLE(sp);
                if (product < (-DBL_MAX) || product > DBL_MAX)
                    ERRORF(("Numeric overflow: %"PRIdPINT" * %g\n"
                           , argp->u.number, READ_DOUBLE(sp)));
                STORE_DOUBLE(sp, product);
                sp->type = T_FLOAT;
                assign_svalue_no_free(argp, sp);
                break;
            }

            /* Unsupported type2 */
            OP_ARG_ERROR(2, TF_NUMBER|TF_FLOAT, sp->type);
            /* NOTREACHED */
        }

        if (argp->type == T_CHAR_LVALUE)
        {
            sp--;
            if (sp->type != T_NUMBER)
            {
                OP_ARG_ERROR(2, TF_NUMBER, sp->type);
                /* NOTREACHED */
            }
            {
                p_int left = (unsigned char)*argp->u.charp;
                p_int right = sp->u.number;

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
            }
            sp->u.number = (unsigned char)(*argp->u.charp *= sp->u.number);
            break;
        }

        if (argp->type == T_FLOAT)
        {
            STORE_DOUBLE_USED
            double d;

            sp--;
            if (sp->type == T_FLOAT)
            {
                d = READ_DOUBLE(argp) * READ_DOUBLE(sp);
                if (d < (-DBL_MAX) || d > DBL_MAX)
                    ERRORF(("Numeric overflow: %g * %g\n"
                           , READ_DOUBLE(argp), READ_DOUBLE(sp)));
                STORE_DOUBLE(argp, d);
                *sp = *argp;
            }
            else if (sp->type == T_NUMBER)
            {
                d = READ_DOUBLE(argp) * (double)sp->u.number;
                if (d < (-DBL_MAX) || d > DBL_MAX)
                    ERRORF(("Numeric overflow: %g * %"PRIdPINT"\n"
                           , READ_DOUBLE(argp), sp->u.number));
                STORE_DOUBLE(argp, d);
                *sp = *argp;
            }
            else
            {
                OP_ARG_ERROR(2, TF_NUMBER|TF_FLOAT, sp->type);
                /* NOTREACHED */
            }
            break;
        }

        if (argp->type == T_STRING)
        {
            string_t * result;
            size_t reslen;
            size_t len;

            sp--;
            if (sp->type != T_NUMBER)
            {
                OP_ARG_ERROR(2, TF_NUMBER, sp->type);
                /* NOTREACHED */
            }
            if (sp->u.number < 0)
            {
                ERROR("Bad right arg to *=: negative number\n");
                /* NOTREACHED */
            }

            len = mstrsize(argp->u.str);

            if (len > (size_t)PINT_MAX
             || (   len != 0
                 && PINT_MAX / (p_int)len < sp->u.number)
             || (   sp->u.number != 0
                 && PINT_MAX / sp->u.number < (p_int)len)
               )
                ERRORF(("Result string too long (%"PRIdPINT" * %zu).\n"
                       , sp->u.number, len
                       ));

            reslen = (size_t)sp->u.number * len;
            result = mstr_repeat(argp->u.str, (size_t)sp->u.number);
            if (!result)
                ERRORF(("Out of memory (%zu bytes).\n", reslen));

            DYN_STRING_COST(reslen)

            free_string_svalue(argp);
            put_string(argp, result);
            assign_svalue_no_free(sp, argp);
            break;
        }

        if (argp->type == T_POINTER)
        {
            vector_t *result;
            mp_int reslen;
            p_uint len;

            sp--;
            if (sp->type != T_NUMBER)
            {
                OP_ARG_ERROR(2, TF_NUMBER, sp->type);
                /* NOTREACHED */
            }
            if (sp->u.number < 0)
            {
                ERROR("Bad right arg to *=: negative number\n");
                /* NOTREACHED */
            }

            inter_sp = sp;
            inter_pc = pc;
            len = VEC_SIZE(argp->u.vec);
            reslen = sp->u.number * (mp_int)len;
            result = allocate_uninit_array(reslen);
            DYN_ARRAY_COST(reslen);

            if (sp->u.number > 0 && len)
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
            assign_svalue_no_free(sp, argp);
            break;
        }

        OP_ARG_ERROR(1, TF_STRING|TF_FLOAT|TF_POINTER|TF_NUMBER
                    , argp->type);
        /* NOTREACHED */
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

#ifdef DEBUG
        TYPE_TEST_LEFT(sp, T_LVALUE);
#endif

        /* Set argp to the actual value designated by sp[0] */
        for ( argp = sp->u.lvalue
            ; T_LVALUE == argp->type || T_PROTECTED_LVALUE == argp->type
            ; argp = argp->u.lvalue)
            NOOP;

        /* Now do it */
        if (argp->type == T_NUMBER)
        {
            sp--;
            if (sp->type == T_NUMBER)
            {
                if (sp->u.number == 0)
                    ERROR("Division by zero\n");
                if (argp->u.number == PINT_MIN && sp->u.number == -1)
                    ERRORF(("Numeric overflow: %"PRIdPINT" / -1\n"
                           , argp->u.number
                           ));
                sp->u.number = argp->u.number /= sp->u.number;
                break;
            }

            if (sp->type == T_FLOAT)
            {
                double dtmp;
                STORE_DOUBLE_USED

                dtmp = READ_DOUBLE( sp );
                if (dtmp == 0.)
                    ERROR("Division by zero\n");
                dtmp = (double)argp->u.number / dtmp;
                if (dtmp < (-DBL_MAX) || dtmp > DBL_MAX)
                    ERRORF(("Numeric overflow: %"PRIdPINT" / %g\n"
                           , argp->u.number, READ_DOUBLE(sp)));
                STORE_DOUBLE(sp, dtmp);
                sp->type = T_FLOAT;
                assign_svalue_no_free(argp, sp);
                break;
            }

            /* Unsupported type2 */
            OP_ARG_ERROR(2, TF_NUMBER|TF_FLOAT, sp->type);
            /* NOTREACHED */
        }

        if (argp->type == T_CHAR_LVALUE)
        {
            sp--;
            if (sp->type != T_NUMBER)
            {
                OP_ARG_ERROR(2, TF_NUMBER, sp->type);
                /* NOTREACHED */
            }
            if (sp->u.number == 0)
                ERROR("Division by zero\n");
            sp->u.number = (unsigned char)(*argp->u.charp /= sp->u.number);
            break;
        }

        if (argp->type == T_FLOAT)
        {
            STORE_DOUBLE_USED
            double d;

            sp--;
            if (sp->type == T_FLOAT)
            {
                d = READ_DOUBLE(sp);
                if (d == 0.0)
                    ERROR("Division by zero\n");
                d = READ_DOUBLE(argp) / d;
                if (d < (-DBL_MAX) || d > DBL_MAX)
                    ERRORF(("Numeric overflow: %g / %g\n"
                           , READ_DOUBLE(argp), READ_DOUBLE(sp)));
                STORE_DOUBLE(argp, d);
                *sp = *argp;
            }
            else if (sp->type == T_NUMBER)
            {
                p_int i;
                i = sp->u.number;
                if (i == 0)
                    ERROR("Division by zero\n");
                d = READ_DOUBLE(argp) / (double)i;
                if (d < (-DBL_MAX) || d > DBL_MAX)
                    ERRORF(("Numeric overflow: %g / %"PRIdPINT"\n"
                           , READ_DOUBLE(argp), sp->u.number));
                STORE_DOUBLE(argp, d);
                *sp = *argp;
            }
            else
            {
                OP_ARG_ERROR(2, TF_NUMBER|TF_FLOAT, sp->type);
                /* NOTREACHED */
            }
            break;
        }
        OP_ARG_ERROR(1, TF_FLOAT|TF_NUMBER, argp->type);
        /* NOTREACHED */
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

#ifdef DEBUG
        TYPE_TEST_LEFT(sp, T_LVALUE);
#endif

        /* Set argp to the actual value designated by sp[0] */
        for ( argp = sp->u.lvalue
            ; T_LVALUE == argp->type || T_PROTECTED_LVALUE == argp->type
            ; argp = argp->u.lvalue)
            NOOP;

        /* Now do it */
        if (argp->type == T_NUMBER)
        {
            sp--;
            if (sp->type != T_NUMBER)
            {
                OP_ARG_ERROR(2, TF_NUMBER, sp->type);
                /* NOTREACHED */
            }
            if (sp->u.number == 0)
                ERROR("Division by zero\n");
            sp->u.number = argp->u.number %= sp->u.number;
            break;
        }

        if (argp->type == T_CHAR_LVALUE)
        {
            sp--;
            if (sp->type != T_NUMBER)
            {
                OP_ARG_ERROR(2, TF_NUMBER, sp->type);
                /* NOTREACHED */
            }
            if (sp->u.number == 0)
                ERROR("Division by zero\n");
            sp->u.number = (unsigned char)(*argp->u.charp %= sp->u.number);
            break;
        }

        OP_ARG_ERROR(1, TF_NUMBER, argp->type);
        /* NOTREACHED */
    }

    CASE(F_AND_EQ);                 /* --- and_eq              --- */
    {
        /* Intersect the value designated by lvalue sp[0] with sp[-1],
         * assign the result to sp[0] and also leave it on the stack.
         *
         * Possible type combinations:
         *   int    & int    -> int
         *   string & string -> string
         *   array  & array  -> array
         *   array  & mapping -> array
         *   mapping & array -> mapping
         *   mapping & mapping -> mapping
         */

        svalue_t *argp;

#ifdef DEBUG
        TYPE_TEST_LEFT(sp, T_LVALUE);
#endif

        /* Set argp to the actual value designated by sp[0] */
        for ( argp = sp->u.lvalue
            ; T_LVALUE == argp->type || T_PROTECTED_LVALUE == argp->type
            ; argp = argp->u.lvalue)
            NOOP;

        /* Now do it */
        if (argp->type == T_NUMBER)  /* Intersect a number */
        {
            if (sp[-1].type != T_NUMBER)
            {
                OP_ARG_ERROR(2, TF_NUMBER, sp[-1].type);
                /* NOTREACHED */
            }
            sp--;
            sp->u.number = argp->u.number &= sp->u.number;
            break;
        }

        if (argp->type == T_CHAR_LVALUE)
        {
            sp--;
            if (sp->type != T_NUMBER)
            {
                OP_ARG_ERROR(2, TF_NUMBER, sp->type);
                /* NOTREACHED */
            }
            sp->u.number = (unsigned char)(*argp->u.charp &= sp->u.number);
            break;
        }

        if (argp->type == T_POINTER)
        {
            /* Intersect an array */

            if (sp[-1].type == T_POINTER)
            {
                vector_t *vec1, *vec2;

                inter_sp = sp - 2;
                vec1 = argp->u.vec;
                vec2 = sp[-1].u.vec;
                argp->type = T_NUMBER;
                vec1 = intersect_array(vec1, vec2);
                put_ref_array(argp, vec1);
                sp--;
                sp->u.vec = argp->u.vec;
                free_svalue(sp+1);
            }
            else if (sp[-1].type == T_MAPPING)
            {
                vector_t *vec;
                mapping_t * map;

                inter_sp = sp - 2;
                vec = argp->u.vec;
                map = sp[-1].u.map;
                argp->type = T_NUMBER;
                vec = map_intersect_array(vec, map);
                put_ref_array(argp, vec);
                sp--;
                put_array(sp, argp->u.vec);
                free_svalue(sp+1);
            }
            else
            {
                OP_ARG_ERROR(2, TF_POINTER|TF_MAPPING, sp[-1].type);
                /* NOTREACHED */
            }
            break;
        }

        if (argp->type == T_MAPPING)
        {
            /* Intersect a mapping */

            mapping_t *result;

            if (sp[-1].type != T_POINTER && sp[-1].type != T_MAPPING)
            {
                OP_ARG_ERROR(2, TF_MAPPING|TF_POINTER, sp[-1].type);
                /* NOTREACHED */
            }

            inter_sp = sp;

            result = map_intersect(argp->u.map, sp-1);

            put_mapping(argp, result);

            free_svalue(sp);
            sp--;

            put_ref_mapping(sp, result);
            break;
        }

        if (argp->type == T_STRING)
        {
            string_t * result;

            if (sp[-1].type != T_STRING)
            {
                OP_ARG_ERROR(2, TF_STRING, sp[-1].type);
                /* NOTREACHED */
            }
            inter_sp = sp;
            result = intersect_strings(argp->u.str, (sp-1)->u.str, MY_FALSE);
            free_string_svalue(argp);
            put_string(argp, result);
            free_svalue(sp);
            sp--;
            free_string_svalue(sp);
            put_ref_string(sp, result);
            break;
        }

        OP_ARG_ERROR(1, TF_NUMBER|TF_STRING|TF_POINTER, argp->type);
        /* NOTREACHED */
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

#ifdef DEBUG
        TYPE_TEST_LEFT(sp, T_LVALUE);
#endif

        /* Set argp to the actual value designated by sp[0] */
        for ( argp = sp->u.lvalue
            ; T_LVALUE == argp->type || T_PROTECTED_LVALUE == argp->type
            ; argp = argp->u.lvalue)
            NOOP;

        /* Now do it */
        if (argp->type == T_NUMBER)
        {
            sp--;
            if (sp->type != T_NUMBER)
            {
                OP_ARG_ERROR(2, TF_NUMBER, sp->type);
                /* NOTREACHED */
            }
            sp->u.number = argp->u.number |= sp->u.number;
            break;
        }

        if (argp->type == T_CHAR_LVALUE)
        {
            sp--;
            if (sp->type != T_NUMBER)
            {
                OP_ARG_ERROR(2, TF_NUMBER, sp->type);
                /* NOTREACHED */
            }
            sp->u.number = (unsigned char)(*argp->u.charp |= sp->u.number);
            break;
        }

        if (argp->type == T_POINTER)
        {
            /* Join an array */

            vector_t *vec1, *vec2;

            if (sp[-1].type != T_POINTER)
            {
                OP_ARG_ERROR(2, TF_POINTER, sp[-1].type);
                /* NOTREACHED */
            }
            inter_sp = sp;
            inter_pc = pc;
            vec1 = argp->u.vec;
            vec2 = sp[-1].u.vec;
            vec1 = join_array(vec1, vec2);
              /* The new vec1 may be one of the original vec1 or vec2 */
            put_ref_array(argp, vec1);
            sp--;
            sp->u.vec = argp->u.vec;
            free_svalue(sp+1);
            break;
        }

        OP_ARG_ERROR(1, TF_NUMBER|TF_POINTER, argp->type);
        /* NOTREACHED */
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

#ifdef DEBUG
        TYPE_TEST_LEFT(sp, T_LVALUE);
#endif

        /* Set argp to the actual value designated by sp[0] */
        for ( argp = sp->u.lvalue
            ; T_LVALUE == argp->type || T_PROTECTED_LVALUE == argp->type
            ; argp = argp->u.lvalue)
            NOOP;

        /* Now do it */
        if (argp->type == T_NUMBER)
        {
            sp--;
            if (sp->type != T_NUMBER)
            {
                OP_ARG_ERROR(2, TF_NUMBER, sp->type);
                /* NOTREACHED */
            }
            sp->u.number = argp->u.number ^= sp->u.number;
            break;
        }

        if (argp->type == T_CHAR_LVALUE)
        {
            sp--;
            if (sp->type != T_NUMBER)
            {
                OP_ARG_ERROR(2, TF_NUMBER, sp->type);
                /* NOTREACHED */
            }
            sp->u.number = (unsigned char)(*argp->u.charp ^= sp->u.number);
            break;
        }

        if (argp->type == T_POINTER)
        {
            /* Symm-diff an array */

            vector_t *vec1, *vec2;

            if (sp[-1].type != T_POINTER)
            {
                OP_ARG_ERROR(2, TF_POINTER, sp[-1].type);
                /* NOTREACHED */
            }
            inter_sp = sp - 2;
            vec1 = argp->u.vec;
            vec2 = sp[-1].u.vec;
            argp->type = T_NUMBER;
            vec1 = symmetric_diff_array(vec1, vec2);
            put_ref_array(argp, vec1);
            sp--;
            sp->u.vec = argp->u.vec;
            free_svalue(sp+1);
            break;
        }

        OP_ARG_ERROR(1, TF_NUMBER|TF_POINTER, argp->type);
        /* NOTREACHED */
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

        p_uint shift;
        svalue_t *argp;

#ifdef DEBUG
        TYPE_TEST_LEFT(sp, T_LVALUE);
#endif

        /* Set argp to the actual value designated by sp[0] */
        for ( argp = sp->u.lvalue
            ; T_LVALUE == argp->type || T_PROTECTED_LVALUE == argp->type
            ; argp = argp->u.lvalue)
            NOOP;

        /* Now do it */
        if (argp->type == T_NUMBER)
        {
            sp--;
            if (sp->type != T_NUMBER)
            {
                OP_ARG_ERROR(2, TF_NUMBER, sp->type);
                /* NOTREACHED */
            }
            shift = sp->u.number;
            argp->u.number <<= shift > MAX_SHIFT ? MAX_SHIFT : shift;
            sp->u.number = argp->u.number;
            break;
        }

        if (argp->type == T_CHAR_LVALUE)
        {
            sp--;
            if (sp->type != T_NUMBER)
            {
                OP_ARG_ERROR(2, TF_NUMBER, sp->type);
                /* NOTREACHED */
            }
            shift = sp->u.number;
            *argp->u.charp <<= shift > MAX_SHIFT ? MAX_SHIFT : shift;
            sp->u.number = (unsigned char)(*argp->u.charp);
            break;
        }

        OP_ARG_ERROR(1, TF_NUMBER, argp->type);
        /* NOTREACHED */
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

        p_uint shift;
        svalue_t *argp;

#ifdef DEBUG
        TYPE_TEST_LEFT(sp, T_LVALUE);
#endif

        /* Set argp to the actual value designated by sp[0] */
        for ( argp = sp->u.lvalue
            ; T_LVALUE == argp->type || T_PROTECTED_LVALUE == argp->type
            ; argp = argp->u.lvalue)
            NOOP;

        /* Now do it */
        if (argp->type == T_NUMBER)
        {
            sp--;
            if (sp->type != T_NUMBER)
            {
                OP_ARG_ERROR(2, TF_NUMBER, sp->type);
                /* NOTREACHED */
            }
            shift = sp->u.number;
            argp->u.number >>= shift > MAX_SHIFT ? (MAX_SHIFT+1) : shift;
            sp->u.number = argp->u.number;
            break;
        }

        if (argp->type == T_CHAR_LVALUE)
        {
            sp--;
            if (sp->type != T_NUMBER)
            {
                OP_ARG_ERROR(2, TF_NUMBER, sp->type);
                /* NOTREACHED */
            }
            shift = sp->u.number;
            *argp->u.charp >>= shift > MAX_SHIFT ? MAX_SHIFT : shift;
            sp->u.number = (unsigned char)(*argp->u.charp);
            break;
        }

        OP_ARG_ERROR(1, TF_NUMBER, argp->type);
        /* NOTREACHED */ break;
    }

    CASE(F_RSHL_EQ);               /* --- rshl_eq              --- */
    {
        /* Logically shift the value designated by lvalue sp[0] right by
         * sp[-1], assign the result to sp[0] and also leave it on the stack.
         *
         * Possible type combinations:
         *   int        >>> int                -> int
         */

        p_uint shift;
        svalue_t *argp;
#ifdef DEBUG
        TYPE_TEST_LEFT(sp, T_LVALUE);
#endif

        /* Set argp to the actual value designated by sp[0] */
        for ( argp = sp->u.lvalue
            ; T_LVALUE == argp->type || T_PROTECTED_LVALUE == argp->type
            ; argp = argp->u.lvalue)
            NOOP;

        /* Now do it */
        if (argp->type == T_NUMBER)
        {
            sp--;
            if (sp->type != T_NUMBER)
            {
                OP_ARG_ERROR(2, TF_NUMBER, sp->type);
                /* NOTREACHED */
            }
            shift = sp->u.number;
            if (shift > MAX_SHIFT)
                argp->u.number = 0;
            else
                argp->u.number = (p_uint)argp->u.number >> shift;
            sp->u.number = argp->u.number;
            break;
        }

        if (argp->type == T_CHAR_LVALUE)
        {
            sp--;
            if (sp->type != T_NUMBER)
            {
                OP_ARG_ERROR(2, TF_NUMBER, sp->type);
                /* NOTREACHED */
            }
            shift = sp->u.number;
            if (shift > MAX_SHIFT)
                *argp->u.charp = 0;
            else
                *argp->u.charp = (p_uint)*argp->u.charp >> shift;
            sp->u.number = (unsigned char)*argp->u.charp;
            break;
        }

        OP_ARG_ERROR(1, TF_NUMBER, argp->type);
        /* NOTREACHED */
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
        svalue_t * svp = sp;
        sp++;
        while (svp->type == T_LVALUE || svp->type == T_PROTECTED_LVALUE)
            svp = svp->u.lvalue;
        assign_svalue_no_free(sp, svp);
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

        if (sp->type == T_POINTER)
        {
            /* The argument is an array: flatten it */

            vector_t *vec;  /* the array */
            svalue_t *svp;  /* pointer into the array */
            p_int i;         /* (remaining) vector size */

            vec = sp->u.vec;
            i = VEC_SIZE(vec);

            /* Check if there is enough space on the stack.
             */
            if (i + (sp - VALUE_STACK) >= EVALUATOR_STACK_SIZE)
            {
                errorf("VM Stack overflow: %"PRIdMPINT" too high.\n"
                     , ((mp_int)i + (sp - VALUE_STACK) - EVALUATOR_STACK_SIZE) );
                /* NOTREACHED */
                break;
            }

            /* Push the array elements onto the stack, overwriting the
             * array value itself.
             */
            if (deref_array(vec))
            {
                for (svp = vec->item; --i >= 0; )
                {
                    if (destructed_object_ref(svp))
                    {
                        put_number(sp, 0);
                        sp++;
                        svp++;
                    }
                    else
                        assign_svalue_no_free(sp++, svp++);
                }
            }
            else
            {
                /* The array will be freed, so use a faster function */
                for (svp = vec->item; --i >= 0; ) {
                    if (destructed_object_ref(svp))
                    {
                        put_number(sp, 0);
                        sp++;
                        svp++;
                    }
                    else
                        transfer_svalue_no_free(sp++, svp++);
                }
                free_empty_vector(vec);
            }

            sp--; /* undo the last extraneous sp++ */
        }
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

        unsigned short func_index;   /* function index within program */
        unsigned short func_offset;
          /* function index within the current object's program.
           * This way local function may be redefined through inheritance.
           */
        funflag_t  flags;     /* the function flags */
        fun_hdr_p  funstart;  /* the actual function (code) */
        
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
        LOAD_SHORT(func_index, pc);
        func_offset = (unsigned short)(func_index + function_index_offset);

        /* Find the function in the function table. As the function may have
         * been redefined by inheritance, we must look in the last table,
         * which is pointed to by current_object.
         */

        if (func_offset >= current_object->prog->num_functions)
        {
            fatal("call_function: "
                  "Illegal function index: offset %hu (index %hu), "
                  "%d functions - current object %s\n"
                 , func_offset, func_index
                 , current_object->prog->num_functions
                 , get_txt(current_object->name)
                 );
        }

        /* NOT current_prog, which can be an inherited object. */
        flags = current_object->prog->functions[func_offset];

        /* If the function was cross-defined, get the real offset */
        if (flags & NAME_CROSS_DEFINED)
        {
            func_offset += CROSSDEF_NAME_OFFSET(flags);
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
        flags = setup_new_frame1(func_offset, 0, 0);
        funstart = (fun_hdr_p)(current_prog->program + (flags & FUNSTART_MASK));
        csp->funstart = funstart;

        /* Setup the stack, arguments and local vars */
        sp = setup_new_frame2(funstart, sp, MY_FALSE, MY_FALSE);
        inter_sp = sp;
        
        // check the argument types
        check_function_args(FUNCTION_INDEX(funstart), current_prog, funstart);
        
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
        pc = FUNCTION_CODE(funstart);
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
        fun_hdr_p funstart;         /* the actual function (code) */
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

        inheritp = setup_inherited_call(prog_index);

        /* Search for the function definition and determine the offsets.
         */
        if (instruction != F_CALL_INHERITED_NOARGS)
            csp->num_local_variables = sp - ap + 1;
        else
            csp->num_local_variables = 0;
        flags = setup_new_frame1(
          func_index,
          function_index_offset + inheritp->function_index_offset,
          inheritp->variable_index_offset
        );
        funstart = (fun_hdr_p)(current_prog->program + (flags & FUNSTART_MASK));
        csp->funstart = funstart;

        /* Setup the stack, arguments and local vars */
        sp = setup_new_frame2(funstart, sp, MY_FALSE, MY_FALSE);
        inter_sp = sp;
        
        // check the arguments
        check_function_args(FUNCTION_INDEX(funstart), current_prog, funstart);
        
        /* Finish the setup */
        fp = inter_fp;
        pc = FUNCTION_CODE(funstart);
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

            int_call_lambda(ap, num_arg, MY_FALSE, MY_FALSE);

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



#ifdef USE_NEW_INLINES
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
        assign_checked_svalue_no_free(sp, inter_context+LOAD_UINT8(pc));
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
        assign_checked_svalue_no_free(sp, inter_context+var_index);
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
        sp->type = T_LVALUE;
        sp->u.lvalue = inter_context + LOAD_UINT8(pc);
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
        sp->type = T_LVALUE;
        sp->u.lvalue = inter_context + var_index;
        break;
      }

#endif /* USE_NEW_INLINES */

    CASE(F_PUSH_IDENTIFIER_LVALUE);  /* --- push_identifier_lvalue <num> --- */
        /* Push an lvalue onto the stack pointing to object-global variable
         * <num>.
         *
         * <num> is an uint8 and used as index in the current objects
         * variable table.
         */
        sp++;
        sp->type = T_LVALUE;
        sp->u.lvalue = find_value((int)(LOAD_UINT8(pc) ));
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
        assign_checked_svalue_no_free(sp
                                     , find_virtual_value((int)(LOAD_UINT8(pc)))
        );
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
        sp->type = T_LVALUE;
        sp->u.lvalue = find_virtual_value((int)(LOAD_UINT8(pc) ));
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
        assign_checked_svalue_no_free(sp, find_value((int)var_index));
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
        sp->type = T_LVALUE;
        sp->u.lvalue = find_value((int)var_index);
        break;
    }
                         /* --- push_local_variable_lvalue <num> --- */
    CASE(F_PUSH_LOCAL_VARIABLE_LVALUE);
        /* Push an lvalue onto the stack pointing to local variable <num>.
         *
         * <num> is an uint8 and used as index onto the framepointer.
         */
        sp++;
        sp->type = T_LVALUE;
        sp->u.lvalue = fp + LOAD_UINT8(pc);
        break;

    CASE(F_PUSH_INDEXED_S_LVALUE); /* --- push_indexed_s_lvalue --- */
        /* Op. (struct v=sp[-2], mixed i=sp[-1], short idx=sp[0])
         *
         * Compute the lvalue &(v[i]) and push it into the stack. If v has
         * just one ref left, the indexed item is stored in indexing_quickfix
         * and the lvalue refers to that variable.
         *
         * <idx> gives the index of the expected struct type - the
         * operator accepts a struct of this type, or any of its children.
         * An negative <idx> accepts any struct.
         */

        sp = check_struct_op(sp, 0, -2, pc);
        sp = push_indexed_lvalue(sp, pc);
        break;

    CASE(F_PUSH_INDEXED_LVALUE);    /* --- push_indexed_lvalue --- */
        /* Operator F_PUSH_INDEXED_LVALUE(vector  v=sp[-1], int   i=sp[0])
         * Operator F_PUSH_INDEXED_LVALUE(mapping v=sp[-1], mixed i=sp[0])
         *
         * Compute the lvalue &(v[i]) and push it into the stack. If v has
         * just one ref left, the indexed item is stored in indexing_quickfix
         * and the lvalue refers to that variable.
         */

        {
            svalue_t * svp = sp-1;

            while (svp->type == T_LVALUE || svp->type == T_PROTECTED_LVALUE)
                svp = svp->u.lvalue;
            if (svp->type == T_STRUCT)
            {
                ERRORF(("Illegal type to []: %s lvalue, "
                        "expected string/mapping/vector lvalue.\n"
                       , typename(svp->type)
                      ));
                /* NOTREACHED */
            }
        }
        sp = push_indexed_lvalue(sp, pc);
        break;

    CASE(F_PUSH_RINDEXED_LVALUE);   /* --- push_rindexed_lvalue --- */
        /* Operator F_PUSH_RINDEXED_LVALUE(vector v=sp[-1], int i=sp[0])
         *
         * Compute the lvalue &(v[<i]) and push it into the stack. If v has
         * just one ref left, the indexed item is stored in indexing_quickfix
         * and the lvalue refers to that variable.
         */

        sp = push_rindexed_lvalue(sp, pc);
        break;

    CASE(F_PUSH_AINDEXED_LVALUE);   /* --- push_aindexed_lvalue --- */
        /* Operator F_PUSH_AINDEXED_LVALUE(vector v=sp[-1], int i=sp[0])
         *
         * Compute the lvalue &(v[>i]) and push it into the stack. If v has
         * just one ref left, the indexed item is stored in indexing_quickfix
         * and the lvalue refers to that variable.
         */

        sp = push_aindexed_lvalue(sp, pc);
        break;

    CASE(F_INDEX_S_LVALUE);         /* --- index_s_lvalue     --- */
        /* Op. (struct &v=sp[0], int i=sp[-2], short * idx=sp[-1])
         *
         * Compute the index &(v[i]) of lvalue <v> and push it into the stack.
         * The computed index is a lvalue itself. 
         *
         * <idx> gives the index of the expected struct type - the
         * operator accepts a struct of this type, or any of its children.
         * An negative <idx> accepts any struct.
         */

        sp = check_struct_op(sp, -1, 1, pc);
        sp = index_lvalue(sp, pc);
        break;

    CASE(F_INDEX_LVALUE);           /* --- index_lvalue       --- */
        /* Operator F_INDEX_LVALUE (string|vector &v=sp[0], int   i=sp[-1])
         *          F_INDEX_LVALUE (mapping       &v=sp[0], mixed i=sp[-1])
         *
         * Compute the index &(v[i]) of lvalue <v> and push it into the stack.
         * The computed index is a lvalue itself.  If <v> is a string-lvalue,
         * it is made a malloced string if necessary, and the pushed result
         * will be a lvalue pointing to a CHAR_LVALUE stored in
         * <special_lvalue>.
         */

        {
            svalue_t * svp = sp;

            while (svp->type == T_LVALUE || svp->type == T_PROTECTED_LVALUE)
                svp = svp->u.lvalue;
            if (svp->type == T_STRUCT)
            {
                ERRORF(("Illegal type to []: %s lvalue, "
                        "expected string/mapping/vector lvalue.\n"
                       , typename(svp->type)
                      ));
                /* NOTREACHED */
            }
        }
        sp = index_lvalue(sp, pc);
        break;

    CASE(F_RINDEX_LVALUE);          /* --- rindex_lvalue      --- */
        /* Operator F_RINDEX_LVALUE (string|vector &v=sp[0], int   i=sp[-1])
         *
         * Compute the index &(v[<i]) of lvalue <v> and push it into the
         * stack. The computed index is a lvalue itself.
         * If <v> is a string-lvalue, it is made a malloced string if
         * necessary, and the pushed result will be a lvalue pointing to a
         * CHAR_LVALUE stored in <special_lvalue>.
         */

        sp = rindex_lvalue(sp, pc);
        break;

    CASE(F_AINDEX_LVALUE);          /* --- aindex_lvalue      --- */
        /* Operator F_AINDEX_LVALUE (string|vector &v=sp[0], int   i=sp[-1])
         *
         * Compute the index &(v[>i]) of lvalue <v> and push it into the
         * stack. The computed index is a lvalue itself.
         * If <v> is a string-lvalue, it is made a malloced string if
         * necessary, and the pushed result will be a lvalue pointing to a
         * CHAR_LVALUE stored in <special_lvalue>.
         */

        sp = aindex_lvalue(sp, pc);
        break;

    CASE(F_S_INDEX);                /* --- s_index            --- */
        /* Operator F_S_INDEX (struct v=sp[-2], mixed i=sp[-1], short idx=sp[0])
         *
         * Compute the value (v->i) and push it onto the stack.  If the value
         * would be a destructed object, 0 is pushed onto the stack and the
         * ref to the object is removed from the struct.
         *
         * <idx> gives the index of the expected struct type - the
         * operator accepts a struct of this type, or any of its children.
         * An negative <idx> accepts any struct.
         */

        sp = check_struct_op(sp, 0, -2, pc);
        sp = push_indexed_value(sp, pc);
        break;

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
        sp = push_indexed_value(sp, pc);
        break;

    CASE(F_RINDEX);                 /* --- rindex              --- */
        /* Operator F_RINDEX (string|vector v=sp[0], int   i=sp[-1])
         *
         * Compute the value (v[<i]) and push it onto the stack.  If the value
         * would be a destructed object, 0 is pushed onto the stack and the
         * ref to the object is removed from the vector/mapping.
         */

        sp = push_rindexed_value(sp, pc);
        break;

    CASE(F_AINDEX);                 /* --- aindex              --- */
        /* Operator F_AINDEX (string|vector v=sp[0], int   i=sp[-1])
         *
         * Compute the value (v[<i]) and push it onto the stack.  If the value
         * would be a destructed object, 0 is pushed onto the stack and the
         * ref to the object is removed from the vector/mapping.
         */

        sp = push_aindexed_value(sp, pc);
        break;

    CASE(F_RANGE_LVALUE);           /* --- range_lvalue        --- */
        /* Operator F_RANGE_LVALUE (string|vector &v=sp[0]
         *                         , int i2=sp[-1], i1=sp[-2])
         *
         * Compute the range &(v[i1..i2]) of lvalue <v> and push it into the
         * stack.  The value pushed is a lvalue pointing to <special_lvalue>.
         * <special_lvalue> then is the POINTER_RANGE_- resp.
         * STRING_RANGE_LVALUE.
         *
         * TODO: Four different instructions for this? A single instruction plus
         * TODO:: argument would be as well.
         */

        inter_pc = pc;
        sp = range_lvalue(NN_RANGE, sp);
        break;

    CASE(F_NR_RANGE_LVALUE);           /* --- nr_range_lvalue     --- */
        /* Operator F_NR_RANGE_LVALUE (string|vector &v=sp[0]
         *                         , int i2=sp[-1], i1=sp[-2])
         *
         * Compute the range &(v[i1..<i2]) of lvalue <v> and push it into the
         * stack.  The value pushed is a lvalue pointing to <special_lvalue>.
         * <special_lvalue> then is the POINTER_RANGE_- resp.
         * STRING_RANGE_LVALUE.
         */

        inter_pc = pc;
        sp = range_lvalue(NR_RANGE, sp);
        break;

    CASE(F_RN_RANGE_LVALUE);           /* --- rn_range_lvalue     --- */
        /* Operator F_RN_RANGE_LVALUE (string|vector &v=sp[0]
         *                         , int i2=sp[-1], i1=sp[-2])
         *
         * Compute the range &(v[<i1..i2]) of lvalue <v> and push it into the
         * stack.  The value pushed is a lvalue pointing to <special_lvalue>.
         * <special_lvalue> then is the POINTER_RANGE_- resp.
         * STRING_RANGE_LVALUE.
         */

        inter_pc = pc;
        sp = range_lvalue(RN_RANGE, sp);
        break;

    CASE(F_RR_RANGE_LVALUE);           /* --- rr_range_lvalue     --- */
        /* Operator F_RR_RANGE_LVALUE (string|vector &v=sp[0]
         *                         , int i2=sp[-1], i1=sp[-2])
         *
         * Compute the range &(v[<i1..<i2]) of lvalue <v> and push it into the
         * stack.  The value pushed is a lvalue pointing to <special_lvalue>.
         * <special_lvalue> then is the POINTER_RANGE_- resp.
         * STRING_RANGE_LVALUE.
         */

        inter_pc = pc;
        sp = range_lvalue(RR_RANGE, sp);
        break;

    CASE(F_NA_RANGE_LVALUE);           /* --- na_range_lvalue     --- */
        /* Operator F_NA_RANGE_LVALUE (string|vector &v=sp[0]
         *                         , int i2=sp[-1], i1=sp[-2])
         *
         * Compute the range &(v[i1..>i2]) of lvalue <v> and push it into the
         * stack.  The value pushed is a lvalue pointing to <special_lvalue>.
         * <special_lvalue> then is the POINTER_RANGE_- resp.
         * STRING_RANGE_LVALUE.
         */

        inter_pc = pc;
        sp = range_lvalue(NA_RANGE, sp);
        break;

    CASE(F_AN_RANGE_LVALUE);           /* --- an_range_lvalue     --- */
        /* Operator F_AN_RANGE_LVALUE (string|vector &v=sp[0]
         *                         , int i2=sp[-1], i1=sp[-2])
         *
         * Compute the range &(v[>i1..i2]) of lvalue <v> and push it into the
         * stack.  The value pushed is a lvalue pointing to <special_lvalue>.
         * <special_lvalue> then is the POINTER_RANGE_- resp.
         * STRING_RANGE_LVALUE.
         */

        inter_pc = pc;
        sp = range_lvalue(AN_RANGE, sp);
        break;

    CASE(F_RA_RANGE_LVALUE);           /* --- ra_range_lvalue     --- */
        /* Operator F_RA_RANGE_LVALUE (string|vector &v=sp[0]
         *                         , int i2=sp[-1], i1=sp[-2])
         *
         * Compute the range &(v[<i1..>i2]) of lvalue <v> and push it into the
         * stack.  The value pushed is a lvalue pointing to <special_lvalue>.
         * <special_lvalue> then is the POINTER_RANGE_- resp.
         * STRING_RANGE_LVALUE.
         */

        inter_pc = pc;
        sp = range_lvalue(RA_RANGE, sp);
        break;

    CASE(F_AR_RANGE_LVALUE);           /* --- ar_range_lvalue     --- */
        /* Operator F_AR_RANGE_LVALUE (string|vector &v=sp[0]
         *                         , int i2=sp[-1], i1=sp[-2])
         *
         * Compute the range &(v[>i1..<i2]) of lvalue <v> and push it into the
         * stack.  The value pushed is a lvalue pointing to <special_lvalue>.
         * <special_lvalue> then is the POINTER_RANGE_- resp.
         * STRING_RANGE_LVALUE.
         */

        inter_pc = pc;
        sp = range_lvalue(AR_RANGE, sp);
        break;

    CASE(F_AA_RANGE_LVALUE);           /* --- aa_range_lvalue     --- */
        /* Operator F_AA_RANGE_LVALUE (string|vector &v=sp[0]
         *                         , int i2=sp[-1], i1=sp[-2])
         *
         * Compute the range &(v[>i1..>i2]) of lvalue <v> and push it into the
         * stack.  The value pushed is a lvalue pointing to <special_lvalue>.
         * <special_lvalue> then is the POINTER_RANGE_- resp.
         * STRING_RANGE_LVALUE.
         */

        inter_pc = pc;
        sp = range_lvalue(AA_RANGE, sp);
        break;

    CASE(F_NX_RANGE_LVALUE);           /* --- nx_range_lvalue     --- */
        /* Operator F_NX_RANGE_LVALUE (string|vector &v=sp[0]
         *                            , int i1=sp[-1])
         *
         * Compute the range &(v[i1..]) of lvalue <v> and push it into the
         * stack.  The value pushed is a lvalue pointing to <special_lvalue>.
         * <special_lvalue> then is the POINTER_RANGE_- resp.
         * STRING_RANGE_LVALUE.
         *
         * We implement this by pushing '1' onto the stack and then
         * call F_NR_RANGE_LVALUE, effectively computing &(v[i1..<1]).
         */

        inter_pc = pc;
        sp++;
        sp[0] = sp[-1];       /* Pull up the 'v' */
        put_number(sp-1, 1);  /* 'Push' the 1 for the upper bound */
        sp = range_lvalue(NR_RANGE, sp);
        break;

    CASE(F_RX_RANGE_LVALUE);           /* --- rx_range_lvalue     --- */
        /* Operator F_RX_RANGE_LVALUE (string|vector &v=sp[0]
         *                            , int i1=sp[-1])
         *
         * Compute the range &(v[<i1..]) of lvalue <v> and push it into the
         * stack.  The value pushed is a lvalue pointing to <special_lvalue>.
         * <special_lvalue> then is the POINTER_RANGE_- resp.
         * STRING_RANGE_LVALUE.
         *
         * We implement this by pushing '1' onto the stack and then
         * call F_RR_RANGE_LVALUE, effectively computing &(v[<i1..<1]).
         */

        inter_pc = pc;
        sp++;
        sp[0] = sp[-1];       /* Pull up the 'v' */
        put_number(sp-1, 1);  /* 'Push' the 1 for the upper bound */
        sp = range_lvalue(RR_RANGE, sp);
        break;

    CASE(F_AX_RANGE_LVALUE);           /* --- ax_range_lvalue     --- */
        /* Operator F_AX_RANGE_LVALUE (string|vector &v=sp[0]
         *                            , int i1=sp[-1])
         *
         * Compute the range &(v[>i1..]) of lvalue <v> and push it into the
         * stack.  The value pushed is a lvalue pointing to <special_lvalue>.
         * <special_lvalue> then is the POINTER_RANGE_- resp.
         * STRING_RANGE_LVALUE.
         *
         * We implement this by pushing '1' onto the stack and then
         * call F_AR_RANGE_LVALUE, effectively computing &(v[>i1..<1]).
         */

        inter_pc = pc;
        sp++;
        sp[0] = sp[-1];       /* Pull up the 'v' */
        put_number(sp-1, 1);  /* 'Push' the 1 for the upper bound */
        sp = range_lvalue(AR_RANGE, sp);
        break;

                        /* --- push_protected_indexed_s_lvalue --- */
    CASE(F_PUSH_PROTECTED_INDEXED_S_LVALUE);
        /* Op. (struct  v=sp[-2], mixed i=sp[-1], short idx=sp[0])
         *
         * Compute the lvalue &(v[i]), store it in a struct
         * protected_lvalue, and push the protector as PROTECTED_LVALUE
         * into the stack.
         *
         * short <idx> gives the index of the expected struct type - the
         * operator accepts a struct of this type, or any of its children.
         * An negative <idx> accepts any struct.
         */

        sp = check_struct_op(sp, 0, 3, pc);
        sp = push_protected_indexed_lvalue(sp, pc);
        break;

                          /* --- push_protected_indexed_lvalue --- */
    CASE(F_PUSH_PROTECTED_INDEXED_LVALUE);
        /* Op. (vector  v=sp[-1], int   i=sp[0])
         * Op. (mapping v=sp[-1], mixed i=sp[0])
         *
         * Compute the lvalue &(v[i]), store it in a struct
         * protected_lvalue, and push the protector as PROTECTED_LVALUE
         * into the stack.
         */

        if ((sp-1)->type == T_STRUCT)
        {
            ERRORF(("Illegal type to []: %s, expected vector/mapping.\n"
                   , typename((sp-1)->type)
                  ));
            /* NOTREACHED */
        }
        sp = push_protected_indexed_lvalue(sp, pc);
        break;

                         /* --- push_protected_rindexed_lvalue --- */
    CASE(F_PUSH_PROTECTED_RINDEXED_LVALUE);
        /* Op. (vector v=sp[-1], int i=sp[0])
         *
         * Compute the lvalue &(v[<i]), store it in a struct
         * protected_lvalue, and push the protector as PROTECTED_LVALUE
         * into the stack.
         */

        sp = push_protected_rindexed_lvalue(sp, pc);
        break;

                         /* --- push_protected_aindexed_lvalue --- */
    CASE(F_PUSH_PROTECTED_AINDEXED_LVALUE);
        /* Op. (vector v=sp[-1], int i=sp[0])
         *
         * Compute the lvalue &(v[>i]), store it in a struct
         * protected_lvalue, and push the protector as PROTECTED_LVALUE
         * into the stack.
         */

        sp = push_protected_aindexed_lvalue(sp, pc);
        break;

                      /* --- push_protected_indexed_map_lvalue --- */
    CASE(F_PUSH_PROTECTED_INDEXED_MAP_LVALUE);
        /* Op. (mapping m=sp[-2], mixed i=sp[-1], int   j=sp[0])
         *
         * Compute the lvalue &(m[i:j]), store it in a struct
         * protected_lvalue, and push the protector as PROTECTED_LVALUE
         * into the stack.
         */

        push_protected_indexed_map_lvalue(sp, pc);
        break;

                               /* --- protected_index_s_lvalue --- */
    CASE(F_PROTECTED_INDEX_S_LVALUE);
        /* Operator (struct &v=sp[0], mixed i=sp[-2], short idx=sp[-1])
         *
         * Compute the index &(*v[i]) of lvalue <v>, wrap it into a
         * protector, and push the reference to the protector as
         * PROTECTED_LVALUE onto the stack.
         *
         * short <idx> gives the index of the expected struct type - the
         * operator accepts a struct of this type, or any of its children.
         * An negative <idx> accepts any struct.
         */

        sp = check_struct_op(sp, -1, 1, pc);
        sp = protected_index_lvalue(sp, pc);
        break;

                                 /* --- protected_index_lvalue --- */
    CASE(F_PROTECTED_INDEX_LVALUE);
        /* Operator (string|vector &v=sp[0], int   i=sp[-1])
         *          (mapping       &v=sp[0], mixed i=sp[-1])
         *
         * Compute the index &(*v[i]) of lvalue <v>, wrap it into a
         * protector, and push the reference to the protector as
         * PROTECTED_LVALUE onto the stack.
         *
         * If <v> is a protected non-string-lvalue, the protected_lvalue
         * referenced by <v>.u.lvalue will be deallocated, and the
         * protector itself will be stored in <last_indexing_protector>
         * for the time being.
         *
         * If <v> is a string-lvalue, it is made a malloced string if
         * necessary.
         */

        if ((sp-1)->type == T_STRUCT)
        {
            ERRORF(("Illegal type to []: %s, expected string/vector/mapping.\n"
                   , typename((sp-1)->type)
                  ));
            /* NOTREACHED */
        }
        sp = protected_index_lvalue(sp, pc);
        break;

                                /* --- protected_rindex_lvalue --- */
    CASE(F_PROTECTED_RINDEX_LVALUE);
        /* Operator (string|vector &v=sp[0], int   i=sp[-1])
         *
         * Compute the index &(*v[<i]) of lvalue <v>, wrap it into a
         * protector, and push the reference to the protector as
         * PROTECTED_LVALUE onto the stack.
         *
         * If <v> is a protected non-string-lvalue, the protected_lvalue
         * referenced by <v>.u.lvalue will be deallocated, and the
         * protector itself will be stored in <last_indexing_protector>
         * for the time being.
         *
         * If <v> is a string-lvalue, it is made a malloced string if
         * necessary.
         */

        sp = protected_rindex_lvalue(sp, pc);
        break;

                                /* --- protected_aindex_lvalue --- */
    CASE(F_PROTECTED_AINDEX_LVALUE);
        /* Operator (string|vector &v=sp[0], int   i=sp[-1])
         *
         * Compute the index &(*v[>i]) of lvalue <v>, wrap it into a
         * protector, and push the reference to the protector as
         * PROTECTED_LVALUE onto the stack.
         *
         * If <v> is a protected non-string-lvalue, the protected_lvalue
         * referenced by <v>.u.lvalue will be deallocated, and the
         * protector itself will be stored in <last_indexing_protector>
         * for the time being.
         *
         * If <v> is a string-lvalue, it is made a malloced string if
         * necessary.
         */

        sp = protected_aindex_lvalue(sp, pc);
        break;

                              /* --- protected_range_lvalue --- */
    CASE(F_PROTECTED_RANGE_LVALUE);
        /* Operator (string|vector &v=sp[0], int i2=sp[-1], i1=sp[-2])
         *
         * Compute the range &(v[i1..i2]) of lvalue <v>, wrap it into a
         * protector, and push the reference to the protector onto the
         * stack.
         *
         * If <v> is a protected lvalue itself, its protecting svalue will
         * be used in the result protector.
         *
         * If <v> is a string-lvalue, it is made a malloced string if
         * necessary.
         */

        inter_pc = pc;
        sp = protected_range_lvalue(NN_RANGE, sp);
        break;

                           /* --- protected_nr_range_lvalue --- */
    CASE(F_PROTECTED_NR_RANGE_LVALUE);
        /* Operator (string|vector &v=sp[0], int i2=sp[-1], i1=sp[-2])
         *
         * Compute the range &(v[i1..<i2]) of lvalue <v>, wrap it into a
         * protector, and push the reference to the protector onto the
         * stack.
         *
         * If <v> is a protected lvalue itself, its protecting svalue will
         * be used in the result protector.
         *
         * If <v> is a string-lvalue, it is made a malloced string if
         * necessary.
         */

        inter_pc = pc;
        sp = protected_range_lvalue(NR_RANGE, sp);
        break;

                             /* --- protected_rn_range_lvalue --- */
    CASE(F_PROTECTED_RN_RANGE_LVALUE);
        /* Operator (string|vector &v=sp[0], int i2=sp[-1], i1=sp[-2])
         *
         * Compute the range &(v[<i1..i2]) of lvalue <v>, wrap it into a
         * protector, and push the reference to the protector onto the
         * stack.
         *
         * If <v> is a protected lvalue itself, its protecting svalue will
         * be used in the result protector.
         *
         * If <v> is a string-lvalue, it is made a malloced string if
         * necessary.
         */

        inter_pc = pc;
        sp = protected_range_lvalue(RN_RANGE, sp);
        break;

                             /* --- protected_rr_range_lvalue --- */
    CASE(F_PROTECTED_RR_RANGE_LVALUE);
        /* Operator (string|vector &v=sp[0], int i2=sp[-1], i1=sp[-2])
         *
         * Compute the range &(v[<i1..<i2]) of lvalue <v>, wrap it into a
         * protector, and push the reference to the protector onto the
         * stack.
         *
         * If <v> is a protected lvalue itself, its protecting svalue will
         * be used in the result protector.
         *
         * If <v> is a string-lvalue, it is made a malloced string if
         * necessary.
         */

        inter_pc = pc;
        sp = protected_range_lvalue(RR_RANGE, sp);
        break;

                           /* --- protected_na_range_lvalue --- */
    CASE(F_PROTECTED_NA_RANGE_LVALUE);
        /* Operator (string|vector &v=sp[0], int i2=sp[-1], i1=sp[-2])
         *
         * Compute the range &(v[i1..>i2]) of lvalue <v>, wrap it into a
         * protector, and push the reference to the protector onto the
         * stack.
         *
         * If <v> is a protected lvalue itself, its protecting svalue will
         * be used in the result protector.
         *
         * If <v> is a string-lvalue, it is made a malloced string if
         * necessary.
         */

        inter_pc = pc;
        sp = protected_range_lvalue(NA_RANGE, sp);
        break;

                             /* --- protected_an_range_lvalue --- */
    CASE(F_PROTECTED_AN_RANGE_LVALUE);
        /* Operator (string|vector &v=sp[0], int i2=sp[-1], i1=sp[-2])
         *
         * Compute the range &(v[>i1..i2]) of lvalue <v>, wrap it into a
         * protector, and push the reference to the protector onto the
         * stack.
         *
         * If <v> is a protected lvalue itself, its protecting svalue will
         * be used in the result protector.
         *
         * If <v> is a string-lvalue, it is made a malloced string if
         * necessary.
         */

        inter_pc = pc;
        sp = protected_range_lvalue(AN_RANGE, sp);
        break;

                           /* --- protected_ra_range_lvalue --- */
    CASE(F_PROTECTED_RA_RANGE_LVALUE);
        /* Operator (string|vector &v=sp[0], int i2=sp[-1], i1=sp[-2])
         *
         * Compute the range &(v[<i1..>i2]) of lvalue <v>, wrap it into a
         * protector, and push the reference to the protector onto the
         * stack.
         *
         * If <v> is a protected lvalue itself, its protecting svalue will
         * be used in the result protector.
         *
         * If <v> is a string-lvalue, it is made a malloced string if
         * necessary.
         */

        inter_pc = pc;
        sp = protected_range_lvalue(RA_RANGE, sp);
        break;

                             /* --- protected_ar_range_lvalue --- */
    CASE(F_PROTECTED_AR_RANGE_LVALUE);
        /* Operator (string|vector &v=sp[0], int i2=sp[-1], i1=sp[-2])
         *
         * Compute the range &(v[>i1..<i2]) of lvalue <v>, wrap it into a
         * protector, and push the reference to the protector onto the
         * stack.
         *
         * If <v> is a protected lvalue itself, its protecting svalue will
         * be used in the result protector.
         *
         * If <v> is a string-lvalue, it is made a malloced string if
         * necessary.
         */

        inter_pc = pc;
        sp = protected_range_lvalue(AR_RANGE, sp);
        break;

                             /* --- protected_aa_range_lvalue --- */
    CASE(F_PROTECTED_AA_RANGE_LVALUE);
        /* Operator (string|vector &v=sp[0], int i2=sp[-1], i1=sp[-2])
         *
         * Compute the range &(v[>i1..>i2]) of lvalue <v>, wrap it into a
         * protector, and push the reference to the protector onto the
         * stack.
         *
         * If <v> is a protected lvalue itself, its protecting svalue will
         * be used in the result protector.
         *
         * If <v> is a string-lvalue, it is made a malloced string if
         * necessary.
         */

        inter_pc = pc;
        sp = protected_range_lvalue(AA_RANGE, sp);
        break;

                              /* --- protected_nx_range_lvalue --- */
    CASE(F_PROTECTED_NX_RANGE_LVALUE);
        /* Operator (string|vector &v=sp[0], i1=sp[-1])
         *
         * Compute the range &(v[i1..]) of lvalue <v>, wrap it into a
         * protector, and push the reference to the protector onto the
         * stack.
         *
         * If <v> is a protected lvalue itself, its protecting svalue will
         * be used in the result protector.
         *
         * If <v> is a string-lvalue, it is made a malloced string if
         * necessary.
         *
         * We implement it by pushing '1' onto the stack and then
         * calling protected_nr_range_lvalue, effectively computing
         * &(v[i1..<1]).
         */

        inter_pc = pc;
        sp++;
        sp[0] = sp[-1];       /* Pull up the 'v' */
        put_number(sp-1, 1);  /* 'Push' the 1 for the upper bound */
        sp = protected_range_lvalue(NR_RANGE, sp);
        break;

                          /* --- protected_rx_range_lvalue --- */
    CASE(F_PROTECTED_RX_RANGE_LVALUE);
        /* Operator (string|vector &v=sp[0], int i1=sp[-1])
         *
         * Compute the range &(v[<i1..]) of lvalue <v>, wrap it into a
         * protector, and push the reference to the protector onto the
         * stack.
         *
         * If <v> is a protected lvalue itself, its protecting svalue will
         * be used in the result protector.
         *
         * If <v> is a string-lvalue, it is made a malloced string if
         * necessary.
         *
         * We implement it by pushing '1' onto the stack and then
         * calling protected_nr_range_lvalue, effectively computing
         * &(v[<i1..<1]).
         */

        inter_pc = pc;
        sp++;
        sp[0] = sp[-1];       /* Pull up the 'v' */
        put_number(sp-1, 1);  /* 'Push' the 1 for the upper bound */
        sp = protected_range_lvalue(RR_RANGE, sp);
        break;

                          /* --- protected_ax_range_lvalue --- */
    CASE(F_PROTECTED_AX_RANGE_LVALUE);
        /* Operator (string|vector &v=sp[0], int i1=sp[-1])
         *
         * Compute the range &(v[>i1..]) of lvalue <v>, wrap it into a
         * protector, and push the reference to the protector onto the
         * stack.
         *
         * If <v> is a protected lvalue itself, its protecting svalue will
         * be used in the result protector.
         *
         * If <v> is a string-lvalue, it is made a malloced string if
         * necessary.
         *
         * We implement it by pushing '1' onto the stack and then
         * calling protected_ar_range_lvalue, effectively computing
         * &(v[>i1..<1]).
         */

        inter_pc = pc;
        sp++;
        sp[0] = sp[-1];       /* Pull up the 'v' */
        put_number(sp-1, 1);  /* 'Push' the 1 for the upper bound */
        sp = protected_range_lvalue(AR_RANGE, sp);
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
        fun_hdr_p           funstart;  /* the actual function */
        object_t           *ob;        /* the simul_efun object */
        int                 def_narg;  /* expected number of arguments */
        simul_efun_table_t *entry;

        assign_eval_cost_inl();  /* we're changing objects */

        /* Get the sefun code and the number of arguments on the stack */
        LOAD_SHORT(code, pc);
        def_narg = simul_efunp[code].num_arg;

        if (use_ap
         || def_narg == SIMUL_EFUN_VARARGS
         || (simul_efunp[code].flags & TYPE_MOD_XVARARGS)
           )
        {
            use_ap = MY_FALSE;  /* Reset the flag */
            num_arg = sp - ap + 1;
        }
        else
            num_arg = def_narg;

        /* Correct the number of arguments on the stack */
        if (num_arg != def_narg && def_narg != SIMUL_EFUN_VARARGS)
        {
            /* If it's an XVARARGS, we don't require the last argument. */
            if (simul_efunp[code].flags & TYPE_MOD_XVARARGS)
                def_narg--;

            /* Add eventually missing arguments */
            while (num_arg < def_narg)
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

#ifdef USE_NEW_INLINES
            push_control_stack(sp, pc, fp, inter_context);
#else
            push_control_stack(sp, pc, fp);
#endif /* USE_NEW_INLINES */
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
            new_sp = setup_new_frame2(funstart, sp, MY_TRUE, MY_FALSE);
            inter_sp = new_sp;
            check_function_args(FUNCTION_INDEX(funstart), current_prog, funstart);
            
            /* The simul_efun object should not use simul_efuns itself... */
            previous_ob = current_object;
            current_object = ob;
            current_strings = prog->strings;
            eval_instruction(FUNCTION_CODE(funstart), new_sp);
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
        assign_checked_svalue_no_free(sp, cstart - ix);
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
        assign_checked_svalue_no_free(sp, cstart - ix);
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
            assign_checked_svalue_no_free(sp, data + n);
        }
        free_mapping(m);
        break;
    }

    CASE(F_PUSH_INDEXED_MAP_LVALUE); /* --- push_indexed_map_lvalue --- */
    {
        /* Operator F_PUSH_INDEXED_MAP_LVALUE( mapping m=sp[-2]
         *                                   , mixed i=sp[-1], int j=sp[0])
         *
         * Compute the lvalue &(m[i,j]) and push it into the stack. If v has
         * just one ref left, the indexed item is stored in indexing_quickfix
         * and the lvalue refers to that variable.
         */
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
        data = get_map_lvalue(m, sp);
        if (!data)
        {
            outofmemory("indexed lvalue");
            /* NOTREACHED */
            return MY_FALSE;
        }
        pop_stack();

        if (!m->ref)
        {
            assign_svalue (&indexing_quickfix, data + n);
            sp->type = T_LVALUE;
            sp->u.lvalue = &indexing_quickfix;
            break;
        }
        else
        {
            sp->type = T_LVALUE;
            sp->u.lvalue = data + n;
        }
        free_mapping(m);
        break;
    }

    CASE(F_FOREACH);       /* --- foreach     <nargs> <offset> --- */
    CASE(F_FOREACH_REF);   /* --- foreach_ref <nargs> <offset> --- */
    CASE(F_FOREACH_RANGE); /* --- foreach_range <nargs> <offset> --- */
    {
        /* Initialize a foreach() loop. On the stack are <nargs>-1
         * lvalues where the (l)value(s) are to be stored. The last
         * value on the stack is the (l)value to loop over. (Do not
         * confuse <nargs> with the normal NUM_ARG!).
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
         *                             is mapping
         *   sp[-2] -> array 'm_indices': if the value is a mapping, this
         *                             is the array with the indices.
         *
         * After pushing the values onto the stack, the instruction
         * branches to the FOREACH_NEXT instruction to start the first
         * iteration.
         */

        int vars_required;
        int8_t nargs;
        p_int count, start;
        unsigned short offset;
        Bool gen_refs, use_range, do_extra_loop;
        svalue_t * arg;

        gen_refs = (instruction == F_FOREACH_REF);
        use_range = (instruction == F_FOREACH_RANGE);
        do_extra_loop = MY_FALSE;
        start = 0;

        nargs = load_uint8(&pc);
        LOAD_SHORT(offset, pc);

        /* Unravel the lvalue chain (if any) to get to the actual value
         * to loop over.
         */
        if (gen_refs && sp->type != T_LVALUE)
        {
            ERRORF(("foreach() got a %s, expected a &(string/array/mapping).\n"
                   , typename(sp->type)
                   ));
        }

        for (arg = sp
            ; gen_refs && arg && arg->type == T_LVALUE
            ; arg = arg->u.lvalue)
            NOOP;

        if (use_range && arg->type != T_NUMBER)
            ERRORF(("foreach() got a %s, requires a number for upper range bound.\n"
                   , typename(arg->type)
                   ));

        if (arg->type != T_STRING
         && arg->type != T_POINTER
         && arg->type != T_NUMBER
         && arg->type != T_STRUCT
         && arg->type != T_MAPPING)
            ERRORF(("foreach() got a %s, expected a (&)string/array/mapping/struct or number.\n"
                   , typename(sp->type)
                   ));

        if (gen_refs && arg->type == T_NUMBER)
            ERROR("foreach() got a &number, expected a (&)string/array/mapping/struct or number.\n"
                   );

        /* Find out how many variables we require */

        if (arg->type == T_NUMBER)
        {
            count = arg->u.number;
            if (count < 0 && !use_range)
                ERRORF(("foreach() got a %"PRIdPINT", expected a non-negative "
                        "number.", count));
            vars_required = 1;
        }
        else if (arg->type == T_STRING)
        {
            count = mstrsize(arg->u.str);
            vars_required = 1;

            if (gen_refs)
            {
                string_t *str;

                /* If the string is tabled, i.e. not changeable, or has more
                 * than one reference, allocate a new copy which can be
                 * changed safely.
                 */
                if (!mstr_singular(arg->u.str))
                {
                    memsafe(str = unshare_mstring(arg->u.str), mstrsize(arg->u.str)
                           , "modifiable string");
                    arg->u.str = str;
                }

                /* Replace the string-lvalue on the stack by the string
                 * itself - we don't need the lvalue any more.
                 */
                str = ref_mstring(arg->u.str);
                free_svalue(sp);
                put_string(sp, str);
            }
        }
        else if (arg->type == T_POINTER)
        {
            check_for_destr(arg->u.vec);
            count = VEC_SIZE(arg->u.vec);
            vars_required = 1;

            if (gen_refs)
            {
                /* Replace the array-lvalue on the stack by the array
                 * itself - we don't need the lvalue any more.
                 */
                vector_t * vec = arg->u.vec;

                (void)ref_array(vec);
                free_svalue(sp);
                put_array(sp, vec);
            }
        }
        else if (arg->type == T_STRUCT)
        {
            struct_check_for_destr(arg->u.strct);
            count = struct_size(arg->u.strct);
            vars_required = 1;

            if (gen_refs)
            {
                /* Replace the struct-lvalue on the stack by the struct
                 * itself - we don't need the lvalue any more.
                 */
                struct_t * st = arg->u.strct;

                (void)ref_struct(st);
                free_svalue(sp);
                put_struct(sp, st);
            }
        }
        else
        {
            mapping_t *m;
            vector_t  *indices;

            m = arg->u.map;
            vars_required = 1 + m->num_values;
            indices = m_indices(m);

            count = MAP_SIZE(m);
              /* after m_indices(), else we'd count destructed entries */

            if (gen_refs)
            {
                /* Replace the mapping-lvalue on the stack by the mapping
                 * itself - we don't need the lvalue any more.
                 */
                (void)ref_mapping(m);
                free_svalue(sp);
                put_mapping(sp, m);
            }

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

    CASE(F_FOREACH_NEXT);         /* --- foreach_next <offset> --- */
    {
        /* Start the next (resp. the first) iteration of a foreach()
         * loop. ushort <offset> is the distance to branch back to the
         * loop body, counted from the first byte of the next instruction.
         * For the stack layout, see F_FOREACH.
         */

        unsigned short offset;
        p_int     ix;
        svalue_t *lvalue;  /* Pointer to the first lvalue */
        Bool      gen_refs;


        LOAD_SHORT(offset, pc);

        ix = sp->u.number;
        if (sp->x.generic == 2)
        {
            sp->x.generic = 0;
            /* FOREACH_RANGE with extra loop: don't increment the
             * 'next' number on this one.
             */
        }
        else
        {
            /* Is there something left to iterate? */
            if (0 == sp[-1].u.number)
                break; /* Nope */

            sp->u.number++; /* next number */
        }
        sp[-1].u.number--; /* decrement loop count */

        gen_refs = sp->x.generic;

        if (sp[-1].x.generic < 0)
        {
            /* We loop over a mapping */

            mapping_t *m;
            vector_t  *indices;
            svalue_t  *values;
            p_int        left;

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
                pc -= 3;
                break;
            }

            /* Assign the index we used */
            {
                svalue_t *dest;

#ifdef DEBUG
                if (lvalue->type != T_LVALUE)
                    fatal("Bad argument to foreach(): not a lvalue\n");
                    /* TODO: Give type and value */
#endif
                dest = lvalue->u.lvalue;
                assign_svalue(dest, indices->item+ix);

                lvalue++;
            }

            /* Loop over the values and assign them */
            left = -(sp[-1].x.generic) - 2;
            if (left > m->num_values)
                left = m->num_values;

            for ( ; left > 0; left--, lvalue++, values++)
            {
                svalue_t *dest;

#ifdef DEBUG
                if (lvalue->type != T_LVALUE)
                    fatal("Bad argument to foreach(): not a lvalue\n");
                    /* TODO: Give type and value */
#endif
                dest = lvalue->u.lvalue;
                if (!gen_refs)
                {
                    assign_svalue(dest, values);
                }
                else
                {
                    struct protected_lvalue * prot;

                    free_svalue(dest);

                    prot = (struct protected_lvalue *)xalloc(sizeof *prot);
                    prot->v.type = T_PROTECTED_LVALUE;
                    prot->v.u.lvalue = values;
                    (void)ref_mapping(m);
                    BUILD_MAP_PROTECTOR(prot->protector, m)

                    dest->type = T_LVALUE;
                    dest->u.lvalue = &prot->v;
                }
            }

            /* Ta-Da! */
        }
        else
        {
            lvalue = sp - sp[-1].x.generic - 1;
#ifdef DEBUG
            if (lvalue->type != T_LVALUE)
                fatal("Bad argument to foreach(): not a lvalue\n");
                /* TODO: Give type and value */
#endif
            lvalue = lvalue->u.lvalue;

            if (sp[-2].type == T_NUMBER)
            {
                  free_svalue(lvalue);
                  put_number(lvalue, ix);
            }
            else if (sp[-2].type == T_STRING)
            {
                free_svalue(lvalue);
                if (!gen_refs)
                {
                    put_number(lvalue, get_txt(sp[-2].u.str)[ix]);
                }
                else
                {
                    svalue_t * str = sp-2;
                    struct protected_char_lvalue *val;

                    /* Compute and return the result */

                    (void)ref_mstring(str->u.str);
                    val = (struct protected_char_lvalue *)xalloc(sizeof *val);
                    val->v.type = T_PROTECTED_CHAR_LVALUE;
                    val->v.u.charp = &(get_txt(str->u.str)[ix]);
                    val->lvalue = str;
                    val->start = get_txt(str->u.str);
                    val->protector.type = T_INVALID;

                    lvalue->type = T_LVALUE;
                    lvalue->u.protected_char_lvalue = val;
                }
            }
            else if (sp[-2].type == T_POINTER)
            {
                if (ix >= VEC_SIZE(sp[-2].u.vec))
                    break;
                    /* Oops, this array shrunk while we're looping over it.
                     * We stop processing and continue with the following
                     * FOREACH_END instruction.
                     */

                if (!gen_refs)
                {
                    assign_svalue(lvalue, sp[-2].u.vec->item+ix);
                }
                else
                {
                    svalue_t * vec = sp-2;
                    svalue_t * item;
                    struct protected_lvalue * prot;

                    free_svalue(lvalue);

                    /* Compute the indexed item and set up the protector */

                    item = &vec->u.vec->item[ix];
                    prot = (struct protected_lvalue *)xalloc(sizeof *prot);
                    prot->v.type = T_PROTECTED_LVALUE;
                    prot->v.u.lvalue = item;
                    put_ref_array(&(prot->protector), vec->u.vec);

                    lvalue->type = T_LVALUE;
                    lvalue->u.lvalue = &prot->v;
                }
            }
            else if (sp[-2].type == T_STRUCT)
            {
                if (ix >= struct_size(sp[-2].u.strct))
                    break;
                    /* Oops, somehow the struct managed to shring while
                     * we're looping over it.
                     * We stop processing and continue with the following
                     * FOREACH_END instruction.
                     */
                     
                if (!gen_refs)
                {
                    assign_svalue(lvalue, sp[-2].u.strct->member+ix);
                }
                else
                {
                    svalue_t * st = sp-2;
                    svalue_t * item;
                    struct protected_lvalue * prot;

                    free_svalue(lvalue);

                    /* Compute the indexed item and set up the protector */

                    item = &st->u.strct->member[ix];
                    prot = (struct protected_lvalue *)xalloc(sizeof *prot);
                    prot->v.type = T_PROTECTED_LVALUE;
                    prot->v.u.lvalue = item;
                    put_ref_struct(&(prot->protector), st->u.strct);

                    lvalue->type = T_LVALUE;
                    lvalue->u.lvalue = &prot->v;
                }
            }
            else
              fatal("foreach() requires a string, array, struct or mapping.\n");
              /* If this happens, the check in F_FOREACH failed. */
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
         * Executed when no error occured, it returns into
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

        int i;

        i = (sp->type == T_LVALUE && sp->u.lvalue->type == T_LVALUE);
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

    /* --- Efuns: Strings --- */

    CASE(F_STRLEN);                 /* --- strlen              --- */
    {
        /* EFUN strlen()
         *
         *   int strlen(string str)
         *
         * Returns the length of the string str.
         */

        size_t i;

        if (sp->type == T_STRING)
        {
            i = mstrsize(sp->u.str);
            free_string_svalue(sp);
            put_number(sp, i);
            break;
        }
        if (sp->type == T_NUMBER && sp->u.number == 0)
            break;
        RAISE_ARG_ERROR(1, TF_NULL|TF_STRING, sp->type);
        /* NOTREACHED */
    }

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

        if (sp->type == T_STRING)
        {
            i = mstrsize(sp->u.str);
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

        RAISE_ARG_ERROR(1, TF_NULL|TF_MAPPING|TF_POINTER, sp->type);
        /* NOTREACHED */
    }

    /* --- Efuns: Functions and Closures --- */

    CASE(F_CALL_DIRECT);            /* --- call_direct         --- */
    CASE(F_CALL_OTHER);             /* --- call_other          --- */
    {
        /* EFUN call_other(), call_direct()
         *
         *     unknown call_other(object|string ob, string str, mixed arg, ...)
         *     unknown ob->fun(mixed arg, ...)
         *
         *     unknown call_direct(object|string ob, string str, mixed arg, ...)
         *
         * Call a member function in another object with an argument. The
         * return value is returned from the other object.  The object can be
         * given directly or as a string (i.e. its file name). If it is given
         * by a string and the object does not exist yet, it will be loaded.
         *
#ifdef USE_ARRAY_CALLS
         *     unknown * call_other(object|string *ob, string str, mixed arg, ...)
         *     unknown * ob->fun(mixed arg, ...)
         *
         * Call a member function in other objects with the given arguments.
         * The return values is returned collected in an array.
         * Every object can be given directly or as a string (i.e. its file name).
         * If it is given by a string and the object does not exist yet, it will
         * be loaded.
#endif
         *
         * The difference between call_other() and call_direct()
         * is that the latter does not allow the evaluation of default
         * methods.
         *
         * TODO: A VOID_CALL_OTHER would be nice to have when the result
         * TODO:: is not used.
         */

        svalue_t *arg;
        object_t *ob;
        Bool      b_use_default;

        num_arg = sp - ap + 1;
        inter_pc = pc;
        inter_sp = sp;

        arg = sp - num_arg + 1;

        /* Test the arguments */
        if (arg[0].type != T_OBJECT
         && arg[0].type != T_STRING
#ifdef USE_ARRAY_CALLS
         && arg[0].type != T_POINTER
#endif /* USE_ARRAY_CALLS */
           )
        {
#ifdef USE_ARRAY_CALLS
            RAISE_ARG_ERROR(1, TF_OBJECT|TF_STRING|TF_POINTER, arg[0].type);
#else
            RAISE_ARG_ERROR(1, TF_OBJECT|TF_STRING, arg[0].type);
#endif /* USE_ARRAY_CALLS */
        }

        TYPE_TEST2(arg+1, T_STRING)
        if (get_txt(arg[1].u.str)[0] == ':')
            ERRORF(("Illegal function name in call_other: %s\n",
                  get_txt(arg[1].u.str)));

        /* No external calls may be done when this object is
         * destructed.
         */
        if (current_object->flags & O_DESTRUCTED)
        {
            pop_n_elems(num_arg);
            push_number(sp, 0);
            WARNF(("Call from destructed object '%s' ignored.\n"
                  , get_txt(current_object->name)));
            break;
        }

#ifdef USE_ARRAY_CALLS
        if (arg[0].type != T_POINTER)
#endif /* USE_ARRAY_CALLS */
        {
            /* --- The normal call other to a single object --- */

            assign_eval_cost_inl();

            if (arg[0].type == T_OBJECT)
                ob = arg[0].u.ob;
            else /* it's a string */
            {
                ob = get_object(arg[0].u.str);
                if (ob == NULL)
                    ERRORF(("call_other() failed: can't get object '%s'\n"
                           , get_txt(arg[0].u.str)));
            }

            b_use_default =    (instruction != F_CALL_DIRECT)
                            && (ob != master_ob);

            /* Traceing, if necessary */
            if (TRACEP(TRACE_CALL_OTHER) && TRACE_IS_INTERACTIVE())
            {
                if (!++traceing_recursion)
                {
                    do_trace("Call other ", get_txt(arg[1].u.str), "\n");
                }
                traceing_recursion--;
            }

            /* Call the function with the remaining args on the stack.
             */
            if (!int_apply(arg[1].u.str, ob, num_arg-2, MY_FALSE, b_use_default))
            {
                /* Function not found */
                if (b_use_default) /* int_apply() removed the args */
                    sp -= num_arg-2;
                else
                    pop_n_elems(num_arg-2);
                pop_n_elems(2);
                push_number(sp, 0);
                break;
            }
            sp -= num_arg - 3;

            /* The result of the function call is on the stack. But so
             * is the function name and object that was called.
             * These have to be removed.
             */
            arg = sp;           /* Remember where the function call result is */
            free_string_svalue(--sp);
            free_svalue(--sp);  /* Remove old arguments to call_other */
            *sp = *arg;         /* Re-insert function result */
        }
#ifdef USE_ARRAY_CALLS
        else
        {
            /* --- The other call other to an array of objects --- */

            svalue_t *svp;
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
                    ERROR("Out of memory.\n");
                for (svp = arg->u.vec->item, to = vec->item
                    ; size != 0
                    ; size--, svp++, to++)
                    assign_svalue_no_free(to, svp);
                free_array(arg->u.vec);
                arg->u.vec = vec; /* adopts the reference */
            }

            /* Now loop over the array of objects and call the function
             * in each of it. For that, the arguments are duly replicated
             * for every call.
             */
            size = VEC_SIZE(arg->u.vec);
            svp = arg->u.vec->item;
            for ( ; size != 0; size--, svp++)
            {
                int i;

                assign_eval_cost_inl();
                inter_sp = sp; /* Might be clobbered from previous loop */

                if (svp->type == T_OBJECT)
                    ob = svp->u.ob;
                else if (svp->type == T_STRING)
                {
                    ob = get_object(svp->u.str);
                    if (ob == NULL)
                    {
                        ERRORF(("call_other() failed: can't get object '%s'\n"
                               , get_txt(svp->u.str)));
                        /* NOTREACHED */
                        continue;
                    }
                }
                else if (svp->type == T_NUMBER && svp->u.number == 0)
                {
                    free_svalue(svp);
                    put_number(svp, 0);
                    continue;
                }
                else
                    ERRORF(("Bad arg for call_other() at index %"PRIdMPINT": "
                            "got %s, expected string/object\n"
                           , (mp_int)(svp - arg->u.vec->item)
                           , typename(svp->type)
                           ));

                /* Destructed objects yield 0 */
                if (ob->flags & O_DESTRUCTED)
                {
                    free_svalue(svp);
                    put_number(svp, 0);
                    continue;
                }

                b_use_default =    (instruction != F_CALL_DIRECT)
                                && (ob != master_ob);

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
                for (i = 2; i < num_arg; i++)
                    assign_svalue_no_free(++sp, arg+i);

                /* Call the function with the remaining args on the stack.
                 */
                inter_sp = sp; /* update to new setting */
                if (!int_apply(arg[1].u.str, ob, num_arg-2, MY_FALSE, b_use_default))
                {
                    /* Function not found, Assign 0 as result.
                     */
                    if (b_use_default) /* int_apply() removed the args */
                        sp -= num_arg-2;
                    else
                        pop_n_elems(num_arg-2);
                    free_svalue(svp);
                    put_number(svp, 0);
                }
                else
                {
                    /* Function found - assign the result from the stack */
                    sp -= num_arg-3;
                    free_svalue(svp);
                    transfer_svalue_no_free(svp, sp--);
                }
            } /* for (objects in array) */

            /* Remove the original function call arguments from the stack.
             */
            pop_n_elems(num_arg-2);

            /* Calls complete, left on the stack are now the function name
             * and, in arg, the final result.
             */
            free_string_svalue(sp); sp--;
        }
#endif /* USE_ARRAY_CALLS */

        break;
    }

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
         *   void swap(object obj)
         *
         * Swap out an object. This efun is only used for system internal
         * debugging and can cause a crash.
         */

        object_t *ob;

        /* Test the arguments */
        if (sp->type != T_OBJECT)
            RAISE_ARG_ERROR(1, TF_OBJECT, sp->type);

        ob = sp->u.ob;
        if (ob != current_object
         && !(ob->flags & O_DESTRUCTED)
          ) /* should also check csp */
        {
            if (!O_PROG_SWAPPED(ob))
                (void)swap_program(ob);
            if (!O_VAR_SWAPPED(ob))
                (void)swap_variables(ob);
        }
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
        string_t *object_name = NULL;
        debug_message("%s Received profiling signal, evaluation time > %ld.%06lds\n",
                      ts, (long)profiling_timevalue.tv_sec, (long)profiling_timevalue.tv_usec);
        printf("%s Received profiling signal, evaluation time > %ld.%06lds\n",
                      ts, (long)profiling_timevalue.tv_sec, (long)profiling_timevalue.tv_usec);
        // dump stack trace and continue execution
        object_name = dump_trace(MY_FALSE, NULL);
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
          , Bool b_ign_prot, Bool allowRefs)

/* The low-level implementation of function calls.
 *
 * Call function <fun> in <ob>ject with <num_arg> arguments pushed
 * onto the stack (<inter_sp> points to the last one). static and protected
 * functions can't be called from the outside unless <b_ign_prot> is true.
 * apply_low() takes care of calling shadows where necessary.
 *
 * If <allowRefs> is TRUE, references may be passed as extended varargs
 * ('(varargs mixed *)'). Currently this is used only for simul efuns.
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
           */
          && (   !(cache[ix].flags & (TYPE_MOD_STATIC|TYPE_MOD_PROTECTED)) /* -> neither static nor protected */
              || b_ign_prot
              || (   !(cache[ix].flags & TYPE_MOD_PROTECTED)
                  && current_object == ob
                 ) /* --> static but not protected, and caller is owner */
             )
           )
        {
            /* the cache will tell us in wich program the function is, and
             * where.
             */
            fun_hdr_p funstart;
            
            // check for deprecated functions before pushing a new control stack frame.
            if (cache[ix].flags & TYPE_MOD_DEPRECATED)
                warnf("Callother to deprecated function \'%s\' in object %s (%s).\n",
                      get_txt(fun), get_txt(ob->name), get_txt(ob->prog->name));
            
#ifdef USE_NEW_INLINES
            push_control_stack(inter_sp, inter_pc, inter_fp, inter_context);
#else
            push_control_stack(inter_sp, inter_pc, inter_fp);
#endif /* USE_NEW_INLINES */
            csp->ob = current_object;
            csp->prev_ob = previous_ob;
            csp->num_local_variables = num_arg;
            csp->funstart = funstart = cache[ix].funstart;
            current_prog = cache[ix].progp;
            current_strings = current_prog->strings;
            function_index_offset = cache[ix].function_index_offset;
#ifdef DEBUG
            if (!ob->variables && cache[ix].variable_index_offset)
                fatal("%s Fatal: apply (cached) for object %p '%s' "
                      "w/o variables, but offset %d\n"
                     , time_stamp(), ob, get_txt(ob->name)
                     , cache[ix].variable_index_offset);
#endif
            current_variables = ob->variables;
            if (current_variables)
                current_variables += cache[ix].variable_index_offset;
            inter_sp = setup_new_frame2(funstart, inter_sp, allowRefs, MY_FALSE);
                        
            // check argument types
            check_function_args(FUNCTION_INDEX(funstart), cache[ix].progp, funstart);
            
            previous_ob = current_object;
            current_object = ob;
            save_csp = csp;
            eval_instruction(FUNCTION_CODE(funstart), inter_sp);
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
                fun_hdr_p funstart;

                // check for deprecated functions before pushing a new control stack frame.
                if (progp->functions[fx] & TYPE_MOD_DEPRECATED)
                    warnf("Callother to deprecated function \'%s\' in object %s (%s).\n",
                          get_txt(fun), get_txt(ob->name), get_txt(ob->prog->name));
                
#ifdef USE_NEW_INLINES
                push_control_stack(inter_sp, inter_pc, inter_fp, inter_context);
#else
                push_control_stack(inter_sp, inter_pc, inter_fp);
#endif /* USE_NEW_INLINES */
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
                flags = setup_new_frame1(fx, 0, 0);
                
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
                cache[ix].flags = progp->functions[fx]
                                  & (TYPE_MOD_STATIC|TYPE_MOD_PROTECTED|TYPE_MOD_DEPRECATED);

                /* Static functions may not be called from outside,
                 * Protected functions not even from the inside.
                 */
                if (0 != (cache[ix].flags & (TYPE_MOD_STATIC|TYPE_MOD_PROTECTED))
                  && (   (cache[ix].flags & TYPE_MOD_PROTECTED)
                      || current_object != ob)
                  && !b_ign_prot
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
                inter_sp = setup_new_frame2(funstart, inter_sp, allowRefs, MY_FALSE);
                                
                // check argument types
                check_function_args(fx, progp, funstart);

                previous_ob = current_object;
                current_object = ob;
                save_csp = csp;
                eval_instruction(FUNCTION_CODE(funstart), inter_sp);
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
    if (apply_low(fun, ob, num_arg, b_ign_prot, MY_FALSE))
        return APPLY_FOUND;

    if (b_use_default)
    {
        /* Check if there is a hook */
        svalue_t * hook = driver_hook + H_DEFAULT_METHOD;

        if (hook->type == T_STRING || hook->type == T_CLOSURE)
        {
            /* We got a default method hook.
             * Now we have to rearrange the stack contents to
             * make space for three more values.
             */
            svalue_t result;
            svalue_t * argp;
            int num_extra = (hook->type == T_STRING) ? 2 : 3;
            int i, rc;

            result = const0;

            argp = inter_sp - num_arg + 1;
            for (i = 0; i < num_arg; i++)
                inter_sp[-i+num_extra] = inter_sp[-i];
            inter_sp += num_extra;

            /* Add the three new arguments: &result, ob, fun
             * to the arguments on the stack.
             */
            argp[0].type = T_LVALUE;
            argp[0].u.lvalue = &result;
            if (hook->type == T_CLOSURE)
            {
                put_ref_object(argp+1, ob, "int_apply");
                put_ref_string(argp+2, fun);
            }
            else
                put_ref_string(argp+1, fun);

            /* Call the function */
            if (hook->type == T_STRING)
            {
                rc = apply_low(hook->u.str, ob, num_arg+num_extra, b_ign_prot, MY_TRUE);
            }
            else /* hook->type == T_CLOSURE */
            {
                int_call_lambda(hook, num_arg+num_extra, MY_TRUE, MY_TRUE);
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
                free_svalue(&result);
                rc = APPLY_NOT_FOUND;
            }
            else
            {
                /* Default method found and executed.
                 * Copy the result onto the stack.
                 */
                transfer_svalue(inter_sp, &result);
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
            if (uncaught_error_trace)
            {
                free_array(uncaught_error_trace);
                uncaught_error_trace = NULL;
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
int_call_lambda (svalue_t *lsvp, int num_arg, Bool allowRefs, Bool external)

/* Call the closure <lsvp> with <num_arg> arguments on the stack. On
 * success, the arguments are replaced with the result, else an errorf()
 *
 * If <allowRefs> is TRUE, references may be passed as extended varargs
 * ('(varargs mixed *)'). Currently this is used only for simul efuns.
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
#ifdef USE_NEW_INLINES
    push_control_stack(sp, inter_pc, inter_fp, inter_context);
#else
    push_control_stack(sp, inter_pc, inter_fp);
#endif /* USE_NEW_INLINES */
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
            push_number(inter_sp, 0);
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
            push_number(inter_sp, 0);
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
        check_function_args(FUNCTION_INDEX(csp->funstart), current_prog, csp->funstart);
        if (l->function.lfun.context_size > 0)
            inter_context = l->context;
        if (external)
            eval_instruction(FUNCTION_CODE(csp->funstart), inter_sp);
        else
            inter_pc = FUNCTION_CODE(csp->funstart);

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
            push_number(inter_sp, 0);
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
        fun_hdr_p funstart;

        /* Can't call from a destructed object */
        if (l->ob->flags & O_DESTRUCTED)
        {
            /* inter_sp == sp */
            CLEAN_CSP
            push_number(inter_sp, 0);
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
        funstart = l->function.code + 1;
        csp->funstart = funstart;
        csp->extern_call = external;
        sp = setup_new_frame2(funstart, sp, allowRefs, MY_TRUE);

        current_variables = current_object->variables;
        current_strings = current_prog->strings;
        if (external)
            eval_instruction(FUNCTION_CODE(funstart), sp);
        else
        {
            inter_pc = FUNCTION_CODE(funstart);
            inter_sp = sp;
        }
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
            push_number(inter_sp, 0);
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
                push_number(inter_sp, 0);
                return;
            }

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
                if ( instrs[i].ret_type.typeflags == TYPE_VOID )
                    *p++ = F_RETURN0;
                else
                    *p++ = F_RETURN;

                csp->instruction = i;
                csp->funstart = EFUN_FUNSTART;
                csp->num_local_variables = 0;
                inter_fp = sp - num_arg + 1;
#ifdef USE_NEW_INLINES
                inter_context = NULL;
#endif /* USE_NEW_INLINES */
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
#ifdef USE_NEW_INLINES
    push_control_stack(inter_sp, inter_pc, inter_fp, inter_context);
#else
    push_control_stack(inter_sp, inter_pc, inter_fp);
#endif /* USE_NEW_INLINES */
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
    eval_instruction(FUNCTION_CODE(csp->funstart), inter_sp);
    free_svalue(inter_sp--);  /* Throw away the returned result */
} /* call_function() */

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
            memsafe(*name = new_mstring(buf), strlen(buf), "instruction name");
        }
        else
            *name = ref_mstring(STR_EFUN_CLOSURE);

        return 0;
    }

    if (current_prog)
    {
        if (csp->funstart < current_prog->program
         || csp->funstart > PROGRAM_END(*current_prog))
        {
            static char name_buffer[24];
            string_t * location, *tmp;
            lambda_t * l;

            sprintf(name_buffer, "<lambda 0x%6p>", csp->funstart);
            memsafe(*name = new_mstring(name_buffer), strlen(name_buffer)
                   , "lambda name");
            /* Find the beginning of the lambda structure.*/
            l = (lambda_t *)( (PTRTYPE)(csp->funstart - 1) 
                             - offsetof(lambda_t, function.code));

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
 * efun debug_info().
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

            if (pc2 - FUNCTION_CODE(p->funstart) < 1)
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
                    memsafe(tmp = new_mstring(iname), strlen(iname)
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
                           , (long)(FUNCTION_FROM_CODE(dump_pc) - p[0].funstart)
                           );
            if (rvec)
            {
                NEW_ENTRY(entry, TRACE_TYPE_LAMBDA, ob->prog->name, dump_eval_cost);
                put_number(entry->vec->item+TRACE_NAME, (p_int)p[0].funstart);
                PUT_LOC(entry, (FUNCTION_FROM_CODE(dump_pc) - p[0].funstart));
            }
            continue;
        }

        /* Nothing of the above: a normal program */
        if (file)
            free_mstring(file);
        line = get_line_number(dump_pc, prog, &file);
        memcpy(&name, FUNCTION_NAMEP(p[0].funstart), sizeof name);

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
dump_trace (Bool how, vector_t ** rvec)

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
 * efun debug_info().
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

    fname = check_valid_path(fname, current_object, STR_OPCDUMP, MY_TRUE);
    if (!fname)
        return MY_FALSE;
    f = fopen(get_txt(fname), "w");
    free_mstring(fname);
    if (!f)
        return MY_FALSE;
    FCOUNT_WRITE(fname);


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
        memsafe(s = new_mstring(str), strlen(str), "copy of instruction name");
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
        if ( (num_values = EXTRACT_UCHAR(l->function.code)) == 0xff)
            num_values = svp[-0x100].u.number;
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
            p->u.vec->extra_ref = 0;
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
    count_extra_ref_in_vector(&last_indexing_protector, 1);
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
svalue_t *
v_apply (svalue_t *sp, int num_arg)

/* EFUN apply()
 *
 *     mixed apply(mixed|closure cl, ...)
 *
 * Call the closure <cl> and pass it all the extra arguments
 * given in the call. If the last argument is an array, it
 * is flattened, ie. passed as a bunch of single arguments.
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

    if (sp->type == T_POINTER)
    {
        /* The last argument is an array: flatten it */

        vector_t *vec;  /* the array */
        svalue_t *svp;  /* pointer into the array */
        long i;              /* (remaining) vector size */

        vec = sp->u.vec;
        i = (long)VEC_SIZE(vec);
        num_arg += i - 1;

        /* Check if the target closure can handle
         * all the arguments without overflowing the stack.
         */
        switch( (sp - num_arg + i)->x.closure_type )
        {
        default:
            if ((sp - num_arg + i)->x.closure_type >= 0)
                errorf("Uncallable closure in apply().\n");
            /* else: operator/sefun/efun closure: FALLTHROUGH */
        case CLOSURE_LFUN:
        case CLOSURE_LAMBDA:
        case CLOSURE_BOUND_LAMBDA:
            if (num_arg + (sp - VALUE_STACK) < EVALUATOR_STACK_SIZE)
                break;
            errorf("VM Stack overflow: %zu too high.\n",
                 (size_t)(num_arg + (sp - VALUE_STACK) - EVALUATOR_STACK_SIZE) );
            break;
        }

        /* Push the array elements onto the stack, overwriting the
         * array value itself.
         */
        if (deref_array(vec))
        {
            for (svp = vec->item; --i >= 0; )
            {
                if (destructed_object_ref(svp))
                {
                    put_number(sp, 0);
                    sp++;
                    svp++;
                }
                else
                    assign_svalue_no_free(sp++, svp++);
            }
        }
        else
        {
            /* The array will be freed, so use a faster function */
            for (svp = vec->item; --i >= 0; ) {
                if (destructed_object_ref(svp))
                {
                    put_number(sp, 0);
                    sp++;
                    svp++;
                }
                else
                    transfer_svalue_no_free(sp++, svp++);
            }
            free_empty_vector(vec);
        }

        sp--; /* undo the last extraneous sp++ */
    }

    /* Prepare to call the closure */

    args = sp -num_arg +1;

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
int_call_resolved (Bool b_use_default, svalue_t *sp, int num_arg)

/* EFUN call_resolved(), call_direct_resolved()
 *
 *   int call_resolved(mixed & result, object ob, string func, ...)
 *   int call_direct_resolved(mixed & result, object ob, string func, ...)
 *
 * Similar to call_other(_direct)(). If ob->func() is defined and publicly
 * accessible, any of the optional extra arguments are passed to
 * ob->func(...). The result of that function call is stored in
 * result, which must be passed by reference.
 *
 * If the current object is already destructed, or the ob does not
 * exist, or ob does not define a public accessible function named
 * func, call_direct_resolved() returns 0 as failure code, else 1 for
 * success.
 *
 * If the current object is already destructed, or the ob does not
 * exist, or ob does not define a public accessible function named
 * func and no default method is available, call_resolved() returns 0.
 * If the call succeeded, the efun returns 1; if the call succeeded
 * through a default method, the efun returns -1.
 *
 * ob can also be a file_name. If a string is passed for ob, and
 * no object with that name does exist, an error occurs.
 */

{
    svalue_t *arg;
    object_t *ob;
    int rc;

    arg = sp - num_arg + 1;

    /* Test the arguments */
    if (arg[1].type == T_NUMBER)
        ob = NULL;
    else if (arg[1].type == T_OBJECT)
        ob = arg[1].u.ob;
    else /* it's a string */
    {
        ob = get_object(arg[1].u.str);
        if (!ob)
            errorf("call_resolved() failed: can't get object '%s'.\n"
                 , get_txt(arg[1].u.str));
    }

    /* No external calls may be done when this object is
     * destructed.
     * Similar, don't do calls if the target object is destructed.
     */
    if (current_object->flags & O_DESTRUCTED
     || NULL == ob)
    {
        sp = _pop_n_elems(num_arg, sp);
        push_number(sp, 0);
        inter_sp = sp;
        warnf("Call from destructed object '%s' ignored.\n"
             , get_txt(current_object->name));
        return sp;
    }

    /* Handle traceing. */
    if (TRACEP(TRACE_CALL_OTHER) && TRACE_IS_INTERACTIVE())
    {
        if (!++traceing_recursion)
        {
            inter_sp = sp;
            do_trace("Call other ", get_txt(arg[2].u.str), "\n");
        }
        traceing_recursion--;
    }

    /* Send the remaining arguments to the function.
     */
    if (ob == master_ob)
        b_use_default = MY_FALSE;
    rc = int_apply(arg[2].u.str, ob, num_arg-3, MY_FALSE, b_use_default);
    if (rc == APPLY_NOT_FOUND)
    {
        /* Function not found */
        if (b_use_default)
            sp -= num_arg-3;
        else
            sp = _pop_n_elems(num_arg-3, sp);
        sp = _pop_n_elems(2, sp);
        free_svalue(sp);
        put_number(sp, 0);
        return sp;
    }

    /* The result of the function call is on the stack. But, so
     * is the function name and object that was called.
     * These have to be removed.
     */
    sp = inter_sp;
    transfer_svalue(arg, sp--);  /* Copy the function call result */
    sp = _pop_n_elems(2, sp);     /* Remove old arguments to call_solved */
    free_svalue(sp);             /* Free the lvalue */
    put_number(sp, rc == APPLY_FOUND ? 1 : -1);

    return sp;
} /* f_call_resolved() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_call_resolved (svalue_t *sp, int num_arg)

/* EFUN call_resolved()
 *
 * This is just a wrapper around the real implementation.
 */

{
    return int_call_resolved(MY_TRUE, sp, num_arg);
} /* v_call_resolved() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_call_direct_resolved (svalue_t *sp, int num_arg)

/* EFUN call_direct_resolved()
 *
 * This is just a wrapper around the real implementation.
 */

{
    return int_call_resolved(MY_FALSE, sp, num_arg);
} /* v_call_direct_resolved() */

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
