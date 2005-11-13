/*---------------------------------------------------------------------------
 * Gamedriver: Bytecode Interpreter
 *
 *---------------------------------------------------------------------------
 * This module implements the bytecode interpreter for the compiled LPC
 * programs. The machine is implemented as a stackmachine with separate
 * stacks for values and control.
 *
 * See also 'exec.h' for the details of program storage, and 'datatypes.h'
 * for the details of value storage.
 *
 * --- Evaluator Stack ---
 *
 *    The evaluation stack is an array of 'struct svalue's (see datatypes.h
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
 *    start_of_stack  -----
 *
 *    The interpreter assumes that there are no destructed objects
 *    on the stack - to aid in this, the functions remove_object_from_stack()
 *    and check_for_destr() replace destructed objects by value 0.
 *
 *
 * --- Control Stack ---
 *
 *    During nested function calls, the return information to the higher
 *    functions are stored on the control stack.
 *
 *    One particularity about the current implementation is that every
 *    inter-object call (ie. every 'extern_call') constitutes in a recursive
 *    call to eval_instruction().
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
 *    The _CATCH context differs from the others in that it allows the
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
 *    256 instructions, the less often used instructions are grouped into
 *    'xefuns'+'xcodes', 'tefuns'+'tcodes'  and 'vefuns'+'tcodes', and the
 *    byte defining for the instruction within its group is prefixed by
 *    a byte defining the group: F_ESCAPE for xefuns, F_TEFUN for tefuns,
 *    and F_VEFUN for vefuns.
 *    
 *    Like normal machine instructions and efuns, xefuns are implemented as
 *    cases in a giant switch() instruction, while the implementations of
 *    tefuns/vefuns are called via function tables (tefuns take a fixed
 *    number of arguments, vefuns a variable number).
 *
 *    Implementationwise, every machine instruction, efun or else, is assigned
 *    a unique number and a preprocessor symbol F_<name>. The relation between
 *    the assigned number and the bytecode is linear: normal instructions and
 *    efuns occupy the number range <offset>..<offset>+255, xefuns the range
 *    <offset>+256..<offset>+383, tefuns the range <offset>+384..<offset>+511,
 *    and vefuns the range from <offset>+512 . <offset> is currently 256, this
 *    allows the lexer to use the machine instruction numbers as an extension
 *    of the charset to encode references to efuns directly.
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
 */

/*-------------------------------------------------------------------------*/

#include "driver.h"

#include "my-alloca.h"
#include "my-rusage.h"
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <setjmp.h>
#include <ctype.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef MARK
#include <prof.h>
#endif

#ifdef HAVE_CRYPT_H
     /* solaris defines crypt() here */
#    include <crypt.h>
#endif
#if !defined(HAVE_CRYPT) && defined(HAVE__CRYPT)
#    define crypt(pass, salt) _crypt(pass, salt)
#endif

#define INTERPRET
#define USES_SVALUE_STRLEN
#include "interpret.h"
#include "array.h"
#include "backend.h"
#include "call_out.h"
#include "closure.h"
#include "comm.h"
#include "ed.h"
#include "efuns.h"
#include "exec.h"
#include "gcollect.h"
#include "heartbeat.h"
#include "instrs.h"
#include "lex.h"
#include "main.h"
#include "mapping.h"
#include "object.h"
#include "otable.h"
#include "parse.h"
#include "prolang.h"
#include "ptrtable.h"
#include "random.h"
#include "sent.h"
#include "simulate.h"
#include "simul_efun.h"
#include "stralloc.h"
#include "smalloc.h"
#include "sprintf.h"
#include "swap.h"
#include "switch.h"
#include "wiz_list.h"



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
    struct object        * save_command_giver;
    struct svalue        * save_sp;
      /* The saved global values
       */
};

/* --- struct cache: one entry of the apply cache
 *
 * Every entry in the apply cache holds information about a function
 * call, both for functions found and not found.
 */

struct cache
{
    char *name;
      /* The name of the cached function, shared for existing functions,
       * allocated if the object does not have the function.
       */
    struct program *progp;
      /* The pointer to the program code of the function, or NULL if the
       * object does not implement the function.
       */
    int id;
      /* The id_number of the program. */

    /* TODO: funflags */ uint32 flags;
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

/* --- struct mvf_info: structure used by m_values()/unmkmapping() ---
 *
 * This structure is passed by reference to the filter functions used
 * by the two efuns and passes information from one filter call to the
 * next.
 */
struct mvf_info
{
    struct svalue * svp;
      /* m_values_filter: Pointer to next result vector entry
       * m_unmake_filter: Pointer to result array of svalues
       */
    int             num;
      /* m_values_filter: Column to retrieve.
       * m_unmake_filter: Next row to retrieve
       */
    int             width;
      /* m_unmake_filter: width of the mapping
       */
};

/*-------------------------------------------------------------------------*/
/* Macros */

#define ERRORF(s) {inter_pc = pc; inter_sp = sp; error s ;}
#define ERROR(s) ERRORF((s))
  /* ERRORF((...)) acts like error(...), except that first the local pc and sp
   * are copied into the global variables.
   * ERROR() is an easier to type form of ERRORF() when your error message
   * is just one string. It will be redefined below for the tabled
   * efuns.
   */

#define FATALF(s) {inter_pc = pc; inter_sp = sp; fatal s ;}
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

/*-------------------------------------------------------------------------*/
/* Tracing */

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

/* The possible tracing options ('trace_level'), encoded as bitflags.
 * These values are mirrored in mudlib/sys/trace.h .
 */
#define TRACE_CALL          1  /* Trace all lfun calls */
#define TRACE_CALL_OTHER    2  /* Trace inter-object calls */
#define TRACE_RETURN        4  /* Trace function returns */
#define TRACE_ARGS          8  /* Print function arguments and results */
#define TRACE_EXEC         16  /* Trace all executed instructions */
#define TRACE_HEART_BEAT   32  /* Trace heartbeat code */
#define TRACE_APPLY        64  /* Trace (internal) applies */
#define TRACE_OBJNAME     128  /* Print the object names */

#ifdef TRACE_CODE
/* The buffers for the traced code:
 */

static int              previous_instruction[TOTAL_TRACE_LENGTH];
static int              stack_size[TOTAL_TRACE_LENGTH];
static bytecode_p       previous_pc[TOTAL_TRACE_LENGTH];
static struct program * previous_programs[TOTAL_TRACE_LENGTH];
static struct object  * previous_objects[TOTAL_TRACE_LENGTH];
  /* These arrays, organized as ring buffers, hold the vitals of the
   * last TOTAL_TRACE_LENGTH instructions executed. Yet unused entries
   * are 0 resp. NULL.
   */

static int              last = TOTAL_TRACE_LENGTH - 1;
  /* Index to the last used entry in the ringbuffers above.
   */

#endif

/* --- Macros --- */

#define TRACETST(b) (O_GET_INTERACTIVE(command_giver)->trace_level & (b))

  /* Return TRUE if the any of the tracing options <b> are requested
   * by the interactive user.
   */

#define TRACEP(b) (trace_level & (b) && trace_test(b))
  /* Return TRUE if tracing options <b> are both active in trace_level
   * and requested by the interactive user.
   */

#define TRACEHB \
  ( current_heart_beat == NULL || \
    (O_GET_INTERACTIVE(command_giver)->trace_level & TRACE_HEART_BEAT))

  /* Return TRUE if either the current execution is not caused
   * by a heart beat call, or if heartbeat tracing is allowed.
   */

#define MIN_TRACE_COST (0x100000 + \
        CATCH_RESERVED_COST + 2 * MASTER_RESERVED_COST + 2 * MAX_TRACE)
#define MAX_TRACE_COST ((int32)(0x80000000 - MIN_TRACE_COST))

  /* TRACE_EXEC is implemented by setting eval_cost to a positive value,
   * thus triggering the 'eval cost too big' check on every instruction
   * (slightly faster than adding an explicite check).
   * MAX_TRACE_COST is the evaluation limit in this mode, MIN_TRACE_COST
   * provides a safety margin to both sides, allowing to distinguish a
   * genuine but excessive 'eval too big' (e.g. a function call with
   * thousands of shadows - each shadow adds one tick) from a exec trace.
   */

#define SET_TRACE_EXEC (trace_level & TRACE_EXEC && eval_cost < 0 && \
        (eval_cost += MAX_TRACE_COST, assigned_eval_cost += MAX_TRACE_COST))

  /* If TRACE_EXEC is requested but not active yet (eval ticks negative),
   * add MAX_TRACE_COST to eval_cost to activate this trace mode.
   */

#define TRACE_EXEC_P \
 ( TRACEP(TRACE_EXEC) || \
   (eval_cost -= MAX_TRACE_COST, assigned_eval_cost -= MAX_TRACE_COST, \
   MY_FALSE) )
   /* If TRACE_EXEC is active, deactivate it by subtracting MAX_TRACE_COST
    * from eval_cost.
    */

/*-------------------------------------------------------------------------*/
/* Variables */

/* The virtual machine's registers.
 *
 * While the interpreter is in eval_instruction(), some of the values are
 * kept in local variables for greater speed, with the globals being updated
 * only when necessary.
 * The affected variables are: inter_pc, inter_sp, TODO: which else?
 */

struct svalue *inter_sp;
  /* Points to last valid value on the value stack.
   */
#ifndef SMALLOC_LPC_TRACE
static
#endif
       bytecode_p inter_pc;
  /* Next bytecode to interpret.
   */

static struct svalue *inter_fp;
  /* Framepointer: pointer to first argument.
   */

static bytecode_p *break_sp;
  /* Points to address to branch to at next F_BREAK from within a switch().
   * This is actually a stack of addresses with break_sp pointing to the
   * bottom with the most recent entry. This break stack is stored on
   * the evaluator stack, one address per struct svalue (which incidentally
   * stored in the u.string field), between the functions temporary values
   * and its local variables.
   * TODO: Since this stores an opcode* in a svalue, it should get its
   * TODO:: own union type, and break_sp should be an struct svalue *.
   */

struct program *current_prog;
  /* The current program. This is usually current_object->prog, but can
   * differ when executing an inherited program.
   */

static char **current_strings;
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

struct svalue *current_variables;
  /* Pointer to begin of the current variable block.
   * This is current_object->variables + variable_index_offset for
   * faster access.
   */
  

/* Other Variables */

struct svalue apply_return_value = { T_NUMBER };
  /* This variable holds the result from a call to apply(), transferred
   * properly from the interpreter stack where the called function
   * left it.
   * push_ and pop_apply_value() handle this particular transfer.
   */

static struct svalue start_of_stack[EVALUATOR_STACK_SIZE<<1];
  /* The evaluator stack, sized with (hopefully) enough fudge to handle
   * function arguments and overflows.
   * The stack grows upwards, and <inter_sp> points to last valid entry.
   */
  
struct svalue catch_value = { T_INVALID } ;
  /* Holds the value throw()n from within a catch() while the throw
   * is executed.
   */

static struct control_stack control_stack[MAX_TRACE];
static struct control_stack *csp;
  /* The control stack holds copies of the machine registers for previous
   * function call levels, with <csp> pointing to the last valid
   * entry, describing the last context.
   * This also means that control_stack[0] will have almost no interesting
   * values as it will terminate execution. Especially control_stack[0].prog
   * is NULL to mark the bottom.
   */


#ifdef APPLY_CACHE_STAT
p_int apply_cache_hit  = 0;
p_int apply_cache_miss = 0;
  /* Number of hits and misses in the apply cache.
   */
#endif

static struct cache cache[CACHE_SIZE];
  /* The apply cache.
   */

static struct
  {
    struct svalue v;
      /* The target value:
       *   .v.type: T_CHAR_LVALUE
       *   .v.u.string: the char to modify
       * or
       *   .v.type: T_{POINTER,STRING}_RANGE_LVALUE
       *   .v.u.{vec,string}: the target value holding the range
       *   .index1, .index2, .size: see below
       */
    int index1;  /* First index of the range */
    int index2;  /* Last index of the range plus 1 */
    int size;    /* Current(?) size of the value */
  }
special_lvalue;
  /* When assigning to vector and string ranges or elements, the
   * target information is stored in this structure.
   * TODO: Having one global structure counts as 'ugly'.
   * Used knowingly by: (r)index_lvalue(), transfer_pointer_range(),
   *                    assign_string_range().
   * Used unknowingly by: assign_svalue(), transfer_svalue(),
   *                    add_number_to_svalue(), F_VOID_ASSIGN.
   */

static struct svalue indexing_quickfix = { T_NUMBER };
  /* When indexing arrays and mappings with just one ref, especially
   * for the purpose of getting a lvalue, the indexed item is copied
   * into this variable and indexed from here.
   * Used by operators: push_(r)indexed_lvalue, push_indexed_value,
   *                    push_indexed_map_lvalue.
   * TODO: Rename this variable, or better: devise a nice solution.
   * TODO:: Use the protected_lvalues instead?
   * TODO:: To quote Marion:
   * TODO::     marion says: but this is crude too
   * TODO::     marion blushes.
   * TODO: Is it made sure that this var will be vacated before the
   * TODO:: next use? Otoh, if not it's no problem as the value is
   * TODO:: by definition volatile.
   */

struct svalue last_indexing_protector = { T_NUMBER };
  /* When indexing a protected non-string-lvalue, this variable receives
   * the protecting svalue for the duration of the operation (actually
   * until the next indexing operation (TODO: not nice)).
   * This is necessary because the indexing operation necessarily destroys
   * the protector structure, even though the protection is still needed.
   * Used by: protected_index_lvalue().
   */

#ifdef OPCPROF

#define MAXOPC 0x280
  /* Number of different instructions to trace.
   */

static int opcount[MAXOPC];
  /* Counter array for instruction profiling: each entry counts the
   * usages of one instruction. The full instruction code (not the
   * opcode) is used as index.
   */

#endif

#ifdef DEBUG

static struct program *check_a_lot_ref_counts_search_prog;
  /* Program you developer are especially interested in.
   */

static struct pointer_table *ptable;
  /* check_a_lot_of_ref_counts: the table of structures already
   * visited.
   */

#endif

/*-------------------------------------------------------------------------*/
/* Forward declarations */

static Bool apply_low(char *, struct object *, int, Bool);
static void call_simul_efun(int code, struct object *ob, int num_arg);
#ifdef DEBUG
static void check_extra_ref_in_vector(struct svalue *svp, mp_int num);
#endif

/*-------------------------------------------------------------------------*/

/* Assign the evaluation cost elapsed since the last call to the
 * current_object and it's user's wizlist entry. Then set assigned_eval_cost
 * to the current eval_cost so that later calls can do the same.
 *
 * This function must be called at least whenever the execution leaves
 * one object for another one.
 */
 
#define ASSIGN_EVAL_COST \
    if (current_object->user)\
        current_object->user->cost += eval_cost - assigned_eval_cost;\
    current_object->ticks += eval_cost - assigned_eval_cost;\
    {\
        unsigned long carry = current_object->ticks / 1000000000;\
        if (carry)\
        {\
            current_object->gigaticks += carry;\
            current_object->ticks %= 1000000000;\
        }\
    }\
    assigned_eval_cost = eval_cost;

void assign_eval_cost(void) { ASSIGN_EVAL_COST }

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
    invalid_entry.progp = (struct program *)1;
  
    for (i = 0; i < CACHE_SIZE; i++)
        cache[i] = invalid_entry;
}

/*=========================================================================*/

/*                         S V A L U E S                                   */

/*-------------------------------------------------------------------------*/
/* The following functions handle svalues, ie. the data referenced
 * by the struct svalues. 'Freeing' in this context therefore never means
 * a struct svalue, only the data referenced by it.
 *
 * free_string_svalue(v): free string svalue <v>.
 * free_object_svalue(v): free object svalue <v>.
 * zero_object_svalue(v): replace the object in svalue <v> by number 0.
 * free_svalue(v):        free the svalue <v>.
 * assign_svalue_no_free(to,from): put a copy of <from> into <to>; <to>
 *                        is considered empty.
 * assign_checked_svalue_no_free(to,from,sp,pc): put a copy of <from> into <to>;
 *                        <to> is considered empty, <from> may be destructed
 *                        object.
 * assign_local_svalue_no_free(to,from,sp,pc): put a copy of local var <from>
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
 * static add_number_to_svalue(dest,i): add <i> to lvalue <dest>.
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
    struct svalue v;
      /* .v.type: T_PROTECTED_LVALUE
       * .v.u.lvalue: the protected value
       */
    struct svalue protector; /* protects .v.u.lvalue (or its holder) */
};

/* --- struct protected_char_lvalue: protect a char in a string
 */
struct protected_char_lvalue
{
    struct svalue v;
      /* .v.type: T_PROTECTED_CHAR_LVALUE
       * .v.u.string: points to the char to access
       */
    struct svalue protector; /* protects .lvalue */
    struct svalue *lvalue;   /* the string containing the char */
    char *start;
      /* must be == lvalue->u.string, otherwise the string has been
       * changed and this lvalue is invalid
       */
};

/* --- struct protected_range_lvalue: protect a range in a string or vector
 */
struct protected_range_lvalue {
    struct svalue v;
      /* .v.type: T_PROTECTED_{POINTER,STRING}_RANGE_LVALUE
       * .v.u.{string,vec}: the target value holding the range
       */
    struct svalue protector; /* protects .lvalue */
    struct svalue *lvalue;   /* the value holding the range */
    int index1, index2;      /* first and last index of the range */
    int size;                /* original size of .lvalue */

    /* .v.u.{vec,string} must be == .lvalue->u.{vec,string}, otherwise
     * the target has been changed and the range information (index, size)
     * is no longer valid.
     */
};

/*-------------------------------------------------------------------------*/
/* Forward declarations */

static void transfer_pointer_range(struct svalue *source);
static void transfer_protected_pointer_range(
    struct protected_range_lvalue *dest, struct svalue *source);
static void assign_string_range(struct svalue *source, Bool do_free);
static void assign_protected_string_range(
    struct protected_range_lvalue *dest,struct svalue *source, Bool do_free);

/*-------------------------------------------------------------------------*/
LOCAL_INLINE void
free_string_svalue (struct svalue *v)

/* Free the string svalue <v>; <v> must be of type T_STRING.
 */

{
    switch(v->x.string_type)
    {
    case STRING_MALLOC:
        xfree(v->u.string);
        break;
    case STRING_SHARED:
        free_string(v->u.string);
        break;
    }
}

/*-------------------------------------------------------------------------*/
void
free_object_svalue (struct svalue *v)

/* Free the object svalue <v>; <v> must be of type T_OBJECT.
 */

{
    struct object *ob = v->u.ob;

    free_object(ob, "free_object_svalue");
}

/*-------------------------------------------------------------------------*/
void
zero_object_svalue (struct svalue *v)

/* Change <v> from an object svalue to the svalue-number 0.
 */

{
    struct object *ob = v->u.ob;

    free_object(ob, "zero_object_svalue");
    v->type = T_NUMBER;
    v->u.number = 0;
}

/*-------------------------------------------------------------------------*/
static void
free_protector_svalue (struct svalue *v)

/* Free the svalue <v> which contains a protective reference to a vector
 * or to a mapping.
 */

{
    switch (v->type)
    {
      case T_POINTER:
        free_vector(v->u.vec);
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
void
free_svalue (struct svalue *v)

/* Free the svalue <v>, which may be of any type.
 * Afterwards, the content of <v> is undefined.
 */

{
    switch (v->type)
    {
    case T_STRING:
        switch(v->x.string_type)
        {
        case STRING_MALLOC:
            xfree(v->u.string);
            break;
        case STRING_SHARED:
            free_string(v->u.string);
            break;
        }
        break;

    case T_OBJECT:
      {
        struct object *ob = v->u.ob;
        free_object(ob, "free_svalue");
        break;
      }

    case T_QUOTED_ARRAY:
    case T_POINTER:
        free_vector(v->u.vec);
        break;

    case T_MAPPING:
        free_mapping(v->u.map);
        break;

    case T_SYMBOL:
        free_string(v->u.string);
        break;

    case T_CLOSURE:
        free_closure(v);
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
              if (p->lvalue->type == T_STRING
               && p->lvalue->u.string == p->start)
              {
                  p->lvalue->x.string_type = STRING_MALLOC;
              }
              else
              {
                  xfree(p->start);
              }
              free_protector_svalue(&p->protector);
              xfree(p);
              break;
          }

        case T_PROTECTED_STRING_RANGE_LVALUE:
          {
              struct protected_range_lvalue *p;

              p = v->u.protected_range_lvalue;
              if (p->lvalue->type == T_STRING
               && p->lvalue->u.string == p->v.u.string)
              {
                  p->lvalue->x.string_type = STRING_MALLOC;
              }
              else
              {
                  xfree(p->v.u.string);
              }
              free_protector_svalue(&p->protector);
              xfree(p);
              break;
          }

        case T_PROTECTED_POINTER_RANGE_LVALUE:
          {
              struct protected_range_lvalue *p;

              p = v->u.protected_range_lvalue;
              free_vector(p->v.u.vec);
              free_protector_svalue(&p->protector);
              xfree(p);
              break;
          }

        case T_ERROR_HANDLER:
          {
              struct svalue *p;

              p = v->u.lvalue;
              (*p->u.error_handler)(p);
              break;
          }
        } /* switch (v->u.lvalue->type) */
        break; /* case T_LVALUE */

    }
} /* free_svalue() */

/*-------------------------------------------------------------------------*/
static void
malloced_string_copy (struct svalue *svp, char *str)

/* Assign a copy of the allocated <str>ing to the (empty) svalue <svp>.
 *
 * Called from assign_svalue_no_free().
 */

{
    char *p;

    p = xalloc(malloced_strlen(str));
    if (!p)
    {
        svp->type = T_NUMBER;
        error("Out of memory\n");
    }
    strcpy(p, str);
    svp->u.string = p;
} /* malloced_string_copy() */

/*-------------------------------------------------------------------------*/
LOCAL_INLINE void
assign_svalue_no_free (struct svalue *to, struct svalue *from)

/* Put a duplicate of svalue <from> into svalue <to>, meaning that the original
 * value is either copied when appropriate, or its refcount is increased.
 * <to> is considered empty at the time of call.
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
        switch(from->x.string_type)
        {
        case STRING_MALLOC:        /* No idea to make the string shared */
            malloced_string_copy(to, from->u.string);
            break;
        case STRING_VOLATILE:      /* Good idea to make it shared */
            to->x.string_type = STRING_SHARED;
            if ( !(to->u.string = make_shared_string(from->u.string)) )
            {
                to->type = T_NUMBER;
                error("Out of memory\n");
            }
            break;
        case STRING_SHARED:        /* It already is shared */
            increment_string_ref(from->u.string);
            break;
#ifdef DEBUG
        default:
            fatal("Bad string type %d\n", from->x.string_type);
#endif
        }
        break;
 
    case T_OBJECT:
        add_ref(to->u.ob, "ass to var");
        break;

    case T_QUOTED_ARRAY:
    case T_POINTER:
        to->u.vec->ref++;
        break;

    case T_SYMBOL:
        increment_string_ref(to->u.string);
        break;

    case T_CLOSURE:
        if (CLOSURE_MALLOCED(to->x.closure_type))
            to->u.lambda->ref++;
        else
            add_ref(to->u.ob, "ass to var");
        break;

    case T_MAPPING:
        to->u.map->ref++;
        break;
    }
} /* assign_svalue_no_free() */

/*-------------------------------------------------------------------------*/
static INLINE void
assign_checked_svalue_no_free (struct svalue *to, struct svalue *from
                              , struct svalue *sp, bytecode_p pc
                              )

/* Put a duplicate of svalue <from> into svalue <to>, meaning that the original
 * value is either copied when appropriate, or its refcount is increased.
 * <to> is considered empty at the time of call.
 * <from> may point to a variable or vector element, so it might contain
 * a destructed object. In that case, <to> is set to svalue-number 0.
 *
 * <sp> and <pc> are the current stackpointer and program counter and are
 * needed to update <inter_xx> in case of errors.
 */

{
    switch (from->type)
    {
    case T_STRING:
        switch(from->x.string_type)
        {
        case STRING_MALLOC:        /* No idea to make the string shared */
          {
            char *p;
            char *str;

            p = xalloc(malloced_strlen(str = from->u.string));
            if (!p) {
                to->type = T_NUMBER;
                inter_sp = sp;
                inter_pc = pc;
                error("Out of memory\n");
            }
            (void)strcpy(p, str);
            to->type = T_STRING;
            to->x.string_type = STRING_MALLOC;
            to->u.string = p;
            return;
          }

        case STRING_SHARED:        /* It already is shared */
            increment_string_ref( to->u.string = from->u.string );
            break;
#ifdef DEBUG
        default:
            fatal("Bad string type %d\n", from->x.string_type);
#endif
        }
        to->type = T_STRING;
        to->x.string_type = STRING_SHARED;
        return;

    case T_OBJECT:
      {
        struct object *ob = from->u.ob;
        if ( !(ob->flags & O_DESTRUCTED) ) {
            add_ref(ob, "ass to var");
            break;
        }
        zero_object_svalue(from);
        break;
      }

    case T_QUOTED_ARRAY:
    case T_POINTER:
        from->u.vec->ref++;
        break;

    case T_SYMBOL:
        increment_string_ref(from->u.string);
        break;

    case T_CLOSURE:
        if (CLOSURE_MALLOCED(from->x.closure_type))
            from->u.lambda->ref++;
        else
            add_ref(from->u.ob, "ass to var");
        break;

    case T_MAPPING:
        from->u.map->ref++;
        break;
    }
    *to = *from;
} /* assign_checked_svalue_no_free() */

/*-------------------------------------------------------------------------*/
static INLINE void
assign_local_svalue_no_free ( struct svalue *to, struct svalue *from
                            , struct svalue *sp, bytecode_p pc
                            )

/* Put a duplicate of svalue <from> into svalue <to>, meaning that the original
 * value is either copied when appropriate, or its refcount is increased.
 * <to> is considered empty at the time of call.
 * 
 * <from> is meant to point to a local variable, which might be an arg
 * to the current lfun and may thus contain a VOLATILE string. If that is
 * the case, the string is made shared before assignment.
 * If <from> is a lvalue, the chain is unraveled and the final non-lvalue
 * is assigned. If that value is a destructed object, 0 is assigned.
 *
 * <sp> and <pc> are the current stackpointer and program counter and are
 * needed to update <inter_xx> in case of errors.
 */

{
assign_from_lvalue:
    switch (from->type) {
      case T_STRING:
        switch(from->x.string_type) {
          case STRING_MALLOC:        /* No idea to make the string shared */
          {
            char *p;
            char *str;

            p = xalloc(malloced_strlen(str = from->u.string));
            if (!p) {
                to->type = T_NUMBER;
                inter_sp = sp;
                inter_pc = pc;
                error("Out of memory\n");
            }
            (void)strcpy(p, str);
            to->type = T_STRING;
            to->x.string_type = STRING_MALLOC;
            to->u.string = p;
            return;
          }
          case STRING_SHARED:        /* It already is shared */
            increment_string_ref( to->u.string = from->u.string );
            break;
          case STRING_VOLATILE:
            if ( !(to->u.string = make_shared_string(from->u.string)) ) {
                to->type = T_STRING;
                to->x.string_type = STRING_SHARED;
                increment_string_ref(to->u.string = STR_DEFAULT);
                inter_sp = sp;
                inter_pc = pc;
                error("Out of memory\n");
            }
            break;
#ifdef DEBUG
          default:
            fatal("Bad string type %d\n", from->x.string_type);
#endif
        }
        to->type = T_STRING;
        to->x.string_type = STRING_SHARED;
        return;
      case T_OBJECT:
        add_ref(from->u.ob, "assign_local_lvalue_no_free");
        break;
      case T_QUOTED_ARRAY:
      case T_POINTER:
        from->u.vec->ref++;
        break;
      case T_SYMBOL:
        increment_string_ref(from->u.string);
        break;
      case T_CLOSURE:
        if (CLOSURE_MALLOCED(from->x.closure_type))
            from->u.lambda->ref++;
        else
            add_ref(from->u.ob, "ass to var");
        break;
      case T_MAPPING:
        from->u.map->ref++;
        break;
      case T_LVALUE:
      case T_PROTECTED_LVALUE:
        from = from->u.lvalue;
        if (from->type == T_OBJECT && from->u.ob->flags & O_DESTRUCTED) {
            zero_object_svalue(from);
            break;
        }
        goto assign_from_lvalue;
      case T_PROTECTED_CHAR_LVALUE:
        to->type = T_NUMBER;
        to->u.number = *from->u.string;
        return;
    }
    *to = *from;
} /* assign_local_svalue_no_free() */

/*-------------------------------------------------------------------------*/
static INLINE
void assign_lrvalue_no_free (struct svalue *to, struct svalue *from)

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
        if (to->x.string_type != STRING_SHARED)
        {
            to->x.string_type = STRING_SHARED;
            to->u.string = make_shared_string(from->u.string);
            if (from->x.string_type == STRING_MALLOC)
            {
                xfree(from->u.string);
            }
            *from = *to;
        }
        increment_string_ref(from->u.string);
        break;

    case T_OBJECT:
        add_ref(to->u.ob, "ass to var");
        break;

    case T_QUOTED_ARRAY:
    case T_POINTER:
        to->u.vec->ref++;
        break;

    case T_SYMBOL:
        increment_string_ref(to->u.string);
        break;

    case T_CLOSURE:
        if (CLOSURE_MALLOCED(to->x.closure_type))
            to->u.lambda->ref++;
        else
            add_ref(to->u.ob, "ass to var");
        break;

    case T_MAPPING:
        to->u.map->ref++;
        break;

    case T_LVALUE:
        to->u.lvalue = from;
        break;
    }
} /* assign_lrvalue_no_free() */

/*-------------------------------------------------------------------------*/
void
assign_svalue (struct svalue *dest, struct svalue *v)

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
     * If <dest> is a lvalue, the loop will traverse the lvalue chain until
     * the actual svalue is found.
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
            switch(dest->x.string_type)
            {
            case STRING_MALLOC:
                xfree(dest->u.string);
                break;
            case STRING_SHARED:
                free_string(dest->u.string);
                break;
            }
            break;

        case T_OBJECT:
          {
            struct object *ob = dest->u.ob;
            free_object(ob, "assign_svalue");
            break;
          }

        case T_QUOTED_ARRAY:
        case T_POINTER:
          {
            struct vector *vec = dest->u.vec;
            assign_svalue_no_free(dest, v);
              /* TODO: leaks vec if out of memory */
            free_vector(vec);
            return;
          }

        case T_MAPPING:
          {
            struct mapping *map = dest->u.map;
            assign_svalue_no_free(dest, v); /* leaks map if out of memory */
            free_mapping(map);
            return;
          }

        case T_SYMBOL:
            free_string(dest->u.string);
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
                *dest->u.string = v->u.number;
            return;

        case T_PROTECTED_CHAR_LVALUE:
          {
            struct protected_char_lvalue *p;

            p = (struct protected_char_lvalue *)dest;
            if (p->lvalue->type == T_STRING
             && p->lvalue->u.string == p->start)
            {
                if (v->type == T_NUMBER)
                    *p->v.u.string = v->u.number;
            }
            return;
          }
        
        case T_POINTER_RANGE_LVALUE:
            if (v->type == T_POINTER)
            {
                v->u.vec->ref++; /* transfer_...() will free it once */
                transfer_pointer_range(v);
            }
            return;

        case T_PROTECTED_POINTER_RANGE_LVALUE:
            if (v->type == T_POINTER)
            {
                v->u.vec->ref++; /* transfer_...() will free it once */
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
LOCAL_INLINE void
transfer_svalue_no_free (struct svalue *dest, struct svalue *v)

/* Move the value <v> into <dest>. If <v> is an unshared string, it
 * is made shared.
 *
 * <dest> is assumed to be invalid before the call, <v> is invalid after.
 */

{
    /* If <v> is a string, share it */
    if (v->type == T_STRING && v->x.string_type == STRING_VOLATILE)
    {
        dest->type = T_STRING;
        dest->x.string_type = STRING_SHARED;
        if ( !(dest->u.string = make_shared_string(v->u.string)) )
        {
            dest->type = T_NUMBER;
            error("Out of memory\n");
        }
    }
    else /* just copy the data */
    {
        *dest = *v;
    }
} /* transfer_svalue_no_free() */

/*-------------------------------------------------------------------------*/
INLINE static void
transfer_svalue_no_free_spc ( struct svalue *dest, struct svalue *v
                            , struct svalue *sp, bytecode_p pc)

/* Move the value <v> into <dest>. If <v> is a volatile string, it
 * is made shared.
 *
 * <dest> is assumed to be invalid before the call, <v> is invalid after.
 *
 * This function may be called from eval_instruction() and needs to receive
 * the current stackpointer <sp> and the current programcounter <pc>.
 */

{
    if (v->type == T_STRING && v->x.string_type == STRING_VOLATILE)
    {
        dest->type = T_STRING;
        dest->x.string_type = STRING_SHARED;
        if ( !(dest->u.string = make_shared_string(v->u.string)) )
        {
            dest->type = T_NUMBER;
            inter_sp = sp;
            inter_pc = pc;
            error("Out of memory\n");
        }
    }
    else
    {
        *dest = *v;
    }
} /* transfer_svalue_no_free_spc() */

/*-------------------------------------------------------------------------*/
void
transfer_svalue (struct svalue *dest, struct svalue *v)

/* Move svalue <v> into svalue <dest>.
 *
 * <dest> is considered a valid svalue and therefore freed before the
 * assignment. <v> will be invalid after the call.
 *
 * If <dest> is a lvalue, <v> will be moved into the svalue referenced
 * to by <dest>. If <v> is a volatile string, it is made shared.
 *
 * F_VOID_ASSIGN uses a modified copy of this code (for speed reasons).
 */

{

    /* Free the <dest> svalue.
     * If <dest> is a lvalue, the loop will traverse the lvalue chain until
     * the actual svalue is found.
     * If a T_xxx_LVALUE is found, the transfer will be done here
     * immediately.
     */
  
    for(;;)
    {
        switch (dest->type)
        {
        case T_LVALUE:
        case T_PROTECTED_LVALUE:
            dest = dest->u.lvalue;
            continue;

        case T_STRING:
            switch(dest->x.string_type)
            {
            case STRING_MALLOC:
                xfree(dest->u.string);
                break;
            case STRING_SHARED:
                free_string(dest->u.string);
                break;
            }
            break;

        case T_OBJECT:
          {
            struct object *ob = dest->u.ob;
            free_object(ob, "transfer_svalue");
            break;
          }

        case T_QUOTED_ARRAY:
        case T_POINTER:
            free_vector(dest->u.vec);
            break;
            
        case T_SYMBOL:
            free_string(dest->u.string);
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
                *dest->u.string = v->u.number;
            }
            else
                free_svalue(v);
            return;

        case T_PROTECTED_CHAR_LVALUE:
          {
            struct protected_char_lvalue *p;

            p = (struct protected_char_lvalue *)dest;
            if (p->lvalue->type == T_STRING
             && p->lvalue->u.string == p->start)
            {
                if (v->type == T_NUMBER)
                {
                    *p->v.u.string = v->u.number;
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
 
    /* Transfer the value, making volatile strings shared */
    if (v->type == T_STRING && v->x.string_type == STRING_VOLATILE)
    {
        dest->type = T_STRING;
        dest->x.string_type = STRING_SHARED;
        dest->u.string = make_shared_string(v->u.string);
        /* TODO: Nanue? No check for dest->u.string != NULL? 
         * TODO:: Like in transfer_svalue_no_free_spc()?
         */
    }
    else
    {
        *dest = *v;
    }
} /* transfer_svalue() */

/*-------------------------------------------------------------------------*/
static void
transfer_pointer_range (struct svalue *source)

/* Transfer the vector <source> to the vector range defined by
 * <special_lvalue>, modifying the target vector in special_lvalue
 * accordingly. <source> is freed once in the call.
 *
 * If <source> is not a vector, it is just freed.
 */

{
    if (source->type == T_POINTER)
    {
        struct vector *sv;   /* Source vector (from source) */
        struct vector *dv;   /* Destination vector (from special_lvalue) */
        struct vector *rv;   /* Result vector */
        int dsize;           /* Size of destination vector */
        int ssize;           /* Size of source vector */
        int index1, index2;  /* First and last index of destination range */
        int i;

        /* Setup the variables */
        dsize = special_lvalue.size;
        index1 = special_lvalue.index1;
        index2 = special_lvalue.index2;
        dv = special_lvalue.v.u.lvalue->u.vec;
        sv = source->u.vec;
        ssize = VEC_SIZE(sv);

        if (ssize + index1 - index2 == 0)
        {
            /* <source> fits exactly into the target range */
          
            struct svalue *s, *d;  /* Copy source and destination */

            s = sv->item;
            d = dv->item + index1;

            dv->ref++; /* protect against recursive refs during the copy */
            
            /* If there is just one ref to the source, use the faster
             * transfer instead of the slow assign for the copy.
             */
            if (!--sv->ref)
            {
                sv->ref++;
                for (i = ssize; --i >= 0; )
                {
                    transfer_svalue(d++, s++);
                }
                free_empty_vector(sv);
            }
            else
            {
                for (i = ssize; --i >= 0; )
                {
                    assign_svalue(d++, s++);
                }

                /* The if() above effectively did the 'free_svalue(source)' */
            }

            free_vector(dv); /* Undo the ref++ above */
        }
        else
        {
            /* Create a new vector */
          
            struct svalue *s, *d; /* Copy source and destination */

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
            free_vector(sv);

            s = dv->item + index2;
            for (i = dsize - index2; --i >= 0; )
            {
                assign_svalue_no_free(d++, s++);
            }

            free_vector(dv); /* this can make the lvalue invalid to use */
        }
    }
    else
        /* Not a pointer: just free it */
        free_svalue(source);

} /* transfer_pointer_range() */

/*-------------------------------------------------------------------------*/
static void
transfer_protected_pointer_range ( struct protected_range_lvalue *dest
                                 , struct svalue *source)

/* Transfer the vector <source> to the vector range defined by
 * <dest>, modifying the target vector in <dest>
 * accordingly. <source> is freed once in the call.
 *
 * If <source> is not a vector, it is just freed.
 */

{
    if (source->type == T_POINTER && dest->v.u.vec == dest->lvalue->u.vec)
    {
        struct vector *sv;   /* Source vector (from source) */
        struct vector *dv;   /* Dest vector (from dest) */
        struct vector *rv;   /* Result vector */
        int dsize;           /* Size of the dest vector */
        int ssize;           /* Size of the source vector */
        int index1, index2;  /* Target range indices */
        int i;

        /* Setup the variables */
        dsize = dest->size;
        index1 = dest->index1;
        index2 = dest->index2;
        dv = dest->v.u.vec;
        sv = source->u.vec;
        ssize = VEC_SIZE(sv);
        
        if (ssize + index1 - index2 == 0)
        {
            /* <source> fits exactly into the target range */
          
            struct svalue *s, *d; /* Copy source and destination */

            s = sv->item;
            d = dv->item + index1;
 
            /* If there is just one ref to the source, use the faster
             * transfer instead of the slow assign for the copy.
             */
            if (!--sv->ref)
            {
                sv->ref++;
                for (i = ssize; --i >= 0; )
                {
                    transfer_svalue(d++, s++);
                }
                free_empty_vector(sv);
            }
            else
            {
                for (i = ssize; --i >= 0; )
                {
                    assign_svalue(d++, s++);
                }
                /* The if() above effectively did the 'free_svalue(source)' */
            }
        }
        else
        {
            /* Create a new vector */
          
            struct svalue *s, *d;  /* Copy source and destination */

            rv = allocate_array(dsize + ssize + index1 - index2);
            dest->lvalue->u.vec = rv;
            
            s = dv->item;
            d = rv->item;
            for (i = index1; --i >= 0; )
            {
                assign_svalue_no_free(d++, s++);
            }
            
            s = sv->item;
            for (i = ssize; --i >= 0; ) {
                assign_svalue_no_free(d++, s++);
            }
            free_vector(sv);
            
            s = dv->item + index2;
            for (i = dsize - index2; --i >= 0; )
            {
                assign_svalue_no_free(d++, s++);
            }
            
            free_vector(dv); /* this can make the lvalue invalid to use */
        }
    }
    else
        /* Not a pointer: just free it */
        free_svalue(source);

} /* transfer_protected_pointer_range() */

/*-------------------------------------------------------------------------*/
static void
assign_string_range (struct svalue *source, Bool do_free)

/* Transfer the string <source> to the string range defined by
 * <special_lvalue>, modifying the target string in special_lvalue
 * accordingly. If <do_free> is TRUE, <source> is freed once in the call.
 *
 * If <source> is not a string, it is just freed resp. ignored.
 */

{
    if (source->type == T_STRING)
    {
        struct svalue *dsvp;  /* destination svalue (from special_lvalue) */
        char *ds;             /* destination string (from dsvp) */
        char *ss;             /* source string (from source) */
        char *rs;             /* result string */
        int dsize;            /* size of destination string */
        int ssize;            /* size of source string */
        int index1, index2;   /* range indices */

        /* Set variables */
        dsize = special_lvalue.size;
        index1 = special_lvalue.index1;
        index2 = special_lvalue.index2;
        dsvp = special_lvalue.v.u.lvalue;
        ds = dsvp->u.string;
        ss = source->u.string;
        ssize = svalue_strlen(source);

        /* Create the new string */
        rs = xalloc(dsize + ssize + index1 - index2 + 1);
        if (!rs)
        {
            /* We don't pop the stack here --> don't free source */
            error("Out of memory\n");
        }

        if (index1)
            memcpy(rs, ds, index1);
        if (ssize)
            memcpy(rs + index1, ss, ssize);
        strcpy(rs + index1 + ssize, ds + index2);

        /* Assign the new string in place of the old */
        free_string_svalue(dsvp);
        dsvp->x.string_type = STRING_MALLOC;
        dsvp->u.string = rs;

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
                              , struct svalue *source
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
        struct svalue *dsvp;  /* destination value (from dest) */
        char *ss;             /* source string (from source) */
        char *ds;             /* destination string (from dsvp) */
        char *rs;             /* result string */
        int dsize;            /* size of destination string */
        int ssize;            /* size of source string */
        int index1, index2;   /* range indices */

        /* Set variables */
        dsize = dest->size;
        index1 = dest->index1;
        index2 = dest->index2;
        dsvp = dest->lvalue;
        ds = dest->v.u.string;
        
        /* If the lvalue is no longer valid, free it */
        if (dsvp->u.string != ds)
        {
            if (do_free)
            {
                free_svalue(source);
                xfree(dest->v.u.string);
                xfree(dest);
            }
            return;
        }

        /* Create a new string */
        ss = source->u.string;
        ssize = svalue_strlen(source);
        rs = xalloc(dsize + ssize + index1 - index2 + 1);
        if (!rs)
        {
            error("Out of memory\n");
        }

        if (index1)
            memcpy(rs, ds, index1);
        if (ssize)
            memcpy(rs + index1, ss, ssize);
        strcpy(rs + (dest->index2 = index1 + ssize), ds + index2);
        xfree(ds);

        dest->v.u.string = dsvp->u.string = rs;
        if (do_free)
        {
            free_string_svalue(source);
            dest->v.x.string_type = STRING_MALLOC;
            free_protector_svalue(&dest->protector);
            xfree(dest);
        }
    }
    else
    {
        /* Not a string: just free it */
        if (do_free)
        {
            free_svalue(source);
            dest->v.x.string_type = STRING_MALLOC;
            free_protector_svalue(&dest->protector);
            xfree(dest);
        }
    }
} /* transfer_protected_string_range() */

/*-------------------------------------------------------------------------*/
static int
add_number_to_svalue (struct svalue *dest, int i)

/* Add the number <i> to the (PROTECTED_)LVALUE <dest>, and return
 * the sum. If <dest> is of the wrong type, an error is generated.
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
        error("Reference to bad type to ++/--\n");
        return i;
        
    case T_NUMBER:
        return dest->u.number += i;
 
    case T_PROTECTED_LVALUE:
        return add_number_to_svalue(dest, i);

    case T_CHAR_LVALUE:
        return (*dest->u.string) += i;
 
    case T_PROTECTED_CHAR_LVALUE:
      {
        struct protected_char_lvalue *p;

        p = (struct protected_char_lvalue *)dest;
        if (p->lvalue->type == T_STRING
         && p->lvalue->u.string == p->start)
        {
            i = *p->v.u.string += i;
        }
        return i;
      }
    } /* switch() */

    /* NOTREACHED */
}

/*-------------------------------------------------------------------------*/
static struct vector *
inter_add_array (struct vector *q, struct vector **vpp)

/* Append array <q> to array *<vpp>. Both <q> and *<vpp> are freed,
 * the result vector (just one ref) is assigned to *<vpp> and also returned.
 *
 * <inter_sp> is supposed to point at the two vectors and will be decremented
 * by 2.
 */

{
    struct vector *p;       /* The second summand vector */
    mp_int cnt;
    struct vector *r;       /* Result vector */
    struct svalue *s, *d;   /* Pointers for copying: src and dest */
    mp_int p_size, q_size;  /* Sizes of p and q */

    p = *vpp;
    
    inter_sp -= 2;

    /* Out of memory might result in some memory leaks. Better that freeing
     * arrays with 0 ref count, or indigestion in garbage_collection() .
     * It will simply give some more debugging output...
     */

    /* *vpp could be in the summands, thus don't free p / q before
     * assigning.
     * On the other hand, with an uninitialized array, we musn't assign
     * before the copying is done.
     */

    p_size = VEC_SIZE(p);
    q_size = VEC_SIZE(q);
    s = p->item;
    
    /* Allocate the result vector and copy p into it.
     */
    if (!(p->ref-1))
    {
        /* p will be deallocated completely - try to optimize a bit */

#ifdef MALLOC_smalloc
        /* We try to expand the existing memory for p (without moving)
         * instead of allocating a completely new vector.
         * TODO: We overallocate in anticipation of further additions?
         */
        d = malloc_increment_size(p, q_size << 1);
        if ( NULL != d)
        {
            /* We got the additional memory */
            r = p;
            r->ref = 1;
            
            r->user->size_array -= p_size;
            r->user = current_object->user;
            r->user->size_array += p_size + q_size;
            
            if (p_size + q_size > MAX_ARRAY_SIZE)
            {
                /* Oops, overflow - invalidate everythings */
                *vpp = allocate_array(0);
                d = r->item + p_size;
                for (cnt = q_size; --cnt >=0; )
                {
                    d[cnt].type = T_INVALID;
                }
                free_vector(r);
                free_vector(q);
                error("Illegal array size: %ld.\n", p_size + q_size);
            }
        } else
#endif
        /* Just allocate a new vector and memcopy p into it. */
        {
            r = allocate_uninit_array(p_size + q_size);
            p->ref--;
            d = r->item;
            for (cnt = p_size; --cnt >= 0; )
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
        r = allocate_uninit_array(p_size + q_size);
        p->ref--;
        d = r->item;
        for (cnt = p_size; --cnt >= 0; ) {
            assign_checked_svalue_no_free (d++, s++, inter_sp, inter_pc);
        }
    }
    
    /* Add the values from q. Again, try to optimize */
    s = q->item;
    if (!--q->ref)
    {
        for (cnt = q_size; --cnt >= 0; )
        {
            if (s->type == T_OBJECT && s->u.ob->flags & O_DESTRUCTED)
                zero_object_svalue(s);
            *d++ = *s++;
        }
        *vpp = r;
        free_empty_vector(q);
    }
    else
    {
        for (cnt = q_size; --cnt >= 0; ) {
            assign_checked_svalue_no_free (d++, s++, inter_sp, inter_pc);
        }
        *vpp = r;
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
 * _push_object(ob, sp), push_object(ob):
 *     Push a non-destructed object onto the stack.
 * _push_valid_object(ob, sp), push_valid_object(ob):
 *     Push an object onto the stack.
 * _put_object(ob, sp):
 *     Put an object onto the stack.
 * put_object(ob, sp):
 *     Put an object onto the stack.
 * _push_number(n, sp), push_number(n):
 *     Push a number onto the stack.
 * _push_shared_string(p, sp), push_shared_string(p),
 * push_referenced_shared_string(p):
 *     Push a shared string onto the stack.
 * push_string_shared(p):
 *     Share a string and push it onto the stack.
 * _push_string_malloced(p, sp), push_string_malloced(p):
 *     Malloc a copy of a string and push it onto the stack.
 * _push_malloced_string(p, sp), push_malloced_string(p):
 *     Push a malloced string onto the stack.
 * put_malloced_string(p):
 *     Put a malloced string onto the stack.
 * _push_volatile_string(p, sp), push_volatile_string(p):
 *     Push a volatile string onto the stack.
 * push_svalue(v), push_svalue_block(num,v):
 *     Push one or more svalues onto the stack.
 * pop_stack(), _drop_n_elems(n,sp):
 *     Pop (free) elements from the stack.
 * drop_stack():
 *     Drop (remove w/o freeing) an element from the stack.
 * stack_overflow(sp,fp,pc):
 *     Handle a stack overflow.
 * push_vector(v), push_referenced_vector(v):
 *     Push a vector onto the stack.
 * push_referenced_mapping(m):
 *     Push a mapping onto the stack.
 *
 * and as macros only:
 *
 * put_vector(v,sp), put_referenced_vector(v,sp):
 *     Put a vector onto the stack.
 * push_mapping(m,sp):
 *     Push a mapping onto the stack.
 */

/*-------------------------------------------------------------------------*/
INLINE static struct svalue *
_push_object (struct object *ob, struct svalue *sp)

/* Push the object <ob> onto the stack, currently ending at <sp>, and
 * return the new stackpointer.
 * <ob> must not be destructed.
 * The ref to <ob> is incremented.
 */

{
    sp++;
    sp->type = T_OBJECT;
    sp->u.ob = ob;
    add_ref(ob, "push_object");
    return sp;
}

/*-------------------------------------------------------------------------*/
void
push_object (struct object *ob)

/* Push the object <ob> onto the stack defined by <inter_sp>.
 * <ob> must not be destructed.
 * The ref to <ob> is incremented.
 */

{
    inter_sp++;
    inter_sp->type = T_OBJECT;
    inter_sp->u.ob = ob;
    add_ref(ob, "push_object");
}

/*-------------------------------------------------------------------------*/
INLINE static struct svalue *
_push_valid_ob (struct object *ob, struct svalue *sp)

/* Push the object <ob> onto the stack, currently ending at <sp>, and
 * return the new stackpointer.
 * If <ob> is destructed, the number 0 is pushed.
 * The ref to <ob> is incremented if necessary.
 */

{
    sp++;
    if (ob->flags & O_DESTRUCTED)
    {
        sp->type = T_NUMBER;
        sp->u.number = 0;
    } else
    {
        sp->type = T_OBJECT;
        sp->u.ob = ob;
        add_ref(ob, "push_valid_ob");
    }
    return sp;
}

/*-------------------------------------------------------------------------*/
void
push_valid_ob (struct object *ob)

/* Push the object <ob> onto the stack defined by <inter_sp>.
 * If <ob> is destructed, the number 0 is pushed.
 * The ref to <ob> is incremented if necessary.
 */

{
    inter_sp++;
    if (ob->flags & O_DESTRUCTED)
    {
        inter_sp->type = T_NUMBER;
        inter_sp->u.number = 0;
    } else
    {
        inter_sp->type = T_OBJECT;
        inter_sp->u.ob = ob;
        add_ref(ob, "push_valid_ob");
    }
}

/*-------------------------------------------------------------------------*/
static INLINE void
_put_object (struct object *ob, struct svalue *sp)

/* Put the object <ob> into the last stackelement, given by <sp>.
 * <ob> must not be destructed.
 */

{
    sp->type = T_OBJECT;
    sp->u.ob = ob;
    add_ref(ob, "put_object");
}

/*-------------------------------------------------------------------------*/
void
put_object (struct object *ob, struct svalue *sp)

/* Put the object <ob> into the last stackelement, given by <sp>.
 * <ob> must not be destructed.
 */

{
    sp->type = T_OBJECT;
    sp->u.ob = ob;
    add_ref(ob, "put_object");
}

/*-------------------------------------------------------------------------*/
INLINE static struct svalue *
_push_number (p_int n, struct svalue *sp)

/* Push the number <n> onto the stack, currently ending at <sp>, and return
 * the new stackpointer.
 */

{
    sp++;
    sp->type = T_NUMBER;
    sp->u.number = n;
    return sp;
}

/*-------------------------------------------------------------------------*/
void
push_number (p_int n)

/* Push the number <n> onto the stack as defined by <inter_sp>.
 */

{
    inter_sp++;
    inter_sp->type = T_NUMBER;
    inter_sp->u.number = n;
}

/*-------------------------------------------------------------------------*/
static INLINE struct svalue *
_push_shared_string (char *p, struct svalue *sp)

/* Push the shared string <p> onto the stack, currently ending at <sp>,
 * and return the new stackpointer.
 * The refs of <p> are incremented.
 */

{
    sp++;
    sp->type = T_STRING;
    sp->x.string_type = STRING_SHARED;
    increment_string_ref( sp->u.string = p );
    return sp;
}

/*-------------------------------------------------------------------------*/
void
push_shared_string (char *p)

/* Push the shared string <p> onto the stack as defined by <inter_sp>.
 * The refs of <p> are incremented.
 */

{
    inter_sp = _push_shared_string(p, inter_sp);
}

/*-------------------------------------------------------------------------*/
void
push_referenced_shared_string (char *p)

/* Push the shared string <p> onto the stack as defined by <inter_sp>.
 * The refs of <p> are _not_ incremented.
 */

{
    struct svalue *sp = inter_sp;

    sp++;
    sp->type = T_STRING;
    sp->x.string_type = STRING_SHARED;
    sp->u.string = p;
    inter_sp = sp;
}

/*-------------------------------------------------------------------------*/
void
push_string_shared (char *p)

/* Make a shared string of <p> and push it onto the stack as defined
 * by <inter_sp>.
 */

{
    inter_sp++;
    inter_sp->type = T_STRING;
    inter_sp->x.string_type = STRING_SHARED;
    inter_sp->u.string = make_shared_string(p);
}

/*-------------------------------------------------------------------------*/
static struct svalue *
_push_string_malloced (char *p, struct svalue *sp)

/* Malloc a copy of string <p>, push it onto the stack currently ending
 * at <sp>, and return the new stackpointer.
 */

{
    char *s;

    s = xalloc(strlen(p)+1);
    strcpy(s, p);
    sp++;
    sp->type = T_STRING;
    sp->x.string_type = STRING_MALLOC;
    sp->u.string = s;
    return sp;
}

/*-------------------------------------------------------------------------*/
void
push_string_malloced (char *p)

/* Malloc a copy of string <p> and push it onto the stack as defined
 * by <inter_sp>.
 */

{
    struct svalue *sp;
    char *s;

    s = xalloc(strlen(p)+1);
    strcpy(s, p);
    sp = ++inter_sp;
    sp->type = T_STRING;
    sp->x.string_type = STRING_MALLOC;
    sp->u.string = s;
}

/*-------------------------------------------------------------------------*/
INLINE static struct svalue *
_push_malloced_string (char *p, struct svalue *sp)

/* Push malloced string <p> onto the stack, currently ending at <sp>,
 * and return the new stackpointer.
 * <p> is not copied!
 */

{
    sp++;
    sp->type = T_STRING;
    sp->x.string_type = STRING_MALLOC;
    sp->u.string = p;
    return sp;
}

/*-------------------------------------------------------------------------*/
LOCAL_INLINE
void push_malloced_string (char *p)

/* Push malloced string <p> onto the stack as defined by <inter_sp>.
 * <p> is not copied!
 */

{
    inter_sp++;
    inter_sp->type = T_STRING;
    inter_sp->x.string_type = STRING_MALLOC;
    inter_sp->u.string = p;
}

/*-------------------------------------------------------------------------*/
static INLINE void
put_malloced_string (char *p, struct svalue *sp)

/* Put malloced string <p> into the current stack entry as defined
 * by <inter_sp>. <p> is not copied!
 */

{
    sp->type = T_STRING;
    sp->x.string_type = STRING_MALLOC;
    sp->u.string = p;
}

/*-------------------------------------------------------------------------*/
static INLINE struct svalue *
_push_volatile_string (char *p, struct svalue *sp)

/* Push the volatile string <p> onto the stack, currently ending at <sp>,
 * and return the new stackpointer.
 */

{
    sp++;
    sp->type = T_STRING;
    sp->x.string_type = STRING_VOLATILE;
    sp->u.string = p;
    return sp;
}

/*-------------------------------------------------------------------------*/
void
push_volatile_string (char *p)

/* Push the volatile string <p> onto the stack as defined by <inter_sp>.
 */

{
    inter_sp++;
    inter_sp->type = T_STRING;
    inter_sp->x.string_type = STRING_VOLATILE;
    inter_sp->u.string = p;
}

/*-------------------------------------------------------------------------*/
void
push_svalue (struct svalue *v)

/* Push the svalue <v> onto the stack as defined by <inter_sp>.
 * Same semantic as assign_svalue_no_free().
 */

{
    assign_svalue_no_free(++inter_sp, v);
}

/*-------------------------------------------------------------------------*/
void
push_svalue_block (int num, struct svalue *v)

/* Push all <num> svalues starting at <v> onto the stack as defined by
 * <inter_sp>. Same semantic as assign_svalue_no_free().
 */

{
    struct svalue *w;

    for (w = inter_sp; --num >= 0; v++)
    {
        w++;
        assign_lrvalue_no_free(w, v);
    }
    inter_sp = w;
}

/*-------------------------------------------------------------------------*/
LOCAL_INLINE void
pop_stack (void)

/* Pop the topmost element from the stack as defined by <inter_sp>,
 * using free_svalue().
 */

{
#ifdef DEBUG
    if (inter_sp < start_of_stack)
        fatal("Stack underflow.\n");
#endif
    free_svalue(inter_sp--);
}

/*-------------------------------------------------------------------------*/
void
drop_stack (void)

/* Drop the topmost elemeent from the stack as defined by <inter_sp>,
 * without actually freeing it.
 */

{
    inter_sp--;
}

/*-------------------------------------------------------------------------*/
INLINE static struct svalue *
_pop_n_elems (int n, struct svalue *sp)

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
        free_svalue(sp--);
    return sp;
}

/*-------------------------------------------------------------------------*/
struct svalue *
pop_n_elems (int n, struct svalue *sp)

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
        free_svalue(sp--);
    return sp;
}

/*-------------------------------------------------------------------------*/
static void
stack_overflow (struct svalue *sp, struct svalue *fp, bytecode_p pc)

/* Recover from a stack overflow by popping all the elements between the
 * current stack end <sp> and the begin of the frame <fp>.
 * The function then assigns the new <sp> == <fp> and the <pc> to the
 * corresponding inter_xx variables and generates an error.
 */

{
    _pop_n_elems(sp-fp, sp);
    ERROR("stack overflow\n")
}

/*-------------------------------------------------------------------------*/
void
push_vector (struct vector *v)

/* Push vector <v> onto the stack as defined by <inter_sp>.
 * The refs of <v> are incremented.
 */

{
    v->ref++;
    inter_sp++;
    inter_sp->type = T_POINTER;
    inter_sp->u.vec = v;
}

/*-------------------------------------------------------------------------*/
void
push_referenced_vector (struct vector *v)

/* Push vector <v> onto the stack as defined by <inter_sp>.
 * The refs of <v> are _not_ incremented.
 */

{
    inter_sp++;
    inter_sp->type = T_POINTER;
    inter_sp->u.vec = v;
}

/*-------------------------------------------------------------------------*/
void
push_referenced_mapping (struct mapping *m)

/* Push mapping <m> onto the stack as defined by <inter_sp>.
 * The refs of <m> are _not_ incremented.
 */

{
    inter_sp++;
    inter_sp->type = T_MAPPING;
    inter_sp->u.map = m;
}

/*-------------------------------------------------------------------------*/
/* Macro-only functions:
 */

#define put_vector(v) ( \
        sp->type = T_POINTER,\
        (sp->u.vec = (v))->ref++\
)

#define put_referenced_vector(v) ( \
        sp->type = T_POINTER,\
        sp->u.vec = (v)\
)

#define push_mapping(m) ( \
    sp++,\
    sp->type = T_MAPPING,\
    (sp->u.map = (m))->ref++\
)

/*-------------------------------------------------------------------------*/
/* Fast version of several functions, must come last so to not disturb
 * the actual definitions:
 */

#define put_object(ob)          _put_object(ob, sp)
#define push_object(ob)         (sp = _push_object((ob), sp))
#define push_valid_ob(ob)       (sp = _push_valid_ob((ob), sp))
#define push_number(n)          (sp = _push_number(n, sp))
#define push_shared_string(p)   (sp = _push_shared_string((p), sp))
#define push_string_malloced(p) (sp = _push_string_malloced(p, sp))
#define push_malloced_string(s) (sp = _push_malloced_string((s), sp))
#define push_volatile_string(s) (sp = _push_volatile_string((s), sp))
#define pop_stack()             free_svalue(sp--)
#define pop_n_elems(n)          (sp = _pop_n_elems((n), sp))
#define STACK_OVERFLOW(sp, fp, pc) stack_overflow(sp, fp, pc)
#define push_vector(v) ( \
        sp++,\
        sp->type = T_POINTER,\
        (sp->u.vec = (v))->ref++\
)

#define push_referenced_vector(v) ( \
        sp++,\
        sp->type = T_POINTER,\
        sp->u.vec = (v)\
)

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
 *   push_protected_indexed_lvalue(vector|mapping v, int|mixed i)
 *     Return &(v[i]), protected.
 *   push_protected_rindexed_lvalue(vector v, int i)
 *     Return &(v[<i]), protected.
 *   push_protected_indexed_map_lvalue(mapping m, mixed i, int j)
 *     Return &(m[i:j]), protected.
 *   index_lvalue(vector|mapping|string & v, int|mixed i)
 *     Return &(*v[i]), unprotected, using special_lvalue.
 *   rindex_lvalue(vector|string & v, int i)
 *     Return &(*v[<i]), unprotected, using special_lvalue.
 *   protected_index_lvalue(vector|mapping|string & v, int|mixed i)
 *     Return &(*v[i]), protected.
 *   protected_rindex_lvalue(vector|string & v, int i)
 *     Return &(*v[<i]), protected.
 *   range_lvalue(vector|string & v, int i2, int i1)
 *     Return &(*v[i1..i2]), unprotected, using special_lvalue.
 *   protected_range_lvalue(vector|string & v, int i2, int i1)
 *     Return &(*v[i1..i2]), protected.
 *   push_indexed_value(string|vector|mapping v, int|mixed i)
 *     Return v[i].
 *   push_rindexed_value(string|vector v, int i)
 *     Return v[<i].
 *   f_extract_lvalue(string|vector &v, int i)
 *     Return &(*v[i..]), unprotected, using special_lvalue.
 */

/*-------------------------------------------------------------------------*/
static INLINE struct svalue *
push_indexed_lvalue (struct svalue *sp, bytecode_p pc)

/* Operator F_PUSH_INDEXED_LVALUE(vector  v=sp[-1], int   i=sp[0])
 * Operator F_PUSH_INDEXED_LVALUE(mapping v=sp[-1], mixed i=sp[0])
 *
 * Compute the lvalue &(v[i]) and push it into the stack. If v has just
 * one ref left, the indexed item is stored in indexing_quickfix and the
 * lvalue refers to that variable.
 */

{
    struct svalue *i;     /* the index value */
    struct svalue *vec;   /* the indexed vector or mapping */
    struct svalue *item;  /* the indexed element vec[i] */
    int ind;              /* numeric value of *i */

    /* Get the arguments */
    i = sp;
    vec = sp - 1;

    /* Index a vector.
     */
    if (vec->type == T_POINTER)
    {
        if (i->type != T_NUMBER || (ind = i->u.number) < 0)
        {
            ERROR("Illegal index for []: not a (positive) number.\n")
            /* TODO: Print type and value of i */
            return NULL;
        }
        if ((size_t)ind >= VEC_SIZE(vec->u.vec))
        {
            ERRORF(("Index out of bounds for []: %ld, vector size: %lu.\n"
                   , (long)ind, VEC_SIZE(vec->u.vec)))
            return NULL;
        }
        
        /* Compute the indexed element */
        item = &vec->u.vec->item[ind];
        
        if (vec->u.vec->ref == 1)
        {
            /* Rescue the indexed item as vec will go away */
            assign_svalue (&indexing_quickfix, item);
            item = &indexing_quickfix;
        }
        
        /* Remove the arguments from the stack */
        sp = vec;                
        free_vector(vec->u.vec);

        /* Return the result */
        vec->type = T_LVALUE;
        vec->u.lvalue = item;
        return sp;
    }

    /* Index a mapping
     */
    if (vec->type == T_MAPPING)
    {
        struct mapping *m;

        m = vec->u.map;

        if (!m->num_values)
        {
            ERROR("Indexing a mapping of width 0.\n")
            return NULL;
        }
        
        /* Compute the indexed element */
        item = get_map_lvalue(m, i, MY_TRUE);

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
    dump_trace(MY_TRUE);
    error("(lvalue)Indexing on illegal type.\n");
    /* TODO: Print the type */
    return sp;
} /* push_indexed_lvalue() */

/*-------------------------------------------------------------------------*/
static INLINE struct svalue *
push_rindexed_lvalue (struct svalue *sp, bytecode_p pc)

/* Operator F_PUSH_RINDEXED_LVALUE(vector v=sp[-1], int i=sp[0])
 *
 * Compute the lvalue &(v[<i]) and push it into the stack. If v has just
 * one ref left, the indexed item is stored in indexing_quickfix and the
 * lvalue refers to that variable.
 */

{
    struct svalue *i;     /* the index value */
    struct svalue *vec;   /* the vector */
    struct svalue *item;  /* the indexed item */
    int ind;              /* the numeric value of *i */

    /* Get the arguments */
    i = sp;
    vec = sp - 1;
    
    /* Index a vector.
     */
    if (vec->type == T_POINTER)
    {
        if (i->type != T_NUMBER || (ind = i->u.number) <= 0)
        {
            ERROR("Illegal index for [<]: not a (positive) number.\n")
            /* TODO: Print type and value of i */
            return NULL;
        }
        if ( (ind = VEC_SIZE(vec->u.vec) - ind) < 0) {
            ERRORF(("Index out of bounds for [<]: %ld, vector size: %lu.\n"
                   , (long)(i->u.number), VEC_SIZE(vec->u.vec)))
            return NULL;
        }

        /* Compute the indexed item */
        item = &vec->u.vec->item[ind];
        
        if (vec->u.vec->ref == 1)
        {
            /* Rescue the indexed item as vec will go away */
            assign_svalue (&indexing_quickfix, item);
            item = &indexing_quickfix;
        }
        
        /* Remove the arguments from the stack */
        sp = vec;
        free_vector(vec->u.vec);

        /* Return the result */
        vec->type = T_LVALUE;
        vec->u.lvalue = item;
        return sp;
    }

    /* Indexing on illegal type */
    inter_sp = sp;
    inter_pc = pc;
    dump_trace(MY_TRUE);
    error("(lvalue)Indexing on illegal type.\n");
    /* TODO: Print type */
    return NULL;
} /* push_rindexed_lvalue() */

/*-------------------------------------------------------------------------*/
/* void BUILD_MAP_PROTECTOR(struct svalue *dest, struct mapping *m)
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
    struct hash_mapping *hm;             \
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
static INLINE struct svalue *
push_protected_indexed_lvalue (struct svalue *sp, bytecode_p pc)

/* Op. F_PUSH_PROTECTED_INDEXED_LVALUE(vector  v=sp[-1], int   i=sp[0])
 * Op. F_PUSH_PROTECTED_INDEXED_LVALUE(mapping v=sp[-1], mixed i=sp[0])
 *
 * Compute the lvalue &(v[i]), store it in a struct protected_lvalue, and
 * push the protector as PROTECTED_LVALUE into the stack.
 */

{
    struct svalue           * i;       /* the index */
    struct svalue           * vec;     /* the vector */
    struct svalue           * item;    /* the indexed element */
    struct protected_lvalue * lvalue;  /* the protector */
    int                       ind;     /* numeric value of *i */

    /* Get the arguments */
    i = sp;
    vec = sp - 1;
    
    /* Index a vector.
     */
    if (vec->type == T_POINTER)
    {
        if (i->type != T_NUMBER || (ind = i->u.number) < 0)
        {
            ERROR("Illegal index for []: not a (positive) number.\n")
            /* TODO: Print type and value */
            return NULL;
        }
        
        if ((size_t)ind >= VEC_SIZE(vec->u.vec))
        {
            ERRORF(("Index out of bounds for []: %ld, vector size: %lu.\n"
                   , (long)ind, VEC_SIZE(vec->u.vec)))
            return NULL;
        }

        /* Compute the indexed item and set up the protector */

        item = &vec->u.vec->item[ind];
        lvalue = (struct protected_lvalue *)xalloc(sizeof *lvalue);
        lvalue->v.type = T_PROTECTED_LVALUE;
        lvalue->v.u.lvalue = item;
        lvalue->protector.type = T_POINTER;
        lvalue->protector.u.vec = vec->u.vec;
          /* The one ref to vec is transferred from *vec */

        /* Remove the arguments and return the result */
        sp = vec;
        vec->type = T_LVALUE;
        vec->u.lvalue = &lvalue->v;
        return sp;
    }

    /* Index a mapping
     */
    if (vec->type == T_MAPPING)
    {
        struct mapping *m;

        m = vec->u.map;
        
        if (!m->num_values)
        {
            ERROR("Indexing a mapping of width 0.\n");
            return NULL;
        }
        
        /* Compute the indexed item and set up the protector */

        item = get_map_lvalue(m, i, MY_TRUE);

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
    dump_trace(MY_TRUE);
    error("(lvalue)Indexing on illegal type.\n");
    /* TODO: Print type */
    return NULL;
} /* push_protected_indexed_lvalue() */

/*-------------------------------------------------------------------------*/
static INLINE struct svalue *
push_protected_rindexed_lvalue (struct svalue *sp, bytecode_p pc)

/* Op. F_PUSH_PROTECTED_RINDEXED_LVALUE(vector v=sp[-1], int i=sp[0])
 *
 * Compute the lvalue &(v[<i]), store it in a struct protected_lvalue, and
 * push the protector as PROTECTED_LVALUE into the stack.
 */

{
    struct svalue           * i;       /* the index */
    struct svalue           * vec;     /* the vector */
    struct svalue           * item;    /* the indexed element */
    struct protected_lvalue * lvalue;  /* the protector */
    int                       ind;     /* numeric value of *i */

    /* Get the arguments */
    
    i = sp;
    vec = sp - 1;

    /* Index a vector
     */
    if (vec->type == T_POINTER)
    {
        if (i->type != T_NUMBER || (ind = i->u.number) <= 0)
        {
            ERROR("Illegal index for [<]: not a (positive) number.\n")
            /* TODO: Print type and value of i */
            return NULL;
        }

        if ( (ind = VEC_SIZE(vec->u.vec) - ind) < 0)
        {
            ERRORF(("Index out of bounds for [<]: %ld, vector size: %lu\n"
                   , (long) i->u.number, VEC_SIZE(vec->u.vec)))
            return NULL;
        }

        /* Compute the indexed element and setup the protector */

        item = &vec->u.vec->item[ind];
        
        lvalue = (struct protected_lvalue *)xalloc(sizeof *lvalue);
        lvalue->v.type = T_PROTECTED_LVALUE;
        lvalue->v.u.lvalue = item;
        lvalue->protector.type = T_POINTER;
        lvalue->protector.u.vec = vec->u.vec;
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
    dump_trace(MY_TRUE);
    error("(lvalue)Indexing on illegal type.\n");
    /* TODO: Print type */
    return NULL;
} /* push_protected_rindexed_lvalue() */

/*-------------------------------------------------------------------------*/
static INLINE struct svalue *
push_protected_indexed_map_lvalue (struct svalue *sp, char *pc)

/* Op. F_PUSH_PROTECTED_INDEXED_MAP_LVALUE(mapping m=sp[-2], mixed i=sp[-1]
 *                                                         , int   j=sp[0])
 *
 * Compute the lvalue &(m[i:j]), store it in a struct protected_lvalue, and
 * push the protector as PROTECTED_LVALUE into the stack.
 */

{
    struct svalue           * i;       /* the index */
    struct svalue           * vec;     /* the vector */
    struct svalue           * item;    /* the indexed element */
    struct protected_lvalue * lvalue;  /* the protector */

    /* Get the arguments */
    i = sp - 1;
    vec = sp - 2;

    /* Index a mapping.
     */
    if (vec->type == T_MAPPING)
    {
        struct mapping *m;

        m = vec->u.map;
        if (sp->u.number != T_NUMBER
         || (p_uint)sp->u.number >= (p_uint)m->num_values
            /* using uints automagically checks for negative indices */
           )
        {
            ERROR("Illegal subindex for []: not a number or out of bounds.\n")
            /* TODO: This error message should be done nicer. */
            return NULL;
        }

        /* Compute the indexed element and setup the protector */
        
        item = get_map_lvalue(m, i, MY_TRUE) + sp->u.number;

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
    dump_trace(MY_TRUE);
    error("(lvalue)Indexing on illegal type.\n");
    /* TODO: Print type */
    return NULL;
} /* push_protected_indexed_map_lvalue() */

/*-------------------------------------------------------------------------*/
static INLINE struct svalue *
index_lvalue (struct svalue *sp, bytecode_p pc)

/* Operator F_INDEX_LVALUE (string|vector &v=sp[0], int   i=sp[-1])
 *          F_INDEX_LVALUE (mapping       &v=sp[0], mixed i=sp[-1])
 *
 * Compute the index &(v[i]) of lvalue <v> and push it into the stack. The
 * computed index is a lvalue itself.
 * If <v> is a string-lvalue, it is made a malloced string if necessary,
 * and the pushed result will be a lvalue pointing to a CHAR_LVALUE stored
 * in <special_lvalue>.
 */

{
    struct svalue *vec;   /* the vector/mapping */
    struct svalue *i;     /* the index */
    int            ind;   /* numeric value of <i> */
    short          type;  /* type of <vec> */

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
        struct vector *v = vec->u.vec;

        if (i->type != T_NUMBER || (ind = i->u.number) < 0)
        {
            ERROR("Illegal index for []: not a (positive) number.\n")
            /* TODO: Print type and value of i */
            return NULL;
        }

        if ((size_t)ind >= VEC_SIZE(v))
        {
            ERRORF(("Index for [] out of bounds: %ld, vector size: %lu\n"
                   , (long)ind, VEC_SIZE(v)))
            return NULL;
        }

        /* Remove the arguments and push the result */
        
        sp = i;

        sp->type = T_LVALUE;
        sp->u.lvalue = &v->item[ind];
        return sp;
    }

    /* Index a string.
     */
    if (type == T_STRING)
    {
        if (i->type != T_NUMBER || (ind = i->u.number) < 0)
        {
            ERROR("Illegal index for []: not a (positive) number.\n")
            /* TODO: Print type and value of i */
            return NULL;
        }

        if (ind >= _svalue_strlen(vec) )
        {
            ERRORF(("Index out for [] of bounds: %ld, string length: %ld.\n"
                   , (long)ind, (long)_svalue_strlen(vec)))
            return NULL;
        }
        
        /* If the string is not malloced, i.e. changeable, allocate
         * a new copy which can be changed.
         */
        if (vec->x.string_type != STRING_MALLOC)
        {
            char *p = string_copy(vec->u.string);

            if (vec->x.string_type == STRING_SHARED)
                free_string(vec->u.string);
            vec->x.string_type = STRING_MALLOC;
            vec->u.string = p;
        }

        /* Remove the arguments and create and push the result. */
        
        sp = i;
        
        sp->type = T_LVALUE;
        sp->u.lvalue = &special_lvalue.v;
        special_lvalue.v.type = T_CHAR_LVALUE;
        special_lvalue.v.u.string = &vec->u.string[ind];
        return sp;
    }

    /* Index a mapping.
     */
    if (type == T_MAPPING)
    {
        struct svalue *item;
        struct mapping *m;

        m = vec->u.map;
        if (!m->num_values)
        {
            ERROR("Indexing a mapping of width 0.\n");
            return NULL;
        }

        /* Compute the element */
        item = get_map_lvalue(m, i, MY_TRUE);

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
    dump_trace(MY_TRUE);
    error("(lvalue)Indexing on illegal type.\n");
    /* TODO: Print type */
    return NULL;
} /* index_lvalue() */

/*-------------------------------------------------------------------------*/
static INLINE struct svalue *
rindex_lvalue (struct svalue *sp, bytecode_p pc)

/* Operator F_RINDEX_LVALUE (string|vector &v=sp[0], int   i=sp[-1])
 *
 * Compute the index &(v[<i]) of lvalue <v> and push it into the stack. The
 * computed index is a lvalue itself.
 * If <v> is a string-lvalue, it is made a malloced string if necessary,
 * and the pushed result will be a lvalue pointing to a CHAR_LVALUE stored
 * in <special_lvalue>.
 */

{
    struct svalue *vec;   /* the vector/string */
    struct svalue *i;     /* the index */
    int            ind;   /* numeric value of <i> */
    short          type;  /* type of <vec> */

    /* get the arguments */
    vec = sp;
    i = sp -1;

    if (i->type != T_NUMBER || (ind = i->u.number) <= 0)
    {
        ERROR("Illegal index for [<]: not a (positive) number.\n")
        /* TODO: Print type and value of i */
        return NULL;
    }
    
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
        struct vector *v = vec->u.vec;


        if ( (ind = VEC_SIZE(v) - ind) < 0)
        {
            ERRORF(("Index out of bounds for [<]: %ld, vector size: %lu\n"
                   , (long) i->u.number, VEC_SIZE(vec->u.vec)))
            return NULL;
        }

        /* Remove the arguments and return the result */
        
        sp = i;
        sp->type = T_LVALUE;
        sp->u.lvalue = &v->item[ind];
        return sp;
    }

    /* Index a string
     */
    if (type == T_STRING)
    {
        if ( (ind = _svalue_strlen(vec) - ind) < 0)
        {
            ERRORF(("Index out of bounds for [<]: %ld, string length: %ld\n"
                   , (long) i->u.number, (long)_svalue_strlen(vec)))
            return NULL;
        }

        /* If the string is not malloced, i.e. changeable, allocate a
         * copy in vec which can be changed.
         */
        if (vec->x.string_type != STRING_MALLOC)
        {
            char *p = string_copy(vec->u.string);
            
            if (vec->x.string_type == STRING_SHARED)
                free_string(vec->u.string);
            vec->x.string_type = STRING_MALLOC;
            vec->u.string = p;
        }

        /* Remove the argument and return the result */
        
        sp = i;
        sp->type = T_LVALUE;
        sp->u.lvalue = &special_lvalue.v;
        special_lvalue.v.type = T_CHAR_LVALUE;
        special_lvalue.v.u.string = &vec->u.string[ind];
        return sp;
    }

    /* Indexing on illegal type */

    inter_sp = sp;
    inter_pc = pc;
    dump_trace(MY_TRUE);
    error("(lvalue)Indexing on illegal type.\n");
    /* TODO: Print the type */
    return NULL;
} /* rindex_lvalue() */

/*-------------------------------------------------------------------------*/
static INLINE struct svalue *
protected_index_lvalue (struct svalue *sp, bytecode_p pc)

/* Operator F_PROTECTED_INDEX_LVALUE (string|vector &v=sp[0], int   i=sp[-1])
 *          F_PROTECTED_INDEX_LVALUE (mapping       &v=sp[0], mixed i=sp[-1])
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
    struct svalue *vec;   /* the indexed value */
    struct svalue *i;     /* the index */
    int            ind;   /* numeric value of <i> */
    short          type;  /* type of <vec> */

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
            struct vector *v = vec->u.vec;
            struct protected_lvalue *lvalue;

            if (i->type != T_NUMBER || (ind = i->u.number) < 0)
            {
                ERROR("Illegal index for []: not a (positive) number.\n")
                /* TODO: Print type and value of i */
                return NULL;
            }
            
            if ((size_t)ind >= VEC_SIZE(v))
            {
                ERRORF(("Index for [] out of bounds: %ld, vector size: %lu.\n"
                       , (long)ind, VEC_SIZE(v)))
                return NULL;
            }

            /* Drop the arguments */
            sp = i;
            
            /* Compute and return the result */
            
            v->ref++; /* the ref from the protector */
            lvalue = (struct protected_lvalue *)xalloc(sizeof *lvalue);
            lvalue->v.type = T_PROTECTED_LVALUE;
            lvalue->v.u.lvalue = &v->item[ind];
            lvalue->protector.type = T_POINTER;
            lvalue->protector.u.vec = v;
            
            sp->type = T_LVALUE;
            sp->u.lvalue = &lvalue->v;
            
            return sp;
        }

        /* Index a string.
         */
        if (type == T_STRING)
        {
            struct protected_char_lvalue *val;

            if (i->type != T_NUMBER || (ind = i->u.number) < 0)
            {
                ERROR("Illegal index for []: not a (positive) number.\n")
                /* TODO: Print type and value of i */
                return NULL;
            }

            if (ind > svalue_strlen(vec) )
            {
                ERRORF(("Index for [] out of bounds: %ld, string length: %ld.\n"
                       , (long)ind, (long)_svalue_strlen(vec)))
                return NULL;
            }
            
            /* If the string is not allocated, ie. changeable, allocate
             * a new changeable copy.
             */
            if (vec->x.string_type != STRING_MALLOC)
            {
                char *p = string_copy(vec->u.string);
                
                if (vec->x.string_type == STRING_SHARED)
                    free_string(vec->u.string);
                vec->u.string = p;
                /* string_type is used by svalue_strlen */
                vec->x.string_type = STRING_MALLOC;
            }
            
            /* Make the string 'VOLATILE' so that we have full control
             * over its deallocation.
             */
            vec->x.string_type = STRING_VOLATILE;

            /* Drop the arguments */
            sp = i;
            
            /* Compute and return the result */
            
            val = (struct protected_char_lvalue *)xalloc(sizeof *val);
            val->v.type = T_PROTECTED_CHAR_LVALUE;
            val->v.u.string = &vec->u.string[ind];
            val->lvalue = vec;
            val->start = vec->u.string;
            val->protector.type = T_INVALID;

            sp->type = T_LVALUE;
            sp->u.protected_char_lvalue = val;
            
            return sp;
        }

        /* Index a mapping.
         */
        if (type == T_MAPPING)
        {
            struct svalue *item;
            struct protected_lvalue *lvalue;
            struct mapping *m;

            m = vec->u.map;
            if (!m->num_values)
            {
                ERROR("Indexing a mapping of width 0.\n");
                return NULL;
            }

            /* Compute the indexed element */
            item = get_map_lvalue(m, i, MY_TRUE);

            /* Build the protector */
            m->ref++;
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
            
            if (i->type != T_NUMBER || (ind = i->u.number) < 0)
            {
                ERROR("Illegal index for []: not a (positive) number.\n")
                /* TODO: Print type and value of i */
                return NULL;
            }
            
            if (ind > svalue_strlen(vec) )
            {
                ERRORF(("Index for [] out of bounds: %ld, string length: %ld\n"
                       , (long)ind, (long)_svalue_strlen(vec)))
                return NULL;
            }

            /* If the string is not allocated, ie changeable, allocate
             * a copy we can change.
             */
            if (vec->x.string_type != STRING_MALLOC)
            {
                char *p = string_copy(vec->u.string);

                if (vec->x.string_type == STRING_SHARED)
                    free_string(vec->u.string);
                vec->u.string = p;
            }

            /* Make the string 'VOLATILE' so that we have full control
             * over its deallocation.
             */
            vec->x.string_type = STRING_VOLATILE;

            /* Build the protector */
            val = (struct protected_char_lvalue *)xalloc(sizeof *val);
            val->v.type = T_PROTECTED_CHAR_LVALUE;
            val->v.u.string = &vec->u.string[ind];
            val->lvalue = vec;
            val->start = vec->u.string;

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
        dump_trace(MY_TRUE);
        error("(lvalue)Indexing on illegal type.\n");
        /* TODO: Print type */
        return NULL;
    } /* for(ever) */

    /* NOTREACHED */
    return NULL;
} /* protected_index_lvalue() */

/*-------------------------------------------------------------------------*/
static INLINE struct svalue *
protected_rindex_lvalue (struct svalue *sp, bytecode_p pc)

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
    struct svalue *vec;   /* the indexed value */
    struct svalue *i;     /* the index */
    int            ind;   /* numeric value of <i> */
    short          type;  /* type of <vec> */

    /* Get arguments */
    vec = sp->u.lvalue;
    i = sp -1;

    if (i->type != T_NUMBER || (ind = i->u.number) <= 0)
    {
        ERROR("Illegal indexi for [<]: not a (positive) number.\n")
        /* TODO: Print type and value of i */
        return NULL;
    }

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
            struct vector *v = vec->u.vec;
            struct protected_lvalue *lvalue;

            if ( (ind = VEC_SIZE(v) - ind) < 0)
            {
                ERRORF(("Index for [<] out of bounds: %ld, vector size: %lu\n"
                       , (long)i->u.number, VEC_SIZE(v)))
                return NULL;
            }

            /* Create the protector for the result */
            
            v->ref++; /* the ref from the protector */
            lvalue = (struct protected_lvalue *)xalloc(sizeof *lvalue);
            lvalue->v.type = T_PROTECTED_LVALUE;
            lvalue->v.u.lvalue = &v->item[ind];
            lvalue->protector.type = T_POINTER;
            lvalue->protector.u.vec = v;

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

            if ( (ind = svalue_strlen(vec)  - ind) < 0)
            {
                ERRORF(("Index for [<] out of bounds: %ld, string length: %ld.\n"
                       , (long)i->u.number, (long)svalue_strlen(vec)))
                return NULL;
            }
            
            /* If the string is not allocated, ie. changeable, allocate
             * a new changeable copy.
             */
            if (vec->x.string_type != STRING_MALLOC)
            {
                char *p = string_copy(vec->u.string);
                
                if (vec->x.string_type == STRING_SHARED)
                    free_string(vec->u.string);
                vec->u.string = p;
            }
            
            /* Make the string 'VOLATILE' so that we have full control
             * over its deallocation.
             */
            vec->x.string_type = STRING_VOLATILE;

            /* Build the protector */
            val = (struct protected_char_lvalue *)xalloc(sizeof *val);
            val->v.type = T_PROTECTED_CHAR_LVALUE;
            val->v.u.string = &vec->u.string[ind];
            val->lvalue = vec;
            val->start = vec->u.string;
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
            
            if ( (ind = svalue_strlen(vec)  - ind) < 0)
            {
                ERRORF(("Index for [<] out of bounds: %ld, string length: %ld.\n"
                       , (long)i->u.number, (long)svalue_strlen(vec)))
                return NULL;
            }
            
            /* If the string is not allocated, ie changeable, allocate
             * a copy we can change.
             */
            if (vec->x.string_type != STRING_MALLOC)
            {
                char *p = string_copy(vec->u.string);
                if (vec->x.string_type == STRING_SHARED)
                    free_string(vec->u.string);
                vec->u.string = p;
            }

            /* Make the string 'VOLATILE' so that we have full control
             * over its deallocation.
             */
            vec->x.string_type = STRING_VOLATILE;

            /* Build the protector */
            val = (struct protected_char_lvalue *)xalloc(sizeof *val);
            val->v.type = T_PROTECTED_CHAR_LVALUE;
            val->v.u.string = &vec->u.string[ind];
            val->lvalue = vec;
            val->start = vec->u.string;
            
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
        dump_trace(MY_TRUE);
        error("(lvalue)Indexing on illegal type.\n");
        /* TODO: Print the type */
        return NULL;
    } /* for(ever) */
    
    /* NOTREACHED */
    return NULL;
} /* protected_rindex_lvalue() */

/*-------------------------------------------------------------------------*/
static struct svalue *
range_lvalue (int code, struct svalue *sp)

/* Operator F_RANGE_LVALUE (string|vector &v=sp[0], int i2=sp[-1], i1=sp[-2])
 * and the operators F_{NR,RN,RR}_RANGE_LVALUE.
 *
 * Compute the range &(v[i1..i2]) of lvalue <v> and push it into the stack.
 * The value pushed is a lvalue pointint to <special_lvalue>. <special_lvalue>
 * then is the POINTER_RANGE_- resp. STRING_RANGE_LVALUE.
 *
 * <code> is a two-byte flag determining whether the indexes are counted
 * from the beginning ('[i1..' and '..i2]') or the the end of the vector
 * or string ('[<i1..' and '..<i2]'). If <code>&0xff00 is TRUE, the lower
 * index is counted from the end, if <code>&0x00ff is TRUE, the upper
 * index is counted from the end.
 * TODO: This code thingy is not really nice.
 */

{
    struct svalue *vec;         /* the indexed vector or string */
    struct svalue *i;           /* the index */
    int            ind1, ind2;  /* Lower and upper range index */
    short          type;        /* type of <vec> */
    int            size;        /* size of <vec> in elements */

    /* Get the arguments */
    vec = sp;
    i = sp-1;
    
#ifdef DEBUG
    if (sp->type != T_LVALUE) {
        inter_sp = sp;
        error("wrong type to range_lvalue\n");
        /* TODO: Print type */
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
        size = VEC_SIZE(vec->u.vec);
        break;
    case T_STRING:
        special_lvalue.v.type = T_STRING_RANGE_LVALUE;
        size = svalue_strlen(vec);
        break;
    default:
        inter_sp = sp;
        dump_trace(MY_TRUE);
        error("(lvalue)Range index on illegal type.\n");
        /* TODO: Print type */
        return NULL;
    }

    /* Get and check the upper bound i2 */

    if (i->type != T_NUMBER)
    {
        inter_sp = sp;
        error("Illegal upper range index: not a number.\n");
        /* TODO: Print type */
        return NULL;
    }
    
    if (code & 0xff)
    {
        ind2 = size - i->u.number;
    }
    else
    {
        ind2 = i->u.number;
    }

    
    if (++ind2 < 0 || ind2 > size)
    {
        inter_sp = sp;
        error("Upper range index out of bounds: %ld, size: %ld.\n"
             , (long)i->u.number, (long)size);
        return NULL;
    }

    /* Get and check the lower bound i1 */

    if ((--i)->type != T_NUMBER)
    {
        inter_sp = sp;
        error("Illegal lower range index: not a number.\n");
        /* TODO: Print the type */
        return NULL;
    }
    
    if (code & 0xff00)
    {
        ind1 = size - i->u.number;
    }
    else
    {
        ind1 = i->u.number;
    }

    if (ind1 < 0 || ind1 > size)
    {   /* Appending (ind1 == size) is allowed */
        inter_sp = sp;
        error("Lower range index out of bounds: %ld, size: %ld.\n"
             , (long)i->u.number, (long)size);
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
static struct svalue *
protected_range_lvalue (int code, struct svalue *sp)

/* X-Operator F_PROTECTED_RANGE_LVALUE
 *                       (string|vector &v=sp[0], int i2=sp[-1], i1=sp[-2])
 * and the x-operators F_PROTECTED_{NR,RN,RR}_RANGE_LVALUE and
 * F_PROTECTED_EXTRACT_LVALUE.
 *
 * Compute the range &(v[i1..i2]) of lvalue <v>, wrap it into a protector,
 * and push the reference to the protector onto the stack.
 * 
 * If <v> is a protected lvalue itself, its protecting svalue will be used
 * in the result protector.
 * 
 * If <v> is a string-lvalue, it is made a malloced string if necessary.
 *
 * <code> is a two-byte flag determining whether the indexes are counted
 * from the beginning ('[i1..' and '..i2]') or the the end of the vector
 * or string ('[<i1..' and '..<i2]'). If <code>&0xff00 is TRUE, the lower
 * index is counted from the end, if <code>&0x00ff is TRUE, the upper
 * index is counted from the end.
 * TODO: This code thingy is not really nice.
 */

{
    struct svalue *vec;         /* the indexed vector or string */
    struct svalue *i;           /* the index */
    int            ind1, ind2;  /* Lower and upper range index */
    short          type;        /* type of <vec> */
    int            size;        /* size of <vec> in elements */
    short          lvalue_type; /* Result type */
    struct svalue  protector;   /* Protecting svalue saved from v */
    struct protected_range_lvalue *new_lvalue;
                                /* Result protector structure */

#ifdef DEBUG
    if (sp->type != T_LVALUE)
    {
        inter_sp = sp;
        error("wrong type to protected_range_lvalue\n");
        /* TODO: Print type */
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
        vec->u.vec->ref++; /* Count the coming protector */
        lvalue_type = T_PROTECTED_POINTER_RANGE_LVALUE;
        size = VEC_SIZE(vec->u.vec);
        break;
        
    case T_STRING:
        /* If the string is not allocated, ie changeable, allocate
         * a copy we can change.
         */
        if (vec->x.string_type != STRING_MALLOC)
        {
            char *p = string_copy(vec->u.string);
        
            if (vec->x.string_type == STRING_SHARED)
                free_string(vec->u.string);
            vec->u.string = p;
        }
        /* Make the string 'VOLATILE' so that we have full control
         * over its deallocation.
         */
        vec->x.string_type = STRING_VOLATILE;

        lvalue_type = T_PROTECTED_STRING_RANGE_LVALUE;
        size = svalue_strlen(vec);
        break;

    default:
        inter_sp = sp;
        dump_trace(MY_TRUE);
        error("(lvalue)Range index on illegal type.\n");
        /* TODO: Print type */
        return NULL;
    }

    /* Get and check the upper index i2 */

    if (i->type != T_NUMBER)
    {
        inter_sp = sp;
        error("Illegal upper range index: not a number.\n");
        /* TODO: Print type. */
        return NULL;
    }
    
    if (code & 0xff)
    {
        ind2 = size - i->u.number;
    }
    else
    {
        ind2 = i->u.number;
    }
    
    if (++ind2 < 0 || ind2 > size) {
        inter_sp = sp;
        error("Upper range index out of bounds: %ld, size: %ld.\n"
             , (long)i->u.number, (long)size);
        return NULL;
    }

    /* Get and check the lower index i1 */
    
    if ((--i)->type != T_NUMBER)
    {
        inter_sp = sp;
        error("Illegal lower range index: not a number.\n");
        /* TODO: Print type. */
        return NULL;
    }
    
    if (code & 0xff00)
    {
        ind1 = size - i->u.number;
    }
    else
    {
        ind1 = i->u.number;
    }
    
    if (ind1 < 0 || ind1 > size)
    { 
        /* Appending (ind1 == size) is allowed */
        inter_sp = sp;
        error("Lower range index out of bounds: %ld, size: %ld.\n"
             , (long)i->u.number, (long)size);
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
static INLINE struct svalue *
push_indexed_value (struct svalue *sp, bytecode_p pc)

/* Operator F_INDEX (string|vector v=sp[0], int   i=sp[-1])
 *          F_INDEX (mapping       v=sp[0], mixed i=sp[-1])
 *
 * Compute the value (v[i]) and push it onto the stack.
 * If the value would be a destructed object, 0 is pushed onto the stack
 * and the ref to the object is removed from the vector/mapping.
 *
 * Mapping indices may use <indexing_quickfix> for temporary storage.
 */

{
    struct svalue *vec;  /* the indexed value */
    struct svalue *i;    /* the index */
    int            ind;  /* numeric value of <i> */

    /* Get arguments */
    i = sp;
    vec = sp - 1;
    
    switch (vec->type)
    {
    case T_STRING:
      {
        if (i->type != T_NUMBER || (ind = i->u.number) < 0)
        {
            ERROR("Illegal index for []: not a (positive) number.\n")
            /* TODO: Print type and value */
            return NULL;
        }

        /* Index the string */
        if (ind > _svalue_strlen(vec))
            ind = 0;
        else
            ind = vec->u.string[ind];

        /* Drop the args and return the result */
        
        free_string_svalue(vec);

        sp = vec;
        sp->type = T_NUMBER;
        sp->u.number = ind;

        return sp;
      }
      
    case T_POINTER:
        if (i->type != T_NUMBER || (ind = i->u.number) < 0)
        {
            ERROR("Illegal index for []: not a (positive) number.\n")
            /* TODO: Print type and value */
            return NULL;
        }
        
        /* Drop the arguments */
        sp = vec;

        if (ind < 0 || (size_t)ind >= VEC_SIZE(vec->u.vec))
        {
            ERRORF(("Index for [] out of bounds: %ld, vector size: %lu\n"
                   , (long)ind, VEC_SIZE(vec->u.vec)))
            return NULL;
        }
        
        /* Assign the indexed element to the sp entry holding vec.
         * Decrement the vector ref manually to optimize the case that
         * this is the last ref to the vector.
         */
        if (!--vec->u.vec->ref)
        {
            struct svalue *p, tmp;

            vec->u.vec->ref++;  /* needed for free_vector() */

            /* Copy the indexed element into <tmp>
             */
            p = &vec->u.vec->item[ind];
            if (p->type == T_OBJECT && p->u.ob->flags & O_DESTRUCTED)
            {
                free_object_svalue(p);
                tmp.type = T_NUMBER;
                tmp.u.number = 0;
            }
            else
            {
                tmp = *p;
            }

            /* Invalidate the old space of the result value and free
             * the vector.
             */
            p->type = T_NUMBER;
            free_vector(vec->u.vec);

            /* Return the result */
            *sp = tmp;
            return sp;
        }
        
        /* The vector continues to live: keep the refcount as it is
         * and just assign the indexed element for the result.
         */
        assign_checked_svalue_no_free(sp, &vec->u.vec->item[ind], sp, pc);
        return sp;
        
    case T_MAPPING:
      {
        struct svalue *item;
        struct mapping *m;

        m = vec->u.map;
        
        if (!m->num_values)
        {
            inter_sp = sp;
            inter_pc = pc;
            dump_trace(MY_TRUE);
            error("(value)Indexing a mapping of width 0.\n");
            return NULL;
        }

        /* Get the item */
        item = get_map_lvalue(m, i, MY_FALSE);
        
        /* Drop the arguments */
        
        free_svalue(i); /* must come before the free(m) in case i and m are
                         * identical: if not done, the following test for
                         * m->ref == 1 would not be possible.
                         */
        
        if (m->ref == 1)
        {
            /* Only one ref left to the mapping: rescue the indexed
             * item in indexing_quickfix before the free(m) will deallocate
             * it.
             */
            assign_svalue (&indexing_quickfix, item);
            item = &indexing_quickfix;
        }
        free_mapping(m);

        /* Return the result */
        sp = vec;
        assign_checked_svalue_no_free(sp, item, sp, pc);
        return sp;
      }

    default:
        inter_sp = sp;
        inter_pc = pc;
        dump_trace(MY_TRUE);
        error("(value)Indexing on illegal type.\n");
        /* TODO: Print type */
        return NULL;
    }
    
    /* NOTREACHED */
    return NULL;
} /* push_indexed_value() */

/*-------------------------------------------------------------------------*/
static INLINE struct svalue *
push_rindexed_value (struct svalue *sp, bytecode_p pc)

/* Operator F_RINDEX (string|vector v=sp[0], int   i=sp[-1])
 *
 * Compute the value (v[<i]) and push it onto the stack.
 * If the value would be a destructed object, 0 is pushed onto the stack
 * and the ref to the object is removed from the vector/mapping.
 */

{
    struct svalue *vec;  /* the indexed value */
    struct svalue *i;    /* the index */
    int            ind;  /* numeric value of <i> */

    /* Get arguments */
    i = sp;
    vec = sp - 1;
    
    if (i->type != T_NUMBER || (ind = i->u.number) <= 0)
    {
        ERROR("Illegal index for [<]: not a (positive) number.\n")
        /* TODO: Print type and value */
        return NULL;
    }
    
    switch (vec->type)
    {
    case T_STRING:
      {
        /* Index the string */
        
        if ( (ind = _svalue_strlen(vec) - ind) < 0 )
            ind = 0;
        else
            ind = vec->u.string[ind];
        
        /* Drop the args and return the result */
        
        free_string_svalue(vec);
        
        sp = vec;
        sp->type = T_NUMBER;
        sp->u.number = ind;
        return sp;
      }
      
    case T_POINTER:
        /* Drop the arguments */
        sp = vec;
        if ( (ind = VEC_SIZE(vec->u.vec) - ind) < 0) {
            ERRORF(("Index for [<] out of bounds: %ld, vector size: %lu\n"
                   , (long)i->u.number, VEC_SIZE(vec->u.vec)))
            return NULL;
        }

        /* Assign the indexed element to the sp entry holding vec.
         * Decrement the vector ref manually to optimize the case that
         * this is the last ref to the vector.
         */
        if (!--vec->u.vec->ref)
        {
            struct svalue *p, tmp;

            vec->u.vec->ref++; /* needed for free_vector() */
            
            /* Copy the indexed element into <tmp>
             */
            p = &vec->u.vec->item[ind];
            if (p->type == T_OBJECT && p->u.ob->flags & O_DESTRUCTED)
            {
                free_object_svalue(p);
                tmp.type = T_NUMBER;
                tmp.u.number = 0;
            }
            else
            {
                tmp = *p;
            }
            
            /* Invalidate the old space of the result value and free
             * the vector.
             */
            p->type = T_NUMBER;
            free_vector(vec->u.vec);

            /* Return the result */
            *sp = tmp;
            return sp;
        }

        /* The vector continues to live: keep the refcount as it is
         * and just assign the indexed element for the result.
         */
        assign_checked_svalue_no_free(sp, &vec->u.vec->item[ind], sp, pc);
        return sp;
      
    default:
        inter_sp = sp;
        inter_pc = pc;
        dump_trace(MY_TRUE);
        error("(value)Indexing on illegal type.\n");
        /* TODO: Print type */
        return NULL;
    }

    /* NOTREACHED */
    return NULL;
} /* push_rindexed_value() */

/*-------------------------------------------------------------------------*/
struct svalue *
f_extract_lvalue (struct svalue *sp)

/* T-Operator F_EXTRACT_LVALUE (string|vector &v=sp[0], int i1=sp[-1])
 *
 * Compute the range &(v[i1..]) of lvalue <v> and push it into the stack.
 * The value pushed is a lvalue pointing to <special_lvalue>. <special_lvalue>
 * then is the POINTER_RANGE_- resp. STRING_RANGE_LVALUE.
 *
 * If <i1> is negative, the range begin is indexed from the end of the string
 * or vector.
 */

{
    struct svalue *vec;   /* the indexed value */
    int            ind1;  /* the numeric value of the index */
    short          type;  /* type of vec */
    int            size;  /* size of vec */

#ifdef DEBUG
    if (sp->type != T_LVALUE)
    {
        inter_sp = sp;
        error("wrong type to range_lvalue\n");
        /* TODO: Print type */
        return NULL;
    }
#endif

    /* Get the vector argument and deref the lvalue(s).
     */
    vec = sp;
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
        size = VEC_SIZE(vec->u.vec);
        break;

    case T_STRING:
        special_lvalue.v.type = T_STRING_RANGE_LVALUE;
        size = svalue_strlen(vec);
        break;

    default:
        inter_sp = sp;
        dump_trace(MY_TRUE);
        error("(lvalue)range lvalue on illegal type.\n");
        /* TODO: Print type */
        return NULL;
    }
    
    /* Get and test the lower range index */
    
    sp--;
    if (sp->type != T_NUMBER)
    {
        inter_sp = sp+1;
        error("Illegal index for [..]: not a number.\n");
        /* TODO: Print type */
        return NULL;
    }
    if (sp->u.number < 0)
    {
        ind1 = size + sp->u.number;
    }
    else
    {
        ind1 = sp->u.number;
    }
    
    if (ind1 < 0 || ind1 > size)
    { 
        /* Appending is allowed */
        inter_sp = sp+1;
        error("Index for [..] out of bounds: %ld, vector size: %ld\n"
             , (long)sp->u.number, (long)size);
        return NULL;
    }

    /* Setup and return the result */
    special_lvalue.v.u.lvalue = vec;
    special_lvalue.size = size;
    special_lvalue.index1 = ind1;
    special_lvalue.index2 = size;
    
    sp->type = T_LVALUE;
    sp->u.lvalue = &special_lvalue.v;
    return sp;
} /* f_extract_lvalue() */

/*=========================================================================*/
/*-------------------------------------------------------------------------*/
void m_indices_filter ( struct svalue *key
                      , struct svalue *data UNUSED
                      , void *extra /* is a struct svalue ** */ )

/* Filter function used by mapping:m_indices() to implement the
 * m_indices() efun. It is here take advantage of the inline expansion
 * of assign_svalue_no_free().
 *
 * <key> points to a key in a mapping, <extra> points to a struct svalue*
 * pointing to a storage place. *key is assigned to **extra, *extra is
 * incremented afterwards.
 */

{
#ifdef __MWERKS__
#    pragma unused(data)
#endif
    struct svalue **svpp = (struct svalue **)extra;

    assign_svalue_no_free( (*svpp)++, key );
}

/*-------------------------------------------------------------------------*/
static void m_values_filter ( struct svalue *key UNUSED
                            , struct svalue *data
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
}

/*-------------------------------------------------------------------------*/
static void m_unmake_filter ( struct svalue *key
                            , struct svalue *data
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
}

/*-------------------------------------------------------------------------*/
static struct program *
search_inherited (char *str, struct program *prg, int *offpnt)

/* Auxiliary function to efun replace_program(): check if program <str>
 * is inherited by <prg>. If yes, return the originating program and
 * store the (accumulated) variable and function offsets in offpnt[0]
 * and offpnt[1] resp.
 *
 * If the program is not found, return NULL.
 *
 * Nested inherits are handled in a depth search, the function recurses
 * for this.
 */

{
    struct program *tmp;
    int i;

#ifdef DEBUG
    if (d_flag)
    {
        debug_message("search_inherited started\n");
        debug_message("searching for PRG(%s) in PRG(%s)\n",str,prg->name);
        debug_message("num_inherited=%d\n", prg->num_inherited);
    }
#endif
    
    /* Loop through all inherited programs, returning directly when
     * the name program was found.
     */
    for ( i = 0; i < prg->num_inherited; i++)
    {
#ifdef DEBUG
        if (d_flag)
        {
            debug_message("index %d:\n",i);
            debug_message("checking PRG(%s)\n", prg->inherit[i].prog->name);
        }
#endif
        /* Duplicate virtual inherits don't count */
        if ( prg->inherit[i].is_extra )
            continue;
        
        if ( strcmp(str, prg->inherit[i].prog->name ) == 0 )
        {
#ifdef DEBUG
            if (d_flag)
                debug_message("match found\n");
#endif
            offpnt[0] = prg->inherit[i].variable_index_offset;
            offpnt[1] = prg->inherit[i].function_index_offset;
            return prg->inherit[i].prog;
        }
        else if ( NULL != (tmp = search_inherited(str, prg->inherit[i].prog,offpnt)) )
        {
#ifdef DEBUG
            if (d_flag)
                debug_message("deferred match found\n");
#endif
            offpnt[0] += prg->inherit[i].variable_index_offset;
            offpnt[1] += prg->inherit[i].function_index_offset;
            return tmp;
        }
    }
    
#ifdef DEBUG
    if (d_flag)
        debug_message("search_inherited failed\n");
#endif

    return NULL;
} /* search_inherited() */

/*-------------------------------------------------------------------------*/
static struct replace_ob *
retrieve_replace_program_entry (void)

/* Auxiliary function to efun replace_program(): test if a program
 * replacement is already scheduled for the current object. If yes,
 * return the pointer to the replace_ob struct, else return NULL.
 */

{
    struct replace_ob *r_ob;

    for (r_ob = obj_list_replace; r_ob; r_ob = r_ob->next)
    {
        if (r_ob->ob == current_object)
            return r_ob;
    }
    return NULL;
}

/*-------------------------------------------------------------------------*/
#ifdef DEBUG
static INLINE struct svalue *
find_value (int num)

/* Return the address of object-global variable number <num> in the
 * current variable block.
 *
 * <num> is the index of the variable in the current object's variable
 * array.
 */

{
    if (num >= current_object->prog->num_variables) {
        fatal("Illegal variable access %d(%d).\n",
            num, current_object->prog->num_variables);
    }
    return &current_variables[num];
}

#else

#define find_value(num) (&current_variables[(num)])

#endif

/*-------------------------------------------------------------------------*/
static INLINE struct svalue *
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
    struct inherit *inheritp;
    struct program *progp;
    char *progpp; /* actually a struct program **, but some compilers... */

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
    while (*(struct program **)progpp != progp)
        progpp += sizeof(struct inherit);
    inheritp = (struct inherit *)
                 (((PTRTYPE)(progpp)) - offsetof(struct inherit, prog));
#ifdef DEBUG
    /* TODO: Remove me if nobody reports an error. */
    {
        struct inherit * inheritp2 = (struct inherit *)(
                 ((PTRTYPE)(progpp))-
                 ((PTRTYPE)(&((struct inherit *)0)->prog)-(PTRTYPE) 0)
               );
        if (inheritp != inheritp2)
        {
            fprintf(stderr, "DEBUG: inheritp %p, inheritp2 %p, progpp %p\n"
                   , inheritp, inheritp2, progpp);
            inheritp = inheritp2;
        }
    }
#endif

    /* Compute the actual variable address */
    
    num += inheritp->variable_index_offset;
    return &current_object->variables[num];
      /* TODO: Why not '&current_variables[num]'? */
} /* find_virtual_value() */

/*-------------------------------------------------------------------------*/
static char *
add_slash (char *str)

/* Prepend a slash ('/') in front of string <str> and return an allocated
 * copy of the result.
 */

{
    char *tmp;

    tmp = xalloc(strlen(str)+2);
    if (tmp) {
        *tmp = '/';
        strcpy(tmp+1,str);
    }
    return tmp;
} /* add_slash() */


/*-------------------------------------------------------------------------*/
static void
bad_arg_pc (int arg, int instr, struct svalue *sp, bytecode_p pc)
  NORETURN;

static void
bad_arg_pc (int arg, int instr, struct svalue *sp, bytecode_p pc)

/* Argument number <arg> to instruction <instr> was "bad".
 * Set inter_sp/pc to <sp>/<pc> and raise a runtime error with
 * a descriptive message.
 *
 * TODO: It would be nice if the function would say why the argument was bad.
 * TODO:: In general, the should be a generic error function which is used
 * TODO:: even within The Large Switch - not this 'goto bad_arg_1;' crap.
 */

{
    ERRORF(("Bad argument %d to %s()\n", arg, get_f_name(instr)))
    /* NOTREACHED */
}

/*-------------------------------------------------------------------------*/
void
bad_efun_arg (int arg, int instr, struct svalue *sp)

/* Argument number <arg> to instruction <instr> was "bad".
 *
 * inter_sp is updated to <sp>, inter_pc is supposed to be correct (and
 * pointing just after the opcode).
 *
 * If <instr> is negative, it is used as an offset to inter_pc to find
 * the actual instruction code at <inter_pc>+<instr>.
 *
 * TODO: It would be nice if the function would say why the argument was bad.
 * TODO: This function depends on the order and values of F_ESCAPE,
 * TODO:: F_TEFUN and F_VEFUN.
 */

{
    inter_sp = sp;
    if (instr < 0)
    {
        /* Find and decode the actual instruction at the given offset */
        bytecode_p pc = inter_pc + instr;

        instr = *pc;
        if (instr <= F_VEFUN - F_OFFSET)
            instr = pc[1] | (instr << F_ESCAPE_BITS);
    }
    error("Bad argument %d to %s()\n", arg, get_f_name(instr));
}

/*-------------------------------------------------------------------------*/
void
bad_xefun_arg (int arg, struct svalue *sp)

/* Argument number <arg> to a xefun (fixed args) or tefun was "bad".
 *
 * inter_sp is updated to <sp>, inter_pc is supposed to be correct and
 * pointing just after the opcode.
 */

{
    bad_efun_arg(arg, -2, sp);
}

/*-------------------------------------------------------------------------*/
void
bad_xefun_vararg (int arg, struct svalue *sp)

/* Argument number <arg> to a xefun (var. args) or vefun was "bad".
 *
 * inter_sp is updated to <sp>, inter_pc is supposed to be correct and
 * pointing just after the opcode.
 */

{
    bad_efun_arg(arg, -3, sp);
}

/*-------------------------------------------------------------------------*/
Bool
_privilege_violation (char *what, struct svalue *where, struct svalue *sp)

/* Call the mudlib to check for a privilege violation:
 *
 *   master->privilege_violation(what, current_object, where)
 *
 * where <what> describes the type of the violation, and <where> is the
 * data used in the violation. <sp> is the current stack setting.
 * This function returns TRUE (operation allowed, _not_ a violation)
 * if the lfun returns a number > 0, or FALSE if the lfun returns 0.
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
    struct svalue *svp;

    /* Trusted objects */
    if (current_object == master_ob) return MY_TRUE;
    if (current_object == simul_efun_object) return MY_TRUE;
    
    /* Setup and call the lfun */
    push_volatile_string(what);
    push_valid_ob(current_object);
    sp++;
    assign_svalue_no_free(sp, where);
    inter_sp = sp;
    svp = apply_master_ob(STR_PRIVILEGE, 3);
   
    /* Is there a lfun to call? */
    if (!svp || svp->type != T_NUMBER || svp->u.number < 0)
    {
        inter_sp = sp-3;
        error("privilege violation: %s\n", what);
        /* TODO: Print full args and types */
    }

    /* Return the result */
    return svp->u.number ? MY_TRUE : MY_FALSE;
} /* _privilege_violation() */

/*-------------------------------------------------------------------------*/
Bool
privilege_violation4 ( char *what,    struct object *whom
                     , char *how_str, int how_num
                     , struct svalue *sp)

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
 * 
 * This function returns TRUE (operation allowed, _not_ a violation)
 * if the lfun returns a number > 0, or FALSE if the lfun returns 0.
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
    struct svalue *svp;

    /* Trust these objects */
    if (current_object == master_ob) return MY_TRUE;
    if (current_object == simul_efun_object) return MY_TRUE;

    /* Set up the lfun call */
    
    push_volatile_string(what);
    push_valid_ob(current_object);
    if (!whom)
    {
        push_volatile_string(how_str);
        push_number(how_num);
    }
    else
    {
        push_object(whom);
        if (how_str)
            push_volatile_string(how_str);
        else
            push_number(how_num);
    }
    inter_sp = sp;
    svp = apply_master_ob(STR_PRIVILEGE, 4);

    /* Was it the proper lfun to call? */
    if (!svp || svp->type != T_NUMBER || svp->u.number < 0)
    {
        inter_sp = sp-4;
        error("privilege violation : %s\n", what);
        /* TODO: Print full args and types */
    }

    /* Return the result */
    return svp->u.number ? MY_FALSE : MY_TRUE;
}

/*-------------------------------------------------------------------------*/
#define privilege_violation(what, where) (\
        inter_pc = pc,\
        _privilege_violation(what, where, sp)\
)
  /* Just as _privilege_violation(), just that it automatically uses
   * local <sp> and <pc> and updates the inter_ variables from them.
   */

/*-------------------------------------------------------------------------*/
static Bool
strpref (const char *p, const char *s)

/* Return TRUE if string <s> begins with string <p>, FALSE if not.
 * Used by the function trace_test().
 */

{
    while (*p)
        if (*p++ != *s++)
            return MY_FALSE;
    return MY_TRUE;
}

/*-------------------------------------------------------------------------*/
static Bool
trace_test (int b)

/* Test if tracing of the given option(s) <b> is allowed right now.
 * The function tests the options <b> against what the current interactive
 * requested, and if a trace_prefix is given, if the prefix matches the
 * name of the current object.
 */

{
    struct interactive *ip;

    return command_giver 
        && NULL != (ip = O_GET_INTERACTIVE(command_giver))
        && ip->sent.type == SENT_INTERACTIVE
        && (ip->trace_level & b)
        && (ip->trace_prefix == NULL
            || (current_object
                && strpref(ip->trace_prefix, current_object->name)))
    ;
} /* trace_test() */

/*-------------------------------------------------------------------------*/
static void
do_trace (char *msg, char *fname, char *post)

/* If not in a heartbeat, or if heartbeat tracing is allowed, generate
 * a tracemessage of the form '<tracedepth> <msg> <objname> <fname> <post>'
 * and print it to the player using add_message().
 *
 * <obj_name> is filled in only if TRACE_OBJNAME is requested, else
 * the empty string is used.
 */

{
    char buf[10000];
    char *objname;

    if (!TRACEHB)
        return;
    objname = TRACETST(TRACE_OBJNAME) 
              ? (current_object && current_object->name ? current_object->name 
                                                        : "??")  
              : "";
    sprintf(buf, "*** %d %*s %s %s %s%s", tracedepth, tracedepth, ""
               , msg, objname, fname, post);
    add_message(buf);
#ifdef DEBUG
    add_message(message_flush);
#endif
} /* do_trace() */

/*-------------------------------------------------------------------------*/
static void
do_trace_call (fun_hdr_p funstart)

/* Trace a call to the function starting at <funstart>.
 */

{
    char *name;

    if (!++traceing_recursion) /* Do not recurse! */
    {
        int save_var_ix_offset = variable_index_offset;
          /* TODO: Might be clobbered, but where? */

        /* Trace the function itself */
        memcpy(&name, &FUNCTION_NAME(funstart), sizeof name);
        do_trace("Call direct ", name, " ");

        /* If requested, also trace the arguments */
        if (TRACEHB)
        {
            if (TRACETST(TRACE_ARGS))
            {
                int i;
                struct svalue *svp;

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
do_trace_return (struct svalue *sp)

/* Trace the return from a function call; <sp> is the current stack pointer,
 * pointing to the result.
 */

{
    tracedepth--; /* We leave this level */

    if (!++traceing_recursion)
    {
        if (trace_test(TRACE_RETURN))
        {
            inter_sp = sp;
            do_trace("Return", "", "");
            if (TRACEHB) {
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
    (void)SET_TRACE_EXEC;
}

/*-------------------------------------------------------------------------*/
static INLINE struct con_struct *
push_error_context (struct svalue *sp)

/* Create a catch recovery context, using <sp> as the stackpointer to save,
 * link it into the recovery stack and return the longjmp context struct.
 */

{
    struct catch_context *p;

    p = (struct catch_context *)xalloc (sizeof *p);
    p->save_sp = sp;
    p->save_csp = csp;
    p->save_command_giver = command_giver;
    p->recovery_info.last = error_recovery_pointer;
    p->recovery_info.type = ERROR_RECOVERY_CATCH;
    error_recovery_pointer = &p->recovery_info;
    return &p->recovery_info.con;
} /* push_error_context() */

/*-------------------------------------------------------------------------*/
static INLINE void
pop_error_context (void)

/* Pop and discard the top entry in the error recovery stack, assuming
 * that it's a catch recovery entry.
 *
 * This function is called when the catch() completed normally.
 */

{
    struct catch_context *p;

    p = (struct catch_context *)error_recovery_pointer;

#ifdef DEBUG
    if (p->recovery_info.type != ERROR_RECOVERY_CATCH)
        fatal("Catch: error context stack underflow");
    if (csp != p->save_csp-1)
        fatal("Catch: Lost track of csp");
    /* Note: the command_giver might have changed (with the exec() efun),
     * so testing it is of no use.
     */
#endif
    error_recovery_pointer = p->recovery_info.last;
    xfree(p);
} /* pop_error_context() */

/*-------------------------------------------------------------------------*/
static struct svalue *
pull_error_context (struct svalue *sp)

/* Restore the context saved by a catch() after a throw() or runtime error
 * occured. <sp> is the current stackpointer and is used to pop the elements
 * pushed since the catch().
 *
 * The function pops the topmost recovery entry, which must be the catch
 * recovery entry, restores the important global variables and returns
 * the saved stack pointer.
 */

{
    struct catch_context *p;
    struct control_stack *csp2;

    p = (struct catch_context *)error_recovery_pointer;

    if (p->recovery_info.type != ERROR_RECOVERY_CATCH)
        fatal("Catch: error context stack underflow");

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

    /* Restore the global variables and the evaluator stack */
    csp = p->save_csp;
    pop_n_elems(sp - p->save_sp);
    command_giver = p->save_command_giver;

    /* Remove the context from the context stack */
    error_recovery_pointer = p->recovery_info.last;
    xfree(p);
    
    return sp;
} /* pop_error_context() */

/*-------------------------------------------------------------------------*/
INLINE static void 
push_control_stack ( struct svalue *sp
                   , bytecode_p     pc
                   , struct svalue *fp
                   )

/* Push the current execution context onto the control stack.
 * On stack overflow, raise a 'too deep recursion' error.
 */

{

    /* Check for overflow */
    if (csp >= &control_stack[MAX_USER_TRACE-1])
    {
        if (!num_error || csp == &control_stack[MAX_TRACE-1])
        {
            ERROR("Too deep recursion.\n")
        }
    }

    /* Move csp to the next entry and fill it with the current context
     */
    csp++;
    
    /* csp->funstart  has to be set later, it is used only for tracebacks. */
    csp->fp = fp;
    csp->prog = current_prog;
    /* csp->extern_call = MY_FALSE; It is set by eval_instruction() */
    csp->pc = pc;
    csp->function_index_offset = function_index_offset;
    csp->current_variables = current_variables;
    csp->break_sp = break_sp;
} /* push_control_stack() */

/*-------------------------------------------------------------------------*/
static void
pop_control_stack (void)

/* Pop the last entry from the control stack and restore the execution
 * context from it - except for extern_call of which the old value will
 * be used immediately after the pop.
 */

{
#ifdef DEBUG
    if (csp == control_stack - 1)
        fatal("Popped out of the control stack");
#endif

    if ( NULL != (current_prog = csp->prog) ) /* is 0 when we reach the bottom */
        current_strings = current_prog->strings;
    inter_pc = csp->pc;
    inter_fp = csp->fp;
    function_index_offset = csp->function_index_offset;
    current_variables     = csp->current_variables;
    break_sp = csp->break_sp;
    csp--;
} /* pop_control_stack() */


/*-------------------------------------------------------------------------*/
static INLINE /* TODO: funflags */ uint32
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
    struct program *progp;
    /* TODO: funflags */ uint32 flags;

    progp = current_prog;
    flags = progp->functions[fx];

    /* If the function is inherited, find the real function definition
     * and adjust the offsets to point to its code and variables.
     * This is an iteration walking along the inherit chain.
     */
    fun_ix_offs += fx;
    while(flags & NAME_INHERITED)
    {
        struct inherit *inheritp;

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
static INLINE struct svalue *
setup_new_frame2 (fun_hdr_p funstart, struct svalue *sp)

/* Before calling the function at <funstart>, massage the data on the
 * stack ending at <sp> to match the formal argumentlist of the function
 * (excessive args are removed, missing args are provided as 0),
 * and allocate the local variables on the stack.
 * 
 * csp->num_local_variables is supposed to hold the number of actual
 * arguments on the stack.
 *
 * Result is the new stackpointer, the framepointer <inter_fp>,
 * csp->num_local_variables and <break_sp> are set up.
 */

{
    short i;        /* Difference between number of formal and actual args;
                     * Number of (uninitialized) local variables
                     */
    int   num_arg;  /* Number of formal args */

    /* Setup the frame pointer */
    inter_fp = sp - csp->num_local_variables + 1;
    
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
                (++sp)->type = T_POINTER;
                sp->u.vec = allocate_uninit_array(0);
            }
            else
            {
                /* More actual than formal parameters */

                struct vector *v;

                csp->num_local_variables = num_arg;
                
                /* Move the extra args into an array and put that
                 * onto the stack
                 */
                v = allocate_uninit_array(i);
                while (--i >= 0)
                {
                    if (sp->type == T_LVALUE)
                        num_arg = -1; /* mark error condition */
                    v->item[i] = *sp--;
                }
                
                (++sp)->type = T_POINTER;
                sp->u.vec = v;
                
                if (num_arg < 0)
                {
                    bytecode_p pc = funstart; /* for the ERROR() macro */
                    
                    ERROR("Varargs argument passed by reference.\n");
                }
            }

            /* Clear the local variables */
            if ( 0 != (i = FUNCTION_NUM_VARS(funstart)) )
            {
                csp->num_local_variables += i;
                do {
                    *++sp = const0;
                } while (--i > 0);
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

    /* Check for stack overflow */
    if ( sp >= &start_of_stack[EVALUATOR_STACK_SIZE] )
        STACK_OVERFLOW(sp, csp->fp, funstart);

    /* Count the call depth for traces and handle tracing */
    tracedepth++;
    if (TRACEP(TRACE_CALL))
    {
      inter_sp = sp;
      do_trace_call(funstart);
    }


    /* Initialize the break stack, pointing to the entry above
     * the first available svalue.
     */
    break_sp = (bytecode_p *)&sp[1].u.string;

    return sp;
} /* setup_new_frame2() */

/*-------------------------------------------------------------------------*/
static /* TODO: funflag */ uint32
setup_new_frame (int fx)

/* Setup a call for function <fx> in the current program.
 * Result are the flags for the function.
 */

{
    /* TODO: funflag */ uint32 flags;

    flags = setup_new_frame1(fx, 0, 0);
    inter_sp = setup_new_frame2(
      current_prog->program + (flags & FUNSTART_MASK), inter_sp
    );
    current_variables = current_object->variables + variable_index_offset;
    current_strings = current_prog->strings;
    return flags;
}

/*-------------------------------------------------------------------------*/
#if !defined(SUPPLY_PARSE_COMMAND) || defined(COMPAT_MODE)
static
#endif
       void
check_for_destr (struct vector *v)

/* Check the vector <v> for destructed objects and replace them with
 * svalue 0s. Subvectors are not checked, though.
 *
 * This function is used by certain efuns (parse_command(), unique_array(),
 * map_array()) to make sure that the data passed to the efuns is valid,
 * avoiding game crashes (though this won't happen on simple operations
 * like assign_svalue).
 * TODO: The better way is to make the affected efuns resistant against
 * TODO:: destructed objects, and keeping this only as a safeguard and
 * TODO:: to save memory.
 */

{
    int i;
    struct svalue *p;

    for (p = v->item, i = VEC_SIZE(v); --i >= 0 ; p++ )
    {
        if (p->type != T_OBJECT)
            continue;
        if (!(p->u.ob->flags & O_DESTRUCTED))
            continue;
        zero_object_svalue(p);
    }
}

/*-------------------------------------------------------------------------*/
void
reset_machine (Bool first)

/* Reset the virtual machine. <first> is true on the very first call
 * (the cold boot, so to speak). Subsequent calls pass <first> as false
 * and this way make sure that all values currently on the stack
 * are properly removed.
 */

{
    csp = control_stack - 1;
    traceing_recursion = -1;
    if (first)
    {
        inter_sp = start_of_stack - 1;
        tracedepth = 0;
    }
    else
        inter_sp = _pop_n_elems(inter_sp - start_of_stack + 1, inter_sp);
}

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
    if (error_recovery_pointer->type != ERROR_RECOVERY_BACKEND) {
        debug_message("error_recovery_context_exists inconsistent!");
        return 1;
    }
    if (csp != control_stack - 1) {
        debug_message("csp inconsistent!");
        return 2;
    }
    if (inter_sp != start_of_stack - 1) {
        debug_message("sp inconsistent!");
        return 3;
    }
    return 0;
}
#endif

/*-------------------------------------------------------------------------*/
void
free_interpreter_temporaries (void)

/* Free all svalue the interpreter holds in global variables.
 * Usually the values are freed whenever a new value is stored, but
 * this function allows e.g. the garbage collector to free them all
 * at once.
 */

{
    free_protector_svalue(&last_indexing_protector);
    last_indexing_protector.type = T_NUMBER;
    free_svalue(&indexing_quickfix);
    indexing_quickfix.type = T_NUMBER;
    free_svalue(&apply_return_value);
    apply_return_value.type = T_NUMBER;
}

/*-------------------------------------------------------------------------*/
void
remove_object_from_stack (struct object *ob)

/* Object <ob> was/will be destructed, so remove all references from
 * the stack.
 */

{
    struct svalue *svp;

    for (svp = start_of_stack; svp <= inter_sp; svp++)
    {
        if (svp->type != T_OBJECT)
            continue;
        if (svp->u.ob != ob)
            continue;
        free_object(ob, "remove_object_from_stack");
        svp->type = T_NUMBER;
        svp->u.number = 0;
    }
}

/*-------------------------------------------------------------------------*/
static void
eval_instruction (bytecode_p first_instruction
                 , struct svalue *initial_sp)

/* Evaluate the code starting at <first_instruction>, using <inital_sp>
 * as the stack pointer. All other variables like current_prog must be
 * setup before the call. The function will return upon encountering
 * a F_RETURN instruction for which extern_call is true.
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
    register bytecode_p     pc;  /* Current program pointer */
    register struct svalue *fp;  /* Current frame pointer */
    register struct svalue *sp;  /* Current stack pointer */
      /* For speed reasons, these variables shadow their global counterparts,
       * allowing more optimisations.
       * gcc feels better about setjmp() when variables are declared register.
       * Still we might get 'variable foo might be clobbered' warnings, but
       * declaring them as volatile would degrade optimization, so we don't.
       */
    int num_arg;      /* Number of arguments given to the current instr */
    int instruction;  /* The current instruction code */
#ifdef DEBUG
    struct svalue *expected_stack; /* Expected stack at the instr end */
#endif

    /* Handy macros. Some of these are redefined later for multi-
     * byte instructions.
     */
#   ifdef DEBUG
#       define GET_NUM_ARG \
            if (num_arg != GET_UINT8(pc-1)) {\
                fatal("Argument count error for %s: %d vs. %d.\n", get_f_name(instruction), num_arg, GET_UINT8(pc-1));}
        /* What is the point of this macro? */
#   else /* DEBUG */
#       define GET_NUM_ARG num_arg = GET_UINT8(pc); pc++;
#   endif /* DEBUG */
      /* Get and/or test the number of arguments.
       */

#   define TYPE_TEST1(arg, t) if ( (arg)->type != t ) goto bad_arg_1;
#   define TYPE_TEST2(arg, t) if ( (arg)->type != t ) goto bad_arg_2;
#   define TYPE_TEST3(arg, t) if ( (arg)->type != t ) goto bad_arg_3;
#   define TYPE_TEST4(arg, t) if ( (arg)->type != t ) goto bad_arg_4;
      /* Test the type of a certain argument.
       */

#   ifdef MARK
#        define CASE(x) case (x)-F_OFFSET: MARK(x);
#   else
#        define CASE(x) case (x)-F_OFFSET:
#   endif
      /* Macro to build the case: labels for the evaluator switch.
       * Remember that the F_xxx tokens are used both in the compiler and,
       * minus F_OFFSET, as bytecodes.
       * 'MARK' adds profiling support.
       */

    /* Setup the variables.
     * The next F_RETURN at this level will return out of eval_instruction().
     */
    csp->extern_call = MY_TRUE;
    sp = initial_sp;
    pc = first_instruction;
    fp = inter_fp;
    (void)SET_TRACE_EXEC;

    /* ------ The evaluation loop ------ */
    
again:
    /* Get the next instruction and increment the pc */

    instruction = LOAD_CODE(pc);
      /* If this a xcode, the second byte will be added later */

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
        if (previous_objects[last])
        {
            /* Need to free the previously stored object */
            if (!--previous_objects[last]->ref)
            {
                previous_objects[last]->ref++;
                free_object(previous_objects[last], "TRACE_CODE");
            }
        }
        previous_objects[last] = current_object;
        current_object->ref++;  /* Don't forget this ref! */
        previous_programs[last] = current_prog;
#   endif  /* ifdef TRACE_CODE */
        
#   ifdef SMALLOC_LPC_TRACE
        inter_pc = pc;
#   endif

#   ifdef OPCPROF
        opcount[instruction]++;
#   endif

    /* Test the evaluation cost.
     */
    if (++eval_cost >= 0)
    {
        /* It could be a real 'too long eval', or just the trick used to
         * implement instruction tracing.
         */

        if (eval_cost >= MIN_TRACE_COST && eval_cost < MAX_TRACE_COST)
        {
            /* Instruction tracing: print the name of the instruction,
             * but guard against recursions.
             */
            if (TRACE_EXEC_P)
            {
                if (!++traceing_recursion)
                {
                    inter_sp = sp;
                    do_trace("Exec ", get_f_name(instruction), "\n");
                    instruction = EXTRACT_UCHAR(pc-1);
                }
                traceing_recursion--;
            }
        }
        else
        {
            /* Evaluation too long. Restore some globals and throw
             * an error.
             */

            if (eval_cost >= MAX_TRACE_COST)
            {
                /* EXEC tracing active: undo the change to eval_cost done
                 * before.
                 */
                eval_cost -= MAX_TRACE_COST;
                assigned_eval_cost -= MAX_TRACE_COST;
            }
            printf("eval_cost too big %ld\n", eval_cost - initial_eval_cost);

            assign_eval_cost();
            if (error_recovery_pointer->type <= ERROR_RECOVERY_BACKEND)
            {
                CLEAR_EVAL_COST;
            }
            inter_pc = pc;
            inter_fp = fp;
            ERROR("Too long evaluation. Execution aborted.\n")
        }
    }

#if defined(DEBUG)

    /* Get the expected number of arguments and determined the expected
     * stack setting.
     */
    if (instrs[instruction].min_arg != instrs[instruction].max_arg)
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

    if (num_arg != -1)
    {
        expected_stack = sp - num_arg +
            ( instrs[instruction].ret_type == TYPE_VOID ? 0 : 1 );
    }
    else
    {
        expected_stack = NULL;
    }
#endif /* DEBUG */

    /* The monster switch to execute the instruction.i
     * The order of the cases is held (mostly) in the order
     * the instructions appear in func_spec.
     */
    switch(instruction)
    {
    default:
        fatal("Undefined instruction %s (%d)\n", get_f_name(instruction),
              instruction);
        /* NOTREACHED */
bad_arg_1: bad_arg_pc(1, instruction, sp, pc);
bad_arg_2: bad_arg_pc(2, instruction, sp, pc);
bad_arg_3: bad_arg_pc(3, instruction, sp, pc);
bad_arg_4: bad_arg_pc(4, instruction, sp, pc);
bad_left:  ERRORF(("Bad left type to %s.\n",  get_f_name(instruction)))
bad_right: ERRORF(("Bad right type to %s.\n", get_f_name(instruction)))
        /* NOTREACHED */
        return; /* hint for data flow analysis */

#ifdef F_ILLEGAL
    case 255:
    CASE(F_ILLEGAL);                /* --- illegal             --- */
        inter_pc = pc;
        fatal("Illegal instruction\n");
        /* NOTREACHED */
#endif /* F_ILLEGAL */

    CASE(F_TEFUN);                  /* --- tefun <code>        --- */
    {
        /* Call the tabled efun 0x100 + <code>, where <code> is
         * a uint8 in the range 0x80..0xff.
         * The efun takes a fixed number of arguments which are
         * on the stack.
         */
      
        int code;
        
        code = LOAD_UINT8(pc);
#ifdef TRACE_CODE
        previous_instruction[last] = code + 0x100;
#endif
#ifdef OPCPROF
        opcount[code+0x100]++;
#endif
        inter_pc = pc;
        inter_sp = sp;
        ASSIGN_EVAL_COST
        sp = (*efun_table[code-128])(sp);
        break;
    }
    
    CASE(F_VEFUN);                 /* --- vefun <code> <nargs> --- */
    {
        /* Call the tabled efun 0x200 + <code>, where <code> is
         * a uint8 in the range 0x00..0x7f, with uint8 <nargs>
         * arguments on the stack.
         */
        int code;
        int numarg;

        code = LOAD_UINT8(pc);
        numarg = LOAD_UINT8(pc);
#ifdef TRACE_CODE
        previous_instruction[last] = code + 0x200;
#endif
#ifdef OPCPROF
        opcount[code+0x200]++;
#endif
        inter_pc = pc;
        inter_sp = sp;
        ASSIGN_EVAL_COST
        sp = (*vefun_table[code])(sp, numarg);
        break;
    }
    
    /* F_ESCAPE contains a sub-switch() and is handled at the
     * end.
     */

    /* --- Predefined functions with counterparts in LPC --- */
        
    CASE(F_IDENTIFIER);             /* --- identifier <var_ix> --- */
        /* Push value of object variable <var_ix>.
         * It is possible that it is a variable that points to
         * a destructed object. In that case, it has to be replaced by 0.
         *
         * <var_ix> is a uint8.
         */
        sp++;
        assign_checked_svalue_no_free(sp, find_value((int)(LOAD_UINT8(pc)))
                                     , sp, pc);
        break;

    CASE(F_STRING);                /* --- string <ix>          --- */
    {
        /* Push the string current_strings[<ix>] onto the stack,
         * <ix> being a (16-Bit) ushort, stored low byte first.
         * See also the F_CSTRINGx functions.
         */
        unsigned short string_number;

        LOAD_SHORT(string_number, pc);
        push_shared_string(current_strings[string_number]);
        break;
    }
    
    CASE(F_CSTRING3);               /* --- cstring3 <ix>       --- */
    {
        /* Push the string current_strings[0x3<ix>] onto the stack.
         * <ix> is a 8-Bit uint.
         */
        push_shared_string(current_strings[LOAD_UINT8(pc)+0x300]);
        break;
    }
    
    CASE(F_CSTRING2);               /* --- cstring2 <ix>       --- */
    {
        /* Push the string current_strings[0x2<ix>] onto the stack.
         * <ix> is a 8-Bit uint.
         */
        push_shared_string(current_strings[LOAD_UINT8(pc)+0x200]);
        break;
    }
    
    CASE(F_CSTRING1);               /* --- cstring1 <ix>       --- */
    {
        /* Push the string current_strings[0x1<ix>] onto the stack.
         * <ix> is a 8-Bit uint.
         */
        push_shared_string(current_strings[LOAD_UINT8(pc)+0x100]);
        break;
    }
    
    CASE(F_CSTRING0);               /* --- cstring0 <ix>       --- */
    {
        /* Push the string current_strings[0x0<ix>] onto the stack.
         * <ix> is a 8-Bit uint.
         */
        push_shared_string(current_strings[LOAD_UINT8(pc)]);
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
        push_number(0);
        break;
        
    CASE(F_CONST1);                 /* --- const1              --- */
        /* Push the number 1 onto the stack.
         */
        push_number(1);
        break;
        
    CASE(F_CLIT);                   /* --- clit <num>          --- */
    {
        /* Push the number <num> onto the stack.
         * <num> is a 8-Bit uint.
         */
        push_number((p_int)LOAD_UINT8(pc));
        break;
    }
    
    CASE(F_NCLIT);                  /* --- nclit <num>         --- */
    {
        /* Push the number -<num> onto the stack.
         * <num> is a 8-Bit uint.
         */
        push_number(-(p_int)LOAD_UINT8(pc));
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
         * TODO: It should be rewritten to use the LOAD_ macros (but
         * TODO:: then the compiler needs to use them, too.
         */

#if SIZEOF_P_INT == 4
        sp++;
        sp->type = T_FLOAT;

        memcpy((char *)&sp->u.mantissa, pc, sizeof(sp->u.mantissa));
        memcpy((char *)&sp->x.exponent, pc + sizeof(sp->u.mantissa), sizeof(sp->x.exponent));
        pc += sizeof(sp->u.mantissa)+sizeof(sp->x.exponent);
#else
        int32 mantissa;
        /* TODO: int16 */ short exponent;

        sp++;
        sp->type = T_FLOAT;

        memcpy((char *)&mantissa, pc, sizeof(mantissa));
        sp->u.mantissa = mantissa;

        memcpy((char *)&exponent, pc + sizeof(mantissa), sizeof(exponent));
        sp->x.exponent = exponent;
        pc += sizeof(mantissa)+sizeof(exponent);
#endif
        break;
    }

    CASE(F_CLOSURE);                /* --- closure <ix>        --- */
    {
        /* Push the closure value <ix> onto the stack.
         * <ix> is a uint16, stored low byte first.
         * Values 0xf000..0xffff are efun and simul-efun symbols, the others
         * are operators and literals.
         * Simul-efun symbols (0xf800..0xffff) and true efun symbolx (0xf000..
         * 0xf7ff for which instrs[].Default >= 0) are made signed and stored
         * as they are.
         * Operator symbols (0xf000..0xf7ff for which instrs[].Default == -1)
         * are moved into their 0xe800..0xefff range, then made signed and
         * stored.
         */

        /* TODO: uint16 */ unsigned short tmp_ushort;
        /* TODO: int32 */ int ix;

        LOAD_SHORT(tmp_ushort, pc);

        ix = tmp_ushort;
        if (ix < 0xf000)
        {
            sp++;
            closure_literal(sp, ix);
        }
        else
        {
            sp++;
            sp->type = T_CLOSURE;
            sp->x.closure_type = (short)
              (ix >= CLOSURE_SIMUL_EFUN_OFFS
                ? ix
                : (instrs[ix - CLOSURE_EFUN_OFFS].Default == -1
                    ? ix + CLOSURE_OPERATOR-CLOSURE_EFUN
                    : ix));
            add_ref(sp->u.ob = current_object, "closure");
        }
        break;
    }

    CASE(F_SYMBOL);                 /* --- symbol <ix> <num>   --- */
    {
        /* Push a symbol of current_strings[<ix>] with <num> quotes
         * onto the stack.
         * <ix> is a uint16, stored low byte first. <num> is a uint8.
         */

        char *str;
        /* TODO: uint16 */ unsigned short string_number;
        
        LOAD_SHORT(string_number, pc);
        
        sp++;
        sp->type = T_SYMBOL;
        sp->x.quotes = LOAD_UINT8(pc);
        sp->u.string = str = current_strings[string_number];
        increment_string_ref(str);
        break;
    }

    CASE(F_RETURN0);                /* --- return0             --- */
        /* Return from the function with result value 0.
         */
        push_number(0);
        /* FALLTHROUGH */
        
    CASE(F_RETURN);                 /* --- return              --- */
    {
        /* Return from the function with the result topmost on the stack.
         * If this is an .extern_call, eval_instruction() is left here.
         */
      
        struct svalue *svp; /* Save of current sp */

        svp = sp;

        /* Deallocate frame, but not the result value.
         */
#ifdef DEBUG
        if (fp + csp->num_local_variables != sp)
            fatal("Bad stack at F_RETURN\n");
#endif
        while (sp != fp)
            free_svalue(--sp);
        *sp = *svp;

        /* Restore the previous execution context */
        if ( NULL != (current_prog = csp->prog) ) /* is 0 when we reach the bottom */
            current_strings = current_prog->strings;
        function_index_offset = csp->function_index_offset;
        current_variables     = csp->current_variables;
        break_sp = csp->break_sp;

        if (csp->extern_call)
        {
            /* eval_instruction() must be left - setup the globals */
          
            ASSIGN_EVAL_COST
            current_object = csp->ob;
            previous_ob = csp->prev_ob;
            inter_pc = csp->pc;
            inter_fp = csp->fp;
            if (trace_level)
            {
                do_trace_return(sp);
                if (csp == control_stack - 2)
                    traceing_recursion = -1;
            }
            csp--;
            inter_sp = sp;
            return;
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
      
        pc = *break_sp;
        break_sp += sizeof(struct svalue)/sizeof(*break_sp);
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
         *  v[]: case values, char* or p_int, host byte order
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
         *   TODO: I guess that lookup ranges are used for an efficient
         *   TODO:: implementation of sparse ranges like 'case 0: case 2:
         *   TODO:: case 4:' or the like. 
         *   TODO: This code still makes too many un-macro'ed mem accesses.
         * 
         */

        mp_int offset;  /* Length of instruction and range-table area */
        mp_int def_offs;  /* Offset to code for the 'default' case */
        int tablen; /* Number of single case entries, multiplied by 4 */
        int len;  /* Number of bytes per offset/length value (1..3) */
        int type;  /* Start position for search */
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
        if ( !(len = tablen & 3) )
        {
            /* Oops, first lets align the switch */
            align_switch(pc);
            tablen = EXTRACT_UCHAR(pc);
            len = tablen & 3;
        }
        tablen &= ~3;

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
         * 
         * TODO: This code assumes sizeof(char*) == 4.
         */
        if (len > 1)
        {
            tablen += *(unsigned char *)(p0++) << 8;
            if (len > 2)
            {
                tablen += *(unsigned char *)(p0++) << 16;
#if SIZEOF_P_INT == 4
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
            union { unsigned char b[sizeof(p_int)-1]; short s; } abuf;
              /* TODO: Assumes sizeof(p_int)-1 >= sizeof(short) */
              /* TODO: Assumes sizeof(p_int) == 4 */
              /* TODO: Assumes sizeof(short) == 2 */

            /* Gather the bytes a0..a2 into abuf.b[] */
            b = ((p_int)p0-1) & sizeof abuf.b;
              /* The number of a-bytes after 'o*n' */
            memcpy((char *)abuf.b, p0, sizeof abuf.b);
            a = sizeof abuf.b - b;
              /* The number of remaining bytes */
            memcpy((char *)(abuf.b + a), (char *)(p1 + a), b);
            def_offs = abuf.s;
            type = abuf.b[2];
            if (len > 2)
            {
                def_offs += p1[3] << 16;
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
        break_sp -= sizeof(struct svalue)/sizeof(*break_sp);
        *break_sp = break_addr;

        /* Get the search value from the argument passed on the
         * stack. This also does the type checking.
         */
        if (type & 0x20)
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
                switch(sp->x.string_type)
                {
                case STRING_SHARED:
                    s = (mp_int)sp->u.string;
                    break;
                default:
                    s = (mp_int)findstring(sp->u.string);
                    break;
                }
            }
            else
            {
                /* No string - bad wizard! */
                goto bad_arg_1;
            }
        }
        else
        {
            /* Numeric switch */
          
            if (sp->type != T_NUMBER) goto bad_arg_1;
            s = sp->u.number;
        }
        pop_stack();

        /* Setup the binary search:
         *   l points roughly into the middle of the table,
         *   d is 1/4 of the (assumed) total size of the table
         */
        i = type & 0x1f;
        l = tabstart + off_tab[i];
        d = (off_tab[i]+sizeof(p_int)) >> 1 & ~(sizeof(p_int)-1);
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

        num_arg = LOAD_UINT8(pc);
          /* GET_NUM_ARG doesn't work here. Trust me. */
        inter_pc = pc;
        i = e_sscanf(num_arg, sp);
        pop_n_elems(num_arg-1);
        free_svalue(sp);
        put_number(i);
        break;
    }

#ifdef F_PARSE_COMMAND
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
        struct svalue *arg;

        assign_eval_cost();
        num_arg = LOAD_UINT8(pc);
          /* GET_NUM_ARG doesn't work here either. */
        inter_pc = pc;
        inter_sp = sp;
        arg = sp - num_arg + 1;
        if (arg[0].type != T_STRING)
            goto bad_arg_1;
        if (arg[1].type != T_OBJECT && arg[1].type != T_POINTER)
            goto bad_arg_2;
        if (arg[2].type != T_STRING)
            goto bad_arg_3;
        if (arg[1].type == T_POINTER)
            check_for_destr(arg[1].u.vec);

        i = parse(arg[0].u.string, &arg[1], arg[2].u.string, &arg[3],
                  num_arg-3);
        pop_n_elems(num_arg);        /* Get rid of all arguments */
        push_number(i);                /* Push the result value */
        break;
    }
#endif /* PARSE_COMMAND */

    CASE(F_LOCAL);                  /* --- local <ix>          --- */

        /* Fetch the value of local variable <ix> and push it
         * onto the stack.
         */
        sp++;
        assign_local_svalue_no_free(sp, fp + LOAD_UINT8(pc), sp, pc);
        break;

    CASE(F_CATCH);        /* --- catch <offset> <guarded code> --- */
    {
        /* catch(...instructions...)
         *
         * Execute the instructions (max. uint8 <offset> bytes) following the
         * catch statement. If an error occurs, or a throw() is executed,
         * catch that exception, push the <catch_value> (a global var)
         * onto the stack and continue execution at instruction
         * <pc>+1+<offset>.
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
        bytecode_p new_pc;  /* Address of first instruction after the catch() */

        /* Compute address of next instruction after the CATCH statement.
         */
        offset = LOAD_UINT8(pc);
        new_pc = pc + offset;

        /* Increase the eval_cost for the duration of the catch so that
         * there is enough time left to handle an eval-too-big error.
         */
        eval_cost += CATCH_RESERVED_COST;
        assigned_eval_cost += CATCH_RESERVED_COST;
        
        /* 'Fake' a subroutine call from <new_pc>
         */
        push_control_stack(sp, new_pc, fp);
        csp->ob = current_object;
        csp->extern_call = MY_FALSE;
#ifndef DEBUG
        csp->num_local_variables = 0;        /* No extra variables */
#else
        csp->num_local_variables = (csp-1)->num_local_variables;
          /* TODO: Marion added this, but why? For 'expected_stack'? */
#endif
        csp->funstart = csp[-1].funstart;
        
        /* Not really necessary, but tells gcc to complain less */
        inter_pc = pc;
        inter_sp = sp;
        inter_fp = fp;

        /* Save some globals on the error stack that must be restored
         * separately after a longjmp, then set the jump.
         */
        if ( setjmp( push_error_context(sp)->text ) )
        {
            /* A throw() or error occured. We have to restore the
             * control and error stack manually here.
             * 
             * The error value to return is stored in
             * the global <catch_value>.
             */
#ifdef DEBUG
            /* Restore the value of expected_stack also. It is always 0
             * for catch().
             */
            expected_stack = NULL;
#endif
            sp = pull_error_context(inter_sp);

            /* beware of errors after set_this_object() */
            current_object = csp->ob;

            pop_control_stack();
            pc = inter_pc;
            fp = inter_fp;
            
            /* Push the catch return value */
            *(++sp) = catch_value;
            catch_value.type = T_INVALID;

            /* Restore the old eval costs */
            eval_cost -= CATCH_RESERVED_COST;
            assigned_eval_cost -= CATCH_RESERVED_COST;
            

            /* If we are out of memory, throw a new error */
            if (out_of_memory)
            {
                inter_sp = sp;
                error("Out of memory\n");
            }

            /* Execution continues at the <new_pc> stored before */
        }
        else  /* catch() setup */
        {
            /* Not really necessary, but tells gcc to complain less */
            pc = inter_pc;
            sp = inter_sp;
            fp = inter_fp;
        }
        
        /* Not really necessary, but tells gcc to complain less */
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
      
        struct svalue *svp;

        /* Get the designated value */
#ifdef DEBUG
        if (sp->type != T_LVALUE)
            ERROR("Bad argument to ++: not a lvalue.\n")
            /* TODO: Give value and type */
#endif
        svp = sp->u.lvalue;

        /* Now increment where we can */
        if (svp->type == T_NUMBER)
        {
            svp->u.number++;
            sp--;
            break;
        }
        else if (svp->type == T_CHAR_LVALUE)
        {
            (*svp->u.string)++;
            /* TODO: This might shorten the string if *(svp->u.string) is 0xff.
             * TODO:: And since u.string is 'signed char', overflow behaviour
             * TODO:: isn't even defined.
             */
            sp--;
            break;
        }
        else if (svp->type == T_LVALUE
              || svp->type == T_PROTECTED_LVALUE)
        {
            inter_sp = sp;
            add_number_to_svalue(svp, 1);
            sp--;
            break;
        }

        ERROR("++ of non-numeric argument\n")
        /* TODO: Give type and value */
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
      
        struct svalue *svp;

        /* Get the designated value */
#ifdef DEBUG
        if (sp->type != T_LVALUE)
            ERROR("Bad argument to --: not a lvalue.\n")
            /* TODO: Give type and value */
#endif
        svp = sp->u.lvalue;

        /* Now decrement where we can */
        if (svp->type == T_NUMBER)
        {
            svp->u.number--;
            sp--;
            break;
        }
        else if (svp->type == T_CHAR_LVALUE)
        {
            (*svp->u.string)--;
            /* TODO: This might shorten the string if *(svp->u.string) is 0x01.
             * TODO:: And since u.string is 'signed char', underflow behaviour
             * TODO:: isn't even defined.
             */
            sp--;
            break;
        }
        else if (svp->type == T_LVALUE
              || svp->type == T_PROTECTED_LVALUE)
        {
            inter_sp = sp;
            add_number_to_svalue(svp, -1);
            sp--;
            break;
        }
        
        ERROR("-- of non-numeric argument\n")
        /* TODO: Give type and value */
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

        struct svalue *svp;

        /* Get the designated value */
#ifdef DEBUG
        if (sp->type != T_LVALUE)
            ERROR("Bad argument to ++: not a lvalue.\n")
            /* TODO: Give the type and value */
#endif
        svp = sp->u.lvalue;

        /* Do the push and increment */
        if (svp->type == T_NUMBER)
        {
            put_number( svp->u.number++ );
            break;
        }
        else if (svp->type == T_CHAR_LVALUE)
        {
            put_number( (*svp->u.string)++ );
            break;
        }
        else if (svp->type == T_LVALUE
              || svp->type == T_PROTECTED_LVALUE)
        {
            inter_sp = sp;
            put_number(add_number_to_svalue(svp, 1) - 1);
            break;
        }
        
        ERROR("++ of non-numeric argument\n")
        /* TODO: Give type and value */
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

        struct svalue *svp;

        /* Get the designated value */
#ifdef DEBUG
        if (sp->type != T_LVALUE)
            ERROR("Bad argument to --: not a lvalue.\n")
            /* TODO: Give the type and value */
#endif
        svp = sp->u.lvalue;
        
        /* Do the push and decrement */
        if (svp->type == T_NUMBER)
        {
            put_number( svp->u.number-- );
            break;
        }
        else if (svp->type == T_CHAR_LVALUE)
        {
            put_number( (*svp->u.string)-- );
            break;
        }
        else if (svp->type == T_LVALUE
              || svp->type == T_PROTECTED_LVALUE)
        {
            inter_sp = sp;
            put_number(add_number_to_svalue(svp, -1) + 1);
            break;
        }
        
        ERROR("-- of non-numeric argument\n")
        /* TODO: Give the type and value */
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

        struct svalue *svp;

        /* Get the designated value */
#ifdef DEBUG
        if (sp->type != T_LVALUE)
            ERROR("Bad argument to ++: not a lvalue\n")
            /* TODO: Give type and value */
#endif
        svp = sp->u.lvalue;

        /* Do the increment and push */
        if (svp->type == T_NUMBER)
        {
            put_number( ++(svp->u.number) );
            break;
        }
        else if (svp->type == T_CHAR_LVALUE)
        {
            put_number( ++(*svp->u.string) );
            break;
        }
        else if (svp->type == T_LVALUE
              || svp->type == T_PROTECTED_LVALUE)
        {
            inter_sp = sp;
            put_number(add_number_to_svalue(svp, 1));
            break;
        }
        
        ERROR("++ of non-numeric argument\n")
        /* TODO: Give type and value */
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

        struct svalue *svp;

        /* Get the designated value */
#ifdef DEBUG
        if (sp->type != T_LVALUE)
            ERROR("Bad argument to --: not a lvalue\n")
            /* TODO: Give the type and value */
#endif
        svp = sp->u.lvalue;

        /* Do the decrement and push */
        if (svp->type == T_NUMBER)
        {
            put_number( --(svp->u.number) );
            break;
        }
        else if (svp->type == T_CHAR_LVALUE)
        {
            put_number( --(*svp->u.string) );
            break;
        }
        else if (svp->type == T_LVALUE
              || svp->type == T_PROTECTED_LVALUE)
        {
            inter_sp = sp;
            put_number(add_number_to_svalue(svp, -1));
            break;
        }

        ERROR("-- of non-numeric argument\n")
        /* TODO: Give the type and value */
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
      
        struct svalue *dest;

        /* Get the designated lvalue */
#ifdef DEBUG
        if (sp->type != T_LVALUE)
            fatal("Bad argument to F_ASSIGN: not a lvalue\n");
            /* TODO: Give type and value */
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
         * VOID_ASSIGNs occur pretty often, so the implementation uses an
         * adopted copy of the transfer_svalue() code.
         *
         * Make sure that complex destinations like arrays are not freed
         * before the assignment is complete - see the comments to
         * assign_svalue().
         */
      
        struct svalue *dest;

        /* Get the designated value */
#ifdef DEBUG
        if (sp->type != T_LVALUE)
            fatal("Bad argument to F_VOID_ASSIGN: not a lvalue\n");
            /* TODO: Give type and value */
#endif
        dest = sp->u.lvalue;

        /* Free the destination svalue so that transfer_svalue_no_free_spc()
         * can be used. However, if the dest is a lvalue, a pointer or
         * mapping, the assignment takes place right here and the next
         * instruction will be executed by means of a 'goto again'.
         */
        switch(dest->type)
        {
        case T_STRING:
            switch(dest->x.string_type)
            {
            case STRING_MALLOC:
                xfree(dest->u.string);
                break;
            case STRING_SHARED:
                free_string(dest->u.string);
                break;
            }
            break;

        case T_OBJECT:
          {
            struct object *ob = dest->u.ob;
            free_object(ob, "void_assign");
            break;
          }

        case T_SYMBOL:
            free_string(dest->u.string);
            break;

        case T_CLOSURE:
            free_closure(dest);
            break;


        /* The cases below all assign right in the case block */
            
        case T_QUOTED_ARRAY:
        case T_POINTER:
          {
            struct vector *v = dest->u.vec;

            transfer_svalue_no_free_spc(dest, sp-1, sp, pc);
            sp -= 2;
            free_vector(v);
            goto again;
          }

        case T_MAPPING:
          {
            struct mapping *m = dest->u.map;

            transfer_svalue_no_free_spc(dest, sp-1, sp, pc);
            sp -= 2;
            free_mapping(m);
            goto again;
          }

        case T_CHAR_LVALUE:
          {
            if (sp[-1].type == T_NUMBER)
            {
                *dest->u.string = sp[-1].u.number;
            }
            else
            {
                free_svalue(sp-1);
            }
            sp -= 2;
            goto again;
          }
          
        /* the assignment class of operators always gets 'fresh' lvalues.
         * Thus, if we encounter a protected lvalue of any flavour, this is
         * due to a dereference of a reference stored in the original
         * lvalue, and the protected lvalue must not be freed.
         */

        case T_PROTECTED_CHAR_LVALUE:
          {
            struct protected_char_lvalue *p;

            p = (struct protected_char_lvalue *)dest;
            if (p->lvalue->type == T_STRING
             && p->lvalue->u.string == p->start)
            {
                if (sp[-1].type == T_NUMBER)
                {
                    *p->v.u.string = sp[-1].u.number;
                    sp -= 2;
                    goto again;
                }
            }
            sp--;
            pop_stack();
            goto again;
          }

        case T_POINTER_RANGE_LVALUE:
            transfer_pointer_range(sp-1);
            sp -= 2;
            goto again;

        case T_PROTECTED_POINTER_RANGE_LVALUE:
            transfer_protected_pointer_range(
              (struct protected_range_lvalue *)dest, sp-1
            );
            sp -= 2;
            goto again;

        case T_STRING_RANGE_LVALUE:
            inter_sp = sp;
            assign_string_range(sp-1, MY_TRUE);
            sp -= 2;
            goto again;

        case T_PROTECTED_STRING_RANGE_LVALUE:
            inter_sp = sp;
            assign_protected_string_range(
              (struct protected_range_lvalue *)dest, sp-1, MY_TRUE
            );
            sp -= 2;
            goto again;

        case T_LVALUE:
        case T_PROTECTED_LVALUE:
          {
            /* This may be a chain of LVALUEs - we might as well
             * call transfer_svalue() to deal with it
             */
            transfer_svalue(dest->u.lvalue, sp-1);
            sp -= 2;
            goto again;
          }
        }

        /* Nothing complicated: dest was just freed, now transfer
         * the data.
         */
        transfer_svalue_no_free_spc(dest, sp-1, sp, pc);
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
            switch ( sp->type )
            {
            case T_STRING:
              {
                char *res;
                int l = _svalue_strlen(sp-1);
                
                res = xalloc(l + _svalue_strlen(sp) + 1);
                if (!res)
                    ERROR("Out of memory\n")
                strcpy(res, (sp-1)->u.string);
                strcpy(res+l, sp->u.string);
                free_string_svalue(sp);
                sp--;
                free_string_svalue(sp);
                put_malloced_string(res, sp);
                break;
              }
              
            case T_NUMBER:
              {
                char buff[80];
                char *res;
                int len1;

                buff[sizeof(buff)-1] = '\0';
                sprintf(buff, "%ld", sp->u.number);
                if (buff[sizeof(buff)-1] != '\0')
                    FATAL("Buffer overflow in F_ADD: int number too big.\n")
                res = xalloc((len1 = svalue_strlen(sp-1)) + strlen(buff) + 1);
                if (!res)
                    ERROR("Out of memory\n")
                strcpy(res, (sp-1)->u.string);
                strcpy(res+len1, buff);
                pop_n_elems(2);
                push_malloced_string(res);
                break;
              }
              
            case T_FLOAT:
              {
                char buff[160];
                char *res;
                int len1;

                buff[sizeof(buff)-1] = '\0';
                sprintf(buff, "%g", READ_DOUBLE( sp ) );
                if (buff[sizeof(buff)-1] != '\0')
                    FATAL("Buffer overflow in F_ADD: float number too big.\n")
                res = xalloc((len1 = svalue_strlen(sp-1)) + strlen(buff) + 1);
                if (!res)
                    ERROR("Out of memory\n")
                strcpy(res, (sp-1)->u.string);
                strcpy(res+len1, buff);
                sp--;
                free_string_svalue(sp);
                put_malloced_string(res, sp);
                break;
              }

            default:
                goto bad_add;
            }
            break;
            /* End of case T_STRING */

          case T_NUMBER:
            switch ( sp->type )
            {
            case T_STRING:
              {
                char buff[80], *res;
                int len1;

                buff[sizeof(buff)-1] = '\0';
                sprintf(buff, "%ld", (sp-1)->u.number);
                if (buff[sizeof(buff)-1] != '\0')
                    FATAL("Buffer overflow in F_ADD: int number too big.\n")
                res = xalloc(svalue_strlen(sp) + (len1 = strlen(buff)) + 1);
                if (!res)
                    ERROR("Out of memory\n")
                strcpy(res, buff);
                strcpy(res+len1, sp->u.string);
                free_string_svalue(sp);
                sp--;
                put_malloced_string(res, sp);
                break;
              }

            case T_NUMBER:
              {
                int i;
                i = sp->u.number + (sp-1)->u.number;
                sp--;
                sp->u.number = i;
                break;
              }

            case T_FLOAT:
              {
                STORE_DOUBLE_USED
                double sum;

                sum = (double)((sp-1)->u.number) + READ_DOUBLE(sp);
                STORE_DOUBLE(sp-1, sum);
                sp--;
                sp->type = T_FLOAT;
                break;
              }

            default:
                goto bad_add;
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
                STORE_DOUBLE(sp-1, sum);
                sp--;
                break;
            }
            if (sp->type == T_NUMBER)
            {
                sum = READ_DOUBLE(sp-1) + (double)(sp->u.number);
                STORE_DOUBLE(sp-1, sum);
                sp--;
                break;
            }
            if (sp->type == T_STRING)
            {
                char buff[160];
                char *res;
                int len1;

                buff[sizeof(buff)-1] = '\0';
                sprintf(buff, "%g", READ_DOUBLE(sp-1) );
                if (buff[sizeof(buff)-1] != '\0')
                    FATAL("Buffer overflow in F_ADD: float number too big.\n")
                res = xalloc(svalue_strlen(sp) + (len1 = strlen(buff)) + 1);
                if (!res)
                    error("Out of memory\n");
                strcpy(res, buff);
                strcpy(res+len1, (sp)->u.string);
                free_string_svalue(sp);
                sp--;
                put_malloced_string(res, sp);
                break;
            }
            goto bad_add;
          }
          /* End of case T_FLOAT */

        case T_POINTER:
          {
            if (sp->type != T_POINTER) goto bad_add;
            inter_sp = sp;
            inter_pc = pc;
            inter_add_array(sp->u.vec, &(sp-1)->u.vec);
            sp--;
            break;
          }

        case T_MAPPING:
          {
            struct mapping *m;

            if (sp->type != T_MAPPING) goto bad_add;
            check_map_for_destr((sp-1)->u.map);
            check_map_for_destr(sp->u.map);
            m = add_mapping((sp-1)->u.map,sp->u.map);
            if (!m) {
                ERROR("Out of memory.n")
            }
            pop_n_elems(2);
            push_mapping(m); /* This will make ref count == 2 */
            m->ref--;
            break;
          }

        default:
        bad_add:
            ERROR("Bad type of arg to '+'\n")
            /* TODO: Give type, value and position. */
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
         *   vector      - vector             -> vector
         *   mapping     - mapping            -> mapping
         */
    
        int i;

        if ((sp-1)->type == T_NUMBER)
        {
            if (sp->type == T_NUMBER)
            {
                i = (sp-1)->u.number - sp->u.number;
                sp--;
                sp->u.number = i;
                break;
            }
            if (sp->type == T_FLOAT)
            {
                STORE_DOUBLE_USED
                double diff;

                diff = (double)((sp-1)->u.number) - READ_DOUBLE(sp);
                sp--;
                STORE_DOUBLE(sp, diff);
                sp->type = T_FLOAT;
                break;
            }
        }
        else if ((sp-1)->type == T_FLOAT)
        {
            STORE_DOUBLE_USED
            double diff;

            if (sp->type == T_FLOAT)
            {
                diff = READ_DOUBLE(sp-1) - READ_DOUBLE(sp);
                sp--;
                STORE_DOUBLE(sp, diff);
                break;
            }
            if (sp->type == T_NUMBER)
            {
                diff = READ_DOUBLE(sp-1) - (double)(sp->u.number);
                sp--;
                STORE_DOUBLE(sp, diff);
                break;
            }
        }
        else if ((sp-1)->type == T_POINTER && sp->type == T_POINTER)
        {
            struct vector *v;

            v = sp->u.vec;
            if (v->ref > 1)
            {
                v->ref--;
                v = slice_array(v, 0, VEC_SIZE(v) - 1 );
            }
            sp--;
            /* subtract_array already takes care of destructed objects */
            sp->u.vec = subtract_array(sp->u.vec, v);
            break;
        }
        else if ((sp-1)->type == T_MAPPING && sp->type == T_MAPPING)
        {
            struct mapping *m;

            m = subtract_mapping(sp[-1].u.map, sp->u.map);
            free_mapping(sp->u.map);
            sp--;
            free_mapping(sp->u.map);
            sp->u.map = m;
            break;
        }
        else
            goto bad_arg_1;

        goto bad_arg_2;
    }

    CASE(F_MULTIPLY);               /* --- multiply            --- */
    {
        /* Multiply sp[-1] by sp[0] pop both arguments from the stack
         * and push the result.
         * TODO: Could be extended to cover arrays and mappings.
         *
         * Possible type combinations:
         *   int         * int                -> int
         *   float       * (int,float)        -> float
         *   int         * float              -> float
         */
    
        int i;

        switch ( sp[-1].type )
        {
        case T_NUMBER:
            if (sp->type == T_NUMBER)
            {
                i = (sp-1)->u.number * sp->u.number;
                sp--;
                sp->u.number = i;
                break;
            }
            if (sp->type == T_FLOAT)
            {
                STORE_DOUBLE_USED
                double product;

                product = (sp-1)->u.number * READ_DOUBLE(sp);
                sp--;
                STORE_DOUBLE(sp, product);
                sp->type = T_FLOAT;
                break;
            }
            goto bad_arg_2;
        case T_FLOAT:
          {
            STORE_DOUBLE_USED
            double product;

            if (sp->type == T_FLOAT)
            {
                product = READ_DOUBLE(sp-1) * READ_DOUBLE(sp);
                STORE_DOUBLE(sp-1, product);
                sp--;
                break;
            }
            if (sp->type == T_NUMBER)
            {
                product = READ_DOUBLE(sp-1) * sp->u.number;
                STORE_DOUBLE(sp-1, product);
                sp--;
                break;
            }
            goto bad_arg_2;
          }
        default:
            goto bad_arg_1;
        }
        break;
    }

    CASE(F_DIVIDE);                 /* --- divide              --- */
    {
        /* Divide sp[-1] by sp[0] pop both arguments from the stack
         * and push the result.
         * TODO: Could be extended to cover arrays and mappings.
         *
         * Possible type combinations:
         *   int         / int                -> int
         *   float       / (int,float)        -> float
         *   int         / float              -> float
         */
    
        int i;

        if ((sp-1)->type == T_NUMBER)
        {
            if (sp->type == T_NUMBER) {
                if (sp->u.number == 0)
                    ERROR("Division by zero\n")
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
                    ERROR("Division by zero\n")
                sp--;
                dtmp = sp->u.number / dtmp;
                STORE_DOUBLE(sp, dtmp);
                sp->type = T_FLOAT;
                break;
            }
            goto bad_arg_2;
        }
        else if ((sp-1)->type == T_FLOAT)
        {
            double dtmp;
            STORE_DOUBLE_USED

            if (sp->type == T_FLOAT)
            {
                dtmp = READ_DOUBLE( sp );
                if (dtmp == 0.) {
                    ERROR("Division by zero\n")
                    return;
                }
                sp--;
                dtmp = READ_DOUBLE(sp) / dtmp;
                STORE_DOUBLE(sp, dtmp);
                break;
            }
            if (sp->type == T_NUMBER)
            {
                if (sp->u.number == 0) {
                    ERROR("Division by zero\n")
                    return;
                }
                dtmp = (float)sp->u.number;
                sp--;
                dtmp = READ_DOUBLE(sp) / dtmp;
                STORE_DOUBLE(sp, dtmp);
                break;
            }
            goto bad_arg_2;
        }
        goto bad_arg_1;
        break;
    }
    
    CASE(F_MOD);                    /* --- mod                 --- */
    {
        /* Compute sp[-1] modulus sp[0] pop both arguments from the stack
         * and push the result.
         * TODO: Could be extended to cover arrays and mappings.
         * TODO: Define properly and add the rem operation.
         *
         * Possible type combinations:
         *   int         % int                -> int
         */
    
        int i;

        if ((sp-1)->type != T_NUMBER)
            goto bad_arg_1;
        if (sp->type != T_NUMBER)
            goto bad_arg_2;
        if (sp->u.number == 0)
        {
            ERROR("Modulus by zero.\n")
            return;
        }
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
            i = strcmp((sp-1)->u.string, sp->u.string) > 0;
            free_string_svalue(sp);
            sp--;
            free_string_svalue(sp);
            put_number(i);
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
            sp->type = T_NUMBER;
            sp->u.number = i;
            break;
        }
        
        if (!( (sp-1)->type & (T_NUMBER|T_STRING|T_FLOAT) ))
            goto bad_arg_1;
        if (!(  sp   ->type & (T_NUMBER|T_STRING|T_FLOAT) ))
            goto bad_arg_2;
        ERROR("Arguments to > don't match\n")
        /* TODO: Give type and value */
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
            i = strcmp((sp-1)->u.string, sp->u.string) >= 0;
            free_string_svalue(sp);
            sp--;
            free_string_svalue(sp);
            put_number(i);
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
            sp->type = T_NUMBER;
            sp->u.number = i;
            break;
        }
        
        if (!( (sp-1)->type & (T_NUMBER|T_STRING|T_FLOAT) ))
            goto bad_arg_1;
        if (!(  sp   ->type & (T_NUMBER|T_STRING|T_FLOAT) ))
            goto bad_arg_2;
        ERROR("Arguments to >= don't match\n")
        /* TODO: Give type and value */
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
            i = strcmp((sp-1)->u.string, sp->u.string) < 0;
            free_string_svalue(sp);
            sp--;
            free_string_svalue(sp);
            put_number(i);
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
            sp->type = T_NUMBER;
            sp->u.number = i;
            break;
        }
        
        if (!( (sp-1)->type & (T_NUMBER|T_STRING|T_FLOAT) ))
            goto bad_arg_1;
        if (!(  sp   ->type & (T_NUMBER|T_STRING|T_FLOAT) ))
            goto bad_arg_2;
        ERROR("Arguments to < don't match\n")
        /* TODO: Give error and type */
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
            i = strcmp((sp-1)->u.string, sp->u.string) <= 0;
            free_string_svalue(sp);
            sp--;
            free_string_svalue(sp);
            put_number(i);
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
            sp->type = T_NUMBER;
            sp->u.number = i;
            break;
        }

        if (!( (sp-1)->type & (T_NUMBER|T_STRING|T_FLOAT) ))
            goto bad_arg_1;
        if (!(  sp   ->type & (T_NUMBER|T_STRING|T_FLOAT) ))
            goto bad_arg_2;
        
        ERROR("Arguments to <= don't match\n")
        /* TODO: Give type and value */
    }
    
    CASE(F_EQ);                     /* --- eq                  --- */
    {
        /* Test if sp[-1] == sp[0]. If yes, push 1 onto the stack,
         * else 0 (of course after popping both arguments).
         *
         * Comparable types are all types, each to its own. Comparisons
         * between distinct types always yield 'unequal'.
         * Vectors and mappings are compared by ref only.
         */
      
        int i;

        if ((sp-1)->type != sp->type)
        {
            pop_stack();
            free_svalue(sp);
            put_number(0);
            break;
        }
        
        switch(sp->type)
        {
        case T_NUMBER:
            i = (sp-1)->u.number == sp->u.number;
            break;
        case T_POINTER:
            i = (sp-1)->u.vec == sp->u.vec;
            break;
        case T_STRING:
            i = strcmp((sp-1)->u.string, sp->u.string) == 0;
            break;
        case T_OBJECT:
            i = (sp-1)->u.ob == sp->u.ob;
            break;
        case T_FLOAT:
            /* This is of little use... well, at least 0. == 0. ... */
        case T_CLOSURE:
        case T_SYMBOL:
        case T_QUOTED_ARRAY:
            i = (sp-1)->u.string  == sp->u.string &&
                (sp-1)->x.generic == sp->x.generic;
            break;
        case T_MAPPING:
            i = (sp-1)->u.map == sp->u.map;
            break;
        default:
            if (sp->type == T_LVALUE)
                error("Reference passed to !=\n");
            fatal("Illegal type to !=\n");
              /* TODO: Give type and value */
            /* NOTREACHED */
            return;
        }

        pop_stack();
        free_svalue(sp);
        put_number(i);
        break;
    }

    CASE(F_NE);                     /* --- ne                  --- */
    {
        /* Test if sp[-1] != sp[0]. If yes, push 1 onto the stack,
         * else 0 (of course after popping both arguments).
         *
         * Comparable types are all types, each to its own. Comparisons
         * between distinct types always yield 'unequal'.
         * Vectors and mappings are compared by ref only.
         */
      
        int i;

        if ((sp-1)->type != sp->type)
        {
            pop_stack();
            assign_svalue(sp, &const1);
            break;
        }

        switch(sp->type)
        {
        case T_NUMBER:
            i = (sp-1)->u.number != sp->u.number;
            break;
        case T_STRING:
            i = strcmp((sp-1)->u.string, sp->u.string);
            break;
        case T_POINTER:
            i = (sp-1)->u.vec != sp->u.vec;
            break;
        case T_OBJECT:
            i = (sp-1)->u.ob != sp->u.ob;
            break;
        case T_FLOAT:
            /* This is of little use... well, at least 0. == 0. ... */
        case T_CLOSURE:
        case T_SYMBOL:
        case T_QUOTED_ARRAY:
            i = (sp-1)->u.string  != sp->u.string ||
                (sp-1)->x.generic != sp->x.generic;
            break;
        case T_MAPPING:
            i = (sp-1)->u.map != sp->u.map;
            break;
        default:
            if (sp->type == T_LVALUE)
                error("Reference passed to !=\n");
            fatal("Illegal type to !=\n");
            /* NOTREACHED */
            return;
        }
        
        pop_stack();
        free_svalue(sp);
        put_number(i);
        break;
    }

    CASE(F_COMPL);                  /* --- compl               --- */
        /* Compute the binary complement of number sp[0] and leave
         * that on the stack.
         */
        if (sp->type != T_NUMBER)
            ERROR("Bad argument to ~\n")
            /* TODO: Give type and value */
        sp->u.number = ~ sp->u.number;
        break;
        
    CASE(F_AND);                    /* --- and                 --- */
    {
        /* Compute the intersection of sp[-1] and sp[0] and leave
         * the result on the stack.
         *
         * Possible type combinations:
         *   int    & int    -> int
         *   vector & vector -> vector
         *
         * TODO: Extend this to mappings.
         */
      
        int i;

        if (sp->type == T_POINTER && (sp-1)->type == T_POINTER)
        {
            inter_sp = sp - 2;
            (sp-1)->u.vec = intersect_array(sp->u.vec, (sp-1)->u.vec);
            sp--;
            break;
        }
        
        if ((sp-1)->type != T_NUMBER)
            goto bad_arg_1;
        if (sp->type != T_NUMBER)
            goto bad_arg_2;

        i = (sp-1)->u.number & sp->u.number;
        sp--;
        sp->u.number = i;
        break;
    }
    
    CASE(F_OR);                     /* --- or                  --- */
    {
        /* Compute the binary-or of sp[-1] and sp[0] and leave
         * the result on the stack.
         *
         * Possible type combinations:
         *   int    | int    -> int
         *
         * TODO: Extend this to vectors and mappings.
         */
      
        int i;

        if ((sp-1)->type != T_NUMBER)
            goto bad_arg_1;
        if (sp->type != T_NUMBER)
            goto bad_arg_2;
        i = (sp-1)->u.number | sp->u.number;
        sp--;
        sp->u.number = i;
        break;
    }
    
    CASE(F_XOR);                    /* --- xor                 --- */
    {
        /* Compute the binary-xor of sp[-1] and sp[0] and leave
         * the result on the stack.
         *
         * Possible type combinations:
         *   int ^ int    -> int
         *
         * TODO: Extend this to vectors and mappings.
         */
      
        int i;

        if ((sp-1)->type != T_NUMBER)
            goto bad_arg_1;
        if (sp->type != T_NUMBER)
            goto bad_arg_2;
        
        i = (sp-1)->u.number ^ sp->u.number;
        sp--;
        sp->u.number = i;
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
      
        int i;

        if ((sp-1)->type != T_NUMBER)
            goto bad_arg_1;
        if (sp->type != T_NUMBER)
            goto bad_arg_2;

        i = sp->u.number;
        sp--;
        sp->u.number = (uint)i > MAX_SHIFT ? 0 : sp->u.number << i;
        break;
    }

    CASE(F_RSH);                    /* --- rsh                 --- */
    {
        /* Shift number sp[-1] right by sp[0] bits and leave
         * the result on the stack.
         *
         * Possible type combinations:
         *   int >> int    -> int
         *
         * TODO: Extend this to vectors and mappings.
         * TODO: Implement an arithmetic shift.
         */
      
        int i;

        if ((sp-1)->type != T_NUMBER)
            goto bad_arg_1;
        if (sp->type != T_NUMBER)
            goto bad_arg_2;
        
        i = sp->u.number;
        sp--;
        sp->u.number >>= (uint)i > MAX_SHIFT ? MAX_SHIFT : i;
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
        put_number(0);
        break;

    CASE(F_RANGE);                  /* --- range               --- */
    CASE(F_NR_RANGE);               /* --- nr_range            --- */
    CASE(F_RN_RANGE);               /* --- rn_range            --- */
    CASE(F_RR_RANGE);               /* --- rr_range            --- */
      {
        /* Compute the range sp[-1]..sp[0] from string/array sp[-2]
         * and leave it on the stack.
         */

        if (sp[-1].type != T_NUMBER)
            ERROR("Bad type of start interval to [..] range.\n")
            /* TODO: Give type and value */
        if (sp[0].type != T_NUMBER)
            ERROR("Bad type of end interval to [..] range.\n")
            /* TODO: Give type and value */

        if (sp[-2].type == T_POINTER)
        {
            /* Slice a range from an array */

            struct vector *v;
            int size, i1, i2;

            size = VEC_SIZE(sp[-2].u.vec);
            
            if (instruction == F_RANGE-F_OFFSET
             || instruction == F_NR_RANGE-F_OFFSET)
                i1 = sp[-1].u.number;
            else
                i1 = size - sp[-1].u.number;

            if (instruction == F_RANGE-F_OFFSET
             || instruction == F_RN_RANGE-F_OFFSET)
                i2 = sp[0].u.number;
            else
                i2 = size - sp[0].u.number;
            if (i2 >= size)
                i2 = size - 1;

            pop_stack();
            pop_stack();
            
            v = slice_array(sp->u.vec, i1, i2);
            
            free_vector(sp->u.vec);
            if (v)
            {
                sp->u.vec = v;
            }
            else
            {
                put_number(0);
            }
        }
        else if (sp[-2].type == T_STRING)
        {
            /* Slice a range from string */

            int len, from, to;
            char *res;

            len = svalue_strlen(&sp[-2]);
            if (instruction == F_RANGE-F_OFFSET
             || instruction == F_NR_RANGE-F_OFFSET)
                from = sp[-1].u.number;
            else
                from = len - sp[-1].u.number;
            if (from < 0)
            {
                from = 0;
            }
            
            if (instruction == F_RANGE-F_OFFSET
             || instruction == F_RN_RANGE-F_OFFSET)
                to = sp[0].u.number;
            else
                to = len - sp[0].u.number;
            if (to >= len)
                to = len-1;

            if (to < from)
            {
                pop_n_elems(3);
                push_constant_string("");
                break;
            }

            if (to == len-1)
            {
                res = string_copy(sp[-2].u.string + from);
                pop_n_elems(3);
                push_malloced_string(res);
                break;
            }

            res = xalloc(to - from + 2);
            strncpy(res, sp[-2].u.string + from, to - from + 1);
            res[to - from + 1] = '\0';
            pop_n_elems(3);
            push_malloced_string(res);
        }
        else
        {
            ERROR("Bad argument to [..] range operand: neither string nor array.\n")
            /* TODO: Give type and value */
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
         *   string      + (string,int,float) -> string
         *   int         + int                -> int
         *   float       + (float,int)        -> float
         *   vector      + vector             -> vector
         *   mapping     + mapping            -> mapping
         */

        short type2;         /* type and value of sp[-1] */
        union u u2;
        struct svalue *argp; /* the actual value of sp[0] */

        type2 = sp[-1].type;
        u2 = sp[-1].u;

#ifdef DEBUG
        if (sp->type != T_LVALUE)
            goto bad_arg_1;
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
            char *new_string;
            
            /* Perform the addition, creating new_string */
            if (type2 == T_STRING)
            {
                int l = _svalue_strlen(argp);
                
                if ( !(new_string = xalloc(l + strlen(u2.string) + 1)) )
                    ERROR("Out of memory\n");
                strcpy(new_string, argp->u.string);
                strcpy(new_string+l, u2.string);
                free_string_svalue(sp-1);
                sp -= 2;
            }
            else if (type2 == T_NUMBER)
            {
                char buff[80];
                
                buff[sizeof(buff)-1] = '\0';
                sprintf(buff, "%ld", (long)u2.number);
                if (buff[sizeof(buff)-1] != '\0')
                    FATAL("Buffer overflow in F_ADD_EQ: int number too big.\n")
                if ( !(new_string =
                       xalloc(svalue_strlen(argp) + strlen(buff) + 1)) )
                    ERROR("Out of memory\n");
                strcpy(new_string, argp->u.string);
                strcat(new_string, buff);
                sp -= 2;
            }
            else if (type2 == T_FLOAT)
            {
                char buff[160];
                
                buff[sizeof(buff)-1] = '\0';
                sprintf(buff, "%g", READ_DOUBLE(sp-1) );
                if (buff[sizeof(buff)-1] != '\0')
                    FATAL("Buffer overflow in F_ADD_EQ: float number too big.\n")
                if ( !(new_string =
                       xalloc(svalue_strlen(argp) + strlen(buff) + 1)) )
                    ERROR("Out of memory\n");
                strcpy(new_string, argp->u.string);
                strcat(new_string, buff);
                sp -= 2;
            }
            else
            {
                goto bad_arg_2;
            }

            /* Replace *argp by the new string */
            free_string_svalue(argp);
            argp->x.string_type = STRING_MALLOC;
            argp->u.string = new_string;
            break;
          }
          
        case T_NUMBER:  /* Add to a number */
            if (type2 == T_NUMBER)
            {
                if (instruction == F_VOID_ADD_EQ - F_OFFSET)
                {
                    argp->u.number += u2.number;
                    sp -= 2;
                    goto again;
                }
                (--sp)->u.number = argp->u.number += u2.number;
                goto again;
            }
            else
            {
                ERROR("Bad type number to rhs +=.\n")
                /* TODO: Give type and value */
            }
            break;
            
        case T_CHAR_LVALUE:  /* Add to a character in a string */
            if (type2 == T_NUMBER)
            {
                if (instruction == F_VOID_ADD_EQ - F_OFFSET)
                {
                    *argp->u.string += u2.number;
                    sp -= 2;
                    goto again;
                }
                (--sp)->u.number = *argp->u.string += u2.number;
                /* TODO: This might create a 0x00 character in a string */
                goto again;
            }
            else
            {
                ERROR("Bad type number to rhs +=.\n")
                /* TODO: Give type and value */
            }
            break;
            
        case T_MAPPING:  /* Add to a mapping */
            if (type2 != T_MAPPING)
            {
                ERROR("Bad type to rhs +=.\n")
                /* TODO: Give type and value */
            }
            else
            {
                check_map_for_destr(u2.map);
                add_to_mapping(argp->u.map, u2.map);
                sp -= 2;
                free_mapping(u2.map);
            }
            break;

        case T_POINTER:  /* Add to an array */
            if (type2 != T_POINTER)
            {
                ERROR("Bad type to rhs +=.\n")
                /* TODO: Give type and value */
            }
            else
            {
                struct vector *v;
                
                inter_sp = sp;
                inter_pc = pc;
                v = inter_add_array(u2.vec, &argp->u.vec);
                if (instruction == F_VOID_ADD_EQ - F_OFFSET)
                {
                    sp -= 2;
                    goto again;
                }
                sp--;
                sp->u.vec = v;
                v->ref++;
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
                STORE_DOUBLE(argp, d);
                sp -= 2;
            }
            else if (type2 == T_NUMBER)
            {
                STORE_DOUBLE_USED
                double d;

                d = READ_DOUBLE(argp) + (double)sp[-1].u.number;
                STORE_DOUBLE(argp, d);
                sp -= 2;
            }
            else
            {
                goto bad_right;
            }
            break;

        default:
            ERROR("Bad type to lhs +=\n")
            /* TODO: Give type and value */
        } /* end of switch */

        /* If the instruction is F_ADD_EQ, leave the result on the stack */
        if (instruction != F_VOID_ADD_EQ - F_OFFSET)
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
         *
         * Possible type combinations:
         *   int         - int                -> int
         *   float       - (float,int)        -> float
         *   vector      - vector             -> vector
         *   mapping     - mapping            -> mapping
         */

        short type2;         /* type and value of sp[-1] */
        union u u2;
        struct svalue *argp; /* the actual value of sp[0] */

        type2 = sp[-1].type;
        u2 = sp[-1].u;

#ifdef DEBUG
        if (sp->type != T_LVALUE)
            goto bad_arg_1;
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
            if (type2 != T_NUMBER)
                goto bad_right;
            sp--;
            sp->u.number = argp->u.number -= u2.number;
            break;
            
        case T_CHAR_LVALUE:  /* Subtract from a char in a string */
            if (type2 != T_NUMBER)
                goto bad_right;
            sp--;
            sp->u.number = *argp->u.string -= u2.number;
            /* TODO: May result in a 0x00 character in a string */
            break;
            
        case T_POINTER:  /* Subtract from an array */
          {
            struct vector *v, *v_old;

            if (type2 != T_POINTER)
                goto bad_right;
            
            v = u2.vec;

            /* Duplicate the minuend array if necessary, as
             * the subtraction will change and free it
             */
            if (v->ref > 1)
            {
                v->ref--;
                v = slice_array(v, 0, VEC_SIZE(v)-1 );
            }
            sp--;
            v_old = argp->u.vec;
            v = subtract_array(v_old, v);
            argp->u.vec = v;
            put_vector(v);
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
                STORE_DOUBLE(argp, d);
                *sp = *argp;
            }
            else if (type2 == T_NUMBER)
            {
                STORE_DOUBLE_USED
                double d;

                sp--;
                d = READ_DOUBLE(argp) - (double)sp->u.number;
                STORE_DOUBLE(argp, d);
                *sp = *argp;
            }
            else
            {
                goto bad_right;
            }
            break;
            
        case T_MAPPING:  /* Subtract from a mapping */
            if (type2 == T_MAPPING && !sp[-1].u.map->num_values)
            {
                struct mapping *m;

                sp--;
                m = sp->u.map;

                /* Test for the special case 'm - m' */
                if (m == argp->u.map)
                {
                    /* m->ref is > 1, because the content of the lvalue is
                     * associated with a ref
                     */
                    m->ref--;
                    m = copy_mapping(m);
                }
                
                walk_mapping(m, sub_from_mapping_filter, argp->u.map);
                free_mapping(m);
                (sp->u.map = argp->u.map)->ref++;
            }
            else
            {
                goto bad_right;
            }
            break;

        default:
            goto bad_left;
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
         *
         * TODO: Extend this to arrays and mappings.
         */

        struct svalue *argp;
        
#ifdef DEBUG
        if (sp->type != T_LVALUE)
            goto bad_arg_1;
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
                goto bad_right;
            sp->u.number = argp->u.number *= sp->u.number;
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
                STORE_DOUBLE(argp, d);
                *sp = *argp;
            }
            else if (sp->type == T_NUMBER)
            {
                d = READ_DOUBLE(argp) * (double)sp->u.number;
                STORE_DOUBLE(argp, d);
                *sp = *argp;
            }
            else
                goto bad_right;
            break;
        }
        goto bad_left;
        /* NOTREACHED */ break;
    }

    CASE(F_DIV_EQ);                 /* --- div_eq              --- */
    {
        /* Divide the value designated by lvalue sp[0] by sp[-1],
         * assign the result to sp[0] and also leave it on the stack.
         *
         * Possible type combinations:
         *   int         / int                -> int
         *   float       / (float,int)        -> float
         *
         * TODO: Extend this to arrays and mappings.
         */

        struct svalue *argp;

#ifdef DEBUG
        if (sp->type != T_LVALUE)
            goto bad_arg_1;
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
                goto bad_right;
            if (sp->u.number == 0)
                ERROR("Division by 0\n")
            sp->u.number = argp->u.number /= sp->u.number;
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
                    ERROR("Division by 0\n")
                d = READ_DOUBLE(argp) / d;
                STORE_DOUBLE(argp, d);
                *sp = *argp;
            }
            else if (sp->type == T_NUMBER)
            {
                p_int i;
                i = sp->u.number;
                if (i == 0)
                    ERROR("Division by 0\n")
                d = READ_DOUBLE(argp) / (double)i;
                STORE_DOUBLE(argp, d);
                *sp = *argp;
            }
            else
                goto bad_right;
            break;
        }
        goto bad_left;
        /* NOTREACHED */ break;
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

        struct svalue *argp;
        
#ifdef DEBUG
        if (sp->type != T_LVALUE)
            goto bad_arg_1;
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
                goto bad_right;
            if (sp->u.number == 0)
                ERROR("Division by 0\n")
            sp->u.number = argp->u.number %= sp->u.number;
            break;
        }
        goto bad_left;
        /* NOTREACHED */ break;
    }

    CASE(F_AND_EQ);                 /* --- and_eq              --- */
    {
        /* Intersect the value designated by lvalue sp[0] with sp[-1],
         * assign the result to sp[0] and also leave it on the stack.
         *
         * Possible type combinations:
         *   int         & int                -> int
         *   array       & array              -> array
         *
         * TODO: Extend this to mappings.
         */

        struct svalue *argp;

#ifdef DEBUG
        if (sp->type != T_LVALUE)
            goto bad_arg_1;
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
                goto bad_right;
            sp--;
            sp->u.number = argp->u.number &= sp->u.number;
            break;
        }
        
        if (argp->type == T_POINTER && sp[-1].type == T_POINTER)
        {
            /* Intersect an array */

            struct vector *vec1, *vec2;

            inter_sp = sp - 2;
            vec1 = argp->u.vec;
            vec2 = sp[-1].u.vec;
            argp->type = T_NUMBER;
            vec1 = intersect_array(vec1, vec2);
            argp->type = T_POINTER;
            sp--;
            vec1->ref++;
            sp->u.vec = argp->u.vec = vec1;
            free_svalue(sp+1);
            break;
        }
        goto bad_left;
        /* NOTREACHED */ break;
    }
    
    CASE(F_OR_EQ);                  /* --- or_eq               --- */
    {
        /* Binary-Or the value designated by lvalue sp[0] with sp[-1],
         * assign the result to sp[0] and also leave it on the stack.
         *
         * Possible type combinations:
         *   int         & int                -> int
         *
         * TODO: Extend this to mappings and arrays.
         */

        struct svalue *argp;

#ifdef DEBUG
        if (sp->type != T_LVALUE)
            goto bad_arg_1;
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
                goto bad_right;
            sp->u.number = argp->u.number |= sp->u.number;
            break;
        }
        goto bad_left;
        /* NOTREACHED */ break;
    }

    CASE(F_XOR_EQ);                 /* --- xor_eq              --- */
    {
        /* Binary-XOr the value designated by lvalue sp[0] with sp[-1],
         * assign the result to sp[0] and also leave it on the stack.
         *
         * Possible type combinations:
         *   int         ^ int                -> int
         *
         * TODO: Extend this to mappings and arrays.
         */

        struct svalue *argp;

#ifdef DEBUG
        if (sp->type != T_LVALUE)
            goto bad_arg_1;
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
                goto bad_right;
            sp->u.number = argp->u.number ^= sp->u.number;
            break;
        }
        goto bad_left;
        /* NOTREACHED */ break;
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

        int i;
        struct svalue *argp;

#ifdef DEBUG
        if (sp->type != T_LVALUE)
            goto bad_arg_1;
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
                goto bad_right;
            i = sp->u.number;
            argp->u.number <<= (uint)i > MAX_SHIFT ? MAX_SHIFT : i;
            sp->u.number = argp->u.number;
            break;
        }
        goto bad_left;
        /* NOTREACHED */ break;
    }
    
    CASE(F_RSH_EQ);                 /* --- rsh_eq              --- */
    {
        /* Shift the value designated by lvalue sp[0] right by sp[-1],
         * assign the result to sp[0] and also leave it on the stack.
         *
         * Possible type combinations:
         *   int        << int                -> int
         *
         * TODO: Implement an arithmetic shift.
         */

        int i;
        struct svalue *argp;
#ifdef DEBUG
        if (sp->type != T_LVALUE)
            goto bad_arg_1;
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
                goto bad_right;
            i = sp->u.number;
            argp->u.number >>= (uint)i > MAX_SHIFT ? MAX_SHIFT : i;
            sp->u.number = argp->u.number;
            break;
        }
        goto bad_left;
        /* NOTREACHED */ break;
    }

    /* --- Machine internal instructions --- */
        
    CASE(F_POP_VALUE);              /* --- pop_value           --- */
        /* Pop the topmost value from the stack (freeing it).
         * Simple, huh?
         */
        pop_stack();
        break;
        
    CASE(F_DUP);                    /* --- dup                 --- */
        /* Push a duplicate of sp[0] onto the stack.
         */
        sp++;
        assign_svalue_no_free(sp, sp-1);
        break;

    CASE(F_LBRANCH);                /* --- lbranch <offset>    --- */
    {
        /* Jump by (16-Bit) short <offset> bytes.
         * The <offset> is counted from its first byte (TODO: Ugh).
         */
      
        short offset;

        GET_SHORT(offset, pc); 
        pc += offset;
        break;
    }

    CASE(F_LBRANCH_WHEN_ZERO); /* --- lbranch_when_zero <offset> --- */
    {
        /* Jump by (16-Bit) short <offset> bytes if sp[0] is number 0.
         * The <offset> is counted from its first byte (TODO: Ugh).
         * sp[0] is popped from the stack.
         */
      
        short offset;

        if (sp->type == T_NUMBER && sp->u.number == 0)
        {
            GET_SHORT(offset, pc); 
            pc += offset;
            sp--;
            break;
        }
        pc += 2;
        pop_stack();
        break;
    }

    CASE(F_LBRANCH_WHEN_NON_ZERO); /* --- lbranch_when_non_zero <offset> --- */
    {
        /* Jump by (16-Bit) short <offset> bytes if sp[0] is not number 0.
         * The <offset> is counted from its first byte (TODO: Ugh).
         * sp[0] is popped from the stack.
         */
      
        short offset;

        if (sp->type != T_NUMBER || sp->u.number != 0)
        {
            GET_SHORT(offset, pc); 
            pc += offset;
            pop_stack();
            break;
        }
        pc += 2;
        sp--;
        break;
    }
    
    CASE(F_BRANCH);                 /* --- branch <offset>     --- */
    {
        /* Jump forward by uint8 <offset> bytes.
         * The <offset> is counted from the next instruction.
         */
      
        pc += GET_UINT8(pc)+1;
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
                pc += GET_UINT8(pc) + 1;
                break;
            }
            sp--;
            pc++;
            break;
        }
        else
        {
            free_svalue(sp);
            sp--;
            pc++;
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
                pc++;
                break;
            }
        }
        else
        {
            free_svalue(sp);
        }
        sp--;
        pc += GET_UINT8(pc) + 1;
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
        pc += 1;
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
                pc += 1;
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

                 /* --- call_function_by_address <index> <num> --- */
    CASE(F_CALL_FUNCTION_BY_ADDRESS);
    {
        /* Call the function <index> with <num> args on the stack.
         * <index> is a (16-Bit) unsigned short, giving the index within
         * the programs function table. <num> a uint8.
         *
         * Since the function may be redefined through inheritance, the
         * function must be searched in the current_objects program, which
         * might not be the current_program.
         *
         * The code is used to implement calls to non-private functions.
         */

        unsigned short func_index;   /* function index within program */
        unsigned short func_offset;
          /* function index within the current object's program.
           * This way local function may be redefined through inheritance.
           */
        /* TODO: funflags */ uint32 flags; /* the function flags */
        fun_hdr_p      funstart;     /* the actual function (code) */

        /* Get the function's index */
        LOAD_SHORT(func_index, pc);
        func_offset = func_index + function_index_offset;

        /* Find the function in the function table. As the function may have
         * been redefined by inheritance, we must look in the last table,
         * which is pointed to by current_object.
         */
#ifdef DEBUG
        if (func_offset >= current_object->prog->num_functions)
            fatal("Illegal function index: %hu (%hu), %d functions\n"
                 , func_offset, func_index
                 , current_object->prog->num_functions);
#endif

        /* NOT current_prog, which can be an inherited object. */
        flags = current_object->prog->functions[func_offset];

        /* If the function was cross-defined, get the real offset */
        if (flags & NAME_CROSS_DEFINED)
        {
            func_offset += (flags & INHERIT_MASK) - ( (INHERIT_MASK + 1) >> 1);
        }
        
        /* Save all important global stack machine registers */
        push_control_stack(sp, pc+1, fp);

        /* Set the current program back to the objects program _after_
         * the control stack push, since here is where we search for
         * the function.
         */
        current_prog = current_object->prog;
        
        /* Search for the function definition and determine the offsets.
         */
        csp->num_local_variables = GET_UINT8(pc);
        flags = setup_new_frame1(func_offset, 0, 0);
        funstart = (fun_hdr_p)(current_prog->program + (flags & FUNSTART_MASK));
        csp->funstart = funstart;

        /* Setup the stack, arguments and local vars */
        sp = setup_new_frame2(funstart, sp);

        /* Finish the setup */
        current_variables = current_object->variables + variable_index_offset;
        current_strings = current_prog->strings;
        fp = inter_fp;
        pc = FUNCTION_CODE(funstart);
        csp->extern_call = MY_FALSE;
        break;
    }

           /* --- call_explicit_inherited <prog> <index> <num> --- */
    CASE(F_CALL_EXPLICIT_INHERITED);
    {
        /* Call the (inherited) function <index> in program <prog> with
         * <num> arguments on the stack.
         * 
         * <index> is a (16-Bit) unsigned short, giving the index within
         * the programs function table.
         * <prog> is a (16-Bit) unsigned short, giving the index within
         * the current programs inherit table. 
         * <num> a uint8.
         */
      
        unsigned short prog_index;  /* Index within the inherit table */
        unsigned short func_index;  /* Index within the function table */
        /* TODO: funflags */ uint32 flags;  /* the functions flags */
        fun_hdr_p funstart;         /* the actual function (code) */
        struct inherit *inheritp;   /* the inheritance descriptor */

        /* Get the program and function index, and determine the
         * inheritance descriptor
         */
        LOAD_SHORT(prog_index, pc);
        LOAD_SHORT(func_index, pc);

        inheritp = &current_prog->inherit[prog_index];

#ifdef DEBUG
        if (func_index >= inheritp->prog->num_functions)
        {
            fprintf(stderr, "program index : %d\n", prog_index);
            fprintf(stderr, "function index: %d\n", func_index);
            fprintf(stderr, "#functions    : %d\n", inheritp->prog->num_functions);
            fatal("Illegal function index\n");
        }
#endif

        /* Save all important global stack machine registers */
        push_control_stack(sp, pc+1, fp);

        /* If we do an explicit call into a virtually inherited base class we
         * have to find the first instance of the inherited variables.
         * This cannot be done at compile time because it depends on the
         * _object_ (i.e. the runtime environment) in which current_prog
         * is running.
         * TODO: A better compiler might do some backpatching and at least
         * TODO:: leave hints where the variables are, so that we can omit
         * TODO:: the explicite search. Or some load-time patching.
         */
        if (current_prog != current_object->prog
         && inheritp->prog->num_variables
         && (current_prog->variable_names[inheritp->variable_index_offset
                                          +inheritp->prog->num_variables-1
                                         ].flags & TYPE_MOD_VIRTUAL)
         && !(inheritp->prog->variable_names[inheritp->prog->num_variables-1
                                            ].flags & TYPE_MOD_VIRTUAL)
           )
        {
            /* Now search for the first virtual inheritance of the program
             * in the inherit list of the topmost program.
             * Don't get confused by normal inherits, though.
             */

            int i = current_object->prog->num_inherited;
            struct inherit *inh = current_object->prog->inherit;

            while (i)
            {
                if (inh->prog == inheritp->prog
                 && current_object->prog
                                  ->variable_names[inh->variable_index_offset
                                                  +inh->prog->num_variables-1
                                                  ].flags&TYPE_MOD_VIRTUAL
                   )
                    break;
                inh++;
                i--;
            }

            if (i)
            {
                /* found, so adjust the inheritp and the offsets
                 * to start with
                 */
                inheritp = inh;
                current_variables = current_object->variables;
                function_index_offset = 0;
            }
#ifdef DEBUG
            else { /* this shouldn't happen! */
                fprintf(stderr,
                        "Adjusting variable offsets because of virtual "
                        "inheritance for call from %s into %s (topmost "
                        "program %s) FAILED, please check the inherit "
                        "tree and report it.\n",
                        current_prog->name, inheritp->prog->name,
                        current_object->prog->name);
            }
#endif
        }

        /* Set the current program to the inherited program _after_
         * the control stack push, since there is where we search for
         * the function.
         */
        current_prog = inheritp->prog;

        /* Search for the function definition and determine the offsets.
         */
        csp->num_local_variables = EXTRACT_UCHAR(pc);
        flags = setup_new_frame1(
          func_index,
          function_index_offset + inheritp->function_index_offset,
          inheritp->variable_index_offset
        );
        funstart = (fun_hdr_p)(current_prog->program + (flags & FUNSTART_MASK));
        csp->funstart = funstart;
        
        /* Setup the stack, arguments and local vars */
        sp = setup_new_frame2(funstart, sp);

        /* Finish the setup */
        fp = inter_fp;
        pc = FUNCTION_CODE(funstart);
        current_variables += variable_index_offset;
        current_strings = current_prog->strings;
        csp->extern_call = MY_FALSE;
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
        sp->type = T_LVALUE;
        sp->u.lvalue = find_value((int)(LOAD_UINT8(pc) ));
        break;

    CASE(F_VIRTUAL_VARIABLE);    /* --- virtual_variable <num> --- */
        /* Push the virtual object-global variable <num> onto the stack.
         * It is possible that it is a variable that points to
         * a destructed object. In that case, it has to be replaced by 0.
         *
         * <num> is an uint8 and used as index in the current objects
         * variable table.
         */
        sp++;
        assign_checked_svalue_no_free(
          sp,
          find_virtual_value((int)(LOAD_UINT8(pc))),
          sp, pc
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

#ifdef F_IDENTIFIER16

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
        assign_checked_svalue_no_free(sp, find_value((int)var_index), sp, pc);
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
    
#endif /* F_IDENTIFIER16 */

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

    CASE(F_PUSH_INDEXED_LVALUE);    /* --- push_indexed_lvalue --- */
        /* Operator F_PUSH_INDEXED_LVALUE(vector  v=sp[-1], int   i=sp[0])
         * Operator F_PUSH_INDEXED_LVALUE(mapping v=sp[-1], mixed i=sp[0])
         *
         * Compute the lvalue &(v[i]) and push it into the stack. If v has
         * just one ref left, the indexed item is stored in indexing_quickfix
         * and the lvalue refers to that variable.
         */
    
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
        
    CASE(F_INDEX);                  /* --- index              --- */
        /* Operator F_INDEX (string|vector v=sp[0], int   i=sp[-1])
         *          F_INDEX (mapping       v=sp[0], mixed i=sp[-1])
         *
         * Compute the value (v[i]) and push it onto the stack.  If the value
         * would be a destructed object, 0 is pushed onto the stack and the
         * ref to the object is removed from the vector/mapping.
         *
         * Mapping indices may use <indexing_quickfix> for temporary storage.
         */

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
        sp = range_lvalue(0x000, sp);
        break;

    CASE(F_NR_RANGE_LVALUE);           /* --- nr_range_lvalue     --- */
        /* Operator F_RANGE_LVALUE (string|vector &v=sp[0]
         *                         , int i2=sp[-1], i1=sp[-2])
         *
         * Compute the range &(v[i1..<i2]) of lvalue <v> and push it into the
         * stack.  The value pushed is a lvalue pointing to <special_lvalue>.
         * <special_lvalue> then is the POINTER_RANGE_- resp.
         * STRING_RANGE_LVALUE.
         */
    
        inter_pc = pc;
        sp = range_lvalue(0x001, sp);
        break;
      
    CASE(F_RN_RANGE_LVALUE);           /* --- rn_range_lvalue     --- */
        /* Operator F_RANGE_LVALUE (string|vector &v=sp[0]
         *                         , int i2=sp[-1], i1=sp[-2])
         *
         * Compute the range &(v[<i1..i2]) of lvalue <v> and push it into the
         * stack.  The value pushed is a lvalue pointing to <special_lvalue>.
         * <special_lvalue> then is the POINTER_RANGE_- resp.
         * STRING_RANGE_LVALUE.
         */
    
        inter_pc = pc;
        sp = range_lvalue(0x100, sp);
        break;

    CASE(F_RR_RANGE_LVALUE);           /* --- rr_range_lvalue     --- */
        /* Operator F_RANGE_LVALUE (string|vector &v=sp[0]
         *                         , int i2=sp[-1], i1=sp[-2])
         *
         * Compute the range &(v[<i1..<i2]) of lvalue <v> and push it into the
         * stack.  The value pushed is a lvalue pointing to <special_lvalue>.
         * <special_lvalue> then is the POINTER_RANGE_- resp.
         * STRING_RANGE_LVALUE.
         */
    
        inter_pc = pc;
        sp = range_lvalue(0x101, sp);
        break;

    CASE(F_SIMUL_EFUN); /* --- simul_efun <code> [<num_arg>]   --- */
    {
        /* Call the simul_efun <code>. If it's a function taking varargs,
         * <num_arg> gives the number of arguments, otherwise the compiler
         * already took care of fixing the stack.
         *
         * <code> is an uint8 and indexes the function list *simul_efunp.
         * <num_arg> is an uint8.
         */
      
        int code;            /* the function index */
        fun_hdr_p funstart;  /* the actual function */
        struct object *ob;   /* the simul_efun object */
        struct simul_efun_table_s *entry;

        ASSIGN_EVAL_COST  /* we're changing objects */

        /* Get the sefun code and the number of arguments on the stack */
        code = (int)LOAD_UINT8(pc);
        num_arg = simul_efunp[code].num_arg;
        if (num_arg == SIMUL_EFUN_VARARGS)
        {
            num_arg = (int)LOAD_UINT8(pc);
        }
        
        /* No external calls may be done when this object is destructed.
         */
        if (current_object->flags & O_DESTRUCTED)
        {
            pop_n_elems(num_arg);
            push_number(0);
            break;
        }

        /* Make sure the simul_efun object exists; loading it when
         * necessary.
         */
        if ( !(ob = simul_efun_object) )
        {
            inter_sp = sp;
            inter_pc = pc;
            if ( !(ob = get_simul_efun_object()) )
            {
                error("Couldn't load simul_efun object\n");
            }
        }

        /* Get the function code information */
        entry = &simul_efun_table[code];
        
        if ( NULL != (funstart = entry->funstart) )
        {
            /* The entry is valid: call the sefun by recursing into
             * eval_instruction(), so we can get the result from the
             * stack.
             * TODO: Any particular reason for the recursion?
             */
            struct program *prog;
            struct svalue *new_sp;

            push_control_stack(sp, pc, fp);
            csp->ob = current_object;
            csp->prev_ob = previous_ob;
            csp->funstart = funstart;
            csp->num_local_variables = num_arg;
            current_prog = prog = entry->program;
            function_index_offset = entry->function_index_offset;
            current_variables = ob->variables + entry->variable_index_offset;
            new_sp = setup_new_frame2(funstart, sp);
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

        /* TODO: Is this true?:
         * The simul_efun was never called before, and/or the simul_efun
         * object was reloaded since the last use.
         * Call the sefun the old fashioned way with apply()
         */
        inter_sp = sp;
        inter_pc = pc;
        call_simul_efun(code, ob, num_arg);
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
        struct vector *v;
        unsigned short num;
        struct svalue *value, *item;

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
            transfer_svalue_no_free_spc(item++, value++, sp, pc);

        /* Leave the array on the stack (ref count is already ok) */
        sp->type = T_POINTER;
        sp->u.vec = v;
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
        struct mapping *m;
        struct svalue *data;
        int num_values;
        struct svalue *value;

        /* Get the size and width from the code.
         */
        if (instruction == F_M_CAGGREGATE - F_OFFSET)
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

        /* Get the mapping */
        m = allocate_mapping(i, num_values);
        if (!m)
            ERROR("Out of memory\n")

        /* Set sp and value to the first single value on the stack.
         */
        sp = value = sp - (i * (num_values+1)) + 1;
        while (--i >= 0)
        {
            /* Create/reget the mapping entry */
            data = get_map_lvalue(m, value, MY_TRUE);
            free_svalue(value++);
            for (j = num_values; --j >= 0;)
            {
                /* Copy over the entry data */
                if (data->type != T_NUMBER)
                    free_svalue(data);
                transfer_svalue_no_free_spc(data++, value++, sp, pc);
            }
        }

        /* Put the mapping onto the stack */
        sp->type = T_MAPPING;
        sp->u.map = m;
        break;
    }

    CASE(F_EXTRACT2);               /* --- extract2            --- */
    {
        /* Compute the range sp[0]..end from string/array sp[-1]
         * and leave it on the stack. If sp[0] is negative, it is
         * counted from the end of the string/array.
         *
         * The compiler generates this for [x..] and [<x..] ranges,
         * and also if the efun extract() is called with just two arguments.
         * TODO: Get rid of efun extract() and sanitize this code with
         * TODO:: the range codes.
         */

        int len, from;
        struct svalue *arg;

        arg = sp - 1;

        if (arg->type == T_STRING)
        {
            /* Slice an array */
            char *res;

            len = _svalue_strlen(&arg[0]);
            if ((arg+1)->type != T_NUMBER)
            {
                ERRORF(("Index value must be a number.\n"));
                /* NOTREACHED */
                return; /* Flow control hint */
            }
            from = arg[1].u.number;
            sp--;
            if (from < 0) {
                from = len + from;
                if (from < 0)
                    from = 0;
            }
            if (from >= len) {
                pop_stack();
                push_constant_string("");
                break;
            }
            res = string_copy(arg->u.string + from);
            free_string_svalue(sp);
            put_malloced_string(res, sp);
            break;
        }
        
        if (arg->type != T_POINTER)
        {
            ERRORF(("Indexed value is neither string nor array.\n"));
            /* NOTREACHED */
            return; /* Flow control hint */
        }

        /* Slice an array */
        {
            struct vector *v, *res;

            if ((arg+1)->type != T_NUMBER)
            {
                ERRORF(("Index value must be a number.\n"));
                /* NOTREACHED */
                return; /* Flow control hint */
            }
            v = arg->u.vec;
            len = VEC_SIZE(v);
            from = arg[1].u.number;
            sp--;
            if (from < 0) {
                from = len + from;
            }
            res = slice_array(v, from, len-1);
            free_vector(v);
            put_referenced_vector(res);
            break;
        }
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
            push_number(0);
        else
            push_object(previous_ob);
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
#define MY_LAMBDA_VALUE_OFFSET (sizeof(struct svalue) + \
            ((PTRTYPE)(&((struct lambda *)0)->function.code[1])-(PTRTYPE) 0) )
        int ix;
        struct svalue * cstart;
        
        /* Get the value index */
        ix = LOAD_UINT8(pc);

        /* Get the pointer to the last constant value */
        cstart = (struct svalue *)((char *)(csp->funstart)
                                   - LAMBDA_VALUE_OFFSET);
#ifdef DEBUG
        /* TODO: Remove this if nobody complains. */
        if (MY_LAMBDA_VALUE_OFFSET != LAMBDA_VALUE_OFFSET)
        {
           fprintf(stderr, "DEBUG: lambda offset: %ld should be %ld\n"
                  , (long)LAMBDA_VALUE_OFFSET, (long)MY_LAMBDA_VALUE_OFFSET
                  );
            cstart = (struct svalue *)((char *)(csp->funstart)
                                   - MY_LAMBDA_VALUE_OFFSET);
        }
#endif
        sp++; 
        assign_checked_svalue_no_free(sp, cstart - ix, sp, pc);
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
        int ix;
        struct svalue * cstart;
        
        /* Get the value index */
        ix = (EXTRACT_UCHAR(pc) << 8) + EXTRACT_UCHAR(pc + 1);
        pc += 2;
          /* TODO: A LOAD_SHORT() would be nice, but the compiler would
           * TODO:: have to agree.
           */

        /* Get the pointer to the last constant value */
        cstart = (struct svalue *)((char *)(csp->funstart)
                                   - LAMBDA_VALUE_OFFSET);

#ifdef DEBUG
        /* TODO: Remove this if nobody complains. */
        if (MY_LAMBDA_VALUE_OFFSET != LAMBDA_VALUE_OFFSET)
        {
           fprintf(stderr, "DEBUG: lambda offset: %ld should be %ld\n"
                  , (long)LAMBDA_VALUE_OFFSET, (long)MY_LAMBDA_VALUE_OFFSET
                  );
            cstart = (struct svalue *)((char *)(csp->funstart)
                                   - MY_LAMBDA_VALUE_OFFSET);
        }
#endif

        sp++;
        assign_checked_svalue_no_free(sp, cstart - ix, sp, pc);
        break;
    }

    CASE(F_MAP_INDEX);              /* --- map_index           --- */
    {
        /* Operator F_MAP_INDEX( mapping m=sp[-2], mixed i=sp[-1], int j=sp[0])
         *
         * Compute m[i,j] and push it onto the stack.
         */

        struct mapping *m;
        mp_int n;
        struct svalue *data;

        TYPE_TEST1(sp-2, T_MAPPING)
        TYPE_TEST3(sp, T_NUMBER)
        
        m = sp[-2].u.map;
        n = sp->u.number;
        
        if (n < 0 || n >= m->num_values)
        {
            ERRORF(("Illegal sub-index %ld, mapping width is %ld.\n"
                 , (long)n, (long)m->num_values))
        }
        
        sp--; /* the key */
        data = get_map_lvalue(m, sp, MY_FALSE);
        pop_stack();

        if (data == &const0)
        {
            put_number(0);
        }
        else
        {
            assign_checked_svalue_no_free(sp, data + n, sp, pc);
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
        struct svalue *data;
        struct mapping *m;
        mp_int n;

        TYPE_TEST1(sp-2, T_MAPPING)
        TYPE_TEST3(sp, T_NUMBER)
        
        m = sp[-2].u.map;
        n = sp->u.number;
        if (n < 0 || n >= m->num_values)
        { 
            ERRORF(("Illegal sub-index %ld, mapping width is %ld.\n"
                    , (long)n, (long)m->num_values))
        }
        
        sp--; /* the key */
        data = get_map_lvalue(m, sp, MY_TRUE);
        pop_stack();
        
        if (!--m->ref)
        {
            assign_svalue (&indexing_quickfix, data + n);
            m->ref++;
            free_mapping(m);
            sp->type = T_LVALUE;
            sp->u.lvalue = &indexing_quickfix;
            break;
        }
        
        sp->type = T_LVALUE;
        sp->u.lvalue = data + n;
        break;
    }

#ifdef F_JUMP
    CASE(F_JUMP);                   /* --- jump <dest>         --- */
    {
        /* Jump to the (16-Bit) ushort address <dest> (absolute jump).
         */
      
        unsigned short dest;

        GET_SHORT(dest, pc);
        pc = current_prog->program + dest;
        break;
    }
#endif /* F_JUMP */

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
         */
      
        int i;

        if (sp->type == T_OBJECT)
        {
            i = (sp->u.ob->flags & O_CLONE);
        }
        else if (sp->type == T_STRING)
        {
            struct object *o;

            o = find_object(sp->u.string);
            if (!o)
                ERRORF(("No such object '%s'.\n", sp->u.string));
            i = o->flags & O_CLONE;
        }
        else
            goto bad_arg_1;
        free_svalue(sp);
        put_number(i ? 1 : 0);
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
        put_number(i);
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
        put_number(i);
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
        put_number(i);
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
        put_number(i);
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
        put_number(i);
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
        put_number(i);
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
        put_number(i);
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
        put_number(i);
        break;
    }
    
    CASE(F_CTIME);                  /* --- ctime               --- */
    {
        /* EFUN ctime()
         *
         *   string ctime(int clock = time())
         *
         * Interpret the argument clock as number of seconds since Jan,
         * 1st, 1970, 0.00 and convert it to a nice date and time string.
         */

        char *ts, *cp;

        TYPE_TEST1(sp, T_NUMBER)
        ts = time_string(sp->u.number);
        cp = strchr(ts, '\n');

        /* If the string contains nl characters, extract the substring
         * before the first one. Else just copy the (volatile) result
         * we got.
         */
        if (cp)
        {
            int len = cp - ts;
            cp = xalloc(len + 1);
            if (!cp)
                ERROR("Out of memory\n")
            strncpy(cp, ts, len);
            cp[len] = 0;
        }
        else
        {
            cp = string_copy(ts);
            if (!cp)
                ERROR("Out of memory\n")
        }
        put_malloced_string(cp, sp);
        break;
    }

    CASE(F_ED);                     /* --- ed <nargs>          --- */
        /* EFUN ed()
         *
         *   int ed()
         *   int ed(string file)
         *   int ed(string file, string func)
         *
         * Calling without arguments will start the editor ed with the
         * name of the error file, that was returned by
         * master->valid_read(0, geteuid(this_player()), "ed_start",
         * this_player()), usually something like ~/.err. If that file is
         * empty, ed will immediatly exit again.
         * Calling ed() with argument file will start the editor on the
         * file. If the optional argument func is given, this function
         * will be called after exiting the editor.
         *
         * Result is 1 if the editor could be started, else 0. TODO: ???
         */

        if (current_object->flags & O_DESTRUCTED)
        {
            /* could confuse the master... */
            ERROR("Calling ed from destructed object.\n")
        }
        
        GET_NUM_ARG
        assign_eval_cost();
        inter_pc = pc;
        inter_sp = sp;
        
        if (num_arg == 0)
        {
            ed_start(NULL, NULL, NULL);
            push_number(1);
            break;
        }
        else if (num_arg == 1)
        {
            TYPE_TEST1(sp, T_STRING)
            ed_start(sp->u.string, NULL, NULL);
            break;
        }
        else
        {
            TYPE_TEST1(sp-1, T_STRING)
            if (sp->type == T_STRING)
                ed_start((sp-1)->u.string, sp->u.string, current_object);
            else if (sp->type == T_NUMBER)
                ed_start((sp-1)->u.string, NULL, NULL);
            else
                goto bad_arg_2;
            pop_stack();
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

#ifdef FLOAT_FORMAT_1
        if (sp->type == T_NUMBER)
        {
            sp->u.number = - sp->u.number;
            break;
        }
        else if (sp->type == T_FLOAT)
        {
            sp->u.mantissa ^= 0x80000000;
            break;
        }
#else
        if (sp->type == T_NUMBER || sp->type == T_FLOAT)
        {
            sp->u.number = - sp->u.number;
            break;
        }
#endif
        ERROR("Bad argument to unary minus\n")
          
    CASE(F_PRINTF);                 /* --- printf <nargs>      --- */
        /* EFUN printf()
         *
         *   void printf(string format, ...)
         *
         * A cross between sprintf() and write(). Returns void and prints
         * the result string to the user.
         */
        
        assign_eval_cost();
        GET_NUM_ARG
        inter_pc = pc;
        inter_sp = sp;
        TYPE_TEST1(sp - num_arg + 1, T_STRING)
        add_message("%s", string_print_formatted((sp-num_arg+1)->u.string,
                                                 num_arg-1, sp-num_arg+2));
        pop_n_elems(num_arg);
        break;

    CASE(F_RANDOM);                 /* --- random              --- */
        /* EFUN random()
         *
         *   int random(int n)
         *
         * Returns a number in the random range [0 .. n-1].
         *
         * The random number generator is proven to deliver an equal
         * distribution of numbers over a big range, with no repetition of
         * number sequences for a long time.
         * 
         * TODO: Implement a second generator with better distrubution over
         * TODO:: small ranges and intervals.
         */
    
        TYPE_TEST1(sp, T_NUMBER)
        if (sp->u.number <= 0)
        {
            sp->u.number = 0;
            break;
        }
        sp->u.number = random_number(sp->u.number);
        break;

    CASE(F_THROW);                  /* --- throw               --- */
        /* EFUN throw()
         *
         *   void throw(mixed arg)
         *
         * Abort execution. If the current program execution was initiated by
         * catch(), that catch expression will return arg as error code.
         */

        assign_eval_cost();
        transfer_svalue_no_free_spc(&catch_value, sp--, sp, pc);
        inter_sp = sp;
        inter_pc = pc;
        throw_error(); /* do the longjump, with extra checks... */
        break;

    CASE(F_TIME);                   /* --- time                --- */
        /* EFUN time()
         *
         *   int time()
         *
         * Return number of seconds ellapsed since 1. Jan 1970, 0.0:0 GMT
         * 
         * Actually the time is updated only once in every backend cycle.
         */

        push_number(current_time);
        break;
        
    /* --- Efuns: Strings --- */
        
    CASE(F_CAPITALIZE);             /* --- capitalize          --- */
        /* EFUN capitalize()
         *
         *     string capitalize(string str)
         *
         * Convert the first character in str to upper case, and return
         * the new string.
         */

        TYPE_TEST1(sp, T_STRING)
        if (islower((unsigned char)(sp->u.string[0])))
        {
            char *str;

            /* Change malloc'ed strings in place, for others
             * make a copy.
             */
            if (STRING_MALLOC == sp->x.string_type)
                sp->u.string[0] += 'A' - 'a';
            else
            {
                str = string_copy(sp->u.string);
                str[0] += 'A' - 'a';
                pop_stack();
                push_malloced_string(str);
            }
        }
        break;

    CASE(F_CRYPT);                  /* --- crypt               --- */
    {
        /* EFUN crypt()
         *
         *   string crypt(string str, int seed)
         *   string crypt(string str, string seed)
         *
         * Crypt the string str using the integer seed or two characters
         * from the string seed as a seed. If seed is equal 0, then
         * a random seed is used.
         *         
         * The result has the first two characters as the seed.
         */

        char salt[2];
        char *res;
        static char choise[] =
            "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789./";

        TYPE_TEST1(sp-1, T_STRING)
        if (sp->type == T_STRING && svalue_strlen(sp) >= 2)
        {
            salt[0] = sp->u.string[0];
            salt[1] = sp->u.string[1];
        }
        else if (sp->type == T_NUMBER)
        {
            salt[0] = choise[random_number((sizeof choise) - 1)];
            salt[1] = choise[random_number((sizeof choise) - 1)];
        }
        else
            goto bad_arg_2;

        res = string_copy(crypt((sp-1)->u.string, salt));
        pop_n_elems(2);
        push_malloced_string(res);
        break;
    }

    CASE(F_EXPLODE);                /* --- explode             --- */
    {
        /* EFUN explode()
         * 
         *   string *explode(string str, string del)
         *   
         * Return an array of strings, created when the string str is
         * split into substrings as divided by del.
         */
      
        struct vector *v;
        
        TYPE_TEST1(sp-1, T_STRING)
        TYPE_TEST2(sp,   T_STRING)
        inter_sp = sp;
        inter_pc = pc;

        v = explode_string((sp-1)->u.string, sp->u.string);
        free_string_svalue(sp);
        sp--;
        free_string_svalue(sp);
        put_referenced_vector(v);
        break;
    }

    CASE(F_IMPLODE);                /* --- implode             --- */
    {
        /* EFUN implode()
         *
         *   string implode(mixed *arr, string del)
         *
         * Concatenate all strings found in array arr, with the string
         * del between each element. Only strings are used from the array.
         */

        char *str;

        TYPE_TEST1(sp-1, T_POINTER)
        TYPE_TEST2(sp,   T_STRING)
        
        str = implode_string((sp-1)->u.vec, sp->u.string);
        if (!str)
            ERROR("Out of memory\n")
              
        free_string_svalue(sp);
        sp--;
        free_vector(sp->u.vec);
        
        if (str)
        {
            sp->type = T_STRING;
            sp->x.string_type = STRING_MALLOC;
            sp->u.string = str;
        }
        else
        {
            put_number(0);
        }
        break;
    }

    CASE(F_LOWER_CASE);             /* --- lower_case          --- */
    {
        /* EFUN lower_case()
         *
         *   string lower_case(string str)
         *
         * Convert all characters in str to lower case, and return the
         * new string.
         */

        char *str, *s, *d, c;
        ptrdiff_t initial_len;

        TYPE_TEST1(sp, T_STRING)

        /* Set s to the first uppercase character and store it in c */
        for ( s = sp->u.string
            ; '\0' != (c = *s) && !isupper((unsigned char)c)
            ; s++) NOOP;

        if (c)
        {
            /* Yes, there is something to change... */
          
            if (STRING_MALLOC == sp->x.string_type)
            {
                /* Scan the rest of the string and lower it */
                for ( ; '\0' != (c = *s); s++)
                    if (isupper((unsigned char)c))
                        *s = tolower(c);
            }
            else
            {
                /* We need to make a copy of the shared string.
                 * so fold the copying with the case changing.
                 */
              
                initial_len = s - sp->u.string;
                str = xalloc(svalue_strlen(sp)+1);
                if (initial_len)
                    memcpy(str, sp->u.string, initial_len);
                for(d = str + initial_len; '\0' != (c = *s++) ; )
                {
                    if (isupper((unsigned char)c))
                        c = tolower(c);
                    *d++ = c;

                }
                *d = '\0';
                free_string_svalue(sp);
                put_malloced_string(str, sp);
            }
        }
        break;
    }

    CASE(F_REGEXP);                 /* --- regexp              --- */
    {
        /* EFUN regexp()
         *
         *   string *regexp(string *list, string pattern)
         *   
         * Match the pattern pattern against all strings in list, and return a
         * new array with all strings that matched. This function uses the
         * same syntax for regular expressions as ed().
         */

        struct vector *v;

        TYPE_TEST1(sp-1, T_POINTER)
        TYPE_TEST2(sp,   T_STRING)
        v = match_regexp((sp-1)->u.vec, sp->u.string);
        pop_stack();
        free_svalue(sp);
        if (v == NULL)
            put_number(0);
        else 
            put_referenced_vector(v);
        break;
    }

    CASE(F_SPRINTF);                /* --- sprintf <nargs>     --- */
    {
        /* EFUN sprintf()
         *
         *   string sprintf(string fmt, ...)
         *
         * Generate a string according to the <fmt> and the following
         * arguments and put it onto the stack.
         *
         * <fmt> follows the C-style sprintf-format string in style,
         * and partly in meaning, too.
         */

        char *s;

        /*
         * string_print_formatted() returns a pointer to it's internal
         * buffer, or to an internal constant...  Either way, it must
         * be copied before it's returned as a string.
         */

        assign_eval_cost();
        GET_NUM_ARG
        inter_pc = pc;
        inter_sp = sp;
        TYPE_TEST1(sp - num_arg + 1, T_STRING)
        s = string_print_formatted((sp-num_arg+1)->u.string,
                                   num_arg-1, sp-num_arg+2);
        pop_n_elems(num_arg);
        if (!s)
            push_number(0);
        else
            /* string_print_formatted() owns the string returned,
             * so copy it.
             */
            push_malloced_string(string_copy(s));
        break;
    }

    CASE(F_STRLEN);                 /* --- strlen              --- */
    {
        /* EFUN strlen()
         *
         *   int strlen(string str)
         *   
         * Returns the length of the string str.
         */
      
        int i;

        if (sp->type == T_STRING)
        {
            i = _svalue_strlen(sp);
            free_string_svalue(sp);
            put_number(i);
            break;
        }
        if (sp->type == T_NUMBER && sp->u.number == 0)
            break;
        goto bad_arg_1;
    }

    CASE(F_TERMINAL_COLOUR);    /* --- terminal_colour <nargs> --- */
    {
        /* EFUN terminal_colour()
         *
         *   varargs string terminal_colour( string str, mapping map,
         *                                   int wrap, int indent )
         * 
         * Expands all colour-defines from the input-string and replaces them
         * by the apropriate values found for the color-key inside the given
         * mapping. The mapping has the format "KEY" : "value", non-string
         * contents are ignored.
         */

        int    indent = 0;
        int    wrap = 0;
        char * str;

        GET_NUM_ARG;
        if ( num_arg >= 3 )
        {
            if ( num_arg == 4 )
            {
                TYPE_TEST4(sp, T_NUMBER );
                indent = (sp--)->u.number;
            }
            TYPE_TEST3(sp,T_NUMBER);
            wrap = (sp--)->u.number;
        }

        TYPE_TEST1(sp-1, T_STRING);
        TYPE_TEST2(sp, T_MAPPING);

        inter_sp = sp;
        inter_pc = pc;

        str = e_terminal_colour(sp[-1].u.string, sp->u.map, indent, wrap);

        pop_stack();

        if (str != sp->u.string)
        {
            /* terminal_colour() actually changed the string */
            free_svalue(sp);
            put_malloced_string(str, sp);
        }
        break;
    }

    CASE(F_CLEAR_BIT);              /* --- clear_bit           --- */
    {
        /* EFUN clear_bit()
         *
         *     string clear_bit(string str, int n)
         *
         * Return the new string where bit n is cleared in string str.
         * Note that the old string str is not modified.
         *
         * Each character contains 6 bits. So you can store a value
         * between 0 and 63 ( 2^6=64) in one character. Starting
         * character is the blank character " " which has the value 0.
         * The first charcter in the string is the one with the lowest
         * bits (0-5).
         */
      
        char *str;
        int len, ind, bitnum;
        struct svalue *strp;

        /* Get and test the arguments */
        TYPE_TEST1(sp-1, T_STRING)
        TYPE_TEST2(sp,   T_NUMBER)
        bitnum = sp->u.number;
        sp = strp = sp-1;
        if (bitnum > MAX_BITS)
            ERRORF(("clear_bit: too big bit number: %d\n", bitnum))
              
        len = svalue_strlen(strp);
        ind = bitnum/6;
        if (ind >= len)
        {
            /* Return first argument unmodified! */
            break;
        }
        
        /* Malloc'ed strings are modified in place, others are copied first.
         */
        if (strp->x.string_type == STRING_MALLOC)
        {
            str = strp->u.string;
        }
        else
        {
            str = xalloc(len+1);
            memcpy(str, strp->u.string, len+1); /* Including null byte */
            free_string_svalue(strp);
            strp->x.string_type = STRING_MALLOC;
            strp->u.string = str;
        }
        
        if (str[ind] > 0x3f + ' ' || str[ind] < ' ')
            ERRORF(("Illegal bit pattern in clear_bit character %d\n", ind))

        str[ind] = ((str[ind] - ' ') & ~(1 << (bitnum % 6))) + ' ';

        break;
    }

    CASE(F_SET_BIT);                /* --- set_bit             --- */
    {
        /* EFUN set_bit()
         *
         *   string set_bit(string str, int n)
         *
         * Return the new string where bit n is set in string str. Note
         * that the old string str is not modified.
         *
         * The new string will automatically be extended if needed.
         */

        char *str;
        int len, old_len, ind, bitnum;
        struct svalue *strp;

        TYPE_TEST1(sp-1, T_STRING)
        TYPE_TEST2(sp,   T_NUMBER)
        
        bitnum = sp->u.number;
        sp = strp = sp-1;
        if (bitnum > MAX_BITS)
            ERRORF(("set_bit: too big bit number: %d\n", bitnum))
              
        len = svalue_strlen(strp);
        old_len = len;
        ind = bitnum/6;

        /* Malloc'ed strings of the right size are modified in place,
         * others are copied first.
         */
        if ( (ind < len || (len = ind + 1, MY_FALSE) )
         &&  strp->x.string_type == STRING_MALLOC )
        {
            str = strp->u.string;
        }
        else
        {
            str = xalloc(len+1);
            str[len] = '\0';
            if (old_len)
                memcpy(str, strp->u.string, old_len);
            if (len > old_len)
                memset(str + old_len, ' ', len - old_len);
            free_string_svalue(strp);
            strp->x.string_type = STRING_MALLOC;
            strp->u.string = str;
        }
        
        if (str[ind] > 0x3f + ' ' || str[ind] < ' ')
            ERRORF(("Illegal bit pattern in set_bit character %d\n", ind))

        str[ind] = ((str[ind] - ' ') | 1 << (bitnum % 6) ) + ' ';
        sp = strp;
        break;
    }
    
    CASE(F_TEST_BIT);               /* --- test_bit            --- */
    {
        /* EFUN test_bit()
         *
         *   int test_bit(string str, int n)
         *
         * Return 0 or 1 of bit n was set in string str.
         */

        int len;

        TYPE_TEST1(sp-1, T_STRING)
        TYPE_TEST2(sp,   T_NUMBER)
        
        len = svalue_strlen(sp-1);
        if (sp->u.number/6 >= len)
        {
            sp--;
            free_string_svalue(sp);
            put_number(0);
            break;
        }
        
        if ( ((sp-1)->u.string[sp->u.number/6] - ' ') 
            & 1 << (sp->u.number % 6) )
        {
            sp--;
            free_string_svalue(sp);
            put_number(1);
        }
        else
        {
            sp--;
            free_string_svalue(sp);
            put_number(0);
        }
        break;
    }

    /* --- Efuns: Arrays and Mappings --- */
        
    CASE(F_ALLOCATE);               /* --- allocate            --- */
    {
        /* EFUN allocate()
         *
         *     mixed *allocate(int size)
         *
         * Allocate an empty array of <size> elements.
         */

        struct vector *v;

        TYPE_TEST1(sp, T_NUMBER)
        inter_sp = sp;
        inter_pc = pc;
        v = allocate_array(sp->u.number); /* Will have ref count == 1 */
        sp->type = T_POINTER;
        sp->u.vec = v;
        break;
    }
    
    CASE(F_FILTER);                 /* --- filter <nargs>      --- */
    CASE(F_FILTER_ARRAY);          /* --- filter_array <nargs> --- */
    {
        /* EFUN filter()
         *
         *   mixed * filter (mixed *arg, string fun, object ob, mixed extra...)
         *   mixed * filter (mixed *arg, closure cl, mixed extra...)
         *   mixed * filter (mixed *arg, mapping map, mixed extra...)
         *
         *  mapping filter (mapping arg, string func, object ob, mixed extra...)
         *  mapping filter (mapping arg, closure cl, mixed extra...)
         *
         * Call the function <ob>-><func>() resp. the closure <cl> for
         * every element of the array or mapping <arg>, and return
         * a result made from those elements for which the function
         * call returns TRUE.
         *
         * If <ob> is omitted, it defaults to this_object().
         * TODO: F_FILTER_ARRAY is obsolete.
         */

        struct svalue *arg;

        GET_NUM_ARG
        ASSIGN_EVAL_COST
        inter_pc = pc;
        arg = sp - num_arg + 1;

        if (F_FILTER-F_OFFSET == instruction && arg->type == T_MAPPING)
        {
            sp = filter_mapping(sp, num_arg, MY_TRUE);
        }
        else
        {
            sp = f_filter_array(sp, num_arg);
        }
        break;
    }

    CASE(F_FILTER_INDICES);      /* --- filter_indices <nargs> --- */
        /* EFUN filter_indices()
         *
         *   mapping filter_indices(mapping, string func, object ob, ...)
         *   mapping filter_indices(mapping, closure cl, ...)
         * 
         * ob->func() is called resp. cl applied to every element in the
         * mapping, with first argument being the key of the
         * element, and then the extra args that were given to
         * filter_mapping. If the function returns true, the element is
         * added to the result mapping. ob can also be a file_name of an
         * object. If the second arg is a string and the third is not an
         * object, this_object() will be used as default.
         */
    
        GET_NUM_ARG
        ASSIGN_EVAL_COST
        inter_pc = pc; /* apply_low() needs this */
        sp = filter_mapping(sp, num_arg, MY_FALSE);
        break;

    CASE(F_MAP);                    /* --- map <nargs>         --- */
    CASE(F_MAP_ARRAY);              /* --- map_array <nargs>   --- */
    {
        /* EFUN map()
         *
         *   mixed * map(mixed *arg, string func, object ob, mixed extra...)
         *   mixed * map(mixed *arg, closure cl, mixed extra...)
         *
         *   mapping map(mapping arg, string func, object ob, mixed extra...)
         *   mapping map(mapping arg, closure cl, mixed extra...)
         *
         * Call the function <ob>-><func>() resp. the closure <cl> for
         * every element of the array or mapping <arg>, and return a result
         * made up from the returned values.
         *
         * If <ob> is omitted, it defaults to this_object().
         * TODO: map_array() is obsolete.
         */

        struct vector *res;
        struct svalue *arg;
        char *func;         /* Function to call, doubles as svalue* :-( */
        struct object *ob;
        int num_extra;

        assign_eval_cost();
        GET_NUM_ARG
        inter_pc = pc;
        inter_sp = sp;
        
        arg = sp - num_arg + 1;
        if (F_MAP-F_OFFSET == instruction && arg->type == T_MAPPING)
        {
            sp = map_mapping(sp, num_arg, MY_TRUE);
            break;
        }
        
        TYPE_TEST1(arg, T_POINTER)
          
        /* Determine object and function to call */
        ob = NULL;
        if (arg[1].type == T_CLOSURE)
        {
            func = (char *)(arg + 1);
              /* map_array() know how to deal with this *sigh* */
            num_extra = num_arg - 2;
        }
        else
        {
            TYPE_TEST2(arg+1, T_STRING)
            func = arg[1].u.string;

            if (num_arg > 2)
            {
                if (arg[2].type == T_OBJECT)
                    ob = arg[2].u.ob;
                else if (arg[2].type == T_STRING
                    &&   NULL != ( ob = get_object(arg[2].u.string) ))
                    NOOP;
                else
                    goto bad_arg_3;
                num_extra = num_arg - 3;
            }
            else
            {
                ob = current_object;
                num_extra = 0;
            }
        }

        /* Check and map the array */
        if (arg[0].type == T_POINTER)
        {
            check_for_destr(arg[0].u.vec);
            map_array (arg[0].u.vec, func, ob,
                        num_extra, sp - num_extra + 1);
            res = sp[1].u.vec;
        }
        else
        {
            res = NULL;
        }

        /* Clean up the stack and push the result */
        pop_n_elems (num_arg - 1);
        free_svalue(sp);
        if (res)
        {
            put_referenced_vector (res);
        }
        else
            put_number (0);
        break;
    }
    
    CASE(F_MAP_INDICES);            /* --- map_indices <nargs> --- */
        /* EFUN map_indices()
         *
         *   mapping map_indices(mapping m, string func, object ob, ...)
         *   mapping map_indices(mapping m, closure cl, ...)
         *
         * ob->func() is called resp. cl applied to every element in the
         * mapping, with the key of the element as first argument, and
         * then the extra args that were given to map_mapping.
         * The data item in the mapping is replaced by the return value
         * of the function. ob can also be a file_name of an object.
         * If the second arg is a string and the third is not an
         * object, this_object() will be used as default.
         */

        GET_NUM_ARG
        ASSIGN_EVAL_COST
        inter_pc = pc; /* apply_low() needs this */
        sp = map_mapping(sp, num_arg, MY_FALSE);
        break;

    CASE (F_MEMBER_ARRAY);          /* --- member_array        --- */
    {
        /* EFUN member_array()
         *
         *   int member_array(mixed item, mixed *arr)
         *   int member_array(mixed item, string arr)
         *
         * Returns the index of the first occurence of item in array arr,
         * or occurence of a character in a string. If not found, then -1
         * is returned.
         * TODO: Practically obsoleted by member().
         */

        /* Search in an array */

        if (sp->type == T_POINTER)
        {
            struct vector *vec;   /* Vector searched */
            struct svalue *item;  /* Pointer into the vector array */
            struct svalue *key;   /* Item searched */
            int cnt;              /* Size of vec */

            vec = sp->u.vec;
            item = vec->item;
            key = sp - 1;
            cnt = VEC_SIZE(vec);
            switch(key->type)
            {
            case T_STRING:
              {
                char *str;

                str = key->u.string;
                for(; --cnt >= 0; item++)
                {
                    if (item->type == T_STRING
                     && !strcmp(key->u.string, item->u.string)
                       )
                        break;
                }
                break;
              }

            case T_FLOAT:
            case T_CLOSURE:
            case T_SYMBOL:
            case T_QUOTED_ARRAY:
              {
                short type;
                short x_generic;

                type = key->type;
                x_generic = key->x.generic;
                for(; --cnt >= 0; item++)
                {
                    if (key->u.string == item->u.string
                     && x_generic == item->x.generic
                     && item->type == type
                       )
                        break;
                }
                break;
              }

            case T_NUMBER:
                if (!key->u.number)
                {
                    /* Search for 0 is special: it also finds destructed
                     * objects
                     */
                    short type;

                    for (; --cnt >= 0; item++)
                    {
                        if ( (type = item->type) == T_NUMBER)
                        {
                            if ( !item->u.number )
                                break;
                        }
                        else if (type == T_OBJECT)
                        {
                            if (item->u.ob->flags & O_DESTRUCTED)
                            {
                                assign_svalue(item, &const0);
                                break;
                            }
                        }
                    }
                    break;
                }
                
                /* FALLTHROUGH */                

            case T_MAPPING:
            case T_OBJECT:
            case T_POINTER:
              {
                short type = key->type;

                for(; --cnt >= 0; item++)
                {
                    if (key->u.number == item->u.number
                     && item->type == type)
                        break;
                }
                break;
              }

            default:
                if (sp[-1].type == T_LVALUE)
                    error("Reference passed to member_array()\n");
                fatal("Bad type to member_array(): %d\n", sp[-1].type);
            }
            
            if (cnt >= 0)
            {   /* Return -1 for failure */
                cnt = VEC_SIZE(vec) - cnt - 1;
            }
            
            pop_stack();
            free_svalue(sp);
            put_number(cnt);
            break;
        }

        /* Or search in a string */
        
        if (sp->type == T_STRING)
        {
            char *str, *str2;
            int i;

            if (sp[-1].type != T_NUMBER)
                goto bad_arg_1;
            str = sp->u.string;
            i = sp[-1].u.number;
            str2 = i & ~0xff ? NULL : strchr(str, i);
            i = str2 ? str2 - str : -1;
            pop_stack();
            free_svalue(sp);
            put_number(i);
            break;
        }

        /* Otherwise, it's something unsearchable */

        goto bad_arg_2;
    }

    CASE (F_MEMBER);                /* --- member              --- */
    {
        /* EFUN member()
         *
         *   int member(mixed *array, mixed elem)
         *   int member(mapping m, mixed key)
         *   int member(string s, int elem)
         * 
         * For arrays and strings, returns the index of the second arg in
         * the first arg, or -1 if none found. For mappings it checks, if
         * key is present in mapping m and returns 1 if so, 0 if key is
         * not in m.
         */

        /* --- Search an array --- */
      
        if (sp[-1].type == T_POINTER)
        {

            struct vector *vec;
            union  u       sp_u;
            int cnt;

            vec = sp[-1].u.vec;
            cnt = VEC_SIZE(vec);
            sp_u = sp->u;

            switch(sp->type)
            {
            case T_STRING:
              {
                char *str;
                struct svalue *item;

                str = sp_u.string;
                for(item = vec->item; --cnt >= 0; item++)
                {
                    if (item->type == T_STRING
                     && !strcmp(sp_u.string, item->u.string))
                        break;
                }
                break;
              }

            case T_FLOAT:
            case T_CLOSURE:
            case T_SYMBOL:
            case T_QUOTED_ARRAY:
              {
                short x_generic;
                short type;
                struct svalue *item;

                type = sp->type;
                x_generic = sp->x.generic;
                for(item = vec->item; --cnt >= 0; item++)
                {
                    if (sp_u.string == item->u.string
                     && x_generic == item->x.generic
                     && item->type == type)
                        break;
                }
                break;
              }
              
            case T_NUMBER:
                if (!sp_u.number)
                {
                    /* Search for 0 is special: it also finds destructed
                     * objects
                     */

                    struct svalue *item;
                    short type;

                    for (item = vec->item; --cnt >= 0; item++)
                    {
                        if ( (type = item->type) == T_NUMBER)
                        {
                            if ( !item->u.number )
                                break;
                        }
                        else if (type == T_OBJECT)
                        {
                            if (item->u.ob->flags & O_DESTRUCTED)
                            {
                                assign_svalue(item, &const0);
                                break;
                            }
                        }
                    }
                    break;
                }

                /* FALLTHROUGH */
                
            case T_MAPPING:
            case T_OBJECT:
            case T_POINTER:
              {
                struct svalue *item;
                short type = sp->type;

                for (item = vec->item; --cnt >= 0; item++)
                {
                    if (sp_u.number == item->u.number
                     && item->type == type)
                        break;
                }
                break;
              }
              
            default:
                if (sp->type == T_LVALUE)
                    error("Reference passed to member()\n");
                fatal("Bad type to member(): %d\n", sp->type);
            }
            
            if (cnt >= 0)
            {
                /* Return -1 for failure */
                cnt = VEC_SIZE(vec) - cnt - 1;
            }
            pop_stack();
            free_svalue(sp);
            put_number(cnt);
            break;
        }
        
        /* --- Search a string --- */
      
        if (sp[-1].type == T_STRING)
        {
            char *str, *str2;
            int i;

            if (sp->type != T_NUMBER)
                goto bad_arg_2;
            str = sp[-1].u.string;
            i = sp->u.number;
            str2 = i & ~0xff ? NULL : strchr(str, i);
            i = str2 ? str2 - str : -1;
            pop_stack();
            free_svalue(sp);
            put_number(i);
            break;
        }

        /* --- Search a string --- */
      
        if (sp[-1].type == T_MAPPING)
        {
            int i;

            i = get_map_lvalue(sp[-1].u.map, sp, MY_FALSE) != &const0;
            pop_stack();
            free_svalue(sp);
            put_number(i);
            break;
        }

        /* Otherwise it's not searchable */

        goto bad_arg_1;
    }

    CASE(F_MKMAPPING);              /* mkmapping <nargs>       --- */
    {
        /* EFUN mkmapping()
         *
         *   mapping mkmapping(mixed *arr1, mixed *arr2,...)
         *
         * Returns a mapping with indices from 'arr1' and values from
         * 'arr2'... . arr1[0] will index arr2...[0], arr1[1] will index
         * arr2...[1], etc. If the arrays are of unequal size, the mapping
         * will only contain as much elements as are in the smallest
         * array.
         */
      
        int i, length, num_values;
        struct mapping *m;
        struct svalue *key;

        GET_NUM_ARG

        /* Check the arguments and set length to the size of
         * the shortest array.
         */
        length = MAX_ARRAY_SIZE;
        for (i = -num_arg; ++i <= 0; )
        {
            if ( sp[i].type != T_POINTER )
                bad_arg_pc(i+num_arg, instruction, sp, pc);
            if (length > (int)VEC_SIZE(sp[i].u.vec))
                length = VEC_SIZE(sp[i].u.vec);
        }

        /* Allocate the mapping */
        num_values = num_arg - 1;
        m = allocate_mapping(length, num_values);
        if (!m)
            ERROR("Out of memory\n")
        
        /* Shift key through the first array and assign the values
         * from the others.
         */
        key = &(sp-num_values)->u.vec->item[length];
        while (--length >= 0)
        {
            struct svalue *dest;

            dest = get_map_lvalue(m, --key, MY_TRUE);
            for (i = -num_values; ++i <= 0; )
            {
                /* If a key value appears multiple times, we have to free
                 * a previous assigned value to avoid a memory leak
                 */
                assign_svalue(dest++, &sp[i].u.vec->item[length]);
            }
        }
        
        /* Clean up the stack and push the result */
        pop_n_elems(num_arg);
        push_mapping(m);  /* Adds the second ref */
        sp->u.map->ref--; /* This will make ref count == 1 */
        break;
    }
    
    CASE(F_M_DELETE);               /* --- m_delete            --- */
    {
        /* EFUN m_delete()
         *
         *   mapping m_delete(mapping map, mixed key)
         *
         * Remove the entry with index 'key' from mapping 'map'. The
         * changed mapping 'map' is also returned as result.
         * If the mapping does not have an        entry with index 'key',
         * nothing is changed.
         */

        struct mapping *m;

        TYPE_TEST1(sp-1, T_MAPPING)
        m = (sp-1)->u.map;
        remove_mapping(m, sp);
        pop_stack();
        /* leave the mapping on the stack */
        break;
    }

    CASE(F_M_INDICES);              /* --- m_indices           --- */
    {
        /* EFUN m_indices()
         *
         *   mixed *m_indices(mapping map)
         *
         * Returns an array containing the indices of mapping 'map'.
         */

        struct mapping *m;
        struct vector *v;

        TYPE_TEST1(sp, T_MAPPING)
        m = sp->u.map;
        inter_pc = pc;
        inter_sp = sp;
        
        v = m_indices(m);
        
        free_mapping(m);
        put_referenced_vector(v);
        break;
    }
    
    CASE(F_M_VALUES);               /* --- m_values            --- */
    {
        /* EFUN m_values()
         *
         *   mixed *m_values(mapping map)
         *   mixed *m_values(mapping map, int index)
         *
         * Returns an array with the values of mapping 'map'.
         * If <index> is given as a number between 0 and the width of
         * the mapping, the values from the given column are returned,
         * else the values of the first column.
         */

        struct mapping *m;
        struct vector *v;
        struct mvf_info vip;
        mp_int size;
        int num;

        /* Get and check the arguments */
        TYPE_TEST2(sp, T_NUMBER);
        num = sp->u.number;
        sp--;
        if (sp->type != T_MAPPING || (m = sp->u.map)->num_values < 1)
            goto bad_arg_1;
        if (num < 0 || num >= m->num_values)
            ERROR("Illegal index to m_values().\n");
        
        /* Get the size of the mapping */
        check_map_for_destr(m);
        size = MAP_SIZE(m);
        
        v = allocate_array(size);

        /* Extract the desired column from the mapping */
        vip.svp = v->item;
        vip.num = num;
        walk_mapping(m, m_values_filter, &vip);
        free_mapping(m);

        /* Push the result */
        put_referenced_vector(v);
        break;
    }
    
    CASE(F_SIZEOF);                 /* --- sizeof              --- */
    {
        /* EFUN sizeof()
         *
         *   int sizeof(mixed arr)
         * 
         * Returns the number of elements of an array or or the number of
         * keys in a mapping.
         *
         * For scalar values (like integers), 0 is returned.
         */

        int i;

        if (sp->type == T_POINTER)
        {
            i = VEC_SIZE(sp->u.vec);
            free_svalue(sp);
            put_number(i);
            break;
        }
        
        if (sp->type == T_MAPPING)
        {
            struct mapping *m = sp->u.map;
            check_map_for_destr(m);
            i = MAP_SIZE(m);
            free_svalue(sp);
            put_number(i);
            break;
        }
        
        if (sp->type == T_NUMBER && sp->u.number == 0)
            break;
        
        goto bad_arg_1;
    }
    
    CASE(F_SORT_ARRAY);             /* --- sort_array          --- */
    {
        /* EFUN sort_array()
         *
         *   mixed *sort_array(mixed *arr, string wrong_order, object|string ob)
         *   mixed *sort_array(mixed *arr, closure cl)
         *
         * Returns an array sorted either by the ordering function
         * ob->wrong_order(a, b), or by the closure expression 'cl'.
         * If the 'arr' argument equals 0, the result is also 0.
         * 'ob' is the object in which the ordering function is called
         * and may be given as object or by its filename. If omitted, the
         * function is called in the current object.
         *
         * The elements from the array to be sorted are passed in pairs to
         * the function 'wrong_order' as arguments.
         * The function should return a positive number if the elements
         * are in the wrong order. It should return 0 or a negative
         * number if the elements are in the correct order.
         */

        struct vector *res;
        struct svalue *arg;
        struct object *ob;
        char *func;         /* Function to call, also used as struct svalue* */

        assign_eval_cost();
        inter_pc = pc;
        TYPE_TEST1(sp-2, T_POINTER)
        inter_sp = sp;
        arg = sp - 2; ob = 0;

        /* Get the arguments */
        
        if (arg[1].type == T_CLOSURE)
        {
            func = (char *)(arg + 1);
              /* sort_array() knows how to handle this :-( */
        }
        else
        {
            TYPE_TEST2(arg+1, T_STRING)
            func = arg[1].u.string;
            if (arg[2].type == T_OBJECT)
                ob = arg[2].u.ob;
            else if (arg[2].type == T_STRING)
                ob = get_object(arg[2].u.string);

            if (!ob)
                goto bad_arg_3;
        }

        if (arg[0].type == T_POINTER)
        {
            /* sort_array already takes care of destructed objects */
            res = sort_array (
              slice_array(arg[0].u.vec, 0, VEC_SIZE(arg[0].u.vec)-1),
              func, ob);
        }
        else
            res = NULL;

        /* Clean up the stack and push the result */
        pop_n_elems (3);
        sp++;
        if (res)
        {
            sp->type = T_POINTER;
            sp->u.vec = res;
        }
        else
            put_number(0);
        break;
    }
    
    CASE(F_UNIQUE_ARRAY);           /* --- unique_array        --- */
    {
        /* EFUN unique_array()
         *
         *   mixed unique_array(object *obarr, string seperator)
         *   mixed unique_array(object *obarr, string seperator, mixed skip)
         *
         * Groups objects together for which the separator function
         * returns the same value. obarr should be an array of objects,
         * other types are ignored. The separator function is called only
         * once in each object in obarr. If no separator function is
         * given, 0 is used instead of a return value.
         * If a 3rd argument is given and this argument matches the
         * return value of the separator function this object will not be
         * included in the returned array.
         */

        struct vector *res;
      
        assign_eval_cost();
        inter_pc = pc;
        inter_sp = sp;
        TYPE_TEST1(sp-2, T_POINTER)
        TYPE_TEST2(sp-1, T_STRING)
        
        check_for_destr((sp-2)->u.vec);
        res = make_unique((sp-2)->u.vec, (sp-1)->u.string, sp);

        /* Clean up the stack and push the result */
        pop_stack();
        pop_stack();
        free_svalue(sp);
        
        if (res)
        {
            sp->type  = T_POINTER;
            sp->u.vec = res;  /* ref count is already 1 */
        }
        else
            put_number (0);
        
        break;
    }

    CASE(F_UNMKMAPPING);            /* --- unmkmapping         --- */
    {
        /* EFUN unmkmapping()
         *
         *   mixed* unmkmapping(mapping map)
         *
         * Take mapping <map> and return an array of arrays with the keys
         * and values from the mapping.
         *
         * The return array has the form ({ keys[], data0[], data1[], ... }).
         */
      
        struct svalue *svp;
        struct mapping *m;
        struct vector *v;
        struct mvf_info vip;
        mp_int size;
        int i;

        /* Get the arguments */
        if (sp->type != T_MAPPING)
            goto bad_arg_1;
        m = sp->u.map;
        
        /* Determine the size of the mapping and allocate the result vector */
        check_map_for_destr(m);
        size = MAP_SIZE(m);
        v = allocate_array(m->num_values+1);
        
        /* Allocate the sub vectors */
        for (i = 0, svp = v->item; i <= m->num_values; i++, svp++)
        {
            struct vector *v2;

            v2 = allocate_array(size);
            svp->type = T_POINTER;
            svp->u.vec = v2;
        }
        
        /* Copy the elements from the mapping into the vector brush */
        vip.svp = v->item;
        vip.num = 0;
        vip.width = m->num_values;
        walk_mapping(m, m_unmake_filter, &vip);

        /* Clean up the stack and push the result */
        free_mapping(m);
        put_referenced_vector(v);
        break;
    }
    
    CASE(F_WIDTHOF);                /* --- widthof             --- */
    {
        /* EFUN widthof()
         *
         *   int widthof (mapping map)
         *
         * Returns the number of values per key of mapping <map>.
         * If <map> is 0, the result is 0.
         */

        int width;

        if (sp->type == T_NUMBER && sp->u.number == 0)
            break;
        TYPE_TEST1(sp, T_MAPPING)
        width = sp->u.map->num_values;
        free_mapping(sp->u.map);
        put_number(width);
        break;
    }

    /* --- Efuns: Functions and Closures --- */
        
    CASE(F_APPLY);                  /* --- apply <nargs>       --- */
    {
        /* EFUN apply()
         *
         *     mixed apply(closure cl, ...)
         *
         * Call the closure <cl> and pass it all the extra arguments
         * given in the call. If the last argument is an array, it
         * is flattened, ie. passed as a bunch of single arguments.
         */

        struct svalue *args;

        GET_NUM_ARG
          
        if (sp->type == T_POINTER)
        {
            /* The last argument is an array: flatten it */

            struct vector *vec;  /* the array */
            struct svalue *svp;  /* pointer into the array */
            int i;               /* (remaining) vector size */

            vec = sp->u.vec;
            i = VEC_SIZE(vec);
            num_arg += i - 1;
            if ( num_arg > 0x100 /* TODO: MAX_something? */)
            {
                /* Check if the target closure can handle more
                 * than 256 args.
                 */
                switch( (sp - num_arg + i)->x.closure_type )
                {
                case CLOSURE_LFUN:
                case CLOSURE_ALIEN_LFUN:
                case CLOSURE_LAMBDA:
                case CLOSURE_BOUND_LAMBDA:
                    if (num_arg + (sp - start_of_stack) < EVALUATOR_STACK_SIZE)
                        break;
                default:
                    bad_arg_pc(num_arg - i + 1, instruction, sp, pc);
                    /* TODO: Be more specific */
                }
            }

            /* Push the array elements onto the stack, overwriting the
             * array value itself.
             */
            if (--vec->ref)
            {
                for (svp = vec->item; --i >= 0; )
                {
                    assign_svalue_no_free(sp++, svp++);
                }
            }
            else
            {
                /* The array will be freed, so use a faster function */
                for (svp = vec->item; --i >= 0; ) {
                    transfer_svalue_no_free_spc(sp++, svp++, sp, pc);
                }
                free_empty_vector(vec);
            }
            
            sp--; /* undo the last extraneous sp++ */
        }

        /* Prepare to call the closure */
        
        args = sp -num_arg +1;
        TYPE_TEST1(args, T_CLOSURE)
          
        /* No external calls may be done when this object is
         * destructed.
         */
        if (current_object->flags & O_DESTRUCTED)
        {
            pop_n_elems(num_arg);
            push_number(0);
            break;
        }

        inter_pc = pc;
        inter_sp = sp;
        ASSIGN_EVAL_COST
          
        call_lambda(args, num_arg - 1);

        /* Cleanup the stack */
        sp = args;
        free_closure(sp);
        *sp = sp[1];
        break;
    }

    CASE(F_BIND_LAMBDA);            /* --- bind_lambda         --- */
    {
        /* EFUN bind_lambda()
         *
         *     closure bind_lambda(closure cl, object ob = 1)
         *
         * Binds an unbound closure <cl> to object <ob> and return the
         * bound closure.
         *
         * If the optional argument ob is not this_object(), the privilege
         * violation ("bind_lambda", this_object(), ob) occurs. 
         *
         * If the argument <ob> is omitted, the closure is bound to
         * this_object(), and additionally the function accepts unbindable
         * closures without complaint.
         * 
         * Note: the 'default' value for <ob> is const1 so that the omittal
         * can be detected.
         */

        struct object *ob;

        TYPE_TEST1(sp-1, T_CLOSURE)
        if (sp->type == T_OBJECT)
        {
            /* If <ob> is given, check for a possible privilege breach */
            ob = sp->u.ob;
            if (ob != current_object
             && !privilege_violation("bind_lambda", sp))
            {
                free_object(ob, "bind_lambda");
                sp--;
                break;
            }
        }
        else if (sp->type == T_NUMBER && sp->u.number == 1)
        {
            /* this_object is ok */
            ob = current_object;
            add_ref(ob, "bind_lambda");
        }
        else
            goto bad_arg_2;
        
        sp--;  /* points to the closure now */
        
        switch(sp->x.closure_type)
        {
        case CLOSURE_LFUN:
        case CLOSURE_LAMBDA:
        case CLOSURE_IDENTIFIER:
        case CLOSURE_PRELIMINARY:
            /* Unbindable closures. Free the ob reference and
             * throw an error (unless <ob> has been omitted)
             */
            free_object(ob, "bind_lambda");
            if (sp[1].type == T_NUMBER)
                break;
            goto bad_arg_1;
            
        case CLOSURE_ALIEN_LFUN:
            /* Rebind an alien lfun to the given object */
            free_object(sp->u.lambda->ob, "bind_lambda");
            sp->u.lambda->ob = ob;
            break;

        default:
            /* efun, simul_efun, operator closures: rebind it */
            
            free_object(sp->u.ob, "bind_lambda");
            sp->u.ob = ob;
            break;
            
        case CLOSURE_BOUND_LAMBDA:
          {
            /* Rebind an already bound lambda closure */

            struct lambda *l;

            if ( (l = sp->u.lambda)->ref == 1)
            {
                /* We are the only user of the lambda: simply rebind it.
                 */
              
                struct object **obp;

                obp = &l->ob;
                free_object(*obp, "bind_lambda");
                *obp = ob;
                break;
            }
            else
            {
                /* We share the closure with others: create our own
                 * copy, bind it and put it onto the stack in place of
                 * the original one.
                 */
                struct lambda *l2;

                l->ref--;
                l2 = xalloc(sizeof *l);
                l2->ref = 1;
                l2->ob = ob;
                l2->function.lambda = l->function.lambda;
                l->function.lambda->ref++;
                sp->u.lambda = l2;
                break;
            }
          }
          
        case CLOSURE_UNBOUND_LAMBDA:
          {
            /* Whee, an unbound lambda: create the bound-lambda structure
             * and put it onto the stack in place of the unbound one.
             */
            
            struct lambda *l;

            l = xalloc(sizeof *l);
            l->ref = 1;
            l->ob = ob;
            l->function.lambda = sp->u.lambda;
            /* The ref to the unbound closure is just transferred from
             * sp to l->function.lambda.
             */
            sp->x.closure_type = CLOSURE_BOUND_LAMBDA;
            sp->u.lambda = l;
            break;
          }
        }
        break;
    }

    CASE(F_CALL_OTHER);             /* --- call_other <nargs>  --- */
    {
        /* EFUN call_other()
         * 
         *     unknown call_other(object ob, string str, mixed arg, ...)
         *     unknown ob->fun(mixed arg, ...)
         *
         * Call a member function in another object with an argument. The
         * return value is returned from the other object.  The object can be
         * given directly or as a string (i.e. its file name). If it is given
         * by a string and the object does not exist yet, it will be loaded.
         *
         * TODO: The -> does not care for a simul_efun call_other(), this
         * TODO:: should be rectified in the compiler.
         */

        struct svalue *arg;
        struct object *ob;

        ASSIGN_EVAL_COST
        GET_NUM_ARG
        inter_pc = pc;
        inter_sp = sp;

        /* Get object and function to call */
        arg = sp - num_arg + 1;
        if (arg[0].type == T_OBJECT)
            ob = arg[0].u.ob;
        else if (arg[0].type == T_STRING)
        {
            ob = get_object(arg[0].u.string);
            if (ob == NULL)
                ERROR("call_other() failed\n")
        }
        else
            goto bad_arg_1;

        TYPE_TEST2(arg+1, T_STRING)
        if (arg[1].u.string[0] == ':')
            ERRORF(("Illegal function name in call_other: %s\n",
                  arg[1].u.string))
          
        /* No external calls may be done when this object is
         * destructed.
         */
        if (current_object->flags & O_DESTRUCTED)
        {
            pop_n_elems(num_arg);
            push_number(0);
            break;
        }
        
        /* Traceing, if necessary */
        if (TRACEP(TRACE_CALL_OTHER))
        {
            if (!++traceing_recursion)
            {
                do_trace("Call other ", arg[1].u.string, "\n");
            }
            traceing_recursion--;
        }

        /* Call the function with the remaining args on the stack.
         */
        if (!apply_low(arg[1].u.string, ob, num_arg-2, MY_FALSE))
        {
            /* Function not found */
            pop_n_elems(num_arg);
            push_number(0);
            break;
        }
        sp -= num_arg - 3;
        
        /* The result of the function call is on the stack. But, so
         * is the function name and object that was called.
         * These have to be removed.
         */
        arg = sp;           /* Remember where the function call result is */
        free_string_svalue(--sp);
        free_svalue(--sp);  /* Remove old arguments to call_other */
        *sp = *arg;         /* Re-insert function result */
        break;
    }

    CASE(F_FUNCALL);                /* --- funcall <nargs>     --- */
    {
        /* EFUN funcall()
         *
         *   mixed funcall(closure cl, mixed arg ...)
         *
         * Evaluates the closure. The extra args will be passed as args
         * to the closure. If cl is not a closure, it will simply be
         * returned.
         */

        struct svalue *args;

        GET_NUM_ARG
        args = sp -num_arg +1;

        if (args->type == T_CLOSURE)
        {
            /* No external calls may be done when this object is
             * destructed.
             */
            if (current_object->flags & O_DESTRUCTED) {
                pop_n_elems(num_arg);
                push_number(0);
                break;
            }
            
            inter_pc = pc;
            inter_sp = sp;
            ASSIGN_EVAL_COST

            /* Call the closure and push the result */
            call_lambda(args, num_arg - 1);
            sp = args;
            free_closure(sp);
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
        break;
    }

    CASE(F_LAMBDA);                 /* --- lambda              --- */
    {
        /* EFUN lambda()
         *
         *   closure lambda(mixed *arr, mixed)
         *
         * Constructs a lambda closure, like lambda function in LISP.
         * The closure is bound the creating object, and thus can contain
         * references to global variables.
         *
         * The first argument is an array describing the arguments
         * (symbols) passed to the closure upon evaluation by funcall()
         * or apply(). It may be 0 if no arguments are required.
         */

        struct lambda *l;
        struct vector *args;

        inter_pc = pc;
        inter_sp = sp;
        if (sp[-1].type != T_POINTER)
        {
            /* If '0' is given for the args array, replace it
             * with the null-vector.
             */
            if (sp[-1].type != T_NUMBER || sp[-1].u.number)
                goto bad_arg_1;
            (args = &null_vector)->ref++;
        }
        else
        {
            args = sp[-1].u.vec;
        }
        
        /* Create the lambda closure */
        l = lambda(args, sp, current_object);
        add_ref(l->ob = current_object, "lambda");

        /* Clean up the stack and push the result */
        pop_stack();
        free_vector(args);

        sp->type = T_CLOSURE;
        sp->x.closure_type = CLOSURE_LAMBDA;
        sp->u.lambda = l;
        break;
    }

    CASE(F_QUOTE);                  /* --- quote               --- */
    {
        /* EFUN quote()
         *
         *   mixed quote(mixed)
         *
         * Converts arrays to quoted arrays and strings to symbols.
         * Symbols and quoted arrays get quoted once more.
         */
      
        switch (sp->type)
        {
        case T_QUOTED_ARRAY:
        case T_SYMBOL:
            sp->x.quotes++;
            break;

        case T_POINTER:
            sp->type = T_QUOTED_ARRAY;
            sp->x.quotes = 1;
            break;

        case T_STRING:
            if (sp->x.string_type != STRING_SHARED)
            {
                /* Symbols must be shared strings */
              
                char *str = sp->u.string;
                
                sp->u.string = make_shared_string(str);
                if (sp->x.string_type == STRING_MALLOC)
                    xfree(str);
            }
            sp->type = T_SYMBOL;
            sp->x.quotes = 1;
            break;

        default:
            goto bad_arg_1;
        }
        
        break;
    }

    CASE(F_SYMBOL_FUNCTION);        /* --- symbol_function     --- */
    {
        /* EFUN symbol_function()
         *
         *   closure symbol_function(symbol arg)
         *   closure symbol_function(string arg)
         *   closure symbol_function(string arg, object|string ob)
         *
         * Constructs a lfun closure, efun closure or operator closure
         * from the first arg (string or symbol). For lfuns, the second
         * arg is the object that the lfun belongs to, specified by
         * the object itself or by its name (the object will be loaded
         * in the second case)
         *
         * Private lfuns can never be accessed this way, static and
         * protected lfuns only if <ob> is the current object.
         */

        struct object *ob;
        struct program *prog;
        int i;

        /* If 'arg' is not a symbol, make sure it's a shared string. */
        if (sp[-1].type != T_SYMBOL)
        {
            if (sp[-1].type == T_STRING)
            {
                if (sp[-1].x.string_type != STRING_SHARED)
                {
                    char *str;

                    str = sp[-1].u.string;
                    sp[-1].u.string = make_shared_string(str);
                    if (sp[-1].x.string_type == STRING_MALLOC)
                        xfree(str);
                    sp[-1].x.string_type = STRING_SHARED;
                }
            }
            else
                goto bad_arg_1;
        }
        
        /* If 'ob' is not of type object, it might be the name of
         * an object to load, or we need to make an efun symbol.
         */
        if (sp->type != T_OBJECT)
        {
            /* If it's the number 0, an efun symbol is desired */
            if (sp->type == T_NUMBER && sp->u.number == 0)
            {
                sp--;
                inter_pc = pc;
                symbol_efun(sp);
                break;
            }
            
            /* Find resp. load the object by name */
            TYPE_TEST2(sp, T_STRING)
            inter_sp = sp;
            inter_pc = pc;
            ob = get_object(sp->u.string);
            if (!ob)
                error("Object '%s' not found.\n", sp->u.string);
            add_ref(ob, "symbol_function");
            free_svalue(sp);
            sp->u.ob = ob;
            sp->type = T_OBJECT;
        }
        else
        {
            ob = sp->u.ob;
        }
        
        /* We need the object's program */
        if (O_PROG_SWAPPED(ob))
        {
            ob->time_of_ref = current_time;
            if (load_ob_from_swap(ob) < 0)
            {
                inter_sp = sp;
                error("Out of memory\n");
            }
        }
        
        /* Find the function in the program */
        prog = ob->prog;
        i = find_function(sp[-1].u.string, prog);
        
        /* If the function exists and is visible, create the closure
         */
        if ( i >= 0
          && ( !(prog->functions[i] & (TYPE_MOD_STATIC|TYPE_MOD_PROTECTED|TYPE_MOD_PRIVATE) )
             || (    !(prog->functions[i] & TYPE_MOD_PRIVATE)
                  && current_object == ob) 
             ) 
           )
        {
            struct lambda *l;
            ph_int closure_type;

            l = xalloc(sizeof *l);
            if (!l)
            {
                error("Out of memory.\n");
                /* NOTREACHED */
                break;
            }

            /* Set the closure */
            if (ob == current_object)
            {
                l->function.index = i;
                closure_type = CLOSURE_LFUN;
                  /* current_object already has an extra ref from being
                   * argument, so we don't need to add one for the l->ob
                   * below.
                   */
            }
            else
            {
                l->function.alien.ob = ob;
                l->function.alien.index = i;
                closure_type = CLOSURE_ALIEN_LFUN;
                add_ref(current_object, "symbol_function");
                  /* We adopt the ref of ob from the arguments, and
                   * add a ref to current_object for the l->ob below.
                   */
            }

            l->ref = 1;
            l->ob = current_object; /* see above regarding the ref count */
            
            /* Clean up the stack and push the result */
            sp--;
            decrement_string_ref(sp->u.string);
            sp->type = T_CLOSURE;
            sp->x.closure_type = closure_type;
            sp->u.lambda = l;
            
            /* A last thing: take care of a pending replace_program */
            if ( !(prog->flags & P_REPLACE_ACTIVE)
             ||  !lambda_ref_replace_program(l, closure_type, 0, 0, 0) )
            {
                ob->flags |= O_LAMBDA_REFERENCED;
            }
            
            break;
        }

        /* Symbol can't be created - free the stack and push 0 */
        free_object(ob, "symbol_function");
        sp--;
        free_string(sp->u.string);
        put_number(0);
        break;
    }

    CASE(F_CALL_OUT);               /* --- call_out <nargs>    --- */
        /* EFUN call_out()
         *
         *     void call_out(string fun, int delay, mixed arg, ...)
         *     void call_out(closure cl, int delay, mixed arg, ...)
         *
         * Set up a call to function fun or closure cl in the current
         * object. The call will take place in delay seconds, with the
         * remaining argument list provided. delay can be a minimum time
         * of one second.
         */

        GET_NUM_ARG
        inter_pc = pc;
        sp = new_call_out(sp, num_arg);
        break;
        
    CASE(F_CALL_OUT_INFO);          /* --- call_out_info       --- */
        /* EFUN call_out_info()
         *
         *     mixed *call_out_info(void)
         *
         * Get information about all pending call outs. The result is an
         * array in which every entry is itself an array describing one
         * call_out.
         *
         * The efun causes the the privilege violation ("call_out_info",
         * this_object()). If it is not satisfied, the result will be
         * the empty array.
         */

        inter_sp = sp;
        inter_pc = pc;
        if (privilege_violation("call_out_info", sp))
        {
            push_referenced_vector(get_all_call_outs());
        }
        else
        {
            push_vector(&null_vector);
        }
        break;
        
    CASE(F_FIND_CALL_OUT);          /* --- find_call_out       --- */
    {
        /* EFUN find_call_out()
         *
         *   int find_call_out(string func)
         *   int find_call_out(closure func)
         *
         * Find the first call-out due to be executed for function func
         * in the current object, and return the time left. If no call-out
         * is found return -1.
         */
      
        inter_pc = pc;
        find_call_out(current_object, sp, MY_FALSE);
        break;
    }
    
    CASE(F_REMOVE_CALL_OUT);        /* --- remove_call_out     --- */
    {
        /* EFUN remove_all_out()
         *
         *   int remove_call_out(string fun)
         *   int remove_call_out(closure fun)
         *
         * Remove next pending call-out for function fun in this object.
         * The time left is returned.
         *
         * -1 is returned if there were no call-outs pending to this
         * function.
         */
      
        inter_pc = pc;
        find_call_out(current_object, sp, MY_TRUE);
        break;
    }
    
    CASE(F_SET_HEART_BEAT);         /* --- set_heart_beat      --- */
    {
        /* EFUN set_heart_beat()
         *
         *   int set_heart_beat(int flag)
         *
         * Enable or disable heart beat. The driver will apply
         * the lfun heart_beat() to the current object every 2 seconds,
         * if it is enabled. If the heart beat is not needed for the
         * moment, then do disable it. This will reduce system overhead.
         *         
         * Return true for success, and false for failure.
         *  
         * Disabling an already disabled heart beat (and vice versa
         * enabling and enabled heart beat) counts as failure.
         */
      
        int i;

        TYPE_TEST1(sp, T_NUMBER)
        i = set_heart_beat(current_object, sp->u.number);
        sp->u.number = i;
        break;
    }
    
    /* --- Efuns: Objects --- */
        
    CASE(F_CLONE_OBJECT);           /* --- clone_object        --- */
    {
        /* EFUN clone_object()
         *
         *   object clone_object(string name)
         *   object clone_object(object template)
         *
         * Clone a new object from definition <name>, or alternatively from
         * the object <template>. In both cases, the new object is given an
         * unique name and returned.
         */

        struct object *ob;

        assign_eval_cost();
        inter_sp = sp;
        inter_pc = pc;

        /* Get the argument and clone the object */
        if (sp->type == T_STRING)
            ob = clone_object(sp->u.string);
        else if (sp->type == T_OBJECT)
            ob = clone_object(sp->u.ob->load_name);
        else
            goto bad_arg_1;

        free_svalue(sp);
        
        if (ob)
        {
            sp->type = T_OBJECT;
            sp->u.ob = ob;
            add_ref(ob, "F_CLONE_OBJECT");
        }
        else
        {
            put_number(0);
        }
        break;
    }

    CASE(F_DESTRUCT);               /* --- destruct            --- */
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

        if (T_NUMBER != sp->type || sp->u.number)
        {
            assign_eval_cost();
            TYPE_TEST1(sp, T_OBJECT)
            inter_sp = sp;
            inter_pc = pc;
            destruct_object(sp);
        }
        pop_stack();
        break;

    CASE(F_EXEC);                   /* --- exec                --- */
    {
        /* EFUN exec()
         *
         *   object exec(object new, object old)
         *
         * exec() switches the connection from the interactive object old
         * to the object new. If the new object is also interactive, it's
         * connection will be transferred to the old object, thus
         * exchaning the two connections between the object. If the new
         * object is not interactive, the old will not be interactive
         * anymore after the exec call succeeded.
         * It is used to load different "user objects" or to reconnect
         * link dead users.
         *
         * To provide security mechanisms, the interpreter calls
         * master->valid_exec(current_program, new, old), which must
         * return anything other than 0 to allow this exec() invocation.
         */

        int i;

        assign_eval_cost();
        TYPE_TEST1(sp-1, T_OBJECT)
        TYPE_TEST2(sp,   T_OBJECT)
        inter_sp = sp;
        inter_pc = pc;
        i = replace_interactive((sp-1)->u.ob, sp->u.ob, current_prog->name);
        pop_stack();
        free_svalue(sp); /* object might have been destructed */
        put_number(i);
        break;
    }

    CASE(F_FIND_OBJECT);            /* --- find_object         --- */
    {
        /* EXEC find_object()
         *
         *   object find_object(string str)
         *
         * Find an object with the file_name str. If the object isn't loaded,
         * it will not be found.
         */

        struct object *ob;

        TYPE_TEST1(sp, T_STRING)
        ob = find_object(sp->u.string);
        free_svalue(sp);
        if (ob)
            put_object(ob);
        else
            put_number(0);
        break;
    }

    CASE(F_FUNCTION_EXISTS);        /* --- function_exists     --- */
    {
        /* EXEC function_exists()
         *
         *   string function_exists(string str, object ob)
         *
         * Return the file name of the object that defines the function
         * str in object ob. The returned value can be different from
         * file_name(ob) if the function is defined in an inherited
         * object. In native mode, the returned name always begins with a
         * '/' (absolute path). 0 is returned if the function was not
         * defined, or was defined as static.
         */

        char *str, *res, *p;

        TYPE_TEST1(sp-1, T_STRING)
        TYPE_TEST2(sp,   T_OBJECT)
        inter_sp = sp; /* error possible when out of memory */
        str = function_exists((sp-1)->u.string, sp->u.ob);
        free_svalue(sp);
        free_svalue(--sp);
        if (str)
        {
            /* Make a copy of the string so that we can remove
             * a the trailing '.c'. In non-compat mode, we also
             * have to add the leading '/'.
             */
            p = strrchr (str, '.');

            if (p)
                *p = '\0';  /* temporarily mask out the '.c' */
            
#ifdef COMPAT_MODE
            res = string_copy (str);
#else
            res = add_slash(str);
#endif
            if (p)
                *p = '.';  /* undo the change above */
            
            if (!res)
            {
                sp--;
                ERROR("Out of memory\n")
            }
            put_malloced_string(res, sp);
        }
        else
        {
            put_number(0);
        }
        break;
    }

    CASE(F_INPUT_TO);               /* --- input_to <nargs>    --- */
    {
        /* EFUN input_to()
         *
         *   void input_to(string fun)
         *   void input_to(string fun, int flag, ...)
         *
         * Enable next line of user input to be sent to the local
         * function fun as an argument. The input line will not be
         * parsed, only when it starts with a "!" (like a kind of shell
         * escape) (this feature may be disabled).
         * The function <fun> may be static, but must not be private (or
         * it won't be found).
         *
         * Note that fun is not called immediately but after pressing the
         * RETURN key.
         *
         * If input_to() is called more than once in the same execution,
         * only the first call has any effect.
         *
         * The optional 3rd and following args will be passed as second and
         * subsequent args to the function fun. (This feature is was
         * added only recently, to avoid the need for global variables)
         */
      
        GET_NUM_ARG
        inter_pc = pc;
        sp = input_to(sp, num_arg);
        break;
    }

    CASE(F_INTERACTIVE);            /* --- interactive         --- */
    {
        /* EFUN interactive()
         *
         *   int interactive(object ob)
         *
         * Return non-zero if ob, or when the argument is omitted, this
         * object(), is an interactive user. Will return 1 if the
         * object is interactive, else 0.
         */

        int i;
        struct object *ob;
        struct interactive *ip;

        TYPE_TEST1(sp, T_OBJECT)
        ob = sp->u.ob;
        ip = O_GET_INTERACTIVE(ob);
        i = ip && ip->sent.type == SENT_INTERACTIVE && !ip->do_close;
        decr_object_ref(ob, "interactive");
        put_number(i);
        break;
    }

    CASE(F_LOAD_NAME);              /* --- load_name           --- */
    {
        /* EFUN load_name()
         *
         *   string load_name()
         *   string load_name(object obj)
         *
         * If <obj> is a clone, return the load_name() of <obj>'s blueprint.
         * If <obj> is a blueprint, return the filename from which the
         * blueprint was compiled.
         *
         * For virtual objects this efun of course returns the virtual
         * filename.  If <obj> is omitted, the name for the current object is
         * returned.
         *
         * In contrast to the object_name(), the load name can not be changed
         * by with rename_object(). However, if an object uses
         * replace_program() the load name no longer reflects the actual
         * behaviour of an object.
         *
         * The returned name starts with a '/', unless the driver is running
         * in COMPAT mode.
         */

        char *s;

        TYPE_TEST1(sp, T_OBJECT);
        s = sp->u.ob->load_name;
        free_object_svalue(sp);
        sp->type = T_STRING;
        sp->x.string_type = STRING_SHARED;
        increment_string_ref(sp->u.string = s);
        break;
    }

    CASE(F_LOAD_OBJECT);            /* --- load_object         --- */
    {
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

        struct object *ob;

        ASSIGN_EVAL_COST
        inter_pc = pc;
        inter_sp = sp;
        TYPE_TEST1(sp, T_STRING);
        ob = get_object(sp->u.string);
        free_svalue(sp);
        if (ob)
        {
            sp->type = T_OBJECT;
            sp->u.ob = ob;
            add_ref(ob, "F_LOAD_OBJECT");
        }
        else
        {
            put_number(0);
        }
        break;
    }

    CASE(F_OBJECT_NAME);            /* --- object_name         --- */
    {
        /* EFUN object_name()
         *
         *   string object_name()
         *   string object_name(object ob)
         *
         * Get the name of an object <ob> or, if no argument is given, of
         * the current object.
         *
         * This name is the name under which the object is stored in the
         * muds object table. It is initialised at the creation of the
         * object such that blueprints are named after the file they are
         * compiled from (without the trailing '.c'), and clones receive
         * the name of their blueprint, extended by '#' followed by
         * a unique non-negative number. These rules also apply to
         * virtual objects - the real name/type of virtual objects
         * is ignored.
         *
         * The name of an object can be changed with rename_object(), and
         * object_name() will reflect any of these changes.
         *
         * The returned name always begins with '/' (absolute path),
         * except when the parser runs in COMPAT mode.
         */

        char *name,*res;

        TYPE_TEST1(sp, T_OBJECT)

        name = sp->u.ob->name;
#ifdef COMPAT_MODE
        res = string_copy(name);
#else
        res = add_slash(name);
#endif
        if (!res)
            ERROR("Out of memory\n")
        free_object_svalue(sp);
        put_malloced_string(res, sp);
        break;
    }

    CASE(F_REPLACE_PROGRAM);        /* --- replace_program     --- */
    {
        /* EFUN replace_program()
         *
         *   void replace_program(string program)
         *
         * Substitutes a program with an inherited one. This is useful if you
         * consider the performance and memory consumption of the driver. A
         * program which doesn't need any additional variables and functions
         * (except during creation) can call replace_program() to increase the
         * function-cache hit-rate of
         * the driver which decreases with the number of programs in the
         * system. Any object can call replace_program() but looses all extra
         * variables and functions which are not defined by the inherited
         * program.
         *
         * When replace_program() takes effect, shadowing is stopped on
         * the object since 3.2@166.
         *
         * It is not possible to replace the program of an object after
         * (lambda) closures have been bound to it. It is of course
         * possible to first replace the program and then bind lambda
         * closures to it.
         *
         * The program replacement does not take place with the call to
         * the efun, but is merely scheduled to be carried out at the end
         * of the backend cycle. This may cause closures to have
         * references to then vanished lfuns of the object. This poses no
         * problem as long as these references are never executed after
         * they became invalid.
         */
      
        struct replace_ob *tmp;
        int name_len;
        char *name;
        struct program *new_prog;  /* the replacing program */
        int offsets[2];            /* the offsets of the replacing prog */

        TYPE_TEST1(sp, T_STRING)
          
        if (!current_object)
            ERROR("replace_program called with no current object\n")
        if (current_object == simul_efun_object)
            ERROR("replace_program on simul_efun object\n")
        if (current_object->flags & O_LAMBDA_REFERENCED)
            ERROR(
              "Cannot schedule replace_program after binding lambda closures\n")

        /* Create the full program name with a trailing '.c' and without
         * a leading '/' to match the internal name representation.
         */
        name_len = svalue_strlen(sp);
        name = alloca(name_len+3);
        strcpy(name, sp->u.string);
        if (name[name_len-2] != '.' || name[name_len-1] != 'c')
            strcat(name,".c");
        if (*name == '/')
            name++;
        
        new_prog = search_inherited(name, current_object->prog, offsets);
        if (!new_prog)
        {
            /* Given program not inherited, maybe it's the current already.
             */
            if (!strcmp(name, current_object->prog->name ))
            {
                new_prog = current_object->prog;
                offsets[0] = offsets[1] = 0;
            }
            else
            {
                ERROR("program to replace the current one with has "
                      "to be inherited\n")
            }
        }
        
        /* Program found, now create a new replace program entry, or
         * change an existing one.
         */
        if (!(current_object->prog->flags & P_REPLACE_ACTIVE)
         || !(tmp = retrieve_replace_program_entry()) )
        {
            tmp = xalloc(sizeof *tmp);
            tmp->lambda_rpp = NULL;
            tmp->ob = current_object;
            tmp->next = obj_list_replace;
            obj_list_replace = tmp;
            current_object->prog->flags |= P_REPLACE_ACTIVE;
        }
        
        tmp->new_prog = new_prog;
        tmp->var_offset = offsets[0];
        tmp->fun_offset = offsets[1];

        pop_stack();
        break;
    }

    CASE(F_SET_NEXT_RESET);         /* --- set_next_reset()    --- */
    {
        /* EFUN set_next_reset()
         *
         *   int set_next_reset (int delay)
         *
         * Instruct the gamedriver to reset this object not earlier than in
         * <delay> seconds. If a negative value is given as delay, the object
         * will never reset (useful for blueprints). If 0 is given, the
         * object's reset time is not changed.
         *
         * Result is the former delay to the objects next reset (which can be
         * negative if the reset was overdue).
         */
      
        int new_time;

        TYPE_TEST1(sp, T_NUMBER)
        new_time = sp->u.number;
        if (current_object->flags & O_DESTRUCTED)
        {
            sp->u.number = 0;
        }
        else
        {
#ifndef OLD_RESET
            sp->u.number = current_object->time_reset - current_time;
            if (new_time < 0)
                current_object->time_reset = 0;
            else if (new_time > 0)
                current_object->time_reset = new_time + current_time;
#else
            sp->u.number = current_object->next_reset - current_time;
            if (new_time < 0)
                current_object->next_reset = MAXINT;
            else if (new_time > 0)
                current_object->next_reset = new_time + current_time;
#endif
        }
        break;
    }

    CASE(F_SET_THIS_OBJECT);        /* --- set_this_object     --- */
    {
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
      
        TYPE_TEST1(sp, T_OBJECT)
        if (current_variables == master_ob->variables
         || current_variables == simul_efun_object->variables
         || privilege_violation("set_this_object", sp))
        {
            struct control_stack *p;

            /* Find the 'extern_call' entry in the call stack which
             * determined the current this_object().
             */
            for (p = csp; !p->extern_call; p--) NOOP;

            p->extern_call |= CS_PRETEND;
            p->pretend_to_be = current_object = sp->u.ob;
        }
        pop_stack();
        break;
    }

    CASE(F_SNOOP);                  /* --- snoop <nargs>       --- */
    {
        /* EFUN snoop()
         *
         *   object snoop(object snooper)
         *   object snoop(object snooper, object snoopee)
         *
         * Starts a snoop from 'snooper' on 'snoopee', or if 'snoopee' is not
         * given, terminates any snoop from 'snooper'.
         * On success, 'snoopee' is returned, else 0.
         * 
         * The snoop is checked with the master object for validity.
         * It will also fail if the 'snoopee' is being snooped already or
         * if a snoop would result in a recursive snoop action.
         */

        int i;

        assign_eval_cost();
        GET_NUM_ARG
        inter_pc = pc;
        inter_sp = sp;
        if (num_arg == 1)
        {
            TYPE_TEST1(sp,   T_OBJECT)
            i = set_snoop(sp->u.ob, 0);
        }
        else
        {
            TYPE_TEST1(sp-1, T_OBJECT)
            TYPE_TEST2(sp,   T_OBJECT)
            i = set_snoop((sp-1)->u.ob, sp->u.ob);
            pop_stack();
        }
        free_svalue(sp);
        put_number(i);
        break;
    }

    CASE(F_TELL_OBJECT);            /* --- tell_object         --- */
        /* EFUN tell_object()
         *
         *   void tell_object(object ob, string str)
         *
         * Send a message str to object ob. If it is an interactive
         * object (a user), then the message will go to him (her?),
         * otherwise the lfun catch_tell() of the living will be called
         * with the message as argument.
         */

        ASSIGN_EVAL_COST
        TYPE_TEST1(sp-1, T_OBJECT)
        TYPE_TEST2(sp,   T_STRING)
        inter_sp = sp;
        inter_pc = pc;
        tell_object((sp-1)->u.ob, sp->u.string);
        free_string_svalue(sp);
        sp--;
        if (sp->type == T_OBJECT) /* not self-destructed */
            free_object_svalue(sp);
        sp--;
        break;

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
            push_object(current_interactive);
        else
            push_number(0);
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
            push_number(0);
            break;
        }
        push_object(current_object);
        break;

    CASE(F_USERS);                  /* --- users               --- */
        /* EFUN users
         *
         *   object *users(void)
         *
         * Return an array containing all interactive users.
         */

        push_referenced_vector(users());
        break;

    CASE(F_WRITE);                  /* --- write               --- */
        /* EFUN write()
         *
         *   void write(mixed msg)
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
         *
         * If the write() function is invoked by a command of an living
         * but not interactive object and the given argument is a string
         * then the lfun catch_tell() of the living will be invoked with
         * the message as argument.
         *
         * TODO: this efun could be made the counterpart of MudOS' receive().
         * TODO: the accepted types and their interpretations should follow
         * TODO:: the to_string() or sprintf() representations.
         */

        assign_eval_cost();
        inter_pc = pc;
        inter_sp = sp;
        do_write(sp);
        pop_stack();
        break;

    /* --- Efuns: Files --- */
        
    CASE(F_CAT);                    /* --- cat <nargs>         --- */
    {
        /* EFUN cat()
         *
         *     int cat(string pathi [, int start [, int num]])
         *
         * List the file found at path.
         * The optional arguments start and num are start line
         * number and number of lines. If they are not given the whole
         * file is printed from the beginning.
         *
         * Result is the number of lines printed.
         */

        int i;
        struct svalue *arg;
        int start, len;

        assign_eval_cost();
        GET_NUM_ARG
        inter_pc = pc;
        inter_sp = sp;
        arg = sp- num_arg + 1;

        /* Get and test the arguments */
        TYPE_TEST1(arg, T_STRING)
        start = 0; len = 0;
        if (num_arg > 1)
        {
            TYPE_TEST2(arg+1, T_NUMBER)
            start = arg[1].u.number;
            if (num_arg == 3)
            {
                if (arg[2].type != T_NUMBER)
                    goto bad_arg_3;
                len = arg[2].u.number;
            }
        }

        /* Print the file */
        i = print_file(arg[0].u.string, start, len);
        pop_n_elems(num_arg);
        push_number(i);
        break;
    }
    
    CASE(F_FILE_SIZE);              /* --- file_size           --- */
    {
        /* EFUN file_size()
         *
         *   int file_size(string file)
         *
         * Returns the size of the file in bytes.
         *        
         * Size FSIZE_NOFILE (-1) indicates that the file either does not
         * exist, or that it is not readable for the calling object/user.
         * Size FSIZE_DIR (-2) indicates that it is a directory.
         */
      
        int i;

        assign_eval_cost();
        TYPE_TEST1(sp, T_STRING)
        inter_sp = sp;
        inter_pc = pc;
        i = file_size(sp->u.string);
        free_svalue(sp);
        put_number(i);
        break;
    }

    CASE(F_GET_DIR);                /* --- get_dir             --- */
    {
        /* EFUN get_dir()
         *
         *   string *get_dir(string str, int mask)
         *
         * This function takes a path as argument and returns an array of
         * file names and attributes in that directory.
         *
         * The filename part of the path may contain '*' or '?' as
         * wildcards: every '*' matches an arbitrary amount of characters
         * (or just itself). Thus get_dir ("/path/ *") would return an
         * array of all files in directory "/path/", or just ({ "/path/ *"
         * }) if this file happens to exist.
         *
         * The optional second argument mask can be used to get
         * information about the specified files.
         *
         * GETDIR_EMPTY (0x00)  return an empty array (not very useful)
         * GETDIR_NAMES (0x01)  put the file names into the returned array.
         * GETDIR_SIZES (0x02)  put the file sizes into the returned array.
         * GETDIR_DATES (0x04)  put the file modification dates into the
         *                      returned array.
         * GETDIR_UNSORTED (0x20) if this mask bit is set, the result of
         *                      get_dir() will _not_ be sorted.
         * The values of mask can be added together.
         */
      
        struct vector *v;
        
        TYPE_TEST1(sp-1, T_STRING)
        TYPE_TEST2(sp,   T_NUMBER)
        inter_sp = sp;
        inter_pc = pc;
        
        v = get_dir(sp[-1].u.string, sp->u.number);
        sp--; /* think 'free_int_svalue(sp--) */
        free_string_svalue(sp);
        if (v)
        {
            sp->type  = T_POINTER;
            sp->u.vec = v;
        }
        else
        {
            put_number(0);
        }
        break;
    }
    
    CASE(F_MKDIR);                  /* --- mkdir               --- */
    {
        /* EFUN mkdir()
         *
         *   int mkdir(string path)
         *
         * Make a directory named path. Return 1 for success and 0 for
         * failure.
         */

        int i;
        char *path;

        assign_eval_cost();
        inter_pc = pc;
        inter_sp = sp;
        TYPE_TEST1(sp, T_STRING)
          
        path = check_valid_path(sp->u.string, current_object, "mkdir", MY_TRUE);
        i = !(path == 0 || mkdir(path, 0775) == -1);
        free_svalue(sp);
        put_number(i);
        break;
    }
    
    CASE(F_READ_BYTES);             /* --- read_bytes <nargs>  --- */
    {
        /* EFUN read_bytes()
         *
         *   string read_bytes (string file, int start, int number)
         *
         * Reads a given amount of bytes from file.
         * If <start> is not given or 0, the file is read from the
         * beginning, else from the <start>th byte on. If <start> is
         * negative, it is counted from the end of the file.
         * <number> is the number of bytes to read. 0 or negative values
         * are possible, but not useful.
         * If <start> would be outside the actual size of the file, 0 is
         * returned instead of a string.
         * TODO: Can't read nul-characters.
         */

        char *str;
        struct svalue *arg;
        int start, len;

        assign_eval_cost();
        GET_NUM_ARG
        inter_pc = pc;
        inter_sp = sp;
        arg = sp- num_arg + 1;
        TYPE_TEST1(arg,   T_STRING)

        /* Get the arguments */
        start = 0;
        len = 0;
        if (num_arg > 1)
        {
            TYPE_TEST2(arg+1, T_NUMBER)
            start = arg[1].u.number;
            if (num_arg == 3)
            {
                if (arg[2].type != T_NUMBER)
                    goto bad_arg_2;
                len = arg[2].u.number;
                sp--;
            }
            sp--;
        }

        /* Read the file */
        str = read_bytes(arg[0].u.string, start, len);
        pop_stack();
        if (str == 0)
            push_number(0);
        else
        {
            push_string_malloced(str);
            xfree(str);
        }
        break;
    }
    
    CASE(F_READ_FILE);              /* --- read_file           --- */
    {
        /* EFUN read_file()
         *
         *   string read_file(string file, int start, int number)
         *
         * Reads lines from file.
         * If <start> is not given or 0, the file is read from the
         * beginning, else from the numbered line on.
         * If <number> is not given or 0, the whole file is read, else
         * just the given amount of lines.
         * If <start> would be outside the actual size of the file, 0 is
         * returned instead of a string.
         */

        char *str;
        struct svalue *arg;
        int start, len;

        assign_eval_cost();
        GET_NUM_ARG
        inter_pc = pc;
        inter_sp = sp;
        arg = sp- num_arg + 1;
        TYPE_TEST1(arg,   T_STRING)

        /* Get the arguments */
        start = 0;
        len = 0;
        if (num_arg > 1)
        {
            TYPE_TEST2(arg+1, T_NUMBER)
            start = arg[1].u.number;
            if (num_arg == 3)
            {
                if (arg[2].type != T_NUMBER)
                    goto bad_arg_3;
                len = arg[2].u.number;
                sp--;
            }
            sp--;
        }

        /* Read the file */
        str = read_file(arg[0].u.string, start, len);
        pop_stack();
        if (str == 0)
            push_number(0);
        else
        {
            push_malloced_string(str);
        }
        break;
    }
    
    CASE(F_RENAME);                 /* --- rename              --- */
    {
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

        int i;

        assign_eval_cost();
        inter_pc = pc;
        inter_sp = sp;
        TYPE_TEST1(sp-1, T_STRING)
        TYPE_TEST2(sp,   T_STRING)
        
        i = do_rename((sp-1)->u.string, sp->u.string);
        pop_n_elems(2);
        push_number(i);
        break;
    }

    CASE(F_RESTORE_OBJECT);         /* --- restore_object      --- */
    {
        /* EFUN restore_object()
         *
         *   int restore_object(string name)
         *
         * Restore values of variables for current object from file name.
         * If name ends in ``.c'', this suffix is stripped of by the parser.
         *
         * The master object will probably append a .o to the name.
         *
         * Return 1 on success, 0 on failure.
         */
      
        int i;

        assign_eval_cost();
        TYPE_TEST1(sp, T_STRING)
        inter_sp = sp;
        inter_pc = pc;
        i = restore_object(current_object, sp->u.string) ? 1 : 0;
        free_svalue(sp);
        put_number(i);
        break;
    }

    CASE(F_RM);                     /* --- rm                  --- */
    {
        /* EFUN rm()
         *
         *   int rm(string file)
         *
         * Remove the file. Returns 0 for failure and 1 for success.
         */
      
        int i;

        assign_eval_cost();
        TYPE_TEST1(sp, T_STRING)
        inter_sp = sp;
        inter_pc = pc;
        i = remove_file(sp->u.string);
        free_svalue(sp);
        put_number(i);
        break;
    }
    
    CASE(F_RMDIR);                  /* --- rmdir               --- */
    {
        /* EFUN rmdir()
         *
         *   int rmdir(string dir)
         *
         * Remove directory dir. Return 1 on success, 0 on failure.
         */

        int i;
        char *path;

        assign_eval_cost();
        inter_pc = pc;
        inter_sp = sp;
        TYPE_TEST1(sp, T_STRING)
        path = check_valid_path(sp->u.string, current_object, "rmdir", MY_TRUE);
        i = !(path == 0 || rmdir(path) == -1);
        free_svalue(sp);
        put_number(i);
        break;
    }
    
    CASE(F_SAVE_OBJECT);            /* --- save_object         --- */
        /* EFUN save_object()
         *
         *   void save_object(string name)
         *
         * Save values of variables of this object in the file name.
         */

        assign_eval_cost();
        TYPE_TEST1(sp, T_STRING)
        inter_sp = sp;
        inter_pc = pc;
        save_object(current_object, sp->u.string);
        pop_stack();
        break;
        
    CASE(F_TAIL);                   /* --- tail                --- */
        /* EFUN tail()
         *
         *   void tail(string file)
         *
         * Print out the tail of a file. There is no specific amount of
         * lines given to the output. Only a maximum of 1000 bytes will
         * be printed.
         */

        assign_eval_cost();
        TYPE_TEST1(sp, T_STRING)
        inter_sp = sp;
        inter_pc = pc;
        if (tail(sp->u.string))
            assign_svalue(sp, &const1);
        else
            assign_svalue(sp, &const0);
        break;
        
    CASE(F_WRITE_BYTES);            /* --- write_bytes         --- */
    {
        /* EFUN write_bytes()
         *
         *   int write_bytes(string file, int start, string str)
         *
         * Write string str to file file by overwriting the old bytes at
         * position start. If start is a negative value then it will be
         * counted from the end of the file. The file will not be
         * appended, instead the function will be aborted. Returns 1 for
         * success 0 for failure during execution.
         */
      
        int i;

        assign_eval_cost();
        TYPE_TEST1(sp-2, T_STRING)
        TYPE_TEST2(sp-1, T_NUMBER)
        inter_sp = sp;
        inter_pc = pc;
        if (sp->type != T_STRING)
            goto bad_arg_3;
        
        i = write_bytes((sp-2)->u.string, (sp-1)->u.number, sp->u.string);
        pop_stack();
        sp--;
        free_svalue(sp);
        put_number(i);
        break;
    }
    
    CASE(F_WRITE_FILE);             /* --- write_file          --- */
    {
        /* EFUN write_file()
         *
         *   int write_file(string file, string str)
         *
         * Append the string str to the file file. Returns 1 for success
         * and 0 if any failure occured.
         */

        int i;

        assign_eval_cost();
        TYPE_TEST1(sp-1, T_STRING)
        TYPE_TEST2(sp,   T_STRING)
        inter_sp = sp;
        inter_pc = pc;

        i = write_file((sp-1)->u.string, sp->u.string);
        pop_stack();
        free_svalue(sp);
        put_number(i);
        break;
    }
    
    /* --- Efuns: Driver and System --- */
        
    CASE(F_QUERY_LOAD_AVERAGE);     /* --- query_load_average  --- */
        /* EFUN query_load_average()
         *
         *   string query_load_average(void)
         *
         * Returns the load of the mud. Two figures are given, executed
         * commands/second and compiled lines/second.
         */

        push_string_malloced(query_load_av());
        break;
        
    /* --- Efuns: Inventories --- */
        
    CASE(F_ALL_INVENTORY);          /* --- all_inventory       --- */
    {
        /* EFUN all_inventory()
         *
         *     object *all_inventory(object ob = this_object())
         *
         * Returns an array of the objects contained in the inventory
         * of ob.
         */
      
        struct vector *vec;

        TYPE_TEST1(sp, T_OBJECT)
        inter_sp = sp;
        inter_pc = pc;
        
        vec = all_inventory(sp->u.ob);
        free_object_svalue(sp);

        if (vec == NULL)
        {
            put_number(0);
        }
        else
        {
            sp->type  = T_POINTER;
            sp->u.vec = vec;  /* ref count is already 1 */
        }
        break;
    }

    CASE(F_DEEP_INVENTORY);         /* --- deep_inventory      --- */
    {
        /* EFUN deep_inventory()
         *
         *   object *deep_inventory(void)
         *   object *deep_inventory(object ob)
         *
         * Returns an array of the objects contained in the inventory of
         * ob (or this_object() if no arg given) and in the inventories
         * of these objects, climbing down recursively.
         */

        struct vector *vec;

        TYPE_TEST1(sp, T_OBJECT)
        inter_sp = sp;
        inter_pc = pc;
        vec = deep_inventory(sp->u.ob, 0);
        free_object_svalue(sp);
        sp->type = T_POINTER;
        sp->u.vec = vec;
        break;
    }

    CASE(F_ENVIRONMENT);            /* --- environment <nargs> --- */
    {
        /* EFUN environment()
         *
         *   object environment(void)
         *   object environment(object obj)
         *
         * Returns the surrounding object of obj. If no argument are
         * given, it returns the surrounding of the current object.
         *
         * Destructed objects do not have an environment.
         */

        struct object *ob;

        GET_NUM_ARG
        if (num_arg)
        {
            if (sp->type == T_OBJECT)
            {
                ob = sp->u.ob->super;
                free_object_svalue(sp);
            }
            else if (sp->type == T_STRING)
            {
                inter_sp = sp;
                inter_pc = pc;
                ob = environment(sp);
                free_string_svalue(sp);
            }
            else
                goto bad_arg_1;
        }
        else if (!(current_object->flags & O_DESTRUCTED))
        {
            ob = current_object->super;
            sp++;
        }
        else
        {
            ob = NULL; /* != environment(this_object()) *boggle* */
            sp++;
        }

        if (ob)
            put_object(ob);
        else
            put_number(0);
        break;
    }
    
    CASE(F_FIRST_INVENTORY);        /* --- first_inventory     --- */
    {
        /* EFUN first_inventory()
         *
         *   object first_inventory()
         *   object first_inventory(string ob)
         *   object first_inventory(object ob)
         *
         * Get the first object in the inventory of ob, where ob is
         * either an object or the file name of an object. If ob is not
         * given, the current object is assumed.
         */

        struct object *ob;

        if (sp->type == T_OBJECT)
        {
            ob = sp->u.ob->contains;
            free_object_svalue(sp);
        }
        else if (sp->type == T_STRING)
        {
            inter_sp = sp;
            inter_pc = pc;
            ob = first_inventory(sp);
            free_string_svalue(sp);
        }
        else
            goto bad_arg_1;
        
        if (ob)
            put_object(ob);
        else
            put_number(0);
        break;
    }

    CASE(F_MOVE_OBJECT);            /* --- move_object         --- */
    {
        /* EFUN move_object()
         *
         *   void move_object(mixed item, mixed dest)
         *
         * The item, which can be a file_name or an object, is moved into
         * it's new environment dest, which can also be file_name or an
         * object.
         *
         * In !compat mode, the only object that can be moved with
         * move_object() is the calling object itself.
         *
         * Since 3.2.1, the innards of move_object() are implemented in
         * the mudlib, using the M_MOVE_OBJECT driver hooks.
         */

        struct object *item, *dest;

        ASSIGN_EVAL_COST
        inter_pc = pc;
        inter_sp = sp;
        
        if ((sp-1)->type == T_OBJECT)
            item = (sp-1)->u.ob;
        else if ((sp-1)->type == T_STRING)
        {
            item = get_object((sp-1)->u.string);
            if (!item)
                error("move_object failed\n");
            free_string_svalue(sp-1);
            sp[-1].type = T_OBJECT;
            sp[-1].u.ob = item;
            add_ref(item, "move_object");
        }
        else
            goto bad_arg_1;
        
        if (sp->type == T_OBJECT)
            NOOP;
        else if (sp->type == T_STRING)
        {
            dest = get_object(sp->u.string);
            if (!dest)
                error("move_object failed\n");
            free_string_svalue(sp);
            sp->type = T_OBJECT;
            sp->u.ob = dest;
            add_ref(dest, "move_object");
        }
        else
            goto bad_arg_2;

        /* move_object() reads its arguments directly from the stack */
        move_object();
        sp -= 2;
        break;
    }
    
    CASE(F_NEXT_INVENTORY);         /* --- next_inventory      --- */
    {
        /* EFUN next_inventory()
         *
         *   object next_inventory()
         *   object next_inventory(object ob)
         *
         * Get next object in the same inventory as ob. If ob is not
         * given, the current object will be used.
         *
         * This efun is mostly used together with the efun
         * first_inventory().
         */

        struct object *ob;

        TYPE_TEST1(sp, T_OBJECT)
        ob = sp->u.ob;
        free_object(ob, "next_inventory");
        if (ob->next_inv)
            put_object(ob->next_inv);
        else
            put_number(0);
        break;
    }

    CASE(F_PRESENT);                /* --- present             --- */
      {
        /* EFUN present()
         *
          * object present(mixed str)
         * object present(mixed str, object ob)
         *
         * If an object that identifies (*) to the name ``str'' is present
         * in the inventory or environment of this_object (), then return
         * it. If "str" has the form "<id> <n>" the <n>-th object matching
         * <id> will be returned.
         *
         * "str" can also be an object, in which case the test is much faster
         * and easier.
         *
         * A second optional argument ob is the enviroment where the search
         * for str takes place. Normally this_player() is a good choice.
         * Only the inventory of ob is searched, not its environment.
         */
          
        struct svalue *arg;
        struct object *ob;

        assign_eval_cost();
        GET_NUM_ARG
        inter_pc = pc;
        arg = sp - num_arg + 1;

        /* Get the arguments */
        if (arg->type != T_STRING && arg->type != T_OBJECT)
            goto bad_arg_1;
        ob = NULL;
        if (num_arg > 1)
        {
            TYPE_TEST2(arg+1, T_OBJECT)
            ob = arg[1].u.ob;
            pop_stack();
        }
        
        inter_sp = sp;
        ob = object_present(arg, ob);

        free_svalue(arg);
        if (ob)
            put_object(ob);
        else
            put_number(0);
        break;
      }
        
    CASE(F_SAY);                    /* --- say <nargs>         --- */
      {
        /* EFUN say()
         *
         *   void say(string str)
         *   void say(string str, object exclude)
         *   void say(string str, object *excludes)
         *   void say(mixed *arr)
         *   void say(mixed *arr, object exclude)
         *   void say(mixed *arr, object *excludes)
         *
         * There are two major modes of calling:
         *
         * If the first argument is a string and no second argument is
         * given, str will be send to all livings in the current room
         * except to the initiating object which called the say() function.
         * If the second argument is an object the str also won't be sent
         * to this on. If the second argument is an array of objects the
         * message won't be sent to the objects in the array. If the
         * receiving object(s) is not interactive, the lfun catch_tell()
         * will be applied to the object, with str as argument.
         *
         * If the first argument is an array, the applied lfun catch_msg()
         * of all living objects (except the initiating one which invoked
         * the say() function) will be called. This array will be given as
         * first argument and the one who invoked say() as second argument to
         * lfun catch_msg(). If the second argument to say() is an object
         * or an array of objects then those objects will also be
         * excluded from the call of catch_msg().
         *
         * The 'initiating object' determines according to these rules:
         *  - if the say() is called from within a living object, this
         *     becomes the initiator
         *  - if the say() is called from within a dead object as result
         *    of a user action (i.e. this_player() is valid), this_player()
         *     becomes the initiator.
         *  - Else the object calling the say() becomes the initiator.
         */

        static struct {
            INIT_VEC_TYPE;
            struct svalue second_item[1];
        } vtmp = { VEC_INIT(2, 1, T_NUMBER), { { T_OBJECT } } };
          /* Informational struct passed to say() giving the object
           * to exclude in the second item.
           * TODO: Update this after commenting say() 
           */

        ASSIGN_EVAL_COST
        GET_NUM_ARG
        inter_pc = pc;
        inter_sp = sp;
        
        if (num_arg == 1)
        {
            /* No objects to exclude */

            if (sp->type != T_STRING && sp->type != T_POINTER)
                goto bad_arg_1;
            
            vtmp.v.item[0].type = T_NUMBER;
              /* this marks the place for the command_giver */
            vtmp.v.item[1].type = T_NUMBER;
              /* will not match any object... */
            say(sp, &vtmp.v);
        }
        else
        {
            /* We have objects to exclude */

            if (sp[-1].type != T_STRING && sp[-1].type != T_POINTER)
                goto bad_arg_1;
            
            if ( sp->type == T_POINTER )
            {
                say(sp-1, sp->u.vec);
            }
            else if (sp->type == T_OBJECT)
            {
                vtmp.v.item[0].type = T_NUMBER;
                vtmp.v.item[1].type = T_OBJECT;
                vtmp.v.item[1].u.ob = sp->u.ob;
                add_ref(sp->u.ob, "ass to var");
                say(sp-1, &vtmp.v);
            }
            else
                goto bad_arg_2;
            pop_stack();
        }
        
        pop_stack();
        break;
      }
  
    CASE(F_TELL_ROOM);              /* --- tell_room <nargs>   --- */
      {
        /* EFUN tell_room()
         *
         *   void tell_room(object ob, string str)
         *   void tell_room(object ob, string str, object *exclude)
         *   void tell_room(object ob, mixed *msg)
         *   void tell_room(object ob, mixed *msg, object *exclude)
         *
         * Send a message str to all living objects in the room ob. ob
         * can also be the name of the room given as a string. If a
         * receiving object is not a interactive user the lfun
         * catch_tell() of the object will be invoked with the message as
         * argument. If living objects define catch_tell(), the string
         * will also be sent to that instead of being written to the
         * user.
         * If array *exclude is given, all objects contained in
         * *exclude are excluded from the message str.
         *
         * If the second arg is an array, catch_msg() will be called in
         * all listening livings.
         */

        struct svalue *arg;
        struct vector *avoid;
        struct object *ob;

        ASSIGN_EVAL_COST
        GET_NUM_ARG
        inter_pc = pc;
        inter_sp = sp;
        arg = sp- num_arg + 1;
        
        /* Test the arguments */
        if (arg[0].type == T_OBJECT)
            ob = arg[0].u.ob;
        else if (arg[0].type == T_STRING)
        {
            ob = get_object(arg[0].u.string);
            if (!ob)
                ERROR("Object not found.\n")
        }
        else
            goto bad_arg_1;
        
        if (arg[1].type != T_STRING && arg[1].type != T_POINTER)
            goto bad_arg_2;
        
        if (num_arg == 2)
        {
            avoid = &null_vector;
        }
        else
        {
            /* Sort the list of objects to exclude for faster
             * operation.
             */
  
            struct vector *vtmpp;
            static struct svalue stmp = { T_POINTER };

            if (arg[2].type != T_POINTER)
                goto bad_arg_3;

            stmp.u.vec = arg[2].u.vec;
            vtmpp = order_alist(&stmp, 1, MY_TRUE);
            avoid = vtmpp->item[0].u.vec;
            sp->u.vec = avoid; /* in case of an error, this will be freed. */
            sp--;
            vtmpp->item[0].u.vec = stmp.u.vec;
            free_vector(vtmpp);
        }
        
        tell_room(ob, sp, avoid);

        if (num_arg > 2)
            free_vector(avoid);
        pop_stack();
        pop_stack();
        break;
      }

    /* --- Efuns: Verbs and Commands --- */
        
    CASE(F_ADD_ACTION);             /* --- add_action <nargs>  --- */
    {
        /* EFUN add_action()
         *
         *   void add_action(string fun, string cmd [, int flag])
         *   void add_action(string fun) // historical
         *
         * Add an action (verb + function) to the commandgiver.
         * TODO: In the long run, get rid of actions.
         */

        struct svalue *arg;
        struct svalue *verb;

        GET_NUM_ARG
        inter_pc = pc;
        inter_sp = sp;
        arg = sp - num_arg + 1;
        TYPE_TEST1(arg, T_STRING)
          
        verb = NULL;
        if (num_arg >= 2)
        {
            TYPE_TEST2(arg+1, T_STRING)
            if (num_arg > 2)
            {
                if (arg[2].type != T_NUMBER)
                    goto bad_arg_3;
            }
            verb = &arg[1];
        }

        if (add_action( &arg[0], verb
                      , num_arg > 2 ? arg[2].u.number : 0))
        {
            /* silent error condition, deallocate strings by hand */
            pop_n_elems(num_arg);
        }
        else
        {
            /* add_action has reused the strings or freed it */
            sp -= num_arg;
        }
        break;
    }

    CASE(F_COMMAND);                /* --- command <nargs>     --- */
    {
        /* EFUN command()
         *
         *   int command(string str)             // native
         *   int command(string str, object ob)  // !native
         *
         * Execute str as a command given directly by the user. Any
         * effects of the command will apply to the current object.
         *
         * Return value is 0 for failure. Otherwise a numeric value is
         * returned which tells the evaluation cost. Bigger number means
         * higher cost.  The evaluation cost is approximately the number
         * of LPC machine code instructions executed.
         *
         * In native mode, command() can effect only the calling object.
         * If native mode is not enabled, command() can get an optional
         * second arg, that specifies the object that the command is to
         * be applied to.
         * If command() is called on another object, it is not possible
         * to call static functions in this way, to give some protection
         * against illegal forces.
         *
         * TODO: With add_action(), this should go in the long run.
         */

        int i;
        struct svalue *arg;

        assign_eval_cost();
        GET_NUM_ARG
        inter_pc = pc;
        inter_sp = sp;
        
        arg = sp - num_arg + 1;
        if (num_arg == 1)
        {
            TYPE_TEST1(sp,   T_STRING)
            i = command_for_object(arg[0].u.string, NULL);
        }
        else
        {
            TYPE_TEST1(sp-1, T_STRING)
            TYPE_TEST2(sp,   T_OBJECT)
#ifndef NATIVE_MODE
            i = command_for_object(arg[0].u.string, arg[1].u.ob);
            pop_stack();
#else
            ERROR("Too many arguments to command()\n")
#endif
        }
        
        free_svalue(sp);
        put_number(i);
        break;
    }

    CASE(F_DISABLE_COMMANDS);       /* --- disable_commands    --- */
        /* EFUN disable_commands()
         *
         *   void disable_commands()
         *
         * Disable this object to use commands normally accessible to
         * users.
         */

        inter_sp = sp;
        enable_commands(MY_FALSE);
        break;
        
    CASE(F_ENABLE_COMMANDS);        /* --- enable_commands     --- */
        /* EFUN enable_commands()
         *
         *   void enable_commands()
         *
         * Enable this object to use commands normally accessible to
         * users.
         */

        inter_sp = sp;
        enable_commands(MY_TRUE);
        break;
        
    CASE(F_LIVING);                 /* --- living              --- */
    {
        /* EFUN living()
         *
         * int living(object ob)
         *
         * Return true if ob is a living object (that is,
         * enable_commands() has been called from inside the ob).
         * ob may be 0, in which case the result is obviously 0, too.
         */

        int i;

        if (sp->type != T_OBJECT)
        {
            if (sp->type == T_NUMBER && !sp->u.number)
                break;
            goto bad_arg_1;
        }
        
        i = (sp->u.ob->flags & O_ENABLE_COMMANDS) != 0;
        free_object_svalue(sp);
        put_number(i);
        break;
    }
    
    CASE(F_NOTIFY_FAIL);            /* --- notify_fail         --- */
        /* EFUN notify_fail()
         *
         *   int notify_fail(string str)
         *   int notify_fail(closure cl)
         *
         * Store str as the error message given instead of the default
         * message ``What ?''. The result is always 0.
         *
         * If a closure is given, it is executed to return the error
         * message string, but not before all attempts to execute the
         * commandline failed (read: not at the time of the call to
         * notify_fail()).
         *
         * If notify_fail() is called more than once, only the last call
         * will be used.
         */
    
        if (sp->type != T_STRING && sp->type != T_CLOSURE)
            goto bad_arg_1;
        set_notify_fail_message(sp);
        put_number(0);
        break;
        
    CASE(F_QUERY_ACTIONS);          /* --- query_actions       --- */
    {
        /* EFUN query_actions()
         *
         *   mixed *query_actions(object ob, mixed mask_or_verb)
         *
         * query_actions takes either an object or a filename as first
         * argument and a bitmask (int) or string as a second argument.
         * If the second argument is a string, query_actions() will return
         * an array containing information (see below) on the verb or
         * zero if the living object "ob" cannot use the verb. If the
         * second argument is a bitmask, query_actions() will return a
         * flat array containing information on all verbs added to ob.
         * The second argument is optional (default is the bitmask 1).
         *      1:  the verb
         *      2:  type
         *      4:  short_verb
         *      8:  object
         *     16: function
         *
         * "type" is one of the values defined in <sent.h> (/sys/sent.h)
         * (which is provided with the parser source).
         *
         * SENT_PLAIN       added with add_action (fun, cmd);
         * SENT_SHORT_VERB  added with add_action (fun, cmd, 1);
         * SENT_NO_SPACE    added with add_action (fun); add_xverb (cmd);
         * SENT_NO_VERB     just an add_action (fun); without a verb
         * SENT_MARKER      internal, won't be in the returned array
         */
      
        struct vector *v;
        struct svalue *arg;
        struct object *ob;

        arg = sp - 1;
        inter_sp = sp;
        inter_pc = pc;
        
        /* Get the arguments */
        if (arg[0].type == T_OBJECT)
            ob = arg[0].u.ob;
        else
        {
            TYPE_TEST1(arg, T_STRING);
            ob = get_object(arg[0].u.string);
            if (!ob)
                error("query_actions() failed\n");
        }

        /* Get the actions */
        if (arg[1].type == T_STRING)
            v = get_action(ob, arg[1].u.string);
        else if (arg[1].type == T_NUMBER)
            v = get_all_actions(ob, arg[1].u.number);
        else {
            TYPE_TEST2(arg+1, T_OBJECT);
            v = get_object_actions(ob, arg[1].u.ob);
        }

        /* Clean up the stack and push the result */
        pop_stack();
        free_svalue(arg);
        if (v)
        {
            arg->type  = T_POINTER;
            arg->u.vec = v;
        } else
            put_number(0);
        
        break;
    }

    CASE(F_QUERY_VERB);             /* --- query_verb          --- */
        /* EFUN query_verb()
         *
         *   string query_verb(void)
         *
         * Give the name of the current command, or 0 if not executing
         * from a command. This allows add_action() of several commands
         * to the same function. query_verb() returns 0 when invoked by a
         * function which was started by a call_out or the heart beat.
         * Also when a user logs in query_verb() returns 0.
         */
    
        if (!last_verb)
        {
            push_number(0);
            break;
        }
        push_shared_string(last_verb);
        break;

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
            push_object(command_giver);
        else
            push_number(0);
        break;
        
    /* --- Efuns: Not used in Compat mode --- */
        
#ifdef F_EXPORT_UID
    CASE(F_EXPORT_UID);             /* --- export_uid          --- */
    {
        /* EFUN export_uid()
         *
         *   void export_uid(object ob)
         *
         * Set the uid of object ob to the current object's effective uid.
         * It is only possible when object ob has an effective uid of 0.
         */
      
        struct object *ob;

        TYPE_TEST1(sp, T_OBJECT)
        if (!current_object->eff_user)
            ERROR("Illegal to export uid 0\n")
        ob = sp->u.ob;
        if (!ob->eff_user)        /* Only allowed to export when null */
            ob->user = current_object->eff_user;
        free_object(ob, "export_uid");
        sp--;
        break;
    }
#endif /* F_EXPORT_UID */
    
#ifdef F_GETEUID
    CASE(F_GETEUID);             /* --- geteuid                --- */
    {
        /* EFUN geteuid()
         *
         *   string geteuid(object ob)
         *
         * Get the effective user-id of the object (mostly a wizard or
         * domain name). Standard objects cloned by this object will get
         * that userid. The effective userid is also used for checking access
         * permissions. If ob is omitted, is this_object() as default.
         */
      
        struct object *ob;

        TYPE_TEST1(sp, T_OBJECT)
        ob = sp->u.ob;

        if (ob->eff_user)
        {
            char *tmp;
            tmp = ob->eff_user->name;
            pop_stack();
            push_constant_string(tmp);
        }
        else
        {
            free_svalue(sp);
            put_number(0);
        }
        break;
    }
#endif /* F_GETEUID */
    
#ifdef F_SETEUID
    CASE(F_SETEUID);                /* --- seteuid                --- */
    {
        /* EFUN seteuid()
         *
         *   int seteuid(string str)
         *
         * Set effective uid to str. The calling object must be
         * privileged to do so by the master object. In most
         * installations it can always be set to the current uid of the
         * object, to the uid of the creator of the object file, or to 0.
         *
         * When this value is 0, the current object's uid can be changed
         * by export_uid(), and only then.
         *
         * Objects with euid 0 cannot load or clone other objects.
         */

        struct svalue *ret;
        struct svalue *argp;

        argp = sp;
        if (argp->type == T_NUMBER)
        {
            /* Clear the euid of this_object */
          
            if (argp->u.number != 0)
                goto bad_arg_1;
            current_object->eff_user = 0;
            free_svalue(argp);
            argp->type = T_NUMBER;
            argp->u.number = 1;
            break;
        }
        
        if (argp->type != T_STRING)
            goto bad_arg_1;
        
        /* Call the master to clear this use of seteuid() */
        
        assign_eval_cost();
        inter_sp = _push_volatile_string(argp->u.string,
            _push_valid_ob(current_object, sp) );
        inter_pc = pc;
        ret = apply_master_ob(STR_VALID_SETEUID, 2);
        if (!ret || ret->type != T_NUMBER || ret->u.number != 1)
        {
            if (out_of_memory)
            {
                error("Out of memory\n");
                /* NOTREACHED */
                break;
            }
            free_svalue(argp);
            argp->type = T_NUMBER;
            argp->u.number = 0;
            break;
        }
        current_object->eff_user = add_name(argp->u.string);
        free_svalue(argp);
        argp->type = T_NUMBER;
        argp->u.number = 1;
        break;
    }
#endif /* F_SETEUID */

#if defined(F_GETUID) || defined(F_CREATOR)
#ifdef F_GETUID
    CASE(F_GETUID);                 /* --- getuid              --- */
#else
    CASE(F_CREATOR);                /* --- creator             --- */
#endif
    {
        /* EFUN getuid()
         *
         *   string getuid(object ob)
         *   string creator(object ob)
         *
         * User-ids are not used in compat mode, instead the uid is
         * then called 'creator'.
         * Get user-id of the object, i.e. the name of the wizard or
         * domain that is responsible for the object. This name is also
         * the name used in the wizlist. If no arg is given, use
         * this_object() as default.
         */

        struct object *ob;
        char *name;

        TYPE_TEST1(sp, T_OBJECT)
        ob = sp->u.ob;
        decr_object_ref(ob, "getuid");
        if ( NULL != (name = ob->user->name) )
        {
            sp->type = T_STRING;
            sp->x.string_type = STRING_SHARED;
            increment_string_ref(sp->u.string = name);
        }
        else
        {
            put_number(0);
        }
        break;
    }
#endif

    /* --- Efuns: Only used in Compat mode --- */
        
#ifdef F_TRANSFER
    CASE(F_TRANSFER);
    {
        int i;
        struct object *dest;

        assign_eval_cost();
        TYPE_TEST1(sp-1, T_OBJECT)
        inter_sp = sp;
        inter_pc = pc;
        if (sp->type == T_OBJECT) {
            dest = sp->u.ob;
        } else if (sp->type == T_STRING) {
            dest = get_object(sp->u.string);
            if (dest == 0)
                ERROR("Object not found.\n")
            free_string_svalue(sp);
            put_object(dest);
        } else goto bad_arg_2;
        i = transfer_object(sp-1);
        if (i) {
            /* the objects on the stack might have been changed into 0 if
             * destructed, thus free_object_svalue is not applicable.
             */
            free_svalue(sp);
            free_svalue(sp-1);
        }
        sp--;
        put_number(i);
        break;
    }
#endif

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
        
#ifdef F_RUSAGE
    CASE(F_RUSAGE);                 /* --- rusage              --- */
    {
        /* EFUN rusage()
         *
         *   int *rusage(void)
         *
         * Return an array with current system resource usage statistics,
         * as returned by the getrusage(2) of Unix.
         * namely: utime, stime, maxrss, rus.ru_ixrss, rus.ru_idrss,
         * rus.ru_isrss, rus.ru_minflt, rus.ru_majflt, rus.ru_nswap,
         * rus.ru_inblock, rus.ru_oublock, rus.ru_msgsnd,
         * rus.ru_msgrcv, rus.ru_nsignals, rus.ru_nvcsw,
         * rus.ru_nivcsw
         * TODO: The indices should be in an include file.
         */

        struct rusage rus;
        struct vector *res;
        struct svalue *v;
#ifndef GETRUSAGE_RESTRICTED
        int maxrss;
#endif

        if (getrusage(RUSAGE_SELF, &rus) < 0) {
            push_number(0);
            break;
        }
        res = allocate_array(16);
        v = res->item;
        v[ 0].u.number = RUSAGE_TIME(rus.ru_utime);
        v[ 1].u.number = RUSAGE_TIME(rus.ru_stime);
#ifndef GETRUSAGE_RESTRICTED
        maxrss = rus.ru_maxrss;
#ifdef sun
        maxrss *= getpagesize() / 1024;
#endif
        v[ 2].u.number = maxrss;
        v[ 3].u.number = rus.ru_ixrss;
        v[ 4].u.number = rus.ru_idrss;
        v[ 5].u.number = rus.ru_isrss;
        v[ 6].u.number = rus.ru_minflt;
        v[ 7].u.number = rus.ru_majflt;
        v[ 8].u.number = rus.ru_nswap;
        v[ 9].u.number = rus.ru_inblock;
        v[10].u.number = rus.ru_oublock;
        v[11].u.number = rus.ru_msgsnd;
        v[12].u.number = rus.ru_msgrcv;
        v[13].u.number = rus.ru_nsignals;
        v[14].u.number = rus.ru_nvcsw;
        v[15].u.number = rus.ru_nivcsw;
#endif /* GETRUSAGE_RESTRICTED */
        push_referenced_vector(res);
        break;
    }
#endif

    /* --- Optional Efuns: Alists --- */
        
#ifdef F_ASSOC
    CASE(F_ASSOC);                  /* --- assoc <nargs>       --- */
    {
        /* EFUN assoc()
         *
         *     int   assoc (mixed key, mixed *keys)
         *     mixed assoc (mixed key, mixed *alist [, mixed fail] )
         *     mixed assoc (mixed key, mixed *keys, mixed *data [, mixed fail])
         *
         * Search for <key> in the <alist> resp. in the <keys>.
         *
         * When the key list of an alist contains destructed objects
         * it is better not to free them till the next reordering by
         * order_alist to retain the alist property.
         *
         * TODO: Make the alist-efuns xefuns.
         */
        struct svalue *args;
        struct vector *keys,*data;
        struct svalue *fail_val;
        int ix;

        GET_NUM_ARG
        args = sp -num_arg +1;
        TYPE_TEST2(args+1, T_POINTER)

        /* Analyse the arguments */
        if ( !VEC_SIZE(args[1].u.vec)
         ||  args[1].u.vec->item[0].type != T_POINTER )
        {
            keys = args[1].u.vec;
            if (num_arg == 2)
            {
                data = NULL;
            }
            else
            {
                if (args[2].type != T_POINTER
                 || VEC_SIZE(args[2].u.vec) != VEC_SIZE(keys))
                {
                    goto bad_arg_3;
                }
                data = args[2].u.vec;
            }
            if (num_arg == 4)
            {
                fail_val = &args[3];
            }
            else
            {
                fail_val = &const0;
            }
        }
        else
        {
            keys = args[1].u.vec->item[0].u.vec;
            if (VEC_SIZE(args[1].u.vec) > 1)
            {
                if (args[1].u.vec->item[1].type != T_POINTER
                 || VEC_SIZE(args[1].u.vec->item[1].u.vec) != VEC_SIZE(keys))
                {
                    goto bad_arg_2;
                }
                data = args[1].u.vec->item[1].u.vec;
            }
            else
            {
                data = NULL;
            }

            if (num_arg == 3) fail_val = &args[2];
            else if (num_arg == 2) fail_val = &const0;
            else
            {
                ERROR ("too many args to efun assoc\n")
                return;
            }
        }
        
        /* Call assoc() and push the result */
        ix = assoc(&args[0],keys);
        if (data == NULL)
        {
            pop_n_elems(num_arg);
            push_number(ix);
        }
        else
        {
            assign_svalue(args
                         , ix == -1
                           ? fail_val
                           : (data->item[ix].type == T_OBJECT
                              && data->item[ix].u.ob->flags & O_DESTRUCTED
                             ? &const0
                             : &data->item[ix])
                         );
            pop_n_elems(num_arg-1);
        }
        break;
    }
#endif /* F_ASSOC */

#ifdef F_INSERT_ALIST
    CASE(F_INSERT_ALIST)           /* --- insert_alist <nargs> --- */
    {
        /* EFUN insert_alist()
         *
         *   mixed* insert_alist (mixed key, mixed data..., mixed * alist)
         *   int    insert_alist (mixed key, mixed * keys)
         *
         * 1. Form: Alist Insertion
         *
         *   The <key> and all following <data> values are inserted
         *   into the <alist>. If an entry for <key> already exists
         *   in the list, just the data values are replaced. The number
         *   of <data> values must match the number of data arrays
         *   in the alist, naturally.
         *
         *   Result is the updated <alist>.
         *
         * 2. Form: Key Insertion
         *
         *   Insert the <key> into the (ordered) array of <keys>, so that
         *   subsequent assoc()s can perform quick lookups. Result is the
         *   index at which <key> was inserted (or already found).
         *
         *   CAVEAT: when working with string keys, the index might no longer
         *     be valid after the next call to insert_alist().
         */
        /* When the key list of an alist contains destructed objects
           it is better not to free them till the next reordering by
           order_alist to retain the alist property.
         */
        int i;
        struct vector *list;
        int listsize;
        size_t keynum;
        struct svalue *key,*key_data,*ret;
        static struct { INIT_VEC_TYPE; } tempvec =
          { VEC_INIT(1, 1, T_NUMBER) };
          /* Mock-alist for the key-insertion form */

        GET_NUM_ARG
        if (sp->type != T_POINTER)
            bad_arg_pc(num_arg,F_INSERT_ALIST-F_OFFSET, sp, pc);

        /* Make up an alist if only a key-insertion is required */
        if ( !(listsize = VEC_SIZE(sp->u.vec))
         ||  sp->u.vec->item[0].type != T_POINTER )
        {
            list = &tempvec.v;
            *list->item = *sp;
            listsize = 1;
        }
        else
            list = sp->u.vec;
        
        /* Check the validity of the alist */
        keynum = VEC_SIZE(list->item[0].u.vec);
        for (i = 1; i < listsize; i++)
        {
            if (list->item[i].type != T_POINTER
             || VEC_SIZE(list->item[i].u.vec) != keynum)
            {
                bad_arg_pc(num_arg,F_INSERT_ALIST-F_OFFSET, sp, pc);
            }
        }
        
        /* Get and test the data to insert */
        if (num_arg == 2)
        {
            if (sp[-1].type != T_POINTER)
            {
                key_data = NULL;
                key = sp-1;
            }
            else
            {
                if (VEC_SIZE(sp[-1].u.vec) != (size_t)listsize)
                    goto bad_arg_1;
                key_data = key = sp[-1].u.vec->item;
            }
        }
        else
        {
            if (num_arg - 1 != listsize)
                goto bad_arg_1;
            key_data = key = sp-num_arg+1;
        }
        
        /* Do the insertion */
        inter_sp = sp; /* array might get too big */
        ret = insert_alist(key,key_data,list);
        pop_n_elems(num_arg);
        sp++;
        *sp = *ret;
        break;
    }
#endif /* F_INSERT_ALIST */

#ifdef F_INTERSECT_ALIST
    CASE(F_INTERSECT_ALIST);        /* --- intersect_alist     --- */
    {
        /* EFUN intersect_alist()
         *
         *   mixed * intersect_alist (mixed * list1, mixed * list2)
         *
         * Does a fast set intersection on alist key vectors (NOT on full
         * alists!).  The operator '&' does set intersection on arrays in
         * general.
         */

        struct vector *tmp;

        TYPE_TEST1(sp-1, T_POINTER)
        TYPE_TEST2(sp,   T_POINTER)
        tmp = intersect_alist( (sp-1)->u.vec, sp->u.vec );
        pop_stack();
        free_vector(sp->u.vec);
        sp->u.vec = tmp;
        break;
    }
#endif /* F_INTERSECT_ALIST */

#ifdef F_ORDER_ALIST
    CASE(F_ORDER_ALIST);            /* --- order_alist <nargs> --- */
    {
        /* EFUN order_alist()
         *
         *   mixed *order_alist(mixed *keys, mixed *|void data, ...)
         *
         * Creates an alist.
         *
         * Either takes an array containing keys, and others containing
         * the associated data, where all arrays are to be of the same
         * length, or takes a single array that contains as first member
         * the array of keys and has an arbitrary number of other members
         * containing data, each of wich has to be of the same length as
         * the key array. Returns an array holding the sorted key array
         * and the data arrays; the same permutation that is applied to
         * the key array is applied to all data arrays.
         */

        int i;
        struct svalue *args;
        struct vector *list;
        int listsize;
        Bool reuse;
        size_t keynum;

        GET_NUM_ARG
        args = sp-num_arg+1;

        /* Get the key array to order */
        TYPE_TEST1(args, T_POINTER)
        if (num_arg == 1
          && ((list = args->u.vec), (listsize = VEC_SIZE(list)))
          && list->item[0].type == T_POINTER)
        {
            args     = list->item;
            reuse = (list->ref == 1);
        }
        else
        {
            listsize = num_arg;
            reuse = MY_TRUE;
        }
        keynum = VEC_SIZE(args[0].u.vec);

        /* Get the data arrays to order */
        for (i = 0; i < listsize; i++)
        {
            if (args[i].type != T_POINTER
             || VEC_SIZE(args[i].u.vec) != keynum)
            {
                ERRORF(("bad data array %d in call to order_alist\n",i))
            }
        }

        /* Create the alist */
        list = order_alist(args, listsize, reuse);
        pop_n_elems(num_arg);
        sp++;
        sp->type = T_POINTER;
        sp->u.vec = list;
        break;
    }
#endif /* F_ORDER_ALIST */

    /* --- Optional Efuns: Miscellaneous --- */
        
#ifdef F_PROCESS_STRING
    CASE(F_PROCESS_STRING);         /* --- process_string      --- */
    {
        /* EFUN process_string()
         *
         * string process_string(string str)
         *
         * Searches string str for occurences of a "value by function
         * call", which is @@ followed by an implicit function call. See
         * "value_by_function_call" in the principles section.
         *
         * The value should contain a string like this:
         * @@function[:filename][|arg|arg]@@
         *
         * function must return a string or else the string which should be
         * processed will be returned unchanged.
         *
         * Note that process_string() does not recurse over returned
         * replacement values. If a function returns another function
         * description, that description will not be replaced.
         *
         * Note that both filename and args are optional.
         */
      
        char *str;

        assign_eval_cost();
        TYPE_TEST1(sp, T_STRING)
        inter_sp = sp;
        inter_pc = pc;
        str = process_string(sp->u.string);
        if (str != sp->u.string)
        {
            free_string_svalue(sp);
            put_malloced_string(str, sp);
        }
        break;
    }
#endif /* F_PROCESS_STRING */
    
#ifdef F_SET_LIGHT
    CASE(F_SET_LIGHT);              /* --- set_light           --- */
    {
        /* EFUN set_light()
         *
         * int set_light(int n)
         *
         * An object is by default dark. It can be set to not dark by
         * calling set_light(1). The environment will then also get this
         * light. The returned value is the total number of lights in
         * this room. So if you call set_light(0) it will return the
         * light level of the current object.
         *
         * Note that the value of the argument is added to the light of
         * the current object.
         */
      
        struct object *o1;

        TYPE_TEST1(sp, T_NUMBER)
        add_light(current_object, sp->u.number);
        o1 = current_object;
        while (o1->super)
            o1 = o1->super;
        sp->u.number = o1->total_light;
        break;
    }
#endif /* F_SET_LIGHT */

    /* --- XEfun and XCodes --- */

    CASE(F_ESCAPE);                  /* --- escape <instr> ... --- */
      {

        /* A prefixed instruction. */
        
#define XCASE(x) CASE((x)-0x100)
#undef GET_NUM_ARG
#define GET_NUM_ARG num_arg = EXTRACT_UCHAR(pc); pc++;

#undef TYPE_TEST1
#define TYPE_TEST1(arg, t) if ( (arg)->type != t ) goto xbad_arg_1;

        int code;  /* the actual instruction code */

        code = LOAD_CODE(pc);

#ifdef TRACE_CODE
        previous_instruction[last] = code + 0x100;
#endif

#ifdef OPCPROF
        opcount[code+0x100]++;
#endif

        switch(code)
        {

        default:
            fatal("Unknown stackmachine escape code: %d (%d)\n"
                 , code, code+0x100);
        xbad_arg_1: instruction = code + 0x100; goto bad_arg_1;
        xbad_arg_2: instruction = code + 0x100; goto bad_arg_2;
        xbad_arg_3: instruction = code + 0x100; goto bad_arg_3;
                    
        /* --- Machine Instructions --- */
          
        XCASE(F_END_CATCH);         /* --- esc end_catch       --- */
            /* For a catch(...guarded code...) statement, the compiler
             * generates a F_END_CATCH as last instruction of the
             * guarded code.
             *
             * Executed when no error occured, it cleans up the
             * error recovery information pushed by the F_CATCH
             * and leaves a 0 on the stack.
             *
             * dump_trace() checks for this bytecode, but accepts a normal
             * instruction as well as an escaped instruction.
             */

            pop_stack();
            pop_control_stack();
            pc = inter_pc;
            fp = inter_fp;
            pop_error_context();
            push_number(0);
            eval_cost -= CATCH_RESERVED_COST;
            assigned_eval_cost -= CATCH_RESERVED_COST;
            break;
            
                      /* --- esc breakn_continue <num> <offset> ---*/
        XCASE(F_BREAKN_CONTINUE);
            /* Implement the 'continue;' statement from within
             * a nested surrounding structure.
             * 
             * Pop <num>+1 (uint8) break-levels from the break stack
             * and jump by (16-Bit) short <offset> bytes, counted from the
             * first by of <offset>
             */
          
            break_sp +=
              LOAD_UINT8(pc) * (sizeof(struct svalue)/sizeof(*break_sp));
            /* FALLTHROUGH */
            
                             /* --- esc break_continue <offset> ---*/
        XCASE(F_BREAK_CONTINUE);
        {
            /* Implement the 'continue;' statement for the immediate
             * surrounding structure.
             * 
             * Pop one break-level from the break stack and jump
             * by (16-Bit) unsigned short <offset> bytes, counted from the
             * first by of <offset>
             *
             * Pitfall: the offset is added to the current pc in 16-Bit
             * unsigned arithmetic, allowing to jump backwards using big
             * enough values.
             *
             * TODO: Make that a proper signed short.
             */
          
            /* TODO: uint16 */ unsigned short offset;

            break_sp += sizeof(struct svalue)/sizeof(*break_sp);
            GET_SHORT(offset, pc);
            offset += pc - current_prog->program;
            pc = current_prog->program + offset;
            break;
        }

                      /* --- esc push_protected_indexed_lvalue --- */
        XCASE(F_PUSH_PROTECTED_INDEXED_LVALUE);
            /* Op. (vector  v=sp[-1], int   i=sp[0])
             * Op. (mapping v=sp[-1], mixed i=sp[0])
             *
             * Compute the lvalue &(v[i]), store it in a struct
             * protected_lvalue, and push the protector as PROTECTED_LVALUE
             * into the stack.
             */
        
            sp = push_protected_indexed_lvalue(sp, pc);
            break;

                     /* --- esc push_protected_rindexed_lvalue --- */
        XCASE(F_PUSH_PROTECTED_RINDEXED_LVALUE);
            /* Op. (vector v=sp[-1], int i=sp[0])
             *
             * Compute the lvalue &(v[<i]), store it in a struct
             * protected_lvalue, and push the protector as PROTECTED_LVALUE
             * into the stack.
             */

            sp = push_protected_rindexed_lvalue(sp, pc);
            break;

                  /* --- esc push_protected_indexed_map_lvalue --- */
        XCASE(F_PUSH_PROTECTED_INDEXED_MAP_LVALUE);
            /* Op. (mapping m=sp[-2], mixed i=sp[-1], int   j=sp[0])
             *
             * Compute the lvalue &(m[i:j]), store it in a struct
             * protected_lvalue, and push the protector as PROTECTED_LVALUE
             * into the stack.
             */

            push_protected_indexed_map_lvalue(sp, pc);
            break;

                             /* --- esc protected_index_lvalue --- */
        XCASE(F_PROTECTED_INDEX_LVALUE);
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

            sp = protected_index_lvalue(sp, pc);
            break;

                            /* --- esc protected_rindex_lvalue --- */
        XCASE(F_PROTECTED_RINDEX_LVALUE);
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

                              /* --- esc protected_range_lvalue --- */
        XCASE(F_PROTECTED_RANGE_LVALUE);
            /* X-Op (string|vector &v=sp[0], int i2=sp[-1], i1=sp[-2])
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
            sp = protected_range_lvalue(0x000, sp);
            break;

                           /* --- esc protected_nr_range_lvalue --- */
        XCASE(F_PROTECTED_NR_RANGE_LVALUE);
            /* X-Op (string|vector &v=sp[0], int i2=sp[-1], i1=sp[-2])
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
            sp = protected_range_lvalue(0x001, sp);
            break;

                           /* --- esc protected_rn_range_lvalue --- */
        XCASE(F_PROTECTED_RN_RANGE_LVALUE);
            /* X-Op (string|vector &v=sp[0], int i2=sp[-1], i1=sp[-2])
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
            sp = protected_range_lvalue(0x100, sp);
            break;

                           /* --- esc protected_rr_range_lvalue --- */
        XCASE(F_PROTECTED_RR_RANGE_LVALUE);
            /* X-Op (string|vector &v=sp[0], int i2=sp[-1], i1=sp[-2])
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
            sp = protected_range_lvalue(0x101, sp);
            break;

                            /* --- esc protected_extract_lvalue --- */
        XCASE(F_PROTECTED_EXTRACT_LVALUE);
          {
            /* X-Op (string|vector &v=sp[0], int ix=sp[-1])
             *
             * Compute the range &(v[ix..<1]) of lvalue <v>, wrap it into a
             * protector, and push the reference to the protector onto the
             * stack. If <ix> is negative, compute the range &(v[<-ix..<1]).
             * 
             * If <v> is a protected lvalue itself, its protecting svalue will
             * be used in the result protector.
             * 
             * If <v> is a string-lvalue, it is made a malloced string if
             * necessary.
             *
             * Used for sscanf() and parse_command() only.
             */

            sp[1] = sp[0]; /* Move the vector up */
            put_number(1); /* Fake the second index */
            sp++;

            if (sp[-2].u.number < 0 && sp[-2].type == T_NUMBER)
            {
                sp[-2].u.number = -sp[-2].u.number;
                inter_pc = pc;
                sp = protected_range_lvalue(0x101, sp);
            }
            else
            {
                inter_pc = pc;
                sp = protected_range_lvalue(0x001, sp);
            }
            break;
          }

        XCASE(F_UNDEF);             /* --- esc undef           --- */
          {
            /* Catch-all instructions for declared but not implemented
             * (defined) functions. Usually used by the compiler to
             * handle prototypes (in that case it is the first and only
             * instruction of the generated stub), it is also inserted
             * into closures when the object the closure is bound to
             * is destructed.
             */
            
            char *name;

            /* pc has already been incremented */
            if (pc > current_prog->program && pc <= PROGRAM_END(*current_prog))
            {
                /* Copy the function name pointer into name.
                 * TODO: This code relies on the layout of function headers.
                 */
                memcpy(&name, pc - 5 - sizeof name, sizeof name);
                  /* '5': two opcode bytes plus the function header */
            }
            else
            {
                name = "Object the closure was bound to has been destructed";
                /* TODO: WHich object? */
            }
            ERRORF(("Undefined function: %s\n", name))
          }

        /* --- XEfuns: Miscellaneous --- */

        XCASE(F_ABS);               /* --- esc abs             --- */
        {
            /* XEFUN abs()
             *
             *  int   abs (int arg)
             *  float abs (float arg)
             *
             * Returns the absolute value of the argument <arg>.
             */

            switch(sp->type)
            {
            default:
                goto xbad_arg_1;

            case T_NUMBER:
                if (sp->u.number < 0)
                    sp->u.number = - sp->u.number;
                break;

            case T_FLOAT:
              {
                STORE_DOUBLE_USED
                double x;

                x = READ_DOUBLE(sp);
                if (x < 0.0)
                    STORE_DOUBLE(sp, -(x));
                break;
              }
            }
            break;
        }

        XCASE(F_SIN);               /* --- esc sin             --- */
          {
            /* XEFUN sin()
             *
             *  float sin(float)
             *
             * Returns the sinus of the argument.
             */

            STORE_DOUBLE_USED
            double d;

            if (sp->type != T_FLOAT) goto xbad_arg_1;
            d = sin(READ_DOUBLE(sp));
            STORE_DOUBLE(sp, d);
            break;
          }

        XCASE(F_ASIN);              /* --- esc asin            --- */
          {
            /* XEFUN asin()
             *
             *  float asin(float)
             *
             * Returns the inverse sinus of the argument.
             */

            STORE_DOUBLE_USED
            double d;

            if (sp->type != T_FLOAT || (d = READ_DOUBLE(sp)) < -1. || d > 1. )
                goto xbad_arg_1;
            d = asin(d);
            STORE_DOUBLE(sp, d);
            break;
          }

        XCASE(F_COS);               /* --- esc cos             --- */
          {
            /* XEFUN cos()
             *
             *  float cos(float)
             *
             * Returns the cosinus of the argument.
             */

            STORE_DOUBLE_USED
            double d;

            if (sp->type != T_FLOAT) goto xbad_arg_1;
            d = cos(READ_DOUBLE(sp));
            STORE_DOUBLE(sp, d);
            break;
          }

        XCASE(F_ACOS);              /* --- esc acos            --- */
          {
            /* XEFUN acos()
             *
             *  float acos(float)
             *
             * Returns the inverse cosinus of the argument.
             */

            STORE_DOUBLE_USED
            double d;

            if (sp->type != T_FLOAT || (d = READ_DOUBLE(sp)) < -1. || d > 1. )
                goto xbad_arg_1;
            d = acos(d);
            STORE_DOUBLE(sp, d);
            break;
          }

        XCASE(F_TAN);               /* --- esc tan             --- */
          {
            /* XEFUN tan()
             *
             *  float tan(float)
             *
             * Returns the tangens of the argument.
             */

            STORE_DOUBLE_USED
            double d;

            if (sp->type != T_FLOAT) goto xbad_arg_1;
            d = tan(READ_DOUBLE(sp));
            STORE_DOUBLE(sp, d);
            break;
          }

        XCASE(F_ATAN);              /* --- esc atan            --- */
          {
            /* XEFUN atan()
             *
             *   float atan(float)
             *
             * Returns the inverse tangens of the argument.
             * TODO: atan2() would be nice to have.
             */

            STORE_DOUBLE_USED
            double d;

            if (sp->type != T_FLOAT) goto xbad_arg_1;
            d = atan(READ_DOUBLE(sp));
            STORE_DOUBLE(sp, d);
            break;
          }

        XCASE(F_LOG);               /* --- esc log             --- */
          {
            /* XEFUN log()
             *
             *   float log(float)
             *
             * Returns the natural logarithmus of the argument.
             */

            STORE_DOUBLE_USED
            double d;

            if (sp->type != T_FLOAT || (d = READ_DOUBLE(sp)) <= 0.)
                goto xbad_arg_1;
            d = log(d);
            STORE_DOUBLE(sp, d);
            break;
          }

        XCASE(F_EXP);               /* --- esc exp             --- */
          {
            /* XEFUN exp()
             *
             *   float exp(float)
             *
             * Returns the e to the power of the argument.
             */

            STORE_DOUBLE_USED
            double d;

            if (sp->type != T_FLOAT) goto xbad_arg_1;
            d = exp(READ_DOUBLE(sp));
            STORE_DOUBLE(sp, d);
            break;
          }

        XCASE(F_SQRT);              /* --- esc sqrt            --- */
          {
            /* XEFUN sqrt()
             *
             *   float sqrt(float)
             *
             * Returns the square root of the argument.
             */

            STORE_DOUBLE_USED
            double d;

            if (sp->type != T_FLOAT || (d = READ_DOUBLE(sp)) < 0.)
                goto xbad_arg_1;
            d = sqrt(d);
            STORE_DOUBLE(sp, d);
            break;
          }

        XCASE(F_GET_TYPE_INFO);     /* --- esc get_type_info   --- */
          {
            /* XEFUN get_type_info()
             *
             *   mixed get_type_info(mixed arg, int flag)
             *
             * Returns info about the type of arg, as controlled by the flag.
             *
             * If the optional argument flag is not a number, an array is
             * returned, whose first element is an integer denoting the data
             * type, as defined in <lpctypes.h>. The second entry can contain
             * additional information about arg.
             * If flag is the number 0, only the first element of that array
             * (i.e. the data type) is returned (as int). If flag is 1, the
             * second element is returned.
             * If <arg> is a closure, the <flag> setting 2 lets the efun
             * return the object the closure is bound to.
             * For every other <flag> setting, -1 is returned.
             *
             * For mappings, the second entry is the width (i.e. number of
             * data items per key).
             *
             * For closures, symbols and quoted arrays a the number of quotes
             * is returned as additional info about arg.
             *
             * For all other data types, -1 is returned as additional info.
             * TODO: The flags should be defined in an include file.
             */

            mp_int i, j;

            i = sp[-1].type;

            /* Determine the second return value */
            switch(i)
            {
            default:
                j = -1;
                break;
            case T_MAPPING:
                j = sp[-1].u.map->num_values;
                break;
            case T_CLOSURE:
                if (sp->u.number == 2 && sp->type == T_NUMBER)
                {
                    struct object *ob;

                    sp--;
                    switch(sp->x.closure_type)
                    {
                    default:
                        ob = NULL;
                        break;
                    case CLOSURE_LFUN:
                    case CLOSURE_IDENTIFIER:
                        ob = sp->u.lambda->ob;
                        break;
                    case CLOSURE_ALIEN_LFUN:
                        ob = sp->u.lambda->function.alien.ob;
                        break;
                    }
                    free_closure(sp);
                    if (!ob || ob->flags & O_DESTRUCTED)
                        put_number(0);
                    else
                        put_object(ob);
                    goto again;
                    /* NOTREACHED */
                }
            case T_SYMBOL:
            case T_QUOTED_ARRAY:
                j = sp[-1].x.generic;
                break;
            }

            /* Depending on flag, return the proper value */
            if (sp->type == T_NUMBER)
            {
                free_svalue(--sp);
                if (sp[1].u.number != 1)
                    if (sp[1].u.number)
                        j = -1;
                    else
                        j = i;
                put_number(j);
            }
            else
            {
                struct vector *v;

                inter_sp = sp;
                inter_pc = pc;
                v = allocate_array(2);
                v->item[0].u.number = i;
                v->item[1].u.number = j;
                free_svalue(sp);
                free_svalue(--sp);
                put_referenced_vector(v);
            }
            
            break;
          }

        XCASE(F_RAISE_ERROR);       /* --- esc raise_error     --- */
          {
            /* XEFUN raise_error()
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

            if (sp->type != T_STRING)
                goto xbad_arg_1;
            ERRORF(("%s", sp->u.string));
          }
          
        XCASE(F_REFERENCEP);        /* --- esc referencep      --- */
          {
            /* XEFUN referencep()
             *
             *   int referencep(mixed arg)
             *
             * Returns true if arg was passed by reference to the current
             * function, instead of the usual call-by-value.
             */

            int i;

            if (sp->type != T_LVALUE)
                goto xbad_arg_1;
            i = sp->u.lvalue->type == T_LVALUE;
            free_svalue(sp);
            put_number(i);
            break;
          }

        XCASE(F_TYPEOF);            /* --- esc typeof          --- */
          {
            /* XEFUN typeof()
             *
             *   int typeof(mixed)
             *
             * Returns a code for the type of the argument, as defined in
             * <sys/lpctypes.h>
             */

            mp_int i = sp->type;
            free_svalue(sp);
            put_number(i);
            break;
          }

        XCASE(F_TO_INT);            /* --- esc to_int          --- */
          {
            /* EFUN to_int()
             *
             *   int to_int(string)
             *   int to_int(float)
             *   int to_int(int)
             *   int to_int(closure)
             *
             * Floats are truncated to integer values, strings with leadings
             * digits are converted to integers up to the first non-digit.
             * variable-closures are converted into their function index.
             * Integers are just returned.
             */

            int n;

            switch(sp->type)
            {
            default:
                goto xbad_arg_1;
       
            case T_FLOAT:
                n = (long)READ_DOUBLE(sp);
                break;
      
            case T_STRING:
                n = atol(sp->u.string);
                /* TODO: make this strtol() */
                free_string_svalue(sp);
                break;
        
            case T_CLOSURE:
                if (sp->x.closure_type != CLOSURE_IDENTIFIER)
                    goto xbad_arg_1;
                n = sp->u.lambda->function.index;
                free_closure(sp);
                break;
        
            case T_NUMBER:
                n = sp->u.number;
                break;
            }
            put_number(n);
            break;
          }

        XCASE(F_TO_FLOAT);          /* --- esc to_float           --- */
          {
            /* EFUN to_float()
             *
             *   float to_float(int)
             *   float to_float(string)
             *   float to_float(float)
             *
             * Ints are expanded to floats, strings are converted up to the
             * first character that doesnt belong into a float.
             * Floats are just returned.
             */

            STORE_DOUBLE_USED
            double d;

            d = 0.0;
            switch(sp->type)
            {
            default:
                goto xbad_arg_1;
       
            case T_NUMBER:
                d = (double)sp->u.number;
                break;
    
            case T_FLOAT:
                NOOP;
                break;
       
            case T_STRING:
                d = atof(sp->u.string);
                /* TODO: make this strtof() or so */
                free_string_svalue(sp);
                break;
            }
            
            if (sp->type != T_FLOAT)
            {
                sp->type = T_FLOAT;
                STORE_DOUBLE(sp, d);
            }
            break;
          }

        XCASE(F_TO_STRING);         /* --- esc to_string       --- */
          {
            /* XEFUN to_string()
             *
             *   string to_string(mixed)
             *
             * The argument is converted to a string. Works with int, float,
             * object, arrays (to convert an array of int back into a string),
             * symbols, strings, and closures.
             *
             * Converts variable/lfun closure to the appropriate names.
             */
            
            char buf[1024], *s;

            s = NULL;
            buf[sizeof(buf)-1] = '\0';
            switch(sp->type)
            {
            default:
                goto xbad_arg_1;

            case T_NUMBER:
                sprintf(buf,"%ld", sp->u.number);
                if (buf[sizeof(buf)-1] != '\0')
                    FATAL("Buffer overflow in F_TO_STRING: "
                          "int number too big.\n")
                s = string_copy(buf);
                break;

            case T_FLOAT:
                sprintf(buf,"%g", READ_DOUBLE(sp));
                if (buf[sizeof(buf)-1] != '\0')
                    FATAL("Buffer overflow in F_TO_STRING: "
                          "int number too big.\n")
                s = string_copy(buf);
                break;

            case T_OBJECT:
#ifndef COMPAT_MODE
                s = add_slash(sp->u.ob->name);
#else
                s = string_copy(sp->u.ob->name);
#endif
                if (!s)
                    ERROR("Out of memory\n")
                free_object_svalue(sp);
                break;

            case T_POINTER:
              {
                /* Arrays of ints are considered exploded strings and
                 * converted back accordingly, ie. up to the first 0
                 * or the first non-int.
                 */

                long size;
                struct svalue *svp;
                char c, *d;

                size = VEC_SIZE(sp->u.vec) + 1;
                svp = sp->u.vec->item;
                d = s = xalloc(size);
                for (;;)
                {
                    if (!--size)
                    {
                        *d++ = '\0';
                        break;
                    }
                    if (svp->type != T_NUMBER || !(c = svp->u.number) )
                    {
                        *d++ = '\0';
                        d = string_copy(s);
                        xfree(s);
                        s = d;
                        break;
                    }
                    *d++ = c;
                    svp++;
                }
                free_vector(sp->u.vec);
                break;
              }

            case T_CLOSURE:
              {
                /* Convert the various types of closures into a string */
                
                struct lambda *l = sp->u.lambda;
                struct object *ob;
                int ix;

                switch(sp->x.closure_type)
                {
          
                case CLOSURE_IDENTIFIER: /* Variable Closure */
                  {
                    /* We need the program resident */
                    if (O_PROG_SWAPPED(l->ob))
                    {
                        l->ob->time_of_ref = current_time;
                        if (load_ob_from_swap(l->ob) < 0)
                            ERROR("Out of memory.\n");
                    }
                    
                    sp->type = T_STRING;
                    
                    if (((short)l->function.index) >= 0)
                    {
                        /* Get the variable name */
                        sp->x.string_type = STRING_SHARED;
                        increment_string_ref(
                          sp->u.string =
                            l->ob->prog->variable_names[l->function.index].name
                        );
                    }
                    else
                    {
                        /* Variable vanished in a replace_program() */
                        sp->x.string_type = STRING_VOLATILE;
                        sp->u.string = "dangling var closure";
                    }
                    break;
                  }
   
                case CLOSURE_LFUN: /* Lfun closure */
                case CLOSURE_ALIEN_LFUN:
                  {
                    struct program *prog;
                    fun_hdr_p fun;
                    /* TODO: funflags */ uint32 flags;
                    char *function_name;
                    struct inherit *inheritp;

                    if (sp->x.closure_type == CLOSURE_LFUN)
                    {
                        ob = l->ob;
                        ix = l->function.index;
                    }
                    else
                    {
                        ob = l->function.alien.ob;
                        ix = l->function.alien.index;
                        /* TODO: ix: After a replace_program, can this index
                         * TODO:: be negative?
                         */
                    }

                    /* Get the program resident */
                    if (O_PROG_SWAPPED(ob)) {
                        ob->time_of_ref = current_time;
                        if (load_ob_from_swap(ob) < 0)
                            ERROR("Out of memory\n");
                    }

                    /* Find the true definition of the function */
                    prog = ob->prog;
                    flags = prog->functions[ix];
                    while (flags & NAME_INHERITED)
                    {
                        inheritp = &prog->inherit[flags & INHERIT_MASK];
                        ix -= inheritp->function_index_offset;
                        prog = inheritp->prog;
                        flags = prog->functions[ix];
                    }
                    
                    /* Copy the function name pointer (a shared string) */
                    fun = prog->program + (flags & FUNSTART_MASK);
                    memcpy((char *)&function_name, &FUNCTION_NAME(fun)
                          , sizeof function_name
                    );
                    sp->type = T_STRING;
                    sp->x.string_type = STRING_SHARED;
                    increment_string_ref(sp->u.string = function_name);
                    break;
                  }
        
                default:
                    goto xbad_arg_1;
                }
                break;
              }

            case T_SYMBOL:
              {
                /* Easy: the symbol value is a string */
                sp->type = T_STRING;
                sp->x.string_type = STRING_SHARED;
                break;
              }

            case T_STRING:
                break;
            }

            if (sp->type != T_STRING)
                put_malloced_string(s, sp);
            break;
          }

        XCASE(F_TO_ARRAY);          /* --- esc to_array        --- */
          {
            /* XEFUN to_array()
             *
             *   mixed *to_array(string)
             *   mixed *to_array(symbol)
             *   mixed *to_array(quotedarray)
             *   mixed *to_array(mixed *)
             *
             * Strings and symbols are converted to an int array that
             * consists of the args characters, with 0 == '\0' as last
             * character stored.
             * Quoted arrays are ``dequoted'', and arrays are left as they
             * are.
             */

            struct vector *v;
            char *s;
            struct svalue *svp;

            if (sp->type == T_STRING || sp->type == T_SYMBOL)
            {
                /* Split the string into an array of ints */

                inter_sp = sp;
                inter_pc = pc;
                v = allocate_uninit_array(svalue_strlen(sp) + 1);
                s = sp->u.string;
                svp = v->item;
                while (svp->type = T_NUMBER, svp->u.number =  *s++)
                    svp++;
                free_string_svalue(sp);
                sp->type = T_POINTER;
                sp->u.vec = v;
                break;
            }
            else if (sp->type == T_QUOTED_ARRAY)
            {
                /* Unquote it fully */
                sp->type = T_POINTER;
                break;
            }
            else if (sp->type == T_POINTER)
            {
                /* Good as it is */
                break;
            }
            else
                goto xbad_arg_1;
            break;
          }

                             /* --- esc set_extra_wizinfo_size --- */
        XCASE(F_SET_EXTRA_WIZINFO_SIZE);
          {
            /* XEFUN set_extra_wizinfo_size()
             *
             *   void set_extra_wizinfo_size(int size)
             *
DESCRIPTION
             * Indicate that the wizlist should contain an array of this size
             * with extra info foreach wizard. Causes the privilege violation
             * ("set_extra_wizinfo_size", this_object(), size).
             *
             * TODO: The extra_wizinfo idea could be applied to single objects
             * TODO:: and - ta da! - we have driver supported properties.
             * TODO:: Which then could be used to implement uids/euids etc.
             */
            
            if (sp->type != T_NUMBER)
                goto xbad_arg_1;
            if (privilege_violation("set_extra_wizinfo_size", sp))
                wiz_info_extra_size = sp->u.number;
            sp--;
            break;
          }

        /* --- XEfuns: Strings --- */
          
        XCASE(F_STRSTR);            /* --- esc strstr          --- */
          {
            /* XEFUN strstr()
             *
             *   int strstr (string str, string str2, int pos)
             *
             * Returns the index of str2 in str searching from position pos.
             * If str2 is not found in str, -1 is returned. The returned
             * index is relativ to the beginning of the string.
             *
             * If pos is negativ, it counts from the end of the string.
             */

            char *p1, *p2;
            int offs;

            if (sp[-2].type != T_STRING) goto xbad_arg_1;
            if (sp[-1].type != T_STRING) goto xbad_arg_2;
            if (sp[ 0].type != T_NUMBER) goto xbad_arg_3;
            
            p1 = sp[-2].u.string;
            if ( 0 != (offs = sp->u.number) )
            {
                /* Set p1 to the offset */
              
                if (offs < 0)
                {
                    offs += svalue_strlen(sp-2);
                }
              
                /* The loop is necessary because the allocated
                 * length might not be the real length.
                 * TODO: Lars sighs deeply.
                 */
                do {
                    if (!*p1++)
                    {
                        p1--;
                        break;
                    }
                } while (--offs);
            }
            
            /* Now do the search starting at p1 */
            p2 = strstr(p1, sp[-1].u.string);
            sp--;
            pop_stack();
            free_string_svalue(sp);
            put_number(p2 ? (p2 - p1) + sp[2].u.number : -1);
            break;
          }

        /* --- XEfuns: Arrays and Mappings --- */

        XCASE(F_M_ALLOCATE);        /* --- esc m_allocate      --- */
          {
            /* XEFUN m_allocate()
             *
             *   mapping m_allocate(int size, int width)
             *
             * Reserve memory for a mapping.
             *
             * size is the number of entries (i.e. keys) to reserve, width is
             * the number of data items per entry. If the optional width is
             * omitted, 1 is used as default.
             */
            
            if ( sp[-1].type != T_NUMBER || sp[-1].u.number < 0)
                goto xbad_arg_1;
            if ( sp->type != T_NUMBER || sp->u.number < 0)
                goto xbad_arg_2;
            sp--;
            if (!(sp->u.map = allocate_mapping(sp->u.number, sp[1].u.number)))
            {
                sp++;
                /* sp points to a number-typed svalue, so freeing won't
                 * be a problem.
                 */
                ERROR("Out of memory\n")
            }
            sp->type = T_MAPPING;
            break;
          }

        XCASE(F_M_CONTAINS);     /* --- esc m_contains <nargs> --- */
          {
            /* XEFUN m_contains()
             *
             *   int m_contains(mixed &data1, ..., &dataN, map, key)
             *
             * If the mapping contains the key map, the corresponding values
             * are assigned to the data arguments, which massed be passed by
             * reference, and 1 is returned. If key is not in map, 0 is
             * returned and the data args are left unchanged.
             * It is possible to use this function for a 0-value mapping, in
             * which case it has the same effect as member(E).
             */
            
            struct svalue *item;
            int i;

            GET_NUM_ARG

            /* Test the arguments */
            for (i = -num_arg; ++i < -1; )
                if (sp[i].type != T_LVALUE)
                    bad_arg_pc(num_arg + i, code + 0x100, sp, pc);
            if (sp[-1].type != T_MAPPING ||
                sp[-1].u.map->num_values != num_arg -2)
                    bad_arg_pc(num_arg + i, code + 0x100, sp, pc);
            
            item = get_map_lvalue(sp[-1].u.map, sp, MY_FALSE);
            if (item == &const0)
            {
                /* Not found */
                pop_n_elems(num_arg-1);
                free_svalue(sp);
                put_number(0);
                break;
            }
            
            free_svalue(sp--); /* free key */

            /* Copy the elements */
            for (i = -num_arg + 1; ++i < 0; )
            {
                /* get_map_lvalue() may return destructed objects. */
                /* TODO: May this cause problems elsewhere, too? */
                if (T_OBJECT == item->type
                 && (O_DESTRUCTED & item->u.ob->flags))
                {
                    assign_svalue(sp[i].u.lvalue, &const0);
                    item++;
                }
                else
                    /* mapping must not have been freed yet */
                    assign_svalue(sp[i].u.lvalue, item++);
                free_svalue(&sp[i]);
            }

            free_svalue(sp--); /* free mapping */
            sp += 3 - num_arg;
            put_number(1);
            break;
          }

        /* --- XEfuns: Functions and Closures --- */

        XCASE(F_CALLER_STACK);      /* --- esc caller_stack    --- */
          {
            /* XEFUN caller_stack()
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

            int depth, i;
            Bool done;
            struct control_stack *p;
            struct vector *v;
            struct svalue *svp;

            TYPE_TEST1(sp, T_NUMBER)

            /* Determine the depth of the call stack */
            p = csp;
            for (depth = 0, done = MY_FALSE; ; depth++)
            {
                do {
                    if (p == control_stack)
                    {
                        done = MY_TRUE;
                        break;
                    }
                } while ( !(--p)[1].extern_call );
                if (done)
                    break;
            }

            /* Allocate and fill in the result array */
            v = allocate_uninit_array(depth + (sp->u.number ? 1 : 0));
            p = csp;
            for (i = 0, svp = v->item, done = MY_FALSE; i < depth; i++, svp++)
            {
                struct object *prev;
                do {
                    if (p == control_stack)
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
                {
                    svp->type = T_NUMBER;
                    svp->u.number = 0;
                }
                else
                {
                    svp->type = T_OBJECT;
                    svp->u.ob = prev;
                    add_ref(prev, "caller_stack");
                }
            }

#ifdef DEBUG
            if (i < depth)
            {
                error("Computed stack depth to %d, but found only %d objects\n"
                     , depth, i);
                /* NOTREACHED */
                break;
            }
#endif

            /* If so desired, add the interactive object */
            if (sp->u.number)
            {
                if ( current_interactive
                 && !(current_interactive->flags & O_DESTRUCTED))
                {
                    svp->type = T_OBJECT;
                    svp->u.ob = current_interactive;
                    add_ref(current_interactive, "caller_stack");
                }
                else
                {
                    svp->type = T_NUMBER;
                    svp->u.number = 0;
                }
            }

            /* Assign the result and return */
            sp->type = T_POINTER;
            sp->u.vec = v;
            break;
          }

        XCASE(F_CALLER_STACK_DEPTH);  /* esc caller_stack_depth --- */
          {
            /* XEFUN caller_stack_depth()
             *
             *   int caller_stack_depth(void)
             *
             * Returns the number of previous objects on the stack. This
             * can be used for security checks.
             */

            int depth;
            Bool done;
            struct control_stack *p;

            /* Determine the depth of the call stack */
            p = csp;
            for (depth = 0, done = MY_FALSE; ; depth++)
            {
                do {
                    if (p == control_stack)
                    {
                        done = MY_TRUE;
                        break;
                    }
                } while ( !(--p)[1].extern_call );
                if (done)
                    break;
            }

            push_number(depth);
            break;
          }

        XCASE(F_CALL_RESOLVED);   /* --- esc call_resolved <nargs> --- */
          {
            /* XEFUN call_resolved()
             *
             *   int call_resolved(mixed & result, object ob, string func, ...)
             *
             * Similar to call_other(). If ob->func() is defined and publicly
             * accessible, any of the optional extra arguments are passed to
             * ob->func(...). The result of that function call is stored in
             * result, which must be passed by reference.
             *
             * If the current object is already destructed, or the ob does not
             * exist, or ob does not define a public accessible function named
             * func, call_resolved() returns 0 as failure code, else 1 for
             * success.
             *
             * ob can also be a file_name. If a string is passed for ob, and
             * no object with that name does exist, an error occurs.
             */

            struct svalue *arg;
            struct object *ob;

            ASSIGN_EVAL_COST
            GET_NUM_ARG
            inter_pc = pc;
            inter_sp = sp;
            arg = sp - num_arg + 1;

            /* Test the arguments */
            if (arg[0].type != T_LVALUE)
                goto xbad_arg_1;
            
            if (arg[1].type == T_OBJECT)
                ob = arg[1].u.ob;
            else if (arg[1].type == T_STRING)
            {
                ob = get_object(arg[1].u.string);
                if (!ob)
                    ERROR("call_resolved() failed: can't get object.\n")
            }
            else
                goto xbad_arg_2;
            
            if (arg[2].type != T_STRING)
                goto xbad_arg_3;
            
            /* No external calls may be done when this object is
             * destructed.
             */
            if (current_object->flags & O_DESTRUCTED)
            {
                pop_n_elems(num_arg);
                push_number(0);
                break;
            }

            /* Handle traceing. */
            if (TRACEP(TRACE_CALL_OTHER))
            {
                if (!++traceing_recursion)
                {
                    inter_sp = sp;
                    do_trace("Call other ", arg[1].u.string, "\n");
                }
                traceing_recursion--;
            }

            /* Send the remaining arguments to the function.
             */
            if (!apply_low(arg[2].u.string, ob, num_arg-3, MY_FALSE))
            {
                /* Function not found */
                pop_n_elems(num_arg-1);
                free_svalue(sp);
                put_number(0);
                break;
            }
            
            /* The result of the function call is on the stack. But, so
             * is the function name and object that was called.
             * These have to be removed.
             */
            sp = inter_sp;
            transfer_svalue(arg, sp--);  /* Copy the function call result */
            pop_n_elems(2);        /* Remove old arguments to call_solved */
            free_svalue(sp);        /* Free the lvalue */
            put_number(1);
            break;
          }

        XCASE(F_EXTERN_CALL);       /* --- esc extern_call     --- */
          {
            /* XEFUN extern_call()
             *
             *   int extern_call();
             *
             * Returns zero, if the function that is currently being executed
             * was called by a local call, non-zero for call_other(), driver
             * applies, closure calls, etc. Currently the only return value
             * for them is 1, but later the various methods may be
             * distinguished by means of the return value.
             */

            push_number((csp->extern_call & ~CS_PRETEND) ? 1 : 0);
            break;
          }

        XCASE(F_GET_EVAL_COST);     /* --- esc get_eval_cost   --- */
          {
            /* XEFUN get_eval_cost()
             *
             *   int get_eval_cost()
             *
             * Returns the remaining evaluation cost the current
             * execution (the current command) may use up.
             *
             * It starts at a driver given high value (__MAX_EVAL_COST__) and
             * is reduced with each executed statement.
             */

            push_number(-eval_cost);
            break;
          }

        XCASE(F_PREVIOUS_OBJECT);   /* --- esc previous_object --- */
          {
            /* XEFUN previous_object()
             *
             *   object previous_object(int i)
             *
             * Follow back the last <i> call_other()s and return the calling
             * object (i.e. previous_object(2) returns the caller of the
             * caller). It must hold 0 <= i < caller_stack_depth().
             *
             * There is an important special case: in functions called by the
             * gamedriver in reaction to some external event (e.g. commands
             * added by add_action), previous_object() will return
             * this_object(), but previous_object(0) will return 0.
             */
            
            int i;
            struct control_stack *p;
            struct object *prev_ob;

            /* Test the arguments */
            if (sp->type != T_NUMBER)
                goto xbad_arg_1;
            i = sp->u.number;
            if (i > MAX_TRACE) {
                sp->u.number = 0;
                break;
            }

            /* Set p back to the <i>th extern call */
            p = csp;
            do {
                do {
                    if (p == control_stack) {
                        sp->u.number = 0;
                        goto again;
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
                put_object(prev_ob);
            break;
          }
          
        /* --- XEfuns: Objects --- */

        XCASE(F_PROGRAM_TIME);      /* --- esc program_time    --- */
          {
            /* XEFUN program_time()
             *
             *   int program_time()
             *   int program_time(object ob)
             *
             * Returns the creation (compilation) time of the object's
             * program. Default is this_object(), if no arg is given.
             */
            
            int32 load_time;
            
            if (sp->type != T_OBJECT)
                goto xbad_arg_1;
            
            if (O_PROG_SWAPPED(sp->u.ob))
            {
                sp->u.ob->time_of_ref = current_time;
                if (load_ob_from_swap(sp->u.ob) < 0)
                {
                    sp--;
                    ERROR("Out of memory\n")
                }
            }
            load_time = sp->u.ob->prog->load_time;
            
            free_object_svalue(sp);
            put_number(load_time);
            break;
          }

        XCASE(F_PROGRAM_NAME);      /* --- esc program_name    --- */
          {
            /* XEFUN program_name()
             * 
             *   string program_name()
             *   string program_name(object obj)
             *
             * Returns the name of the program of <obj>, resp. the name of the
             * program of the current object if <obj> is omitted.
             *
             * The returned name is usually the name from which the blueprint
             * of <obj> was compiled (the 'load name'), but changes if an
             * object replaces its programs with the efun replace_program().
             *
             * The name always ends in '.c'. It starts with a '/' unless the
             * driver is running in COMPAT mode.
             */

            char *name, *res;
            struct object *ob;

            TYPE_TEST1(sp, T_OBJECT)

            ob = sp->u.ob;
            if (O_PROG_SWAPPED(ob))
            {
                ob->time_of_ref = current_time;
                if (load_ob_from_swap(ob) < 0)
                {
                    ERROR("Out of memory\n");
                }
            }
            name = ob->prog->name;
#ifdef COMPAT_MODE
            res = string_copy(name);
#else
            res = add_slash(name);
#endif
            if (!res)
                ERROR("Out of memory\n")
            free_object_svalue(sp);
            put_malloced_string(res, sp);
            break;
          }

                             /* --- esc query_once_interactive --- */
        XCASE(F_QUERY_ONCE_INTERACTIVE);
          {
            /* XEFUN query_once_interactive()
             *
             *   int query_once_interactive(object ob)
             *
             * True if the object is or once was interactive.
             */

            struct object *obj;

            if (sp->type != T_OBJECT) goto xbad_arg_1;

            obj = sp->u.ob;
            put_number(obj->flags & O_ONCE_INTERACTIVE ? 1 : 0);
            decr_object_ref(obj, "query_once_interactive");
            break;
          }

        /* --- XEfuns: Network IO --- */

#ifdef F_QUERY_IMP_PORT
        XCASE(F_QUERY_IMP_PORT);    /* --- esc query_imp_port  --- */
          {
            /* XEFUN query_imp_port()
             *
             *   int query_imp_port(void)
             *
             * Returns the port number that is used for the inter mud
             * protocol.
             */

            push_number(udp_port);
            break;
          }
#endif

        XCASE(F_QUERY_INPUT_PENDING); /* --- esc query_input_pending --- */
          {
            /* XEFUN query_input_pending()
             *
             *   object query_input_pending(object ob)
             *
             * If ob is interactive and currently has an input_to() pending,
             * the object that has called the input_to() is returned,
             * else 0.
             */

            struct object *ob;
            struct interactive *ip;

            TYPE_TEST1(sp, T_OBJECT)

            ob = sp->u.ob;
            if (NULL != (ip = O_GET_INTERACTIVE(ob))
             && ip->sent.type == SENT_INTERACTIVE && ip->input_to)
            {
                add_ref(sp->u.ob = ip->input_to->ob, "query_input_pending");
            }
            else
            {
                put_number(0);
            }

            decr_object_ref(ob, "query_input_pending");
            break;
          }

        XCASE(F_QUERY_IP_NAME);     /* --- esc query_ip_name   --- */
          {
            /* XEFUN query_ip_name()
             *
             *   string query_ip_name(object ob)
             *
             * Give the ip-name for user the current user or for the optional
             * argument ob. An asynchronous process 'hname' is used to find
             * out these names in parallel. If there are any failures to find
             * the ip-name, then the ip-number is returned instead.
             */
            
            inter_pc = pc;
            sp = query_ip_name(sp, MY_TRUE);
            break;
          }

        XCASE(F_QUERY_IP_NUMBER);   /* --- esc query_ip_number --- */
          {
            /* XEFUN query_ip_number()
             *
             *   string query_ip_number(object  ob)
             *   string query_ip_number(mixed & ob)
             *
             * Give the ip-number for the current user or the optional
             * argument ob.
             *
             * If ob is given as reference (and it must be a valid object
             * then), it will upon return be set to the struct sockaddr_in of
             * the queried object, represented by an array of integers, one
             * integer per address byte:
             *   ob[0.. 1]: sin_family
             *   ob[2.. 3]: sin_port
             *   ob[4.. 7]: sin_addr
             *   ob[8..15]: undefined.
             */

            inter_pc = pc;
            sp = query_ip_name(sp, MY_FALSE);
            break;
          }

        XCASE(F_QUERY_MUD_PORT);    /* --- esc query_mud_port  --- */
          {
            /* XEFUN query_mud_port()
             *
             *   int query_mud_port(void)
             *   int query_mud_port(object user)
             *   int query_mud_port(int num)
             *
             * Returns the port number the parser uses for user connections.
             *
             * If no argument is given, the port for this_player() is
             * returned. If this_player() is not existing or not interactive,
             * the first port number open for connections is returned.
             *
             * If an user object is given, the port used for its connection is
             * returned.
             * If a positive number is given, the <num>th port number the
             * parser uses for connections is returned (given that there are
             * that many ports).
             * If -1 is given, the number of ports open for connections is
             * returned.
             */

            inter_pc = pc;
            sp = query_ip_port(sp);
            break;
          }

        /* --- XEfuns: Driver and System --- */

        XCASE(F_GARBAGE_COLLECTION);  /* --- esc garbage_collection --- */
          {
            /* XEFUN garbage_collection()
             *
             *   void garbage_collection(void)
             *
             * Tell the parser to initiate a garbage collection after the
             * current execution ended.
             */

            extra_jobs_to_do = garbage_collect_to_do = MY_TRUE;
            time_last_gc = 0;  /* mark it as an 'unconditional' GC */
            break;
          }

        /* --- XEfuns: Inventories */

        XCASE(F_ALL_ENVIRONMENT); /* --- esc all_environment <nargs> --- */
        {
            /* XEFUN all_environment()
             *
             *   object *all_environment()
             *   object *all_environment(object o)
             *
             * Returns an array with all environments object <o> is in. If <o>
             * is omitted, the environments of the current object is returned.
             *
             * If <o> has no environment, or if <o> is destructed, 0 is
             * returned.
             */

            GET_NUM_ARG
            if (num_arg && sp->type != T_OBJECT) goto xbad_arg_1;
            inter_sp = sp;
            sp = x_all_environment(sp, num_arg);
            break;
        }

        /* --- Optional XEfuns --- */

#ifdef F_COPY_MAPPING
        XCASE(F_COPY_MAPPING);      /* --- esc copy_mapping    --- */
          {
            /* XEFUN copy_mapping()
             *
             *   mapping copy_mapping(mapping)
             *
             * This efun is needed to create copies of mappings instead of
             * just passing a reference, like adding/subtraction from a
             * mapping do.
             * TODO: This efun is outdated by the copy() efun.
             */

            struct mapping *m, *m2;

            TYPE_TEST1(sp, T_MAPPING)
            m = sp->u.map;
            check_map_for_destr(m);
            m2 = copy_mapping(m);
            free_mapping(m);
            sp->u.map = m2;
            break;
          }
#endif

#ifdef F_EXTRACT
          /* TODO: Get rid of efun extract() altogether */
        XCASE(F_EXTRACT1);          /* --- esc extract1        --- */
          {
            /* XEFUN extract1()
             *
             *   string extract(string arg)
             *
             * Generated by the compiler when it finds efun extract()
             * used with just the string, this efun returns the string.
             */

            if (sp->type != T_STRING)
                goto xbad_arg_1;

            break;
          }
          
        XCASE(F_EXTRACT);           /* --- esc extract         --- */
          {
            /* XEFUN extract()
             *
             *   string  extract(string str, int from, int to)
             *   string  extract(string str, int from)
             *   mixed * extract(mixed * arr, int from, int to)
             *   mixed * extract(mixed * arr, int from)
             *
             * Extract a substring from a string, resp. a subarray
             * from an array.
             * 
             * This is the old notation for str[from..to] and supported
             * only for hysterical raisins. The distinctive point is that
             * negative values for from and to implement the from-the-end
             * indexing.
             */

            if (sp[-1].type != T_NUMBER) goto xbad_arg_2;
            if (sp[0].type != T_NUMBER) goto xbad_arg_3;

            if (sp[-2].type == T_POINTER)
            {
                /* Extract from an array */
              
                struct vector *v;
                mp_int end, size;

                v = sp[-2].u.vec;
                v =
                  slice_array(
                    v,
                    sp[-1].u.number,
                    (end = sp[0].u.number) >= (size = VEC_SIZE(v)) ?
                      size - 1 :
                      end
                  );
                pop_n_elems(3);
                if (v)
                {
                    push_referenced_vector(v);
                }
                else
                {
                    push_number(0);
                }
            }
            else if (sp[-2].type == T_STRING)
            {
                /* Extract from a string */
              
                int len, from, to;
                char *res;

                len = _svalue_strlen(&sp[-2]);
                from = sp[-1].u.number;
                if (from < 0) {
                    from = len + from;
                    if (from < 0)
                        from = 0;
                }
                to = sp[0].u.number;
                if (to < 0)
                    to = len + to;
                if (to >= len)
                    to = len-1;
                if (to < from) {
                    pop_n_elems(3);
                    push_constant_string("");
                    break;
                }
                if (to == len-1) {
                    res = string_copy(sp[-2].u.string + from);
                    pop_n_elems(3);
                    push_malloced_string(res);
                    break;
                }
                res = xalloc(to - from + 2);
                strncpy(res, sp[-2].u.string + from, to - from + 1);
                res[to - from + 1] = '\0';
                pop_n_elems(3);
                push_malloced_string(res);
            }
            else
            {
                goto xbad_arg_1;
            }
            break;
          }
#endif

#ifdef F_SWAP
        XCASE(F_SWAP);              /* --- esc swap            --- */
          {
            /* XEFUN swap()
             *
             *   void swap(object obj)
             *
             * Swap out an object. This efun is only used for system internal
             * debugging and can cause a crash.
             */

            struct object *ob;

            if (sp->type != T_OBJECT) goto xbad_arg_1;
            ob = sp->u.ob;
            if (ob != current_object) /* should also check csp */
            {
                if (!O_PROG_SWAPPED(ob))
                    (void)swap_program(ob);
                if (!O_VAR_SWAPPED(ob))
                    (void)swap_variables(ob);
            }
            pop_stack();
            break;
          }
#endif

        } /* switch(code) */
        break;
      } /* end of F_ESCAPE */

    } /* end of the monumental switch */

    /* Instruction executed */
    
#ifdef DEBUG
    if (expected_stack && expected_stack != sp)
    {
        fatal("Bad stack after evaluation.\n"
              "sp: %lx expected: %lx\n"
              "Instruction %d, num arg %d\n",
              (long)sp, (long)expected_stack,
              instruction + F_OFFSET, num_arg);
    }

    if (sp < fp + csp->num_local_variables - 1)
    {
        fatal("Bad stack after evaluation.\n"
              "sp: %lx minimum expected: %lx\n"
              "Instruction %d, num arg %d\n",
              (long)sp, (long)(fp + csp->num_local_variables - 1),
              instruction + F_OFFSET, num_arg);
    }
#endif /* DEBUG */

    /* Execute the next instruction */
    
    goto again;

    /* Get rid of the handy but highly local macros */
#   undef GET_NUM_ARG
#   undef TYPE_TEST1
#   undef TYPE_TEST2
#   undef TYPE_TEST3
#   undef TYPE_TEST4
#   undef CASE
} /* eval_instruction() */

/*-------------------------------------------------------------------------*/

/* These macros are no longer needed:
 */
#undef push_malloced_string
#undef push_number

/*-------------------------------------------------------------------------*/
static Bool
apply_low (char *fun, struct object *ob, int num_arg, Bool b_ign_prot)

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
    struct program *progp;
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
        struct object *shadow;

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
        if (load_ob_from_swap(ob) < 0)
            error("Out of memory\n");

    progp = ob->prog;
    
#ifdef DEBUG
    if (ob->flags & O_DESTRUCTED)
        fatal("apply() on destructed object '%s' function '%s'\n"
             , ob->name != NULL ? ob->name : "<null>"
             , fun != NULL ? fun : "<null>"
             );
#endif
    
    if ( !((p_int)fun & 2) )
    {
        /* Heuristic to find out if fun is an unshared string,
         * building on the six-byte-overhead of shared strings.
         * This means that every shared string has a set bit 1,
         * whereas other strings have no overhead and thus bit 1
         * cleared. Function names are always shared, so if there
         * is no shared string twin for fun, the called function does
         * not exist anywhere. As a side effect, we get the shared
         * string for fun which makes the cache-lookup much faster.
         */
        fun = findstring(fun);
        if (!fun)
            goto failure2;
    }
    /* *fun is now (more or less) guaranteed to be a shared string */

    /* Get the hashed index into the cache */
    ix =
      ( progp->id_number ^ (p_int)fun ^ ( (p_int)fun >> APPLY_CACHE_BITS ) )
         & (CACHE_SIZE-1);
    
    /* Check if we have an entry for this function call */
    if (cache[ix].id == progp->id_number
     && (cache[ix].name == fun || !strcmp(cache[ix].name, fun))
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
          /* Static functions may not be called from outside. */
          && (   !cache[ix].flags
              || b_ign_prot
              || (   !(cache[ix].flags & TYPE_MOD_PROTECTED)
                  && current_object == ob)) 
           )
        {
            /* the cache will tell us in wich program the function is, and
             * where.
             */
            fun_hdr_p funstart;

            push_control_stack(inter_sp, inter_pc, inter_fp);
            csp->ob = current_object;
            csp->prev_ob = previous_ob;
            csp->num_local_variables = num_arg;
            csp->funstart = funstart = cache[ix].funstart;
            current_prog = cache[ix].progp;
            current_strings = current_prog->strings;
            function_index_offset = cache[ix].function_index_offset;
            current_variables = ob->variables
                                + cache[ix].variable_index_offset;
            inter_sp = setup_new_frame2(funstart, inter_sp);
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
        char *shared_name;

#ifdef APPLY_CACHE_STAT
        apply_cache_miss++;
#endif
        /* This call to findstring() is not really necessary, but serves
         * as safeguard should a non-shared string escape the attention
         * of the 'heuristic' filter before.
         */
        shared_name = (cache[ix].name == fun) ? fun : findstring(fun);
        if ( NULL != shared_name)
        {
            int fx;

            /* Yup, fun is a function _somewhere_ */
            
            eval_cost++;
            fx = find_function(shared_name, progp);
            if (fx >= 0)
            {
                /* Found the function - setup the control stack and
                 * create a new cache entry.
                 */
              
                /* TODO: funflags */ uint32 flags;
                fun_hdr_p funstart;

                push_control_stack(inter_sp, inter_pc, inter_fp);
                  /* if an error occurs here, it won't leave the cache in an
                   * inconsistent state.
                   */
                csp->ob = current_object;
                csp->prev_ob = previous_ob;
                if (!cache[ix].progp)
                {
                    /* The old cache entry was for an undefined function,
                     * so the name had to be malloced.
                     */
                    xfree(cache[ix].name);
                }

                cache[ix].id = progp->id_number;
                cache[ix].name = shared_name;
                csp->num_local_variables = num_arg;
                current_prog = progp;
                flags = setup_new_frame1(fx, 0, 0);
                current_strings = current_prog->strings;
                cache[ix].progp = current_prog;
                cache[ix].function_index_offset = function_index_offset;
                cache[ix].variable_index_offset = variable_index_offset;
                current_variables = ob->variables
                                    + variable_index_offset;
                funstart = current_prog->program + (flags & FUNSTART_MASK);
                cache[ix].funstart = funstart;
                cache[ix].flags = progp->functions[fx] 
                                  & (TYPE_MOD_STATIC|TYPE_MOD_PROTECTED);
                
                /* Static functions may not be called from outside. */
                if (0 != cache[ix].flags
                  && ((cache[ix].flags & TYPE_MOD_PROTECTED)
                      || current_object != ob)
                  && !b_ign_prot
                    )
                {
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
                inter_sp = setup_new_frame2(funstart, inter_sp);
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
        } /* end if(shared_name) */
        
        /* We have to mark this function as non-existant in this object. */

        if (!cache[ix].progp)
        {
            /* The old cache entry was for an undefined function, so the
               name had to be malloced */
            xfree(cache[ix].name);
        }
        
        cache[ix].id = progp->id_number;
        cache[ix].name = string_copy(fun);
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
    if (fun[0] == ':')
        error("Illegal function call\n");

failure2:
    /* Failure. Deallocate stack. */
    return MY_FALSE;
} /* apply_low() */

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
struct svalue *
sapply_int (char *fun, struct object *ob, int num_arg, Bool b_find_static)

/* Call function <fun> in <ob>ject with <num_arg> arguments pushed
 * onto the stack (<inter_sp> points to the last one). static and protected
 * functions can't be called from the outside unless <b_find_static> is true.
 * sapply() takes care of calling shadows where necessary.
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
    struct svalue *expected_sp;
#endif

    /* Handle tracing */
    if (TRACEP(TRACE_APPLY))
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
    if (!apply_low(fun, ob, num_arg, b_find_static))
    {
        inter_sp = _pop_n_elems(num_arg, inter_sp);
        return NULL;
    }
    transfer_svalue(&apply_return_value, inter_sp);
    inter_sp--;

#ifdef DEBUG
    if (expected_sp != inter_sp)
        fatal("Corrupt stack pointer.\n");
#endif

    return &apply_return_value;
} /* sapply_int() */

/*-------------------------------------------------------------------------*/
struct svalue *
apply (char *fun, struct object *ob, int num_arg)

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
    return sapply_int(fun, ob, num_arg, MY_FALSE);
}

/*-------------------------------------------------------------------------*/
static void
secure_apply_error (struct svalue *save_sp, struct control_stack *save_csp)

/* Recover from an error during a secure apply. <save_sp> and <save_csp>
 * are the saved evaluator stack and control stack pointers, saving the
 * state from when secure_apply() was entered.
 *
 * The function pops all the arguments for the call from the stack, and
 * then calls runtime_error() in the master object with the necessary
 * information, unless it is a triple fault - in that case only a
 * debug_message() is generated.
 */

{
    if (csp != save_csp)
    { 
        /* could be error before push */
        csp = save_csp+1;
        previous_ob = csp->prev_ob;
        current_object = csp->ob;
        pop_control_stack();
    }
    
    inter_sp = _pop_n_elems (inter_sp - save_sp, inter_sp);
    
    if (num_error == 3)
    {
        if (!out_of_memory)
        {
            debug_message("Master failure: %s", current_error);
            xfree(current_error);
            xfree(current_error_file);
            xfree(current_error_object_name);
        }
    }
    else if (!out_of_memory)
    {
        int a;
        struct object *save_cmd;

        push_malloced_string(current_error);
        a = 1;
        if (current_error_file)
        {
            push_malloced_string(current_error_file);
            push_malloced_string(current_error_object_name);
            push_number(current_error_line_number);
            a += 3;
        }
        save_cmd = command_giver;
        apply_master_ob(STR_RUNTIME, a);
        command_giver = save_cmd;
    }
    num_error--;
    
} /* secure_apply_error() */

/*-------------------------------------------------------------------------*/
struct svalue *
secure_apply (char *fun, struct object *ob, int num_arg)

/* Call function <fun> in <ob>ject with <num_arg> arguments pushed
 * onto the stack (<inter_sp> points to the last one). static and protected
 * functions can't be called from the outside.
 * secure_apply() takes care of calling shadows where necessary.
 *
 * secure_apply() returns a pointer to the function result when the call was
 * successfull, or NULL on failure. The arguments are popped in any case.
 * The result pointer, if returned, points to a static area which will be
 * overwritten with the next secure_apply().
 *
 * The function call will swap in the object and also unset its reset status.
 *
 * Errors during the execution are caught (this is the big difference
 * to sapply()/apply()) and cause secure_apply() to return NULL.
 */

{
    struct error_recovery_info error_recovery_info;
    struct svalue *save_sp;
    struct control_stack *save_csp;
    struct svalue *result;

    if (ob->flags & O_DESTRUCTED)
        return NULL;

    error_recovery_info.last = error_recovery_pointer;
    error_recovery_info.type = ERROR_RECOVERY_APPLY;
    error_recovery_pointer = &error_recovery_info;
    save_sp = inter_sp;
    save_csp = csp;
    if (setjmp(error_recovery_info.con.text))
    {
        secure_apply_error(save_sp - num_arg, save_csp);
        result = NULL;
    }
    else
    {
        result = sapply(fun, ob, num_arg);
    }
    error_recovery_pointer = error_recovery_info.last;
    return result;
} /* secure_apply() */

/*-------------------------------------------------------------------------*/
struct svalue *
apply_master_ob (char *fun, int num_arg)

/* Call function <fun> in the master object with <num_arg> arguments pushed
 * onto the stack (<inter_sp> points to the last one). static and protected
 * functions can be called from the outside. The function takes care
 * of calling shadows where necessary.
 *
 * secure_apply() returns a pointer to the function result when the call was
 * successfull, or NULL on failure. The arguments are popped in any case.
 * The result pointer, if returned, points to a static area which will be
 * overwritten with the next secure_apply().
 *
 * The function makes sure that there is a master object to be called. If
 * necessary, a new one is compiled or, failing that, an old one is
 * reactivated.
 *
 * Errors during the execution are caught and case the function to 
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
    struct svalue *save_sp;
    struct control_stack *save_csp;
    struct svalue *result;

    /* Get the master object. */
    assert_master_ob_loaded();
    
    /* Tap into the eval_cost reserve if the end is near */
    if ( (  eval_cost >= MIN_TRACE_COST
          ? eval_cost > MAX_TRACE_COST - MASTER_RESERVED_COST
          : eval_cost > -MASTER_RESERVED_COST)
        &&  eval_cost_reserve > 1)
    {
        eval_cost -= eval_cost_reserve;
        assigned_eval_cost -= eval_cost_reserve;
        eval_cost_reserve >>= 1;
        reserve_used = MY_TRUE;
    }
    
    /* Setup the the error recovery and call the function */
    error_recovery_info.last = error_recovery_pointer;
    error_recovery_info.type = ERROR_RECOVERY_APPLY;
    error_recovery_pointer = &error_recovery_info;
    save_sp = inter_sp;
    save_csp = csp;
    if (setjmp(error_recovery_info.con.text))
    { 
        secure_apply_error(save_sp - num_arg, save_csp);
        printf("Error in master_ob->%s()\n", fun);
        debug_message("Error in master_ob->%s()\n", fun);
        result = NULL;
    }
    else
    {
        result = sapply_int(fun, master_ob, num_arg, MY_TRUE);
    }
    
    /* Free the reserve if we used it */
    if (reserve_used)
    {
        eval_cost_reserve <<= 1;
        assigned_eval_cost = eval_cost += eval_cost_reserve;
    }
    error_recovery_pointer = error_recovery_info.last;

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
    
    static struct object *destructed_master_ob = NULL;
      /* Old, destructed master object */
    
    int i;

    if (!master_ob || master_ob->flags & O_DESTRUCTED)
    {
        /* The master object has been destructed. Free our reference,
         * and load a new one.
         */
        if (inside || !master_ob)
        {
            struct object *ob;
            struct object *prev;
            Bool removed = MY_FALSE; 
              /* TRUE if the old master was already on the list of
               * destructed objects.
               */

            /* A recursive call while loading the master, or there
             * was no master to begin with.
             * If there is a destructed master, reactivate that
             * one, else stop the driver.
             */
          
            if (!destructed_master_ob)
            {
                fprintf(stderr, "Failed to load master object '%s'.\n"
                              , master_name);
                add_message("Failed to load master object '%s'!\n"
                           , master_name);
                exit(1);
            }

            /* If we come here, we had a destructed master and failed
             * to load a new one. Now try to reactivate the
             * old one again.
             */

            /* First, make sure that there is no half-done object
             * using the masters name.
             */
            if ( NULL != (ob = find_object(master_name)) )
            {
                emergency_destruct(ob);
            }
            
            /* Get the destructed master */
            ob = destructed_master_ob;
            destructed_master_ob = NULL;
            
            /* Remove the destructed master from the list
             * of destructed objects
             */
            if (new_destructed)
            {
                if (ob == destructed_objs)
                {
                    destructed_objs = ob->next_all;
                    removed = MY_TRUE;
                    new_destructed--;
                }
                else
                {
                    for ( prev = destructed_objs
                        ; prev && prev->next_all != ob
                        ; prev = prev->next_all
                        ) NOOP;
                    if (prev)
                    {
                        prev->next_all = ob->next_all;
                        removed = MY_TRUE;
                        new_destructed--;
                    }
                }
            }
            ob->flags &= ~O_DESTRUCTED;
            
            /* Restore the old masters variable space.
             * Remember: as long as the objects are in the 'destructed'
             * list, they still have all variables.
             */
            if (!removed && ob->prog->num_variables)
            {
                int save_privilege = malloc_privilege;
                int j;
                struct svalue *v;

                malloc_privilege = MALLOC_SYSTEM;
                ob->variables = v = (struct svalue *)
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
#ifndef OLD_RESET
            if (!obj_list_end)
                obj_list_end = ob;
#endif
            ob->super = NULL;
            ob->contains = NULL;
            ob->next_inv = NULL;
            
            /* Reactivate the old master */
            master_ob = ob;
            add_ref(ob, "assert_master_ob_loaded");
            if (current_object == &dummy_current_object_for_loads)
                current_object = master_ob;
            push_number(removed);
            sapply(STR_REACTIVATE, ob, 1);
            push_number(2 - (removed ? 1 : 0));
            sapply(STR_INAUGURATE, ob, 1);
            fprintf(stderr, "Old master reactivated.\n");
            inside = MY_FALSE;
            return;
            
        } /* if (inside || !master_obj) */

        /* A normal call to assert_master_ob_loaded: just load a new one */

        fprintf(stderr, "assert_master_ob_loaded: Reloading master.c\n");
        destructed_master_ob = master_ob;

        /* Clear the pointer, in case the load failed.
         */
        master_ob = NULL;
        inside = MY_TRUE;
        clear_auto_include_string();
        if (!current_object)
        {
            current_object = &dummy_current_object_for_loads;
        }
        
        /* don't free the closure hooks now since they might be 
         * still in use - the backend will take care of them.
         */
        free_closure_hooks(closure_hook, NUM_CLOSURE_HOOKS);
        for (i = NUM_CLOSURE_HOOKS; i--;)
            closure_hook[i] = const0;
        
        init_telopts();
        
        master_ob = get_object(master_name);
        if (current_object == &dummy_current_object_for_loads)
        {
            /* This might be due to the above assignment, or to setting
             * it in the backend.
             */
            current_object = master_ob;
        }
        
        initialize_master_uid();
        push_number(3);
        apply_master_ob(STR_INAUGURATE, 1);
        assert_master_ob_loaded();
          /* ...in case inaugurate_master() destructed this object again */
        inside = MY_FALSE;
        add_ref(master_ob, "assert_master_ob_loaded");
        
        if (destructed_master_ob)
            free_object(destructed_master_ob, "assert_master_ob_loaded");
        
        fprintf(stderr, "Reloading done.\n");
    }

    /* Master exists. Nothing to see here, move along... */
    
} /* assert_master_ob_loaded() */

/*-------------------------------------------------------------------------*/
void
call_lambda (struct svalue *lsvp, int num_arg)

/* Call the closure <lsvp> with <num_arg> arguments on the stack. On
 * success, the arguments are replaced with the result, else an error()
 * is generated.
 */

{
#  define CLEAN_CSP \
        previous_ob = csp->prev_ob; \
        current_object = csp->ob; \
        pop_control_stack();
  /* Macro to undo all the call preparations in case the closure
   * can't be called after all.
   */

    struct svalue *sp;
    struct lambda *l = lsvp->u.lambda;

    sp = inter_sp;

    /* Basic setup for the new control frame.
     * If the closure can't be called, all this has to be undone
     * using the macro CLEAN_CSP.
     */
    push_control_stack(sp, inter_pc, inter_fp);
    csp->ob = current_object;
    csp->prev_ob = previous_ob;
    csp->num_local_variables = num_arg;
    previous_ob = current_object;

    switch(lsvp->x.closure_type)
    {

    case CLOSURE_LFUN:  /* --- lfun closure --- */
      {
        /* TODO: funflags */ uint32 flags;
        fun_hdr_p funstart;

        /* Reference the object the lfun is bound to */
        l->ob->time_of_ref = current_time;
        l->ob->flags &= ~O_RESET_STATE;

        current_object = l->ob;
       
        /* Can't call from a destructed object */
        if (l->ob->flags & O_DESTRUCTED)
        {
            /* inter_sp == sp */
            CLEAN_CSP
            error("Object '%s' the closure was bound to has been "
                  "destructed\n", l->ob->name);
            /* NOTREACHED */
            return; 
        }

        /* Make the object resident */
        if (current_object->flags & O_SWAPPED
         && load_ob_from_swap(current_object) < 0)
        {
            /* inter_sp == sp */
            CLEAN_CSP
            error("Out of memory\n");
            /* NOTREACHED */
            return; 
        }

        /* Ok, object and program are there */
        
        current_prog = current_object->prog;
        /* inter_sp == sp */
        flags = setup_new_frame(l->function.index);
        funstart = current_prog->program + (flags & FUNSTART_MASK);
        csp->funstart = funstart;
        eval_instruction(FUNCTION_CODE(funstart), inter_sp);
        /* The result is on the stack (inter_sp) */
        return;
      }
      
    case CLOSURE_ALIEN_LFUN:  /* --- alien lfun closure --- */
      {
        /* TODO: funflags */ uint32 flags;
        fun_hdr_p funstart;

        /* Reference the bound and the originating object */
        l->ob->time_of_ref = current_time;
        l->function.alien.ob->time_of_ref = current_time;
        l->function.alien.ob->flags &= ~O_RESET_STATE;

        current_object = l->ob;

        /* Can't call from a destructed object */
        if (l->ob->flags & O_DESTRUCTED)
        {
            /* inter_sp == sp */
            CLEAN_CSP
            error("Object '%s' the closure was bound to has been "
                  "destructed\n", l->ob->name);
            /* NOTREACHED */
            return; 
        }

        /* Can't call a function in a destructed object */
        if (l->function.alien.ob->flags & O_DESTRUCTED)
        {
            /* inter_sp == sp */
            CLEAN_CSP
            error("Object '%s' holding the closure has been "
                  "destructed\n", l->function.alien.ob->name);
            /* NOTREACHED */
            return; 
        }

        /* Make the objects resident */
        if ( (   current_object->flags & O_SWAPPED
              && load_ob_from_swap(current_object) < 0)
         ||  (   l->function.alien.ob->flags & O_SWAPPED
              && load_ob_from_swap(l->function.alien.ob) < 0)
           )
        {
            /* inter_sp == sp */
            CLEAN_CSP
            error("Out of memory\n");
            /* NOTREACHED */
            return; 
        }

        /* Finish the setup of the control frame.
         * This is a real inter-object call, so we create a second
         * frame to really capture the control flow.
         */
        csp->extern_call = MY_TRUE;
        csp->funstart = NULL;
        push_control_stack(sp, 0, inter_fp);
        csp->ob = current_object;
        csp->prev_ob = previous_ob;
        csp->num_local_variables = num_arg;
        previous_ob = current_object;
        current_object = l->function.alien.ob;
        current_prog = current_object->prog;
        /* inter_sp == sp */
        flags = setup_new_frame(l->function.alien.index);
        funstart = current_prog->program + (flags & FUNSTART_MASK);
        csp->funstart = funstart;
        eval_instruction(FUNCTION_CODE(funstart), inter_sp);
        /* The result is on the stack (inter_sp) */
        current_object = csp->ob;
        previous_ob = csp->prev_ob;
        pop_control_stack();
        return;
      }

    case CLOSURE_IDENTIFIER:  /* --- variable closure --- */
      {
        short i; /* the signed variant of struct lambda->function.index */

        CLEAN_CSP  /* no call will be done */
        if (num_arg)
            error("Arguments passed to variable closure.\n");

        /* Don't use variables in a destructed object */
        if (l->ob->flags & O_DESTRUCTED)
        {
            error("Object '%s' the closure was bound to has been destructed\n"
                 , l->ob->name);
            /* NOTREACHED */
            return; 
        }
        
        /* Make the object resident */
        if (   (l->ob->flags & O_SWAPPED)
             && load_ob_from_swap(l->ob) < 0
           )
        {
            error("Out of memory.\n");
            /* NOTREACHED */
            return;
        }
        
        /* Do we have the variable? */
        if ( (i = l->function.index) < 0)
        {
            error("Variable not inherited\n");
            /* NOTREACHED */
            return;
        }
        
        l->ob->time_of_ref = current_time;
        assign_svalue_no_free(++sp, &l->ob->variables[i]);
        inter_sp = sp;
        return;
      }
      
    case CLOSURE_BOUND_LAMBDA:  /* --- bound lambda closure --- */
      {
        struct lambda *l2;

        /* Deref the closure and then treat the resulting unbound
         * lambda like a normal lambda
         */
        l2 = l->function.lambda;
        l2->ob = l->ob;
        l = l2;
      }
      /* FALLTHROUGH */

    case CLOSURE_LAMBDA:
      {
        fun_hdr_p funstart;

        current_object = l->ob;
        
        /* Can't call from a destructed object */
        if (l->ob->flags & O_DESTRUCTED)
        {
            /* inter_sp == sp */
            CLEAN_CSP
            error("Object '%s' the closure was bound to has been "
                  "destructed\n", l->ob->name);
            /* NOTREACHED */
            return; 
        }

        /* Make the object resident */
        if (current_object->flags & O_SWAPPED
         && load_ob_from_swap(current_object) < 0)
        {
            /* inter_sp == sp */
            CLEAN_CSP
            error("Out of memory\n");
            /* NOTREACHED */
            return; 
        }

        /* Reference the object */
        current_object->time_of_ref = current_time;
        current_object->flags &= ~O_RESET_STATE;

        /* Finish the setup */
        
        current_prog = current_object->prog;
        variable_index_offset = 0;
        function_index_offset = 0;
        funstart = l->function.code + 1;
        csp->funstart = funstart;
        sp = setup_new_frame2(funstart, sp);
        current_variables = current_object->variables;
        current_strings = current_prog->strings;
        eval_instruction(FUNCTION_CODE(funstart), sp);
        /* The result is on the stack (inter_sp) */
        return;
      }

    case CLOSURE_UNBOUND_LAMBDA:
    case CLOSURE_PRELIMINARY:
        /* no valid current_object ==> pop the control stack */
        /* inter_sp == sp */
        CLEAN_CSP
        break;

    default: /* --- efun-, simul efun-, operator closure */
      {
        int i;  /* the closure type */

        current_object = lsvp->u.ob;

        /* Can't call from a destructed object */
        if (current_object->flags & O_DESTRUCTED)
        {
            /* inter_sp == sp */
            CLEAN_CSP
            error("Object '%s' the closure was bound to has been "
                  "destructed\n", current_object->name);
            /* NOTREACHED */
            return; 
        }

        /* Make the object resident */
        if (current_object->flags & O_SWAPPED
         && load_ob_from_swap(current_object) < 0)
        {
            /* inter_sp == sp */
            CLEAN_CSP
            error("Out of memory\n");
            /* NOTREACHED */
            return; 
        }

        /* Reference the object */
        current_object->time_of_ref = current_time;
    
        i = lsvp->x.closure_type;
        if (i < CLOSURE_SIMUL_EFUN)
        {
            /* It's an operator or efun */
          
            if (i == CLOSURE_EFUN + F_UNDEF - F_OFFSET) 
            {
                /* The closure was discovered to be bound to a destructed
                 * object and thus disabled.
                 */
                CLEAN_CSP
                error("Object the closure was bound to has been destructed\n");
                /* NOTREACHED */
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
                bytecode_t code[5];  /* the code fragment */
                bytecode_p p;        /* the code pointer */

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
                        *p++ = def - F_OFFSET;
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
                            inter_pc = csp->funstart = (char *)(p_int)i;
                            error("Too few arguments to %s\n", instrs[i].name);
                        }
                    }
                }
                else if (num_arg > max && max != -1)
                {
                    csp->extern_call = MY_TRUE;
                    inter_pc = csp->funstart = (char *)(p_int)i;
                    error("Too many arguments to %s\n", instrs[i].name);
                }

                /* Store the instruction code */
                if (i > 0xff)
                    *p++ = i >> F_ESCAPE_BITS;
                *p++ = i;

                /* Store the <nargs> code, if necessary */
                if (min != max)
                    *p++ = num_arg;

                /* And finally the return instruction */
                if ( instrs[i].ret_type == TYPE_VOID )
                    *p++ = F_RETURN0-F_OFFSET;
                else
                    *p++ = F_RETURN-F_OFFSET;

                /* Note: TubMud suggested marking efun closures by
                 * clearing csp->prog here, and adding a corresponding
                 * test in dump_trace(). However, csp->prog must remain
                 * valid else the driver will think on F_RETURN that
                 * it encountered the bottom of the cs stack and
                 * forget to update current_strings. At least.
                 * TODO: If more places crop up where a ob->prog of NULL
                 * TODO:: is a problem (like in dump_trace()), we should
                 * TODO:: invent a fake struct prog to keep things easy.
                 * TODO:: See also p-990203-1.
                 */
                csp->funstart = FUNCTION_FROM_CODE(code);
                csp->num_local_variables = 0;
                inter_fp = sp - num_arg + 1;
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
            struct object *ob;

            /* Mark the call as sefun closure */
            inter_pc = csp->funstart = SIMUL_EFUN_FUNSTART;
            
            /* Get the simul_efun object */
            if ( !(ob = simul_efun_object) )
            {
                /* inter_sp == sp */
                if ( !(ob = get_simul_efun_object()) ) {
                    csp->extern_call = MY_TRUE;
                    error("Couldn't load simul_efun object\n");
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
    error("Uncallable closure\n");
    /* NOTREACHED */
    return;

#   undef CLEAN_CSP
} /* call_lambda() */

/*-------------------------------------------------------------------------*/
struct svalue *
secure_call_lambda (struct svalue *closure, int num_arg)

/* Call the closure <closure> with <num_arg> arguments on the stack.
 * On success, the functions returns a pointer to the result on the stack,
 * on failure it returns NULL. The arguments are removed in either case.
 *
 * This error recovery is the difference to call_lambda().
 */

{
    struct error_recovery_info error_recovery_info;
    struct svalue *save_sp;
    struct control_stack *save_csp;
    struct svalue *result;

    error_recovery_info.last = error_recovery_pointer;
    error_recovery_info.type = ERROR_RECOVERY_APPLY;
    error_recovery_pointer = &error_recovery_info;
    save_sp = inter_sp;
    save_csp = csp;
    
    if (setjmp(error_recovery_info.con.text))
    {
        secure_apply_error(save_sp - num_arg, save_csp);
        result = NULL;
    }
    else
    {
        call_lambda(closure, num_arg);
        transfer_svalue((result = &apply_return_value), inter_sp);
        inter_sp--;
    }
    error_recovery_pointer = error_recovery_info.last;
    return result;
} /* secure_call_lambda() */

/*-------------------------------------------------------------------------*/
static void
call_simul_efun (int code, struct object *ob, int num_arg)

/* Call the simul_efun <code> in the sefun object <ob> with <num_arg>
 * arguments on the stack. If it can't be found in the <ob>ject, the
 * function queries the auxiliary sefun objects in <simul_efun_vector>.
 *
 * The function is looked up in the objects by name.
 *
 * Leave the result on the stack on return.
 */

{
    char *function_name;

    function_name = simul_efunp[code].name;

    /* First, try calling the function in the given object */
    if (!apply_low(function_name, ob, num_arg, MY_FALSE))
    {
        /* Function not found: try the alternative sefun objects */
        if (simul_efun_vector)
        {
            int i;
            struct svalue *v;

            i = VEC_SIZE(simul_efun_vector);
            for (v = simul_efun_vector->item+1; MY_TRUE; v++)
            {
                if (--i <= 0 || v->type != T_STRING)
                {
                    error("Calling a vanished simul_efun\n");
                    return;
                }
                if ( !(ob = get_object(v->u.string)) )
                    continue;
                if (apply_low(function_name, ob, num_arg, MY_FALSE))
                    return;
            }
            return;
        }
        error("Calling a vanished simul_efun\n");
        return;
    }
    /*
     * The result of the function call is on the stack.
     */
} /* call_simul_efun() */

/*-------------------------------------------------------------------------*/
char *
function_exists (char *fun, struct object *ob)

/* Search for the function <fun> in the object <ob>. If existing, return
 * the name of the program, if not return NULL.
 *
 * Visibility rules apply: static and protected functions can't be
 * found from the outside.
 */

{
    char *shared_name;
    fun_hdr_p funstart;
    struct program *progp;
    int ix;
    /* TODO: funflags */ uint32 flags;

#ifdef DEBUG
    if (ob->flags & O_DESTRUCTED)
        fatal("function_exists() on destructed object\n");
#endif

    /* Make the program resident */
    if (O_PROG_SWAPPED(ob))
    {
        ob->time_of_ref = current_time;
        if (load_ob_from_swap(ob) < 0)
            error("Out of memory\n");
    }
    
    shared_name = findstring(fun);
    progp = ob->prog;

    /* Check if the function exists at all */
    if ( (ix = find_function(shared_name, progp)) < 0)
        return NULL;
   
    /* Is it visible for the caller? */
    flags = progp->functions[ix];
    
    if (flags & TYPE_MOD_PRIVATE
     || (flags & TYPE_MOD_STATIC && current_object != ob))
        return NULL;
    
    /* Resolve inheritance */
    while (flags & NAME_INHERITED)
    {
        struct inherit *inheritp;

        inheritp = &progp->inherit[flags & INHERIT_MASK];
        ix -= inheritp->function_index_offset;
        progp = inheritp->prog;
        flags = progp->functions[ix];
    }
    
    funstart = progp->program  + (flags & FUNSTART_MASK);

    /* And after all this, the function may be undefined */
    if (FUNCTION_CODE(funstart)[0] == F_ESCAPE - F_OFFSET
     && FUNCTION_CODE(funstart)[1] == F_UNDEF  - F_OFFSET - 0x100)
    {
        return NULL;
    }

    /* We got it. */
    return progp->name;
} /* function_exists() */

/*-------------------------------------------------------------------------*/
void
call_function (struct program *progp, int fx)

/* Call the function <fx> in program <progp> for the current_object.
 * This is done with no frame set up. No arguments are passed,
 * returned values are removed.
 *
 * Right now this function is used just for heartbeats, and the
 * way of calling prevents shadows from being called.
 */
  
{
    /* TODO: funflags */ uint32 flags;
    fun_hdr_p funstart;

    push_control_stack(inter_sp, inter_pc, inter_fp);
    csp->ob = current_object;
    csp->prev_ob = previous_ob;
#ifdef DEBUG
    if (csp != control_stack)
        fatal("call_function with bad csp\n");
#endif
    csp->num_local_variables = 0;
    current_prog = progp;
    flags = setup_new_frame(fx);
    funstart = current_prog->program + (flags & FUNSTART_MASK);
    csp->funstart = funstart;
    previous_ob = current_object;
    tracedepth = 0;
    eval_instruction(FUNCTION_CODE(funstart), inter_sp);
    free_svalue(inter_sp--);  /* Throw away the returned result */
} /* call_function() */

/*-------------------------------------------------------------------------*/
int
get_line_number (bytecode_p p, struct program *progp, char **namep)

/* Look up the line number for address <p> within the program <progp>.
 * Result is the line number, and *<namep> is set to the name of the
 * source resp. include file.
 *
 * If the code was generated from an included file, and if the name lengths
 * allow it, the returned name is "<program name> (<include filename>)".
 * In this case, the returned *<namep> points to a static buffer.
 *
 * TODO: (an old comment which might no longer be true): This can be done
 * TODO:: much more efficiently, but that change has low priority.)
 * TODO: The incinfo-handling would profit from a pooled allocator.
 */
{
    /* Datastructure to keep track of included files */
    struct incinfo
    {
        char *name;             /* Name of parent file */
        struct incinfo *super;  /* Pointer to parent entry */
        int super_line;         /* Line number within parent file */
    };

    int offset;            /* (Remaining) program offset to resolve */
    int i;                 /* Current line number */
    char **include_names;  /* Pointer to the array of include file names */
    struct incinfo *inctop = NULL;  /* The include information stack. */
    int relocated_from = 0;
    int relocated_to = -1;
    p_int old_total; 
      /* If the line numbers needed SYSTEM privilege to be swapped in,
       * this value is the old progp->total_size, serving as a flag
       * that the line numbers have to be deallocated again when
       * the function finishes.
       */

    if (!progp || !p)
        return 0;
    
    old_total = 0;

    /* Get the line numbers */
    if (!progp->line_numbers)
    {
        if (!load_line_numbers_from_swap(progp))
        {
            /* Uhhmm, out of memory - try to pull some rank */
          
            int save_privilege;

            old_total = progp->total_size;
            save_privilege = malloc_privilege;
            malloc_privilege = MALLOC_SYSTEM;
            load_line_numbers_from_swap(progp);
            malloc_privilege = save_privilege;
        }
    }

    /* Get the offset within the program */
    offset = (int)(p - progp->program);
    if (p < progp->program || p > PROGRAM_END(*progp))
    {
        printf("get_line_number(): Illegal offset %d in object %s\n"
              , offset, progp->name);
        debug_message("get_line_number(): Illegal offset %d in object %s\n"
                     , offset, progp->name);
        return 0;
    }
    
    include_names = progp->strings + progp->num_strings;

    /* Decode the line number information until the line number
     * for offset is found.
     */
    for (i = 0, p = progp->line_numbers; ; )
    {
        int o;

        o = GET_CODE(p);

        if (o <= 63)  /* 0x00..0x3F */
        {
            if (o >= LI_MAXOFFSET)  /* 0x3c..0x3f */
            {
                if (o != LI_MAXOFFSET)
                {
                    switch (o)
                    {

                    case LI_INCLUDE:
                      {
                        /* Included file: push the information */
                        
                        struct incinfo *inc_new;

                        i++;
                        inc_new = xalloc(sizeof *inc_new);
                        /* TODO: What if this fails? */
                        inc_new->name = *--include_names;
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
        /* TODO: What is this? */

    /* Here, i is the line number, and if inctop is not NULL, the
     * code originates from the included file pointed to by inctop.
     * In either case, set *<namep> to the pointer to the name
     * of the file.
     */
    
    if (inctop)
    {
        /* The code was included */
      
        static char namebuf[80];

        *namep = inctop->name;
        if (strlen(*namep) + strlen(progp->name) < sizeof(namebuf) - 3)
        {
            sprintf(namebuf, "%s (%s)", progp->name, *namep);
            *namep = namebuf;
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
      
        *namep = progp->name;
    }
    
    if (old_total)
    {
        /* We used SYSTEM priviledged memory - now we have to return it.
         */

        xfree(progp->line_numbers);
        total_prog_block_size -= progp->total_size - old_total;
        progp->total_size = old_total;
        reallocate_reserved_areas();
    }

    /* Return the line number */
    return i;
} /* get_line_number() */

/*-------------------------------------------------------------------------*/
int
get_line_number_if_any (char **name)

/* Look up the line number for the current execution address.
 * Result is the line number, and *<namep> is set to the name of the
 * source resp. include file.
 *
 * The function recognizes sefun and lambda closures, the latter return
 * the approximate position offset of the offending instruction within
 * the closure.
 *
 * *<namep> may point to a static buffer.
 */

{
    if (csp >= &control_stack[0] && csp->funstart == SIMUL_EFUN_FUNSTART)
    {
        *name = "<simul_efun closure>";
        return 0;
    }
    
    if (current_prog)
    {
        if (csp->funstart < current_prog->program
         || csp->funstart > PROGRAM_END(*current_prog))
        {
            static char name_buffer[24];

            sprintf(name_buffer, "<lambda 0x%6lx>", (long)csp->funstart);
            *name = name_buffer;
            return inter_pc - csp->funstart - 2;
        }
        return get_line_number(inter_pc, current_prog, name);
    }
    
    *name = "";
    return 0;
}

/*-------------------------------------------------------------------------*/
char *
dump_trace (Bool how
#ifndef TRACE_CODE
                     UNUSED
#endif
           )

/* Write out a traceback, starting from the first frame. If a heart_beat()
 * is involved, return the name of the object that had it.
 *
 * If TRACE_CODE is defined and <how> is true, the last executed
 * instructions are printed, too.
 */

{
#if defined(__MWERKS__) && !defined(TRACE_CODE)
#    pragma unused(how)
#endif
    struct control_stack *p;  /* Control frame under inspection */
    char *ret = NULL;
    bytecode_p pc = inter_pc;
    int line = 0;
    char *name;
    char *file;
    struct object *ob = NULL;
    bytecode_p last_catch = NULL;  /* Last found catch */

    if (!current_prog)
        return NULL;
    
    if (csp < &control_stack[0])
    {
        printf("No trace.\n");
        debug_message("No trace.\n");
        return NULL;
    }

    /* Print the last instructions if required */
#ifdef TRACE_CODE
    if (how) {
#ifdef DEBUG
        (void)last_instructions(60, MY_TRUE, NULL);
        printf("%6lx: %3d %3d %3d %3d %3d %3d %3d %3d\n", (long)pc,
          pc[0], pc[1], pc[2], pc[3], pc[4], pc[5], pc[6], pc[7] );
#else  /* DEBUG */
        last_instructions(20, MY_TRUE, NULL);
#endif /* DEBUG */
    }
#endif /* TRACE_CODE */

    /* Loop through the call stack.
     * The organisation of the control stack results in the information
     * for this frame (p[0]) being stored in the next (p[1]).
     * Confused now? Good.
     */
    p = &control_stack[0];
    do {
        bytecode_p      dump_pc;  /* the frame's pc */
        struct program *prog;     /* the frame's program */
        
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
        }
        else
        {
            dump_pc = p[1].pc;
            prog = p[1].prog;
        }
        
        /* Use some heuristics first to see if it could possibly be a CATCH.
         * The pc should point at a F_END_CATCH instruction, or at a LBRANCH
         * to that instruction.
         */
        if (p > &control_stack[0] && p->funstart == p[-1].funstart)
        {
            bytecode_p pc2 = p->pc;

            if (!pc2)
                goto not_catch;  /* shouldn't happen... */

            if (GET_CODE(pc2) == F_LBRANCH - F_OFFSET)
            {
                short offset;
                pc2++;
                GET_SHORT(offset, pc2);
                if (offset <= 0)
                    goto not_catch;
                pc2 += offset;
            }

#if F_END_CATCH - F_OFFSET >= 0x100
            if (pc2 - FUNCTION_CODE(p->funstart) < 2)
                goto not_catch;
            
            if (GET_CODE(pc2-2) != F_ESCAPE-F_OFFSET
             || GET_CODE(pc2-1) != F_END_CATCH-F_OFFSET-0x100)
            {
                goto not_catch;
            }
#else
            if (pc2 - FUNCTION_CODE(p->funstart) < 1)
                goto not_catch;
            
            if (GET_CODE(pc2-1) != F_END_CATCH-F_OFFSET)
            {
                goto not_catch;
            }
#endif
            if (last_catch == pc2)
                goto not_catch;
            last_catch = pc2;
            name = "CATCH";
            goto name_computed;
        }
        
not_catch:  /* The frame does not point at a catch here */

        /* Efun symbol? */
        if (!prog || !dump_pc)
        {
            /* TODO: See comments in call_lambda(): this code
             * TODO:: should never be reached.
             */
            printf("<function symbol> in '%20s' ('%20s')\n"
                  , ob->prog->name, ob->name);
            debug_message("<function symbol> in '%20s' ('%20s')\n"
                         , ob->prog->name, ob->name);
            continue;
        }
        
        /* simul_efun closure? */
        if (p[0].funstart == SIMUL_EFUN_FUNSTART)
        {
            printf("<simul_efun closure> bound to '%20s' ('%20s')\n",
                ob->prog->name, ob->name);
            debug_message("<simul_efun closure> bound to '%20s' ('%20s')\n",
                ob->prog->name, ob->name);
            continue;
        }
        
        /* Lambda closure? */
        if (p[0].funstart < prog->program
         || p[0].funstart > PROGRAM_END(*prog))
        {
            printf("<lambda 0x%6lx> in '%20s' ('%20s')offset %ld\n",
                (long)p[0].funstart, ob->prog->name, ob->name,
                FUNCTION_FROM_CODE(dump_pc) - p[0].funstart);
            debug_message("<lambda 0x%6lx> in '%20s' ('%20s')offset %ld\n",
                (long)p[0].funstart, ob->prog->name, ob->name,
                FUNCTION_FROM_CODE(dump_pc) - p[0].funstart);
            continue;
        }

        /* Nothing of the above: a normal program */
        line = get_line_number(dump_pc, prog, &file);
        memcpy(&name, &FUNCTION_NAME(p[0].funstart), sizeof name);
        
name_computed: /* Jump target from the catch detection */
        
        /* Print the name and line */

        if (strcmp(name, "heart_beat") == 0 && p != csp)
            ret = p->extern_call ? (p->ob ? p->ob->name : NULL) : ob->name;
        
        printf("'%15s' in '%20s' ('%20s')line %d\n",
                     name, file, ob->name, line);
        debug_message("'%15s' in '%20s' ('%20s')line %d\n",
                     name, file, ob->name, line);
    } while (++p <= csp);

    /* Done */
    return ret;
}

/*-------------------------------------------------------------------------*/
void
invalidate_apply_low_cache (void)

/* Called in the (unlikely) case that all programs had to be renumbered,
 * this invalidates the call cache.
 */

{
    int i;

    for (i = 0; i < CACHE_SIZE; i++)
        cache[i].id = 0;
}

/*-------------------------------------------------------------------------*/
/* add a large amount of eval_cost */
void
add_eval_cost (int num)

/* Add a (large) <num> to eval_cost, with proper regard to tracing.
 *
 * TODO: This function should be used in more places with large data
 * TODO:: traffic.
 */

{
    if (eval_cost < 0)
    {
        /* Normal evaluation accounting */
        eval_cost += num;
        if (eval_cost > 0)
            eval_cost = 0;
        return;
    }
    
    if (eval_cost >= MIN_TRACE_COST && eval_cost < MAX_TRACE_COST)
    {
        /* Special tracing accounting */
        eval_cost += num;
        if (eval_cost - MAX_TRACE_COST >= 0)
            eval_cost = MAX_TRACE_COST;
        return;
    }

    /* Otherwise the eval_cost is already too high */
} /* add_eval_cost() */


#ifdef MALLOC_smalloc

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
            struct object *ob;

            if (NULL != (ob = previous_objects[i])
             && ob->flags & O_DESTRUCTED
             && ob->ref
               )
            {
                ob->ref = 0;
                ob->prog->ref = 0;
                clear_inherit_ref(ob->prog);
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
        if (!cache[i].progp)
            note_malloced_block_ref(cache[i].name);
    }
#ifdef TRACE_CODE
    for (i = TOTAL_TRACE_LENGTH; --i >= 0; )
    {
        struct object *ob;

        if ( NULL != (ob = previous_objects[i]) )
        {
            if (ob->flags & O_DESTRUCTED)
            {
                previous_objects[i] = 0;
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

#endif /* MALLOC_smalloc */
/*=========================================================================*/

/*                            D E B U G G I N G                            */

/*-------------------------------------------------------------------------*/
/* test stuff ... -- LA */
#ifdef OPCPROF
void
opcdump (void)

/* Print the usage statistics for the opcodes.
 * Called from the command parser for the "opcdump" command.
 */

{
    int i;

    for(i = 0; i < MAXOPC; i++)
        if (opcount[i])
#ifdef VERBOSE_OPCPROF
            printf("%d: \"%-16s\" %6d\n",i+F_OFFSET,get_f_name(i), opcount[i]);
#else
            printf("%d: %d\n", i+F_OFFSET, opcount[i]);
#endif
    fflush(stdout);
}
#endif


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
    static char buff[10];
    bytecode_p from, to;
    int b;
    
    b = (a+1) % TOTAL_TRACE_LENGTH;
    from = previous_pc[a];
    to = previous_pc[b];
    
    if (to - from < 2)
        return "";
    
    if (to - from == 2)
    {
        sprintf(buff, "%d", GET_CODE(from+1));
        return buff;
    }
    
    if (to - from == 3)
    {
        short arg;

        GET_SHORT(arg, from+1);
        sprintf(buff, "%d", arg);
        return buff;
    }
    
    if (to - from == 5)
    {
        int32 arg;

        GET_INT32(arg, from+1);
        ((char *)&arg)[0] = from[1];
        ((char *)&arg)[1] = from[2];
        ((char *)&arg)[2] = from[3];
        ((char *)&arg)[3] = from[4];
        sprintf(buff, "%ld", (long)arg);
        return buff;
    }

    return "";
}

/*-------------------------------------------------------------------------*/
static void
last_instr_output (char *str, struct svalue **svpp)

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
        if ( !(str = string_copy(str)) )
            error("Out of memory\n");
        (*svpp)->type = T_STRING;
        (*svpp)->x.string_type = STRING_MALLOC;
        (*svpp)->u.string = str;
        (*svpp)++;
    }
    else
    {
        printf("%s\n", str);
    }
}

/*-------------------------------------------------------------------------*/
static Bool
program_referenced (struct program *prog, struct program *prog2)

/* Return TRUE if <prog2> inherits <prog>.
 *
 * Auxiliary function to last_instructions().
 */

{
    struct inherit *inh;
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
program_exists (struct program *prog, struct object *guess)

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
last_instructions (int length, Bool verbose, struct svalue **svpp)

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
    struct object *old_obj;
    char old_file[160], buf[400];
    int old_line, line = 0;

    old_obj = NULL;
    old_file[0] = old_file[sizeof old_file - 1] = '\0';
    old_line = 0;
    i = (last - length + TOTAL_TRACE_LENGTH) % TOTAL_TRACE_LENGTH;

    /* Walk through the instructions */
    do {
        i = (i + 1) % TOTAL_TRACE_LENGTH;
        if (previous_instruction[i] != 0)
        {
            if (verbose)
            {
                char *file;
                struct program *ppr;
                bytecode_p ppc;

                ppr = previous_programs[i];
                ppc = previous_pc[i]+1;
                if (!program_exists(ppr, previous_objects[i]))
                {
                    file = "program deallocated";
                    line = 0;
                }
                else if (ppc < ppr->program || ppc > PROGRAM_END(*ppr))
                {
                    file = "<lambda ???>";
                    line = 0;
                }
                else
                {
                    line = get_line_number(ppc, ppr, &file);
                }
                
                if (previous_objects[i] != old_obj || strcmp(file, old_file))
                {
                    sprintf(buf, "%.170s %.160s line %d",
                      previous_objects[i]->name, file, line
                    );
                    last_instr_output(buf, svpp);
                    old_obj = previous_objects[i];
                    strncpy(old_file, file, sizeof old_file - 1);
                }
            }
            sprintf(buf, "%6lx: %3d %8s %-25s (%d)", (long)previous_pc[i],
                   previous_instruction[i], /* instrs.h has these numbers */
                   get_arg(i),
                   get_f_name(previous_instruction[i]),
                   stack_size[i] + 1
            );
            if (verbose && line != old_line)
                sprintf(buf + strlen(buf), "\tline %d", old_line = line);
            last_instr_output(buf, svpp);
        }
    } while (i != last);
    
    return last;
} /* last_instructions() */

/*-------------------------------------------------------------------------*/
struct svalue *
f_last_instructions (struct svalue *sp)

/* TEFUN last_instructions()
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
    struct vector *v, *v2;
    mp_int num_instr, size;
    struct svalue *svp;

    /* Test the arguments */
    if (sp[-1].type != T_NUMBER || (num_instr = sp[-1].u.number) <= 0)
        bad_xefun_arg(1, sp);
    if (sp->type != T_NUMBER)
        bad_xefun_arg(2, sp);
    
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
    sp->type = T_POINTER;
    sp->u.vec = v;
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


#ifdef DEBUG

/*-------------------------------------------------------------------------*/
void
count_inherits (struct program *progp)

/* Check Refcounts: Increment the extra_ref of all programs inherited
 * by <progp>. If one of those programs has not been visited yet,
 * its extra_ref is set to 1 and this function is called recursively.
 *
 * If check_..._search_prog is set and equal to one of the inherited
 * programs, a notice is printed.
 */

{
    int i;
    struct program *progp2;

    /* Clones will not add to the ref count of inherited progs */
    for (i = 0; i < progp->num_inherited; i++)
    {
        progp2 = progp->inherit[i].prog;
        progp2->extra_ref++;
        if (progp2 == check_a_lot_ref_counts_search_prog)
            printf("Found prog, inherited by %s, new total ref %ld\n",
              progp->name, progp2->ref);
        if (NULL == register_pointer(ptable, progp2))
            continue;
        progp2->extra_ref = 1;
        count_inherits(progp2);
    }
} /* count_inherits() */

/*-------------------------------------------------------------------------*/
static void
count_extra_ref_in_mapping_filter ( struct svalue *key, struct svalue *data
                                  , void * extra)

/* Count the extra refs for <key> and the associated <data>. <extra>
 * is a mp_int giving the number of data values.
 */

{
    count_extra_ref_in_vector(key, 1);
    count_extra_ref_in_vector(data, (mp_int)extra);
}

/*-------------------------------------------------------------------------*/
static void
check_extra_ref_in_mapping_filter (struct svalue *key, struct svalue *data
                                  , void * extra)

/* Check the extra refs for <key> and the associated <data>. <extra>
 * is a mp_int giving the number of data values.
 */

{
    check_extra_ref_in_vector(key, 1);
    check_extra_ref_in_vector(data, (mp_int)extra);
}

/*-------------------------------------------------------------------------*/
void
count_extra_ref_in_object (struct object *ob)

/* Count the extra refs for object <ob>. If the object has been visited
 * before, extra_ref is just incremented. Otherwise, extra_ref is
 * set to 1 and all depending refs are counted.
 *
 * If check_..._search_prog is set and matches the objects program,
 * a notice is printed.
 */

{
    ob->extra_ref++;
    if ( NULL == register_pointer(ptable, ob) )
        return;
    
    ob->extra_ref = 1;
    if ( !O_PROG_SWAPPED(ob) )
    {
        ob->prog->extra_ref++;
        if (ob->prog == check_a_lot_ref_counts_search_prog)
            printf("Found program for object %s\n", ob->name);
    }
    
    /* Clones will not add to the ref count of inherited progs */
    if (O_PROG_SWAPPED(ob))
    {
        /* hmmm, what are we going to do here?
           we could swap in the object, but this would make debugging
           of swapping rather unrealistic.
           At any rate, doing nothing is probably better then referencing
           a pointer to a freed memory block... unless you can guarantee
           that freed blocks are never reused again...
           ... and the pointer is lost, anyway.
        */
        NOOP
    }
    else
    {
        if (NULL == register_pointer(ptable, ob->prog))
            return;
        ob->prog->extra_ref = 1;
        count_inherits(ob->prog);
    }
    
    if (ob->flags & O_SHADOW)
    {
        struct ed_buffer *buf;

        if ( NULL != (buf = O_GET_SHADOW(ob)->ed_buffer) )
            count_ed_buffer_extra_refs(buf);
    }
} /* count_extra_ref_in_closure() */

/*-------------------------------------------------------------------------*/
static void
count_extra_ref_in_closure (struct lambda *l, ph_int type)

/* Count the extra refs in the closure <l> of type <type>.
 */
  
{
    if (CLOSURE_HAS_CODE(type))
    {
        /* We need to count the extra_refs in the constant values. */
      
        mp_int num_values;
        struct svalue *svp;

        svp = (struct svalue *)l;
        if ( (num_values = EXTRACT_UCHAR(l->function.code)) == 0xff)
            num_values = svp[-0xff].u.number;
        svp -= num_values;
        count_extra_ref_in_vector(svp, num_values);
    }
    else
    {
        /* Count the referenced closures and objects */
        if (type == CLOSURE_BOUND_LAMBDA)
        {
            struct lambda *l2 = l->function.lambda;

            if (NULL != register_pointer(ptable, l2) )
                count_extra_ref_in_closure(l2, CLOSURE_UNBOUND_LAMBDA);
        }
        else if (type == CLOSURE_ALIEN_LFUN)
        {
            count_extra_ref_in_object(l->function.alien.ob);
        }
    }
    
    if (type != CLOSURE_UNBOUND_LAMBDA)
        count_extra_ref_in_object(l->ob);
} /* count_extra_ref_in_closure() */

/*-------------------------------------------------------------------------*/
void
count_extra_ref_in_vector (struct svalue *svp, mp_int num)

/* Count the extra_refs of all <num> values starting at <svp>.
 */

{
    struct svalue *p;

    for (p = svp; p < svp+num; p++)
    {
        switch(p->type)
        {

        case T_CLOSURE:
            if (CLOSURE_MALLOCED(p->x.closure_type))
            {
                struct lambda *l;

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
check_extra_ref_in_vector (struct svalue *svp, mp_int num)

/* Check the extra_refs of the <num> values starting at <svp>
 */
  
{
    struct svalue *p;

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
check_a_lot_ref_counts (struct program *search_prog)

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
 * TODO: Is the process still correct?
 * TODO: Put this code somewhere else.
 */

{
    struct object *ob;
    int i;

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
        debug_message("Out of memory while checking all refcounts.\n");
        return;
    }
    
    /* List of all objects.
     */
    for (ob = obj_list; ob; ob = ob->next_all)
    {
        if (ob->flags & O_DESTRUCTED)
        {
            /* This shouldn't happen */
            debug_message("Found destructed object '%s' where it shouldn't "
                          "be.\n", ob->name);
            continue;
        }
        if (O_VAR_SWAPPED(ob))
            load_ob_from_swap(ob);
        count_extra_ref_in_vector(ob->variables, ob->extra_num_variables);
        count_extra_ref_in_object(ob);
    }
    
    if (master_ob)
        master_ob->extra_ref++;
    
    if (d_flag > 3)
    {
        debug_message("obj_list evaluated\n");
    }

    /* The current stack.
     */
    count_extra_ref_in_vector(start_of_stack, inter_sp - start_of_stack + 1);
    if (d_flag > 3)
    {
        debug_message("stack evaluated\n");
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
                count_extra_ref_in_object(ob);
        }
    }
#endif

    count_extra_ref_in_vector(&indexing_quickfix, 1);
    count_extra_ref_in_vector(&last_indexing_protector, 1);
    null_vector.extra_ref++;
    
    /* To cound the closure hooks properly, we have to fiddle the
     * types a bit.
     */
    for (i = NUM_CLOSURE_HOOKS; --i >= 0; )
    {
        if (closure_hook[i].type == T_CLOSURE
         && closure_hook[i].x.closure_type == CLOSURE_LAMBDA)
        {
            closure_hook[i].x.closure_type = CLOSURE_UNBOUND_LAMBDA;
        }
    }
    
    count_extra_ref_in_vector(closure_hook, NUM_CLOSURE_HOOKS);
    
    /* Undo the type fiddling.
     */
    for (i = NUM_CLOSURE_HOOKS; --i >= 0; )
    {
        if (closure_hook[i].type == T_CLOSURE
         && closure_hook[i].x.closure_type == CLOSURE_UNBOUND_LAMBDA)
        {
            closure_hook[i].x.closure_type = CLOSURE_LAMBDA;
        }
    }
    
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
        debug_message("Out of memory while checking all refcounts.\n");
        return;
    }

    for (ob=obj_list; ob; ob = ob->next_all) {
        if (ob->flags & O_DESTRUCTED)  /* shouldn't happen */
            continue;
        
        if (ob->ref != ob->extra_ref)
             fatal("Bad ref count in object %s, %ld - %ld\n", ob->name,
                  ob->ref, ob->extra_ref);

        if ( !(ob->flags & O_SWAPPED) )
        {
            if (ob->prog->ref != ob->prog->extra_ref)
            {
                /* an inheriting file might be swapped */
                if (TIME_TO_SWAP > 0 && time_to_swap + 1 > 0
                 && ob->prog->ref > ob->prog->extra_ref)
                {
                    debug_message("high ref count in prog %s, %ld - %ld\n",
                        ob->prog->name, ob->prog->ref, ob->prog->extra_ref);
                }
                else
                {
                    check_a_lot_ref_counts(ob->prog);
                    fatal("Bad ref count in prog %s, %ld - %ld\n",
                        ob->prog->name, ob->prog->ref, ob->prog->extra_ref);
                }
            }
        } /* !SWAPPED */
        check_extra_ref_in_vector(ob->variables, ob->extra_num_variables);
    } /* for */

    check_extra_ref_in_vector(start_of_stack, inter_sp - start_of_stack + 1);

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
#define ERROR(s) {inter_sp = sp; error(s);}

#undef TYPE_TEST1
#define TYPE_TEST1(arg1, type1, instruction) {\
    if ((arg1)->type != (type1)) {\
        bad_efun_arg(1, (instruction)-F_OFFSET, sp);\
    }\
}

#undef TYPE_TEST2
#define TYPE_TEST2(arg1, type2, instruction) {\
    if ((arg1)->type != (type2)) {\
        bad_efun_arg(2, (instruction)-F_OFFSET, sp);\
    }\
}

/*-------------------------------------------------------------------------*/
struct svalue *
f_trace (struct svalue *sp)

/* TEFUN trace()
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
 * The master-lfun query_player_level() is called to verify the
 * usage of this efun.
 */

{
    int ot;
    struct interactive *ip;

    TYPE_TEST1(sp, T_NUMBER, F_TRACE)
    ot = -1;

    /* If the command_giver is allowed to do so... */
    if (command_giver
     && NULL != (ip = O_GET_INTERACTIVE(command_giver))
     && ip->sent.type == SENT_INTERACTIVE)
    {
        struct svalue *arg;

        assign_eval_cost();
        inter_sp = _push_volatile_string("trace", sp);
        arg = apply_master_ob(STR_PLAYER_LEVEL, 1);
        if (!arg)
        {
            if (out_of_memory)
                error("Out of memory\n");
        }
        else
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
    (void)SET_TRACE_EXEC;
    return sp;
} /* f_trace() */

/*-------------------------------------------------------------------------*/
struct svalue *
f_traceprefix (struct svalue *sp)

/* TEFUN traceprefix()
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
 * The master-lfun query_player_level() is called to verify the usage of this
 * efun.
 */

{
    char *old;
    struct interactive *ip;

    if (sp->type != T_STRING && sp->type != T_NUMBER)
        bad_xefun_arg(1, sp);

    old = 0;

    /* If the command_giver is allowed to do that... */
    if (command_giver
     && NULL != (ip = O_GET_INTERACTIVE(command_giver))
     && ip->sent.type == SENT_INTERACTIVE)
    {
        struct svalue *arg;
        
        inter_sp = _push_volatile_string("trace", sp);
        assign_eval_cost();
        arg = apply_master_ob(STR_PLAYER_LEVEL,1);
        if (!arg)
        {
            if (out_of_memory)
                error("Out of memory\n");
        }
        else
        {
            /* ... then so shall it be */
            if (arg && (arg->type != T_NUMBER || arg->u.number))
            {
                old = ip->trace_prefix;
                if (sp->type == T_STRING)
                {
                    ip->trace_prefix = make_shared_string(sp->u.string);
                }
                else
                    ip->trace_prefix = NULL;
            }
        }
    }
    
    free_svalue(sp);
    
    /* Return the old prefix */
    if (old)
    {
        sp->type = T_STRING;
        sp->x.string_type = STRING_SHARED;
        sp->u.string = old;
    }
    else
    {
        put_number(0);
    }
    return sp;
} /* f_trace() */

/***************************************************************************/
