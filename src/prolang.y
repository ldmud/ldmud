%{
%line
/*---------------------------------------------------------------------------
 * LPC compiler
 *
 *---------------------------------------------------------------------------
 * TODO: Some code parts 'know' which instructions are xcodes and which normal.
 * TODO:: Conditional compiles would be nice there.
 * TODO: The handling of virtual inherits is a bit vague, too.
 *
 * This is the grammar definition and bytecode compiler of LPC. However, this
 * file is not passed to byacc directly, but first preprocessed by make_func,
 * among other things to synchronise the tokens with the other bytecodes
 * (reason being that yacc doesn't know an include construct). The following
 * keywords are recognized and replaced by make_func:
 *
 *    %line:  generates a #line statement to synchronize the C compiler.
 *
 *    %typemap TYPE<name>:<value>,...,TYPE_<name>:<value>
 *       Generates a lookup table TYPE_<name> -> <value>. Unspecified
 *       TYPEs are given the value 0.
 *
 *    %hookmap <hookname>:<value>,...,<hookname>:<value>
 *       Generates a lookup table <hookname> -> <value>. Unspecified
 *       driverhook entries are given the value 0.
 *
 * In addition, make_func implements a simple preprocessor using the
 * keywords %if, %elif, %else and %endif so that parsing rules can be
 * activated/deactivated from config.h defines.
 *---------------------------------------------------------------------------
 * To compile a file, open the file  to yield a filedescriptor 'fd', then call
 *
 *     compile_file(fd, <filename>, <isMasterObj>);
 *
 * then close the file again. The compiled program is 'returned' in
 * the global compiled_prog - on error, this variable is returned as NULL.
 * If after the compilation the variable inherit_file is
 * not NULL, the compilation failed because it encountered an
 * "inherit 'name'" statement for which no object could be found: the
 * 'name' was stored in inherit_file and has to be compiled first.
 *
 * It is the task of the caller to make sure that the compiler is not called
 * recursively.
 *
 * If there is any initialization of a global variable, a function '__INIT'
 * is generated with the initialization code. The code is generated in
 * fragments whenever a variable initialization is encountered; the fragments
 * are therefore potentially spread over the whole program code. The fragments
 * are linked by JUMP instructions with jump to the next fragment, just
 * the last fragment ends in a RETURN0.
 *
 * When inheriting from another object, a call will automatically be made
 * to call __INIT in that code from the current __INIT.
 *---------------------------------------------------------------------------
 * The compiler is a simple one-pass compiler with immediate code generation.
 * The problem of forward references is solved with various backpatching
 * structures (explained where declared).
 *
 * The most tricky part is that of lvalue (and with it reference) generation
 * in contexts where rvalues are sensible as well. This is so especially
 * because the order of arguments on the stack differs between the
 * instructions :-(. The approach is to generate rvalues, but keep the
 * position, and size and alternatives of the instruction(s) in a struct
 * lrvalue, so that a later change into lvalues is possible. Additionally
 * these instructions can be modified to generated protected lvalues as well.
 * TODO: This whole thing is quite complex and not very well documented.
 * TODO:: It's propably easier to rewrite interpreter and compiler...
 *
 * Another challenge is the compilation of inline closures, as they
 * are implemented as separate lfuns (with synthetic names), but encountered
 * in the middle of a regular lfun or even another inline closure. The
 * compiler therefore stores his state when it recognizes another
 * inline closures, and then resets its state as if a normal lfun is
 * compiled. When the inline closure is complete, its data is moved into
 * a backup storage area, and the compiler restores its previous state.
 *---------------------------------------------------------------------------
 */

#undef lint  /* undef so that precompiled headers can be used */

#include "driver.h"
#include "typedefs.h"

#include "my-alloca.h"
#include <ctype.h>
#include <stdio.h>
#include <stdarg.h>
#include <assert.h>

#include "prolang.h"

#include "array.h"
#include "backend.h"
#include "closure.h"
#include "exec.h"
#include "gcollect.h"
#include "interpret.h"
#include "instrs.h"
#include "lex.h"
#include "main.h"
#include "mapping.h"
#include "mstrings.h"
#include "object.h"
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

#undef DEBUG_INLINES
  /* Define this to activate lots of debugging output during the compilation
   * of inline closures.
   */

#define lint  /* redef again to prevent spurious warnings */

#define YYMAXDEPTH        600

/*-------------------------------------------------------------------------*/

typedef struct block_scope_s       block_scope_t;
typedef struct const_list_s        const_list_t;
typedef struct const_list_svalue_s const_list_svalue_t;
typedef struct struct_init_s       struct_init_t;
typedef struct efun_shadow_s       efun_shadow_t;
typedef struct mem_block_s         mem_block_t;

/*-------------------------------------------------------------------------*/
/* Exported result variables */

int32 current_id_number = 0;
  /* The id-number of the compiled program.
   */

int num_virtual_variables;
  /* Number of virtual variables.
   * When creating the bytecode, the non-virtual variable indices
   * are offset by this value, in effect collecting the virtual
   * variables at the start of the variable indices.
   */

program_t *compiled_prog;
  /* After yyparse(), the finished program.
   */

string_t *inherit_file;
  /* Used as a flag: if it is set to a tabled string after yyparse(),
   * this string should be loaded as an object, and the original object
   * must be loaded again.
   */

int num_parse_error;
  /* Number of errors in the compile.
   */

Bool variables_defined;
  /* TRUE: Variable definitions have been encountered.
   */

/*-------------------------------------------------------------------------*/
/* Table which hook may be of which type.
 * It is here because make_func has to touch this file anyway, but
 * it will be used by simulate:f_set_driver_hook().
 */

#define SH(x) - -(1 << (x))

short hook_type_map[NUM_DRIVER_HOOKS] =
%hookmap \
    H_MOVE_OBJECT0: 0, \
    H_MOVE_OBJECT1: 0, \
    H_LOAD_UIDS:      SH(T_CLOSURE), \
    H_CLONE_UIDS:     SH(T_CLOSURE), \
    H_CREATE_SUPER:                 SH(T_STRING), \
    H_CREATE_OB:                    SH(T_STRING), \
    H_CREATE_CLONE:                 SH(T_STRING), \
    H_RESET:                        SH(T_STRING), \
    H_CLEAN_UP:       SH(T_CLOSURE) SH(T_STRING), \
    H_MODIFY_COMMAND: SH(T_CLOSURE) SH(T_STRING) SH(T_MAPPING), \
    H_NOTIFY_FAIL:    SH(T_CLOSURE) SH(T_STRING), \
    H_NO_IPC_SLOT:                  SH(T_STRING), \
    H_INCLUDE_DIRS:   SH(T_CLOSURE)              SH(T_POINTER), \
    H_TELNET_NEG:     SH(T_CLOSURE) SH(T_STRING), \
    H_NOECHO:         SH(T_CLOSURE) SH(T_STRING), \
    H_ERQ_STOP:       SH(T_CLOSURE), \
    H_MODIFY_COMMAND_FNAME: SH(T_STRING), \
    H_COMMAND:        SH(T_CLOSURE) SH(T_STRING), \
    H_SEND_NOTIFY_FAIL: SH(T_CLOSURE) SH(T_STRING), \
    H_AUTO_INCLUDE:   SH(T_CLOSURE) SH(T_STRING), \
    H_DEFAULT_METHOD: SH(T_CLOSURE) SH(T_STRING), \
    H_DEFAULT_PROMPT: SH(T_CLOSURE) SH(T_STRING), \
    H_PRINT_PROMPT:   SH(T_CLOSURE) SH(T_STRING), \
    H_REGEXP_PACKAGE: SH(T_NUMBER), \
    H_MSG_DISCARDED:  SH(T_CLOSURE) SH(T_STRING), \

#undef SH

/*-------------------------------------------------------------------------*/
/* Types */

/* --- struct const_list_s: One element in a constant list ---
 *
 * When initializing variables statically with arrays ({ ... }),
 * a list of these structures is used to collect the information about
 * the array content.
 */

struct const_list_s
{
    const_list_t *next;
    svalue_t val;
    string_t * member; /* NULL, or the member name to initialize */
};

/* --- struct const_list_svalue_s: Head of a constant list ---
 *
 * When initializing variables statically with arrays ({ ... }),
 * the initializer-svalue_t* will point to an instance of this c_l_svalue_s.
 * In fact, the initializer points to the .head member.
 *
 * The .head svalue_t is a T_ERROR_HANDLER pointing to a deallocation
 * function for the list.
 */

struct const_list_svalue_s
{
    svalue_t     head;  /* the error handler */
    const_list_t list;  /* First element of the list */
    const_list_t *last_member; /* For nested struct initialisations */
};

/* --- struct struct_init_s: Descriptor for one struct literal member
 *
 * When createing struct literals at runtime, a list of these structures
 * keeps the information about the order and type of members encountered.
 */

struct struct_init_s
{
    struct_init_t * next;  /* Next member entry */
    vartype_t       type;  /* Type of expression */
    string_t      * name;  /* Member name, or NULL if unnamed */
};

/* --- struct efun_shadow_s: Store info about masked efuns ---
 *
 * This structure is used when global identifiers shadow efun names.
 */

struct efun_shadow_s
{
    efun_shadow_t *next;    /* Linkpointer for the list of shadows */
    ident_t       *shadow;  /* Identifier of the shadow */
};


/*-------------------------------------------------------------------------*/
/* Macros */

#define NON_VIRTUAL_OFFSET_TAG 0x4000
   /* Tag or'ed on inherit.variable_index_offset for non-virtual
    * inherits for the duration of the compilation.
    * The variable_index_offsets of such marked variables do not
    * yet the the num_virtual_variables offset into account.
    */

#define align(x) (((x) + (sizeof(char*)-1) ) & ~(sizeof(char*)-1) )

#define defined_function(s) \
    ((s)->type == I_TYPE_GLOBAL ? (s)->u.global.function : -1)
  /* Return the index of the function <s> if global (and therefore existing),
   * and -1 otherwise.
   */

#define set_vartype(t, id, ptr) \
    ( t.type = (id), t.t_struct = (ptr) )
#define set_fulltype(t, id, ptr) \
    ( t.typeflags = (id), t.t_struct = (ptr) )
  /* Set the full/vartype <t> to type(flags) <id> and the struct
   * typeobject pointer to <ptr>.
   */

#define NEW_INHERITED_INDEX (0xfffff)
  /* While inserting a new inherit, this marks the newly inherited
   * things.
   */

/* Values for %type <number> foreach_expr
 */
#define FOREACH_LOOP  0  /* Normal foreach loop value */
#define FOREACH_REF   1  /* Referenced foreach loop value */
#define FOREACH_RANGE 2  /* Integer range as loop value */

/*-------------------------------------------------------------------------*/
/* The generated information (code and otherwise) is kept in several
 * memory areas, each of which can grow dynamically and independent
 * from the others.
 *
 * The first NUMPAREAS are save with the program code after compilation,
 * the others are of internal use for the compiler only.
 */

enum e_saved_areas {
   A_PROGRAM = 0
    /* (bytecode_t): Program code.
     */
 , A_STRINGS
    /* (string_t*) Strings used by the program, all tabled.
     */
 , A_VARIABLES
    /* (variable_t) The information for all non-virtual variables.
     */
 , A_VIRTUAL_VAR
    /* (variable_t) The information for all virtual variables.
     */
 , A_LINENUMBERS
    /* (char) The linenumber information.
     */
 , A_INHERITS
    /* (inherit_t) The information for the inherited programs.
     */
 , A_ARGUMENT_TYPES
    /* (vartype_t) Types of the arguments of all functions with
     * typechecking. The argument types for a specific function
     * can be found using the ARGUMENT_INDEX
     */
 , A_ARGUMENT_INDEX
    /* (unsigned short) Index of the first argument type of function <n>.
     * INDEX_START_NONE is used for functions with no type information.
     */

 , A_INCLUDES
    /* (include_t) Tabled descriptors of all included files, in the order
     * of appearance.
     */

 , A_STRUCT_DEFS
    /* (struct_def_t) Tabled descriptors of all struct definitions.
     */

 , NUMPAREAS  /* Number of saved areas */
};

enum e_internal_areas {
   A_FUNCTIONS = NUMPAREAS
     /* (function_t): Function definitions
      */

 , A_STRING_NEXT
   /* (int) During compilation, the strings in A_STRINGS are organized
    * in a hash table (prog_string_indizes/_tags). The hash chains are
    * linked together using the indizes in this area. The end of
    * a chain is marked by a negative next-index.
    */

 , A_LOCAL_TYPES
   /* (fulltype_t) The full types of local and context variables.
    * For normal functions, only the beginning of the area is used.
    * The rest is used stack-wise for nested inline closures.
    */

 , A_INLINE_PROGRAM
    /* (bytecode_t, char): Program and linenumbers saved from the compiled
     * but not yet inserted inline closures.
     */
 , A_INLINE_CLOSURE
    /* (inline_closure_t): The currently pending inline closures. The lexical
     * nesting is achieved with the .prev/.next pointers in the
     * inline_closure_t structures.
     */

 , A_STRUCT_MEMBERS
    /* (struct_member_t) While a struct definition is parsed, the member
     * descriptors are collected here.
     */

 , NUMAREAS  /* Total number of areas */
};


/* --- struct mem_block_s: One memory area ---
 * Every mem_block keeps one memory area. As it grows by using realloc(),
 * no pointers should be kept into such an area (offsets are ok).
 */

struct mem_block_s
{
    char    *block;         /* Pointer to the allocated memory */
    mp_uint  current_size;  /* Used size of the mem_block */
    mp_uint  max_size;      /* Allocated size of the mem_block */
};


#define START_BLOCK_SIZE  2048
  /* Initial size of an area/mem_block.
   */

static mem_block_t mem_block[NUMAREAS];
  /* All memory areas.
   */


#define PROGRAM_BLOCK ((bytecode_p)(mem_block[A_PROGRAM].block))
  /* The current program block, properly typed.
   */

#define CURRENT_PROGRAM_SIZE (mem_block[A_PROGRAM].current_size)
  /* The current program size.
   */


#define LINENUMBER_BLOCK ((char *)(mem_block[A_LINENUMBERS].block))
  /* The current linenumber block, properly typed.
   */

#define LINENUMBER_SIZE (mem_block[A_LINENUMBERS].current_size)
  /* The current linenumber data size.
   */


#define FUNCTION(n) ((function_t *)mem_block[A_FUNCTIONS].block + (n))
  /* Return the function_t* for function number <n>.
   */

#define FUNCTION_COUNT (mem_block[A_FUNCTIONS].current_size / sizeof (function_t))
  /* Number of function_t stored so far in A_FUNCTIONS.
   */


#define INHERIT_COUNT (mem_block[A_INHERITS].current_size / sizeof(inherit_t))
  /* Number of inherit_t stored so far in A_INHERITS.
   */


#define ARGUMENT_INDEX(n) ((unsigned short *)mem_block[A_ARGUMENT_INDEX].block)[n]
  /* Lookup the start index of the types for function number <n>.
   */


#define ARGTYPE_COUNT (mem_block[A_ARGUMENT_TYPES].current_size / sizeof(vartype_t))
  /* Number of vartype_t stored so far in A_ARGUMENT_TYPES.
   */

#define ARGUMENT_TYPE(n)  ((vartype_t *)mem_block[A_ARGUMENT_TYPES].block)[n]
  /* Index the vartype_t <n>.
   */


#define NV_VARIABLE(n) ((variable_t *)mem_block[A_VARIABLES].block + (n))
  /* Return the variable_t* for the non-virtual variable <n>.
   */

#define NV_VARIABLE_COUNT (mem_block[A_VARIABLES].current_size / sizeof(variable_t))
#define V_VARIABLE_COUNT  (mem_block[A_VIRTUAL_VAR].current_size / sizeof(variable_t))
  /* Number of variables stored so var in A_VARIABLES resp. A_VIRTUAL_VAR.
   */

#define V_VARIABLE(n)  ((variable_t *)mem_block[A_VIRTUAL_VAR].block + \
                        (n) - VIRTUAL_VAR_TAG)
  /* Return the variable_t* for the virtual variable <n> (still including
   * the offset).
   */

#define VARIABLE(n) ((n) & VIRTUAL_VAR_TAG ? V_VARIABLE(n) : NV_VARIABLE(n))
  /* Return the variable_t* for the variable <n>, virtual or not.
   */

#define INHERIT(n)     ((inherit_t *)mem_block[A_INHERITS].block)[n]
  /* Index the inherit_t <n>.
   */

#define INHERIT_COUNT  (mem_block[A_INHERITS].current_size / sizeof(inherit_t))
  /* Return the number of inherits encountered so far.
   */

#define STRUCT_DEF(n)     ((struct_def_t *)mem_block[A_STRUCT_DEFS].block)[n]
  /* Index the struct_def_t <n>.
   */

#define STRUCT_COUNT  (mem_block[A_STRUCT_DEFS].current_size / sizeof(struct_def_t))
  /* Return the number of structs encountered so far.
   */

#define STRUCT_MEMBER(n)   ((struct_member_t *)mem_block[A_STRUCT_MEMBERS].block)[n]
  /* Index the struct_member_t <n>.
   */

#define STRUCT_MEMBER_COUNT (mem_block[A_STRUCT_MEMBERS].current_size / sizeof(struct_member_t))
  /* Return the number of struct members stored.
   */

#define PROG_STRING(n) ((string_t **)mem_block[A_STRINGS].block)[n]
  /* Index the pointer for program string <n>.
   */

#define STRING_COUNT  (mem_block[A_STRINGS].current_size / sizeof(string_t *))
  /* Return the number of program strings encountered so far.
   */

#define PROG_STRING_NEXT(n) ((int *)mem_block[A_STRING_NEXT].block)[n]
  /* Index the chain-index for program string <n>.
   */

#define INCLUDE_COUNT  (mem_block[A_INCLUDES].current_size / sizeof(include_t))
  /* Return the total number of include files encountered so far.
   */

#define LOCAL_TYPE_COUNT  (mem_block[A_LOCAL_TYPES].current_size / sizeof(fulltype_t))
  /* Return the total number of types.
   */

#define LOCAL_TYPE(n) ((fulltype_t *)mem_block[A_LOCAL_TYPES].block)[n]
  /* Return the local/context var type at index <n>.
   */

#define INLINE_PROGRAM_BLOCK(n) ((bytecode_p)(mem_block[A_INLINE_PROGRAM].block + (n)))
  /* Return the inline-closure program block at address <n>, properly typed.
   */

#define INLINE_PROGRAM_SIZE (mem_block[A_INLINE_PROGRAM].current_size)
  /* The current program size.
   */

#define INLINE_CLOSURE(n) ((inline_closure_t *)mem_block[A_INLINE_CLOSURE].block)[n]
  /* Return the inline-closure program block at address <n>, properly typed.
   */

#define INLINE_CLOSURE_COUNT  (mem_block[A_INLINE_CLOSURE].current_size/sizeof(inline_closure_t))
  /* Return the number of saved inline-closures.
   */

#if MAX_LOCAL > 256
  /* There are only 8 bit opcodes for accessing
   * local variables.
   */
#  error "Maximum number of local variables too high."
#endif

#define CONTEXT_VARIABLE_BASE 256
  /* Indicates context variables in ident->u.local.num
   * when deriving new context variables from them.
   * Should be greater or equal to MAX_LOCAL.
   */



/*-------------------------------------------------------------------------*/
/* Information describing nested local blocks (scopes).
 */
struct block_scope_s
{
    int     first_local;  /* Number of first local defined in this scope */
    int     num_locals;   /* Number of locals defined in this scope */
    int     num_cleared;
      /* Number of locals that have been cleared by earlier CLEAR_LOCALS */
    Bool    clobbered;
      /* Local variables beyond num_locals may be clobbered */
    mp_uint addr;
      /* Address of CLEAR_LOCALS instruction, needed for backpatching */
    Bool    accessible;   /* True if variables of this block are accessible */
};

static block_scope_t block_scope[COMPILER_STACK_SIZE];
  /* A static stack of block scopes, indexed by <block_depth>-1.
   * TODO: This should be dynamic.
   */

static int block_depth;
  /* The nesting depth of blocks ( '{ ... }' ), used to distinguish
   * local variable definitions.
   * block_depth = 0: not used, would mean 'global'
   *             = 1: function arguments
   *             = 2: function local variables
   *             > 2: vars of nested blocks within the function
   */

/*-------------------------------------------------------------------------*/
/* Information describing inline closures.
*/

typedef struct inline_closure_s inline_closure_t;

struct inline_closure_s
{
    mp_int prev;
      /* Index of the enclosing inline closure, or -1 if none.
       */

    mp_int next;
      /* Index of an enclosed inline closure.
       * This is only used temporarily.
       */

    /* --- Compilation information --- */
    mp_uint end;
      /* While compiling the closure: end of the program code before
       * the closure. It is not identical to .start because of alignment.
       */
    mp_uint start;
      /* While compiling the closure: start address of the code in A_PROGRAM.
       * For pending closures: start address of the code in A_INLINE_PROGRAM.
       */
    mp_uint length;
      /* Length of the compiled code.
       */
    mp_uint li_start;
      /* While compiling the closure: start address of the data in
       * A_LINENUMBERS.
       * For pending closures: start address of the data in A_INLINE_PROGRAM.
       */
    mp_uint li_length;
      /* Length of the linenumber data.
       */
    int function;
      /* Function index
       */
    fulltype_t returntype;
      /* The return type (uncounted reference).
       */
    ident_t * ident;
      /* The ident entry with the function name.
       */
    int num_args;
      /* Number of arguments.
       */
    Bool parse_context;
      /* TRUE if the context variable definitions are parsed.
       */
    int start_line;
      /* Starting line number, used to adjust the generated linenumbers.
       */
    int end_line;
      /* Ending line number, used to adjust the generated linenumbers.
       */

    /* --- Saved Globals --- */
    void * include_handle;
      /* Current include state.
       */
    fulltype_t exact_types;
      /* The enclosing return type setting (reference not counted).
       */
    int block_depth;
      /* Block depth at definition point.
       * +1: Context depth
       * +2: Argument depth
       */
    int num_locals;
    int max_num_locals;
      /* Current and max number of locals at definition point.
       */
    int break_stack_size;
    int max_break_stack_size;
      /* Current and max break stack size at definition point.
       */
    mp_uint full_local_type_start;
    mp_uint full_context_type_start;
      /* Start indices of the local/context variable type information
       * in A_LOCAL_TYPES.
       */
    mp_uint full_local_type_size;
      /* Current size of the A_LOCAL_TYPES memblocks.
       */
};

static inline_closure_t * current_inline;
  /* NULL, or pointer to the current inline_closure_t structure while
   * compiling an inline closure.
   * This variable is also used as flag that we're currently compiling
   * an inline_closure.
   */

static unsigned int inline_closure_id;
  /* ID Number for the inline closure name.
   */

/*-------------------------------------------------------------------------*/
/* Other Variables */

static Bool disable_sefuns;
  /* TRUE: Sefuns will be ignored.
   */

static char *last_yalloced = NULL;
  /* Head of blocklist allocated with yalloc().
   */

static program_t NULL_program;
  /* Empty program_t structure for initialisations.
   */

static p_int comp_stack[COMPILER_STACK_SIZE];
  /* A stack of addresses (offsets) in the generated program code for
   * later backpatching.
   */

static size_t comp_stackp;
  /* Index of the next unused entry in <comp_stack>.
   */

static p_int last_initializer_end;
  /* Address of the argument of the final JUMP instruction of the
   * previous INIT fragment.
   * A negative value if there is no previous fragment (this also means
   * that the INIT functions hasn't been created yet).
   */

static p_int first_initializer_start;
  /* Address of the 'num_arg' byte in the function header of the first
   * INIT fragment.
   */

static Bool variables_initialized;
  /* TRUE if the code for all variables has been created.
   */

static fulltype_t def_function_returntype;
static ident_t *  def_function_ident;
static int        def_function_num_args;
  /* Globals to keep the state while a function is parsed:
   *   _returntype: the returntype (uncounted reference)
   *   _ident:      the function's identifier.
   *   _num_args:   number of formal arguments.
   */

static mem_block_t type_of_arguments;
  /* The vartypes of arguments when calling functions must be saved,
   * to be used afterwards for checking. And because function calls
   * can be done as an argument to a function calls, a stack of argument types
   * is needed. This stack does not need to be freed between compilations,
   * but will be reused.
   */

static fulltype_t * type_of_locals = NULL;
  /* The full types of the local variables.
   * Points to a location in mem_block A_LOCAL_TYPES, it is NULL between
   * compilations.
   */

static fulltype_t * type_of_context = NULL;
  /* The full types of the context variables.
   * Points to a location in mem_block A_LOCAL_TYPES, it is NULL between
   * compilations.
   */

static int current_number_of_locals = 0;
  /* Current (active) number of local variables at this point in the
   * function.
   */

static int max_number_of_locals = 0;
  /* Total number of local variables used in this function so far.
   */

static ident_t *all_locals = NULL;
  /* List of defined local variables, listed in reverse order of definition.
   * This also means that the variables are listed in reverse order of
   * nested block scopes.
   */

static fulltype_t exact_types;
  /* If .typeflags is 0, don't check nor require argument and function types.
   * Otherwise it's the full return type of the function, including
   * visibility (reference not counted).
   */

static funflag_t default_varmod;
static funflag_t default_funmod;
  /* Default visibility modifiers for variables resp. function.
   */

static int heart_beat;
  /* Number of the heart_beat() function, or < 0 if none.
   */

static int call_other_sefun;
  /* Index of the call_other() sefun, or < 0 if none;
   */

static ident_t *all_globals = NULL;
  /* List of all created global identifiers (variables and functions).
   */

static efun_shadow_t *all_efun_shadows = NULL;
  /* List of all shadow markers for efuns shadowed by global identifiers.
   */

static p_int switch_pc;
  /* When compiling a switch, this is the address of the first byte
   * after the SWITCH instruction.
   */

static bc_offset_t current_break_address;
  /* If != 0, the compiler is in a break-able environment and this
   * variable points to the first offset-part of a series of LBRANCHes
   * which implement the break statement. Stored in every offset-part
   * is the address of the offset of the next LBRANCH in the series. The
   * last FBRANCH is marked by having a negative offset value.
   *
   * There are a few special values/flags for this variable:
   */
#define BREAK_ADDRESS_MASK   0x0003ffff
  /* Mask for the offset-address part of the variable.
   */
#define BREAK_ON_STACK        (0x04000000)
  /* Bitflag: true when the break-address is stored on the break stack,
   * and therefore the BREAK instruction has to be used.
   */
#define BREAK_FROM_SWITCH     (0x08000000)
  /* TODO: We are compiling a switch instruction.
   */
#define CASE_LABELS_ENABLED   (0x10000000)
  /* The "case" and "default" statements are allowed since we're
   * compiling a switch(). This flag is turned off for loops or
   * conditions embedded in a switch().
   */
#define BREAK_DELIMITER       (-0x20000000)
  /* Special value: no break encountered (yet).
   */

static bc_offset_t current_continue_address;
  /* If != 0, the compiler is in a continue-able environment and this
   * variable points to the first offset-part of a series of  FBRANCHes
   * which implement the continue statement. Stored in every offset-part
   * is the address of the offset of the next FBRANCH in the series. The
   * last FBRANCH is marked by having a negative offset value.
   *
   * A special case are continues inside a switch, as for these the
   * switch()es have to be terminated too using the BREAK_CONTINUE
   * instructions (which also have an offset-part). The c_c_a therefore
   * also encodes the switch()-nesting depth in the top bits of the
   * variable.
   */
#define CONTINUE_ADDRESS_MASK   0x0003ffff
  /* Mask for the offset-address part of the variable.
   */
#define SWITCH_DEPTH_UNIT       0x00040000
  /* The switch depth is encoded in multiples of this value.
   * This way we don't have to shift.
   */
#define SWITCH_DEPTH_MASK       0x3ffc0000
  /* Mask for the switch-nesting depth part of the variable.
   */
#define CONTINUE_DELIMITER     -0x40000000
  /* Special value: no continue encountered (yet).
   */

static int current_struct;
  /* Index of the current structure to be defined.
   */

static p_uint last_expression;
  /* If >= 0, the address of the last instruction which by itself left
   * a value on the stack. If there is no such instruction, the value
   * is (unsigned)-1.
   */

static Bool last_string_is_new;
  /* TRUE: the last string stored with store_prog_string() was indeed
   * a new string.
   */

static int prog_string_indizes[0x100];
  /* Hash table for the program strings holding the initial indices
   * for the hash chains.
   */

static char prog_string_tags[32];
  /* Bitflags showing which entries in prog_string_indizes[] are valid:
   * if (_tags[n] & (1 << b)) then _indizes[n*8 + b] is valid.
   */

static string_t *last_string_constant = NULL;
  /* The current (last) string constant, a tabled string.
   * It is also used to optimize "foo"+"bar" constructs.
   */

static int current_break_stack_need = 0;
  /* Current depth of the required switch/break stack at this point
   * in a function.
   */

static int max_break_stack_need = 0;
  /* Total depth of the required switch/break stack for this function.
   * This information is required when computing the 'num_locals'
   * for the function header.
   */

static p_int stored_bytes;
  /* Size of the program at the last time of store_line_number_info().
   */

static p_int stored_lines;
  /* Current linenumber at the last time of store_line_number_info().
   */

static int simple_includes;
  /* Number of simple includes since the last real one.
   */

static p_uint last_include_start;
  /* Address in A_LINENUMBERS of the last include information.
   * It is used to remove information about includes which do
   * not generate information ('simple includes').
   */

static int argument_level;
  /* Nesting level of function call arguments.
   * Used to detect nested function calls, like foo( bar () ).
   */

static Bool got_ellipsis[COMPILER_STACK_SIZE];
  /* Flags indexed by <argument_level>, telling if the current function
   * arguments used the L_ELLIPSIS operator.
   * TODO: This should be dynamic.
   */

static const char * compiled_file;
  /* The name of the program to be compiled. While current_loc.file reflects
   * the name of the source file currently being read, this name is always
   * the program's name. Set by prolog().
   */

static const fulltype_t Type_Any     = { TYPE_ANY, NULL };
static const fulltype_t Type_Unknown = { TYPE_UNKNOWN, NULL };
static const vartype_t  VType_Unknown = { TYPE_UNKNOWN, NULL };
static const fulltype_t Type_Number  = { TYPE_NUMBER, NULL };
static const fulltype_t Type_Float   = { TYPE_FLOAT, NULL };
static const fulltype_t Type_String  = { TYPE_STRING, NULL };
static const fulltype_t Type_Object  = { TYPE_OBJECT, NULL };
static const fulltype_t Type_Closure = { TYPE_CLOSURE, NULL };
static const fulltype_t Type_Mapping = { TYPE_MAPPING, NULL };
static const fulltype_t Type_Symbol  = { TYPE_SYMBOL, NULL };
static const fulltype_t Type_Void    = { TYPE_VOID, NULL };
static const fulltype_t Type_Quoted_Array = { TYPE_QUOTED_ARRAY, NULL };
static const fulltype_t Type_Ptr_Any = { TYPE_ANY|TYPE_MOD_POINTER, NULL };
static const fulltype_t Type_Ref_Any = { TYPE_ANY|TYPE_MOD_REFERENCE, NULL };
static const fulltype_t Type_Ref_Number = { TYPE_NUMBER|TYPE_MOD_REFERENCE, NULL };
  /* Constants for the known simple types.
   */

/*-------------------------------------------------------------------------*/
/* Forward declarations */

struct lvalue_s; /* Defined within YYSTYPE aka %union */

static void define_local_variable (ident_t* name, fulltype_t actual_type, typeflags_t opt_star, struct lvalue_s *lv, Bool redeclare, Bool with_init);
static void init_local_variable (ident_t* name, struct lvalue_s *lv, int assign_op, fulltype_t type2);
static Bool add_lvalue_code ( struct lvalue_s * lv, int instruction);
static void insert_pop_value(void);
static void arrange_protected_lvalue(p_int, int, p_int, int);
static int insert_inherited(char *, string_t *, program_t **, function_t *, int, bytecode_p);
  /* Returnvalues from insert_inherited(): */
#  define INHERITED_NOT_FOUND            (-1)
#  define INHERITED_WILDCARDED_ARGS      (-2)
#  define INHERITED_WILDCARDED_NOT_FOUND (-3)
static void store_line_number_relocation(int relocated_from);
int yyparse(void);
static void add_new_init_jump(void);
static void transfer_init_control(void);
static void copy_variables(program_t *, funflag_t);
static int copy_functions(program_t *, funflag_t type);
static void copy_structs(program_t *, funflag_t);
static void new_inline_closure (void);
static void fix_function_inherit_indices(program_t *);
static void fix_variable_index_offsets(program_t *);
static short store_prog_string (string_t *str);

/*-------------------------------------------------------------------------*/
void
yyerror (const char *str)

/* Raise the parse error <str>: usually generate the error message and log it.
 * If this is the first error in this file, account the wizard with an error.
 * If too many errors occured already, do nothing.
 */

{
    char *context;

    if (num_parse_error > 5)
        return;
    context = lex_error_context();
    fprintf(stderr, "%s %s line %d: %s%s.\n"
                  , time_stamp(), current_loc.file->name, current_loc.line
                  , str, context);
    /* TODO: lex should implement a function get_include_stack() which
     * TODO:: returns an svalue-array with the current include stack.
     * TODO:: This could be printed, and also passed to parse_error().
     */
    fflush(stderr);
    parse_error(MY_FALSE, current_loc.file->name, current_loc.line
               , str, context);
    if (num_parse_error == 0)
        save_error(str, current_loc.file->name, current_loc.line);
    num_parse_error++;
} /* yyerror() */

/*-------------------------------------------------------------------------*/
void
yyerrorf (const char *format, ...)

/* Generate an yyerror() using printf()-style arguments.
 */

{
    va_list va;
    char buff[5120];
    char fixed_fmt[1000];

    format = limit_error_format(fixed_fmt, sizeof(fixed_fmt), format);
    va_start(va, format);
    vsprintf(buff, format, va);
    va_end(va);
    yyerror(buff);
} /* yyerrorf() */

/*-------------------------------------------------------------------------*/
void
yywarn (const char *str)

/* Raise the parse warning <str>: usually generate the warning message and
 * log it.
 */

{
    char *context;

    context = lex_error_context();
    fprintf(stderr, "%s %s line %d: Warning: %s%s.\n"
                  , time_stamp(), current_loc.file->name, current_loc.line
                  , str, context);
    /* TODO: lex should implement a function get_include_stack() which
     * TODO:: returns an svalue-array with the current include stack.
     * TODO:: This could be printed, and also passed to parse_error().
     */
    fflush(stderr);
    parse_error(MY_TRUE, current_loc.file->name, current_loc.line
               , str, context);
    if (master_ob && num_parse_error == 0)
        save_error(str, current_loc.file->name, current_loc.line);
    /* TODO: Introduce a 'master_is_loading' flag to prevent this call while
     * TODO:: the master is inactive.
     */
} /* yywarn() */

/*-------------------------------------------------------------------------*/
void
yywarnf (const char *format, ...)

/* Generate an yywarn() using printf()-style arguments.
 */

{
    va_list va;
    char buff[5120];
    char fixed_fmt[1000];

    format = limit_error_format(fixed_fmt, sizeof(fixed_fmt), format);
    va_start(va, format);
    vsprintf(buff, format, va);
    va_end(va);
    yywarn(buff);
} /* yywarnf() */

/*-------------------------------------------------------------------------*/
static void *
yalloc (size_t size)

/* Allocate a block of <size>, add it at the head of the last_yalloced
 * list, and return the pointer.
 *
 * Together with yfree(), this allocator is able to free intermediate
 * results in the epilog() which were thrown away due to an error.
 * TODO: A stack'ish mempool could do this?
 */

{
    char **p;

    p = xalloc(size+sizeof(char*));
    if (!p)
    {
        fatal("Out of memory in compiler.\n");
        return NULL;
    }
    *p++ = last_yalloced;
    last_yalloced = (char *)p;
    return p;
} /* yalloc() */

/*-------------------------------------------------------------------------*/
static void
yfree (void *block)

/* Free the block last allocated by yalloc().
 */

{
    char **p;

    p = (char **)block;
    if (p != (char **)last_yalloced)
    {
        debug_message("%s Block mismatch", time_stamp());
        return;
    }
    last_yalloced = *--p;
    xfree(p);
} /* yfree() */

/*-------------------------------------------------------------------------*/
static char *
ystring_copy (char *str)

/* Duplicate the string <str> using yalloc() and return the new one.
 */

{
    char *p;

    p = yalloc(strlen(str)+1);
    strcpy(p, str);
    return p;
} /* ystring_copy() */

/*-------------------------------------------------------------------------*/
static void
add_string_constant (void)

/* Add the string <last_lex_string> to the string in <last_string_constant>.
 * This is used to optimize "foo" + "bar" constructs.
 */

{
    string_t *tmp;

    tmp = mstr_add(last_string_constant, last_lex_string);
    if (!tmp)
    {
        yyerrorf("Out of memory for string literal (%zu bytes)"
                , (mstrsize(last_string_constant)
                         +mstrsize(last_lex_string))
                );
        return;
    }
    free_mstring(last_string_constant);
    free_mstring(last_lex_string); last_lex_string = NULL;
    last_string_constant = make_tabled(tmp);
    if (!last_string_constant)
    {
        yyerrorf("Out of memory for string literal (%zu bytes)",
                mstrsize(tmp));
    }
} /* add_string_constant() */

/*-------------------------------------------------------------------------*/
static char *
realloc_mem_block (mem_block_t *mbp, mp_int size)

/* Resize memblock <mbp> to hold at least <size> bytes, but at least
 * double its current size.
 *
 * Return NULL when out of memory, or a pointer to the newly allocated
 * memory area (ie. mbp->block).
 */

{
    mp_uint max_size;
    char *p;

    max_size = mbp->max_size;
    do {
        max_size *= 2;
    } while ((mp_uint)size > max_size);

    p = rexalloc(mbp->block, max_size);
    if (!p)
    {
        lex_close("Out of memory");
        return NULL;
    }
    mbp->block = p;
    mbp->max_size = max_size;
    return p;
} /* realloc_mem_block() */

/*-------------------------------------------------------------------------*/
static INLINE void
extend_mem_block (int n, size_t size)

/* Reserve <size> bytes at the current position in memory area <n>.
 * This does increase the .current_size .
 */

{
    mem_block_t *mbp = &mem_block[n];

    if (size)
    {
        if (mbp->current_size + size > mbp->max_size)
        {
            if (!realloc_mem_block(mbp, mbp->current_size + size))
                return;
        }
        mbp->current_size += size;
    }
} /* extend_mem_block() */

/*-------------------------------------------------------------------------*/
static INLINE void
add_to_mem_block (int n, void *data, size_t size)

/* Add the <data> block of <size> bytes to the memory area <n>.
 */

{
    mem_block_t *mbp = &mem_block[n];

    if (size)
    {
        if (mbp->current_size + size > mbp->max_size)
        {
            if (!realloc_mem_block(mbp, mbp->current_size + size))
                return;
        }
        memcpy(mbp->block + mbp->current_size, data, size);
        mbp->current_size += size;
    }
} /* add_to_mem_block() */

/*-------------------------------------------------------------------------*/
#define byte_to_mem_block(n, b) \
    ((void)((mem_block[n].current_size == mem_block[n].max_size \
             ? !!realloc_mem_block(&mem_block[n],0) : 1) \
            ? (mem_block[n].block[mem_block[n].current_size++] = (char)(b)) \
            : 0)\
    )
  /* Add the byte <b> to the memory area <n>, which is resized
   * if necessary.
   */

/* ==============================   TYPES   ============================== */

/*-------------------------------------------------------------------------*/
static INLINE void
assign_full_to_vartype(vartype_t * dest, fulltype_t src)

/* Assign a fulltype_t variable to a vartype_t variable.
 */

{
    dest->type = src.typeflags;
    dest->t_struct = src.t_struct;

} /* assign_full_to_vartype() */

/*-------------------------------------------------------------------------*/
static INLINE void
assign_var_to_fulltype(fulltype_t * dest, vartype_t src)

/* Assign a vartype_t variable to a fulltype_t variable.
 */

{
    dest->typeflags = src.type;
    dest->t_struct = src.t_struct;

} /* assign_var_to_fulltype() */

/*-------------------------------------------------------------------------*/
static INLINE Bool
equal_types (fulltype_t e, fulltype_t t)

/* Return TRUE if <e> and <t> are compatible basic types.
 */

{
    return (e.typeflags & TYPEID_MASK) == (t.typeflags & TYPEID_MASK)
      && e.t_struct == t.t_struct;

} /* equal_types() */


static INLINE Bool
basic_type (typeflags_t e, typeflags_t t)

/* Return TRUE if <e> and <t> are compatible basic types.
 */

{
    e &= TYPEID_MASK;
    t &= TYPEID_MASK;

    return e == TYPE_ANY
        || e == t
        || t == TYPE_ANY
    ;
} /* basic_type() */


static INLINE Bool
BASIC_TYPE (fulltype_t e, fulltype_t t)

/* Return TRUE if <e> and <t> are compatible basic types.
 */

{
    return basic_type(e.typeflags, t.typeflags);
} /* BASIC_TYPE() */


static INLINE Bool
TYPE (fulltype_t e, fulltype_t t)

/* Return TRUE if <e> and <t> are compatible basic xor pointer types.
 */

{
    typeflags_t ef = e.typeflags & TYPEID_MASK;
    typeflags_t tf = t.typeflags & TYPEID_MASK;

    return basic_type(ef & TYPE_MOD_MASK, tf & TYPE_MOD_MASK)
        || (   (ef & TYPE_MOD_POINTER) && (tf & TYPE_MOD_POINTER) 
            && basic_type( ef & (TYPE_MOD_MASK & ~TYPE_MOD_POINTER)
                         , tf & (TYPE_MOD_MASK & ~TYPE_MOD_POINTER))
           )
    ;
} /* TYPE() */


static INLINE Bool
vtype (vartype_t e, vartype_t t)

/* Return TRUE if <e> and <t> are compatible basic xor pointer types.
 */

{
    fulltype_t et, tt;

    assign_var_to_fulltype(&et, e);
    assign_var_to_fulltype(&tt, t);
    return TYPE(et, tt);
} /* vtype() */

static INLINE Bool
MASKED_TYPE (fulltype_t e, fulltype_t t)

/* Return TRUE if <e> and <t> are compatible basic types, or if both
 * are pointer types and one of them is a *ANY.
 */

{
    typeflags_t ef = e.typeflags & TYPEID_MASK;
    typeflags_t tf = t.typeflags & TYPEID_MASK;

    return  basic_type(ef, tf) 
        || ( ef == (TYPE_MOD_POINTER|TYPE_ANY) && ef & TYPE_MOD_POINTER ) 
        || ( tf == (TYPE_MOD_POINTER|TYPE_ANY) && ef & TYPE_MOD_POINTER ) 
    ;

} /* MASKED_TYPE() */


static INLINE Bool
REDEFINED_TYPE (fulltype_t e, fulltype_t t)

/* Return TRUE if type <t> is a proper redefinition of <e>.
 * This is the case if <e> and <t> are compatible base types,
 * or if one of them is *ANY.
 */

{
    typeflags_t ef = e.typeflags & TYPEID_MASK;
    typeflags_t tf = t.typeflags & TYPEID_MASK;
    return basic_type(ef, tf ) 
        || (tf == (TYPE_MOD_POINTER|TYPE_ANY) ) 
        || (ef == (TYPE_MOD_POINTER|TYPE_ANY) ) 
    ;
} /* REDEFINED_TYPE() */


/*-------------------------------------------------------------------------*/
static char *
get_f_visibility (funflag_t flags)

/* Return (in a static buffer) a textual representation of the visibility
 * flags in <flags>.
 */

{
    static char buff[120];
    size_t len;

    buff[0] = '\0';
    if (flags & TYPE_MOD_STATIC)
        strcat(buff, "static ");
    if (flags & TYPE_MOD_NO_MASK)
        strcat(buff, "nomask ");
    if (flags & TYPE_MOD_PRIVATE)
        strcat(buff, "private ");
    if (flags & TYPE_MOD_PROTECTED)
        strcat(buff, "protected ");
    if (flags & TYPE_MOD_PUBLIC)
        strcat(buff, "public ");
    if (flags & TYPE_MOD_VARARGS)
        strcat(buff, "varargs ");
    if (flags & TYPE_MOD_DEPRECATED)
        strcat(buff, "deprecated ");

    len = strlen(buff);
    if (len)
        buff[len-1] = '\0';

    return buff;
} /* get_f_visibility() */

/*-------------------------------------------------------------------------*/
static char *
get_visibility (fulltype_t type)

/* Return (in a static buffer) a textual representation of the visibility
 * portion of <type>.
 */

{
    return get_f_visibility(type.typeflags);
} /* get_visibility() */

/*-------------------------------------------------------------------------*/
char *
get_type_name (fulltype_t type)

/* Return (in a static buffer) a textual representation of <type>.
 */

{
    static char buff[120];
    static char *type_name[] = { "unknown", "int", "string", "void", "object",
                                 "mapping", "float", "mixed", "closure",
                                 "symbol", "quoted_array", "struct" };

    Bool pointer = MY_FALSE, reference = MY_FALSE;

    buff[0] = '\0';
    if (type.typeflags & TYPE_MOD_STATIC)
        strcat(buff, "static ");
    if (type.typeflags & TYPE_MOD_NO_MASK)
        strcat(buff, "nomask ");
    if (type.typeflags & TYPE_MOD_PRIVATE)
        strcat(buff, "private ");
    if (type.typeflags & TYPE_MOD_PROTECTED)
        strcat(buff, "protected ");
    if (type.typeflags & TYPE_MOD_PUBLIC)
        strcat(buff, "public ");
    if (type.typeflags & TYPE_MOD_VARARGS)
        strcat(buff, "varargs ");
    if (type.typeflags & TYPE_MOD_DEPRECATED)
        strcat(buff, "deprecated ");

    type.typeflags &= TYPE_MOD_MASK;

    if (type.typeflags & TYPE_MOD_POINTER)
    {
        pointer = MY_TRUE;
        type.typeflags &= ~TYPE_MOD_POINTER;
    }
    if (type.typeflags & TYPE_MOD_REFERENCE)
    {
        reference = MY_TRUE;
        type.typeflags &= ~TYPE_MOD_REFERENCE;
    }

    if (type.typeflags >= sizeof type_name / sizeof type_name[0])
        fatal("Bad type %"PRIu32": %s line %d\n"
             , type.typeflags,  current_loc.file->name, current_loc.line);

    strcat(buff, type_name[type.typeflags]);

    if  (type.typeflags == TYPE_STRUCT)
    {
        strcat(buff, " ");
        if (type.t_struct)
        {
            char buff2[100];
            strcat(buff, get_txt(type.t_struct->name));
            sprintf(buff2, " %p", type.t_struct);
            strcat(buff, buff2);
        }
    }

    if (pointer)
        strcat(buff, " *");
    if (reference)
        strcat(buff, " &");

    return buff;
} /* get_type_name() */

/*-------------------------------------------------------------------------*/
static char *
get_two_types (fulltype_t type1, fulltype_t type2)

/* Return (in a static buffer) the text "(<type1> vs. <type2>)".
 */
{
    static char buff[100];

    strcpy(buff, "(");
    strcat(buff, get_type_name(type1));
    strcat(buff, " vs ");
    strcat(buff, get_type_name(type2));
    strcat(buff, ")");
    return buff;
} /* get_two_types() */

/*-------------------------------------------------------------------------*/
static char *
get_two_vtypes (vartype_t type1, vartype_t type2)

/* Return (in a static buffer) the text "(<type1> vs. <type2>)".
 */
{
    fulltype_t ftype1, ftype2;

    assign_var_to_fulltype(&ftype1, type1);
    assign_var_to_fulltype(&ftype2, type2);
    return get_two_types(ftype1, ftype2);
} /* get_two_vtypes() */

/*-------------------------------------------------------------------------*/
static void
type_error (char *str, fulltype_t type)

/* Generate an yyerror with the message "<str>: <type>".
 */
{
    char *p;

    p = get_type_name(type);
    yyerrorf("%s: \"%s\"", str, p);
} /* type_error() */

/*-------------------------------------------------------------------------*/
static void
argument_type_error (int instr, fulltype_t type)

/* Generate an yyerror with the message "Bad argument to <instr>: <type>".
 */

{
    char *p;

    p = get_type_name(type);
    yyerrorf("Bad argument to %s: \"%s\"", instrs[instr].name, p);
} /* argument_type_error() */

/*-------------------------------------------------------------------------*/
static void
efun_argument_error(int arg, int instr
                   , fulltype_t * expected, fulltype_t got
                   )
{
    char msg[1024];

    msg[0] = '\0';
    for (; expected->typeflags; expected++)
    {
        if (msg[0] != '\0')
            strcat(msg, "/");
        strcat(msg, get_type_name(*expected));
    }
    yyerrorf("Bad arg %d type to %s(): got %s, expected %s"
            , arg, instrs[instr].name, get_type_name(got), msg);
} /* efun_argument_error() */

/*-------------------------------------------------------------------------*/
static Bool
compatible_types (fulltype_t t1, fulltype_t t2, Bool is_assign)

/* Compare the two types <t1> and <t2> and return TRUE if they are compatible.
 * Rules:
 *   - every type is compatible to itself
 *   - TYPE_UNKNOWN is incompatible to everything
 *   - TYPE_ANY is compatible to everything
 *   - two POINTER types are compatible if at least one is *TYPE_ANY.
 *
 * If <is_assign> is true, it is assumed that <t2> will be assigned
 * to a var of <t1>, and the following rules have to match as well:
 *   - a struct <t1> is compatible to a derived struct <t2>.
 *   - if <t1> is a struct, <t2> must be a struct or TYPE_ANY.
 */

{
    t1.typeflags &= TYPEID_MASK;
    t2.typeflags &= TYPEID_MASK;
    if (t1.typeflags == TYPE_UNKNOWN || t2.typeflags == TYPE_UNKNOWN)
        return MY_FALSE;
    if (t1.typeflags == TYPE_ANY || t2.typeflags == TYPE_ANY)
        return MY_TRUE;

    if (is_assign
     && (t1.typeflags & PRIMARY_TYPE_MASK) == T_STRUCT
     && (t2.typeflags & PRIMARY_TYPE_MASK) == T_STRUCT
       )
    {
        struct_type_t * id1, * id2;

        /* Check if t1 is a base-struct of t2 */
        id1 = t1.t_struct;
        id2 = t2.t_struct;

        while (id2 != NULL && id1 != id2)
        {
            id2 = id2->base;
        }

        /* If the base structs match, just pretend that t2 is the
         * same struct as t1. This will make the following tests
         * work as normal.
         */
        if (id2 != NULL)
            t2.t_struct = id1;
    }

    if (t1.typeflags == t2.typeflags)
    {
        if (t1.t_struct == t2.t_struct)
            return MY_TRUE;
    }

    if ((t1.typeflags & TYPE_MOD_POINTER) && (t2.typeflags & TYPE_MOD_POINTER))
    {
        if ((t1.typeflags & TYPE_MOD_MASK) == (TYPE_ANY|TYPE_MOD_POINTER)
         || (t2.typeflags & TYPE_MOD_MASK) == (TYPE_ANY|TYPE_MOD_POINTER))
            return MY_TRUE;
    }

    if (is_assign && (t1.typeflags & PRIMARY_TYPE_MASK) == T_STRUCT)
    {
        /* Check if t2 is a struct.  If it is, the above
         * check already made sure that the type matches.
         */
        if ((t2.typeflags & PRIMARY_TYPE_MASK) != T_STRUCT)
            return MY_FALSE;
    }

    return MY_FALSE;
} /* compatible_types() */

/*-------------------------------------------------------------------------*/
static INLINE void
i_add_arg_type (fulltype_t type)

/* Add another function argument type to the argument type stack.
 */

{
    mem_block_t *mbp = &type_of_arguments;

    if (mbp->current_size + sizeof type > mbp->max_size)
    {
        mbp->max_size *= 2;
        mbp->block = rexalloc((char *)mbp->block, mbp->max_size);
    }
    assign_full_to_vartype((vartype_t*)(mbp->block + mbp->current_size), type);
    mbp->current_size += sizeof(vartype_t);
} /* i_add_arg_type() */

#define add_arg_type(t) i_add_arg_type(t)

/*-------------------------------------------------------------------------*/
static INLINE void
pop_arg_stack (int n)

/* Pop (remove) the last <n> types from the argument stack.
 */

{
    vartype_t * vp;

    vp = (vartype_t*)(type_of_arguments.block + type_of_arguments.current_size);
    while (n > 0)
    {
        type_of_arguments.current_size -= sizeof (vartype_t);
        n--;
        vp--;
    }
} /* pop_arg_stack() */

/*-------------------------------------------------------------------------*/
static INLINE vartype_t *
get_argument_types_start (int n)

/* Get the type of the <n>th last argument from the stack.
 * <n> must be >= 1.
 */

{
    return
        &((vartype_t *)
         (type_of_arguments.block + type_of_arguments.current_size))[ - n];
} /* get_arguments_type_start() */

/*-------------------------------------------------------------------------*/
static INLINE void
check_aggregate_types (int n)

/* The last <n> types on the argument stack are an aggregate type.
 * Combine the single types and make sure that none is a reference type.
 */

{
    vartype_t *argp;
    typeid_t   mask;

    argp = (vartype_t *) (type_of_arguments.block +
          (type_of_arguments.current_size -= sizeof (vartype_t) * n) );

    /* We're just interested in TYPE_MOD_REFERENCE, so we preset all
     * other bits with 1.
     */
    for (mask = ~TYPE_MOD_REFERENCE; --n >= 0; )
    {
        mask |= argp->type;
        argp++;
    }

    if (!(~mask & 0xffff))
        yyerror("Can't trace reference assignments.");
} /* check_aggregate_types() */


/*-------------------------------------------------------------------------*/
static INLINE void
warn_function_shadow ( const string_t *pubProg,  string_t * pubFun
                     , const string_t *privProg, string_t * privFun
                     )

/* Issue a warning that the public function <pubProg>::<pubFun>() shadows the
 * private function <privProg>::<privFun>().
 * Both <pubProg> and <privProg> can be NULL.
 * If the function is __INIT(), no warning is printed.
 */

{
    string_t *pubCProg = NULL;
    string_t *privCProg = NULL;

    if (mstreq(pubFun, STR_VARINIT)
     && mstreq(privFun, STR_VARINIT))
        return;

    if (pubProg != NULL)  pubCProg = cvt_progname(pubProg);
    if (privProg != NULL) privCProg = cvt_progname(privProg);

    if (pubCProg != NULL)
    {
        if (privCProg != NULL)
            yywarnf("public %s::%s() shadows private %s::%s()"
                  , get_txt(pubCProg), get_txt(pubFun)
                  , get_txt(privCProg), get_txt(privFun)
                );
        else
            yywarnf("public %s::%s() shadows private %s()"
                  , get_txt(pubCProg), get_txt(pubFun)
                  , get_txt(privFun)
                );
    }
    else if (privCProg != NULL)
        yywarnf("public %s() shadows private %s::%s()"
              , get_txt(pubFun)
              , get_txt(privCProg), get_txt(privFun)
            );
    else
        yywarnf("public %s() shadows private %s()"
              , get_txt(pubFun)
              , get_txt(privFun)
            );
    
    if (pubCProg != NULL)  free_mstring(pubCProg);
    if (privCProg != NULL) free_mstring(privCProg);
} /* warn_function_shadow() */

/* =============================   CODEGEN   ============================= */

/*-------------------------------------------------------------------------*/
static INLINE char *
realloc_a_program (size_t size)

/* If necessary, increase the allocated size of the A_PROGRAM area so that at
 * least <size> more bytes can be stored in it.
 *
 * Return NULL when out of memory, or a pointer to the (possibly newly
 * allocated) memory area (ie. mem_block[A_PROGRAM].block).
 */

{
    mem_block_t * mbp = &mem_block[A_PROGRAM];
    mp_uint new_size = mbp->current_size + size;

    if (new_size <= mbp->max_size)
        return mbp->block;
    return realloc_mem_block(mbp, new_size);
} /* realloc_a_program() */

/*-------------------------------------------------------------------------*/

#define ins_byte(b) byte_to_mem_block(A_PROGRAM, b)

#ifndef ins_byte

static INLINE void
ins_byte (bytecode_t b)

/* Add the byte <b> to the A_PROGRAM area.
 */

{
    if (mem_block[A_PROGRAM].current_size == mem_block[A_PROGRAM].max_size )
    {
        if (!realloc_a_program(1))
        {
            yyerrorf("Out of memory: program size %"PRIuMPINT"\n"
                    , mem_block[A_PROGRAM].current_size + 1);
            return;
        }
    }
    mem_block[A_PROGRAM].block[mem_block[A_PROGRAM].current_size++] = b;
} /* ins_byte() */

#endif

/*-------------------------------------------------------------------------*/
static void
ins_f_code (unsigned int b)

/* Add the instruction <b> to the A_PROGRAM area, taking care of encoding
 * multi-byte instructions properly.
 */

{
    if (instrs[b].prefix)
        ins_byte(instrs[b].prefix);
    ins_byte(instrs[b].opcode);
} /* ins_f_code() */

/*-------------------------------------------------------------------------*/
static void
ins_short (long l)

/* Add the 2-byte number <l> to the A_PROGRAM area in a fixed byteorder.
 */

{
    if (l > (long)USHRT_MAX || l < SHRT_MIN)
        yyerrorf("Compiler error: too large number %lx passed to ins_short()"
                , l);

    if (realloc_a_program(sizeof(short)))
    {
        put_short(PROGRAM_BLOCK + CURRENT_PROGRAM_SIZE, l);
        CURRENT_PROGRAM_SIZE += sizeof(short);
    }
    else
    {
        yyerrorf("Out of memory: program size %"PRIuMPINT"\n"
                , CURRENT_PROGRAM_SIZE + sizeof(short));
    }
} /* ins_short() */

/*-------------------------------------------------------------------------*/
static void
upd_short (bc_offset_t offset, long l)

/* Store the 2-byte number <l> at <offset> in the A_PROGRAM area in
 * a fixed byteorder.
 */

{
    if (l > (long)USHRT_MAX || l < SHRT_MIN)
        yyerrorf("Compiler error: too large number %ld passed to upd_short()"
                , l);

    put_short(PROGRAM_BLOCK + offset, l);

} /* upd_short() */

/*-------------------------------------------------------------------------*/
static short
read_short (bc_offset_t offset)

/* Return the 2-byte number stored at <offset> in the A_PROGRAM area.
 */

{
    return get_short(PROGRAM_BLOCK + offset);

} /* read_short() */

/*-------------------------------------------------------------------------*/
static void
ins_jump_offset (bc_offset_t l)
/* Add the jump offset <l> to the A_PROGRAM area in a fixed byteorder.
 */
{
    if (realloc_a_program(sizeof(l)))
    {
        put_bc_offset(PROGRAM_BLOCK + CURRENT_PROGRAM_SIZE, l);
        CURRENT_PROGRAM_SIZE += sizeof(l);
    }
    else
    {
        yyerrorf("Out of memory: program size %"PRIuMPINT"\n"
                 , CURRENT_PROGRAM_SIZE + sizeof(l));
    }
} /* ins_jump_offset() */

/*-------------------------------------------------------------------------*/
static void
upd_jump_offset (bc_offset_t offset, bc_offset_t l)
/* Store the new offset <l> at <offset> in the A_PROGRAM area in
 * a fixed byteorder.
 */
{
    put_bc_offset(PROGRAM_BLOCK + offset, l);
} /* upd_jump_offset() */

/*-------------------------------------------------------------------------*/
static bc_offset_t
read_jump_offset (bc_offset_t offset)
/* Return the jump offset stored at <offset> in the A_PROGRAM area.
 */
{
    return get_bc_offset(PROGRAM_BLOCK + offset);
} /* read_jump_offset() */

/*-------------------------------------------------------------------------*/
static void
ins_uint32 (uint32_t l)
/* Add the uint32_t <l> to the A_PROGRAM area.
 */
{
    if (realloc_a_program(sizeof(uint32_t)))
    {
        put_uint32(PROGRAM_BLOCK + CURRENT_PROGRAM_SIZE, l);
        CURRENT_PROGRAM_SIZE += sizeof(uint32_t);
    }
    else
    {
        yyerrorf("Out of memory: program size %"PRIuMPINT"\n"
                , CURRENT_PROGRAM_SIZE + sizeof(uint32_t));
    }
} /* ins_uint32() */

/*-------------------------------------------------------------------------*/
static void upd_uint32 (bc_offset_t offset, uint32_t l) UNUSED;
static void
upd_uint32 (bc_offset_t offset, uint32_t l)
/* Store the uint32_t <l> at <offset> in the A_PROGRAM are.
 */
{
    put_uint32(PROGRAM_BLOCK + offset, l);
} /* upd_uint32() */

/*-------------------------------------------------------------------------*/
static uint32 read_uint32 (bc_offset_t offset) UNUSED;
static uint32
read_uint32 (bc_offset_t offset)
// Return the uint32_t stored at <offset> in the A_PROGRAM area.
{
    return get_uint32(PROGRAM_BLOCK + offset);
} /* read_uint32() */

/*-------------------------------------------------------------------------*/
static void
ins_uint16 (uint16_t l)
/* Add the uint16_t <l> to the A_PROGRAM area.
 */
{
    if (realloc_a_program(sizeof(l)))
    {
        put_uint16(PROGRAM_BLOCK + CURRENT_PROGRAM_SIZE, l);
        CURRENT_PROGRAM_SIZE += sizeof(l);
    }
    else
    {
        yyerrorf("Out of memory: program size %"PRIuMPINT"\n"
                 , CURRENT_PROGRAM_SIZE + sizeof(l));
    }
} /* ins_uint16() */

/*-------------------------------------------------------------------------*/
static INLINE void
ins_p_int (p_int num)

/* Add the number <num> to the A_PROGRAM area in a fixed byteorder.
 */
{
    if (realloc_a_program(sizeof(num)))
    {
        /* F_NUMBER expects the number in the host format. Therefore memcpy()
         * is OK. interpret.c will read the number with memcpy() as well.
         * TODO: use a suitable PUT_ from bytecode.h (change in interpret.c as well!)
         */
        memcpy(mem_block[A_PROGRAM].block + CURRENT_PROGRAM_SIZE, &num, sizeof(num));
        CURRENT_PROGRAM_SIZE += sizeof(num);
    }
    else
    {
        yyerrorf("Out of memory: program size %"PRIuMPINT"\n"
                 , mem_block[A_PROGRAM].current_size + sizeof(num));
    }
} /* ins_p_int() */

/*-------------------------------------------------------------------------*/
static INLINE void
upd_p_int (mp_uint offset, p_int num)

/* Store the number <num> at <offset> in the A_PROGRAM area in a fixed byteorder.
 */
{
    /* F_NUMBER expects the number in the host format. Therefore memcpy()
     * is OK. interpret.c will read the number with memcpy() as well.
     * TODO: use a suitable PUT_ from bytecode.h (change in interpret.c as well!)
     */
    memcpy(mem_block[A_PROGRAM].block + offset, &num, sizeof(num));

} /* upd_p_int() */

/*-------------------------------------------------------------------------*/
static p_int
read_p_int (mp_uint offset)

/* Return the <number> stored at <offset> in the A_PROGRAM area.
 */
{
    p_int number;
    /* TODO: use GET_ function from bytecode.h */
    memcpy(&number, mem_block[A_PROGRAM].block + offset, sizeof(number));

    return number;
} /* read_p_int() */

/*-------------------------------------------------------------------------*/
static void
ins_number (p_int num)

/* Insert code to push number <num> onto the stack.
 * The function tries to find the shortest sequence to do so.
 */

{
    if (num == 0)
        ins_f_code(F_CONST0);
    else if (num == 1)
    {
        ins_f_code(F_CONST1);
    }
    else if (num == -1)
        ins_f_code(F_NCONST1);
    else if (num >= 0 && num <= 0x0FF)
    {
        ins_f_code(F_CLIT);
        ins_byte((num & 0xFF));
    }
    else if (num < 0 && num >= -0x0FF)
    {
        ins_f_code(F_NCLIT);
        ins_byte(((-num) & 0xFF));
    }
    else
    {
        ins_f_code(F_NUMBER);
        ins_p_int(num);
    }
} /* ins_number() */

/*-------------------------------------------------------------------------*/
/* The following macros are used for a speedy codegeneration within bigger
 * functions.
 *
 * To insert at max <n> bytes, the function has to declare
 *
 *    PREPARE_INSERT(n)
 *
 * among the variables and can the use the following macros to add bytes:
 *
 *    add_f_code(i): to add instruction <i> to the program
 *    add_byte(b):   to add byte <b> to the program
 *    add_short(s):  to add short <s> to the program
 *
 * Except for add_f_code(), none of the macros adapts CURRENT_PROGRAM_SIZE,
 * and add_f_code() increments the _SIZE only for the prefix byte if any.
 */

#define PREPARE_INSERT(n) \
    bytecode_p __PREPARE_INSERT__p = (\
      realloc_a_program(n) ? (PROGRAM_BLOCK + CURRENT_PROGRAM_SIZE) : NULL);

#define add_byte(b)   (void) STORE_UINT8(__PREPARE_INSERT__p, (b))

#define add_short(s) STORE_SHORT(__PREPARE_INSERT__p, (s))

#define add_f_code(i) \
    do{ if (instrs[i].prefix) { \
            add_byte(instrs[i].prefix); \
            CURRENT_PROGRAM_SIZE++; \
        }\
        add_byte(instrs[i].opcode); \
    }while(0)

/*-------------------------------------------------------------------------*/
static void
push_address (void)

/* Push the current program size as address onto the compiler stack.
 */

{
    if (comp_stackp >= COMPILER_STACK_SIZE)
    {
        yyerror("Compiler stack overflow");
        /* Don't store the address, but keep proper track of the depth.
         */
        comp_stackp++;
        return;
    }
    comp_stack[comp_stackp++] = mem_block[A_PROGRAM].current_size;
} /* push_address() */

/*-------------------------------------------------------------------------*/
static void
push_explicit (p_int address)

/* Push the program <address> onto the compiler stack.
 */

{
    if (comp_stackp >= COMPILER_STACK_SIZE)
    {
        yyerror("Compiler stack overflow");
        /* Don't store the address, but keep proper track of the depth.
         */
        comp_stackp++;
        return;
    }
    comp_stack[comp_stackp++] = address;
} /* push_explicit() */

/*-------------------------------------------------------------------------*/
static p_int
pop_address (void)

/* Pop the most recent stored address from the compiler stack and return
 * it.
 */

{
    if (comp_stackp == 0)
        fatal("Compiler stack underflow.\n");
    if (comp_stackp > COMPILER_STACK_SIZE)
    {
        /* Nothing to retrieve, but keep track of the depth */
        --comp_stackp;
        return 0;
    }
    return comp_stack[--comp_stackp];
} /* pop_address() */

/*-------------------------------------------------------------------------*/
static Bool
fix_branch (int ltoken, p_int dest, p_int loc)

/* Backpatch a branch instruction at <loc> to jump to <dest>.
 * If the offset exceeds the 255 range, the branch instruction is changed
 * into its long-branch variant <ltoken>.
 *
 * Return TRUE if the long branch had to be used, FALSE otherwise.
 * TODO: This really confuses the line number detection code, as suddenly
 * TODO:: the recorded offset are no longer accurate.
 */

{
    p_int offset;  /* The branch offset */

    offset = dest - (loc +1);

    if (offset > 0xff)
    {
        /* We need a long branch. That also means that we have to
         * move the following code and adapt remembered addresses.
         */
        bc_offset_t i, j;
        bytecode_p p;

        mem_block[A_PROGRAM].block[loc] = 0; /* Init it */

        /* Update the break address */
        if ( current_break_address > loc
         && !(current_break_address & (BREAK_ON_STACK|BREAK_DELIMITER) ) )
        {
            for (i = current_break_address & BREAK_ADDRESS_MASK
                ; (j = read_jump_offset(i)) > loc; )
            {
                upd_jump_offset(i, j+1);
                i = j;
            }
            current_break_address++;
        }

        /* Update the continue address */
        if ( (current_continue_address & CONTINUE_ADDRESS_MASK) > loc
         && !(current_continue_address & CONTINUE_DELIMITER ) )
        {
            for(i = current_continue_address & CONTINUE_ADDRESS_MASK;
              (j=read_jump_offset(i)) > loc; )
            {
                upd_jump_offset(i, j+1);
                i = j;
            }
            current_continue_address++;
        }

        ins_byte(0); /* Just to make sure the memory is there */

        /* Move the code */
        p = PROGRAM_BLOCK + mem_block[A_PROGRAM].current_size-1;
        i = mem_block[A_PROGRAM].current_size - loc;
        for( ; --i >= 0; --p )
        {
            PUT_CODE(p, GET_CODE(p-1));
        }

        /* Store the new branch instruction */
        PUT_CODE(p, ltoken);
        upd_short(loc, offset+2);

        if (offset > 0x7ffd)
            yyerrorf("Compiler limit: Too much code to branch over: %"
                      PRIdPINT" bytes", offset);

        return MY_TRUE;
    }
    else
    {
        /* Just update the offset */
        mem_block[A_PROGRAM].block[loc] = offset;
        return MY_FALSE;
    }
} /* fix_branch() */

/*-------------------------------------------------------------------------*/
static bytecode_p
yyget_space (p_int size)

/* Callback function for switch: return a pointer to <size> more bytes
 * in the program area.
 */

{
    if (realloc_a_program(size))
    {
        CURRENT_PROGRAM_SIZE += size;
        return PROGRAM_BLOCK + CURRENT_PROGRAM_SIZE - size;
    }
    yyerrorf("Out of memory: program size %"PRIdMPINT"\n"
            , mem_block[A_PROGRAM].current_size + size);
    return NULL;
} /* yyget_space() */

/*-------------------------------------------------------------------------*/
static void
yymove_switch_instructions (int len, p_int blocklen)

/* Callback function for switch: move the <blocklen> bytecodes at <switch_pc>
 * back by <len> bytes to <switch_pc>+<len>. A continue address in the
 * affected area is corrected.
 */

{
    bc_offset_t i, j;

    if (realloc_a_program(len))
    {
        CURRENT_PROGRAM_SIZE += len;

        /* Adjust the continue address, if any */
        if ( (current_continue_address & CONTINUE_ADDRESS_MASK) > switch_pc
         && !(current_continue_address & CONTINUE_DELIMITER ) )
        {
            for(i = current_continue_address & CONTINUE_ADDRESS_MASK;
              (j=read_jump_offset(i)) > switch_pc; )
            {
                    upd_jump_offset(i, j+len);
                    i = j;
            }
            current_continue_address += len;
        }

        memmove(
          mem_block[A_PROGRAM].block + switch_pc + len,
          mem_block[A_PROGRAM].block + switch_pc,
          blocklen
        );
    }
    else
    {
        yyerrorf("Out of memory: program size %"PRIdMPINT"\n"
                , mem_block[A_PROGRAM].current_size + len);
    }
} /* yymove_switch_instructions() */

/*-------------------------------------------------------------------------*/
static void
yycerrorl (const char *s1, const char *s2, int line1, int line2)

/* Callback function for switch: Raise an error <s1> in file <s2> at
 * lines <line1> and <line2>.
 * <s1> may contain one '%s' to insert s2, <s2> may contain one or
 * or two '%d' to insert line1 and line2.
 */

{
    char buff[100];

    sprintf(buff, s2, line1, line2);
    yyerrorf(s1, buff);
} /* yycerrorl() */

/*-------------------------------------------------------------------------*/
static void
update_lop_branch ( p_uint address, int instruction )

/* <address> points to the branch offset value of an LAND/LOR operation,
 * currently set to 0. Update that offset to branch to the current end
 * of the program.
 *
 * If that branch is too long, the code is rewritten:
 *
 *     Original:             Rewritten:
 *
 *      <expr1>                <expr1>
 *      LOR/LAND l             DUP
 *      <expr2>                LBRANCH_<instruction>
 *   l:                        POP_VALUE
 *                             <expr2>
 *                          l:
 *
 * The extra DUP compensates the svalue the LBRANCH eats.
 * The LBRANCH_<instruction> needs to be passed suiting the logical
 * operator: LBRANCH_WHEN_ZERO for LAND, LBRANCH_WHEN_NON_ZERO for LOR.
 */

{
    p_int offset;

    last_expression = -1;

    offset = mem_block[A_PROGRAM].current_size - ( address + 1);
    if (offset > 0xff)
    {
        /* A long branch is needed */

        int i;
        bytecode_p p;

        ins_short(0);
        ins_byte(0);
        p = PROGRAM_BLOCK + mem_block[A_PROGRAM].current_size-1;
        for (i = offset; --i >= 0; --p )
            *p = p[-3];
        p[-4] = F_DUP;
        p[-3] = instruction;
        upd_short(address+1, offset+3);
        if (offset > 0x7ffc)
            yyerrorf("Compiler limit: Too much code to skip for ||/&&:"
                     " %"PRIdPINT" bytes" , offset);
        p[0]  = F_POP_VALUE;
    }
    else
    {
        mem_block[A_PROGRAM].block[address] = offset;
    }
} /* update_lop_branch() */

/*-------------------------------------------------------------------------*/
static void
shuffle_code (p_uint start1, p_uint start2, p_uint end)

/* Reverse the order of the program blocks [start1..start2[ and [start2..end[
 */

{
    p_uint len1 = start2 - start1;
    p_uint len2 = end - start2;

    bytecode_p pStart1 = PROGRAM_BLOCK + start1;
    bytecode_p pStart2 = PROGRAM_BLOCK + start2;

    bytecode_p * pTmp;

    if (!len1 || !len2)
        return;

    pTmp = xalloc(len1);
    if (!pTmp)
    {
        yyerror("(shuffle_code) Out of memory");
        return;
    }
    memmove(pTmp, pStart1, len1);
    memmove(pStart1, pStart2, len2);
    memmove(pStart1+len2, pTmp, len1);
    xfree(pTmp);
} /* shuffle_code() */

/* ========================   LOCALS and SCOPES   ======================== */

/*-------------------------------------------------------------------------*/
void
free_all_local_names (void)

/* Free all local names, and reset the counters.
 */

{
    ident_t *p,*q;

    for (q = all_locals; NULL != (p = q);)
    {
        q = p->next_all;
        free_shared_identifier(p);
    }

    while (current_number_of_locals > 0 && type_of_locals)
    {
        current_number_of_locals--;
        free_fulltype_data(&type_of_locals[current_number_of_locals]);
    }

    all_locals = NULL;
    current_number_of_locals = 0;
    max_number_of_locals = 0;
    current_break_stack_need = 0;
    max_break_stack_need = 0;
} /* free_all_local_names() */

/*-------------------------------------------------------------------------*/
static void
free_local_names (int depth)

/* Free all locals in the all_locals list which are of higher or
 * the same <depth>, and adjust the counters.
 * A <depth> of 0 is equivalent to calling free_all_local_names().
 */

{
    ident_t *q;

    if (!depth)
    {
        free_all_local_names();
        return;
    }

    /* Are the locals of the given depth? */
    if (!all_locals || all_locals->u.local.depth < depth)
        return;

    if (all_locals->u.local.depth > depth)
        fatal("List of locals clobbered: depth %d, block_depth %d\n"
             , all_locals->u.local.depth, depth);

    while (all_locals != NULL && all_locals->u.local.depth >= depth)
    {
        q = all_locals;
        all_locals = q->next_all;
        free_shared_identifier(q);
        current_number_of_locals--;
        free_fulltype_data(&type_of_locals[current_number_of_locals]);
    }
} /* free_local_names() */

/*-------------------------------------------------------------------------*/
static ident_t *
add_local_name (ident_t *ident, fulltype_t type, int depth)

/* Declare a new local variable <ident> with the type <type> on
 * the scope depth <depth>. The references of <type> are NOT adopted.
 * Return the (adjusted) ident for the new variable.
 */

{
    if ((type.typeflags & PRIMARY_TYPE_MASK) == TYPE_VOID)
    {
        yyerrorf( "Illegal to define variable '%s' as type 'void'"
                , get_txt(ident->name));
    }

    if (current_number_of_locals >= MAX_LOCAL) /* size of type recording array */
        yyerror("Too many local variables");

    else
    {
        ref_fulltype_data(&type);

        if (ident->type != I_TYPE_UNKNOWN)
        {
            /* We're overlaying some other definition.
             * If it's a global, or if we are in an inline-closure arg list,
             * it's ok.
             */
#ifdef DEBUG_INLINES
if (current_inline && current_inline->block_depth+2 == block_depth 
    && ident->type != I_TYPE_GLOBAL)
    printf("DEBUG: redeclare local '%s' as inline arg, depth %d\n", 
           get_txt(ident->name), block_depth);
#endif /* DEBUG_INLINES */
            if (ident->type != I_TYPE_GLOBAL
             && !(current_inline && current_inline->block_depth+2 == block_depth)
               )
            {
                yywarnf( "Variable '%s' shadows previous declaration"
                       , get_txt(ident->name));
            }
            ident = make_shared_identifier_mstr(ident->name, I_TYPE_LOCAL, depth);
        }

        /* Initialize the ident */
        ident->type = I_TYPE_LOCAL;
        ident->u.local.num = current_number_of_locals;
        ident->u.local.depth = depth;
        ident->u.local.context = -1;

        /* Put the ident into the list of all locals */
        if (all_locals && all_locals->u.local.depth > depth)
            fatal("List of locals clobbered: depth %d, adding depth %d\n"
                 , all_locals->u.local.depth, depth);
        ident->next_all = all_locals;
        all_locals = ident;

        /* Record the type */
        type_of_locals[current_number_of_locals++] = type;

        /* And update the scope information */
        if (current_number_of_locals > max_number_of_locals)
            max_number_of_locals = current_number_of_locals;
        block_scope[depth-1].num_locals++;
    }

    return ident;
} /* add_local_name() */

/*-------------------------------------------------------------------------*/
static ident_t *
redeclare_local (ident_t *ident, fulltype_t type, int depth)

/* Redeclare a local name <ident>, to <type> at <depth>; the references
 * of <type> are NOT adopted (nor freed on error).
 * If this happens on a deeper level, it is legal: the new declaration
 * is added and the new identifier is returned.
 * If it is illegal, an yyerror() is risen and the ident of the older
 * declaration is returned for error recovery.
 */

{
    if (all_locals && all_locals->u.local.depth > depth)
    {
        fatal("List of locals clobbered: list depth %d, "
              "block depth %d\n"
              , all_locals->u.local.depth, depth);
    }


    if (ident->u.local.depth >= depth
     || (ident->u.local.depth == 1 && depth == 2)
     || (current_inline && ident->u.local.depth == current_inline->block_depth+2
                        && depth == current_inline->block_depth+3)
       )
    {
        yyerrorf("Illegal to redeclare local name '%s'", get_txt(ident->name));
    }
    else
    {
        ident = add_local_name(ident, type, depth);
    }

    return ident;
} /* redeclare_local() */

/*-------------------------------------------------------------------------*/
static ident_t *
add_context_name (inline_closure_t *closure, ident_t *ident, fulltype_t type, int num)

/* Declare a new context variable <ident> with the type <type> for the
 * currently compiled inline closure. The references of <type> are NOT adopted.
 * <num> is -1 for independent context
 * variables, or the index of the inherited local variable.
 * Return the (adjusted) ident for the new variable.
 */

{
    int depth;
    block_scope_t * block;

    depth = closure->block_depth+1;
    block = & block_scope[depth-1];

#ifdef DEBUG_INLINES
printf("DEBUG: add_context_name('%s', num %d) depth %d, context %d\n", 
       get_txt(ident->name), num, depth, block->num_locals);
#endif /* DEBUG_INLINES */
    if (block->num_locals >= MAX_LOCAL) /* size of type recording array */
    {
        yyerror("Too many context variables");
    }
    else if (num < 0
     && block->first_local + block->num_locals >= MAX_LOCAL
        )
    {
        yyerror("Too many local variables");
    }
    else
    {
        ref_fulltype_data(&type);

        if (ident->type != I_TYPE_UNKNOWN)
        {
            /* We're overlaying some other definition, but that's ok.
             */
            ident = make_shared_identifier_mstr(ident->name, I_TYPE_LOCAL, depth);
            assert (ident->type == I_TYPE_UNKNOWN);
        }

        /* Initialize the ident */
        ident->type = I_TYPE_LOCAL;
        ident->u.local.depth = depth;
        if (num < 0)
        {
            /* First initialize it as a local variable
             * of the outer function, so they can be
             * referenced during initialization.
             */
            ident->u.local.num = block->first_local + block->num_locals;
            ident->u.local.context = -1;
        }
        else
        {
            ident->u.local.num = num;
            ident->u.local.context = block->num_locals;
        }

        /* Put the ident into the list of all locals.
         */
        if (all_locals && all_locals->u.local.depth > depth)
        {
            /* This context variable was detected after we already
             * added locals - find the proper insertion point.
             */
            ident_t * prev, *this;

            for ( prev = all_locals, this = all_locals->next_all
                ; this && this->u.local.depth > depth
                ; prev = this, this = this->next_all) NOOP;

            ident->next_all = this;
            prev->next_all = ident;
        }
        else
        {
            ident->next_all = all_locals;
            all_locals = ident;
        }

        /* Record the type */
        type_of_context[block->num_locals] = type;
        if (num < 0)
            type_of_locals[ident->u.local.num] = type;

        block->num_locals++;
    }

    return ident;
} /* add_context_name() */

/*-------------------------------------------------------------------------*/
static ident_t *
check_for_context_local (ident_t *ident, fulltype_t * pType)

/* The LPC code uses local variable <ident>. If we're compiling
 * an inline closure, check if it is an inherited local for which
 * no context variable has been created yet. If yes, create the context
 * variable.
 * Return the (possibly updated) ident, and store the variables type
 * in *<pType>.
 */

{
    int depth = ident->u.local.depth;

    if (!block_scope[depth-1].accessible)
        yyerrorf("Variable '%s' is in scope but inaccessible.\n",
                 get_txt(ident->name));

    if (current_inline
     && depth <= current_inline->block_depth
       )
    {
        inline_closure_t *closure;
        mp_int closure_nr;
        fulltype_t type;

        closure = current_inline;
        closure_nr = closure - &(INLINE_CLOSURE(0));
        closure->next = -1;

        /* Go through all inline closures to our local variable
         * and record this path using the ->next pointers.
         *
         * Stop at the last inline closure before the variable's
         * scope, because the information about that scope was
         * saved there.
         */
        for (;;)
        {
            inline_closure_t *outer_closure;
            mp_int outer_closure_nr;

            outer_closure_nr = closure->prev;
            if (outer_closure_nr == -1)
                /* It is a local variable of the current function. */
                break;

            outer_closure = &(INLINE_CLOSURE(outer_closure_nr));
            outer_closure->next = closure_nr;

            if (outer_closure->block_depth < depth)
                /* The variable belongs to outer_closure. */
                break;

            closure = outer_closure;
            closure_nr = outer_closure_nr;
        }

        if (ident->u.local.context >= 0)
        {
            /* It's a context variable. */
            type = LOCAL_TYPE(closure->full_context_type_start
                              + ident->u.local.context
                             );
        }
        else
        {
            /* It's a local variable. */
            type = LOCAL_TYPE(closure->full_local_type_start
                              + ident->u.local.num
                             );
        }

        /* Now pass this context variable through
         * all surrounding inline closures.
         */
        while (MY_TRUE)
        {
            /* Skip closures whose context is being parsed,
             * because current_inline is not created
             * in their runtime.
             */
            if (!closure->parse_context)
            {
                ref_fulltype_data(&type);
                ident = add_context_name(closure, ident, type,
                     ident->u.local.context >= 0
                     ? CONTEXT_VARIABLE_BASE + ident->u.local.context
                     : ident->u.local.num);
            }

            if (closure->next == -1)
                break;

            closure = &(INLINE_CLOSURE(closure->next));
        }

        *pType = type;
    }
    else if (ident->u.local.context >= 0)
        *pType = type_of_context[ident->u.local.context];
    else
        *pType = type_of_locals[ident->u.local.num];

    return ident;
} /* check_for_context_local() */

/*-------------------------------------------------------------------------*/
static void
adapt_context_names (void)

/* Convert all explicit context variables
 * from local variables to context variables.
 */

{
    int depth;
    block_scope_t *scope;

    depth = current_inline->block_depth+1;
    scope = block_scope + depth - 1;

    /* Are there locals of the given depth? */
    if (all_locals && all_locals->u.local.depth >= depth)
    {
        ident_t *q = all_locals;

        while (q != NULL && q->u.local.depth > depth)
            q = q->next_all;

        while (q != NULL && q->u.local.depth == depth)
        {
            q->u.local.context = q->u.local.num - scope->first_local;
            q->u.local.num = -1;

            q = q->next_all;
        }
    }
} /* adapt_context_names() */

/*-------------------------------------------------------------------------*/
static void
init_scope (int depth)

/* Initialize the block_scope entry for block_depth <depth>.
 */

{
    block_scope[depth-1].num_locals = 0;
    block_scope[depth-1].first_local = current_number_of_locals;
    block_scope[depth-1].num_cleared = 0;
    block_scope[depth-1].clobbered = MY_FALSE;
    block_scope[depth-1].addr = 0;
    block_scope[depth-1].accessible = MY_TRUE;
} /* init_scope() */

/*-------------------------------------------------------------------------*/
static void
enter_block_scope (void)

/* Enter a new scope and initialize it.
 */

{
    if (block_depth == COMPILER_STACK_SIZE)
        yyerror("Too deep nesting of local blocks.\n");

    block_depth++;
    init_scope(block_depth);

} /* enter_block_scope() */

/*-------------------------------------------------------------------------*/
static void
leave_block_scope (Bool dontclobber)

/* Leave the current scope, freeing all local names defined in that scope.
 *
 * <dontclobber> should be MY_TRUE if the stack of the to-be-left scope
 * is independent of the outer scope (i.e. the scope of closures).
 */

{
        free_local_names(block_depth);
        block_depth--;
        if (block_depth && !dontclobber
         && (block_scope[block_depth].num_locals
          || block_scope[block_depth].clobbered))
        {
            /* the block we just left may have clobbered local variables */
            block_scope[block_depth-1].clobbered = MY_TRUE;
        }
} /* leave_block_scope() */


/* ======================   GLOBALS and FUNCTIONS   ====================== */

/*-------------------------------------------------------------------------*/
static unsigned short
store_argument_types ( int num_arg )

/* Store the <num_arg> argument types from global type_of_locals[] into
 * the proper memblock and return the new argument start index.
 * It is task of the caller to store this start index where it belongs.
 *
 * If exact_types are not required, the function just returns
 * INDEX_START_NONE.
 */

{
    unsigned short argument_start_index;

    /* Store the function arguments, if required.
     */
    if (!exact_types.typeflags)
    {
        argument_start_index = INDEX_START_NONE;
    }
    else
    {
        int i;

        /* Save the argument types.
         */
        argument_start_index = ARGTYPE_COUNT;
        for (i = 0; i < num_arg; i++)
        {
            vartype_t vt;

            assign_full_to_vartype(&vt, type_of_locals[i]);
            ref_vartype_data(&vt);
            add_to_mem_block(A_ARGUMENT_TYPES, &vt, sizeof vt);
        }
    }

    return argument_start_index;
} /* store_argument_types() */

/*-------------------------------------------------------------------------*/
static int
define_new_function ( Bool complete, ident_t *p, int num_arg, int num_local
                    , p_int offset, funflag_t flags, fulltype_t type)

/* Define a new function <p> with the characteristics <num_arg>, <num_local>,
 * program <offset>, <flags> and <type>.
 * The references of <type> are NOT adopted.
 * Result is the number (index) of the function.
 *
 * The function is called whenever a function header (return type, name
 * and arguments) has been parsed - <complete> is FALSE then. Additionally,
 * the function is called as well after a functionbody has been parsed,
 * <complete> is TRUE then.
 *
 * This function is called at least twice for all function definitions:
 * first as prototype (flags & NAME_PROTOTYPE) when the function def is
 * encountered, then a second time for real when the function has been
 * completed. Explicite prototypes can cause additional calls.
 */

{
    int num;
    function_t fun;
    unsigned short argument_start_index;

    /* Move the visibility-info into flags */
    flags |= type.typeflags & ~TYPE_MOD_MASK;

    do {
        function_t *funp;
        Bool args_differ, compare_args;

        if (p->type != I_TYPE_GLOBAL) break;
        if ((num = p->u.global.function) < 0) break;

        funp = FUNCTION(num);

        if ((funp->flags & (NAME_INHERITED|TYPE_MOD_PRIVATE|NAME_HIDDEN|NAME_UNDEFINED))
         == (NAME_INHERITED|TYPE_MOD_PRIVATE|NAME_HIDDEN))
        {
            break;
        }

        /* The function was already defined. It may be one of several reasons:
         *
         *   1. There has been a prototype.
         *   2. There was the same function defined by inheritance.
         *   3. This function has been called, but not yet defined.
         *   4. The function is defined twice.
         *   5. A "late" prototype has been encountered.
         */

        args_differ = MY_FALSE;
        compare_args = MY_FALSE;

        /* The following checks are useful only when done before
         * a functionbody appears, otherwise the warning/error message
         * line numbers will be misleading.
         */
        if (!complete)
        {
            if ((funp->flags & TYPE_MOD_NO_MASK)
             && !(funp->flags & (NAME_PROTOTYPE|NAME_UNDEFINED))
             && ((flags & (NAME_PROTOTYPE|NAME_UNDEFINED)) == (NAME_PROTOTYPE|NAME_UNDEFINED))
               )
                yyerrorf("Illegal to redefine 'nomask' function \"%s\""
                        , get_txt(p->name));

            if (!(funp->flags & (NAME_UNDEFINED|NAME_PROTOTYPE|NAME_INHERITED) ) )
            {
                yyerrorf("Redeclaration of function %s.", get_txt(p->name));
                if ( !(flags & NAME_PROTOTYPE) )
                    free_mstring(p->name);
                return num;
            }

            /* It was either an undefined but used function, or an inherited
             * function. In both cases, we now consider this to be THE new
             * definition. It might also have been a prototype to an already
             * defined function.
             *
             * Check arguments only when types are supposed to be tested,
             * and if this function really has been defined already.
             *
             * 'nomask' functions may not be redefined.
             */
            if (exact_types.typeflags && funp->type.typeflags != TYPE_UNKNOWN)
            {
                fulltype_t t1, t2;

                if (funp->num_arg > num_arg && !(funp->flags & TYPE_MOD_VARARGS))
                    yyerrorf("Incorrect number of arguments in redefinition of '%s'.", get_txt(p->name));
                else if (funp->num_arg == num_arg
                      && ((funp->flags ^ flags) & TYPE_MOD_XVARARGS)
                      && !(funp->flags & TYPE_MOD_VARARGS))
                    yyerrorf("Incorrect number of arguments in redefinition of '%s'.", get_txt(p->name));
                else
                {
                    unsigned short first_arg;

                    first_arg = ARGUMENT_INDEX(num);
                    if (first_arg == INDEX_START_NONE)
                    {
                        if (num_arg && !(funp->flags & NAME_TYPES_LOST) )
                            yyerrorf(
                              "Redefined function '%s' not compiled with type testing."
                            , get_txt(p->name));
                    }
                    else
                    {
                        /* We can compare the arguments */
                        compare_args = MY_TRUE;
                    }
                } /* cases (number of arguments) */

                /* If it's a prototype->function redefinition, check if the
                 * visibility is conserved.
                 */
                {
#                   define TYPE_MOD_VIS \
                           ( TYPE_MOD_NO_MASK \
                           | TYPE_MOD_PRIVATE | TYPE_MOD_PUBLIC \
                           | TYPE_MOD_PROTECTED)
                    funflag_t f1 = funp->flags;
                    funflag_t f2 = flags;

                    /* Smooth out irrelevant differences */
                    if (f1 & TYPE_MOD_STATIC) f1 |= TYPE_MOD_PROTECTED;
                    if (f2 & TYPE_MOD_STATIC) f2 |= TYPE_MOD_PROTECTED;

                    if (!(f1 & (NAME_INHERITED|NAME_TYPES_LOST))
                     && ((f1 ^ f2) & TYPE_MOD_VIS)
                       )
                    {
                        char buff[120];

                        t2 = funp->type;
                        strncpy(buff, get_visibility(t2), sizeof(buff)-1);
                        buff[sizeof(buff) - 1] = '\0'; // strncpy() does not guarantee NUL-termination
                        yywarnf("Inconsistent declaration of '%s': Visibility changed from '%s' to '%s'"
                               , get_txt(p->name), buff, get_visibility(type));
                    }
#                   undef TYPE_MOD_VIS
                }

                /* Check if the 'varargs' attribute is conserved */

                t1 = type;       t1.typeflags &= TYPE_MOD_MASK;
                t2 = funp->type; t2.typeflags &= TYPE_MOD_MASK;
                if (!MASKED_TYPE(t1, t2))
                {
                    if (pragma_pedantic)
                        yyerrorf("Inconsistent declaration of '%s': Return type mismatch %s", get_txt(p->name), get_two_types(t2, t1));
                    else if (pragma_check_overloads)
                        yywarnf("Inconsistent declaration of '%s': Return type mismatch %s", get_txt(p->name), get_two_types(t2, t1));
                }

                if (pragma_pedantic
                 && (funp->flags ^ flags) & TYPE_MOD_VARARGS
                 &&  funp->flags & TYPE_MOD_VARARGS
                   )
                {
                    yywarnf("Redefinition of '%s' loses 'varargs' modifier."
                           , get_txt(p->name));
                }

                /* Check that the two argument lists are compatible */

                if (compare_args)
                {
                    int i;
                    unsigned short first_arg;
                    vartype_t *argp;
                    int num_args = num_arg;

                    /* Don't check newly added arguments */
                    if (num_args > funp->num_arg)
                        num_args = funp->num_arg;

                    first_arg = ARGUMENT_INDEX(num);

                    argp = (vartype_t *)mem_block[A_ARGUMENT_TYPES].block
                           + first_arg;

                    if (funp->flags & TYPE_MOD_XVARARGS)
                        num_args--; /* last argument is ok */

                    for (i = 0; i < num_args; i++ )
                    {
                        t1 = type_of_locals[i];
                        t1.typeflags &= TYPE_MOD_RMASK;
                        assign_var_to_fulltype(&t2, argp[i]);
                        t2.typeflags &= TYPE_MOD_MASK;
                        if (!MASKED_TYPE(t1, t2))
                        {
                            args_differ = MY_TRUE;
                            if (pragma_pedantic)
                                yyerrorf("Argument type mismatch in "
                                         "redefinition of '%s': arg %d %s"
                                        , get_txt(p->name), i+1, get_two_types(t1, t2)
                                        );
                            else if (pragma_check_overloads)
                                yywarnf("Argument type mismatch in "
                                         "redefinition of '%s': arg %d %s"
                                        , get_txt(p->name), i+1, get_two_types(t1, t2)
                                        );
                        }
                    } /* for (all args) */

                } /* if (compare_args) */

            } /* if (exact_types && already defined) */

         } /* if (!complete) */

        /* Remember the heart_beat() function */
        if (mstreq(p->name, STR_HEART_BEAT))
            heart_beat = num;

        /* If it was yet another prototype, then simply return. */
        if (flags & NAME_PROTOTYPE)
        {
            return num;
        }

        /* This is the completion of an earlier prototype: check
         * and update the arguments if necessary, and flesh
         * out the function structure.
         */

        if (funp->num_arg != num_arg || args_differ)
        {
            /* Arguments changed. The only reasonable way this can happen
             * is if this function redefined an inherited one.
             * For that case, we re-create the arguments, for all other cases
             * (to be on the safe side), we turn off type
             * checking as we have no way of deciding which definition is the
             * correct one.
             */
            if (funp->flags & NAME_INHERITED)
            {
                funp->num_arg = num_arg;
                ARGUMENT_INDEX(num) = store_argument_types(num_arg);
            }
            else
            {
                funp->num_arg = num_arg;
                ARGUMENT_INDEX(num) = INDEX_START_NONE;
                flags |= NAME_TYPES_LOST;
            }

        }

        funp->num_local = num_local;
        funp->flags = flags;
        funp->offset.pc = offset;
        free_fulltype_data(&funp->type);
        funp->type = type;
        ref_fulltype_data(&funp->type);

        /* That's it */
        return num;

    } while(0); /* Test and handle for already defined functions */

    /* It's a new function! */

    if (mstreq(p->name, STR_HEART_BEAT))
        heart_beat = FUNCTION_COUNT;

    /* Fill in the function_t */
    fun.name      = p->name; /* adopt the ref */
    fun.offset.pc = offset;
    fun.flags     = flags;
    fun.num_arg   = num_arg;
    fun.num_local = num_local; /* will be updated later */
    fun.type      = type;
    ref_fulltype_data(&fun.type);

    num = FUNCTION_COUNT;

    if (p->type != I_TYPE_GLOBAL)
    {
        /* This is the first _GLOBAL use of this identifier:
         * make an appropriate entry in the identifier table.
         */

        if (p->type != I_TYPE_UNKNOWN)
        {
            /* The ident has been used before otherwise, so
             * get a fresh structure.
             */
            p = make_shared_identifier_mstr(p->name, I_TYPE_GLOBAL, 0);
        }
        /* should be I_TYPE_UNKNOWN now. */

        init_global_identifier(p, /* bVariable: */ MY_TRUE);
        p->next_all = all_globals;
        all_globals = p;
    }
    else if (p->u.global.variable == I_GLOBAL_VARIABLE_FUN)
    {
        /* The previous _GLOBAL use is the permanent efun definition:
         * mark the efun as shadowed.
         */
        efun_shadow_t *q;

        q = xalloc(sizeof(efun_shadow_t));
        q->shadow = p;
        q->next = all_efun_shadows;
        all_efun_shadows = q;
    }
    /* else: Other cases don't need special treatment */

    p->u.global.function = num;

    /* Store the function_t in the functions area */
    add_to_mem_block(A_FUNCTIONS, &fun, sizeof fun);

    /* Store the function arguments, if required,
     * and save the position of the argument types.
     */
    argument_start_index = store_argument_types(num_arg);
    add_to_mem_block( A_ARGUMENT_INDEX, &argument_start_index
                    , sizeof argument_start_index);

    return num;
} /* define_new_function() */

/*-------------------------------------------------------------------------*/
static void
define_variable (ident_t *name, fulltype_t type)

/* Define a new global variable <name> of type <type>.
 * The references of <type> are NOT adopted.
 */

{
    variable_t dummy;
    typeflags_t flags = type.typeflags;
    int n;

    if ((flags & PRIMARY_TYPE_MASK) == TYPE_VOID)
    {
        yyerrorf( "Illegal to define variable '%s' as type 'void'"
                , get_txt(name->name));
    }

    if (name->type != I_TYPE_GLOBAL)
    {
        /* This is the first _GLOBAL use of this identifier:
         * make an appropriate entry in the identifier table.
         */

        if (name->type != I_TYPE_UNKNOWN)
        {
            /* The ident has been used before otherwise, so
             * get a fresh structure.
             */
            name = make_shared_identifier_mstr(name->name, I_TYPE_GLOBAL, 0);
        }

        init_global_identifier(name, /* bVariable: */ MY_TRUE);
        name->next_all = all_globals;
        all_globals = name;
    }
    else if (name->u.global.function == I_GLOBAL_FUNCTION_OTHER
          && (name->u.global.efun >= 0 || name->u.global.sim_efun >= 0)
            )
    {
        /* The previous _GLOBAL use is the permanent efun definition:
         * mark the efun as shadowed.
         */
        efun_shadow_t *q;

        q = xalloc(sizeof(efun_shadow_t));
        q->shadow = name;
        q->next = all_efun_shadows;
        all_efun_shadows = q;
    }

    /* Prepare the new variable_t */

    if (flags & TYPE_MOD_NOSAVE)
    {
        /* 'nosave' is internally saved as 'static' (historical reason) */
        flags |= TYPE_MOD_STATIC;
        flags ^= TYPE_MOD_NOSAVE;
    }

    /* If the variable already exists, make sure that we can redefine it */
    if ( (n = name->u.global.variable) >= 0)
    {
        typeflags_t vn_flags = VARIABLE(n)->type.typeflags;

        /* Visible nomask variables can't be redefined */
        if ( vn_flags & TYPE_MOD_NO_MASK && !(flags & NAME_HIDDEN))
            yyerrorf( "Illegal to redefine 'nomask' variable '%s'"
                    , get_txt(name->name));

        /* We can redefine inherited variables if they are private or hidden,
         * or if at least one of them is static.
         */
        if (  (   !(vn_flags & NAME_INHERITED)
               || (   !(vn_flags & (TYPE_MOD_PRIVATE|NAME_HIDDEN))
                   && !((flags | vn_flags) & TYPE_MOD_STATIC)
                  )
              )
            && !(flags & NAME_INHERITED)
           )
        {
            if (vn_flags & NAME_INHERITED)
                yyerrorf("Illegal to redefine inherited variable '%s'"
                        , get_txt(name->name));
            else
                yyerrorf("Illegal to redefine global variable '%s'"
                        , get_txt(name->name));
        }

        if (((flags | vn_flags) & (TYPE_MOD_STATIC|TYPE_MOD_PRIVATE))
            == TYPE_MOD_STATIC
         && !(flags & NAME_INHERITED)
           )
        {
            yywarnf("Redefining inherited %s variable '%s' with a %s variable"
                   , (vn_flags & TYPE_MOD_STATIC)
                     ? "nosave" : "non-nosave"
                   , get_txt(name->name)
                   , (flags & TYPE_MOD_STATIC) ? "nosave" : "non-nosave"
                   );
        }

        /* Make sure that at least one of the two definitions is 'static'.
         * The variable which has not been inherited gets first pick.
         */
        if (flags & NAME_INHERITED)
        {
            flags |= ~(vn_flags) & TYPE_MOD_STATIC;
        }
        else
        {
            vn_flags |=   ~flags & TYPE_MOD_STATIC;
            VARIABLE(n)->type.typeflags = vn_flags;
        }
    }

    type.typeflags = flags;

    dummy.name = ref_mstring(name->name);
    dummy.type = type;
    ref_fulltype_data(&dummy.type);

    if (flags & TYPE_MOD_VIRTUAL)
    {
        if (!(flags & NAME_HIDDEN))
            name->u.global.variable = VIRTUAL_VAR_TAG | V_VARIABLE_COUNT;
        add_to_mem_block(A_VIRTUAL_VAR, &dummy, sizeof dummy);
    }
    else
    {
        if (!(flags & NAME_HIDDEN))
            name->u.global.variable = NV_VARIABLE_COUNT;
        add_to_mem_block(A_VARIABLES, &dummy, sizeof dummy);
    }
} /* define_variable() */

/*-------------------------------------------------------------------------*/
static void
redeclare_variable (ident_t *name, fulltype_t type, int n)

/* The variable <name> is inherited virtually with number <n>.
 * Redeclare it from its original type to <type>.
 */

{
    typeflags_t flags = type.typeflags;

    if (name->type != I_TYPE_GLOBAL)
    {
        /* This is the first _GLOBAL use of this identifier:
         * make an appropriate entry in the identifier table.
         */

        /* I_TYPE_UNKNOWN */
        init_global_identifier(name, /* bVariable: */ MY_TRUE);
        name->next_all = all_globals;
        all_globals = name;
    }
    else if (name->u.global.function == I_GLOBAL_FUNCTION_OTHER
          && (name->u.global.efun >= 0 || name->u.global.sim_efun >= 0)
            )
    {
        /* The previous _GLOBAL use is the permanent efun definition:
         * mark the efun as shadowed.
         */
        efun_shadow_t *q;

        q = xalloc(sizeof(efun_shadow_t));
        q->shadow = name;

        q->next = all_efun_shadows;
        all_efun_shadows = q;
    }
    /* else: the variable is inherited after it has been defined
     * in the child program.
     */

    /* The variable is hidden, do nothing else */
    if (flags & NAME_HIDDEN)
        return;

    if (name->u.global.variable >= 0 && name->u.global.variable != n)
    {
        if (VARIABLE(name->u.global.variable)->type.typeflags & TYPE_MOD_NO_MASK )
            yyerrorf( "Illegal to redefine 'nomask' variable '%s'"
                    , get_txt(name->name));
    }
    else if (V_VARIABLE(n)->type.typeflags & TYPE_MOD_NO_MASK
          && !(V_VARIABLE(n)->type.typeflags & NAME_HIDDEN)
          && (V_VARIABLE(n)->type.typeflags ^ flags) & TYPE_MOD_STATIC )
    {
        yyerrorf("Illegal to redefine 'nomask' variable \"%s\""
                , get_txt(name->name));
    }

    if (flags & TYPE_MOD_NOSAVE)
    {
        /* 'nosave' is internally saved as 'static' (historical reason) */
        flags |= TYPE_MOD_STATIC;
        flags ^= TYPE_MOD_NOSAVE;
    }

    type.typeflags = flags;

    name->u.global.variable = n;
    free_fulltype_data(&V_VARIABLE(n)->type);
    V_VARIABLE(n)->type = type;
    ref_fulltype_data(&V_VARIABLE(n)->type);
} /* redeclare_variable() */

/*-------------------------------------------------------------------------*/
static int
verify_declared (ident_t *p)

/* Check that <p> is a global variable.
 * If yes, return the index of that variable, -1 otherwise.
 */

{
    int r;

    if (p->type != I_TYPE_GLOBAL
     || (r = p->u.global.variable) < 0)
    {
        yyerrorf("Variable %s not declared !", get_txt(p->name));
        return -1;
    }

    return r;
} /* verify_declared() */

/*-------------------------------------------------------------------------*/
static int
define_global_variable (ident_t* name, fulltype_t actual_type, typeflags_t opt_star, Bool with_init)

/* This is called directly from a parser rule: <type> [*] <name>
 * if with_init is true, then an initialization of this variable will follow.
 * It creates the global variable and returns its index.
 */

{
    int i;
    
    variables_defined = MY_TRUE;

    if (!(actual_type.typeflags & (TYPE_MOD_PRIVATE | TYPE_MOD_PUBLIC
                          | TYPE_MOD_PROTECTED)))
    {
        actual_type.typeflags |= default_varmod;
    }

    if (actual_type.typeflags & TYPE_MOD_VARARGS)
    {
        yyerror("can't declare a variable as varargs");
            actual_type.typeflags &= ~TYPE_MOD_VARARGS;
    }
  
    actual_type.typeflags |= opt_star;

    if (!pragma_share_variables)
        actual_type.typeflags |= VAR_INITIALIZED;

    define_variable(name, actual_type);
    i = verify_declared(name); /* Is the var declared? */

#ifdef DEBUG
    if (i == -1)
        fatal("Variable not declared after defining it.\n");
#endif

    /* Initialize float values with 0.0. */
    if (with_init
       || (!(actual_type.typeflags & TYPE_MOD_POINTER)
         && (actual_type.typeflags & PRIMARY_TYPE_MASK) == TYPE_FLOAT
          ))
    {

        /* Prepare the init code */
        transfer_init_control();

        /* If this is the first variable initialization and
         * pragma_share_variables is in effect, insert
         * the check for blueprint/clone initialisation:
         *    if (clonep(this_object())) return 1;
         */
        if (!variables_initialized && pragma_share_variables)
        {
            ins_f_code(F_THIS_OBJECT);
            ins_f_code(F_CLONEP);
            ins_f_code(F_BRANCH_WHEN_ZERO);
            ins_byte(2);
            ins_f_code(F_CONST1);
            ins_f_code(F_RETURN);
        }

        /* Initialize floats with 0.0 */
        if(!with_init)
        {
            PREPARE_INSERT(5)
            /* Must come after the non-local program code inserts! */

            add_f_code(F_FCONST0);

#ifdef DEBUG
            if (i & VIRTUAL_VAR_TAG)
            {
                /* When we want to allow 'late' initializers for
                 * inherited variables, it must have a distinct syntax,
                 * lest name clashs remain undetected, making LPC code
                 * hard to debug.
                 */
                fatal("Newly declared variable is virtual\n");
            }
#endif
            variables_initialized = MY_TRUE; /* We have __INIT code */
            if (!pragma_share_variables)
                VARIABLE(i)->type.typeflags |= VAR_INITIALIZED;

            /* Push the variable reference and create the assignment */

            if (i + num_virtual_variables > 0xff)
            {
                add_f_code(F_PUSH_IDENTIFIER16_LVALUE);
                add_short(i + num_virtual_variables);
                CURRENT_PROGRAM_SIZE += 1;
            }
            else
            {
                add_f_code(F_PUSH_IDENTIFIER_LVALUE);
                add_byte(i + num_virtual_variables);
            }

            /* Ok, assign */
            add_f_code(F_VOID_ASSIGN);
            CURRENT_PROGRAM_SIZE += 4;
            add_new_init_jump();
        } /* PREPARE_INSERT() block */
    } /* if (float variable) */

    return i;
} /* define_global_variable() */

/*-------------------------------------------------------------------------*/
static void
init_global_variable (int i, ident_t* name, fulltype_t actual_type
                     , typeflags_t opt_star, int assign_op, fulltype_t exprtype)

/* This is called directly from a parser rule: <type> [*] <name> = <expr>
 * It will be called after the call to define_global_variable().
 * It assigns the result of <expr> to the variable.
 */

{
    PREPARE_INSERT(4)

    if (!(actual_type.typeflags & (TYPE_MOD_PRIVATE | TYPE_MOD_PUBLIC
                          | TYPE_MOD_PROTECTED)))
    {
        actual_type.typeflags |= default_varmod;
    }

    actual_type.typeflags |= opt_star;

#ifdef DEBUG
    if (i & VIRTUAL_VAR_TAG)
    {
        /* When we want to allow 'late' initializers for
         * inherited variables, it must have a distinct syntax,
         * lest name clashs remain undetected, making LPC code
         * hard to debug.
         */
        fatal("Newly declared variable is virtual\n");
    }
#endif
    variables_initialized = MY_TRUE; /* We have __INIT code */

    /* Push the variable reference and create the assignment */

    if (i + num_virtual_variables > 0xff)
    {
        add_f_code(F_PUSH_IDENTIFIER16_LVALUE);
        add_short(i + num_virtual_variables);
        CURRENT_PROGRAM_SIZE += 1;
    }
    else
    {
        add_f_code(F_PUSH_IDENTIFIER_LVALUE);
        add_byte(i + num_virtual_variables);
    }

    /* Only simple assigns are allowed */
    if (assign_op != F_ASSIGN)
       yyerror("Illegal initialization");

    /* Do the types match? */
    actual_type.typeflags &= TYPE_MOD_MASK;
    if (!compatible_types(actual_type, exprtype, MY_TRUE))
    {
        yyerrorf("Type mismatch %s when initializing %s"
                , get_two_types(actual_type, exprtype)
                , get_txt(name->name));
    }

    /* Ok, assign */
    add_f_code(F_VOID_ASSIGN);
    CURRENT_PROGRAM_SIZE += 3;
    add_new_init_jump();
} /* init_global_variable() */

/*-------------------------------------------------------------------------*/
static void
store_function_header ( p_int start
                      , string_t * name, unsigned short fx
                      , fulltype_t returntype, int num_args, int num_vars
                      )

/* Store a function header into the program block at address <start>.
 * The caller has to make sure that there is enough space.
 * The references of <returntype> are adopted.
 * <fx> is the index of the function in the defining program.
 */

{
    bytecode_p p;
    vartype_t  rtype;

    p = &(PROGRAM_BLOCK[start]);

    /* FUNCTION_NAME */
    memcpy(p, &name, sizeof name);
    p += sizeof name;
    (void)ref_mstring(name);

    // FUNCTION_INDEX
    memcpy(p, &fx, sizeof fx);
    p+= sizeof fx;
    
    /* FUNCTION_TYPE */
    assign_full_to_vartype(&rtype, returntype);
    memcpy(p, &rtype, sizeof(rtype));
    p += sizeof(rtype);

    /* FUNCTION_NUM_ARGS */
    if (returntype.typeflags & TYPE_MOD_XVARARGS)
      *p++ = num_args | ~0x7f;
    else
      *p++ = num_args;

    /* FUNCTION_NUM_VARS */
    *p   = num_vars;
} /* store_function_header() */

/*-------------------------------------------------------------------------*/
static void
get_function_information (function_t * fun_p, program_t * prog, int ix)

/* Read the function information for function <ix> in program <prog>
 * (which may be inherited) and store it in *<fun_p>. It is the callers
 * responsibility to set <fun_p>->flags _before_ calling this function.
 *
 * In particular, this function sets these <fun_p> fields: .name, .rtype
 * .num_args, and it modifies .flags.
 */

{
    fun_hdr_p funstart;
    vartype_t rtype;
    funflag_t flags;

    /* Find the real function code */
    while ( (flags = prog->functions[ix]) & NAME_INHERITED)
    {
        inherit_t * ip;
        ip = &prog->inherit[flags & INHERIT_MASK];
        ix -= ip->function_index_offset;
        prog = ip->prog;
    }
    funstart = &prog->program[flags & FUNSTART_MASK];
    memcpy(&fun_p->name, FUNCTION_NAMEP(funstart), sizeof fun_p->name);

    memcpy(&rtype, FUNCTION_TYPEP(funstart), sizeof(rtype));
    assign_var_to_fulltype(&fun_p->type, rtype);
    ref_fulltype_data(&fun_p->type);
    fun_p->num_arg = FUNCTION_NUM_ARGS(funstart) & 0x7f;
    if (FUNCTION_NUM_ARGS(funstart) & ~0x7f)
        fun_p->type.typeflags |= TYPE_MOD_XVARARGS;
    if (FUNCTION_CODE(funstart)[0] == F_UNDEF)
    {
        fun_p->flags |= NAME_UNDEFINED;
    }
} /* get_function_information() */

/*-------------------------------------------------------------------------*/
static void
def_function_typecheck (fulltype_t returntype, ident_t * ident, Bool is_inline)

/* Called after parsing the '<type> <functionname>' part of a function
 * definition, this function performs the typecheck, makes sure that
 * the function name is put into the list of globals, and initialises
 * the block scoping.
 *
 * If <is_inline> is TRUE, the function to be compiled is an inline closure,
 * which requires a slightly different handling. This function is called
 * after 'func <type>' has been parsed, and is provided with a synthetic
 * function name.
 */

{
    if (is_inline)
    {
        new_inline_closure();
        enter_block_scope(); /* Scope for context */
        enter_block_scope(); /* Argument scope */
    }
    else
    {
        block_depth = 1;
        init_scope(block_depth);
    }

    if (!(returntype.typeflags & (TYPE_MOD_PRIVATE | TYPE_MOD_PUBLIC
                                  | TYPE_MOD_PROTECTED | TYPE_MOD_STATIC)))
    {
        returntype.typeflags |= default_funmod;
    }

    /* Require exact types? */
    if (returntype.typeflags & TYPE_MOD_MASK)
    {
        exact_types = returntype;
    }
    else
    {
        if (pragma_strict_types != PRAGMA_WEAK_TYPES)
            yyerrorf("\"#pragma %s_types\" requires type of function"
                    , pragma_strict_types == PRAGMA_STRICT_TYPES
                      ? "strict" : "strong" );
        exact_types.typeflags = 0;
    }

    if (returntype.typeflags & TYPE_MOD_NOSAVE)
    {
        yyerror("can't declare a function as nosave");
        returntype.typeflags &= ~TYPE_MOD_NOSAVE;
    }

    if (ident->type == I_TYPE_UNKNOWN)
    {
        /* prevent freeing by exotic name clashes */
        init_global_identifier(ident, /* bVariable: */ MY_TRUE);
        ident->next_all = all_globals;
        all_globals = ident;
    }

    /* Store the data */
    if (is_inline)
    {
        current_inline->ident = ident;
        current_inline->returntype = returntype;
    }
    else
    {
        def_function_returntype = returntype;
        def_function_ident = ident;
    }
} /* def_function_typecheck() */

/*-------------------------------------------------------------------------*/
static void
def_function_prototype (int num_args, Bool is_inline)

/* Called after parsing '<type> <name> ( <args> ) of a function definition,
 * this function creates the function prototype entry.
 *
 * If <is_inline> is TRUE, the function to be compiled is an inline closure,
 * which requires a slightly different handling. This function is called
 * after 'func <type> <arguments> <context>' has been parsed.
 */

{
    ident_t * ident;
    fulltype_t returntype;
    int fun;

    if (is_inline)
    {
        ident = current_inline->ident;
        returntype = current_inline->returntype;
    }
    else
    {
        ident = def_function_ident;
        returntype = def_function_returntype;
    }

    /* We got the complete prototype: define it */

    if ( current_number_of_locals
     && (type_of_locals[current_number_of_locals-1].typeflags
         & TYPE_MOD_VARARGS)
       )
    {
        /* The last argument has to allow an array. */
        fulltype_t *t;

        returntype.typeflags |= TYPE_MOD_XVARARGS;

        t = type_of_locals + (current_number_of_locals-1);
        if (!(t->typeflags & TYPE_MOD_POINTER)
         && (t->typeflags & TYPE_MOD_RMASK) != TYPE_ANY
           )
        {
            if ((t->typeflags & TYPE_MOD_RMASK) != TYPE_UNKNOWN)
                yyerror(
                  "varargs parameter must be declared array or mixed");
            /* Keep the visibility, but change the type to
             * '&any'
             */
            t->typeflags &= ~TYPE_MOD_RMASK;
            t->typeflags |= TYPE_ANY;
        }
    }

    /* Define a prototype. If it is a real function, then the
     * prototype will be updated below.
     */
    fun = define_new_function( MY_FALSE, ident, num_args, 0, 0
                             , NAME_UNDEFINED|NAME_PROTOTYPE
                             , returntype);

    /* Store the data */
    if (is_inline)
    {
        current_inline->returntype = returntype;
        current_inline->num_args = num_args;
        current_inline->function = fun;
    }
    else
    {
        def_function_returntype = returntype;
        def_function_num_args = num_args;
    }
} /* def_function_prototype() */

/*-------------------------------------------------------------------------*/
static void
def_function_complete ( p_int body_start, Bool is_inline)

/* Called after completely parsing a function definition,
 * this function updates the function header and closes all scopes..
 * Argument is the program index where the space for the function header
 * was made, or -1 if there was no body.
 *
 * If <is_inline> is TRUE, the function to be compiled is an inline closure,
 * which requires a slightly different handling. This function is called
 * after the complete closure has been parsed.
 */

{
    ident_t    * ident;
    fulltype_t   returntype;
    int          num_args;

    if (is_inline)
    {
        ident = current_inline->ident;
        returntype = current_inline->returntype;
        num_args = current_inline->num_args;
    }
    else
    {
        ident = def_function_ident;
        returntype = def_function_returntype;
        num_args = def_function_num_args;
    }

    if (body_start < 0)
    {
        /* function_body was a ';' -> prototype
         * Just norm the visibility flags unless it is a prototype
         * for an already inherited function.
         */

        funflag_t *flagp;

        flagp = (funflag_t *)(&FUNCTION(ident->u.global.function)->flags);
        if (!(*flagp & NAME_INHERITED))
        {
            *flagp |= returntype.typeflags
                      & (*flagp & TYPE_MOD_PUBLIC
                        ? (TYPE_MOD_NO_MASK)
                        : (TYPE_MOD_NO_MASK|TYPE_MOD_PRIVATE
                          |TYPE_MOD_STATIC|TYPE_MOD_PROTECTED
                          |TYPE_MOD_PUBLIC)
                        );
        }
    }
    else
    {
        /* function_body was a block: generate the
         * function header and update the ident-table entry.
         */
        int fx; // index of new function in the program
        int num_vars = max_number_of_locals - num_args
                                            + max_break_stack_need;

        fx = define_new_function(MY_TRUE, ident
                            , num_args
                            , num_vars
                            , body_start + FUNCTION_PRE_HDR_SIZE
                            , 0, returntype);

        store_function_header( body_start
                             , ident->name
                             , fx
                             , returntype
                             , num_args
                             , num_vars
                             );


        /* Catch a missing return if the function has a return type */
        if ((returntype.typeflags & PRIMARY_TYPE_MASK) != TYPE_VOID
         && (   (returntype.typeflags & PRIMARY_TYPE_MASK) != TYPE_UNKNOWN
             || pragma_strict_types
            )
           )
        {
            /* Check if the previous instruction is a RETURN, or
             * at least a non-continuing instruction.
             */
            bytecode_t last = PROGRAM_BLOCK[CURRENT_PROGRAM_SIZE-1];

            if (F_RETURN == last || F_RETURN0 == last
             || F_RAISE_ERROR == last || F_THROW == last
               )
            {
                /* Good, the last instruction seems to be a 'return'.
                 * But just in case we're looking at the data field
                 * of a different opcode or a conditional return: insert a
                 * proper default return as well.
                 */
                if (pragma_warn_missing_return)
                    ins_f_code(F_DEFAULT_RETURN);
                else
                    ins_f_code(F_RETURN0);
            }
            else
            {
                /* There is no 'return' here: most likely it is missing
                 * altogether.
                 * If warn_missing_return is enabled, issue the warning,
                 * but always insert a normal F_RETURN0: with the pragma
                 * active it's no use to warn again at runtime, and without
                 * the pragma no warning is desired anyway.
                 */
                if (pragma_warn_missing_return)
                    yywarnf("Missing 'return <value>' statement");

                ins_f_code(F_RETURN0);
            }
        }
        else
        {
            ins_f_code(F_RETURN0);
        }
    }

    /* Clean up for normal functions.
     * Do not free the function returntype - it is still held in A_FUNCTIONS
     * and freed after the compile.
     * Inline closures need some of the information for some more processing.
     */
    if (is_inline)
    {
        /* Keep block_depth, and local names */
    }
    else
    {
        free_all_local_names();
        block_depth = 0;
    }

} /* def_function_complete() */

/* =============================   STRUCTS   ============================= */

/*-------------------------------------------------------------------------*/
static int
define_new_struct ( Bool proto, ident_t *p, funflag_t flags)

/* Define a new struct <p> with the visibility <flags>.
 * If <proto> is TRUE, the function is called for a struct forward
 * declaration; if <proto> is FALSE, the struct is about to be defined.
 *
 * Result is the index (id) of the struct in the struct_defs table.
 * If the struct would be a duplicate, -1 is returned instead of the index.
 *
 * If a prototype is encountered, the struct definition is stored
 * with an additional visibility flag of NAME_PROTOTYPE.
 *
 * If NAME_HIDDEN is set in flags, the struct is added to the program
 * but no visibility checks occur - this is for inherited structs
 * which are no longer visible, but have to be kept in order to
 * keep the struct ids intact.
 */

{
    int          num;
    struct_def_t sdef;

    /* If this is a redeclaration, check for consistency. */
    if (p->type == I_TYPE_GLOBAL && (num = p->u.global.struct_id) >= 0
     && !(flags & NAME_HIDDEN)
      )
    {
        struct_def_t *pdef;

        pdef = &STRUCT_DEF(num);

        /* Check if the visibility is conserved.
         */
        {
#            define TYPE_MOD_VIS \
            ( TYPE_MOD_NO_MASK \
            | TYPE_MOD_PRIVATE | TYPE_MOD_PUBLIC \
            | TYPE_MOD_PROTECTED)
            funflag_t f1 = pdef->flags;
            funflag_t f2 = flags;

            /* Smooth out irrelevant differences */
            if (f1 & TYPE_MOD_STATIC) f1 |= TYPE_MOD_PROTECTED;
            if (f2 & TYPE_MOD_STATIC) f2 |= TYPE_MOD_PROTECTED;

            if ( ((f1 ^ f2) & TYPE_MOD_VIS) )
            {
                char buff[120];

                strncpy(buff, get_f_visibility(pdef->flags), sizeof(buff)-1);
                buff[sizeof(buff)-1] = '\0'; // strncpy() does not guarantee NUL termination
                yywarnf("Inconsistent declaration of struct %s: "
                        "Visibility changed from '%s' to '%s'"
                       , get_txt(p->name), buff, get_f_visibility(flags));
            }
#           undef TYPE_MOD_VIS
        }

        /* If this is just another prototype, return */
        if (proto)
            return num;

        /* If this is a redefinition of a completed struct, complain
         * and return.
         */
        if (!proto && !(pdef->flags & NAME_PROTOTYPE))
        {
            yyerrorf("Duplicate definition of struct %s"
                    , get_txt(p->name));
            return -1;
        }

        /* At this point, we have in our hands the definition of a
         * previously just declared struct.
         * Update the stored information and return its index.
         */
        pdef->flags = flags & ~NAME_PROTOTYPE;

        return num;
    }

    /* This is a new struct! */

    /* Fill in the struct_def_t */
    sdef.type  = struct_new_prototype(ref_mstring(p->name));
    sdef.flags = proto ? (flags | NAME_PROTOTYPE)
                       : (flags & ~NAME_PROTOTYPE);
    sdef.inh = -1;

    num = STRUCT_COUNT;

    if (p->type != I_TYPE_GLOBAL)
    {
        /* This is the first _GLOBAL use of this identifier:
         * make an appropriate entry in the identifier table.
         */

        if (p->type != I_TYPE_UNKNOWN)
        {
            /* The ident has been used before otherwise, so
             * get a fresh structure.
             */
            p = make_shared_identifier_mstr(p->name, I_TYPE_GLOBAL, 0);
        }
        /* should be I_TYPE_UNKNOWN now. */

        init_global_identifier(p, /* bVariable: */ MY_FALSE);
        p->next_all = all_globals;
        all_globals = p;
    }

    if  (!(flags & NAME_HIDDEN))
        p->u.global.struct_id = num;

    /* Store the function_t in the functions area */
    add_to_mem_block(A_STRUCT_DEFS, &sdef, sizeof sdef);

    return num;
} /* define_new_struct() */

/*-------------------------------------------------------------------------*/
static int
find_struct ( string_t * name )

/* Find the struct <name> and return its index. Return -1 if not found.
 */

{
    ident_t * p;

    p = find_shared_identifier_mstr(name, I_TYPE_GLOBAL, 0);

    /* Find the global struct identifier */
    while (p != NULL && p->type != I_TYPE_GLOBAL)
        p = p->inferior;

    if (p == NULL || p->u.global.struct_id < 0)
        return -1;
    if (STRUCT_DEF(p->u.global.struct_id).flags & NAME_HIDDEN)
        return -1;
    return p->u.global.struct_id;
} /* find_struct() */

/*-------------------------------------------------------------------------*/
static void
add_struct_member ( string_t *name, vartype_t type
                  , struct_type_t * from_struct )

/* Add a new member <name> with type <type> to A_STRUCT_MEMBERS for the
 * to the most recently defined struct <current_struct>.
 * If <from_struct> is not NULL, it is the type of the struct from
 * which the member is inherited.
 * Raise an error if a member of the same name already exists.
 */

{
    struct_def_t *pdef;

    pdef = &STRUCT_DEF(current_struct);

    if (STRUCT_MEMBER_COUNT != 0)
    {
        /* Not the first member: check if the name already occured */
        int i;

        for ( i = STRUCT_MEMBER_COUNT-1 ; i >= 0 ; i--)
        {
            if (mstreq(name, STRUCT_MEMBER(i).name))
            {
                if (pdef->type->base
                 && pdef->type->base->num_members > i
                   )
                    yyerrorf("Duplicate member '%s' in struct '%s', "
                             "inherited from struct '%s'"
                            , get_txt(name)
                            , get_txt(struct_t_name(pdef->type))
                            , get_txt(struct_t_name(pdef->type->base))
                            );
                else
                    yyerrorf("Duplicate member '%s' in struct '%s'"
                            , get_txt(name)
                            , get_txt(struct_t_name(pdef->type))
                            );
                return;
            }
        }
    }

    /* Member is ok: add it */
    if (STRUCT_MEMBER_COUNT == STRUCT_MAX_MEMBERS)
    {
        yyerrorf("Too many members for struct '%s'"
                , get_txt(struct_t_name(pdef->type)));
    }
    else
    {
        struct_member_t member;

        member.name = ref_mstring(name);
        member.type = type;
        ref_vartype_data(&member.type);
        add_to_mem_block(A_STRUCT_MEMBERS, &member, sizeof member);
    }
} /* add_struct_member() */

/*-------------------------------------------------------------------------*/
static void
finish_struct ( const char * prog_name, int32 prog_id)

/* The definition for struct <current_struct> has been parsed completely,
 * now complete the struct type object with the A_STRUCT_MEMBERS data.
 */

{
    struct_def_t *pdef;
    struct_type_t *base;
    string_t *name;

    pdef = &STRUCT_DEF(current_struct);

    /* Retrieve the .base pointer so that the error handling won't
     * get confused about it.
     * Also get a safety copy of the name.
     */
    base = pdef->type->base;
    pdef->type->base = NULL;
    name = ref_mstring(struct_t_name(pdef->type));

    /* Fill in the prototype */
    pdef->type = struct_fill_prototype(pdef->type
                                      , new_tabled(prog_name)
                                      , prog_id
                                      , base
                                      , STRUCT_MEMBER_COUNT
                                      , &STRUCT_MEMBER(0)
                                      );

    if (pdef->type)
    {
        /* Success: Free the safety copies */
        free_mstring(name);
    }
    else
    {
        /* Failure: Recreate the prototype as the old one got deleted */
        pdef->type = struct_new_prototype(name);
    }

    /* Clear the STRUCT_MEMBER block - the definitions have already
     * been adopted or cleared by the struct_fill_prototype().
     */
    mem_block[A_STRUCT_MEMBERS].current_size = 0;
} /* finish_struct() */

/*-------------------------------------------------------------------------*/
static Bool
create_struct_literal ( struct_def_t * pdef, int length, struct_init_t * list)

/* The compiler has created code for <length> expressions in order
 * to create a struct literal of struct <pdef>.
 * Analyze the <list> of member descriptions and generate the appropriate
 * bytecode.
 *
 * Return TRUE on success, and FALSE if an error occured (the caller will
 * then clean up the bytecode).
 */

{
    struct_init_t * p;
    struct_member_t * pmember;
    void * block;    /* Allocation block for flags and index */
    Bool * flags;    /* Flag: which struct members have been set */
    int  * ix;       /* For each expr in order, list the struct member index */
    int    consumed; /* To check if we used all elements */
    int    count, member;
    int    i;
    Bool got_error = MY_FALSE;

    /* Check if there is one member assigned by name */
    for (p = list; p != NULL; p = p->next)
        if (p->name != NULL)
            break;

    if (length == 0 || p == NULL)
    {
        /* Simplest case: all members assigned by position. */

        /* Check the types */
        if (exact_types.typeflags && length > 0)
        {
            for (member = 0, pmember = pdef->type->member, p = list
                ; member < length && member < struct_t_size(pdef->type)
                ; member++, pmember++, p = p->next
                )
            {
                if (!vtype(pmember->type, p->type) )
                {
                    yyerrorf("Type mismatch %s for member '%s' "
                             "in struct '%s'"
                            , get_two_vtypes(pmember->type, p->type)
                            , get_txt(pmember->name)
                            , get_txt(struct_t_name(pdef->type))
                            );
                    got_error = MY_TRUE;
                }
            }

            if (got_error)
                return MY_FALSE;
        }

        /* The types check out - create the bytecode */
        ins_f_code(F_S_AGGREGATE);
        ins_short(pdef - &STRUCT_DEF(0));
        ins_byte(length);

        return MY_TRUE;
    }

    /* We have named members in there - sort them out */

    consumed = 0;

    block = xalloc( struct_t_size(pdef->type) * sizeof(*flags)
                  + length * sizeof(*ix));
    flags = (Bool *)block;
    ix = (int *)((char *)block + struct_t_size(pdef->type) * sizeof(*flags));

    for (i = 0; i < struct_t_size(pdef->type); i++)
    {
        flags[i] = MY_FALSE;
    }

    for (i = 0; i < length; i++)
    {
        ix[i] = -1;
    }

    /* Loop through list: assign the named members.
     */
    for (p = list, count = 0; p != NULL; p = p->next, count++)
    {

        if (p->name == NULL)
        {
            if (!got_error)
            {
                yyerrorf( "Can't mix named and unnamed initializers "
                          "in struct '%s'"
                        , get_txt(struct_t_name(pdef->type))
                        );
                got_error = MY_TRUE;
            }
            continue;
        }

        consumed++;
        pmember = NULL; /* avoids a warning */
        member = struct_find_member(pdef->type, p->name);
        if (member >= 0)
            pmember = &pdef->type->member[member];

        if (member < 0)
        {
            yyerrorf( "No such member '%s' in struct '%s'"
                    , get_txt(p->name)
                    , get_txt(struct_t_name(pdef->type))
                    );
            got_error = MY_TRUE;
        }
        else if (flags[member])
        {
            yyerrorf( "Multiple initializations of member '%s' "
                      "in struct '%s'"
                    , get_txt(p->name)
                    , get_txt(struct_t_name(pdef->type))
                    );
            got_error = MY_TRUE;
        }
        else if (exact_types.typeflags
              && !vtype( pmember->type , p->type) )
        {
            yyerrorf("Type mismatch %s when initializing member '%s' "
                     "in struct '%s'"
                    , get_two_vtypes(pmember->type, p->type)
                    , get_txt(p->name)
                    , get_txt(struct_t_name(pdef->type))
                    );
            got_error = MY_TRUE;
        }
        else
        {
            flags[member] = MY_TRUE;
            ix[count] = member;
        }
    } /* for() */

    if (got_error)
    {
        xfree(block);
        return MY_FALSE;
    }

    /* Sanity checks */

    if (consumed < length)
    {
        yyerrorf("Too many elements for struct '%s'"
                , get_txt(struct_t_name(pdef->type))
                );
        xfree(block);
        return MY_FALSE;
    }

    for (i = 0; i < length; i++)
    {
        if (ix[i] < 0)
        {
            fatal("struct literal: expression %d not assigned to any member.\n"
                 , i);
            /* NOTREACHED */
        }
    }

    /* Finally, create the code */
    ins_f_code(F_S_M_AGGREGATE);
    ins_short(pdef - &STRUCT_DEF(0));
    ins_byte(length);
    for (i = length-1; i >= 0; i--)
        ins_byte(ix[i]);

    /* Done */
    xfree(block);

    return MY_TRUE;
} /* create_struct_literal() */

/*-------------------------------------------------------------------------*/
static short
get_struct_index (struct_type_t * pType)

/* Return the index of struct type <pType> in this program's A_STRUCT_DEFS.
 * Return -1 if not found.
 */

{
    short i;

    for (i = 0; (size_t)i < STRUCT_COUNT; i++)
    {
        if (STRUCT_DEF(i).type == pType)
            return i;
    }
    return -1;
} /* get_struct_index() */

/*-------------------------------------------------------------------------*/
static short
find_struct_by_member (string_t * name, int * pNum)

/* Among the structs known by this program, find the (smallest) one
 * defining member <name>.
 * Result:
 *   >= 0: Index of the struct in A_STRUCT_DEFS, *<pNum> index of the member
 *   FSM_NO_STRUCT (-1): No struct with such a member
 *   FSM_AMBIGUOUS (-2): Multiple unrelated structs define the member
 * In case of the errors, the function also issues a compiler error.
 */

#define FSM_NO_STRUCT (-1)
#define FSM_AMBIGUOUS (-2)

{
    short rc = FSM_NO_STRUCT;
    struct_def_t * pRC = NULL;
    int  member = -1;
    int i;

    for (i = 0; (size_t)i < STRUCT_COUNT; i++)
    {
        int num;
        struct_def_t * pdef = &STRUCT_DEF(i);

        /* If we already found a struct, check if this one is
         * a relative. If yes, we can immediately continue
         * to the next.
         */
        if (pRC != NULL)
        {
            struct_type_t * pTest;

            for ( pTest = pdef->type
                ; pTest != NULL && pTest != pRC->type
                ; pTest = pTest->base
                )
              NOOP;

            if (pTest != NULL)
                continue;
        }

        /* Lookup the member in this struct. If not found,
         * continue to the next struct.
         */
        num = struct_find_member(pdef->type, name);
        if (num < 0)
            continue;

        /* If we already found a struct, check if this one is
         * a relative.
         */
        if (pRC != NULL)
        {
            struct_type_t * pTest;

            /* Is the newly found struct a child of the
             * one we already know? If yes, skip it.
             */
            for ( pTest = pdef->type
                ; pTest != NULL && pTest != pRC->type
                ; pTest = pTest->base
                )
              NOOP;

            if (pTest != NULL)
                continue;

            /* Is the newly found struct a parent of
             * the one we already know? If yes, keep it
             * instead of the one we have; if no, the two
             * structs are completely unrelated and the
             * lookup is ambiguous.
             */

            for ( pTest = pRC->type
                ; pTest != NULL && pTest != pdef->type
                ; pTest = pTest->base
                )
              NOOP;

            if (pTest == NULL)
            {
                yyerrorf("Multiple structs found for member '%s': "
                         "struct %s, struct %s"
                        , get_txt(name)
                        , get_txt(struct_t_name(pRC->type))
                        , get_txt(struct_t_name(pdef->type))
                        );
                *pNum = -1;
                return FSM_AMBIGUOUS;
            }
        } /* if (pRC) */

        /* It's a successful lookup */
        rc = i;
        pRC = pdef;
        member = num;
    } /* for (all structs) */

    if (rc < 0)
    {
        yyerrorf("No struct found for member '%s'", get_txt(name));
    }

    *pNum = member;
    return rc;
} /* find_struct_by_member() */

/*-------------------------------------------------------------------------*/
static void
struct_epilog (void)

/* After a successful parse, make sure that all structs are defined,
 * try to reactivate existing structs, and publish the new ones.
 *
 * If an error occures, num_parse_error will be incremented.
 */

{
    int i;

    /* Check that all structs are defined.
     */
    for (i = 0; (size_t)i < STRUCT_COUNT; i++)
    {
        if (STRUCT_DEF(i).flags & NAME_PROTOTYPE)
        {
            yyerrorf("struct '%s' defined just as prototype"
                    , get_txt(struct_t_name(STRUCT_DEF(i).type))
                    );
            return;
        }
    }

    /* For all structs defined in this program, check if they just
     * replicate an existing older struct.
     */
    for (i = 0; (size_t)i < STRUCT_COUNT; i++)
    {
        struct_type_t *pSType = STRUCT_DEF(i).type;
        struct_type_t *pOld;
        int ii;

        if (STRUCT_DEF(i).inh >= 0)
            continue;

        pOld = struct_lookup_type(pSType);
        if (!pOld || !struct_type_equivalent(pSType, pOld))
            continue;

        /* pOld has the same structure as pSType, so lets
         * replace the latter with the former.
         * First in the structs themselves.
         */
        for (ii = 0; (size_t)ii < STRUCT_COUNT; ii++)
        {
            if (ii != i)
                struct_type_update(STRUCT_DEF(ii).type, pSType, pOld); 
        }

        /* Update variable types */

        for (ii = 0; (size_t)ii < NV_VARIABLE_COUNT; ii++)
        {
            fulltype_t * pType = &NV_VARIABLE(ii)->type;

            if ((pType->typeflags & PRIMARY_TYPE_MASK) == TYPE_STRUCT
             && pType->t_struct == pSType
               )
            {
                free_struct_type(pType->t_struct);
                pType->t_struct = ref_struct_type(pOld);
            }
        }

        for (ii = 0; (size_t)ii < V_VARIABLE_COUNT; ii++)
        {
            fulltype_t * pType = &V_VARIABLE(ii)->type;

            if ((pType->typeflags & PRIMARY_TYPE_MASK) == TYPE_STRUCT
             && pType->t_struct == pSType
               )
            {
                free_struct_type(pType->t_struct);
                pType->t_struct = ref_struct_type(pOld);
            }
        }

        /* Update the function return types */
        {
            int num_functions = FUNCTION_COUNT;
            function_t * f = (function_t *)mem_block[A_FUNCTIONS].block;

            for (ii = num_functions; --ii >= 0; f++)
            {
                /* Ignore all functions but those actually defined in
                 * this program.
                 */
                if (f->flags & (NAME_INHERITED|NAME_UNDEFINED|NAME_CROSS_DEFINED))
                    continue;

                if ((f->type.typeflags & PRIMARY_TYPE_MASK) == TYPE_STRUCT
                 && f->type.t_struct == pSType
                   )
                {
                    vartype_t type;
                    fun_hdr_p funhdr;

                    free_struct_type(f->type.t_struct);
                    f->type.t_struct = ref_struct_type(pOld);

                    funhdr = (fun_hdr_p)
                             &mem_block[A_PROGRAM].block[f->offset.pc];
                    memcpy(&type, FUNCTION_TYPEP(funhdr), sizeof(type));
                    type.t_struct = pOld;
                    memcpy(FUNCTION_TYPEP(funhdr), &type, sizeof(type));
                }
            } /* for(ii) */
        }

        /* Update function argument types */

        for (ii = 0; (size_t)ii < ARGTYPE_COUNT; ii++)
        {
            vartype_t * pType = &ARGUMENT_TYPE(ii);

            if ((pType->type & PRIMARY_TYPE_MASK) == TYPE_STRUCT
             && pType->t_struct == pSType
               )
            {
                free_struct_type(pType->t_struct);
                pType->t_struct = ref_struct_type(pOld);
            }
        }

        /* And finally, replace the struct in the A_STRUCT memblock */
        free_struct_type(pSType);
        STRUCT_DEF(i).type = ref_struct_type(pOld);
    } /* for(i) */

    /* Publish all struct types defined in this program.
     * It is safe to publish types twice.
     */
    for (i = 0; (size_t)i < STRUCT_COUNT; i++)
    {
        if (STRUCT_DEF(i).inh < 0)
            struct_publish_type(STRUCT_DEF(i).type);
    } /* for(i) */

} /* struct_epilog() */


/* =========================   Inline Closures   =-======================= */

/*-------------------------------------------------------------------------*/
static void
new_inline_closure (void)

/* Create a new inline closure structure and push it on top of the stack.
 */

{
    inline_closure_t ict;

    if (current_inline == NULL)
    {
        ict.prev = -1;
    }
    else
    {
        ict.prev = current_inline - &(INLINE_CLOSURE(0));
    }
#ifdef DEBUG_INLINES
printf("DEBUG: new inline #%"PRIuMPINT": prev %"PRIdMPINT"\n", INLINE_CLOSURE_COUNT, ict.prev);
#endif /* DEBUG_INLINES */

    /* Initialize the other fields */
    ict.end = CURRENT_PROGRAM_SIZE;
    ict.start = CURRENT_PROGRAM_SIZE;
    ict.length = 0;
    ict.li_start = LINENUMBER_SIZE;
    ict.li_length = 0;
    ict.function = -1;
    ict.ident = NULL;
    ict.returntype.typeflags = 0;
    ict.returntype.t_struct = NULL;
    ict.num_args = 0;
    ict.parse_context = MY_FALSE;
    ict.start_line = stored_lines;
    ict.end_line = stored_lines;

#ifdef DEBUG_INLINES
printf("DEBUG:   start: %"PRIuMPINT", depth %d, locals: %d/%d, break: %d/%d\n", 
       CURRENT_PROGRAM_SIZE, block_depth, current_number_of_locals, 
       max_number_of_locals, current_break_stack_need, max_break_stack_need);
#endif /* DEBUG_INLINES */
    ict.block_depth          = block_depth;
    ict.break_stack_size     = current_break_stack_need;
    ict.max_break_stack_size = max_break_stack_need;
    ict.num_locals           = current_number_of_locals;
    ict.max_num_locals       = max_number_of_locals;
    ict.exact_types          = exact_types;
    ict.include_handle       = get_include_handle();
    ict.full_local_type_start   = type_of_locals - &(LOCAL_TYPE(0));
    ict.full_context_type_start = type_of_context - &(LOCAL_TYPE(0));
    ict.full_local_type_size    = mem_block[A_LOCAL_TYPES].current_size;
#ifdef DEBUG_INLINES
printf("DEBUG:   local types: %"PRIuMPINT", context types: %"PRIuMPINT"\n", 
       ict.full_local_type_start, ict.full_context_type_start);
#endif /* DEBUG_INLINES */

    /* Extend the type memblocks */
    {
        mp_uint type_count = LOCAL_TYPE_COUNT;

        extend_mem_block(A_LOCAL_TYPES, 2 * MAX_LOCAL * sizeof(fulltype_t));
        memset(&LOCAL_TYPE(type_count), 0
              , (LOCAL_TYPE_COUNT - type_count) * sizeof(fulltype_t));

        type_of_context = &(LOCAL_TYPE(type_count));
        type_of_locals = &(LOCAL_TYPE(type_count+MAX_LOCAL));
#ifdef DEBUG_INLINES
printf("DEBUG:   type ptrs: %p, %p\n", 
       type_of_locals, type_of_context );
#endif /* DEBUG_INLINES */
    }

    max_break_stack_need = current_break_stack_need = 0;
    max_number_of_locals = current_number_of_locals = 0;

    /* Add the structure to the memblock */
    add_to_mem_block(A_INLINE_CLOSURE, &ict, sizeof(ict));
    current_inline = &(INLINE_CLOSURE(INLINE_CLOSURE_COUNT-1));
} /* new_inline_closure() */

/*-------------------------------------------------------------------------*/
static void
finish_inline_closure (Bool bAbort)

/* The compilation of the current inline closure is finished - move
 * everything out of the way of the ongoing compilation.
 * Note that only the codeblock .start/.length is saved; if there is
 * already code generated afterwards, it is moved forward. Ditto for
 * the linenumbers.
 *
 * If <bAbort> is TRUE, the closure is just finished, but not stored.
 */

{
    mp_uint backup_start, start, length, end;
    int offset;
    
#ifdef DEBUG_INLINES
{
    mp_int index = current_inline - &(INLINE_CLOSURE(0));
printf("DEBUG: %s inline #%"PRIdMPINT": prev %"PRIdMPINT", end %"PRIuMPINT
       ", start %"PRIuMPINT", length %"PRIuMPINT", function %d pc %"PRIu32"\n", 
       bAbort ? "abort" : "finish", index, current_inline->prev, 
       current_inline->end, current_inline->start, current_inline->length, 
       current_inline->function, FUNCTION(current_inline->function)->offset.pc);
printf("DEBUG:   depth %d, locals: %d/%d, break: %d/%d\n", 
       current_inline->block_depth, current_inline->num_locals, 
       current_inline->max_num_locals, current_inline->break_stack_size, 
       current_inline->max_break_stack_size);
}
#endif /* DEBUG_INLINES */

    /* Move the program code into the backup storage */
    start = current_inline->start;
    length = current_inline->length;
    end = current_inline->end;

    if (!bAbort)
    {
        backup_start = INLINE_PROGRAM_SIZE;
#ifdef DEBUG_INLINES
printf("DEBUG:   move code to backup %"PRIuMPINT"\n", backup_start);
#endif /* DEBUG_INLINES */
        add_to_mem_block( A_INLINE_PROGRAM, PROGRAM_BLOCK+start, length);
        current_inline->start = backup_start;
    }
    else
    {
        current_inline->length = 0; /* Marks this one invalid */
    }

    if (start + length < CURRENT_PROGRAM_SIZE)
    {
#ifdef DEBUG_INLINES
printf("DEBUG:   move code forward: from %"PRIuMPINT", length %"PRIuMPINT
       ", to %"PRIuMPINT"\n", 
       start+length, CURRENT_PROGRAM_SIZE - length - start, end);
#endif /* DEBUG_INLINES */
        memmove( PROGRAM_BLOCK+end
               , PROGRAM_BLOCK+start+length
               , CURRENT_PROGRAM_SIZE - length - start
               );
    }
    CURRENT_PROGRAM_SIZE -= length + (start - end);
    stored_bytes -= length + (start - end);

#ifdef DEBUG_INLINES
printf("DEBUG:   program size: %"PRIuMPINT"\n", CURRENT_PROGRAM_SIZE);
#endif /* DEBUG_INLINES */

    /* Move the linenumber data into the backup storage */
    start = current_inline->li_start;
    length = current_inline->li_length;
    if (!bAbort)
    {
        backup_start = INLINE_PROGRAM_SIZE;
#ifdef DEBUG_INLINES
printf("DEBUG:   move li data to %"PRIuMPINT", from %"PRIuMPINT" length %"
        PRIuMPINT"\n", 
        backup_start, start, length);
#endif /* DEBUG_INLINES */
        add_to_mem_block( A_INLINE_PROGRAM, LINENUMBER_BLOCK+start, length);
        current_inline->li_start = backup_start;
    }

    /* Skip the lines with the closure. */
    offset = current_inline->end_line - current_inline->start_line;
    while (offset > 0)
    {
        int lines;

        lines = offset;
        if (lines > LI_MAXEMPTY)
            lines = LI_MAXEMPTY;
        offset -= lines;
        LINENUMBER_BLOCK[start++] = (char)(256 - lines);
        length--;
    }

    if (start + length < LINENUMBER_SIZE)
    {
#ifdef DEBUG_INLINES
printf("DEBUG:   move li data forward: from %"PRIuMPINT", length %"PRIuMPINT
       ", to %"PRIuMPINT"\n", 
       start+length, LINENUMBER_SIZE - length - start, start);
#endif /* DEBUG_INLINES */
        memmove( LINENUMBER_BLOCK+start
               , LINENUMBER_BLOCK+start+length
               , LINENUMBER_SIZE - length - start
               );
    }
    LINENUMBER_SIZE -= length;

    free_local_names(current_inline->block_depth+1);

    /* Restore the globals */
    block_depth              = current_inline->block_depth;
    current_number_of_locals = current_inline->num_locals;
    max_number_of_locals     = current_inline->max_num_locals;
    current_break_stack_need = current_inline->break_stack_size;
    max_break_stack_need     = current_inline->max_break_stack_size;
    exact_types              = current_inline->exact_types;

#ifdef DEBUG_INLINES
printf("DEBUG:   local types: %"PRIuMPINT", context types: %"PRIuMPINT"\n", 
       current_inline->full_local_type_start, current_inline->full_context_type_start);
#endif /* DEBUG_INLINES */
    type_of_locals = &(LOCAL_TYPE(current_inline->full_local_type_start));
    type_of_context = &(LOCAL_TYPE(current_inline->full_context_type_start));
#ifdef DEBUG_INLINES
printf("DEBUG:   type ptrs: %p, %p\n", type_of_locals, type_of_context );
#endif /* DEBUG_INLINES */

    /* Don't free the current_inline->returntype as it's not counted. */

    while (mem_block[A_LOCAL_TYPES].current_size
           > current_inline->full_local_type_size)
    {
        mem_block[A_LOCAL_TYPES].current_size -= sizeof(fulltype_t);
        free_fulltype_data( (fulltype_t*)
                            (mem_block[A_LOCAL_TYPES].block
                             + mem_block[A_LOCAL_TYPES].current_size)
                          );
    }

    /* Remove the structure from the lexical nesting stack */
    if (current_inline->prev == -1)
        current_inline = NULL;
    else
        current_inline = &(INLINE_CLOSURE(current_inline->prev));
} /* finish_inline_closure() */

/*-------------------------------------------------------------------------*/
static void
insert_pending_inline_closures (void)

/* The compilation is a point where pending inline closures can be
 * inserted. Do that now.
 */

{
    mp_int ix;
#ifdef DEBUG_INLINES
if (INLINE_CLOSURE_COUNT != 0) printf("DEBUG: insert_inline_closures(): %"
                                      PRIuMPINT" pending\n", 
                                      INLINE_CLOSURE_COUNT);
#endif /* DEBUG_INLINES */

    for (ix = 0; (size_t)ix < INLINE_CLOSURE_COUNT; ix++)
    {
        inline_closure_t * ict = &(INLINE_CLOSURE(ix));
#ifdef DEBUG_INLINES
printf("DEBUG:   #%"PRIdMPINT": start %"PRIuMPINT", length %"PRIuMPINT
       ", function %d: new start %"PRIuMPINT"\n", 
       ix, ict->start, ict->length, ict->function, CURRENT_PROGRAM_SIZE);
#endif /* DEBUG_INLINES */
        if (ict->length != 0)
        {
            CURRENT_PROGRAM_SIZE = align(CURRENT_PROGRAM_SIZE);

            store_line_number_info();
            if (stored_lines > ict->start_line)
                store_line_number_backward(stored_lines - ict->start_line);
            else 
                while (stored_lines < ict->start_line)
                {
                    int lines;

                    lines = ict->start_line - stored_lines;
                    if (lines > LI_MAXEMPTY)
                        lines = LI_MAXEMPTY;
                    stored_lines += lines;
                    byte_to_mem_block(A_LINENUMBERS, 256 - lines);
                }

            FUNCTION(ict->function)->offset.pc = CURRENT_PROGRAM_SIZE + FUNCTION_PRE_HDR_SIZE;
            add_to_mem_block(A_PROGRAM, INLINE_PROGRAM_BLOCK(ict->start)
                            , ict->length);
#ifdef DEBUG_INLINES
printf("DEBUG:        li_start %"PRIuMPINT", li_length %"PRIuMPINT
       ", new li_start %"PRIuMPINT"\n", 
       ict->li_start, ict->li_length, LINENUMBER_SIZE);
#endif /* DEBUG_INLINES */

            add_to_mem_block(A_LINENUMBERS, INLINE_PROGRAM_BLOCK(ict->li_start)
                            , ict->li_length);
            stored_lines = ict->end_line;
            stored_bytes += ict->length;
        }
    }

    /* Empty the datastorages */
    mem_block[A_INLINE_CLOSURE].current_size = 0;
    mem_block[A_INLINE_PROGRAM].current_size = 0;
} /* insert_pending_inline_closure() */

/*-------------------------------------------------------------------------*/
static Bool
prepare_inline_closure (fulltype_t returntype)

/* Called after parsing 'func <type>', this creates the identifier
 * with the synthetic function name. The function also sets up the inline
 * closure structure and block scope.
 *
 * If the name can't be generated, FALSE is returned, otherwise TRUE.
 */

{
    char name[256+MAXPATHLEN+1];
    ident_t * ident;

    /* Create the name of the new inline function.
     * We have to make sure the name is really unique.
     */
    do
    {
        char * start;

        sprintf(name, "__inline_%s_%d_#%04x", current_loc.file->name
                     , current_loc.line, inline_closure_id++);

        /* Convert all non-alnums (but '#') to '_' */
        for (start = name; *start != '\0'; start++)
        {
            if (!isalnum((unsigned char)(*start)) && '#' != *start)
                *start = '_';
        }
    } while (    find_shared_identifier(name, 0, 0)
              && inline_closure_id != 0);
    if (inline_closure_id == 0)
    {
        yyerror("Can't generate unique name for inline closure");
        return MY_FALSE;
    }

    ident = make_shared_identifier(name, I_TYPE_UNKNOWN, 0);

    /* The lfuns implementing the inline closures should not
     * be callable directly (without the CLOSURE svalue), and also not
     * overrideable.
     */
    returntype.typeflags |= TYPE_MOD_NO_MASK | TYPE_MOD_PRIVATE;

    def_function_typecheck(returntype, ident, MY_TRUE);
#ifdef DEBUG_INLINES
printf("DEBUG: New inline closure name: '%s'\n", name);
printf("DEBUG:   current_inline->depth: %d\n", current_inline->block_depth);
printf("DEBUG:           context depth: %d\n", current_inline->block_depth+1);
printf("DEBUG:               arg depth: %d\n", current_inline->block_depth+2);
printf("DEBUG:           current depth: %d\n", block_depth);
#endif /* DEBUG_INLINES */

    return MY_TRUE;
} /* prepare_inline_closure() */

/*-------------------------------------------------------------------------*/
static Bool
inline_closure_prototype (int num_args)

/* Called after parsing 'func <type> <arguments> <context>', this function
 * creates the function prototype entry.
 *
 * Return FALSE if out of memory (the internal structures have been cleaned
 * up then), TRUE otherwise.
 *
 * TODO: This function shares a lot of code with the generic function
 * TODO:: setup. To do this, use entry#0 for gathering the normal
 * TODO:: function information, and entries #1.. for the actual inlines.
 * TODO:: Or use a handful of globals, and save the in the closure entries
 * TODO:: as needed.
 */

{
#ifdef DEBUG_INLINES
printf("DEBUG: inline_closure_prototype(%d)\n", num_args);
#endif /* DEBUG_INLINES */
    def_function_prototype(num_args, MY_TRUE);

#ifdef DEBUG_INLINES
printf("DEBUG:   current_inline->depth: %d: %d\n", current_inline->block_depth, block_scope[current_inline->block_depth-1].num_locals);
printf("DEBUG:           context depth: %d: %d\n", current_inline->block_depth+1, block_scope[current_inline->block_depth+1-1].num_locals);
printf("DEBUG:               arg depth: %d: %d\n", current_inline->block_depth+2, block_scope[current_inline->block_depth+2-1].num_locals);
printf("DEBUG:           current depth: %d: %d\n", block_depth, block_scope[block_depth].num_locals);
printf("DEBUG:   Function index: %d\n", current_inline->function);
#endif /* DEBUG_INLINES */

    store_line_number_info();

    /* A function with code: align the function and
     * make space for the function header.
     */
    current_inline->end = CURRENT_PROGRAM_SIZE;
#ifdef DEBUG_INLINES
printf("DEBUG:   program size: %"PRIuMPINT" align to %"PRIuMPINT"\n", 
       CURRENT_PROGRAM_SIZE, align(CURRENT_PROGRAM_SIZE));
#endif /* DEBUG_INLINES */
    CURRENT_PROGRAM_SIZE = align(CURRENT_PROGRAM_SIZE);
    current_inline->start = CURRENT_PROGRAM_SIZE;
    current_inline->li_start = LINENUMBER_SIZE;
    current_inline->start_line = stored_lines;
    stored_bytes = CURRENT_PROGRAM_SIZE; /* Ignore the alignment. */
    
    if (realloc_a_program(FUNCTION_HDR_SIZE))
    {
        CURRENT_PROGRAM_SIZE += FUNCTION_HDR_SIZE;
    }
    else
    {
        yyerrorf("Out of memory: program size %"PRIuMPINT"\n"
                , mem_block[A_PROGRAM].current_size + FUNCTION_HDR_SIZE);
        finish_inline_closure(MY_TRUE);
        return MY_FALSE;
    }

    return MY_TRUE;
} /* inline_closure_prototype() */

/*-------------------------------------------------------------------------*/
static void
complete_inline_closure ( void )

/* Called after parsing 'func <type> <arguments> <block>', this function
 * updates the function header and moves the closure into the pending
 * area. After that, the function also completes the generation of
 * the F_CONTEXT_CLOSURE instruction.
 *
 * TODO: This function shares a lot of code with the generic function
 * TODO:: setup. To do this, use entry#0 for gathering the normal
 * TODO:: function information, and entries #1.. for the actual inlines.
 * TODO:: Or use a handful of globals, and save the in the closure entries
 * TODO:: as needed.
 */

{
    p_int start, li_start;
#ifdef DEBUG_INLINES
printf("DEBUG: Generate inline closure function:\n");
#endif /* DEBUG_INLINES */
    if  (current_inline->include_handle != get_include_handle())
    {
        yyerror("Implementation restriction: Inline closure must not span "
                "include file limits");
        /* Clean up */
        leave_block_scope(MY_TRUE);  /* Argument scope */
        leave_block_scope(MY_TRUE);  /* Context scope */
        finish_inline_closure(MY_TRUE);
        return;
    }

    start = current_inline->start;
    li_start = current_inline->li_start;

#ifdef DEBUG_INLINES
printf("DEBUG:   current_inline->depth: %d: %d\n", current_inline->block_depth, block_scope[current_inline->block_depth-1].num_locals);
printf("DEBUG:           context depth: %d: %d\n", current_inline->block_depth+1, block_scope[current_inline->block_depth+1-1].num_locals);
printf("DEBUG:               arg depth: %d: %d\n", current_inline->block_depth+2, block_scope[current_inline->block_depth+2-1].num_locals);
printf("DEBUG:           current depth: %d: %d\n", block_depth, block_scope[block_depth-1].num_locals);
#endif /* DEBUG_INLINES */

    /* Generate the function header and update the ident-table entry.
     */
    def_function_complete(start, MY_TRUE);

    current_inline->length = CURRENT_PROGRAM_SIZE - start;

    store_line_number_info();
    current_inline->li_length = LINENUMBER_SIZE - li_start;
    current_inline->end_line = stored_lines;

    /* Add the code to push the values of the inherited local
     * variables onto the stack, followed by the F_CONTEXT_CLOSURE
     * instruction. Since this code is after the recorded .length,
     * the finish_inline_closure() call will move it backward into
     * its rightful place.
     */
    {
        int num_explicit_context = 0;
        int depth = current_inline->block_depth+1;
        block_scope_t * context = &(block_scope[depth-1]);

#ifdef DEBUG_INLINES
printf("DEBUG:   %d context vars, depth %d\n", context->num_locals, depth);
#endif /* DEBUG_INLINES */
        if (context->num_locals != 0)
        {
            Bool got_mapped;

            /* To get the context->local information in the right order
             * we read the locals as they are and store the information
             * in an array.
             */
            int * lcmap = alloca(context->num_locals * sizeof(int));
            ident_t * id;
            int i;

            for (i = 0; i < context->num_locals; i++)
                lcmap[i] = -1;

            for (id = all_locals
                ; id && id->u.local.depth >= depth
                ; id = id->next_all)
            {
#ifdef DEBUG_INLINES
if (id->u.local.depth == depth) printf("DEBUG:     '%s': local %d, context %d\n", 
                                       get_txt(id->name), id->u.local.num, id->u.local.context);
#endif /* DEBUG_INLINES */
                if (id->u.local.depth == depth
                 && id->u.local.context >= 0
                 && id->u.local.num >= 0
                   )
                {
                    lcmap[id->u.local.context] = id->u.local.num;
                }
            }

            /* Got all context->local mappings, now create the bytecode */
            got_mapped = MY_FALSE;
            for (i = 0; i < context->num_locals; i++)
            {
                if (lcmap[i] != -1)
                {
                    if (lcmap[i] >= CONTEXT_VARIABLE_BASE)
                    {
                        ins_f_code(F_CONTEXT_IDENTIFIER);
                        ins_byte(lcmap[i] - CONTEXT_VARIABLE_BASE);
                    }
                    else
                    {
                        ins_f_code(F_LOCAL);
                        ins_byte(lcmap[i]);
                    }
                    got_mapped = MY_TRUE;
#ifdef DEBUG_INLINES
printf("DEBUG:     -> F_LOCAL %d\n", lcmap[i]);
#endif /* DEBUG_INLINES */
                }
                else if (got_mapped)
                {
                    /* This shouldn't happen, as all explicite context
                     * variables are created before the first implicite
                     * reference can be encountered.
                     */
                    fatal("Explicite context var #%d has higher index than "
                          "implicite context variables.", i);
                }
                else
                    num_explicit_context++;
            }
        } /* Push local vars */

        /* Add the context_closure instruction */
#ifdef DEBUG_INLINES
printf("DEBUG:     -> F_CONTEXT_CLOSURE %d %d %d\n", current_inline->function
      , num_explicit_context, context->num_locals - num_explicit_context);
#endif /* DEBUG_INLINES */
        ins_f_code(F_CONTEXT_CLOSURE);
        ins_short(current_inline->function);
        ins_byte(context->first_local);
        ins_short(num_explicit_context);
        ins_short(context->num_locals - num_explicit_context);
    } /* Complete F_CONTEXT_CLOSURE instruction */


    /* Clean up */
    leave_block_scope(MY_TRUE);  /* Argument scope */
    leave_block_scope(MY_TRUE);  /* Context scope */
    finish_inline_closure(MY_FALSE);
} /* complete_inline_closure() */

/* =========================   PROGRAM STRINGS   ========================= */

/*-------------------------------------------------------------------------*/
static short
store_prog_string (string_t *str)

/* Add the tabled string <str> to the strings used by the program.
 * The function takes care that the same string is not stored twice.
 * Result is the index of the string in the table, the function
 * adopts the reference of <str>.
 */

{
    mp_uint str_size, next_size;
    long hash;
    char mask, *tagp;
    int i, *indexp;

    /* Compute the hash and the tagmask for the hash table */
    /* TODO: This assumes 32-Bit pointers */
    hash = (long)str ^ (long)str >> 16;
    hash = (hash ^ hash >> 8);
    mask = 1 << (hash & 7);
    hash = hash & 0xff;

    indexp = &prog_string_indizes[hash];
    tagp = &prog_string_tags[hash >> 3];

    if (*tagp & mask)
    {
        /* There is a hash chain for this hash: search the
         * string in there.
         */
        i = *indexp;
        for(;;)
        {
            if ( PROG_STRING(i) == str )
            {
                free_mstring(str); /* Drop the extra ref. */
                last_string_is_new = MY_FALSE;
                return i;
            }
            if ((i = PROG_STRING_NEXT(i)) < 0)
                break;
        }

        /* Not found: re-get the initial 'next'-index */
        i = *indexp;
    }
    else
    {
        /* The first time this hash shows up (which also implies
         * that <str> is a new string.
         */
        *tagp |= mask;
        i = -1;
    }

    /* Add a totally new string */

    str_size = mem_block[A_STRINGS].current_size;
    next_size = mem_block[A_STRING_NEXT].current_size;

    /* Make sure we have enough memory */
    if (str_size + sizeof(string_t *) > mem_block[A_STRINGS].max_size
     || next_size + sizeof(int) > mem_block[A_STRING_NEXT].max_size
       )
    {
        if (!realloc_mem_block(&mem_block[A_STRINGS], 0)
         || !realloc_mem_block(&mem_block[A_STRING_NEXT], 0))
        {
            if (i < 0)
                *tagp &= ~mask;
            last_string_is_new = MY_FALSE;
            return 0;
        }
    }

    /* Add the string pointer */
    mem_block[A_STRINGS].current_size = str_size + sizeof(string_t *);
    *((string_t **)(mem_block[A_STRINGS].block+str_size)) = str;

    /* Add the old prog_string_index[] */
    mem_block[A_STRING_NEXT].current_size = next_size + sizeof(int);
    *((int *)(mem_block[A_STRING_NEXT].block+next_size)) = i;

    /* Store the string index as new prog_string_index[] */
    *indexp = str_size / sizeof str;

    last_string_is_new = MY_TRUE;
    return *indexp;
} /* store_prog_string() */

/*-------------------------------------------------------------------------*/
static void
delete_prog_string (void)

/* Remove the program string last added with store_prog_string().
 */

{
    string_t *str;
    int size;
    long hash;
    char mask, *tagp;
    int *indexp;

    /* Remove the string from the A_STRINGS area and free it */
    size = mem_block[A_STRINGS].current_size - sizeof(string_t *);
    free_mstring(
      str = *(string_t**)(mem_block[A_STRINGS].block+size)
    );
    mem_block[A_STRINGS].current_size = size;

    /* Remove the string from the hash table */

    size = (mem_block[A_STRING_NEXT].current_size -= sizeof(int));

    /* TODO: Assumes 32-Bit pointers */
    hash = (long)str ^ (long)str >> 16;
    hash = (hash ^ hash >> 8);
    mask = 1 << (hash & 7);
    hash = hash & 0xff;
    indexp = &prog_string_indizes[hash];
    tagp = &prog_string_tags[hash >> 3];

    if ( ( *indexp = *((int *)(mem_block[A_STRING_NEXT].block+size)) ) < 0)
        /* Hash chain empty */
        *tagp &= ~mask;

} /* delete_prog_string() */


/*=========================================================================*/

#if defined(__MWERKS__) && !defined(WARN_ALL)
#    pragma warn_possunwant off
#    pragma warn_implicitconv off
#endif
%}

/*=========================================================================*/

/*                           P A R S E R                                   */

/*-------------------------------------------------------------------------*/

%token L_ASSIGN
%token L_ARROW
%token L_BREAK
%token L_CASE
%token L_CATCH
%token L_CLOSURE
%token L_CLOSURE_DECL
%token L_COLON_COLON
%token L_CONTINUE
%token L_DEC
%token L_DEFAULT
%token L_DO
%token L_ELLIPSIS
%token L_ELSE
%token L_EQ
%token L_FUNC
%token L_BEGIN_INLINE
%token L_END_INLINE
%token L_FLOAT
%token L_FLOAT_DECL
%token L_FOR
%token L_FOREACH
%token L_GE
%token L_IDENTIFIER
%token L_IF
%token L_INC
%token L_INHERIT
%token L_INLINE_FUN
%token L_INT
%token L_LAND
%token L_LE
%token L_LOCAL
%token L_LOR
%token L_LSH
%token L_MAPPING
%token L_MIXED
%token L_NE
%token L_NO_MASK
%token L_NOSAVE
%token L_DEPRECATED
%token L_NOT
%token L_NUMBER
%token L_OBJECT
%ifdef USE_PARSE_COMMAND
%token L_PARSE_COMMAND
%endif
%token L_PRIVATE
%token L_PROTECTED
%token L_PUBLIC
%token L_QUOTED_AGGREGATE
%token L_RANGE
%token L_RETURN
%token L_RSH
%token L_RSHL
%token L_SSCANF
%token L_STATIC
%token L_STATUS
%token L_STRING
%token L_STRING_DECL
%token L_STRUCT
%token L_SWITCH
%token L_SYMBOL
%token L_SYMBOL_DECL
%token L_VARARGS
%token L_VIRTUAL
%token L_VOID
%token L_WHILE

/* Textbook solution to the 'dangling else' shift/reduce conflict.
 */

%nonassoc LOWER_THAN_ELSE
%nonassoc L_ELSE

/*-------------------------------------------------------------------------*/
/* The yacc stack type */

/* Note: vartype_t and fulltype_t references are not counted!
 * Throughout the compiler fulltype_ts are used even if the values
 * are not intended to have visibility information.
 */

%union
{
%line
    p_int number;
      /* Literal numbers, or whereever a number is required.
       */

    double float_number;
      /* Literal floats */

    struct {
        p_int number;
        unsigned short inhIndex;
    } closure;
      /* A closure (#'xxx). The .number determines the exact
       * nature of the closure.
       * For lfun closures, an inhIndex > 0 determines the
       * (inheritance index + 1) of a direct reference to an
       * inherited closure.
       */

    struct {
        string_t *name;  /* The tabled string with the name */
        int   quotes;    /* Number of quotes */
    } symbol;
      /* A literal symbol.
       */

    ident_t *ident;
      /* L_IDENTIFIER, L_INLINE_FUN: The recognized identifier
       */

    typeflags_t typeflags;
      /* Just the typeflags (reference, pointer, visibility).
       */

    fulltype_t type;
      /* The datatype, not intended to have visibility flags.
       */

    fulltype_t fulltype;
      /* The fulltype (datatype plus visibility) of entities.
       */

    funflag_t inh_flags[2];
      /* Inheritance: [0]: code inheritance qualifiers
       *              [1]: variable inheritance qualifiers
       */

    svalue_t *initialized;
      /* Position where to store the variable initializer.
       */

    p_int numbers[2];
      /* Often used to save the current break/continue address.
       */

    p_uint address;
      /* Address of an instruction. */

    struct {
        bytecode_p     p;       /* The condition code */
        unsigned short length;  /* Length of the condition code */
        unsigned short line;    /* Last source line of the condition code */
    } expression;
      /* Expressions are used to save the code for a loop-condition
       * while the body is compiled.
       */

    struct s_lrvalue
    {
        fulltype_t type;   /* Type of the expression */
        uint32     start;  /* Startaddress of the instruction */
        short      code;   /* Alternative instruction */
        uint32     end;    /* Endaddress+1 of the instruction */
    }
    lrvalue;
      /* Used for expressions which may return a rvalue or lvalues.
       * It is also used by the index range generation to move around
       * the index expressions.
       * Lvalue generation in places where either a r- or an lvalue
       * is acceptible first generates the rvalue code, but stores
       * the necessary information to patch the code to produce
       * lvalues in this structure.
       * For more information, see arrange_protected_lvalue().
       */

    struct s_index
    {
        int        inst;   /* Type of the index */
        uint32     start;  /* Startaddress of the index */
        uint32     end;    /* Endaddress+1 of the index */
        fulltype_t type1;  /* Type of index, resp. lower bound */
        fulltype_t type2;  /* Type of other index, resp. upper bound */
    }
    index;
      /* This is used to parse and return the indexing operation
       * of an array or mapping.
       * .inst gives the type of the operation:
       *   F_INDEX:     [x]
       *   F_RINDEX:    [<x]
       *   F_AINDEX:    [>x]
       *   F_RANGE:     [ x.. y]
       *   F_RN_RANGE:  [<x.. y]
       *   F_NR_RANGE:  [ x..<y]
       *   F_RR_RANGE:  [<x..<y]
       *   F_AN_RANGE:  [>x.. y]
       *   F_AR_RANGE:  [>x..<y]
       *   F_NA_RANGE:  [ x..>y]
       *   F_RA_RANGE:  [<x..>y]
       *   F_AA_RANGE:  [>x..>y]
       *   F_NX_RANGE:  [ x..  ]
       *   F_RX_RANGE:  [<x..  ]
       *   F_AX_RANGE:  [>x..  ]
       * .start and .end are the bytecode limits of the whole
       * operation.
       * .type1 and optionally .type2 are the types of the
       * index values.
       */

    struct lvalue_s {
        union {
            bytecode_p p;
            bytecode_t simple[2];
        } u;
        unsigned short length;
        fulltype_t type;
    } lvalue;
      /* Used in assigns to communicate how an lvalue has to be accessed
       * (by passing on the bytecode to create) and what type it is.
       * .length = 0: u.simple contains the bytecode to create
       * .length != 0: u.p points to the bytecode of .length bytes.
       */

    struct {
        p_int key;     /* shared string ptr, or a number */
        Bool numeric;  /* TRUE: .key is a number */
    } case_label;
      /* Used to return the value of a 'case' label.
       */

    char *string;
      /* An allocated string */

    string_t *sh_string;
      /* A shared string */

    struct {
        char    *super; /* NULL, or the allocated qualifier */
        ident_t *real;  /* The function identifier */
    } function_name;
      /* A qualified function name: "<super>::<func>" */

    struct {
        int    simul_efun;    /* -1, or index of the simul_efun */
        p_int  start;         /* Address of the function call */
        efun_override_t efun_override; /* set on (s)efun:: prefix. */
    } function_call_head;
      /* Used to save address and possible sefun-index over
       * the argument parsing in a function call.
       */

    struct {
        int length;            /* Number of initializers parsed */
        /* Description of initializers parsed: */
        struct struct_init_s * list;  /* Head of list */
        struct struct_init_s * last;  /* Tail of list */
    } struct_init_list;
      /* For runtime struct literals: head of the list describing
       * the encountered member initializers.
       */

    struct {
        string_t * name;  /* Member name, or NULL if unnamed */
        vartype_t  type;  /* Member expr type */
    } struct_init_member;
      /* For runtime struct literals: information about a single
       * member initializer.
       */

} /* YYSTYPE */

/*-------------------------------------------------------------------------*/

%type <number>       L_NUMBER constant
%type <float_number> L_FLOAT
%type <closure>      L_CLOSURE
%type <symbol>       L_SYMBOL
%type <number>       L_QUOTED_AGGREGATE
%type <ident>        L_IDENTIFIER L_INLINE_FUN L_LOCAL
%type <typeflags>    optional_star type_modifier type_modifier_list
%type <fulltype>     type
%type <fulltype>     opt_basic_type basic_type
%type <fulltype>     non_void_type opt_basic_non_void_type basic_non_void_type
%type <fulltype>     name_list local_name_list
%type <inh_flags>    inheritance_qualifier inheritance_qualifiers
%type <typeflags>    inheritance_modifier_list inheritance_modifier
%type <fulltype>     inline_opt_type
%type <type>         decl_cast cast
%type <lrvalue>      note_start comma_expr expr0 expr4
%type <lrvalue>      function_call
%type <lrvalue>      inline_func
%type <lrvalue>      catch sscanf
%type <lrvalue>      for_init_expr for_expr
%type <lrvalue>      comma_expr_decl expr_decl
%ifdef USE_PARSE_COMMAND
%type <lrvalue>      parse_command
%endif
%type <lvalue>       lvalue name_lvalue local_name_lvalue foreach_var_lvalue
%type <index>        index_range index_expr
%type <case_label>   case_label
%type <address>      optional_else
%type <string>       anchestor
%type <sh_string>    call_other_name identifier
%type <fulltype>     member_name_list
%type <struct_init_member> struct_init
%type <struct_init_list>   opt_struct_init opt_struct_init2
%type <sh_string>    struct_member_name
%type <function_name> function_name


/* Special uses of <number> */

%type <number> function_body
  /* program address or -1 */

%type <number> argument argument_list lvalue_list
%type <number> inline_opt_args
  /* number of arguments */

%type <number> expr_list expr_list3 e_expr_list2 expr_list2
  /* Number of expressions in an expression list */

%type <number> m_expr_values
  /* Number of values for a mapping entry (ie the 'width') */

%type <number> L_ASSIGN
  /* Instruction code of the assignment, e.g. F_ADD_EQ */

%type <number> foreach_expr
  /* FOREACH_LOOP  (0) Normal foreach loop value
   * FOREACH_REF   (1) Referenced foreach loop value
   * FOREACH_RANGE (2) Integer range as loop value
   */

%type <number> foreach_vars
  /* Number of variables given to foreach
   */

%type <number> opt_catch_mods opt_catch_mod_list opt_catch_modifier
  /* Bitflags for catch() modes: CATCH_FLAG_xxx from simulate.h
   */

/* Special uses of <numbers> */

%type <numbers> condStart
  /* [0]: current_break_address
   * [1]: address of the branch-offset of the if
   */

%type <numbers> m_expr_list m_expr_list2
  /* [0]: number of entries in a mapping literal
   * [1]: width of the mapping literal
   */

/* Special uses of <lrvalue> */

%type <lrvalue> pre_inc_dec
  /* .code: The instruction F_PRE_INC or F_PRE_DEC.
   * .start: The CURRENT_PROGRAM_SIZE where this inst was encountered.
   */

/*-------------------------------------------------------------------------*/

%right    L_ASSIGN
%right    '?'
%left     L_LOR
%left     L_LAND
%left     '|'
%left     '^'
%left     '&'
%left     L_EQ    L_NE
%left     '<'     L_LE  '>' L_GE
%left     L_LSH   L_RSH L_RSHL
%left     '+'     '-'
%left     '*'     '/'   '%'
%right    '~'     L_NOT
%nonassoc L_INC   L_DEC
%left     L_ARROW '['
%%

/*-------------------------------------------------------------------------*/

all: program;

program:
      program def possible_semi_colon
    | /* empty */ ;

possible_semi_colon:
      /* empty */
    | ';' { yywarn("Extra ';' ignored"); };

note_start: { $$.start = CURRENT_PROGRAM_SIZE; };

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/* Function prototypes
 * Function definitions
 * Variable definitions
 * Inheritance
 * Default visibility
 */

def:  type optional_star L_IDENTIFIER  /* Function definition or prototype */

      {
          $1.typeflags |= $2;
          def_function_typecheck($1, $3, MY_FALSE);
      }

      '(' argument ')'

      {
          def_function_prototype($6, MY_FALSE);
      }

      function_body

      {
          def_function_complete($9, MY_FALSE);
          insert_pending_inline_closures();
      }

    | name_list ';' /* Variable definition */
      {
          insert_pending_inline_closures();
      }

    | struct_decl
    | inheritance
    | default_visibility
; /* def */


function_body:
      /* A function with code: align the function and
       * make space for the function header.
       * Result is the address of the FUNCTION_NAME space.
       */
      {
%line
          CURRENT_PROGRAM_SIZE = align(CURRENT_PROGRAM_SIZE);
          $<number>$ = CURRENT_PROGRAM_SIZE;
          if (realloc_a_program(FUNCTION_HDR_SIZE))
          {
              CURRENT_PROGRAM_SIZE += FUNCTION_HDR_SIZE;
          }
          else
          {
              yyerrorf("Out of memory: program size %"PRIuMPINT"\n"
                      , mem_block[A_PROGRAM].current_size + FUNCTION_HDR_SIZE);
              YYACCEPT;
          }
      }

      block

      { $$ = $<number>1; }

    | ';' { $$ = -1; }
; /* function_body */


/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
/* Inline functions
 */

inline_func:
      L_FUNC inline_opt_type

      {
#ifdef DEBUG_INLINES
printf("DEBUG: After inline_opt_type: program size %"PRIuMPINT"\n", CURRENT_PROGRAM_SIZE);
#endif /* DEBUG_INLINES */
          if (!prepare_inline_closure($2))
              YYACCEPT;
      }

      inline_opt_args

      {
#ifdef DEBUG_INLINES
printf("DEBUG: After inline_opt_args: program size %"PRIuMPINT"\n", CURRENT_PROGRAM_SIZE);
#endif /* DEBUG_INLINES */

          /* deactivate argument scope while parsing explicit context */
          block_scope[current_inline->block_depth+1].accessible = MY_FALSE;
          current_inline->parse_context = MY_TRUE;
          assert(block_scope[current_inline->block_depth].first_local == 0);

          /* Set the context scope as it would belong to the enclosing function. */
          if (current_inline->prev != -1 && INLINE_CLOSURE(current_inline->prev).parse_context)
          {
              /* If we are within the context of another closure, count from there.
               * Get its context scope and take their current variable count
               * (Its first_local was adjusted the same way, so just add its current
               * number of context variables.)
               */
              block_scope_t *scope = block_scope + INLINE_CLOSURE(current_inline->prev).block_depth;
              block_scope[current_inline->block_depth].first_local = scope->first_local + scope->num_locals;
          }
          else
              block_scope[current_inline->block_depth].first_local = current_inline->num_locals;

          type_of_locals = &(LOCAL_TYPE(current_inline->full_local_type_start));
          /* Note, that type_of_context must not be reset, as add_context_name()
           * needs it where it points now. check_for_context_local() will take
           * care of finding the right type for context variables.
           */

      }

      inline_opt_context

      {
#ifdef DEBUG_INLINES
printf("DEBUG: After inline_opt_context: program size %"PRIuMPINT"\n", CURRENT_PROGRAM_SIZE);
#endif /* DEBUG_INLINES */

          /* Complete the F_CLEAR_LOCALS at the beginning of the context block. */
          block_scope_t *scope = block_scope + current_inline->block_depth;
          inline_closure_t *outer_closure;
          int * outer_max_num_locals;

          if (scope->num_locals > scope->num_cleared)
          {
              mem_block[A_PROGRAM].block[scope->addr+2]
                = (char)(scope->num_locals - scope->num_cleared);
          }

          /* reactivate argument scope */
          block_scope[current_inline->block_depth+1].accessible = MY_TRUE;
          current_inline->parse_context = MY_FALSE;
          adapt_context_names();
          type_of_locals = type_of_context + MAX_LOCAL;

          /* Find the correct max_num_locals to update.
           * That is the one of the last closure with parse_context set.
           */
          outer_max_num_locals = &(current_inline->max_num_locals);
          outer_closure = current_inline;

          while (outer_closure->prev != -1)
          {
              outer_closure = &(INLINE_CLOSURE(outer_closure->prev));
              if (!outer_closure->parse_context)
                  break;
              outer_max_num_locals = &(outer_closure->max_num_locals);
          }

          if (scope->first_local + scope->num_locals > *outer_max_num_locals)
              *outer_max_num_locals = scope->first_local + scope->num_locals;

          /* Check whether we clobbered some other local or context variables. */
          if (scope->num_locals || scope->clobbered)
          {
              if (current_inline->prev != -1)
              {
                  outer_closure = &(INLINE_CLOSURE(current_inline->prev));
                  if (outer_closure->parse_context)
                      block_scope[outer_closure->block_depth].clobbered = MY_TRUE;
                  else
                      block_scope[current_inline->block_depth-1].clobbered = MY_TRUE;
              }
              else
                  block_scope[current_inline->block_depth-1].clobbered = MY_TRUE;
          }

          if (!inline_closure_prototype($4))
              YYACCEPT;
      }

      block

      {
#ifdef DEBUG_INLINES
printf("DEBUG: After inline block: program size %"PRIuMPINT"\n", CURRENT_PROGRAM_SIZE);
#endif /* DEBUG_INLINES */
         $$.start = current_inline->end;
         $$.code = -1;
         $$.type = Type_Closure;

         complete_inline_closure();
      }


    | L_BEGIN_INLINE

      {
          int i;

#ifdef DEBUG_INLINES
printf("DEBUG: After L_BEGIN_INLINE: program size %"PRIuMPINT"\n", CURRENT_PROGRAM_SIZE);
#endif /* DEBUG_INLINES */
          if (!prepare_inline_closure(Type_Any))
              YYACCEPT;

          /* Synthesize $1..$9 as arguments */
          for (i = 1; i < 10; i++)
          {
              char name[4];
              ident_t *ident;

              sprintf(name, "$%d", i);
              ident = make_shared_identifier(name, I_TYPE_UNKNOWN, 0);
              add_local_name(ident, Type_Any, block_depth);
          }

          if (!inline_closure_prototype(9))
              YYACCEPT;

          /* Put the code block in its own scope apart from the
           * parameters, so that define_local_variable doesn't
           * assume that there are already 9 Variables.
           */
          enter_block_scope();
#ifdef DEBUG_INLINES
printf("DEBUG: Before comma_expr: program size %"PRIuMPINT"\n", CURRENT_PROGRAM_SIZE);
#endif /* DEBUG_INLINES */
      }

      statements inline_comma_expr

      L_END_INLINE

      {
#ifdef DEBUG_INLINES
printf("DEBUG: After L_END_INLINE: program size %"PRIuMPINT"\n", CURRENT_PROGRAM_SIZE);
#endif /* DEBUG_INLINES */

         /* Complete the F_CLEAR_LOCALS at the beginning of the block. */
         block_scope_t *scope = block_scope + block_depth - 1;

         if (scope->num_locals > scope->num_cleared)
         {
              mem_block[A_PROGRAM].block[scope->addr+2]
                = (char)(scope->num_locals - scope->num_cleared);
         }

         leave_block_scope(MY_FALSE);

         $$.start = current_inline->end;
         $$.code = -1;
         $$.type = Type_Closure;

         complete_inline_closure();
      }

; /* inline_func */

inline_opt_args:
      /* empty */
      {
          int i;

          /* Synthesize $1..$9 as arguments */
          for (i = 1; i < 10; i++)
          {
              char name[4];
              ident_t *ident;

              sprintf(name, "$%d", i);
              ident = make_shared_identifier(name, I_TYPE_UNKNOWN, 0);
              add_local_name(ident, Type_Any, block_depth);
          }

          $$ = 9;
      }
    | '(' argument ')'  { $$ = $2; }
; /* inline_opt_args */

inline_opt_type:
      /* empty */
      {
#ifdef DEBUG_INLINES
          printf("DEBUG: inline_opt_type default: ANY\n");
#endif /* DEBUG_INLINES */
          $$ = Type_Any;
      }
    | basic_type optional_star
      {
#ifdef DEBUG_INLINES
          printf("DEBUG: inline_opt_type: %c%s\n", $2 ? '*' : ' ', get_type_name($1));
#endif /* DEBUG_INLINES */
          set_fulltype($$, $1.typeflags | $2, $1.t_struct);
      }
; /* inline_opt_type */

inline_opt_context:
      /* empty */
    |  ':' inline_context_list inline_semi
; /* inline_opt_context */

inline_semi:
      /* empty */
    | ';'
; /* inline_semi */

inline_context_list:
      /* empty */
    | context_decl
    | inline_context_list ';' context_decl
; /* inline_context_list */


context_decl:
      local_name_list
      { /* Empty action to void value from local_name_list */ }
; /* context_decl */


inline_comma_expr:
      /* Empty: nothing to do */
    | comma_expr
      {
          /* Add a F_RETURN to complete the statement */
          ins_f_code(F_RETURN);
      }
; /* inline_comma_expr */


/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
/* Definition of a struct
 */

struct_decl:
      type_modifier_list L_STRUCT L_IDENTIFIER ';'
      {
          (void)define_new_struct(MY_TRUE, $3, $1);
      }
    | type_modifier_list L_STRUCT L_IDENTIFIER
      { 
          size_t i;

          /* Free any struct members left over from a previous
           * struct parse. This should happen only in case
           * of errors.
           */
          for (i = 0; i < STRUCT_MEMBER_COUNT; i++)
          {
              free_struct_member_data(&STRUCT_MEMBER(i));
          }
          mem_block[A_STRUCT_MEMBERS].current_size = 0;

          current_struct = define_new_struct(MY_FALSE, $3, $1);
          if (current_struct < 0)
              YYACCEPT;
      }
      opt_base_struct '{' opt_member_list '}' ';'
      { 
          finish_struct(compiled_file, current_id_number+1);
      }
; /* struct_decl */

opt_base_struct:
      /* empty */ { }
    | '(' L_IDENTIFIER ')'
      {
          /* Look up the struct id for the given identifier */

          int num = -1;

          if ($2->type == I_TYPE_UNKNOWN)
          {
              /* Identifier -> no such struct encountered yet */
              yyerrorf("Unknown base struct '%s'", get_txt($2->name));
          }
          else
          {
              ident_t *p = $2;

              /* Find the global struct identifier */
              while (p != NULL && p->type != I_TYPE_GLOBAL)
                  p = p->inferior;

              if (p == NULL || (num = p->u.global.struct_id) < 0)
              {
                  yyerrorf("Unknown base struct '%s'", get_txt($2->name));
              }
              else if (STRUCT_DEF(num).flags & NAME_PROTOTYPE)
              {
                  yyerrorf("Undefined base struct '%s'", get_txt($2->name));
              }
              else if (!struct_t_unique_name(STRUCT_DEF(num).type))
              {
                  yyerrorf("Incomplete base struct '%s'", get_txt($2->name));
              }
              else
              {
                  struct_type_t *ptype = STRUCT_DEF(num).type;
                  struct_type_t *ctype = STRUCT_DEF(current_struct).type;
                  // record pointer to base struct even if the base struct has no members
                  ctype->base = ref_struct_type(ptype);
                  
                  if (struct_t_size(ptype) > 0)
                  {
                      int count;
                      struct_member_t *member;

                      member = ptype->member;
                      count = struct_t_size(ptype);

                      for ( ; count > 0; count--, member++ )
                          add_struct_member(member->name, member->type, ptype);
                  }
              }
          } /* if type == UNKNOWN */
      }
; /* opt_base_struct */

opt_member_list:
      /* empty */
    | member_list
; /* opt_member_list */

member_list:
      member
    | member_list member
; /* member_list */

member:
      member_name_list ';'
      {
          /* The member_name_list adds the struct members. */
      }
; /* member */

member_name_list:
      basic_non_void_type optional_star L_IDENTIFIER
      {
          fulltype_t actual_type = $1;
          vartype_t type;

          actual_type.typeflags |= $2;

          assign_full_to_vartype(&type, actual_type);
          add_struct_member($3->name, type, NULL);

          $$ = $1;
      }
    | member_name_list ',' optional_star L_IDENTIFIER
      {
          fulltype_t actual_type = $1;
          vartype_t type;

          actual_type.typeflags |= $3;

          assign_full_to_vartype(&type, actual_type);
          add_struct_member($4->name, type, NULL);

          $$ = $1;
      }
; /* member_name_list */


/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/* Inheritance specification
 */

inheritance:
      inheritance_qualifiers L_INHERIT string_constant ';'
      {
%line
          /* We got an inheritance: look up the name object and copy
           * the functions and variables into this program.
           *
           * If the inherited object hasn't been loaded yet, store the
           * name in inherit_file and abort the compile.
           *
           * copy_variables() might add extra inherits for virtual inheritance.
           * For this reason, copy_functions() can't know the actual index
           * of the new inherit, so it sets it to NEW_INHERITED_INDEX instead.
           * This is changed later to the actual value by
           * fix_function_inherit_indices() .
           */

          object_t *ob;
          inherit_t inherit;

          if (CURRENT_PROGRAM_SIZE
           && !(((function_t *)(mem_block[A_FUNCTIONS].block+
                         mem_block[A_FUNCTIONS].current_size))[-1].flags &
                         NAME_INHERITED)
             )
          {
              yyerror("illegal to inherit after defining functions");
          }

          /* Check the inheritance qualifiers.
           * A variable 'nosave' inherit is internally stored as 'static',
           * a functions 'nosave' inherit is not allowed.
           */
          if ($1[1] & TYPE_MOD_NOSAVE)
          {
              $1[1] |= TYPE_MOD_STATIC;
              $1[1] ^= TYPE_MOD_NOSAVE;
          }

          if ($1[0] & TYPE_MOD_NOSAVE)
          {
              $1[0] ^= TYPE_MOD_NOSAVE;
              yyerror("illegal to inherit code as 'nosave'");
          }

          /* First, try to call master->inherit_file().
           * Since simulate::load_object() makes sure that the master has been
           * loaded, this test can only fail when the master is compiled.
           */
          if (master_ob && !(master_ob->flags & O_DESTRUCTED)
           && !EVALUATION_TOO_LONG()
             )
          {
              svalue_t *res;

              push_ref_string(inter_sp, last_string_constant);

              if (!compat_mode)
              {
                  char * filename;
                  filename = alloca(strlen(current_loc.file->name)+2);
                  *filename = '/';
                  strcpy(filename+1, current_loc.file->name);
                  push_c_string(inter_sp, filename);
              }
              else
                  push_c_string(inter_sp, current_loc.file->name);

              res = apply_master(STR_INHERIT_FILE, 2);

              if (res && !(res->type == T_NUMBER && !res->u.number))
              {
                  /* We got a result - either a new name or a "reject it"
                   * value.
                   */

                  char * cp;

                  if (res->type != T_STRING)
                  {
                      yyerrorf("Illegal to inherit file '%s'."
                              , get_txt(last_string_constant));
                      YYACCEPT;
                  }

                  for (cp = get_txt(res->u.str); *cp == '/'; cp++) NOOP;

                  if (!legal_path(cp))
                  {
                      yyerrorf("Illegal path '%s'.", get_txt(res->u.str));
                      YYACCEPT;
                  }

                  /* Ok, now replace the parsed string with the name
                   * we just got.
                   */
                  free_mstring(last_string_constant);
                  last_string_constant = new_tabled(cp);
              }
              /* else: no result - use the string as it is */
          }
          else if (EVALUATION_TOO_LONG())
          {
              yyerrorf("Can't call master::%s for "
                       "'%s': eval cost too big"
                      , get_txt(STR_INHERIT_FILE)
                      , get_txt(last_string_constant));
              /* use the string as it is */
          }


          /* Look up the inherited object and swap it in.
           */
          ob = find_object(last_string_constant);
          if (ob == 0)
          {
              inherit_file = last_string_constant;
              last_string_constant = NULL;
              /* Return back to load_object() */
              YYACCEPT;
          }
          ob->time_of_ref = current_time;

          if (ob->flags & O_SWAPPED && load_ob_from_swap(ob) < 0)
          {
              free_mstring(last_string_constant);
              last_string_constant = NULL;
              yyerrorf("Out of memory when unswapping '%s'", get_txt(ob->name));
              YYACCEPT;
          }

          /* Legal to inherit? */
          if (ob->prog->flags & P_NO_INHERIT)
          {
              yyerror("Illegal to inherit an object which sets "
                      "'#pragma no_inherit'.");
              YYACCEPT;
          }

          free_mstring(last_string_constant);
          last_string_constant = NULL;

          /* Set up the inherit structure */
          inherit.prog = ob->prog;
          if ($1[1] & TYPE_MOD_VIRTUAL)
              inherit.inherit_type = INHERIT_TYPE_VIRTUAL;
          else
              inherit.inherit_type = INHERIT_TYPE_NORMAL;
          inherit.function_index_offset = FUNCTION_COUNT;
          inherit.inherit_depth = 1;

          /* If it's a virtual inherit, check if it has been
           * inherited virtually before. If yes, don't bother to insert it
           * again.
           * For all types of inherits, check if the same program has already
           * been inherited at the toplevel.
           */
          {
              inherit_t *inheritp;
              int j;
              Bool duplicate_toplevel = MY_FALSE;

              inheritp = (inherit_t *)(mem_block[A_INHERITS].block);
              j = mem_block[A_INHERITS].current_size;
              for (; (j -= sizeof(inherit_t)) >= 0; inheritp++)
              {
                  if (inheritp->prog == ob->prog)
                  {
                      /* Check for duplicate toplevel inherit.
                       * Since the check for duplicate virtual inherits
                       * may change the inherit_depth, this test must
                       * come first
                       */
                      if (inheritp->inherit_depth == 1)
                          duplicate_toplevel = MY_TRUE;

                      /* Check for duplicate virtual inherit */
                      if (($1[1] & TYPE_MOD_VIRTUAL)
                       && !(inheritp->variable_index_offset & NON_VIRTUAL_OFFSET_TAG)
                       && !(inherit.inherit_type & INHERIT_TYPE_DUPLICATE)
                         )
                      {
                          inherit.inherit_type |= INHERIT_TYPE_DUPLICATE;
                          inheritp->inherit_depth = 1;
                      }

                  }
              }
              if  (duplicate_toplevel)
              {
                  if (pragma_pedantic)
                  {
                      yyerrorf("Program '%s' already inherited"
                              , get_txt(inherit.prog->name));
                      YYACCEPT;
                  }
                  else
                      yywarnf("Program '%s' already inherited"
                             , get_txt(inherit.prog->name));
              }
          }

          if (!(inherit.inherit_type & INHERIT_TYPE_DUPLICATE))
          {
              /* Copy the functions and variables, and take
               * care of the initializer.
               */
              int initializer;

              copy_structs(ob->prog, $1[0]);

              initializer = copy_functions(ob->prog, $1[0]);
              copy_variables(ob->prog, $1[1]);

              if (initializer > -1)
              {
                  /* We inherited a __INIT() function: create a call */

                  transfer_init_control();
                  ins_f_code(F_SAVE_ARG_FRAME);
                  ins_f_code(F_CALL_INHERITED);
                  ins_short(INHERIT_COUNT);
                  ins_short(initializer);
                  ins_f_code(F_RESTORE_ARG_FRAME);
                  ins_f_code(F_POP_VALUE);
                  add_new_init_jump();
              }

              /* Fix up the inherit indices */
              fix_function_inherit_indices(ob->prog);

              /* Update and store the inherit structure.
               *
               * If the program was inherited non-virtual, the v_i_offset
               * may become negative here if the program itself inherits
               * other programs with variables virtually. That is ok
               * because in the final program the sub-inherited virtual
               * variables no longer are immediately before the programs
               * non-virtual variables, but the program's code doesn't know
               * that and continues to 'offset over' them.
               */
              inherit.variable_index_offset
                = $1[1] & TYPE_MOD_VIRTUAL
                  ? V_VARIABLE_COUNT - ob->prog->num_variables
                  : (NV_VARIABLE_COUNT - ob->prog->num_variables)
                    | NON_VIRTUAL_OFFSET_TAG;
              add_to_mem_block(A_INHERITS, &inherit, sizeof inherit);
              num_virtual_variables = V_VARIABLE_COUNT;
          } /* if (!(inherit.inherit_type & INHERIT_TYPE_DUPLICATE)) */
      }
; /* inheritance */


inheritance_qualifiers:
      /* Inheritance can be qualified simple ("public inherit...")
       * or separate for code and variables.
       */

      inheritance_modifier_list
      {
          $$[0] = $$[1] = $1;

          /* Allow 'static nosave inherit foo' as the short form
           * of 'static functions nosave variables inherit foo'; meaning
           * that we have to prevent the qualifier test in the
           * inheritance rule from triggering.
           */
          if ($1 & TYPE_MOD_NOSAVE)
          {
              $$[0] ^= TYPE_MOD_NOSAVE;
          }

      }

    | inheritance_qualifier inheritance_qualifiers
      {
          $$[0] = $1[0] | $2[0];
          $$[1] = $1[1] | $2[1];
      }
; /* inheritance_qualifiers */


inheritance_modifier:
      L_VIRTUAL { $$ = TYPE_MOD_VIRTUAL; } ;


inheritance_modifier_list:
      type_modifier_list
      { $$ = $1; }
    | inheritance_modifier_list inheritance_modifier type_modifier_list
      { $$ = $1 | $2 | $3; }
; /* inheritance_modifier_list */


inheritance_qualifier:
      type optional_star L_IDENTIFIER
      {
          static ident_t    *last_identifier;
          static typeflags_t last_modifier;
%line

          /* The inherit statement must only specify visibility
           * e.g. not "inherit int * foobar"
           */
          if ($1.typeflags & TYPE_MOD_MASK)
          {
              yyerror("syntax error");
          }

          /* Check if there were any modifiers at all */
          if ( !($1.typeflags & ~TYPE_MOD_MASK) )
          {
              /* take lookahead into account */
              if ($3 == last_identifier)
              {
                  last_identifier = NULL;
                  $$[0] = $$[1] = 0;
                  break; /* TODO: Assumes that byacc uses a switch() */
              }
          }
          else
          {
              last_modifier = $1.typeflags & ~TYPE_MOD_MASK;
          }

          last_identifier = $3;

          if ($2) /* No "*" allowed TODO: So why it's there? */
          {
              yyerror("syntax error");
          }

          /* The L_IDENTIFIER must be one of "functions" or "variables" */
          if (mstreq(last_identifier->name, STR_FUNCTIONS))
          {
                $$[0] = last_modifier;
                $$[1] = 0;
          }
          else if (mstreq(last_identifier->name, STR_VARIABLES))
          {
                $$[0] = 0;
                $$[1] = last_modifier;
          }
          else
          {
              yyerrorf("Unrecognized inheritance modifier '%s'"
                      , get_txt(last_identifier->name));
              $$[0] = $$[1] = 0;
          }
      }
; /* inheritance_qualifier */

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
/* Default visibility.
 *
 * We use the inheritance modifier notation to specify the default
 * visibility of functions and variables.
 */

default_visibility:
    L_DEFAULT inheritance_qualifiers ';'
      {
          if ($2[0] & ~( TYPE_MOD_PRIVATE | TYPE_MOD_PUBLIC
                       | TYPE_MOD_PROTECTED | TYPE_MOD_STATIC | TYPE_MOD_DEPRECATED)
             )
          {
              yyerror("Default visibility specification for functions "
                      "accepts only 'private', 'protected', 'public', "
                      "'static' or 'deprecated'");
              YYACCEPT;
          }

          if ($2[1] & ~( TYPE_MOD_PRIVATE | TYPE_MOD_PUBLIC
                       | TYPE_MOD_PROTECTED | TYPE_MOD_DEPRECATED)
             )
          {
              yyerror("Default visibility specification for variables "
                      "accepts only 'private', 'protected', 'public' "
                      "or 'deprecated'"
                      );
              YYACCEPT;
          }

          default_funmod = $2[0];
          default_varmod = $2[1];
      }
; /* default_visibility */

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
/* Type specifications and casts
 *
 * The type rules are used to parse variable and function types, casts,
 * or just visibility e.g for inheritance.
 */

optional_star:
      /* empty */ { $$ = 0; }
    | '*'         { $$ = TYPE_MOD_POINTER; } ;


type: type_modifier_list opt_basic_type
      {
          set_fulltype($$, $1 | $2.typeflags, $2.t_struct);
      } ;


non_void_type: type_modifier_list opt_basic_non_void_type
      {
          set_fulltype($$, $1 | $2.typeflags, $2.t_struct);
      } ;


type_modifier_list:
      /* empty */                      { $$ = 0; }
    | type_modifier_list type_modifier { $$ = $1 | $2; } ;


type_modifier:
      L_NO_MASK    { $$ = TYPE_MOD_NO_MASK; }
    | L_STATIC     { $$ = TYPE_MOD_STATIC; }
    | L_PRIVATE    { $$ = TYPE_MOD_PRIVATE; }
    | L_PUBLIC     { $$ = TYPE_MOD_PUBLIC; }
    | L_VARARGS    { $$ = TYPE_MOD_VARARGS; }
    | L_PROTECTED  { $$ = TYPE_MOD_PROTECTED; }
    | L_NOSAVE     { $$ = TYPE_MOD_NOSAVE; }
    | L_DEPRECATED   { $$ = TYPE_MOD_DEPRECATED; }
;


opt_basic_type:
      basic_type
    | /* empty */ { $$ = Type_Unknown; } ;


opt_basic_non_void_type:
      basic_non_void_type
    | /* empty */ { $$ = Type_Unknown; } ;


basic_non_void_type:
      L_STATUS       { $$ = Type_Number;  }
    | L_INT          { $$ = Type_Number;  }
    | L_STRING_DECL  { $$ = Type_String;  }
    | L_OBJECT       { $$ = Type_Object;  }
    | L_CLOSURE_DECL { $$ = Type_Closure; }
    | L_SYMBOL_DECL  { $$ = Type_Symbol;  }
    | L_FLOAT_DECL   { $$ = Type_Float;   }
    | L_MAPPING      { $$ = Type_Mapping; }
    | L_MIXED        { $$ = Type_Any;     }
    | L_STRUCT identifier
      {
          int num;

          num = find_struct($2);
          if (num < 0)
          {
              yyerrorf("Unknown struct '%s'", get_txt($2));
              $$ = Type_Unknown;
          }
          else
          {
              $$.typeflags = TYPE_STRUCT;
              $$.t_struct = STRUCT_DEF(num).type;
          }

          free_mstring($2);
      }

; /* basic_non_void_type */


basic_type:
      basic_non_void_type
    | L_VOID         { $$ = Type_Void;    }
; /* basic_type */


cast:
      '(' basic_type optional_star ')'
      {
          set_fulltype($$, $2.typeflags | $3, $2.t_struct);
      }
;


/* TODO: Remove decl_casts - they are practically useless */
decl_cast:
      '(' '{' basic_type optional_star '}' ')'
      {
          set_fulltype($$, $3.typeflags | $4, $3.t_struct);
      }
;


/* A generic identifier */

identifier:
      L_IDENTIFIER
      {
          string_t *p;

          /* Extract the string from the ident structure */
          p = ref_mstring($1->name);
          $$ = p;
      }

    | L_LOCAL
      {
          $$ = ref_mstring($1->name);
      }
;


/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/* Argument and variable definitions
 */

argument:
      /* empty */ { $$ = 0; }
    | L_VOID { $$ = 0; }
    | argument_list ;


argument_list:
      new_arg_name                   { $$ = 1; }
    | argument_list ',' new_arg_name { $$ = $1 + 1; } ;


new_arg_name:
      non_void_type optional_star L_IDENTIFIER
      {
          if (exact_types.typeflags && $1.typeflags == 0)
          {
              yyerror("Missing type for argument");
              add_local_name($3, Type_Any, block_depth);
                /* Supress more errors */
          }
          else
          {
              fulltype_t argtype;

              set_fulltype(argtype, $1.typeflags | $2, $1.t_struct);
              add_local_name($3, argtype, block_depth);
          }
      }

    | non_void_type optional_star L_LOCAL
      {
          /* A local name is redeclared. */
          if (current_inline == NULL)
          {
              /* Since this is the argument list of a function, it can't be
               * legal.
               */
              yyerror("Illegal to redeclare local name");
          }
          else
          {
              /* However, it is legal for the argument list of an inline
               * closure.
               */
              fulltype_t argtype;

              set_fulltype(argtype, $1.typeflags | $2, $1.t_struct);
              redeclare_local($3, argtype, block_depth);
          }
      }
; /* new_arg_name */



name_list:
      /* Simple variable definition */
      type optional_star L_IDENTIFIER
      {
%line
          if ($1.typeflags == 0)
              yyerror("Missing type");

          define_global_variable($3, $1, $2, MY_FALSE);
          $$ = $1;
      }

    /* Variable definition with initialization */

    | type optional_star L_IDENTIFIER
      {
          if ($1.typeflags == 0)
              yyerror("Missing type");

          $<number>$ = define_global_variable($3, $1, $2, MY_TRUE); 
      }

      L_ASSIGN expr0
      {
          init_global_variable($<number>4, $3, $1, $2, $5, $6.type);
          $$ = $1;
      }

    | name_list ',' optional_star L_IDENTIFIER
      {
          define_global_variable($4, $1, $3, MY_FALSE);
          $$ = $1;
      }

    /* Variable definition with initialization */

    | name_list ',' optional_star L_IDENTIFIER
      {
          $<number>$ = define_global_variable($4, $1, $3, MY_TRUE); 
      }

      L_ASSIGN expr0
      {
          init_global_variable($<number>5, $4, $1, $3, $6, $7.type);
          $$ = $1;
      }

; /* name_list */


/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
/* Blocks and simple statements.
 */

block: '{' statements_block '}'


statements_block:

      { enter_block_scope(); }

      statements

      {
          /* If this is a local block, the declarations inserted
           * a code fragment to zero out the locals (previous blocks
           * may have left values in them). Complete the fragment
           * with the number of locals to clear, now that we
           * know it.
           */
          {
              block_scope_t *scope = block_scope + block_depth - 1;

              if (scope->num_locals > scope->num_cleared)
              {
                  mem_block[A_PROGRAM].block[scope->addr+2]
                    = (char)(scope->num_locals - scope->num_cleared);
              }
          }
     
          leave_block_scope(MY_FALSE);
      }
; /* block_statements */


statements:
      /* empty */
    | statements local_name_list ';'
    | statements statement
;


local_name_list:
      basic_type optional_star L_IDENTIFIER
      {
          struct lvalue_s lv;
          define_local_variable($3, $1, $2, &lv, MY_FALSE, MY_FALSE);
          
          $$ = $1;
      }
    | basic_type optional_star L_LOCAL
      {
          struct lvalue_s lv;
          define_local_variable($3, $1, $2, &lv, MY_TRUE, MY_FALSE);
          
          $$ = $1;
      }
    | basic_type optional_star L_IDENTIFIER
      {
          define_local_variable($3, $1, $2, &$<lvalue>$, MY_FALSE, MY_TRUE);
      }
      L_ASSIGN expr0
      {
          init_local_variable($3, &$<lvalue>4, $5, $6.type);
          
          $$ = $1;
      }
    | basic_type optional_star L_LOCAL
      {
          define_local_variable($3, $1, $2, &$<lvalue>$, MY_TRUE, MY_TRUE);
      }
      L_ASSIGN expr0
      {
          init_local_variable($3, &$<lvalue>4, $5, $6.type);
          
          $$ = $1;
      }
    | local_name_list ',' optional_star L_IDENTIFIER
      {
          struct lvalue_s lv;
          define_local_variable($4, $1, $3, &lv, MY_FALSE, MY_FALSE);
          
          $$ = $1;
      }
    | local_name_list ',' optional_star L_LOCAL
      {
          struct lvalue_s lv;
          define_local_variable($4, $1, $3, &lv, MY_TRUE, MY_FALSE);
          
          $$ = $1;
      }
    | local_name_list ',' optional_star L_IDENTIFIER
      {
          define_local_variable($4, $1, $3, &$<lvalue>$, MY_FALSE, MY_TRUE);
      }
      L_ASSIGN expr0
      {
          init_local_variable($4, &$<lvalue>5, $6, $7.type);
          
          $$ = $1;
      }
    | local_name_list ',' optional_star L_LOCAL
      {
          define_local_variable($4, $1, $3, &$<lvalue>$, MY_TRUE, MY_TRUE);
      }
      L_ASSIGN expr0
      {
          init_local_variable($4, &$<lvalue>5, $6, $7.type);
          
          $$ = $1;
      }
; /* local_name_list */


statement:
      comma_expr ';'
      {
          insert_pop_value();
#ifdef F_BREAK_POINT
          if (d_flag)
              ins_f_code(F_BREAK_POINT);
#endif /* F_BREAK_POINT */
          /* if (exact_types && !BASIC_TYPE($1.type, TYPE_VOID))
           *    yyerror("Value thrown away");
           */
      }

    | error ';' /* Synchronisation point */
    | cond | while | do | for | foreach | switch
    | return ';'
    | block
    | /* empty */ ';'

    | L_BREAK ';'
      {
          /* Compile the break statement */

          if (current_break_address == 0)
              yyerror("break statement outside loop");

          if (current_break_address & BREAK_ON_STACK)
          {
              /* We break from a switch() */

              ins_f_code(F_BREAK);
          }
          else
          {
              /* A normal loop break: add the FBRANCH to the list */

              ins_f_code(F_FBRANCH);
              ins_jump_offset(current_break_address & BREAK_ADDRESS_MASK);
              current_break_address = CURRENT_PROGRAM_SIZE - sizeof(int32);
              if (current_break_address > BREAK_ADDRESS_MASK)
                  yyerrorf("Compiler limit: (L_BREAK) value too large: %"PRIdPINT
                          , current_break_address);
          }
      }

    | L_CONTINUE ';'        /* This code is a jump */
      {
          p_int depth;
%line
          if (current_continue_address == 0)
              yyerror("continue statement outside loop");

          if ( 0 != (depth = (current_continue_address & SWITCH_DEPTH_MASK)) )
          {
              /* A continue inside a switch */

              /* For more than 255 nested switches, generate a series
               * of BREAKN_CONTINUE instructions.
               */
              while (depth > SWITCH_DEPTH_UNIT*256)
              {
                  ins_f_code(F_BREAKN_CONTINUE);
                  ins_byte(255);
                  ins_jump_offset(4);
                  depth -= SWITCH_DEPTH_UNIT*256;
              }

              /* BREAK_CONTINUE the last switches */
              if (depth > SWITCH_DEPTH_UNIT)
              {
                  depth /= SWITCH_DEPTH_UNIT;
                  ins_f_code(F_BREAKN_CONTINUE);
                  ins_byte(depth-1);
              }
              else
              {
                  ins_f_code(F_BREAK_CONTINUE);
              }
          }
          else
          {
              /* Normal continue */
              ins_f_code(F_FBRANCH);
          }

          /* In either case, handle the list of continues alike */
          ins_jump_offset(current_continue_address & CONTINUE_ADDRESS_MASK);
          current_continue_address =
                        ( current_continue_address & SWITCH_DEPTH_MASK ) |
                        ( CURRENT_PROGRAM_SIZE - sizeof(int32) );
      }
; /* statement */


return:
      L_RETURN
      {
          fulltype_t rtype = exact_types;

          rtype.typeflags &= TYPE_MOD_MASK;

          if (exact_types.typeflags
           && !BASIC_TYPE(rtype, Type_Void))
              type_error("Must return a value for a function declared",
                         exact_types);
          ins_f_code(F_RETURN0);
      }

    | L_RETURN comma_expr
      {
%line
          fulltype_t type2 = $2.type;

          if (exact_types.typeflags)
          {
              fulltype_t rtype = exact_types;

              rtype.typeflags &= TYPE_MOD_MASK;

              /* More checks, ie. mixed vs non-mixed, would be nice,
               * but the general type tracking is too lacking for it.
               */
              if (!MASKED_TYPE(type2, rtype))
              {
                  char tmp[100];
                  strcpy(tmp, get_type_name(type2));
                  yyerrorf("Return type not matching: got %s, expected %s"
                         , tmp, get_type_name(rtype));
              }
          }

          if (type2.typeflags & TYPE_MOD_REFERENCE)
          {
              yyerror("May not return a reference");
          }

          if (last_expression == CURRENT_PROGRAM_SIZE - 1
           && mem_block[A_PROGRAM].block[last_expression] ==
                  F_CONST0 )
          {
              /* Optimize "CONST0 RETURN" to "RETURN0" */
              mem_block[A_PROGRAM].block[last_expression] =
                    F_RETURN0;
              last_expression = -1;
          }
          else
              ins_f_code(F_RETURN);
      }
; /* return */

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
/* The while() statement
 *
 * It is compiled into:
 *
 *       BRANCH c
 *    l: <body>
 *    c: <cond>
 *       BBRANCH_WHEN_NON_ZERO l
 */

while:
      {
          /* Save the previous environment */

          $<numbers>$[0] = current_continue_address;
          $<numbers>$[1] = current_break_address;

          push_address(); /* Remember the starting address */
      }

      L_WHILE '(' comma_expr ')'

      {
%line
          p_int addr = pop_address();
          p_int length = CURRENT_PROGRAM_SIZE - addr;
          bytecode_p expression;

          /* Take the <cond> code, add the BBRANCH instruction and
           * store all of it outside the program. After the <body>
           * has been compiled, the code will be put back in.
           */
          expression = yalloc(length+2);
          memcpy(expression, mem_block[A_PROGRAM].block+addr, length);
          if (last_expression == CURRENT_PROGRAM_SIZE - 1
           && expression[length-1] == F_NOT
             )
          {
              /* Optimizize
               *   NOT
               *   BBRANCH_WHEN_NON_ZERO
               * into
               *   BBRANCH_WHEN_ZERO
               */
              length--;
              expression[length] = F_BBRANCH_WHEN_ZERO;
          }
          else
          {
              expression[length] = F_BBRANCH_WHEN_NON_ZERO;
          }

          /* Save the code as 'expression' */
          $<expression>$.p = expression;
          $<expression>$.length = length;
          $<expression>$.line = current_loc.line;

          /* Restart codegeneration for the body where we began */
          CURRENT_PROGRAM_SIZE = addr;
          last_expression = -1;

          /* The initial branch to the condition code */
          ins_f_code(F_BRANCH);
          push_address();
          ins_byte(0);

          current_continue_address = CONTINUE_DELIMITER;
          current_break_address = BREAK_DELIMITER;
      }

      statement

      {
%line
          /* The body compiled ok. Now patch up the breaks and continues
           * and insert the condition checking.
           */

          p_int offset;
          bc_offset_t next_addr;
          p_int addr = pop_address();

          /* Update the offsets of all continue BRANCHes
           * (resp BREAK_CONTINUEs) to branch to the current address.
           */
          for ( ; current_continue_address > 0
                ; current_continue_address = next_addr)
          {
              next_addr = read_jump_offset(current_continue_address);
              upd_jump_offset(current_continue_address,
                  CURRENT_PROGRAM_SIZE - current_continue_address);
          }

          /* If necessary, update the leading BRANCH to an LBRANCH */
          offset = fix_branch( F_LBRANCH, CURRENT_PROGRAM_SIZE, addr);

          /* Add the condition code to the program */
          if ($<expression>6.line != current_loc.line)
              store_line_number_info();
          add_to_mem_block(A_PROGRAM, $<expression>6.p, $<expression>6.length+2);
          yfree($<expression>6.p);

          /* Complete the branch at the end of the condition code */
          offset += addr + 1 - ( CURRENT_PROGRAM_SIZE - 1 );
          if (offset < -0xff)
          {
              /* We need a LBRANCH instead of the BBRANCH */

              bytecode_p codep;

              if (offset < -0x8000)
                  yyerror("offset overflow");
              codep = PROGRAM_BLOCK + --CURRENT_PROGRAM_SIZE - 1;
              *codep = *codep == F_BBRANCH_WHEN_NON_ZERO
                       ? F_LBRANCH_WHEN_NON_ZERO
                       : F_LBRANCH_WHEN_ZERO
              ;
              ins_short(offset);
          }
          else
          {
              /* Just add the short offset */
              mem_block[A_PROGRAM].block[CURRENT_PROGRAM_SIZE-1] = -offset;
          }

          if ($<expression>6.line != current_loc.line)
              store_line_number_relocation($<expression>6.line);

          /* Now that we have the end of the while(), we can finish
           * up the breaks.
           */
          for( ; current_break_address > 0
               ; current_break_address = next_addr)
          {
              next_addr = read_jump_offset(current_break_address);
              upd_jump_offset(current_break_address,
                  CURRENT_PROGRAM_SIZE - current_break_address);
          }

          /* Restore the previous environment */
          current_continue_address = $<numbers>1[0];
          current_break_address    = $<numbers>1[1];
      }
; /* while */

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
/* The do-while() statement
 *
 * It is compiled into:
 *
 *    l: <body>
 *       <cond>
 *       BBRANCH_WHEN_NON_ZERO l
 */

do:
      {
          /* Save the previous environment */
          $<numbers>$[0] = current_continue_address;
          $<numbers>$[1] = current_break_address;

          current_break_address = BREAK_DELIMITER;
          current_continue_address = CONTINUE_DELIMITER;

          push_address(); /* Address to branch back to */
      }

      L_DO statement L_WHILE

      {
          /* The body is complete - we can already patch up
           * the continue statements.
           */

          bc_offset_t next_addr;
          p_int current;
%line
          current = CURRENT_PROGRAM_SIZE;
          for(; current_continue_address > 0
              ; current_continue_address = next_addr)
          {
              next_addr = read_jump_offset(current_continue_address);
              upd_jump_offset(current_continue_address,
                  current - current_continue_address);
          }
      }

      '(' comma_expr ')' ';'

      {
%line
          /* The loop is complete - we just need the final branch
           * instruction and to patch up the breaks.
           */

          p_int offset;
          bc_offset_t next_addr;
          p_int addr = pop_address();
          mp_uint current;
          bytecode_p dest;

          current = CURRENT_PROGRAM_SIZE;
          if (!realloc_a_program(3))
          {
              yyerrorf("Out of memory: program size %"PRIuMPINT"\n", current+3);
              YYACCEPT;
          }

          /* Add the branch statement */
          dest = PROGRAM_BLOCK + current;
          if (current == last_expression + 1 && dest[-1] == F_NOT)
          {
              /* Optimize 'NOT BBRANCH_WHEN_NON_ZERO' to 'BBRANCH_WHEN_ZERO'
               */
              offset = addr - current;
              if (offset < -0xff)
              {
                  if (offset < -0x8000)
                      yyerror("offset overflow");
                  PUT_CODE(dest-1, F_LBRANCH_WHEN_ZERO);
                  PUT_SHORT(dest, offset);
                  current += 2;
              }
              else
              {
                  PUT_CODE(dest-1, F_BBRANCH_WHEN_ZERO);
                  PUT_UINT8(dest, -offset);
                  current++;
              }
          }
          else
          {
              offset = addr - ( current + 1 );
              if (offset < -0xff) {
                  if (offset < -0x8000)
                      yyerror("offset overflow");
                  STORE_CODE(dest, F_LBRANCH_WHEN_NON_ZERO);
                  STORE_SHORT(dest, offset);
                  current += 3;
              } else {
                  STORE_CODE(dest, F_BBRANCH_WHEN_NON_ZERO);
                  STORE_UINT8(dest, -offset);
                  current += 2;
              }
          }

          CURRENT_PROGRAM_SIZE = current;

          /* Now that we have the end of the do-while(), we can finish
           * up the breaks.
           */
          for (; current_break_address > 0
               ; current_break_address = next_addr)
          {
              next_addr = read_jump_offset(current_break_address);
              upd_jump_offset(current_break_address,
                  current - current_break_address);
          }

          /* Restore the previous environment */
          current_continue_address = $<numbers>1[0];
          current_break_address    = $<numbers>1[1];
      }
; /* do */

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
/* The for() statement.
 *
 * It is compiled as:
 *
 *    CLEAR_LOCALS
 *    <init>
 *    POP
 *    BRANCH c
 * l: <body>
 *    <incr>
 *    POP
 * c: <cond>
 *    BBRANCH_WHEN_NON_ZERO l
 */

for:
      L_FOR '('

      {
%line
          /* Save the previous environment */
          $<numbers>$[0] = current_continue_address;
          $<numbers>$[1] = current_break_address;

          /* Open a new scope to all variables local to the
           * for-statement as a whole.
           */
          enter_block_scope();
      }

      for_init_expr ';'

      {
%line
          /* Get rid of whatever init_expr computed */
          insert_pop_value();

          /* From here, the <body> will be placed eventually */

          current_continue_address = CONTINUE_DELIMITER;
          $<number>$ = CURRENT_PROGRAM_SIZE;
      }

      for_expr ';'

      {
%line
          /* Add the BBRANCH to the condition and save it all
           * in an 'expression' on the compiler stack for later
           * re-insertion.
           */

          p_int start, length;
          bytecode_p expression;

          start = $<number>6;
          length = CURRENT_PROGRAM_SIZE - start;
          expression = yalloc(length+2);
          memcpy(expression, mem_block[A_PROGRAM].block + start, length );

          /* Add the branch instruction */
          if (last_expression == CURRENT_PROGRAM_SIZE - 1
           && expression[length-1] == F_NOT
             )
          {
              /* Optimize 'NOT BBRANCH_WHEN_NON_ZERO'
               * to 'BBRANCH_WHEN_ZERO'
               */
              length--;
              expression[length] = F_BBRANCH_WHEN_ZERO;
          }
          else
          {
              expression[length] = F_BBRANCH_WHEN_NON_ZERO;
          }

          /* Save the codeblock on the stack */
          $<expression>$.p = expression;
          $<expression>$.length = length;
          $<expression>$.line = current_loc.line;

          /* Restart codegeneration from here */
          CURRENT_PROGRAM_SIZE = start;
          last_expression = -1;
      }

      for_expr ')'

      {
%line
          /* Save the <incr> code block on the compiler stack
           * for later re-insertion and start the compilation
           * of the loop body.
           */

          p_int length;

          /* Save the code block */
          insert_pop_value();
          length = CURRENT_PROGRAM_SIZE - $<number>6;
          $<expression>$.p = yalloc(length);
          if (length)
              memcpy( $<expression>$.p
                    , mem_block[A_PROGRAM].block + $<number>6
                    , length );
          $<expression>$.length = length;
          $<expression>$.line = current_loc.line;

          /* Restart the codegeneration for the body */
          CURRENT_PROGRAM_SIZE = $<number>6;
          last_expression = -1;
          current_break_address = BREAK_DELIMITER;

          ins_f_code(F_BRANCH); /* over the body to the condition */
          ins_byte(0);

          /* Fix the number of locals to clear, now that we know it
           */
          {
              block_scope_t *scope = block_scope + block_depth - 1;

              if (scope->num_locals > scope->num_cleared)
              {
                  mem_block[A_PROGRAM].block[scope->addr+2]
                    = (char)(scope->num_locals - scope->num_cleared);
              }
          }
      }

      statement

      {
%line
          /* The loop is complete, now add the <incr> and <cond>
           * code saved on the compiler stack and patch up
           * the break and continues.
           */

          p_int offset;
          bc_offset_t next_addr;

          /* Patch up the continues */
          for (; current_continue_address > 0
               ; current_continue_address = next_addr)
          {
              next_addr = read_jump_offset(current_continue_address);
              upd_jump_offset(current_continue_address,
                  CURRENT_PROGRAM_SIZE - current_continue_address);
          }

          if ( $<expression>9.line != current_loc.line
           || (    $<expression>12.line != current_loc.line
                && $<expression>12.length)
             )
              store_line_number_info();

          /* Add the <incr> code block if needed */
          if ($<expression>12.length)
          {
              add_to_mem_block(A_PROGRAM, $<expression>12.p
                                        , $<expression>12.length);
              if ($<expression>12.line != $<expression>9.line)
                  store_line_number_relocation($<expression>12.line);
          }
          yfree($<expression>12.p);

          /* Fix the branch over the body */
          offset =
            fix_branch( F_LBRANCH, CURRENT_PROGRAM_SIZE, $<number>6 + 1);

          /* Add the <cond> code block */
          add_to_mem_block(A_PROGRAM, $<expression>9.p, $<expression>9.length+2);
          yfree($<expression>9.p);

          /* Create the branch back after the condition */
          offset += $<number>6 + 2 - ( CURRENT_PROGRAM_SIZE - 1 );
          if (offset < -0xff)
          {
              bytecode_p codep;

              if (offset < -0x8000)
                  yyerror("offset overflow");

              codep = PROGRAM_BLOCK + --CURRENT_PROGRAM_SIZE - 1;
              *codep = *codep == F_BBRANCH_WHEN_NON_ZERO
                       ? F_LBRANCH_WHEN_NON_ZERO
                       : F_LBRANCH_WHEN_ZERO
              ;
              ins_short(offset);
          }
          else
          {
              mem_block[A_PROGRAM].block[CURRENT_PROGRAM_SIZE-1] = -offset;
          }

          if ($<expression>9.line != current_loc.line)
              store_line_number_relocation($<expression>9.line);

          /* Now complete the break instructions.
           */
          for (; current_break_address > 0
               ; current_break_address = next_addr)
          {
              next_addr = read_jump_offset(current_break_address);
              upd_jump_offset(current_break_address,
                  CURRENT_PROGRAM_SIZE - current_break_address);
          }

          /* Restore the previous environment */
          current_continue_address = $<numbers>3[0];
          current_break_address    = $<numbers>3[1];

          /* and leave the for scope */
          leave_block_scope(MY_FALSE);
      }
; /* for */

/* Special rules for 'int <name> = <expr>' declarations in the first
 * for() expression.
 */
for_init_expr:
      /* EMPTY */
      {
          last_expression = mem_block[A_PROGRAM].current_size;
          ins_number(1);
            /* insert_pop_value() will optimize this away */
      }
    | comma_expr_decl
; /* for_init_expr */


comma_expr_decl:
      expr_decl
    | comma_expr_decl
      {
          insert_pop_value();
      }
      ',' expr_decl
; /* comma_expr_decl */


expr_decl:
      expr0 /* compile the expression as usual */

    | local_name_lvalue L_ASSIGN expr0
      {
          /* We got a "int <name> = <expr>" type expression. */

%line
          fulltype_t type2;

          /* Check the assignment for validity */
          type2 = $3.type;
          if (exact_types.typeflags
           && !compatible_types($1.type, type2, MY_TRUE))
          {
              yyerrorf("Bad assignment %s", get_two_types($1.type, type2));
          }

          if ($2 != F_ASSIGN)
          {
              yyerror("Only plain assignments allowed here");
          }

          if (type2.typeflags & TYPE_MOD_REFERENCE)
              yyerror("Can't trace reference assignments");

          /* Add the bytecode to create the lvalue and do the
           * assignment.
           */
          if (!add_lvalue_code(&$1, $2))
              YYACCEPT;
      }

    | local_name_lvalue
      {
          /* We got a "int <name>" type expression.
           * Compile it as if it was a "int <name> = 0" expression.
           */
%line

          /* Insert the implied push of number 0 */
          ins_number(0);

          /* Add the bytecode to create the lvalue and do the
           * assignment.
           */
          if (!add_lvalue_code(&$1, F_ASSIGN))
              YYACCEPT;
      }
; /* expr_decl */


for_expr:
      /* EMPTY */
      {
          last_expression = mem_block[A_PROGRAM].current_size;
          ins_number(1);
      }
    | comma_expr
; /* for_expr */

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
/* The foreach() statement
 *
 * It is compiled into:                      or when <statement> is empty:
 *
 *       CLEAR_LOCALS                          CLEAR_LOCALS
 *       PUSH_(LOCAL_)LVALUE <var1>            <expr>
 *       ...                                   POP_VALUE
 *       PUSH_(LOCAL_)LVALUE <varn>           [POP_VALUE for integer ranges]
 *       <expr>
 *       FOREACH(_REF) <numargs> c
 *    l: <body>
 *    c: FOREACH_NEXT l
 *    e: FOREACH_END
 *
 * continue's branch to c, break's to e.
 */

foreach:

      L_FOREACH '('

      {
          /* Save the previous environment */
          $<numbers>$[0] = current_continue_address;
          $<numbers>$[1] = current_break_address;

          current_break_address = BREAK_DELIMITER;
          current_continue_address = CONTINUE_DELIMITER;

          /* Open a new scope to all variables local to the
           * foreach-statement as a whole.
           */
          enter_block_scope();
      }

      foreach_vars foreach_in

      {
%line
          /* Remember the starting address of the expression */
          $<address>$ = CURRENT_PROGRAM_SIZE;
      }

      foreach_expr ')'

      {
%line
          /* Fix the number of locals to clear, now that we know it
           */
          {
              block_scope_t *scope = block_scope + block_depth - 1;

              if (scope->num_locals > scope->num_cleared)
              {
                  mem_block[A_PROGRAM].block[scope->addr+2]
                    = (char)(scope->num_locals - scope->num_cleared);
              }
          }

          /* Create the FOREACH instruction, leaving the branch field
           * blank.
           */
          switch ($7)
          {
          case FOREACH_LOOP:
              ins_f_code(F_FOREACH); break;
          case FOREACH_REF:
              ins_f_code(F_FOREACH_REF); break;
          case FOREACH_RANGE:
              ins_f_code(F_FOREACH_RANGE); break;
          default:
              yyerrorf("Unknown foreach_expr type %ld.\n", (long)$7);
              fatal("Unknown foreach_expr type %ld.\n", (long)$7);
              /* NOTREACHED */
          }
          ins_byte($4+1);
          ins_short(0);

          push_address(); /* Address to branch back to */
      }

      statement

      {
          /* The body is complete - patch up the continue and
           * break statements and generate the remaining statements.
           */

          bc_offset_t next_addr;
          p_int addr;
          mp_uint current;

%line
          current = CURRENT_PROGRAM_SIZE;
          addr = pop_address(); /* Where the body began */

          /* One obvious optimisation: when there is no code in
           * the body, we can save space and even more time by
           * just compiling the expression.
           * Too bad that we can't find out whether the expression
           * has side effects or not, otherwise we could try to
           * remove it, too.
           */
          if (addr == (p_int)current)
          {
              p_int expr_addr;  /* Address of the expr0 */
              p_int start_addr; /* Address of the first PUSH_LOCAL_LVALUE */
              bytecode_p src, dest;

              expr_addr = $<address>6;
              start_addr = expr_addr - $4*2;
              current = start_addr + (addr - 4 - expr_addr);
              for ( src = PROGRAM_BLOCK + expr_addr,
                    dest = PROGRAM_BLOCK + start_addr
                  ; expr_addr < addr-4
                  ; src++, dest++, expr_addr++)
                  *dest = *src;
              CURRENT_PROGRAM_SIZE = current;
              ins_f_code(F_POP_VALUE);
              current++;
              if ($7 == FOREACH_RANGE)
              {
                  ins_f_code(F_POP_VALUE);
                  current++;
              }
          }
          else /* Create the full statement */
          {
              /* First patch up the continue statements */

              for(; current_continue_address > 0
                  ; current_continue_address = next_addr)
              {
                  next_addr = read_jump_offset(current_continue_address);
                  upd_jump_offset(current_continue_address,
                      current - current_continue_address);
              }

              /* Create the FOREACH_NEXT instruction and update
               * the branch of the earlier F_FOREACH.
               */

              upd_short(addr - 2, current - addr);

              ins_f_code(F_FOREACH_NEXT);
              ins_short(current + 3 - addr);

              current += 3;

              /* Finish up the breaks.
               */
              for (; current_break_address > 0
                   ; current_break_address = next_addr)
              {
                  next_addr = read_jump_offset(current_break_address);
                  upd_jump_offset(current_break_address,
                      current - current_break_address);
              }

              /* Finish with the FOREACH_END.
               */
              ins_f_code(F_FOREACH_END);
          }

          /* Restore the previous environment */
          current_continue_address = $<numbers>3[0];
          current_break_address    = $<numbers>3[1];

          /* and leave the scope */
          leave_block_scope(MY_FALSE);
      }
; /* foreach */


foreach_vars : /* Parse and count the number of lvalues */
      foreach_var_decl                   { $$ = 1; }
    | foreach_vars ',' foreach_var_decl  { $$ = $1 + 1; }
; /* foreach_vars */


foreach_var_decl:  /* Generate the code for one lvalue */

      /* TODO: It is tempting to add an alternative "| lvalue",
       * TODO:: but then we get masses of reduce/reduce conflicts
       * TODO:: between lvalue and expr4. Dunno why.
       */
      foreach_var_lvalue
      {
          /* Add the bytecode to create the lvalue, and good is.
           */

%line
          if (!add_lvalue_code(&$1, 0))
              YYACCEPT;
      }

; /* foreach_var_decl */


foreach_var_lvalue:  /* Gather the code for one lvalue */

      local_name_lvalue
    | name_lvalue

; /* foreach_var_lvalue */

foreach_in:
    /* The purpose of this rule is to avoid making "in" a reserved
     * word. Instead we require an identifier/local with the
     * name "in" as alternative to ":". Main reason to allow "in"
     * is MudOS compatibility.
     * TODO: Make MudOS-compats switchable.
     */

      identifier

      {
          if (!mstreq($1, STR_IN))
              yyerror("Expected keyword 'in' in foreach()");
          free_mstring($1);
      }

    | ':'
; /* foreach_in */

foreach_expr:
      expr0
      {
          fulltype_t dtype;
          Bool       gen_refs;

%line
          gen_refs = ($1.type.typeflags & TYPE_MOD_MASK & (~TYPE_MOD_RMASK)) != 0;
          set_fulltype(dtype, $1.type.typeflags & TYPE_MOD_RMASK, $1.type.t_struct);

          if (!(dtype.typeflags & TYPE_MOD_POINTER)
           && dtype.typeflags != TYPE_ANY
           && dtype.typeflags != TYPE_STRING
           && dtype.typeflags != TYPE_MAPPING
           && (dtype.typeflags != TYPE_NUMBER || gen_refs)
           && (exact_types.typeflags || dtype.typeflags != TYPE_UNKNOWN)
             )
          {
              type_error("Expression for foreach() of wrong type", $1.type);
          }

          $$ = gen_refs ? FOREACH_REF : FOREACH_LOOP;
      }

    | expr0 L_RANGE expr0
      {
          fulltype_t dtype;

%line
          if (($1.type.typeflags & (~TYPE_MOD_RMASK)) != 0)
          {
              type_error("Expression for foreach() of wrong type", $1.type);
          }

          set_fulltype(dtype, $1.type.typeflags & TYPE_MOD_RMASK, $1.type.t_struct);

          if (dtype.typeflags != TYPE_ANY
           && dtype.typeflags != TYPE_NUMBER
           && (exact_types.typeflags || dtype.typeflags != TYPE_UNKNOWN)
             )
          {
              type_error("Expression for foreach() of wrong type", $1.type);
          }

          if (($3.type.typeflags & (~TYPE_MOD_RMASK)) != 0)
          {
              type_error("Expression for foreach() of wrong type", $3.type);
          }

          set_fulltype(dtype, $3.type.typeflags & TYPE_MOD_RMASK, $3.type.t_struct);

          if (dtype.typeflags != TYPE_ANY
           && dtype.typeflags != TYPE_NUMBER
           && (exact_types.typeflags || dtype.typeflags != TYPE_UNKNOWN)
             )
          {
              type_error("Expression for foreach() of wrong type", $3.type);
          }

          $$ = FOREACH_RANGE;
      }
; /* foreach_expr */

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
/* The switch statement.
 *
 * switch.h explains how the bytecode looks like.
 *
 * Note that the actual switch rule is:
 *
 *   switch: L_SWITCH ( comma_expr ) '{' switch_block '}'
 *
 * and that case and default are explicitly parsed in the
 * switch_block rule. Each group of statements after a
 * label have their own scope, so that variable declarations
 * within the switch block may not cross case labels.
 *
 * That also means that in contrast to C the code
 *
 *    switch(x);
 * or switch(x) write("Foo");
 * or switch(x) {{ case "foo": break; }}
 *
 * is syntactically not ok.
 */

switch:
      L_SWITCH '(' comma_expr ')'

      {
        /* We start a new switch(), which might be nested into
         * an outer switch().
         */

        case_state_t *statep;
%line
        current_break_stack_need++;
        if ( current_break_stack_need > max_break_stack_need )
            max_break_stack_need = current_break_stack_need;

        /* Save the previous switch state */
        if ( !(statep = yalloc(sizeof(case_state_t))) )
        {
            yyerrorf("Out of memory: case state (%zu bytes)"
                    , sizeof(case_state_t));
            YYACCEPT;
        }
        *statep = case_state;
        case_state.previous = statep;
        push_explicit(current_break_address);
        push_explicit(switch_pc);

        /* Create the SWITCH instruction plus two empty bytes */
        ins_f_code(F_SWITCH);
        switch_pc = mem_block[A_PROGRAM].current_size;
        ins_short(0);

        /* Set up the new switch generation */
        case_state.list0 = case_state.list1 = NULL;
        case_state.zero = NULL;
        case_state.no_string_labels = MY_TRUE;
        case_state.some_numeric_labels = MY_FALSE;
        case_state.default_addr = 0;

        current_break_address =
                BREAK_ON_STACK | BREAK_FROM_SWITCH | CASE_LABELS_ENABLED ;
        if (current_continue_address)
            current_continue_address += SWITCH_DEPTH_UNIT;
      }
      
      '{'

      switch_block
      
      '}'

      {
%line
        /* The statement (which hopefully contained cases) is complete.
         * Now create the lookup tables and restore the previous state.
         */

        case_state_t *statep;

        current_break_address &=
            ~(BREAK_ON_STACK|BREAK_FROM_SWITCH|CASE_LABELS_ENABLED);

        if (!case_state.default_addr)
        {
            /* no default given -> create one */
            case_state.default_addr = CURRENT_PROGRAM_SIZE-switch_pc;
        }

        /* it isn't unusual that the last case/default has no break */
        ins_f_code(F_BREAK);

        /* Create the lookup tables */
        store_case_labels(
          CURRENT_PROGRAM_SIZE-switch_pc,
          case_state.default_addr,
          case_state.no_string_labels || case_state.some_numeric_labels,
          case_state.zero,
          yyget_space, yymove_switch_instructions, yyerror, yycerrorl
        );

        /* Restore the previous state */
        switch_pc = pop_address();
        current_break_address = pop_address();
        statep = case_state.previous;
        case_state = *statep;
        yfree(statep);
        if (current_continue_address)
            current_continue_address -= SWITCH_DEPTH_UNIT;
        current_break_stack_need--;
      }
; /* switch */


switch_block:
      switch_block switch_statements
    | switch_statements
; /* switch_block */


switch_statements: switch_label statements_block ;


switch_label: case | default ;


case: L_CASE case_label ':'
    {
%line
        /* Mark the current program address as another
         * case target for the current switch.
         */
        case_list_entry_t *temp;

        /* Should be within a switch statement. */
        assert(current_break_address & CASE_LABELS_ENABLED);

        /* Get and fill in a new case entry structure */
        if ( !(temp = new_case_entry()) )
        {
            yyerror("Out of memory: new case entry");
            break;
        }

        if ( !(temp->key = $2.key) )
        {
            case_state.zero = temp;
        }
        temp->addr = mem_block[A_PROGRAM].current_size - switch_pc;
        temp->line = current_loc.line;
    }

    | L_CASE case_label L_RANGE case_label ':'
    {
%line
        /* Mark the current program address as another
         * range-case target for the current switch.
         */

        case_list_entry_t *temp;

        if ( !$2.numeric || !$4.numeric )
            yyerror("String case labels not allowed as range bounds");

        /* Should be within a switch statement. */
        assert(current_break_address & CASE_LABELS_ENABLED);

        /* A range like "case 4..2" is illegal,
         * a range like "case 4..4" counts as simple "case 4".
         */
        if ($2.key >= $4.key)
        {
            if ($2.key > $4.key)
            {
                yyerrorf("Illegal case range: lower limit %ld > upper limit %ld"
                        , (long)$2.key, (long)$4.key);
                break;
            }
            if ( !(temp = new_case_entry()) )
            {
                yyerror("Out of memory: new case entry");
                break;
            }
            temp->key = $2.key;
            temp->addr = CURRENT_PROGRAM_SIZE - switch_pc;
            temp->line = current_loc.line;
        }

        /* Get and fill in the two case entries */

        if ( !(temp = new_case_entry()) )
        {
            yyerror("Out of memory: new case entry");
            break;
        }
        temp->key = $2.key;
        temp->addr = 1; /* marks the lower bound of the range */
        temp->line = current_loc.line;

        if ( !(temp = new_case_entry()) ) {
            yyerror("Out of memory: new case entry");
            break;
        }
        temp->key = $4.key;
        temp->addr = CURRENT_PROGRAM_SIZE - switch_pc;
        temp->line = 0; /* marks the upper bound of the range */
    }
; /* case */


case_label:
      constant
      {
%line
          if ( 0 != ($$.key = $1) ) {
              if ( !(case_state.no_string_labels) )
                  yyerror("Mixed case label list not allowed");
              case_state.some_numeric_labels = 1;
          }
          $$.numeric = MY_TRUE;
      }

    | string_constant
      {
%line
          if ( case_state.some_numeric_labels )
              yyerror("Mixed case label list not allowed");

          case_state.no_string_labels = MY_FALSE;
          store_prog_string(last_string_constant);
          $$.key = (p_int)last_string_constant;
          $$.numeric = MY_FALSE;
          last_string_constant = NULL;
      }
; /* case_label */


default:
      L_DEFAULT ':'
      {
%line
          /* Mark the current program address as the default target
           * for the current switch.
           */

          /* Should be within a switch statement. */
          assert(current_break_address & CASE_LABELS_ENABLED);

          if (case_state.default_addr)
              yyerror("Duplicate default");

          case_state.default_addr = CURRENT_PROGRAM_SIZE - switch_pc;
    }
; /* default */

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
/* The if()-statement.
 *
 * This is compiled as:           resp. as:
 *
 *     <cond>                        <cond>
 *     BRANCH_WHEN_ZERO e            BRANCH_WHEN_ZERO e
 *     <if-part>                     <if-part>
 *  e:                               BRANCH f
 *                                e: <else-part>
 *                                f:
 *
 */

condStart:
      L_IF '(' comma_expr ')'
      {
          /* When we enter a condition, we must not allow case labels
           * anymore.
           */

          mp_uint current;
          bytecode_p current_code;

          /* Turn off the case labels */

          $$[0] = current_break_address;
          current_break_address &= ~CASE_LABELS_ENABLED;

          current = CURRENT_PROGRAM_SIZE;
          if (!realloc_a_program(2))
          {
              yyerrorf("Out of memory: program size %"PRIuMPINT"\n", current+3);
              YYACCEPT;
          }
          current_code = PROGRAM_BLOCK + current;

          /* Add the branch instruction, with the usual optimization */
          if (last_expression == current - 1
           && current_code[-1] == F_NOT)
          {
              current_code[-1] = F_BRANCH_WHEN_NON_ZERO;
          }
          else
          {
              *current_code = F_BRANCH_WHEN_ZERO;
              current++;
          }

          $$[1] = current;
          CURRENT_PROGRAM_SIZE = current + 1;
      }
; /* condStart */


cond:
      condStart
      statement
      optional_else
      {
          p_int destination, location, offset;

          /* Complete the branch over the if-part */
          destination = (p_int)$3;
          location = $1[1];
          if ( (offset = destination - location) > 0x100)
          {
              fix_branch(
                mem_block[A_PROGRAM].block[location-1] ==
                 F_BRANCH_WHEN_ZERO ?
                  F_LBRANCH_WHEN_ZERO :
                  F_LBRANCH_WHEN_NON_ZERO
                ,
                destination, location
              );
          }
          else
          {
              mem_block[A_PROGRAM].block[location] = offset - 1;
          }

          /* Restore the previous case-labels status without
           * changing the actual break-address.
           */
          current_break_address |= $1[0] & CASE_LABELS_ENABLED;
      }
; /* cond */


optional_else:
      /* empty */ %prec LOWER_THAN_ELSE
      {
          /* The if-part ends here */
          $$ = CURRENT_PROGRAM_SIZE;
      }

    | L_ELSE
      {
          /* Add the branch over the else part */
          ins_f_code(F_BRANCH);
          $<address>$ = CURRENT_PROGRAM_SIZE;
          ins_byte(0);
      }
      statement
      {
          /* Fix up the branch over the else part and return
           * the start address of the else part.
           */
          $$ = fix_branch( F_LBRANCH, CURRENT_PROGRAM_SIZE, $<address>2);
          $$ += $<address>2 + 1;
      }
; /* optional_else */

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
/* Constants
 *
 * The rules here implement constant folding for numeric and string constants.
 */

constant:
      constant '|'   constant { $$ = $1 |  $3; }
    | constant '^'   constant { $$ = $1 ^  $3; }
    | constant '&'   constant { $$ = $1 &  $3; }
    | constant L_EQ  constant { $$ = $1 == $3; }
    | constant L_NE  constant { $$ = $1 != $3; }
    | constant '>'   constant { $$ = $1 >  $3; }
    | constant L_GE  constant { $$ = $1 >= $3; }
    | constant '<'   constant { $$ = $1 <  $3; }
    | constant L_LE  constant { $$ = $1 <= $3; }
    | constant L_LSH constant { $$ = (p_uint)$3 > MAX_SHIFT ? 0 : $1 << $3; }
    | constant L_RSH constant { $$ = (p_uint)$3 > MAX_SHIFT ? ($1 >= 0 ? 0 : -1) : ($1 >> $3); }
    | constant L_RSHL constant { $$ = (p_uint)$3 > MAX_SHIFT ? 0 : ((p_uint)$1 >> $3); }
    | constant '+'   constant { $$ = $1 +  $3; }
    | constant '-'   constant { $$ = $1 -  $3; }
    | constant '*'   constant { $$ = $1 *  $3; }
    | constant '%'   constant
      {
          if ($3)
          {
              $$ = $1 % $3;
          }
          else
          {
              yyerror("modulus by zero");
              $$ = 0;
          }
      }
    | constant '/'        constant
      {
          if ($3) {
              $$ = $1 / $3;
          } else {
              yyerror("division by zero");
              $$ = 0;
          }
      }
    | '(' constant ')' { $$ = $2; }
    | '-'   constant %prec '~' { $$ = -$2; }
    | L_NOT constant { $$ = !$2; }
    | '~'   constant { $$ = ~$2; }
    | L_NUMBER
; /* constant */


string_constant:
      L_STRING
      {
          last_string_constant = last_lex_string;
          last_lex_string = NULL;
      }
    | string_constant '+' L_STRING
      {
          add_string_constant();
      }
    | L_STRING L_STRING
      { fatal("L_STRING LSTRING: presence of rule should prevent its reduction\n"); }
    | string_constant '+' L_STRING L_STRING
      { fatal("L_STRING LSTRING: presence of rule should prevent its reduction\n"); }
    | '(' string_constant ')'
; /* string_constant */


/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
/* Expressions
 *
 * expr0 (with the help of the precedence and assoc specifications) handles
 * most of the expressions, and returns normal rvalues (as lrvalues).
 *
 * expr4 contains the expressions atoms (literal values), function calls
 * and expressions returning values which might be used as rvalues
 * as well as lvalues. It returns full lrvalues.
 *
 * lvalue contains expressions for unprotected lvalues and returns lvalues.
 *
 * name_lvalue is a subrule of lvalue and can be used where lvalues of
 * variables are needed (foreach() is one example).
 *
 * local_name_lvalue is to be used in contexts where new local variables
 * may be defined on the fly (for example "for(int i...").
 *
 * index_expr and index_range are used to parse and compile the two
 * forms of array indexing operations.
 */

comma_expr:
      expr0
    | comma_expr
      {
          insert_pop_value();
      }

      ',' expr0

      {
          $$.type = $4.type;
      }
; /* comma_expr */


expr0:
    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

      /* Normal assign:               ||= (&&= analog):
       *
       *  <expr0>               <lvalue>         <lvalue>
       *  <lvalue>              LDUP             LDUP
       *  ASSIGN-operator       LOR l            DUP
       *                        <expr0>          LBRANCH_WHEN_NON_ZERO l
       *                    l:  SWAP_VALUES      POP_VALUE
       *                        ASSIGN           <expr0>
       *                                      l: SWAP_VALUES
       *                                          ASSIGN
       */

      lvalue L_ASSIGN 

      {
          if ($2 == F_LAND_EQ || $2 == F_LOR_EQ)
          {
              if (!add_lvalue_code(&$1, 0))
                  YYACCEPT;

              /* Add the operator specific code */

              if ($2 == F_LAND_EQ)
              {
                  /* Insert the LDUP, LAND and remember the position */

                  ins_f_code(F_LDUP);
                  ins_f_code(F_LAND);
                  $<address>$ = CURRENT_PROGRAM_SIZE;
                  ins_byte(0);
              }
              else if ($2 == F_LOR_EQ)
              {
                  /* Insert the LDUP, LOR and remember the position */

                  ins_f_code(F_LDUP);
                  ins_f_code(F_LOR);
                  $<address>$ = CURRENT_PROGRAM_SIZE;
                  ins_byte(0);
              }
          }
      }
      
      expr0 %prec L_ASSIGN

      {
          fulltype_t type1, type2, restype;
%line
          $$ = $4;

          type1 = $1.type;
          type2 = $4.type;
          restype = type2; /* Assume normal assignment */

          /* Check the validity of the assignment */
          if (exact_types.typeflags
           && !compatible_types(type1, type2, MY_TRUE)
             )
          {
              Bool ok = MY_FALSE;

              switch($2)
              {
              case F_LAND_EQ:
              case F_LOR_EQ:
                  ok = MY_TRUE;
                  break;

              case F_ADD_EQ:
                  switch(type1.typeflags)
                  {
                  case TYPE_STRING:
                      if (type2.typeflags == TYPE_NUMBER
                       || type2.typeflags == TYPE_FLOAT)
                      {
                          ok = MY_TRUE;
                      }
                      break;
                  case TYPE_FLOAT:
                      if (type2.typeflags == TYPE_NUMBER)
                      {
                          ok = MY_TRUE;
                      }
                      break;
                  }
                  break;

              case F_SUB_EQ:
                  switch(type1.typeflags)
                  {
                  case TYPE_FLOAT:
                      if (type2.typeflags == TYPE_NUMBER)
                      {
                          ok = MY_TRUE;
                      }
                      break;
                  }
                  break;

              case F_MULT_EQ:
                  switch(type1.typeflags)
                  {
                  case TYPE_STRING:
                      if (type2.typeflags == TYPE_NUMBER)
                      {
                          ok = MY_TRUE;
                      }
                      break;
                  case TYPE_FLOAT:
                      if (type2.typeflags == TYPE_NUMBER)
                      {
                          ok = MY_TRUE;
                      }
                      break;
                  default:
                      if ((type1.typeflags & TYPE_MOD_POINTER) && type2.typeflags == TYPE_NUMBER)
                      {
                          ok = MY_TRUE;
                      }
                  }
                  break;

              case F_DIV_EQ:
                  switch(type1.typeflags)
                  {
                  case TYPE_FLOAT:
                      if (type2.typeflags == TYPE_NUMBER)
                      {
                          ok = MY_TRUE;
                      }
                      break;
                  }
                  break;

              case F_AND_EQ:
                  switch(type1.typeflags)
                  {
                  case TYPE_MAPPING:
                      if (type2.typeflags & TYPE_MOD_POINTER)
                      {
                          ok = MY_TRUE;
                      }
                      break;
                  default:
                      if ((type1.typeflags & TYPE_MOD_POINTER)
                       && type2.typeflags == TYPE_MAPPING
                         )
                      {
                          ok = MY_TRUE;
                      }
                      break;
                  }
                  break;

              } /* switch(assign op) */

              if (!ok)
              {
                  yyerrorf("Bad assignment %s", get_two_types(type1, type2));
              }

              /* Operator assignment: result type is determined by assigned-to
               * type.
               */
              restype = type1;
          }

          if (type2.typeflags & TYPE_MOD_REFERENCE)
              yyerror("Can't trace reference assignments.");

          /* Special checks for struct assignments */
          if (IS_TYPE_STRUCT(type1) || IS_TYPE_STRUCT(type2)
             )
          {
              restype = type1;
              if ($2 != F_ASSIGN)
                  yyerror("Only plain assigment allowed for structs");
          }

          if ($2 == F_LAND_EQ || $2 == F_LOR_EQ)
          {
              /* Update the offset the earlier LAND/LOR instruction */

              if ($2 == F_LAND_EQ)
              {
                  update_lop_branch($<address>3, F_LBRANCH_WHEN_ZERO);
              }
              else if ($2 == F_LOR_EQ)
              {
                  update_lop_branch($<address>3, F_LBRANCH_WHEN_NON_ZERO);
              }

              /* Insert the SWAP and the ASSIGN */

              ins_f_code(F_SWAP_VALUES);
              ins_f_code(F_ASSIGN);
          }
          else
          {
              if (!add_lvalue_code(&$1, $2))
                  YYACCEPT;
          }
          $$.end = CURRENT_PROGRAM_SIZE;
          $$.type = restype;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | error L_ASSIGN expr0  %prec L_ASSIGN
      {
          yyerror("Bad assignment: illegal lhs (target)");
          $$ = $3;
          $$.type = Type_Any;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr0 '?'
      {
          /* Insert the branch to the :-part and remember this address */
          ins_f_code(F_BRANCH_WHEN_ZERO);
          $<address>$ = CURRENT_PROGRAM_SIZE;
          ins_byte(0);
      }

      expr0

      {
          /* Insert the branch over the :-part, and update
           * the earlier branch to the :-part.
           */

          p_int address, offset;

          address = (p_int)$<address>3;

          /* The branch to the end */
          ins_f_code(F_BRANCH);
          $<address>$ = CURRENT_PROGRAM_SIZE;
          ins_byte(0);

          /* Update the earlier branch to point here */
          offset = CURRENT_PROGRAM_SIZE - ( address + 1);
          if (offset > 0xff - 1)
          {
              /* We have to make it a long branch and move the code
               * generated so far.
               */

              int i;
              bytecode_p p;

              $<address>$ = CURRENT_PROGRAM_SIZE;
              ins_byte(0);
              p = PROGRAM_BLOCK + mem_block[A_PROGRAM].current_size-1;
              for (i = offset; --i >= 0; --p )
                  *p = p[-1];
              p[-2] = F_LBRANCH_WHEN_ZERO;
              upd_short(address, offset+2);
              if (offset > 0x7ffd)
                  yyerror("offset overflow");
          }
          else
          {
              mem_block[A_PROGRAM].block[address] = offset;
          }
      }

      ':' expr0 %prec '?'

      {
          /* Update the earlier branch skipping the :-part
           * and check the types of the two parts.
           */
          p_int address, old_address;
          int offset;
          fulltype_t type1, type2;

          last_expression = -1;

          old_address = $<address>3;
          address = $<address>5;
          offset = mem_block[A_PROGRAM].current_size - ( address + 1);
          if (offset > 0xff)
          {
              /* We have to make the branch a long branch.
               * This could also mean that the first branch now
               * have to become a long branch, too.
               */
              int i;
              bytecode_p p;

              ins_byte(0);
              p = PROGRAM_BLOCK + mem_block[A_PROGRAM].current_size-1;
              for( i = offset; --i >= 0; --p )
                  *p = p[-1];
              p[-2] = F_LBRANCH;
              upd_short(address, offset+2);
              if (offset > 0x7ffd)
                  yyerror("offset overflow");
              if ( mem_block[A_PROGRAM].block[old_address-1] ==
                  F_BRANCH_WHEN_ZERO )
                  mem_block[A_PROGRAM].block[old_address]++;
              else
                  upd_short(old_address,read_short(old_address)+1);
          }
          else
          {
              mem_block[A_PROGRAM].block[address] = offset;
          }

          /* Check the types and determine the result type */
          type1 = $4.type;
          type2 = $7.type;

          $$ = $1;
          $$.end = CURRENT_PROGRAM_SIZE;
          if (!compatible_types(type1, type2, MY_FALSE))
          {
              $$.type = Type_Any;
              if ((type1.typeflags & TYPE_MOD_POINTER) != 0
               && (type2.typeflags & TYPE_MOD_POINTER) != 0)
                  $$.type.typeflags |= TYPE_MOD_POINTER;
              /* TODO: yyinfof("Different types to ?: */
          }
          else if (type1.typeflags == TYPE_ANY)
              $$.type = type2;
          else if (type2.typeflags == TYPE_ANY)
              $$.type = type1;
          else if (type1.typeflags == (TYPE_MOD_POINTER|TYPE_ANY) )
              $$.type = type2;
          else if (type2.typeflags == (TYPE_MOD_POINTER|TYPE_ANY) )
              $$.type = type1;
          else
              $$.type = type1;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr0 L_LOR %prec L_LOR
      {
          /* Insert the LOR and remember the position */

          ins_f_code(F_LOR);
          $<address>$ = CURRENT_PROGRAM_SIZE;
          ins_byte(0);
      }

      expr0

      {
          /* Update the offset the earlier LOR instruction */

          update_lop_branch($<address>3, F_LBRANCH_WHEN_NON_ZERO);

          $$ = $1;
          $$.end = CURRENT_PROGRAM_SIZE;

          /* Determine the result type */
          if (equal_types($1.type, $4.type))
              $$.type = $1.type;
          else
              $$.type = Type_Any;  /* Return type can't be known */
      } /* LOR */

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr0 L_LAND %prec L_LAND
      {
          /* Insert the LAND and remember the position */

          ins_f_code(F_LAND);
          $<address>$ = CURRENT_PROGRAM_SIZE;
          ins_byte(0);
      }

      expr0

      {
          /* Update the offset the earlier LAND instruction */

          update_lop_branch($<address>3, F_LBRANCH_WHEN_ZERO);

          $$ = $1;
          $$.end = CURRENT_PROGRAM_SIZE;

          /* Determine the return type */
          if (equal_types($1.type, $4.type))
              $$.type = $1.type;
          else
              $$.type = Type_Any;        /* Return type can't be known */
       } /* LAND */

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr0 '|' expr0
      {
          $$ = $1;

          if (($1.type.typeflags | $3.type.typeflags) & TYPE_MOD_POINTER)
          {
              if (exact_types.typeflags
               && ($1.type.typeflags ^ $3.type.typeflags) & TYPE_MOD_POINTER
                 )
                  yyerrorf("Incompatible types for arguments to | %s"
                          , get_two_types($1.type, $3.type));
              if (equal_types($1.type, $3.type))
                  $$.type = $1.type;
              else
              {
                  $$.type = Type_Ptr_Any;
              }
          }
          else
          {
              if (exact_types.typeflags
               && !BASIC_TYPE($1.type, Type_Number))
                  type_error("Bad argument 1 to |", $1.type);
              if (exact_types.typeflags
               && !BASIC_TYPE($3.type, Type_Number))
                  type_error("Bad argument 2 to |", $3.type);
              $$.type = Type_Number;
          }
          ins_f_code(F_OR);

          $$.end = CURRENT_PROGRAM_SIZE;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr0 '^' expr0
      {
          $$ = $1;

          if (($1.type.typeflags | $3.type.typeflags) & TYPE_MOD_POINTER)
          {
              if (exact_types.typeflags
               && ($1.type.typeflags ^ $3.type.typeflags) & TYPE_MOD_POINTER
                 )
                  yyerrorf("Incompatible types for arguments to | %s"
                          , get_two_types($1.type, $3.type));
              if (equal_types($1.type, $3.type))
                  $$.type = $1.type;
              else
                  $$.type = Type_Ptr_Any;
          }
          else
          {
              if (exact_types.typeflags && !BASIC_TYPE($1.type, Type_Number))
                  type_error("Bad argument 1 to ^", $1.type);
              if (exact_types.typeflags && !BASIC_TYPE($3.type, Type_Number))
                  type_error("Bad argument 2 to ^", $3.type);
              $$.type = Type_Number;
          }
          ins_f_code(F_XOR);

          $$.end = CURRENT_PROGRAM_SIZE;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr0 '&' expr0
      {
          $$ = $1;
          ins_f_code(F_AND);
          $$.end = CURRENT_PROGRAM_SIZE;
          $$.type = Type_Any;

          /* Check the types */
          /* TODO: Implement the typechecks, including result type
           * TODO:: by table lookups.
           */
          if (exact_types.typeflags)
          {
              fulltype_t first_type  = $1.type;
              fulltype_t second_type = $3.type;
              if ( first_type.typeflags == TYPE_ANY
               &&  second_type.typeflags == TYPE_ANY )
              {
                    /* $$ == TYPE_ANY is correct */
              }
              else if (first_type.typeflags == TYPE_MAPPING)
              {
                  if (second_type.typeflags != TYPE_MAPPING
                   && !(second_type.typeflags & TYPE_MOD_POINTER)
                   && second_type.typeflags != TYPE_ANY
                     )
                  {
                      type_error("Bad argument 2 to &", second_type );
                  }
                  $$.type = Type_Mapping;
              }
              else if ( (first_type.typeflags | second_type.typeflags) & TYPE_MOD_POINTER)
              {
                  if ((first_type.typeflags & TYPE_MOD_POINTER)
                   && second_type.typeflags == TYPE_MAPPING
                     )
                  {
                      $$.type = first_type;
                  }
                  else if (first_type.typeflags  == TYPE_NUMBER
                   || second_type.typeflags == TYPE_NUMBER)
                  {
                      yyerrorf("Incompatible types for arguments to & %s"
                              , get_two_types(first_type, second_type));
                  }
                  else if (( !( first_type.typeflags & TYPE_MOD_POINTER )
                           || first_type.typeflags & TYPE_MOD_REFERENCE)
                        && first_type.typeflags != TYPE_ANY)
                  {
                      type_error("Bad argument 1 to &", first_type );
                  }
                  else if (( !( second_type.typeflags & TYPE_MOD_POINTER )
                           ||   second_type.typeflags & TYPE_MOD_REFERENCE)
                        && second_type.typeflags != TYPE_ANY)
                  {
                      type_error("Bad argument 2 to &", first_type );
                  }
                  else {
                      fulltype_t f_type = first_type;
                      fulltype_t s_type = second_type;

                      f_type.typeflags &= ~TYPE_MOD_POINTER;
                      s_type.typeflags &= ~TYPE_MOD_POINTER;
                      if ( !BASIC_TYPE(f_type, s_type) )
                      {
                          yyerrorf("Incompatible types for arguments to & %s"
                                  , get_two_types(first_type, second_type));
                      }
                      else
                      {
                          $$.type = Type_Ptr_Any;
                      }
                  }
              }
              else
              {
                  if ( !BASIC_TYPE(first_type, Type_Number)
                   &&  !BASIC_TYPE(first_type, Type_String) )
                      type_error("Bad argument 1 to &", first_type );
                  if ( !BASIC_TYPE(second_type, Type_Number)
                   &&  !BASIC_TYPE(second_type, Type_String) )
                      type_error("Bad argument 2 to &", second_type);
                  if ( first_type.typeflags == TYPE_ANY )
                      $$.type =   BASIC_TYPE(second_type, Type_Number)
                                ? Type_Number : Type_String;
                  else
                      $$.type =   BASIC_TYPE(first_type, Type_Number)
                                ? Type_Number : Type_String;
              }
          } /* end of exact_types code */
      } /* end of '&' code */

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr0 L_EQ expr0
      {
          fulltype_t t1 = $1.type, t2 = $3.type;

          $$ = $1;
          if (exact_types.typeflags
           && !equal_types(t1, t2)
           && t1.typeflags != TYPE_ANY && t2.typeflags != TYPE_ANY
           && !(t1.typeflags == TYPE_NUMBER && t2.typeflags == TYPE_FLOAT)
           && !(t1.typeflags == TYPE_FLOAT && t2.typeflags == TYPE_NUMBER)
             )
          {
              yyerrorf("== always false because of different types %s"
                      , get_two_types($1.type, $3.type));
          }
          ins_f_code(F_EQ);
          $$.type = Type_Number;
          $$.end = CURRENT_PROGRAM_SIZE;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr0 L_NE expr0
      {
          fulltype_t t1 = $1.type, t2 = $3.type;

          $$ = $1;
          if (exact_types.typeflags
           && !equal_types(t1, t2)
           && t1.typeflags != TYPE_ANY && t2.typeflags != TYPE_ANY
           && !(t1.typeflags == TYPE_NUMBER && t2.typeflags == TYPE_FLOAT)
           && !(t1.typeflags == TYPE_FLOAT && t2.typeflags == TYPE_NUMBER)
             )
           {
              yyerrorf("!= always true because of different types %s"
                      , get_two_types($1.type, $3.type));
          }
          ins_f_code(F_NE);
          $$.end = CURRENT_PROGRAM_SIZE;
          $$.type = Type_Number;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr0 '>'  expr0
      {
          $$ = $1;
          $$.type = Type_Number;;
          ins_f_code(F_GT);
          $$.end = CURRENT_PROGRAM_SIZE;
      }
    | expr0 L_GE  expr0
      {
          $$ = $1;
          $$.type = Type_Number;
          ins_f_code(F_GE);
          $$.end = CURRENT_PROGRAM_SIZE;
      }
    | expr0 '<'  expr0
      {
          $$ = $1;
          $$.type = Type_Number;
          ins_f_code(F_LT);
          $$.end = CURRENT_PROGRAM_SIZE;
      }
    | expr0 L_LE  expr0
      {
          $$ = $1;
          $$.type = Type_Number;
          ins_f_code(F_LE);
          $$.end = CURRENT_PROGRAM_SIZE;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr0 L_LSH expr0
      {
          $$ = $1;
          ins_f_code(F_LSH);
          $$.end = CURRENT_PROGRAM_SIZE;
          $$.type = Type_Number;
          if (exact_types.typeflags)
          {
              if (!BASIC_TYPE($1.type, Type_Number))
                  type_error("Bad argument 1 to '<<'", $1.type);
              if (!BASIC_TYPE($3.type, Type_Number))
                  type_error("Bad argument 2 to '<<'", $3.type);
          }
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr0 L_RSH expr0
      {
          $$ = $1;
          ins_f_code(F_RSH);
          $$.type = Type_Number;
          $$.end = CURRENT_PROGRAM_SIZE;
          if (exact_types.typeflags)
          {
              if (!BASIC_TYPE($1.type, Type_Number))
                  type_error("Bad argument 1 to '>>'", $1.type);
              if (!BASIC_TYPE($3.type, Type_Number))
                  type_error("Bad argument 2 to '>>'", $3.type);
          }
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr0 L_RSHL expr0
      {
          $$ = $1;
          ins_byte(F_RSHL);
          $$.end = CURRENT_PROGRAM_SIZE;
          $$.type = Type_Number;
          if (exact_types.typeflags)
          {
              if (!BASIC_TYPE($1.type, Type_Number))
                  type_error("Bad argument 1 to '>>>'", $1.type);
              if (!BASIC_TYPE($3.type, Type_Number))
                  type_error("Bad argument 2 to '>>>'", $3.type);
          }
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr0 '+'
      {
%line
          $<numbers>$[0] = last_expression;
          $<numbers>$[1] = last_string_is_new;
      }
      expr0
      {
          /* Type checks of this case are complicated, therefore
           * we'll do almost all of them at run-time.
           * Here we just try to fold "string" + "string", and
           * disallow additions of structs.
           */

          mp_uint current_size;
          bytecode_p p;
%line
          $$ = $1;

          current_size = CURRENT_PROGRAM_SIZE;
          p = &(PROGRAM_BLOCK[current_size]);

          /* Check if we can combine strings: the last four bytes must be two 
           * CSTRINGx instructions.
           * TODO: handle F_STRING as well.
           */
          if (last_expression + 2 == current_size
           && $<numbers>3[0] + 4 == (mp_int)current_size
           && ((p[-2]-(F_CSTRING0)) & ~3) == 0
           && ((p[-4]-(F_CSTRING0)) & ~3) == 0
             )
          {
              /* Yup, we can combine the two strings.
               */
              string_t *str1, *str2, *sum;
              int i;

              /* Retrieve both strings from the A_STRINGS area
               * and catenate them.
               */
              str1 = ((string_t**)(mem_block[A_STRINGS].block))
                [p[-3] | (p[-4]-(F_CSTRING0))<<8 ];
              str2 = ((string_t**)(mem_block[A_STRINGS].block))
                [p[-1] | (p[-2]-(F_CSTRING0))<<8 ];
              sum = mstr_add(str1, str2);
              if (!sum)
              {
                  yyerrorf("Out of memory for string literal (%zu bytes)"
                          , (mstrsize(str1)+mstrsize(str2))
                          );
                  YYACCEPT;
              }

              /* If possible, try to delete the constituent strings
               * from the string area.
               */
              if (last_string_is_new)
                  delete_prog_string();
              if ($<numbers>3[1])
                  delete_prog_string();

              /* Store the new string and update the CSTRING
               * instructions.
               */
              sum = make_tabled(sum);
              if (!sum)
              {
                  yyerror("Out of memory for string literal");
                  YYACCEPT;
              }
              i = store_prog_string(sum);

              last_expression = current_size - 4;
              if (i < 0x400)
              {
                  p[-4] = F_CSTRING0 + (i>>8);
                  p[-3] = i;
                  CURRENT_PROGRAM_SIZE = current_size - 2;
              }
              else
              {
                  p[-4] = F_STRING;
                  upd_short(current_size - 3, i);
                  CURRENT_PROGRAM_SIZE = current_size - 1;
              }
              $$.type = Type_String;
          }
          else
          {
              /* Just add */
              ins_f_code(F_ADD);
              $$.type = Type_Any;
              if (equal_types($1.type, $4.type))
                  $$.type = $1.type;
              else if ($1.type.typeflags == TYPE_STRING)
                  $$.type = Type_String;
              else if (($1.type.typeflags == TYPE_NUMBER || $1.type.typeflags == TYPE_FLOAT)
                     && $4.type.typeflags == TYPE_STRING)
                  $$.type = Type_String;
              else if ($1.type.typeflags == TYPE_FLOAT
                    && ($4.type.typeflags == TYPE_NUMBER || $4.type.typeflags == TYPE_ANY))
                  $$.type = Type_Float;
              else if (($1.type.typeflags == TYPE_NUMBER || $1.type.typeflags == TYPE_ANY)
                    && $4.type.typeflags == TYPE_FLOAT)
                  $$.type = Type_Float;
              else if (IS_TYPE_STRUCT($1.type) || IS_TYPE_STRUCT($4.type))
                  yyerrorf("Bad arguments to '+': %s"
                          , get_two_types($1.type, $4.type)
                          );
          }
          $$.end = CURRENT_PROGRAM_SIZE;
      } /* '+' */

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr0 '-' expr0
      {
%line
          $$ = $1;
          $$.type = Type_Any;

          if (exact_types.typeflags)
          {
              fulltype_t type1 = $1.type;
              fulltype_t type2 = $3.type;

              if (equal_types(type1, type2))
              {
                  static char matchok[] =
%typemap TYPE_ANY:1,TYPE_NUMBER:1,TYPE_FLOAT:1,TYPE_MAPPING:1,TYPE_STRING:1

                  if ( type1.typeflags & (TYPE_MOD_POINTER|TYPE_MOD_REFERENCE)
                       ? (type1.typeflags & (TYPE_MOD_POINTER|TYPE_MOD_REFERENCE))
                         == TYPE_MOD_POINTER
                       : matchok[type1.typeflags]
                       )
                  {
                      $$.type = type1;
                  }
                  else
                  {
                      type_error("Bad arguments to '-'", type1);
                  }
              }
              else if (   (type1.typeflags & (TYPE_MOD_POINTER|TYPE_MOD_REFERENCE))
                       == TYPE_MOD_POINTER)
              {
                  if ((type2.typeflags | TYPE_MOD_POINTER) == (TYPE_MOD_POINTER|TYPE_ANY)
                   || (   type2.typeflags & TYPE_MOD_POINTER
                       && type1.typeflags == (TYPE_MOD_POINTER|TYPE_ANY))
                     )
                  {
                      $$.type = type1;
                  }
                  else
                  {
                      yyerror("Arguments to '-' don't match");
                  }
              }
              else switch (type1.typeflags)
              {
              case TYPE_ANY:
                  switch (type2.typeflags)
                  {
                  case TYPE_NUMBER:
                      /* number or float -> TYPE_ANY */
                      break;
                  case TYPE_MAPPING:
                  case TYPE_FLOAT:
                  case TYPE_STRING:
                      $$.type = type2;
                      break;
                  default:
                      if ( (type2.typeflags & (TYPE_MOD_POINTER|TYPE_MOD_REFERENCE)) ==
                             TYPE_MOD_POINTER)
                      {
                          $$.type = Type_Ptr_Any;
                          break;
                      }
                      else
                      {
                          type_error("Bad argument number 2 to '-'", type2);
                          break;
                      }
                  }
                  break;

              case TYPE_NUMBER:
                  if (type2.typeflags == TYPE_FLOAT || type2.typeflags == TYPE_ANY)
                  {
                      $$.type = type2;
                  }
                  else
                  {
                      yyerror("Arguments to '-' don't match");
                  }
                  break;

              case TYPE_FLOAT:
                  if (type2.typeflags == TYPE_NUMBER || type2.typeflags == TYPE_ANY)
                  {
                      $$.type = Type_Float;
                  }
                  else
                  {
                      yyerror("Arguments to '-' don't match");
                  }
                  break;

              case TYPE_STRING:
                  if (type2.typeflags == TYPE_STRING || type2.typeflags == TYPE_ANY)
                  {
                      $$.type = Type_String;
                  }
                  else
                  {
                      yyerror("Arguments to '-' don't match");
                  }
                  break;

              case TYPE_MAPPING:
                  if (type2.typeflags == TYPE_ANY)
                  {
                      $$.type = type1;
                  }
                  else
                  {
                      yyerror("Arguments to '-' don't match");
                  }
                    break;

              default:
                  type_error("Bad argument number 1 to '-'", type1);
                  break;
              }
          } /* if (exact_types) */

          ins_f_code(F_SUBTRACT);
          $$.end = CURRENT_PROGRAM_SIZE;
      } /* '-' */

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr0 '*' expr0
      {
          fulltype_t type1, type2;

          $$ = $1;

          type1 = $1.type;
          type2 = $3.type;

          if (exact_types.typeflags)
          {
              if (!BASIC_TYPE(type1, Type_Number)
               && type1.typeflags != TYPE_FLOAT
               && type1.typeflags != TYPE_STRING
               && !(type1.typeflags & TYPE_MOD_POINTER)
                 )
                  type_error("Bad argument number 1 to '*'", type1);
              if (!BASIC_TYPE(type2, Type_Number)
               && type2.typeflags != TYPE_FLOAT
               && type2.typeflags != TYPE_STRING
               && !(type2.typeflags & TYPE_MOD_POINTER)
                 )
                  type_error("Bad argument number 2 to '*'", type2);
          }

          ins_f_code(F_MULTIPLY);
          $$.end = CURRENT_PROGRAM_SIZE;

          if (type1.typeflags == TYPE_FLOAT || type2.typeflags == TYPE_FLOAT )
          {
              $$.type = Type_Float;
          }
          else if (type1.typeflags == TYPE_STRING || type2.typeflags == TYPE_STRING)
          {
              $$.type = Type_String;;
          }
          else if (type1.typeflags & TYPE_MOD_POINTER)
          {
              $$.type = type1;
          }
          else if (type2.typeflags & TYPE_MOD_POINTER)
          {
              $$.type = type2;
          }
          else if (type1.typeflags == TYPE_ANY || type2.typeflags == TYPE_ANY)
          {
              $$.type = Type_Any;
          }
          else
          {
              $$.type = Type_Number;
          }
      } /* '*' */

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr0 '%' expr0
      {
          if (exact_types.typeflags)
          {
              if (!BASIC_TYPE($1.type, Type_Number))
                  type_error("Bad argument number 1 to '%'", $1.type);
              if (!BASIC_TYPE($3.type, Type_Number))
                  type_error("Bad argument number 2 to '%'", $3.type);
          }

          $$ = $1;
          ins_f_code(F_MOD);
          $$.end = CURRENT_PROGRAM_SIZE;
          $$.type = Type_Number;
      }
    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr0 '/' expr0
      {
          fulltype_t type1, type2;

          $$ = $1;

          type1 = $1.type;
          type2 = $3.type;

          if (exact_types.typeflags)
          {
              if ( !BASIC_TYPE(type1, Type_Number) && type1.typeflags != TYPE_FLOAT)
                  type_error("Bad argument number 1 to '/'", type1);
              if ( !BASIC_TYPE(type2, Type_Number) && type2.typeflags != TYPE_FLOAT)
                  type_error("Bad argument number 2 to '/'", type2);
          }

          ins_f_code(F_DIVIDE);
          $$.end = CURRENT_PROGRAM_SIZE;

          if (type1.typeflags == TYPE_FLOAT || type2.typeflags == TYPE_FLOAT )
          {
              $$.type = Type_Float;
          }
          else
          {
              $$.type = Type_Number;
          }
      } /* '/' */

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | decl_cast expr0 %prec '~'
      {
          $$ = $2;
          $$.type = $1;
          if (exact_types.typeflags
           && $2.type.typeflags != TYPE_ANY
           && $2.type.typeflags != TYPE_UNKNOWN
           && $1.typeflags != TYPE_VOID
             )
              type_error("Casts are only legal for type mixed, or when unknown", $2.type);
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | cast expr0 %prec '~'
      {
          $$ = $2;
          $$.type = $1;
          if ($2.type.typeflags != TYPE_ANY
           && $2.type.typeflags != TYPE_UNKNOWN
           && $1.typeflags != TYPE_VOID
           && !equal_types($1, $2.type)
             )
          {
              switch($1.typeflags)
              {
              default:
                  if (IS_TYPE_STRUCT($1))
                      break; /* Do nothing, just adapt the type information */
                  type_error("Illegal cast", $1);
                  break;
              case TYPE_ANY:
                  /* Do nothing, just adapt the type information */
                  break;
              case TYPE_NUMBER:
                  ins_f_code(F_TO_INT);
                  break;
              case TYPE_FLOAT:
                  ins_f_code(F_TO_FLOAT);
                  break;
              case TYPE_STRING:
                  ins_f_code(F_TO_STRING);
                  break;
              case TYPE_OBJECT:
                  ins_f_code(F_TO_OBJECT);
                  break;
              case TYPE_NUMBER|TYPE_MOD_POINTER:
                  ins_f_code(F_TO_ARRAY);
                  break;
              }
          }
          else if (pragma_warn_empty_casts)
          {
              if (equal_types($1, $2.type))
              {   if ($2.type.typeflags != TYPE_ANY)
                      yywarnf("casting a value to its own type: %s"
                             , get_type_name($1));
              }
              else if ($2.type.typeflags != TYPE_UNKNOWN
                    && $2.type.typeflags != TYPE_ANY)
                  yywarnf("cast will not convert the value: %s"
                         , get_two_types($1, $2.type)
                      );
          }

          $$.end = CURRENT_PROGRAM_SIZE;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | pre_inc_dec  L_IDENTIFIER %prec L_INC
      {
          /* ++/-- of a global variable.
           * We have to distinguish virtual and non-virtual
           * variables here.
           */
          variable_t *varp;
          fulltype_t lvtype;
          int i;
          PREPARE_INSERT(4)
%line
          $$.start = $1.start;
          i = verify_declared($2);
          if (i == -1)
              /* Variable not declared. */
              YYACCEPT;

          if (i & VIRTUAL_VAR_TAG)
          {
              add_f_code(F_PUSH_VIRTUAL_VARIABLE_LVALUE);
              add_byte(i);
              varp = V_VARIABLE(i);
              lvtype = varp->type;
          }
          else
          {
              if ((i + num_virtual_variables) & ~0xff)
              {
                  add_f_code(F_PUSH_IDENTIFIER16_LVALUE);
                  add_short(i + num_virtual_variables);
                  CURRENT_PROGRAM_SIZE += 1;
              }
              else
              {
                  add_f_code(F_PUSH_IDENTIFIER_LVALUE);
                  add_byte(i + num_virtual_variables);
              }
              varp = NV_VARIABLE(i);
              lvtype = varp->type;
          }
          lvtype.typeflags &= TYPE_MOD_MASK;

          // warn about deprecated variables.
          if (varp->type.typeflags & TYPE_MOD_DEPRECATED)
              yywarnf("Using deprecated global variable %s.\n",
                      get_txt(varp->name));

              
          if (exact_types.typeflags
           && !BASIC_TYPE(lvtype, Type_Number)
           && !BASIC_TYPE(lvtype, Type_Float))
          {
              argument_type_error($1.code, lvtype);
          }

          last_expression = CURRENT_PROGRAM_SIZE + 2;

          CURRENT_PROGRAM_SIZE += 3;

          add_f_code($1.code);
          $$.end = CURRENT_PROGRAM_SIZE;
          $$.type = lvtype;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | pre_inc_dec L_LOCAL %prec L_INC
      {
          fulltype_t lvtype;
          PREPARE_INSERT(3)
%line
          $$.start = $1.start;

          $2 = check_for_context_local($2, &lvtype);

          if ($2->u.local.context >= 0)
          {
              add_f_code(F_PUSH_CONTEXT_LVALUE);
              add_byte($2->u.local.context);
          }
          else
          {
              add_f_code(F_PUSH_LOCAL_VARIABLE_LVALUE);
              add_byte($2->u.local.num);
          }
          CURRENT_PROGRAM_SIZE =
            (last_expression = CURRENT_PROGRAM_SIZE + 2) + 1;
          add_f_code($1.code);
          if (exact_types.typeflags
           && !BASIC_TYPE(lvtype, Type_Number)
           && !BASIC_TYPE(lvtype, Type_Float))
          {
              argument_type_error($1.code, lvtype);
          }
          $$.type = lvtype;
          $$.end = CURRENT_PROGRAM_SIZE;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | pre_inc_dec expr4 index_expr %prec '['
      {
          mp_uint current;
          bytecode_p p;
          int start;
          fulltype_t restype;
%line
          $$.start = $1.start;

          if ($3.type1.typeflags & TYPE_MOD_REFERENCE)
              yyerror("Reference used as index");

          restype = Type_Any;

          /* Check the types */
          if (exact_types.typeflags)
          {
              fulltype_t type;

              type = $2.type;
              type.typeflags &= TYPEID_MASK;
              if (type.typeflags & TYPE_MOD_POINTER)
              {
                  if (type.typeflags != (TYPE_MOD_POINTER|TYPE_ANY)
                   && type.typeflags != (TYPE_MOD_POINTER|TYPE_NUMBER) )
                      argument_type_error($1.code, type);
              }
              else switch (type.typeflags)
              {
              case TYPE_MAPPING:
                  if ($3.inst == F_INDEX)
                      break;
                  /* FALLTHROUGH */
              default:
                  type_error("Bad type to indexed lvalue", type);
              case TYPE_ANY:
                  if ($3.inst == F_INDEX)
                      break;
                  /* FALLTHROUGH */
              case TYPE_STRING:
                  if (!BASIC_TYPE($3.type1, Type_Number))
                      type_error("Bad type of index", $3.type1);
                  restype = Type_Number;
                  break;
              }
          } /* if (exact_types) */

          /* Create the code to index the lvalue */

          /* TODO: How does this lvalue-indexing work? */
          current = CURRENT_PROGRAM_SIZE;
          start = $2.start;
          if ($2.code >= 0)
          {
              if ($2.end)
              {
                  int length;
                  bytecode_p q;

                  length = $2.end - start + 1;
                  if (!realloc_a_program(length))
                  {
                      yyerrorf("Out of memory: program size %"PRIuMPINT"\n"
                              , current+length);
                      YYACCEPT;
                  }
                  p = PROGRAM_BLOCK;
                  memcpy(p + current, p + start, length);
                  p += start;
                  q = p + length;
                  length = current - start;
                  for( ; --length >= 0; )
                      *p++ = *q++;
                  if ($2.code == F_PUSH_IDENTIFIER16_LVALUE)
                      p[-3] = $2.code;
                  else
                      p[-1] = $2.code;
                  if ($3.inst == F_INDEX)
                      *p++ = F_INDEX_LVALUE;
                  else if ($3.inst == F_RINDEX)
                      *p++ = F_RINDEX_LVALUE;
                  else
                      *p++ = F_AINDEX_LVALUE;
              }
              else
              {
                  int i;
                  int length;

                  if (!realloc_a_program(3))
                  {
                      yyerrorf("Out of memory: program size %"PRIuMPINT"\n",
                               current+3);
                      YYACCEPT;
                  }
                  p = PROGRAM_BLOCK + start;
                  i = p[1];
                  length = current - start - 2;
                  for( ; --length >= 0; p++)
                      *p = p[2];
                  *p++ = $2.code;
                  *p++ = i;
                  if ($3.inst == F_INDEX)
                      *p++ = F_INDEX_LVALUE;
                  else if ($3.inst == F_RINDEX)
                      *p++ = F_RINDEX_LVALUE;
                  else
                      *p++ = F_AINDEX_LVALUE;
              }
          }
          else
          {
              if (!realloc_a_program(2))
              {
                  yyerrorf("Out of memory: program size %"PRIuMPINT"\n", 
                           current+2);
                  YYACCEPT;
              }
              p = PROGRAM_BLOCK + start;
              if ($3.inst == F_INDEX)
                  *p++ = F_PUSH_INDEXED_LVALUE;
              else if ($3.inst == F_RINDEX)
                  *p++ = F_PUSH_RINDEXED_LVALUE;
              else
                  *p++ = F_PUSH_AINDEXED_LVALUE;
          }

          /* Finally store the actual instruction */
          *p = $1.code;
          last_expression = current + 1;
          CURRENT_PROGRAM_SIZE = current + 2;
          $$.end = CURRENT_PROGRAM_SIZE;
          $$.type = restype;
      } /* pre_inc_dec expr4 [index_expr] */

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | pre_inc_dec expr4 '[' expr0 ',' expr0 ']' %prec '['
      {
          mp_uint current;
          bytecode_p p;
%line
          $$.start = $1.start;

          if ($4.type.typeflags & TYPE_MOD_REFERENCE
           || $6.type.typeflags & TYPE_MOD_REFERENCE)
              yyerror("Reference used as index");

          /* Check the types */
          if (exact_types.typeflags)
          {
              fulltype_t type;

              type = $2.type;
              type.typeflags &= TYPEID_MASK;
              switch (type.typeflags)
              {
              default:
                  type_error("Bad type to indexed lvalue", type);
                  break;
              case TYPE_ANY:
              case TYPE_MAPPING:
                  break;
              }
          } /* if (exact_types) */

          /* We don't have to do much: we can take the rvalue
           * produced by <expr4> and add our PUSH_INDEXED_MAP_LVALUE
           */
          current = CURRENT_PROGRAM_SIZE;
          if (!realloc_a_program(2))
          {
              yyerrorf("Out of memory: program size %"PRIuMPINT"\n", 
                       current+2);
              YYACCEPT;
          }
          p = PROGRAM_BLOCK + current;
          *p++ = F_PUSH_INDEXED_MAP_LVALUE;

          /* Finally store the actual instruction */
          *p = $1.code;
          last_expression = current + 1;
          CURRENT_PROGRAM_SIZE = current + 2;
          $$.end = CURRENT_PROGRAM_SIZE;
          $$.type = Type_Any;
      } /* pre_inc_dec expr4 [expr0 ',' expr0] */

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | L_NOT expr0
      {
          $$ = $2;
          last_expression = CURRENT_PROGRAM_SIZE;
          ins_f_code(F_NOT);        /* Any type is valid here. */
          $$.end = CURRENT_PROGRAM_SIZE;
          $$.type = Type_Number;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '~' expr0
      {
%line
          $$ = $2;
          ins_f_code(F_COMPL);
          if (exact_types.typeflags && !BASIC_TYPE($2.type, Type_Number))
              type_error("Bad argument to ~", $2.type);
          $$.end = CURRENT_PROGRAM_SIZE;
          $$.type = Type_Number;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '-' expr0 %prec '~'
      {
          fulltype_t type;
%line
          $$ = $2;

          if (CURRENT_PROGRAM_SIZE - last_expression == 2
           && mem_block[A_PROGRAM].block[last_expression] ==
                F_CLIT )
          {
              mem_block[A_PROGRAM].block[last_expression] =
                F_NCLIT;
          }
          else if (CURRENT_PROGRAM_SIZE - last_expression == 1
           && mem_block[A_PROGRAM].block[last_expression] ==
                F_CONST1 )
          {
              mem_block[A_PROGRAM].block[last_expression] =
                F_NCONST1;
          }
          else if (CURRENT_PROGRAM_SIZE - last_expression == 1 + sizeof(p_int)
           && mem_block[A_PROGRAM].block[last_expression] ==
                F_NUMBER )
          {
              p_int number;
              number = read_p_int(last_expression + 1);
              number = -number;
              upd_p_int(last_expression + 1, number);
          }
          else
          {
              ins_f_code(F_NEGATE);
          }
          $$.end = CURRENT_PROGRAM_SIZE;

          type = $2.type;
          if (exact_types.typeflags
           && !BASIC_TYPE(type, Type_Number)
           && type.typeflags != TYPE_FLOAT )
              type_error("Bad argument to unary '-'", type);
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | lvalue L_INC %prec L_INC
      {
%line
          /* Create the code to push the lvalue plus POST_INC */
          $$.start = CURRENT_PROGRAM_SIZE;
          if (!add_lvalue_code(&$1, F_POST_INC))
              YYACCEPT;
          $$.end = CURRENT_PROGRAM_SIZE;

          /* Check the types */
          if (exact_types.typeflags
           && !BASIC_TYPE($1.type, Type_Number)
           && !BASIC_TYPE($1.type, Type_Float)
             )
              type_error("Bad argument to ++", $1.type);

          $$.type = $1.type;
      } /* post-inc */

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | lvalue L_DEC %prec L_DEC
      {
%line
          $$.start = CURRENT_PROGRAM_SIZE;

          /* Create the code to push the lvalue plus POST_DEC */
          if (!add_lvalue_code(&$1, F_POST_DEC))
              YYACCEPT;

          /* Check the types */
          if (exact_types.typeflags
           && !BASIC_TYPE($1.type, Type_Number)
           && !BASIC_TYPE($1.type, Type_Float)
             )
              type_error("Bad argument to --", $1.type);

          $$.end = CURRENT_PROGRAM_SIZE;
          $$.type = $1.type;
      } /* post-dec */

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr4
      {
          $$ = $1;
      }

; /* expr0 */


pre_inc_dec:
      L_INC { $$.code = F_PRE_INC; $$.start = CURRENT_PROGRAM_SIZE; }
    | L_DEC { $$.code = F_PRE_DEC; $$.start = CURRENT_PROGRAM_SIZE; }
;


expr4:
      function_call  %prec '~'
    | inline_func    %prec '~' {}
    | catch          %prec '~'
    | sscanf         %prec '~'
%ifdef USE_PARSE_COMMAND
    | parse_command  %prec '~'
%endif /* USE_PARSE_COMMAND */

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | L_STRING
      {
          /* Push a constant string */

          int string_number;
          PREPARE_INSERT(3)
          string_t *p;
%line
          p = last_lex_string;
          last_lex_string = NULL;
          $$.start = last_expression = CURRENT_PROGRAM_SIZE;
          $$.type = Type_String;
          $$.code = -1;

          string_number = store_prog_string(p);
          if ( string_number <= 0xff )
          {
              add_f_code(F_CSTRING0);
              add_byte(string_number);
          }
          else if ( string_number <= 0x1ff )
          {
              add_f_code(F_CSTRING1);
              add_byte(string_number);
          }
          else if ( string_number <= 0x2ff )
          {
              add_f_code(F_CSTRING2);
              add_byte(string_number);
          }
          else if ( string_number <= 0x3ff )
          {
              add_f_code(F_CSTRING3);
              add_byte(string_number);
          }
          else
          {
              add_f_code(F_STRING);
              add_short(string_number);
              CURRENT_PROGRAM_SIZE++;
          }
          CURRENT_PROGRAM_SIZE += 2;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | L_NUMBER
      {
          /* Store a number */

          p_int current;
          p_int number;
          PREPARE_INSERT(1 + sizeof (p_int))
%line
          $$.start = last_expression = current = CURRENT_PROGRAM_SIZE;
          $$.code = -1;
          number = $1;
          if ( number == 0 )
          {
              current++;
              add_f_code(F_CONST0);
              $$.type = Type_Any;
              /* TODO: Introduce a TYPE_NULL instead */
          }
          else if ( number == 1 )
          {
              add_f_code(F_CONST1);
              current++;
              $$.type = Type_Number;
          }
          else if ( number >= 0 && number <= 0xff )
          {
              add_f_code(F_CLIT);
              add_byte(number);
              current += 2;
              $$.type = Type_Number;
          }
          else if ( number < 0 && number >= -0x0ff )
          {
              add_f_code(F_NCLIT);
              add_byte(-number);
              current += 2;
              $$.type = Type_Number;
          }
          else
          {
              add_f_code(F_NUMBER);
              upd_p_int((char*)__PREPARE_INSERT__p - mem_block[A_PROGRAM].block, $1);
              current += 1 + sizeof (p_int);
              $$.type = Type_Number;
          }
          CURRENT_PROGRAM_SIZE = current;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | L_CLOSURE
      {
          int ix, inhIndex;

          $$.start = CURRENT_PROGRAM_SIZE;
          $$.code = -1;
          if (!pragma_warn_deprecated)
              ins_byte(F_NO_WARN_DEPRECATED);
          ix = $1.number;
          inhIndex = $1.inhIndex;

          // check for deprecated functions
          if (ix < CLOSURE_EFUN_OFFS)
          {
              // check only closures not directly to inherited functions (#'::fun),
              // they were checked by the lexxer.
              if (!inhIndex && ix < FUNCTION_COUNT)
              {
                  // ok, closure to lfun.
                  function_t *fun = FUNCTION(ix);
                  if (fun->flags & TYPE_MOD_DEPRECATED)
                  {
                      yywarnf("Creating lfun closure to deprecated function %s",
                              get_txt(fun->name));
                  }
              }
              else if (ix >= CLOSURE_IDENTIFIER_OFFS)
              {
                  // closure to global variable
                  // the lexxer only creates closure to non-virtual variables - our luck ;)
                  variable_t *varp = NV_VARIABLE(ix - CLOSURE_IDENTIFIER_OFFS - num_virtual_variables);
                  if (varp->type.typeflags & TYPE_MOD_DEPRECATED)
                      yywarnf("Creating closure to deprecated global variable %s.\n",
                              get_txt(varp->name));
              }
          }
          ins_f_code(F_CLOSURE);
          ins_short(ix);
          ins_short(inhIndex);
          $$.type = Type_Closure;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | L_SYMBOL
      {
          /* Generate a symbol */

          int string_number;
          int quotes;

          $$.start = CURRENT_PROGRAM_SIZE;
          $$.code = -1;
          quotes = $1.quotes;
          string_number = store_prog_string($1.name);
          if (quotes == 1 && string_number < 0x100)
          {
                /* One byte shorter than the other way */
                ins_f_code(F_CSTRING0);
                ins_byte(string_number);
                ins_f_code(F_QUOTE);
          }
          else
          {
                ins_f_code(F_SYMBOL);
                ins_short(string_number);
                ins_byte(quotes);
          }
          $$.type = Type_Symbol;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | L_FLOAT
      {
          /* Generate a float literal */

          int exponent;

          $$.start = CURRENT_PROGRAM_SIZE;
          $$.code = -1;
          ins_f_code(F_FLOAT);
          ins_uint32 ( SPLIT_DOUBLE( $1, &exponent) );
          ins_uint16 ( exponent );
          $$.type = Type_Float;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '(' note_start comma_expr ')'        %prec '~'
      {
          /* A nested expression */

          $$.type = $3.type;
          $$.start = $2.start;
          $$.code = -1;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '(' '{' note_start expr_list '}' ')' %prec '~'
      {
          /* Generate an array */

          check_aggregate_types($4);
            /* We don't care about these types,
             * unless a reference appears
             */

          ins_f_code(F_AGGREGATE);
          ins_short($4);
          if (max_array_size && $4 > (p_int)max_array_size)
              yyerror("Illegal array size");
          $$.type = Type_Ptr_Any;
          $$.start = $3.start;
          $$.code = -1;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | L_QUOTED_AGGREGATE note_start expr_list '}' ')' %prec '~'
      {
          /* Generate a quoted array by generating a normal
           * array first and then applying QUOTE as often
           * as possible.
           */

          int quotes;

          check_aggregate_types($3);
            /* We don't care about these types,
             * unless a reference appears
             */

          ins_f_code(F_AGGREGATE);
          ins_short($3);
          if (max_array_size && $3 > (p_int)max_array_size)
              yyerror("Illegal array size");
          $$.type = Type_Quoted_Array;
          $$.start = $2.start;
          $$.code = -1;
          quotes = $1;
          do {
                ins_f_code(F_QUOTE);
          } while (--quotes);
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '(' '[' ':' note_start

          /* Generate an empty mapping of given width */

      {
          ins_number(0);
      }

      expr0 ']' ')'

      {
          ins_f_code(F_M_ALLOCATE);

          $$.type = Type_Mapping;
          $$.start = $4.start;
          $$.code = -1;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '(' '[' note_start m_expr_list ']' ')'
      {
          /* Generate a mapping */

          mp_int num_keys;

          check_aggregate_types($4[0]);
          num_keys = $4[0] / ($4[1]+1);

          if ((num_keys|$4[1]) & ~0xffff)
              yyerror("cannot handle more than 65535 keys/values "
                      "in mapping aggregate");

          if ( (num_keys | $4[1]) &~0xff)
          {
              ins_f_code(F_M_AGGREGATE);
              ins_short(num_keys);
              ins_short($4[1]);
          }
          else
          {
              ins_f_code(F_M_CAGGREGATE);
              ins_byte(num_keys);
              ins_byte($4[1]);
          }

          $$.type = Type_Mapping;
          $$.start = $3.start;
          $$.code = -1;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '(' '<' note_start '>' ')'
      {
          yyerror("Missing identifier for empty struct literal");
          $$.type = Type_Unknown;
          $$.start = $3.start;
          $$.code = -1;
      }
    | '(' '<' note_start error ')'
      {
          /* Rule allows the parser to resynchronize after errors */
          $$.type = Type_Unknown;
          $$.start = $3.start;
          $$.code = -1;
      }
    | '(' '<' identifier '>'
      {
          int num;

          num = find_struct($3);
          if (num < 0)
          {
              yyerrorf("Unknown struct '%s'", get_txt($3));
              YYACCEPT;
          }
          $<number>$ = num;
          free_mstring($3);
      }

      note_start opt_struct_init ')'

      {
          /* Generate a literal struct */

          int num = $<number>5;
          struct_def_t *pdef = &(STRUCT_DEF(num));

          if ($7.length > STRUCT_MAX_MEMBERS
           || $7.length > struct_t_size(pdef->type))
          {
              /* Too many elements - create an empty struct */
              yyerrorf("Too many elements for literal struct '%s'"
                      , get_txt(struct_t_name(pdef->type)));
              CURRENT_PROGRAM_SIZE = $6.start;
              create_struct_literal(pdef, 0, NULL);
          }
          else if (!create_struct_literal(pdef, $7.length, $7.list))
          {
              /* Creation failed - create an empty struct */
              CURRENT_PROGRAM_SIZE = $6.start;
              create_struct_literal(pdef, 0, NULL);
          }

          /* Free the list of member descriptors */
          while ($7.list != NULL)
          {
              struct_init_t * p = $7.list;
              $7.list = p->next;
              if (p->name != NULL)
                  free_mstring(p->name);
              xfree(p);
          }

          $$.type.typeflags = TYPE_STRUCT;
          $$.type.t_struct = pdef->type;
          $$.start = $6.start;
          $$.code = -1;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr4 L_ARROW struct_member_name
      {
          /* Lookup a struct member */
          short s_index = -1;

          $$.start = $1.start;
          $$.code = -1;
          $$.type = $1.type; /* default */

          if (!IS_TYPE_ANY($1.type) && !IS_TYPE_STRUCT($1.type))
          {
              yyerrorf("Bad type for struct lookup: %s"
                      , get_type_name($1.type));
          }
          else
          {
              if (IS_TYPE_STRUCT($1.type))
              {
                  s_index = get_struct_index($1.type.t_struct);
                  if (s_index == -1)
                      yyerrorf("Unknown type in struct dereference: %s\n"
                              , get_type_name($1.type)
                              );
              }

              /* At this point: s_index >= 0: $1 is of type struct
               *                         < 0: $1 is of type mixed
               */

              if ($3 != NULL)
              {
                  int num;
                  struct_type_t * ptype = NULL;

                  if (s_index >= 0)
                  {
                      ptype = $1.type.t_struct;

                      num = struct_find_member(ptype, $3);
                      if (num < 0)
                      {
                          yyerrorf("No such member '%s' for struct '%s'"
                                  , get_txt($3)
                                  , get_txt(struct_t_name(ptype))
                                  );
                      }
                  }
                  else /* $1 is of type mixed */
                  {
                      s_index = find_struct_by_member($3, &num);
                      if (s_index >= 0)
                          ptype = STRUCT_DEF(s_index).type;
                  }

                  /* If this is a legal struct lookup, num >= 0 at this point
                   */
                  if (num >= 0)
                  {
                      ins_number(num);
                      ins_number(s_index);
                      ins_f_code(F_S_INDEX);
                      if (ptype != NULL)
                          assign_var_to_fulltype(&$$.type
                                                , ptype->member[num].type);
                  }
              }
              else /* Runtime lookup */
              {
                  ins_number(s_index);
                  ins_f_code(F_S_INDEX);
                  $$.type = Type_Any;
              }

              $$.end = CURRENT_PROGRAM_SIZE-1;
          }

          if ($3 != NULL)
              free_mstring($3);
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '&' '(' expr4 L_ARROW struct_member_name ')'

      {
          /* Create a reference to a struct member */
          short s_index = -1;

          $$.start = $3.start;
          $$.code = -1;
          $$.type = $3.type; /* default */

          if (!IS_TYPE_ANY($3.type) && !IS_TYPE_STRUCT($3.type))
          {
              yyerrorf("Bad type for struct lookup: %s"
                      , get_type_name($3.type));
          }
          else
          {
              /* '&(struct->member->member)' generates a simple
               * F_S_INDEX for the first lookup instead of a suitable
               * lvalue lookup. I don't understand the lvalue generation
               * well enough to correct the generated code, so for now
               * I restrict the lookup to one level.
               */
              if ($3.end != 0
               && F_S_INDEX == mem_block[A_PROGRAM].block[$3.end]
                 )
              {
                  yyerror("Implementation restriction: Only a single struct "
                          "member lookup allowed inside a &()");
              }

              if (IS_TYPE_STRUCT($3.type))
              {
                  s_index = get_struct_index($3.type.t_struct);
                  if (s_index == -1)
                      yyerrorf("Unknown type in lvalue struct derefence: %s\n"
                              , get_type_name($3.type)
                              );
              }

              /* At this point: s_index >= 0: $1 is of type struct
               *                         < 0: $1 is of type mixed
               */

              if ($5 != NULL)
              {
                  int num;
                  struct_type_t * ptype = NULL;

                  if (s_index >= 0)
                  {
                      ptype = $3.type.t_struct;

                      num = struct_find_member(ptype, $5);
                      if (num < 0)
                      {
                          yyerrorf("No such member '%s' for struct '%s'"
                                  , get_txt($5)
                                  , get_txt(struct_t_name(ptype))
                                  );
                      }
                  }
                  else /* $3 is of type mixed */
                  {
                      s_index = find_struct_by_member($5, &num);
                      if (s_index >= 0)
                          ptype = STRUCT_DEF(s_index).type;
                  }

                  /* If this is a legal struct lookup, num >= 0 at this point
                   */
                  if (num >= 0)
                  {
                      ins_number(num);
                      ins_number(s_index);
                      arrange_protected_lvalue($3.start, $3.code, $3.end,
                         F_PROTECTED_INDEX_S_LVALUE
                      );
                      if (ptype != NULL)
                      {
                          assign_var_to_fulltype(&$$.type
                                                , ptype->member[num].type);
                          $$.type.typeflags |= TYPE_MOD_REFERENCE;
                      }
                  }
              }
              else /* Runtime lookup */
              {
                  ins_number(s_index);
                  arrange_protected_lvalue($3.start, $3.code, $3.end,
                     F_PROTECTED_INDEX_S_LVALUE
                  );

                  $$.type = Type_Ref_Any;
              }

              $$.end = CURRENT_PROGRAM_SIZE-1;
          }

          if ($5 != NULL)
              free_mstring($5);
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr4 index_range %prec '['
      {
%line
          /* Generate a range expression */

          $$.start = $1.start;
          $$.code = -1;

          ins_f_code($2.inst);

          /* Check the types */
          if (exact_types.typeflags)
          {
              fulltype_t type;

              $1.type.typeflags &= TYPEID_MASK;
              $$.type = type = $1.type;
              if ((type.typeflags & TYPE_MOD_POINTER) == 0
               && type.typeflags != TYPE_ANY && type.typeflags != TYPE_STRING)
              {
                  type_error("Bad type of argument used for range", type);
                  $$.type = Type_Any;
              }
              type = $2.type1;
              if (type.typeflags != TYPE_ANY && type.typeflags != TYPE_NUMBER)
                  type_error("Bad type of index", type);
              type = $2.type2;
              if (type.typeflags != TYPE_ANY && type.typeflags != TYPE_NUMBER)
                  type_error("Bad type of index", type);
          }
          else
          {
              $$.type = Type_Any;
          }
      } /* expr4 index_range */

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '&' L_IDENTIFIER                        %prec '~'
      {
          /* Reference to a global variable, virtual or non-virtual.
           * We generate PUSH_LVALUE code and mark the type
           * as TYPE_MOD_REFERENCE.
           */

          int i;
          mp_uint current;
          bytecode_p p;
          variable_t *varp;
%line
          i = verify_declared($2);
          if (i == -1)
              /* variable not declared */
              YYACCEPT;

          $$.start = current = CURRENT_PROGRAM_SIZE;
          $$.code = -1;

          if (!realloc_a_program(3))
          {
              yyerrorf("Out of memory: program size %"PRIuMPINT"\n", current+3);
              YYACCEPT;
          }
          p = PROGRAM_BLOCK + current;

          if (i & VIRTUAL_VAR_TAG)
          {
              *p++ = F_PUSH_VIRTUAL_VARIABLE_LVALUE;
              *p = i;
              varp = V_VARIABLE(i);
          }
          else
          {
              if ((i + num_virtual_variables) & ~0xff)
              {
                  *p = F_PUSH_IDENTIFIER16_LVALUE;
                  upd_short(++current, i + num_virtual_variables);
              }
              else
              {
                  *p++ = F_PUSH_IDENTIFIER_LVALUE;
                  *p = i + num_virtual_variables;
              }
              varp = NV_VARIABLE(i);
          }

          CURRENT_PROGRAM_SIZE = current + 2;
          if (i == -1)
              $$.type = Type_Ref_Any;
          else
          {
              if (varp->type.typeflags & TYPE_MOD_DEPRECATED)
                  yywarnf("Referencing deprecated global variable %s.\n",
                          get_txt(varp->name));
              
              $$.type = varp->type;
              $$.type.typeflags = ($$.type.typeflags & TYPE_MOD_MASK)
                                  | TYPE_MOD_REFERENCE;
          }
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '&' L_LOCAL                        %prec '~'
      {
          /* Reference to a local variable.
           * We generate PUSH_LVALUE code and mark the type
           * as TYPE_MOD_REFERENCE.
           */

          mp_uint current;
          bytecode_p p;
%line
          $2 = check_for_context_local($2, &$$.type);

          $$.start = current = CURRENT_PROGRAM_SIZE;
          $$.code = -1;
          if (!realloc_a_program(2))
          {
              yyerrorf("Out of memory: program size %"PRIuMPINT"\n", 
                       current+2);
              YYACCEPT;
          }
          p = PROGRAM_BLOCK + current;

          if ($2->u.local.context >= 0)
          {
              *p++ = F_PUSH_CONTEXT_LVALUE;
              *p = $2->u.local.context;
          }
          else
          {
              *p++ = F_PUSH_LOCAL_VARIABLE_LVALUE;
              *p = $2->u.local.num;
          }
          $$.type.typeflags |= TYPE_MOD_REFERENCE;
          CURRENT_PROGRAM_SIZE = current + 2;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '&' '(' expr4 index_expr ')'        %prec '~'
      {
%line
          /* Generate the proper indexing operator */

          if ($4.inst == F_INDEX)
              arrange_protected_lvalue($3.start, $3.code, $3.end,
                 F_PROTECTED_INDEX_LVALUE
              );
          else if ($4.inst == F_RINDEX)
              arrange_protected_lvalue($3.start, $3.code, $3.end,
                 F_PROTECTED_RINDEX_LVALUE
              );
          else
              arrange_protected_lvalue($3.start, $3.code, $3.end,
                 F_PROTECTED_AINDEX_LVALUE
              );

          $$.start = $3.start;
          $$.code = -1;

          if ($4.type1.typeflags & TYPE_MOD_REFERENCE)
                yyerror("Reference used as index");

          /* Compute the result type */
          if (!exact_types.typeflags)
          {
                $$.type = Type_Ref_Any;
          }
          else
          {
              fulltype_t type;

              type = $3.type;
              type.typeflags &= TYPEID_MASK;
              if (type.typeflags & TYPE_MOD_POINTER)
              {
                    $$.type = type;
                    $$.type.typeflags &= ~TYPE_MOD_POINTER;
              }
              else if (type.typeflags == TYPE_MAPPING && $4.inst == F_INDEX)
              {
                  $4.type1 = Type_Any;
                  $$.type = Type_Ref_Any;
              }
              else switch (type.typeflags)
              {
              default:
                  type_error("Bad type to indexed reference", type);
                  /* FALLTHROUGH */
              case TYPE_ANY:
                  if ($4.inst == F_INDEX)
                      $4.type1 = Type_Any;
                  $$.type = Type_Ref_Any;
                  break;
              case TYPE_STRING:
                  $$.type = Type_Ref_Number;
                  break;
              }
              if (!BASIC_TYPE($4.type1, Type_Number))
                  type_error("Bad type of index", $4.type1);
          }
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '&' '(' expr4 '[' expr0 ',' expr0 ']' ')'
      {
%line
          /* Generate the proper indexing operator */

          $$.start = $3.start;
          $$.code = -1;
          $$.type = Type_Ref_Any;
          ins_f_code(F_PUSH_PROTECTED_INDEXED_MAP_LVALUE);

          if ($5.type.typeflags & TYPE_MOD_REFERENCE)
                yyerror("Reference used as index");

          /* Compute the result type */
          if (exact_types.typeflags)
          {
              fulltype_t type;

              type = $3.type;
              type.typeflags &= TYPEID_MASK;
              if (type.typeflags != TYPE_ANY && type.typeflags != TYPE_MAPPING)
              {
                  type_error("Bad type to indexed value", type);
              }
              type = $7.type;
              if (type.typeflags != TYPE_ANY && type.typeflags != TYPE_NUMBER)
                  type_error("Bad type of index", type);
          }
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '&' '(' expr4 index_range ')'        %prec '~'
        {
%line
          /* Generate the proper indexing operator */
          int prot_op;

          switch($4.inst)
          {
          case F_RANGE:    prot_op = F_PROTECTED_RANGE_LVALUE;    break;
          case F_NR_RANGE: prot_op = F_PROTECTED_NR_RANGE_LVALUE; break;
          case F_RN_RANGE: prot_op = F_PROTECTED_RN_RANGE_LVALUE; break;
          case F_RR_RANGE: prot_op = F_PROTECTED_RR_RANGE_LVALUE; break;
          case F_NA_RANGE: prot_op = F_PROTECTED_NA_RANGE_LVALUE; break;
          case F_AN_RANGE: prot_op = F_PROTECTED_AN_RANGE_LVALUE; break;
          case F_RA_RANGE: prot_op = F_PROTECTED_RA_RANGE_LVALUE; break;
          case F_AR_RANGE: prot_op = F_PROTECTED_AR_RANGE_LVALUE; break;
          case F_AA_RANGE: prot_op = F_PROTECTED_AA_RANGE_LVALUE; break;
          case F_NX_RANGE: prot_op = F_PROTECTED_NX_RANGE_LVALUE; break;
          case F_RX_RANGE: prot_op = F_PROTECTED_RX_RANGE_LVALUE; break;
          case F_AX_RANGE: prot_op = F_PROTECTED_AX_RANGE_LVALUE; break;
          default:
              fatal("Unsupported range type %d %s\n"
                   , $4.inst, get_f_name($4.inst));
          }

          arrange_protected_lvalue($3.start, $3.code, $3.end
                                  , prot_op
          );

          $$.start = $3.start;
          $$.code = -1;

          /* Compute the result type */
          if (!exact_types.typeflags)
          {
              $$.type = Type_Ref_Any;
          }
          else
          {
              fulltype_t type;

              $3.type.typeflags &= TYPEID_MASK;
              $$.type = type = $3.type;
              if ((type.typeflags & TYPE_MOD_POINTER) == 0
               && type.typeflags != TYPE_ANY && type.typeflags != TYPE_STRING)
              {
                  type_error("Bad type of argument used for range", type);
                  $$.type = Type_Any;
              }
              type = $4.type1;
              if (type.typeflags != TYPE_ANY && type.typeflags != TYPE_NUMBER)
                  type_error("Bad type of index", type);
              type = $4.type2;
              if (type.typeflags != TYPE_ANY && type.typeflags != TYPE_NUMBER)
                  type_error("Bad type of index", type);
          }
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
%// The following expressions can be patched to lvalues for use in index_lvalue.
    | L_IDENTIFIER
      {
          /* Access a global variable */
          int i;
          mp_uint current;
          bytecode_p p;
          variable_t *varp;
%line
          i = verify_declared($1);
          if (i == -1)
              /* variable not declared */
              YYACCEPT;

          $$.start = current = CURRENT_PROGRAM_SIZE;
          $$.end = 0;

          if (!realloc_a_program(3))
          {
              yyerrorf("Out of memory: program size %"PRIuMPINT"\n", current+3);
              YYACCEPT;
          }
          p = PROGRAM_BLOCK + current;

          if (i & VIRTUAL_VAR_TAG)
          {
              /* Access a virtual variable */

              $$.code = F_PUSH_VIRTUAL_VARIABLE_LVALUE;
              *p++ = F_VIRTUAL_VARIABLE;
              *p = i;
              varp = V_VARIABLE(i);
              $$.type = varp->type;
          }
          else
          {
              /* Access a non-virtual variable */

              if ((i + num_virtual_variables) & ~0xff)
              {
                  $$.code = F_PUSH_IDENTIFIER16_LVALUE;
                  *p = F_IDENTIFIER16;
                  upd_short(++current, i + num_virtual_variables);
                  $$.end = current+1;
              }
              else
              {
                  $$.code = F_PUSH_IDENTIFIER_LVALUE;
                  *p++ = F_IDENTIFIER;
                  *p = i + num_virtual_variables;
              }
              varp = NV_VARIABLE(i);
              $$.type = varp->type;
          }
          if (varp->type.typeflags & TYPE_MOD_DEPRECATED)
          {
              yywarnf("Using deprecated global variable %s.\n",
                      get_txt(varp->name));
          }
          $$.type.typeflags &= TYPE_MOD_MASK;

          CURRENT_PROGRAM_SIZE = current + 2;
          if (i == -1)
              $$.type = Type_Any;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | L_LOCAL
      {
          /* Access a local variable */

          mp_uint current;
          bytecode_p p;
%line
          $1 = check_for_context_local($1, &$$.type);

          $$.start = current = CURRENT_PROGRAM_SIZE;
          $$.end = 0;
          if (!realloc_a_program(2))
          {
              yyerrorf("Out of memory: program size %"PRIuMPINT"\n", current+2);
              YYACCEPT;
          }
          p = PROGRAM_BLOCK + current;

          if ($1->u.local.context >= 0)
          {
              $$.code = F_PUSH_CONTEXT_LVALUE;
              *p++ = F_CONTEXT_IDENTIFIER;
              *p = $1->u.local.context;
          }
          else
          {
              $$.code = F_PUSH_LOCAL_VARIABLE_LVALUE;
              *p++ = F_LOCAL;
              *p = $1->u.local.num;
          }
          CURRENT_PROGRAM_SIZE = current + 2;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr4 index_expr %prec '['
      {
%line
          /* Generate (R)INDEX/PUSH_(R)INDEXED_LVALUE */

          $$.start = $1.start;
          $$.end = CURRENT_PROGRAM_SIZE;
          if ($2.inst == F_INDEX)
          {
              $$.code = F_PUSH_INDEXED_LVALUE;
              ins_f_code(F_INDEX);
          }
          else if ($2.inst == F_RINDEX)
          {
              $$.code = F_PUSH_RINDEXED_LVALUE;
              ins_f_code(F_RINDEX);
          }
          else
          {
              $$.code = F_PUSH_AINDEXED_LVALUE;
              ins_f_code(F_AINDEX);
          }

          if ($2.type1.typeflags & TYPE_MOD_REFERENCE)
              yyerror("Reference used as index");

          /* Check and compute the types */
          if (exact_types.typeflags)
          {
              fulltype_t type;

              type = $1.type;
              type.typeflags &= TYPEID_MASK;
              if (type.typeflags & TYPE_MOD_POINTER)
              {
                  $$.type = type;
                  $$.type.typeflags &= ~TYPE_MOD_POINTER;
              }
              else if (type.typeflags == TYPE_MAPPING && $2.inst == F_INDEX)
              {
                  $2.type1 = Type_Any;
                  $$.type = Type_Any;
              }
              else switch (type.typeflags)
              {
              default:
                  type_error("Bad type to indexed value", type);
                  /* FALLTHROUGH */
              case TYPE_ANY:
                  if ($2.inst == F_INDEX)
                      $2.type1 = Type_Any;
                  $$.type = Type_Any;
                  break;
              case TYPE_STRING:
                  $$.type = Type_Number;
                  break;
              }
              if (!BASIC_TYPE($2.type1, Type_Number))
                  type_error("Bad type of index", $2.type1);
          }
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr4 '[' expr0 ',' expr0 ']' %prec '['
      {
%line
          /* Generate MAP_INDEX/PUSH_INDEXED_MAP_LVALUE */

          $$.start = $1.start;
          $$.end = CURRENT_PROGRAM_SIZE;
          $$.code = F_PUSH_INDEXED_MAP_LVALUE;
          $$.type = Type_Any;
          ins_f_code(F_MAP_INDEX);

          if ($3.type.typeflags & TYPE_MOD_REFERENCE)
              yyerror("Reference used as index");

          /* Check and compute types */
          if (exact_types.typeflags)
          {
              fulltype_t type;

              type = $1.type;
              type.typeflags &= TYPEID_MASK;
              if (type.typeflags != TYPE_ANY && type.typeflags != TYPE_MAPPING)
              {
                  type_error("Bad type to indexed value", type);
              }
              type = $5.type;
              type.typeflags &= TYPEID_MASK;
              if (type.typeflags != TYPE_ANY && type.typeflags != TYPE_NUMBER)
                  type_error("Bad type of index", type);
          }
      }
; /* expr4 */


lvalue:
    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
      name_lvalue

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr4 index_expr %prec '['
      {
          /* Generate/add an (R)INDEX_LVALUE */

          bytecode_p p, q;
          p_int start, current;
%line
          start = $1.start;
          current = CURRENT_PROGRAM_SIZE;

          p = PROGRAM_BLOCK;
          q = yalloc(current-start+2); /* assign uses an extra byte */

          /* First change the rvalue 'expr4' into an lvalue.
           */
          if ($1.code >= 0)
          {
              p_int end, start2;

              if ( 0 != (end = $1.end) )
              {
                  /* Multibyte instruction */
                  start2 = end+1;
                  if ($1.code == F_PUSH_IDENTIFIER16_LVALUE)
                      p[start] = $1.code;
                  else
                      p[end] = $1.code;
                  memcpy(q, p + start2, current - start2);
                  memcpy(q + current - start2, p + start, start2 - start);
                  if ($2.inst == F_INDEX)
                      q[current - start] = F_INDEX_LVALUE;
                  else if ($2.inst == F_RINDEX)
                      q[current - start] = F_RINDEX_LVALUE;
                  else
                      q[current - start] = F_AINDEX_LVALUE;
              }
              else
              {
                  /* Simple relocation/insertion */
                  bytecode_t c;

                  start2 = start + 2;
                  c = p[start+1];
                  memcpy(q, p + start2, current - start2);
                  p = q + current - start2;
                  *p++ = $1.code;
                  *p++ = c;
                  if ($2.inst == F_INDEX)
                      *p = F_INDEX_LVALUE;
                  else if ($2.inst == F_RINDEX)
                      *p = F_RINDEX_LVALUE;
                  else
                      *p = F_AINDEX_LVALUE;
              }
          }
          else
          {
              /* We can just copy the instruction block
               * and add a PUSH_(R)INDEXED_LVALUE
               */
              memcpy(q, p + start, current - start);
              if ($2.inst == F_INDEX)
                  q[current - start] = F_PUSH_INDEXED_LVALUE;
              else if ($2.inst == F_RINDEX)
                  q[current - start] = F_PUSH_RINDEXED_LVALUE;
              else
                  q[current - start] = F_PUSH_AINDEXED_LVALUE;
          }

          /* This is what we return */
          $$.length = current + 1 - start;
          $$.u.p = q;

          CURRENT_PROGRAM_SIZE = start;
          last_expression = -1;

          if ($2.type1.typeflags & TYPE_MOD_REFERENCE)
              yyerror("Reference used as index");

          /* Check and compute types */
          if (exact_types.typeflags)
          {
              fulltype_t type;

              type = $1.type;
              type.typeflags &= TYPEID_MASK;
              if (type.typeflags & TYPE_MOD_POINTER)
              {
                  $$.type = type;
                  $$.type.typeflags &= ~TYPE_MOD_POINTER;
              }
              else if (type.typeflags == TYPE_MAPPING && $2.inst == F_INDEX)
              {
                  $2.type1 = Type_Any;
                  $$.type = Type_Any;
              }
              else switch (type.typeflags)
              {
              default:
                  type_error("Bad type to indexed lvalue", type);
                  /* FALLTHROUGH */
              case TYPE_ANY:
                  if ($2.inst == F_INDEX)
                      $2.type1 = Type_Any;
                  $$.type = Type_Any;
                  break;
              case TYPE_STRING:
                  $$.type = Type_Number;
                  break;
              }
              if (!BASIC_TYPE($2.type1, Type_Number))
                  type_error("Bad type of index", $2.type1);
          }
          else
          {
              $$.type = Type_Any;
          }
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr4 '[' expr0 ',' expr0 ']' %prec '['
      {
          /* Generate/add an PUSH_INDEXED_MAP_LVALUE */

          bytecode_p p, q;
          p_int start, current;
%line

          /* Well, just generate the code: expr4 must be
           * a mapping, or a runtime error will occur.
           */
          start = $1.start;
          current = CURRENT_PROGRAM_SIZE;
          p = PROGRAM_BLOCK;
          q = yalloc(current-start+2); /* assign uses an extra byte */
          memcpy(q, p + start, current - start);
          q[current - start] = F_PUSH_INDEXED_MAP_LVALUE;

          $$.length = current + 1 - start;
          $$.u.p = q;
          $$.type = Type_Any;
          CURRENT_PROGRAM_SIZE = start;
          last_expression = -1;

          if ($3.type.typeflags & TYPE_MOD_REFERENCE)
              yyerror("Reference used as index");

          /* Check and compute types */
          if (exact_types.typeflags)
          {
              fulltype_t type;

              type = $1.type;
              type.typeflags &= TYPEID_MASK;
              if (type.typeflags != TYPE_ANY && type.typeflags != TYPE_MAPPING)
              {
                  type_error("Bad type to indexed value", type);
              }
              type = $5.type;
              type.typeflags &= TYPEID_MASK;
              if (type.typeflags != TYPE_ANY && type.typeflags != TYPE_NUMBER)
                  type_error("Bad type of index", type);
          }
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr4 index_range %prec '['
      {
          /* RANGE_LVALUE generation */

          bytecode_p p, q;
          p_int start, current;
          int indexing_code;

%line
          switch ($2.inst)
          {
          case F_RANGE:    indexing_code = F_RANGE_LVALUE; break;
          case F_NR_RANGE: indexing_code = F_NR_RANGE_LVALUE; break;
          case F_RN_RANGE: indexing_code = F_RN_RANGE_LVALUE; break;
          case F_RR_RANGE: indexing_code = F_RR_RANGE_LVALUE; break;
          case F_NA_RANGE: indexing_code = F_NA_RANGE_LVALUE; break;
          case F_AN_RANGE: indexing_code = F_AN_RANGE_LVALUE; break;
          case F_RA_RANGE: indexing_code = F_RA_RANGE_LVALUE; break;
          case F_AR_RANGE: indexing_code = F_AR_RANGE_LVALUE; break;
          case F_AA_RANGE: indexing_code = F_AA_RANGE_LVALUE; break;
          case F_NX_RANGE: indexing_code = F_NX_RANGE_LVALUE; break;
          case F_RX_RANGE: indexing_code = F_RX_RANGE_LVALUE; break;
          case F_AX_RANGE: indexing_code = F_AX_RANGE_LVALUE; break;
          default:
              errorf("Unsupported range type %d %s\n"
                   , $2.inst, get_f_name($2.inst));
          }

          start = $1.start;
          current = CURRENT_PROGRAM_SIZE;
          p = PROGRAM_BLOCK;
          q = yalloc(current-start+3);

          /* Change the expr4 into an lvalue
           */
          if ($1.code < 0)
          {
                yyerror("Need lvalue for range lvalue.");
          }
          else
          {
              p_int end, start2;

              if ( 0 != (end = $1.end) )
              {
                  /* Multibyte instruction */
                  start2 = end+1;
                  if ($1.code == F_PUSH_IDENTIFIER16_LVALUE)
                  {
                      p[start] = $1.code;
                  }
                  else
                  {
                      p[end] = $1.code;
                  }
              }
              else
              {
                  /* Simple relocation/replacement */
                  start2 = start+2;
                  p[start] = $1.code;
              }

              /* Do the actual relocation */
              memcpy(q, p + start2, current - start2);
              memcpy(q + current - start2, p + start, start2 - start);
              current -= start;

              /* Insert the indexing code */
              if (instrs[indexing_code].prefix)
              {
                  q[current++] = instrs[indexing_code].prefix;
              }
              q[current] = instrs[indexing_code].opcode;
          }

          /* This is what we return */
          $$.length = current + 1;
          $$.u.p = q;

          CURRENT_PROGRAM_SIZE = start;
          last_expression = -1;

          /* Compute and check the types */
          if (exact_types.typeflags)
          {
              fulltype_t type;

              $1.type.typeflags &= TYPEID_MASK;
              $$.type = type = $1.type;
              if ((type.typeflags & TYPE_MOD_POINTER) == 0
               &&  type.typeflags != TYPE_ANY
               &&  type.typeflags != TYPE_STRING)
              {
                  type_error("Bad type of argument used for range", type);
                  $$.type = Type_Any;
              }
              type = $2.type1;
              if (type.typeflags != TYPE_ANY && type.typeflags != TYPE_NUMBER)
                  type_error("Bad type of index", type);
              type = $2.type2;
              if (type.typeflags != TYPE_ANY && type.typeflags != TYPE_NUMBER)
                  type_error("Bad type of index", type);
          }
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr4 L_ARROW struct_member_name
      {
          /* Create a struct member lvalue */

          short s_index = -1;
          int num;
          vartype_t member_type;

          /* If the struct lookup is ok, set num and member_type */

          num = 0;
          member_type = VType_Unknown;

          if (!IS_TYPE_ANY($1.type) && !IS_TYPE_STRUCT($1.type))
          {
              yyerrorf("Bad type for struct lookup: %s"
                      , get_type_name($1.type));
          }
          else
          {
              if (IS_TYPE_STRUCT($1.type))
              {
                  s_index = get_struct_index($1.type.t_struct);
                  if (s_index == -1)
                      yyerrorf("Unknown type in lvalue struct dereference: %s\n"
                              , get_type_name($1.type)
                              );
              }

              /* At this point: s_index >= 0: $1 is of type struct
               *                         < 0: $1 is of type mixed
               */

              if ($3 != NULL)
              {
                  struct_type_t * ptype = NULL;

                  if (s_index >= 0)
                  {
                      ptype = $1.type.t_struct;

                      num = struct_find_member(ptype, $3);
                      if (num < 0)
                      {
                          yyerrorf("No such member '%s' for struct '%s'"
                                  , get_txt($3)
                                  , get_txt(struct_t_name(ptype))
                                  );
                      }
                      else
                          member_type = ptype->member[num].type;
                  }
                  else /* $1 is of type mixed */
                  {
                      s_index = find_struct_by_member($3, &num);
                      if (s_index >= 0)
                      {
                          ptype = STRUCT_DEF(s_index).type;
                          member_type = ptype->member[num].type;
                      }
                  }
              }
              else /* Runtime lookup */
              {
                  assign_full_to_vartype(&member_type, Type_Any);
              }
          }


          /* We have to generate some code, so if the struct lookup is
           * invalid, we just play along and generate code to look up
           * member #0 in whatever we got.
           */

          {
              bytecode_p p, q;
              p_int start, current;

              if ($3 != NULL)
              {
                  /* Insert the index code */
                  ins_number(num);
              }

              /* Insert the struct type index */
              ins_number(s_index);

              /* Generate/add an INDEX_S_LVALUE */

              start = $1.start;
              current = CURRENT_PROGRAM_SIZE;

              p = PROGRAM_BLOCK;
              q = yalloc(current-start+2); /* assign uses an extra byte */

              /* First change the rvalue 'expr4' into an lvalue.
               */
              if ($1.code >= 0)
              {
                  p_int end, start2;

                  if ( 0 != (end = $1.end) )
                  {
                      /* Multibyte instruction */
                      start2 = end+1;
                      if ($1.code == F_PUSH_IDENTIFIER16_LVALUE)
                          p[start] = $1.code;
                      else
                          p[end] = $1.code;
                      memcpy(q, p + start2, current - start2);
                      memcpy(q + current - start2, p + start, start2 - start);
                      q[current - start] = F_INDEX_S_LVALUE;
                  }
                  else
                  {
                      /* Simple relocation/insertion */
                      bytecode_t c;

                      start2 = start + 2;
                      c = p[start+1];
                      memcpy(q, p + start2, current - start2);
                      p = q + current - start2;
                      *p++ = $1.code;
                      *p++ = c;
                      *p = F_INDEX_S_LVALUE;
                  }
              }
              else
              {
                  /* We can just copy the instruction block
                   * and add a PUSH_(R)INDEXED_LVALUE
                   */
                  memcpy(q, p + start, current - start);
                  q[current - start] = F_PUSH_INDEXED_S_LVALUE;
              }

              /* This is what we return */
              $$.length = current + 1 - start;
              $$.u.p = q;

              CURRENT_PROGRAM_SIZE = start;
              last_expression = -1;

              assign_var_to_fulltype(&$$.type, member_type);
          }

          if ($3 != NULL)
              free_mstring($3);
      }

; /* lvalue */


name_lvalue:
    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    L_IDENTIFIER
      {
          /* Generate the lvalue for a global variable */

          int i;
          variable_t *varp;
%line
          $$.length = 0;
          i = verify_declared($1);
          if (i == -1)
              /* variable not declared */
              YYACCEPT;

          if (i & VIRTUAL_VAR_TAG)
          {
              $$.u.simple[0] = F_PUSH_VIRTUAL_VARIABLE_LVALUE;
              $$.u.simple[1] = i;
              varp = V_VARIABLE(i);
              $$.type = varp->type;
              $$.type.typeflags &= TYPE_MOD_MASK;
          }
          else
          {
              if ((i + num_virtual_variables) & ~0xff)
              {
                  bytecode_p q;

                  q = yalloc(4); /* assign uses an extra byte */
                  $$.length = 3;
                  $$.u.p = q;
                  q[0] = F_PUSH_IDENTIFIER16_LVALUE;
                  PUT_SHORT(q+1, i + num_virtual_variables);
                  $$.type = NV_VARIABLE(i)->type;
                  $$.type.typeflags &= TYPE_MOD_MASK;
              }
              else
              {
                  $$.u.simple[0] = F_PUSH_IDENTIFIER_LVALUE;
                  $$.u.simple[1] = i + num_virtual_variables;
              }
              varp = NV_VARIABLE(i);
              $$.type = varp->type;
              $$.type.typeflags &= TYPE_MOD_MASK;
          }
          if (varp->type.typeflags & TYPE_MOD_DEPRECATED)
              yywarnf("Using deprecated global variable %s.\n",
                      get_txt(varp->name));
          
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | L_LOCAL
      {
%line
          /* Generate the lvalue for a local */

          $1 = check_for_context_local($1, &$$.type);

          if ($1->u.local.context >= 0)
          {
              $$.u.simple[0] = F_PUSH_CONTEXT_LVALUE;
              $$.u.simple[1] = $1->u.local.context;
          }
          else
          {
              $$.u.simple[0] = F_PUSH_LOCAL_VARIABLE_LVALUE;
              $$.u.simple[1] = $1->u.local.num;
          }
          $$.length = 0;
      }
; /* name_lvalue */


local_name_lvalue:
      basic_type optional_star L_IDENTIFIER
      {
          define_local_variable($3, $1, $2, &$$, MY_FALSE, MY_TRUE);
      }
    | basic_type optional_star L_LOCAL
      {
          define_local_variable($3, $1, $2, &$$, MY_TRUE, MY_TRUE);
      }
; /* local_name_lvalue */


/* The following rules are used to parse and compile the various
 * forms of array indexing/ranging operations.
 * They used at various places in the rules of expr0, expr4 and lvalue.
 */

index_expr :
      '[' expr0 ']'
        {
            $$.inst = F_INDEX;
            $$.start = $2.start;
            $$.end = $2.end;
            $$.type1 = $2.type;
            if (!pragma_warn_deprecated)
            {
                ins_byte(F_NO_WARN_DEPRECATED);
                $$.end++;
            }

        }

    | '[' '<' expr0 ']'
        {
            $$.inst = F_RINDEX;
            $$.start = $3.start;
            $$.end = $3.end;
            $$.type1 = $3.type;
            if (!pragma_warn_deprecated)
            {
                ins_byte(F_NO_WARN_DEPRECATED);
                $$.end++;
            }
        }

    | '[' '>' expr0 ']'
        {
            $$.inst = F_AINDEX;
            $$.start = $3.start;
            $$.end = $3.end;
            $$.type1 = $3.type;
            if (!pragma_warn_deprecated)
            {
                ins_byte(F_NO_WARN_DEPRECATED);
                $$.end++;
            }
        }

; /* index_expr */

index_range :
    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
      '['           L_RANGE     expr0 ']'

      {
          /* Simulate an expression yielding 0 for the lower bound.
           * We pretend that it's part of the upper bound expr.
           */

          p_int current;
          p_int length;
          bytecode_p mark, p;

          current = CURRENT_PROGRAM_SIZE;

          if (!realloc_a_program(1))
          {
              yyerrorf("Out of memory: program size %"PRIdPINT"\n", current+1);
              YYACCEPT;
          }

          mark = PROGRAM_BLOCK + $3.start;
          p = PROGRAM_BLOCK + current;
          length = current - $3.start;
          for( ; --length >= 0; p--) PUT_CODE(p, GET_CODE(p-1));
          STORE_CODE(mark, F_CONST0);
          CURRENT_PROGRAM_SIZE++;
          $3.end++;

          if (pragma_range_check)
          {
              ins_byte(F_ARRAY_RANGE_CHECK);
              $3.end++;
          }

          /* Return the data */

          $$.inst  = F_RANGE;
          $$.start = $3.start;
          $$.end   = $3.end;
          $$.type1 = Type_Number;
          $$.type2 = $3.type;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '['           L_RANGE '<' expr0 ']'

      {
          /* Simulate an expression yielding 0 for the lower bound.
           * We pretend that it's part of the upper bound expr.
           */

          p_int current;
          p_int length;
          bytecode_p mark, p;

          current = CURRENT_PROGRAM_SIZE;

          if (!realloc_a_program(1))
          {
              yyerrorf("Out of memory: program size %"PRIdPINT"\n", current+1);
              YYACCEPT;
          }

          mark = PROGRAM_BLOCK + $4.start;
          p = PROGRAM_BLOCK + current;
          length = current - $4.start;
          for( ; --length >= 0; p--) PUT_CODE(p, GET_CODE(p-1));
          STORE_CODE(mark, F_CONST0);
          CURRENT_PROGRAM_SIZE++;
          $4.end++;

          if (pragma_range_check)
          {
              ins_byte(F_ARRAY_RANGE_CHECK);
              $4.end++;
          }

          /* Return the data */

          $$.inst  = F_NR_RANGE;
          $$.start = $4.start;
          $$.end   = $4.end;
          $$.type1 = Type_Number;
          $$.type2 = $4.type;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '['           L_RANGE '>' expr0 ']'

      {
          /* Simulate an expression yielding 0 for the lower bound.
           * We pretend that it's part of the upper bound expr.
           */

          p_int current;
          p_int length;
          bytecode_p mark, p;

          current = CURRENT_PROGRAM_SIZE;

          if (!realloc_a_program(1))
          {
              yyerrorf("Out of memory: program size %"PRIdPINT"\n", current+1);
              YYACCEPT;
          }

          mark = PROGRAM_BLOCK + $4.start;
          p = PROGRAM_BLOCK + current;
          length = current - $4.start;
          for( ; --length >= 0; p--) PUT_CODE(p, GET_CODE(p-1));
          STORE_CODE(mark, F_CONST0);
          CURRENT_PROGRAM_SIZE++;
          $4.end++;

          if (pragma_range_check)
          {
              ins_byte(F_ARRAY_RANGE_CHECK);
              $4.end++;
          }

          /* Return the data */

          $$.inst  = F_NA_RANGE;
          $$.start = $4.start;
          $$.end   = $4.end;
          $$.type1 = Type_Number;
          $$.type2 = $4.type;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '['     expr0 L_RANGE     expr0 ']'
      {
          if (pragma_range_check)
          {
              ins_byte(F_ARRAY_RANGE_CHECK);
              $4.end++;
          }

          $$.inst  = F_RANGE;
          $$.start = $2.start;
          $$.end   = $4.end;
          $$.type1 = $2.type;
          $$.type2 = $4.type;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '['     expr0 L_RANGE '<' expr0 ']'
      {
          if (pragma_range_check)
          {
              ins_byte(F_ARRAY_RANGE_CHECK);
              $5.end++;
          }

          $$.inst  = F_NR_RANGE;
          $$.start = $2.start;
          $$.end   = $5.end;
          $$.type1 = $2.type;
          $$.type2 = $5.type;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '[' '<' expr0 L_RANGE     expr0 ']'
      {
          if (pragma_range_check)
          {
              ins_byte(F_ARRAY_RANGE_CHECK);
              $5.end++;
          }

          $$.inst  = F_RN_RANGE;
          $$.start = $3.start;
          $$.end   = $5.end;
          $$.type1 = $3.type;
          $$.type2 = $5.type;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '[' '<' expr0 L_RANGE '<' expr0 ']'
      {
          if (pragma_range_check)
          {
              ins_byte(F_ARRAY_RANGE_CHECK);
              $6.end++;
          }

          $$.inst  = F_RR_RANGE;
          $$.start = $3.start;
          $$.end   = $6.end;
          $$.type1 = $3.type;
          $$.type2 = $6.type;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '['     expr0 L_RANGE '>' expr0 ']'
      {
          if (pragma_range_check)
          {
              ins_byte(F_ARRAY_RANGE_CHECK);
              $5.end++;
          }

          $$.inst  = F_NA_RANGE;
          $$.start = $2.start;
          $$.end   = $5.end;
          $$.type1 = $2.type;
          $$.type2 = $5.type;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '[' '>' expr0 L_RANGE     expr0 ']'
      {
          if (pragma_range_check)
          {
              ins_byte(F_ARRAY_RANGE_CHECK);
              $5.end++;
          }

          $$.inst  = F_AN_RANGE;
          $$.start = $3.start;
          $$.end   = $5.end;
          $$.type1 = $3.type;
          $$.type2 = $5.type;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '[' '<' expr0 L_RANGE '>' expr0 ']'
      {
          if (pragma_range_check)
          {
              ins_byte(F_ARRAY_RANGE_CHECK);
              $6.end++;
          }

          $$.inst  = F_RA_RANGE;
          $$.start = $3.start;
          $$.end   = $6.end;
          $$.type1 = $3.type;
          $$.type2 = $6.type;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '[' '>' expr0 L_RANGE '<' expr0 ']'
      {
          if (pragma_range_check)
          {
              ins_byte(F_ARRAY_RANGE_CHECK);
              $6.end++;
          }

          $$.inst  = F_AR_RANGE;
          $$.start = $3.start;
          $$.end   = $6.end;
          $$.type1 = $3.type;
          $$.type2 = $6.type;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '[' '>' expr0 L_RANGE '>' expr0 ']'
      {
          if (pragma_range_check)
          {
              ins_byte(F_ARRAY_RANGE_CHECK);
              $6.end++;
          }

          $$.inst  = F_AA_RANGE;
          $$.start = $3.start;
          $$.end   = $6.end;
          $$.type1 = $3.type;
          $$.type2 = $6.type;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '['     expr0 L_RANGE           ']'
      {
          if (pragma_range_check)
          {
              ins_byte(F_ARRAY_RANGE_CHECK);
              $2.end++;
          }

          $$.inst  = F_NX_RANGE;
          $$.start = $2.start;
          $$.end   = $2.end;
          $$.type1 = $2.type;
          $$.type2 = Type_Number;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '[' '<' expr0 L_RANGE           ']'
      {
          /* Simulate an expression yielding <1 for the upper bound.
           * We pretend that it's part of the lower bound expr.
           */

          if (pragma_range_check)
          {
              ins_byte(F_ARRAY_RANGE_CHECK);
              $3.end++;
          }

          $$.inst  = F_RX_RANGE;
          $$.start = $3.start;
          $$.end   = $3.end;
          $$.type1 = $3.type;
          $$.type2 = Type_Number;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '[' '>' expr0 L_RANGE           ']'
      {
          /* Simulate an expression yielding <1 for the upper bound.
           * We pretend that it's part of the lower bound expr.
           */

          if (pragma_range_check)
          {
              ins_byte(F_ARRAY_RANGE_CHECK);
              $3.end++;
          }

          $$.inst  = F_AX_RANGE;
          $$.start = $3.start;
          $$.end   = $3.end;
          $$.type1 = $3.type;
          $$.type2 = Type_Number;
      }
; /* index_range */


/* The following rules are used to construct array and
 * mapping literals in expr4, and argument lists in function calls.
 * Besides compiling the values, the rules count the number
 * of values generated and add the types to the arg_types[].
 */

expr_list:
      /* empty */     { $$ = 0; }
    | expr_list2      { $$ = $1; }
    | expr_list2 ','  { $$ = $1; }  /* Allow a terminating comma */
; /* expr_list */

expr_list2:
      expr0                 { $$ = 1;      add_arg_type($1.type); }
    | expr_list2 ',' expr0  { $$ = $1 + 1; add_arg_type($3.type); }
; /* expr_list2 */

e_expr_list2:
      expr0
      {
          $$ = 1;
          if (!got_ellipsis[argument_level])
              add_arg_type($1.type);
          else
              add_arg_type(Type_Any);
      }

    | expr0 L_ELLIPSIS
      {
          PREPARE_INSERT(2);

          $$ = 0;
          got_ellipsis[argument_level] = MY_TRUE;
          add_f_code(F_FLATTEN_XARG);
          CURRENT_PROGRAM_SIZE++;
      }

    | e_expr_list2 ',' expr0
      {
          $$ = $1 + 1;
          if (!got_ellipsis[argument_level])
              add_arg_type($3.type);
          else
              add_arg_type(Type_Any);
      }

    | e_expr_list2 ',' expr0 L_ELLIPSIS
      {
          PREPARE_INSERT(2);

          $$ = $1;
          got_ellipsis[argument_level] = MY_TRUE;
          add_f_code(F_FLATTEN_XARG);
          CURRENT_PROGRAM_SIZE++;
      }
; /* e_expr_list2 */

expr_list3:
      /* empty */
      { $$ = 0; }

    | expr0
      {
          $$ = 1;
          if (!got_ellipsis[argument_level])
              add_arg_type($1.type);
          else
              add_arg_type(Type_Any);
      }

    | expr0 L_ELLIPSIS
      {
          PREPARE_INSERT(2);

          $$ = 0;
          got_ellipsis[argument_level] = MY_TRUE;
          add_f_code(F_FLATTEN_XARG);
          CURRENT_PROGRAM_SIZE++;
      }

    | e_expr_list2 ',' expr0
      {
          $$ = $1 + 1;
          if (!got_ellipsis[argument_level])
              add_arg_type($3.type);
          else
              add_arg_type(Type_Any);
      }

    | e_expr_list2 ',' expr0 L_ELLIPSIS
      {
          PREPARE_INSERT(2);

          $$ = $1;
          got_ellipsis[argument_level] = MY_TRUE;
          add_f_code(F_FLATTEN_XARG);
          CURRENT_PROGRAM_SIZE++;
      }
; /* expr_list3 */


m_expr_list:
      /* empty */          { $$[0] = 0; $$[1]= 1; }
    | m_expr_list2      /* { $$ = $1; } */
    | m_expr_list2 ','  /* { $$ = $1; } Allow a terminating comma */
    | expr_list2           { $$[0] = $1; $$[1] = 0; }
    | expr_list2 ','       { $$[0] = $1; $$[1] = 0; }
; /* m_expr_list */

m_expr_list2:
      expr0  m_expr_values
      {
          $$[0] = 1 + $2;
          $$[1] = $2;
          add_arg_type($1.type); /* order doesn't matter */
      }

    | m_expr_list2 ',' expr0 m_expr_values
      {
          if ($1[1] != $4) {
              yyerror("Inconsistent number of values in mapping literal");
          }
          $$[0] = $1[0] + 1 + $4;
          $$[1] = $1[1];
          add_arg_type($3.type);
      }
; /* m_expr_list2 */

m_expr_values:
      ':' expr0                { $$ = 1;      add_arg_type($2.type); }
    | m_expr_values ';' expr0  { $$ = $1 + 1; add_arg_type($3.type); }
; /* m_expr_values */



/* Rule used to parse a static or dynamic member name in lookups */

struct_member_name:
      identifier
      { $$ = $1; }

    | L_STRING L_STRING
      { fatal("presence of rule should prevent its reduction"); }

    | L_STRING
      {
          $$ = last_lex_string; /* Adopt the reference */
          last_lex_string = NULL;
      }

    |  '(' expr0 ')'
      {
          $$ = NULL;
          if ($2.type.typeflags != TYPE_STRING
           && (pragma_strict_types != PRAGMA_WEAK_TYPES
               || $2.type.typeflags != TYPE_UNKNOWN)
           && $2.type.typeflags != TYPE_ANY
           && $2.type.typeflags != TYPE_NUMBER
              )
              type_error("Illegal type for struct member name", $2.type);
      }

; /* struct_member_name */


/* The following rules are used to parse struct literals in expressions */

opt_struct_init:
      /* empty */       { $$.length = 0; $$.list = $$.last = NULL; }
    | opt_struct_init2  possible_comma { $$ = $1; }
    | opt_struct_init2 ',' error
      {
          /* Allow the parser to resynchronize */
          $$.length = 0; $$.list = $$.last = NULL;
      }
; /* opt_struct_init */

possible_comma :
      /* empty */
    | ','
; /* possible_comma */

opt_struct_init2:
      /* empty */
      struct_init
      {
          struct_init_t * p;

          p = xalloc(sizeof(*p));
          p->next = NULL;
          p->name = $1.name;
          p->type = $1.type;
          $$.length = 1;
          $$.list = p;
          $$.last = p;
      }

    | opt_struct_init2 ',' struct_init
      {
          struct_init_t * p;

          p = xalloc(sizeof(*p));
          p->next = NULL;
          p->name = $3.name;
          p->type = $3.type;
          $$.length = $1.length + 1;
          $$.list = $1.list;
          $1.last->next = p;
          $$.last = p;
      }
; /* opt_struct_init2 */

struct_init:
      identifier ':' expr0
      {
          $$.name = $1;
          assign_full_to_vartype(&$$.type, $3.type);
      }
    | expr0
      {
          $$.name = NULL;
          assign_full_to_vartype(&$$.type, $1.type);
      }
; /* struct_init */

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
/* Function calls and inline functions.
 */

function_call:

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
      function_name
      {
%line
          /* The generic function call by name.
           *
           * It may be an ordinary intra-object function call.
           * But, if the function is not defined, then it might be a call
           * to a simul_efun. If it is, then we make it a simul_efun or
           * even call_other(), of which the latter requires the function
           * name as argument.
           * It might even be a real efun.
           */

          ident_t *real_name;

          /* Save the (simple) state */
          $<function_call_head>$.start = CURRENT_PROGRAM_SIZE;
          $<function_call_head>$.simul_efun = -1;

          if($1.super)
          {
              if (strcmp($1.super, get_txt(STR_EFUN)) == 0)
                  $<function_call_head>$.efun_override = OVERRIDE_EFUN;
              else if (strcmp($1.super, get_txt(STR_SEFUN)) == 0)
                  $<function_call_head>$.efun_override = OVERRIDE_SEFUN;
              else
                  $<function_call_head>$.efun_override = OVERRIDE_NONE;
          }
          else
              $<function_call_head>$.efun_override = OVERRIDE_NONE;

          /* Insert the save_arg_frame instruction.
           * If it's not really needed, we'll remove it later.
           */
          {
              PREPARE_INSERT(2)
              add_f_code(F_SAVE_ARG_FRAME);
              CURRENT_PROGRAM_SIZE++;
          }

          if (argument_level+1 == sizeof(got_ellipsis)/sizeof(got_ellipsis[0]))
          {
              yyerror("Functions nested too deeply.");
              YYACCEPT;
          }
          argument_level++;
          got_ellipsis[argument_level] = MY_FALSE;

          real_name = $1.real;
            /* we rely on the fact that $1.real->type is either
             * I_TYPE_UNKNOWN or I_TYPE_GLOBAL here. All others are filtered
             * by the lexical analysis.
             */

          if (real_name->type == I_TYPE_UNKNOWN)
          {
              /* prevent freeing by exotic name clashes */
              /* also makes life easier below */
              init_global_identifier(real_name, /* bVariable: */ MY_TRUE);
              real_name->next_all = all_globals;
              all_globals = real_name;
          }
          else if ( ($1.super ? ( $<function_call_head>$.efun_override == OVERRIDE_SEFUN )
                              : ( real_name->u.global.function < 0 ))
                && real_name->u.global.sim_efun >= 0
                && !disable_sefuns)
          {
              /* It's a real simul-efun */

              $<function_call_head>$.simul_efun = real_name->u.global.sim_efun;
              /* real_name->u.global.sim_efun is >=0 (see above), so it can
               * be casted to unsigned long before comparison (SEFUN_TABLE_SIZE
               * is unsigned long) */
              if ((unsigned long)real_name->u.global.sim_efun >= SEFUN_TABLE_SIZE)
              {
                  /* The simul-efun has to be called by name:
                   * prepare the extra args for the call_other
                   */
                  PREPARE_INSERT(8)
                  string_t *p;

                  p = ref_mstring(real_name->name);
                  add_f_code(F_STRING);
                  add_short(store_prog_string(
                    ref_mstring(query_simul_efun_file_name())));
                  add_f_code(F_STRING);
                  add_short(store_prog_string(p));
                  CURRENT_PROGRAM_SIZE += 6;
              }
          }
      }

      '(' expr_list3 ')'

      {
          /* We got the arguments. Now we have to generate the
           * proper instructions to call the function.
           */
%line
          int        f = 0;             /* Function index */
          int        simul_efun;
          vartype_t *arg_types = NULL;  /* Argtypes from the program */
          int        first_arg;         /* Startindex in arg_types[] */
          Bool       ap_needed;         /* TRUE if arg frame is needed */
          Bool       has_ellipsis;      /* TRUE if '...' was used */

          has_ellipsis = got_ellipsis[argument_level];
          ap_needed = MY_FALSE;

          $$.start = $<function_call_head>2.start;
          $$.code = -1;

          if ( $4 >= 0xff )
              /* since num_arg is encoded in just one byte, and 0xff
               * is taken for SIMUL_EFUN_VARARG */
              yyerrorf("Too many arguments to function");

          do {
              /* The function processing is in a big do...while(0)
               * block so we can exit out of it prematurely and
               * still get the required arg-frame handling
               * afterwards
               */

              if ( !disable_sefuns
               && (simul_efun = $<function_call_head>2.simul_efun) >= 0)
              {
                  /* SIMUL EFUN */

                  PREPARE_INSERT(6)

                  function_t *funp;

                  funp = &simul_efunp[simul_efun];

                  if (funp->num_arg != SIMUL_EFUN_VARARGS
                   && !(funp->flags & TYPE_MOD_XVARARGS))
                  {
                      if ($4 > funp->num_arg)
                          yyerrorf("Too many arguments to simul_efun %s"
                                  , get_txt(funp->name));

                      if ($4 < funp->num_arg && !has_ellipsis)
                      {
                          if (pragma_pedantic)
                              yyerrorf("Missing arguments to simul_efun %s"
                                      , get_txt(funp->name));
                          else
                          {
                              yywarnf("Missing arguments to simul_efun %s"
                                     , get_txt(funp->name));
                              ap_needed = MY_TRUE;
                          }
                      }

                  }
                  
                  if (funp->flags & TYPE_MOD_DEPRECATED)
                      yywarnf("Calling deprecated simul_efun \'%s\'",
                              get_txt(funp->name));
                      
                  if (funp->num_arg == SIMUL_EFUN_VARARGS
                   || (funp->flags & TYPE_MOD_XVARARGS)
                   || has_ellipsis)
                      ap_needed = MY_TRUE;

                  /* simul_efun is >= 0, see above) */
                  if ((unsigned long)simul_efun >= SEFUN_TABLE_SIZE)
                  {
                      /* call-other: the number of arguments will be
                       * corrected at runtime.
                       */
                      add_f_code(F_CALL_DIRECT);
                      CURRENT_PROGRAM_SIZE++;
                      ap_needed = MY_TRUE;
                  }
                  else
                  {
                      /* Direct call */

                      if (ap_needed)
                      {
                          add_f_code(F_USE_ARG_FRAME);
                          CURRENT_PROGRAM_SIZE++;
                      }
                      add_f_code(F_SIMUL_EFUN);
                      add_short(simul_efun);
                      CURRENT_PROGRAM_SIZE += 3;
                  }
                  $$.type = funp->type;
                  $$.type.typeflags &= TYPE_MOD_MASK;
              } /* if (simul-efun) */

              else if ($1.super ? ($<function_call_head>2.efun_override == OVERRIDE_NONE)
                                : (f = defined_function($1.real)) >= 0
                      )
              {
                  /* LFUN or INHERITED LFUN */

                  PREPARE_INSERT(6)

                  function_t *funp;
                  function_t  inherited_function;

                  ap_needed = MY_TRUE;

                  if ($1.super)
                  {
                      /* Inherited lfun: check its existance and call it */

                      program_t *super_prog;
                      int ix;

                      ix = insert_inherited( $1.super, $1.real->name
                                           , &super_prog, &inherited_function
                                           , $4, (bytecode_p)__PREPARE_INSERT__p
                                           );

                      if (ix < 0)
                      {
                          switch(ix) {
                          case INHERITED_NOT_FOUND:
                              yyerror("function not defined by inheritance as specified");
                              break;
                          case INHERITED_WILDCARDED_ARGS:
                              yyerror("wildcarded call to inherited function can't pass arguments");
                              break;
                          case INHERITED_WILDCARDED_NOT_FOUND:
                              ap_needed = MY_FALSE;
                              /* Not an error, but we can't do argument
                               * checks either.
                               */
                              break;
                          default:
                              fatal("Unknown return code %d from insert_inherited()\n", ix);
                              break;
                          }

                          $$.type = Type_Any;
                          break; /* Out of do..while(0) */
                      }

                      /* Find the argument types */
                      if (super_prog
                       && NULL != (arg_types = super_prog->argument_types))
                      {
                          first_arg = super_prog->type_start[ix];
                      }
                      else
                      {
                          first_arg = INDEX_START_NONE;
                      }

                      funp = &inherited_function;
                  }
                  else
                  {
                      /* Normal lfun in this program */

                      ap_needed = MY_TRUE;
                      add_f_code(F_CALL_FUNCTION);
                      add_short(f);
                      funp = FUNCTION(f);
                      arg_types = (vartype_t *)mem_block[A_ARGUMENT_TYPES].block;
                      first_arg = ARGUMENT_INDEX(f);
                      CURRENT_PROGRAM_SIZE += 3;
                  }

                  /* Verify that the function has been defined already.
                   * For inherited functions this is a no-brainer.
                   */
                  if (funp->flags & (NAME_UNDEFINED|NAME_HIDDEN))
                  {
                      if ( !(funp->flags & (NAME_PROTOTYPE|NAME_INHERITED))
                       && exact_types.typeflags )
                      {
                          yyerrorf("Function %.50s undefined", get_txt(funp->name));
                      }
                      else if ((funp->flags
                                & (NAME_UNDEFINED|NAME_PROTOTYPE|NAME_HIDDEN))
                               == NAME_HIDDEN)
                      {
                          yyerrorf("Function %.50s is private", get_txt(funp->name));
                      }
                  }
                  // warn about obsoleted functions
                  if (funp->flags & TYPE_MOD_DEPRECATED)
                      yywarnf("Calling deprecated function \'%s\'",
                              get_txt(funp->name));


                  $$.type = funp->type; /* Result type */
                  $$.type.typeflags &= TYPE_MOD_MASK;

                  /* Check number of arguments.
                   */
                  if (funp->num_arg != $4
                   && !(funp->flags & TYPE_MOD_VARARGS)
                   && (first_arg != INDEX_START_NONE)
                   && exact_types.typeflags
                   && !has_ellipsis)
                  {
                      if (funp->num_arg-1 > $4 || !(funp->flags & TYPE_MOD_XVARARGS))
                        yyerrorf("Wrong number of arguments to %.60s: "
                                 "expected %ld, got %ld"
                                , get_txt($1.real->name)
                                , (long)funp->num_arg, (long)$4);
                  }

                  /* Check the argument types.
                   */
                  if (exact_types.typeflags && first_arg != INDEX_START_NONE)
                  {
                      int i;
                      vartype_t *argp;
                      int num_arg, anum_arg;

                      if ( 0 != (num_arg = funp->num_arg) )
                      {
                          /* There are arguments to check */

                          int argno; /* Argument number for error message */

                          if (funp->flags & TYPE_MOD_XVARARGS)
                              num_arg--; /* last argument is checked separately */

                          if (num_arg > (anum_arg = $4) )
                              num_arg = anum_arg;

                          arg_types += first_arg;
                          argp = get_argument_types_start(anum_arg);

                          for (argno = 1, i = num_arg; --i >= 0; argno++)
                          {
                              fulltype_t tmp1, tmp2;

                              assign_var_to_fulltype(&tmp1, *argp);
                              tmp1.typeflags &= TYPE_MOD_RMASK;
                              assign_var_to_fulltype(&tmp2, *arg_types);
                              tmp2.typeflags &= TYPE_MOD_MASK;

                              argp++;
                              arg_types++;

                              if (!REDEFINED_TYPE(tmp1, tmp2))
                              {
                                  yyerrorf("Bad type for argument %d of %s %s",
                                    argno,
                                    get_txt(funp->name),
                                    get_two_types(tmp2, tmp1));
                              }
                          } /* for (all args) */

                          if (funp->flags & TYPE_MOD_XVARARGS)
                          {
                              fulltype_t tmp1, tmp2;
                              /* varargs argument is either a pointer type or mixed */
                              assign_var_to_fulltype(&tmp2, *arg_types);
                              tmp2.typeflags &= TYPE_MOD_MASK;
                              tmp2.typeflags &= ~TYPE_MOD_POINTER;

                              for (i = anum_arg - num_arg; --i >=0; )
                              {
                                  assign_var_to_fulltype(&tmp1, *argp);
                                  tmp1.typeflags &= TYPE_MOD_RMASK;
                                  argp++;

                                  if (!MASKED_TYPE(tmp1,tmp2))
                                  {
                                      yyerrorf("Bad type for argument %d of %s %s",
                                          anum_arg - i,
                                          get_txt(funp->name),
                                          get_two_types(tmp2, tmp1));
                                  }
                              }
                          } /* if (xvarargs) */

                      } /* if (has args) */
                  } /* if (check types) */
              } /* if (inherited lfun) */

              else if ( (f = lookup_predef($1.real)) != -1 )
              {
                  /* EFUN */

                  PREPARE_INSERT(8)

                  fulltype_t *argp;
                  int min, max, def, num_arg;
                  int f2;

                  /* Get the information from the efun table */
                  min = instrs[f].min_arg;
                  max = instrs[f].max_arg;
                  def = instrs[f].Default;
                  $$.type = instrs[f].ret_type;
                  argp = &efun_arg_types[instrs[f].arg_index];

                  /* Warn if the efun is deprecated */
                  if (pragma_warn_deprecated && instrs[f].deprecated != NULL)
                      yywarnf("%s() is deprecated: %s"
                             , instrs[f].name, instrs[f].deprecated);

                  num_arg = $4;

                  /* Check and/or complete number of arguments */
                  if (def && num_arg == min-1 && !has_ellipsis)
                  {
                      /* Default argument */
                      add_f_code(def);
                      CURRENT_PROGRAM_SIZE++;
                      max--;
                      min--;
                  }
                  else if (num_arg < min
                        && !has_ellipsis
                        && (   (f2 = proxy_efun(f, num_arg)) < 0
                            || (f = f2, MY_FALSE) )
                           )
                  {
                      /* Not enough args, and no proxy_efun to replace this */
                      yyerrorf("Too few arguments to %s", instrs[f].name);
                  }
                  else if (num_arg > max && max != -1)
                  {
                      yyerrorf("Too many arguments to %s", instrs[f].name);
                      pop_arg_stack (num_arg - max);
                      $4 -= num_arg - max; /* Don't forget this for the final pop */
                      num_arg = max;
                  }

                  /* Check the types of the arguments
                   */
                  if (max != -1 && exact_types.typeflags && num_arg)
                  {
                      int        argn;
                      vartype_t *aargp;

                      aargp = get_argument_types_start(num_arg);

                      /* Loop over all arguments and compare each given
                       * type against all allowed types in efun_arg_types()
                       */
                      for (argn = 0; argn < num_arg; argn++)
                      {
                          fulltype_t tmp1, tmp2;
                          fulltype_t *beginArgp = argp;

                          assign_var_to_fulltype(&tmp1, *aargp); aargp++;
                          tmp1.typeflags &= TYPE_MOD_MASK;
                          for (;;argp++)
                          {
                              tmp2 = *argp;
                              if ( !tmp2.typeflags )
                              {
                                  /* Possible types for this arg exhausted */
                                  efun_argument_error(argn+1, f, beginArgp
                                                     , tmp1);
                                  break;
                              }

                              /* break if types are compatible; take care to
                               * handle references correctly
                               */
                              if (equal_types(tmp1, tmp2)
                               || (IS_TYPE_STRUCT(tmp1) && IS_TYPE_STRUCT(tmp2))
                                 )
                                  break;

                              if ((tmp1.typeflags &
                                     ~(TYPE_MOD_POINTER|TYPE_MOD_REFERENCE)) ==
                                    TYPE_ANY)
                              {
                                  if (tmp1.typeflags & TYPE_MOD_POINTER & ~tmp2.typeflags)
                                  {
                                      if ((tmp2.typeflags & ~TYPE_MOD_REFERENCE) !=
                                            TYPE_ANY)
                                      {
                                          continue;
                                      }
                                  }
                                  if ( !( (tmp1.typeflags ^ tmp2.typeflags) & TYPE_MOD_REFERENCE) )
                                      break;
                              }
                              else if ((tmp2.typeflags &
                                     ~(TYPE_MOD_POINTER|TYPE_MOD_REFERENCE)) ==
                                    TYPE_ANY)
                              {
                                  if (tmp2.typeflags & TYPE_MOD_POINTER & ~tmp1.typeflags)
                                      continue;
                                  if ( !( (tmp1.typeflags ^ tmp2.typeflags) & TYPE_MOD_REFERENCE) )
                                      break;
                              }
                          } /* end for (efun_arg_types) */

                          /* Advance argp to point to the allowed argtypes
                           * of the next arg.
                           */
                          while((argp++)->typeflags) NOOP;
                      } /* for (all args) */
                  } /* if (check arguments) */

                  /* If the function takes a variable number of arguments
                   * the ap is needed and evaluated automatically.
                   * If the function takes a fixed number of arguments, but
                   * the ellipsis has been used, the ap is needed but not
                   * evaluated automatically.
                   */
                  if (max != min)
                  {
                      ap_needed = MY_TRUE;
                  }
                  else if (has_ellipsis)
                  {
                      ap_needed = MY_TRUE;
                      add_byte(F_USE_ARG_FRAME);
                      CURRENT_PROGRAM_SIZE++;
                  }

                  /* Alias for an efun? */
                  if (f > LAST_INSTRUCTION_CODE)
                      f = efun_aliases[f-LAST_INSTRUCTION_CODE-1];

                  if (instrs[f].prefix)
                  {
                      /* This efun needs a prefix byte */
                      add_byte(instrs[f].prefix);
                      CURRENT_PROGRAM_SIZE++;
                  }
                  add_byte(instrs[f].opcode);
                  CURRENT_PROGRAM_SIZE++;

                  /* If the efun doesn't return a value, fake a 0.
                   * This is especially important is ap_needed, as the
                   * restore_arg_frame expects a result on the stack.
                   */
                  if ( instrs[f].ret_type.typeflags == TYPE_VOID )
                  {
                      last_expression = mem_block[A_PROGRAM].current_size;
                      add_f_code(F_CONST0);
                      CURRENT_PROGRAM_SIZE++;
                  }
              } /* efun */

              else if ($<function_call_head>2.efun_override)
              {
                  yyerrorf(($<function_call_head>2.efun_override == OVERRIDE_EFUN)
                      ? "Unknown efun: %s" : "Unknown simul-efun: %s", get_txt($1.real->name));
                  $$.type = Type_Any;
              }
              else
              {
                  /* There is no such function, but maybe it's defined later,
                   * maybe it's resolved through (cross-)inheritance.
                   * epilog() will take care of it.
                   */
                  PREPARE_INSERT(4)

                  function_t *funp;

                  f = define_new_function(MY_FALSE,
                      $1.real, 0, 0, 0, NAME_UNDEFINED, Type_Unknown
                  );
                  ap_needed = MY_TRUE;
                  add_f_code(F_CALL_FUNCTION);
                  add_short(f);
                  CURRENT_PROGRAM_SIZE += 3;
                  funp = FUNCTION(f);
                  if (exact_types.typeflags)
                  {
                      yyerrorf("Undefined function '%.50s'", get_txt($1.real->name));
                  }
                  else if (pragma_pedantic)
                  {
                      yywarnf("Undefined function '%.50s'", get_txt($1.real->name));
                  }
                  $$.type = Type_Any;  /* Just a guess */
              }
          } while (0); /* Function handling */

          /* Do the post processing of the arg frame handling */
          if (ap_needed)
          {
              /* Restore the previous arg frame pointer */

              PREPARE_INSERT(2)

              add_f_code(F_RESTORE_ARG_FRAME);
              CURRENT_PROGRAM_SIZE++;
          }
          else if (!ap_needed)
          {
              /* Since the arg frame is not needed, remove the
               * earlier save_arg_frame instruction.
               */

              bytecode_p src, dest;
              size_t left;

              dest = PROGRAM_BLOCK + $<function_call_head>2.start;
              src = dest+1;
              left = CURRENT_PROGRAM_SIZE - $<function_call_head>2.start - 1;

              while (left-- > 0)
              {
                  *dest++ = *src++;
              }

              CURRENT_PROGRAM_SIZE--;

              /* If last_expression lies within the program area
               * that was moved one bytecode adjust it accordingly.
               */
              if(last_expression > $<function_call_head>2.start)
                  last_expression--;
          }

          argument_level--;

          if ($1.super)
              yfree($1.super);
          pop_arg_stack($4);   /* Argument types no longer needed */
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr4 L_ARROW call_other_name %prec L_ARROW
      {
%line
          int string_number;
          string_t *name;

          /* Save the (simple) state */
          $<function_call_head>$.start = CURRENT_PROGRAM_SIZE;

          /* Insert the save_arg_frame instruction.
           * If it's not really needed, we'll remove it later.
           * Putting this code block before the <expr4> in the rule
           * however yields a faulty grammar.
           */
          {
              char *p, *q;
              p_int left;

              if (!realloc_a_program(1))
              {
                  yyerrorf("Out of memory: program size %"PRIuMPINT"\n"
                          , mem_block[A_PROGRAM].current_size + 2);
                  YYACCEPT;
              }

              /* Move the generated code forward by 1 */
              p = mem_block[A_PROGRAM].block + CURRENT_PROGRAM_SIZE - 1;
              q = p + 1;
              for (left = CURRENT_PROGRAM_SIZE - $1.start
                  ; left > 0
                  ; left--, p--, q--)
                  *q = *p;

              /* p now points to program[$1.start]-1.
               * Store the instruction there.
               */
              p[1] = F_SAVE_ARG_FRAME;
              CURRENT_PROGRAM_SIZE += 1;

              /* No expression to optimize here anymore. */
              last_expression = -1;
          }

          if (argument_level+1 == sizeof(got_ellipsis)/sizeof(got_ellipsis[0]))
          {
              yyerror("Functions nested too deeply.");
              YYACCEPT;
          }
          argument_level++;
          got_ellipsis[argument_level] = MY_FALSE;

          /* If call_other() has been replaced by a sefun, and
           * if we need to use F_CALL_DIRECT to call it, we have
           * to insert additional code before the <expr4> already parsed.
           * Putting this code block before the <expr4> in the rule
           * however yields a faulty grammar.
           */

          if (!disable_sefuns
           && call_other_sefun >= 0
           && (unsigned long)call_other_sefun >= SEFUN_TABLE_SIZE)
          {
              /* The simul-efun has to be called by name:
               * insert the extra args for the call_other
               */
              char *p, *q;
              p_int left;

              if (!realloc_a_program(6))
              {
                  yyerrorf("Out of memory: program size %"PRIuMPINT"\n"
                          , mem_block[A_PROGRAM].current_size + 2);
                  YYACCEPT;
              }

              /* Move the generated code forward by 6 */
              p = mem_block[A_PROGRAM].block + CURRENT_PROGRAM_SIZE - 1;
              q = p + 6;
              for (left = CURRENT_PROGRAM_SIZE - $1.start
                  ; left > 0
                  ; left--, p--, q--)
                  *q = *p;

              /* p now points to program[$1.start]-1.
               * Store the first two call-other args there.
               */
              p[1] = F_STRING;
              upd_short($1.start+1, store_prog_string(
                        ref_mstring(query_simul_efun_file_name())));
              p[4] = F_STRING;
              upd_short($1.start+4, store_prog_string(ref_mstring(STR_CALL_OTHER)));

              CURRENT_PROGRAM_SIZE += 6;
          }

%line
          /* If we received a string as call_other_name, it's a constant call.
           */
          name = $3;

          if (name)
          {
              /* Push the function name (the expr4 is already on the stack)
               */
              string_number = store_prog_string(name);
              if (string_number <= 0x0ff )
              {
                  ins_f_code(F_CSTRING0);
                  ins_byte(string_number);
              }
              else if ( string_number <= 0x1ff )
              {
                  ins_f_code(F_CSTRING1);
                  ins_byte(string_number);
              }
              else if ( string_number <= 0x2ff )
              {
                  ins_f_code(F_CSTRING2);
                  ins_byte(string_number);
              }
              else if ( string_number <= 0x3ff )
              {
                  ins_f_code(F_CSTRING3);
                  ins_byte(string_number);
              }
              else
              {
                  ins_f_code(F_STRING);
                  ins_short(string_number);
              }
          } /* if (name) */
          /* otherwise the name was given by an expression for which
           * the code and value have been already generated.
           */

      }

      '(' expr_list3 ')'

      {
          /* Now generate the CALL_OTHER resp. the SIMUL_EFUN instruction. */

          PREPARE_INSERT(10)
          Bool has_ellipsis;
          Bool ap_needed;

          has_ellipsis = got_ellipsis[argument_level];
          ap_needed = MY_TRUE;

          if (!disable_sefuns && call_other_sefun >= 0)
          {
              /* SIMUL EFUN */

              function_t *funp;
              int num_arg;

              num_arg = $6 + 2; /* Don't forget the obj and the fun! */

              funp = &simul_efunp[call_other_sefun];
              if (num_arg > funp->num_arg
               && !(funp->flags & TYPE_MOD_XVARARGS)
               && !has_ellipsis)
                  yyerrorf("Too many arguments to simul_efun %s"
                          , get_txt(funp->name));

              /* call_other_sefun is >= 0 (see above) */
              if ((unsigned long)call_other_sefun >= SEFUN_TABLE_SIZE)
              {
                  /* call-other: the number of arguments will be
                   * detected and corrected at runtime.
                   */
                  add_f_code(F_CALL_DIRECT);
                  CURRENT_PROGRAM_SIZE++;
              }
              else
              {
                  /* Direct call */
                  if (funp->num_arg != SIMUL_EFUN_VARARGS
                   && !(funp->flags & TYPE_MOD_XVARARGS)
                   && !has_ellipsis)
                  {
                      int i;

                      i = funp->num_arg - num_arg;
                      if (funp->flags & TYPE_MOD_XVARARGS)
                          i--; /* Last argument may be omitted */

                      if (i > 4)
                      {
                          if (!realloc_a_program(i+2))
                          {
                              yyerrorf("Out of memory: program size %"PRIuMPINT"\n"
                                      , mem_block[A_PROGRAM].current_size + i+2);
                              YYACCEPT;
                          }

                          __PREPARE_INSERT__p = PROGRAM_BLOCK
                                                + CURRENT_PROGRAM_SIZE;
                      }
                      CURRENT_PROGRAM_SIZE += i;
                      while ( --i >= 0 )
                      {
                          add_f_code(F_CONST0);
                      }
                  }

                  if (funp->num_arg != SIMUL_EFUN_VARARGS
                   && !(funp->flags & TYPE_MOD_XVARARGS)
                   && !has_ellipsis)
                      ap_needed = MY_FALSE;

                  if (ap_needed)
                  {
                      add_f_code(F_USE_ARG_FRAME);
                      CURRENT_PROGRAM_SIZE++;
                  }
                  add_f_code(F_SIMUL_EFUN);
                  add_short(call_other_sefun);
                  CURRENT_PROGRAM_SIZE += 3;
              }
              $$.type = funp->type;
              $$.type.typeflags &= TYPE_MOD_MASK;
          }
          else /* true call_other */
          {
              add_f_code(F_CALL_OTHER);
              CURRENT_PROGRAM_SIZE++;
              $$.type = instrs[F_CALL_OTHER].ret_type;
          }
          $$.code = -1;
          $$.start = $1.start;
          pop_arg_stack($6);
            /* No good need of these arguments because we don't
             * know what we are going to call.
             */

          /* Do the post processing of the arg frame handling */
          if (ap_needed)
          {
              /* Restore the previous arg frame pointer */

              add_f_code(F_RESTORE_ARG_FRAME);
              CURRENT_PROGRAM_SIZE++;
          }
          else
          {
              /* Since the arg frame is not needed, remove the
               * earlier save_arg_frame instruction.
               */

              bytecode_p src, dest;
              size_t left;

              dest = PROGRAM_BLOCK + $<function_call_head>4.start;
              src = dest+1;
              left = CURRENT_PROGRAM_SIZE - $<function_call_head>4.start - 1;

              while (left-- > 0)
              {
                  *dest++ = *src++;
              }

              CURRENT_PROGRAM_SIZE--;

              /* If last_expression lies within the program area
               * that was moved one bytecode adjust it accordingly.
               */
              if(last_expression > $<function_call_head>4.start)
                  last_expression--;
          }

          argument_level--;

      }

; /* function_call */


call_other_name:
      identifier
      { $$ = $1; }

    | L_STRING L_STRING
      { fatal("presence of rule should prevent its reduction"); }

    | L_STRING
      {
          $$ = last_lex_string; /* Adopt the reference */
          last_lex_string = NULL;
      }

    |  '(' expr0 ')'
      {
          $$ = NULL;
          if ($2.type.typeflags != TYPE_STRING
           && (pragma_strict_types != PRAGMA_WEAK_TYPES
               || $2.type.typeflags != TYPE_UNKNOWN)
           && $2.type.typeflags != TYPE_ANY)
              type_error("Illegal type for lfun name", $2.type);
      }

; /* call_other_name */


function_name:
      L_IDENTIFIER
      {
          $$.super = NULL;
          $$.real  = $1;
      }

    | L_LOCAL
      {
          ident_t *lvar = $1;
          ident_t *fun = find_shared_identifier_mstr(lvar->name, I_TYPE_UNKNOWN, 0);

          /* Search the inferior list for this identifier for a global
           * (function) definition.
           */

          while (fun && fun->type > I_TYPE_GLOBAL)
              fun = fun->inferior;

          if (!fun || fun->type != I_TYPE_GLOBAL)
          {
              yyerrorf("Undefined function '%.50s'\n", get_txt(lvar->name));
              YYACCEPT;
          }

          $$.super = NULL;
          $$.real  = fun;
      }

    | L_COLON_COLON L_IDENTIFIER
      {
          *($$.super = yalloc(1)) = '\0';
          $$.real  = $2;
      }

    | L_COLON_COLON L_LOCAL
      {
          ident_t *lvar = $2;

          *($$.super = yalloc(1)) = '\0';
          $$.real  = lvar;
      }

    | anchestor L_COLON_COLON L_IDENTIFIER
      {
%line
          /* Attempt to call an efun directly even though there
           * is a nomask simul-efun for it?
           */
          if ( !strcmp($1, "efun")
           && $3->type == I_TYPE_GLOBAL
           && $3->u.global.sim_efun >= 0
           && simul_efunp[$3->u.global.sim_efun].flags & TYPE_MOD_NO_MASK
           && master_ob
           && (!EVALUATION_TOO_LONG())
             )
          {
              /* Yup, check it with a privilege violation.
               * If it's denied, ignore the "efun::" qualifier.
               */

              svalue_t *res;

              push_ref_string(inter_sp, STR_NOMASK_SIMUL_EFUN);
              push_c_string(inter_sp, current_loc.file->name);
              push_ref_string(inter_sp, $3->name);
              res = apply_master(STR_PRIVILEGE, 3);
              if (!res || res->type != T_NUMBER || res->u.number < 0)
              {
                  yyerrorf("Privilege violation: nomask simul_efun %s"
                          , get_txt($3->name));
                  yfree($1);
                  $$.super = NULL;
              }
              else if (!res->u.number)
              {
                  yfree($1);
                  $$.super = NULL;
              }
              else
              {
                  $$.super = $1;
              }
          }
          else if (EVALUATION_TOO_LONG())
          {
              yyerrorf("Can't call master::%s for "
                       "'nomask simul_efun %s': eval cost too big"
                      , get_txt(STR_PRIVILEGE), get_txt($3->name));
              yfree($1);
              $$.super = NULL;
          }
          else /* the qualifier is ok */
              $$.super = $1;

          $$.real = $3; /* and don't forget the function ident */
      }

    | anchestor L_COLON_COLON L_LOCAL
      {
%line
          ident_t *lvar = $3;

          /* Attempt to call an efun directly even though there
           * is a nomask simul-efun for it?
           */
          if ( !strcmp($1, "efun")
           && lvar->type == I_TYPE_GLOBAL
           && lvar->u.global.sim_efun >= 0
           && simul_efunp[lvar->u.global.sim_efun].flags & TYPE_MOD_NO_MASK
           && master_ob
           && (!EVALUATION_TOO_LONG())
             )
          {
              /* Yup, check it with a privilege violation.
               * If it's denied, ignore the "efun::" qualifier.
               */

              svalue_t *res;

              push_ref_string(inter_sp, STR_NOMASK_SIMUL_EFUN);
              push_c_string(inter_sp, current_loc.file->name);
              push_ref_string(inter_sp, lvar->name);
              res = apply_master(STR_PRIVILEGE, 3);
              if (!res || res->type != T_NUMBER || res->u.number < 0)
              {
                  yyerrorf("Privilege violation: nomask simul_efun %s"
                          , get_txt(lvar->name));
                  yfree($1);
                  $$.super = NULL;
              }
              else if (!res->u.number)
              {
                  yfree($1);
                  $$.super = NULL;
              }
              else
              {
                  $$.super = $1;
              }
          }
          else if (EVALUATION_TOO_LONG())
          {
              yyerrorf("Can't call master::%s for "
                       "'nomask simul_efun %s': eval cost too big"
                      , get_txt(STR_PRIVILEGE), get_txt(lvar->name));
              yfree($1);
              $$.super = NULL;
          }
          else /* the qualifier is ok */
              $$.super = $1;

          $$.real = lvar; /* and don't forget the function ident */
      }

; /* function_name */


anchestor:
      L_IDENTIFIER
      {
          $$ = ystring_copy(get_txt($1->name));
      }

    | L_STRING L_STRING
      { fatal("presence of rule should prevent its reduction"); }

    | L_STRING
      {
          $$ = ystring_copy(get_txt(last_lex_string));
          free_mstring(last_lex_string);
          last_lex_string = NULL;
      }
; /* anchestor */



/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
/* The catch()-statement
 */

catch:
      L_CATCH
      {
          ins_f_code(F_SAVE_ARG_FRAME);
          $<address>$ = CURRENT_PROGRAM_SIZE;
          ins_f_code(F_CATCH);
          ins_byte(0); /* Placeholder for flags */
          ins_byte(0); /* Placeholder for the jump offset */
      }

      '(' comma_expr note_start opt_catch_mods ')'

      {
%line
          p_int origstart, start, modstart, offset;
          p_int flags = $6;

          /* Get the address of the CATCH instruction
           * and of the modifications
           */
          origstart = start = $<address>2;
          modstart = $5.start;

          /* If there were code creating modifiers, move their code
           * before the F_CATCH (currently only 'reserve' does that).
           * We need to do this before we add the END_CATCH.
           */
          if (flags & CATCH_FLAG_RESERVE)
          {
              shuffle_code(start, modstart, CURRENT_PROGRAM_SIZE);
              start += CURRENT_PROGRAM_SIZE - modstart;
          }

          ins_f_code(F_END_CATCH);

          /* Modify the instruction if necessary */
          if (flags)
          {
              bytecode_p p;
              p = PROGRAM_BLOCK + start + 1;
              *p = flags & 0xff;
          }

          /* Update the offset field of the CATCH instruction */
          offset = CURRENT_PROGRAM_SIZE - (start + 3);
          if (offset >= 0x100)
          {
              /* Too big offset, change
               *
               *      CATCH l
               *      <expr>
               *   l: END_CATCH
               *
               * to
               *
               *      CATCH l0
               *      BRANCH l1
               *  l0: LBRANCH l2
               *  l1: <expr>
               *  l2: END_CATCH
               */

              int i;
              bytecode_p p;

              if (!realloc_a_program(5))
              {
                  yyerrorf("Out of memory: program size %"PRIuMPINT"\n"
                          , CURRENT_PROGRAM_SIZE + 5);
                  YYACCEPT;
              }
              CURRENT_PROGRAM_SIZE += 5;
              p = PROGRAM_BLOCK + CURRENT_PROGRAM_SIZE - 1;
              for( i = offset; --i >= 0; --p ) *p = p[-5];
              p[-5] = 2;
              p[-4] = F_BRANCH ;
              p[-3] = 3;
              p[-2] = F_LBRANCH;
              upd_short(start + 6, offset+2);
              if (offset > 0x7ffd)
                  yyerror("offset overflow");
          }
          else
          {
              mem_block[A_PROGRAM].block[start+2] = offset;
          }

          /* Restore the argument frame */
          ins_f_code(F_RESTORE_ARG_FRAME);

          $$.start = origstart;
          $$.type  = Type_Any;
          $$.code = -1;
      }
; /* catch */


opt_catch_mods :
      ';' opt_catch_mod_list

      {
          $$ = $2;
      }

    | /* empty */

      {
         $$  = 0;
      }
; /* opt_catch_mods */


opt_catch_mod_list :
      opt_catch_mod_list ',' opt_catch_modifier
      {
          if ($1 & $3 & CATCH_FLAG_RESERVE)
          {
              /* On multiple 'reserve's, use only the first one */
              yywarnf("Multiple 'reserve' modifiers in catch()");
              insert_pop_value();
          }
          $$ = $1 | $3;
      }

    | opt_catch_modifier
      {
          $$ = $1;
      }
; /* opt_catch_mod_list */


opt_catch_modifier :
      identifier
      {
          $$ = 0;

          if (mstreq($1, STR_NOLOG))
              $$ = CATCH_FLAG_NOLOG;
          else if (mstreq($1, STR_PUBLISH))
              $$ = CATCH_FLAG_PUBLISH;
          else if (mstreq($1, STR_RESERVE))
              yyerrorf("Bad 'reserve' modifier in catch(): missing expression");
          else
              yyerrorf("Illegal modifier '%s' in catch() - "
                      "expected 'nolog', 'publish' or 'reserve <expr>'"
                      , get_txt($1)
                      );
          free_mstring($1);
      }
    | identifier expr0
      {
          $$ = 0;

          if (mstreq($1, STR_NOLOG)
           || mstreq($1, STR_PUBLISH)
             )
          {
              yyerrorf("Bad modifier '%s' in catch(): no expression allowed"
                      , get_txt($1)
                      );
          }
          else if (mstreq($1, STR_RESERVE))
          {
              if ($2.type.typeflags != TYPE_NUMBER
               && $2.type.typeflags != TYPE_UNKNOWN
               && $2.type.typeflags != TYPE_ANY
                 )
                  yyerrorf("Bad 'reserve' expression type to catch(): %s, "
                           "expected int"
                          , get_type_name($2.type)
                          );
              $$ = CATCH_FLAG_RESERVE;
          }
          else
              yyerrorf("Illegal modifier '%s' in catch() - "
                      "expected 'nolog', 'publish' or 'reserve <expr>'"
                      , get_txt($1)
                      );
          free_mstring($1);
      }
; /* opt_catch_modifier */

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
/* sscanf() and parse_command()
 *
 * Both sscanf() and parse_command() are special in that they take
 * unmarked lvalues as arguments. Parsing the lvalue arguments
 * is the biggest part of the problem.
 *
 * TODO: Make a special efun-argument type "lvalue" so that this
 * TODO:: problem can be solved generically?
 */

sscanf:
      L_SSCANF note_start '(' expr0 ',' expr0 lvalue_list ')'
      {
          ins_f_code(F_SSCANF);
          ins_byte($7 + 2);
          $$.start = $2.start;
          $$.type = Type_Number;
          $$.code = -1;
      }
; /* sscanf */

%ifdef USE_PARSE_COMMAND
parse_command:
      L_PARSE_COMMAND note_start
      '(' expr0 ',' expr0 ',' expr0 lvalue_list ')'
      {
          ins_f_code(F_PARSE_COMMAND);
          ins_byte($9 + 3);
          $$.start = $2.start;
          $$.type = Type_Number;
          $$.code = -1;
      }
; /* parse_command */
%endif /* USE_PARSE_COMMAND */


lvalue_list:
      /* empty */ { $$ = 0; }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | lvalue_list ',' L_IDENTIFIER
      {
          /* Push the lvalue for a global variable */

          int i;
          variable_t *varp;
%line
          $$ = 1 + $1;

          i = verify_declared($3);
          if (i == -1)
              /* variable not declared */
              YYACCEPT;

          if (i & VIRTUAL_VAR_TAG)
          {
              ins_f_code(F_PUSH_VIRTUAL_VARIABLE_LVALUE);
              ins_byte(i);
              varp = V_VARIABLE(i);
          }
          else
          {
              if ((i + num_virtual_variables) & ~0xff)
              {
                  ins_f_code(F_PUSH_IDENTIFIER16_LVALUE);
                  ins_short(i + num_virtual_variables);
              }
              else
              {
                  ins_f_code(F_PUSH_IDENTIFIER_LVALUE);
                  ins_byte(i + num_virtual_variables);
              }
              varp = NV_VARIABLE(i);
          }
          if (varp->type.typeflags & TYPE_MOD_DEPRECATED)
          {
              yywarnf("Using deprecated global variable %s.\n",
                      get_txt(varp->name));
          }          
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | lvalue_list ',' L_LOCAL
      {
%line
          /* Push the lvalue for a local variable */

          fulltype_t dummy;
          $3 = check_for_context_local($3, &dummy);

          if ($3->u.local.context >= 0)
          {
              ins_f_code(F_PUSH_CONTEXT_LVALUE);
              ins_byte($3->u.local.context);
          }
          else
          {
              ins_f_code(F_PUSH_LOCAL_VARIABLE_LVALUE);
              ins_byte($3->u.local.num);
          }
          $$ = 1 + $1;
      }

    | lvalue_list ',' expr4 index_expr
      {
          /* Generate a PROTECTED_(R)INDEX_LVALUE */

%line
          $$ = 1 + $1;

          if ($4.inst == F_INDEX)
              arrange_protected_lvalue($3.start, $3.code, $3.end,
                F_PROTECTED_INDEX_LVALUE
              );
          else if ($4.inst == F_RINDEX)
              arrange_protected_lvalue($3.start, $3.code, $3.end,
                F_PROTECTED_RINDEX_LVALUE
              );
          else
              arrange_protected_lvalue($3.start, $3.code, $3.end,
                F_PROTECTED_AINDEX_LVALUE
              );

          if ($4.type1.typeflags & TYPE_MOD_REFERENCE)
              yyerror("Reference used as index");

          if (exact_types.typeflags)
          {
              fulltype_t type;

              type = $3.type;
              type.typeflags &= TYPEID_MASK;
              if ( !(type.typeflags & TYPE_MOD_POINTER) )
                 switch (type.typeflags)
                 {
                 case TYPE_MAPPING:
                     if ($4.inst == F_INDEX)
                     {
                        $4.type1 = Type_Any;
                        break;
                     }
                     /* FALLTHROUGH */
                 default:
                     type_error("Bad type to indexed lvalue", type);
                     /* FALLTHROUGH */
                 case TYPE_ANY:
                     if ($4.inst == F_INDEX)
                         $4.type1 = Type_Any;
                     $4.type1 = Type_Any;
                     break;
                 case TYPE_STRING:
                     break;
                 }
                 if (!BASIC_TYPE($4.type1, Type_Number))
                     type_error("Bad type of index", $4.type1);
          }
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | lvalue_list ',' expr4 '[' expr0 ',' expr0 ']'
      {
%line
          /* Generate a PUSH_PROTECTED_INDEXED_MAP_LVALUE */

          $$ = 1 + $1;
          ins_f_code(F_PUSH_PROTECTED_INDEXED_MAP_LVALUE);
          if ($5.type.typeflags & TYPE_MOD_REFERENCE)
                yyerror("Reference used as index");

          /* Compute and check types */
          if (exact_types.typeflags)
          {
              fulltype_t type;

              type = $3.type;
              type.typeflags &= TYPEID_MASK;
              if (type.typeflags != TYPE_ANY && type.typeflags != TYPE_MAPPING)
              {
                  type_error("Bad type to indexed value", type);
              }
              type = $7.type;
              type.typeflags &= TYPEID_MASK;
              if (type.typeflags != TYPE_ANY && type.typeflags != TYPE_NUMBER)
                  type_error("Bad type of index", type);
          }
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | lvalue_list ',' expr4 index_range
      {
%line
          /* Generate/add the PROTECTED range LVALUE */

          int prot_op;

          switch ($4.inst)
          {
          case F_RANGE:    prot_op = F_PROTECTED_RANGE_LVALUE; break;
          case F_NR_RANGE: prot_op = F_PROTECTED_NR_RANGE_LVALUE; break;
          case F_RN_RANGE: prot_op = F_PROTECTED_RN_RANGE_LVALUE; break;
          case F_RR_RANGE: prot_op = F_PROTECTED_RR_RANGE_LVALUE; break;
          case F_NA_RANGE: prot_op = F_PROTECTED_NA_RANGE_LVALUE; break;
          case F_AN_RANGE: prot_op = F_PROTECTED_AN_RANGE_LVALUE; break;
          case F_RA_RANGE: prot_op = F_PROTECTED_RA_RANGE_LVALUE; break;
          case F_AR_RANGE: prot_op = F_PROTECTED_AR_RANGE_LVALUE; break;
          case F_AA_RANGE: prot_op = F_PROTECTED_AA_RANGE_LVALUE; break;
          case F_NX_RANGE: prot_op = F_PROTECTED_NX_RANGE_LVALUE; break;
          case F_RX_RANGE: prot_op = F_PROTECTED_RX_RANGE_LVALUE; break;
          case F_AX_RANGE: prot_op = F_PROTECTED_AX_RANGE_LVALUE; break;
          default:
              errorf("Unsupported range type %d %s\n"
                   , $4.inst, get_f_name($4.inst));
          }

          $$ = 1 + $1;
          arrange_protected_lvalue($3.start, $3.code, $3.end
                                  , prot_op
          );

          /* Compute and check types */
          if (exact_types.typeflags)
          {
              fulltype_t type;

              type = $3.type;
              type.typeflags &= TYPEID_MASK;
              if ((type.typeflags & TYPE_MOD_POINTER) == 0
                && type.typeflags != TYPE_ANY && type.typeflags != TYPE_STRING)
              {
                  type_error("Bad type of argument used for range", type);
              }
              type = $4.type1;
              type.typeflags &= TYPEID_MASK;
              if (type.typeflags != TYPE_ANY && type.typeflags != TYPE_NUMBER)
                  type_error("Bad type of index", type);
              type = $4.type2;
              type.typeflags &= TYPEID_MASK;
              if (type.typeflags != TYPE_ANY && type.typeflags != TYPE_NUMBER)
                  type_error("Bad type of index", type);
          }
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | lvalue_list ',' expr4 L_ARROW struct_member_name
      {
          /* Create a reference to a struct member */
          short s_index = -1;

          $$ = 1 + $1;

          if (!IS_TYPE_ANY($3.type) && !IS_TYPE_STRUCT($3.type))
          {
              yyerrorf("Bad type for struct lookup: %s"
                      , get_type_name($3.type));
          }
          else
          {
              if (IS_TYPE_STRUCT($3.type))
              {
                  s_index = get_struct_index($3.type.t_struct);
                  if (s_index == -1)
                      yyerrorf("Unknown type in lvalue struct dereference: %s\n"
                              , get_type_name($3.type)
                              );
              }

              /* At this point: s_index >= 0: $1 is of type struct
               *                         < 0: $1 is of type mixed
               */

              if ($5 != NULL)
              {
                  int num;

                  if (s_index >= 0)
                  {
                      struct_type_t * ptype = $3.type.t_struct;

                      num = struct_find_member(ptype, $5);
                      if (num < 0)
                      {
                          yyerrorf("No such member '%s' for struct '%s'"
                                  , get_txt($5)
                                  , get_txt(struct_t_name(ptype))
                                  );
                      }
                  }
                  else /* $3 is of type mixed */
                  {
                      s_index = find_struct_by_member($5, &num);
                  }

                  /* If this is a legal struct lookup, num >= 0 at this point
                   */
                  if (num >= 0)
                  {
                      ins_number(num);
                      ins_number(s_index);
                      arrange_protected_lvalue($3.start, $3.code, $3.end,
                         F_PROTECTED_INDEX_S_LVALUE
                      );
                  }
              }
              else /* Runtime lookup */
              {
                  ins_number(s_index);
                  arrange_protected_lvalue($3.start, $3.code, $3.end,
                     F_PROTECTED_INDEX_S_LVALUE
                  );
              }
          }

          if ($5 != NULL)
              free_mstring($5);
      }

; /* lvalue_list */

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

%%
%line

#ifdef __MWERKS__
#    pragma warn_possunwant reset
#    pragma warn_implicitconv reset
#endif

/*=========================================================================*/

/*-------------------------------------------------------------------------*/
static void
define_local_variable (ident_t* name, fulltype_t actual_type, typeflags_t opt_star, struct lvalue_s *lv, Bool redeclare, Bool with_init)

/* This is called directly from a parser rule: <type> [*] <name>
 * if with_init is true, then an initialization of this variable will follow.
 * if redeclare is true, then a local name is redeclared.
 * It creates the local variable and returns the corresponding lvalue
 * in lv.
 */

{
    /* redeclare:
     *    MY_FALSE: A new local variable
     *    MY_TRUE:  A local name is redeclared. If this happens
     *              on a deeper level, it is even legal.
     */

    block_scope_t *scope = block_scope + block_depth - 1;
    ident_t *q;

    actual_type.typeflags |= opt_star;

    if (current_inline && current_inline->parse_context)
    {
#ifdef DEBUG_INLINES
printf("DEBUG:   context name '%s'\n", get_txt(name->name));
#endif /* DEBUG_INLINES */

        if (redeclare && current_inline->block_depth+1 <= name->u.local.depth)
        {
            yyerrorf("Illegal to redeclare local name '%s'"
                , get_txt(name->name));
            q = name;
        }
        else
            q = add_context_name(current_inline, name, actual_type, -1);

        scope = block_scope + current_inline->block_depth;
    }
    else
    {
        if(redeclare)
            q = redeclare_local(name, actual_type, block_depth);
        else
            q = add_local_name(name, actual_type, block_depth);
    }

    if (scope->clobbered)
    {
        /* finish the previous CLEAR_LOCALS, if any */
        if (scope->num_locals - 1 > scope->num_cleared)
            mem_block[A_PROGRAM].block[scope->addr+2]
              = (char)(scope->num_locals - 1 - scope->num_cleared);
        scope->clobbered = MY_FALSE;
        scope->num_cleared = scope->num_locals - 1;
    }

    if (scope->num_locals == scope->num_cleared + 1)
    {
        /* First definition of a local, so insert the
         * clear_locals bytecode and remember its position
         */
        scope->addr = mem_block[A_PROGRAM].current_size;
        ins_f_code(F_CLEAR_LOCALS);
        ins_byte(scope->first_local + scope->num_cleared);
        ins_byte(0);
    }

    lv->u.simple[0] = F_PUSH_LOCAL_VARIABLE_LVALUE;
    lv->u.simple[1] = q->u.local.num;

    lv->length = 0;
    lv->type = actual_type;

    if (!with_init)
    {
        /* If this is a float variable, we need to insert an appropriate
         * initializer, as the default svalue-0 is not a valid float value.
         */

%line
        if (!(actual_type.typeflags & TYPE_MOD_POINTER)
         && (actual_type.typeflags & PRIMARY_TYPE_MASK) == TYPE_FLOAT
          )
        {
            ins_f_code(F_FCONST0);
            if (!add_lvalue_code(lv, F_VOID_ASSIGN))
                return;

        } /* if (float variable) */
    }
} /* define_local_variable() */

/*-------------------------------------------------------------------------*/
static void
init_local_variable ( ident_t* name, struct lvalue_s *lv, int assign_op
                    , fulltype_t type2)

/* This is called directly from a parser rule: <type> [*] <name> = <expr>
 * It will be called after the call to define_local_variable().
 * It assigns the result of <expr> to the variable.
 */

{
    /* We got a "<name> = <expr>" type declaration. */

%line

#ifdef DEBUG_INLINES
if (current_inline && current_inline->parse_context) 
  printf("DEBUG: inline context decl: name = expr, program_size %"PRIuMPINT"\n", 
         CURRENT_PROGRAM_SIZE);
#endif /* DEBUG_INLINES */

    type2.typeflags &= TYPEID_MASK;

    /* Check the assignment for validity */
    if (exact_types.typeflags && !compatible_types(lv->type, type2, MY_TRUE))
    {
        yyerrorf("Bad assignment %s", get_two_types(lv->type, type2));
    }

    if (assign_op != F_ASSIGN)
    {
        yyerror("Only plain assignments allowed here");
    }

    if (type2.typeflags & TYPE_MOD_REFERENCE)
        yyerror("Can't trace reference assignments");

    if (!add_lvalue_code(lv, F_VOID_ASSIGN))
        return;
} /* init_local_variable() */

/*-------------------------------------------------------------------------*/
static Bool
add_lvalue_code ( struct lvalue_s * lv, int instruction)

/* Add the lvalue code held in * <lv> to the end of the program.
 * If <instruction> is not zero, it is the code for an instruction
 * to be added after the lvalue code.
 * Return TRUE on success, and FALSE on failure.
 */

{
    p_int length;

    /* Create the code to push the lvalue */
    length = lv->length;
    if (length)
    {
        add_to_mem_block(A_PROGRAM, lv->u.p, length);
        yfree(lv->u.p);
        last_expression = CURRENT_PROGRAM_SIZE;
    }
    else
    {
        bytecode_p source, dest;
        mp_uint current_size;

        source = lv->u.simple;
        current_size = CURRENT_PROGRAM_SIZE;
        if (!realloc_a_program(2))
        {
            yyerrorf("Out of memory: program size %"PRIuMPINT"\n"
                    , current_size+2);
            return MY_FALSE;
        }
        CURRENT_PROGRAM_SIZE = (last_expression = current_size + 2);
        dest = PROGRAM_BLOCK + current_size;
        *dest++ = *source++;
        *dest++ = *source;
    }

    if (instruction != 0)
       ins_f_code(instruction);

    return MY_TRUE;
} /* add_lvalue_code() */

/*-------------------------------------------------------------------------*/
static void
insert_pop_value (void)

/* Remove the last value computed from the stack. If possible, use
 * last_expression to prohibit that value from being generated
 * in the first place.
 * TODO: check the sizeof() during bytecode cleanup...
 */

{
    /* We don't have to fear sideeffects and try to prevent
     * the value from being generated if the last expression is not too long
     * ago.
     */
    if (last_expression == CURRENT_PROGRAM_SIZE - sizeof(bytecode_t))
    {
         /* The following ops have no data in the bytecode. */
        switch ( mem_block[A_PROGRAM].block[last_expression])
        {
                /* The following ops have no data in the bytecode. */
            case F_ASSIGN:
                mem_block[A_PROGRAM].block[last_expression] =
                F_VOID_ASSIGN;
                break;
            case F_ADD_EQ:
                mem_block[A_PROGRAM].block[last_expression] =
                F_VOID_ADD_EQ;
                break;
            case F_PRE_INC:
            case F_POST_INC:
                mem_block[A_PROGRAM].block[last_expression] =
                F_INC;
                break;
            case F_PRE_DEC:
            case F_POST_DEC:
                mem_block[A_PROGRAM].block[last_expression] =
                F_DEC;
                break;
            case F_CONST0:
            case F_CONST1:
            case F_NCONST1:
                mem_block[A_PROGRAM].current_size = last_expression;
                break;
            default:
                ins_f_code(F_POP_VALUE);
                break;
        }
    }
    else if (last_expression == CURRENT_PROGRAM_SIZE - sizeof(bytecode_t)
                                                     -  sizeof(char))
    {
        /* The following ops are followed by 1 chars of data in the bytecode. */
        switch ( mem_block[A_PROGRAM].block[last_expression])
        {
            case F_CLIT:
            case F_NCLIT:
            case F_CSTRING0:
            case F_CSTRING1:
            case F_CSTRING2:
            case F_CSTRING3:
                mem_block[A_PROGRAM].current_size = last_expression;
                break;
            default:
                ins_f_code(F_POP_VALUE);
                break;
        }
    }
    else if (last_expression == CURRENT_PROGRAM_SIZE - sizeof(bytecode_t) 
                                                     - sizeof(short))
    {
        /* The following ops are followed by 2 chars of data in the bytecode. */
        if ( mem_block[A_PROGRAM].block[last_expression] == F_STRING)
            mem_block[A_PROGRAM].current_size = last_expression;
        else
            ins_f_code(F_POP_VALUE);            
    }
    else if (last_expression == CURRENT_PROGRAM_SIZE - sizeof(bytecode_t) 
                                                     - sizeof(p_int))
    {
        /* The following ops are followed by sizeof(p_int) chars of data in 
         * the bytecode. */
        if ( mem_block[A_PROGRAM].block[last_expression] == F_NUMBER)
            mem_block[A_PROGRAM].current_size = last_expression;
        else
            ins_f_code(F_POP_VALUE);            
    }
    else
    {
        /* last expression unknown or too long ago - just pop whatever there
         * is on the stack */
        ins_f_code(F_POP_VALUE);
    }
    
    last_expression = -1;
} /* insert_pop_value() */

/*-------------------------------------------------------------------------*/
static void
arrange_protected_lvalue (p_int start, int code, p_int end, int newcode)

/* Arrange the creation of a (protected) lvalue instead of a normal lvalue
 * or even rvalue (mostly used when passing arguments by reference).
 * The arguments mean in general:
 *   start: start address of the instruction
 *   end:   end address+1 of the instruction
 *   code:  lvalue-generating instruction alternative to the one now
 *          stored at <start>
 *   newcode: additional instruction to insert.
 *
 * The following scenarios exist:
 *
 * code >= 0 && end != 0:
 *     The multi-byte instruction in [<start>..<end>[ (which might
 *     be a complete indexing operation, but always excludes the
 *     actual instruction bytes to change) is moved to the end of
 *     the current program, then its alternative <code> and <newcode>
 *     are appended.
 *
 *     Cases are:
 *         global
 *             IDENTIFIER16 -> PUSH_IDENTIFIER16_LVALUE
 *         expr4->x
 *             S_INDEX      -> PUSH_PROTECTED_INDEXED_S_LVALUE
 *         expr4[x]
 *             INDEX        -> PUSH_PROTECTED_INDEXED_LVALUE
 *         expr4[<x]
 *             RINDEX       -> PUSH_PROTECTED_RINDEXED_LVALUE
 *         expr4[>x]
 *             AINDEX       -> PUSH_PROTECTED_AINDEXED_LVALUE
 *         expr4[x,y]
 *             MAP_INDEX    -> PUSH_PROTECTED_INDEXED_MAP_LVALUE
 *
 *     The 'global' case is special in that the code block only
 *     consists of the instruction and its 2-byte argument - all
 *     other cases are much bigger and the instruction to change
 *     is right at the end without argument.
 *
 * code >= 0 && end == 0:
 *     The instruction at <start> (1 byte code, 1 byte argument)
 *     removed (the argument byte is preserved), instead the instructions
 *     <code> plus the preserved argument byte and <newcode> are appended
 *     to the end of the current code.
 *
 *     Cases are:
 *         global:
 *             VIRTUAL_VARIABLE -> PUSH_VIRTUAL_VARIABLE_LVALUE
 *             IDENTIFIER       -> PUSH_IDENTIFIER_LVALUE
 *         local
 *             LOCAL            -> PUSH_LOCAL_LVALUE
 *
 * code < 0:
 *     The original instruction doesn't need or have an alternative,
 *     and <newcode> is a protected-index-lvalue code, for which
 *     the appropriate push-protected-index-lvalue code has
 *     to be appended to the program.
 *
 *     Cases where this code is generated:
 *         F_STRING, F_NUMBER, F_CLOSURE, F_FLOAT, F_SYMBOL,
 *         (expr0), ({ expr,... }), '({ expr,... }), ([...]),
 *         x[a..b], x[<a..b], x[a..<b], x[<a..<b], x[a..], x[<a..],
 *         &global, &local, &(expr4[x]), &(expr4[<x]), &(expr4[x,y]),
 *         &(expr4[x..y]), &(expr4[<x..y]), &(expr4[x..<y]),
 *         &(expr4[<x..<y]).
 *
 *     Cases accepted by the function:
 *         &(expr4[x]):  F_PROTECTED_INDEX_LVALUE
 *                       -> F_PUSH_PROTECTED_INDEXED_LVALUE;
 *         &(expr4[<x]): F_PROTECTED_RINDEX_LVALUE
 *                       -> F_PUSH_PROTECTED_RINDEXED_LVALUE;
 *         &(expr4->x):  F_PROTECTED_INDEX_S_LVALUE
 *                       -> F_PUSH_PROTECTED_INDEXED_S_LVALUE;
 *
 * TODO: I am surprised this works at all.
 */

{
    mp_uint current;
    bytecode_p p;

    current = CURRENT_PROGRAM_SIZE;
    if (code >= 0)
    {
        if (end)
        {
            /* Variant 1: cycle a codeblock and modify instructions */

            p_int length;
            bytecode_p q;

            length = end - start + 1;

            /* Get enough memory */
            if (!realloc_a_program(length))
            {
                yyerrorf("Out of memory: program size %"PRIuMPINT"\n"
                        , CURRENT_PROGRAM_SIZE + length);
                return;
            }

            /* Cycle the indexing code to the end, where it belongs:
             *
             *   <indexing-code> <instrs>
             * is changed via
             *   <...>           <instrs> <indexing-code>
             * to
             *   <instrs> <indexing-code>
             */
            p = PROGRAM_BLOCK;
            memcpy(p + current, p + start, length);
            p += start;
            q = p + length;
            length = current - start;
            do *p++ = *q++; while (--length);

            /* Adjust the code... */
            switch(code)
            {
            case F_PUSH_INDEXED_S_LVALUE:
                code = F_PUSH_PROTECTED_INDEXED_S_LVALUE;
                break;
            case F_PUSH_INDEXED_LVALUE:
                code = F_PUSH_PROTECTED_INDEXED_LVALUE;
                break;
            case F_PUSH_RINDEXED_LVALUE:
                code = F_PUSH_PROTECTED_RINDEXED_LVALUE;
                break;
            case F_PUSH_AINDEXED_LVALUE:
                code = F_PUSH_PROTECTED_AINDEXED_LVALUE;
                break;
            case F_PUSH_INDEXED_MAP_LVALUE:
                code = F_PUSH_PROTECTED_INDEXED_MAP_LVALUE;
                break;
            case F_PUSH_IDENTIFIER16_LVALUE:
                PUT_CODE(p-3, code);
                goto code_stored;
            default:
                fatal("Unexpected lvalue code\n");
            }

            /* ...and store it in place of the indexing instruction
             * right before p == current
             */
            PUT_CODE(p-1, instrs[code].opcode);
        code_stored:
            /* Append the newcode instruction (current will be adjusted
             * at the end of the function).
             */
            PUT_CODE(p, instrs[newcode].opcode);
        }
        else
        {
            /* Variant 2: Change
             *   <old-code> <arg> <instrs...>
             * to
             *   <instrs...> <code> <arg> <newcode>
             */

            int instr_arg;
            p_int length;

            if (!realloc_a_program(2))
            {
                yyerrorf("Out of memory: program size %"PRIuMPINT"\n"
                        , CURRENT_PROGRAM_SIZE + 2);
                return;
            }

            p = PROGRAM_BLOCK + start;
            instr_arg = p[1];
            length = current - start - 2;
            for( ; --length >= 0; p++) PUT_CODE(p, GET_CODE(p+2));
            STORE_CODE(p, code);
            STORE_CODE(p, instr_arg);
            PUT_CODE(p, instrs[newcode].opcode);
        }
    }
    else
    {
        /* Variant 3: Just add a modified <newcode> */

        switch(newcode)
        {
        case F_PROTECTED_INDEX_S_LVALUE:
            newcode = F_PUSH_PROTECTED_INDEXED_S_LVALUE;
            break;
        case F_PROTECTED_INDEX_LVALUE:
            newcode = F_PUSH_PROTECTED_INDEXED_LVALUE;
            break;
        case F_PROTECTED_RINDEX_LVALUE:
            newcode = F_PUSH_PROTECTED_RINDEXED_LVALUE;
            break;
        case F_PROTECTED_AINDEX_LVALUE:
            newcode = F_PUSH_PROTECTED_AINDEXED_LVALUE;
            break;
        default:
            yyerror("Need lvalue for range lvalue.");
        }

        if (!realloc_a_program(2))
        {
            yyerrorf("Out of memory: program size %"PRIuMPINT"\n"
                    , CURRENT_PROGRAM_SIZE + 2);
            return;
        }

        p = PROGRAM_BLOCK + current;
        PUT_CODE(p, instrs[newcode].opcode);
    }

    /* Correct the program size */
    CURRENT_PROGRAM_SIZE = current + 1;
} /* arrange_protected_lvalue() */

/*-------------------------------------------------------------------------*/
int
proxy_efun (int function, int num_arg UNUSED)

/* If the number of arguments doesn't fit the <function>, maybe there
 * is an alternative.
 * Return the code of the alternative efun, or -1 if there is none.
 */

{
#if defined(__MWERKS__)
#    pragma unused(num_arg)
#endif

    if (function == F_PREVIOUS_OBJECT)
    {
        /* num_arg == 0 */
        return F_PREVIOUS_OBJECT0;
    }

    return -1;
} /* proxy_efun() */

/*-------------------------------------------------------------------------*/
static void
transfer_init_control (void)

/* The compiler is about to generate another INIT fragment at the current
 * address: update the JUMP of the last INIT fragment to point to this
 * address.
 * If this is the first call, the function header for __INIT is generated
 * as well.
 */

{
    if (last_initializer_end < 0)
    {
        /* First call: we have to generate the __INIT function
         * header.
         */

        CURRENT_PROGRAM_SIZE = align(CURRENT_PROGRAM_SIZE);
          /* Must happen before store_function_header() */
        realloc_a_program(FUNCTION_HDR_SIZE);

        store_function_header( CURRENT_PROGRAM_SIZE
                             , STR_VARINIT
                             , 0 // actually, this is wrong and will be corrected in epilog()
                             , Type_Any
                             , 0 /* num_args */
                             , 0 /* num_vars */
                             );
        first_initializer_start =   CURRENT_PROGRAM_SIZE
                                  + FUNCTION_PRE_HDR_SIZE;
        CURRENT_PROGRAM_SIZE += FUNCTION_HDR_SIZE;
    }
    else if ((p_int)(CURRENT_PROGRAM_SIZE - sizeof(bc_offset_t)) == last_initializer_end)
    {
        /* The new INIT fragment directly follows the old one, so
         * just overwrite the JUMP instruction of the last.
         */
        mem_block[A_PROGRAM].current_size -= sizeof(bc_offset_t)+sizeof(bytecode_t);
    }
    else
    {
        /* Change the address of the last jump after the last
         * initializer to this point.
         */
        upd_jump_offset(last_initializer_end, mem_block[A_PROGRAM].current_size);
    }
} /* transfer_init_control() */

/*-------------------------------------------------------------------------*/
static void
add_new_init_jump (void)

/* The compiler just finished an INIT fragment: add a JUMP instruction
 * and let last_initializer_end point to its offset.
 */

{
    ins_f_code(F_JUMP);
    last_initializer_end = (bc_offset_t)CURRENT_PROGRAM_SIZE;
    
    if (realloc_a_program(sizeof(bc_offset_t)))
    {
        // just 'reserve' space for the offset, will be updated later.
        put_bc_offset(PROGRAM_BLOCK + last_initializer_end, 0);
        CURRENT_PROGRAM_SIZE += sizeof(bc_offset_t);
    }
    else
    {
        yyerrorf("Out of memory: program size %"PRIuMPINT"\n"
                , CURRENT_PROGRAM_SIZE + sizeof(bc_offset_t));
    }
} /* add_new_init_jump() */

/*-------------------------------------------------------------------------*/
static short
lookup_inherited (const char *super_name, string_t *real_name
                 , inherit_t **pIP, funflag_t *pFlags)

/* Lookup an inherited function <super_name>::<real_name> and return
 * it's function index, setting *pIP to the inherit_t pointer and
 * *pFlags to the function flags.
 * Return -1 if not found, *pIP set to NULL, and *pFlags set to 0.
 *
 * <super_name> can be an empty string or the (partial) name of one
 * of the inherits. <real_name> must be shared string.
 */
{
    inherit_t *ip, *foundp;
    int num_inherits, super_length;
    short found_ix;

    found_ix = -1;
    *pIP = NULL;
    *pFlags = 0;

    if (!real_name)
        return -1;

    /* Strip leading '/' */
    while (*super_name == '/')
        super_name++;
    super_length = strlen(super_name);
    num_inherits = INHERIT_COUNT;

    /* TODO: Is this really necessary?  real_name should be tabled
     * already.
     */
    {
        string_t *tmp;

        tmp = find_tabled(real_name);

#ifdef DEBUG
        if (!tmp)
            fprintf(stderr, "DEBUG: insert_inherited(): Can't find function "
                            "'%s'.\n", get_txt(real_name));
        else if (tmp != real_name)
            fprintf(stderr, "DEBUG: insert_inherited(): Function "
                            "'%s' is not a tabled string.\n"
                          , get_txt(real_name));
#endif
        if (tmp && tmp != real_name)
        {
            free_mstring(real_name);
            real_name = ref_mstring(tmp);
        }
    }

    /* Search the function in all inherits.
     * For normal inherits its sufficient to search the inherits
     * from the back in order to get the topmost definition; however,
     * with virtual inherits the order gets messed up.
     */
    ip = (inherit_t *)mem_block[A_INHERITS].block;
    for ( foundp = NULL ; num_inherits > 0 ; ip++, num_inherits--)
    {
        short i;

        if (ip->inherit_type & INHERIT_TYPE_DUPLICATE)
            /* this is a duplicate inherit */
            continue;

        /* Test if super_name matches the end of the name of the inherit. */
        if (super_length)
        {
            /* ip->prog->name includes .c */
            int l = mstrsize(ip->prog->name)-2;

            if (l < super_length)
                continue;
            if (l > super_length
             && get_txt(ip->prog->name)[l-super_length-1] != '/')
                continue;
            if (strncmp(super_name, get_txt(ip->prog->name) + l - super_length,
                        super_length) != 0)
                continue;
        }

        /* Look for the function */
        if ( (i = find_function(real_name, ip->prog)) < 0)
            continue;

        /* Found one */
        if (foundp == NULL
         || ip->inherit_depth < foundp->inherit_depth
           )
        {
            foundp = ip;
            found_ix = i;

            if (foundp->inherit_depth < 2) /* toplevel inherit */
                break;
        }
    } /* for (all inherits) */

    if (foundp != NULL)
    {
        funflag_t flags;

        /* Found it! */
        ip = foundp;

        *pFlags = flags = ip->prog->functions[found_ix];

        if (flags & NAME_INHERITED)
        {
            /* The parent inherits the function itself: we have to
             * check if it's a virtual inheritance.
             */

            inherit_t *ip2;
            program_t *prog1, *prog2;
            int numvar2;

            prog1 = ip->prog;
            ip2 = &prog1->inherit[flags & INHERIT_MASK];
            prog2 = ip2->prog;

            if ( 0 != (numvar2 = prog2->num_variables)
             && prog1->variables[ip2->variable_index_offset+numvar2-1
                                ].type.typeflags
                 & TYPE_MOD_VIRTUAL
             &&  !(prog2->variables[numvar2-1].type.typeflags & TYPE_MOD_VIRTUAL) )
            {
                /* The source was virtually inherited - we have to find
                 * the first inheritance of the program.
                 * And adjust the function index, of course.
                 */
                do --ip; while (ip->prog != prog2);
                found_ix -= ip2->function_index_offset;
            }
        }

        *pIP = ip;
    } /* if (foundp) */

    return found_ix;
} /* lookup_inherited() */

/*-------------------------------------------------------------------------*/
short
find_inherited_function ( const char * super_name
                        , const char * real_name
                        , unsigned short * pInherit
                        , funflag_t * flags
                        )

/* Lookup an inherited function <super_name>::<real_name> and return
 * it's function index as result, and the inheritance index in *<pInherit>.
 * Return -1 if not found.
 *
 * The returned function index is not adjusted for the compiled program's
 * function table.
 *
 * This function is called by the lexer to resolve #'<inherited_fun> closures,
 * and by restore_value()/restore_object() to restore closure values.
 *
 * <super_name> can be an empty string or the (partial) name of one
 * of the inherits.
 */

{
    inherit_t *ip;
    string_t *rname;
    short     ix;

    rname = find_tabled_str(real_name);

    ix =  rname ? lookup_inherited(super_name, rname, &ip, flags) : -1;
    if (ix >= 0) /* Also return the inherit index. */
        *pInherit = ip - (inherit_t *)mem_block[A_INHERITS].block;
    else
        *pInherit = 0;
    return ix;
} /* find_inherited_function() */

/*-------------------------------------------------------------------------*/
static int
insert_inherited (char *super_name, string_t *real_name
                 , program_t **super_p, function_t *fun_p
                 , int num_arg, bytecode_p __prepare_insert__p
                 )

/* The compiler encountered a <super_name>::<real_name>() call with
 * <num_arg> arguments; the codepointer is <__prepare_insert__p>.
 *
 * Look up the function information and set *<super_p> and *<fun_p>
 * the program pointer and the function_t information. Also compile
 * the function call(s).
 *
 * Result is the function index, or one of the negative error codes:
 * INHERITED_NOT_FOUND (-1): the function wasn't found.
 * INHERITED_WILDCARDED_ARGS (-2): it was a wildcarded supercall with
 *   arguments
 * INHERITED_WILDCARDED_NOT_FOUND (-3): it was a wildcarded supercall,
 *   but not a single function was found.

 * Result is -1 if the function wasn't found, -2 if it was a wildcarded
 * supercall to a function with arguments, otherwise the function index.
 *
 * <super_name> can be an empty string, the (partial) name of one
 * of the inherits, or a wildcarded name (and no args). In the latter
 * case, the function is called in all inherits matching the pattern.
 * The results from such a wildcarded call are returned in an array,
 * <super_p>, <fun_p> and the returned function index are those of
 * the first function found.
 */

{
    inherit_t *ip;
    funflag_t flags;
    short found_ix;

    found_ix = lookup_inherited(super_name, real_name, &ip, &flags);

    if (ip != NULL)
    {
        /* Found it! */
        bytecode_p __PREPARE_INSERT__p = __prepare_insert__p;

        /* Generate the function call */
        add_f_code(F_CALL_INHERITED);
        add_short(ip - (inherit_t *)mem_block[A_INHERITS].block);
        add_short(found_ix);
        CURRENT_PROGRAM_SIZE += 5;

        /* Return the program pointer */
        *super_p = ip->prog;

        /* Return a copy of the function structure */
        fun_p->flags = flags & ~INHERIT_MASK;
        get_function_information(fun_p, ip->prog, found_ix);
        fun_p->name = real_name;
        return found_ix;
    } /* if (ip) */


    /* Inherit not found, maybe it's a wildcarded call */
    if (strpbrk(super_name, "*?"))
    {
        Bool *was_called;  /* Flags which inh. fun has been called already */
        inherit_t *ip0;
        int num_inherits;
        int calls = 0;
        int ip_index;
        int first_index;
        short i;

        /* Prepare wildcard calls with arguments. */
        if (num_arg)
        {
            /* We have to put an array on the stack, behind the argument
             * frame. The size of the array we'll have to insert later,
             * as we don't know the number of calls yet.
             *
             * For each call we'll duplicate the argument frame, and enter
             * the result into that array. So the opcodes are as follows:
             *
             *   Initialize array:
             *     F_ARRAY <calls>
             *     F_MOVE_VALUE <num_arg>
             *
             *   Copy argument frame, call, enter the result:
             *     F_SAVE_ARG_FRAME
             *     F_DUP_N 1 <num_arg>
             *
             *     F_CALL_INHERITED <inh> <fx>
             *
             *     F_RESTORE_ARG_FRAME
             *     F_PUT_ARRAY_ELEMENT <num_arg+1> <num_call>
             *
             *   Repeat the step for each call.
             *
             *   Remove the arguments from the stack and move the resulting
             *   array before the argument frame.
             *     F_POP_N <num_arg>
             *     F_MOVE_VALUE 0
             *
             * As an optimization the last call doesn't need to duplicate the
             * arguments. Also the F_RESTORE_ARG_FRAME and F_SAVE_ARG_FRAME
             * between consecutive calls can be omitted.
             */

            /* 6 bytes are already reserved in the program. */
            bytecode_p __PREPARE_INSERT__p = __prepare_insert__p;

            /* Create array for the return values. */
            add_f_code(F_ARRAY0);
            add_short(0);
            /* Move array behind arguments and argument frame. */
            add_f_code(F_MOVE_VALUE);
            add_byte(num_arg);

            CURRENT_PROGRAM_SIZE += 5;
        }
        else
        {
            /* Without arguments we call with F_CALL_INHERITED_NOARGS.
             *
             * All the results are put consecutively on the stack.
             * There is special handling for F_CALL_INHERITED_NOARGS, so
             * that the results are not taken as arguments even though
             * they lie within the argument frame (above ap).
             *
             * Afterwards we call F_AGGREGATE to make the resulting
             * array of them.
             */
        }

        *super_p = NULL;
        num_inherits = INHERIT_COUNT;

        was_called = alloca(sizeof(*was_called)*num_inherits);
        for (i = 0; i < num_inherits; i++)
            was_called[i] = MY_FALSE;

        /* Test every inherit if the name matches and if
         * it does, generate the function call.
         */
        ip0 = (inherit_t *)mem_block[A_INHERITS].block;
        first_index = num_inherits > 0 ? INHERITED_WILDCARDED_NOT_FOUND
                                       : INHERITED_NOT_FOUND;
        for (; num_inherits > 0; ip0++, num_inherits--)
        {
            PREPARE_INSERT(13)

            /* ip->prog->name includes .c */
            int l = mstrsize(ip0->prog->name) - 2;

            ip = ip0; /* ip will be changed in the body */

            if (ip->inherit_type & INHERIT_TYPE_DUPLICATE)
                /* duplicate inherit */
                continue;

            if (ip->inherit_depth > 1)
                /* Only consider direct inherits, otherwise we would even
                 * call functions in sub-inherits which have been redefined.
                 */
                continue;

            if ( !match_string(super_name, get_txt(ip->prog->name), l) )
                continue;

            if ( (i = find_function(real_name, ip->prog)) < 0)
                continue;

            /* Found a match */

            flags = ip->prog->functions[i];
            if (flags & NAME_INHERITED)
            {
                /* The parent inherits the function itself: we have to
                 * check if it's a virtual inheritance.
                 */

                inherit_t *ip2;
                program_t *prog1, *prog2;
                int numvar2;

                prog1 = ip->prog;
                ip2 = &prog1->inherit[flags & INHERIT_MASK];
                prog2 = ip2->prog;

                if ( 0 != (numvar2 = prog2->num_variables)
                 &&  prog1->variables[ip2->variable_index_offset+numvar2-1
                                     ].type.typeflags
                     & TYPE_MOD_VIRTUAL
                 &&  !(prog2->variables[numvar2-1].type.typeflags & TYPE_MOD_VIRTUAL) )
                {
                    /* The function was virtually inherited - we have to find
                     * the first inheritance of that program and adjust the
                     * function index, of course.
                     */
                    do --ip; while (ip->prog != prog2);
                    i -= ip2->function_index_offset;
                } /* if (virtually inherited) */
            } /* if (inherited) */

            ip_index = ip - (inherit_t *)mem_block[A_INHERITS].block;

            /* The (new) ip might be duplicate inherit, or point to
             * a virtually inherited function we called already.
             */
            if ((ip->inherit_type & INHERIT_TYPE_DUPLICATE)
             || was_called[ip_index])
                /* duplicate inherit */
                continue;

            /* Generate the function call.
             */
            if (num_arg)
            {
                if (!calls)
                    add_f_code(F_SAVE_ARG_FRAME);
                add_f_code(F_DUP_N);
                add_byte(1);
                add_byte(num_arg);
                add_f_code(F_CALL_INHERITED);
                add_short(ip_index);
                add_short(i);
                add_f_code(F_PUT_ARRAY_ELEMENT);
                add_short(num_arg+2);
                add_byte(calls);

                CURRENT_PROGRAM_SIZE += (calls ? 12 : 13);
            }
            else
            {
                add_f_code(F_CALL_INHERITED_NOARGS);
                add_short(ip_index);
                add_short(i);
                CURRENT_PROGRAM_SIZE += 5;
            }

            /* Mark this function as called */
            was_called[ip_index] = MY_TRUE;

            if (!calls) /* First function found */
            {
                first_index = i;

                /* Return the program pointer to the caller */
                *super_p = ip->prog;

                /* Return a copy of the function structure to the caller */
                fun_p->flags = flags & ~INHERIT_MASK;
                get_function_information(fun_p, ip->prog, i);
                fun_p->name = real_name;
            }

            calls++;
        } /* for() */

        /* The calls above left their results on the stack.
         * Combine them into a single array (which might be empty).
         */
        if (!num_arg)
        {
            PREPARE_INSERT(3)
            add_f_code(F_AGGREGATE);
            add_short(calls);
            CURRENT_PROGRAM_SIZE += 3;
        }
        else
        {
            /* Backpatch number of calls. */
            put_short(__prepare_insert__p+1, calls);

            /* And now a lot of backpatching, depending on how
             * many calls we made.
             */
            if (!calls)
            {
                /* Remove our code altogether and add some to
                 * remove the arguments and put an empty array
                 * as a result on the stack.
                 */
                bytecode_p __PREPARE_INSERT__p = __prepare_insert__p;

                add_f_code(F_POP_N);
                add_byte(num_arg);
                add_f_code(F_ARRAY0);
                add_short(0);
                /* 5 bytes like the original code. */
            }
            else if (calls == 1)
            {
                /* There was only one call, remove the F_DUP_N
                 * call above and patch the F_PUT_ARRAY_ELEMENT
                 * accordingly.
                 */
                memmove(__prepare_insert__p + 5,  __prepare_insert__p + 9, 9);
                put_short(__prepare_insert__p + 11,  1);
                put_uint8(__prepare_insert__p + 14, instrs[F_MOVE_VALUE].opcode);
                put_uint8(__prepare_insert__p + 15, 0);

                CURRENT_PROGRAM_SIZE -= 2;
            }
            else
            {
                /* In the last but one call an F_RESTORE_ARG_FRAME
                 * has to be inserted. In the last call the F_DUP_N
                 * must be removed.
                 */
                bytecode_p addr = PROGRAM_BLOCK + CURRENT_PROGRAM_SIZE - 16;
                put_uint8(addr++, instrs[F_RESTORE_ARG_FRAME].opcode);
                put_uint8(addr++, instrs[F_PUT_ARRAY_ELEMENT].opcode);
                put_short(addr, num_arg+1); addr+=2;
                put_uint8(addr++, calls-2);

                memmove(addr, addr + 2, 9);
                put_short(addr + 6, 1);
                addr += 9;
                put_uint8(addr++, instrs[F_MOVE_VALUE].opcode);
                put_uint8(addr++, 0);
            }
        }

        return first_index;
    }

    /* No such function */
    return INHERITED_NOT_FOUND;
} /* insert_inherited() */

/*-------------------------------------------------------------------------*/
static void
cross_define (function_t *from, function_t *to, int32 offset)

/* The function <to> is a cross-definition from real function <from>,
 * separated by <offset>.
 * Set the flags and offset of <to> accordingly to point to <from>, and
 * synchronize the NO_MASK flag of both.
 */

{
    short nomask;
    to->flags = (to->flags & ~NAME_UNDEFINED)
              | (from->flags & (NAME_UNDEFINED|NAME_PROTOTYPE))
              | NAME_CROSS_DEFINED | NAME_HIDDEN | NAME_INHERITED;
    to->offset.func = MAKE_CROSSDEF_OFFSET(offset);
    nomask = (from->flags|to->flags) & TYPE_MOD_NO_MASK;
    from->flags |= nomask;
    to  ->flags |= nomask;
} /* cross_define() */

/*-------------------------------------------------------------------------*/
static funflag_t *
get_virtual_function_id (program_t *progp, int fx)

/* Return a pointer to the flags of the first entry of function <fx> in <progp>
 * that is inherited virtual (i.e. the first entry we encounter that doesn't have
 * TYPE_MOD_VIRTUAL).
 *
 * This function takes care of resolving cross-definitions and inherits
 * to the real function flag.
 */

{
    funflag_t flags;
    funflag_t *last;

    flags = progp->functions[fx];

    /* Handle a cross-define */
    if (flags & NAME_CROSS_DEFINED)
    {
        fx += CROSSDEF_NAME_OFFSET(flags);
        flags = progp->functions[fx];
    }
    
    /* This one is inherited virtual. We wont get called otherwise. */
    last = &progp->functions[fx];

    /* Walk the inherit chain */
    while((flags & (NAME_INHERITED|TYPE_MOD_VIRTUAL)) == (NAME_INHERITED|TYPE_MOD_VIRTUAL))
    {
        inherit_t *inheritp;

        inheritp = &progp->inherit[flags & INHERIT_MASK];
        progp = inheritp->prog;
        fx -= inheritp->function_index_offset;
        flags = progp->functions[fx];
    }

    /* This is the one */
    return &progp->functions[fx];
} /* get_virtual_function_id() */

/*-------------------------------------------------------------------------*/

static void
copy_structs (program_t *from, funflag_t flags)

/* Copy the struct definitions from program <from> which is inherited
 * with visibility <flags>.
 */

{
    int struct_id;

    for (struct_id = 0; struct_id < from->num_structs; struct_id++)
    {
        int id;
        ident_t *p;
        struct_def_t *pdef = from->struct_defs + struct_id;
        funflag_t f;

        f = flags | pdef->flags;

        /* Is this struct visible to us? */
        if (pdef->flags & (TYPE_MOD_PRIVATE|NAME_HIDDEN))
        {
            f |= NAME_HIDDEN;
        }

        /* Duplicate definition? */
        id = find_struct(struct_t_name(pdef->type));
        if (!(f & NAME_HIDDEN) && id >= 0)
        {
            /* We have a struct with this name. Check if we just
             * inherited it again, or if it's a name clash.
             */
            if (STRUCT_DEF(id).type != pdef->type)
            {
                if (STRUCT_DEF(id).inh >= 0)
                {
                    inherit_t * pInh = &INHERIT(STRUCT_DEF(id).inh);
                    yyerrorf("Different structs '%s' inherited from '%s' "
                             "and from '%s'"
                            , get_txt(struct_t_name(STRUCT_DEF(id).type))
                            , get_txt(from->name)
                            , get_txt(pInh->prog->name)
                            );
                }
                else
                    yyerrorf("struct '%s' inherited from '%s' differs "
                             "from existing struct"
                            , get_txt(struct_t_name(STRUCT_DEF(id).type))
                            , get_txt(from->name)
                            );
                continue;
            }

            f |= NAME_HIDDEN;
        }

        /* New struct */
        p = make_global_identifier(get_txt(struct_t_name(pdef->type)), I_TYPE_GLOBAL);
        if (p == NULL)
            continue;

        /* Create a new struct entry, then replace the struct prototype
         * type with the one we inherited.
         */
        current_struct = define_new_struct( MY_FALSE, p, f);
        free_struct_type(STRUCT_DEF(current_struct).type);
        STRUCT_DEF(current_struct).type = ref_struct_type(pdef->type);
        STRUCT_DEF(current_struct).inh = INHERIT_COUNT;
    }
} /* copy_structs() */

/*-------------------------------------------------------------------------*/
static int
copy_functions (program_t *from, funflag_t type)

/* The functions of the program <from> are inherited with visibility <type>.
 * Copy all the function definitions into this program, but as UNDEFINED
 * so that they can be redefined in the current program. The epilog()
 * will later update the non-redefined inherited functions and also copy
 * the types.
 *
 * An explicit call to an inherited function will not be
 * done through this entry (because this entry can be replaced by a new
 * definition). If an function defined by inheritance is called,
 * this is done with F_CALL_INHERITED
 *
 * The result is the function index of the inherited __INIT function,
 * or -1 if the inherited program doesn't have an initializer.
 */

{
    int initializer = -1;
    int i;
    uint32 first_func_index, current_func_index;
    function_t *fun_p;
    unsigned short *ixp;

    /* Make space for the inherited function structures */
    if (mem_block[A_FUNCTIONS].max_size <
        mem_block[A_FUNCTIONS].current_size +
          from->num_functions * sizeof(function_t) )
    {
        if (!realloc_mem_block(&mem_block[A_FUNCTIONS],
                          mem_block[A_FUNCTIONS].current_size +
                            from->num_functions * sizeof(function_t)))
            return 0;
    }

    /* The new functions will be stored from here */
    fun_p = (function_t *)
        (mem_block[A_FUNCTIONS].block + mem_block[A_FUNCTIONS].current_size);

    /* Copy the function definitions one by one and adjust the flags.
     * For now, we mask out the INHERIT field in the flags and
     * use NEW_INHERITED_INDEX for the value.
     */
    for (i = 0; i < from->num_functions; i++, fun_p++)
    {
        funflag_t  flags;
        int i2; /* The index of the real function */

        flags = from->functions[i];
        fun_p->offset.inherit = NEW_INHERITED_INDEX;
        i2 = i;

        if (flags & NAME_INHERITED)
        {
            /* The inherit-index has to be recomputed */
            fun_p->flags =
                (flags & ~INHERIT_MASK) | NAME_INHERITED | NAME_HIDDEN;

            /* If cross-defined, get the real function index */
            if (flags & NAME_CROSS_DEFINED)
            {
                fun_p->offset.func = flags & INHERIT_MASK;
                i2 += CROSSDEF_NAME_OFFSET(flags);
            }
        }
        else
        {
            /* Also, the function-code offset needs adjustment */
            fun_p->flags =
                (flags & ~FUNSTART_MASK) | NAME_INHERITED | NAME_HIDDEN;
        }

        /* Copy the function information */
        get_function_information(fun_p, from, i2);

    } /* for (inherited functions) pass 1 */

    /* Point back to the begin of the copied function data */
    fun_p = (function_t *)
        (mem_block[A_FUNCTIONS].block + mem_block[A_FUNCTIONS].current_size);

    /* Unhide all function for which names exist */
    ixp = from->function_names;
    for (i = from->num_function_names; --i >= 0; )
    {
        fun_p[*ixp++].flags &= ~NAME_HIDDEN;
    }

    first_func_index = current_func_index =
      mem_block[A_FUNCTIONS].current_size / sizeof (function_t);
    mem_block[A_FUNCTIONS].current_size += sizeof *fun_p * from->num_functions;

    /* Loop again over the inherited functions, checking visibility
     * and re/crossdefinition, and updating their function indices.
     * Do not call define_new_function() from here, as duplicates would
     * be removed.
     */
    for (i = 0; i < from->num_functions; i++, current_func_index++)
    {
        function_t fun;
        funflag_t new_type;
        unsigned short tmp_short;
        ident_t* p;

        fun = fun_p[i];
          /* Prepare some data to be used if this function will not be
           * redefined.
           * fun.name has already it's ref as a newly defined function in from
           */

        fun.flags |= type & TYPE_MOD_NO_MASK;

        /* Perform a lot of tests and actions for the visibility
         * and definitiability. The switch() allows us to abort
         * easily without using gotos.
         */
        switch (0) {
        default:
            /* Ignore cross defines.
             * They are the only complete invisible entries.
             */
            if (fun.flags & NAME_CROSS_DEFINED)
                break;

            /* Visible: create a new identifier for it */
            p = make_global_identifier(get_txt(fun.name), I_TYPE_GLOBAL);
            if (!p)
                break;

            if (p->type != I_TYPE_UNKNOWN)
            {
                /* We got this ident already somewhere */

                int32 n; /* existing function index */

                n = p->u.global.function;

                /* If the identifier is (also) an lfun, handle it, even if
                 * it's overloaded by something else as well. If we didn't
                 * subsequent inheritors would receive illegal function
                 * start offsets.
                 */
                if ( n >= 0)
                {
                    /* Already inherited from somewhere else.
                     * Don't try to resolve cross-references inside the
                     * currently inherited program; not only is this superflous,
                     * but it can also lead to circular cross-inheritance
                     * when there was a misplaced prototype or an explicit
                     * directive to inherit a multiply inherited function
                     * from a particular base class (the latter is not
                     * implemented). In these cases, the information that lead
                     * to the non-standard preference would be very hard to
                     * reconstruct.
                     */
                    if ((uint32)n < first_func_index)
                    {
                        /* We already have a function definition/prototype
                         * for this name.
                         */

                        function_t *OldFunction = FUNCTION(n);

                        if ( !(OldFunction->flags & NAME_INHERITED) )
                        {
                            /* Since inherits are not possible after
                             * functions have been compiled, the only
                             * way to get here is when we had a prototype
                             * for the function.
                             * It's not fatal, but annoying.
                             */
                            yywarnf(
                                "Misplaced prototype for %s in %s ignored.\n"
                                , get_txt(fun.name), current_loc.file->name
                            );
                            cross_define( &fun, OldFunction
                                        , current_func_index - n );
                            p->u.global.function = current_func_index;
                        }
                        else if ( (fun.flags & (TYPE_MOD_PRIVATE|NAME_HIDDEN|NAME_UNDEFINED))
                                    == (TYPE_MOD_PRIVATE|NAME_HIDDEN) )
                        {
                            /* There is already one function with this
                            * name. Ignore the private one, as we
                            * only need it for useful error messages.
                            */

                            break;
                        }
                        else if ( (OldFunction->flags & (TYPE_MOD_PRIVATE|NAME_HIDDEN|NAME_UNDEFINED))
                                     == (TYPE_MOD_PRIVATE|NAME_HIDDEN) )
                        {
                            /* The old one was invisible, ignore it
                             * and take this one.
                             */

                            p->u.global.function = current_func_index;
                        }
                        else if ((fun.flags | type) & TYPE_MOD_VIRTUAL
                              && OldFunction->flags & TYPE_MOD_VIRTUAL
                          &&    get_virtual_function_id(from, i)
  == get_virtual_function_id(INHERIT(OldFunction->offset.inherit).prog
                , n - INHERIT(OldFunction->offset.inherit).function_index_offset
                     )
                                 )
                        {
                            /* Entries denote the same function and both
                             * entries are visible. We have to use
                             * cross_define nonetheless, to get consistant
                             * redefinition (and to avoid the nomask
                             * checking that comes next), and we prefer
                             * the first one.
                             *
                             * It is important, that both entries are
                             * indeed visible, because otherwise invisible
                             * (i.e. private) functions would be made
                             * visible again by another visible occurrence
                             * of the same function. The originally invisible
                             * occurrence would then be subject to
                             * redefinition and nomask checking.
                             */
                            OldFunction->flags |= fun.flags &
                                (TYPE_MOD_PUBLIC|TYPE_MOD_NO_MASK);
                            OldFunction->flags &= fun.flags |
				~(TYPE_MOD_STATIC|TYPE_MOD_PRIVATE|TYPE_MOD_PROTECTED|NAME_HIDDEN);
                            cross_define( OldFunction, &fun
                                        , n - current_func_index );
                        }
                        else if ( (fun.flags & OldFunction->flags & TYPE_MOD_NO_MASK)
                             &&  !( (fun.flags|OldFunction->flags) & NAME_UNDEFINED ) )
                        {
                            yyerrorf(
                              "Illegal to inherit 'nomask' function '%s' twice",
                              get_txt(fun.name));
                        }
                        else if ((   fun.flags & TYPE_MOD_NO_MASK
                                  || OldFunction->flags & NAME_UNDEFINED )
                              && !(fun.flags & NAME_UNDEFINED)
                                )
                        {
                            /* This function is visible and existing, but the
                             * inherited one is not, or this one is also nomask:
                             * prefer this one one.
                             */
                            cross_define( &fun, OldFunction
                                        , current_func_index - n );
                            p->u.global.function = current_func_index;
                        }
                        else
                        {
                            /* At least one of the functions is visible
                             * or redefinable: prefer the first one.
                             */

                            cross_define( OldFunction, &fun
                                        , n - current_func_index );
                        }
                    } /* if (n < first_func_index) */
                    else if ( (fun.flags & (TYPE_MOD_PRIVATE|NAME_HIDDEN|NAME_UNDEFINED)) 
                                != (TYPE_MOD_PRIVATE|NAME_HIDDEN) )
                    {
                        /* This is the dominant definition in the superclass,
                         * inherit this one.
                         */
#ifdef DEBUG
                        /* The definition we picked before can't be
                         * cross-defined, because cross-defines won't
                         * be registered as global identifiers.
                         * So the previous definition should be
                         * nominally invisible so we can redefine it.
                         */
                        if ( (FUNCTION(n)->flags & (TYPE_MOD_PRIVATE|NAME_HIDDEN|NAME_UNDEFINED))
                                != (TYPE_MOD_PRIVATE|NAME_HIDDEN) )
                        {
                            fatal(
                              "Inconsistent definition of %s() within "
                              "superclass '%s'.\n"
                            , get_txt(fun.name), get_txt(from->name)
                            );
                        }
#endif
                        p->u.global.function = current_func_index;
                    }
                }

                /* Handle the non-lfun aspects of the identifier */
                {
                    if (n != I_GLOBAL_FUNCTION_OTHER
                     || (p->u.global.efun < 0 && p->u.global.sim_efun < 0)
                     || (fun.flags & (TYPE_MOD_PRIVATE|NAME_HIDDEN)) == 0
                     || (fun.flags & (NAME_UNDEFINED)) != 0
                       )
                     {
                        /* This is not an inherited private function shadowing
                         * a (simul-)efun.
                         */

                        if (p->u.global.efun >= 0 || p->u.global.sim_efun >= 0)
                        {
                            /* This inherited function shadows an efun */

                            efun_shadow_t *q;

                            q = xalloc(sizeof(efun_shadow_t));
                            if (!q) {
                                yyerrorf("Out of memory: efun shadow (%zu bytes)"
                                        , sizeof(efun_shadow_t));
                                break;
                            }
                            q->shadow = p;
                            q->next = all_efun_shadows;
                            all_efun_shadows = q;
                        }

                        /* Update the symbol table entry to point
                         * to the newly read function, unless of course
                         * the code above already took care of that change.
                         */
                        if (p->u.global.function < 0)
                            p->u.global.function = current_func_index;
                    }
                    /* else: inherited private defined function must not hide
                     * the (simul-)efun and is thusly not added to
                     * the symbol-table.
                     */
                }
            } /* if (p != I_TYPE_UNKNOWN) */

            if (p->type == I_TYPE_UNKNOWN)
            {
                /* First time this function-ident was ever encountered.
                 * Just make a new global.
                 */

                init_global_identifier(p, /* bVariable: */ MY_TRUE);
                p->u.global.function  = current_func_index;
                p->next_all = all_globals;
                all_globals = p;
            }

            /* Done with re/crossdefinition, now handle visibility.
             * Especially: public functions should not become private
             * when inherited 'private'.
             */
            new_type = type;
            if (fun.flags & TYPE_MOD_PUBLIC)
                new_type &= ~(TYPE_MOD_PRIVATE|TYPE_MOD_STATIC);
            fun.flags |= new_type;

            /* Recognize an inherited heart_beat(), making it possible
             * to mask it.
             */
            if ((heart_beat == -1)
             && mstreq(fun.name, STR_HEART_BEAT))
            {
                heart_beat = current_func_index;
            }

            /* Recognize the initializer function */
            if (mstreq(fun.name, STR_VARINIT))
            {
                initializer = i;
                fun.flags |= NAME_UNDEFINED;
            }
        } /* switch() for visibility/redefinability */

        /* Copy information about the types of the arguments, if it is
         * available.
         */
        tmp_short = INDEX_START_NONE; /* Presume not available. */
        if (from->type_start != 0)
        {
            if (from->type_start[i] != INDEX_START_NONE)
            {
                /* They are available for function number 'i'. Copy types of
                 * all arguments, and remember where they started.
                 */
                tmp_short = ARGTYPE_COUNT;
                if (fun.num_arg)
                {
                    int ix;

                    ix = ARGTYPE_COUNT;

                    add_to_mem_block(
                      A_ARGUMENT_TYPES,
                      &from->argument_types[from->type_start[i]],
                      (sizeof (vartype_t)) * fun.num_arg
                    );

                    for ( ; (size_t)ix < ARGTYPE_COUNT; ix++)
                        ref_vartype_data(&ARGUMENT_TYPE(ix));
                }

            }
        }
        else
        {
            fun.flags |= NAME_TYPES_LOST;
        }

        /* Save the index where they started. Every function will have an
         * index where the type info of arguments starts.
         */
        add_to_mem_block(A_ARGUMENT_INDEX, &tmp_short, sizeof tmp_short);

        /* Finally update the entry in the A_FUNCTIONS area */
        fun_p[i] = fun;
    } /* for (inherited functions), pass 2 */

    return initializer;
} /* copy_functions() */

/*-------------------------------------------------------------------------*/
static void
copy_variables (program_t *from, funflag_t type)

/* Inherit the variables of <from> with visibility <type>.
 * The variables are copied into our program, and it is important that
 * they are stored in the same order with the same index.
 */

{
    int i, j;
    int new_bound, last_bound;
    int variable_index_offset, fun_index_offset;
    uint inheritc;
    inherit_t *inheritp;
    int previous_variable_index_offset;
    int from_variable_index_offset;

    type &= ~VAR_INITIALIZED;

    /* If this is a virtual inherit, find the first inherit
     * for this program and set the from_variable_index_offset.
     */
    from_variable_index_offset = -1;
    if (type & TYPE_MOD_VIRTUAL)
    {
        inheritp = (inherit_t *)(mem_block[A_INHERITS].block);
        j = mem_block[A_INHERITS].current_size;
        for (; (j -= sizeof(inherit_t)) >= 0; inheritp++)
        {
            if (inheritp->prog == from
             && !(inheritp->variable_index_offset & NON_VIRTUAL_OFFSET_TAG) )
            {
                from_variable_index_offset =
                  inheritp->variable_index_offset + VIRTUAL_VAR_TAG;
                break;
            }
        }

        if (variables_initialized && from_variable_index_offset < 0)
            yyerror(
              "illegal to inherit virtually after initializing variables\n"
            );
    }

    fun_index_offset = FUNCTION_COUNT - from->num_functions;
    variable_index_offset = V_VARIABLE_COUNT;

    /* Loop through the inherits and copy the variables,
     * and also in the last run the variables of the inherited program.
     */
    last_bound = 0;  /* Last variable index handled in the previous run */
    i = from->num_inherited;
    for (inheritc = 0, inheritp = from->inherit; MY_TRUE; inheritc++, inheritp++)
    {
        if (--i >= 0)
        {
            /* It's an inherit */

            program_t *progp;

            progp = inheritp->prog;
            new_bound =
              inheritp->variable_index_offset + progp->num_variables;
              /* The end of this program's variables in the inherited
               * program <from>. This way we can compare the variables
               * original type with the type they got through inheritance.
               */

            /* Has a new virtual variable been introduced in this program?
             */
            if (progp->num_variables
             && from->variables[new_bound-1].type.typeflags & TYPE_MOD_VIRTUAL
             && !(progp->variables[progp->num_variables-1].type.typeflags
                  & TYPE_MOD_VIRTUAL)
               )
            {
                inherit_t inherit, *inheritp2;
                int k, inherit_index;
                funflag_t *flagp;
                function_t *funp;

                if (variables_initialized)
                    yyerror("illegal to inherit virtually after "
                            "initializing variables\n"
                    );
                inherit = *inheritp;
                inherit.inherit_type = INHERIT_TYPE_EXTRA;
                inherit.inherit_depth++;

                /* Find the first (virtual) inheritance of this
                 * program.
                 */
                inheritp2 = (inherit_t *)(mem_block[A_INHERITS].block);
                j = mem_block[A_INHERITS].current_size;
                for (; (j -= sizeof(inherit_t)) >= 0; inheritp2++)
                {
                    if (inheritp2->prog == inherit.prog
                     && !(inheritp2->variable_index_offset &
                          NON_VIRTUAL_OFFSET_TAG) )
                    {
                        /* Found it: copy the variable_index_offset */
                        inherit.variable_index_offset =
                          inheritp2->variable_index_offset;
                        break;
                    }
                }

                if (j < 0)
                {
                    /* First occurence of these virtual variables, we're
                     * going to copy them into our variables.
                     */
                    inheritp2 = &inherit;
                    variable_index_offset += new_bound - last_bound;
                    inherit.variable_index_offset =
                      variable_index_offset - progp->num_variables;
                }
                else
                    inherit.inherit_type |= INHERIT_TYPE_DUPLICATE;

                inherit_index = (mem_block[A_INHERITS].current_size) /
                   sizeof(inherit_t);
                inherit.function_index_offset += fun_index_offset;
                add_to_mem_block(A_INHERITS, (char *)&inherit, sizeof inherit);
                  /* If a function is directly inherited from a program that
                   * introduces a virtual variable, the code therein is not
                   * aware of virtual inheritance. For this reason, there are
                   * the extra inherit_ts with an appropriate
                   * variable_index_offset; we have to redirect inheritance
                   * to these inherit_ts.
                   */

                /* Update the offset.inherit in all these functions to point
                 * to the new inherit_t structure. (But only, if it wasn't
                 * already cross-defined to something else.)
                 */
                flagp = from->functions + inheritp->function_index_offset;
                funp = (function_t *)mem_block[A_FUNCTIONS].block +
                    inherit.function_index_offset;
                for (k = inherit.prog->num_functions; --k >= 0; funp++)
                {
                    if ( !(funp->flags & NAME_CROSS_DEFINED)
                     && (*flagp & (NAME_INHERITED|NAME_CROSS_DEFINED)) ==
                           NAME_INHERITED
                     && (*flagp & INHERIT_MASK) == inheritc )
                    {
                        funp->offset.inherit = inherit_index;
                    }
                    flagp++;
                }

                if (j >= 0)
                {
                    /* There has been another instance of this virtual
                     * superclass before: no need to check the visibility
                     * of the variables again.
                     */
                    if (new_bound > last_bound)
                        last_bound = new_bound;
                    continue;
                }
                previous_variable_index_offset = -1;
            }
            else
            {
                /* Normal, nonvirtual inherit.
                 * We wait with the visibility check until it's really
                 * useful, and then do several inherits in one go.
                 */
                continue;
            }
        }
        else
        {
            /* Handle the variables of <from>.
             * After that, we will loop once more in here, but
             * the if() below will notice that.
             * As a side effect we terminate immediately if <from>
             * had no variables on its own.
             */
            previous_variable_index_offset = from_variable_index_offset;
            new_bound = from->num_variables;
            if (new_bound == last_bound)
                break;
        }

        /* Check the visibility of the newly inspected variables
         * [last_bound..new_bound[.
         */
        for (j = last_bound; j < new_bound; j++)
        {
            ident_t *p;
            funflag_t new_type;

            p = make_global_identifier(get_txt(from->variables[j].name)
                                      , I_TYPE_GLOBAL);
            if (!p)
                return;

            new_type = type;

            /* 'public' variables should not become private when inherited
             * 'private'.
             */
            if (from->variables[j].type.typeflags & TYPE_MOD_PUBLIC)
                new_type &= ~TYPE_MOD_PRIVATE;

            /* define_variable checks for previous 'nomask' definition. */
            if (previous_variable_index_offset >= 0)
            {
                if ( !(from->variables[j].type.typeflags & TYPE_MOD_PRIVATE) )
                {
                    fulltype_t vartype = from->variables[j].type;

                    vartype.typeflags |= new_type | NAME_INHERITED;
                    redeclare_variable(p, vartype,
                                       previous_variable_index_offset + j
                    );
                }
            }
            else
            {
                fulltype_t vartype = from->variables[j].type;

                vartype.typeflags |= new_type 
                        | (from->variables[j].type.typeflags & TYPE_MOD_PRIVATE
                           ? (NAME_HIDDEN|NAME_INHERITED)
                           :  NAME_INHERITED
                          )
                ;

                define_variable(p, vartype);
            }
        } /* end loop through variables */

        last_bound = new_bound; /* Mark how far we got */

    } /* end of loop through inherits */

} /* copy_variables() */

/*-------------------------------------------------------------------------*/
static void
fix_function_inherit_indices (program_t *from)

/* All functions inherited from <from>, which haven't been resolved
 * to belong to some other inherit, are now assigned to the current
 * inherit.
 */

{
    int i, inherit_index;
    function_t *funp;

    inherit_index = INHERIT_COUNT;
    funp =
      (function_t *)
        (mem_block[A_FUNCTIONS].block+mem_block[A_FUNCTIONS].current_size) -
      from->num_functions;

    for (i = from->num_functions; --i >= 0; funp++)
    {
        if ( funp->offset.inherit == NEW_INHERITED_INDEX
         && !(funp->flags & NAME_CROSS_DEFINED) )
        {
            funp->offset.inherit = inherit_index;
        }
    }
} /* fix_function_inherit_indices() */

/*-------------------------------------------------------------------------*/
static void
fix_variable_index_offsets (program_t *new_prog)

/* Add num_virtual_variables to the index_offset of all variables
 * in <new_prog> marked with NON_VIRTUAL_OFFSET_TAG. The tag is removed.
 *
 * Reason is that the non-virtual variables have to be put after
 * the virtual variables, so the offsets of these variables are
 * first counted from 0 up and then corrected in this function after
 * the last virtual inherit.
 */

{
    int i;
    inherit_t *inheritp;

    i = new_prog->num_inherited;
    for (inheritp = new_prog->inherit; --i >= 0; inheritp++)
    {
        if (inheritp->variable_index_offset & NON_VIRTUAL_OFFSET_TAG)
        {
            inheritp->variable_index_offset += num_virtual_variables;
            inheritp->variable_index_offset &= ~NON_VIRTUAL_OFFSET_TAG;
        }
    }
} /* fix_variable_index_offsets() */

/*-------------------------------------------------------------------------*/
void
store_line_number_info (void)

{
    unsigned char c;
    short offset;

    /* Was code generated since the last call?
     * If not, return.
     */
    offset = mem_block[A_PROGRAM].current_size - stored_bytes;
    if (offset <= 0)
        return;
    stored_bytes = mem_block[A_PROGRAM].current_size;

    /* Less than 8 bytes code in 2..9 lines */
    if (offset <= 8
     && current_loc.line - stored_lines >= 2 && current_loc.line - stored_lines <= 9)
    {
        c = offset + 8*(current_loc.line - stored_lines) + 47;
          /* == (lineincr+6) << 3 | (codesize-1) */
        byte_to_mem_block(A_LINENUMBERS, c);
        stored_lines = current_loc.line;
        return;
    }

    /* Use up the excessive amounts of lines */
    stored_lines++;

    while (stored_lines > current_loc.line)
    {
        int lines;

        lines = stored_lines - current_loc.line;
        if (lines > 256)
            lines = 256;
        stored_lines -= lines;
        byte_to_mem_block(A_LINENUMBERS, LI_BACK);
        byte_to_mem_block(A_LINENUMBERS, lines-1);
    }
    
    while (stored_lines < current_loc.line)
    {
        int lines;

        lines = current_loc.line - stored_lines;
        if (lines > LI_MAXEMPTY)
            lines = LI_MAXEMPTY;
        stored_lines += lines;
        c = 256 - lines;
        byte_to_mem_block(A_LINENUMBERS, c);
    }

    while (offset >= LI_MAXOFFSET)
    {
        byte_to_mem_block(A_LINENUMBERS, LI_MAXOFFSET);
        offset -= LI_MAXOFFSET;
    }
    byte_to_mem_block(A_LINENUMBERS, offset);
} /* store_line_number_info() */

/*-------------------------------------------------------------------------*/
static void
store_line_number_relocation (int relocated_from)

/* Since the last store_line_number_info(), the compiler added a code
 * block which was compiled out of order at the earlier line <relocated_from>.
 * Add the relocation marker with the offset to <relocated_from>, call
 * store_line_number_info() for the modified linenumbers and the added
 * codeblock, then restore the current line number.
 */

{
    int save_current, offset;

    save_current = current_loc.line;
    stored_lines -= 2;
    current_loc.line = stored_lines+1;
    offset = current_loc.line - relocated_from;
    if (offset >= LI_SMALL_REL)
    {
        byte_to_mem_block(A_LINENUMBERS, LI_L_RELOCATED);
        byte_to_mem_block(A_LINENUMBERS, offset >> 8);
        byte_to_mem_block(A_LINENUMBERS, offset);
        /* trailing LI_L_RELOCATED allows bidirectional traversal */
        byte_to_mem_block(A_LINENUMBERS, LI_L_RELOCATED);
    }
    else
    {
        byte_to_mem_block(A_LINENUMBERS, LI_RELOCATED + offset);
    }
    store_line_number_info();
    current_loc.line = save_current;
} /* store_line_number_relocation() */

/*-------------------------------------------------------------------------*/
void
store_line_number_backward (int offset)

/* The current line counter is set back by <offset> lines.
 * Adapted the stored_lines counter and add the LI_BACK linenumber entry.
 */

{
    if (offset > 0)
    {
        store_line_number_info();
        stored_lines -= offset;
        while (offset > 256)
        {
            byte_to_mem_block(A_LINENUMBERS, LI_BACK);
            byte_to_mem_block(A_LINENUMBERS, 255);
            offset -= 256;
        }
        byte_to_mem_block(A_LINENUMBERS, LI_BACK);
        byte_to_mem_block(A_LINENUMBERS, offset-1);
    }
} /* store_line_number_backward() */

/*-------------------------------------------------------------------------*/
mp_uint
store_include_info (char *name, char * filename, char delim, int depth)

/* The lexer is going to include <name>, which can be the filename given
 * in an #include directive, or a descriptive name for a different source.
 * The full (file)name of the source as seen by the lexer is <filename>.
 * This will be include depth <depth>.
 * <delim> is either '"' or '>' if this include is from a file, or ')'
 * if it's a different source.
 *
 * Result is the offset of the include information in the mem_block.
 * It is to be considered a handle and has to be passed to
 * store_include_end().
 */

{
    mp_uint rc;

    /* Generate and store the plain include information */
    {
        include_t inc;
        char * tmp;
        size_t len;

        /* Make sure that the filename starts with a leading slash,
         * then make it a tabled string and store it.
         */
        if (*filename != '/')
        {
            tmp = alloca(strlen(filename)+2);
            if (tmp == NULL)
            {
                yyerror("Out of stack memory: copy of filename");
            }
            else
            {
                *tmp = '/';
                strcpy(tmp+1, filename);
                filename = tmp;
            }
        }

        inc.filename = new_tabled(filename);
        if (inc.filename == NULL)
        {
            inc.filename = ref_mstring(STR_DEFAULT);
            yyerror("Out of memory: sharing include filename");
        }

        /* Surround the <name> with the delimiters, then
         * make it a tabled string and store it.
         */
        len = strlen(name);
        tmp = alloca(len+3);
        if (tmp == NULL)
        {
            yyerror("Out of stack memory: copy of name");
        }
        else
        {
            *tmp = delim == '"' ? delim
                                : (delim == '>' ? '<' : '(');
            strcpy(tmp+1, name);
            tmp[len+1] = delim;
            tmp[len+2] = '\0';

            inc.name = new_tabled(tmp);
            if (inc.name == NULL)
            {
                inc.name = ref_mstring(STR_DEFAULT);
                yyerror("Out of memory: sharing include name");
            }
        }

        /* Complete the structure and store it */
        inc.depth = depth;
        rc = mem_block[A_INCLUDES].current_size;
        add_to_mem_block(A_INCLUDES, &inc, sizeof inc);
    }

    /* Store the information for the linenumber tracing */

    {
        if (last_include_start == mem_block[A_LINENUMBERS].current_size)
        {
            simple_includes++;
        }
        else
        {
            simple_includes = 0;
        }

        stored_lines++;  /* don't count the #include line */

        /* Use up the amounts of lines collected */
        while (stored_lines < current_loc.line)
        {
            int lines;

            lines = current_loc.line - stored_lines;
            if (lines > LI_MAXEMPTY) lines = LI_MAXEMPTY;
            stored_lines += lines;
            byte_to_mem_block(A_LINENUMBERS, 256 - lines);
        }

        /* Store the bytecode and mark the position */
        byte_to_mem_block(A_LINENUMBERS, LI_INCLUDE);
        last_include_start = mem_block[A_LINENUMBERS].current_size;

        /* Restart linecount */
        stored_lines = 0;
    }

    return rc;
} /* store_include_info() */

/*-------------------------------------------------------------------------*/
void
store_include_end (mp_uint inc_offset, int include_line)

/* The current include ended. <inc_offset> has to be the offset returned by
 * store_include_info() for this include file, <include_line> is the
 * line number of the #include statement in the including file.
 */

{
    unsigned char c;

    stored_lines = include_line;
    if (last_include_start == mem_block[A_LINENUMBERS].current_size)
    {
        include_t * inc = (include_t *)(mem_block[A_INCLUDES].block + inc_offset);
        /* No code was generated in this include - remove the
         * line number information stored by store_include_info()
         * and tag the include information in A_INCLUDES.
         */

        last_include_start = mem_block[A_LINENUMBERS].current_size - 1;
        stored_lines--;
        while (last_include_start
            && (c = mem_block[A_LINENUMBERS].block[last_include_start - 1])
               >= 0x100 - LI_MAXEMPTY)
        {
            stored_lines += c - 0x100;
            last_include_start--;
        }

        mem_block[A_LINENUMBERS].current_size = last_include_start;
        if (--simple_includes < 0)
        {
            last_include_start--;
        }

        inc->depth = -inc->depth;
    }
    else
    {
        /* Store the include end and correct the linenumber */

        byte_to_mem_block(A_LINENUMBERS, LI_INCLUDE_END);
    }
} /* store_include_end() */

/*-------------------------------------------------------------------------*/
static void
prolog (const char * fname, Bool isMasterObj)

/* Initialize the compiler environment prior to a compile.
 * <fname> is the name of the top LPC file to be compiled.
 * <isMasterObj> is TRUE if this compile is part of the compilation of
 * the master object (in which case sefuns are disabled).
 */

{
    int i;
    ident_t *id;

    /* Initialize the memory for the argument types */
    if (type_of_arguments.block == NULL)
    {
        type_of_arguments.max_size = 100;
        type_of_arguments.block = xalloc(type_of_arguments.max_size);
    }
    type_of_arguments.current_size = 0;

    /* Initialize all the globals */
    variables_defined = MY_FALSE;
    disable_sefuns   = isMasterObj;
    last_expression  = -1;
    compiled_prog    = NULL;  /* NULL means fail to load. */
    heart_beat       = -1;
    comp_stackp      = 0;     /* Local temp stack used by compiler */
    current_continue_address = 0;
    current_break_address    = 0;
    num_parse_error  = 0;
    block_depth      = 0;
    default_varmod = 0;
    default_funmod = 0;
    current_inline = NULL;
    inline_closure_id = 0;

    free_all_local_names();   /* In case of earlier error */

    /* Initialize memory blocks where the result of the compilation
     * will be stored.
     */
    for (i = 0; i < NUMAREAS; i++)
    {
        mem_block[i].block = xalloc(START_BLOCK_SIZE);
        mem_block[i].current_size = 0;
        mem_block[i].max_size = START_BLOCK_SIZE;
    }

    extend_mem_block(A_LOCAL_TYPES, MAX_LOCAL * sizeof(fulltype_t));
    memset(&LOCAL_TYPE(0), 0, LOCAL_TYPE_COUNT * sizeof(fulltype_t));

    type_of_locals = &(LOCAL_TYPE(0));
    type_of_context = type_of_locals;
#ifdef DEBUG_INLINES
printf("DEBUG: prolog: type ptrs: %p, %p\n", type_of_locals, type_of_context );
#endif /* DEBUG_INLINES */

    compiled_file = fname;
    stored_lines = 0;
    stored_bytes = 0;
    last_include_start = -1;
    memset(prog_string_tags, 0, sizeof prog_string_tags);
    num_virtual_variables = 0;
    case_state.free_block = NULL;
    case_state.next_free = NULL;
    last_initializer_end = -4; /* To pass the test in transfer_init_control() */
    variables_initialized = 0;
    argument_level = 0;
    got_ellipsis[0] = MY_FALSE;

    /* Check if call_other() has been replaced by a sefun.
     */
    call_other_sefun = -1;

    if (!disable_sefuns)
    {
        id = make_shared_identifier_mstr(STR_CALL_OTHER, I_TYPE_UNKNOWN, 0);

        if (!id)
            fatal("Out of memory: identifier '%s'.\n", get_txt(STR_CALL_OTHER));

        if (id->type != I_TYPE_UNKNOWN)
        {
            /* This shouldn't be necessary, but just in case... */
            while (id && id->type > I_TYPE_GLOBAL)
                id = id->inferior;

            if ( id
              && id->u.global.function < 0
              && id->u.global.sim_efun >= 0)
            {
                /* There is a sefun for call_other() */
                call_other_sefun = id->u.global.sim_efun;
            }
        }
    } /* if (!disable_sefuns) */

} /* prolog() */

/*-------------------------------------------------------------------------*/
static void
epilog (void)

/* The parser finished - now collect the information and generate
 * the program structure, if the parse was successful.
 */

{
    int          i;
    p_int        size;
    mp_int       num_functions;
    mp_int       num_strings;
    mp_int       num_variables;
    bytecode_p   p;
    ident_t     *g, *q;
    function_t  *f;
    function_t  *funname_start1;  /* The name chains (to sort) */
    function_t  *funname_start2;
    mp_int       num_function_names;
    program_t   *prog;

    /* First, clean up */
#ifdef DEBUG
    if (num_parse_error == 0 && type_of_arguments.current_size != 0)
        fatal("Failed to deallocate argument type stack\n");
#endif

    if (last_string_constant)

    {
        free_mstring(last_string_constant);
        last_string_constant = NULL;
    }

    free_case_blocks();

    for (i = 0; (size_t)i < STRUCT_MEMBER_COUNT; i++)
    {
        free_struct_member_data(&STRUCT_MEMBER(i));
    }
    mem_block[A_STRUCT_MEMBERS].current_size = 0;

    /* If the parse was successful, Make sure that all structs are defined and
     * reactivate old structs where possible.
     * If an error occurs, num_parse_error is incremented and epilog() will
     * bail out below.
     */
    if (!num_parse_error && !inherit_file)
    {
        struct_epilog();
    }

    /* Append the non-virtual variable block to the virtual ones,
     * and take care of the initializers.
     */
    if (V_VARIABLE_COUNT > 0x100)
    {
        yyerror("Too many virtual variables");
    }

    add_to_mem_block(
        A_VIRTUAL_VAR,
        mem_block[A_VARIABLES].block,
        mem_block[A_VARIABLES].current_size
    );
    mem_block[A_VARIABLES].current_size = 0;

    /* Define the __INIT function, but only if there was any code
     * to initialize.
     */
    if (last_initializer_end > 0)
    {
        ident_t *ip;

        ip = make_global_identifier(get_txt(STR_VARINIT), I_TYPE_UNKNOWN);
        if (ip)
        {
            int fx = define_new_function(MY_FALSE, ip, 0, 0, first_initializer_start
                                         , TYPE_MOD_PROTECTED, Type_Unknown);
            // correct the index of __INIT in the function header
            *FUNCTION_INDEXP((fun_hdr_p)(mem_block[A_PROGRAM].block + first_initializer_start)) = fx;
        }
        /* ref count for ip->name was incremented by transfer_init_control() */

        /* Change the last jump after the last initializer into a
         * return(1) statement.
         */
        mem_block[A_PROGRAM].block[last_initializer_end-1] =
            F_CONST1;
        mem_block[A_PROGRAM].block[last_initializer_end-0] =
            F_RETURN;
    } /* if (has initializer) */

    /* Check the string block. We don't have to count the include file names
     * as those won't be accessed from the program code.
     */
    if (mem_block[A_STRINGS].current_size > 0x10000 * sizeof (string_t *))
        yyerror("Too many strings");

    /* Get and check the numbers of functions, strings, and variables */
    num_functions = FUNCTION_COUNT;
    if (num_functions > 0x10000)
    {
        yyerror("Too many functions");
    }
    num_strings = STRING_COUNT;
    num_variables = V_VARIABLE_COUNT;
    if (num_variables >= VIRTUAL_VAR_TAG)
    {
        yyerror("Too many variables");
    }

    num_function_names = 0;
    if (!num_parse_error && !inherit_file)
    {
        /* If the parse was successful, fill in undefined functions,
         * resolve cross-defines, and sort the program names with mergesort.
         */

        function_t **link1, **link2;  /* Linkpointer for the sort */

        f = (function_t *)mem_block[A_FUNCTIONS].block;
        link1 = &funname_start2;
        link2 = &funname_start1;

        for (i = num_functions; --i >= 0; f++)
        {
            funflag_t flags;

            /* If the function was cross-defined, the targeted function might
             * be a cross-definition itself. Unravel such a cross-definition
             * chain and let f->offset.func point to the actual definition.
             */
            if ( f->flags & NAME_CROSS_DEFINED )
            {
                int32 offset;

                offset = GET_CROSSDEF_OFFSET(f->offset.func);
                while (f[offset].flags & NAME_CROSS_DEFINED)
                {
                    f->offset.func = offset + f[offset].offset.func;
                    offset = GET_CROSSDEF_OFFSET(f->offset.func);
                }
            }

            /* If the function is undefined, generate a dummy function
             * with UNDEF as body.
             * Except __INIT, which is created as CONST1 RETURN.
             */
            if ((f->flags & (NAME_UNDEFINED|NAME_INHERITED)) == NAME_UNDEFINED)
            {
                CURRENT_PROGRAM_SIZE = align(CURRENT_PROGRAM_SIZE);
                if (!realloc_a_program(FUNCTION_HDR_SIZE + 2))
                {
                    yyerrorf("Out of memory: program size %"PRIuMPINT"\n"
                            , CURRENT_PROGRAM_SIZE + FUNCTION_HDR_SIZE + 2);
                }
                else
                {
                    f->offset.pc = CURRENT_PROGRAM_SIZE + FUNCTION_PRE_HDR_SIZE;
                    store_function_header( CURRENT_PROGRAM_SIZE
                                         , f->name
                                         , (f - (function_t *)mem_block[A_FUNCTIONS].block)/sizeof(function_t *) // index of this function
                                         , f->type, f->num_arg
                                         , f->num_local);
                    p = PROGRAM_BLOCK + CURRENT_PROGRAM_SIZE
                        + FUNCTION_HDR_SIZE;
                    /* If __INIT() is undefined (i.e. there was a prototype, but
                     * no explicit function nor the automagic initialization code,
                     * then a dummy function is generated. This prevents crashes
                     * when this program is inherited later.
                     */
                    if (mstreq(f->name, STR_VARINIT) && !f->num_arg)
                    {
                        f->flags &= ~NAME_UNDEFINED;
                        *p++ = F_CONST1;
                        *p   = F_RETURN;
                    } else {
                        *p = F_UNDEF;
                    }
                    CURRENT_PROGRAM_SIZE += FUNCTION_HDR_SIZE + 2;
                }
            }

            /* Set the function address resp. inherit index in
             * the function's flags.
             */
            flags = f->flags;
            f->flags = flags & NAME_INHERITED ?
              (flags & ~INHERIT_MASK)  | (f->offset.inherit & INHERIT_MASK) :
              (flags & ~FUNSTART_MASK) | (f->offset.pc & FUNSTART_MASK);
            /* If the function is visible, add it to the list of names
             * to be sorted.
             */
            if ( !(flags & (NAME_HIDDEN|NAME_UNDEFINED|TYPE_MOD_PRIVATE) ) )
            {
                *link1 = f;
                link1 = link2;
                link2 = &f->offset.next;
                num_function_names++;
            }
        }

        /* End the two chains */
        *link1 = NULL;
        *link2 = NULL;

        /* Store line number info for undefined functions */
        store_line_number_info();

        /* Sort the function names */
        if (num_function_names <= 1)
        {
            /* Nothing to sort */
            funname_start1 = funname_start2;
        }
        else
        {
            /* Mergesort again.
             * TODO: Make this a standard function.
             */
            int runlength;

            runlength = 1;
            do {
                function_t *out_start1, *out_start2, **out1, **out2;
                int count1, count2;

                count1 = num_function_names & (runlength-1);
                count2 = num_function_names & runlength;
                if (!count1)
                {
                    out2 = &out_start1;
                    *out2 = funname_start2;
                    while (--count2 >= 0)
                    {
                        out2 = &(*out2)->offset.next;
                    }
                    funname_start2 = *out2;
                    count1 = count2 = runlength;
                    out1 = &out_start2;
                }
                else if (!count2)
                {
                    out2 = &out_start1;
                    *out2 = funname_start1;
                    do
                    {
                        out2 = &(*out2)->offset.next;
                    } while (--count1);
                    funname_start1 = *out2;
                    count1 = count2 = runlength;
                    out1 = &out_start2;
                }
                else
                {
                    out1 = &out_start1;
                    out2 = &out_start2;
                }

                while (funname_start1)
                {
                    while (1) {
                        /* Compare the two pointers.
                         * The comparison operation has to match the
                         * one in closure.c:function_cmp().
                         */
                        if (memcmp(
                              &funname_start2->name,
                              &funname_start1->name,
                              sizeof(char *)
                            ) < 0)
                        {
                            *out1 = funname_start2;
                            out1 = &funname_start2->offset.next;
                            funname_start2 = *out1;
                            if (!--count2)
                            {
                                *out1 = funname_start1;
                                do {
                                    out1 = &(*out1)->offset.next;
                                } while (--count1);
                                funname_start1 = *out1;
                                break;
                            }
                        }
                        else
                        {
                            *out1 = funname_start1;
                            out1 = &funname_start1->offset.next;
                            funname_start1 = *out1;
                            if (!--count1)
                            {
                                *out1 = funname_start2;
                                do {
                                    out1 = &(*out1)->offset.next;
                                } while (--count2);
                                funname_start2 = *out1;
                                break;
                            }
                        }
                    }
                    {
                        function_t **temp;

                        temp = out1;
                        out1 = out2;
                        out2 = temp;
                    }
                    count1 = count2 = runlength;
                }
                *out1 = NULL;
                *out2 = NULL;
                funname_start1 = out_start1;
                funname_start2 = out_start2;

                runlength <<= 1;
            } while (runlength < num_function_names);
        } /* end of sort */

        /* either funname_start1 or funname_start2 now has the
         * sorted list of function names.
         */

        /* If the program is too large, make sure that the
         * name strings are freed again.
         */
        if (CURRENT_PROGRAM_SIZE > FUNSTART_MASK)
        {
            function_t *functions;

            yyerror("Program too large");
            functions = (function_t *)mem_block[A_FUNCTIONS].block;
            for (i = num_functions; --i >= 0; functions++)
            {
                if ( !(functions->flags & (NAME_UNDEFINED|NAME_INHERITED)) ==
                      NAME_UNDEFINED)
                {
                    free_mstring(functions->name);
                }
            }
        }

        /* Done: functions are sorted, resolved, etc etc */
    } /* if (parse successful) */

    /* Free unneeded memory */
    free_all_local_names();

    for (q = all_globals; NULL != (g = q); )
    {
         q = g->next_all;
         free_shared_identifier(g);
    }

    while(last_yalloced)
    {
        yfree(last_yalloced);
        debug_message("%s freeing lost block\n", time_stamp());
    }

    if (all_efun_shadows)
    {
        efun_shadow_t *s, *t;

        for (t = all_efun_shadows; NULL != (s = t); )
        {
            s->shadow->u.global.function = I_GLOBAL_FUNCTION_OTHER;
            s->shadow->u.global.variable = I_GLOBAL_VARIABLE_FUN;
            t = s->next;
            xfree(s);
        }
        all_efun_shadows = NULL;
    }

    all_globals = NULL;
    
    remove_unknown_identifier();

    /* Now create the program structure */
    switch (0) { default:

#if 0
{
    int i, j;

    printf("DEBUG: --- structs in %s ---\n", current_loc.file->name);
    for (i = 0; i < STRUCT_COUNT; i++)
    {
        struct_type_t * ptype;
        ptype = STRUCT_DEF(i).type;
        printf("DEBUG: [%d] struct %s: (%s #%"PRId32") ref %"PRIdPINT
               ", %hd members, base %s, flags %"PRIx32"\n"
              , i, get_txt(ptype->name)
              , ptype->prog_name ? get_txt(ptype->prog_name) : "<none>"
              , ptype->prog_id
              , ptype->ref
              , ptype->num_members
              , ptype->base ? get_txt(ptype->base->name) : "<none>"
              , STRUCT_DEF(i).flags
              );
        fflush(stdout);
#if 1
        for (j = 0; j < ptype->num_members; j++)
        {
            fulltype_t ftype;

            assign_var_to_fulltype(&ftype, ptype->member[j].type);
            printf("DEBUG:       [%d] member %s: %s\n"
                  , j, get_txt(ptype->member[j].name)
                  , get_type_name(ftype)
                  );
            fflush(stdout);
        }
#endif
    }
    printf("DEBUG: ------\n");

}
#endif /* 0 */

        /* On error, don't create anything */
        if (num_parse_error > 0 || inherit_file)
            break;

        /* Compute the size of the program.
         * Right now, we allocate everything in one block.
         */

        size = align(sizeof (program_t));

        if (!pragma_save_types)
        {
            for (i = 0; (size_t)i < ARGTYPE_COUNT; i++)
                free_vartype_data(&ARGUMENT_TYPE(i));
            mem_block[A_ARGUMENT_TYPES].current_size = 0;
            mem_block[A_ARGUMENT_INDEX].current_size = 0;
        }
        for (i = 0; i< NUMPAREAS; i++)
        {
            if (i != A_LINENUMBERS)
                size += align(mem_block[i].current_size);
        }

        size += align(num_function_names * sizeof *prog->function_names);
        size += align(num_functions * sizeof *prog->functions);

        /* Get the program structure */
        if ( !(p = xalloc(size)) )
        {
            yyerrorf("Out of memory: program structure (%"PRIdPINT" bytes)", 
                     size);
            break;
        }

        prog = (program_t *)p;
        *prog = NULL_program;

        /* Set up the program structure */
        if ( !(prog->name = new_mstring(current_loc.file->name)) )
        {
            xfree(prog);
            yyerrorf("Out of memory: filename '%s'", current_loc.file->name);
            break;
        }
        prog->blueprint = NULL;
        prog->total_size = size;
        prog->ref = 0;
        prog->heart_beat = heart_beat;
        prog->id_number =
          ++current_id_number ? current_id_number : renumber_programs();
        prog->flags = (pragma_no_clone ? P_NO_CLONE : 0)
                    | (pragma_no_inherit ? P_NO_INHERIT : 0)
                    | (pragma_no_shadow ? P_NO_SHADOW : 0)
                    | (pragma_share_variables ? P_SHARE_VARIABLES : 0)
                    | (pragma_rtt_checks ? P_RTT_CHECKS : 0)
                    ;

        prog->load_time = current_time;

        total_prog_block_size += prog->total_size + mstrsize(prog->name);
        total_num_prog_blocks += 1;
        p += align(sizeof (program_t));

        /* Add the program code
         */
        prog->program = p;
        if (mem_block[A_PROGRAM].current_size)
            memcpy(p, mem_block[A_PROGRAM].block,
                   mem_block[A_PROGRAM].current_size);
        p += align(mem_block[A_PROGRAM].current_size);

        /* Add the function names right after the program code
         */
        prog->num_function_names = num_function_names;
        prog->function_names = (unsigned short *)p;
        {
            unsigned short *namep;

            namep = (unsigned short *)p;
            if ( NULL != (f = funname_start1) || NULL != (f = funname_start2) )
            {
                do {
                    *namep++ =
                      f - (function_t *)mem_block[A_FUNCTIONS].block;
                } while ( NULL != (f = f->offset.next) );
            }
        }
        p += align(num_function_names * sizeof *prog->function_names);

        /* Add the function flags
         */
        prog->num_functions = num_functions;
        prog->functions = (funflag_t *)p;
        {
            funflag_t *flagp;

            f = (function_t *)mem_block[A_FUNCTIONS].block;
            flagp = (funflag_t *)p;
            for (i = num_functions; --i >= 0; f++)
            {
                *flagp++ = f->flags;
            }
        }
        p += align(num_functions * sizeof *prog->functions);

        /* Add the program strings
         */
        prog->strings = (string_t **)p;
        prog->num_strings = num_strings;
        if (mem_block[A_STRINGS].current_size)
            memcpy(p, mem_block[A_STRINGS].block,
                   mem_block[A_STRINGS].current_size);

        p += align(mem_block[A_STRINGS].current_size);

        /* Add the variable descriptions
         */
        prog->variables = (variable_t *)p;
        prog->num_variables = num_variables;
        if (mem_block[A_VIRTUAL_VAR].current_size)
            memcpy(p, mem_block[A_VIRTUAL_VAR].block,
                   mem_block[A_VIRTUAL_VAR].current_size);

        p += align(mem_block[A_VIRTUAL_VAR].current_size);

        /* Add the inheritance information, and don't forget
         * to delete our internal flags.
         */
        prog->num_inherited = mem_block[A_INHERITS].current_size /
            sizeof (inherit_t);
        if (prog->num_inherited)
        {
            memcpy(p, mem_block[A_INHERITS].block,
                   mem_block[A_INHERITS].current_size);
            prog->inherit = (inherit_t *)p;
        } else {
            prog->inherit = NULL;
        }
        p += align(mem_block[A_INHERITS].current_size);

        /* Add the struct information.
         */
        prog->num_structs = STRUCT_COUNT;
        if (prog->num_structs)
        {
            memcpy(p, mem_block[A_STRUCT_DEFS].block,
                   mem_block[A_STRUCT_DEFS].current_size);
            prog->struct_defs = (struct_def_t *)p;
        } else {
            prog->struct_defs = NULL;
        }
        p += align(mem_block[A_STRUCT_DEFS].current_size);

        /* Add the include file information */
        prog->num_includes = INCLUDE_COUNT;
        if (prog->num_includes)
        {
            memcpy(p, mem_block[A_INCLUDES].block
                    , mem_block[A_INCLUDES].current_size);
            prog->includes = (include_t *)p;
        }
        else
            prog->includes = NULL;
        p += align(mem_block[A_INCLUDES].current_size);

        /* Add the argument type information
         */
        if (pragma_save_types)
        {
            if (mem_block[A_ARGUMENT_TYPES].current_size)
                memcpy(p, mem_block[A_ARGUMENT_TYPES].block,
                       mem_block[A_ARGUMENT_TYPES].current_size);
            prog->argument_types = (vartype_t *)p;
            p += align(mem_block[A_ARGUMENT_TYPES].current_size);
            if (mem_block[A_ARGUMENT_INDEX].current_size)
                memcpy(p, mem_block[A_ARGUMENT_INDEX].block,
                       mem_block[A_ARGUMENT_INDEX].current_size);
            prog->type_start = (unsigned short *)p;
            p += align(mem_block[A_ARGUMENT_INDEX].current_size);
        }
        else
        {
            prog->argument_types = NULL;
            prog->type_start = NULL;
        }

        /* Add the linenumber information.
         */
        {
            size_t linenumber_size;

            linenumber_size = mem_block[A_LINENUMBERS].current_size
                              + sizeof(linenumbers_t);

            if ( !(prog->line_numbers = xalloc(linenumber_size)) )
            {
                total_prog_block_size -= prog->total_size + mstrsize(prog->name)+1;
                total_num_prog_blocks -= 1;
                xfree(prog);
                yyerrorf("Out of memory: linenumber structure (%zu bytes)"
                        , linenumber_size);
                break;
            }
            total_prog_block_size += linenumber_size;
            prog->line_numbers->size = linenumber_size;
            if (mem_block[A_LINENUMBERS].current_size)
                memcpy( prog->line_numbers->line_numbers
                      , mem_block[A_LINENUMBERS].block
                      , mem_block[A_LINENUMBERS].current_size);
        }

        /* Correct the variable index offsets */
        fix_variable_index_offsets(prog);

        prog->swap_num = -1;

        /* Free the memareas */

        for (i = 0; (size_t)i < LOCAL_TYPE_COUNT; i++)
            free_fulltype_data(&LOCAL_TYPE(i));

        for (i = 0; (size_t)i < FUNCTION_COUNT; i++)
            free_fulltype_data(&FUNCTION(i)->type);

        for (i = 0; i < NUMAREAS; i++)
        {
            xfree(mem_block[i].block);
        }

        type_of_locals = NULL;
        type_of_context = NULL;

        /* Reference the program and all inherits, but avoid multiple
         * referencing when an object inherits more than one object
         * and one of the inherited is already loaded and not the
         * last inherited.
         */
        reference_prog(prog, "epilog");
        for (i = 0; i < prog->num_inherited; i++)
        {
            reference_prog(prog->inherit[i].prog, "inheritance");
        }

        /* Return the value */
        compiled_prog = prog;
        return;
    }

    /* If we come here, the program couldn't be created - just
     * free all memory.
     */
    {
        function_t *functions;

        /* Free all function names and type data. */
        functions = (function_t *)mem_block[A_FUNCTIONS].block;
        for (i = num_functions; --i >= 0; functions++)
        {
            if ( !(functions->flags & (NAME_INHERITED|NAME_UNDEFINED))
             && functions->name )
            {
                free_mstring(functions->name);
            }
            free_fulltype_data(&functions->type);
        }

        do_free_sub_strings( num_strings
                           , (string_t **)mem_block[A_STRINGS].block
                           , num_variables
                           , (variable_t *)mem_block[A_VIRTUAL_VAR].block
                           , INCLUDE_COUNT
                           , (include_t *)mem_block[A_INCLUDES].block
                           , STRUCT_COUNT
                           , (struct_def_t *)mem_block[A_STRUCT_DEFS].block
                           );

        /* Free the type information */
        for (i = 0; (size_t)i < ARGTYPE_COUNT; i++)
        {
            free_vartype_data(&ARGUMENT_TYPE(i));
        }

        for (i = 0; (size_t)i < LOCAL_TYPE_COUNT; i++)
        {
            free_fulltype_data(&LOCAL_TYPE(i));
        }

        compiled_prog = NULL;

        for (i = 0; i < NUMAREAS; i++)
        {
            xfree(mem_block[i].block);
        }

        type_of_locals = NULL;
        type_of_context = NULL;
        return;
    }

    /* NOTREACHED */
} /* epilog() */

/*-------------------------------------------------------------------------*/
void
compile_file (int fd, const char * fname,  Bool isMasterObj)

/* Compile an LPC file. See the head comment for instructions.
 */

{
    prolog(fname, isMasterObj);
    start_new_file(fd, fname);
    yyparse();
    /* If the parse failed, either num_parse_error != 0
     * or inherit_file != NULL here.
     */
    epilog();
    end_new_file();
} /* compile_file() */

/*-------------------------------------------------------------------------*/
Bool
is_undef_function (fun_hdr_p fun)

/* Return TRUE if <fun> points to a referenced but undefined function.
 */

{
    return GET_CODE(FUNCTION_CODE(fun)) == F_UNDEF;
} /* is_undef_function() */

/*-------------------------------------------------------------------------*/
#if defined( DEBUG ) && defined ( TRACE_CODE )

static int code_window_offset = -1;

void
set_code_window (void)

/* #pragma set_code_window: Remember the current program position.
 */

{
    code_window_offset = CURRENT_PROGRAM_SIZE;
}

void
show_code_window (void)

/* #pragma show_code_window: Print 32 bytes following the last
 * position remembered with set_code_window to stdout.
 */

{
    int i;
    bytecode_p p;

    if (code_window_offset < 0)
        return;
    p = (bytecode_p)mem_block[A_PROGRAM].block + code_window_offset;
    for (i = 0; i < 32; i++) {
        printf("%3d ", p[i]);
    }
    printf("\n");
    fflush(stdout);
} /* show_code_window() */

#endif

/*-------------------------------------------------------------------------*/
#ifdef GC_SUPPORT
void
count_compiler_refs (void)

/* GC support: mark the memory held by the compiler environment.
 */

{
    if (type_of_arguments.block)
    {
        note_malloced_block_ref(type_of_arguments.block);
    }
}

#endif

#if defined(__MWERKS__) && !defined(WARN_ALL)
#    pragma warn_possunwant off
#    pragma warn_implicitconv off
#endif

/*-------------------------------------------------------------------------*/

/***************************************************************************/
/* vim: filetype=c
 */
