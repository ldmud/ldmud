%define lr.type ielr
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
 * in contexts where rvalues are sensible as well. The approach is to
 * generate rvalues, but keep the position, and size and alternatives of the
 * instruction(s) in a struct lrvalue, so that a later change into lvalues is
 * possible.
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
#include "types.h"
#include "wiz_list.h"
#include "xalloc.h"

#include "pkg-python.h"
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
typedef struct local_variable_s    local_variable_t;
typedef struct global_variable_s   global_variable_t;

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

bool disable_sefuns;
  /* TRUE: Sefuns will be ignored.
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
    H_FILE_ENCODING:  SH(T_CLOSURE) SH(T_STRING), \
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
    fulltype_t      type;  /* Type of expression */
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

/* --- enum variable_use: Information about the usage of a variable.
 */
enum variable_usage
{
    VAR_USAGE_NONE      = 0x00, /* Variable was only declared.       */
    VAR_USAGE_WRITE     = 0x01, /* The variable was only written to. */
    VAR_USAGE_READ      = 0x02, /* The variable was only read from.  */
    VAR_USAGE_READWRITE = 0x03, /* The variable was used both ways.  */
};

/* --- struct local_variable_s: Store info about local variables ---
 *
 * This structure is used in the A_LOCAL_VARIABLES memory block to keep
 * information about the local variables, including context variables.
 */

struct local_variable_s
{
    fulltype_t          type;   /* The type of the variable.  */
    enum variable_usage usage;  /* How the variable was used. */
};

/* --- struct global_variable_s: Store info about global variables ---
 *
 * This structure is used in the A_GLOBAL_VARIABLES memory block to keep
 * information about the global variables, that will not be stored
 * permanently in the program.
 */

struct global_variable_s
{
    enum variable_usage usage;  /* How the variable was used. */
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
    (((s)->type == I_TYPE_GLOBAL && (s)->u.global.function != I_GLOBAL_FUNCTION_OTHER) ? (s)->u.global.function : -1)
  /* Return the index of the function <s> if global (and therefore existing),
   * and -1 otherwise.
   */

#define set_fulltype(t, flags, ptr) \
    ( t.t_flags = (flags), t.t_type = (ptr) )
  /* Set the fulltype <t> to type <ptr> with modifiers <flags>.
   */

#define NEW_INHERITED_INDEX (0xfffff)
  /* While inserting a new inherit, this marks the newly inherited
   * things.
   */

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
 , A_UPDATE_INDEX_MAP
    /* (unsigned short) New variable and function indices for the
     * variables and functions of an obsolete virtually inherited
     * program.
     */
 , A_ARGUMENT_TYPES
    /* (lpctype_t*) Types of the arguments of all functions with
     * typechecking. The argument types for a specific function
     * can be found using the ARGUMENT_INDEX. All entries
     * are counted references.
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

typedef bytecode_t     A_PROGRAM_t;
typedef string_t*      A_STRINGS_t;
typedef variable_t     A_VARIABLES_t;
typedef variable_t     A_VIRTUAL_VAR_t;
typedef char           A_LINENUMBERS_t;
typedef inherit_t      A_INHERITS_t;
typedef unsigned short A_UPDATE_INDEX_MAP_t;
typedef lpctype_t*     A_ARGUMENT_TYPES_t;
typedef unsigned short A_ARGUMENT_INDEX_t;
typedef include_t      A_INCLUDES_t;
typedef struct_def_t   A_STRUCT_DEFS_t;

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

 , A_LOCAL_VARIABLES
   /* (local_variable_t*) The full types of local and context variables.
    * For normal functions, only the beginning of the area is used.
    * The rest is used stack-wise for nested inline closures.
    */

 , A_GLOBAL_VARIABLES
   /* (global_variables_t*) Usage information for not-inherited (and
    * therefore not virtual) global variables.
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

 , A_LVALUE_CODE
    /* (bytecode_t): Area where to put lvalue bytecodes.
     * Used for <expr4> which compile rvalue and lvalue code simultaneously.
     * Also used for <lvalue> which doesn't put the lvalue code directly
     * into the program, because the lvalue code need to be there
     * after the code of the rhs expression.
     */

 , A_DEFAULT_VALUES
    /* (bytecode_t): Area where to put code that initializes function
     * arguments with their default values. Each area starts with
     * a short giving the length of the code block.
     */

 , A_DEFAULT_VALUES_POSITION
    /* (p_int) Position of the code for default values in A_DEFAULT_VALUES
     * for function <n>. This is only used for function prototypes.
     */

 , NUMAREAS  /* Total number of areas */
};

typedef struct inline_closure_s inline_closure_t;

typedef function_t        A_FUNCTIONS_t;
typedef int               A_STRING_NEXT_t;
typedef local_variable_t  A_LOCAL_VARIABLES_t;
typedef global_variable_t A_GLOBAL_VARIABLES_t;
typedef bytecode_t        A_INLINE_PROGRAM_t;
typedef inline_closure_t  A_INLINE_CLOSURE_t;
typedef struct_member_t   A_STRUCT_MEMBERS_t;
typedef bytecode_t        A_LVALUE_CODE_t;
typedef bytecode_t        A_DEFAULT_VALUES_t;
typedef p_int             A_DEFAULT_VALUES_POSITION_t;

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

#define GET_BLOCK(BLOCK_NAME)            ((BLOCK_NAME##_t*)(mem_block[BLOCK_NAME].block))
#define GET_BLOCK_COUNT(BLOCK_NAME)      (mem_block[BLOCK_NAME].current_size / sizeof(BLOCK_NAME##_t))
#define GET_BLOCK_SIZE(BLOCK_NAME)       (mem_block[BLOCK_NAME].current_size)

#define PROGRAM_BLOCK           GET_BLOCK(A_PROGRAM)
  /* The current program block, properly typed.
   */

#define CURRENT_PROGRAM_SIZE    GET_BLOCK_SIZE(A_PROGRAM)
  /* The current program size.
   */


#define LINENUMBER_BLOCK        GET_BLOCK(A_LINENUMBERS)
  /* The current linenumber block, properly typed.
   */

#define LINENUMBER_SIZE         GET_BLOCK_SIZE(A_LINENUMBERS)
  /* The current linenumber data size.
   */


#define FUNCTION(n)             (GET_BLOCK(A_FUNCTIONS) + (n))
  /* Return the function_t* for function number <n>.
   */

#define FUNCTION_COUNT          GET_BLOCK_COUNT(A_FUNCTIONS)
  /* Number of function_t stored so far in A_FUNCTIONS.
   */


#define INHERIT_COUNT           GET_BLOCK_COUNT(A_INHERITS)
  /* Number of inherit_t stored so far in A_INHERITS.
   */

#define UPDATE_INDEX_MAP_COUNT  GET_BLOCK_COUNT(A_UPDATE_INDEX_MAP)
  /* Offset of the next free entry in A_INDEX_MAP.
   */

#define ARGUMENT_INDEX(n)       GET_BLOCK(A_ARGUMENT_INDEX)[n]
  /* Lookup the start index of the types for function number <n>.
   */


#define ARGTYPE_COUNT           GET_BLOCK_COUNT(A_ARGUMENT_TYPES)
  /* Number of lpctype_t* stored so far in A_ARGUMENT_TYPES.
   */

#define ARGUMENT_TYPE(n)        GET_BLOCK(A_ARGUMENT_TYPES)[n]
  /* Index the lpctype_t* <n>.
   */

#define NV_VARIABLE(n)          (GET_BLOCK(A_VARIABLES) + (n))
  /* Return the variable_t* for the non-virtual variable <n>.
   */

#define NV_VARIABLE_COUNT       GET_BLOCK_COUNT(A_VARIABLES)
#define V_VARIABLE_COUNT        GET_BLOCK_COUNT(A_VIRTUAL_VAR)
  /* Number of variables stored so var in A_VARIABLES resp. A_VIRTUAL_VAR.
   */

#define V_VARIABLE(n)           (GET_BLOCK(A_VIRTUAL_VAR) + (n) - VIRTUAL_VAR_TAG)
  /* Return the variable_t* for the virtual variable <n> (still including
   * the offset).
   */

#define VARIABLE(n)             ((n) & VIRTUAL_VAR_TAG ? V_VARIABLE(n) : NV_VARIABLE(n))
  /* Return the variable_t* for the variable <n>, virtual or not.
   */

#define INHERIT(n)              GET_BLOCK(A_INHERITS)[n]
  /* Index the inherit_t <n>.
   */

#define INHERIT_COUNT           GET_BLOCK_COUNT(A_INHERITS)
  /* Return the number of inherits encountered so far.
   */

#define STRUCT_DEF(n)           GET_BLOCK(A_STRUCT_DEFS)[n]
  /* Index the struct_def_t <n>.
   */

#define STRUCT_COUNT            GET_BLOCK_COUNT(A_STRUCT_DEFS)
  /* Return the number of structs encountered so far.
   */

#define STRUCT_MEMBER(n)        GET_BLOCK(A_STRUCT_MEMBERS)[n]
  /* Index the struct_member_t <n>.
   */

#define STRUCT_MEMBER_COUNT     GET_BLOCK_COUNT(A_STRUCT_MEMBERS)
  /* Return the number of struct members stored.
   */

#define PROG_STRING(n)          GET_BLOCK(A_STRINGS)[n]
  /* Index the pointer for program string <n>.
   */

#define STRING_COUNT            GET_BLOCK_COUNT(A_STRINGS)
  /* Return the number of program strings encountered so far.
   */

#define PROG_STRING_NEXT(n)     GET_BLOCK(A_STRING_NEXT)[n]
  /* Index the chain-index for program string <n>.
   */

#define STRING_NEXT_COUNT       GET_BLOCK_COUNT(A_STRING_NEXT)
  /* Return the number of entries in A_STRING_NEXT.
   */

#define INCLUDE(n)              GET_BLOCK(A_INCLUDES)[n]
  /* Returns the include_t <n>.
   */

#define INCLUDE_COUNT           GET_BLOCK_COUNT(A_INCLUDES)
  /* Return the total number of include files encountered so far.
   */

#define LOCAL_VARIABLE_COUNT    GET_BLOCK_COUNT(A_LOCAL_VARIABLES)
  /* Return the total number of local variables.
   */

#define LOCAL_VARIABLE(n)       GET_BLOCK(A_LOCAL_VARIABLES)[n]
  /* Return the local/context var information at index <n>.
   */

#define GLOBAL_VARIABLE_COUNT    GET_BLOCK_COUNT(A_GLOBAL_VARIABLES)
  /* Return the total number of non-virtual global variables.
   */

#define GLOBAL_VARIABLE(n)       GET_BLOCK(A_GLOBAL_VARIABLES)[n]
  /* Return the global variable information at index <n>.
   */

#define INLINE_PROGRAM_BLOCK(n) (GET_BLOCK(A_INLINE_PROGRAM) + (n))
  /* Return the inline-closure program block at address <n>, properly typed.
   */

#define INLINE_PROGRAM_SIZE     GET_BLOCK_SIZE(A_INLINE_PROGRAM)
  /* The current program size.
   */

#define INLINE_CLOSURE(n)       GET_BLOCK(A_INLINE_CLOSURE)[n]
  /* Return the inline-closure program block at address <n>, properly typed.
   */

#define INLINE_CLOSURE_COUNT    GET_BLOCK_COUNT(A_INLINE_CLOSURE)
  /* Return the number of saved inline-closures.
   */

#define LVALUE_BLOCK            GET_BLOCK(A_LVALUE_CODE)
  /* The current block for lvalue code, propertly typed.
   */

#define LVALUE_BLOCK_SIZE       GET_BLOCK_SIZE(A_LVALUE_CODE)
  /* The current size of the lvalue code.
   */

#define DEFAULT_VALUES_BLOCK    GET_BLOCK(A_DEFAULT_VALUES)
  /* The current block for code for default values, propertly typed.
   */

#define DEFAULT_VALUES_BLOCK_SIZE GET_BLOCK_SIZE(A_DEFAULT_VALUES)
  /* The current size of the code for default values.
   */

#define DEFAULT_VALUES_POS(n)     GET_BLOCK(A_DEFAULT_VALUES_POSITION)[n]
  /* Lookup the start position of the default values code block
   * function number <n>.
   */

#define DEFAULT_VALUES_POS_COUNT  GET_BLOCK_SIZE(A_DEFAULT_VALUES_POSITION)
  /* The current number of functions who have a code block for
   * default values.
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
    lpctype_t * exact_types;
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
    mp_uint full_local_var_start;
    mp_uint full_context_var_start;
      /* Start indices of the local/context variable information
       * in A_LOCAL_VARIABLES.
       */
    mp_uint full_local_var_size;
      /* Current size of the A_LOCAL_VARIABLES memblocks.
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
/* Information for parsing functionc all arguments
 */

struct function_call_info_s
{
    string_t *fun_name;
      /* The name of the function for error messages.
       * (Not refcounted.)
       */

    fulltype_t *arg_types;
      /* The next argument to be parsed. If NULL, then no information
       * is available. This is only used for code generation, so at
       * present only for deciding whether to generate lvalues.
       */

    int remaining_arg_types;
      /* The number or remaining entries shown by .arg_types.
       */

    int arg_position;
      /* The position of the current argument
       * (= number of parsed arguments.)
       */

    bool unlimited_args;
      /* The last argument in .arg_types can be repeated arbitrarily.
       */

    bool got_ellipsis;
      /* Whether the current function arguments used the L_ELLIPSIS operator.
       * TODO: This should be dynamic.
       */
};

static int argument_level;
  /* Nesting level of function call arguments.
   * Used to detect nested function calls, like foo( bar () ).
   */

static struct function_call_info_s function_call_info[COMPILER_STACK_SIZE];
  /* Indexed by <argument_level> contains information about the current
   * function call arguments that are parsed.
   */

/*-------------------------------------------------------------------------*/
/* Other Variables */

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

static ident_t * global_variable_initializing;
  /* The global variable that is currently being initialized.
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
  /* The fulltypes of arguments when calling functions must be saved,
   * to be used afterwards for checking. And because function calls
   * can be done as an argument to a function calls, a stack of argument types
   * is needed. This stack does not need to be freed between compilations,
   * but will be reused.
   */

static A_LOCAL_VARIABLES_t* local_variables = NULL;
  /* The full types and usage information of the local variables.
   * Points to a location in mem_block A_LOCAL_VARIABLES, it is NULL between
   * compilations.
   */

static A_LOCAL_VARIABLES_t* context_variables = NULL;
  /* The full types and usage information of the context variables.
   * Points to a location in mem_block A_LOCAL_VARIABLES, it is NULL between
   * compilations.
   */

static int current_number_of_locals = 0;
  /* Current (active) number of local variables at this point in the
   * function.
   */

static int max_number_of_locals = 0;
  /* Total number of local variables used in this function so far.
   */

static int max_number_of_init_locals = 0;
  /* Total number of local variables used in the __INIT function so far.
   * (These are context variables of inline closures used for initialization.)
   */

static ident_t *all_locals = NULL;
  /* List of defined local variables, listed in reverse order of definition.
   * This also means that the variables are listed in reverse order of
   * nested block scopes.
   */

static lpctype_t * exact_types;
  /* If NULL, don't check nor require argument and function types.
   * Otherwise it's the return type of the function. (Reference
   * is not counted, a counted reference is held in the parser tokens.)
   */

static bool arg_types_exhausted;
  /* True, if the A_ARGUMENT_TYPES block is full (USHRT_MAX entries)
   * and a warning was already given about that.
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

static int call_strict_sefun;
  /* Index of the call_strict() sefun, or < 0 if none;
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

static const char * compiled_file;
  /* The name of the program to be compiled. While current_loc.file reflects
   * the name of the source file currently being read, this name is always
   * the program's name. Set by prolog().
   */

  /* A few standard types we often need.
   * We'll initialize them later (using the type functions, so all pointers
   * are correctly set) and then put them into a static storage (and set
   * their ref count to zero).
   */

lpctype_t _lpctype_unknown_array, _lpctype_any_array,    _lpctype_int_float,
          _lpctype_int_array,     _lpctype_string_array, _lpctype_object_array,
          _lpctype_bytes_array,   _lpctype_string_bytes, _lpctype_string_or_bytes_array,
          _lpctype_string_object, _lpctype_string_object_array;
lpctype_t *lpctype_unknown_array = &_lpctype_unknown_array,
          *lpctype_any_array     = &_lpctype_any_array,
          *lpctype_int_float     = &_lpctype_int_float,
          *lpctype_int_array     = &_lpctype_int_array,
          *lpctype_string_array  = &_lpctype_string_array,
          *lpctype_object_array  = &_lpctype_object_array,
          *lpctype_bytes_array   = &_lpctype_bytes_array,
          *lpctype_string_bytes  = &_lpctype_string_bytes,
          *lpctype_string_or_bytes_array = &_lpctype_string_or_bytes_array,
          *lpctype_string_object = &_lpctype_string_object,
          *lpctype_string_object_array = &_lpctype_string_object_array;


/*-------------------------------------------------------------------------*/
/* Forward declarations */

struct lvalue_s; /* Defined within YYSTYPE aka %union */

static ident_t* define_local_variable (ident_t* name, lpctype_t* actual_type, struct lvalue_s *lv, Bool redeclare, Bool with_init);
static void init_local_variable (ident_t* name, struct lvalue_s *lv, int assign_op, fulltype_t type2);
static void use_variable (ident_t* name, enum variable_usage usage);
static void warn_variable_usage (string_t* name, enum variable_usage usage, const char* prefix);
static Bool add_lvalue_code (lvalue_block_t lv, int instruction);
static void insert_pop_value(void);
static void add_type_check (lpctype_t *expected, enum type_check_operation op);
static int insert_inherited(char *, string_t *, program_t **, function_t *, int, bytecode_p);
  /* Returnvalues from insert_inherited(): */
#  define INHERITED_NOT_FOUND            (-1)
#  define INHERITED_WILDCARDED_ARGS      (-2)
#  define INHERITED_WILDCARDED_NOT_FOUND (-3)
static void store_line_number_relocation(int relocated_from);
int yyparse(void);
static void add_new_init_jump(void);
static void transfer_init_control(void);
static void copy_structs(program_t *, funflag_t);
static void new_inline_closure (void);
static int inherit_program(program_t *from, funflag_t funmodifier, funflag_t varmodifier);
static void fix_variable_index_offsets(program_t *);
static short store_prog_string (string_t *str);

/*-------------------------------------------------------------------------*/
void
yyerror (const char *str)

/* Raise the parse error <str>: usually generate the error message and log it.
 * If this is the first error in this file, account the wizard with an error.
 * If too many errors occurred already, do nothing.
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
static INLINE bool
reserve_mem_block (int n, size_t size)

/* Reserve <size> bytes at the current position in memory area <n>.
 * This does not increase the .current_size. Returns true, when successful,
 * false otherwise (usually an out-of-memory condition).
 * (If false, then an error message was already emitted.)
 */

{
    mem_block_t *mbp = &mem_block[n];

    if (size && mbp->current_size + size > mbp->max_size)
        return realloc_mem_block(mbp, mbp->current_size + size);

    return true;
} /* reserve_mem_block() */

/*-------------------------------------------------------------------------*/
static INLINE bool
extend_mem_block (int n, size_t size)

/* Reserve <size> bytes at the current position in memory area <n>.
 * This does increase the .current_size. Returns true, when successful,
 * false otherwise (usually an out-of-memory condition).
 * (If false, then an error message was already emitted.)
 */

{
    if (reserve_mem_block(n, size))
    {
        mem_block[n].current_size += size;
        return true;
    }

    return false;
} /* extend_mem_block() */

/*-------------------------------------------------------------------------*/
static INLINE bool
add_to_mem_block (int n, void *data, size_t size)

/* Add the <data> block of <size> bytes to the memory area <n>.
 * Returns true, when successful, false otherwise
 * (usually an out-of-memory condition).
 * (If false, then an error message was already emitted.)
 */

{
    mem_block_t *mbp = &mem_block[n];

    if (!size)
        return true;

    if (!reserve_mem_block(n, size))
        return false;

    memcpy(mbp->block + mbp->current_size, data, size);
    mbp->current_size += size;

    return true;
} /* add_to_mem_block() */

/*-------------------------------------------------------------------------*/

/* Define functions like add_to_mem_block() but with the correct
 * type for the corresponding block.
 */

#define DEFINE_ADD_TO_BLOCK_BY_PTR(FUN, BLOCK_NAME)               \
    static INLINE void                                            \
    FUN (BLOCK_NAME##_t *element)                                 \
    {                                                             \
        add_to_mem_block(BLOCK_NAME, element, sizeof(*element));  \
    }
#define DEFINE_ADD_TO_BLOCK_BY_VALUE(FUN, BLOCK_NAME)             \
    static INLINE void                                            \
    FUN (BLOCK_NAME##_t element)                                  \
    {                                                             \
        add_to_mem_block(BLOCK_NAME, &element, sizeof(element));  \
    }
#define DEFINE_RESERVE_MEM_BLOCK(FUN, BLOCK_NAME)                 \
    static INLINE bool                                            \
    FUN (size_t count)                                            \
    {                                                             \
        return reserve_mem_block(BLOCK_NAME,                      \
            sizeof(BLOCK_NAME##_t) * count);                      \
    }

DEFINE_ADD_TO_BLOCK_BY_VALUE(ADD_ARGUMENT_TYPE, A_ARGUMENT_TYPES)
DEFINE_ADD_TO_BLOCK_BY_VALUE(ADD_ARGUMENT_INDEX, A_ARGUMENT_INDEX)
DEFINE_ADD_TO_BLOCK_BY_PTR(ADD_FUNCTION, A_FUNCTIONS)
DEFINE_ADD_TO_BLOCK_BY_PTR(ADD_VIRTUAL_VAR, A_VIRTUAL_VAR)
DEFINE_ADD_TO_BLOCK_BY_PTR(ADD_VARIABLE, A_VARIABLES)
DEFINE_ADD_TO_BLOCK_BY_VALUE(ADD_GLOBAL_VARIABLE_INFO, A_GLOBAL_VARIABLES)
DEFINE_ADD_TO_BLOCK_BY_PTR(ADD_STRUCT_DEF, A_STRUCT_DEFS)
DEFINE_ADD_TO_BLOCK_BY_PTR(ADD_STRUCT_MEMBER, A_STRUCT_MEMBERS)
DEFINE_ADD_TO_BLOCK_BY_PTR(ADD_INLINE_CLOSURE, A_INLINE_CLOSURE)
DEFINE_ADD_TO_BLOCK_BY_PTR(ADD_INCLUDE, A_INCLUDES)
DEFINE_ADD_TO_BLOCK_BY_PTR(ADD_INHERIT, A_INHERITS)
DEFINE_ADD_TO_BLOCK_BY_VALUE(ADD_DEFAULT_VALUE_POS, A_DEFAULT_VALUES_POSITION);

DEFINE_RESERVE_MEM_BLOCK(RESERVE_FUNCTIONS, A_FUNCTIONS);
DEFINE_RESERVE_MEM_BLOCK(RESERVE_UPDATE_INDEX_MAP, A_UPDATE_INDEX_MAP);
DEFINE_RESERVE_MEM_BLOCK(RESERVE_INHERITS, A_INHERITS);
DEFINE_RESERVE_MEM_BLOCK(RESERVE_DEFAULT_VALUE_POS, A_DEFAULT_VALUES_POSITION);

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
/*-------------------------------------------------------------------------*/

static size_t
get_f_visibility_buf (typeflags_t flags, char *buf, size_t bufsize)

/* Write a textual representation of the visibility flags in <flags>
 * into <buf> with maximum size <bufsize>.
 */

{
    size_t len;

    if (bufsize <= 0)
        return 0;

    buf[0] = '\0';
    if (flags & TYPE_MOD_STATIC)
        strncat(buf, "static ", bufsize);
    if (flags & TYPE_MOD_NO_MASK)
        strncat(buf, "nomask ", bufsize);
    if (flags & TYPE_MOD_PRIVATE)
        strncat(buf, "private ", bufsize);
    if (flags & TYPE_MOD_PROTECTED)
        strncat(buf, "protected ", bufsize);
    if (flags & TYPE_MOD_PUBLIC)
        strncat(buf, "public ", bufsize);
    if (flags & TYPE_MOD_VISIBLE)
        strncat(buf, "visible ", bufsize);
    if (flags & TYPE_MOD_VARARGS)
        strncat(buf, "varargs ", bufsize);
    if (flags & TYPE_MOD_DEPRECATED)
        strncat(buf, "deprecated ", bufsize);

    len = strlen(buf);
    if (len && buf[len-1] == ' ')
        buf[--len] = '\0';

    return len;
} /* get_f_visibility_buf() */

/*-------------------------------------------------------------------------*/
static char *
get_f_visibility (typeflags_t flags)

/* Return (in a static buffer) a textual representation of the visibility
 * flags in <flags>.
 */

{
    static char buff[120];
    get_f_visibility_buf(flags, buff, sizeof(buff));
    return buff;
} /* get_f_visibility() */

/*-------------------------------------------------------------------------*/
static char *
get_visibility (fulltype_t type)

/* Return (in a static buffer) a textual representation of the visibility
 * portion of <type>.
 */

{
    return get_f_visibility(type.t_flags);
} /* get_visibility() */

/*-------------------------------------------------------------------------*/
size_t
get_lpctype_name_buf (lpctype_t *type, char *buf, size_t bufsize)

/* Write a textual representation of <type> into <buf>.
 * At most <bufsize> bytes (including the trailing '\0') are written.
 * Returns the number of bytes written (excluding the trailing '\0').
 */
{
    static char *type_name[] = { "unknown", "int", "string", "void",
                                 "object", "mapping", "float", "mixed",
                                 "closure", "symbol", "quoted_array", "bytes" };

    if (bufsize <= 0)
        return 0;

    if (type == NULL)
        type = lpctype_mixed;

    switch(type->t_class)
    {
    case TCLASS_PRIMARY:
        {
            char* name;
            size_t len;

            if (type->t_primary >= sizeof type_name / sizeof type_name[0])
                fatal("Bad type %"PRIu32": %s line %d\n"
                     , type->t_primary,  current_loc.file->name
                     , current_loc.line);

            name = type_name[type->t_primary];
            len = strlen(name);

            if(len < bufsize)
            {
                memcpy(buf, name, len+1);
                return len;
            }
            else
            {
                buf[0] = '\0';
                return 0;
            }
        }

    case TCLASS_STRUCT:
        {
            if (type->t_struct.name)
            {
                size_t len = 7 + mstrsize(type->t_struct.name->name);
                if(len < bufsize)
                {
                    memcpy(buf, "struct ", 7);
                    memcpy(buf+7, get_txt(type->t_struct.name->name), len-7);
                    buf[len] = 0;
                    return len;
                }
                else
                {
                    buf[0] = '\0';
                    return 0;
                }
            }
            else // no struct
            {
                snprintf(buf, bufsize, "unknown struct");
                return strlen(buf);
            }

        }

    case TCLASS_ARRAY:
        {
            size_t sublen;
            lpctype_t *basetype;

            basetype = type->t_array.base;
            if(basetype->t_class == TCLASS_UNION)
            {
                buf[0] = '<';
                sublen = get_lpctype_name_buf(basetype, buf + 1, bufsize - type->t_array.depth - 2);
                if(sublen)
                {
                    buf[sublen+1] = '>';
                    sublen += 2;
                }
            }
            else
                sublen = get_lpctype_name_buf(basetype, buf, bufsize - type->t_array.depth);

            if(sublen)
            {
                memset(buf + sublen, '*', type->t_array.depth);
                sublen += type->t_array.depth;
            }

            buf[sublen] = '\0';
            return sublen;
        }

    case TCLASS_UNION:
        {
            lpctype_t *curtype = type;
            char* curbuf = buf;
            size_t sublen;

            if (bufsize < 5)
            {
                buf[0] = 0;
                return 0;
            }

            while (curtype->t_class == TCLASS_UNION)
            {
                sublen = get_lpctype_name_buf(curtype->t_union.member, curbuf, bufsize - 4);
                if(!sublen)
                    break;

                curbuf[sublen] = '|';
                curbuf += sublen + 1;
                curtype = curtype->t_union.head;
            }

            if(sublen)
            {
                sublen = get_lpctype_name_buf(curtype, curbuf, bufsize);
                curbuf += sublen;
            }

            if(!sublen)
            {
                if(curbuf == buf)
                    return 0;
                memcpy(curbuf, "...", 4);
                return curbuf - buf + 3;
            }

            return curbuf - buf;
        }
    }

    /* Not reached. */
    buf[0] = '\0';
    return 0;
} /* get_lpctype_name_buf() */

/*-------------------------------------------------------------------------*/
char *
get_lpctype_name (lpctype_t *type)

/* Return (in a static buffer) a textual representation of <type>.
 */

{
    static char buff[512];

    get_lpctype_name_buf(type, buff, sizeof(buff));
    return buff;
} /* get_lpctype_name() */

/*-------------------------------------------------------------------------*/
size_t
get_fulltype_name_buf (fulltype_t type, char *buf, size_t bufsize)

/* Write a textual representation of <type> into <buf>.
 * At most <bufsize> bytes (including the trailing '\0') are written.
 * Returns the number of bytes written (excluding the trailing '\0').
 */
{
    size_t len;

    if (bufsize <= 0)
        return 0;

    if (type.t_flags & TYPE_MOD_REFERENCE)
    {
        if(bufsize < 4)
        {
            buf[0] = '\0';
            return 0;
        }

        bufsize -= 2;
    }

    len = get_f_visibility_buf(type.t_flags & ~TYPE_MOD_REFERENCE, buf, bufsize);
    if(len && len + 1 < bufsize)
        buf[len++] = ' ';
    len += get_lpctype_name_buf(type.t_type, buf + len, bufsize - len);

    if (type.t_flags & TYPE_MOD_REFERENCE)
    {
        memcpy(buf + len, " &", 3);
        len += 2;
    }

    return len;
} /* get_fulltype_name_buf() */

/*-------------------------------------------------------------------------*/
char *
get_fulltype_name (fulltype_t type)

/* Return (in a static buffer) a textual representation of <type>.
 */

{
    static char buff[1024];

    get_fulltype_name_buf(type, buff, sizeof(buff));
    return buff;
} /* get_fulltype_name() */

/*-------------------------------------------------------------------------*/
static char *
get_two_fulltypes (fulltype_t type1, fulltype_t type2)

/* Return (in a static buffer) the text "(<type1> vs. <type2>)".
 */
{
    static char buff[1024];
    size_t len, len2;

    buff[0] = '(';
    len = 1 + get_fulltype_name_buf(type1, buff+1, sizeof(buff)-10);

    memcpy(buff + len, " vs ", 4);
    len += 4;

    len2 = get_fulltype_name_buf(type2, buff+len, sizeof(buff)-len-1);
    if(!len2)
        memcpy(buff + len, "...)", 5);
    else
        memcpy(buff + len + len2, ")", 2);

    return buff;
} /* get_two_fulltypes() */

/*-------------------------------------------------------------------------*/
static char *
get_two_lpctypes (lpctype_t *type1, lpctype_t *type2)

/* Return (in a static buffer) the text "(<type1> vs. <type2>)".
 */
{
    static char buff[1024];
    size_t len, len2;

    buff[0] = '(';
    len = 1 + get_lpctype_name_buf(type1, buff+1, sizeof(buff)-10);

    memcpy(buff + len, " vs ", 4);
    len += 4;

    len2 = get_lpctype_name_buf(type2, buff+len, sizeof(buff)-len-1);
    if(!len2)
        memcpy(buff + len, "...)", 5);
    else
        memcpy(buff + len + len2, ")", 2);

    return buff;
} /* get_two_lpctypes() */

/*-------------------------------------------------------------------------*/
static void
fulltype_error (char *str, fulltype_t type)

/* Generate an yyerror with the message "<str>: <type>".
 */
{
    char *p;

    p = get_fulltype_name(type);
    yyerrorf("%s: \"%s\"", str, p);
} /* fulltype_error() */

/*-------------------------------------------------------------------------*/
static void
lpctype_error (char *str, lpctype_t *type)

/* Generate an yyerror with the message "<str>: <type>".
 */
{
    char *p;

    p = get_lpctype_name(type);
    yyerrorf("%s: \"%s\"", str, p);
} /* lpctype_error() */

/*-------------------------------------------------------------------------*/
static void
argument_type_error (int instr, lpctype_t *type)

/* Generate an yyerror with the message "Bad argument to <instr>: <type>".
 */

{
    char *p;

    p = get_lpctype_name(type);
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
    for (; expected->t_type; expected++)
    {
        if (msg[0] != '\0')
            strcat(msg, "|");
        strcat(msg, get_fulltype_name(*expected));
    }
    yyerrorf("Bad arg %d type to %s(): got %s, expected %s"
            , arg, instrs[instr].name, get_fulltype_name(got), msg);
} /* efun_argument_error() */

/*-------------------------------------------------------------------------*/
typedef struct unary_op_types_s unary_op_types_t;
typedef struct binary_op_types_s binary_op_types_t;

struct unary_op_types_s
{
    lpctype_t* t;       /* argument to the operator. */
    lpctype_t* result;  /* result type for this operation. */

    lpctype_t* (*resultfun)(lpctype_t* t);
      /* if <resultfun> is not NULL it is called with the argument
       * type, and only if it returns a non-NULL value, this entry
       * matches. If <result> is NULL then the function's result
       * will be regarded as the operator result type.
       * (otherwise <result> will be taken).
       */
};

struct binary_op_types_s
{
    lpctype_t* t1;      /* first argument to the operator. */
    lpctype_t* t2;      /* second argument to the operator. */
    lpctype_t* result;  /* result type for this type combination. */

    lpctype_t* (*resultfun)(lpctype_t* t1, lpctype_t* t2);
      /* If <resultfun> is not NULL it is called with the argument
       * types, and only if it returns a non-NULL value, this
       * entry matches. If <result> is NULL then the function's
       * result will be regarded as the operator result type
       * (otherwise <result> will be taken).
       */

    lpctype_t* (*expfun)(lpctype_t* t1, lpctype_t* t2);
      /* If <expfun> is not NULL it is called with the argument
       * types when this entry didn't match. It can then
       * return a suggested type, that will be printed in the
       * error message.
       */

    lpctype_t* (*rttcfun)(lpctype_t* t1, lpctype_t* t2);
      /* This is used for assigment operations to create the
       * type used to check the second operand at runtime.
       * The function shall return the type used for the check.
       * If NULL then t2 in this entry is used.
       */
};

static lpctype_t*
get_array_member_type (lpctype_t* array)

/* <array> is an array or a union of arrays.
 * Determine the type of its members.
 */

{
    lpctype_t *result = NULL;
    lpctype_t *head = array;

    while (true)
    {
        lpctype_t *member = head->t_class == TCLASS_UNION ? head->t_union.member : head;

        if(member->t_class == TCLASS_ARRAY)
        {
            lpctype_t *oldresult = result;
            result = get_union_type(result, member->t_array.element);
            free_lpctype(oldresult);
        }
        else /* must be mixed or unknown */
        {
            free_lpctype(result);
            return ref_lpctype(member);
        }

        if (head->t_class == TCLASS_UNION)
            head = head->t_union.head;
        else
            break;
    }

    return result;
}

static lpctype_t*
get_union_array_type (lpctype_t* t1, lpctype_t* t2)

/* <t1> and <t2> are arrays. This function creates an array type
 * of the union of the members of both types.
 */

{
    lpctype_t *member, *result;
    lpctype_t *element1 = get_array_member_type(t1),
              *element2 = get_array_member_type(t2);

    if (element1 == lpctype_unknown)
        member = ref_lpctype(element2);
    else if (element2 == lpctype_unknown)
        member = ref_lpctype(element1);
    else
        member = get_union_type(element1, element2);
    result = get_array_type(member);

    free_lpctype(member);
    free_lpctype(element1);
    free_lpctype(element2);

    return result;
}

static lpctype_t*
get_common_array_type (lpctype_t* t1, lpctype_t* t2)

/* <t1> and <t2> are arrays. This function creates an array type
 * of the intersection of the members of both types.
 */

{
    lpctype_t *member, *result;
    lpctype_t *element1 = get_array_member_type(t1),
              *element2 = get_array_member_type(t2);

    if (element1 == lpctype_unknown)
        member = ref_lpctype(element2);
    else if (element2 == lpctype_unknown)
        member = ref_lpctype(element1);
    else
        member = get_common_type(element1, element2);
    result = get_array_type(member);

    free_lpctype(member);
    free_lpctype(element1);
    free_lpctype(element2);

    return result;
}

static lpctype_t*
get_sub_array_type (lpctype_t* t1, lpctype_t* t2)

/* <t1> and <t2> are arrays. This function returns <t1> if
 * there are any common member types, otherwise this functions fails.
 */

{
    lpctype_t *common;
    lpctype_t *element1 = get_array_member_type(t1),
              *element2 = get_array_member_type(t2);

    common = get_common_type(element1, element2);

    free_lpctype(element1);
    free_lpctype(element2);

    if(common == NULL)
        return NULL;

    free_lpctype(common);
    return ref_lpctype(t1);
}


static lpctype_t*
get_first_type (lpctype_t* t1, lpctype_t* t2)

/* Return <t1> as the result type in the type table.
 */

{
    return ref_lpctype(t1);
}


static lpctype_t*
get_second_type (lpctype_t* t1, lpctype_t* t2)

/* Return <t2> as the result type in the type table.
 */

{
    return ref_lpctype(t2);
}

static lpctype_t*
get_argument_type (lpctype_t* t)

/* Return <t> as the result type in the type table.
 * This is the intersection between the type in
 * the table and the type of the argument.
 */

{
    return ref_lpctype(t);
}


/*-------------------------------------------------------------------------*/
static bool
check_unknown_type (lpctype_t* t)

/* Checks if the argument <t> is the unknown type.
 * If it is a compile error will be raised an true returned.
 * Otherwise returns false. If NULL is given, true will be
 * returned but no error given.
 */

{
    if (t == lpctype_unknown)
    {
        yyerror("Function call result must be casted due to pragma strict_types");
        return true;
    }

    return t == NULL;
} /* check_unknown_type() */

/*-------------------------------------------------------------------------*/
/* Operator type table for assignment with addition.
 */
binary_op_types_t types_add_assignment[] = {
    { &_lpctype_string,    &_lpctype_string,    &_lpctype_string,  NULL                  , NULL                   , NULL                   },
    { &_lpctype_string,    &_lpctype_int,       &_lpctype_string,  NULL                  , NULL                   , NULL                   },
    { &_lpctype_string,    &_lpctype_float,     &_lpctype_string,  NULL                  , NULL                   , NULL                   },
    { &_lpctype_bytes,     &_lpctype_bytes,     &_lpctype_bytes,   NULL                  , NULL                   , NULL                   },
    { &_lpctype_int,       &_lpctype_int,       &_lpctype_int,     NULL                  , NULL                   , NULL                   },
    { &_lpctype_float,     &_lpctype_int_float, &_lpctype_float,   NULL                  , NULL                   , NULL                   },
    { &_lpctype_any_array, &_lpctype_any_array, NULL,              &get_sub_array_type   , &get_first_type        , &get_common_array_type },
    { &_lpctype_mapping,   &_lpctype_mapping,   &_lpctype_mapping, NULL                  , NULL                   , NULL                   },
    { NULL, NULL, NULL, NULL, NULL, NULL }
};

/* Operator type table for assignment with subtraction.
 */
binary_op_types_t types_sub_assignment[] = {
    { &_lpctype_string,    &_lpctype_string,    &_lpctype_string,  NULL                  , NULL                   , NULL                   },
    { &_lpctype_bytes,     &_lpctype_bytes,     &_lpctype_bytes,   NULL                  , NULL                   , NULL                   },
    { &_lpctype_int,       &_lpctype_int,       &_lpctype_int,     NULL                  , NULL                   , NULL                   },
    { &_lpctype_float,     &_lpctype_int_float, &_lpctype_float,   NULL                  , NULL                   , NULL                   },
    { &_lpctype_mapping,   &_lpctype_mapping,   &_lpctype_mapping, NULL                  , NULL                   , NULL                   },
    { &_lpctype_mapping,   &_lpctype_any_array, &_lpctype_mapping, NULL                  , NULL                   , NULL                   },
    { &_lpctype_any_array, &_lpctype_mapping,   NULL,              &get_first_type       , NULL                   , NULL                   },
    { &_lpctype_any_array, &_lpctype_any_array, NULL,              &get_sub_array_type   , &get_first_type        , NULL                   },
    { NULL, NULL, NULL, NULL, NULL, NULL }
};

/* Operator type table for assignment with multiplication.
 */
binary_op_types_t types_mul_assignment[] = {
    { &_lpctype_int,       &_lpctype_int,       &_lpctype_int,     NULL                  , NULL                   , NULL                   },
    { &_lpctype_float,     &_lpctype_int_float, &_lpctype_float,   NULL                  , NULL                   , NULL                   },
    { &_lpctype_string,    &_lpctype_int,       &_lpctype_string,  NULL                  , NULL                   , NULL                   },
    { &_lpctype_bytes,     &_lpctype_int,       &_lpctype_bytes,   NULL                  , NULL                   , NULL                   },
    { &_lpctype_any_array, &_lpctype_int,       NULL,              &get_first_type       , NULL                   , NULL                   },
    { NULL, NULL, NULL, NULL, NULL, NULL }
};

/* Operator type table for assignment with division.
 */
binary_op_types_t types_div_assignment[] = {
    { &_lpctype_int,       &_lpctype_int,       &_lpctype_int,     NULL                  , NULL                   , NULL                   },
    { &_lpctype_float,     &_lpctype_int_float, &_lpctype_float,   NULL                  , NULL                   , NULL                   },
    { NULL, NULL, NULL, NULL, NULL, NULL }
};

/* Operator type table for assignment with the binary and.
 */
binary_op_types_t types_binary_and_assignment[] = {
    { &_lpctype_mapping,   &_lpctype_mapping,   &_lpctype_mapping, NULL                  , NULL                   , NULL                   },
    { &_lpctype_mapping,   &_lpctype_any_array, &_lpctype_mapping, NULL                  , NULL                   , NULL                   },
    { &_lpctype_any_array, &_lpctype_mapping,   NULL,              &get_first_type       , NULL                   , NULL                   },
    { &_lpctype_any_array, &_lpctype_any_array, NULL,              &get_common_type      , NULL                   , NULL                   },
    { &_lpctype_int,       &_lpctype_int,       &_lpctype_int,     NULL                  , NULL                   , NULL                   },
    { &_lpctype_string,    &_lpctype_string,    &_lpctype_string,  NULL                  , NULL                   , NULL                   },
    { &_lpctype_bytes,     &_lpctype_bytes,     &_lpctype_bytes,   NULL                  , NULL                   , NULL                   },
    { NULL, NULL, NULL, NULL, NULL, NULL }
};

/* Operator type table for assignment with the binary or and xor.
 */
binary_op_types_t types_binary_or_assignment[] = {
    { &_lpctype_int,       &_lpctype_int,       &_lpctype_int,     NULL                  , NULL                   , NULL                   },
    { &_lpctype_any_array, &_lpctype_any_array, NULL,              &get_sub_array_type   , &get_first_type        , &get_common_array_type },
    { NULL, NULL, NULL, NULL, NULL, NULL }
};

/* Operator type table for the binary or and xor,
 * allowing <int>|<int> and <mixed*>|<mixed*>.
 */
binary_op_types_t types_binary_or[] = {
    { &_lpctype_int,       &_lpctype_int,       &_lpctype_int,     NULL                  , NULL                   , NULL                   },
    { &_lpctype_any_array, &_lpctype_any_array, NULL,              &get_union_array_type , NULL                   , NULL                   },
    { NULL, NULL, NULL, NULL, NULL, NULL }
};

/* Operator type table for the binary and.
 */
binary_op_types_t types_binary_and[] = {
    { &_lpctype_mapping,   &_lpctype_mapping,   &_lpctype_mapping, NULL                  , NULL                   , NULL                   },
    { &_lpctype_mapping,   &_lpctype_any_array, &_lpctype_mapping, NULL                  , NULL                   , NULL                   },
    { &_lpctype_any_array, &_lpctype_mapping,   NULL,              &get_first_type       , NULL                   , NULL                   },
    { &_lpctype_any_array, &_lpctype_any_array, NULL,              &get_common_type      , NULL                   , NULL                   },
    { &_lpctype_int,       &_lpctype_int,       &_lpctype_int,     NULL                  , NULL                   , NULL                   },
    { &_lpctype_string,    &_lpctype_string,    &_lpctype_string,  NULL                  , NULL                   , NULL                   },
    { &_lpctype_bytes,     &_lpctype_bytes,     &_lpctype_bytes,   NULL                  , NULL                   , NULL                   },
    { NULL, NULL, NULL, NULL, NULL, NULL }
};

/* Operator type table for equality tests.
 * Basically there must be a common type, but ints can be compared with floats also.
 */
binary_op_types_t types_equality[] = {
    { &_lpctype_mixed,     &_lpctype_mixed,     &_lpctype_int,     &get_common_type      , NULL                   , NULL                   },
    { &_lpctype_int,       &_lpctype_float,     &_lpctype_int,     NULL                  , NULL                   , NULL                   },
    { &_lpctype_float,     &_lpctype_int,       &_lpctype_int,     NULL                  , NULL                   , NULL                   },
    { NULL, NULL, NULL, NULL, NULL, NULL }
};

/* Operator type table for shift operations.
 * Only ints are allowed.
 */
binary_op_types_t types_shift[] = {
    { &_lpctype_int,       &_lpctype_int,       &_lpctype_int,     NULL                  , NULL                   , NULL                   },
    { NULL, NULL, NULL, NULL, NULL, NULL }
};

/* Operator type table for addition.
 */
binary_op_types_t types_addition[] = {
    { &_lpctype_string,    &_lpctype_string,    &_lpctype_string,  NULL                  , NULL                   , NULL                   },
    { &_lpctype_string,    &_lpctype_int,       &_lpctype_string,  NULL                  , NULL                   , NULL                   },
    { &_lpctype_string,    &_lpctype_float,     &_lpctype_string,  NULL                  , NULL                   , NULL                   },
    { &_lpctype_bytes,     &_lpctype_bytes,     &_lpctype_bytes,   NULL                  , NULL                   , NULL                   },
    { &_lpctype_int,       &_lpctype_string,    &_lpctype_string,  NULL                  , NULL                   , NULL                   },
    { &_lpctype_int,       &_lpctype_int,       &_lpctype_int,     NULL                  , NULL                   , NULL                   },
    { &_lpctype_int,       &_lpctype_float,     &_lpctype_float,   NULL                  , NULL                   , NULL                   },
    { &_lpctype_float,     &_lpctype_string,    &_lpctype_string,  NULL                  , NULL                   , NULL                   },
    { &_lpctype_float,     &_lpctype_int_float, &_lpctype_float,   NULL                  , NULL                   , NULL                   },
    { &_lpctype_any_array, &_lpctype_any_array, NULL,              &get_union_array_type , NULL                   , NULL                   },
    { &_lpctype_mapping,   &_lpctype_mapping,   &_lpctype_mapping, NULL                  , NULL                   , NULL                   },
    { NULL, NULL, NULL, NULL, NULL, NULL }
};

/* Operator type table for subtraction.
 */
binary_op_types_t types_subtraction[] = {
    { &_lpctype_string,    &_lpctype_string,    &_lpctype_string,  NULL                  , NULL                   , NULL                   },
    { &_lpctype_bytes,     &_lpctype_bytes,     &_lpctype_bytes,   NULL                  , NULL                   , NULL                   },
    { &_lpctype_int,       &_lpctype_int,       &_lpctype_int,     NULL                  , NULL                   , NULL                   },
    { &_lpctype_int,       &_lpctype_float,     &_lpctype_float,   NULL                  , NULL                   , NULL                   },
    { &_lpctype_float,     &_lpctype_int_float, &_lpctype_float,   NULL                  , NULL                   , NULL                   },
    { &_lpctype_mapping,   &_lpctype_mapping,   &_lpctype_mapping, NULL                  , NULL                   , NULL                   },
    { &_lpctype_mapping,   &_lpctype_any_array, &_lpctype_mapping, NULL                  , NULL                   , NULL                   },
    { &_lpctype_any_array, &_lpctype_mapping,   NULL,              &get_first_type       , NULL                   , NULL                   },
    { &_lpctype_any_array, &_lpctype_any_array, NULL,              &get_sub_array_type   , &get_first_type        , NULL                   },
    { NULL, NULL, NULL, NULL, NULL, NULL }
};

/* Operator type table for multiplication.
 */
binary_op_types_t types_multiplication[] = {
    { &_lpctype_int,       &_lpctype_int,       &_lpctype_int,     NULL                  , NULL                   , NULL                   },
    { &_lpctype_int,       &_lpctype_float,     &_lpctype_float,   NULL                  , NULL                   , NULL                   },
    { &_lpctype_int,       &_lpctype_string,    &_lpctype_string,  NULL                  , NULL                   , NULL                   },
    { &_lpctype_int,       &_lpctype_bytes,     &_lpctype_bytes,   NULL                  , NULL                   , NULL                   },
    { &_lpctype_int,       &_lpctype_any_array, NULL,              &get_second_type      , NULL                   , NULL                   },
    { &_lpctype_float,     &_lpctype_int_float, &_lpctype_float,   NULL                  , NULL                   , NULL                   },
    { &_lpctype_string,    &_lpctype_int,       &_lpctype_string,  NULL                  , NULL                   , NULL                   },
    { &_lpctype_bytes,     &_lpctype_int,       &_lpctype_bytes,   NULL                  , NULL                   , NULL                   },
    { &_lpctype_any_array, &_lpctype_int,       NULL,              &get_first_type       , NULL                   , NULL                   },
    { NULL, NULL, NULL, NULL, NULL, NULL }
};

/* Operator type table for the modulus operation.
 */
binary_op_types_t types_modulus[] = {
    { &_lpctype_int,       &_lpctype_int,       &_lpctype_int,     NULL                  , NULL                   , NULL                   },
    { NULL, NULL, NULL, NULL, NULL, NULL }
};

/* Operator type table for division.
 */
binary_op_types_t types_division[] = {
    { &_lpctype_int,       &_lpctype_int,       &_lpctype_int,     NULL                  , NULL                   , NULL                   },
    { &_lpctype_int,       &_lpctype_float,     &_lpctype_float,   NULL                  , NULL                   , NULL                   },
    { &_lpctype_float,     &_lpctype_int_float, &_lpctype_float,   NULL                  , NULL                   , NULL                   },
    { NULL, NULL, NULL, NULL, NULL, NULL }
};

/* Operator type table for the unary minus, increment and decrement operation.
 */
unary_op_types_t types_unary_math[] = {
    { &_lpctype_int,                            &_lpctype_int,     NULL                  },
    { &_lpctype_float,                          &_lpctype_float,   NULL                  },
    { NULL, NULL, NULL }
};

/* Operator type table for the range index operation (arg[x..y])
 */
unary_op_types_t types_range_index[] = {
    { &_lpctype_string,                         &_lpctype_string,  NULL                  },
    { &_lpctype_bytes,                          &_lpctype_bytes,   NULL                  },
    { &_lpctype_any_array,                      NULL,              &get_argument_type    },
    { NULL, NULL, NULL }
};

/* Type table for foreach iteration
 */
unary_op_types_t types_foreach_iteration[] = {
    { &_lpctype_int,                            &_lpctype_int,     NULL                  },
    { &_lpctype_string,                         &_lpctype_int,     NULL                  },
    { &_lpctype_bytes,                          &_lpctype_int,     NULL                  },
    { &_lpctype_any_array,                      NULL,              &get_array_member_type},
    { &_lpctype_any_struct,                     &_lpctype_mixed,   NULL                  },
    { &_lpctype_mapping,                        &_lpctype_mixed,   NULL                  },
    { NULL, NULL, NULL }
};

/*-------------------------------------------------------------------------*/
static lpctype_t*
check_unary_op_type (lpctype_t* t, const char* op_name, unary_op_types_t* type_table, lpctype_t *error_type)

/* Checks the argument <t> against the possible combinations in type_table.
 * The result is the union of all result types of matching entries.
 * If there are no matching entries, a compile error is thrown
 * (unless <op_name> is NULL) and <error_type> is returned.
 */

{
    lpctype_t* result = NULL;

    /* No type info? Use the fallback. */
    if (check_unknown_type(t))
        return ref_lpctype(error_type);

    for (unary_op_types_t* entry = type_table; entry->t; ++entry)
    {
        /* Is there any commonality between our
         * argument type and the entry in the table?
         */
        lpctype_t *m = get_common_type(entry->t, t);
        lpctype_t *mresult, *oldresult;
        if (m == NULL)
            continue;

        mresult = ref_lpctype(entry->result);

        /* Then check resultfun, if it exists.
         * It is an additional condition (must return != NULL)
         * and returns the result type unless given in entry->result.
         */
        if (entry->resultfun != NULL)
        {
            lpctype_t *funresult = (*entry->resultfun)(m);
            if (funresult == NULL)
            {
                free_lpctype(m);
                free_lpctype(mresult);
                continue;
            }

            if (mresult == NULL)
                mresult = funresult;
            else
                free_lpctype(funresult);
        }

        oldresult = result;
        result = get_union_type(result, mresult);
        free_lpctype(m);
        free_lpctype(mresult);
        free_lpctype(oldresult);
    }

    if (result != NULL || op_name == NULL)
        return result;

    /* Now get the error message:
     * "Bad argument to <op_name>: <t>, expected ..."
     * and show the unions of all first types in the table.
     */
    if (exact_types)
    {
        lpctype_t* expected = NULL;
        char expbuff[1024];

        for (unary_op_types_t* entry = type_table; entry->t; ++entry)
        {
            lpctype_t *oldexpected = expected;
            expected = get_union_type(expected, entry->t);
            free_lpctype(oldexpected);
        }

        get_lpctype_name_buf(expected, expbuff, sizeof(expbuff));
        free_lpctype(expected);

        yyerrorf("Bad argument to %s: got %s, expected %s"
            , op_name, get_lpctype_name(t), expbuff);
    }

    return ref_lpctype(error_type);

} /* check_unary_op_type() */

/*-------------------------------------------------------------------------*/
static lpctype_t*
check_binary_op_types (lpctype_t* t1, lpctype_t* t2, const char* op_name, binary_op_types_t* type_table, lpctype_t *error_type, lpctype_t **rttc2, bool t2_literal)

/* Checks the arguments <t1> and <t2> against the possible combinations
 * in type_table. The result is the union of all result types of
 * matching entries. If there are no matching entries, a compile error
 * is thrown (unless <op_name> is NULL) and <error_type> is returned.
 *
 * If <rttc2> is not NULL a type suitable for a runtime type check on the
 * second argument is stored there (a union of all allowed types for the
 * second argument that are a subset of <t2>).
 *
 * If <t2_literal> is true, the second operand must be fully handled.
 * To check this, the <rttc2> type will be calculated even if NULL was given.
 * Otherwise an error will be thrown as well.
 */

{
    Bool t1_matched = MY_FALSE;
    lpctype_t* result = NULL;
    lpctype_t* t2check = NULL;

    if (rttc2)
        *rttc2 = NULL;
    else if (t2_literal)
        rttc2 = &t2check;

    /* No type info? Use the fallback as a shortcut. */
    if (t1 == NULL && t2 == NULL)
        return error_type ? ref_lpctype(error_type) : lpctype_mixed;

    if (check_unknown_type(t1))
        t1 = lpctype_mixed;

    if (check_unknown_type(t2))
        t2 = lpctype_mixed;

    for (binary_op_types_t* entry = type_table; entry->t1; ++entry)
    {
        /* Is there any commonality between our first
         * argument type and the entry in the table?
         */
        lpctype_t *m1 = get_common_type(entry->t1, t1);
        lpctype_t *m2, *mresult, *oldresult;
        if (m1 == NULL)
            continue;

        t1_matched = MY_TRUE;
        /* And if so, is there some intersection
         * between <t2> and the table entry?
         */
        m2 = get_common_type(entry->t2, t2);
        if (m2 == NULL)
        {
            free_lpctype(m1);
            continue;
        }

        mresult = ref_lpctype(entry->result);

        /* Then check resultfun, if it exists.
         * It is an additional condition (must return != NULL)
         * and returns the result type unless given in entry->result.
         */
        if (entry->resultfun != NULL)
        {
            lpctype_t *funresult = (*entry->resultfun)(m1,m2);
            if (funresult == NULL)
            {
                free_lpctype(m1);
                free_lpctype(m2);
                free_lpctype(mresult);
                continue;
            }

            if (mresult == NULL)
                mresult = funresult;
            else
                free_lpctype(funresult);
        }

        oldresult = result;
        result = get_union_type(result, mresult);

        if (rttc2)
        {
            lpctype_t *rttcadd;

            if (entry->rttcfun != NULL)
                rttcadd = (*entry->rttcfun)(m1, m2);
            else
                rttcadd = ref_lpctype(entry->t2);

            if (rttcadd)
            {
                lpctype_t *oldrttc2 = *rttc2;
                *rttc2 = get_union_type(*rttc2, rttcadd);
                free_lpctype(rttcadd);
                free_lpctype(oldrttc2);
            }
        }

        free_lpctype(m1);
        free_lpctype(m2);
        free_lpctype(mresult);
        free_lpctype(oldresult);
    }

    if (t2_literal)
    {
        if (!lpctype_contains(t2, *rttc2))
        {
            free_lpctype(result);
            result = NULL;
        }

        free_lpctype(t2check);
    }

    if (result != NULL || op_name == NULL)
        return result;

    /* For the error message, remember whether at least <t1> matched somewhere.
     * If so, "Bad argument 2 to <op_name>: <t2>, expected ..." and show the
     * unions of all second types for each match for t1. Otherwise
     * "Bad argument 1 to <op_name>: <t1>, expected ..." and show the unions
     * of all first types in the table.
     */
    if (!exact_types)
    {
        /* No strong type check, don't print error messages. */
    }
    else if (t1_matched)
    {
        lpctype_t* expected = NULL;
        char expbuff[1024];

        for (binary_op_types_t* entry = type_table; entry->t1; ++entry)
        {
            lpctype_t *m1 = get_common_type(entry->t1, t1);
            lpctype_t *oldexpected, *newexpected;
            if (m1 == NULL)
                continue;

            oldexpected = expected;
            if (entry->expfun)
            {
                lpctype_t *m2 = get_common_type(entry->t2, t2);
                if (m2 == NULL)
                    newexpected = ref_lpctype(entry->t2);
                else
                {
                    newexpected = (*entry->expfun)(m1, m2);
                    free_lpctype(m2);
                }
            }
            else
                newexpected = ref_lpctype(entry->t2);

            expected = get_union_type(expected, newexpected);

            free_lpctype(m1);
            free_lpctype(oldexpected);
            free_lpctype(newexpected);
        }

        get_lpctype_name_buf(expected, expbuff, sizeof(expbuff));
        free_lpctype(expected);

        yyerrorf("Bad argument 2 to %s: got %s, expected %s"
            , op_name, get_lpctype_name(t2), expbuff);
    }
    else
    {
        lpctype_t* expected = NULL;
        char expbuff[1024];

        for (binary_op_types_t* entry = type_table; entry->t1; ++entry)
        {
            lpctype_t *oldexpected = expected;
            expected = get_union_type(expected, entry->t1);
            free_lpctype(oldexpected);
        }

        get_lpctype_name_buf(expected, expbuff, sizeof(expbuff));
        free_lpctype(expected);

        yyerrorf("Bad argument 1 to %s: got %s, expected %s"
            , op_name, get_lpctype_name(t1), expbuff);
    }

    return ref_lpctype(error_type);

} /* check_binary_op_types() */

/*-------------------------------------------------------------------------*/
static lpctype_t*
get_index_result_type (lpctype_t* aggregate, fulltype_t index, int inst, lpctype_t* error_type)

/* Determines the result type of a single index operation on <aggregate>.
 * Also checks whether the <index> and the index operation <inst> are
 * legal for <aggregate>.
 *
 * The result is the union of all possible result types. If there are no
 * possible results, a compile erorr is thrown (and <error_type> is
 * returned, reference added).
 */
{
    lpctype_t* result = NULL;
    lpctype_t* head = aggregate;
    bool can_be_mapping = false;
    bool can_be_array_or_string = false;
    bool found_empty_array = false;

    if (index.t_flags & TYPE_MOD_REFERENCE)
        yyerror("Reference used as index");

    /* No type information, then it might be anything... */
    if (check_unknown_type(aggregate))
        return lpctype_mixed;

    /* Treat index as anything, if we don't know what it is. */
    if (index.t_type == NULL)
        index.t_type = lpctype_mixed;

    /* [<n] or [>n] can only index an array or string,
     * [n] (F_INDEX) can also index a mapping.
     */
    if (inst == F_INDEX)
        can_be_mapping = true;

    /* Array or string indexing need an integer as an index. */
    if (lpctype_contains(lpctype_int, index.t_type))
        can_be_array_or_string = true;
    else if(inst != F_INDEX)
    {
        /* [<n] or [>n] need an integer. */
        if (exact_types)
            lpctype_error("Bad type of index", index.t_type);
        return ref_lpctype(error_type);
    }

    /* Now walk through <aggregate>. */
    while (true)
    {
        lpctype_t *member = head->t_class == TCLASS_UNION ? head->t_union.member : head;

        switch (member->t_class)
        {
        case TCLASS_PRIMARY:
            switch (member->t_primary)
            {
            case TYPE_UNKNOWN:
            case TYPE_ANY:
                /* Can't do anything here... */
                return ref_lpctype(member);

            case TYPE_STRING:
            case TYPE_BYTES:
                if (can_be_array_or_string)
                {
                    /* Indexing a string, add int to the result. */
                    lpctype_t *oldresult = result;
                    result = get_union_type(result, lpctype_int);
                    free_lpctype(oldresult);
                }
                break;

            case TYPE_MAPPING:
                if (can_be_mapping)
                {
                    /* Values can be anything... */
                    free_lpctype(result);
                    result = lpctype_mixed;
                }
                break;

            default:
                /* All other cases we ignore, they cannot be indexed. */
                break;
            }
            break;

        case TCLASS_ARRAY:
            if (can_be_array_or_string)
            {
                /* Add the array member type to the result. */
                if (member->t_array.element == lpctype_unknown)
                    found_empty_array = true;
                else
                {
                    lpctype_t *oldresult = result;
                    result = get_union_type(result, member->t_array.element);
                    free_lpctype(oldresult);
                }
            }
            break;

        default:
            /* All other types can't be indexed. Ignoring. */
            break;
        }

        if (head->t_class == TCLASS_UNION)
            head = head->t_union.head;
        else
            break;
    }

    if (!result)
    {
        if (found_empty_array)
        {
            yyerror("Indexing an empty array");
            return lpctype_mixed;
        }
        else if (exact_types)
            lpctype_error("Bad type to index", aggregate);
        return ref_lpctype(error_type);
    }

    return result;
} /* get_index_result_type() */

/*-------------------------------------------------------------------------*/
static lpctype_t*
get_flattened_type (lpctype_t* t)

/* Determines the result type of argument flattening.
 *
 * The result is the union of all non-array types in <t> and the type
 * of all elements of arrays in <t>.
 */
{
    lpctype_t *result = NULL;
    lpctype_t* head = t;

    if (t == NULL)
        return NULL;

    /* Let's walk through all types in <t>. */
    while (true)
    {
        lpctype_t *oldresult = result;
        lpctype_t *member = head->t_class == TCLASS_UNION ? head->t_union.member : head;
        lpctype_t *add = NULL;

        if (member->t_class == TCLASS_ARRAY)
            add = member->t_array.element;
        else
            add = member;

        result = get_union_type(result, add);
        free_lpctype(oldresult);

        if (head->t_class == TCLASS_UNION)
            head = head->t_union.head;
        else
            break;
    }

    return result;

} /* get_flattened_type() */

/*-------------------------------------------------------------------------*/
static bool
check_assignment_types (fulltype_t src, lpctype_t *dest)

/* Check the types for an assignment of <src> into <dest>.
 * Return true, if the assignment is okay, false otherwise.
 */

{
    if (check_unknown_type(src.t_type))
        return true;

    /* When we have literal, the source must fit wholly. */
    if (src.t_flags & TYPE_MOD_LITERAL)
        return lpctype_contains(src.t_type, dest);
    else
        return has_common_type(src.t_type, dest);
} /* check_assignment_types() */

/*-------------------------------------------------------------------------*/
static void
check_function_call_types (fulltype_t *aargs, int num_aarg, function_t *funp, lpctype_t **dargs)

/* Checks the actual function arguments (<aargs> with <num_aarg> entries)
 * against the function definition <funp> with <dargs> argument type list.
 */

{
    int num_darg = funp->num_arg;
    bool missingargs = false;

    if (funp->flags & TYPE_MOD_XVARARGS)
        num_darg--; /* last argument is checked separately */

    if (num_darg > num_aarg)
    {
        num_darg = num_aarg;
        missingargs = true;
    }

    for (int argno = 1; argno <= num_darg; argno++)
    {
        if (!check_assignment_types(*aargs, *dargs))
        {
            yyerrorf("Bad type for argument %d of %s %s",
                argno,
                get_txt(funp->name),
                get_two_lpctypes(*dargs, aargs->t_type));
        }

        aargs++;
        dargs++;
    } /* for (all args) */

    if ((funp->flags & TYPE_MOD_XVARARGS) && !missingargs)
    {
        lpctype_t *flat_type = get_flattened_type(*dargs);

        for (int argno = num_darg+1; argno <= num_aarg; argno++)
        {
            if (!check_assignment_types(*aargs, flat_type))
            {
                yyerrorf("Bad type for argument %d of %s %s",
                    argno,
                    get_txt(funp->name),
                    get_two_lpctypes(flat_type, aargs->t_type));
            }

            aargs++;
        }

        free_lpctype(flat_type);
    } /* if (xvarargs) */
} /* check_function_call_types() */

/*-------------------------------------------------------------------------*/
static funflag_t
check_visibility_flags (funflag_t flags, funflag_t default_vis, bool function)

/* Checks the given visibility flags.
 *
 * Checks whether only one visibility modifier was given.
 * If no modifier was given use <default_vis> instead.
 * TYPE_MOD_VISIBLE will be removed.
 * If <function> is true, then TYPE_MOD_STATIC is also considered.
 */
{
    funflag_t mask = TYPE_MOD_PRIVATE | TYPE_MOD_PROTECTED | TYPE_MOD_PUBLIC | TYPE_MOD_VISIBLE;
    int num_modifier = 0;

    if(function)
        mask |= TYPE_MOD_STATIC;

    for (funflag_t i = 1; i < mask; i <<= 1)
        if (flags & mask & i)
            num_modifier++;

    if (num_modifier == 0)
        flags |= default_vis;
    else if (num_modifier > 1)
    {
        yyerrorf("Multiple visibility modifier given: %s"
            , get_f_visibility(flags & mask));

        /* Remove them all to avoid confusion till compilation aborts. */
        flags &= ~mask;
    }

    return flags & ~TYPE_MOD_VISIBLE;
} /* check_visibility_flags() */

/*-------------------------------------------------------------------------*/
static INLINE void
i_add_arg_type (fulltype_t type)

/* Add another function argument type to the argument type stack.
 * The reference of <type> is adopted.
 */

{
    mem_block_t *mbp = &type_of_arguments;

    if (mbp->current_size + sizeof type > mbp->max_size)
    {
        mbp->max_size *= 2;
        mbp->block = rexalloc((char *)mbp->block, mbp->max_size);
    }

    *(fulltype_t*)(mbp->block + mbp->current_size) = type;
    mbp->current_size += sizeof(fulltype_t);
} /* i_add_arg_type() */

#define add_arg_type(t) i_add_arg_type(t)

/*-------------------------------------------------------------------------*/
static INLINE void
pop_arg_stack (int n)

/* Pop (remove) the last <n> types from the argument stack.
 */

{
    fulltype_t * vp;

    vp = (fulltype_t*)(type_of_arguments.block + type_of_arguments.current_size);
    while (n > 0)
    {
        type_of_arguments.current_size -= sizeof (fulltype_t);
        n--;
        vp--;

        free_fulltype(*vp);
    }
} /* pop_arg_stack() */

/*-------------------------------------------------------------------------*/
static INLINE fulltype_t *
get_argument_types_start (int n)

/* Get the type of the <n>th last argument from the stack.
 * <n> must be >= 1.
 */

{
    return
        &((fulltype_t *)
         (type_of_arguments.block + type_of_arguments.current_size))[ - n];
} /* get_arguments_type_start() */

/*-------------------------------------------------------------------------*/
static INLINE fulltype_t
get_aggregate_type (int n)

/* The last <n> types on the argument stack are an aggregate type.
 * Combine the single types to a union type and remove them
 * from the argument stack.
 */

{
    fulltype_t *argp;
    lpctype_t *result = NULL;
    typeflags_t flag = TYPE_MOD_LITERAL;
    /* Note that TYPE_MOD_LITERAL is only kept if all values are literals,
     * otherwise the type can be broader and the flag must be removed.
     */

    argp = (fulltype_t *) (type_of_arguments.block +
          (type_of_arguments.current_size -= sizeof (fulltype_t) * n) );

    while (--n >= 0 )
    {
        lpctype_t *oldresult = result;

        result = get_union_type(result, argp->t_type);
        flag &= argp->t_flags;

        free_lpctype(oldresult);
        free_fulltype(*argp);
        argp++;
    }

    if (result)
    {
        lpctype_t *arrtype = get_array_type(result);
        free_lpctype(result);
        return get_fulltype_flags(arrtype, flag);
    }

    return get_fulltype_flags(lpctype_unknown_array, TYPE_MOD_LITERAL);

} /* get_aggregate_type() */


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
static int
ins_f_code_buf (unsigned int b, bytecode_p buf)

/* Add the instruction <b> to the <buf>, taking care of encoding
 * multi-byte instructions properly. Returns the number of
 * bytes written. The buffer must have space for at least 2 bytes.
 */

{
    int pos = 0;
    if (instrs[b].prefix)
        buf[pos++] = instrs[b].prefix;
    buf[pos++] = instrs[b].opcode;

    return pos;
} /* ins_f_code_buf() */

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
#ifndef FLOAT_FORMAT_2
static void
ins_uint32 (uint32_t l)
 // Add the uint32_t <l> to the A_PROGRAM area.
 // Currently only used when not using FLOAT_FORMAT_2.
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
 } // ins_uint32()
#endif // FLOAT_FORMAT_2
/*-------------------------------------------------------------------------*/
static void
ins_double (double d)
/* Add the double <d> to the A_PROGRAM area.
 */
{
    if (realloc_a_program(sizeof(double)))
    {
        put_double(PROGRAM_BLOCK + CURRENT_PROGRAM_SIZE, d);
        CURRENT_PROGRAM_SIZE += sizeof(double);
    }
    else
    {
        yyerrorf("Out of memory: program size %"PRIuMPINT"\n"
                 , CURRENT_PROGRAM_SIZE + sizeof(double));
    }
} /* ins_double() */
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
#ifndef FLOAT_FORMAT_2
static void
ins_uint16 (uint16_t l)
 // Add the uint16_t <l> to the A_PROGRAM area.
 // Currently only used when not using FLOAT_FORMAT_2.
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
 } // ins_uint16()
#endif // FLOAT_FORMAT_2

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

/* ========================   LVALUE CODE   ======================== */
lvalue_block_t
alloc_lvalue_block (p_int size)

/* Creates an empty lvalue block of the given size.
 */

{
    lvalue_block_t result = { LVALUE_BLOCK_SIZE, size };
    if (!extend_mem_block(A_LVALUE_CODE, size))
        result.size = 0;

    return result;
} /* alloc_lvalue_block() */

/*-------------------------------------------------------------------------*/
lvalue_block_t
compose_lvalue_block (lvalue_block_t previous_block, int post_lvalue_instruction, p_int argument_start, int final_instruction)

/* Creates an lvalue code block with the following elements:
 *
 * 1. A previous lvalue block <previous_block>.
 *    If its .size is 0, then it is ignored.
 * 2. Following an instruction (if 0, then ignored)
 * 3. Code from the PROGRAM_BLOCK starting from <argument_start>
 *    to its end (if -1, then ignored).
 * 4. A final instruction (if 0, then ignored)
 */

{
    lvalue_block_t result = previous_block;

    p_int size_to_add = 0;

    if (post_lvalue_instruction)
    {
        size_to_add++;
        if (instrs[post_lvalue_instruction].prefix)
            size_to_add++;
    }

    if (final_instruction)
    {
        size_to_add++;
        if (instrs[final_instruction].prefix)
            size_to_add++;
    }

    if (argument_start >= 0)
         size_to_add += CURRENT_PROGRAM_SIZE - argument_start;

    if (previous_block.size == 0)
    {
        /* No previous block, start a new one at the end. */
        result.start = LVALUE_BLOCK_SIZE;
    }
    else if(previous_block.start + previous_block.size != LVALUE_BLOCK_SIZE)
    {
        /* If we can't append, we need to copy <previous_block> */
        if (!reserve_mem_block(A_LVALUE_CODE, previous_block.size + size_to_add))
            return result;

        result.start = LVALUE_BLOCK_SIZE;
        memcpy(LVALUE_BLOCK + LVALUE_BLOCK_SIZE, LVALUE_BLOCK + previous_block.start, previous_block.size);
        LVALUE_BLOCK_SIZE += previous_block.size;
    }
    else
    {
        /* We can append. */
    }

    if (!reserve_mem_block(A_LVALUE_CODE, size_to_add))
        return result;

    /* Add the first instruction. */
    if (post_lvalue_instruction)
        LVALUE_BLOCK_SIZE += ins_f_code_buf(post_lvalue_instruction, LVALUE_BLOCK + LVALUE_BLOCK_SIZE);

    /* Add the arguments. */
    if (argument_start >= 0)
    {
        p_int len = CURRENT_PROGRAM_SIZE - argument_start;
        memcpy(LVALUE_BLOCK + LVALUE_BLOCK_SIZE, PROGRAM_BLOCK + argument_start, len);
        LVALUE_BLOCK_SIZE += len;
    }

    /* Add the final instruction. */
    if (final_instruction)
        LVALUE_BLOCK_SIZE += ins_f_code_buf(final_instruction, LVALUE_BLOCK + LVALUE_BLOCK_SIZE);

    result.size += size_to_add;
    return result;
} /* make_lvalue_block() */

/*-------------------------------------------------------------------------*/
void
free_lvalue_block (lvalue_block_t block)

/* If the given block is at the end of the mempool for lvalue code,
 * then remove that block. Otherwise just ignore it, the whole
 * mempool will be freed at the end of the compilation.
 */

{
    if (block.start + block.size == LVALUE_BLOCK_SIZE)
        LVALUE_BLOCK_SIZE -= block.size;
} /* free_lvalue_block() */


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

    while (current_number_of_locals > 0 && local_variables)
    {
        current_number_of_locals--;
        free_fulltype(local_variables[current_number_of_locals].type);
    }

    /* Free also types of context variables. */
    if (context_variables && context_variables != &(LOCAL_VARIABLE(0)))
    {
        int i;
        for (i=0; i<MAX_LOCAL; i++)
            free_fulltype(context_variables[i].type);
    }

    all_locals = NULL;
    current_number_of_locals = 0;
    max_number_of_locals = 0;
    current_break_stack_need = 0;
    max_break_stack_need = 0;
    def_function_ident = NULL;
    global_variable_initializing = NULL;
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
        if (q->u.local.context >= 0)
        {
            if (pragma_warn_unused_variables)
                warn_variable_usage(q->name, context_variables[q->u.local.context].usage, "Context");

            free_fulltype(context_variables[q->u.local.context].type);
        }
        else
        {
            current_number_of_locals--;

            if (pragma_warn_unused_variables)
                warn_variable_usage(q->name, local_variables[current_number_of_locals].usage, "Local");

            free_fulltype(local_variables[current_number_of_locals].type);
        }
        free_shared_identifier(q);
    }
} /* free_local_names() */

/*-------------------------------------------------------------------------*/
static ident_t *
add_local_name (ident_t *ident, fulltype_t type, int depth)

/* Declare a new local variable <ident> with the type <type> on
 * the scope depth <depth>. The references of <type> ARE adopted.
 * Return the (adjusted) ident for the new variable.
 */

{
    if (type.t_type == lpctype_void)
    {
        yyerrorf( "Illegal to define variable '%s' as type 'void'"
                , get_txt(ident->name));
    }

    if (current_number_of_locals >= MAX_LOCAL) /* size of type recording array */
        yyerror("Too many local variables");

    else
    {
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
        ident->u.local.initializing = false;

        /* Put the ident into the list of all locals */
        if (all_locals && all_locals->u.local.depth > depth)
            fatal("List of locals clobbered: depth %d, adding depth %d\n"
                 , all_locals->u.local.depth, depth);
        ident->next_all = all_locals;
        all_locals = ident;

        /* Record the type */
        local_variables[current_number_of_locals].usage = VAR_USAGE_NONE;
        local_variables[current_number_of_locals].type = type;
        current_number_of_locals++;

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
 * of <type> ARE adopted (and freed on error, except fatal ones).
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
        free_fulltype(type);
    }
    else
    {
        ident = add_local_name(ident, type, depth);
    }

    return ident;
} /* redeclare_local() */

/*-------------------------------------------------------------------------*/
static ident_t *
add_context_name (inline_closure_t *closure, ident_t *ident, lpctype_t *type, int num)

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
    block = & block_scope[depth-1]; /* The context block scope. */

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
        if (ident->type != I_TYPE_UNKNOWN)
        {
            /* We're overlaying some other definition, but that's ok.
             */
            ident = insert_shared_identifier_mstr(ident->name, I_TYPE_LOCAL, depth);
            assert (ident->type == I_TYPE_UNKNOWN);
        }

        /* Initialize the ident */
        ident->type = I_TYPE_LOCAL;
        ident->u.local.depth = depth;
        ident->u.local.initializing = false;
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
        context_variables[block->num_locals].type = get_fulltype(ref_lpctype(type));

        if (num < 0)
        {
            /* Independently defined context variable. */
            context_variables[block->num_locals].usage = VAR_USAGE_NONE;
            local_variables[ident->u.local.num].type = get_fulltype(ref_lpctype(type));
            local_variables[ident->u.local.num].usage = VAR_USAGE_NONE;
        }
        else
        {
            /* Inherited context variable. */
            context_variables[block->num_locals].usage = VAR_USAGE_WRITE;
        }

        block->num_locals++;
    }

    return ident;
} /* add_context_name() */

/*-------------------------------------------------------------------------*/
static ident_t *
check_for_context_local (ident_t *ident, lpctype_t ** pType)

/* The LPC code uses local variable <ident>. If we're compiling
 * an inline closure, check if it is an inherited local for which
 * no context variable has been created yet. If yes, create the context
 * variable.
 * Return the (possibly updated) ident, and store the variables type
 * in *<pType> (the reference count is not increased).
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
        A_LOCAL_VARIABLES_t *save_context_variables = context_variables;
        mp_int closure_nr;
        lpctype_t* type;

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
            type = LOCAL_VARIABLE(closure->full_context_var_start
                              + ident->u.local.context
                             ).type.t_type;
        }
        else
        {
            /* It's a local variable. */
            type = LOCAL_VARIABLE(closure->full_local_var_start
                              + ident->u.local.num
                             ).type.t_type;
        }

        /* Now pass this context variable through
         * all surrounding inline closures.
         */
        while (MY_TRUE)
        {
            inline_closure_t *next_closure = (closure->next >= 0) ? &(INLINE_CLOSURE(closure->next)) : NULL;

            /* Skip closures whose context is being parsed,
             * because current_inline is not created
             * in their runtime.
             */
            if (!closure->parse_context)
            {
                /* add_context_name() expects type_of_context to point to the
                 * closure's block.
                 */
                if (next_closure)
                    context_variables = &(LOCAL_VARIABLE(next_closure->full_context_var_start));
                else
                    context_variables = save_context_variables;

                if (ident->u.local.context >= 0)
                    LOCAL_VARIABLE(closure->full_context_var_start + ident->u.local.context).usage |= VAR_USAGE_READ;
                else
                    LOCAL_VARIABLE(closure->full_local_var_start + ident->u.local.num).usage |= VAR_USAGE_READ;

                ident = add_context_name(closure, ident, type,
                     ident->u.local.context >= 0
                     ? CONTEXT_VARIABLE_BASE + ident->u.local.context
                     : ident->u.local.num);
            }

            if (!next_closure)
                break;

            closure = next_closure;
        }

        context_variables = save_context_variables;

        *pType = type;
    }
    else if (ident->u.local.context >= 0)
        *pType = context_variables[ident->u.local.context].type.t_type;
    else
        *pType = local_variables[ident->u.local.num].type.t_type;

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
            context_variables[q->u.local.context].usage |= local_variables[q->u.local.num].usage;

            free_fulltype(local_variables[q->u.local.num].type);
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
const char *
get_current_function_name()

/* Get the name of the function currently being defined. If there is
 * no such function, return NULL.
 */
{
    if (def_function_ident)
        return get_txt(def_function_ident->name);
    else
        return NULL;
}

/*-------------------------------------------------------------------------*/
static typeflags_t
read_current_function_arg_flags()

/* Get the flags for the current function call argument
 * according to function_call_info[argument_level]. Also
 * advances the pointer in function_call_info[argument_level]
 * to the next argument.
 */

{
    typeflags_t flags = 0;

    int remaining_arg_types = function_call_info[argument_level].remaining_arg_types;
    if (remaining_arg_types != 0)
    {
        /* Collect the argument flags. */
        fulltype_t *args = function_call_info[argument_level].arg_types;
        if (!args)
            return flags;

        while (args->t_type != NULL)
        {
            flags |= args->t_flags;
            args++;
        }

        /* Advance to the next, unless this is the last and can be repeated. */
        args++;
        if (remaining_arg_types > 1)
        {
            function_call_info[argument_level].remaining_arg_types--;
            function_call_info[argument_level].arg_types = args;
        }
        else if (args->t_type != NULL)
            function_call_info[argument_level].arg_types = args;
    }

    return flags;
} /* read_current_function_arg_flags() */

/*-------------------------------------------------------------------------*/
static unsigned short
store_argument_types ( int num_arg )

/* Store the <num_arg> argument types from global local_variables[] into
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
    if (!exact_types || arg_types_exhausted)
    {
        argument_start_index = INDEX_START_NONE;
    }
    else
    {
        /* Save the argument types.
         */
        argument_start_index = ARGTYPE_COUNT;
        if (num_arg + argument_start_index > 1 + (long)USHRT_MAX)
        {
            arg_types_exhausted = true;
            argument_start_index = INDEX_START_NONE;

            yywarnf("Type buffer exhausted, cannot store and verify argument types.");
        }
        else
        {
            int i;

            for (i = 0; i < num_arg; i++)
            {
                ADD_ARGUMENT_TYPE(ref_lpctype(local_variables[i].type.t_type));
            }
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
 * completed. Explicit prototypes can cause additional calls.
 */

{
    int num;
    function_t fun;
    unsigned short argument_start_index;

    /* Move the visibility-info into flags */
    flags |= type.t_flags & ~TYPE_MOD_MASK;

    do {
        function_t *funp;
        Bool args_differ, compare_args;

        if (p->type != I_TYPE_GLOBAL) break;
        if ((num = p->u.global.function) == I_GLOBAL_FUNCTION_OTHER) break;

        funp = FUNCTION(num);

        if ((funp->flags & (NAME_INHERITED|TYPE_MOD_PRIVATE|NAME_HIDDEN))
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
            if (exact_types && funp->type != lpctype_unknown)
            {
                lpctype_t *new_type, *old_type;

                // get old+new functions flags
                funflag_t new_fflags = flags;
                funflag_t old_fflags = funp->flags;

                // We first check the return types for consistency.
                // If the new function has no type, it will be handled as lpctype_mixed.
                if (type.t_type)
                    new_type = type.t_type;
                else
                    new_type = lpctype_mixed;
                old_type = funp->type;
                if (old_fflags & NAME_INHERITED)
                {
                    // If the existing function was inherited, we (only) require type compatibility
                    if (!has_common_type(new_type, old_type))
                    {
                        if (pragma_pedantic)
                            yyerrorf("Inconsistent declaration of '%s': Return type mismatch %s", get_txt(p->name), get_two_lpctypes(old_type, new_type));
                        else if (pragma_check_overloads)
                            yywarnf("Inconsistent declaration of '%s': Return type mismatch %s", get_txt(p->name), get_two_lpctypes(old_type, new_type));
                    }
                }
                else
                {
                    // In all other cases we require that types are identical.
                    // (All protoypes+definitions in one file should be consistent.
                    if (new_type != old_type)
                    {
                        yyerrorf("Inconsistent declaration of '%s': Return type mismatch %s", get_txt(p->name), get_two_lpctypes(old_type, new_type));
                    }
                }

                // Then check the number of arguments and varargs flags and determine
                // if we should check the argument types (later).

                // For a new declaration of a prototype we require the same arguments.
                if (!(old_fflags & NAME_INHERITED)
                 && ((funp->num_arg != num_arg)
                  || ((old_fflags ^ new_fflags) & TYPE_MOD_XVARARGS)))
                    yyerrorf("Incorrect number of arguments in redeclaration of '%s'", get_txt(p->name));
                // For a redefinition of an inherited function the number of non-optional
                // arguments of the original function must not exceed the number of all
                // arguments of the new function.
                else if ((old_fflags & NAME_INHERITED)
                 && !(old_fflags & TYPE_MOD_VARARGS)  // No non-optional arguments?
                 && !(new_fflags & TYPE_MOD_XVARARGS) // Arbitrary number of arguments?
                 && (funp->num_arg - funp->num_opt_arg - ((old_fflags & TYPE_MOD_XVARARGS) ? 1 : 0)) > num_arg)
                    yyerrorf("Incorrect number of arguments in redefinition of '%s'", get_txt(p->name));
                else
                {
                    unsigned short first_arg;

                    first_arg = ARGUMENT_INDEX(num);
                    if (first_arg == INDEX_START_NONE)
                    {
                        if (num_arg && !(funp->flags & NAME_TYPES_LOST) )
                            yyerrorf(
                              "Redefined function '%s' not compiled with type testing"
                            , get_txt(p->name));
                    }
                    else
                    {
                        /* We can compare the arguments */
                        compare_args = MY_TRUE;
                    }
                } /* cases (number of arguments) */

                /* If it's a prototype->function redefinition, check if the
                 * visibility is conserved. For redefining inherited functions
                 * different visibility is OK.
                 */
#               define TYPE_MOD_VIS \
                        ( TYPE_MOD_NO_MASK \
                        | TYPE_MOD_PRIVATE | TYPE_MOD_PUBLIC \
                        |TYPE_MOD_STATIC | TYPE_MOD_PROTECTED)

                if (!(old_fflags & (NAME_INHERITED|NAME_TYPES_LOST))
                    && ((new_fflags ^ old_fflags) & TYPE_MOD_VIS)
                    )
                {
                    char buff[120];
                    strncpy(buff, get_f_visibility(old_fflags), sizeof(buff)-1);
                    buff[sizeof(buff) - 1] = '\0'; // strncpy() does not guarantee NUL-termination
                    if (pragma_pedantic)
                        yyerrorf("Inconsistent declaration of '%s': Visibility changed from '%s' to '%s'"
                                , get_txt(p->name), buff, get_visibility(type));
                    else
                        yywarnf("Inconsistent declaration of '%s': Visibility changed from '%s' to '%s'"
                                , get_txt(p->name), buff, get_visibility(type));
                }
#               undef TYPE_MOD_VIS

                /* Check if the 'varargs' attribute is conserved. */
                if ((old_fflags ^ flags) & TYPE_MOD_VARARGS
                    &&  old_fflags & TYPE_MOD_VARARGS
                   )
                {
                    // this is a warning in case of re-defining inherited functions
                    // with pedantic, but always an error when prototype->definition
                    if (old_fflags & NAME_INHERITED)
                    {
                        if (pragma_check_overloads)
                            yywarnf("Redefinition of '%s' loses 'varargs' modifier"
                                    , get_txt(p->name));
                    }
                    else
                        yyerrorf("Inconsistent declaration of '%s': 'varargs' modifier lost"
                                , get_txt(p->name));
                }

                /* Check that the two argument lists are compatible */
                if (compare_args)
                {
                    int i;
                    unsigned short first_arg;
                    lpctype_t **argp;
                    int num_args = num_arg;

                    first_arg = ARGUMENT_INDEX(num);
                    argp = GET_BLOCK(A_ARGUMENT_TYPES) + first_arg;

                    if ((new_fflags & TYPE_MOD_XVARARGS) && (old_fflags & NAME_INHERITED))
                        num_args--; /* Compare our varargs argument later. */

                    for (i = 0; i < num_args; i++ )
                    {
                        new_type = local_variables[i].type.t_type;

                        if ((old_fflags & TYPE_MOD_XVARARGS) && i >= funp->num_arg-1)
                        {
                            /* We are comparing against a varargs argument. */
                            if (i == num_arg-1 && (new_fflags & TYPE_MOD_XVARARGS))
                                old_type = ref_lpctype(argp[funp->num_arg-1]);
                            else
                                old_type = get_flattened_type(argp[funp->num_arg-1]);
                        }
                        else if (i >= funp->num_arg)
                            break;
                        else
                            old_type = ref_lpctype(argp[i]);

                        if (new_type != old_type)
                        {
                            args_differ = MY_TRUE;
                            // If it is a redefinition of an inherited function, it might be OK,
                            // if the two arguments are at least compatible.
                            // But if it's a prototype->function redefinition, this is now a error,
                            // because prototype + definition should always be the same.
                            if (old_fflags & NAME_INHERITED)
                            {
                                if (!has_common_type(new_type, old_type))
                                {
                                    if (pragma_pedantic)
                                        yyerrorf("Argument type mismatch in redefinition of '%s': arg %d %s"
                                            , get_txt(p->name), i+1, get_two_lpctypes(new_type, old_type));
                                    else if (pragma_check_overloads)
                                        yywarnf("Argument type mismatch in redefinition of '%s': arg %d %s"
                                            , get_txt(p->name), i+1, get_two_lpctypes(new_type, old_type));
                                }
                            }
                            else
                            {
                                yyerrorf("Inconsistent declaration of '%s': argument type mismatch in definition: arg %d %s"
                                         , get_txt(p->name), i+1, get_two_lpctypes(new_type, old_type));
                            }
                        }

                        free_lpctype(old_type);
                    } /* for (all args) */

                    if ((new_fflags & TYPE_MOD_XVARARGS) && (old_fflags & NAME_INHERITED))
                    {
                        /* Compare our varargs argument against the remainder
                         * of the old function's arguments.
                         */
                        new_type = get_flattened_type(local_variables[num_arg-1].type.t_type);

                        for (; i < funp->num_arg; i++)
                        {
                            old_type = argp[i];

                            if ((old_fflags & TYPE_MOD_XVARARGS) && i == funp->num_arg-1)
                            {
                                /* Both are varargs. */
                                free_lpctype(new_type);
                                new_type = ref_lpctype(local_variables[num_arg-1].type.t_type);
                            }

                            if (new_type != old_type)
                            {
                                args_differ = MY_TRUE;
                                if (!has_common_type(new_type, old_type))
                                {
                                    if (pragma_pedantic)
                                        yyerrorf("Argument type mismatch in redefinition of '%s': arg %d %s"
                                            , get_txt(p->name), i+1, get_two_lpctypes(new_type, old_type));
                                    else if (pragma_check_overloads)
                                        yywarnf("Argument type mismatch in redefinition of '%s': arg %d %s"
                                            , get_txt(p->name), i+1, get_two_lpctypes(new_type, old_type));
                                }
                            }
                        }

                        free_lpctype(new_type);
                    }

                } /* if (compare_args) */

            } /* if (exact_types && already defined) */

         } /* if (!complete) */

        /* Remember the heart_beat() function */
        if (mstreq(p->name, STR_HEART_BEAT))
            heart_beat = num;

        /* If it was yet another prototype,
         * just update its types and then return.
         */
        if (flags & NAME_PROTOTYPE)
        {
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
                    if (exact_types && !(funp->flags & NAME_TYPES_LOST))
                    {
                        /* Warn about type checks being turned off. */
                        if (pragma_pedantic)
                            yyerrorf("Multiple inconsistent declarations "
                                     "of '%s' encountered: "
                                     "Deactivating argument type checks."
                                    , get_txt(p->name)
                                    );
                        else
                            yywarnf("Multiple inconsistent declarations "
                                     "of '%s' encountered: "
                                     "Deactivating argument type checks."
                                    , get_txt(p->name)
                                    );
                    }

                    funp->num_arg = num_arg;
                    ARGUMENT_INDEX(num) = INDEX_START_NONE;
                    funp->flags |= NAME_TYPES_LOST;
                }
            }

            free_lpctype(funp->type);
            // If the function has no type, it implicitly will be lpctype_mixed from
            // now on.
            if (type.t_type)
                funp->type = ref_lpctype(type.t_type);
            else
                funp->type  = lpctype_mixed; // static, no need to reference them.

            return num;
        }  // end of prototype update

        /* This is the completion of an earlier prototype:
         * now flesh out the function structure.
         */

        if (funp->flags & NAME_INHERITED) /* We didn't adopt the reference yet. */
            ref_mstring(funp->name);

        funp->num_locals = num_local;
        funp->flags = flags;
        funp->offset.pc = offset;

        /* That's it */
        return num;

    } while(0); /* Test and handle for already defined functions */

    /* It's a new function! */

    if (mstreq(p->name, STR_HEART_BEAT))
        heart_beat = FUNCTION_COUNT;

    /* Fill in the function_t */
    fun.name      = p->name;
    fun.offset.pc = offset;
    fun.flags     = flags;
    fun.num_arg   = num_arg;
    fun.num_opt_arg = 0;       /* will be updated later */
    fun.num_locals= num_local; /* will be updated later */
    // If the function has no type, it implicitly will be lpctype_mixed from
    // now on. Background: fun.type being NULL is a nasty source of NULL pointer
    // dereferences because this is apparantly the instance a type can be NULL
    // and is sometimes forgotten.
    if (type.t_type)
        fun.type  = ref_lpctype(type.t_type);
    else
        fun.type  = lpctype_mixed; // static, no need to reference them.
    ref_mstring(fun.name);

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
    ADD_FUNCTION(&fun);

    /* Store the function arguments, if required,
     * and save the position of the argument types.
     */
    argument_start_index = store_argument_types(num_arg);
    ADD_ARGUMENT_INDEX(argument_start_index);

    return num;
} /* define_new_function() */

/*-------------------------------------------------------------------------*/
static void
check_variable_redefinition (ident_t *name, typeflags_t flags)

/* Checks whether the redefinition of variable <name> with one having
 * <flags> is okay. Throws compile errors otherwise.
 */

{
    int n = name->u.global.variable;
    typeflags_t vn_flags = VARIABLE(n)->type.t_flags;

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
} /* check_variable_redefinition */

/*-------------------------------------------------------------------------*/
static ident_t *
define_variable (ident_t *name, fulltype_t type)

/* Define a new global variable <name> of type <type>.
 * The references of <type> are NOT adopted.
 * Return the identifier for the variable.
 */

{
    variable_t dummy;
    typeflags_t flags = type.t_flags;
    int n;

    if (type.t_type == lpctype_void)
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
          && (name->u.global.efun != I_GLOBAL_EFUN_OTHER
           || name->u.global.sim_efun != I_GLOBAL_SEFUN_OTHER
#ifdef USE_PYTHON
           || is_python_efun(name)
#endif
             )
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
    n = name->u.global.variable;
    if (n != I_GLOBAL_VARIABLE_OTHER && n != I_GLOBAL_VARIABLE_FUN)
    {
        typeflags_t vn_flags = VARIABLE(n)->type.t_flags;

        check_variable_redefinition(name, flags);

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
            VARIABLE(n)->type.t_flags = vn_flags;
        }
    }

    type.t_flags = flags;

    dummy.name = ref_mstring(name->name);
    dummy.type = ref_fulltype(type);

    if (flags & TYPE_MOD_VIRTUAL)
    {
        if (!(flags & NAME_HIDDEN))
            name->u.global.variable = VIRTUAL_VAR_TAG | V_VARIABLE_COUNT;
        ADD_VIRTUAL_VAR(&dummy);
    }
    else
    {
        if (!(flags & NAME_HIDDEN))
            name->u.global.variable = NV_VARIABLE_COUNT;
        ADD_VARIABLE(&dummy);
        ADD_GLOBAL_VARIABLE_INFO((global_variable_t){.usage = VAR_USAGE_NONE});
    }

    return name;
} /* define_variable() */

/*-------------------------------------------------------------------------*/
static void
redeclare_variable (ident_t *name, fulltype_t type, int n)

/* The variable <name> is inherited virtually with number <n>.
 * Adjust its modifier accordingly. The pure type shouldn't
 * have changed. The references of <type> are NOT adopted.
 */

{
    typeflags_t flags = type.t_flags;
    typeflags_t varflags;
    variable_t *variable;

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
          && (name->u.global.efun != I_GLOBAL_EFUN_OTHER
           || name->u.global.sim_efun != I_GLOBAL_SEFUN_OTHER
#ifdef USE_PYTHON
           || is_python_efun(name)
#endif
             )
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

    if (flags & TYPE_MOD_NOSAVE)
    {
        /* 'nosave' is internally saved as 'static' (historical reason) */
        flags |= TYPE_MOD_STATIC;
        flags ^= TYPE_MOD_NOSAVE;
    }

    if (name->u.global.variable != I_GLOBAL_VARIABLE_OTHER
     && name->u.global.variable != I_GLOBAL_VARIABLE_FUN
     && name->u.global.variable != n)
    {
        check_variable_redefinition(name, flags);
    }

    name->u.global.variable = n;

    variable = V_VARIABLE(n);
    varflags = variable->type.t_flags;

    assert(variable->name == name->name);
    assert(variable->type.t_type == type.t_type);

    /* The most visible modifier wins here. */
    if ((flags|varflags) & TYPE_MOD_PUBLIC)
    {
        varflags &= ~(TYPE_MOD_PRIVATE | TYPE_MOD_PROTECTED | NAME_HIDDEN);
        varflags |= TYPE_MOD_PUBLIC;
    }
    else if (!(flags&(TYPE_MOD_PRIVATE|TYPE_MOD_PROTECTED)))
    {
        varflags &= ~(TYPE_MOD_PRIVATE | TYPE_MOD_PROTECTED | NAME_HIDDEN);
    }
    else if (!(varflags&(TYPE_MOD_PRIVATE|TYPE_MOD_PROTECTED)))
    {
        /* It's already visible. */
    }
    else if (flags & TYPE_MOD_PROTECTED)
    {
        varflags &= ~(TYPE_MOD_PRIVATE | NAME_HIDDEN);
        varflags |= TYPE_MOD_PROTECTED;
    }

    /* Preserve nosave only, if both of them have it. */
    if (!(flags & varflags & TYPE_MOD_STATIC))
        varflags &= ~TYPE_MOD_STATIC;

    /* If either of them is nomask, the resulting var is, too. */
    varflags |= flags & TYPE_MOD_NO_MASK;

    variable->type.t_flags = varflags;
} /* redeclare_variable() */

/*-------------------------------------------------------------------------*/
static ident_t*
get_initialized_variable (ident_t *p)

/* Check that <p> is a variable that is not just being initialized.
 * If it is an initializing variable, return the next outer variable or NULL,
 * if non exists. If <p> is not a variable at all, return also NULL.
 * If NULL is returned an error is thrown, also.
 */

{
    bool foundinitializing = false;
    ident_t *result = p;

    while (result)
    {
        switch (result->type)
        {
            case I_TYPE_LOCAL:
                if (result->u.local.initializing)
                {
                    foundinitializing = true;
                    result = result->inferior;
                    continue;
                }
                break;

            case I_TYPE_GLOBAL:
                if (result->u.global.variable != I_GLOBAL_VARIABLE_OTHER
                 && result->u.global.variable != I_GLOBAL_VARIABLE_FUN)
                {
                    if (global_variable_initializing == result)
                        foundinitializing = true;
                    else
                        break;
                }

                /* FALLTHROUGH */
            default:
                result = NULL;
                break;
        }

        break;
    }

    if (!result)
    {
        if (foundinitializing)
            yyerrorf("Variable %s not initialized", get_txt(p->name));
        else
            yyerrorf("Variable %s not declared", get_txt(p->name));
    }

    return result;
} /* get_initialized_variable() */

/*-------------------------------------------------------------------------*/
static int
define_global_variable (ident_t* name, fulltype_t actual_type, Bool with_init)

/* This is called directly from a parser rule: <type> <name>
 * if with_init is true, then an initialization of this variable will follow.
 * It creates the global variable and returns its index.
 */

{
    int i;

    variables_defined = MY_TRUE;

    actual_type.t_flags = check_visibility_flags(actual_type.t_flags, default_varmod, false);

    if (actual_type.t_flags & TYPE_MOD_VARARGS)
    {
        yyerror("can't declare a variable as varargs");
            actual_type.t_flags &= ~TYPE_MOD_VARARGS;
    }

    if (!pragma_share_variables)
        actual_type.t_flags |= VAR_INITIALIZED;

    name = define_variable(name, actual_type);
    i = name->u.global.variable;

#ifdef DEBUG
    if (name->type != I_TYPE_GLOBAL || i == I_GLOBAL_VARIABLE_OTHER)
        fatal("Variable not declared after defining it.\n");
#endif

    /* Initialize float values with 0.0. */
    if (with_init || actual_type.t_type == lpctype_float)
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
                VARIABLE(i)->type.t_flags |= VAR_INITIALIZED;

            /* Push the variable reference and create the assignment */

            if (i > 0xff)
            {
                add_f_code(F_PUSH_IDENTIFIER16_LVALUE);
                add_short(i);
                CURRENT_PROGRAM_SIZE += 1;
            }
            else
            {
                add_f_code(F_PUSH_IDENTIFIER_LVALUE);
                add_byte(i);
            }

            /* Ok, assign */
            add_f_code(F_VOID_ASSIGN);
            CURRENT_PROGRAM_SIZE += 4;
            add_new_init_jump();
        } /* PREPARE_INSERT() block */
        else
          global_variable_initializing = name;
    } /* if (float variable) */

    return i;
} /* define_global_variable() */

/*-------------------------------------------------------------------------*/
static void
init_global_variable (int i, ident_t* name, fulltype_t actual_type
                     , int assign_op, fulltype_t exprtype)

/* This is called directly from a parser rule: <type> <name> = <expr>
 * It will be called after the call to define_global_variable().
 * It assigns the result of <expr> to the variable.
 */

{
    add_type_check(actual_type.t_type, TYPECHECK_VAR_INIT);

    PREPARE_INSERT(4)

    global_variable_initializing = NULL;

    if (!(actual_type.t_flags & (TYPE_MOD_PRIVATE | TYPE_MOD_PUBLIC
                          | TYPE_MOD_PROTECTED)))
    {
        actual_type.t_flags |= default_varmod;
    }

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

    if (i > 0xff)
    {
        add_f_code(F_PUSH_IDENTIFIER16_LVALUE);
        add_short(i);
        CURRENT_PROGRAM_SIZE += 1;
    }
    else
    {
        add_f_code(F_PUSH_IDENTIFIER_LVALUE);
        add_byte(i);
    }

    /* Only simple assigns are allowed */
    if (assign_op != F_ASSIGN)
       yyerror("Illegal initialization");

    /* Do the types match? */
    actual_type.t_flags &= TYPE_MOD_MASK;

    if (!check_assignment_types(exprtype, actual_type.t_type))
    {
        yyerrorf("Type mismatch %s when initializing %s"
                , get_two_lpctypes(actual_type.t_type, exprtype.t_type)
                , get_txt(name->name));
    }

    /* Ok, assign */
    add_f_code(F_VOID_ASSIGN);
    CURRENT_PROGRAM_SIZE += 3;
    add_new_init_jump();

    use_variable(name, (exprtype.t_flags & TYPE_MOD_REFERENCE) ? VAR_USAGE_READWRITE : VAR_USAGE_WRITE);
} /* init_global_variable() */

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
    const program_t *inhprogp;
    int inhfx;
    function_t * header = get_function_header_extended(prog, ix, &inhprogp, &inhfx);

    fun_p->name = header->name;
    fun_p->type = ref_lpctype(header->type);

    fun_p->num_arg = header->num_arg;
    fun_p->num_opt_arg = header->num_opt_arg;
    if (is_undef_function(inhprogp->program + (inhprogp->functions[inhfx] & FUNSTART_MASK)))
        fun_p->flags |= NAME_UNDEFINED;
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

    returntype.t_flags = check_visibility_flags(returntype.t_flags, default_funmod, true);

    /* Require exact types? */
    exact_types = returntype.t_type;
    if (exact_types == NULL)
    {
        if (pragma_strict_types != PRAGMA_WEAK_TYPES)
            yyerrorf("\"#pragma %s_types\" requires type of function"
                    , pragma_strict_types == PRAGMA_STRICT_TYPES
                      ? "strict" : "strong" );
    }

    if (returntype.t_flags & TYPE_MOD_NOSAVE)
    {
        yyerror("can't declare a function as nosave");
        returntype.t_flags &= ~TYPE_MOD_NOSAVE;
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
    fulltype_t * returntype;
    int fun;

    if (is_inline)
    {
        ident = current_inline->ident;
        returntype = &current_inline->returntype;
    }
    else
    {
        ident = def_function_ident;
        returntype = &def_function_returntype;
    }

    /* We got the complete prototype: define it */

    if ( current_number_of_locals
     && (local_variables[current_number_of_locals-1].type.t_flags
         & TYPE_MOD_VARARGS)
       )
    {
        /* The last argument has to allow an array. */
        local_variable_t *t;

        returntype->t_flags |= TYPE_MOD_XVARARGS;

        t = local_variables + (current_number_of_locals-1);
        if (!lpctype_contains(lpctype_unknown_array, t->type.t_type))
        {
            yyerror("varargs parameter must be declared array or mixed");

            /* Keep the visibility, but change the type to 'mixed'.
             */
            free_lpctype(t->type.t_type);
            t->type.t_type = lpctype_mixed;
        }
    }

    /* Define a prototype. If it is a real function, then the
     * prototype will be updated below.
     */
    fun = define_new_function( MY_FALSE, ident, num_args, 0, 0
                             , NAME_UNDEFINED|NAME_PROTOTYPE
                             , *returntype);

    /* Store the data */
    if (is_inline)
    {
        current_inline->num_args = num_args;
        current_inline->function = fun;
    }
    else
    {
        def_function_num_args = num_args;
    }
} /* def_function_prototype() */

/*-------------------------------------------------------------------------*/
static void
def_function_complete (bool has_code, p_uint body_start, struct statement_s statements, bool is_inline)

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

    if (!has_code)
    {
        /* function_body was a ';' -> prototype
         * Just norm the visibility flags unless it is a prototype
         * for an already inherited function.
         */

        funflag_t *flagp;

        flagp = (funflag_t *)(&FUNCTION(ident->u.global.function)->flags);
        if (!(*flagp & NAME_INHERITED))
        {
            *flagp |= returntype.t_flags
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
        int num_vars = max_number_of_locals - num_args
                                            + max_break_stack_need;

        define_new_function(MY_TRUE, ident
                            , num_args
                            , num_vars
                            , body_start + FUNCTION_PRE_HDR_SIZE
                            , 0, returntype);

        /* Catch a missing return if the function has a return type */
        if (returntype.t_type != lpctype_void
         && (   returntype.t_type != lpctype_unknown
             || pragma_strict_types
            )
           )
        {
            /* Check if the statement block has a return, or
             * at least a non-continuing instruction.
             */
            if (!statements.may_finish)
            {
                /* Good. Just in case our information is wrong,
                 * insert a proper default return as well.
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

/*-------------------------------------------------------------------------*/
static bool
save_default_value (p_int start)

/* Save the default value code from the A_PROGRAM block (starting at <start>)
 * into the A_DEFAULT_VALUES block. Returns true on success.
 */

{
    if (CURRENT_PROGRAM_SIZE - start > USHRT_MAX)
    {
        yyerrorf("Too complex default value");
    }
    else if (reserve_mem_block(A_DEFAULT_VALUES, sizeof(unsigned short) + CURRENT_PROGRAM_SIZE - start))
    {
        put_short(DEFAULT_VALUES_BLOCK + DEFAULT_VALUES_BLOCK_SIZE, (unsigned short)(CURRENT_PROGRAM_SIZE - start));
        DEFAULT_VALUES_BLOCK_SIZE += sizeof(unsigned short);
        memcpy(DEFAULT_VALUES_BLOCK + DEFAULT_VALUES_BLOCK_SIZE, PROGRAM_BLOCK + start, CURRENT_PROGRAM_SIZE - start);
        DEFAULT_VALUES_BLOCK_SIZE += CURRENT_PROGRAM_SIZE - start;

        CURRENT_PROGRAM_SIZE = start;
        return true;
    }

    CURRENT_PROGRAM_SIZE = start;
    return false;
} /* save_default_value() */

/*-------------------------------------------------------------------------*/
static size_t
reserve_default_value_block (int num_opt, p_int offset)

/* Create space in the program for the default values of <num_opt> arguments.
 * <offset> is the offset into the DEFAULT_VALUES_BLOCK, where the
 * corresponding code is. The function returns the size of the reserved space.
 */

{
    size_t size = 0;
    bytecode_t *block;

    if (!num_opt)
        return 0;

    /* Insert the code for default values. */
    block = DEFAULT_VALUES_BLOCK + offset;
    for (int i = 0; i < num_opt; i++)
    {
        unsigned short chunk = get_short(block);
        block += sizeof(unsigned short) + chunk;
        size += chunk;
    }

    if (size > USHRT_MAX)
    {
        yyerrorf("Too many/complex default values.");
        return 0;
    }
    else
    {
        size += num_opt * sizeof(unsigned short);

        if (!realloc_a_program(size))
        {
            yyerrorf("Out of memory: program size %"PRIuMPINT"\n", CURRENT_PROGRAM_SIZE + size);
            return 0;
        }

        CURRENT_PROGRAM_SIZE += size;
    }

    return size;
} /* reserve_default_value_block() */

/*-------------------------------------------------------------------------*/
static p_int
copy_default_value_block (int num_opt, p_int offset, p_int start, size_t size)

/* Copy the code for initializing default values of <num_opt> arguments into
 * the program. <offset> is the offset into the DEFAULT_VALUES_BLOCK, where the
 * corresponding code is. <start> is the functions start address of the function,
 * <size> the size of the default value code block (as returned by
 * reserve_default_value_block()). Returns the new start address for the
 * function.
 */

{
    bytecode_t *funcode = PROGRAM_BLOCK;
    bytecode_t *src, *dst;
    p_int addr = start - size;
    p_int pos = 0;

    funcode += addr + FUNCTION_HDR_SIZE;
    dst = funcode + sizeof(unsigned short) * num_opt;
    src = DEFAULT_VALUES_BLOCK + offset;

    for (int i = 0; i < num_opt; i++)
    {
        unsigned short chunk = get_short(src);
        src += sizeof(unsigned short);

        pos += chunk;

        put_short(funcode + sizeof(unsigned short) * i, pos);
        memcpy(dst, src, chunk);
        src += chunk;
        dst += chunk;
    }

    /* We should end up at the original beginning of the function. */
    assert(start + FUNCTION_HDR_SIZE == dst - PROGRAM_BLOCK);

    /* We can now discard the default values block. */
    if (DEFAULT_VALUES_BLOCK + DEFAULT_VALUES_BLOCK_SIZE == src)
        DEFAULT_VALUES_BLOCK_SIZE = offset;

    return addr;
} /* copy_default_value_block() */

/* =============================   STRUCTS   ============================= */

/*-------------------------------------------------------------------------*/
static int
define_new_struct ( Bool proto, ident_t *p, const char * prog_name, funflag_t flags)

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
    if (p->type == I_TYPE_GLOBAL && (num = p->u.global.struct_id) != I_GLOBAL_STRUCT_NONE
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
    flags = check_visibility_flags(flags, 0, false);
    if (flags & TYPE_MOD_STATIC)
    {
        yyerror("Can't declare a struct as static");
        flags &= ~TYPE_MOD_STATIC;
    }
    if (flags & TYPE_MOD_VARARGS)
    {
        yyerror("Can't declare a struct as varargs");
        flags &= ~TYPE_MOD_VARARGS;
    }

    /* Fill in the struct_def_t */
    sdef.type  = struct_new_prototype(ref_mstring(p->name)
                                    , new_unicode_tabled(prog_name));
    sdef.flags = proto ? (flags | NAME_PROTOTYPE)
                       : (flags & ~NAME_PROTOTYPE);
    sdef.inh = -1;

    update_struct_type(sdef.type->name->lpctype, sdef.type);

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
    ADD_STRUCT_DEF(&sdef);

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

    if (p == NULL || p->u.global.struct_id == I_GLOBAL_STRUCT_NONE)
        return -1;
    if (STRUCT_DEF(p->u.global.struct_id).flags & NAME_HIDDEN)
        return -1;
    return p->u.global.struct_id;
} /* find_struct() */

/*-------------------------------------------------------------------------*/
static void
add_struct_member ( string_t *name, lpctype_t *type
                  , struct_type_t * from_struct )

/* Add a new member <name> with type <type> to A_STRUCT_MEMBERS for the
 * to the most recently defined struct <current_struct>.
 * If <from_struct> is not NULL, it is the type of the struct from
 * which the member is inherited.
 * Raise an error if a member of the same name already exists.
 * The references of <type> are NOT adopted.
 */

{
    struct_def_t *pdef;

    pdef = &STRUCT_DEF(current_struct);

    if (STRUCT_MEMBER_COUNT != 0)
    {
        /* Not the first member: check if the name already occurred */
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
        member.type = ref_lpctype(type);
        ADD_STRUCT_MEMBER(&member);
    }
} /* add_struct_member() */

/*-------------------------------------------------------------------------*/
static void
finish_struct ( int32 prog_id)

/* The definition for struct <current_struct> has been parsed completely,
 * now complete the struct type object with the A_STRUCT_MEMBERS data.
 */

{
    struct_def_t *pdef;
    struct_type_t *base;
    string_t *name, *prog_name;

    pdef = &STRUCT_DEF(current_struct);

    /* Retrieve the .base pointer so that the error handling won't
     * get confused about it.
     * Also get a safety copy of the name.
     */
    base = pdef->type->base;
    pdef->type->base = NULL;
    name = ref_mstring(struct_t_name(pdef->type));
    prog_name = ref_mstring(struct_t_pname(pdef->type));

    /* Fill in the prototype */
    pdef->type = struct_fill_prototype(pdef->type
                                      , prog_id
                                      , base
                                      , STRUCT_MEMBER_COUNT
                                      , &STRUCT_MEMBER(0)
                                      );

    if (pdef->type)
    {
        /* Success: Free the safety copies */
        free_mstring(name);
        free_mstring(prog_name);
    }
    else
    {
        /* Failure: Recreate the prototype as the old one got deleted */
        pdef->type = struct_new_prototype(name, prog_name);
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
 * Return TRUE on success, and FALSE if an error occurred (the caller will
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
        if (exact_types && length > 0)
        {
            for (member = 0, pmember = pdef->type->member, p = list
                ; member < length && member < struct_t_size(pdef->type)
                ; member++, pmember++, p = p->next
                )
            {
                if (!check_assignment_types(p->type, pmember->type))
                {
                    yyerrorf("Type mismatch %s for member '%s' "
                             "in struct '%s'"
                            , get_two_lpctypes(pmember->type, p->type.t_type)
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
        else if (exact_types
              && !check_assignment_types(p->type, pmember->type) )
        {
            yyerrorf("Type mismatch %s when initializing member '%s' "
                     "in struct '%s'"
                    , get_two_lpctypes(pmember->type, p->type.t_type)
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
get_struct_index (struct_name_t * pName)

/* Return the index of struct name <pName> in this program's A_STRUCT_DEFS.
 * Return -1 if not found.
 */

{
    short i;

    for (i = 0; (size_t)i < STRUCT_COUNT; i++)
    {
        if (STRUCT_DEF(i).type->name == pName)
            return i;
    }
    return -1;
} /* get_struct_index() */

/*-------------------------------------------------------------------------*/
static lpctype_t*
get_struct_member_result_type (lpctype_t* structure, string_t* member_name, bool strict_lookup, short* struct_index, int* member_index)

/* Determines the result type of a struct member lookup operation
 * <structure> -> <member_name> resp. <structure> . <member_name>.
 * It checks whether <structure> is a valid struct type for access
 * to <member_name>.
 *
 * <member_name> is NULL for runtime lookups, otherwise it contains the
 * name of the struct member to lookup at compile time.
 *
 * If <strict_lookup> is true, an error is thrown if the member could not
 * be found, otherwise it is degraded to a runtime lookup.
 *
 * If <member_name> is non-NULL, then the corresponding struct index will
 * be written into <struct_index> and the member index into <member_index>.
 * If the name is ambiguous then the compiler must downgrade to a runtime
 * lookup and this function will return -1 (FSM_AMBIGUOUS) in <struct_index>
 * and -1 in <member_index>. If the name is not found -2 (FSM_NO_STRUCT)
 * will be returned in <struct_index> and -1 in <member_index>.
 * If <member_name> is NULL, FSM_AMBIGUOUS will be returned in <struct_index>
 * and -1 in <member_index>.
 *
 * Returns the member type (or a union of all possible member types)
 * or NULL upon a compile error.
 */

#define FSM_AMBIGUOUS (-1)
#define FSM_NO_STRUCT (-2)

{
    lpctype_t* head = structure;    /* Used to walk over the type.                   */
    lpctype_t* result = NULL;       /* Result type.                                  */
    struct_type_t* fstruct = NULL;  /* Found structure definition.                   */
    bool is_struct = false;         /* There was at least one struct in <structure>. */

    *struct_index = FSM_NO_STRUCT;
    *member_index = -1;

    if (check_unknown_type(structure))
        return NULL;

    while (true)
    {
        /* Let's go through <structure> and see, if there are any structs in it. */
        lpctype_t *unionmember = head->t_class == TCLASS_UNION ? head->t_union.member : head;

        switch (unionmember->t_class)
        {
        case TCLASS_PRIMARY:
            switch (unionmember->t_primary)
            {
            case TYPE_UNKNOWN:
            case TYPE_ANY:
                /* Can be anything, even a struct... */
                is_struct = true;

                if (member_name != NULL)
                {
                    /* Let's look for all known structs if the have
                     * a corresponding member.
                     */

                    for (size_t i = 0; i < STRUCT_COUNT; i++)
                    {
                        struct_def_t * pdef = &STRUCT_DEF(i);

                        /* We just look at the direct members, so we automatically
                         * get the smallest struct that defines the member.
                         */
                        int idx = struct_find_direct_member(pdef->type, member_name);
                        if (idx < 0)
                            continue;

                        switch (*struct_index)
                        {
                            case FSM_NO_STRUCT:
                                fstruct = pdef->type;
                                *struct_index = i;
                                *member_index = idx;
                                break;

                            case FSM_AMBIGUOUS:
                                break;

                            default:
                                /* We did already found a struct.
                                 * This other struct is different from the current one
                                 * (because 'mixed' can't be in a union with other types
                                 * and all we don't have a struct twice in STRUCT_DEF),
                                 * so we have ambiguity here.
                                 */
                                *struct_index = FSM_AMBIGUOUS;
                                *member_index = -1;
                                fstruct = NULL;
                                break;
                        }

                        lpctype_t *oldresult = result;
                        result = get_union_type(result, pdef->type->member[idx].type);
                        free_lpctype(oldresult);
                    } /* for (all structs) */
                }
                else
                {
                    /* Struct not known, member name not known, can be anything... */
                    *struct_index = FSM_AMBIGUOUS;
                    *member_index = -1;
                    free_lpctype(result);
                    return lpctype_mixed;
                }
                break;

            default:
                /* All other cases we ignore, they cannot be indexed. */
                break;
            }
            break;

        case TCLASS_STRUCT:
            {
                struct_type_t *pdef = unionmember->t_struct.def;
                if (pdef == NULL)
                    break;

                int midx = -1;

                is_struct = true;

                if (member_name != NULL)
                {
                    /* Let's see, if this one has <member>. */
                    midx = struct_find_member(pdef, member_name);
                    if (midx < 0)
                        break;
                }

                switch (*struct_index)
                {
                    case FSM_NO_STRUCT:
                        fstruct = pdef;
                        *struct_index = get_struct_index(fstruct->name);
                        *member_index = midx;
                        if (*struct_index == -1)
                        {
                            yyerrorf("Unknown type in struct dereference: struct %s\n"
                                    , get_txt(fstruct->name->name));
                            *struct_index = FSM_AMBIGUOUS;
                        }
                        break;

                    case FSM_AMBIGUOUS:
                        break;

                    default:
                        /* Check whether this struct and <fstruct> are related. */
                        if (struct_baseof(fstruct, pdef))
                            break;
                        if (struct_baseof(pdef, fstruct))
                        {
                            fstruct = pdef;
                            *struct_index = get_struct_index(fstruct->name);
                            *member_index = midx;
                            break;
                        }

                        /* Not related, fall back to runtime lookup. */
                        *struct_index = FSM_AMBIGUOUS;
                        *member_index = -1;
                        fstruct = NULL;
                        break;
                }

                if (midx < 0) /* This also means that member_name == NULL */
                {
                    /* This is a runtime lookup. We can't guess the type,
                     * because at runtime we might have a derived structs
                     * with additional members.
                     */
                    free_lpctype(result);

                    /* It doesn't get any better, so return... */
                    return lpctype_mixed;
                }
                else
                {
                    lpctype_t *oldresult = result;
                    result = get_union_type(result, pdef->member[midx].type);
                    free_lpctype(oldresult);
                }
                break;
            }

        default:
            /* All other types can't have struct members. Ignoring. */
            break;
        }

        if (head->t_class == TCLASS_UNION)
            head = head->t_union.head;
        else
            break;
    }

    if (*struct_index == FSM_NO_STRUCT)
    {
        if (!is_struct)
            yyerrorf("Bad type for struct lookup: %s"
                    , get_lpctype_name(structure));
        else if (strict_lookup)
            yyerrorf("No such member '%s' for struct '%s'"
                    , get_txt(member_name)
                    , get_lpctype_name(structure)
                    );
        else
        {
            *struct_index = FSM_AMBIGUOUS;
            *member_index = -1;
            return lpctype_mixed;
        }

        return NULL;
    }

    return result;
} /* get_struct_member_result_type() */

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

        if (STRUCT_DEF(i).inh >= 0)
            continue;

        pOld = struct_lookup_type(pSType);
        if (!pOld || !struct_type_equivalent(pSType, pOld))
            continue;

        /* pOld has the same structure as pSType, so lets
         * replace the latter with the former.
         */
        for (int struct_idx = 0; (size_t)struct_idx < STRUCT_COUNT; struct_idx++)
        {
            if (struct_idx != i)
                struct_type_update(STRUCT_DEF(struct_idx).type, pSType, pOld);
        }

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
    ict.returntype.t_flags = 0;
    ict.returntype.t_type = NULL;
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
    ict.full_local_var_start = local_variables - &(LOCAL_VARIABLE(0));
    ict.full_context_var_start = context_variables - &(LOCAL_VARIABLE(0));
    ict.full_local_var_size  = mem_block[A_LOCAL_VARIABLES].current_size;
#ifdef DEBUG_INLINES
printf("DEBUG:   local types: %"PRIuMPINT", context types: %"PRIuMPINT"\n", 
       ict.full_local_var_start, ict.full_context_var_start);
#endif /* DEBUG_INLINES */

    /* Extend the type memblocks */
    {
        mp_uint type_count = LOCAL_VARIABLE_COUNT;

        extend_mem_block(A_LOCAL_VARIABLES, 2 * MAX_LOCAL * sizeof(A_LOCAL_VARIABLES_t));
        memset(&LOCAL_VARIABLE(type_count), 0
              , (LOCAL_VARIABLE_COUNT - type_count) * sizeof(A_LOCAL_VARIABLES_t));

        context_variables = &(LOCAL_VARIABLE(type_count));
        local_variables = &(LOCAL_VARIABLE(type_count+MAX_LOCAL));
#ifdef DEBUG_INLINES
printf("DEBUG:   type ptrs: %p, %p\n", 
       local_variables, context_variables );
#endif /* DEBUG_INLINES */
    }

    max_break_stack_need = current_break_stack_need = 0;
    max_number_of_locals = current_number_of_locals = 0;

    /* Add the structure to the memblock */
    ADD_INLINE_CLOSURE(&ict);
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

    /* Update last_expression, so it doesn't point into
     * the inline closure code block that we just removed.
     */
    if (bAbort)
        last_expression = -1;
    else
        last_expression = end; /* There is the F_CONTEXT_CLOSURE. */

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
       current_inline->full_local_var_start, current_inline->full_context_var_start);
#endif /* DEBUG_INLINES */
    local_variables = &(LOCAL_VARIABLE(current_inline->full_local_var_start));
    context_variables = &(LOCAL_VARIABLE(current_inline->full_context_var_start));
#ifdef DEBUG_INLINES
printf("DEBUG:   type ptrs: %p, %p\n", local_variables, context_variables );
#endif /* DEBUG_INLINES */

    /* Don't free the current_inline->returntype as it's not counted. */

    mem_block[A_LOCAL_VARIABLES].current_size = current_inline->full_local_var_size;

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
prepare_inline_closure (lpctype_t *returntype)

/* Called after parsing 'func <type>', this creates the identifier
 * with the synthetic function name. The function also sets up the inline
 * closure structure and block scope.
 *
 * If the name can't be generated, FALSE is returned, otherwise TRUE.
 */

{
    char name[256+MAXPATHLEN+1];
    fulltype_t funtype;
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
    funtype = get_fulltype(returntype);
    funtype.t_flags |= TYPE_MOD_NO_MASK | TYPE_MOD_PRIVATE;

    def_function_typecheck(funtype, ident, MY_TRUE);
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
complete_inline_closure (struct statement_s statements)

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
    def_function_complete(true, start, statements, true);

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
                    /* This shouldn't happen, as all explicit context
                     * variables are created before the first implicit
                     * reference can be encountered.
                     */
                    fatal("Explicit context var #%d has higher index than "
                          "implicit context variables.", i);
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
 * The function adopts the reference of <str>.
 * Result is the index of the string in the table or -1 in case of errors (out of memory).
 */

{
    mp_uint str_size, next_size;
    hash32_t hash;
    int i, *indexp;

    /* Compute the hash and the tagmask for the hash table */
    hash = hashpointer(str) & 0xff;

    indexp = &prog_string_indizes[hash];

    if (*indexp >= 0)
    {
        /* There is a hash chain for this hash: search the
         * string in there.
         */
        i = *indexp;
        for(;;)
        {
            if ( PROG_STRING(i) == str )
            {
                // same string as the new one.
                free_mstring(str); /* Drop the extra ref. */
                last_string_is_new = MY_FALSE;
                return i;
            }
            if ((i = PROG_STRING_NEXT(i)) < 0)
                break;
        }

        /* Not found: re-get the initial 'next'-index. After insertation of the
         * new string its PROG_STRING_NEXT will point to i (the old *indexp)
         */
        i = *indexp;
    }
    else
    {
        /* The first time this hash shows up (which also implies
         * that <str> is a new string. i will be used to terminate this
         * hash chain below.
         */
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
            yyerrorf("Out of memory for new program string (%zu bytes).",
                    sizeof(string_t *) + sizeof(int));
            last_string_is_new = MY_FALSE;
            return -1;
        }
    }

    /* Add the string pointer to the A_STRING area. */
    PROG_STRING(STRING_COUNT) = str;
    mem_block[A_STRINGS].current_size += sizeof(A_STRINGS_t);

    /* Add the old prog_string_index[] as a new entry in A_STRING_NEXT */
    PROG_STRING_NEXT(STRING_NEXT_COUNT) = i;
    mem_block[A_STRING_NEXT].current_size += sizeof(A_STRING_NEXT_t);

    /* Store the string index in A_STRINGS as new prog_string_index[] at its
     * hash position. */
    *indexp = str_size / sizeof str;

    last_string_is_new = MY_TRUE;
    return *indexp;
} /* store_prog_string() */

/*-------------------------------------------------------------------------*/
static int
ins_prog_string (string_t *str)

/* Add the tabled string <str> to the strings used by the program
 * and inserts code to put it on the stack into the current bytecode.
 * The function adopts the reference of <str>.
 * Returns the number of bytes written to the bytecode.
 */
{
    PREPARE_INSERT(3);
    int string_number = store_prog_string(str);
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
        CURRENT_PROGRAM_SIZE += 3;
        return 3;
    }
    CURRENT_PROGRAM_SIZE += 2;
    return 2;
} /* ins_prog_string */

/*-------------------------------------------------------------------------*/
static void
delete_prog_string (void)

/* Remove the program string last added with store_prog_string().
 */

{
    string_t *str;
    hash32_t hash;
    int *indexp;

    /* Remove the string from the A_STRINGS area */
    mem_block[A_STRINGS].current_size -= sizeof(A_STRINGS_t);
    str = PROG_STRING(STRING_COUNT);

    /* Remove the string from the hash table */
    hash = hashpointer(str) & 0xff;
    indexp = &prog_string_indizes[hash];

    // let *indexp in the hash table point to the former next string.
    mem_block[A_STRING_NEXT].current_size -= sizeof(int);
    *indexp = PROG_STRING_NEXT(STRING_NEXT_COUNT);
    // BTW: if that is now -1, the hash chain is empty.

    // and finally free the string
    free_mstring(str);

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
%token L_BYTES
%token L_BYTES_DECL
%token L_CASE
%token L_CATCH
%token L_CLOSURE
%token L_CLOSURE_DECL
%token L_COLON_COLON
%token L_CONTINUE
%token L_DEC
%token L_DEFAULT
%token L_DO
%token L_DUMMY
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
%token L_INT
%token L_LAND
%token L_LE
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
%token L_PRIVATE
%token L_PROTECTED
%token L_PUBLIC
%token L_QUOTED_AGGREGATE
%token L_RANGE
%token L_RETURN
%token L_RSH
%token L_RSHL
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
%token L_VISIBLE
%token L_VOID
%token L_WHILE

/* Textbook solution to the 'dangling else' shift/reduce conflict.
 */

%nonassoc LOWER_THAN_ELSE
%nonassoc L_ELSE

/*-------------------------------------------------------------------------*/
/* The yacc stack type */

/* Throughout the compiler fulltype_ts are used even if the values
 * are not intended to have visibility information, to record
 * the TYPE_MOD_REFERENCE flag.
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
      /* L_IDENTIFIER: The recognized identifier
       */

    typeflags_t typeflags;
      /* Just the typeflags (reference, pointer, visibility).
       */

    fulltype_t fulltype;
      /* The fulltype (datatype plus visibility) of entities.
       */

    lpctype_t *lpctype;
      /* An LPC type without modifiers.
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

    struct statement_s statement;
      /* Information about a statement or block of statements.
       */

    struct
    {
        struct statement_s statements;
        bool has_default;
    } switch_block;
      /* Information about a switch label and its statements.
       */

    struct
    {
        p_uint address;          /* Address of the code for the else block. */
        struct statement_s statements; /* Information about the statements. */
    } else_block;
      /* Information about an else block.
       */

    struct
    {
        struct statement_s statements; /* Information about the statements. */
        p_uint address;                /* Starting address. */
        bool has_code;                 /* false for a declaration. */
    } function_block;
      /* Information about a function body.
       */

    struct {
        bytecode_p     p;       /* The condition code */
        unsigned short length;  /* Length of the condition code */
        unsigned short line;    /* Last source line of the condition code */
    } expression;
      /* Expressions are used to save the code for a loop-condition
       * while the body is compiled.
       */

    struct rvalue_s
    {
        ident_t *  name;   /* A corresponding variable to be marked. */
        fulltype_t type;   /* Type of the expression */
        uint32     start;  /* Startaddress of the expression */
    } rvalue;
      /* Just a simple expression. */

    struct lrvalue_s
    {
        ident_t *      name;     /* A corresponding variable to be marked. */
        fulltype_t     type;     /* Type of the expression */
        uint32         start;    /* Startaddress of the instruction */
        lvalue_block_t lvalue;   /* Code of the expression as an lvalue */
    }
    lrvalue;
      /* Used for expressions which may return a rvalue or lvalues.
       * This happens when this expression is followed by an
       * index or range expression to yield an lvalue.
       * Then the indexed expression should be converted to an lvalue, too.
       * (Because for strings, the string itself must be replaced,
       * when a single character changes. For arrays and strings
       * an assignment to a range expression might replace the whole
       * array/string.)
       *
       * Lvalue generation in places where either a r- or an lvalue
       * is acceptible first generates the rvalue code, but stores
       * the entire code for the lvalue generation in the LVALUE_BLOCK.
       * .lvalue contains the pointers therein.
       *
       * If .lvalue.size == 0, then the expression cannot be an lvalue.
       */

    struct
    {
        fulltype_t type;         /* Type of the expression         */
        uint32     start;        /* Startaddress of the expression */
        bool       might_lvalue; /* Might be an lvalue reference.  */
    } function_call_result;
      /* A function call expression. */

    struct incdec_s
    {
        uint32     start;  /* Current programm pointer */
        short      code;   /* The opcode (F_PRE_INC or F_PRE_DEC) */
    }
    incdec;
      /* For pre-increment or -decrement remembers which opcode to use,
       * because it must be issued after the following expression has
       * been compiled.
       */

    struct s_index
    {
        short      rvalue_inst;  /* Index opcode for rvalues */
        short      lvalue_inst;  /* Index opcode for lvalues */
        short      vlvalue_inst; /* Index opcode for reseating lvalues
                                    This is not used for ranges. */
        uint32     start;        /* Startaddress of the index */
        uint32     end;          /* Endaddress+1 of the index */
        fulltype_t type1;        /* Type of index, resp. lower bound */
        fulltype_t type2;        /* Type of other index, resp. upper bound */
    }
    index;
      /* This is used to parse and return the indexing operation
       * of an array or mapping.
       * .rvalue_inst gives the type of the operation:
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
       * .lvalue_inst contains the corresponding opcode for
       * lvalue generation.
       * .start and .end are the bytecode limits of the whole
       * operation.
       * .type1 and optionally .type2 are the types of the
       * index values.
       */

    struct lvalue_s {
        lpctype_t *    type;
        ident_t *      name;
        lvalue_block_t lvalue;
        short          vlvalue_inst;
        short          num_arg;
    } lvalue;
      /* Used in assigns to communicate how an lvalue has to be accessed
       * (by passing on the bytecode to create) and what type it is.
       *
       * An lvalue expression is not put directly in the program code,
       * because it will be evaluated after the right hand side of the
       * assignment (eg. a = b = c; will be evaluated from right to
       * left). The compiled bytecodes for the lvalue are stored in the
       * LVALUE_BLOCK and should be freed using free_lvalue_block().
       *
       * If this lvalue expression can be used for reseating assignments,
       * then vlvalue_inst (if != 0) will contain the instruction that
       * must replace the last instruction in the lvalue block.
       * (The last instruction will have .num_arg bytes following it.)
       *
       * If the lvalue directly refers to a variable, name points to
       * the identifier, otherwise it's NULL.
       */

    struct {
        p_int key;     /* shared string ptr, or a number */
        Bool numeric;  /* TRUE: .key is a number */
    } case_label;
      /* Used to return the value of a 'case' label.
       */

    struct
    {
        unsigned short num_vars; /* Number of lvalues in foreach clause. */
        unsigned short type_idx; /* Index into A_ARGUMENT_TYPES of the first variable. */
    } foreach_variables;
      /* Used for the lvalue declaration in a foreach clause. */

    struct
    {
        lpctype_t* expr_type; /* Type of the expression.       */
        enum
        {
            FOREACH_LOOP,     /* Normal foreach loop value     */
            FOREACH_REF,      /* Referenced foreach loop value */
            FOREACH_RANGE     /* Integer range as loop value   */
        } foreach_type;
    } foreach_expression;
      /* Used for the expressions to iterate over in a foreach clause. */

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

    struct
    {
        p_int start;          /* Starting position in A_DEFAULT_VALUES. */
        short num;            /* Number of arguments. */
        short num_opt;        /* Number of arguments with default values. */
    } function_arguments;
      /* Keeps track of formal arguments of a function.
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
        fulltype_t type;  /* Member expr type */
    } struct_init_member;
      /* For runtime struct literals: information about a single
       * member initializer.
       */

    struct
    {
        bool strict_member; /* true, if we have ob.fun(),
                             * false for ob->fun().
                             */
    } struct_member_operator;

} /* YYSTYPE */

/*-------------------------------------------------------------------------*/
/* Destructors in case of an error. */

%destructor { free_lpctype($$);       } <lpctype>
%destructor { free_fulltype($$);      } <fulltype>
%destructor { free_lpctype($$.type);  } <lvalue>
%destructor { free_fulltype($$.type); } <rvalue> <lrvalue> <struct_init_member>
%destructor { free_fulltype($$.type1);
              free_fulltype($$.type2); } <index>
%destructor { free_lpctype($$.expr_type); } <foreach_expression>

/*-------------------------------------------------------------------------*/

%type <number>       L_NUMBER constant
%type <float_number> L_FLOAT
%type <closure>      L_CLOSURE
%type <symbol>       L_SYMBOL
%type <number>       L_QUOTED_AGGREGATE
%type <ident>        L_IDENTIFIER
%type <typeflags>    type_modifier type_modifier_list

%type <number>       optional_stars
%type <fulltype>     type non_void_type
%type <lpctype>      opt_basic_type basic_type opt_basic_non_void_type
%type <lpctype>      basic_non_void_type single_basic_non_void_type
%type <fulltype>     name_list 
%type <lpctype>      local_name_list
%type <inh_flags>    inheritance_qualifier inheritance_qualifiers
%type <typeflags>    inheritance_modifier_list inheritance_modifier
%type <lpctype>      inline_opt_type
%type <lpctype>      decl_cast cast
%type <statement>    statement statements statements_block block inline_block inline_comma_expr cond while do for foreach switch return
%type <switch_block> switch_block switch_statements
%type <address>      note_start new_arg_name
%type <rvalue>       comma_expr opt_default_value
%type <function_arguments> argument argument_list inline_opt_args
%type <function_block> function_body
%type <lrvalue>      expr4 expr0
%type <rvalue>       inline_func
%type <rvalue>       catch
%type <lvalue>       lvalue name_lvalue name_var_lvalue local_name_lvalue foreach_var_lvalue lvalue_reference
%type <foreach_variables> foreach_vars
%type <foreach_expression> foreach_expr
%type <index>        index_range index_expr
%type <case_label>   case_label
%type <else_block>   optional_else
%type <string>       anchestor
%type <sh_string>    call_other_name identifier
%type <lpctype>      member_name_list
%type <struct_init_member> struct_init
%type <struct_init_list>   opt_struct_init opt_struct_init2
%type <sh_string>    struct_member_name
%type <function_name> function_name
%type <function_call_result> function_call
%type <struct_member_operator> member_operator

/* Special uses of <number> */

%type <number> switch_label
  /* 1 for default, 0 otherwise. */

%type <number> expr_list arg_expr arg_expr_list arg_expr_list2 expr_list2
  /* Number of expressions in an expression list */

%type <number> m_expr_values
  /* Number of values for a mapping entry (ie the 'width') */

%type <number> L_ASSIGN
  /* Instruction code of the assignment, e.g. F_ADD_EQ */

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

%type <incdec> pre_inc_dec
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
%left     '.'     L_ARROW '['
%%

/*-------------------------------------------------------------------------*/

all: program;

program:
      program def possible_semi_colon
    | /* empty */ ;

possible_semi_colon:
      /* empty */
    | ';' { yywarn("Extra ';' ignored"); };

note_start: { $$ = CURRENT_PROGRAM_SIZE; };

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/* Function prototypes
 * Function definitions
 * Variable definitions
 * Inheritance
 * Default visibility
 */

def:  type L_IDENTIFIER  /* Function definition or prototype */

      {
          def_function_typecheck($1, $2, MY_FALSE);
      }

      '(' argument ')'

      {
          int fnum, num_opt = 0;
          p_int offset = 0;
          function_t *funp;

          def_function_prototype($5.num, MY_FALSE);

          /* Remember the current size if we need to revert. */
          $<address>3 = CURRENT_PROGRAM_SIZE;

          fnum = def_function_ident->u.global.function;
          funp = FUNCTION(fnum);

          if ((num_opt = $5.num_opt) > 0)
          {
              /* We had some default values given in the argument list. */
              offset = $5.start;
              if (funp->num_opt_arg && fnum < DEFAULT_VALUES_POS_COUNT && DEFAULT_VALUES_POS(fnum) >= 0)
                  yywarnf("Multiple declaration of default values for '%s' encountered", get_txt(funp->name));

              funp->num_opt_arg = num_opt;
          }
          else
          {
              /* Where there any default values in a previous prototype? */
              num_opt = funp->num_opt_arg;
              if (num_opt)
              {
                  offset = (fnum < DEFAULT_VALUES_POS_COUNT) ? DEFAULT_VALUES_POS(fnum) : -1;
                  if (offset < 0)
                  {
                      funp->num_opt_arg = num_opt = 0;
                  }
              }
          }

          $<number>$ = reserve_default_value_block(num_opt, offset);
          if (!$<number>$)
              $5.num_opt = 0;
      }

      function_body

      {
          p_uint offset = $8.address;

          if (!$8.has_code && $5.num_opt > 0)
          {
              /* It is just a prototype, but it has default values.
               * Record the position for the implementation later on.
               */
              int fnum = def_function_ident->u.global.function;

              if (RESERVE_DEFAULT_VALUE_POS(fnum - DEFAULT_VALUES_POS_COUNT + 1))
              {
                  for (int i = DEFAULT_VALUES_POS_COUNT; i < fnum; i++)
                      ADD_DEFAULT_VALUE_POS(-1);
                  ADD_DEFAULT_VALUE_POS($5.start);
              }

              FUNCTION(fnum)->num_opt_arg = $5.num_opt;
              /* Undo the space reservation. */
              CURRENT_PROGRAM_SIZE = $<address>3;
          }
          else if ($8.has_code && $<number>7)
          {
              /* Now we need to copy the initialization into the program code. */
              int fnum = def_function_ident->u.global.function;
              function_t *funp = FUNCTION(fnum);

              offset = copy_default_value_block(
                  funp->num_opt_arg,
                  $5.num_opt > 0 ? $5.start : DEFAULT_VALUES_POS(fnum),
                  offset, $<number>7);

              if (!$5.num_opt)
                  DEFAULT_VALUES_POS(fnum) = -1;
          }

          def_function_complete($8.has_code, offset, $8.statements, false);

          insert_pending_inline_closures();
          free_fulltype($1);
      }

    | name_list ';' /* Variable definition */
      {
          insert_pending_inline_closures();
          free_fulltype($1);
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
          $<address>$ = CURRENT_PROGRAM_SIZE;
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

      {
          $$.has_code = true;
          $$.address = $<address>1;
          $$.statements = $2;
      }

    | ';'
      {
          $$.has_code = false;
          $$.address = 0;
          $$.statements = (struct statement_s){ .may_return = false, .may_break = false, .may_continue = false, .may_finish = true, .is_empty = true, .warned_dead_code = false };
      }
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
          {
              free_lpctype($2);
              YYACCEPT;
          }
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

          /* Set type of locals back to the locals of the outer block.*/
          local_variables = &(LOCAL_VARIABLE(current_inline->full_local_var_start));

          /* Note, that context_variables must not be reset, as add_context_name()
           * needs it where it points now. check_for_context_local() will take
           * care of finding the right type for context variables.
           */
          $<address>$ = CURRENT_PROGRAM_SIZE;
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
          local_variables = context_variables + MAX_LOCAL;

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
          if (current_inline->block_depth > 0 && (scope->num_locals || scope->clobbered))
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

          if (!inline_closure_prototype($4.num))
          {
              free_lpctype($2);
              YYACCEPT;
          }

          $<number>$ = reserve_default_value_block($4.num_opt, $4.start);
          if (!$<number>$)
              $4.num_opt = 0;
          else
          {
              int fnum = current_inline->ident->u.global.function;
              FUNCTION(fnum)->num_opt_arg = $4.num_opt;
          }
      }

      inline_block

      {
#ifdef DEBUG_INLINES
printf("DEBUG: After inline block: program size %"PRIuMPINT"\n", CURRENT_PROGRAM_SIZE);
#endif /* DEBUG_INLINES */
         $$.start = $<address>5;
         $$.type = get_fulltype(lpctype_closure);
         $$.name = NULL;

         copy_default_value_block($4.num_opt, $4.start, current_inline->start + $<number>7, $<number>7);

         complete_inline_closure($8);
         free_lpctype($2);
      }


    | L_BEGIN_INLINE

      {
          int i;

#ifdef DEBUG_INLINES
printf("DEBUG: After L_BEGIN_INLINE: program size %"PRIuMPINT"\n", CURRENT_PROGRAM_SIZE);
#endif /* DEBUG_INLINES */
          if (!prepare_inline_closure(lpctype_mixed))
              YYACCEPT;

          /* Synthesize $1..$9 as arguments */
          for (i = 1; i < 10; i++)
          {
              char name[4];
              ident_t *ident;

              sprintf(name, "$%d", i);
              ident = make_shared_identifier(name, I_TYPE_UNKNOWN, 0);
              add_local_name(ident, get_fulltype(lpctype_mixed), block_depth);
              use_variable(ident, VAR_USAGE_READWRITE);
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
         $$.type = get_fulltype(lpctype_closure);
         $$.name = NULL;

         complete_inline_closure((struct statement_s)
                                 {
                                    .may_return =       $3.may_return       || $4.may_return,
                                    .may_break =        $3.may_break        || $4.may_break,
                                    .may_continue =     $3.may_continue     || $4.may_continue,
                                    .may_finish =       $3.may_finish       && $4.may_finish,
                                    .is_empty =         $3.is_empty         && $4.is_empty,
                                    .warned_dead_code = $3.warned_dead_code || $4.warned_dead_code,
                                  });
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
              add_local_name(ident, get_fulltype(lpctype_mixed), block_depth);
              use_variable(ident, VAR_USAGE_READWRITE);
          }

          $$.num = 9;
          $$.num_opt = 0;
          $$.start = 0;
      }
    | '(' argument ')'  { $$ = $2; }
; /* inline_opt_args */

inline_opt_type:
      /* empty */
      {
#ifdef DEBUG_INLINES
          printf("DEBUG: inline_opt_type default: ANY\n");
#endif /* DEBUG_INLINES */
          $$ = lpctype_mixed;
      }
    | basic_type
      {
#ifdef DEBUG_INLINES
          printf("DEBUG: inline_opt_type: %s\n", get_lpctype_name($1));
#endif /* DEBUG_INLINES */
          $$ = $1;
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
      { free_lpctype($1); }
; /* context_decl */


inline_comma_expr:
      /* Empty: nothing to do */
      {
          $$ = (struct statement_s){ .may_return = false, .may_break = false, .may_continue = false, .may_finish = true, .is_empty = true, .warned_dead_code = false };
      }
    | comma_expr
      {
          /* Add a F_RETURN to complete the statement */
          ins_f_code(F_RETURN);

          use_variable($1.name, VAR_USAGE_READ);
          check_unknown_type($1.type.t_type);
          free_fulltype($1.type);

          $$ = (struct statement_s){ .may_return = true, .may_break = false, .may_continue = false, .may_finish = false, .is_empty = false, .warned_dead_code = false };
      }
; /* inline_comma_expr */

/* Allow inline_func to complete the inline closure and therefore
 * cleanly leave the block scope by catching any errors here.
 */
inline_block:
      block
    | error
      {
          $$ = (struct statement_s){ .may_return = true, .may_break = false, .may_continue = false, .may_finish = true, .is_empty = true, .warned_dead_code = true };
      }
; /* inline_block */

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
/* Definition of a struct
 */

struct_decl:
      type_modifier_list L_STRUCT L_IDENTIFIER ';'
      {
          (void)define_new_struct(MY_TRUE, $3, compiled_file, $1);
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

          current_struct = define_new_struct(MY_FALSE, $3, compiled_file, $1);
          if (current_struct < 0)
              YYACCEPT;
      }
      opt_base_struct '{' opt_member_list '}' ';'
      {
          finish_struct(current_id_number+1);
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

              if (p == NULL || (num = p->u.global.struct_id) == I_GLOBAL_STRUCT_NONE)
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
          free_lpctype($1);
      }
; /* member */

member_name_list:
      basic_non_void_type L_IDENTIFIER
      {
          add_struct_member($2->name, $1, NULL);
          $$ = $1;
      }
    | member_name_list ',' optional_stars L_IDENTIFIER
      {
          lpctype_t* type = get_array_type_with_depth($1, $3);
          add_struct_member($4->name, type, NULL);
          free_lpctype(type);
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
           * Otherwise inherit_program() will do the rest.
           */

          object_t *ob;

          if (CURRENT_PROGRAM_SIZE
           && !(FUNCTION(FUNCTION_COUNT-1)->flags & NAME_INHERITED))
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

          if ($1[1] & TYPE_MOD_VARARGS)
          {
              $1[1] ^= TYPE_MOD_VARARGS;
              yyerror("illegal to inherit variables as 'varargs'");
          }

          $1[0] = check_visibility_flags($1[0], 0, true);
          $1[1] = check_visibility_flags($1[1], 0, false);

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
                  last_string_constant = new_tabled(cp, res->u.str->info.unicode);
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

          /* Let's check whether we already have this inherit. */
          {
              inherit_t *inheritp = GET_BLOCK(A_INHERITS);
              int j = INHERIT_COUNT;
              bool duplicate_toplevel = false;

              for (; --j >= 0; inheritp++)
              {
                  if (inheritp->prog == ob->prog && inheritp->inherit_depth == 1)
                  {
                      duplicate_toplevel = true;
                      break;
                  }
              }

              if  (duplicate_toplevel)
              {
                  if (pragma_pedantic)
                  {
                      yyerrorf("Program '%s' already inherited"
                              , get_txt(inheritp->prog->name));
                      YYACCEPT;
                  }
                  else
                      yywarnf("Program '%s' already inherited"
                             , get_txt(inheritp->prog->name));
              }
          }

          /* Copy the functions and variables, and take
           * care of the initializer.
           */
          int initializer = inherit_program(ob->prog, $1[0], $1[1]);
          if (initializer > -1)
          {
              /* We inherited a __INIT() function: create a call */

              transfer_init_control();
              ins_f_code(F_SAVE_ARG_FRAME);
              ins_f_code(F_CALL_INHERITED);
              ins_short(INHERIT_COUNT-1);
              ins_short(initializer);
              ins_f_code(F_RESTORE_ARG_FRAME);
              ins_f_code(F_POP_VALUE);
              add_new_init_jump();
          }

          num_virtual_variables = V_VARIABLE_COUNT;
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
          $$[0] &= ~TYPE_MOD_NOSAVE;
          $$[1] &= ~TYPE_MOD_VARARGS;
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
      type L_IDENTIFIER
      {
          static ident_t    *last_identifier;
          static typeflags_t last_modifier;
%line
          /* We use 'type' instead of only 'type_modifier_list'
           * to avoid parser conflicts between:
           *
           *    private functions(int i) {}
           * and
           *    private functions inherit "i";
           *
           * So this is the same right hand side as 'def'
           * for function definitions.
           * TODO: If 'functions' and 'variables' would become
           * keywords, then this wouldn't be ambiguous.
           *
           * But for now we just check that no type was given.
           */
          if ($1.t_type)
          {
              yyerror("syntax error");
              free_fulltype($1);
          }

          /* Check if there were any modifiers at all */
          do
          {
              if ( !$1.t_flags )
              {
                  /* take lookahead into account */
                  if ($2 == last_identifier)
                  {
                      last_identifier = NULL;
                      $$[0] = $$[1] = 0;
                      break;
                  }
              }
              else
              {
                  last_modifier = $1.t_flags;
              }

              last_identifier = $2;

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
          } while(0);
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
          if ($2[0] & ~( TYPE_MOD_PRIVATE | TYPE_MOD_PUBLIC | TYPE_MOD_VISIBLE
                       | TYPE_MOD_PROTECTED | TYPE_MOD_STATIC | TYPE_MOD_DEPRECATED)
             )
          {
              yyerror("Default visibility specification for functions "
                      "accepts only 'private', 'protected', 'visible', 'public', "
                      "'static' or 'deprecated'");
              YYACCEPT;
          }

          if ($2[1] & ~( TYPE_MOD_PRIVATE | TYPE_MOD_PUBLIC | TYPE_MOD_VISIBLE
                       | TYPE_MOD_PROTECTED | TYPE_MOD_DEPRECATED)
             )
          {
              yyerror("Default visibility specification for variables "
                      "accepts only 'private', 'protected', 'visible', 'public' "
                      "or 'deprecated'"
                      );
              YYACCEPT;
          }

          default_funmod = check_visibility_flags($2[0], 0, true);
          default_varmod = check_visibility_flags($2[1], 0, false);
      }
; /* default_visibility */

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
/* Type specifications and casts
 *
 * The type rules are used to parse variable and function types, casts,
 * or just visibility e.g for inheritance.
 */

optional_stars:
      /* empty */        { $$ = 0; }
    | optional_stars '*' { $$ = $1+1; } ;

type: type_modifier_list opt_basic_type
      {
          set_fulltype($$, $1, $2);
      } ;


non_void_type: type_modifier_list opt_basic_non_void_type
      {
          set_fulltype($$, $1, $2);
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
    | L_DEPRECATED { $$ = TYPE_MOD_DEPRECATED; }
    | L_VISIBLE    { $$ = TYPE_MOD_VISIBLE; }
;


opt_basic_type:
      basic_type
    | /* empty */ { $$ = NULL; } ;


opt_basic_non_void_type:
      basic_non_void_type
    | /* empty */ { $$ = NULL; } ;


basic_non_void_type:
      single_basic_non_void_type
    | basic_non_void_type '|' single_basic_non_void_type
      {
          $$ = get_union_type($1, $3);
          free_lpctype($1);
          free_lpctype($3);
      }
; /* basic_non_void_type */


single_basic_non_void_type:
      L_STATUS       { $$ = lpctype_int;        }
    | L_INT          { $$ = lpctype_int;        }
    | L_BYTES_DECL   { $$ = lpctype_bytes;      }
    | L_STRING_DECL  { $$ = pragma_no_bytes_type ? lpctype_string_bytes : lpctype_string; }
    | L_OBJECT       { $$ = lpctype_object;     }
    | L_CLOSURE_DECL { $$ = lpctype_closure;    }
    | L_SYMBOL_DECL  { $$ = lpctype_symbol;     }
    | L_FLOAT_DECL   { $$ = lpctype_float;      }
    | L_MAPPING      { $$ = lpctype_mapping;    }
    | L_MIXED        { $$ = lpctype_mixed;      }
    | L_STRUCT identifier
      {
          int num;

          num = find_struct($2);
          if (num < 0)
          {
              yyerrorf("Unknown struct '%s'", get_txt($2));
              $$ = lpctype_any_struct;
          }
          else
          {
              $$ = get_struct_type(STRUCT_DEF(num).type);
          }

          free_mstring($2);
      }
    | single_basic_non_void_type '*'
      {
          $$ = get_array_type($1);
          free_lpctype($1);
      }
    | '<' basic_non_void_type '>'
      {
          $$ = $2;
      }
; /* single_basic_non_void_type */


basic_type:
      basic_non_void_type
    | L_VOID         { $$ = lpctype_void;    }
; /* basic_type */


cast:
      '(' single_basic_non_void_type ')'
      {
          $$ = $2;
      }
;


/* TODO: Remove decl_casts - they are practically useless */
decl_cast:
      '(' '{' basic_type '}' ')'
      {
          $$ = $3;
      }
;


/* A generic identifier */

identifier:
      L_IDENTIFIER
      {
          /* Extract the string from the ident structure */
          $$ = ref_mstring($1->name);
      }
;


/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/* Argument and variable definitions
 */

argument:
      /* empty */
      {
          $$.num = 0;
          $$.num_opt = 0;
          $$.start = 0;
      }
    | L_VOID
      {
          $$.num = 0;
          $$.num_opt = 0;
          $$.start = 0;
      }
    | argument_list ;


argument_list:
      new_arg_name
      {
          $$.num = 1;
          $$.num_opt = 0;
          $$.start = 0;

          if ($1 != UINT32_MAX)
          {
              /* We have a default value for this argument. */
              $$.start = DEFAULT_VALUES_BLOCK_SIZE;

              /* Now move the initialization code to the A_DEFAULT_VALUES block. */
              if (save_default_value($1))
                  $$.num_opt++;
          }
      }
    | argument_list ',' new_arg_name
      {
          $$ = $1;
          $$.num++;

          if ($3 != UINT32_MAX)
          {
              /* We have a default value for the argument. */

              if ($$.num_opt == 0)
              {
                  /* And this one is the first default value. */
                  $$.start = DEFAULT_VALUES_BLOCK_SIZE;
              }

              /* Now move the initialization code to the A_DEFAULT_VALUES block. */
              if (save_default_value($3))
                  $$.num_opt++;
          }
          else if ($$.num_opt != 0 && !(local_variables[current_number_of_locals-1].type.t_flags & TYPE_MOD_VARARGS))
              yyerrorf("Missing default value for argument");
      } ;


new_arg_name:
      non_void_type L_IDENTIFIER opt_default_value
      {
          funflag_t illegal_flags = $1.t_flags & (TYPE_MOD_STATIC|TYPE_MOD_NO_MASK|TYPE_MOD_PRIVATE|TYPE_MOD_PUBLIC|TYPE_MOD_VIRTUAL|TYPE_MOD_PROTECTED|TYPE_MOD_NOSAVE|TYPE_MOD_VISIBLE);
          ident_t *varident = NULL;

          if (illegal_flags)
          {
              yyerrorf("Illegal modifier for function argument: %s"
                     , get_f_visibility(illegal_flags));
              $1.t_flags &= ~illegal_flags;
          }

          if (!$1.t_type)
          {
              if (exact_types)
                  yyerror("Missing type for argument");

              /* Supress more errors */
              $1.t_type = lpctype_mixed;
          }

          if ($2->type == I_TYPE_LOCAL)
          {
              /* A local name is redeclared. */
              if (current_inline == NULL)
              {
                  /* Since this is the argument list of a function, it can't be
                   * legal.
                   */
                  yyerror("Illegal to redeclare local name");
                  free_fulltype($1);
              }
              else
              {
                  /* However, it is legal for the argument list of an inline
                   * closure.
                   */
                  varident = redeclare_local($2, $1, block_depth);
              }
          }
          else
              varident = add_local_name($2, $1, block_depth);

          /* Arguments may be ignored,
           * thus will not show a warning if done so. */
          use_variable(varident, VAR_USAGE_READWRITE);

          /* Default value. */
          if ($3.start != UINT32_MAX && varident != NULL)
          {
              /* Check the types. */
              if ($1.t_flags & TYPE_MOD_VARARGS)
              {
                  yyerrorf("varargs parameter must not have a default value");
              }
              else if (!check_assignment_types($3.type, $1.t_type))
              {
                  yyerrorf("Type mismatch %s for default value of %s"
                      , get_two_lpctypes($1.t_type, $3.type.t_type)
                      , get_txt(varident->name));
              }

              /* Add an lvalue and the assignment. */
              add_type_check($1.t_type, TYPECHECK_VAR_INIT);

              PREPARE_INSERT(3)
              add_f_code(F_PUSH_LOCAL_VARIABLE_LVALUE);
              add_byte(varident->u.local.num);
              add_f_code(F_VOID_ASSIGN);
              CURRENT_PROGRAM_SIZE += 3;
          }

          free_fulltype($3.type);

          $$ = $3.start;
      }
; /* new_arg_name */

opt_default_value:
      /* empty */
      {
        $$.start = UINT32_MAX;
        $$.name = NULL;
        $$.type = get_fulltype(NULL);
      }
    | L_ASSIGN expr0
      {
        use_variable($2.name, VAR_USAGE_READ);
        if ($1 != F_ASSIGN)
           yyerror("Illegal initialization");

        $$.start = $2.start;
        $$.name = $2.name;
        $$.type = $2.type;
        free_lvalue_block($2.lvalue);
      }
; /* opt_default_value */


name_list:
      /* Simple variable definition */
      type L_IDENTIFIER
      {
%line
          if ($1.t_type == NULL)
          {
              yyerror("Missing type");
              $1.t_type = lpctype_mixed;
          }

          define_global_variable($2, $1, MY_FALSE);
          $$ = $1;
      }

    /* Variable definition with initialization */

    | type L_IDENTIFIER
      {
          if ($1.t_type == NULL)
          {
              yyerror("Missing type");
              $1.t_type = lpctype_mixed;
          }

          $<number>$ = define_global_variable($2, $1, MY_TRUE);
      }

      L_ASSIGN expr0
      {
          use_variable($5.name, VAR_USAGE_READ);
          init_global_variable($<number>3, $2, $1, $4, $5.type);
          free_fulltype($5.type);
          free_lvalue_block($5.lvalue);
          $$ = $1;
      }

    | name_list ',' optional_stars L_IDENTIFIER
      {
          fulltype_t type;
          type.t_type = get_array_type_with_depth($1.t_type, $3);
          type.t_flags = $1.t_flags;

          define_global_variable($4, type, MY_FALSE);
          free_fulltype(type);
          $$ = $1;
      }

    /* Variable definition with initialization */

    | name_list ',' optional_stars L_IDENTIFIER
      {
          fulltype_t type;
          type.t_type = get_array_type_with_depth($1.t_type, $3);
          type.t_flags = $1.t_flags;

          $<number>$ = define_global_variable($4, type, MY_TRUE); 
          free_fulltype(type);
      }

      L_ASSIGN expr0
      {
          fulltype_t type;
          type.t_type = get_array_type_with_depth($1.t_type, $3);
          type.t_flags = $1.t_flags;

          use_variable($7.name, VAR_USAGE_READ);
          init_global_variable($<number>5, $4, type, $6, $7.type);

          free_fulltype(type);
          free_fulltype($7.type);
          free_lvalue_block($7.lvalue);
          $$ = $1;
      }

; /* name_list */


/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
/* Blocks and simple statements.
 */

block:
    '{' statements_block '}'
    {
        $$ = $2;
    }
; /* block */


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

          $$ = $2;
      }
; /* block_statements */


statements:
      /* empty */
      {
          $$ = (struct statement_s){ .may_return = false, .may_break = false, .may_continue = false, .may_finish = true, .is_empty = true, .warned_dead_code = false };
      }
    | statements local_name_list ';'
      {
          free_lpctype($2);
          $$ = $1;
          $$.is_empty = false;

          if (pragma_warn_dead_code && !$1.may_finish && !$$.warned_dead_code)
          {
              yywarnf("Unreachable code");
              $$.warned_dead_code = true;
          }
      }
    | statements statement
      {
          $$ = (struct statement_s)
               {
                 .may_return       = $1.may_return       || $2.may_return,
                 .may_break        = $1.may_break        || $2.may_break,
                 .may_continue     = $1.may_continue     || $2.may_continue,
                 .may_finish       = $1.may_finish       && $2.may_finish,
                 .is_empty         = $1.is_empty         && $2.is_empty,
                 .warned_dead_code = $1.warned_dead_code || $2.warned_dead_code,
               };

          if (pragma_warn_dead_code && !$1.may_finish && !$2.is_empty && !$$.warned_dead_code)
          {
              yywarnf("Unreachable code");
              $$.warned_dead_code = true;
          }
      }
;


local_name_list:
      basic_type L_IDENTIFIER
      {
          define_local_variable($2, $1, NULL, $2->type == I_TYPE_LOCAL, MY_FALSE);

          $$ = $1;
      }
    | basic_type L_IDENTIFIER
      {
          $2 = define_local_variable($2, $1, &$<lvalue>$, $2->type == I_TYPE_LOCAL, MY_TRUE);
      }
      L_ASSIGN expr0
      {
          use_variable($5.name, VAR_USAGE_READ);
          init_local_variable($2, &$<lvalue>3, $4, $5.type);

          free_fulltype($5.type);
          free_lvalue_block($5.lvalue);
          $$ = $1;
      }
    | local_name_list ',' optional_stars L_IDENTIFIER
      {
          lpctype_t* type = get_array_type_with_depth($1, $3);
          define_local_variable($4, type, NULL, $4->type == I_TYPE_LOCAL, MY_FALSE);
          free_lpctype(type);

          $$ = $1;
      }
    | local_name_list ',' optional_stars  L_IDENTIFIER
      {
          lpctype_t* type = get_array_type_with_depth($1, $3);
          $4 = define_local_variable($4, type, &$<lvalue>$, $4->type == I_TYPE_LOCAL, MY_TRUE);
          free_lpctype(type);
      }
      L_ASSIGN expr0
      {
          use_variable($7.name, VAR_USAGE_READ);
          init_local_variable($4, &$<lvalue>5, $6, $7.type);

          free_fulltype($7.type);
          free_lvalue_block($7.lvalue);
          $$ = $1;
      }
; /* local_name_list */


statement:
      comma_expr ';'
      {
          bytecode_t last = F_ILLEGAL;

          insert_pop_value();
#ifdef F_BREAK_POINT
          if (d_flag)
              ins_f_code(F_BREAK_POINT);
#endif /* F_BREAK_POINT */

          free_fulltype($1.type);

          if (CURRENT_PROGRAM_SIZE > $1.start)
              last = PROGRAM_BLOCK[CURRENT_PROGRAM_SIZE-1];

          $$ = (struct statement_s)
               {
                    .may_return = (last == F_RAISE_ERROR || last == F_THROW),
                    .may_break = false,
                    .may_continue = false,
                    .may_finish = (last != F_RAISE_ERROR && last != F_THROW),
                    .is_empty = last == F_RAISE_ERROR,
                    .warned_dead_code = false,
               };
      }

    | error ';' /* Synchronisation point */
      {
          $$ = (struct statement_s){ .may_return = true, .may_break = false, .may_continue = false, .may_finish = true, .is_empty = true, .warned_dead_code = true };
      }
    | cond | while | do | for | foreach | switch
    | return ';'
      {
          $$ = $1;
      }
    | block
    | /* empty */ ';'
      {
          $$ = (struct statement_s){ .may_return = false, .may_break = false, .may_continue = false, .may_finish = true, .is_empty = true, .warned_dead_code = false };
      }
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
                  yyerrorf("Compiler limit: (L_BREAK) value too large: %"PRIdBcOffset
                          , current_break_address);
          }
          $$ = (struct statement_s){ .may_return = false, .may_break = true, .may_continue = false, .may_finish = false, .is_empty = false, .warned_dead_code = false };
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

          $$ = (struct statement_s){ .may_return = false, .may_break = false, .may_continue = true, .may_finish = false, .is_empty = false, .warned_dead_code = false };
      }
; /* statement */


return:
      L_RETURN
      {
          if (exact_types && exact_types != lpctype_void && exact_types != lpctype_mixed)
              lpctype_error("Must return a value for a function declared",
                         exact_types);
          ins_f_code(F_RETURN0);

          $$ = (struct statement_s){ .may_return = true, .may_break = false, .may_continue = false, .may_finish = false, .is_empty = false, .warned_dead_code = false };
      }

    | L_RETURN comma_expr
      {
%line
          fulltype_t type2 = $2.type;

          use_variable($2.name, VAR_USAGE_READ);

          if (exact_types)
          {
              check_unknown_type(type2.t_type);

              /* More checks, ie. mixed vs non-mixed, would be nice,
               * but the general type tracking is too lacking for it.
               */
              if (!check_assignment_types(type2, exact_types))
              {
                  char tmp[512];
                  get_fulltype_name_buf(type2, tmp, sizeof(tmp));

                  yyerrorf("Return type not matching: got %s, expected %s"
                         , tmp, get_lpctype_name(exact_types));
              }
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

          free_fulltype($2.type);

          $$ = (struct statement_s){ .may_return = true, .may_break = false, .may_continue = false, .may_finish = false, .is_empty = false, .warned_dead_code = false };
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

          use_variable($4.name, VAR_USAGE_READ);
          check_unknown_type($4.type.t_type);

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

          free_fulltype($4.type);

          $$ = (struct statement_s){ .may_return = $7.may_return, .may_break = false, .may_continue = false, .may_finish = true, .is_empty = false, .warned_dead_code = false };
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

          use_variable($7.name, VAR_USAGE_READ);
          check_unknown_type($7.type.t_type);

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

          free_fulltype($7.type);

          $$ = (struct statement_s)
               {
                    .may_return = $3.may_return,
                    .may_break = false,
                    .may_continue = false,
                    .may_finish = $3.may_break || $3.may_continue || $3.may_finish,
                    .is_empty = false,
                    .warned_dead_code = $3.warned_dead_code,
                };
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

      for_cond_expr ';'

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

      for_iter_expr ')'

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

          $$ = (struct statement_s){ .may_return = $13.may_return, .may_break = false, .may_continue = false, .may_finish = true, .is_empty = false, .warned_dead_code = false };
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
      {
          free_fulltype($1.type);
          free_lvalue_block($1.lvalue);
      }
    | local_name_lvalue L_ASSIGN expr0
      {
          /* We got a "int <name> = <expr>" type expression. */

%line
          fulltype_t type2;
          Bool res;

          /* Check the assignment for validity */
          type2 = $3.type;
          if (exact_types
           && !check_assignment_types(type2, $1.type))
          {
              yyerrorf("Bad assignment %s", get_two_lpctypes($1.type, type2.t_type));
          }

          if ($2 != F_ASSIGN)
          {
              yyerror("Only plain assignments allowed here");
          }

          add_type_check($1.type, TYPECHECK_VAR_INIT);

          use_variable($3.name, VAR_USAGE_READ);
          use_variable($1.name, ($3.type.t_flags & TYPE_MOD_REFERENCE) ? VAR_USAGE_READWRITE : VAR_USAGE_WRITE);

          /* Add the bytecode to create the lvalue and do the
           * assignment.
           */
          free_lvalue_block($3.lvalue);
          res = add_lvalue_code($1.lvalue, $2);

          free_lpctype($1.type);
          free_fulltype($3.type);

          $1.name->u.local.initializing = false;

          if (!res)
              YYACCEPT;
      }

    | local_name_lvalue
      {
          /* We got a "int <name>" type expression.
           * Compile it as if it was a "int <name> = 0" expression.
           */
%line
          Bool res;

          /* Insert the implied push of number 0 */
          ins_number(0);

          /* Add the bytecode to create the lvalue and do the
           * assignment.
           */
          res = add_lvalue_code($1.lvalue, F_ASSIGN);

          free_lpctype($1.type);

          $1.name->u.local.initializing = false;

          if (!res)
              YYACCEPT;
      }
; /* expr_decl */


for_cond_expr:
      /* EMPTY */
      {
          last_expression = mem_block[A_PROGRAM].current_size;
          ins_number(1);
      }
    | comma_expr
      {
          use_variable($1.name, VAR_USAGE_READ);
          check_unknown_type($1.type.t_type);
          free_fulltype($1.type);
      }
; /* for_cond_expr */

for_iter_expr:
      /* EMPTY */
      {
          last_expression = mem_block[A_PROGRAM].current_size;
          ins_number(1);
      }
    | comma_expr
      {
          free_fulltype($1.type);
      }
; /* for_iter_expr */

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
 *    c: FOREACH_NEXT <typeidx> l
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

          /* Mark all variables as initialized. */
          for (ident_t *var = all_locals; var != NULL && var->u.local.depth == block_depth; var = var->next_all)
          {
              var->u.local.initializing = false;
          }

          if (!arg_types_exhausted)
          {
              lpctype_t *lvtype = ARGUMENT_TYPE($4.type_idx);
              /* Let's check the argument types. */
              if ($7.foreach_type == FOREACH_RANGE)
              {
                  /* The first lvalue need to be an integer.*/
                  if (!lpctype_contains(lpctype_int, lvtype))
                      yyerrorf("Wrong variable type to foreach range: got %s, expected int", get_lpctype_name(lvtype));

                  /* We don't need the types on the A_ARGUMENT_TYPES anymore,
                   * but foreach_expr might have used them already,
                   * so we can't remove them now.
                   */
                  $4.type_idx = USHRT_MAX;
              }
              else
              {
                  /* We only need to check the first lvalue. Any further lvalues
                   * are only used when iterating over mappings and there we
                   * don't know the type anyway.
                   */
                  lpctype_t *expected = check_unary_op_type($7.expr_type, "foreach", types_foreach_iteration, lpctype_mixed);
                  if (!has_common_type(expected, lvtype))
                  {
                      char buf[512];
                      get_lpctype_name_buf(lvtype, buf, sizeof(buf));
                      yyerrorf("Wrong variable type to foreach: got %s, expected %s", buf, get_lpctype_name(expected));
                  }
                  free_lpctype(expected);
              }
          }

          /* Create the FOREACH instruction, leaving the branch field
           * blank.
           */
          switch ($7.foreach_type)
          {
          case FOREACH_LOOP:
              ins_f_code(F_FOREACH); break;
          case FOREACH_REF:
              ins_f_code(F_FOREACH_REF); break;
          case FOREACH_RANGE:
              ins_f_code(F_FOREACH_RANGE); break;
          default:
              yyerrorf("Unknown foreach_expr type %ld.\n", (long)$7.foreach_type);
              fatal("Unknown foreach_expr type %ld.\n", (long)$7.foreach_type);
              /* NOTREACHED */
          }
          ins_byte($4.num_vars+1);
          ins_short(0);

          push_address(); /* Address to branch back to */

          /* Deactivate rttc if we don't have the types or the pragma is not active */
          if (arg_types_exhausted || !pragma_rtt_checks)
              $4.type_idx = USHRT_MAX;
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
              start_addr = expr_addr - $4.num_vars*2;
              current = start_addr + (addr - 4 - expr_addr);
              for ( src = PROGRAM_BLOCK + expr_addr,
                    dest = PROGRAM_BLOCK + start_addr
                  ; expr_addr < addr-4
                  ; src++, dest++, expr_addr++)
                  *dest = *src;
              CURRENT_PROGRAM_SIZE = current;
              ins_f_code(F_POP_VALUE);
              current++;
              if ($7.foreach_type == FOREACH_RANGE)
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
              ins_short($4.type_idx);
              ins_short(current + 5 - addr);

              current += 5;

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

          free_lpctype($7.expr_type);

          $$ = (struct statement_s){ .may_return = $10.may_return, .may_break = false, .may_continue = false, .may_finish = true, .is_empty = false, .warned_dead_code = false };
      }
; /* foreach */


foreach_vars : /* Parse and count the number of lvalues */
      foreach_var_decl
      {
          $$.num_vars = 1;
          $$.type_idx = ARGTYPE_COUNT-1;
      }
    | foreach_vars ',' foreach_var_decl
      {
          $$ = $1;
          $$.num_vars++;

          /* The variable count is stored in a byte at F_FOREACH. */
          if ($$.num_vars == UCHAR_MAX)
              yyerror("Too many variables in foreach");
      }
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
          Bool res = add_lvalue_code($1.lvalue, 0);

          if (!arg_types_exhausted)
          {
              int arg_idx = ARGTYPE_COUNT;

              /* We use USHRT_MAX to indicate that F_FOREACH should
               * not check the types, therefore we warn at it.
               */
              if (arg_idx >= USHRT_MAX)
              {
                  arg_types_exhausted = true;
                  yywarnf("Type buffer exhausted, cannot store and verify argument types");

                  free_lpctype($1.type);
              }
              else
              {
                  ADD_ARGUMENT_TYPE($1.type); // Adapt ref
              }
          }
          else
              free_lpctype($1.type);

          /* We allow non-read variables without warnings for foreach,
             so mark them as read. */
          use_variable($1.name, VAR_USAGE_READWRITE);

          if (!res)
              YYACCEPT;
      }

; /* foreach_var_decl */


foreach_var_lvalue:  /* Gather the code for one lvalue */

      local_name_lvalue
    | name_var_lvalue

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
          lpctype_t *dtype;
          Bool       gen_refs;

%line
          gen_refs = ($1.type.t_flags & TYPE_MOD_REFERENCE) != 0;
          dtype = $1.type.t_type;

          use_variable($1.name, VAR_USAGE_READ);
          check_unknown_type(dtype);

          /* Allowed are arrays of all kinds, strings, mappings,
           * ints (but not &int), mixed and unknown (when !exact_types).
           */
          if (!has_common_type(lpctype_any_array, dtype)
           && !has_common_type(lpctype_any_struct, dtype)
           && !lpctype_contains(lpctype_string, dtype)
           && !lpctype_contains(lpctype_bytes, dtype)
           && !lpctype_contains(lpctype_mapping, dtype)
           && (gen_refs || !lpctype_contains(lpctype_int, dtype))
           && (exact_types || dtype != lpctype_unknown)
             )
          {
              fulltype_error("Expression for foreach() of wrong type", $1.type);
          }

          $$.foreach_type = gen_refs ? FOREACH_REF : FOREACH_LOOP;
          $$.expr_type = $1.type.t_type;

          free_lvalue_block($1.lvalue);
      }

    | expr0 L_RANGE expr0
      {
          lpctype_t *dtype;

%line
          if (($1.type.t_flags & TYPE_MOD_REFERENCE) != 0)
          {
              fulltype_error("Expression for foreach() of wrong type", $1.type);
          }

          dtype = $1.type.t_type;

          use_variable($1.name, VAR_USAGE_READ);
          use_variable($3.name, VAR_USAGE_READ);

          if (!check_unknown_type(dtype)
           && !lpctype_contains(lpctype_int, dtype))
          {
              fulltype_error("Expression for foreach() of wrong type", $1.type);
          }

          if (($3.type.t_flags & TYPE_MOD_REFERENCE) != 0)
          {
              fulltype_error("Expression for foreach() of wrong type", $3.type);
          }

          dtype = $3.type.t_type;

          if (!check_unknown_type(dtype)
           && !lpctype_contains(lpctype_int, dtype))
          {
              fulltype_error("Expression for foreach() of wrong type", $3.type);
          }

          $$.foreach_type = FOREACH_RANGE;
          $$.expr_type = ref_lpctype(lpctype_int);

          free_fulltype($1.type);
          free_fulltype($3.type);
          free_lvalue_block($3.lvalue);
          free_lvalue_block($1.lvalue);
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
        use_variable($3.name, VAR_USAGE_READ);
        check_unknown_type($3.type.t_type);

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

        free_fulltype($3.type);

        $$ = (struct statement_s){ .may_return = $7.statements.may_return,
                                   .may_break = false,
                                   .may_continue = $7.statements.may_continue,
                                   .may_finish = !$7.has_default || $7.statements.may_break || $7.statements.may_finish,
                                   .is_empty = false,
                                   .warned_dead_code = $7.statements.warned_dead_code,
                                 };
      }
; /* switch */


switch_block:
      switch_block switch_statements
      {
        $$.has_default = $1.has_default || $2.has_default;
        $$.statements = (struct statement_s)
                        {
                            .may_return =       $1.statements.may_return       || $2.statements.may_return,
                            .may_break =        $1.statements.may_break        || $2.statements.may_break,
                            .may_continue =     $1.statements.may_continue     || $2.statements.may_continue,
                            .may_finish =                                         $2.statements.may_finish,
                            .is_empty =         false,
                            .warned_dead_code = $1.statements.warned_dead_code || $2.statements.warned_dead_code,
                        };
      }
    | switch_statements
; /* switch_block */


switch_statements:
    switch_label statements_block
    {
        $$.has_default = $1 != 0;
        $$.statements = $2;
    }
; /* switch_statements */


switch_label:
      case    { $$ = 0; }
    | default { $$ = 1; }
; /* switch_label */


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

    | case_string_label
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


case_string_label: string_constant | bytes_constant;


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

          use_variable($3.name, VAR_USAGE_READ);
          check_unknown_type($3.type.t_type);

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

          free_fulltype($3.type);
      }
; /* condStart */


cond:
      condStart
      statement
      optional_else
      {
          p_int destination, location, offset;

          /* Complete the branch over the if-part */
          destination = (p_int)$3.address;
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

          $$ = (struct statement_s)
               {
                    .may_return =       $2.may_return       || $3.statements.may_return,
                    .may_break =        $2.may_break        || $3.statements.may_break,
                    .may_continue =     $2.may_continue     || $3.statements.may_continue,
                    .may_finish =       $2.may_finish       || $3.statements.may_finish,
                    .is_empty =         false,
                    .warned_dead_code = $2.warned_dead_code || $3.statements.warned_dead_code,
               };
      }
; /* cond */


optional_else:
      /* empty */ %prec LOWER_THAN_ELSE
      {
          /* The if-part ends here */
          $$.address = CURRENT_PROGRAM_SIZE;
          $$.statements = (struct statement_s){ .may_return = false, .may_break = false, .may_continue = false, .may_finish = true, .is_empty = true, .warned_dead_code = false };
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
          $$.address = fix_branch( F_LBRANCH, CURRENT_PROGRAM_SIZE, $<address>2);
          $$.address += $<address>2 + 1;
          $$.statements = $3;
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


bytes_constant:
      L_BYTES
      {
          last_string_constant = last_lex_string;
          last_lex_string = NULL;
      }
    | bytes_constant '+' L_BYTES
      {
          add_string_constant();
      }
    | L_BYTES L_BYTES
      { fatal("L_BYTES L_BYTES: presence of rule should prevent its reduction\n"); }
    | bytes_constant '+' L_BYTES L_BYTES
      { fatal("L_BYTES L_BYTES: presence of rule should prevent its reduction\n"); }
    | '(' bytes_constant ')'
; /* bytes_constant */


/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
/* Expressions
 *
 * expr0 (with the help of the precedence and assoc specifications) handles
 * most of the expressions, and returns full lrvalues.
 *
 * expr4 contains the expressions atoms (literal values), function calls
 * and expressions returning values which might be used as rvalues
 * as well as lvalues. It returns full lrvalues.
 *
 * lvalue contains expressions for unprotected lvalues and returns lvalues.
 *
 * name_var_lvalue is a special lvalue that ignores any lvalues in the variable
 * and is used the modify the direct content of the variable (normally
 * if a variable already contents an lvalue, then an lvalue of that variable
 * would use that lvalue; with name_var_lvalue this doesn't happen).
 * It currently is used by foreach().
 *
 * local_name_lvalue is to be used in contexts where new local variables
 * may be defined on the fly (for example "for(int i...").
 *
 * index_expr and index_range are used to parse and compile the two
 * forms of array indexing operations.
 */

comma_expr:
      expr0
      {
        $$.start = $1.start;
        $$.type = $1.type;
        $$.name = $1.name;
        free_lvalue_block($1.lvalue);
      }
    | comma_expr
      {
          insert_pop_value();
      }

      ',' expr0

      {
          $$.start = $1.start;
          $$.type = $4.type;
          $$.name = $4.name;

          free_fulltype($1.type);
          free_lvalue_block($4.lvalue);
      }
; /* comma_expr */


expr0:
    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

      /* Normal assign:               ||= (&&= analog):
       *
       *  <expr0>               <prot-lvalue>    <protected-lvalue>
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
              if (!add_lvalue_code($1.lvalue, F_MAKE_PROTECTED))
              {
                  free_lpctype($1.type);
                  YYACCEPT;
              }

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
          const char* op_name;
          binary_op_types_t* op_table = NULL;
          lpctype_t *rttc2 = NULL;
%line
          $$ = $4;
          $$.lvalue = (lvalue_block_t) {0, 0};

          free_lvalue_block($4.lvalue);

          type1.t_type = $1.type;
          type1.t_flags = 0;
          type2 = $4.type;

          /* Check the validity of the assignment */
          /* There only need to be a common subset:
           *
           * int|string val = 10;
           * int result;
           *
           * if(intp(val))
           *    result = val;
           *
           * But if it's a zero (0), pass lpctype_mixed as the result type, because of:
           *
           *  string a;
           *  int b;
           *
           *  a = b = 0;
           */
          if (check_unknown_type(type2.t_type))
              type2.t_type = lpctype_mixed;

          switch($2)
          {
              case F_ASSIGN:
              case F_LAND_EQ:
              case F_LOR_EQ:
                  break;

              case F_ADD_EQ:
                  op_name = "+=";
                  op_table = types_add_assignment;
                  break;

              case F_SUB_EQ:
                  op_name = "-=";
                  op_table = types_sub_assignment;
                  break;

              case F_MULT_EQ:
                  op_name = "*=";
                  op_table = types_mul_assignment;
                  break;

              case F_DIV_EQ:
                  op_name = "/=";
                  op_table = types_div_assignment;
                  break;

              case F_MOD_EQ:
                  op_name = "%=";
                  op_table = types_modulus;
                  break;

              case F_AND_EQ:
                  op_name = "&=";
                  op_table = types_binary_and_assignment;
                  break;

              case F_OR_EQ:
                  op_name = "|=";
                  op_table = types_binary_or_assignment;
                  break;

              case F_XOR_EQ:
                  op_name = "^=";
                  op_table = types_binary_or_assignment;
                  break;

              case F_LSH_EQ:
                  op_name = "<<=";
                  op_table = types_shift;
                  break;

              case F_RSH_EQ:
                  op_name = ">>=";
                  op_table = types_shift;
                  break;

              case F_RSHL_EQ:
                  op_name = ">>>=";
                  op_table = types_shift;
                  break;

              default:
                  fatal("Unknown assignment operation %"PRIdPINT".\n", $2);
                  break;
          } /* switch(assign op) */

          if (op_table)
          {
              /* Operator assignment: result type is determined by assigned-to
               * type.
               */
              restype = ref_fulltype(type1);

              free_lpctype(check_binary_op_types(type1.t_type, type2.t_type, op_name, op_table, NULL,
                pragma_rtt_checks ? &rttc2 : NULL, (type2.t_flags & TYPE_MOD_LITERAL) ? true : false));
          }
          else
          {
              /* No operator table, then both types must be compatible. */

              if(type2.t_type == lpctype_mixed && !type2.t_flags)
              {
                  restype.t_type = lpctype_mixed;
                  rttc2 = ref_lpctype(type1.t_type);
              }
              else
              {
                  restype.t_type = get_common_type(type1.t_type, type2.t_type);
                  if (!restype.t_type)
                      yyerrorf("Bad assignment %s", get_two_fulltypes(type1, type2));
                  else if ((type2.t_flags & TYPE_MOD_LITERAL) && !lpctype_contains(type2.t_type, type1.t_type))
                      yyerrorf("Bad assignment %s", get_two_fulltypes(type1, type2));
                  else
                      rttc2 = ref_lpctype(type1.t_type);
              }
              restype.t_flags = type2.t_flags;
          }

          /* Special checks for struct assignments */
          if (is_type_struct(type1.t_type) || is_type_struct(type2.t_type))
          {
              free_fulltype(restype);
              restype = ref_fulltype(type1);
              if ($2 != F_ASSIGN)
                  yyerror("Only plain assignment allowed for structs");
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

              if (rttc2)
                 add_type_check(rttc2, TYPECHECK_ASSIGNMENT);

              /* Insert the SWAP and the ASSIGN */

              ins_f_code(F_SWAP_VALUES);
              ins_f_code(F_ASSIGN);
          }
          else
          {
              if (rttc2)
                 add_type_check(rttc2, TYPECHECK_ASSIGNMENT);

              if (!add_lvalue_code($1.lvalue, $2))
              {
                  free_lpctype($1.type);
                  free_fulltype($4.type);
                  free_fulltype(restype);
                  free_lpctype(rttc2);
                  YYACCEPT;
              }
          }
          free_lpctype(rttc2);

          $$.type = restype;

          /* For a normal assignment the lvalue is not marked as
           * read, even if the resulting expr0 is used, because the
           * assignment itself is then still superfluous. For all
           * other assignments the lvalue's content is used.
           */
          if ($2 == F_ASSIGN)
              $$.name = NULL;
          else
              $$.name = $1.name;

          use_variable($4.name, VAR_USAGE_READ);
          if ($4.type.t_flags & TYPE_MOD_REFERENCE)
              use_variable($1.name, VAR_USAGE_READWRITE);

          free_lpctype($1.type);
          free_fulltype($4.type);
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | error L_ASSIGN expr0  %prec L_ASSIGN
      {
          yyerror("Bad assignment: illegal lhs (target)");

          $$ = $3;
          $$.type.t_type = lpctype_mixed;
          $$.type.t_flags = 0;
          $$.lvalue = (lvalue_block_t) {0, 0};
          free_fulltype($3.type);
          free_lvalue_block($3.lvalue);
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr0 '?'
      {
          use_variable($1.name, VAR_USAGE_READ);
          check_unknown_type($1.type.t_type);

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
          $$.type = get_fulltype(get_union_type(type1.t_type, type2.t_type));
          $$.name = NULL;
          $$.lvalue = (lvalue_block_t) {0, 0};

          use_variable($4.name, VAR_USAGE_READ);
          use_variable($7.name, VAR_USAGE_READ);

          free_fulltype($1.type);
          free_fulltype($4.type);
          free_fulltype($7.type);
          free_lvalue_block($7.lvalue);
          free_lvalue_block($4.lvalue);
          free_lvalue_block($1.lvalue);
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr0 L_LOR %prec L_LOR
      {
          use_variable($1.name, VAR_USAGE_READ);
          check_unknown_type($1.type.t_type);

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

          /* Determine the result type */
          $$.type.t_type = get_union_type($1.type.t_type, $4.type.t_type);
          $$.type.t_flags = 0;
          $$.name = $4.name;
          $$.lvalue = (lvalue_block_t) {0, 0};

          free_fulltype($1.type);
          free_fulltype($4.type);
          free_lvalue_block($4.lvalue);
          free_lvalue_block($1.lvalue);
      } /* LOR */

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr0 L_LAND %prec L_LAND
      {
          use_variable($1.name, VAR_USAGE_READ);
          check_unknown_type($1.type.t_type);

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

          /* Determine the result type */
          $$.type = $4.type; /* It's the second value or zero. */
          $$.name = $4.name;
          $$.lvalue = (lvalue_block_t) {0, 0};

          free_fulltype($1.type);
          free_lvalue_block($4.lvalue);
          free_lvalue_block($1.lvalue);
       } /* LAND */

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr0 '|' expr0
      {
          lpctype_t *result = check_binary_op_types($1.type.t_type, $3.type.t_type, "|", types_binary_or, lpctype_mixed, NULL, false);

          $$ = $1;
          $$.type = get_fulltype(result);
          $$.name = NULL;
          $$.lvalue = (lvalue_block_t) {0, 0};

          use_variable($1.name, VAR_USAGE_READ);
          use_variable($3.name, VAR_USAGE_READ);

          free_fulltype($1.type);
          free_fulltype($3.type);
          free_lvalue_block($3.lvalue);
          free_lvalue_block($1.lvalue);

          ins_f_code(F_OR);
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr0 '^' expr0
      {
          lpctype_t *result = check_binary_op_types($1.type.t_type, $3.type.t_type, "^", types_binary_or, lpctype_mixed, NULL, false);

          $$ = $1;
          $$.type = get_fulltype(result);
          $$.name = NULL;
          $$.lvalue = (lvalue_block_t) {0, 0};

          use_variable($1.name, VAR_USAGE_READ);
          use_variable($3.name, VAR_USAGE_READ);

          free_fulltype($1.type);
          free_fulltype($3.type);
          free_lvalue_block($3.lvalue);
          free_lvalue_block($1.lvalue);

          ins_f_code(F_XOR);
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr0 '&' expr0
      {
          lpctype_t *result = check_binary_op_types($1.type.t_type, $3.type.t_type, "&", types_binary_and, lpctype_mixed, NULL, false);

          $$ = $1;
          $$.type = get_fulltype(result);
          $$.name = NULL;
          $$.lvalue = (lvalue_block_t) {0, 0};

          use_variable($1.name, VAR_USAGE_READ);
          use_variable($3.name, VAR_USAGE_READ);

          free_fulltype($1.type);
          free_fulltype($3.type);
          free_lvalue_block($3.lvalue);
          free_lvalue_block($1.lvalue);

          ins_f_code(F_AND);
      } /* end of '&' code */

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr0 L_EQ expr0
      {
          lpctype_t *result = check_binary_op_types($1.type.t_type, $3.type.t_type, NULL, types_equality, NULL, NULL, false);

          if (result == NULL)
          {
              yyerrorf("== always false because of different types %s"
                      , get_two_fulltypes($1.type, $3.type));
          }

          use_variable($1.name, VAR_USAGE_READ);
          use_variable($3.name, VAR_USAGE_READ);

          free_fulltype($1.type);
          free_fulltype($3.type);
          free_lvalue_block($3.lvalue);
          free_lvalue_block($1.lvalue);
          free_lpctype(result);

          ins_f_code(F_EQ);

          $$ = $1;
          $$.type = get_fulltype(lpctype_int);
          $$.name = NULL;
          $$.lvalue = (lvalue_block_t) {0, 0};
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr0 L_NE expr0
      {
          lpctype_t *result = check_binary_op_types($1.type.t_type, $3.type.t_type, NULL, types_equality, NULL, NULL, false);

          if (result == NULL)
          {
              yyerrorf("!= always true because of different types %s"
                      , get_two_fulltypes($1.type, $3.type));
          }

          use_variable($1.name, VAR_USAGE_READ);
          use_variable($3.name, VAR_USAGE_READ);

          free_fulltype($1.type);
          free_fulltype($3.type);
          free_lvalue_block($3.lvalue);
          free_lvalue_block($1.lvalue);
          free_lpctype(result);

          ins_f_code(F_NE);

          $$ = $1;
          $$.type = get_fulltype(lpctype_int);
          $$.name = NULL;
          $$.lvalue = (lvalue_block_t) {0, 0};
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr0 '>'  expr0
      {
          check_unknown_type($1.type.t_type);
          check_unknown_type($3.type.t_type);

          $$ = $1;
          $$.type = get_fulltype(lpctype_int);
          $$.name = NULL;
          $$.lvalue = (lvalue_block_t) {0, 0};

          use_variable($1.name, VAR_USAGE_READ);
          use_variable($3.name, VAR_USAGE_READ);

          free_fulltype($1.type);
          free_fulltype($3.type);
          free_lvalue_block($3.lvalue);
          free_lvalue_block($1.lvalue);

          ins_f_code(F_GT);
      }
    | expr0 L_GE  expr0
      {
          check_unknown_type($1.type.t_type);
          check_unknown_type($3.type.t_type);

          $$ = $1;
          $$.type = get_fulltype(lpctype_int);
          $$.name = NULL;
          $$.lvalue = (lvalue_block_t) {0, 0};

          use_variable($1.name, VAR_USAGE_READ);
          use_variable($3.name, VAR_USAGE_READ);

          free_fulltype($1.type);
          free_fulltype($3.type);
          free_lvalue_block($3.lvalue);
          free_lvalue_block($1.lvalue);

          ins_f_code(F_GE);
      }
    | expr0 '<'  expr0
      {
          check_unknown_type($1.type.t_type);
          check_unknown_type($3.type.t_type);

          $$ = $1;
          $$.type = get_fulltype(lpctype_int);
          $$.name = NULL;
          $$.lvalue = (lvalue_block_t) {0, 0};

          use_variable($1.name, VAR_USAGE_READ);
          use_variable($3.name, VAR_USAGE_READ);

          free_fulltype($1.type);
          free_fulltype($3.type);
          free_lvalue_block($3.lvalue);
          free_lvalue_block($1.lvalue);

          ins_f_code(F_LT);
      }
    | expr0 L_LE  expr0
      {
          check_unknown_type($1.type.t_type);
          check_unknown_type($3.type.t_type);

          $$ = $1;
          $$.type = get_fulltype(lpctype_int);
          $$.name = NULL;
          $$.lvalue = (lvalue_block_t) {0, 0};

          use_variable($1.name, VAR_USAGE_READ);
          use_variable($3.name, VAR_USAGE_READ);

          free_fulltype($1.type);
          free_fulltype($3.type);
          free_lvalue_block($3.lvalue);
          free_lvalue_block($1.lvalue);

          ins_f_code(F_LE);
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr0 L_LSH expr0
      {
          /* Just check the types. */
          free_lpctype(check_binary_op_types($1.type.t_type, $3.type.t_type, "<<", types_shift, NULL, NULL, false));

          $$ = $1;
          $$.type = get_fulltype(lpctype_int);
          $$.name = NULL;
          $$.lvalue = (lvalue_block_t) {0, 0};

          use_variable($1.name, VAR_USAGE_READ);
          use_variable($3.name, VAR_USAGE_READ);

          free_fulltype($1.type);
          free_fulltype($3.type);
          free_lvalue_block($3.lvalue);
          free_lvalue_block($1.lvalue);

          ins_f_code(F_LSH);
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr0 L_RSH expr0
      {
          free_lpctype(check_binary_op_types($1.type.t_type, $3.type.t_type, ">>", types_shift, NULL, NULL, false));

          $$ = $1;
          $$.type = get_fulltype(lpctype_int);
          $$.name = NULL;
          $$.lvalue = (lvalue_block_t) {0, 0};

          use_variable($1.name, VAR_USAGE_READ);
          use_variable($3.name, VAR_USAGE_READ);

          free_fulltype($1.type);
          free_fulltype($3.type);
          free_lvalue_block($3.lvalue);
          free_lvalue_block($1.lvalue);

          ins_f_code(F_RSH);
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr0 L_RSHL expr0
      {
          free_lpctype(check_binary_op_types($1.type.t_type, $3.type.t_type, ">>>", types_shift, NULL, NULL, false));

          $$ = $1;
          $$.type = get_fulltype(lpctype_int);
          $$.name = NULL;
          $$.lvalue = (lvalue_block_t) {0, 0};

          use_variable($1.name, VAR_USAGE_READ);
          use_variable($3.name, VAR_USAGE_READ);

          free_fulltype($1.type);
          free_fulltype($3.type);
          free_lvalue_block($3.lvalue);
          free_lvalue_block($1.lvalue);

          ins_byte(F_RSHL);
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
          string_t *str1 = NULL, *str2 = NULL;
          if (last_expression + 2 == current_size
           && $<numbers>3[0] + 4 == (mp_int)current_size
           && ((p[-2]-(F_CSTRING0)) & ~3) == 0
           && ((p[-4]-(F_CSTRING0)) & ~3) == 0
             )
          {
              /* Retrieve both strings from the A_STRINGS area
               */
              str1 = PROG_STRING( p[-3] | (p[-4]-(F_CSTRING0))<<8 );
              str2 = PROG_STRING( p[-1] | (p[-2]-(F_CSTRING0))<<8 );
          }
          if (str1 && str2
           && (str1->info.unicode == STRING_BYTES) == (str2->info.unicode == STRING_BYTES)
            )
          {
              /* Yup, we can combine the two strings.
               */
              string_t *sum;
              int i;

              /* Catenate the strings.
               */
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
              $$.type = get_fulltype(lpctype_string);
          }
          else
          {
              /* Just add */
              lpctype_t *result = check_binary_op_types($1.type.t_type, $4.type.t_type, "+", types_addition, lpctype_mixed, NULL, false);
              $$.type = get_fulltype(result);

              ins_f_code(F_ADD);
          }

          $$.name = NULL;
          $$.lvalue = (lvalue_block_t) {0, 0};

          use_variable($1.name, VAR_USAGE_READ);
          use_variable($4.name, VAR_USAGE_READ);

          free_fulltype($1.type);
          free_fulltype($4.type);
          free_lvalue_block($4.lvalue);
          free_lvalue_block($1.lvalue);
      } /* '+' */

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr0 '-' expr0
      {
%line
          $$ = $1;
          lpctype_t *result = check_binary_op_types($1.type.t_type, $3.type.t_type, "-", types_subtraction, lpctype_mixed, NULL, false);
          $$.type = get_fulltype(result);
          $$.name = NULL;
          $$.lvalue = (lvalue_block_t) {0, 0};

          use_variable($1.name, VAR_USAGE_READ);
          use_variable($3.name, VAR_USAGE_READ);

          ins_f_code(F_SUBTRACT);
          free_fulltype($1.type);
          free_fulltype($3.type);
          free_lvalue_block($3.lvalue);
          free_lvalue_block($1.lvalue);
      } /* '-' */

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr0 '*' expr0
      {
          $$ = $1;
          lpctype_t *result = check_binary_op_types($1.type.t_type, $3.type.t_type, "*", types_multiplication, lpctype_mixed, NULL, false);
          $$.type = get_fulltype(result);
          $$.name = NULL;
          $$.lvalue = (lvalue_block_t) {0, 0};

          use_variable($1.name, VAR_USAGE_READ);
          use_variable($3.name, VAR_USAGE_READ);

          ins_f_code(F_MULTIPLY);
          free_fulltype($1.type);
          free_fulltype($3.type);
          free_lvalue_block($3.lvalue);
          free_lvalue_block($1.lvalue);
      } /* '*' */

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr0 '%' expr0
      {
          $$ = $1;
          lpctype_t *result = check_binary_op_types($1.type.t_type, $3.type.t_type, "%", types_modulus, lpctype_int, NULL, false);
          $$.type = get_fulltype(result);
          $$.name = NULL;
          $$.lvalue = (lvalue_block_t) {0, 0};

          use_variable($1.name, VAR_USAGE_READ);
          use_variable($3.name, VAR_USAGE_READ);

          ins_f_code(F_MOD);
          free_fulltype($1.type);
          free_fulltype($3.type);
          free_lvalue_block($3.lvalue);
          free_lvalue_block($1.lvalue);
      }
    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr0 '/' expr0
      {
          $$ = $1;
          lpctype_t *result = check_binary_op_types($1.type.t_type, $3.type.t_type, "/", types_division, lpctype_int, NULL, false);
          $$.type = get_fulltype(result);
          $$.name = NULL;
          $$.lvalue = (lvalue_block_t) {0, 0};

          use_variable($1.name, VAR_USAGE_READ);
          use_variable($3.name, VAR_USAGE_READ);

          ins_f_code(F_DIVIDE);
          free_fulltype($1.type);
          free_fulltype($3.type);
          free_lvalue_block($3.lvalue);
          free_lvalue_block($1.lvalue);
      } /* '/' */

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | decl_cast expr0 %prec '~'
      {
          /* Declarative casts are legal from general to more specialized types.
           * So we're getting the common type between the cast expression
           * and the value, and if it's different, that means, it's more specialized.
           *
           * We always allow casts from lpctype_unknown and lpctype_mixed
           * (because lpctype_unknown must be cast to avoid type errors,
           * and changing the strict_types pragma to strong_types - and therefore
           * the result type of call_other from unknown to mixed - should not
           * introduce additional errors).
           */

          $$ = $2;
          $$.lvalue = (lvalue_block_t) {0, 0};

          if ($2.type.t_type == lpctype_unknown || $2.type.t_type == lpctype_mixed)
          {
              $$.type = get_fulltype($1);
          }
          else
          {
              lpctype_t *result = get_common_type($1, $2.type.t_type);

              if(result == NULL || result == $2.type.t_type)
              {
                  /* No common type or not specialized. */
                  if ($2.type.t_type == $1)
                      yyerrorf("Declarative cast of a value to its own type: %s", get_lpctype_name($1));
                  else
                      yyerrorf("Declarative casts are only legal from general to specialized types: %s", get_two_lpctypes($2.type.t_type, $1));
              }

              if(result == NULL)
                  $$.type = get_fulltype($1);
              else
              {
                  $$.type = get_fulltype(result);
                  free_lpctype($1);
              }
          }

          add_type_check($$.type.t_type, TYPECHECK_DECL_CAST);

          free_fulltype($2.type);
          free_lvalue_block($2.lvalue);
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | cast expr0 %prec '~'
      {
          $$ = $2;
          $$.type = get_fulltype($1);
          $$.lvalue = (lvalue_block_t) {0, 0};

          /* We are trying to convert the value to the new type
           * and give an error, when we can't find a suitable conversion.
           * If the source or destination type is mixed or unknown,
           * we just do a declarative cast.
           */

          if($1 == lpctype_mixed)
          {
              if(pragma_warn_empty_casts)
              {
                  if (pragma_pedantic)
                      yyerrorf("Casting a value to mixed has no effect");
                  else
                      yywarnf("Casting a value to mixed has no effect");
              }
          }
          else if($2.type.t_type == lpctype_mixed || $2.type.t_type == lpctype_unknown)
          {
              if(pragma_warn_empty_casts)
              {
                  if (pragma_pedantic)
                      yyerrorf("Casting a value of an unknown type has no effect");
                  else
                      yywarnf("Casting a value of an unknown type has no effect");
              }
              add_type_check($$.type.t_type, TYPECHECK_CAST);
          }
          else if($1 == $2.type.t_type)
          {
              if(pragma_warn_empty_casts)
              {
                  if (pragma_pedantic)
                      yyerrorf("Casting a value to its own type: %s", get_lpctype_name($1));
                  else
                      yywarnf("Casting a value to its own type: %s", get_lpctype_name($1));
              }
              add_type_check($$.type.t_type, TYPECHECK_CAST);
          }
          else if($1 == lpctype_int)
          {
              ins_f_code(F_TO_INT);
          }
          else if($1 == lpctype_float)
          {
              ins_f_code(F_TO_FLOAT);
          }
          else if($1 == lpctype_string)
          {
              ins_f_code(F_TO_STRING);
          }
          else if($1 == lpctype_object)
          {
              ins_f_code(F_TO_OBJECT);
          }
          else if($1 == lpctype_int_array)
          {
              ins_f_code(F_TO_ARRAY);
          }
          else if(is_type_struct($1))
          {
              /* Do nothing, just adapt the type information */
              add_type_check($$.type.t_type, TYPECHECK_CAST);
          }
          else
          {
              lpctype_error("Illegal cast", $1);
          }

          free_fulltype($2.type);
          free_lvalue_block($2.lvalue);
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | pre_inc_dec lvalue %prec L_INC
      {
          lpctype_t *result;
          $$.start = $1.start;

          if (!add_lvalue_code($2.lvalue, $1.code))
          {
              free_lpctype($2.type);
              YYACCEPT;
          }

          result = get_common_type($2.type, lpctype_int_float);
          if(result == NULL)
          {
              argument_type_error($1.code, $2.type);
              result = lpctype_int_float;
          }

          $$.type = get_fulltype(result);
          $$.name = $2.name;
          $$.lvalue = (lvalue_block_t) {0, 0};

          free_lpctype($2.type);
      }
    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | L_NOT expr0
      {
          check_unknown_type($2.type.t_type);

          $$ = $2;
          last_expression = CURRENT_PROGRAM_SIZE;
          ins_f_code(F_NOT);        /* Any type is valid here. */
          $$.type = get_fulltype(lpctype_int);
          $$.lvalue = (lvalue_block_t) {0, 0};

          free_fulltype($2.type);
          free_lvalue_block($2.lvalue);
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '~' expr0
      {
%line
          $$ = $2;
          if (!check_unknown_type($2.type.t_type)
           && exact_types && !lpctype_contains(lpctype_int, $2.type.t_type))
              fulltype_error("Bad argument to ~", $2.type);

          ins_f_code(F_COMPL);
          $$.type = get_fulltype(lpctype_int);
          $$.lvalue = (lvalue_block_t) {0, 0};

          free_fulltype($2.type);
          free_lvalue_block($2.lvalue);
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '-' expr0 %prec '~'
      {
%line
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

          $$ = $2;
          $$.type = get_fulltype(check_unary_op_type($2.type.t_type, "unary '-'", types_unary_math, lpctype_mixed));
          $$.lvalue = (lvalue_block_t) {0, 0};

          free_fulltype($2.type);
          free_lvalue_block($2.lvalue);
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | lvalue L_INC %prec L_INC
      {
%line
          /* Create the code to push the lvalue plus POST_INC */
          $$.start = CURRENT_PROGRAM_SIZE;
          if (!add_lvalue_code($1.lvalue, F_POST_INC))
          {
              free_lpctype($1.type);
              YYACCEPT;
          }

          /* Check the types */
          $$.type = get_fulltype(check_unary_op_type($1.type, "++", types_unary_math, lpctype_mixed));
          $$.name = $1.name;
          $$.lvalue = (lvalue_block_t) {0, 0};

          free_lpctype($1.type);
      } /* post-inc */

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | lvalue L_DEC %prec L_DEC
      {
%line
          $$.start = CURRENT_PROGRAM_SIZE;

          /* Create the code to push the lvalue plus POST_DEC */
          if (!add_lvalue_code($1.lvalue, F_POST_DEC))
          {
              free_lpctype($1.type);
              YYACCEPT;
          }

          /* Check the types */
          $$.type = get_fulltype(check_unary_op_type($1.type, "--", types_unary_math, NULL));
          $$.name = $1.name;
          $$.lvalue = (lvalue_block_t) {0, 0};

          free_lpctype($1.type);
      } /* post-dec */

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | lvalue_reference %prec '~'

      {
          uint32 current = CURRENT_PROGRAM_SIZE;

          if (!$1.lvalue.size)
          {
              current = $1.lvalue.start;
          }
          else if(!add_lvalue_code($1.lvalue, F_MAKE_PROTECTED))
          {
              free_lpctype($1.type);
              YYACCEPT;
          }

          $$.start = current;
          $$.type = get_fulltype($1.type); /* Adapt the reference. */
          $$.type.t_flags |= TYPE_MOD_REFERENCE;
          $$.name = $1.name;
          $$.lvalue = (lvalue_block_t) {0, 0};
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | lvalue_reference L_ASSIGN expr0 %prec L_ASSIGN
      {
          fulltype_t type1, type2, restype;
%line
          $$ = $3;

          if ($2 != F_ASSIGN)
              yyerror("Only plain assignments allowed with references");

          /* Change the instruction into a reseating instruction. */
          if ($1.vlvalue_inst)
              LVALUE_BLOCK[$1.lvalue.start + $1.lvalue.size - $1.num_arg - 1] = $1.vlvalue_inst;
          else
              yyerror("Illegal rhs for assignment");

          type1.t_type = $1.type;
          type1.t_flags = 0;
          type2 = $3.type;

          if (check_unknown_type(type2.t_type))
              restype.t_type = lpctype_mixed;
          else if(type2.t_type == lpctype_mixed && !type2.t_flags)
              restype.t_type = lpctype_mixed;
          else
              restype.t_type = get_common_type(type1.t_type, type2.t_type);
          restype.t_flags = type2.t_flags;

          if (exact_types
           && (!restype.t_type
            || ((type2.t_flags & TYPE_MOD_LITERAL) && !lpctype_contains(type2.t_type, type1.t_type))))
          {
              yyerrorf("Bad assignment %s", get_two_fulltypes(type1, type2));
              restype = ref_fulltype(type1);
          }

          /* Special checks for struct assignments */
          if (is_type_struct(type1.t_type) || is_type_struct(type2.t_type))
          {
              free_fulltype(restype);
              restype = ref_fulltype(type1);
          }

          add_type_check(type1.t_type, TYPECHECK_ASSIGNMENT);
          if (!add_lvalue_code($1.lvalue, $2))
          {
              free_lpctype($1.type);
              free_fulltype($3.type);
              free_fulltype(restype);
              YYACCEPT;
          }

          $$.type = restype;
          $$.name = NULL;
          $$.lvalue = (lvalue_block_t) {0, 0};

          use_variable($3.name, VAR_USAGE_READ);
          if ($3.type.t_flags & TYPE_MOD_REFERENCE)
              use_variable($1.name, VAR_USAGE_READWRITE);

          free_lpctype($1.type);
          free_fulltype($3.type);
          free_lvalue_block($3.lvalue);
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr4
      {
%line
          $$ = $1;
      }

; /* expr0 */


lvalue_reference:
    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
      '&' name_lvalue
      {
          $$ = $2;
          use_variable($2.name, VAR_USAGE_READWRITE);
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '&' '(' lvalue ')'
      {
          $$ = $3;
          use_variable($3.name, VAR_USAGE_READWRITE);
      }
    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '&' '(' function_call ')'
      {
          /* We abuse the .lvalue.start to save the start
           * of the expression in the program code.
           */
          $$.lvalue = (lvalue_block_t) {$3.start, 0};
          $$.type = $3.type.t_type;
          $$.vlvalue_inst = 0;
          $$.num_arg = 0;
          $$.name = 0;
      }
; /* lvalue_reference */



pre_inc_dec:
      L_INC { $$.code = F_PRE_INC; $$.start = CURRENT_PROGRAM_SIZE; }
    | L_DEC { $$.code = F_PRE_DEC; $$.start = CURRENT_PROGRAM_SIZE; }
;


expr4:
      function_call  %prec '~'
      {
          /* And add an opcode to make it into an rvalue,
           * just to be on the safe side.
           */
          if ($1.might_lvalue)
              ins_f_code(F_MAKE_RVALUE);

          $$.lvalue = (lvalue_block_t) {0, 0};
          $$.start =  $1.start;
          $$.type =   $1.type;
          $$.name =   NULL;
      }
    | inline_func    %prec '~'
      {
          $$.lvalue = (lvalue_block_t) {0, 0};
          $$.start =  $1.start;
          $$.type =   $1.type;
          $$.name =   NULL;
      }
    | catch          %prec '~'
      {
          $$.lvalue = (lvalue_block_t) {0, 0};
          $$.start =  $1.start;
          $$.type =   $1.type;
          $$.name =   NULL;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | L_STRING
      {
          /* Push a constant string */

          string_t *p;
%line
          p = last_lex_string;
          last_lex_string = NULL;
          $$.start = last_expression = CURRENT_PROGRAM_SIZE;
          $$.type = get_fulltype_flags(lpctype_string, TYPE_MOD_LITERAL);
          $$.lvalue = (lvalue_block_t) {0, 0};
          $$.name =   NULL;

          ins_prog_string(p);
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | L_BYTES L_BYTES
      {
          /* The rule forces the parser to look ahead and
           * thus lets the lexer execute any concatenation.
           */
          fatal("failed to concatenate byte sequence");

          /* Just so yacc doesn't complain... */
          $$.start = CURRENT_PROGRAM_SIZE;
          $$.type = get_fulltype_flags(lpctype_bytes, TYPE_MOD_LITERAL);
          $$.lvalue = (lvalue_block_t) {0, 0};
          $$.name =   NULL;
      }

    | L_BYTES
      {
          /* Push a constant byte sequence */

          string_t *p;
%line
          p = last_lex_string;
          last_lex_string = NULL;
          $$.start = last_expression = CURRENT_PROGRAM_SIZE;
          $$.type = get_fulltype_flags(lpctype_bytes, TYPE_MOD_LITERAL);
          $$.lvalue = (lvalue_block_t) {0, 0};
          $$.name =   NULL;

          ins_prog_string(p);
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
          $$.lvalue = (lvalue_block_t) {0, 0};
          $$.name =   NULL;
          number = $1;
          if ( number == 0 )
          {
              current++;
              add_f_code(F_CONST0);
              $$.type = get_fulltype(lpctype_mixed);
              /* TODO: Introduce a TYPE_NULL instead */
          }
          else if ( number == 1 )
          {
              add_f_code(F_CONST1);
              current++;
              $$.type = get_fulltype_flags(lpctype_int, TYPE_MOD_LITERAL);
          }
          else if ( number >= 0 && number <= 0xff )
          {
              add_f_code(F_CLIT);
              add_byte(number);
              current += 2;
              $$.type = get_fulltype_flags(lpctype_int, TYPE_MOD_LITERAL);
          }
          else if ( number < 0 && number >= -0x0ff )
          {
              add_f_code(F_NCLIT);
              add_byte(-number);
              current += 2;
              $$.type = get_fulltype_flags(lpctype_int, TYPE_MOD_LITERAL);
          }
          else
          {
              add_f_code(F_NUMBER);
              upd_p_int((char*)__PREPARE_INSERT__p - mem_block[A_PROGRAM].block, $1);
              current += 1 + sizeof (p_int);
              $$.type = get_fulltype_flags(lpctype_int, TYPE_MOD_LITERAL);
          }
          CURRENT_PROGRAM_SIZE = current;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | L_CLOSURE
      {
          int ix, inhIndex;

          $$.start = CURRENT_PROGRAM_SIZE;
          $$.lvalue = (lvalue_block_t) {0, 0};
          $$.name =   NULL;
          if (!pragma_warn_deprecated)
              ins_byte(F_NO_WARN_DEPRECATED);
          ix = $1.number;
          inhIndex = $1.inhIndex;

          // check for deprecated functions
%ifdef USE_PYTHON
          if (ix < CLOSURE_PYTHON_EFUN_OFFS)
%else
          if (ix < CLOSURE_EFUN_OFFS)
%endif
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
                  int varidx = ix - CLOSURE_IDENTIFIER_OFFS - num_virtual_variables;
                  variable_t *varp = NV_VARIABLE(varidx);

                  if (varp->type.t_flags & TYPE_MOD_DEPRECATED)
                      yywarnf("Creating closure to deprecated global variable %s.\n",
                              get_txt(varp->name));
                  GLOBAL_VARIABLE(varidx).usage = VAR_USAGE_READWRITE;
              }
          }
          ins_f_code(F_CLOSURE);
          ins_short(ix);
          ins_short(inhIndex);
          $$.type = get_fulltype_flags(lpctype_closure, TYPE_MOD_LITERAL);
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | L_SYMBOL
      {
          /* Generate a symbol */

          int string_number;
          int quotes;

          $$.start = CURRENT_PROGRAM_SIZE;
          $$.lvalue = (lvalue_block_t) {0, 0};
          $$.name =   NULL;
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
          $$.type = get_fulltype_flags(lpctype_symbol, TYPE_MOD_LITERAL);
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | L_FLOAT
      {
          /* Generate a float literal */

          $$.start = CURRENT_PROGRAM_SIZE;
          $$.lvalue = (lvalue_block_t) {0, 0};
          $$.name =   NULL;
          ins_f_code(F_FLOAT);
#ifdef FLOAT_FORMAT_2
          ins_double($1);
#else
          int exponent;
          ins_uint32 ( SPLIT_DOUBLE( $1, &exponent) );
          ins_uint16 ( exponent );
#endif  /* FLOAT_FORMAT_2 */
          $$.type = get_fulltype_flags(lpctype_float, TYPE_MOD_LITERAL);
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '(' note_start comma_expr ')'        %prec '~'
      {
          /* A nested expression */

          $$.type = $3.type;
          $$.start = $2;
          $$.lvalue = (lvalue_block_t) {0, 0};
          $$.name = $3.name;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '(' '{' note_start expr_list '}' ')' %prec '~'
      {
          /* Generate an array */

          ins_f_code(F_AGGREGATE);
          ins_short($4);
          if (max_array_size && $4 > (p_int)max_array_size)
              yyerror("Illegal array size");
          $$.type = get_aggregate_type($4);
          $$.start = $3;
          $$.lvalue = (lvalue_block_t) {0, 0};
          $$.name =   NULL;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | L_QUOTED_AGGREGATE note_start expr_list '}' ')' %prec '~'
      {
          /* Generate a quoted array by generating a normal
           * array first and then applying QUOTE as often
           * as possible.
           */

          int quotes;

          pop_arg_stack($3);

          ins_f_code(F_AGGREGATE);
          ins_short($3);
          if (max_array_size && $3 > (p_int)max_array_size)
              yyerror("Illegal array size");
          $$.type = get_fulltype_flags(lpctype_quoted_array, TYPE_MOD_LITERAL);
          $$.start = $2;
          $$.lvalue = (lvalue_block_t) {0, 0};
          $$.name =   NULL;
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

          use_variable($6.name, VAR_USAGE_READ);
          check_unknown_type($6.type.t_type);

          $$.type = get_fulltype_flags(lpctype_mapping, TYPE_MOD_LITERAL);
          $$.start = $4;
          $$.lvalue = (lvalue_block_t) {0, 0};
          $$.name = NULL;

          free_fulltype($6.type);
          free_lvalue_block($6.lvalue);
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '(' '[' note_start m_expr_list ']' ')'
      {
          /* Generate a mapping */

          mp_int num_keys;

          pop_arg_stack($4[0]);
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

          $$.type = get_fulltype_flags(lpctype_mapping, TYPE_MOD_LITERAL);
          $$.start = $3;
          $$.lvalue = (lvalue_block_t) {0, 0};
          $$.name =   NULL;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '(' '<' note_start '>' ')'
      {
          yyerror("Missing identifier for empty struct literal");
          $$.type = get_fulltype(lpctype_unknown);
          $$.start = $3;
          $$.lvalue = (lvalue_block_t) {0, 0};
          $$.name =   NULL;
      }
    | '(' '<' note_start error ')'
      {
          /* Rule allows the parser to resynchronize after errors */
          $$.type = get_fulltype(lpctype_unknown);
          $$.start = $3;
          $$.lvalue = (lvalue_block_t) {0, 0};
          $$.name =   NULL;
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
              CURRENT_PROGRAM_SIZE = $6;
              create_struct_literal(pdef, 0, NULL);
          }
          else if (!create_struct_literal(pdef, $7.length, $7.list))
          {
              /* Creation failed - create an empty struct */
              CURRENT_PROGRAM_SIZE = $6;
              create_struct_literal(pdef, 0, NULL);
          }

          /* Free the list of member descriptors */
          while ($7.list != NULL)
          {
              struct_init_t * p = $7.list;
              $7.list = p->next;
              if (p->name != NULL)
                  free_mstring(p->name);
              free_fulltype(p->type);
              xfree(p);
          }

          $$.type = get_fulltype_flags(get_struct_type(pdef->type), TYPE_MOD_LITERAL);
          $$.start = $6;
          $$.lvalue = (lvalue_block_t) {0, 0};
          $$.name =   NULL;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
%// The following expressions can be patched to lvalues for use in index_lvalue.
    | L_IDENTIFIER
      {
          /* Access a local or global variable */
          mp_uint current;
          bytecode_p p;
          ident_t *varident;
%line
          varident = get_initialized_variable($1);
          if (!varident)
              /* variable not declared */
              YYACCEPT;

          $$.start = current = CURRENT_PROGRAM_SIZE;

          if (!realloc_a_program(3))
          {
              yyerrorf("Out of memory: program size %"PRIuMPINT"\n", current+3);
              YYACCEPT;
          }
          p = PROGRAM_BLOCK + current;

          if (varident->type == I_TYPE_GLOBAL)
          {
              /* A global variable. */
              int i = varident->u.global.variable;
              variable_t *varp;

              if (i & VIRTUAL_VAR_TAG)
              {
                  /* Access a virtual variable */

                  bytecode_p q;

                  $$.lvalue = alloc_lvalue_block(2);
                  q = LVALUE_BLOCK + $$.lvalue.start;

                  q[0] = F_PUSH_VIRTUAL_VARIABLE_LVALUE;
                  q[1] = i;

                  *p++ = F_VIRTUAL_VARIABLE;
                  *p = i;

                  varp = V_VARIABLE(i);
                  $$.type = ref_fulltype(varp->type);
              }
              else
              {
                  /* Access a non-virtual variable */

                  if (i & ~0xff)
                  {
                      bytecode_p q;

                      $$.lvalue = alloc_lvalue_block(3);
                      q = LVALUE_BLOCK + $$.lvalue.start;

                      q[0] = F_PUSH_IDENTIFIER16_LVALUE;
                      PUT_SHORT(q+1, i);

                      *p = F_IDENTIFIER16;
                      upd_short(++current, i);
                  }
                  else
                  {
                      bytecode_p q;

                      $$.lvalue = alloc_lvalue_block(2);
                      q = LVALUE_BLOCK + $$.lvalue.start;

                      q[0] = F_PUSH_IDENTIFIER_LVALUE;
                      q[1] = i;

                      *p++ = F_IDENTIFIER;
                      *p = i;
                  }
                  varp = NV_VARIABLE(i);
                  $$.type = ref_fulltype(varp->type);
              }

              if (varp->type.t_flags & TYPE_MOD_DEPRECATED)
              {
                  yywarnf("Using deprecated global variable %s.\n",
                          get_txt(varp->name));
              }

              $$.type.t_flags = 0;

              CURRENT_PROGRAM_SIZE = current + 2;
          }
          else
          {
              /* A local variable. */
              bytecode_p q;
              lpctype_t *type;

              varident = check_for_context_local(varident, &type);

              $$.type = get_fulltype(ref_lpctype(type));

              $$.lvalue = alloc_lvalue_block(2);
              q = LVALUE_BLOCK + $$.lvalue.start;

              if (varident->u.local.context >= 0)
              {
                  q[0] = F_PUSH_CONTEXT_LVALUE;
                  q[1] = varident->u.local.context;

                  *p++ = F_CONTEXT_IDENTIFIER;
                  *p = varident->u.local.context;
              }
              else
              {
                  q[0] = F_PUSH_LOCAL_VARIABLE_LVALUE;
                  q[1] = varident->u.local.num;

                  *p++ = F_LOCAL;
                  *p = varident->u.local.num;
              }
              CURRENT_PROGRAM_SIZE = current + 2;
          }

          $$.name = varident;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr4 member_operator struct_member_name %prec L_ARROW
      {
          /* Lookup a struct member */
          short s_index = -1;
          int m_index = -1;

%line
          lpctype_t* result = get_struct_member_result_type($1.type.t_type, $3, $2.strict_member, &s_index, &m_index);
          if (!result)
              result = lpctype_mixed;

          /* We don't need the lvalue for <expr4>. */
          free_lvalue_block($1.lvalue);

          $$.start = $1.start;
          $$.type = get_fulltype(result);

          if ($3 != NULL) /* Compile time lookup. */
          {
              if (s_index == FSM_AMBIGUOUS) /* Not anymore. */
                  ins_prog_string($3);
              else
              {
                  ins_number(m_index);
                  free_mstring($3);
              }
          }

          ins_number(s_index);

          /* Put that expression also into an lvalue buffer. */
          $$.lvalue = compose_lvalue_block((lvalue_block_t) {0, 0}, 0, $1.start, $2.strict_member ? F_S_INDEX_LVALUE : F_SX_INDEX_LVALUE);
          $$.name = NULL;

          use_variable($1.name, VAR_USAGE_READ);
          ins_f_code($2.strict_member ? F_S_INDEX : F_SX_INDEX);

          free_fulltype($1.type);
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr4 index_expr %prec '['
      {
%line
          /* Generate (R)INDEX/PUSH_(R)INDEXED_LVALUE */

          $$.start = $1.start;

          /* Is <expr4> already an lvalue? */
          if ($1.lvalue.size > 0)
          {
              /* Make <expr4> a protected lvalue, because we have to
               * evaluate the index expression first, before using it.
               */
              $$.lvalue = compose_lvalue_block($1.lvalue, F_MAKE_PROTECTED, $2.start, $2.lvalue_inst);
          }
          else
          {
              /* We can just copy the instruction block
               * and add a the index operation.
               */
              $$.lvalue = compose_lvalue_block((lvalue_block_t) {0, 0}, 0, $1.start, $2.lvalue_inst);
          }

          ins_f_code($2.rvalue_inst);

          /* Check and compute the types */
          $$.type = get_fulltype(get_index_result_type($1.type.t_type, $2.type1, $2.rvalue_inst, lpctype_mixed));
          $$.name = NULL;

          use_variable($1.name, VAR_USAGE_READ);

          free_fulltype($1.type);
          free_fulltype($2.type1);
          free_fulltype($2.type2);
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr4 index_range %prec '['
      {
%line
          /* Generate a range expression */
          $$.start = $1.start;

          /* Is <expr4> already an lvalue? */
          if ($1.lvalue.size > 0)
          {
              /* Make <expr4> a protected lvalue, because we have to
               * evaluate the index expression first, before using it.
               */
              $$.lvalue = compose_lvalue_block($1.lvalue, F_MAKE_PROTECTED, $2.start, $2.lvalue_inst);
          }
          else
          {
              /* We can just copy the instruction block
               * and add a the index operation.
               */
              $$.lvalue = compose_lvalue_block((lvalue_block_t) {0, 0}, 0, $1.start, $2.lvalue_inst);
          }

          ins_f_code($2.rvalue_inst);

          /* Check the types */
          $$.type = get_fulltype(check_unary_op_type($1.type.t_type, "range index", types_range_index, lpctype_mixed));
          $$.name = NULL;

          use_variable($1.name, VAR_USAGE_READ);

          if (!lpctype_contains(lpctype_int, $2.type1.t_type))
              fulltype_error("Bad type of index", $2.type1);
          if (!lpctype_contains(lpctype_int, $2.type2.t_type))
              fulltype_error("Bad type of index", $2.type2);

          free_fulltype($1.type);
          free_fulltype($2.type1);
          free_fulltype($2.type2);
      } /* expr4 index_range */

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr4 '[' expr0 ',' expr0 ']' %prec '['
      {
%line
          /* Generate MAP_INDEX/PUSH_INDEXED_MAP_LVALUE */

          $$.start = $1.start;

          /* Well, just generate the code: expr4 must be
           * a mapping, or a runtime error will occur.
           * Therefore we don't need its lvalue.
           */
          free_lvalue_block($5.lvalue);
          free_lvalue_block($3.lvalue);
          free_lvalue_block($1.lvalue);
          $$.lvalue = compose_lvalue_block((lvalue_block_t) {0, 0}, 0, $1.start, F_MAP_INDEX_LVALUE);
          $$.type = get_fulltype(lpctype_mixed);
          $$.name = NULL;

          ins_f_code(F_MAP_INDEX);

          /* Check and compute types */
          if ($3.type.t_flags & TYPE_MOD_REFERENCE
           || $5.type.t_flags & TYPE_MOD_REFERENCE)
              yyerror("Reference used as index");

          check_unknown_type($1.type.t_type);

          if (exact_types && !lpctype_contains(lpctype_mapping, $1.type.t_type))
              fulltype_error("Bad type to indexed lvalue", $1.type);

          if (exact_types && !lpctype_contains(lpctype_int, $5.type.t_type))
              fulltype_error("Bad type of index", $5.type);

          use_variable($1.name, VAR_USAGE_READ);
          use_variable($3.name, VAR_USAGE_READ);
          use_variable($5.name, VAR_USAGE_READ);

          free_fulltype($1.type);
          free_fulltype($3.type);
          free_fulltype($5.type);
      }
; /* expr4 */


name_lvalue:
    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    L_IDENTIFIER
      {
          /* Generate the lvalue for a local or global variable */
          ident_t *varident;
%line
          varident = get_initialized_variable($1);
          if (!varident)
              /* variable not declared */
              YYACCEPT;

          if (varident->type == I_TYPE_GLOBAL)
          {
              /* A global variable. */
              int i = varident->u.global.variable;
              variable_t *varp;

              if (i & VIRTUAL_VAR_TAG)
              {
                  bytecode_p q;

                  $$.lvalue = alloc_lvalue_block(2);
                  q = LVALUE_BLOCK + $$.lvalue.start;

                  q[0] = F_PUSH_VIRTUAL_VARIABLE_LVALUE;
                  q[1] = i;
                  $$.vlvalue_inst = F_PUSH_VIRTUAL_VARIABLE_VLVALUE;
                  $$.num_arg = 1;

                  varp = V_VARIABLE(i);
                  $$.type = ref_lpctype(varp->type.t_type);
              }
              else
              {
                  if (i & ~0xff)
                  {
                      bytecode_p q;

                      $$.lvalue = alloc_lvalue_block(3);
                      q = LVALUE_BLOCK + $$.lvalue.start;

                      q[0] = F_PUSH_IDENTIFIER16_LVALUE;
                      PUT_SHORT(q+1, i);

                      $$.vlvalue_inst = F_PUSH_IDENTIFIER16_VLVALUE;
                      $$.num_arg = 2;
                  }
                  else
                  {
                      bytecode_p q;

                      $$.lvalue = alloc_lvalue_block(2);
                      q = LVALUE_BLOCK + $$.lvalue.start;

                      q[0] = F_PUSH_IDENTIFIER_LVALUE;
                      q[1] = i;

                      $$.vlvalue_inst = F_PUSH_IDENTIFIER_VLVALUE;
                      $$.num_arg = 1;
                  }
                  varp = NV_VARIABLE(i);
                  $$.type = ref_lpctype(varp->type.t_type);
              }
              if (varp->type.t_flags & TYPE_MOD_DEPRECATED)
                  yywarnf("Using deprecated global variable %s.\n",
                          get_txt(varp->name));
          }
          else
          {
              lpctype_t *type;
              bytecode_p q;
%line
              /* Generate the lvalue for a local */

              varident = check_for_context_local(varident, &type);

              $$.type = ref_lpctype(type);
              $$.lvalue = alloc_lvalue_block(2);
              q = LVALUE_BLOCK + $$.lvalue.start;

              if (varident->u.local.context >= 0)
              {
                  q[0] = F_PUSH_CONTEXT_LVALUE;
                  q[1] = varident->u.local.context;

                  $$.vlvalue_inst = F_PUSH_CONTEXT_VLVALUE;
                  $$.num_arg = 1;
              }
              else
              {
                  q[0] = F_PUSH_LOCAL_VARIABLE_LVALUE;
                  q[1] = varident->u.local.num;

                  $$.vlvalue_inst = F_PUSH_LOCAL_VARIABLE_VLVALUE;
                  $$.num_arg = 1;
              }
          }

          $$.name = varident;
          use_variable(varident, VAR_USAGE_WRITE);
      }
; /* name_lvalue */

lvalue:
    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    name_lvalue

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr4 index_expr %prec '['
      {
          /* Generate/add an (R)INDEX_LVALUE */

%line
          /* First change the rvalue 'expr4' into an lvalue.
           */
          if ($1.lvalue.size > 0)
          {
              /* Make <expr4> a protected lvalue, because we have to
               * evaluate the index expression first, before using it.
               */
              $$.lvalue = compose_lvalue_block($1.lvalue, F_MAKE_PROTECTED, $2.start, $2.lvalue_inst);
          }
          else
          {
              /* We can just copy the instruction block
               * and add a the index operation.
               */
              $$.lvalue = compose_lvalue_block((lvalue_block_t) {0, 0}, 0, $1.start, $2.lvalue_inst);
          }

          $$.name = NULL;
          $$.vlvalue_inst = $2.vlvalue_inst;
          $$.num_arg = 0;

          /* Remove the code from the program block. */
          CURRENT_PROGRAM_SIZE = $1.start;
          last_expression = -1;

          /* Check and compute the types */
          $$.type = get_index_result_type($1.type.t_type, $2.type1, $2.rvalue_inst, lpctype_mixed);

          use_variable($1.name, VAR_USAGE_READ);

          free_fulltype($1.type);
          free_fulltype($2.type1);
          free_fulltype($2.type2);
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr4 '[' expr0 ',' expr0 ']' %prec '['
      {
          /* Generate/add an PUSH_INDEXED_MAP_LVALUE */

%line

          /* Well, just generate the code: expr4 must be
           * a mapping, or a runtime error will occur.
           * Therefore we don't need its lvalue.
           */
          free_lvalue_block($5.lvalue);
          free_lvalue_block($3.lvalue);
          free_lvalue_block($1.lvalue);
          $$.lvalue = compose_lvalue_block((lvalue_block_t) {0, 0}, 0, $1.start, F_MAP_INDEX_LVALUE);
          $$.type = lpctype_mixed;
          $$.name = NULL;

          $$.vlvalue_inst = F_MAP_INDEX_VLVALUE;
          $$.num_arg = 0;

          /* Remove the code from the program block. */
          CURRENT_PROGRAM_SIZE = $1.start;
          last_expression = -1;

          /* Check and compute types */
          if ($3.type.t_flags & TYPE_MOD_REFERENCE
           || $5.type.t_flags & TYPE_MOD_REFERENCE)
              yyerror("Reference used as index");

          if (exact_types && !lpctype_contains(lpctype_mapping, $1.type.t_type))
              fulltype_error("Bad type to indexed lvalue", $1.type);

          if (exact_types && !lpctype_contains(lpctype_int, $5.type.t_type))
              fulltype_error("Bad type of index", $5.type);

          use_variable($1.name, VAR_USAGE_READ);
          use_variable($3.name, VAR_USAGE_READ);
          use_variable($5.name, VAR_USAGE_READ);

          free_fulltype($1.type);
          free_fulltype($3.type);
          free_fulltype($5.type);
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr4 index_range %prec '['
      {
          /* RANGE_LVALUE generation */

%line
          /* Change the expr4 into an lvalue
           */
          if ($1.lvalue.size <= 0)
          {
                yyerror("Need lvalue for range lvalue.");

                /* We can't continue without an lvalue on our hand. */
                free_fulltype($1.type);
                free_fulltype($2.type1);
                free_fulltype($2.type2);
                YYACCEPT;
          }
          else
          {
              /* Make <expr4> a protected lvalue, because we have to
               * evaluate the index expression first, before using it.
               */
              $$.lvalue = compose_lvalue_block($1.lvalue, F_MAKE_PROTECTED, $2.start, $2.lvalue_inst);
          }

          $$.name = NULL;
          $$.vlvalue_inst = 0;
          $$.num_arg = 0;

          /* Remove the code from the program block. */
          CURRENT_PROGRAM_SIZE = $1.start;
          last_expression = -1;

          /* Compute and check the types */
          $$.type = check_unary_op_type($1.type.t_type, "range index", types_range_index, lpctype_mixed);

          if (exact_types && !lpctype_contains(lpctype_int, $2.type1.t_type))
              fulltype_error("Bad type of index", $2.type1);
          if (exact_types && !lpctype_contains(lpctype_int, $2.type2.t_type))
              fulltype_error("Bad type of index", $2.type2);

          use_variable($1.name, VAR_USAGE_READ);

          free_fulltype($1.type);
          free_fulltype($2.type1);
          free_fulltype($2.type2);
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr4 member_operator struct_member_name %prec L_ARROW
      {
          /* Create a struct member lvalue */
          short s_index = -1;
          int m_index = -1;

%line
          lpctype_t* result = get_struct_member_result_type($1.type.t_type, $3, $2.strict_member, &s_index, &m_index);
          if (!result)
              result = lpctype_mixed;

          /* We don't need the lvalue for <expr4>. */
          free_lvalue_block($1.lvalue);

          /* We have to generate some code, so if the struct lookup is
           * invalid, we just play along and generate code to look up
           * member #-1 in whatever we got.
           */

          {
              if ($3 != NULL) /* Compile time lookup. */
              {
                  if (s_index == FSM_AMBIGUOUS) /* Not anymore. */
                      ins_prog_string($3);
                  else
                  {
                      ins_number(m_index);
                      free_mstring($3);
                  }
              }

              /* Insert the struct type index and the index opcode */
              ins_number(s_index);
              ins_f_code($2.strict_member ? F_S_INDEX_LVALUE : F_SX_INDEX_LVALUE);

              /* Now move that into an lvalue buffer. */
              $$.lvalue = compose_lvalue_block((lvalue_block_t) {0, 0}, 0, $1.start, 0);
              $$.name = NULL;

              $$.vlvalue_inst = $2.strict_member ? F_S_INDEX_VLVALUE : F_SX_INDEX_VLVALUE;
              $$.num_arg = 0;

              /* And remove it from the program block. */
              CURRENT_PROGRAM_SIZE = $1.start;
              last_expression = -1;

              $$.type = result;
          }

          use_variable($1.name, VAR_USAGE_READ);

          free_fulltype($1.type);
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | L_DUMMY
      {
          /* This rule is there to distinguish the 'lvalue' rules
           * from the expr4 rules and thus to prevent conflicts
           * because of the LALR simplication process.
           * (Alternatively the canonical-lr parser could be used.)
           */
           fatal("There should be no reduction with this rule.");

           $$.type = lpctype_mixed;
           $$.lvalue = (lvalue_block_t) {0, 0};
           $$.name = NULL;
           $$.vlvalue_inst = 0;
           $$.num_arg = 0;
      }

; /* lvalue */


name_var_lvalue:
    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    name_lvalue
      {
          bytecode_p q = LVALUE_BLOCK + $1.lvalue.start;

          $$ = $1;

          q[$1.lvalue.size - $1.num_arg - 1] = $1.vlvalue_inst;
      }
; /* name_var_lvalue */


local_name_lvalue:
      basic_type L_IDENTIFIER
      {
          $2 = define_local_variable($2, $1, &$$, $2->type == I_TYPE_LOCAL, MY_TRUE);

          ref_lpctype($$.type);
          free_lpctype($1);
      }
; /* local_name_lvalue */


/* The following rules are used to parse and compile the various
 * forms of array indexing/ranging operations.
 * They used at various places in the rules of expr0, expr4 and lvalue.
 */

index_expr :
      '[' expr0 ']'
        {
            $$.rvalue_inst  = F_INDEX;
            $$.lvalue_inst  = F_INDEX_LVALUE;
            $$.vlvalue_inst = F_INDEX_VLVALUE;
            $$.start        = $2.start;
            $$.end          = CURRENT_PROGRAM_SIZE;
            $$.type1        = $2.type;
            $$.type2.t_type = NULL;
            if (!pragma_warn_deprecated)
            {
                ins_byte(F_NO_WARN_DEPRECATED);
                $$.end++;
            }

            use_variable($2.name, VAR_USAGE_READ);

            free_lvalue_block($2.lvalue);
        }

    | '[' '<' expr0 ']'
        {
            $$.rvalue_inst  = F_RINDEX;
            $$.lvalue_inst  = F_RINDEX_LVALUE;
            $$.vlvalue_inst = F_RINDEX_VLVALUE;
            $$.start        = $3.start;
            $$.end          = CURRENT_PROGRAM_SIZE;
            $$.type1        = $3.type;
            $$.type2.t_type = NULL;
            if (!pragma_warn_deprecated)
            {
                ins_byte(F_NO_WARN_DEPRECATED);
                $$.end++;
            }

            use_variable($3.name, VAR_USAGE_READ);

            free_lvalue_block($3.lvalue);
        }

    | '[' '>' expr0 ']'
        {
            $$.rvalue_inst  = F_AINDEX;
            $$.lvalue_inst  = F_AINDEX_LVALUE;
            $$.vlvalue_inst = F_AINDEX_VLVALUE;
            $$.start        = $3.start;
            $$.end          = CURRENT_PROGRAM_SIZE;
            $$.type1        = $3.type;
            $$.type2.t_type = NULL;
            if (!pragma_warn_deprecated)
            {
                ins_byte(F_NO_WARN_DEPRECATED);
                $$.end++;
            }

            use_variable($3.name, VAR_USAGE_READ);

            free_lvalue_block($3.lvalue);
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
              free_fulltype($3.type);
              YYACCEPT;
          }

          mark = PROGRAM_BLOCK + $3.start;
          p = PROGRAM_BLOCK + current;
          length = current - $3.start;
          for( ; --length >= 0; p--) PUT_CODE(p, GET_CODE(p-1));
          STORE_CODE(mark, F_CONST0);
          CURRENT_PROGRAM_SIZE++;

          if (pragma_range_check)
              ins_byte(F_ARRAY_RANGE_CHECK);

          /* Return the data */

          $$.rvalue_inst  = F_RANGE;
          $$.lvalue_inst  = F_RANGE_LVALUE;
          $$.start        = $3.start;
          $$.end          = CURRENT_PROGRAM_SIZE;
          $$.type1        = get_fulltype(lpctype_int);
          $$.type2        = $3.type;

          use_variable($3.name, VAR_USAGE_READ);

          free_lvalue_block($3.lvalue);
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
              free_fulltype($4.type);
              YYACCEPT;
          }

          mark = PROGRAM_BLOCK + $4.start;
          p = PROGRAM_BLOCK + current;
          length = current - $4.start;
          for( ; --length >= 0; p--) PUT_CODE(p, GET_CODE(p-1));
          STORE_CODE(mark, F_CONST0);
          CURRENT_PROGRAM_SIZE++;

          if (pragma_range_check)
              ins_byte(F_ARRAY_RANGE_CHECK);

          /* Return the data */

          $$.rvalue_inst = F_NR_RANGE;
          $$.lvalue_inst = F_NR_RANGE_LVALUE;
          $$.start       = $4.start;
          $$.end         = CURRENT_PROGRAM_SIZE;
          $$.type1       = get_fulltype(lpctype_int);
          $$.type2       = $4.type;

          use_variable($4.name, VAR_USAGE_READ);

          free_lvalue_block($4.lvalue);
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
              free_fulltype($4.type);
              YYACCEPT;
          }

          mark = PROGRAM_BLOCK + $4.start;
          p = PROGRAM_BLOCK + current;
          length = current - $4.start;
          for( ; --length >= 0; p--) PUT_CODE(p, GET_CODE(p-1));
          STORE_CODE(mark, F_CONST0);
          CURRENT_PROGRAM_SIZE++;

          if (pragma_range_check)
              ins_byte(F_ARRAY_RANGE_CHECK);

          /* Return the data */

          $$.rvalue_inst = F_NA_RANGE;
          $$.lvalue_inst = F_NA_RANGE_LVALUE;
          $$.start       = $4.start;
          $$.end         = CURRENT_PROGRAM_SIZE;
          $$.type1       = get_fulltype(lpctype_int);
          $$.type2       = $4.type;

          use_variable($4.name, VAR_USAGE_READ);

          free_lvalue_block($4.lvalue);
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '['     expr0 L_RANGE     expr0 ']'
      {
          if (pragma_range_check)
              ins_byte(F_ARRAY_RANGE_CHECK);

          $$.rvalue_inst = F_RANGE;
          $$.lvalue_inst = F_RANGE_LVALUE;
          $$.start       = $2.start;
          $$.end         = CURRENT_PROGRAM_SIZE;
          $$.type1       = $2.type;
          $$.type2       = $4.type;

          use_variable($2.name, VAR_USAGE_READ);
          use_variable($4.name, VAR_USAGE_READ);

          free_lvalue_block($4.lvalue);
          free_lvalue_block($2.lvalue);
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '['     expr0 L_RANGE '<' expr0 ']'
      {
          if (pragma_range_check)
              ins_byte(F_ARRAY_RANGE_CHECK);

          $$.rvalue_inst = F_NR_RANGE;
          $$.lvalue_inst = F_NR_RANGE_LVALUE;
          $$.start       = $2.start;
          $$.end         = CURRENT_PROGRAM_SIZE;
          $$.type1       = $2.type;
          $$.type2       = $5.type;

          use_variable($2.name, VAR_USAGE_READ);
          use_variable($5.name, VAR_USAGE_READ);

          free_lvalue_block($5.lvalue);
          free_lvalue_block($2.lvalue);
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '[' '<' expr0 L_RANGE     expr0 ']'
      {
          if (pragma_range_check)
              ins_byte(F_ARRAY_RANGE_CHECK);

          $$.rvalue_inst = F_RN_RANGE;
          $$.lvalue_inst = F_RN_RANGE_LVALUE;
          $$.start       = $3.start;
          $$.end         = CURRENT_PROGRAM_SIZE;
          $$.type1       = $3.type;
          $$.type2       = $5.type;

          use_variable($3.name, VAR_USAGE_READ);
          use_variable($5.name, VAR_USAGE_READ);

          free_lvalue_block($5.lvalue);
          free_lvalue_block($3.lvalue);
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '[' '<' expr0 L_RANGE '<' expr0 ']'
      {
          if (pragma_range_check)
              ins_byte(F_ARRAY_RANGE_CHECK);

          $$.rvalue_inst = F_RR_RANGE;
          $$.lvalue_inst = F_RR_RANGE_LVALUE;
          $$.start       = $3.start;
          $$.end         = CURRENT_PROGRAM_SIZE;
          $$.type1       = $3.type;
          $$.type2       = $6.type;

          use_variable($3.name, VAR_USAGE_READ);
          use_variable($6.name, VAR_USAGE_READ);

          free_lvalue_block($6.lvalue);
          free_lvalue_block($3.lvalue);
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '['     expr0 L_RANGE '>' expr0 ']'
      {
          if (pragma_range_check)
              ins_byte(F_ARRAY_RANGE_CHECK);

          $$.rvalue_inst = F_NA_RANGE;
          $$.lvalue_inst = F_NA_RANGE_LVALUE;
          $$.start       = $2.start;
          $$.end         = CURRENT_PROGRAM_SIZE;
          $$.type1       = $2.type;
          $$.type2       = $5.type;

          use_variable($2.name, VAR_USAGE_READ);
          use_variable($5.name, VAR_USAGE_READ);

          free_lvalue_block($5.lvalue);
          free_lvalue_block($2.lvalue);
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '[' '>' expr0 L_RANGE     expr0 ']'
      {
          if (pragma_range_check)
              ins_byte(F_ARRAY_RANGE_CHECK);

          $$.rvalue_inst = F_AN_RANGE;
          $$.lvalue_inst = F_AN_RANGE_LVALUE;
          $$.start       = $3.start;
          $$.end         = CURRENT_PROGRAM_SIZE;
          $$.type1       = $3.type;
          $$.type2       = $5.type;

          use_variable($3.name, VAR_USAGE_READ);
          use_variable($5.name, VAR_USAGE_READ);

          free_lvalue_block($5.lvalue);
          free_lvalue_block($3.lvalue);
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '[' '<' expr0 L_RANGE '>' expr0 ']'
      {
          if (pragma_range_check)
              ins_byte(F_ARRAY_RANGE_CHECK);

          $$.rvalue_inst = F_RA_RANGE;
          $$.lvalue_inst = F_RA_RANGE_LVALUE;
          $$.start       = $3.start;
          $$.end         = CURRENT_PROGRAM_SIZE;
          $$.type1       = $3.type;
          $$.type2       = $6.type;

          use_variable($3.name, VAR_USAGE_READ);
          use_variable($6.name, VAR_USAGE_READ);

          free_lvalue_block($6.lvalue);
          free_lvalue_block($3.lvalue);
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '[' '>' expr0 L_RANGE '<' expr0 ']'
      {
          if (pragma_range_check)
              ins_byte(F_ARRAY_RANGE_CHECK);

          $$.rvalue_inst = F_AR_RANGE;
          $$.lvalue_inst = F_AR_RANGE_LVALUE;
          $$.start       = $3.start;
          $$.end         = CURRENT_PROGRAM_SIZE;
          $$.type1       = $3.type;
          $$.type2       = $6.type;

          use_variable($3.name, VAR_USAGE_READ);
          use_variable($6.name, VAR_USAGE_READ);

          free_lvalue_block($6.lvalue);
          free_lvalue_block($3.lvalue);
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '[' '>' expr0 L_RANGE '>' expr0 ']'
      {
          if (pragma_range_check)
              ins_byte(F_ARRAY_RANGE_CHECK);

          $$.rvalue_inst = F_AA_RANGE;
          $$.lvalue_inst = F_AA_RANGE_LVALUE;
          $$.start       = $3.start;
          $$.end         = CURRENT_PROGRAM_SIZE;
          $$.type1       = $3.type;
          $$.type2       = $6.type;

          use_variable($3.name, VAR_USAGE_READ);
          use_variable($6.name, VAR_USAGE_READ);

          free_lvalue_block($6.lvalue);
          free_lvalue_block($3.lvalue);
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '['     expr0 L_RANGE           ']'
      {
          if (pragma_range_check)
              ins_byte(F_ARRAY_RANGE_CHECK);

          $$.rvalue_inst = F_NX_RANGE;
          $$.lvalue_inst = F_NX_RANGE_LVALUE;
          $$.start       = $2.start;
          $$.end         = CURRENT_PROGRAM_SIZE;
          $$.type1       = $2.type;
          $$.type2       = get_fulltype(lpctype_int);

          use_variable($2.name, VAR_USAGE_READ);

          free_lvalue_block($2.lvalue);
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '[' '<' expr0 L_RANGE           ']'
      {
          /* Simulate an expression yielding <1 for the upper bound.
           * We pretend that it's part of the lower bound expr.
           */

          if (pragma_range_check)
              ins_byte(F_ARRAY_RANGE_CHECK);

          $$.rvalue_inst = F_RX_RANGE;
          $$.lvalue_inst = F_RX_RANGE_LVALUE;
          $$.start       = $3.start;
          $$.end         = CURRENT_PROGRAM_SIZE;
          $$.type1       = $3.type;
          $$.type2       = get_fulltype(lpctype_int);

          use_variable($3.name, VAR_USAGE_READ);

          free_lvalue_block($3.lvalue);
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '[' '>' expr0 L_RANGE           ']'
      {
          /* Simulate an expression yielding <1 for the upper bound.
           * We pretend that it's part of the lower bound expr.
           */

          if (pragma_range_check)
              ins_byte(F_ARRAY_RANGE_CHECK);

          $$.rvalue_inst = F_AX_RANGE;
          $$.lvalue_inst = F_AX_RANGE_LVALUE;
          $$.start       = $3.start;
          $$.end         = CURRENT_PROGRAM_SIZE;
          $$.type1       = $3.type;
          $$.type2       = get_fulltype(lpctype_int);

          use_variable($3.name, VAR_USAGE_READ);

          free_lvalue_block($3.lvalue);
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
      expr0                 { $$ = 1;      add_arg_type($1.type); check_unknown_type($1.type.t_type); use_variable($1.name, VAR_USAGE_READ); free_lvalue_block($1.lvalue); }
    | expr_list2 ',' expr0  { $$ = $1 + 1; add_arg_type($3.type); check_unknown_type($3.type.t_type); use_variable($3.name, VAR_USAGE_READ); free_lvalue_block($3.lvalue); }
; /* expr_list2 */


/* Expression lists for function call arguments.
 * These can contain ellipsis ('arr..').
 */

arg_expr_list:
      /* empty */    { $$ = 0; }
    | arg_expr_list2 { $$ = $1; }
; /* arg_expr_list */

arg_expr_list2:
      arg_expr
    | arg_expr_list2 ',' arg_expr  { $$ = $1 + $3; }
; /* arg_expr_list2 */

arg_expr:
      expr0
      {
          typeflags_t flags = read_current_function_arg_flags();

          $$ = 1;
          function_call_info[argument_level].arg_position++;

          if (!function_call_info[argument_level].got_ellipsis)
              add_arg_type($1.type);
          else
          {
              /* Can't determine the exact type of argument <X>,
               * because we don't know the number of previous
               * arguments due to the ellipsis.
               */
              add_arg_type(get_fulltype(lpctype_mixed));
              free_fulltype($1.type);
          }

          if (flags & TYPE_MOD_LVALUE)
          {
              /* We need an lvalue. */

              if ($1.lvalue.size <= 0)
              {
                  yyerrorf("Bad argument %d to %s: not a reference",
                        function_call_info[argument_level].arg_position,
                        get_txt(function_call_info[argument_level].fun_name));
              }
              else
              {
                  CURRENT_PROGRAM_SIZE = $1.start; /* Remove the rvalue code. */
                  add_lvalue_code($1.lvalue, F_MAKE_PROTECTED);
              }
              use_variable($1.name, VAR_USAGE_WRITE);
          }
          else
          {
              free_lvalue_block($1.lvalue);
              use_variable($1.name, VAR_USAGE_READ);
          }
      }
    | expr0 L_ELLIPSIS
      {
          typeflags_t flags = read_current_function_arg_flags();

          $$ = 0;
          function_call_info[argument_level].arg_position++;
          function_call_info[argument_level].got_ellipsis = true;

          if (flags & TYPE_MOD_LVALUE)
          {
              /* We need an lvalue. */
              if ($1.lvalue.size <= 0)
              {
                  yyerrorf("Bad argument %d to %s: not a reference",
                        function_call_info[argument_level].arg_position,
                        get_txt(function_call_info[argument_level].fun_name));
              }
              else
              {
                  CURRENT_PROGRAM_SIZE = $1.start; /* Remove the rvalue code. */
                  add_lvalue_code($1.lvalue, 0);
                  /* The following F_FLATTEN_XARG will honor this. */
              }

              use_variable($1.name, VAR_USAGE_WRITE);
          }
          else
          {
              /* Check that no mandatory lvalue arguments come after that. */
              int remaining_arg_types = function_call_info[argument_level].remaining_arg_types;
              fulltype_t *args = function_call_info[argument_level].arg_types;

              while (remaining_arg_types != 0 && args->t_type != NULL)
              {
                  do
                  {
                      if (args->t_flags & TYPE_MOD_LVALUE)
                      {
                          yyerrorf("Illegal ellipsis in call to %s: mixing reference and normal arguments",
                                get_txt(function_call_info[argument_level].fun_name));
                          args = NULL;
                          break;
                      }

                      args++;
                  } while (args->t_type != NULL);

                  if (!args)
                      break;

                  args++;
                  if (remaining_arg_types > 0)
                      remaining_arg_types--;
              }

              free_lvalue_block($1.lvalue);

              use_variable($1.name, VAR_USAGE_READ);
          }

          ins_f_code(F_FLATTEN_XARG);
          free_fulltype($1.type);
      }
; /* arg_expr */

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
          check_unknown_type($1.type.t_type);
          use_variable($1.name, VAR_USAGE_READ);
          free_lvalue_block($1.lvalue);
      }

    | m_expr_list2 ',' expr0 m_expr_values
      {
          if ($1[1] != $4) {
              yyerror("Inconsistent number of values in mapping literal");
          }
          $$[0] = $1[0] + 1 + $4;
          $$[1] = $1[1];
          add_arg_type($3.type);
          check_unknown_type($3.type.t_type);
          use_variable($3.name, VAR_USAGE_READ);
          free_lvalue_block($3.lvalue);
      }
; /* m_expr_list2 */

m_expr_values:
      ':' expr0                { $$ = 1;      add_arg_type($2.type); check_unknown_type($2.type.t_type); use_variable($2.name, VAR_USAGE_READ); free_lvalue_block($2.lvalue); }
    | m_expr_values ';' expr0  { $$ = $1 + 1; add_arg_type($3.type); check_unknown_type($3.type.t_type); use_variable($3.name, VAR_USAGE_READ); free_lvalue_block($3.lvalue); }
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
          if (!check_unknown_type($2.type.t_type)
           && !lpctype_contains(lpctype_string, $2.type.t_type)
           && !lpctype_contains(lpctype_int, $2.type.t_type))
              fulltype_error("Illegal type for struct member name", $2.type);

          use_variable($2.name, VAR_USAGE_READ);
          free_fulltype($2.type);
          free_lvalue_block($2.lvalue);
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
          $$.type = $3.type;

          use_variable($3.name, VAR_USAGE_READ);

          free_lvalue_block($3.lvalue);
      }
    | expr0
      {
          $$.name = NULL;
          $$.type = $1.type;

          use_variable($1.name, VAR_USAGE_READ);

          free_lvalue_block($1.lvalue);
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
          int efun_idx;

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

          if (argument_level+1 == sizeof(function_call_info)/sizeof(function_call_info[0]))
          {
              yyerror("Functions nested too deeply.");
              YYACCEPT;
          }

          real_name = $1.real;
            /* we rely on the fact that $1.real->type is either
             * I_TYPE_UNKNOWN or I_TYPE_GLOBAL here. All others are filtered
             * by the lexical analysis.
             */

          argument_level++;
          function_call_info[argument_level].fun_name = real_name->name;
          function_call_info[argument_level].arg_position = 0;
          function_call_info[argument_level].got_ellipsis = false;
          function_call_info[argument_level].unlimited_args = false;
          function_call_info[argument_level].remaining_arg_types = 0;

          if (real_name->type == I_TYPE_UNKNOWN)
          {
              /* prevent freeing by exotic name clashes */
              /* also makes life easier below */
              init_global_identifier(real_name, /* bVariable: */ MY_TRUE);
              real_name->next_all = all_globals;
              all_globals = real_name;
          }
          else if ( ($1.super ? ( $<function_call_head>$.efun_override == OVERRIDE_SEFUN )
                              : ( real_name->u.global.function == I_GLOBAL_FUNCTION_OTHER ))
                && real_name->u.global.sim_efun != I_GLOBAL_SEFUN_OTHER
                && !disable_sefuns)
          {
              /* It's a real simul-efun */

              $<function_call_head>$.simul_efun = real_name->u.global.sim_efun;
              /* real_name->u.global.sim_efun is also unsigned, so it can
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
          else if ( real_name->type == I_TYPE_GLOBAL
                && ($1.super ? ($<function_call_head>$.efun_override == OVERRIDE_EFUN)
                             : (defined_function(real_name)) < 0)
%ifdef USE_PYTHON
                && !is_python_efun(real_name)
%endif
                && (efun_idx = lookup_predef(real_name)) != -1)
          {
              /* This is a normal efun.
               * Let's fill function_call_info[].
               */
              if (instrs[efun_idx].arg_index >= 0)
              {
                  function_call_info[argument_level].arg_types = efun_arg_types + instrs[efun_idx].arg_index;
                  function_call_info[argument_level].unlimited_args = instrs[efun_idx].max_arg < 0;
                  function_call_info[argument_level].remaining_arg_types = instrs[efun_idx].max_arg;
              }
          }
      }

      '(' arg_expr_list ')'

      {
          /* We got the arguments. Now we have to generate the
           * proper instructions to call the function.
           */
%line
          int         f = 0;             /* Function index */
          int         simul_efun;
          lpctype_t **arg_types = NULL; /* Argtypes from the program */
          int         first_arg;         /* Startindex in arg_types[] */
          Bool        ap_needed;         /* TRUE if arg frame is needed */
          Bool        has_ellipsis;      /* TRUE if '...' was used */

          has_ellipsis = function_call_info[argument_level].got_ellipsis;
          ap_needed = MY_FALSE;

          $$.start = $<function_call_head>2.start;
          $$.might_lvalue = true;

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

                  if (!(funp->flags & TYPE_MOD_VARARGS))
                  {
                      if ($4 > funp->num_arg && !(funp->flags & TYPE_MOD_XVARARGS))
                          yyerrorf("Too many arguments to simul_efun %s"
                                  , get_txt(funp->name));

                      if ($4 < funp->num_arg - funp->num_opt_arg - ((funp->flags & TYPE_MOD_XVARARGS)?1:0) && !has_ellipsis)
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
                      
                  if ((funp->flags & (TYPE_MOD_VARARGS|TYPE_MOD_XVARARGS))
                   || funp->num_opt_arg
                   || has_ellipsis)
                      ap_needed = MY_TRUE;

                  if (funp->offset.argtypes != NULL)
                      check_function_call_types(get_argument_types_start($4), $4, funp, funp->offset.argtypes);

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
                  $$.type = get_fulltype(ref_lpctype(funp->type));
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

                          $$.type = get_fulltype(lpctype_mixed);
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
                      arg_types = GET_BLOCK(A_ARGUMENT_TYPES);
                      first_arg = ARGUMENT_INDEX(f);
                      CURRENT_PROGRAM_SIZE += 3;
                  }

                  $$.type = get_fulltype(ref_lpctype(funp->type)); /* Result type */

                  /* Verify that the function has been defined already.
                   * For inherited functions this is a no-brainer.
                   */
                  if (funp->flags & (NAME_UNDEFINED|NAME_HIDDEN))
                  {
                      if ( !(funp->flags & (NAME_PROTOTYPE|NAME_INHERITED)) )
                      {
                          if (exact_types)
                              yyerrorf("Function %.50s undefined", get_txt(funp->name));

                          /* Undefined functions will have lpctype_unknown,
                           * change that to mixed so this won't cause any further errors.
                           */
                          free_fulltype($$.type);
                          $$.type = get_fulltype(lpctype_mixed);
                      }
                      else if ((funp->flags
                                & (NAME_PROTOTYPE|NAME_HIDDEN))
                               == NAME_HIDDEN)
                      {
                          yyerrorf("Function %.50s is private", get_txt(funp->name));
                      }
                  }
                  // warn about obsoleted functions
                  if (funp->flags & TYPE_MOD_DEPRECATED)
                      yywarnf("Calling deprecated function \'%s\'",
                              get_txt(funp->name));

                  /* Check number of arguments.
                   */
                  if ((($4 > funp->num_arg && !(funp->flags & TYPE_MOD_XVARARGS))
                     || $4 < funp->num_arg - funp->num_opt_arg - ((funp->flags & TYPE_MOD_XVARARGS)?1:0))
                   && !(funp->flags & TYPE_MOD_VARARGS)
                   && (first_arg != INDEX_START_NONE)
                   && exact_types
                   && !has_ellipsis)
                  {
                      yyerrorf("Wrong number of arguments to %.60s: "
                               "expected %ld, got %ld"
                              , get_txt($1.real->name)
                              , (long)(funp->num_arg - funp->num_opt_arg - ((funp->flags & TYPE_MOD_XVARARGS)?1:0)), (long)$4);
                  }

                  /* Check the argument types.
                   */
                  if (exact_types && first_arg != INDEX_START_NONE)
                      check_function_call_types(get_argument_types_start($4), $4, funp, arg_types + first_arg);

              } /* if (inherited lfun) */

%ifdef USE_PYTHON
              else if ($1.real->type == I_TYPE_GLOBAL && is_python_efun($1.real))
              {
                  /* python-defined EFUN */

                  PREPARE_INSERT(4)

                  /* Check the arguments. */
                  if (exact_types && $4)
                  {
                      /* Check for unknown type. */
                      fulltype_t *argp = get_argument_types_start($4);

                      for (int argn = 0; argn < $4; argn++)
                         check_unknown_type(argp[argn].t_type);
                  }

                  lpctype_t *result = check_python_efun_args($1.real, $4, has_ellipsis, get_argument_types_start($4));

                  add_f_code(F_USE_ARG_FRAME);
                  add_f_code(F_PYTHON_EFUN);
                  add_short($1.real->u.global.python_efun);
                  CURRENT_PROGRAM_SIZE += 4;

                  $$.type = get_fulltype(ref_lpctype(result));

                  /* Always need an arg frame, as these
                   * calls are always handled as varargs.
                   */
                  ap_needed = MY_TRUE;
              }
%endif
              else if ( (f = lookup_predef($1.real)) != -1
                     && instrs[f].arg_index >= 0)
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
                  $$.type = get_fulltype(ref_lpctype(instrs[f].ret_type));
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
                  if (exact_types && num_arg)
                  {
                      int         argn;
                      fulltype_t *aargp;
                      fulltype_t *lastArgp;

                      lastArgp = aargp = get_argument_types_start(num_arg);

                      /* Loop over all arguments and compare each given
                       * type against all allowed types in efun_arg_types()
                       */
                      for (argn = 0; argn < num_arg; argn++)
                      {
                          fulltype_t *beginArgp = argp;

                          /* For varargs efuns we repeat the last entry.
                           */
                          if (max == -1 && argp->t_type == NULL)
                              beginArgp = argp = lastArgp;
                          else
                              lastArgp = argp;

                          if (!check_unknown_type(aargp->t_type))
                          {
                              for (;;argp++)
                              {
                                  if ( argp->t_type == NULL )
                                  {
                                      /* Possible types for this arg exhausted */
                                      efun_argument_error(argn+1, f, beginArgp
                                                         , *aargp);
                                      break;
                                  }

                                  /* Break if types are compatible.
                                   */
                                  if (!((aargp->t_flags ^ argp->t_flags) & TYPE_MOD_REFERENCE)
                                   && check_assignment_types(*aargp, argp->t_type))
                                      break;
                              } /* end for (efun_arg_types) */
                          }

                          /* Advance argp to point to the allowed argtypes
                           * of the next arg.
                           */
                          while((argp++)->t_type) NOOP;

                          /* The pointer on the argument type stack. */
                          aargp++;
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
                  if ( instrs[f].ret_type == lpctype_void )
                  {
                      last_expression = mem_block[A_PROGRAM].current_size;
                      add_f_code(F_CONST0);
                      CURRENT_PROGRAM_SIZE++;
                  }
                  $$.might_lvalue = instrs[f].might_return_lvalue;
              } /* efun */

              else if ($<function_call_head>2.efun_override)
              {
                  yyerrorf(($<function_call_head>2.efun_override == OVERRIDE_EFUN)
                      ? "Unknown efun: %s" : "Unknown simul-efun: %s", get_txt($1.real->name));
                  $$.type = get_fulltype(lpctype_mixed);
              }
              else
              {
                  /* There is no such function, but maybe it's defined later,
                   * maybe it's resolved through (cross-)inheritance.
                   * epilog() will take care of it.
                   */
                  PREPARE_INSERT(4)

                  f = define_new_function(MY_FALSE,
                      $1.real, 0, 0, 0, NAME_UNDEFINED, get_fulltype(lpctype_unknown)
                  );
                  ap_needed = MY_TRUE;
                  add_f_code(F_CALL_FUNCTION);
                  add_short(f);
                  CURRENT_PROGRAM_SIZE += 3;
                  if (exact_types)
                  {
                      yyerrorf("Undefined function '%.50s'", get_txt($1.real->name));
                  }
                  else if (pragma_pedantic)
                  {
                      yywarnf("Undefined function '%.50s'", get_txt($1.real->name));
                  }
                  $$.type = get_fulltype(lpctype_mixed);  /* Just a guess */
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
    | expr4 member_operator call_other_name %prec L_ARROW
      {
%line
          string_t *name;
          int sefun;

          /* Don't need <expr4> as an lvalue. */
          free_lvalue_block($1.lvalue);

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
                  free_fulltype($1.type);
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

          if (argument_level+1 == sizeof(function_call_info)/sizeof(function_call_info[0]))
          {
              yyerror("Functions nested too deeply.");
              free_fulltype($1.type);
              YYACCEPT;
          }
          argument_level++;
          function_call_info[argument_level].fun_name = $3;
          function_call_info[argument_level].arg_position = 0;
          function_call_info[argument_level].got_ellipsis = false;
          function_call_info[argument_level].unlimited_args = false;
          function_call_info[argument_level].remaining_arg_types = 0;

          /* If call_other() has been replaced by a sefun, and
           * if we need to use F_CALL_DIRECT to call it, we have
           * to insert additional code before the <expr4> already parsed.
           * Putting this code block before the <expr4> in the rule
           * however yields a faulty grammar.
           */

          sefun = $2.strict_member ? call_strict_sefun : call_other_sefun;
          if (!disable_sefuns
           && sefun >= 0
           && (unsigned long)sefun >= SEFUN_TABLE_SIZE)
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
                  free_fulltype($1.type);
                  YYACCEPT;
              }

              /* Move the generated code forward by 6 */
              p = mem_block[A_PROGRAM].block + CURRENT_PROGRAM_SIZE - 1;
              q = p + 6;
              for (left = CURRENT_PROGRAM_SIZE - $1.start - 1
                  ; left > 0
                  ; left--, p--, q--)
                  *q = *p;

              /* p now points to program[$1.start].
               * Store the first two call-other args there.
               */
              p[1] = F_STRING;
              upd_short($1.start+2, store_prog_string(
                        ref_mstring(query_simul_efun_file_name())));
              p[4] = F_STRING;
              upd_short($1.start+5, store_prog_string(ref_mstring(
                  $2.strict_member ? STR_CALL_STRICT : STR_CALL_OTHER)));

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
              ins_prog_string(name);
          } /* if (name) */
          /* otherwise the name was given by an expression for which
           * the code and value have been already generated.
           */

          if (!disable_sefuns && sefun >= 0)
          {
              /* Create argument type list for type checking. */
              add_arg_type(ref_fulltype($1.type));
              add_arg_type(get_fulltype(lpctype_string));
          }
      }

      '(' arg_expr_list ')'

      {
          /* Now generate the CALL_OTHER resp. the SIMUL_EFUN instruction. */

          PREPARE_INSERT(10)
          Bool has_ellipsis;
          Bool ap_needed;
          int sefun = $2.strict_member ? call_strict_sefun : call_other_sefun;

          has_ellipsis = function_call_info[argument_level].got_ellipsis;
          ap_needed = MY_TRUE;

          $$.might_lvalue = true;

          if (!disable_sefuns && sefun >= 0)
          {
              /* SIMUL EFUN */

              function_t *funp;
              int num_arg;

              num_arg = $6 + 2; /* Don't forget the obj and the fun! */

              funp = &simul_efunp[sefun];
              if (num_arg > funp->num_arg
               && !(funp->flags & (TYPE_MOD_VARARGS|TYPE_MOD_XVARARGS))
               && !has_ellipsis)
                  yyerrorf("Too many arguments to simul_efun %s"
                          , get_txt(funp->name));

              if (funp->offset.argtypes != NULL)
                  check_function_call_types(get_argument_types_start(num_arg), num_arg, funp, funp->offset.argtypes);

              /* sefun is >= 0 (see above) */
              if ((unsigned long)sefun >= SEFUN_TABLE_SIZE)
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
                  if (!(funp->flags & (TYPE_MOD_VARARGS|TYPE_MOD_XVARARGS))
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

                  if (!(funp->flags & (TYPE_MOD_VARARGS|TYPE_MOD_XVARARGS))
                   && !funp->num_opt_arg
                   && !has_ellipsis)
                      ap_needed = MY_FALSE;

                  if (ap_needed)
                  {
                      add_f_code(F_USE_ARG_FRAME);
                      CURRENT_PROGRAM_SIZE++;
                  }
                  add_f_code(F_SIMUL_EFUN);
                  add_short(sefun);
                  CURRENT_PROGRAM_SIZE += 3;
              }
              $$.type = get_fulltype(funp->type);

              pop_arg_stack(2); /* For sefun call_other there are two more arguments. */
          }
          else /* true call_other */
          {
              int call_instr = $2.strict_member ? F_CALL_STRICT : F_CALL_OTHER;
              add_f_code(call_instr);
              CURRENT_PROGRAM_SIZE++;
              $$.type = get_fulltype(instrs[call_instr].ret_type);

              if (!check_unknown_type($1.type.t_type)
               && !has_common_type(lpctype_string_object, $1.type.t_type)
               && !has_common_type(lpctype_string_object_array, $1.type.t_type))
              {
                  efun_argument_error(1, call_instr, efun_arg_types + instrs[call_instr].arg_index, $1.type);
              }
          }
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

              dest = PROGRAM_BLOCK + $1.start;
              src = dest+1;
              left = CURRENT_PROGRAM_SIZE - $1.start - 1;

              while (left-- > 0)
              {
                  *dest++ = *src++;
              }

              CURRENT_PROGRAM_SIZE--;

              /* If last_expression lies within the program area
               * that was moved one bytecode adjust it accordingly.
               */
              if(last_expression > $1.start)
                  last_expression--;
          }

          argument_level--;

          use_variable($1.name, VAR_USAGE_READ);

          free_fulltype($1.type);
      }

; /* function_call */


member_operator:
      L_ARROW { $$.strict_member = false; }
    | '.'     { $$.strict_member = true;  }
; /* member_operator */


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
          if (!check_unknown_type($2.type.t_type)
           && !lpctype_contains(lpctype_string, $2.type.t_type))
              fulltype_error("Illegal type for lfun name", $2.type);
          free_fulltype($2.type);
          free_lvalue_block($2.lvalue);

          use_variable($2.name, VAR_USAGE_READ);
      }

; /* call_other_name */


function_name:
      L_IDENTIFIER
      {
          ident_t *fun = $1;

          if (fun->type == I_TYPE_LOCAL)
          {
              fun = find_shared_identifier_mstr(fun->name, I_TYPE_UNKNOWN, 0);

              /* Search the inferior list for this identifier for a global
               * (function) definition.
               */

              while (fun && fun->type > I_TYPE_GLOBAL)
                  fun = fun->inferior;

              if (!fun || fun->type != I_TYPE_GLOBAL)
              {
                  yyerrorf("Undefined function '%.50s'\n", get_txt($1->name));
                  YYACCEPT;
              }
          }

          $$.super = NULL;
          $$.real  = fun;
      }

    | L_COLON_COLON L_IDENTIFIER
      {
          *($$.super = yalloc(1)) = '\0';
          $$.real  = $2;
      }

    | anchestor L_COLON_COLON L_IDENTIFIER
      {
%line
          ident_t *fun = $3;

          if (fun->type == I_TYPE_LOCAL)
          {
              fun = find_shared_identifier_mstr(fun->name, I_TYPE_UNKNOWN, 0);

              /* Search the inferior list for this identifier for a global
               * (function) definition.
               */

              while (fun && fun->type > I_TYPE_GLOBAL)
                  fun = fun->inferior;

              if (!fun || fun->type != I_TYPE_GLOBAL)
              {
                  yyerrorf("Undefined function '%.50s'\n", get_txt($3->name));
                  YYACCEPT;
              }
          }

          /* Attempt to call an efun directly even though there
           * is a nomask simul-efun for it?
           */
          if ( !strcmp($1, "efun")
           && fun->type == I_TYPE_GLOBAL
           && fun->u.global.sim_efun != I_GLOBAL_SEFUN_OTHER
           && simul_efunp[fun->u.global.sim_efun].flags & TYPE_MOD_NO_MASK
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
              push_ref_string(inter_sp, fun->name);
              res = apply_master(STR_PRIVILEGE, 3);
              if (!res || res->type != T_NUMBER || res->u.number < 0)
              {
                  yyerrorf("Privilege violation: nomask simul_efun %s"
                          , get_txt(fun->name));
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
                      , get_txt(STR_PRIVILEGE), get_txt(fun->name));
              yfree($1);
              $$.super = NULL;
          }
          else /* the qualifier is ok */
              $$.super = $1;

          $$.real = fun; /* and don't forget the function ident */
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
          modstart = $5;

          free_fulltype($4.type);

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
          $$.type  = get_fulltype(lpctype_string);
          $$.name  = NULL;
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
              if (!check_unknown_type($2.type.t_type)
               && !lpctype_contains(lpctype_int, $2.type.t_type))
                  yyerrorf("Bad 'reserve' expression type to catch(): %s, "
                           "expected int"
                          , get_fulltype_name($2.type)
                          );
              $$ = CATCH_FLAG_RESERVE;
          }
          else
              yyerrorf("Illegal modifier '%s' in catch() - "
                      "expected 'nolog', 'publish' or 'reserve <expr>'"
                      , get_txt($1)
                      );

          use_variable($2.name, VAR_USAGE_READ);

          free_mstring($1);
          free_fulltype($2.type);
          free_lvalue_block($2.lvalue);
      }
; /* opt_catch_modifier */

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

%%
%line

#ifdef __MWERKS__
#    pragma warn_possunwant reset
#    pragma warn_implicitconv reset
#endif

/*=========================================================================*/

/*-------------------------------------------------------------------------*/
static ident_t*
define_local_variable (ident_t* name, lpctype_t* actual_type, struct lvalue_s *lv, Bool redeclare, Bool with_init)

/* This is called directly from a parser rule: <type> <name>
 * if with_init is true, then an initialization of this variable will follow.
 * if redeclare is true, then a local name is redeclared.
 * It creates the local variable and returns the corresponding lvalue
 * in lv.
 * The references of <actual_type> are NOT adopted. Although this function
 * puts a type in <lv.type>, it doesn't increment the reference count of it.
 * The calling function must add the reference, when using <lv> further.
 *
 * If <lv> is not NULL, then code for the variable as an lvalue will be
 * stored in <lv.lvalue>. If it won't be used, it should be freed using
 * free_lvalue_block().
 */

{
    /* redeclare:
     *    MY_FALSE: A new local variable
     *    MY_TRUE:  A local name is redeclared. If this happens
     *              on a deeper level, it is even legal.
     */

    block_scope_t *scope = block_scope + block_depth - 1;
    ident_t *q;
    bytecode_p lvalue_code;

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
            q = redeclare_local(name, get_fulltype(ref_lpctype(actual_type)), block_depth);
        else
            q = add_local_name(name, get_fulltype(ref_lpctype(actual_type)), block_depth);
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

    if (lv)
    {
        lv->lvalue = alloc_lvalue_block(2);
        lvalue_code = LVALUE_BLOCK + lv->lvalue.start;

        lvalue_code[0] = F_PUSH_LOCAL_VARIABLE_LVALUE;
        lvalue_code[1] = q->u.local.num;

        lv->vlvalue_inst = F_PUSH_LOCAL_VARIABLE_VLVALUE;
        lv->num_arg = 1;

        lv->type = actual_type;
        lv->name = q;
    }

    if (!with_init)
    {
        /* If this is a float variable, we need to insert an appropriate
         * initializer, as the default svalue-0 is not a valid float value.
         */

%line
        if (actual_type == lpctype_float)
        {
            ins_f_code(F_FCONST0);
            ins_f_code(F_PUSH_LOCAL_VARIABLE_LVALUE);
            ins_byte(q->u.local.num);
            ins_f_code(F_VOID_ASSIGN);
        } /* if (float variable) */
    }
    else
        q->u.local.initializing = true;

    return q;
} /* define_local_variable() */

/*-------------------------------------------------------------------------*/
static void
init_local_variable ( ident_t* name, struct lvalue_s *lv, int assign_op
                    , fulltype_t exprtype)

/* This is called directly from a parser rule: <type> <name> = <expr>
 * It will be called after the call to define_local_variable().
 * It assigns the result of <expr> to the variable.
 *
 * The lvalue code block in <lv> will be freed.
 */

{
    /* We got a "<name> = <expr>" type declaration. */

%line

#ifdef DEBUG_INLINES
if (current_inline && current_inline->parse_context) 
  printf("DEBUG: inline context decl: name = expr, program_size %"PRIuMPINT"\n", 
         CURRENT_PROGRAM_SIZE);
#endif /* DEBUG_INLINES */

    name->u.local.initializing = false;

    /* Check the assignment for validity */
    if (exact_types && !check_assignment_types(exprtype, lv->type))
    {
        yyerrorf("Bad assignment %s", get_two_lpctypes(lv->type, exprtype.t_type));
    }

    if (assign_op != F_ASSIGN)
    {
        yyerror("Only plain assignments allowed here");
    }

    add_type_check(lv->type, TYPECHECK_VAR_INIT);
    use_variable(name, (exprtype.t_flags & TYPE_MOD_REFERENCE) ? VAR_USAGE_READWRITE : VAR_USAGE_WRITE);

    if (!add_lvalue_code(lv->lvalue, F_VOID_ASSIGN))
        return;
} /* init_local_variable() */

/*-------------------------------------------------------------------------*/
static void
use_variable (ident_t* name, enum variable_usage usage)

/* Registers the usage for the local variable.
 * If <name> is NULL, this function does nothign.
 */

{
    if (!name)
        return;

    if (name->type == I_TYPE_GLOBAL)
    {
        int idx = name->u.global.variable;
        if (!(idx & VIRTUAL_VAR_TAG))
            GLOBAL_VARIABLE(idx).usage |= usage;
    }
    else if (name->u.local.context >= 0)
    {
        // Do we have to look at the outer closure?
        if (name->u.local.depth <= current_inline->block_depth)
            // We must only look at the next one, because check_for_context_local()
            // will have created any intermediate context variables.
            LOCAL_VARIABLE(current_inline->full_context_var_start + name->u.local.context).usage |= usage;
        else
            context_variables[name->u.local.context].usage |= usage;
    }
    else
    {
        local_variables[name->u.local.num].usage |= usage;
    }

} /* use_variable() */

/*-------------------------------------------------------------------------*/
static void
warn_variable_usage (string_t* name, enum variable_usage usage, const char* prefix)

/* Check whether the usage warrants a warning and if so issue one.
 * The pragma warn_unused_variable is not checked here.
 */

{
    switch (usage)
    {
        case VAR_USAGE_NONE:
            yywarnf("%s variable %s was never used", prefix, get_txt(name));
            break;

        case VAR_USAGE_WRITE:
            yywarnf("%s variable %s was assigned a value, but never used", prefix, get_txt(name));
            break;

        case VAR_USAGE_READ:
            yywarnf("%s variable %s was never assigned a value, but was used", prefix, get_txt(name));
            break;

        case VAR_USAGE_READWRITE:
            /* Everything is fine. */
            break;
    }

} /* warn_variable_usage() */

/*-------------------------------------------------------------------------*/
static Bool
add_lvalue_code (lvalue_block_t lv, int instruction)

/* Add the lvalue code held in <lv> to the end of the program.
 * If <instruction> is not zero, it is the code for an instruction
 * to be added after the lvalue code. The lvalue code block in <lv>
 * will be freed.
 * Return TRUE on success, and FALSE on failure.
 */

{
    /* Create the code to push the lvalue */
    if (!add_to_mem_block(A_PROGRAM, LVALUE_BLOCK + lv.start, lv.size))
        return MY_FALSE;

    free_lvalue_block(lv);
    last_expression = CURRENT_PROGRAM_SIZE;

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
    if (last_expression > CURRENT_PROGRAM_SIZE)
    {
        /* No last expression but a value to pop, interesting... */
        ins_f_code(F_POP_VALUE);
    }
    else if (last_expression == CURRENT_PROGRAM_SIZE - sizeof(bytecode_t))
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
add_type_check (lpctype_t *expected, enum type_check_operation op)
/* Adds an instruction for type checking the topmost value
 * on the stack against <expected>.
 */

{
    int idx;

    /* If the required pragmas are not on, skip it. */
    if (!pragma_rtt_checks || !pragma_save_types)
        return;

    /* Also we don't check for mixed... */
    if (expected == lpctype_mixed || expected == lpctype_unknown)
        return;

    /* Now get an index for the type in our type list. */
    for (idx = 0; idx < ARGTYPE_COUNT; idx++)
        if (ARGUMENT_TYPE(idx) == expected)
            break;

    if (idx == ARGTYPE_COUNT)
    {
        /* Check that there is space in the argument type list. */
        if (arg_types_exhausted)
            return;

        if (idx > (long)USHRT_MAX)
        {
            arg_types_exhausted = true;
            yywarnf("Type buffer exhausted, cannot store and verify argument types.");
            return;
        }

        ADD_ARGUMENT_TYPE(ref_lpctype(expected));
    }

    ins_f_code(F_TYPE_CHECK);
    ins_byte(op);
    ins_short(idx);
} /* add_type_check() */

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
          /* Add space for the function header index. */
        realloc_a_program(FUNCTION_HDR_SIZE);

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
    block_depth = 1;
    init_scope(block_depth);
    max_number_of_locals = max_number_of_init_locals;
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

    max_number_of_init_locals = max_number_of_locals;
    block_depth = 0;
    
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
static bool
is_function_defined (program_t *progp, int fx)

/* Checks whether there is code for that function.
 */
{
    const program_t *inhprogp;
    int inh_fx;

    get_function_header_extended(progp, fx, &inhprogp, &inh_fx);
    return !is_undef_function(inhprogp->program + (inhprogp->functions[inh_fx] & FUNSTART_MASK));
} /* is_function_defined */

/*-------------------------------------------------------------------------*/
static bool
adjust_virtually_inherited ( unsigned short *pFX, inherit_t **pIP)

/* The caller is about to create a call to the inherited function <pFX>
 * in <pIP>s program. We check whether this function is a virtually
 * inherited function (<pIP> inherited that function from another
 * program), then we replace <pIP> with the inherit pointing to that
 * virtually inherited program and adjust <pFX> accordingly.
 *
 * Returns false to indicate that this function does no longer exist
 * (when an update to the program doesn't contain it anymore). <pIP>
 * is set to NULL in that case, too. On success the function returns
 * true.
 */
{
    unsigned short fx = *pFX;
    inherit_t *ip = *pIP;
    funflag_t flags = ip->prog->functions[fx];

    if (flags & NAME_INHERITED)
    {
        /* The parent inherits the function itself: we have to
         * check if it's a virtual inheritance.
         */

        inherit_t *ip2;
        program_t *prog1, *prog2;

        prog1 = ip->prog;
        ip2 = &prog1->inherit[flags & INHERIT_MASK];
        prog2 = ip2->prog;

        if (ip2->inherit_type != INHERIT_TYPE_NORMAL)
        {
            /* The source was virtually inherited - we have to find
             * the first inheritance of the program.
             * And adjust the function index, of course.
             */
            do --ip; while (ip->prog != prog2);
            fx -= ip2->function_index_offset;

            /* Is there an update for this program? */
            while (ip->inherit_type & INHERIT_TYPE_MAPPED)
            {
                fx = GET_BLOCK(A_UPDATE_INDEX_MAP)[fx + ip->function_map_offset];
                if (fx == USHRT_MAX)
                {
                    /* Not defined anymore. */
                    *pIP = NULL;
                    return false;
                }

                ip = GET_BLOCK(A_INHERITS) + ip->updated_inherit;
            }

            *pFX = fx;
            *pIP = ip;
        }
    }

    return true;
}

/*-------------------------------------------------------------------------*/

static unsigned short
lookup_inherited (const char *super_name, string_t *real_name
                 , inherit_t **pIP, funflag_t *pFlags)

/* Lookup an inherited function <super_name>::<real_name> and return
 * it's function index, setting *pIP to the inherit_t pointer and
 * *pFlags to the function flags.
 * Return USHRT_MAX if not found, *pIP set to NULL, and *pFlags set to 0.
 *
 * <super_name> can be an empty string or the (partial) name of one
 * of the inherits. <real_name> must be shared string.
 */
{
    inherit_t *ip;
    int num_inherits, super_length;

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
    for (ip = GET_BLOCK(A_INHERITS); num_inherits > 0 ; ip++, num_inherits--)
    {
        inherit_t *foundp;
        unsigned short found_ix;
        short i;

        if (ip->inherit_type & INHERIT_TYPE_MAPPED)
            /* this is an old inherit */
            continue;

        if (ip->inherit_depth > 1)
            /* Only consider direct inherits, otherwise we would call
             * hidden functions (i.e. function made private through the
             * inherit hierarchy).
             */
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

        if (!is_function_defined(ip->prog, i))
            continue;

        /* Found one */
        foundp = ip;
        found_ix = i;
        if (adjust_virtually_inherited(&found_ix, &foundp))
        {
            *pIP = foundp;
            *pFlags = foundp->prog->functions[found_ix];
            return found_ix;
        }
        /* else search for another one. */
    } /* for (all inherits) */

    /* Not found. */
    return USHRT_MAX;
} /* lookup_inherited() */

/*-------------------------------------------------------------------------*/
unsigned short
find_inherited_function ( const char * super_name
                        , const char * real_name
                        , unsigned short * pInherit
                        , funflag_t * flags
                        )

/* Lookup an inherited function <super_name>::<real_name> and return
 * it's function index as result, and the inheritance index in *<pInherit>.
 * Return USHRT_MAX if not found.
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
    unsigned short ix;

    rname = find_tabled_str(real_name, STRING_UTF8);

    ix =  rname ? lookup_inherited(super_name, rname, &ip, flags) : USHRT_MAX;
    if (ix != USHRT_MAX) /* Also return the inherit index. */
        *pInherit = ip - GET_BLOCK(A_INHERITS);
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
    unsigned short found_ix;

    found_ix = lookup_inherited(super_name, real_name, &ip, &flags);

    if (ip != NULL)
    {
        /* Found it! */
        bytecode_p __PREPARE_INSERT__p = __prepare_insert__p;

        /* Generate the function call */
        add_f_code(F_CALL_INHERITED);
        add_short(ip - GET_BLOCK(A_INHERITS));
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
        unsigned short i;

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
        ip0 = GET_BLOCK(A_INHERITS);
        first_index = num_inherits > 0 ? INHERITED_WILDCARDED_NOT_FOUND
                                       : INHERITED_NOT_FOUND;
        for (; num_inherits > 0; ip0++, num_inherits--)
        {
            PREPARE_INSERT(13)

            /* ip->prog->name includes .c */
            int l = mstrsize(ip0->prog->name) - 2;
            int fx;

            ip = ip0; /* ip will be changed in the body */

            if (ip->inherit_depth > 1)
                /* Only consider direct inherits, otherwise we would even
                 * call functions in sub-inherits which have been redefined.
                 */
                continue;

            if ( !match_string(super_name, get_txt(ip->prog->name), l) )
                continue;

            if (ip->inherit_type & INHERIT_TYPE_DUPLICATE)
            {
                /* This is a duplicate inherit, let's search for the original. */
                for (ip = &(INHERIT(0)); ip < ip0; ip++)
                    if (ip->prog == ip0->prog)
                        break;

                assert(ip != ip0); /* There must be an original. */
            }

            if ( (fx = find_function(real_name, ip->prog)) < 0)
                continue;

            if (!is_function_defined(ip->prog, fx))
                continue;

            i = (unsigned short)fx;
            if (!adjust_virtually_inherited(&i, &ip))
                continue;

            /* Found a match */
            flags = ip->prog->functions[i];
            ip_index = ip - GET_BLOCK(A_INHERITS);

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
static void
update_function_identifier (function_t *from, short fromidx, function_t *to, short toidx)

/* The function <to> (function index <toidx> was cross-defined to the real
 * function <from> (with function index <fromidx>).
 * If there is any identifier with their name update it accordingly.
 */

{
    ident_t *ident = find_shared_identifier_mstr(from->name, I_TYPE_GLOBAL, 0);
    if (ident && ident->u.global.function == toidx)
    {
        /* We first have to check, whether <from> is also a cross-defintion. */
        while (from->flags & NAME_CROSS_DEFINED)
        {
            int32 offset = GET_CROSSDEF_OFFSET(from->offset.func);
            from += offset;
            fromidx += offset;
        }

        ident->u.global.function = fromidx;
    }
} /* update_function_identifier() */

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

    assert(fx < progp->num_functions);

    flags = progp->functions[fx];

    /* Handle a cross-define */
    if (flags & NAME_CROSS_DEFINED)
    {
        fx += CROSSDEF_NAME_OFFSET(flags);
        flags = progp->functions[fx];
    }

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

        /* Combine the visibility flags. */
        f = flags | pdef->flags;
        if (pdef->flags & TYPE_MOD_PUBLIC)
            f &= ~(TYPE_MOD_PRIVATE | TYPE_MOD_PROTECTED);
        else if (pdef->flags & TYPE_MOD_PRIVATE)
            f |= NAME_HIDDEN;

        if (f & (TYPE_MOD_PRIVATE | NAME_HIDDEN))
            f &= ~(TYPE_MOD_PROTECTED | TYPE_MOD_PUBLIC);
        else if (f & TYPE_MOD_PROTECTED)
            f &= ~(TYPE_MOD_PUBLIC);

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
        current_struct = define_new_struct( MY_FALSE, p, get_txt(struct_t_pname(pdef->type)), f);
        free_struct_type(STRUCT_DEF(current_struct).type);
        STRUCT_DEF(current_struct).type = ref_struct_type(pdef->type);
        STRUCT_DEF(current_struct).inh = INHERIT_COUNT;
        update_struct_type(STRUCT_DEF(current_struct).type->name->lpctype, pdef->type);
    }
} /* copy_structs() */

/*-------------------------------------------------------------------------*/
static bool
inherit_functions (program_t *from, uint32 inheritidx)

/* Copies the function table from <from> into our program.
 *
 * Put's NAME_INHERITED entries into our program for each
 * function entry in <from>'s program. The inherit index
 * is set to <inheritidx> and the visibility is set to
 * NAME_HIDDEN.
 *
 * Returns true on success, false otherwise (out of memory).
 */

{
    function_t *fun_p;

    /* Make space for the inherited function structures */
    if(!RESERVE_FUNCTIONS(from->num_functions))
        return false;

    /* The new functions will be stored from here */
    fun_p = FUNCTION(FUNCTION_COUNT);

    /* Copy the function definitions one by one and adjust the flags. */
    for (int i = 0; i < from->num_functions; i++, fun_p++)
    {
        funflag_t  flags;
        int i2; /* The index of the real function */

        flags = from->functions[i];
        fun_p->offset.inherit = inheritidx;
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


        /* Copy information about the types of the arguments, if it is
         * available.
         */
        A_ARGUMENT_INDEX_t argindex = INDEX_START_NONE; /* Presume not available. */
        if (from->type_start != 0)
        {
            if (from->type_start[i] != INDEX_START_NONE)
            {
                /* They are available for function number 'i'. Copy types of
                 * all arguments, and remember where they started.
                 */
                argindex = ARGTYPE_COUNT;
                if (fun_p->num_arg)
                {
                    int ix;

                    ix = ARGTYPE_COUNT;

                    add_to_mem_block(
                      A_ARGUMENT_TYPES,
                      &from->argument_types[from->type_start[i]],
                      (sizeof (A_ARGUMENT_TYPES_t)) * fun_p->num_arg
                    );

                    for ( ; (size_t)ix < ARGTYPE_COUNT; ix++)
                        ref_lpctype(ARGUMENT_TYPE(ix));
                }

            }
        }
        else
        {
            fun_p->flags |= NAME_TYPES_LOST;
        }

        /* Save the index where they started. Every function will have an
         * index where the type info of arguments starts.
         */
        ADD_ARGUMENT_INDEX(argindex);

    } /* for (inherited functions) pass 1 */

    return true;
}

/*-------------------------------------------------------------------------*/
static bool
inherit_variable (variable_t *variable, funflag_t varmodifier, int redeclare)

/* Copy a single variable into our program. If <redeclare> >= 0, then
 * it's a virtual variable and already exists with that number. The modifiers
 * of it will then be adjusted accordingly.
 *
 * Returns true on success, false otherwise (out of memory).
 */

{
    ident_t *p;
    funflag_t new_type = varmodifier;

    p = make_global_identifier(get_txt(variable->name), I_TYPE_GLOBAL);
    if (!p)
        return false;

    /* 'public' variables should not become private when inherited
     * 'private'.
     */
    if (variable->type.t_flags & TYPE_MOD_PUBLIC)
        new_type &= ~(TYPE_MOD_PRIVATE | TYPE_MOD_PROTECTED);

    fulltype_t vartype = variable->type;

    vartype.t_flags |= new_type 
                    | (variable->type.t_flags & TYPE_MOD_PRIVATE
                       ? (NAME_HIDDEN|NAME_INHERITED)
                       :  NAME_INHERITED
                      );
    /* The most restrictive visibility wins. */
    if (vartype.t_flags & (TYPE_MOD_PRIVATE | NAME_HIDDEN))
        vartype.t_flags &= ~(TYPE_MOD_PROTECTED | TYPE_MOD_PUBLIC);
    else if (vartype.t_flags & TYPE_MOD_PROTECTED)
        vartype.t_flags &= ~(TYPE_MOD_PUBLIC);

    if (redeclare >= 0)
        redeclare_variable(p, vartype, VIRTUAL_VAR_TAG | redeclare);
    else
        define_variable(p, vartype);
    return true;

} /* inherit_variable() */

/*-------------------------------------------------------------------------*/
static bool
is_old_inherited_function (program_t* from, int first_function_index, inherit_t *oldinheritp, int oldix, bool update_existing)

/* Check whether the function <oldix> in <oldinheritp> is actually inherited
 * from there in <from> (where it is at <first_function_index>+<oldix>).
 * <update_existing> is true, if <oldinheritp> was already fully handled.
 */

{
    function_t* oldfunp = FUNCTION(oldinheritp->function_index_offset);
    function_t* realoldfunp = oldfunp + oldix;

    while (realoldfunp->flags & NAME_CROSS_DEFINED)
        realoldfunp = realoldfunp + GET_CROSSDEF_OFFSET(realoldfunp->offset.func);

    /* Check whether it's still the original function from <oldinheritp>.
     * First condition: It must be in <oldinheritp>'s function block.
     */
    if (realoldfunp < oldfunp || oldfunp + oldinheritp->prog->num_functions <= realoldfunp)
        return false;

    if (update_existing)
    {
        /* Function entry must point to <oldinheritp>. */
        return ((realoldfunp->flags & NAME_INHERITED) && realoldfunp->offset.inherit == (oldinheritp - &INHERIT(0)));
    }
    else
    {
        /* Function entry must point to <oldinheritp>, but because the inherit handling
         * hasn't processed the function block fully, we must look at <from>'s function table.
         */
        funflag_t oldfunflag = from->functions[first_function_index + realoldfunp - oldfunp];
        return (oldfunflag & (NAME_INHERITED|NAME_CROSS_DEFINED)) == NAME_INHERITED
            && from->inherit[oldfunflag & INHERIT_MASK].prog == oldinheritp->prog;
    }
} /* is_old_inherited_function() */

/*-------------------------------------------------------------------------*/
static void
update_duplicate_function (program_t* from, int first_function_index, inherit_t *oldinheritp, inherit_t *newinheritp, int oldix, int newix, bool update_existing)

/* Do the cross-definition for a single function <oldix> in <oldinheritp> to <newix> in <newinheritp>.
 * The function block of <oldinheritp> is at <first_function_index> in <from>.
 * <update_existing> is true, if <oldinheritp> was already fully handled.
 */

{
    function_t* oldfunp = FUNCTION(oldinheritp->function_index_offset);
    function_t* newfunp = FUNCTION(newinheritp->function_index_offset);

    /* A visible entry can't be a cross-definition in an inherited entry. */
    assert(!(newfunp[newix].flags & NAME_CROSS_DEFINED));

    if (is_old_inherited_function(from, first_function_index, oldinheritp, oldix, update_existing))
    {
        /* The function was not cross-defined or overridden in <from>.
         * So, let's cross-define them, from old to new.
         */
        newfunp[newix].flags = (newfunp[newix].flags & ~(NAME_HIDDEN))
                             | (oldfunp[oldix].flags & (TYPE_MOD_STATIC|TYPE_MOD_PRIVATE|TYPE_MOD_PROTECTED|NAME_HIDDEN))
                             | TYPE_MOD_VIRTUAL;

        cross_define(newfunp + newix, oldfunp + oldix,
            newinheritp->function_index_offset + newix - oldinheritp->function_index_offset - oldix);

        update_function_identifier(newfunp + newix, newinheritp->function_index_offset + newix,
                                   oldfunp + oldix, oldinheritp->function_index_offset + oldix);
    }
    else
    {
        /* <from> already did overload or cross-define it.
         * Applying that to the new function.
         */
        cross_define(oldfunp + oldix, newfunp + newix,
            oldinheritp->function_index_offset + oldix - newinheritp->function_index_offset - newix);

        update_function_identifier(oldfunp + oldix, oldinheritp->function_index_offset + oldix,
                                   newfunp + newix, newinheritp->function_index_offset + newix);
    }

} /* update_duplicatefunction() */

/*-------------------------------------------------------------------------*/
static void
update_duplicate_functions (program_t* from, int first_function_index, inherit_t *oldinheritp, inherit_t *dupinheritp, inherit_t *newinheritp, bool update_existing)

/* Do the cross-definitions to <newinheritp> that update_virtual_program()
 * did for another inherit entry <oldinheritp> to the duplicate <dupinheritp>
 * of it (with functions corresponding to the functions starting with
 * <first_function_index> in <from>'s program).
 * <update_existing> is true, if <dupinheritp> was already fully handled.
 */

{
    program_t*  oldprogp = dupinheritp->prog;
    function_t* newfunp = FUNCTION(newinheritp->function_index_offset);
    function_t* oldfunp = FUNCTION(dupinheritp->function_index_offset);
    A_UPDATE_INDEX_MAP_t* fun_map = GET_BLOCK(A_UPDATE_INDEX_MAP) + oldinheritp->function_map_offset;

    /* And now cross-define as given in the function index map. */
    for (int oldix = 0; oldix < oldprogp->num_functions; oldix++)
    {
        if (fun_map[oldix] == USHRT_MAX)
        {
            /* No destination but visible, make it undefined. */
            if (!(oldfunp[oldix].flags & NAME_HIDDEN)
             && is_old_inherited_function(from, first_function_index, dupinheritp, oldix, update_existing))
                oldfunp[oldix].flags = NAME_UNDEFINED|NAME_HIDDEN|TYPE_MOD_PRIVATE;

            continue;
        }

        update_duplicate_function(from, first_function_index, dupinheritp, newinheritp, oldix, fun_map[oldix], update_existing);
    }

    /* And hide any surfaced functions. */
    {
        program_t* newprogp = newinheritp->prog;
        unsigned short *newix = newprogp->function_names;
        int newleft = newprogp->num_function_names;

        for (; --newleft >= 0; newix++)
        {
            if ( (newfunp[*newix].flags & (NAME_HIDDEN|NAME_CROSS_DEFINED)) == NAME_HIDDEN )
                newfunp[*newix].flags |= TYPE_MOD_PRIVATE;
        }
    }

} /* update_duplicate_functions() */

/*-------------------------------------------------------------------------*/
static bool
update_virtual_program (program_t *from, inherit_t *oldinheritp, inherit_t *newinheritp, int num_old_variables, int num_new_variables, int first_function_index, int first_variable_index, bool update_existing, funflag_t varmodifier, int num_existing_inherits)

/* Patch the inherited old program in <oldinheritp> to the new program in
 * <newinheritp>. We may either have already inherited the new program
 * and trying to inherit an old one from <from> (<update_existing> == false),
 * or have inherited an old one und trying to add the new program
 * (<update_existing> == true).
 *
 * <num_old_variables> contains the number of variables in the old
 * program itself (not counting inherited virtual variables).
 * <first_variable_index> denotes the index of the first variable
 * of <oldinheritp> (when <update_existing> == false) resp.
 * <newinheritp> (<update_existing> == true) in <from>.
 * <num_existing_inherits> denotes the number of inherits in our current
 * program without <from>'s inherit.
 *
 * These steps need to be done in these cases:
 * 1. Create a table that maps each variable from the old
 *    to the new program.
 * 2. If <update_existing> == false, inherit the variables
 *    that aren't in the new program anymore.
 *    If <update_existing> == true, remove any old variables
 *    that are in the new program, too.
 * 3. Create the function index mapping and cross-define the functions.
 * 4. If <update_existing> == true, fix all duplicate entries
 *    of the old program that are virtual, too.
 */

{
    A_UPDATE_INDEX_MAP_t *var_map, *fun_map;
    int newvx;
    inherit_t updateinherit;
    program_t *oldprogp = oldinheritp->prog, *newprogp = newinheritp->prog;
    int num_old_virtual_variables = oldprogp->num_variables - num_old_variables;
    int num_new_virtual_variables = newprogp->num_variables - num_new_variables;
    variable_t *last_additional_var;

    assert(num_old_virtual_variables == oldprogp->num_virtual_variables);
    assert(num_new_virtual_variables == newprogp->num_virtual_variables);

    /* We'll add an additional inherit entry for the
     * updated program.
     */
    updateinherit = *newinheritp;
    updateinherit.inherit_depth = oldinheritp->inherit_depth;

    if (update_existing)
        newinheritp->inherit_type |= INHERIT_TYPE_DUPLICATE;
    else
        updateinherit.inherit_type |= INHERIT_TYPE_DUPLICATE;

    oldinheritp->updated_inherit = INHERIT_COUNT;
    oldinheritp->inherit_type |= INHERIT_TYPE_MAPPED;
    oldinheritp->num_additional_variables = 0;
    oldinheritp->variable_map_offset = UPDATE_INDEX_MAP_COUNT;
    oldinheritp->function_map_offset = UPDATE_INDEX_MAP_COUNT + num_old_variables;

    /* And now map *every* old variable to the new one. */

    /* We could have saved <num_old_virtual_variables> entries, because
     * these are never mapped, as they are virtual in <oldprogp> itself.
     * But that would require either recording this number in <oldprog> or
     * <oldinheritp>, or subtracting the number from the offsets, which
     * might make them negative (but they are unsigned shorts). So we'll
     * include them for now...
     */
    if (!RESERVE_UPDATE_INDEX_MAP(num_old_variables + oldprogp->num_functions))
        return false;

    /* We'll walk through the older program's variable and have
     * to find the corresponding variable in the newer program.
     * We do this by walking in parallel through the newer
     * program's variables and looking for a variable with the same
     * name. We can't use a lookup table, as there may be several
     * variables with the same name. So we use the same lookup
     * type as restore_object and hope for the correct order of variables.
     */
    var_map = GET_BLOCK(A_UPDATE_INDEX_MAP) + oldinheritp->variable_map_offset;
    newvx = num_new_virtual_variables;
    last_additional_var = V_VARIABLE(oldinheritp->variable_index_offset + VIRTUAL_VAR_TAG);

    for (int oldvx = num_old_virtual_variables; oldvx < oldprogp->num_variables; oldvx++)
    {
        variable_t *oldvar = oldprogp->variables + oldvx;
        int lastnewvx = newvx;
        bool varfound = false;

        do
        {
            variable_t *newvar = newprogp->variables + newvx;
            if (mstreq(oldvar->name, newvar->name))
            {
                varfound = true;
                break;
            }

            newvx++;
            if (newvx == newprogp->num_variables)
                newvx = num_new_virtual_variables;
        } while (newvx != lastnewvx);

        if (varfound)
        {
            *var_map = newvx - num_new_virtual_variables;

            if (update_existing)
            {
                /* We have to remove the variables from the A_VIRTUAL_VAR
                 * block and cancel the global identifier (by setting
                 * the variable index back to I_GLOBAL_VARIABLE_OTHER).
                 */
                int cur_oldindex = oldinheritp->variable_index_offset + oldvx - num_old_virtual_variables;
                variable_t *cur_oldvar = V_VARIABLE(cur_oldindex + VIRTUAL_VAR_TAG);
                ident_t *oldvar_ident = find_shared_identifier_mstr(cur_oldvar->name, I_TYPE_GLOBAL, 0);

                /* There should have been an identifier, as that is a defined variable. */
                assert(oldvar_ident != NULL);
                /* And it should be a global identifier. */
                assert(oldvar_ident->type == I_TYPE_GLOBAL);

                /* Not redefined? */
                if (oldvar_ident->u.global.variable == (VIRTUAL_VAR_TAG | cur_oldindex))
                    oldvar_ident->u.global.variable = I_GLOBAL_VARIABLE_OTHER;

                free_mstring(cur_oldvar->name);
                free_fulltype(cur_oldvar->type);
            }
            else
            {
                /* We have to update the modifier. */
                int cur_newindex = newinheritp->variable_index_offset + newvx - num_new_virtual_variables;
                if (!inherit_variable(from->variables + first_variable_index + oldvx - num_old_virtual_variables, varmodifier, cur_newindex))
                    return false;
            }

            newvx++;
            if (newvx == newprogp->num_variables)
                newvx = num_new_virtual_variables;
        }
        else
        {
            /* The old variable doesn't exist anymore,
             * but it may be referenced by the inherited program,
             * so we have to preserve that variable.
             */
            *var_map = num_new_variables + oldinheritp->num_additional_variables;

            if (update_existing)
            {
                int cur_oldindex = oldinheritp->variable_index_offset + oldvx - num_old_virtual_variables;
                variable_t *cur_oldvar = V_VARIABLE(cur_oldindex + VIRTUAL_VAR_TAG);
                if (last_additional_var != cur_oldvar)
                {
                    /* We have to move the variable. */
                    ident_t *oldvar_ident = find_shared_identifier_mstr(cur_oldvar->name, I_TYPE_GLOBAL, 0);

                    /* There should have been an identifier, as that is a defined variable. */
                    assert(oldvar_ident != NULL);
                    /* And it should be a global identifier. */
                    assert(oldvar_ident->type == I_TYPE_GLOBAL);

                    if (oldvar_ident->u.global.variable == (VIRTUAL_VAR_TAG | cur_oldindex))
                        oldvar_ident->u.global.variable = VIRTUAL_VAR_TAG | (last_additional_var - V_VARIABLE(VIRTUAL_VAR_TAG));

                    *last_additional_var = *cur_oldvar;
                }

                last_additional_var++;
            }
            else
            {
                /* We'll take the variable description from <from>
                 * instead of <oldprogp>, because the visibility
                 * might have changed.
                 */
                if (!inherit_variable(from->variables + first_variable_index + oldvx - num_old_virtual_variables, varmodifier, -1))
                    return false;
            }

            oldinheritp->num_additional_variables++;
        }
        var_map++;
    }

    mem_block[A_UPDATE_INDEX_MAP].current_size += oldprogp->num_variables * sizeof(A_UPDATE_INDEX_MAP_t);

    if (update_existing && last_additional_var != V_VARIABLE(oldinheritp->variable_index_offset + num_old_variables + VIRTUAL_VAR_TAG))
    {
        /* We have to move all the variables that came after oldinheritp. */
        int last_variable = V_VARIABLE_COUNT;
        int from_idx = oldinheritp->variable_index_offset + num_old_variables, to_idx = last_additional_var - V_VARIABLE(VIRTUAL_VAR_TAG);
        int diff = from_idx - to_idx;
        inherit_t *last_inherit = &INHERIT(INHERIT_COUNT);

        for (inherit_t* inh = oldinheritp + 1; inh < last_inherit; inh++)
        {
            if(inh->variable_index_offset & NON_VIRTUAL_OFFSET_TAG)
                continue;

            if(inh->inherit_type & (INHERIT_TYPE_DUPLICATE))
                continue;

            inh->variable_index_offset -= diff;
        }

        for (; from_idx < last_variable; from_idx++, to_idx++)
        {
            variable_t *var = V_VARIABLE(from_idx + VIRTUAL_VAR_TAG);
            ident_t *var_ident = find_shared_identifier_mstr(var->name, I_TYPE_GLOBAL, 0);

            /* There should have been an identifier, as that is a defined variable. */
            assert(var_ident != NULL);
            /* And it should be a global identifier. */
            assert(var_ident->type == I_TYPE_GLOBAL);

            if (var_ident->u.global.variable == (VIRTUAL_VAR_TAG | from_idx))
                var_ident->u.global.variable = VIRTUAL_VAR_TAG | to_idx;

            *V_VARIABLE(to_idx + VIRTUAL_VAR_TAG) = *var;
        }

        newinheritp->variable_index_offset -= diff;
        updateinherit.variable_index_offset -= diff;
        mem_block[A_VIRTUAL_VAR].current_size -= diff * sizeof(A_VIRTUAL_VAR_t);
    }

    /* So long for the variables, now copy the new functions
     * and cross-define them to the old functions.
     *
     * We need only the cross-define visible functions,
     * because the invisible ones wouldn't be called from
     * outside of <progp> (and <progp> isn't gonna be used
     * anymore).
     * That's why we can walk the function_names list
     * on <inhprog> and <progp> and thus find common
     * functions (both lists are sorted).
     */
    fun_map = GET_BLOCK(A_UPDATE_INDEX_MAP) + oldinheritp->function_map_offset;

    {
        function_t *newfunp;
        function_t *oldfunp;
        unsigned short *oldix = oldprogp->function_names;
        unsigned short *newix = newprogp->function_names;
        int oldleft = oldprogp->num_function_names;
        int newleft = newprogp->num_function_names;

        /* Init each function map entry as undefined. */
        for (int j = 0; j < oldprogp->num_functions; j++)
            fun_map[j] = USHRT_MAX;

        updateinherit.function_index_offset = FUNCTION_COUNT;
        if (!inherit_functions(newprogp, INHERIT_COUNT))
            return false;

        mem_block[A_FUNCTIONS].current_size += sizeof(A_FUNCTIONS_t) * newprogp->num_functions;

        newfunp = FUNCTION(updateinherit.function_index_offset);
        oldfunp = FUNCTION(oldinheritp->function_index_offset);

        for (;oldleft && newleft; oldix++, oldleft--)
        {
            string_t *oldname = oldfunp[*oldix].name;
            int cmp = -1;

            /* Similar to function_cmp() in closure.c,
             * Go through the new functions as long as oldname is smaller.
             */
            while(newleft && (cmp = memcmp(&oldname, &newfunp[*newix].name, sizeof(string_t*))) > 0)
            {
                newix++;
                newleft--;
            }

            if (!cmp) /* They are equal (and therefore newleft > 0) */
            {
                function_t *curoldfunp = oldfunp + *oldix;
                function_t *curnewfunp = newfunp + *newix;

                update_duplicate_function(from, first_function_index, oldinheritp, &updateinherit, *oldix, *newix, update_existing);
                fun_map[*oldix] = *newix;

                /* Check whether their signatures are compatible
                 * and throw a warning otherwise.
                 */
                if (curoldfunp->type != NULL
                 && curoldfunp->type != lpctype_unknown
                 && curnewfunp->type != NULL
                 && curnewfunp->type != lpctype_unknown)
                {
                    /* These are the same checks as with function overloads. */
                    bool differs = false;

                    if ((curoldfunp->num_arg > curnewfunp->num_arg
                      && !(curoldfunp->flags & TYPE_MOD_VARARGS))
                     || (curoldfunp->num_arg == curnewfunp->num_arg
                      && ((curoldfunp->flags ^ curnewfunp->flags) & TYPE_MOD_XVARARGS)
                      && !(curoldfunp->flags & TYPE_MOD_VARARGS)))
                    {
                        differs = true;
                        yywarnf("Number of arguments changed when updating '%s'.", get_txt(oldname));
                    }

                    if (!differs && !has_common_type(curoldfunp->type, curnewfunp->type))
                    {
                        differs = true;
                        yywarnf("Return type changed when updating '%s'; %s", get_txt(oldname), get_two_lpctypes(curoldfunp->type, curnewfunp->type));
                    }

                    if (!differs
                     && !(curoldfunp->flags & NAME_TYPES_LOST)
                     && !(curnewfunp->flags & NAME_TYPES_LOST))
                    {
                        lpctype_t **oldargp = &ARGUMENT_TYPE(ARGUMENT_INDEX(oldinheritp->function_index_offset + *oldix));
                        lpctype_t **newargp = &ARGUMENT_TYPE(ARGUMENT_INDEX(updateinherit.function_index_offset + *newix));

                        int num_arg = curoldfunp->num_arg;
                        if (num_arg > curnewfunp->num_arg)
                            num_arg = curnewfunp->num_arg;
                        if (oldfunp->flags & TYPE_MOD_XVARARGS)
                            num_arg--;

                        for (int i = 0; i < num_arg; i++)
                        {
                            if (!has_common_type(oldargp[i], newargp[i]))
                                yywarnf("Argument type changed when updating '%s': arg %d %s", get_txt(oldname), i+1, get_two_lpctypes(oldargp[i], newargp[i]));
                        }
                    }
                }
            }
        }

        /* And now make all functions that have not been
         * cross-defined as undefined.
         */
        oldix = oldprogp->function_names;
        for (oldleft = oldprogp->num_function_names; --oldleft >= 0; oldix++)
        {
            if (fun_map[*oldix] != USHRT_MAX)
                continue;

            if (is_old_inherited_function(from, first_function_index, oldinheritp, *oldix, update_existing))
                oldfunp[*oldix].flags = NAME_UNDEFINED|NAME_HIDDEN|TYPE_MOD_PRIVATE;
        }

        /* Now we have to take care of the remaining functions with names
         * (the ones still have NAME_HIDDEN, but not being NAME_CROSS_DEFINED).
         * They must either be a registered identifier, being cross-defined,
         * or hidden as TYPE_MOD_PRIVATE. The first and second options
         * one would require to implement the function inherit logic for
         * a function that was inherited and the old inherit's place
         * with the old inherit's modifier. Therefore we use the simple
         * approach and keep all surfacing functions hidden (because they
         * were not part of the inherited program).
         */

        /* Reset newix and newleft. */
        newix = newprogp->function_names;
        newleft = newprogp->num_function_names;

        for (; --newleft >= 0; newix++)
        {
            if ( (newfunp[*newix].flags & (NAME_HIDDEN|NAME_CROSS_DEFINED)) == NAME_HIDDEN )
                newfunp[*newix].flags |= TYPE_MOD_PRIVATE;
        }
    }

    mem_block[A_UPDATE_INDEX_MAP].current_size += oldprogp->num_functions * sizeof(A_UPDATE_INDEX_MAP_t);

    ADD_INHERIT(&updateinherit);

    if (update_existing)
    {
        /* We'll have to do these cross-definitions for each
         * duplicate inherit entry of the old program.
         * But now we can use the update map for that.
         */
        inherit_t* last_inherit = &INHERIT(num_existing_inherits);
        /* We ignore any inherits from the inherited program,
         * they already are updated.
         */

        for (inherit_t* dupinheritp = oldinheritp + 1; dupinheritp < last_inherit; dupinheritp++)
        {
            if (dupinheritp->prog == oldprogp && dupinheritp->inherit_type != INHERIT_TYPE_NORMAL)
            {
                inherit_t dupupdate;

                /* If this is a virtual inherit, it must be a duplicate. */
                assert((dupinheritp->inherit_type & (INHERIT_TYPE_DUPLICATE|INHERIT_TYPE_MAPPED)) == INHERIT_TYPE_DUPLICATE);

                dupupdate = updateinherit;
                dupupdate.inherit_type |= INHERIT_TYPE_DUPLICATE;
                dupupdate.inherit_depth = dupinheritp->inherit_depth;
                dupupdate.function_index_offset = FUNCTION_COUNT;

                dupinheritp->inherit_type |= INHERIT_TYPE_MAPPED;
                dupinheritp->updated_inherit = INHERIT_COUNT;
                dupinheritp->variable_index_offset = oldinheritp->variable_index_offset;
                dupinheritp->num_additional_variables = oldinheritp->num_additional_variables;
                dupinheritp->variable_map_offset = oldinheritp->variable_map_offset;
                dupinheritp->function_map_offset = oldinheritp->function_map_offset;

                if (!inherit_functions(dupupdate.prog, INHERIT_COUNT))
                    return false;

                mem_block[A_FUNCTIONS].current_size += sizeof(A_FUNCTIONS_t) * dupupdate.prog->num_functions;

                /* And now cross-define as given in the function index map. */
                update_duplicate_functions(from, first_function_index, oldinheritp, dupinheritp, &dupupdate, true);

                ADD_INHERIT(&dupupdate);
            }
        }
    }

    return true;
} /* update_virtual_program() */

/*-------------------------------------------------------------------------*/
static bool
copy_updated_inherit (inherit_t *oldinheritp, inherit_t *newinheritp, program_t *from, int first_function_index, int first_variable_index, int last_variable_index, funflag_t varmodifier)

/* We want to copy the variables for <newinheritp>, but we already
 * got them in <oldinheritp>, and that one is an obsolete program
 * (INHERIT_TYPE_MAPPED). Copy the information from there and update
 * variable modifiers from <from> (variables from <first_variable_index>
 * to <last_variable_index>) and <varmodifier>.
 *
 * Returns true on success, false otherwise (out of memory).
 */

{
    /* We need to duplate the update inherit entry. */
    inherit_t inheritupdate = INHERIT(oldinheritp->updated_inherit);

    newinheritp->inherit_type |= INHERIT_TYPE_DUPLICATE|INHERIT_TYPE_MAPPED;
    newinheritp->variable_index_offset = oldinheritp->variable_index_offset;
    newinheritp->num_additional_variables = oldinheritp->num_additional_variables;
    newinheritp->variable_map_offset = oldinheritp->variable_map_offset;
    newinheritp->function_map_offset = oldinheritp->function_map_offset;
    newinheritp->updated_inherit = INHERIT_COUNT;

    /* Adjust modifier and identifier. */
    for (int j = first_variable_index; j < last_variable_index; j++)
    {
        inherit_t* inheritvar = oldinheritp;
        int varidx = j - first_variable_index;

        do
        {
            inherit_t* inheritnext = &INHERIT(inheritvar->updated_inherit);
            varidx = GET_BLOCK(A_UPDATE_INDEX_MAP)[inheritvar->variable_map_offset + varidx];

            if (varidx >= inheritnext->prog->num_variables - inheritnext->prog->num_virtual_variables)
            {
                /* Additional variable. */
                varidx -= inheritnext->prog->num_variables - inheritnext->prog->num_virtual_variables;
                break;
            }

            inheritvar = inheritnext;
        } while (inheritvar->inherit_type & INHERIT_TYPE_MAPPED);

        if (!inherit_variable(from->variables + j, varmodifier, inheritvar->variable_index_offset + varidx))
            return false;
    }

    inheritupdate.inherit_type |= INHERIT_TYPE_DUPLICATE;
    inheritupdate.inherit_depth = newinheritp->inherit_depth;

    /* Add the update function table. */
    inheritupdate.function_index_offset = FUNCTION_COUNT;
    if (!inherit_functions(inheritupdate.prog, INHERIT_COUNT))
        return false;

    mem_block[A_FUNCTIONS].current_size += sizeof(A_FUNCTIONS_t) * inheritupdate.prog->num_functions;

    update_duplicate_functions(from, first_function_index, oldinheritp, newinheritp, &inheritupdate, false);

    ADD_INHERIT(&inheritupdate);

    /* Resolve further inherit mappings. */
    while (inheritupdate.inherit_type & INHERIT_TYPE_MAPPED)
    {
        INHERIT(INHERIT_COUNT-1).updated_inherit = INHERIT_COUNT;

        inheritupdate = INHERIT(inheritupdate.updated_inherit);
        inheritupdate.inherit_type |= INHERIT_TYPE_DUPLICATE;
        inheritupdate.inherit_depth = newinheritp->inherit_depth;
        ADD_INHERIT(&inheritupdate);
    }

    return true;
} /* copy_updated_inherit() */

/*-------------------------------------------------------------------------*/
static bool
inherit_virtual_variables (inherit_t *newinheritp, program_t *from, int first_function_index, int first_variable_index, int last_variable_index, funflag_t varmodifier, int num_existing_inherits)

/* Copy the virtual variables from <from> into our program.
 * <newinheritp> is the inherit structure that is going to be inserted into
 * our programm and should already be initialized. <first_function_index>
 * is the index into <from>'s function of the first function of this inherit
 * (the original function_index_offset in <from>).
 * <first_variable_index> and <last_variable_index> are indices into <from>'s
 * variable corresponding to this virtual inherit (last_variable_index really
 * points one variable behind the last inherit's variable).
 * <num_existing_inherits> denotes the number of inherits in our current
 * program without <from>'s inherit.
 *
 * Returns true on success, false otherwise (out of memory).
 */

{
    program_t* progp = newinheritp->prog;

    /* Do we already know this inherit? */
    inherit_t* first_inherit = GET_BLOCK(A_INHERITS);
    inherit_t* inheritdup = first_inherit;
    inherit_t* inheritorig = NULL;
    bool found = false;

    /* First look for the same program already inherited virtually. */
    for (int i = INHERIT_COUNT; --i >= 0; inheritdup++)
    {
        /* Non-Virtual? */
        if(inheritdup->variable_index_offset & NON_VIRTUAL_OFFSET_TAG)
            continue;

        /* Ignore inherits we already looked at. */
        if(inheritdup->inherit_type & INHERIT_TYPE_DUPLICATE)
            continue;

        if (inheritdup->prog != progp)
            continue;

        if(inheritdup->inherit_type & INHERIT_TYPE_MAPPED)
        {
            /* Found it, but is obsolete.
             * We use its function and variable map and additional variables.
             * And we copy its update inherit entry.
             */
            if (!copy_updated_inherit(inheritdup, newinheritp, from, first_function_index, first_variable_index, last_variable_index, varmodifier))
                return false;
        }
        else
        {
            /* Found it, use their variables. */
            newinheritp->variable_index_offset = inheritdup->variable_index_offset;
            newinheritp->inherit_type |= INHERIT_TYPE_DUPLICATE;

            /* Adjust modifier and identifier. */
            for (int j = first_variable_index; j < last_variable_index; j++)
                if (!inherit_variable(from->variables + j, varmodifier, inheritdup->variable_index_offset + j - first_variable_index))
                    return false;
        }

        found = true;
        break;
    }

    /* Then we search for a program with the same name (also virtually inherited). */
    inheritdup = first_inherit;
    for (int i = num_existing_inherits; !found && --i >= 0; inheritdup++)
    {
        /* Non-Virtual? */
        if(inheritdup->variable_index_offset & NON_VIRTUAL_OFFSET_TAG)
            continue;

        /* We are only interested in the newest one. */
        if(inheritdup->inherit_type & INHERIT_TYPE_MAPPED)
            continue;

        if (!mstreq(inheritdup->prog->name, progp->name))
            continue;

        /* First determine the number of variables. */
        program_t *inhprog = inheritdup->prog;
        int first_inh_variable = inheritdup->variable_index_offset;
        int last_inh_variable = inheritdup->variable_index_offset + inhprog->num_variables - inhprog->num_virtual_variables;

        assert(last_variable_index == first_variable_index + progp->num_variables - progp->num_virtual_variables);

        if ( (inhprog->load_time == progp->load_time)
           ? (inhprog->id_number > progp->id_number)
           : (inhprog->load_time > progp->load_time) )
        {
            /* Phew, the already inherited program is newer.
             * We'll use their variables but also create
             * a mapping from old to new variable indices.
             */
            if (inheritdup->inherit_type & INHERIT_TYPE_DUPLICATE)
                continue; /* The original will come later. */

            update_virtual_program(from
                                  , newinheritp
                                  , inheritdup
                                  , last_variable_index - first_variable_index
                                  , last_inh_variable - first_inh_variable
                                  , first_function_index
                                  , first_variable_index
                                  , false
                                  , varmodifier
                                  , num_existing_inherits
                                  );

            found = true;
        }
        else if (!(inheritdup->inherit_type & INHERIT_TYPE_DUPLICATE))
        {
            /* Damn, we've inherited an old program.
             * We'll have to fix that one now.
             */
            update_virtual_program(from
                                  , inheritdup
                                  , newinheritp
                                  , last_inh_variable - first_inh_variable
                                  , last_variable_index - first_variable_index
                                  , first_function_index
                                  , first_variable_index
                                  , true
                                  , varmodifier
                                  , num_existing_inherits
                                  );

            /* Remember this, in case we meet some duplicates. */
            inheritorig = inheritdup;

            /* Variables still need to be inherited. */
            found = false;
        }
        else
        {
            /* Okay, this is the duplicate of an old program.
             * So we must have seen the non-duplicate version.
             * Use that mapping.
             */
            assert(inheritorig != NULL);
            assert(inheritorig->inherit_type & INHERIT_TYPE_MAPPED);

            inheritdup->inherit_type |= INHERIT_TYPE_MAPPED;
            inheritdup->variable_index_offset = inheritorig->variable_index_offset;
            inheritdup->num_additional_variables = inheritorig->num_additional_variables;
            inheritdup->variable_map_offset = inheritorig->variable_map_offset;
            inheritdup->function_map_offset = inheritorig->function_map_offset;
            inheritdup->updated_inherit = INHERIT_COUNT;

            /* We need to duplicate the update inherit as well. */
            inherit_t inheritupdate = INHERIT(inheritorig->updated_inherit);
            inheritupdate.inherit_type |= INHERIT_TYPE_DUPLICATE;
            inheritupdate.inherit_depth = inheritdup->inherit_depth;

            /* And now we need to cross-define its functions. */
            inheritupdate.function_index_offset = FUNCTION_COUNT;
            if (!inherit_functions(inheritupdate.prog, INHERIT_COUNT))
                return false;

            mem_block[A_FUNCTIONS].current_size += sizeof(A_FUNCTIONS_t) * inheritupdate.prog->num_functions;

            update_duplicate_functions(from, first_function_index, inheritorig, inheritdup, &inheritupdate, true);

            ADD_INHERIT(&inheritupdate);

            /* The update inherit cannot already be obsoleted, too. */
            assert(!(inheritupdate.inherit_type & INHERIT_TYPE_MAPPED));

            /* Continue to other duplicates. */
            found = false;
        }
    }

    if (!found)
    {
        assert(last_variable_index <= first_variable_index + progp->num_variables);

        /* First occurence of these virtual variables, we're
         * going to copy them into our variables.
         */
        for (int i = first_variable_index; i < last_variable_index; i++)
            if (!inherit_variable(from->variables + i, varmodifier, -1))
                return false;
    }

    return true;

} /* inherit_virtual_variables() */


/*-------------------------------------------------------------------------*/
static int
inherit_obsoleted_variables  (inherit_t *newinheritp, program_t *from, int first_variable_index, funflag_t varmodifier)

/* Copy the virtual variables from <from> into our program.
 * This is like inherit_virtual_variables() but just for inherited programs
 * that are obsoleted by other programs. We don't calculate mappings
 * between old and new program, but adapt the mappings from <from>.
 *
 * This is needed, because obsoleted inherited programs may still have
 * variables (ones the old program had, but not the new program)
 * and may still be called, so we need to find it and its mapping.
 *
 * Returns true on success, false otherwise (out of memory).
 */
{
    /* Copy the additional variables. */
    for (int i = first_variable_index, j = 0; j < newinheritp->num_additional_variables; i++, j++)
        if (!inherit_variable(from->variables + i, varmodifier, -1))
            return false;


    return true;
} /* inherit_obsoleted_variables() */


/*-------------------------------------------------------------------------*/
static int
inherit_program (program_t *from, funflag_t funmodifier, funflag_t varmodifier)

/* Copies struct definitions, functions and variables from the program <from>
 * into our program. Functions are copied with visibility <funmodifier>,
 * variables with visibility <varmodifier>.
 *
 * We do this by iterating through <from>'s inherit list and copying the
 * function definitions and variables for each inherit into our program.
 * It is important that the order of function and variables will be preserved.
 *
 * Special care must be taken for virtual inherits in <from>.
 * 1. If this is the first occurrence of an inherit with this name,
 *    its variables get another variable index offset,
 *    because virtual variables are stored at the beginning of
 *    the variable block. This is done by introducing a new
 *    inherit entry (INHERIT_TYPE_EXTRA) that is referenced
 *    by the functions of this inherit.
 * 2. If this inherit is already known don't copy the variables
 *    but adopt the variable index offset of the earlier occurrence.
 *    (Still add a INHERIT_TYPE_EXTRA entry. It'll get the
 *    INHERIT_TYPE_DUPLICATE flag.)
 * 3. If there was already a different inherited program with
 *    the same, check which of them is newer.
 *    a) If the earlier inherit is newer, treat it like case (2),
 *       but add a variable index mapping to the INHERIT_TYPE_EXTRA
 *       structure. Set the INHERIT_TYPE_MAPPED flag. All vanished
 *       variables will be reserved and added to the and of the
 *       variable block.
 *       Also copy the functions again, this time from the newer
 *       program, and cross-define the function entries from the
 *       older program to the newer (unless they we're already
 *       overridden or cross-defined). Make vanished functions
 *       undefined.
 *    b) If the new inherit is newer, do (a) the other way around:
 *       Add a variable index mapping to its inherit entry.
 *       Replace the already copied variables from the old
 *       program with ones from the newer program. (We might move
 *       later inherited virtual variables a little bit.)
 *       And copy and cross-define functions from the newer program.
 *
 * The result is the function index of the inherited __INIT function,
 * or -1 if the inherited program doesn't have an initializer.
 */

{
    int initializer = -1;                      /* Function index of __INIT */
    function_t *fun_p;                         /* Function pointer as we move
                                                * through the list. */
    uint32 first_func_index = FUNCTION_COUNT;  /* Index of the first inherited
                                                * function. */
    unsigned short* new_inherit_indices;       /* For each inherit entry in
                                                * <from> remember the index
                                                * in the current program.
                                                */
    int first_inh_index = INHERIT_COUNT;       /* Index of the first inherited
                                                * program.
                                                */

   /*                                *
    *   Preparations for functions   *
    *                                */

    /* We reserve space for update inherit entries,
     * so there wouldn't happen any reallocation and thus
     * no moving of our inherit entries later on.
     */
    if (!RESERVE_INHERITS(INHERIT_COUNT + 2*from->num_inherited + 1))
        return -1;

    /* For now, we mask out the INHERIT field in the flags and
     * use NEW_INHERITED_INDEX for the value.
     *
     * We'll do cross-definitions and visibility later.
     * For now we just collect the function information.
     * We'll need to do it here at the beginning, so we can easily
     * detect visible definitions (by looping over the function names).
     */
    if (!inherit_functions(from, NEW_INHERITED_INDEX))
        return -1;

    /* Point back to the begin of the copied function data */
    fun_p = FUNCTION(first_func_index);

    /* Unhide all function for which names exist */
    {
        unsigned short *ixp = from->function_names;
        for (int i = from->num_function_names; --i >= 0; )
        {
            fun_p[*ixp++].flags &= ~NAME_HIDDEN;
        }
    }

    mem_block[A_FUNCTIONS].current_size += sizeof *fun_p * from->num_functions;


    /* Shouldn't have VAR_INITILIALIZED set, but you never know... */
    varmodifier &= ~VAR_INITIALIZED;


   /*                                  *
    *   Processing <from>'s inherits   *
    *                                  */

   /* Remember that <from>'s layout looks something like this:
    *
    * Inherit list:                V1    V2    I1  V3  V1 I2
    * Function table:          (H1 V1 H2 V2 H3 I1) V3 (V1 I2) <from>
    * Variable list:  V1 V2 V3 (H1    H2    H3 I1)        I2  <from>
    *
    * Where V1, V2, V3 are virtual inherits either directly or
    * indirectly from the non-virtual inherit I1,and H1 - H3
    * are indirectly by I1 inherited non-virtual programs.
    * The paranthesis denote the area that is indicated by I1's
    * inherit entry.
    *
    * There are two things to pay attention to:
    * 1) For functions before the functions denoted by an
    *    INHERIT_TYPE_EXTRA inherit (function_index_offset),
    *    there may already be (non-virtual) functions from
    *    other not-mentioned inherits.
    * 2) Virtual variables are moved to the beginning of the
    *    variable block (see above). Because of that
    *    variable_index_offset points to the begin of
    *    its block of non-virtual variables.
    *    (So variable_index_offset + progp->num_variables
    *    - progp->num_virtual_variables points to the
    *    end of the variable block.)
    */

    /* Virtual variables need special treatment,
     * as we have to copy the corresponding inherit_t
     * entry from <from> into our own program and
     * sort duplicates out. Therefore first we're
     * going through the the inherit list and
     * handle virtual variables only. At the end all
     * remaining variables are non-virtual ones and
     * will be copied into our own as well.
     *
     * Functions will be handled similarly. First we'll
     * look at all (already) virtual functions and then
     * at the rest (the remaining functions will still have
     * NEW_INHERITED_INDEX as the inherit index). In this
     * phase we just set the inherit index for each function.
     * Cross-definitions will be done later in one big loop.
     */

    /* Remember the last handled variable.
     * This is the index into <from>'s variables.
     */
    int last_bound_variable = 0;

    int inheritnum = from->num_inherited; /* Number of inherits.          */
    new_inherit_indices = xalloc(sizeof(*new_inherit_indices) * inheritnum);

    for (int inheritidx = 0; inheritidx < inheritnum; inheritidx++)
    {
        inherit_t* inheritp = from->inherit + inheritidx;
        program_t* progp    = inheritp->prog;

        /* We'll handle non-virtual variables and functions at the end. */
        if (inheritp->inherit_type == INHERIT_TYPE_NORMAL)
            continue;

        /* Create a new inherit entry. */
        inherit_t newinherit;
        newinherit = *inheritp;
        newinherit.inherit_type = INHERIT_TYPE_EXTRA | (inheritp->inherit_type & INHERIT_TYPE_DUPLICATE);
        newinherit.inherit_depth++;
        newinherit.function_index_offset += first_func_index;
        newinherit.variable_index_offset += V_VARIABLE_COUNT - last_bound_variable;

        assert((inheritp->inherit_type & INHERIT_TYPE_DUPLICATE) || (last_bound_variable == inheritp->variable_index_offset));

        if (inheritp->inherit_type & INHERIT_TYPE_MAPPED)
        {
            if (!(inheritp->inherit_type & INHERIT_TYPE_DUPLICATE))
                inherit_obsoleted_variables(&newinherit, from, last_bound_variable, varmodifier);
            newinherit.inherit_type |= INHERIT_TYPE_MAPPED;
            /* The corresponding updated_inherit entry will
             * be corrected in the loop below.
             */

            if (!(inheritp->inherit_type & INHERIT_TYPE_DUPLICATE))
                last_bound_variable = inheritp->variable_index_offset + inheritp->num_additional_variables;
        }
        else
        {
            int next_bound_variable = inheritp->variable_index_offset + progp->num_variables - progp->num_virtual_variables;

            inherit_virtual_variables(&newinherit, from,
                newinherit.function_index_offset - first_func_index,
                inheritp->variable_index_offset, next_bound_variable, varmodifier, first_inh_index);

            if (!(inheritp->inherit_type & INHERIT_TYPE_DUPLICATE))
                last_bound_variable = next_bound_variable;

            /* Now adjust the function inherit index.
             * If the function isn't cross-defined or overridden in <from>
             * (i.e. it really is a call into the virtual inherit),
             * then call it through <newinherit> now.
             *
             * inherit_virtual_variables may have done some cross-definitions.
             * And not all function within inheritp's range may have its
             * inherit index, because an intermediate inherit might have
             * overridden some of them, then they'll get that inherit's index.
             */
            fun_p = FUNCTION(newinherit.function_index_offset);
            funflag_t* flag_p = from->functions + inheritp->function_index_offset;
            int newinheritidx = INHERIT_COUNT;
            for (int i = progp->num_functions; --i >= 0; fun_p++, flag_p++)
            {
                if ((*flag_p & (NAME_INHERITED|NAME_CROSS_DEFINED)) == NAME_INHERITED
                  && (fun_p->flags & (NAME_INHERITED|NAME_CROSS_DEFINED)) == NAME_INHERITED
                  && (*flag_p & INHERIT_MASK) == inheritidx
                   )
                {
                    fun_p->offset.inherit = newinheritidx;
                }
            }
        }

        new_inherit_indices[inheritidx] = INHERIT_COUNT;
        ADD_INHERIT(&newinherit);
    }

    /* So, now walk over them again to copy variable and function mappings
     * for obsoleted programs (that were already obsoleted in <from>).
     */
    for (int inheritidx = 0; inheritidx < inheritnum; inheritidx++)
    {
        inherit_t *from_old_inheritp = from->inherit + inheritidx;
        inherit_t *cur_old_inheritp;
        int num_vars, num_funs;

        if (!(from_old_inheritp->inherit_type & INHERIT_TYPE_MAPPED))
            continue;

        cur_old_inheritp = &INHERIT(new_inherit_indices[inheritidx]);

        /* Our obsoleted inherit entry shouldn't have an updated_inherit entry, yet. */
        cur_old_inheritp->updated_inherit = new_inherit_indices[from_old_inheritp->updated_inherit];

        /* We can copy the mapping as it is, because the indices
         * are always relative to the inherited program.
         */
        num_vars = cur_old_inheritp->prog->num_variables - cur_old_inheritp->prog->num_virtual_variables;
        num_funs = cur_old_inheritp->prog->num_functions;

        cur_old_inheritp->variable_map_offset = UPDATE_INDEX_MAP_COUNT;
        cur_old_inheritp->function_map_offset = UPDATE_INDEX_MAP_COUNT + num_vars;

        if (!RESERVE_UPDATE_INDEX_MAP(num_vars + num_funs))
            break;

        memcpy(GET_BLOCK(A_UPDATE_INDEX_MAP) + cur_old_inheritp->variable_map_offset,
               from->update_index_map + from_old_inheritp->variable_map_offset,
               (num_vars + num_funs) * sizeof(A_UPDATE_INDEX_MAP_t));

        mem_block[A_UPDATE_INDEX_MAP].current_size += (num_vars + num_funs) * sizeof(A_UPDATE_INDEX_MAP_t);
    }

    xfree(new_inherit_indices);

    /* And now to something completely different, <from> itself. */

    inherit_t frominherit;
    frominherit.prog = from;
    frominherit.function_index_offset = first_func_index;
    frominherit.inherit_depth = 1;

    /* We're done with the virtual extra inherits,
     * copy the remaining variables (variable indices from
     * last_bound_variables to from->num_variables).
     */
    assert(last_bound_variable == from->num_virtual_variables);
    if (varmodifier & TYPE_MOD_VIRTUAL)
    {
        /* And they're gonna be virtual, too...
         */
        frominherit.inherit_type = INHERIT_TYPE_VIRTUAL;
        frominherit.variable_index_offset = V_VARIABLE_COUNT;

        inherit_virtual_variables(&frominherit, from, 0, last_bound_variable,
            from->num_variables, varmodifier, first_inh_index);
    }
    else
    {
        frominherit.inherit_type = INHERIT_TYPE_NORMAL;
        frominherit.variable_index_offset = NV_VARIABLE_COUNT | NON_VIRTUAL_OFFSET_TAG;

        for (int i = last_bound_variable; i < from->num_variables; i++)
            if (!inherit_variable(from->variables + i, varmodifier, -1))
                break;
    }

    /* Hey, we're done with the variables, now to the functions.
     * Set the inherit index for all functions that have none, yet.
     */
    fun_p = FUNCTION(frominherit.function_index_offset);
    int frominheritidx = INHERIT_COUNT;
    for (int i = from->num_functions; --i >= 0; fun_p++)
    {
        if (fun_p->offset.inherit == NEW_INHERITED_INDEX)
            fun_p->offset.inherit = frominheritidx;
    }

    ADD_INHERIT(&frominherit);

    /* And finally, let's do cross-definitions and apply the modifiers.
     *
     * Loop again over the inherited functions, checking visibility
     * and re/crossdefinition, and updating their function indices.
     * Do not call define_new_function() from here, as duplicates would
     * be removed.
     */
    fun_p = FUNCTION(first_func_index);
    for (int newix = 0; newix < from->num_functions; newix++)
    {
        int i = newix;                                 /* Index relative to the inherit
                                                        * Same as index into fun_p.
                                                        */
        int current_func_index = first_func_index + i; /* Index into the current prog. */

        program_t *funprogp = from;
        int funprogidx = i;                            /* Index into <funprog>. */

        function_t fun = fun_p[i];
        if (fun.flags & NAME_CROSS_DEFINED)
        {
            /* We'll check whether this is a cross-definition to an
             * updated virtual program. Then we need to handle the
             * name there.
             */
            int32 offset = GET_CROSSDEF_OFFSET(fun.offset.func);
            if (i + offset >= from->num_functions)
            {
                i += offset;
                current_func_index += offset;
                fun = fun_p[i];

                /* This should be a virtual inherited function. */
                assert(fun.flags & TYPE_MOD_VIRTUAL);
                funprogp = INHERIT(fun.offset.inherit).prog;
                funprogidx = i + first_func_index - INHERIT(fun.offset.inherit).function_index_offset;
            }
        }

        /* Apply nomask now, visibility later when we know
         * that this the dominant definition.
         */
        fun.flags |= funmodifier & TYPE_MOD_NO_MASK;

        /* Perform a lot of tests and actions for the visibility
         * and definitiability. The do-while(false) allows us to abort
         * easily without using gotos.
         */
        do
        {
            /* Ignore cross defines.
             * They are the only complete invisible entries.
             */
            if (fun.flags & NAME_CROSS_DEFINED)
                break;

            /* Visible: create a new identifier for it */
            ident_t* p = make_global_identifier(get_txt(fun.name), I_TYPE_GLOBAL);
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
                if ( n != I_GLOBAL_FUNCTION_OTHER && n != first_func_index + i)
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
                    if ((uint32)n < first_func_index || (uint32)n >= first_func_index + from->num_functions)
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
                        else if ( (fun.flags & (TYPE_MOD_PRIVATE|NAME_HIDDEN))
                                    == (TYPE_MOD_PRIVATE|NAME_HIDDEN) )
                        {
                            /* There is already one function with this
                            * name. Ignore the private one, as we
                            * only need it for useful error messages.
                            */

                            break;
                        }
                        else if ( (OldFunction->flags & (TYPE_MOD_PRIVATE|NAME_HIDDEN))
                                     == (TYPE_MOD_PRIVATE|NAME_HIDDEN) )
                        {
                            /* The old one was invisible, ignore it
                             * and take this one.
                             */

                            p->u.global.function = current_func_index;
                        }
                        else if ((fun.flags | funmodifier) & TYPE_MOD_VIRTUAL
                              && OldFunction->flags & TYPE_MOD_VIRTUAL
                          &&    get_virtual_function_id(funprogp, funprogidx)
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
                    else if ( (fun.flags & (TYPE_MOD_PRIVATE|NAME_HIDDEN))
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
                        if ( (FUNCTION(n)->flags & (TYPE_MOD_PRIVATE|NAME_HIDDEN))
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
                     || (p->u.global.efun == I_GLOBAL_EFUN_OTHER
                      && p->u.global.sim_efun == I_GLOBAL_SEFUN_OTHER
#ifdef USE_PYTHON
                      && !is_python_efun(p)
#endif
                        )
                     || (fun.flags & (TYPE_MOD_PRIVATE|NAME_HIDDEN)) == 0
                       )
                     {
                        /* This is not an inherited private function shadowing
                         * a (simul-)efun.
                         */

                        if (p->u.global.efun != I_GLOBAL_EFUN_OTHER
                         || p->u.global.sim_efun != I_GLOBAL_SEFUN_OTHER
#ifdef USE_PYTHON
                         || is_python_efun(p)
#endif
                           )
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
                        if (p->u.global.function == I_GLOBAL_FUNCTION_OTHER)
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
            funflag_t new_type = funmodifier;
            if (fun.flags & TYPE_MOD_PUBLIC)
                new_type &= ~(TYPE_MOD_PRIVATE | TYPE_MOD_PROTECTED | TYPE_MOD_STATIC);

            fun.flags |= new_type;

            /* The most restrictive visibility wins. */
            if (fun.flags & (TYPE_MOD_PRIVATE | NAME_HIDDEN))
                fun.flags &= ~(TYPE_MOD_PROTECTED | TYPE_MOD_STATIC | TYPE_MOD_PUBLIC);
            else if (fun.flags & TYPE_MOD_PROTECTED)
                fun.flags &= ~(TYPE_MOD_STATIC | TYPE_MOD_PUBLIC);
            else if (fun.flags & TYPE_MOD_STATIC)
                fun.flags &= ~(TYPE_MOD_PUBLIC);

            /* Recognize an inherited heart_beat(), making it possible
             * to mask it.
             */
            if ((heart_beat == -1)
             && mstreq(fun.name, STR_HEART_BEAT))
            {
                heart_beat = current_func_index;
            }

            /* Recognize the initializer function */
            if (mstreq(fun.name, STR_VARINIT)
             && (fun.flags & (TYPE_MOD_PRIVATE|NAME_HIDDEN)) != (TYPE_MOD_PRIVATE|NAME_HIDDEN))
            {
                initializer = i;
                fun.flags |= NAME_UNDEFINED;
            }
        } while(false); /* do loop for visibility/redefinability */


        /* Finally update the entry in the A_FUNCTIONS area */
        fun_p[i] = fun;
    } /* for (inherited functions), pass 2 */

    copy_structs(from, funmodifier & ~(TYPE_MOD_STATIC|TYPE_MOD_VIRTUAL));

    return initializer;

} /* inherit_program() */

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
            inheritp->variable_index_offset &= ~NON_VIRTUAL_OFFSET_TAG;
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

        inc.filename = new_unicode_tabled(filename);
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

            inc.name = new_unicode_tabled(tmp);
            if (inc.name == NULL)
            {
                inc.name = ref_mstring(STR_DEFAULT);
                yyerror("Out of memory: sharing include name");
            }
        }

        /* Complete the structure and store it */
        inc.depth = depth;
        rc = INCLUDE_COUNT;
        ADD_INCLUDE(&inc);
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
        include_t * inc = &(INCLUDE(inc_offset));
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
void
init_compiler ()

/* Initializes the compiler at program startup.
 */

{
    make_static_type(get_array_type(lpctype_unknown),               &_lpctype_unknown_array);
    make_static_type(get_array_type(lpctype_mixed),                 &_lpctype_any_array);
    make_static_type(get_union_type(lpctype_int, lpctype_float),    &_lpctype_int_float);
    make_static_type(get_array_type(lpctype_int),                   &_lpctype_int_array);
    make_static_type(get_array_type(lpctype_string),                &_lpctype_string_array);
    make_static_type(get_array_type(lpctype_object),                &_lpctype_object_array);
    make_static_type(get_array_type(lpctype_bytes),                 &_lpctype_bytes_array);
    make_static_type(get_union_type(lpctype_string, lpctype_bytes), &_lpctype_string_bytes);
    make_static_type(get_union_type(lpctype_string_array, lpctype_bytes_array), &_lpctype_string_or_bytes_array);
    make_static_type(get_union_type(lpctype_string, lpctype_object),&_lpctype_string_object);
    make_static_type(get_array_type(lpctype_string_object),         &_lpctype_string_object_array);
} /* init_compiler() */

/*-------------------------------------------------------------------------*/
static int
get_simul_efun_index (string_t *name)

/* Searches for a simul-efun of that name and returns its index.
 * If none was found or sefuns are disabled, returns -1.
 */

{
    if (!disable_sefuns)
    {
        ident_t *id = make_shared_identifier_mstr(name, I_TYPE_UNKNOWN, 0);

        if (!id)
            fatal("Out of memory: identifier '%s'.\n", get_txt(name));

        if (id->type != I_TYPE_UNKNOWN)
        {
            /* This shouldn't be necessary, but just in case... */
            while (id && id->type > I_TYPE_GLOBAL)
                id = id->inferior;

            if ( id
              && id->u.global.function == I_GLOBAL_FUNCTION_OTHER
              && id->u.global.sim_efun != I_GLOBAL_SEFUN_OTHER)
            {
                /* There is a sefun for call_other() */
                return id->u.global.sim_efun;
            }
        }
    } /* if (!disable_sefuns) */

    return -1;
} /* get_simul_efun_index() */

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

    extend_mem_block(A_LOCAL_VARIABLES, MAX_LOCAL * sizeof(A_LOCAL_VARIABLES_t));
    memset(&LOCAL_VARIABLE(0), 0, LOCAL_VARIABLE_COUNT * sizeof(A_LOCAL_VARIABLES_t));

    local_variables = &(LOCAL_VARIABLE(0));
    context_variables = local_variables;
#ifdef DEBUG_INLINES
printf("DEBUG: prolog: type ptrs: %p, %p\n", local_variables, context_variables );
#endif /* DEBUG_INLINES */

    compiled_file = fname;
    stored_lines = 0;
    stored_bytes = 0;
    last_include_start = -1;
    memset(prog_string_indizes, -1, sizeof prog_string_indizes);
    num_virtual_variables = 0;
    case_state.free_block = NULL;
    case_state.next_free = NULL;
    last_initializer_end = -4; /* To pass the test in transfer_init_control() */
    variables_initialized = 0;
    argument_level = 0;
    function_call_info[0].got_ellipsis = false;
    function_call_info[0].unlimited_args = false;
    function_call_info[0].remaining_arg_types = 0;
    arg_types_exhausted = false;

    max_number_of_init_locals = 0;

    /* Check if call_other() has been replaced by a sefun.
     */
    call_other_sefun = get_simul_efun_index(STR_CALL_OTHER);
    call_strict_sefun = get_simul_efun_index(STR_CALL_STRICT);

} /* prolog() */

/*-------------------------------------------------------------------------*/
static void
epilog (void)

/* The parser finished - now collect the information and generate
 * the program structure, if the parse was successful.
 */

{
    int          i, fx;
    p_int        size;
    mp_int       num_functions;
    mp_int       num_strings;
    mp_int       num_variables;
    mp_int       num_function_headers;
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
            /* Update the function header for __INIT. Look at the __INIT
             * block_scope (#0), whether we need some space for local variables.
             */
            define_new_function(MY_FALSE, ip, 0, max_number_of_init_locals
                                , first_initializer_start
                                , TYPE_MOD_PROTECTED, get_fulltype(lpctype_unknown));
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
    if (STRING_COUNT > 0x10000)
        yyerror("Too many strings");

    /* Get and check the numbers of functions, strings, and variables */
    num_functions = FUNCTION_COUNT;
    if (num_functions > 0xffff)
    {
        yyerror("Too many functions");
    }
    num_strings = STRING_COUNT;
    num_variables = V_VARIABLE_COUNT;
    if (num_variables >= VIRTUAL_VAR_TAG)
    {
        yyerror("Too many variables");
    }

#if 0
    printf("DEBUG: ----- Inherit list for %s: -----\n", current_loc.file->name);
    for(i = 0; i < INHERIT_COUNT; i++)
    {
        inherit_t* inh = &INHERIT(i);

        printf("DEBUG: [%03d: %02x] var: %3d%s, fun: %3d - %-32s ",
            i, inh->inherit_type,
            inh->variable_index_offset & ~NON_VIRTUAL_OFFSET_TAG,
            (inh->variable_index_offset & NON_VIRTUAL_OFFSET_TAG) ? "r" : "v",
            inh->function_index_offset,
            get_txt(inh->prog->name));

        if (inh->inherit_type & INHERIT_TYPE_MAPPED)
            printf("(mapped to %d)\n", inh->updated_inherit);
        else if (inh->inherit_type & INHERIT_TYPE_DUPLICATE)
            printf("(duplicate)\n");
        else if (inh->inherit_type & INHERIT_TYPE_EXTRA)
            printf("(extra)\n");
        else if (inh->inherit_type & INHERIT_TYPE_VIRTUAL)
            printf("(virtual)\n");
        else
            printf("(regular)\n");
    }
    printf("DEBUG: ------\n");

    printf("DEBUG: ----- Function table for %s: -----\n", current_loc.file->name);
    for(i = 0; i < num_functions; i++)
    {
        f = FUNCTION(i);

        printf("DEBUG: [%03d: %08x] %-32s ", i, f->flags, get_txt(f->name));
        if (f->flags & NAME_INHERITED)
        {
            if (f->flags & NAME_CROSS_DEFINED)
                printf("(cross-defined to %d)\n", (int)(i + GET_CROSSDEF_OFFSET(f->offset.func)));
            //else if (f->flags & NAME_HIDDEN)
            //    printf("(hidden)\n");
            else
                printf("(inherited from %s [%d])\n", get_txt(INHERIT(f->offset.inherit).prog->name), f->offset.inherit);
        }
        else if (f->flags & NAME_UNDEFINED)
            printf("(undefined)\n");
        else if (f->flags & NAME_PROTOTYPE)
            printf("(declared)\n");
        else
            printf("(defined)\n");
    }
    printf("DEBUG: ------\n");
#endif

    num_function_names = 0;
    num_function_headers = 0;
    if (!num_parse_error && !inherit_file)
    {
        /* If the parse was successful, fill in undefined functions,
         * resolve cross-defines, and sort the program names with mergesort.
         */

        function_t **link1, **link2;  /* Linkpointer for the sort */

        f = FUNCTION(0);
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

                /* We'll include prototype in the function_names list,
                 * but make it protected. We need them in the function_names
                 * list, so we can easily handle program updates of virtually
                 * inherited programs, as we have to patch prototypes there.
                 * But when prototypes are included in function_names, then
                 * they'll get callable by call_other() and similar functions,
                 * which will result in F_UNDEF throwing an error. So we make
                 * them protected. This'll make sure, no external program
                 * can call them. And inheriting programs can override it
                 * with their own visibility.
                 */
                if (!(f->flags & (TYPE_MOD_PRIVATE|NAME_HIDDEN)))
                    f->flags = ( f->flags & ~TYPE_MOD_PUBLIC ) | TYPE_MOD_PROTECTED;
                else if (f->flags & NAME_PROTOTYPE) /* Private prototypes don't make sense. */
                    yywarnf("Private prototype of '%s' has no definition", get_txt(f->name));
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
            if ( !(flags & (NAME_HIDDEN|TYPE_MOD_PRIVATE) ) )
            {
                *link1 = f;
                link1 = link2;
                link2 = &f->offset.next;
                num_function_names++;
            }

            if ( !(flags & (NAME_INHERITED)))
                num_function_headers++;
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

        /* Raise error if program got too large. */
        if (CURRENT_PROGRAM_SIZE > FUNSTART_MASK)
        {
            yyerror("Program too large");
        }

        /* Done: functions are sorted, resolved, etc etc */

        /* Check for unused variables. */
        if (pragma_warn_unused_variables)
        {
            for (int vidx = 0; vidx < GLOBAL_VARIABLE_COUNT; vidx++)
            {
                /* Check only private variables, that were not inherited. */
                variable_t *varp = NV_VARIABLE(vidx);
                if ((varp->type.t_flags & (NAME_INHERITED|TYPE_MOD_PRIVATE)) == TYPE_MOD_PRIVATE)
                    warn_variable_usage(varp->name, GLOBAL_VARIABLE(vidx).usage, "Global");
            }
        }

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

    /* Remove the concrete struct definition from the lpctype object. */
    for (i = 0; (size_t)i < STRUCT_COUNT; i++)
        clean_struct_type(STRUCT_DEF(i).type->name->lpctype);

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
                free_lpctype(ARGUMENT_TYPE(i));
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
        size += align(num_function_headers * sizeof *prog->function_headers);

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
        if ( !(prog->name = new_unicode_mstring(current_loc.file->name)) )
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
                    | (pragma_warn_rtt_checks ? P_WARN_RTT_CHECKS : 0)
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

        /* Add the function headers right after the program code
         */
        prog->num_function_headers = num_function_headers;
        prog->function_headers = (A_FUNCTIONS_t*)p;

        f = GET_BLOCK(A_FUNCTIONS);
        fx = 0;
        for (i = 0; i< num_functions; i++, f++)
        {
            if ( !(f->flags & (NAME_INHERITED)))
            {
                function_t * head = ((A_FUNCTIONS_t*)p) + fx;

                *head = *f;
                head->offset.fx = i;
                /* We'll adopt the reference of the name. */

                *FUNCTION_HEADER_INDEXP(prog->program + (f->flags & FUNSTART_MASK)) = fx;

                fx++;
            }
        }

        p += align(num_function_headers * sizeof *prog->function_headers);

        /* Add the function names.
         */
        prog->num_function_names = num_function_names;
        prog->function_names = (unsigned short *)p;
        {
            unsigned short *namep;

            namep = (unsigned short *)p;
            if ( NULL != (f = funname_start1) || NULL != (f = funname_start2) )
            {
                do {
                    *namep++ = f - FUNCTION(0);
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

            f = FUNCTION(0);
            flagp = (funflag_t *)p;
            for (i = num_functions; --i >= 0; f++)
            {
                *flagp++ = f->flags;
            }
        }
        p += align(num_functions * sizeof *prog->functions);

        /* Add the program strings
         */
        prog->strings = (A_STRINGS_t *)p;
        prog->num_strings = num_strings;
        if (mem_block[A_STRINGS].current_size)
            memcpy(p, mem_block[A_STRINGS].block,
                   mem_block[A_STRINGS].current_size);

        p += align(mem_block[A_STRINGS].current_size);

        /* Add the variable descriptions
         */
        prog->variables = (A_VIRTUAL_VAR_t *)p;
        prog->num_variables = num_variables;
        prog->num_virtual_variables = num_virtual_variables;
        if (mem_block[A_VIRTUAL_VAR].current_size)
            memcpy(p, mem_block[A_VIRTUAL_VAR].block,
                   mem_block[A_VIRTUAL_VAR].current_size);

        p += align(mem_block[A_VIRTUAL_VAR].current_size);

        /* Add the inheritance information, and don't forget
         * to delete our internal flags.
         */
        prog->num_inherited = INHERIT_COUNT;
        if (prog->num_inherited)
        {
            memcpy(p, mem_block[A_INHERITS].block,
                   mem_block[A_INHERITS].current_size);
            prog->inherit = (A_INHERITS_t *)p;
        } else {
            prog->inherit = NULL;
        }
        p += align(mem_block[A_INHERITS].current_size);

        /* Index table for obsolete inherited programs. */
        prog->update_index_map = (A_UPDATE_INDEX_MAP_t *)p;
        if (mem_block[A_UPDATE_INDEX_MAP].current_size)
            memcpy(p, mem_block[A_UPDATE_INDEX_MAP].block,
                   mem_block[A_UPDATE_INDEX_MAP].current_size);

        p += align(mem_block[A_UPDATE_INDEX_MAP].current_size);

        /* Add the struct information.
         */
        prog->num_structs = STRUCT_COUNT;
        if (prog->num_structs)
        {
            memcpy(p, mem_block[A_STRUCT_DEFS].block,
                   mem_block[A_STRUCT_DEFS].current_size);
            prog->struct_defs = (A_STRUCT_DEFS_t *)p;
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
            prog->includes = (A_INCLUDES_t *)p;
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
            prog->argument_types = (A_ARGUMENT_TYPES_t *)p;
            prog->num_argument_types = ARGTYPE_COUNT;
            p += align(mem_block[A_ARGUMENT_TYPES].current_size);

            if (mem_block[A_ARGUMENT_INDEX].current_size)
                memcpy(p, mem_block[A_ARGUMENT_INDEX].block,
                       mem_block[A_ARGUMENT_INDEX].current_size);
            prog->type_start = (A_ARGUMENT_INDEX_t *)p;
            p += align(mem_block[A_ARGUMENT_INDEX].current_size);
        }
        else
        {
            prog->argument_types = NULL;
            prog->type_start = NULL;
            prog->num_argument_types = 0;

            for (i = 0; (size_t)i < ARGTYPE_COUNT; i++)
                free_lpctype(ARGUMENT_TYPE(i));
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

        f = FUNCTION(0);
        for (i = 0; i< num_functions; i++, f++)
            /* All other references were adopted by the
             * function headers in the program.. */
            if ( (f->flags & (NAME_INHERITED)))
                free_lpctype(f->type);

        for (i = 0; i < NUMAREAS; i++)
        {
            xfree(mem_block[i].block);
        }

        local_variables = NULL;
        context_variables = NULL;

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
        functions = FUNCTION(0);
        for (i = num_functions; --i >= 0; functions++)
        {
            if ( !(functions->flags & NAME_INHERITED) && functions->name )
            {
                /* The other references have been adopted. */
                free_mstring(functions->name);
            }
            free_lpctype(functions->type);
        }

        do_free_sub_strings( num_strings
                           , GET_BLOCK(A_STRINGS)
                           , num_variables
                           , GET_BLOCK(A_VIRTUAL_VAR)
                           , INCLUDE_COUNT
                           , GET_BLOCK(A_INCLUDES)
                           , STRUCT_COUNT
                           , GET_BLOCK(A_STRUCT_DEFS)
                           );

        /* Free the type information */
        for (i = 0; (size_t)i < ARGTYPE_COUNT; i++)
            free_lpctype(ARGUMENT_TYPE(i));

        compiled_prog = NULL;

        for (i = 0; i < NUMAREAS; i++)
        {
            xfree(mem_block[i].block);
        }

        local_variables = NULL;
        context_variables = NULL;
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
is_undef_function (bytecode_p fun)

/* Return TRUE if <fun> points to a referenced but undefined function.
 */

{
    return GET_CODE(fun) == F_UNDEF;
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
clear_compiler_refs (void)

/* GC support: clear the references of memory held by the compiler environment.
 */

{
    /* clear_lpctype_ref handles NULL pointers, so we don't need to check. */
    clear_lpctype_ref(lpctype_unknown_array);
    clear_lpctype_ref(lpctype_any_array);
    clear_lpctype_ref(lpctype_int_float);
    clear_lpctype_ref(lpctype_int_array);
    clear_lpctype_ref(lpctype_string_array);
    clear_lpctype_ref(lpctype_object_array);
    clear_lpctype_ref(lpctype_bytes_array);
    clear_lpctype_ref(lpctype_string_bytes);
    clear_lpctype_ref(lpctype_string_or_bytes_array);
    clear_lpctype_ref(lpctype_string_object);
    clear_lpctype_ref(lpctype_string_object_array);
}

void
count_compiler_refs (void)

/* GC support: mark the memory held by the compiler environment.
 */

{
    if (type_of_arguments.block)
    {
        note_malloced_block_ref(type_of_arguments.block);
    }

    count_lpctype_ref(lpctype_unknown_array);
    count_lpctype_ref(lpctype_any_array);
    count_lpctype_ref(lpctype_int_float);
    count_lpctype_ref(lpctype_int_array);
    count_lpctype_ref(lpctype_string_array);
    count_lpctype_ref(lpctype_object_array);
    count_lpctype_ref(lpctype_bytes_array);
    count_lpctype_ref(lpctype_string_bytes);
    count_lpctype_ref(lpctype_string_or_bytes_array);
    count_lpctype_ref(lpctype_string_object);
    count_lpctype_ref(lpctype_string_object_array);
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
