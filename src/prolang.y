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
 * keywords %if, %elif, %else and %endif; mainly to activate the proper
 * parsing rules for INITIALIZATION_BY___INIT.
 *---------------------------------------------------------------------------
 * A compile file, set its filename into <current_file>, open it to yield a
 * filedescriptor 'fd', then call
 *
 *     compile_file(fd);
 *
 * then close the file again. The compiled program is 'returned' in
 * the global compiled_prog - on error, this variable is returned as NULL.
 * If after the compilation the variable inherit_file is
 * not NULL, the compilation failed because it encountered an
 * "inherit 'name'" statement for which no object could be found: the
 * 'name' was stored in inherit_file and has to be compiled first.
 *
 * It is the task of the caller to make sure not to call the compiler
 * recursively.
 *
%ifdef INITIALIZATION_BY___INIT
 * If there is any initialization of a global variable, a function '__INIT'
 * is generated with the initialization code. The code is generated in
 * fragments whenever a variable initialization is encountered; the fragments
 * are therefore potentially spread over the whole program code. The fragments
 * are linked by JUMP instructions with jump to the next fragment, just
 * the last fragment ends in a RETURN0.
 *
 * When inheriting from another object, a call will automatically be made
 * to call __INIT in that code from the current __INIT.
%else
 * The variable initializers are returned as svalue_t[] in the global
 * variable prog_variable_values. It is the task of the call to free
 * that memory.
%endif
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
 *---------------------------------------------------------------------------
 */

#undef lint  /* undef so that precompiled headers can be used */

#include "driver.h"
#include "typedefs.h"

#include "my-alloca.h"
#include <stdio.h>
#include <stdarg.h>

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
#include "object.h"
#include "simulate.h"
#include "simul_efun.h"
#include "stdstrings.h"
#include "stralloc.h"
#include "svalue.h"
#include "swap.h"
#include "switch.h"
#include "wiz_list.h"
#include "xalloc.h"

#include "../mudlib/sys/driver_hook.h"

#define lint  /* redef again to prevent spurious warnings */

#define YYMAXDEPTH        600

/*-------------------------------------------------------------------------*/

typedef struct block_scope_s       block_scope_t;
typedef struct const_list_s        const_list_t;
typedef struct const_list_svalue_s const_list_svalue_t;
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

char *inherit_file;
  /* Used as a flag: if it is set to a string after yyparse(),
   * this string should be loaded as an object, and the original object
   * must be loaded again.
   */

int num_parse_error;
  /* Number of errors in the compile.
   */

%ifndef INITIALIZATION_BY___INIT
svalue_t *prog_variable_values;
  /* After epilog(), the variable initializers.
   */
%endif /* INITIALIZATION_BY___INIT */

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

#define A_PROGRAM                  0
   /* (bytecode_t): Program code.
    */
#define A_STRINGS                  1
   /* (shared char*) Strings used by the program.
    */
#define A_VARIABLES                2
   /* (variable_t) The information for all non-virtual variables.
    */
#define A_VIRTUAL_VAR              3
   /* (variable_t) The information for all virtual variables.
    */
#define A_LINENUMBERS              4
   /* (char) The linenumber information.
    */
#define A_INHERITS                 5
   /* (inherit_t) The information for the inherited programs.
    */
#define A_ARGUMENT_TYPES           6
   /* (vartype_t) Types of the arguments of all functions with
    * typechecking. The argument types for a specific function
    * can be found using the ARGUMENT_INDEX
    */
#define A_ARGUMENT_INDEX           7
   /* (unsigned short) Index of the first argument type of function <n>.
    * INDEX_START_NONE is used for functions with no type information.
    */

#define A_INCLUDES                 8
   /* (include_t) Tabled descriptors of all included files, in the order
    * of appearance.
    */

#define NUMPAREAS                  9  /* Number of saved areas */

#define A_FUNCTIONS                9
   /* (function_t): Function definitions
    */

%ifndef INITIALIZATION_BY___INIT
#    define A_VARIABLE_VALUES     10
       /* (svalue_t) Initializers for non-virtual variables.
        */
#    define A_VIRTUAL_VAR_VALUES  11
       /* (svalue_t) Initializers for virtual variables.
        */
%endif

#define A_STRING_NEXT             12
   /* (int) During compilation, the strings in A_STRINGS are organized
    * in a hash table (prog_string_indizes/_tags). The hash chains are
    * linked together using the indizes in this area. The end of
    * a chain is marked by a negative next-index.
    */

#define NUMAREAS                  13  /* Total number of areas */


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

%ifndef INITIALIZATION_BY___INIT

#define V_VAR_VALUE(n)  ((svalue_t *)mem_block[A_VARIABLE_VALUES].block + \
                        (n) - VIRTUAL_VAR_TAG)
  /* Return the svalue_t* for the initializer of virtual variable <n>
   * (still including the offset).
   */

#define NV_VAR_VALUE(n)  ((svalue_t *)mem_block[A_VARIABLE_VALUES].block + (n))
  /* Return the svalue_t* for the initializer of non-virtual variable <n>.
   */

%endif

#define INHERIT(n)     ((inherit_t *)mem_block[A_INHERITS].block)[n]
  /* Index the inherit_t <n>.
   */

#define INHERIT_COUNT  (mem_block[A_INHERITS].current_size / sizeof(inherit_t))
  /* Return the number of inherits encountered so far.
   */

#define PROG_STRING(n) ((char **)mem_block[A_STRINGS].block)[n]
  /* Index the pointer for program string <n>.
   */

#define STRING_COUNT  (mem_block[A_STRINGS].current_size / sizeof(char *))
  /* Return the number of program strings encountered so far.
   */

#define PROG_STRING_NEXT(n) ((int *)mem_block[A_STRING_NEXT].block)[n]
  /* Index the chain-index for program string <n>.
   */

#define INCLUDE_COUNT  (mem_block[A_INCLUDES].current_size / sizeof(include_t))
  /* Return the total number of include files encountered so far.
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

static Bool use_local_scopes;
  /* Copy of pragma_use_local_scopes, updated at every entry into
   * a function. Reason is that the pragma must not change inside
   * a function.
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

%ifdef INITIALIZATION_BY___INIT

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

%else

static svalue_t *currently_initialized;
  /* The variable for which currently the initializer is compiled.
   */

%endif /* INITIALIZATION_BY___INIT */

static mem_block_t type_of_arguments;
  /* The vartypes of arguments when calling functions must be saved,
   * to be used afterwards for checking. And because function calls
   * can be done as an argument to a function calls, a stack of argument types
   * is needed. This stack does not need to be freed between compilations,
   * but will be reused.
   */

static vartype_t type_of_locals[MAX_LOCAL];
  /* The short type (ie: just the type, no visibility information) of
   * the local variables.
   */

static fulltype_t full_type_of_locals[MAX_LOCAL];
  /* The full types of the local variables.
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

       fulltype_t exact_types;
  /* If 0, don't check nor require argument and function types.
   * Otherwise it's the full return type of the function, including
   * visibility. The lexer reads this variable when scanning an
   * inline closure.
   */

static fulltype_t default_varmod;
static fulltype_t default_funmod;
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

static p_int current_break_address;
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

static p_int current_continue_address;
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

static fulltype_t current_type;
  /* The current basic type.
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

static char *last_string_constant = NULL;
  /* The current (last) string constant, a shared string.
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


/*-------------------------------------------------------------------------*/
/* Forward declarations */

struct lvalue_s; /* Defined within YYSTYPE aka %union */
static Bool add_lvalue_code ( struct lvalue_s * lv, int instruction);
static void insert_pop_value(void);
static void arrange_protected_lvalue(p_int, int, p_int, int);
static int insert_inherited(char *,char *, program_t **, function_t *, int, bytecode_p);
  /* Returnvalues from insert_inherited(): */
#  define INHERITED_NOT_FOUND            (-1)
#  define INHERITED_WILDCARDED_ARGS      (-2)
#  define INHERITED_WILDCARDED_NOT_FOUND (-3)
static void store_line_number_relocation(int relocated_from);
int yyparse(void);
%ifdef INITIALIZATION_BY___INIT
static void add_new_init_jump(void);
static void transfer_init_control(void);
static void copy_variables(program_t *, fulltype_t);
static int copy_functions(program_t *, fulltype_t type);
%else
static void copy_variables(program_t *, fulltype_t, svalue_t *);
static void copy_functions(program_t *, fulltype_t type);
%endif
static void fix_function_inherit_indices(program_t *);
static void fix_variable_index_offsets(program_t *);

/*-------------------------------------------------------------------------*/
void
yyerror (char *str)

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
                  , time_stamp(), current_file, current_line, str, context);
    /* TODO: lex should implement a function get_include_stack() which
     * TODO:: returns an svalue-array with the current include stack.
     * TODO:: This could be printed, and also passed to parse_error().
     */
    fflush(stderr);
    parse_error(MY_FALSE, current_file, current_line, str, context);
    if (num_parse_error == 0)
        save_error(str, current_file, current_line);
    num_parse_error++;
} /* yyerror() */

/*-------------------------------------------------------------------------*/
void
yyerrorf (char *format, ...)

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
yywarn (char *str)

/* Raise the parse warning <str>: usually generate the warning message and
 * log it.
 */

{
    char *context;

    context = lex_error_context();
    fprintf(stderr, "%s %s line %d: Warning: %s%s\n"
                  , time_stamp(), current_file, current_line, str, context);
    /* TODO: lex should implement a function get_include_stack() which
     * TODO:: returns an svalue-array with the current include stack.
     * TODO:: This could be printed, and also passed to parse_error().
     */
    fflush(stderr);
    parse_error(MY_TRUE, current_file, current_line, str, context);
    if (num_parse_error == 0)
        save_error(str, current_file, current_line);
} /* yywarn() */

/*-------------------------------------------------------------------------*/
void
yywarnf (char *format, ...)

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
    size_t len1;
    char *tmp;

    len1 = strlen(last_string_constant);
    tmp = alloca(len1 + strlen(last_lex_string) + 1);
    strcpy(tmp, last_string_constant);
    strcpy(tmp + len1, last_lex_string);
    free_string(last_string_constant);
    free_string(last_lex_string);
    last_string_constant = make_shared_string(tmp);
    last_lex_string = NULL;
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
    } while (size > max_size);

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
#define BASIC_TYPE(e,t) \
    ((e) == TYPE_ANY || (e) == (t) || (t) == TYPE_ANY)

  /* Return TRUE if <e> and <t> are compatible basic types.
   */

#define TYPE(e,t) \
    (   BASIC_TYPE((e) & TYPE_MOD_MASK, (t) & TYPE_MOD_MASK) \
     || (   ((e) & TYPE_MOD_POINTER) && ((t) & TYPE_MOD_POINTER) \
         && BASIC_TYPE((e) & (TYPE_MOD_MASK & ~TYPE_MOD_POINTER),\
                               (t) & (TYPE_MOD_MASK & ~TYPE_MOD_POINTER))\
        ))

  /* Return TRUE if <e> and <t> are compatible basic xor pointer types.
   */

#define MASKED_TYPE(e,t) \
    (   BASIC_TYPE( (e) , (t) ) \
     || ( (e) == (TYPE_MOD_POINTER|TYPE_ANY) && (t) & TYPE_MOD_POINTER ) \
     || ( (t) == (TYPE_MOD_POINTER|TYPE_ANY) && (e) & TYPE_MOD_POINTER ) \
    )

  /* Return TRUE if <e> and <t> are compatible basic types, or if both
   * are pointer types and one of them is a *ANY.
   */

#define REDEFINED_TYPE(e,t) \
    (   BASIC_TYPE( (e), (t) ) \
     || ( (t) == (TYPE_MOD_POINTER|TYPE_ANY) ) \
     || ( (e) == (TYPE_MOD_POINTER|TYPE_ANY) ) \
    )

  /* Return TRUE if type <t> is a proper redefinition of <e>.
   * This is the case if <e> and <t> are compatible base types,
   * or if one of them is *ANY.
   */

/*-------------------------------------------------------------------------*/
static char *
get_visibility (fulltype_t type)

/* Return (in a static buffer) a textual representation of the visibility
 * portion of <type>.
 */

{
    static char buff[100];
    size_t len;

    buff[0] = '\0';
    if (type & TYPE_MOD_STATIC)
        strcat(buff, "static ");
    if (type & TYPE_MOD_NO_MASK)
        strcat(buff, "nomask ");
    if (type & TYPE_MOD_PRIVATE)
        strcat(buff, "private ");
    if (type & TYPE_MOD_PROTECTED)
        strcat(buff, "protected ");
    if (type & TYPE_MOD_PUBLIC)
        strcat(buff, "public ");
    if (type & TYPE_MOD_VARARGS)
        strcat(buff, "varargs ");

    len = strlen(buff);
    if (len)
        buff[len-1] = '\0';

    return buff;
} /* get_visibility() */

/*-------------------------------------------------------------------------*/
static char *
get_type_name (fulltype_t type)

/* Return (in a static buffer) a textual representation of <type>.
 */

{
    static char buff[100];
    static char *type_name[] = { "unknown", "int", "string", "void", "object",
                                 "mapping", "float", "mixed", "closure",
                                 "symbol", "quoted_array", };

    Bool pointer = MY_FALSE, reference = MY_FALSE;

    buff[0] = '\0';
    if (type & TYPE_MOD_STATIC)
        strcat(buff, "static ");
    if (type & TYPE_MOD_NO_MASK)
        strcat(buff, "nomask ");
    if (type & TYPE_MOD_PRIVATE)
        strcat(buff, "private ");
    if (type & TYPE_MOD_PROTECTED)
        strcat(buff, "protected ");
    if (type & TYPE_MOD_PUBLIC)
        strcat(buff, "public ");
    if (type & TYPE_MOD_VARARGS)
        strcat(buff, "varargs ");

    type &= TYPE_MOD_MASK;

    if (type & TYPE_MOD_POINTER)
    {
        pointer = MY_TRUE;
        type &= ~TYPE_MOD_POINTER;
    }
    if (type & TYPE_MOD_REFERENCE)
    {
        reference = MY_TRUE;
        type &= ~TYPE_MOD_REFERENCE;
    }

    if (type >= sizeof type_name / sizeof type_name[0])
        fatal("Bad type %ld\n", (long)type);

    strcat(buff, type_name[type]);
    strcat(buff," ");
    if (pointer)
        strcat(buff, "* ");
    if (reference)
        strcat(buff, "& ");

    return buff;
} /* get_type_name() */

/*-------------------------------------------------------------------------*/
static char *
get_two_types (fulltype_t type1, fulltype_t type2)

/* Return (in a static buffer) the text "(<type1> vs. <type2>)".
 */
{
    static char buff[100];

    strcpy(buff, "( ");
    strcat(buff, get_type_name(type1));
    strcat(buff, "vs ");
    strcat(buff, get_type_name(type2));
    strcat(buff, ")");
    return buff;
} /* get_two_types() */

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
static Bool
compatible_types (fulltype_t t1, fulltype_t t2)

/* Compare the two types <t1> and <t2> and return TRUE if they are compatible.
 * Rules:
 *   - every type is compatible to itself
 *   - TYPE_UNKNOWN is incompatible to everything
 *   - TYPE_ANY is compatible to everything
 *   - two POINTER types are compatible if at least one is *TYPE_ANY.
 */

{
    if (t1 == TYPE_UNKNOWN || t2 == TYPE_UNKNOWN)
        return MY_FALSE;
    if (t1 == t2)
        return MY_TRUE;
    if (t1 == TYPE_ANY || t2 == TYPE_ANY)
        return MY_TRUE;
    if ((t1 & TYPE_MOD_POINTER) && (t2 & TYPE_MOD_POINTER))
    {
        if ((t1 & TYPE_MOD_MASK) == (TYPE_ANY|TYPE_MOD_POINTER)
         || (t2 & TYPE_MOD_MASK) == (TYPE_ANY|TYPE_MOD_POINTER))
            return MY_TRUE;
    }
    return MY_FALSE;
} /* compatible_types() */

/*-------------------------------------------------------------------------*/
static INLINE void
add_arg_type (vartype_t type)

/* Add another function argument type to the argument type stack.
 */

{
    mem_block_t *mbp = &type_of_arguments;

    if (mbp->current_size + sizeof type > mbp->max_size)
    {
        mbp->max_size *= 2;
        mbp->block = rexalloc((char *)mbp->block, mbp->max_size);
    }
    *(vartype_t*)(mbp->block + mbp->current_size) = type;
    mbp->current_size += sizeof type;
} /* add_arg_type() */

/*-------------------------------------------------------------------------*/
static INLINE void
pop_arg_stack (int n)

/* Pop (remove) the last <n> types from the argument stack.
 */

{
    type_of_arguments.current_size -= sizeof (vartype_t) * n;
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
    vartype_t *argp, mask;

    argp = (vartype_t *) (type_of_arguments.block +
          (type_of_arguments.current_size -= sizeof (vartype_t) * n) );

    /* We're just interested in TYPE_MOD_REFERENCE, so we preset all
     * other bits with 1.
     */
    for (mask = ~TYPE_MOD_REFERENCE; --n >= 0; )
    {
        mask |= *argp++;
    }

    if (!(~mask & 0xffff))
        yyerror("Can't trace reference assignments.");
} /* check_aggregate_types() */


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
ins_byte (unsigned char b)

/* Add the byte <b> to the A_PROGRAM area.
 */

{
    if (mem_block[A_PROGRAM].current_size == mem_block[A_PROGRAM].max_size ) {
        if (!realloc_a_program(1))
        {
            yyerrorf("Out of memory: program size %lu\n"
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
    if (b > 0x100)
        ins_byte(b >> F_ESCAPE_BITS);
    ins_byte(b);
} /* ins_f_code() */

/*-------------------------------------------------------------------------*/
static void
ins_short (long l)

/* Add the 2-byte number <l> to the A_PROGRAM area in a fixed byteorder.
 */

{
    short s = (short)l;

    if (l > (long)USHRT_MAX || l < SHRT_MIN)
        yyerrorf("Compiler error: too large number %lx passed to ins_short()"
                , l);

    if (realloc_a_program(2))
    {
        mp_uint current_size;
        char *dest;

        current_size = CURRENT_PROGRAM_SIZE;
        CURRENT_PROGRAM_SIZE = current_size + 2;
        dest = mem_block[A_PROGRAM].block + current_size;
        PUT_SHORT(dest, s);
    }
    else
    {
        yyerrorf("Out of memory: program size %lu\n"
                , mem_block[A_PROGRAM].current_size + 2);
    }
} /* ins_short() */

/*-------------------------------------------------------------------------*/
static void
upd_short (mp_uint offset, long l)

/* Store the 2-byte number <l> at <offset> in the A_PROGRAM are in
 * a fixed byteorder.
 */

{
    char *dest;
    short s = (short)l;

    if (l > (long)USHRT_MAX || l < SHRT_MIN)
        yyerrorf("Compiler error: too large number %ld passed to upd_short()"
                , l);

    dest = mem_block[A_PROGRAM].block + offset;
    PUT_SHORT(dest, s);
} /* upd_short() */

/*-------------------------------------------------------------------------*/
static short
read_short (mp_uint offset)

/* Return the 2-byte number stored at <offset> in the A_PROGRAM area.
 */

{
    short l;
    char *dest;

    dest = mem_block[A_PROGRAM].block + offset;
    GET_SHORT(l, dest);
    return l;
} /* read_short() */

/*-------------------------------------------------------------------------*/
static void
ins_long (int32 l)

/* Add the 4-byte number <l> to the A_PROGRAM area in a fixed byteorder.
 */

{
    if (realloc_a_program(4))
    {
        mp_uint current_size;
        char *dest;

        current_size = CURRENT_PROGRAM_SIZE;
        CURRENT_PROGRAM_SIZE = current_size + 4;
        dest = mem_block[A_PROGRAM].block + current_size;
        PUT_INT32(dest, l);
    }
    else
    {
        yyerrorf("Out of memory: program size %lu\n"
                , mem_block[A_PROGRAM].current_size + 4);
    }
} /* ins_long() */

/*-------------------------------------------------------------------------*/
static void
upd_long (mp_uint offset, int32 l)

/* Store the 4-byte number <l> at <offset> in the A_PROGRAM are in
 * a fixed byteorder.
 */

{
    char *dest;

    dest = mem_block[A_PROGRAM].block + offset;
    PUT_LONG(dest, l);
} /* upd_long() */

/*-------------------------------------------------------------------------*/
static int32
read_long (mp_uint offset)

/* Return the 4-byte number stored at <offset> in the A_PROGRAM area.
 */

{
    long l;
    char *dest;

    dest = mem_block[A_PROGRAM].block + offset;
    GET_LONG(l, dest);
    return l;
} /* read_long() */

%ifdef INITIALIZATION_BY___INIT
/*-------------------------------------------------------------------------*/
static void
upd_offset (mp_uint offset, long l)

/* Store the 3-byte number <l> at <offset> in the A_PROGRAM are in
 * a fixed byteorder.
 */

{
    char *dest;

    dest = mem_block[A_PROGRAM].block + offset;
    STORE_UINT8(dest, l>>16);
    PUT_SHORT(dest, l & 0xffff);
} /* upd_offset() */

%endif /* INITIALIZATION_BY___INIT */

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
 *    add_byte(b):   to add byte <b> to the program
 *    add_short(s):  to add short <s> to the program
 */

#define PREPARE_INSERT(n) \
    bytecode_p __PREPARE_INSERT__p = (\
      realloc_a_program(n) ? (PROGRAM_BLOCK + CURRENT_PROGRAM_SIZE) : NULL);

#define add_byte(b)   (void) STORE_INT8(__PREPARE_INSERT__p, (b))

#define add_short(s) STORE_SHORT(__PREPARE_INSERT__p, (s))

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
        p_int i, j;
        bytecode_p p;

        mem_block[A_PROGRAM].block[loc] = 0; /* Init it */

        /* Update the break address */
        if ( current_break_address > loc
         && !(current_break_address & (BREAK_ON_STACK|BREAK_DELIMITER) ) )
        {
            for (i = current_break_address & BREAK_ADDRESS_MASK
                ; (j = read_long(i)) > loc; )
            {
                upd_long(i, j+1);
                i = j;
            }
            current_break_address++;
        }

        /* Update the continue address */
        if ( (current_continue_address & CONTINUE_ADDRESS_MASK) > loc
         && !(current_continue_address & CONTINUE_DELIMITER ) )
        {
            for(i = current_continue_address & CONTINUE_ADDRESS_MASK;
              (j=read_long(i)) > loc; )
            {
                upd_long(i, j+1);
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
            yyerrorf("Compiler limit: Too much code to branch over: %ld bytes"
                    , offset);

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
    yyerrorf("Out of memory: program size %lu\n"
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
    mp_int i, j;

    if (realloc_a_program(len))
    {
        CURRENT_PROGRAM_SIZE += len;

        /* Adjust the continue address, if any */
        if ( (current_continue_address & CONTINUE_ADDRESS_MASK) > switch_pc
         && !(current_continue_address & CONTINUE_DELIMITER ) )
        {
            for(i = current_continue_address & CONTINUE_ADDRESS_MASK;
              (j=read_long(i)) > switch_pc; )
            {
                    upd_long(i, j+len);
                    i = j;
            }
            current_continue_address += len;
        }

        move_memory(
          mem_block[A_PROGRAM].block + switch_pc + len,
          mem_block[A_PROGRAM].block + switch_pc,
          blocklen
        );
    }
    else
    {
        yyerrorf("Out of memory: program size %lu\n"
                , mem_block[A_PROGRAM].current_size + len);
    }
} /* yymove_switch_instructions() */

/*-------------------------------------------------------------------------*/
static void
yycerrorl (char *s1, char *s2, int line1, int line2)

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
                     " %ld bytes" , offset);
        p[0]  = F_POP_VALUE;
    }
    else
    {
        mem_block[A_PROGRAM].block[address] = offset;
    }
} /* update_lop_branch() */

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
    }
} /* free_local_names() */

/*-------------------------------------------------------------------------*/
static ident_t *
add_local_name (ident_t *ident, fulltype_t type, int depth)

/* Declare a new local variable <ident> with the type <type> on
 * the scope depth <depth>.
 * Return the (adjusted) ident for the new variable.
 */

{
    if ((type & PRIMARY_TYPE_MASK) == TYPE_VOID)
    {
        yyerrorf( "Illegal to define variable '%s' as type 'void'"
                , ident->name);
    }

    if (current_number_of_locals >= MAX_LOCAL
     || current_number_of_locals >= 256)
        yyerror("Too many local variables");

    else
    {
        if (ident->type != I_TYPE_UNKNOWN)
        {
            /* We're overlaying some other definition */
            ident = make_shared_identifier(ident->name, I_TYPE_LOCAL, depth);
        }

        /* Initialize the ident */
        ident->type = I_TYPE_LOCAL;
        ident->u.local.num = current_number_of_locals;
        ident->u.local.depth = depth;

        /* Put the ident into the list of all locals */
        if (all_locals && all_locals->u.local.depth > depth)
            fatal("List of locals clobbered: depth %d, adding depth %d\n"
                 , all_locals->u.local.depth, depth);
        ident->next_all = all_locals;
        all_locals = ident;

        /* Record the type */
        type_of_locals[current_number_of_locals] = type;
        full_type_of_locals[current_number_of_locals++] = type;

        /* And update the scope information */
        if (current_number_of_locals > max_number_of_locals)
            max_number_of_locals = current_number_of_locals;
        block_scope[depth-1].num_locals++;
    }

    return ident;
} /* add_local_name() */

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
} /* init_scope() */

/*-------------------------------------------------------------------------*/
static void
enter_block_scope (void)

/* Enter a new scope and initialize it (if use_local_scopes requires it).
 */

{
    if (block_depth == COMPILER_STACK_SIZE)
        yyerror("Too deep nesting of local blocks.\n");

    if (use_local_scopes)
    {
        block_depth++;
        init_scope(block_depth);
    }
} /* enter_block_scope() */

/*-------------------------------------------------------------------------*/
static void
leave_block_scope (Bool dontclobber)

/* Leave the current scope (if use_local_scopes requires it), freeing
 * all local names defined in that scope.
 *
 * <dontclobber> should be MY_TRUE if the stack of the to-be-left scope
 * is independent of the outer scope (i.e. the scope of closures).
 */

{
    if (use_local_scopes)
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
    }
} /* leave_block_scope() */

/*-------------------------------------------------------------------------*/
static ident_t *
lookup_local (int num)

/* Lookup the ident_t structure for local variable <num>.
 */

{
    ident_t *p, *q;

    /* First, find the declaration of this local */
    q = NULL;
    for (p = all_locals; p != NULL; p = p->next_all)
    {
        if (p->u.local.num == num)
        {
            q = p;
            break;
        }
    }

    /* q should be set here and point to the previous declaration.
     */
    if (!q)
        fatal("Local identifier %ld not found in list.\n", (long)num);

    return q;
} /* lookup_local() */

/*-------------------------------------------------------------------------*/
static ident_t *
redeclare_local (int num, fulltype_t type, int depth)

/* Redeclare a local name, identified by <num>, to <type> at <depth>.
 * If this happens on a deeper level, it is legal: the new declaration
 * is added and the new identifier is returned.
 * If it is illegal, an yyerror() is risen and the ident of the older
 * declaration is returned for error recovery.
 */

{
    ident_t *p, *q;

    if (all_locals && all_locals->u.local.depth > depth)
    {
        fatal("List of locals clobbered: list depth %d, "
              "block depth %d\n"
              , all_locals->u.local.depth, depth);
    }

    /* First, find the previous declaration of this local */
    q = NULL;
    for (p = all_locals; p != NULL; p = p->next_all)
    {
        if (p->u.local.num == num)
        {
            /* We found the identifier, and due to the list properties
             * it is also the one with deepest depth.
             */
            q = p;
            break;
        }
    }

    /* q should be set here and point to the previous declaration.
     * If it is of lower depth, it may be shadowed. However, it
     * is not possible to shadow an argument (depth 1) with
     * a function-local variable (depth 2).
     */
    if (!q)
        fatal("Local identifier %ld not found in list.\n", (long)num);

    if (q->u.local.depth >= depth
     || (q->u.local.depth == 1 && depth == 2)
       )
    {
        yyerrorf("Illegal to redeclare local name '%s'", q->name);
    }
    else
    {
        /* TODO: Add a warning for shadowed variable */
        q = add_local_name(q, type, depth);
    }

    return q;
} /* redeclare_local() */


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
    if (!exact_types)
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
            add_to_mem_block(A_ARGUMENT_TYPES, &type_of_locals[i],
                             sizeof type_of_locals[i]);
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

    flags |= type & ~TYPE_MOD_MASK; /* Move the visibility-info into flags */

    do {
        function_t *funp;
        Bool args_differ, compare_args;

        if (p->type != I_TYPE_GLOBAL) break;
        if ((num = p->u.global.function) < 0) break;

        funp = FUNCTION(num);

        if ((funp->flags & (NAME_INHERITED|TYPE_MOD_PRIVATE))
         == (NAME_INHERITED|TYPE_MOD_PRIVATE))
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
                yyerrorf("Illegal to redefine 'nomask' function \"%s\"", p->name);

            if (!(funp->flags & (NAME_UNDEFINED|NAME_PROTOTYPE|NAME_INHERITED) ) )
            {
                yyerrorf("Redeclaration of function %s.", p->name);
                if ( !(flags & NAME_PROTOTYPE) )
                    free_string(p->name);
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
            if (exact_types && funp->type != TYPE_UNKNOWN)
            {
                fulltype_t t1, t2;

                if (funp->num_arg > num_arg && !(funp->flags & TYPE_MOD_VARARGS))
                    yyerrorf("Incorrect number of arguments in redefinition of '%s'.", p->name);
                else if (funp->num_arg == num_arg
                      && ((funp->flags ^ flags) & TYPE_MOD_XVARARGS)
                      && !(funp->flags & TYPE_MOD_VARARGS))
                    yyerrorf("Incorrect number of arguments in redefinition of '%s'.", p->name);
                else
                {
                    unsigned short first_arg;

                    first_arg = ARGUMENT_INDEX(num);
                    if (first_arg == INDEX_START_NONE)
                    {
                        if (num_arg && !(funp->flags & NAME_TYPES_LOST) )
                            yyerrorf(
                              "Redefined function '%s' not compiled with type testing."
                                    , p->name
                                    );
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
                    fulltype_t f1 = funp->flags;
                    fulltype_t f2 = flags;

                    /* Smooth out irrelevant differences */
                    if (f1 & TYPE_MOD_STATIC) f1 |= TYPE_MOD_PROTECTED;
                    if (f2 & TYPE_MOD_STATIC) f2 |= TYPE_MOD_PROTECTED;

                    if (!(f1 & (NAME_INHERITED|NAME_TYPES_LOST))
                     && ((f1 ^ f2) & TYPE_MOD_VIS)
                       )
                    {
                        char buff[100];

                        strcpy(buff, get_visibility(funp->flags));
                        yywarnf("Inconsistent declaration of '%s': Visibility changed from '%s' to '%s'"
                               , p->name, buff, get_visibility(flags));
                    }
#                   undef TYPE_MOD_VIS
                }

                /* Check if the 'varargs' attribute is conserved */

                t1 = type & TYPE_MOD_MASK;
                t2 = funp->type & TYPE_MOD_MASK;
                if (!MASKED_TYPE(t1, t2))
                {
                    if (pragma_pedantic)
                        yyerrorf("Inconsistent declaration of '%s': Return type mismatch %s", p->name, get_two_types(t2, t1));
                    else if (pragma_check_overloads)
                        yywarnf("Inconsistent declaration of '%s': Return type mismatch %s", p->name, get_two_types(t2, t1));
                }

                if (pragma_pedantic
                 && (funp->flags ^ flags) & TYPE_MOD_VARARGS
                 &&  funp->flags & TYPE_MOD_VARARGS
                   )
                {
                    yywarnf("Redefinition of '%s' loses 'varargs' modifier."
                           , p->name);
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
                        t1 = type_of_locals[i] & TYPE_MOD_RMASK;
                        t2 = argp[i] & TYPE_MOD_MASK;
                        if (!MASKED_TYPE(t1, t2))
                        {
                            args_differ = MY_TRUE;
                            if (pragma_pedantic)
                                yyerrorf("Argument type mismatch in "
                                         "redefinition of '%s': arg %d %s"
                                        , p->name, i+1, get_two_types(t1, t2)
                                        );
                            else if (pragma_check_overloads)
                                yywarnf("Argument type mismatch in "
                                         "redefinition of '%s': arg %d %s"
                                        , p->name, i+1, get_two_types(t1, t2)
                                        );
                        }
                    } /* for (all args) */

                } /* if (compare_args) */

            } /* if (exact_types && already defined) */

         } /* if (!complete) */

        if (strcmp(p->name, "heart_beat") == 0)
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
        funp->type = type;

        /* That's it */
        return num;

    } while(0); /* Test and handle for already defined functions */

    /* It's a new function! */

    if (strcmp(p->name, "heart_beat") == 0)
        heart_beat = FUNCTION_COUNT;

    /* Fill in the function_t */
    fun.name      = p->name; /* adopt the ref */
    fun.offset.pc = offset;
    fun.flags     = flags;
    fun.num_arg   = num_arg;
    fun.num_local = num_local; /* will be updated later */
    fun.type      = type;

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
            p = make_shared_identifier(p->name, I_TYPE_GLOBAL, 0);
        }
        /* should be I_TYPE_UNKNOWN now. */

        p->type = I_TYPE_GLOBAL;
        p->u.global.variable = I_GLOBAL_VARIABLE_OTHER;
        p->u.global.efun     = I_GLOBAL_EFUN_OTHER;
        p->u.global.sim_efun = I_GLOBAL_SEFUN_OTHER;

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
%ifdef INITIALIZATION_BY___INIT

static void
define_variable (ident_t *name, fulltype_t flags)

%else /* then !INITIALIZATION_BY___INIT */

static void
define_variable (ident_t *name, fulltype_t flags, svalue_t *svp)

%endif /* INITIALIZATION_BY___INIT */

/* Define a new global variable <name> of type <flags>.
 * If !INITIALIZATION_BY___INIT, then <svp> is the initializer for the
 * variable.
 */

{
    variable_t dummy;
    int n;

    if ((flags & PRIMARY_TYPE_MASK) == TYPE_VOID)
    {
        yyerrorf( "Illegal to define variable '%s' as type 'void'"
                , name->name);
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
            name = make_shared_identifier(name->name, I_TYPE_GLOBAL, 0);
        }

        name->type = I_TYPE_GLOBAL;
        name->u.global.function = I_GLOBAL_FUNCTION_VAR;
        name->u.global.variable = I_GLOBAL_VARIABLE_OTHER; /* mark it as 'yet undef' for now */
        name->u.global.efun     = I_GLOBAL_EFUN_OTHER;
        name->u.global.sim_efun = I_GLOBAL_SEFUN_OTHER;

        name->next_all = all_globals;
        all_globals = name;
    }
    else if (name->u.global.function == I_GLOBAL_FUNCTION_EFUN)
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
        /* Visible nomask variables can't be redefined */
        if ( VARIABLE(n)->flags & TYPE_MOD_NO_MASK && !(flags & NAME_HIDDEN))
            yyerrorf( "Illegal to redefine 'nomask' variable '%s'"
                    , name->name);

        /* We can redefine inherited variables if they are private or hidden,
         * or if at least one of them is static.
         */
        if (  (   !(VARIABLE(n)->flags & NAME_INHERITED)
               || (   !(VARIABLE(n)->flags & (TYPE_MOD_PRIVATE|NAME_HIDDEN))
                   && !((flags | VARIABLE(n)->flags) & TYPE_MOD_STATIC)
                  )
              )
            && !(flags & NAME_INHERITED)
           )
        {
            if (VARIABLE(n)->flags & NAME_INHERITED)
                yyerrorf("Illegal to redefine inherited variable '%s'", name->name);
            else
                yyerrorf("Illegal to redefine global variable '%s'", name->name);
        }

        if (((flags | VARIABLE(n)->flags) & (TYPE_MOD_STATIC|TYPE_MOD_PRIVATE))
            == TYPE_MOD_STATIC
         && !(flags & NAME_INHERITED)
           )
        {
            yywarnf("Redefining inherited %s variable '%s' with a %s variable"
                   , (VARIABLE(n)->flags & TYPE_MOD_STATIC)
                     ? "nosave" : "non-nosave"
                   , name->name
                   , (flags & TYPE_MOD_STATIC) ? "nosave" : "non-nosave"
                   );
        }

        /* Make sure that at least one of the two definitions is 'static'.
         * The variable which has not been inherited gets first pick.
         */
        if (flags & NAME_INHERITED)
        {
            flags |= ~(VARIABLE(n)->flags) & TYPE_MOD_STATIC;
        }
        else
        {
            VARIABLE(n)->flags |=   ~flags & TYPE_MOD_STATIC;
        }
    }

    dummy.name = ref_string(name->name);
    dummy.flags = flags;

    if (flags & TYPE_MOD_VIRTUAL)
    {
        if (!(flags & NAME_HIDDEN))
            name->u.global.variable = VIRTUAL_VAR_TAG | V_VARIABLE_COUNT;
        add_to_mem_block(A_VIRTUAL_VAR, &dummy, sizeof dummy);
%ifndef INITIALIZATION_BY___INIT
        add_to_mem_block(A_VIRTUAL_VAR_VALUES, svp, sizeof *svp);
%endif /* INITIALIZATION_BY___INIT */
    }
    else
    {
        if (!(flags & NAME_HIDDEN))
            name->u.global.variable = NV_VARIABLE_COUNT;
        add_to_mem_block(A_VARIABLES, &dummy, sizeof dummy);
%ifndef INITIALIZATION_BY___INIT
        add_to_mem_block(A_VARIABLE_VALUES, svp, sizeof *svp);
%endif /* INITIALIZATION_BY___INIT */
    }
} /* define_variable() */

/*-------------------------------------------------------------------------*/
static void
redeclare_variable (ident_t *name, fulltype_t flags, int n)

/* The variable <name> is inherited virtually with number <n>.
 * Redeclare it from its original type to <flags>.
 */

{
    if (name->type != I_TYPE_GLOBAL)
    {
        /* This is the first _GLOBAL use of this identifier:
         * make an appropriate entry in the identifier table.
         */

        /* I_TYPE_UNKNOWN */
        name->type = I_TYPE_GLOBAL;
        name->u.global.function = I_GLOBAL_FUNCTION_VAR;
        name->u.global.variable = I_GLOBAL_VARIABLE_OTHER; /* default: it's hidden */
        name->u.global.efun     = I_GLOBAL_EFUN_OTHER;
        name->u.global.sim_efun = I_GLOBAL_SEFUN_OTHER;

        name->next_all = all_globals;
        all_globals = name;
    }
    else if (name->u.global.function == I_GLOBAL_FUNCTION_EFUN)
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
        if (VARIABLE(name->u.global.variable)->flags & TYPE_MOD_NO_MASK )
            yyerrorf( "Illegal to redefine 'nomask' variable '%s'"
                    , name->name);
    }
    else if (V_VARIABLE(n)->flags & TYPE_MOD_NO_MASK
          && !(V_VARIABLE(n)->flags & NAME_HIDDEN)
          && (V_VARIABLE(n)->flags ^ flags) & TYPE_MOD_STATIC )
    {
        yyerrorf("Illegal to redefine 'nomask' variable \"%s\"", name->name);
    }

    if (flags & TYPE_MOD_NOSAVE)
    {
        /* 'nosave' is internally saved as 'static' (historical reason) */
        flags |= TYPE_MOD_STATIC;
        flags ^= TYPE_MOD_NOSAVE;
    }

    name->u.global.variable = n;
    V_VARIABLE(n)->flags = flags;
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
        yyerrorf("Variable %s not declared !", p->name);
        return -1;
    }

    return r;
} /* verify_declared() */


/* =========================   PROGRAM STRINGS   ========================= */

/*-------------------------------------------------------------------------*/
static short
store_prog_string (char *str)

/* Add the shared string <str> to the strings used by the program.
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
                free_string(str); /* Drop the extra ref. */
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
    if (str_size + sizeof(char *) > mem_block[A_STRINGS].max_size
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
    mem_block[A_STRINGS].current_size = str_size + sizeof(char *);
    *((char **)(mem_block[A_STRINGS].block+str_size)) = str;

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
    char *str;
    int size;
    long hash;
    char mask, *tagp;
    int *indexp;

    /* Remove the string from the A_STRINGS area and free it */
    size = mem_block[A_STRINGS].current_size - sizeof(char *);
    free_string(
      str = *(char**)(mem_block[A_STRINGS].block+size)
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


/* ==========================   INITIALIZATION   ========================== */

%ifndef INITIALIZATION_BY___INIT

/*-------------------------------------------------------------------------*/
static INLINE fulltype_t
type_rtoc (svalue_t *svp)

/* Return the proper TYPE_ value for the type given by svalue <svp>.
 */

{
    switch (svp->type)
    {
    case T_NUMBER:       return !svp->u.number ? TYPE_ANY : TYPE_NUMBER;
    case T_STRING:       return TYPE_STRING;
    case T_POINTER:      return TYPE_MOD_POINTER | TYPE_ANY;
    case T_FLOAT:        return TYPE_FLOAT;
    case T_CLOSURE:      return TYPE_CLOSURE;
    case T_SYMBOL:       return TYPE_SYMBOL;
    case T_QUOTED_ARRAY: return TYPE_QUOTED_ARRAY;
    case T_MAPPING:      return TYPE_MAPPING;
    default:
        fatal("Bad svalue type at compile time.\n");
    }

    /* NOTREACHED */
    return TYPE_ANY;
} /* type_rtoc() */

/*-------------------------------------------------------------------------*/
static INLINE svalue_t *
copy_svalue (svalue_t *svp)

/* Create another reference to the value <svp> and return it.
 * Of course this only works for numbers, floats and shareable svalues
 * like shared strings or arrays.
 * If the <svp> is not shareable, the function will return a reference
 * to the svalue-0.
 *
 * The function is used to store svalues in the initializer table for
 * a program.
 */

{
    switch (svp->type)
    {
    case T_NUMBER:
    case T_FLOAT:
        break;
    case T_STRING:
        if (svp->x.string_type != STRING_SHARED)
            return &const0;
        /* FALLTHROUGH */
    case T_SYMBOL:
        ref_string(svp->u.string);
        break;
    case T_POINTER:
    case T_QUOTED_ARRAY:
        svp->u.vec->ref++;
        break;
    case T_MAPPING:
        svp->u.map->ref++;
        break;
    case T_CLOSURE:
        addref_closure(svp, "ass to var");
        break;
    default:
        return &const0;
    }

    return svp;
} /* copy_svalue() */

/*-------------------------------------------------------------------------*/
static vector_t *
list_to_vector (size_t length, svalue_t *initialized)

/* <initialized>.u.lvalue points to a const_list of <length> elements: create
 * a vector from this list, store it in <initialized> and also return it.
 */

{
    const_list_t *list;
    vector_t *vec;
    svalue_t *svp;
    void *block;
    const_list_svalue_t *clsv;

%line
    vec = allocate_array(length);
    if (length)
    {
        /* Unravel and copy the constants from the list into the vector
         */
        clsv = initialized->u.const_list;
        list = &clsv->list;
        block = clsv;
        svp = vec->item;
        do {
            *svp++ = list->val;
            list = list->next;
            xfree(block);
        } while ( NULL != (block = list) );
    }

    /* Return the array */
    put_array(initialized, vec);
    return vec;
} /* list_to_vector() */

/*-------------------------------------------------------------------------*/
static void
free_const_list_svalue (svalue_t *svp)

/* Function used as error-handler for const lists: <svp> is in fact
 * a const_list_svalue_t* and this function deallocates all memory
 * associated with the list.
 */

{
    const_list_t *list;
    void *block;

%line
    list = &((const_list_svalue_t *)svp)->list;
    block = svp;
    do {
        free_svalue(&list->val);
        list = list->next;
        xfree(block);
    } while ( NULL != (block = list) );
} /* free_const_list_svalue() */

%endif /* !INITIALIZATION_BY___INIT */


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
%token L_ELSE
%token L_EQ
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
%token L_NOT
%token L_NUMBER
%token L_OBJECT
%ifdef SUPPLY_PARSE_COMMAND
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
        char *name;    /* The shared string with the name */
        int   quotes;  /* Number of quotes */
    } symbol;
      /* A literal symbol.
       */

    ident_t *ident;
      /* L_IDENTIFIER, L_INLINE_FUN: The recognized identifier
       */

    vartype_t type;
      /* The datatype.
       */

    fulltype_t fulltype;
      /* The fulltype (datatype plus visibility) of entities.
       */

    fulltype_t fulltypes[2];
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
        vartype_t type;   /* Type of the expression */
        uint32    start;  /* Startaddress of the instruction */
        short     code;   /* Alternative instruction */
        uint32    end;    /* Endaddress+1 of the instruction */
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
        int       inst;   /* Type of the index */
        uint32    start;  /* Startaddress of the index */
        uint32    end;    /* Endaddress+1 of the index */
        vartype_t type1;  /* Type of index, resp. lower bound */
        vartype_t type2;  /* Type of other index, resp. upper bound */
    }
    index;
      /* This is used to parse and return the indexing operation
       * of an array or mapping.
       * .inst gives the type of the operation:
       *   F_INDEX:     [x]
       *   F_RINDEX:    [<x]
       *   F_RANGE:     [ x.. y]
       *   F_RN_RANGE:  [<x.. y]
       *   F_NR_RANGE:  [ x..<y]
       *   F_RR_RANGE:  [<x..<y]
       *   F_NX_RANGE:  [ x..  ]
       *   F_RX_RANGE:  [<x..  ]
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
        vartype_t type;
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

    char *sh_string;
      /* A shared string */

    struct {
        char    *super; /* NULL, or the allocated qualifier */
        ident_t *real;  /* The function identifier */
    } function_name;
      /* A qualified function name: "<super>::<func>" */

    struct {
        int    simul_efun;  /* -1, or index of the simul_efun */
        p_int  start;       /* Address of the function call */
    } function_call_head;
      /* Used to save address and possible sefun-index over
       * the argument parsing in a function call.
       */

%ifndef INITIALIZATION_BY___INIT

    struct {
        p_int                 length;  /* Length of the list */
        struct const_list_s * l;       /* The list of constants */
    } const_list;
      /* Used to hold the constant elements of an array
       * initializer.
       */

    struct {
        int       function;     /* efun index */
        svalue_t *initialized;  /* svalue to initialize */
    } const_call_head;
      /* Used to hold the information of a constant efun call
       * over the parsing of the arguments.
       */

    svalue_t svalue;
      /* Used for constant float initializers: the float value */

%endif

} /* YYSTYPE */

/*-------------------------------------------------------------------------*/

%type <number>       L_NUMBER constant
%type <float_number> L_FLOAT
%type <closure>      L_CLOSURE
%type <symbol>       L_SYMBOL
%type <number>       L_QUOTED_AGGREGATE
%type <ident>        L_IDENTIFIER L_INLINE_FUN
%type <fulltype>     optional_star type type_modifier_list type_modifier
%type <fulltype>     opt_basic_type basic_type
%type <fulltype>     non_void_type opt_basic_non_void_type basic_non_void_type
%type <fulltypes>    inheritance_qualifier inheritance_qualifiers
%type <fulltype>     inheritance_modifier_list inheritance_modifier
%type <type>         decl_cast cast
%type <lrvalue>      note_start comma_expr expr0 expr4
%type <lrvalue>      function_call inline_fun
%type <lrvalue>      catch sscanf
%type <lrvalue>      for_init_expr for_expr
%type <lrvalue>      comma_expr_decl expr_decl
%ifdef SUPPLY_PARSE_COMMAND
%type <lrvalue>      parse_command
%endif
%type <lvalue>       lvalue name_lvalue local_name_lvalue foreach_var_lvalue
%type <lvalue>       local_name_list new_local new_local_name
%type <index>        index_range index_expr
%type <case_label>   case_label
%type <address>      optional_else
%type <string>       anchestor
%type <sh_string>    call_other_name
%type <function_name> function_name

%ifndef INITIALIZATION_BY___INIT
%type <svalue>       float_constant
%type <const_list>   const_expr_list const_expr_list2 const_expr_list3
%endif

/* Special uses of <number> */

%type <number> function_body
  /* program address or -1 */

%type <number> argument argument_list lvalue_list
  /* number of arguments */

%type <number> expr_list expr_list3 expr_list2
  /* Number of expressions in an expression list */

%type <number> m_expr_values
  /* Number of values for a mapping entry (ie the 'width') */

%type <number> L_ASSIGN
  /* Instruction code of the assignment, e.g. F_ADD_EQ */

%type <number> L_LOCAL
  /* Index number of the local variable */

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

all: program ;

program:
      program def possible_semi_colon
    | /* empty */ ;

possible_semi_colon:
      /* empty */
    | ';' { yywarn("Extra ';'. Ignored."); }
    ;

note_start: { $$.start = CURRENT_PROGRAM_SIZE; } ;

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/* Function prototypes
 * Function definitions
 * Variable definitions
 * Inheritance
 * Default visibility
 */

def:  type optional_star L_IDENTIFIER  /* Function definition or prototype */

      {
          use_local_scopes = pragma_use_local_scopes;
          block_depth = 1;
          init_scope(block_depth);

          if (!($1 & (TYPE_MOD_PRIVATE | TYPE_MOD_PUBLIC
                     | TYPE_MOD_PROTECTED | TYPE_MOD_STATIC)))
          {
              $1 |= default_funmod;
          }

          $2 |= $1; /* $2 is now the complete type */

          /* Require exact types? */
          if ($1 & TYPE_MOD_MASK)
          {
              exact_types = $2;
          }
          else
          {
              if (pragma_strict_types != PRAGMA_WEAK_TYPES)
                  yyerrorf("\"#pragma %s_types\" requires type of function"
                          , pragma_strict_types == PRAGMA_STRICT_TYPES
                            ? "strict" : "strong" );
              exact_types = 0;
          }

          if ($1 & TYPE_MOD_NOSAVE)
          {
              yyerror("can't declare a function as nosave");
              $1 &= ~TYPE_MOD_NOSAVE;
          }

          if ($3->type == I_TYPE_UNKNOWN)
          {
              /* prevent freeing by exotic name clashes */
              ident_t *p = $3;
              p->type = I_TYPE_GLOBAL;
              p->u.global.variable = I_GLOBAL_VARIABLE_OTHER;
              p->u.global.efun     = I_GLOBAL_EFUN_OTHER;
              p->u.global.sim_efun = I_GLOBAL_SEFUN_OTHER;
              p->u.global.function = I_GLOBAL_FUNCTION_VAR;
              p->next_all = all_globals;
              all_globals = p;
          }
      }

      '(' argument ')'

      {
          /* We got the complete prototype: define it */

          if ( current_number_of_locals
           && (full_type_of_locals[current_number_of_locals-1]
               & TYPE_MOD_VARARGS)
             )
          {
%line
              /* The last argument has to allow an array. */
              vartype_t *t;

              $2 |= TYPE_MOD_XVARARGS;

              t = type_of_locals + (current_number_of_locals-1);
              if (!(*t & TYPE_MOD_POINTER)
               && (*t & TYPE_MOD_RMASK) != TYPE_ANY
                 )
              {
                  if ((*t & TYPE_MOD_RMASK) != TYPE_UNKNOWN)
                      yyerror(
                        "varargs parameter must be declared array or mixed");
                  /* Keep the visibility, but change the type to
                   * '&any'
                   */
                  *t &= ~TYPE_MOD_RMASK;
                  *t |= TYPE_ANY;
              }
          }

          /* Define a prototype. If it is a real function, then the
           * prototype will be updated below.
           */
          define_new_function(MY_FALSE, $3, $6, 0, 0,
                              NAME_UNDEFINED|NAME_PROTOTYPE, $2);
      }

      function_body

      {
          /* The function is complete */

          p_int start;
          bytecode_p p;
%line
          if ( (start = $9) < 0)
          {
              /* function_body was a ';' -> prototype
               * Just norm the visibility flags unless it is a prototype
               * for an already inherited function.
               */

              funflag_t *flagp;

              flagp = (funflag_t *)(&FUNCTION($3->u.global.function)->flags);
              if (!(*flagp & NAME_INHERITED))
              {
                  *flagp |= $1 & (*flagp & TYPE_MOD_PUBLIC
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

              p = &(PROGRAM_BLOCK[start]);

              /* FUNCTION_NAME */
              memcpy(p, &$3->name, sizeof $3->name);
              p += sizeof $3->name;

              /* FUNCTION_TYPE */
              *p++ = $2;

              /* FUNCTION_NUM_ARGS */
              if ($2 & TYPE_MOD_XVARARGS)
                *p++ = $6 | ~0x7f;
              else
                *p++ = $6;

              /* FUNCTION_NUM_VARS */
              *p   = max_number_of_locals - $6+ max_break_stack_need;

              define_new_function(MY_TRUE, $3, $6, max_number_of_locals - $6+
                      max_break_stack_need,
                      start + sizeof $3->name + 1, 0, $2);
              ref_string($3->name);

              ins_byte(F_RETURN0); /* catch a missing return */
          }

          /* Clean up */
          free_all_local_names();

          if (first_inline_fun)
              insert_inline_fun_now = MY_TRUE;

          block_depth = 0;
      }

    | type name_list ';' /* Variable definition */
      {
          if ($1 == 0)
              yyerror("Missing type");
          if (first_inline_fun)
              insert_inline_fun_now = MY_TRUE;
      }

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
#ifdef ALIGN_FUNCTIONS
          CURRENT_PROGRAM_SIZE = align(CURRENT_PROGRAM_SIZE);
#endif
          $<number>$ = CURRENT_PROGRAM_SIZE;
          if (realloc_a_program(FUNCTION_HDR_SIZE))
          {
              CURRENT_PROGRAM_SIZE += FUNCTION_HDR_SIZE;
          }
          else
          {
              yyerrorf("Out of memory: program size %lu\n"
                      , mem_block[A_PROGRAM].current_size + FUNCTION_HDR_SIZE);
              YYACCEPT;
          }
      }

      block

      { $$ = $<number>1; }

    | ';' { $$ = -1; }
; /* function_body */

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

%ifdef INITIALIZATION_BY___INIT
          int initializer;
%endif /* INITIALIZATION_BY___INIT */

          if (CURRENT_PROGRAM_SIZE
%ifdef INITIALIZATION_BY___INIT
           && !(((function_t *)(mem_block[A_FUNCTIONS].block+
                         mem_block[A_FUNCTIONS].current_size))[-1].flags &
                         NAME_INHERITED)
%endif /* INITIALIZATION_BY___INIT */
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
           && (!max_eval_cost || eval_cost < max_eval_cost)
             )
          {
              svalue_t *res;

              push_string_shared(last_string_constant);

              if (!compat_mode)
              {
                  char * filename;
                  filename = alloca(strlen(current_file)+2);
                  *filename = '/';
                  strcpy(filename+1, current_file);
                  push_volatile_string(filename);
              }
              else
                  push_volatile_string(current_file);

              res = apply_master(STR_INHERIT_FILE, 2);

              if (res && !(res->type == T_NUMBER && !res->u.number))
              {
                  /* We got a result - either a new name or a "reject it"
                   * value.
                   */

                  char * cp;

                  if (res->type != T_STRING)
                  {
                      yyerrorf("Illegal to inherit file '%s'.", last_string_constant);
                      YYACCEPT;
                  }

                  for (cp = res->u.string; *cp == '/'; cp++) NOOP;

                  if (!legal_path(cp))
                  {
                      yyerrorf("Illegal path '%s'.", res->u.string);
                      YYACCEPT;
                  }

                  /* Ok, now replace the parsed string with the name
                   * we just got.
                   */
                  free_string(last_string_constant);
                  last_string_constant = make_shared_string(cp);
              }
              /* else: no result - use the string as it is */
          }
          else if (max_eval_cost && eval_cost >= max_eval_cost)
          {
              yyerrorf("Can't call master::%s for "
                       "'%s': eval cost too big"
                      , STR_INHERIT_FILE, last_string_constant);
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
              free_string(last_string_constant);
              last_string_constant = NULL;
              yyerrorf("Out of memory when unswapping '%s'", ob->name);
              YYACCEPT;
          }

          /* Legal to inherit? */
          if (ob->prog->flags & P_NO_INHERIT)
          {
              yyerror("Illegal to inherit an object which sets "
                      "'#pragma no_inherit'");
              YYACCEPT;
          }

          free_string(last_string_constant);
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
                              , inherit.prog->name);
                      YYACCEPT;
                  }
                  else
                      yywarnf("Program '%s' already inherited"
                             , inherit.prog->name);
              }
          }


          if (!(inherit.inherit_type & INHERIT_TYPE_DUPLICATE))
          {
              /* Copy the functions and variables, and take
               * care of the initializer.
               */
%ifdef INITIALIZATION_BY___INIT
              initializer = copy_functions(ob->prog, $1[0]);
              copy_variables(ob->prog, $1[1]);

              if (initializer > -1)
              {
                  /* We inherited a __INIT() function: create a call */

                  transfer_init_control();
                  ins_byte(F_CALL_EXPLICIT_INHERITED);
                  ins_short(INHERIT_COUNT);
                  ins_short(initializer);
                  ins_byte(0);        /* Actual number of arguments */
                  ins_byte(F_POP_VALUE);
                  add_new_init_jump();
              }
%else  /* INITIALIZATION_BY___INIT */
              copy_functions(ob->prog, $1[0]);
              copy_variables(ob->prog, $1[1], ob->variables);
%endif /* INITIALIZATION_BY___INIT */

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
    | inheritance_modifier_list inheritance_modifier type_modifier_list
      { $$ = $1 | $2 | $3; }
; /* inheritance_modifier_list */


inheritance_qualifier:
      type optional_star L_IDENTIFIER
      {
          static ident_t    *last_identifier;
          static fulltype_t  last_modifier;
%line

          /* The inherit statement must only specify visibility
           * e.g. not "inherit int * foobar"
           */
          if ($1 & TYPE_MOD_MASK)
          {
              yyerror("syntax error");
          }

          /* Check if there were any modifiers at all */
          if ( !($1 & ~TYPE_MOD_MASK) )
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
              last_modifier = $1 & ~TYPE_MOD_MASK;
          }

          last_identifier = $3;

          if ($2) /* No "*" allowed TODO: So why it's there? */
          {
              yyerror("syntax error");
          }

          /* The L_IDENTIFIER must be one of "functions" or "variables" */
          if (strcmp(last_identifier->name, "functions") == 0)
          {
                $$[0] = last_modifier;
                $$[1] = 0;
          }
          else if (strcmp(last_identifier->name, "variables") == 0)
          {
                $$[0] = 0;
                $$[1] = last_modifier;
          }
          else
          {
              yyerrorf("Unrecognized inheritance modifier '%s'"
                      , last_identifier->name);
              $$[0] = $$[1] = 0;
          }

          /* Free the identifier again if this statement generated it */
          if (last_identifier->type == I_TYPE_UNKNOWN)
                free_shared_identifier(last_identifier);
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
                       | TYPE_MOD_PROTECTED | TYPE_MOD_STATIC)
             )
          {
              yyerror("Default visibility specification for functions "
                      "accepts only 'private', 'protected', 'public' or "
                      "'static'");
              YYACCEPT;
          }

          if ($2[1] & ~( TYPE_MOD_PRIVATE | TYPE_MOD_PUBLIC
                       | TYPE_MOD_PROTECTED)
             )
          {
              yyerror("Default visibility specification for variables "
                      "accepts only 'private', 'protected' or 'public'"
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
          $$ = $1 | $2;
          current_type = $$;
      } ;


non_void_type: type_modifier_list opt_basic_non_void_type
      {
          $$ = $1 | $2;
          current_type = $$;
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
;


opt_basic_type:
      basic_type
    | /* empty */ { $$ = TYPE_UNKNOWN; } ;


opt_basic_non_void_type:
      basic_non_void_type
    | /* empty */ { $$ = TYPE_UNKNOWN; } ;


basic_non_void_type:
      L_STATUS       { $$ = TYPE_NUMBER;  current_type = $$; }
    | L_INT          { $$ = TYPE_NUMBER;  current_type = $$; }
    | L_STRING_DECL  { $$ = TYPE_STRING;  current_type = $$; }
    | L_OBJECT       { $$ = TYPE_OBJECT;  current_type = $$; }
    | L_CLOSURE_DECL { $$ = TYPE_CLOSURE; current_type = $$; }
    | L_SYMBOL_DECL  { $$ = TYPE_SYMBOL;  current_type = $$; }
    | L_FLOAT_DECL   { $$ = TYPE_FLOAT;   current_type = $$; }
    | L_MAPPING      { $$ = TYPE_MAPPING; current_type = $$; }
    | L_MIXED        { $$ = TYPE_ANY;     current_type = $$; }
; /* basic_type */


basic_type:
      basic_non_void_type
    | L_VOID         { $$ = TYPE_VOID;    current_type = $$; }
; /* basic_non_void_type */


cast:
      '(' basic_type optional_star ')'
      {
          $$ = $2 | $3;
      }
;


decl_cast:
      '(' '{' basic_type optional_star '}' ')'
      {
          $$ = $3 | $4;
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
          if (exact_types && $1 == 0)
          {
              yyerror("Missing type for argument");
              add_local_name($3, TYPE_ANY, block_depth);
                /* Supress more errors */
          }
          else
          {
              add_local_name($3, $1 | $2, block_depth);
          }
      }

    | non_void_type optional_star L_LOCAL
      {
          /* A local name is redeclared. Since this is the argument
           * list, it can't be legal.
           */
          yyerror("Illegal to redeclare local name");
      }
; /* new_arg_name */


name_list:
      new_name
    | name_list ',' new_name;


new_name:
      /* Simple variable definition */
      optional_star L_IDENTIFIER
      {
%line
          fulltype_t actual_type = current_type;

          if (!(actual_type & (TYPE_MOD_PRIVATE | TYPE_MOD_PUBLIC
                              | TYPE_MOD_PROTECTED)))
          {
              actual_type |= default_varmod;
          }

          if (actual_type & TYPE_MOD_VARARGS)
          {
              yyerror("can't declare a variable as varargs");
              actual_type &= ~TYPE_MOD_VARARGS;
          }
%ifdef INITIALIZATION_BY___INIT
          define_variable($2, actual_type | $1);

          if (!(actual_type & TYPE_MOD_POINTER)
           && (actual_type & PRIMARY_TYPE_MASK) == TYPE_FLOAT
             )
          {
              int i = verify_declared($2); /* Is the var declared? */

              transfer_init_control();          /* Prepare INIT code */
              variables_initialized = MY_TRUE; /* We have __INIT code */

              {
                  PREPARE_INSERT(5)
                  add_byte(F_FCONST0);

                  /* Push the variable reference and create the assignment */

                  if (i + num_virtual_variables > 0xff)
                  {
                      add_byte(F_PUSH_IDENTIFIER16_LVALUE);
                      add_short(i + num_virtual_variables);
                      CURRENT_PROGRAM_SIZE += 1;
                  }
                  else
                  {
                      add_byte(F_PUSH_IDENTIFIER_LVALUE);
                      add_byte(i + num_virtual_variables);
                  }

                  /* Ok, assign */
                  add_byte(F_VOID_ASSIGN);
                  CURRENT_PROGRAM_SIZE += 4;
                  add_new_init_jump();
              }
          }
%else /* then !INITIALIZATION_BY___INIT */
          if (!(actual_type & TYPE_MOD_POINTER)
           && (actual_type & PRIMARY_TYPE_MASK) == TYPE_FLOAT
             )
          {
              svalue_t s_zero;
              double zero = 0.0;
              STORE_DOUBLE_USED

              s_zero.type = T_FLOAT;
              STORE_DOUBLE(&s_zero, zero);
              define_variable($2, actual_type | $1, &s_zero);
          }
          else
              define_variable($2, actual_type | $1, &const0);
%endif
      }

    /* Variable definition with initialization */

%ifdef INITIALIZATION_BY___INIT

    | optional_star L_IDENTIFIER
      {
          fulltype_t actual_type = current_type;

          if (!(actual_type & (TYPE_MOD_PRIVATE | TYPE_MOD_PUBLIC
                              | TYPE_MOD_PROTECTED)))
          {
              actual_type |= default_varmod;
          }

          define_variable($2, actual_type | $1);
          $<number>$ = verify_declared($2); /* Is the var declared? */
          transfer_init_control();          /* Prepare INIT code */
      }

      L_ASSIGN expr0

      {
          int i = $<number>3;
          PREPARE_INSERT(4)

          fulltype_t actual_type = current_type;

          if (!(actual_type & (TYPE_MOD_PRIVATE | TYPE_MOD_PUBLIC
                              | TYPE_MOD_PROTECTED)))
          {
              actual_type |= default_varmod;
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

          if (i + num_virtual_variables > 0xff)
          {
              add_byte(F_PUSH_IDENTIFIER16_LVALUE);
              add_short(i + num_virtual_variables);
              CURRENT_PROGRAM_SIZE += 1;
          }
          else
          {
              add_byte(F_PUSH_IDENTIFIER_LVALUE);
              add_byte(i + num_virtual_variables);
          }

          /* Only simple assigns are allowed */
          if ($4 != F_ASSIGN)
              yyerror("Illegal initialization");

          /* Do the types match? */
          if (!compatible_types((actual_type | $1) & TYPE_MOD_MASK, $5.type))
          {
              yyerrorf("Type mismatch %s when initializing %s",
                      get_two_types(actual_type | $1, $5.type), $2->name);
          }

          /* Ok, assign */
          add_byte(F_VOID_ASSIGN);
          CURRENT_PROGRAM_SIZE += 3;
          add_new_init_jump();
      }

%else /* then !INITIALIZATION_BY___INIT */

    | optional_star L_IDENTIFIER

      {
          /* svalue_constant can contain identifiers, so define the variable
           * now, lest the identifier could get freed by a name clash.
           */
%line
          int n;
          fulltype_t actual_type = current_type;

          if (!(actual_type & (TYPE_MOD_PRIVATE | TYPE_MOD_PUBLIC
                              | TYPE_MOD_PROTECTED)))
          {
              actual_type |= default_varmod;
          }

          define_variable($2, actual_type | $1 | NAME_INITIALIZED, &const0);
          n = $2->u.global.variable;
          $<initialized>$ = currently_initialized
            = n & VIRTUAL_VAR_TAG ? V_VAR_VALUE(n) : NV_VAR_VALUE(n);
      }

      L_ASSIGN svalue_constant

      {
%line
          /* The parsing of the svalue_constant assigned the value
           * to the currently_initialized buffer set above, so
           * we just have to check the validity.
           */

          fulltype_t actual_type = current_type;

          if (!(actual_type & (TYPE_MOD_PRIVATE | TYPE_MOD_PUBLIC
                              | TYPE_MOD_PROTECTED)))
          {
              actual_type |= default_varmod;
          }

          if ($4 != F_ASSIGN)
              yyerror("Illegal initialization");

          if (exact_types)
              if (!TYPE( actual_type | $1 , type_rtoc($<initialized>3)) )
              {
                  yyerror("Bad initializer type");
              }
    }
%endif /* INITIALIZATION_BY___INIT */

; /* new_name */


/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
/* Blocks and simple statements.
 */

block:
      '{'

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

              if (use_local_scopes && scope->num_locals > scope->num_cleared)
              {
                  mem_block[A_PROGRAM].block[scope->addr+2]
                    = (char)(scope->num_locals - scope->num_cleared);
              }
          }
      }

      '}'

      { leave_block_scope(MY_FALSE); }
; /* block */


statements:
      /* empty */
    | statements basic_type local_name_list ';'
    | statements statement
;


local_name_list:
      new_local
    | local_name_list ',' new_local ;


new_local :
      new_local_name
      {
          /* If this is a float variable, we need to insert an appropriate
           * initializer, as the default svalue-0 is not a valid float value.
           */

          p_int length;
%line

          if (!($1.type & TYPE_MOD_POINTER)
           && ($1.type & PRIMARY_TYPE_MASK) == TYPE_FLOAT
             )
          {
              /* Insert: FCONST0
               *         PUSH_LOCAL_VARIABLE_VALUE <num>
               *         F_VOID_ASSIGN
               */

              /* Insert: FLOAT 0.0 */
              ins_f_code(F_FCONST0);

              /* Add the bytecode to create the lvalue and do the
               * assignment.
               */
              length = $1.length;
              if (length)
              {
                  add_to_mem_block(A_PROGRAM, $1.u.p, length+1);
                  yfree($1.u.p);
                  last_expression = CURRENT_PROGRAM_SIZE-1;
                  mem_block[A_PROGRAM].block[last_expression] = F_VOID_ASSIGN;
              }
              else
              {
                  bytecode_p source, dest;
                  mp_uint current_size;

                  source = $1.u.simple;
                  current_size = CURRENT_PROGRAM_SIZE;
                  if (!realloc_a_program(3))
                  {
                      yyerrorf("Out of memory: program size %lu", current_size+3);
                      YYACCEPT;
                  }
                  CURRENT_PROGRAM_SIZE = (last_expression = current_size + 2) + 1;
                  dest = PROGRAM_BLOCK + current_size;
                  *dest++ = *source++;
                  *dest++ = *source;
                  *dest = F_VOID_ASSIGN;
              }
          }
      }
    | new_local_name L_ASSIGN expr0
      {
          /* We got a "<name> = <expr>" type declaration. */

          p_int length;
          vartype_t type2;
%line

          /* Check the assignment for validity */
          type2 = $3.type;
          if (exact_types && !compatible_types($1.type, type2))
          {
              yyerrorf("Bad assignment %s", get_two_types($1.type, $3.type));
          }

          if ($2 != F_ASSIGN)
          {
              yyerror("Only plain assignments allowed here.");
          }

          if (type2 & TYPE_MOD_REFERENCE)
              yyerror("Can't trace reference assignments.");

          /* Add the bytecode to create the lvalue and do the
           * assignment.
           */
          length = $1.length;
          if (length)
          {
              add_to_mem_block(A_PROGRAM, $1.u.p, length+1);
              yfree($1.u.p);
              last_expression = CURRENT_PROGRAM_SIZE-1;
              mem_block[A_PROGRAM].block[last_expression] = F_VOID_ASSIGN;
          }
          else
          {
              bytecode_p source, dest;
              mp_uint current_size;

              source = $1.u.simple;
              current_size = CURRENT_PROGRAM_SIZE;
              if (!realloc_a_program(3))
              {
                  yyerrorf("Out of memory: program size %lu", current_size+3);
                  YYACCEPT;
              }
              CURRENT_PROGRAM_SIZE = (last_expression = current_size + 2) + 1;
              dest = PROGRAM_BLOCK + current_size;
              *dest++ = *source++;
              *dest++ = *source;
              *dest = F_VOID_ASSIGN;
          }
      }
; /* new_local */

new_local_name:
      optional_star L_IDENTIFIER
      {
          /* A new local variable */

          block_scope_t *scope = block_scope + block_depth - 1;
          ident_t *q;

          q = add_local_name($2, current_type | $1, block_depth);

          if (use_local_scopes && scope->clobbered)
          {
              /* Finish the previous CLEAR_LOCALS, if any */
              if (scope->num_locals - 1 > scope->num_cleared)
                  mem_block[A_PROGRAM].block[scope->addr+2]
                    = (char)(scope->num_locals - 1 - scope->num_cleared);
              scope->clobbered = MY_FALSE;
              scope->num_cleared = scope->num_locals - 1;
          }
          if (use_local_scopes && scope->num_locals == scope->num_cleared + 1)
          {
              /* First definition of a local, so insert the
               * clear_locals bytecode and remember its position
               */
              scope->addr = mem_block[A_PROGRAM].current_size;
              ins_f_code(F_CLEAR_LOCALS);
              ins_byte(scope->first_local + scope->num_cleared);
              ins_byte(0);
          }

          $$.u.simple[0] = F_PUSH_LOCAL_VARIABLE_LVALUE;
          $$.u.simple[1] = q->u.local.num;
          $$.length = 0;
          $$.type = current_type | $1;
      }

    | optional_star L_LOCAL
      {
          /* A local name is redeclared. If this happens on a deeper
           * level, it is even legal.
           */

          ident_t *q;
          block_scope_t *scope = block_scope + block_depth - 1;

          q = redeclare_local($2, current_type | $1, block_depth);

          if (use_local_scopes && scope->clobbered)
          {
              /* Finish the previous CLEAR_LOCALS, if any */
              if (scope->num_locals - 1 > scope->num_cleared)
                  mem_block[A_PROGRAM].block[scope->addr+2]
                    = (char)(scope->num_locals - 1 - scope->num_cleared);
              scope->clobbered = MY_FALSE;
              scope->num_cleared = scope->num_locals - 1;
          }
          if (use_local_scopes && scope->num_locals == scope->num_cleared + 1)
          {
              /* First definition of a local, so insert the
               * clear_locals bytecode and remember its position
               */
              scope->addr = mem_block[A_PROGRAM].current_size;
              ins_f_code(F_CLEAR_LOCALS);
              ins_byte(scope->first_local + scope->num_cleared);
              ins_byte(0);
          }

          $$.u.simple[0] = F_PUSH_LOCAL_VARIABLE_LVALUE;
          $$.u.simple[1] = q->u.local.num;
          $$.length = 0;
          $$.type = current_type | $1;
      }
; /* new_local_name */


statement:
      comma_expr ';'
      {
          insert_pop_value();
#ifdef F_BREAK_POINT
          if (d_flag)
              ins_byte(F_BREAK_POINT);
#endif /* F_BREAK_POINT */
          /* if (exact_types && !BASIC_TYPE($1.type, TYPE_VOID))
           *    yyerror("Value thrown away");
           */
      }

    | error ';' /* Synchronisation point */
    | cond | while | do | for | foreach | switch | case | default
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

              ins_byte(F_BREAK);
          }
          else
          {
              /* A normal loop break: add the FBRANCH to the list */

              ins_byte(F_FBRANCH);
              ins_long(current_break_address & BREAK_ADDRESS_MASK);
              current_break_address = CURRENT_PROGRAM_SIZE - 4;
              if (current_break_address > BREAK_ADDRESS_MASK)
                  yyerrorf("Compiler limit: (L_BREAK) value too large: %ld"
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
                  ins_long(4);
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
              ins_byte(F_FBRANCH);
          }

          /* In either case, handle the list of continues alike */
          ins_long(current_continue_address & CONTINUE_ADDRESS_MASK);
          current_continue_address =
                        ( current_continue_address & SWITCH_DEPTH_MASK ) |
                        ( CURRENT_PROGRAM_SIZE - 4 );
      }
; /* statement */


return:
      L_RETURN
      {
          if (exact_types
           && !BASIC_TYPE(exact_types & TYPE_MOD_MASK, TYPE_VOID))
              type_error("Must return a value for a function declared",
                         exact_types);
          ins_byte(F_RETURN0);
      }

    | L_RETURN comma_expr
      {
%line
          if (exact_types)
          {
              fulltype_t rtype = exact_types & TYPE_MOD_MASK;

              /* More checks, ie. mixed vs non-mixed, would be nice,
               * but the general type tracking is too lacking for it.
               */
              if (!MASKED_TYPE($2.type, rtype))
              {
                  char tmp[100];
                  strcpy(tmp, get_type_name($2.type));
                  yyerrorf("Return type not matching: got %s, expected %s"
                         , tmp, get_type_name(rtype));
              }
          }

          if ($2.type & TYPE_MOD_REFERENCE)
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
              ins_byte(F_RETURN);
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
          $<expression>$.line = current_line;

          /* Restart codegeneration for the body where we began */
          CURRENT_PROGRAM_SIZE = addr;
          last_expression = -1;

          /* The initial branch to the condition code */
          ins_byte(F_BRANCH);
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
          p_int next_addr;
          p_int addr = pop_address();

          /* Update the offsets of all continue BRANCHes
           * (resp BREAK_CONTINUEs) to branch to the current address.
           */
          for ( ; current_continue_address > 0
                ; current_continue_address = next_addr)
          {
              next_addr = read_long(current_continue_address);
              upd_long(current_continue_address,
                  CURRENT_PROGRAM_SIZE - current_continue_address);
          }

          /* If necessary, update the leading BRANCH to an LBRANCH */
          offset = fix_branch( F_LBRANCH, CURRENT_PROGRAM_SIZE, addr);

          /* Add the condition code to the program */
          if ($<expression>6.line != current_line)
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

          if ($<expression>6.line != current_line)
              store_line_number_relocation($<expression>6.line);

          /* Now that we have the end of the while(), we can finish
           * up the breaks.
           */
          for( ; current_break_address > 0
               ; current_break_address = next_addr)
          {
              next_addr = read_long(current_break_address);
              upd_long(current_break_address,
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

          p_int next_addr;
          p_int current;
%line
          current = CURRENT_PROGRAM_SIZE;
          for(; current_continue_address > 0
              ; current_continue_address = next_addr)
          {
              next_addr = read_long(current_continue_address);
              upd_long(current_continue_address,
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
          p_int next_addr;
          p_int addr = pop_address();
          mp_uint current;
          bytecode_p dest;

          current = CURRENT_PROGRAM_SIZE;
          if (!realloc_a_program(3))
          {
              yyerrorf("Out of memory: program size %lu\n", current+3);
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
              next_addr = read_long(current_break_address);
              upd_long(current_break_address,
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
          $<expression>$.line = current_line;

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
          $<expression>$.line = current_line;

          /* Restart the codegeneration for the body */
          CURRENT_PROGRAM_SIZE = $<number>6;
          last_expression = -1;
          current_break_address = BREAK_DELIMITER;

          ins_byte(F_BRANCH); /* over the body to the condition */
          ins_byte(0);

          /* Fix the number of locals to clear, now that we know it
           */
          {
              block_scope_t *scope = block_scope + block_depth - 1;

              if (use_local_scopes && scope->num_locals > scope->num_cleared)
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
           * code save on the compiler stack and patch up
           * the break and continues.
           */

          p_int offset;
          p_int next_addr;

          /* Patch up the continues */
          for (; current_continue_address > 0
               ; current_continue_address = next_addr)
          {
              next_addr = read_long(current_continue_address);
              upd_long(current_continue_address,
                  CURRENT_PROGRAM_SIZE - current_continue_address);
          }

          if ( $<expression>9.line != current_line
           || (    $<expression>12.line != current_line
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

          if ($<expression>9.line != current_line)
              store_line_number_relocation($<expression>9.line);

          /* Now complete the break instructions.
           */
          for (; current_break_address > 0
               ; current_break_address = next_addr)
          {
              next_addr = read_long(current_break_address);
              upd_long(current_break_address,
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
          ins_byte(F_CONST1);
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

          p_int length;
          vartype_t type2;
%line
          /* Check the assignment for validity */
          type2 = $3.type;
          if (exact_types && !compatible_types($1.type, type2))
          {
              yyerrorf("Bad assignment %s", get_two_types($1.type, $3.type));
          }

          if ($2 != F_ASSIGN)
          {
              yyerror("Only plain assignments allowed here.");
          }

          if (type2 & TYPE_MOD_REFERENCE)
              yyerror("Can't trace reference assignments.");

          /* Add the bytecode to create the lvalue and do the
           * assignment.
           */
          length = $1.length;
          if (length)
          {
              add_to_mem_block(A_PROGRAM, $1.u.p, length+1);
              yfree($1.u.p);
              last_expression = CURRENT_PROGRAM_SIZE-1;
              mem_block[A_PROGRAM].block[last_expression] = $2;
          }
          else
          {
              bytecode_p source, dest;
              mp_uint current_size;

              source = $1.u.simple;
              current_size = CURRENT_PROGRAM_SIZE;
              if (!realloc_a_program(3))
              {
                  yyerrorf("Out of memory: program size %lu", current_size+3);
                  YYACCEPT;
              }
              CURRENT_PROGRAM_SIZE = (last_expression = current_size + 2) + 1;
              dest = PROGRAM_BLOCK + current_size;
              *dest++ = *source++;
              *dest++ = *source;
              *dest = $2;
          }
      }
    | local_name_lvalue
      {
          /* We got a "int <name>" type expression. Compile it as if
           * it has been "int <name> = 0".
           */

          p_int length;
%line
          /* Add the bytecode for pushing the number 0 onto the stack */
          ins_f_code(F_CONST0);

          /* Add the bytecode to create the lvalue and do the
           * assignment.
           */
          length = $1.length;
          if (length)
          {
              add_to_mem_block(A_PROGRAM, $1.u.p, length+1);
              yfree($1.u.p);
              last_expression = CURRENT_PROGRAM_SIZE-1;
              mem_block[A_PROGRAM].block[last_expression] = F_ASSIGN;
          }
          else
          {
              bytecode_p source, dest;
              mp_uint current_size;

              source = $1.u.simple;
              current_size = CURRENT_PROGRAM_SIZE;
              if (!realloc_a_program(3))
              {
                  yyerrorf("Out of memory: program size %lu", current_size+3);
                  YYACCEPT;
              }
              CURRENT_PROGRAM_SIZE = (last_expression = current_size + 2) + 1;
              dest = PROGRAM_BLOCK + current_size;
              *dest++ = *source++;
              *dest++ = *source;
              *dest = F_ASSIGN;
          }
      }
; /* expr_decl */


for_expr:
      /* EMPTY */
      {
          last_expression = mem_block[A_PROGRAM].current_size;
          ins_byte(F_CONST1);
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
 *       PUSH_(LOCAL_)LVALUE <varn>
 *       <expr>
 *       FOREACH <numargs> c
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

      expr0 ')'

      {
          vartype_t dtype;

%line
          dtype = $7.type & TYPE_MOD_RMASK;

          if (!(dtype & TYPE_MOD_POINTER)
           && dtype != TYPE_ANY
           && dtype != TYPE_STRING
           && dtype != TYPE_MAPPING
           && (exact_types || dtype != TYPE_UNKNOWN)
             )
          {
              type_error("Expression for foreach() of wrong type", $7.type);
          }

          /* Fix the number of locals to clear, now that we know it
           */
          {
              block_scope_t *scope = block_scope + block_depth - 1;

              if (use_local_scopes && scope->num_locals > scope->num_cleared)
              {
                  mem_block[A_PROGRAM].block[scope->addr+2]
                    = (char)(scope->num_locals - scope->num_cleared);
              }
          }

          /* Create the FOREACH instruction, leaving the branch field
           * blank.
           */
          ins_f_code(F_FOREACH);
          ins_byte($4+1);
          ins_short(0);

          push_address(); /* Address to branch back to */
      }

      statement

      {
          /* The body is complete - patch up the continue and
           * break statements and generate the remaining statements.
           */

          p_int next_addr;
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
          if (addr == current)
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
          }
          else /* Create the full statement */
          {
              /* First patch up the continue statements */

              for(; current_continue_address > 0
                  ; current_continue_address = next_addr)
              {
                  next_addr = read_long(current_continue_address);
                  upd_long(current_continue_address,
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
                  next_addr = read_long(current_break_address);
                  upd_long(current_break_address,
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

          p_int length;

%line
          length = $1.length;
          if (length)
          {
              add_to_mem_block(A_PROGRAM, $1.u.p, length);
              yfree($1.u.p);
          }
          else
          {
              bytecode_p source, dest;
              mp_uint current_size;

              source = $1.u.simple;
              current_size = CURRENT_PROGRAM_SIZE;
              if (!realloc_a_program(2))
              {
                  yyerrorf("Out of memory: program size %lu", current_size+2);
                  YYACCEPT;
              }
              CURRENT_PROGRAM_SIZE = current_size + 2;
              dest = PROGRAM_BLOCK + current_size;
              *dest++ = *source++;
              *dest++ = *source;
          }
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

      L_IDENTIFIER

      {
          if (strcmp($1->name, "in"))
              yyerror("Expected keyword 'in' in foreach()");
          if ($1->type == I_TYPE_UNKNOWN)
              free_shared_identifier($1);
      }

    | L_LOCAL

      {
          ident_t *id;

          /* Find the ident structure for this local */
          for (id = all_locals; id; id = id->next_all)
              if (id->u.local.num == $1)
                  break;

          if (id && strcmp(id->name, "in"))
              yyerror("Expected keyword 'in' in foreach()");
      }

    | ':'
; /* foreach_in */

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
/* The switch statement.
 *
 * switch.h explains how the bytecode looks like.
 *
 * Note that the actual switch rule is:
 *
 *   switch: L_SWITCH ( comma_expr ) block
 *
 * and that case and default are both just special kinds of statement
 * which mark addresses within the statement code to which the
 * switch statement may jump.
 *
 * That also means that in contrast to C the code
 *
 *    switch(x);
 * or switch(x) write("Foo");
 *
 * is syntactically not ok.
 *
 * TODO: Since current_break_address is used to indicate an active switch(),
 * TODO:: the compiler can't compile Duff's device: the inner while() hides
 * TODO:: active switch(). If that is fixed, we can change the 'block'
 * TODO:: back to 'statement' in the grammar rule.
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
            yyerrorf("Out of memory: case state (%lu bytes)"
                    , (unsigned long) sizeof(case_state_t));
            YYACCEPT;
        }
        *statep = case_state;
        case_state.previous = statep;
        push_explicit(current_break_address);
        push_explicit(switch_pc);

        /* Create the SWITCH instruction plus two empty bytes */
        ins_byte(F_SWITCH);
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

      block

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
        ins_byte(F_BREAK);

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


case: L_CASE case_label ':'
    {
%line
        /* Mark the current program address as another
         * case target for the current switch.
         */
        case_list_entry_t *temp;

        if ( !( current_break_address & CASE_LABELS_ENABLED ) )
        {
            yyerror("Case outside switch");
            break;
        }

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
        temp->line = current_line;
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

        if ( !( current_break_address & CASE_LABELS_ENABLED ) )
        {
            yyerror("Case range outside switch");
            break;
        }

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
            temp->line = current_line;
        }

        /* Get and fill in the two case entries */

        if ( !(temp = new_case_entry()) )
        {
            yyerror("Out of memory: new case entry");
            break;
        }
        temp->key = $2.key;
        temp->addr = 1; /* marks the lower bound of the range */
        temp->line = current_line;

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

          if ( !( current_break_address & CASE_LABELS_ENABLED ) ) {
              yyerror("Default outside switch");
              break;
          }

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
              yyerrorf("Out of memory: program size %lu\n", current+3);
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
          ins_byte(F_BRANCH);
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
      { fatal("presence of rule should prevent its reduction"); }
    | string_constant '+' L_STRING L_STRING
      { fatal("presence of rule should prevent its reduction"); }
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
          vartype_t type1, type2, restype;
%line
          $$ = $4;

          type1 = $1.type;
          type2 = $4.type;
          restype = type2; /* Assume normal assignment */

          /* Check the validity of the assignment */
          if (exact_types
           && !compatible_types($1.type, type2)
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
                  switch(type1)
                  {
                  case TYPE_STRING:
                      if (type2 == TYPE_NUMBER || type2 == TYPE_FLOAT)
                      {
                          ok = MY_TRUE;
                      }
                      break;
                  case TYPE_FLOAT:
                      if (type2 == TYPE_NUMBER)
                      {
                          ok = MY_TRUE;
                      }
                      break;
                  }
                  break;

              case F_SUB_EQ:
                  switch(type1)
                  {
                  case TYPE_FLOAT:
                      if (type2 == TYPE_NUMBER)
                      {
                          ok = MY_TRUE;
                      }
                      break;
                  }
                  break;

              case F_MULT_EQ:
                  switch(type1)
                  {
                  case TYPE_STRING:
                      if (type2 == TYPE_NUMBER)
                      {
                          ok = MY_TRUE;
                      }
                      break;
                  case TYPE_FLOAT:
                      if (type2 == TYPE_NUMBER)
                      {
                          ok = MY_TRUE;
                      }
                      break;
                  default:
                      if ((type1 & TYPE_MOD_POINTER) && type2 == TYPE_NUMBER)
                      {
                          ok = MY_TRUE;
                      }
                  }
                  break;

              case F_DIV_EQ:
                  switch(type1)
                  {
                  case TYPE_FLOAT:
                      if (type2 == TYPE_NUMBER)
                      {
                          ok = MY_TRUE;
                      }
                      break;
                  }
                  break;

              case F_AND_EQ:
                  switch(type1)
                  {
                  case TYPE_MAPPING:
                      if (type2 & TYPE_MOD_POINTER)
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

          if (type2 & TYPE_MOD_REFERENCE)
              yyerror("Can't trace reference assignments.");

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
      {   yyerror("Illegal LHS"); $$ = $3; $$.type = TYPE_ANY; }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr0 '?'
      {
          /* Insert the branch to the :-part and remember this address */
          ins_byte(F_BRANCH_WHEN_ZERO);
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
          ins_byte(F_BRANCH);
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
          vartype_t type1, type2;

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

          $$ = $1;
          $$.end = CURRENT_PROGRAM_SIZE;

          /* Check the types and determine the result type */
          type1 = $4.type;
          type2 = $7.type;

          if (!compatible_types(type1, type2))
          {
              $$.type = TYPE_ANY;
              if ((type1 & TYPE_MOD_POINTER) != 0
               && (type2 & TYPE_MOD_POINTER) != 0)
                  $$.type |= TYPE_MOD_POINTER;
              /* TODO: yyinfof("Different types to ?: */
          }
          else if (type1 == TYPE_ANY)
              $$.type = type2;
          else if (type2 == TYPE_ANY)
              $$.type = type1;
          else if (type1 == (TYPE_MOD_POINTER|TYPE_ANY) )
              $$.type = type2;
          else if (type2 == (TYPE_MOD_POINTER|TYPE_ANY) )
              $$.type = type1;
          else
              $$.type = type1;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr0 L_LOR %prec L_LOR
      {
          /* Insert the LOR and remember the position */

          ins_byte(F_LOR);
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
          if ($1.type == $4.type)
              $$.type = $1.type;
          else
              $$.type = TYPE_ANY;  /* Return type can't be known */
      } /* LOR */

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr0 L_LAND %prec L_LAND
      {
          /* Insert the LAND and remember the position */

          ins_byte(F_LAND);
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
          if ($1.type == $4.type)
              $$.type = $1.type;
          else
              $$.type = TYPE_ANY;        /* Return type can't be known */
       } /* LAND */

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr0 '|' expr0
      {
          if (exact_types && !BASIC_TYPE($1.type,TYPE_NUMBER))
              type_error("Bad argument 1 to |", $1.type);
          if (exact_types && !BASIC_TYPE($3.type,TYPE_NUMBER))
              type_error("Bad argument 2 to |", $3.type);
          $$ = $1;
          $$.type = TYPE_NUMBER;
          ins_byte(F_OR);
          $$.end = CURRENT_PROGRAM_SIZE;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr0 '^' expr0
      {
          if (exact_types && !BASIC_TYPE($1.type,TYPE_NUMBER))
              type_error("Bad argument 1 to ^", $1.type);
          if (exact_types && !BASIC_TYPE($3.type,TYPE_NUMBER))
              type_error("Bad argument 2 to ^", $3.type);
          $$ = $1;
          $$.type = TYPE_NUMBER;
          ins_byte(F_XOR);
          $$.end = CURRENT_PROGRAM_SIZE;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr0 '&' expr0
      {
          $$ = $1;
          ins_byte(F_AND);
          $$.type = TYPE_ANY;
          $$.end = CURRENT_PROGRAM_SIZE;

          /* Check the types */
          if (exact_types)
          {
              vartype_t first_type  = $1.type;
              vartype_t second_type = $3.type;
              if ( first_type == TYPE_ANY
               &&  second_type == TYPE_ANY )
              {
                    /* $$ == TYPE_ANY is correct */
              }
              else if (first_type == TYPE_MAPPING)
              {
                  if (second_type != TYPE_MAPPING
                   && !(second_type & TYPE_MOD_POINTER)
                   && second_type != TYPE_ANY
                     )
                  {
                      type_error("Bad argument 2 to &", second_type );
                  }
                  $$.type = TYPE_MAPPING;
              }
              else if ( (first_type | second_type) & TYPE_MOD_POINTER)
              {
                  if (first_type  == TYPE_NUMBER
                   || second_type == TYPE_NUMBER)
                  {
                      yyerrorf("Incompatible types for arguments to & %s"
                              , get_two_types(first_type, second_type));
                  }
                  else if (( !( first_type & TYPE_MOD_POINTER )
                           || first_type & TYPE_MOD_REFERENCE)
                        && first_type != TYPE_ANY)
                  {
                      type_error("Bad argument 1 to &", first_type );
                  }
                  else if (( !( second_type & TYPE_MOD_POINTER )
                           ||   second_type & TYPE_MOD_REFERENCE)
                        && second_type != TYPE_ANY)
                  {
                      type_error("Bad argument 2 to &", first_type );
                  }
                  else if ( !BASIC_TYPE(first_type &~TYPE_MOD_POINTER,
                                           second_type &~TYPE_MOD_POINTER) )
                  {
                      yyerrorf("Incompatible types for arguments to & %s"
                              , get_two_types(first_type, second_type));
                  }
                  else
                  {
                      $$.type = TYPE_ANY | TYPE_MOD_POINTER;
                  }
              }
              else
              {
                  if ( !BASIC_TYPE(first_type ,TYPE_NUMBER)
                   &&  !BASIC_TYPE(first_type ,TYPE_STRING) )
                      type_error("Bad argument 1 to &", first_type );
                  if ( !BASIC_TYPE(second_type,TYPE_NUMBER)
                   &&  !BASIC_TYPE(second_type ,TYPE_STRING) )
                      type_error("Bad argument 2 to &", second_type);
                  if ( first_type == TYPE_ANY )
                      $$.type =   BASIC_TYPE(second_type ,TYPE_NUMBER)
                                ? TYPE_NUMBER : TYPE_STRING;
                  else
                      $$.type =   BASIC_TYPE(first_type ,TYPE_NUMBER)
                                ? TYPE_NUMBER : TYPE_STRING;
              }
          } /* end of exact_types code */
      } /* end of '&' code */

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr0 L_EQ expr0
      {
          vartype_t t1 = $1.type, t2 = $3.type;

          $$ = $1;
          if (exact_types
           && t1 != t2 && t1 != TYPE_ANY && t2 != TYPE_ANY
           && !(t1 == TYPE_NUMBER && t2 == TYPE_FLOAT)
           && !(t1 == TYPE_FLOAT && t2 == TYPE_NUMBER)
             )
          {
              yyerrorf("== always false because of different types %s"
                      , get_two_types($1.type, $3.type));
          }
          ins_byte(F_EQ);
          $$.type = TYPE_NUMBER;
          $$.end = CURRENT_PROGRAM_SIZE;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr0 L_NE expr0
      {
          vartype_t t1 = $1.type, t2 = $3.type;

          $$ = $1;
          if (exact_types
           && t1 != t2 && t1 != TYPE_ANY && t2 != TYPE_ANY
           && !(t1 == TYPE_NUMBER && t2 == TYPE_FLOAT)
           && !(t1 == TYPE_FLOAT && t2 == TYPE_NUMBER)
             )
           {
              yyerrorf("!= always true because of different types %s"
                      , get_two_types($1.type, $3.type));
          }
          ins_byte(F_NE);
          $$.type = TYPE_NUMBER;
          $$.end = CURRENT_PROGRAM_SIZE;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr0 '>'  expr0
      {
          $$ = $1;
          $$.type = TYPE_NUMBER;
          ins_f_code(F_GT);
          $$.end = CURRENT_PROGRAM_SIZE;
      }
    | expr0 L_GE  expr0
      {
          $$ = $1;
          $$.type = TYPE_NUMBER;
          ins_f_code(F_GE);
          $$.end = CURRENT_PROGRAM_SIZE;
      }
    | expr0 '<'  expr0
      {
          $$ = $1;
          $$.type = TYPE_NUMBER;
          ins_f_code(F_LT);
          $$.end = CURRENT_PROGRAM_SIZE;
      }
    | expr0 L_LE  expr0
      {
          $$ = $1;
          $$.type = TYPE_NUMBER;
          ins_f_code(F_LE);
          $$.end = CURRENT_PROGRAM_SIZE;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr0 L_LSH expr0
      {
          $$ = $1;
          ins_byte(F_LSH);
          $$.type = TYPE_NUMBER;
          $$.end = CURRENT_PROGRAM_SIZE;
          if (exact_types)
          {
              if (!BASIC_TYPE($1.type, TYPE_NUMBER))
                  type_error("Bad argument number 1 to '<<'", $1.type);
              if (!BASIC_TYPE($3.type, TYPE_NUMBER))
                  type_error("Bad argument number 2 to '<<'", $3.type);
          }
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr0 L_RSH expr0
      {
          $$ = $1;
          ins_byte(F_RSH);
          $$.type = TYPE_NUMBER;
          $$.end = CURRENT_PROGRAM_SIZE;
          if (exact_types)
          {
              if (!BASIC_TYPE($1.type, TYPE_NUMBER))
                  type_error("Bad argument number 1 to '>>'", $1.type);
              if (!BASIC_TYPE($3.type, TYPE_NUMBER))
                  type_error("Bad argument number 2 to '>>'", $3.type);
          }
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr0 L_RSHL expr0
      {
          $$ = $1;
          ins_byte(F_RSHL);
          $$.type = TYPE_NUMBER;
          $$.end = CURRENT_PROGRAM_SIZE;
          if (exact_types)
          {
              if (!BASIC_TYPE($1.type, TYPE_NUMBER))
                  type_error("Bad argument number 1 to '>>>'", $1.type);
              if (!BASIC_TYPE($3.type, TYPE_NUMBER))
                  type_error("Bad argument number 2 to '>>>'", $3.type);
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
           * we'll do them at run-time.
           * Here we just try to fold "string" + "string".
           */

          mp_uint current_size;
          bytecode_p p;
%line
          $$ = $1;

          current_size = CURRENT_PROGRAM_SIZE;
          p = &(PROGRAM_BLOCK[current_size]);

          /* Check if we can combine strings: the pragma must agree
           * and the last four bytes must be two CSTRINGx instructions.
           */
          if (pragma_combine_strings
           && last_expression + 2 == current_size
           && $<numbers>3[0] + 4 == (mp_int)current_size
           && ((p[-2]-(F_CSTRING0)) & ~3) == 0
           && ((p[-4]-(F_CSTRING0)) & ~3) == 0
             )
          {
              /* Yup, we can combine the two strings.
               */
              char *str1, *str2, *sum;
              int i;

              /* Retrieve both strings from the A_STRINGS area
               * and catenate them.
               */
              str1 = ((char**)(mem_block[A_STRINGS].block))
                [p[-3] | (p[-4]-(F_CSTRING0))<<8 ];
              str2 = ((char**)(mem_block[A_STRINGS].block))
                [p[-1] | (p[-2]-(F_CSTRING0))<<8 ];
              sum = xalloc(strlen(str1) + strlen(str2) + 1);
              strcpy(sum, str1);
              strcat(sum, str2);

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
              i = store_prog_string(make_shared_string(sum));
              xfree(sum);

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
              $$.type = TYPE_STRING;
          }
          else
          {
              /* Just add */
              ins_byte(F_ADD);
              $$.type = TYPE_ANY;
              if ($1.type == $4.type)
                  $$.type = $1.type;
              else if ($1.type == TYPE_STRING)
                  $$.type = TYPE_STRING;
              else if (($1.type == TYPE_NUMBER || $1.type == TYPE_FLOAT)
                     && $4.type == TYPE_STRING)
                  $$.type = TYPE_STRING;
              else if ($1.type == TYPE_FLOAT
                    && ($4.type == TYPE_NUMBER || $4.type == TYPE_ANY))
                  $$.type = TYPE_FLOAT;
              else if (($1.type == TYPE_NUMBER || $1.type == TYPE_ANY)
                    && $4.type == TYPE_FLOAT)
                  $$.type = TYPE_FLOAT;
          }
          $$.end = CURRENT_PROGRAM_SIZE;
      } /* '+' */

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr0 '-' expr0
      {
%line
          $$ = $1;
          $$.type = TYPE_ANY;

          if (exact_types)
          {
              vartype_t type1 = $1.type;
              vartype_t type2 = $3.type;

              if (type1 == type2)
              {
                  static char matchok[] =
%typemap TYPE_ANY:1,TYPE_NUMBER:1,TYPE_FLOAT:1,TYPE_MAPPING:1,TYPE_STRING:1

                  if ( type1 & (TYPE_MOD_POINTER|TYPE_MOD_REFERENCE)
                       ? (type1 & (TYPE_MOD_POINTER|TYPE_MOD_REFERENCE))
                         == TYPE_MOD_POINTER
                       : matchok[type1]
                       )
                  {
                      $$.type = type1;
                  }
                  else
                  {
                      type_error("Bad arguments to '-'", type1);
                  }
              }
              else if (   (type1 & (TYPE_MOD_POINTER|TYPE_MOD_REFERENCE))
                       == TYPE_MOD_POINTER)
              {
                  if ((type2 | TYPE_MOD_POINTER) == (TYPE_MOD_POINTER|TYPE_ANY)
                   || (   type2 & TYPE_MOD_POINTER
                       && type1 == (TYPE_MOD_POINTER|TYPE_ANY))
                     )
                  {
                      $$.type = type1;
                  }
                  else
                  {
                      yyerror("Arguments to '-' don't match");
                  }
              }
              else switch (type1)
              {
              case TYPE_ANY:
                  switch (type2)
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
                      if ( (type2 & (TYPE_MOD_POINTER|TYPE_MOD_REFERENCE)) ==
                             TYPE_MOD_POINTER)
                      {
                          $$.type = TYPE_ANY | TYPE_MOD_POINTER;
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
                  if (type2 == TYPE_FLOAT || type2 == TYPE_ANY)
                  {
                      $$.type = type2;
                  }
                  else
                  {
                      yyerror("Arguments to '-' don't match");
                  }
                  break;

              case TYPE_FLOAT:
                  if (type2 == TYPE_NUMBER || type2 == TYPE_ANY)
                  {
                      $$.type = TYPE_FLOAT;
                  }
                  else
                  {
                      yyerror("Arguments to '-' don't match");
                  }
                  break;

              case TYPE_STRING:
                  if (type2 == TYPE_STRING || type2 == TYPE_ANY)
                  {
                      $$.type = TYPE_STRING;
                  }
                  else
                  {
                      yyerror("Arguments to '-' don't match");
                  }
                  break;

              case TYPE_MAPPING:
                  if (type2 == TYPE_ANY)
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

          ins_byte(F_SUBTRACT);
          $$.end = CURRENT_PROGRAM_SIZE;
      } /* '-' */

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr0 '*' expr0
      {
          vartype_t type1, type2;

          $$ = $1;

          type1 = $1.type;
          type2 = $3.type;

          if (exact_types)
          {
              if (!BASIC_TYPE(type1, TYPE_NUMBER)
               && type1 != TYPE_FLOAT
               && type1 != TYPE_STRING
               && !(type1 & TYPE_MOD_POINTER)
                 )
                  type_error("Bad argument number 1 to '*'", type1);
              if (!BASIC_TYPE(type2, TYPE_NUMBER)
               && type2 != TYPE_FLOAT
               && type2 != TYPE_STRING
               && !(type2 & TYPE_MOD_POINTER)
                 )
                  type_error("Bad argument number 2 to '*'", type2);
          }

          ins_byte(F_MULTIPLY);
          $$.end = CURRENT_PROGRAM_SIZE;

          if (type1 == TYPE_FLOAT || type2 == TYPE_FLOAT )
          {
              $$.type = TYPE_FLOAT;
          }
          else if (type1 == TYPE_STRING || type2 == TYPE_STRING)
          {
              $$.type = TYPE_STRING;
          }
          else if (type1 & TYPE_MOD_POINTER)
          {
              $$.type = type1;
          }
          else if (type2 & TYPE_MOD_POINTER)
          {
              $$.type = type2;
          }
          else if (type1 == TYPE_ANY || type2 == TYPE_ANY)
          {
              $$.type = TYPE_ANY;
          }
          else
          {
              $$.type = TYPE_NUMBER;
          }
      } /* '*' */

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr0 '%' expr0
      {
          if (exact_types)
          {
              if (!BASIC_TYPE($1.type, TYPE_NUMBER))
                  type_error("Bad argument number 1 to '%'", $1.type);
              if (!BASIC_TYPE($3.type, TYPE_NUMBER))
                  type_error("Bad argument number 2 to '%'", $3.type);
          }

          $$ = $1;
          ins_byte(F_MOD);
          $$.end = CURRENT_PROGRAM_SIZE;
          $$.type = TYPE_NUMBER;
      }
    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr0 '/' expr0
      {
          vartype_t type1, type2;

          $$ = $1;

          type1 = $1.type;
          type2 = $3.type;

          if (exact_types)
          {
              if ( !BASIC_TYPE(type1, TYPE_NUMBER) && type1 != TYPE_FLOAT)
                  type_error("Bad argument number 1 to '/'", type1);
              if ( !BASIC_TYPE(type2, TYPE_NUMBER) && type2 != TYPE_FLOAT)
                  type_error("Bad argument number 2 to '/'", type2);
          }

          ins_byte(F_DIVIDE);
          $$.end = CURRENT_PROGRAM_SIZE;

          if (type1 == TYPE_FLOAT || type2 == TYPE_FLOAT )
          {
              $$.type = TYPE_FLOAT;
          }
          else
          {
              $$.type = TYPE_NUMBER;
          }
      } /* '/' */

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | decl_cast expr0 %prec '~'
      {
          $$ = $2;
          $$.type = $1;
          if (exact_types
           && $2.type != TYPE_ANY
           && $2.type != TYPE_UNKNOWN
           && $1 != TYPE_VOID
             )
              type_error("Casts are only legal for type mixed, or when unknown", $2.type);
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | cast expr0 %prec '~'
      {
          $$ = $2;
          $$.type = $1;
          if ($2.type != TYPE_ANY
           && $2.type != TYPE_UNKNOWN
           && $1 != TYPE_VOID
           && $1 != $2.type
             )
          {
              switch($1)
              {
              default:
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
              if ($2.type == $1)
              {   if ($2.type != TYPE_ANY)
                      yywarnf("casting a value to its own type: %s"
                             , get_type_name($1));
              }
              else if ($2.type != TYPE_UNKNOWN && $2.type != TYPE_ANY)
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

          int i;
          PREPARE_INSERT(4)
%line
          $$.start = $1.start;

          i = verify_declared($2);

          if (i != -1)
          {
              if (i & VIRTUAL_VAR_TAG)
              {
                  add_byte(F_PUSH_VIRTUAL_VARIABLE_LVALUE);
                  add_byte(i);
                  i = V_VARIABLE(i)->flags & TYPE_MOD_MASK;
              }
              else
              {
                  if ((i + num_virtual_variables) & ~0xff)
                  {
                      add_byte(F_PUSH_IDENTIFIER16_LVALUE);
                      add_short(i + num_virtual_variables);
                      CURRENT_PROGRAM_SIZE += 1;
                  }
                  else
                  {
                      add_byte(F_PUSH_IDENTIFIER_LVALUE);
                      add_byte(i + num_virtual_variables);
                  }
                  i = NV_VARIABLE(i)->flags & TYPE_MOD_MASK;
              }

              if (exact_types
               && !BASIC_TYPE(i, TYPE_NUMBER)
               && !BASIC_TYPE(i, TYPE_FLOAT))
              {
                  argument_type_error($1.code, i);
              }

              CURRENT_PROGRAM_SIZE += 2;
          }
          else
          {
              /* Variable not declared - try to recover */
              YYACCEPT;
          }

          last_expression = CURRENT_PROGRAM_SIZE;

          CURRENT_PROGRAM_SIZE += 1;

          add_byte($1.code);
          $$.end = CURRENT_PROGRAM_SIZE;
          $$.type = i;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | pre_inc_dec L_LOCAL %prec L_INC
      {
          int i;
          PREPARE_INSERT(3)
%line
          $$.start = $1.start;

          add_byte(F_PUSH_LOCAL_VARIABLE_LVALUE);
          add_byte($2);
          CURRENT_PROGRAM_SIZE =
            (last_expression = CURRENT_PROGRAM_SIZE + 2) + 1;
          add_byte($1.code);
          i = type_of_locals[$2];
          if (exact_types
           && !BASIC_TYPE(i, TYPE_NUMBER)
           && !BASIC_TYPE(i, TYPE_FLOAT))
          {
              argument_type_error($1.code, i);
          }
          $$.type = i;
          $$.end = CURRENT_PROGRAM_SIZE;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | pre_inc_dec expr4 index_expr %prec '['
      {
          mp_uint current;
          bytecode_p p;
          int start, restype;
%line
          if ($3.type1 & TYPE_MOD_REFERENCE)
              yyerror("Reference used as index");

          $$.start = $1.start;

          restype = TYPE_ANY;

          /* Check the types */
          if (exact_types)
          {
              vartype_t type;

              type = $2.type;
              if (type & TYPE_MOD_POINTER)
              {
                  if (type != (TYPE_MOD_POINTER|TYPE_ANY)
                   && type != (TYPE_MOD_POINTER|TYPE_NUMBER) )
                      argument_type_error($1.code, type);
              }
              else switch (type)
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
                  if (!BASIC_TYPE($3.type1, TYPE_NUMBER))
                      type_error("Bad type of index", $3.type1);
                  restype = TYPE_NUMBER;
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
                      yyerrorf("Out of memory: program size %lu\n"
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
                  *p++ = ($3.inst == F_INDEX) ? F_INDEX_LVALUE
                                              : F_RINDEX_LVALUE;
              }
              else
              {
                  int i;
                  int length;

                  if (!realloc_a_program(2))
                  {
                      yyerrorf("Out of memory: program size %lu\n", current+2);
                      YYACCEPT;
                  }
                  p = PROGRAM_BLOCK + start;
                  i = p[1];
                  length = current - start - 2;
                  for( ; --length >= 0; p++)
                      *p = p[2];
                  *p++ = $2.code;
                  *p++ = i;
                  *p++ = ($3.inst == F_INDEX) ? F_INDEX_LVALUE
                                              : F_RINDEX_LVALUE;
              }
          }
          else
          {
              if (!realloc_a_program(2))
              {
                  yyerrorf("Out of memory: program size %lu\n", current+2);
                  YYACCEPT;
              }
              p = PROGRAM_BLOCK + start;
              *p++ = ($3.inst == F_INDEX) ? F_PUSH_INDEXED_LVALUE
                                          : F_PUSH_RINDEXED_LVALUE;
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
          if ($4.type & TYPE_MOD_REFERENCE
           || $6.type & TYPE_MOD_REFERENCE)
              yyerror("Reference used as index");

          $$.start = $1.start;

          /* Check the types */
          if (exact_types)
          {
              vartype_t type;

              type = $2.type;
              switch (type)
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
              yyerrorf("Out of memory: program size %lu\n", current+2);
              YYACCEPT;
          }
          p = PROGRAM_BLOCK + current;
          *p++ = F_PUSH_INDEXED_MAP_LVALUE;

          /* Finally store the actual instruction */
          *p = $1.code;
          last_expression = current + 1;
          CURRENT_PROGRAM_SIZE = current + 2;
          $$.type = TYPE_ANY;
          $$.end = CURRENT_PROGRAM_SIZE;
      } /* pre_inc_dec expr4 [expr0 ',' expr0] */

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | L_NOT expr0
      {
          $$ = $2;
          last_expression = CURRENT_PROGRAM_SIZE;
          ins_byte(F_NOT);        /* Any type is valid here. */
          $$.type = TYPE_NUMBER;
          $$.end = CURRENT_PROGRAM_SIZE;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '~' expr0
      {
%line
          $$ = $2;
          ins_byte(F_COMPL);
          if (exact_types && !BASIC_TYPE($2.type, TYPE_NUMBER))
              type_error("Bad argument to ~", $2.type);
          $$.type = TYPE_NUMBER;
          $$.end = CURRENT_PROGRAM_SIZE;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '-' expr0 %prec '~'
      {
          vartype_t type;
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
              mem_block[A_PROGRAM].block[last_expression] =
                F_NNUMBER;
          }
          else
          {
              ins_byte(F_NEGATE);
          }
          $$.end = CURRENT_PROGRAM_SIZE;

          type = $2.type;
          if (exact_types
           && !BASIC_TYPE(type, TYPE_NUMBER)
           && type != TYPE_FLOAT )
              type_error("Bad argument to unary '-'", type);
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | lvalue L_INC %prec L_INC
      {
%line
          $$.start = CURRENT_PROGRAM_SIZE;

          /* Create the code to push the lvalue plus POST_INC */
          if ($1.length)
          {
              add_to_mem_block(A_PROGRAM, $1.u.p, $1.length);
              yfree($1.u.p);
              last_expression = CURRENT_PROGRAM_SIZE;
              ins_byte(F_POST_INC);
          }
          else
          {
              PREPARE_INSERT(3)
              bytecode_p source;

              CURRENT_PROGRAM_SIZE =
                  (last_expression = CURRENT_PROGRAM_SIZE+2) + 1;
              source = $1.u.simple;
              add_byte(*source++);
              add_byte(*source);
              add_byte(F_POST_INC);
          }

          /* Check the types */
          if (exact_types
           && !BASIC_TYPE($1.type, TYPE_NUMBER)
           && !BASIC_TYPE($1.type, TYPE_FLOAT)
             )
              type_error("Bad argument to ++", $1.type);

          $$.end = CURRENT_PROGRAM_SIZE;
          $$.type = $1.type;
      } /* post-inc */

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | lvalue L_DEC %prec L_DEC
      {
%line
          $$.start = CURRENT_PROGRAM_SIZE;

          /* Create the code to push the lvalue plus POST_DEC */
          if ($1.length)
          {
              add_to_mem_block(A_PROGRAM, $1.u.p, $1.length+1);
              yfree($1.u.p);
              mem_block[A_PROGRAM].block[
                last_expression = CURRENT_PROGRAM_SIZE-1
              ] = F_POST_DEC;
          }
          else
          {
              PREPARE_INSERT(3)
              bytecode_p source;

              CURRENT_PROGRAM_SIZE =
                (last_expression = CURRENT_PROGRAM_SIZE+2) + 1;
              source = $1.u.simple;
              add_byte(*source++);
              add_byte(*source);
              add_byte(F_POST_DEC);
          }

          /* Check the types */
          if (exact_types
           && !BASIC_TYPE($1.type, TYPE_NUMBER)
           && !BASIC_TYPE($1.type, TYPE_FLOAT)
             )
              type_error("Bad argument to --", $1.type);

          $$.end = CURRENT_PROGRAM_SIZE;
          $$.type = $1.type;
      } /* post-dec */

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr4 { $$ = $1; }

; /* expr0 */


pre_inc_dec:
      L_INC { $$.code = F_PRE_INC; $$.start = CURRENT_PROGRAM_SIZE; }
    | L_DEC { $$.code = F_PRE_DEC; $$.start = CURRENT_PROGRAM_SIZE; }
;


expr4:
      function_call  %prec '~'
    | inline_fun
    | catch          %prec '~'
    | sscanf         %prec '~'
%ifdef SUPPLY_PARSE_COMMAND
    | parse_command  %prec '~'
%endif /* SUPPLY_PARSE_COMMAND */

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | L_STRING
      {
          /* Push a constant string */

          int string_number;
          PREPARE_INSERT(3)
          char *p;
%line
          p = last_lex_string;
          last_lex_string = NULL;
          $$.start = last_expression = CURRENT_PROGRAM_SIZE;
          $$.type = TYPE_STRING;
          $$.code = -1;

          string_number = store_prog_string(p);
          if ( string_number <= 0xff )
          {
              add_byte(F_CSTRING0);
              add_byte(string_number);
          }
          else if ( string_number <= 0x1ff )
          {
              add_byte(F_CSTRING1);
              add_byte(string_number);
          }
          else if ( string_number <= 0x2ff )
          {
              add_byte(F_CSTRING2);
              add_byte(string_number);
          }
          else if ( string_number <= 0x3ff )
          {
              add_byte(F_CSTRING3);
              add_byte(string_number);
          }
          else
          {
              add_byte(F_STRING);
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
              add_byte(F_CONST0);
              $$.type = TYPE_ANY;
              /* TODO: TYPE_NULL would be better */
          }
          else if ( number == 1 )
          {
              add_byte(F_CONST1);
              current++;
              $$.type = TYPE_NUMBER;
          }
          else if ( number >= 0 && number <= 0xff )
          {
              add_byte(F_CLIT);
              add_byte(number);
              current += 2;
              $$.type = TYPE_NUMBER;
          }
          else
          {
              add_byte(F_NUMBER);
              memcpy(__PREPARE_INSERT__p, &$1, sizeof $1);
              current += 1 + sizeof (p_int);
              $$.type = TYPE_NUMBER;
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
          ins_byte(F_CLOSURE);
          ins_short(ix);
          ins_short(inhIndex);
          $$.type = TYPE_CLOSURE;
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
                ins_byte(F_CSTRING0);
                ins_byte(string_number);
                ins_byte(F_QUOTE);
          }
          else
          {
                ins_byte(F_SYMBOL);
                ins_short(string_number);
                ins_byte(quotes);
          }
          $$.type = TYPE_SYMBOL;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | L_FLOAT
      {
          /* Generate a float literal */

          int exponent;

          $$.start = CURRENT_PROGRAM_SIZE;
          $$.code = -1;
          if ($1 == 0.0)
              ins_byte(F_FCONST0);
          else
          {
              ins_byte(F_FLOAT);
              ins_long ( SPLIT_DOUBLE( $1, &exponent) );
              ins_short( exponent );
          }
          $$.type = TYPE_FLOAT;
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

          ins_byte(F_AGGREGATE);
          ins_short($4);
          if (max_array_size && $4 > max_array_size)
              yyerror("Illegal array size");
          $$.type = TYPE_MOD_POINTER | TYPE_ANY;
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

          ins_byte(F_AGGREGATE);
          ins_short($3);
          if (max_array_size && $3 > max_array_size)
              yyerror("Illegal array size");
          $$.type = TYPE_QUOTED_ARRAY;
          $$.start = $2.start;
          $$.code = -1;
          quotes = $1;
          do {
                ins_byte(F_QUOTE);
          } while (--quotes);
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '(' '[' ':' note_start

          /* Generate an empty mapping of given width */

      {
          ins_byte(F_CONST0);
      }

      expr0 ']' ')'

      {
          ins_f_code(F_M_ALLOCATE);

          $$.type = TYPE_MAPPING;
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
              ins_byte(F_M_AGGREGATE);
              ins_short(num_keys);
              ins_short($4[1]);
          }
          else
          {
              ins_byte(F_M_CAGGREGATE);
              ins_byte(num_keys);
              ins_byte($4[1]);
          }

          $$.type = TYPE_MAPPING;
          $$.start = $3.start;
          $$.code = -1;
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
          if (exact_types)
          {
              vartype_t type;

              $$.type = type = $1.type;
              if ((type & TYPE_MOD_POINTER) == 0
               && type != TYPE_ANY && type != TYPE_STRING)
              {
                  type_error("Bad type of argument used for range", type);
                  $$.type = TYPE_ANY;
              }
              type = $2.type1;
              if (type != TYPE_ANY && type != TYPE_NUMBER)
                  type_error("Bad type of index", type);
              type = $2.type2;
              if (type != TYPE_ANY && type != TYPE_NUMBER)
                  type_error("Bad type of index", type);
          }
          else
          {
              $$.type = TYPE_ANY;
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
%line
          i = verify_declared($2);

          $$.start = current = CURRENT_PROGRAM_SIZE;
          $$.code = -1;

          if (!realloc_a_program(3))
          {
              yyerrorf("Out of memory: program size %lu\n", current+3);
              YYACCEPT;
          }
          p = PROGRAM_BLOCK + current;

          if (i & VIRTUAL_VAR_TAG)
          {
              *p++ = F_PUSH_VIRTUAL_VARIABLE_LVALUE;
              *p = i;
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
          }

          CURRENT_PROGRAM_SIZE = current + 2;
          if (i == -1)
              $$.type = TYPE_ANY | TYPE_MOD_REFERENCE;
          else
              $$.type = (VARIABLE(i)->flags & TYPE_MOD_MASK) |
                  TYPE_MOD_REFERENCE;
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
          $$.start = current = CURRENT_PROGRAM_SIZE;
          $$.code = -1;
          if (!realloc_a_program(2))
          {
              yyerrorf("Out of memory: program size %lu\n", current+2);
              YYACCEPT;
          }
          p = PROGRAM_BLOCK + current;
          *p++ = F_PUSH_LOCAL_VARIABLE_LVALUE;
          *p = $2;
          CURRENT_PROGRAM_SIZE = current + 2;
          $$.type = type_of_locals[$2] | TYPE_MOD_REFERENCE;
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
          else
              arrange_protected_lvalue($3.start, $3.code, $3.end,
                 F_PROTECTED_RINDEX_LVALUE
              );

          $$.start = $3.start;
          $$.code = -1;

          if ($4.type1 & TYPE_MOD_REFERENCE)
                yyerror("Reference used as index");

          /* Compute the result type */
          if (!exact_types)
          {
                $$.type = TYPE_ANY | TYPE_MOD_REFERENCE;
          }
          else
          {
              vartype_t type;

              type = $3.type;
              if (type & TYPE_MOD_POINTER)
              {
                    $$.type = type & ~TYPE_MOD_POINTER;
              }
              else if (type == TYPE_MAPPING && $4.inst == F_INDEX)
              {
                  $4.type1 = TYPE_ANY;
                  $$.type = TYPE_ANY | TYPE_MOD_REFERENCE;
              }
              else switch (type)
              {
              default:
                  type_error("Bad type to indexed reference", type);
                  /* FALLTHROUGH */
              case TYPE_ANY:
                  if ($4.inst == F_INDEX)
                      $4.type1 = TYPE_ANY;
                  $$.type = TYPE_ANY | TYPE_MOD_REFERENCE;
                  break;
              case TYPE_STRING:
                  $$.type = TYPE_NUMBER | TYPE_MOD_REFERENCE;
                  break;
              }
              if (!BASIC_TYPE($4.type1, TYPE_NUMBER))
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
          $$.type = TYPE_ANY | TYPE_MOD_REFERENCE;
          ins_f_code(F_PUSH_PROTECTED_INDEXED_MAP_LVALUE);

          if ($5.type & TYPE_MOD_REFERENCE)
                yyerror("Reference used as index");

          /* Compute the result type */
          if (exact_types)
          {
              vartype_t type;

              type = $3.type;
              if (type != TYPE_ANY && type != TYPE_MAPPING)
              {
                  type_error("Bad type to indexed value", type);
              }
              type = $7.type;
              if (type != TYPE_ANY && type != TYPE_NUMBER)
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
          case F_NX_RANGE: prot_op = F_PROTECTED_NX_RANGE_LVALUE; break;
          case F_RX_RANGE: prot_op = F_PROTECTED_RX_RANGE_LVALUE; break;
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
          if (!exact_types)
          {
              $$.type = TYPE_ANY | TYPE_MOD_REFERENCE;
          }
          else
          {
              vartype_t type;

              $$.type = type = $3.type;
              if ((type & TYPE_MOD_POINTER) == 0
               && type != TYPE_ANY && type != TYPE_STRING)
              {
                  type_error("Bad type of argument used for range", type);
                  $$.type = TYPE_ANY;
              }
              type = $4.type1;
              if (type != TYPE_ANY && type != TYPE_NUMBER)
                  type_error("Bad type of index", type);
              type = $4.type2;
              if (type != TYPE_ANY && type != TYPE_NUMBER)
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
%line
          i = verify_declared($1);

          $$.start = current = CURRENT_PROGRAM_SIZE;
          $$.end = 0;

          if (!realloc_a_program(3))
          {
              yyerrorf("Out of memory: program size %lu\n", current+3);
              YYACCEPT;
          }
          p = PROGRAM_BLOCK + current;

          if (i & VIRTUAL_VAR_TAG)
          {
              /* Access a virtual variable */

              $$.code = F_PUSH_VIRTUAL_VARIABLE_LVALUE;
              *p++ = F_VIRTUAL_VARIABLE;
              *p = i;
              $$.type = V_VARIABLE(i)->flags & TYPE_MOD_MASK;
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
              $$.type = NV_VARIABLE(i)->flags & TYPE_MOD_MASK;
          }

          CURRENT_PROGRAM_SIZE = current + 2;
          if (i == -1)
              $$.type = TYPE_ANY;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | L_LOCAL
      {
          /* Access a local variable */

          mp_uint current;
          bytecode_p p;
%line
          $$.start = current = CURRENT_PROGRAM_SIZE;
          $$.code = F_PUSH_LOCAL_VARIABLE_LVALUE;
          $$.end = 0;
          if (!realloc_a_program(2))
          {
              yyerrorf("Out of memory: program size %lu\n", current+2);
              YYACCEPT;
          }
          p = PROGRAM_BLOCK + current;
          *p++ = F_LOCAL;
          *p = $1;
          CURRENT_PROGRAM_SIZE = current + 2;
          $$.type = type_of_locals[$1];
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
              ins_byte(F_INDEX);
          }
          else
          {
              $$.code = F_PUSH_RINDEXED_LVALUE;
              ins_byte(F_RINDEX);
          }

          if ($2.type1 & TYPE_MOD_REFERENCE)
              yyerror("Reference used as index");

          /* Check and compute the types */
          if (exact_types)
          {
              int type;

              type = $1.type;
              if (type & TYPE_MOD_POINTER)
              {
                  $$.type = type & ~TYPE_MOD_POINTER;
              }
              else if (type == TYPE_MAPPING && $2.inst == F_INDEX)
              {
                  $2.type1 = TYPE_ANY;
                  $$.type = TYPE_ANY;
              }
              else switch (type)
              {
              default:
                  type_error("Bad type to indexed value", type);
                  /* FALLTHROUGH */
              case TYPE_ANY:
                  if ($2.inst == F_INDEX)
                      $2.type1 = TYPE_ANY;
                  $$.type = TYPE_ANY;
                  break;
              case TYPE_STRING:
                  $$.type = TYPE_NUMBER;
                  break;
              }
              if (!BASIC_TYPE($2.type1, TYPE_NUMBER))
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
          $$.type = TYPE_ANY;
          ins_byte(F_MAP_INDEX);

          if ($3.type & TYPE_MOD_REFERENCE)
              yyerror("Reference used as index");

          /* Check and compute types */
          if (exact_types)
          {
              vartype_t type;

              type = $1.type;
              if (type != TYPE_ANY && type != TYPE_MAPPING)
              {
                  type_error("Bad type to indexed value", type);
              }
              type = $5.type;
              if (type != TYPE_ANY && type != TYPE_NUMBER)
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
                  q[current - start] = ($2.inst == F_INDEX)
                                        ? F_INDEX_LVALUE
                                        : F_RINDEX_LVALUE;
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
                  *p = ($2.inst == F_INDEX)
                        ? F_INDEX_LVALUE
                        : F_RINDEX_LVALUE;
              }
          }
          else
          {
              /* We can just copy the instruction block
               * and add a PUSH_(R)INDEXED_LVALUE
               */
              memcpy(q, p + start, current - start);
              q[current - start] = ($2.inst == F_INDEX)
                                    ? F_PUSH_INDEXED_LVALUE
                                    : F_PUSH_RINDEXED_LVALUE;
          }

          /* This is what we return */
          $$.length = current + 1 - start;
          $$.u.p = q;

          CURRENT_PROGRAM_SIZE = start;
          last_expression = -1;

          if ($2.type1 & TYPE_MOD_REFERENCE)
              yyerror("Reference used as index");

          /* Check and compute types */
          if (exact_types)
          {
              vartype_t type;

              type = $1.type;
              if (type & TYPE_MOD_POINTER)
              {
                  $$.type = type & ~TYPE_MOD_POINTER;
              }
              else if (type == TYPE_MAPPING && $2.inst == F_INDEX)
              {
                  $2.type1 = TYPE_ANY;
                  $$.type = TYPE_ANY;
              }
              else switch (type)
              {
              default:
                  type_error("Bad type to indexed lvalue", type);
                  /* FALLTHROUGH */
              case TYPE_ANY:
                  if ($2.inst == F_INDEX)
                      $2.type1 = TYPE_ANY;
                  $$.type = TYPE_ANY;
                  break;
              case TYPE_STRING:
                  $$.type = TYPE_NUMBER;
                  break;
              }
              if (!BASIC_TYPE($2.type1, TYPE_NUMBER))
                  type_error("Bad type of index", $2.type1);
          }
          else
          {
              $$.type = TYPE_ANY;
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
          $$.type = TYPE_ANY;
          CURRENT_PROGRAM_SIZE = start;
          last_expression = -1;

          if ($3.type & TYPE_MOD_REFERENCE)
              yyerror("Reference used as index");

          /* Check and compute types */
          if (exact_types)
          {
              vartype_t type;

              type = $1.type;
              if (type != TYPE_ANY && type != TYPE_MAPPING)
              {
                  type_error("Bad type to indexed value", type);
              }
              type = $5.type;
              if (type != TYPE_ANY && type != TYPE_NUMBER)
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
          case F_NX_RANGE: indexing_code = F_NX_RANGE_LVALUE; break;
          case F_RX_RANGE: indexing_code = F_RX_RANGE_LVALUE; break;
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
              if (indexing_code > 0xff)
              {
                  q[current++] = indexing_code >> F_ESCAPE_BITS;
              }
              q[current] = indexing_code;
          }

          /* This is what we return */
          $$.length = current + 1;
          $$.u.p = q;

          CURRENT_PROGRAM_SIZE = start;
          last_expression = -1;

          /* Compute and check the types */
          if (exact_types)
          {
              vartype_t type;

              $$.type = type = $1.type;
              if ((type & TYPE_MOD_POINTER) == 0
               &&  type != TYPE_ANY && type != TYPE_STRING)
              {
                  type_error("Bad type of argument used for range", type);
                  $$.type = TYPE_ANY;
              }
              type = $2.type1;
              if (type != TYPE_ANY && type != TYPE_NUMBER)
                  type_error("Bad type of index", type);
              type = $2.type2;
              if (type != TYPE_ANY && type != TYPE_NUMBER)
                  type_error("Bad type of index", type);
          }
      }

; /* lvalue */


name_lvalue:
    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    L_IDENTIFIER
      {
          /* Generate the lvalue for a global */

          int i;
%line
          $$.length = 0;
          i = verify_declared($1);
          if (i & VIRTUAL_VAR_TAG)
          {
              $$.u.simple[0] = F_PUSH_VIRTUAL_VARIABLE_LVALUE;
              $$.u.simple[1] = i;
              $$.type = V_VARIABLE(i)->flags & TYPE_MOD_MASK;
              if (i == -1)
                  $$.type = TYPE_ANY;
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
                  $$.type = NV_VARIABLE(i)->flags & TYPE_MOD_MASK;
              }
              else
              {
                  $$.u.simple[0] = F_PUSH_IDENTIFIER_LVALUE;
                  $$.u.simple[1] = i + num_virtual_variables;
              }
              $$.type = NV_VARIABLE(i)->flags & TYPE_MOD_MASK;
          }
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | L_LOCAL
      {
%line
          /* Generate the lvalue for a local */

          $$.u.simple[0] = F_PUSH_LOCAL_VARIABLE_LVALUE;
          $$.u.simple[1] = $1;
          $$.length = 0;
          $$.type = type_of_locals[$1];
      }
; /* name_lvalue */


local_name_lvalue:
      basic_type new_local_name
       { $$ = $2; }
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
        }

    | '[' '<' expr0 ']'
        {
            $$.inst = F_RINDEX;
            $$.start = $3.start;
            $$.end = $3.end;
            $$.type1 = $3.type;
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
              yyerrorf("Out of memory: program size %lu\n", current+1);
              YYACCEPT;
          }

          mark = PROGRAM_BLOCK + $3.start;
          p = PROGRAM_BLOCK + current;
          length = current - $3.start;
          for( ; --length >= 0; p--) PUT_CODE(p, GET_CODE(p-1));
          STORE_CODE(mark, F_CONST0);
          CURRENT_PROGRAM_SIZE++;
          $3.end++;

          /* Return the data */

          $$.inst  = F_RANGE;
          $$.start = $3.start;
          $$.end   = $3.end;
          $$.type1 = TYPE_NUMBER;
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
              yyerrorf("Out of memory: program size %lu\n", current+1);
              YYACCEPT;
          }

          mark = PROGRAM_BLOCK + $4.start;
          p = PROGRAM_BLOCK + current;
          length = current - $4.start;
          for( ; --length >= 0; p--) PUT_CODE(p, GET_CODE(p-1));
          STORE_CODE(mark, F_CONST0);
          CURRENT_PROGRAM_SIZE++;
          $4.end++;

          /* Return the data */

          $$.inst  = F_NR_RANGE;
          $$.start = $4.start;
          $$.end   = $4.end;
          $$.type1 = TYPE_NUMBER;
          $$.type2 = $4.type;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '['     expr0 L_RANGE     expr0 ']'
      {
          $$.inst  = F_RANGE;
          $$.start = $2.start;
          $$.end   = $4.end;
          $$.type1 = $2.type;
          $$.type2 = $4.type;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '['     expr0 L_RANGE '<' expr0 ']'
      {
          $$.inst  = F_NR_RANGE;
          $$.start = $2.start;
          $$.end   = $5.end;
          $$.type1 = $2.type;
          $$.type2 = $5.type;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '[' '<' expr0 L_RANGE     expr0 ']'
      {
          $$.inst  = F_RN_RANGE;
          $$.start = $3.start;
          $$.end   = $5.end;
          $$.type1 = $3.type;
          $$.type2 = $5.type;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '[' '<' expr0 L_RANGE '<' expr0 ']'
      {
          $$.inst  = F_RR_RANGE;
          $$.start = $3.start;
          $$.end   = $6.end;
          $$.type1 = $3.type;
          $$.type2 = $6.type;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '['     expr0 L_RANGE           ']'
      {
          $$.inst  = F_NX_RANGE;
          $$.start = $2.start;
          $$.end   = $2.end;
          $$.type1 = $2.type;
          $$.type2 = TYPE_NUMBER;
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | '[' '<' expr0 L_RANGE           ']'
      {
          /* Simulate an expression yielding <1 for the upper bound.
           * We pretend that it's part of the lower bound expr.
           */

          $$.inst  = F_RX_RANGE;
          $$.start = $3.start;
          $$.end   = $3.end;
          $$.type1 = $3.type;
          $$.type2 = TYPE_NUMBER;
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

expr_list3:
      /* empty */           { $$ = 0; }
    | expr0                 { $$ = 1;      add_arg_type($1.type); }
    | expr_list2 ',' expr0  { $$ = $1 + 1; add_arg_type($3.type); }
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

          real_name = $1.real;
            /* we rely on the fact that $1.real->type is either
             * I_TYPE_UNKNOWN or I_TYPE_GLOBAL here. All others are filtered
             * by the lexical analysis.
             */

          if (real_name->type == I_TYPE_UNKNOWN)
          {
              /* prevent freeing by exotic name clashes */
              /* also makes life easier below */
              real_name->type = I_TYPE_GLOBAL;
              real_name->u.global.function = I_GLOBAL_FUNCTION_VAR;
              real_name->u.global.variable = I_GLOBAL_VARIABLE_OTHER;
              real_name->u.global.efun     = I_GLOBAL_EFUN_OTHER;
              real_name->u.global.sim_efun = I_GLOBAL_SEFUN_OTHER;
              real_name->next_all = all_globals;
              all_globals = real_name;
          }
          else if (!$1.super
                && real_name->u.global.function < 0
                && real_name->u.global.sim_efun >= 0)
          {
              /* It's a real simul-efun */

              $<function_call_head>$.simul_efun = real_name->u.global.sim_efun;

              if (real_name->u.global.sim_efun & ~0xff)
              {
                  /* The simul-efun has to be called by name:
                   * prepare the extra args for the call_other
                   */
                  PREPARE_INSERT(6)
                  char *p;

                  p = ref_string(real_name->name);
                  add_byte(F_STRING);
                  add_short(store_prog_string(
                    make_shared_string(query_simul_efun_file_name())));
                  add_byte(F_STRING);
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
          PREPARE_INSERT(6)
          int        f = 0;             /* Function index */
          Bool       efun_override;     /* TRUE on explicite efun calls */
          int        simul_efun;
          vartype_t *arg_types = NULL;  /* Argtypes from the program */
          int        first_arg;         /* Startindex in arg_types[] */

          efun_override = ($1.super && strcmp($1.super, "efun") == 0);

          $$.start = $<function_call_head>2.start;
          $$.code = -1;

          if ( $4 >= 0xff )
              /* since num_arg is encoded in just one byte, and 0xff
               * is taken for SIMUL_EFUN_VARARG */
              yyerrorf("Too many arguments to function");

          if ( (simul_efun = $<function_call_head>2.simul_efun) >= 0)
          {
              /* SIMUL EFUN */

              function_t *funp;

              funp = &simul_efunp[simul_efun];

              if (funp->num_arg != SIMUL_EFUN_VARARGS
               && !(funp->flags & TYPE_MOD_XVARARGS))
              {
                  if ($4 > funp->num_arg)
                      yyerrorf("Too many arguments to simul_efun %s"
                              , funp->name);

                  if ($4 < funp->num_arg)
                  {
                      if (pragma_pedantic)
                          yyerrorf("Missing arguments to simul_efun %s"
                                  , funp->name);
                      else
                          yywarnf("Missing arguments to simul_efun %s"
                                 , funp->name);
                  }
              }

              if (simul_efun & ~0xff)
              {
                  /* call-other: the number of arguments will be
                   * corrected at runtime.
                   */
                  add_byte(F_CALL_OTHER);
                  add_byte($4 + 2);
                  CURRENT_PROGRAM_SIZE += 2;
              }
              else
              {
                  /* Direct call: we have to add the missing arguments.
                   * resp. encode the number of arguments passed.
                   */
                  if (funp->num_arg != SIMUL_EFUN_VARARGS
                   && !(funp->flags & TYPE_MOD_XVARARGS))
                  {
                      int i;

                      i = funp->num_arg - $4;
                      if (funp->flags & TYPE_MOD_XVARARGS)
                          i--; /* Last argument may be omitted */

                      if (i > 4)
                      {
                          if (!realloc_a_program(i+2))
                          {
                              yyerrorf("Out of memory: program size %lu\n"
                                      , mem_block[A_PROGRAM].current_size + i+2);
                              YYACCEPT;
                          }

                          __PREPARE_INSERT__p = PROGRAM_BLOCK
                                                + CURRENT_PROGRAM_SIZE;
                      }
                      CURRENT_PROGRAM_SIZE += i;
                      while ( --i >= 0 )
                      {
                          add_byte(F_CONST0);
                      }
                  }

                  add_byte(F_SIMUL_EFUN);
                  add_byte(simul_efun);
                  if (funp->num_arg == SIMUL_EFUN_VARARGS
                   || funp->flags & TYPE_MOD_XVARARGS)
                  {
                      add_byte($4);
                      CURRENT_PROGRAM_SIZE += 3;
                  }
                  else
                  {
                      CURRENT_PROGRAM_SIZE += 2;
                  }
              }
              $$.type = funp->type & TYPE_MOD_MASK;
          } /* if (simul-efun) */

          else if ($1.super ? !efun_override
                            : (f = defined_function($1.real)) >= 0
                  )
          {
              /* LFUN or INHERITED LFUN */
              function_t *funp;
              function_t  inherited_function;

              if ($1.super)
              {
                  /* Inherited lfun: check its existance and call it */

                  program_t *super_prog;
                  int ix;

                  ix = insert_inherited( $1.super, $1.real->name
                                       , &super_prog, &inherited_function
                                       , $4, (bytecode_p)__PREPARE_INSERT__p
                                       );

                  if ($1.real->type == I_TYPE_UNKNOWN)
                  {
                      free_shared_identifier($1.real);
                  }

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
                          /* Not an error, but we can't do argument
                           * checks either.
                           */
                          break;
                      default:
                          fatal("Unknown return code %d from insert_inherited()\n", ix);
                          break;
                      }

                      $$.type = TYPE_ANY;
                      if ($1.super)
                          yfree($1.super);
                      pop_arg_stack($4);  /* Argument types no longer needed */
                      break; /* TODO: this assumes a switch by byacc */
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

                  add_byte(F_CALL_FUNCTION_BY_ADDRESS);
                  add_short(f);
                  funp = FUNCTION(f);
                  arg_types = (vartype_t *)mem_block[A_ARGUMENT_TYPES].block;
                  first_arg = ARGUMENT_INDEX(f);
                  add_byte($4);        /* Actual number of arguments */
                  CURRENT_PROGRAM_SIZE += 4;
              }

              /* Verify that the function has been defined already.
               * For inherited functions this is a no-brainer.
               */
              if (funp->flags & (NAME_UNDEFINED|NAME_HIDDEN))
              {
                  if ( !(funp->flags & (NAME_PROTOTYPE|NAME_INHERITED))
                   && exact_types )
                  {
                      yyerrorf("Function %.50s undefined", funp->name);
                  }
                  else if ((funp->flags
                            & (NAME_UNDEFINED|NAME_PROTOTYPE|NAME_HIDDEN))
                           == NAME_HIDDEN)
                  {
                      yyerrorf("Function %.50s is private", funp->name);
                  }
              }

              $$.type = funp->type & TYPE_MOD_MASK; /* Result type */

              /* Check number of arguments.
               */
              if (funp->num_arg != $4
               && !(funp->flags & TYPE_MOD_VARARGS)
               && (first_arg != INDEX_START_NONE)
               && exact_types)
              {
                  if (funp->num_arg-1 > $4 || !(funp->flags & TYPE_MOD_XVARARGS))
                    yyerrorf("Wrong number of arguments to %.60s", $1.real->name);
              }

              /* Check the argument types.
               */
              if (exact_types && first_arg != INDEX_START_NONE)
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

                          tmp1 = *argp++ & TYPE_MOD_RMASK;
                          tmp2 = *arg_types++ & TYPE_MOD_MASK;
                          if (!REDEFINED_TYPE(tmp1, tmp2))
                          {
                              yyerrorf("Bad type for argument %d of %s %s",
                                argno,
                                funp->name,
                                get_two_types(tmp2, tmp1));
                          }
                      } /* for (all args) */

                      if (funp->flags & TYPE_MOD_XVARARGS)
                      {
                          fulltype_t tmp1, tmp2;
                          /* varargs argument is either a pointer type or mixed */
                          tmp2 = *arg_types & TYPE_MOD_MASK;
                          tmp2 &= ~TYPE_MOD_POINTER;
                          for (i = anum_arg - num_arg; --i >=0; )
                          {
                              tmp1 = *argp++ & TYPE_MOD_RMASK;
                              if (!MASKED_TYPE(tmp1,tmp2))
                              {
                                  yyerrorf("Bad type for argument %d of %s %s",
                                      anum_arg - i,
                                      funp->name,
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
              if (def && num_arg == min-1)
              {
                  /* Default argument */
                  add_byte(def);
                  CURRENT_PROGRAM_SIZE++;
                  max--;
                  min--;
              }
              else if (num_arg < min
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
              if (max != -1 && exact_types && num_arg)
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

                      tmp1 = *aargp++ & TYPE_MOD_MASK;
                      for (;;)
                      {
                          if ( !(tmp2 = *argp) )
                          {
                              /* Possible types for this arg exhausted */
                              yyerrorf("Bad argument %d type to efun %s()",
                                    argn+1, instrs[f].name);
                              break;
                          }
                          argp++;

                          /* break if types are compatible; take care to
                           * handle references correctly
                           */
                          if (tmp1 == tmp2)
                              break;

                          if ((tmp1 &
                                 ~(TYPE_MOD_POINTER|TYPE_MOD_REFERENCE)) ==
                                TYPE_ANY)
                          {
                              if (tmp1 & TYPE_MOD_POINTER & ~tmp2)
                              {
                                  if ((tmp2 & ~TYPE_MOD_REFERENCE) !=
                                        TYPE_ANY)
                                  {
                                      continue;
                                  }
                              }
                              if ( !( (tmp1 ^ tmp2) & TYPE_MOD_REFERENCE) )
                                  break;
                          }
                          else if ((tmp2 &
                                 ~(TYPE_MOD_POINTER|TYPE_MOD_REFERENCE)) ==
                                TYPE_ANY)
                          {
                              if (tmp2 & TYPE_MOD_POINTER & ~tmp1)
                                  continue;
                              if ( !( (tmp1 ^ tmp2) & TYPE_MOD_REFERENCE) )
                                  break;
                          }
                      } /* end for (efun_arg_types) */

                      /* Advance argp to point to the allowed argtypes
                       * of the next arg.
                       */
                      while(*argp++) NOOP;
                  } /* for (all args) */
              } /* if (check arguments) */

              /* Alias for an efun? */
              if (f > LAST_INSTRUCTION_CODE)
                  f = efun_aliases[f-LAST_INSTRUCTION_CODE-1];

              if (f > 255)
              {
                  /* This efun needs a prefix byte */
                  add_byte(f >> F_ESCAPE_BITS);
                  CURRENT_PROGRAM_SIZE++;
              }
              add_byte(f);
              CURRENT_PROGRAM_SIZE++;

              /* Only store number of arguments for instructions
               * that allowed a variable number.
               */
              if (max != min)
              {
                  add_byte($4);/* Number of actual arguments */
                  CURRENT_PROGRAM_SIZE++;
              }

              /* If the efun doesn't return a value, fake a 0 */
              if ( instrs[f].ret_type == TYPE_VOID )
              {
                  last_expression = mem_block[A_PROGRAM].current_size;
                  add_byte(F_CONST0);
                  CURRENT_PROGRAM_SIZE++;
              }
          } /* efun */

          else if (efun_override)
          {
              yyerrorf("Unknown efun: %s", $1.real->name);
              $$.type = TYPE_ANY;
          }
          else
          {
              /* There is no such function, but maybe it's defined later,
               * maybe it's resolved through (cross-)inheritance.
               * epilog() will take care of it.
               */
              function_t *funp;

              f = define_new_function(MY_FALSE,
                  $1.real, 0, 0, 0, NAME_UNDEFINED, TYPE_UNKNOWN
              );
              add_byte(F_CALL_FUNCTION_BY_ADDRESS);
              add_short(f);
              add_byte($4);        /* Number of actual arguments */
              CURRENT_PROGRAM_SIZE += 4;
              funp = FUNCTION(f);
              if (exact_types)
              {
                  yyerrorf("Undefined function '%.50s'", $1.real->name);
              }
              else if (pragma_pedantic)
              {
                  yywarnf("Undefined function '%.50s'", $1.real->name);
              }
              $$.type = TYPE_ANY;  /* Just a guess */
          }

          if ($1.super)
              yfree($1.super);
          pop_arg_stack($4);   /* Argument types no longer needed */
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | expr4 L_ARROW
      {
%line
          /* If call_other() has been replaced by a sefun, and
           * if we need to use F_CALL_OTHER to call it, we have
           * to insert additional code before the <expr4> already parsed.
           * Putting this block before the <expr4> yields a
           * faulty grammar.
           */

          if (call_other_sefun >= 0
           && call_other_sefun & ~0xff)
          {
              /* The simul-efun has to be called by name:
               * insert the extra args for the call_other
               */
              char *p, *q;
              p_int left;

              if (!realloc_a_program(6))
              {
                  yyerrorf("Out of memory: program size %lu\n"
                          , CURRENT_PROGRAM_SIZE + 6);
                  YYACCEPT;
              }

              /* Move the generated code forward by 6 */
              p = mem_block[A_PROGRAM].block + CURRENT_PROGRAM_SIZE;
              q = p + 6;
              for (left = CURRENT_PROGRAM_SIZE - $1.start
                  ; left > 0
                  ; left--)
              {
                  *--q = *--p;
              }

              /* p now points to program[$1.start].
               * Store the first two call-other args there.
               */
              p[0] = F_STRING;
              upd_short($1.start+1, store_prog_string(
                        make_shared_string(query_simul_efun_file_name())));
              p[3] = F_STRING;
              upd_short($1.start+4, store_prog_string(ref_string(STR_CALL_OTHER)));

              CURRENT_PROGRAM_SIZE += 6;
          }
      }

      call_other_name %prec L_ARROW

      {
%line
          int string_number;
          char *p;

          /* If we received a string, it's a constant call. */
          p = $4;

          if (p)
          {
              /* Push the function name (the expr4 is already on the stack
               */
              string_number = store_prog_string(p);
              if (string_number <= 0x0ff )
              {
                  ins_byte(F_CSTRING0);
                  ins_byte(string_number);
              }
              else if ( string_number <= 0x1ff )
              {
                  ins_byte(F_CSTRING1);
                  ins_byte(string_number);
              }
              else if ( string_number <= 0x2ff )
              {
                  ins_byte(F_CSTRING2);
                  ins_byte(string_number);
              }
              else if ( string_number <= 0x3ff )
              {
                  ins_byte(F_CSTRING3);
                  ins_byte(string_number);
              }
              else
              {
                  ins_byte(F_STRING);
                  ins_short(string_number);
              }
          } /* if (p) */
          /* otherwise the name was given by an expression for which
           * the code and value have been already generated.
           */
      }

      '(' expr_list3 ')'

      {
          /* Now generate the CALL_OTHER resp. the SIMUL_EFUN instruction. */

          if (call_other_sefun >= 0)
          {
              /* SIMUL EFUN */

              PREPARE_INSERT(6)
              function_t *funp;
              int num_arg;

              num_arg = $7 + 2; /* Don't forget the obj and the fun! */

              funp = &simul_efunp[call_other_sefun];
              if (num_arg > funp->num_arg && !(funp->flags & TYPE_MOD_XVARARGS))
                  yyerrorf("Too many arguments to simul_efun %s", funp->name);

              if (call_other_sefun & ~0xff)
              {
                  /* call-other: the number of arguments will be
                   * corrected at runtime.
                   */
                  add_byte(F_CALL_OTHER);
                  add_byte(num_arg + 2);
                  CURRENT_PROGRAM_SIZE += 2;
              }
              else
              {
                  /* Direct call: we have to add the missing arguments.
                   * resp. encode the number of arguments passed.
                   */
                  if (funp->num_arg != SIMUL_EFUN_VARARGS
                   && !(funp->flags & TYPE_MOD_XVARARGS))
                  {
                      int i;

                      i = funp->num_arg - num_arg;
                      if (funp->flags & TYPE_MOD_XVARARGS)
                          i--; /* Last argument may be omitted */

                      if (i > 4)
                      {
                          if (!realloc_a_program(i+2))
                          {
                              yyerrorf("Out of memory: program size %lu\n"
                                      , mem_block[A_PROGRAM].current_size + i+2);
                              YYACCEPT;
                          }

                          __PREPARE_INSERT__p = PROGRAM_BLOCK
                                                + CURRENT_PROGRAM_SIZE;
                      }
                      CURRENT_PROGRAM_SIZE += i;
                      while ( --i >= 0 )
                      {
                          add_byte(F_CONST0);
                      }
                  }

                  add_byte(F_SIMUL_EFUN);
                  add_byte(call_other_sefun);
                  if (funp->num_arg == SIMUL_EFUN_VARARGS
                   || funp->flags & TYPE_MOD_XVARARGS)
                  {
                      add_byte(num_arg);
                      CURRENT_PROGRAM_SIZE += 3;
                  }
                  else
                      CURRENT_PROGRAM_SIZE += 2;
              }
              $$.type = funp->type & TYPE_MOD_MASK;
          }
          else /* true call_other */
          {
              ins_byte(F_CALL_OTHER);
              ins_byte($7 + 2);
              $$.type = instrs[F_CALL_OTHER].ret_type;
          }
          $$.code = -1;
          $$.start = $1.start;
          pop_arg_stack($7);
            /* No good need of these arguments because we don't
             * know what we are going to call.
             */
      }

; /* function_call */


call_other_name:
      L_IDENTIFIER
      {
          char *p;

          /* Extract the string from the ident structure */
          p = ref_string($1->name);
          if ($1->type == I_TYPE_UNKNOWN)
              free_shared_identifier($1);
          $$ = p;
      }

    | L_LOCAL
      {
          ident_t *p;

          /* First, find the declaration of this local */
          for (p = all_locals; p != NULL; p = p->next_all)
          {
              if (p->u.local.num == $1)
              {
                  /* We found the identifier.
                   */
                  break;
              }
          }

          if (p)
              $$ = ref_string(p->name);
          else
              fatal("Local variable %ld vanished.\n", (long)$1);
      }

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
          if ($2.type != TYPE_STRING
           && (pragma_strict_types != PRAGMA_WEAK_TYPES || $2.type != TYPE_UNKNOWN)
           && $2.type != TYPE_ANY)
              type_error("Illegal type for lfun name", (p_int)$2.type);
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
          ident_t *lvar = lookup_local($1);
          ident_t *fun = find_shared_identifier(lvar->name, I_TYPE_UNKNOWN, 0);

          /* Search the inferior list for this identifier for a global
           * (function) definition.
           */

          while (fun && fun->type > I_TYPE_GLOBAL)
              fun = fun->inferior;

          if (!fun || fun->type != I_TYPE_GLOBAL)
          {
              yyerrorf("Undefined function '%.50s'\n", lvar->name);
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
          ident_t *lvar = lookup_local($2);

          *($$.super = yalloc(1)) = '\0';
          $$.real  = lvar;
      }

    | anchestor L_COLON_COLON L_LOCAL
      {
%line
          ident_t *lvar = lookup_local($3);

          /* Attempt to call an efun directly even though there
           * is a nomask simul-efun for it?
           */
          if ( !strcmp($1, "efun")
           && lvar->type == I_TYPE_GLOBAL
           && lvar->u.global.sim_efun >= 0
           && simul_efunp[lvar->u.global.sim_efun].flags & TYPE_MOD_NO_MASK
           && master_ob
           && (!max_eval_cost || eval_cost < max_eval_cost)
             )
          {
              /* Yup, check it with a privilege violation.
               * If it's denied, ignore the "efun::" qualifier.
               */

              svalue_t *res;

              push_volatile_string("nomask simul_efun");
              push_volatile_string(current_file);
              push_volatile_string(lvar->name);
              res = apply_master(STR_PRIVILEGE, 3);
              if (!res || res->type != T_NUMBER || res->u.number < 0)
              {
                  yyerrorf("Privilege violation: nomask simul_efun %s"
                          , lvar->name);
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
          else if (max_eval_cost && eval_cost >= max_eval_cost)
          {
              yyerrorf("Can't call master::%s for "
                       "'nomask simul_efun %s': eval cost too big"
                      , STR_PRIVILEGE, lvar->name);
              yfree($1);
              $$.super = NULL;
          }
          else /* the qualifier is ok */
              $$.super = $1;

          $$.real = lvar; /* and don't forget the function ident */
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
           && (!max_eval_cost || eval_cost < max_eval_cost)
             )
          {
              /* Yup, check it with a privilege violation.
               * If it's denied, ignore the "efun::" qualifier.
               */

              svalue_t *res;

              push_volatile_string("nomask simul_efun");
              push_volatile_string(current_file);
              push_volatile_string($3->name);
              res = apply_master(STR_PRIVILEGE, 3);
              if (!res || res->type != T_NUMBER || res->u.number < 0)
              {
                  yyerrorf("Privilege violation: nomask simul_efun %s"
                          , $3->name);
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
          else if (max_eval_cost && eval_cost >= max_eval_cost)
          {
              yyerrorf("Can't call master::%s for "
                       "'nomask simul_efun %s': eval cost too big"
                      , STR_PRIVILEGE, $3->name);
              yfree($1);
              $$.super = NULL;
          }
          else /* the qualifier is ok */
              $$.super = $1;

          $$.real = $3; /* and don't forget the function ident */
      }
; /* function_name */


anchestor:
      L_IDENTIFIER
      {
          $$ = ystring_copy($1->name);
          if ($1->type == I_TYPE_UNKNOWN)
              free_shared_identifier($1);
      }

    | L_STRING L_STRING
      { fatal("presence of rule should prevent its reduction"); }

    | L_STRING
      {
          $$ = ystring_copy(last_lex_string);
          free_string(last_lex_string);
          last_lex_string = NULL;
      }
; /* anchestor */


/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
/* The inline function expression.
 *
 * This expression synthesizes a prototype for the inline function
 * and yields a closure-lrvalue suitable for expr4.
 * The function definition will be provided by the lexer at the next
 * opportunity.
 */

inline_fun:
      L_INLINE_FUN
      {
          /* Synthesize the prototype of the inline function
           * Since we have to declare the function arguments for that,
           * first save the existing locals.
           */

           ident_t * save_all_locals;
           int save_current_number_of_locals;
           int save_max_number_of_locals;
           int save_tol[10], save_ftol[10];
           char name[3];
           int num, i;

           /* Save the old locals information */
           save_all_locals = all_locals;
           save_current_number_of_locals = current_number_of_locals;
           save_max_number_of_locals = max_number_of_locals;

           /* Simulate 'no locals' */
           all_locals = NULL;
           current_number_of_locals = 0;
           max_number_of_locals = 0;
           use_local_scopes = MY_TRUE;
           enter_block_scope();

           /* Declare the 9 parameters (saving the types of the old ones) */
           name[0] = '$'; name[2] = '\0';

           for (i = 0; i < 9; i++)
           {
               save_tol[i] = type_of_locals[i];
               save_ftol[i] = full_type_of_locals[i];
               name[1] = (char)('1' + i);
               add_local_name(make_shared_identifier( name, I_TYPE_UNKNOWN
                                                    , block_depth)
                             , TYPE_ANY, block_depth);
           }

           /* Declare the function */
           num = define_new_function(MY_FALSE, /* id */ $1, 9, 0, 0
                                    , NAME_UNDEFINED|NAME_PROTOTYPE
                                    , TYPE_UNKNOWN|TYPE_MOD_VARARGS|TYPE_MOD_PRIVATE
                                    );

           /* Restore the old locals information */
           leave_block_scope(MY_TRUE);
           use_local_scopes = pragma_use_local_scopes;
           all_locals = save_all_locals;
           current_number_of_locals = save_current_number_of_locals;
           max_number_of_locals = save_max_number_of_locals;

           for (i = 0; i < 9; i++)
           {
               type_of_locals[i] = save_tol[i];
               full_type_of_locals[i] = save_ftol[i];
           }

           /* Insert the call to the lfun closure */

           $$.start = CURRENT_PROGRAM_SIZE;
           $$.code = -1;
           ins_byte(F_CLOSURE);
           ins_short(num);
           ins_short(0);
           $$.type = TYPE_CLOSURE;

      }
; /* inline_fun */


/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
/* The catch()-statement
 */

catch:
      L_CATCH
      {
          $<address>$ = CURRENT_PROGRAM_SIZE;
          ins_byte(F_CATCH);
          ins_byte(0); /* Placeholder for flags */
          ins_byte(0); /* Placeholder for the jump offset */
      }

      '(' comma_expr opt_catch_mods ')'

      {
%line
          p_int start, offset;

          ins_f_code(F_END_CATCH);

          /* Get the address of the CATCH instruction */
          start = $<address>2;

          /* Modify the instruction if necessary */
          if ($5)
          {
              bytecode_p p;
              p = PROGRAM_BLOCK + start + 1;
              *p = $5 & 0xff;
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
                  yyerrorf("Out of memory: program size %lu\n"
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

          $$.start = start;
          $$.type  = TYPE_ANY;
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
          $$ = $1 | $3;
      }

    | opt_catch_modifier
      {
          $$ = $1;
      }
; /* opt_catch_mod_list */


opt_catch_modifier :
      L_IDENTIFIER
      {
          $$ = 0;

          if (!strcmp($1->name, "nolog"))
              $$ = CATCH_FLAG_NOLOG;
          else if (!strcmp($1->name, "publish"))
              $$ = CATCH_FLAG_PUBLISH;
          else
              yyerror("Illegal modifier in catch() - "
                      "expected 'nolog' or 'publish'");
          if ($1->type == I_TYPE_UNKNOWN)
              free_shared_identifier($1);
      }

    | ';' L_LOCAL
      {
          ident_t *id;

          /* Find the ident structure for this local */
          for (id = all_locals; id; id = id->next_all)
              if (id->u.local.num == $2)
                  break;

          if (id && !strcmp(id->name, "nolog"))
              $$ = CATCH_FLAG_NOLOG;
          else if (id && !strcmp(id->name, "publish"))
              $$ = CATCH_FLAG_PUBLISH;
          else
              yyerror("Illegal modifier in catch() - "
                      "expected 'nolog' or 'publish'");
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
          ins_byte(F_SSCANF);
          ins_byte($7 + 2);
          $$.start = $2.start;
          $$.type = TYPE_NUMBER;
          $$.code = -1;
      }
; /* sscanf */

%ifdef SUPPLY_PARSE_COMMAND
parse_command:
      L_PARSE_COMMAND note_start
      '(' expr0 ',' expr0 ',' expr0 lvalue_list ')'
      {
          ins_byte(F_PARSE_COMMAND);
          ins_byte($9 + 3);
          $$.start = $2.start;
          $$.type = TYPE_NUMBER;
          $$.code = -1;
      }
; /* parse_command */
%endif /* SUPPLY_PARSE_COMMAND */


lvalue_list:
      /* empty */ { $$ = 0; }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | lvalue_list ',' L_IDENTIFIER
      {
          /* Push the lvalue for a global variable */

          int i;
%line
          $$ = 1 + $1;
          i = verify_declared($3);
          if (i & VIRTUAL_VAR_TAG)
          {
              ins_byte(F_PUSH_VIRTUAL_VARIABLE_LVALUE);
              ins_byte(i);
          }
          else
          {
              if ((i + num_virtual_variables) & ~0xff)
              {
                  ins_byte(F_PUSH_IDENTIFIER16_LVALUE);
                  ins_short(i + num_virtual_variables);
              }
              else
              {
                  ins_byte(F_PUSH_IDENTIFIER_LVALUE);
                  ins_byte(i + num_virtual_variables);
              }
          }
      }

    /*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
    | lvalue_list ',' L_LOCAL
      {
%line
          /* Push the lvalue for a local variable */

          $$ = 1 + $1;
          ins_byte(F_PUSH_LOCAL_VARIABLE_LVALUE);
          ins_byte($3);
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
          else
              arrange_protected_lvalue($3.start, $3.code, $3.end,
                F_PROTECTED_RINDEX_LVALUE
              );

          if ($4.type1 & TYPE_MOD_REFERENCE)
              yyerror("Reference used as index");

          if (exact_types)
          {
              vartype_t type;

              type = $3.type;
              if ( !(type & TYPE_MOD_POINTER) )
                 switch (type)
                 {
                 case TYPE_MAPPING:
                     if ($4.inst == F_INDEX)
                     {
                        $4.type1 = TYPE_ANY;
                        break;
                     }
                     /* FALLTHROUGH */
                 default:
                     type_error("Bad type to indexed lvalue", type);
                     /* FALLTHROUGH */
                 case TYPE_ANY:
                     if ($4.inst == F_INDEX)
                         $4.type1 = TYPE_ANY;
                     $4.type1 = TYPE_ANY;
                     break;
                 case TYPE_STRING:
                     break;
                 }
                 if (!BASIC_TYPE($4.type1, TYPE_NUMBER))
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
          if ($5.type & TYPE_MOD_REFERENCE)
                yyerror("Reference used as index");

          /* Compute and check types */
          if (exact_types)
          {
              vartype_t type;

              type = $3.type;
              if (type != TYPE_ANY && type != TYPE_MAPPING)
              {
                  type_error("Bad type to indexed value", type);
              }
              type = $7.type;
              if (type != TYPE_ANY && type != TYPE_NUMBER)
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
          case F_NX_RANGE: prot_op = F_PROTECTED_NX_RANGE_LVALUE; break;
          case F_RX_RANGE: prot_op = F_PROTECTED_RX_RANGE_LVALUE; break;
          default:
              errorf("Unsupported range type %d %s\n"
                   , $4.inst, get_f_name($4.inst));
          }

          $$ = 1 + $1;
          arrange_protected_lvalue($3.start, $3.code, $3.end
                                  , prot_op
          );

          /* Compute and check types */
          if (exact_types)
          {
              vartype_t type;

              type = $3.type;
              if ((type & TYPE_MOD_POINTER) == 0
                && type != TYPE_ANY && type != TYPE_STRING)
              {
                  type_error("Bad type of argument used for range", type);
              }
              type = $4.type1;
              if (type != TYPE_ANY && type != TYPE_NUMBER)
                  type_error("Bad type of index", type);
              type = $4.type2;
              if (type != TYPE_ANY && type != TYPE_NUMBER)
                  type_error("Bad type of index", type);
          }
      }

; /* lvalue_list */

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
%ifndef INITIALIZATION_BY___INIT

/* svalue_constant parses the constant variable initializers.
 * The generated value is stored in *currently_initialized.
 *
 * The constant initialization may contain function calls, however,
 * so far only the now rather useless order_alist() is supported.
 */

svalue_constant:
      constant_function_call
    | array_constant
    | constant
      {
          svalue_t *svp = currently_initialized;
%line
          put_number(svp, $1);
      }

    | string_constant
      {
          svalue_t *svp = currently_initialized;
%line
          put_string(svp, last_string_constant);
          last_string_constant = NULL;
      }

    | L_SYMBOL
      {
          svalue_t *svp = currently_initialized;
%line
          svp->type = T_SYMBOL;
          svp->x.quotes = $1.quotes;
          svp->u.string = $1.name;
      }

    | L_QUOTED_AGGREGATE

      { $<initialized>$ = currently_initialized; }

      const_expr_list '}' ')'

      {
          svalue_t *svp = $<initialized>2;
%line
          list_to_vector($3.length, svp);
          svp->type = T_QUOTED_ARRAY;
          svp->x.quotes = $1;
      }

    | float_constant
      {
          *currently_initialized = $1;
      }

    | L_CLOSURE
      {
          p_int ix;
          unsigned short inhIndex;
          svalue_t *svp = currently_initialized;
%line
          ix = $1.number;
          inhIndex = $1.inhIndex;
          svp->type = T_CLOSURE;
          if (ix < CLOSURE_EFUN_OFFS)
          {
              /* Lfun closure */

              lambda_t *l;

              l = xalloc(sizeof *l);
              l->ref = 1;
              l->ob = ref_object(current_object, "closure");
              l->function.lfun.index = ix;
              l->function.lfun.inhIndex = inhIndex;
              svp->u.lambda = l;
              svp->x.closure_type = CLOSURE_PRELIMINARY;
          }
          else if (ix >= CLOSURE_SIMUL_EFUN_OFFS)
          {
              /* Sefun closure */
              svp->x.closure_type = (short)ix;
              svp->u.ob = ref_object(current_object, "closure");
          }
          else
          {
              /* Efun or operator closure */
              if (pragma_warn_deprecated
               && instrs[ix - CLOSURE_EFUN_OFFS].deprecated != NULL)
                  yywarnf("%s() is deprecated: %s"
                         , instrs[ix - CLOSURE_EFUN_OFFS].name
                         , instrs[ix - CLOSURE_EFUN_OFFS].deprecated
                         );

              svp->x.closure_type
                = (short)(  instrs[ix - CLOSURE_EFUN_OFFS].Default == -1
                          ? ix + CLOSURE_OPERATOR-CLOSURE_EFUN
                          : ix);
              svp->u.ob = ref_object(current_object, "closure");
          }
      }

    | '(' '[' ']' ')'
      {
          svalue_t *svp = currently_initialized;
%line
          put_mapping(svp, allocate_mapping(0, 1));
      }

    | '(' '[' ':' constant ']' ')'
      {
          svalue_t *svp = currently_initialized;
%line
          put_mapping(svp, allocate_mapping(0, $4));
      }
; /* svalue_constant */


array_constant:
      '(' '{'

      { $<initialized>$ = currently_initialized; }

      const_expr_list '}' ')'

      {
%line
          list_to_vector($4.length, $<initialized>3);
      }
; /* array_constant */


float_constant:
      L_FLOAT
      {
%line
          STORE_DOUBLE_USED

          $$.type = T_FLOAT;
          STORE_DOUBLE(&$$, $1);
      }

    | '-' float_constant
      {
%line
          STORE_DOUBLE_USED
          double d;

          d = -READ_DOUBLE(&$2);
          $$.type = T_FLOAT;
          STORE_DOUBLE(&$$, d);
      }
; /* float_constant */


const_expr_list:
      /* empty */           { $$.length = 0; }
    | const_expr_list2      { $$ = $1; }
    | const_expr_list2 ','  { $$ = $1; }  /* Allow a trailing comma */
;

const_expr_list2:

      /* empty */

      {
          /* The end of a const_list (or a const_list with just one
           * element) - this is the first rule reduced.
           *
           * Prepare the const list svalue to return the value.
           */

          svalue_t *svp;
          const_list_svalue_t *clsv;
%line
          clsv = xalloc(sizeof *clsv);
          svp = currently_initialized;
          svp->type = T_LVALUE;
          svp->u.lvalue = &clsv->head;
          clsv->head.type = T_ERROR_HANDLER;
          clsv->head.u.error_handler = free_const_list_svalue;
          clsv->list.next = NULL;
          clsv->list.val.type = T_INVALID;
          currently_initialized = &clsv->list.val;
          $<const_list>$.l = &clsv->list;
          $<const_list>$.length = 1;
      }

      svalue_constant
      { $$ = $<const_list>1; }

    | const_expr_list2 ','
      {
          /* One more element to the const_list */

          const_list_t *l;
%line
          l = xalloc(sizeof (const_list_t));
          l->next = NULL;
          l->val.type = T_INVALID;
          currently_initialized = &l->val;
          $1.l->next = l;
      }

      svalue_constant

      {
          $$.l = $1.l->next;
          $$.length = $1.length+1;
      }
; /* const_expr_list2 */


constant_function_call:
      L_IDENTIFIER
      {
          /* I_TYPE_UNKNOWN must not be overrun by annother one, so
           * evaluate the identifier now.
           * We rely on the fact that $1.real->type is either
           * I_TYPE_UNKNOWN or I_TYPE_GLOBAL here. All others are filtered
           * by the lexical analysis.
           */

          $<const_call_head>$.function = $1->u.global.efun;
          $<const_call_head>$.initialized = currently_initialized;
          if ($1->type == I_TYPE_UNKNOWN)
          {
              free_shared_identifier($1);
              $<number>$ = -1;
          }
      }

      '(' const_expr_list3 ')'

      {
          svalue_t *svp;
          const_list_svalue_t *list;
%line
          svp = $<const_call_head>2.initialized;
          list = svp->u.const_list;
          switch($<const_call_head>2.function)
          {
          case F_ORDER_ALIST:
            {
              size_t i, listsize;
              vector_t *vec;

              if ($4.length == 1
               && list->list.val.type == T_POINTER
               && VEC_SIZE(vec = list->list.val.u.vec)
               && vec->item[0].type == T_POINTER
                 )
              {
                  xfree(list);
              }
              else
              {
                  vec = list_to_vector($4.length, svp);
              }

              if ((listsize = VEC_SIZE(vec))
               && vec->item[0].type == T_POINTER)
              {
                  size_t keynum = VEC_SIZE(vec->item[0].u.vec);

                  for (i = 0; i < VEC_SIZE(vec); i++)
                  {
                      if (vec->item[i].type != T_POINTER
                       || VEC_SIZE(vec->item[i].u.vec) != keynum)
                      {
                          yyerrorf("bad data array %ld for alist", (long)i);
                          free_array(vec);
                          *svp = const0;
                          break;
                      }
                  }
              }
              else
              {
                  yyerror("missing argument for order_alist");
              }

              if (listsize)
              {
                  put_array(svp, order_alist(vec->item, listsize, 1));
              }
              else
              {
                  *svp = const0;
              }
              free_array(vec);
              break;
            }
          default:
              yyerror("Illegal function call in initialization");
              free_svalue(svp);
              *svp = const0;
          }
      }
; /* constant_function_call */


const_expr_list3:
      /* empty */       { $$.length =0; $$.l = NULL; }
    | const_expr_list2  { $$ = $1; }
; /* const_expr_list3 */


%endif /* INITIALIZATION_BY___INIT */

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

%%
%line

#ifdef __MWERKS__
#    pragma warn_possunwant reset
#    pragma warn_implicitconv reset
#endif

/*=========================================================================*/

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
            yyerrorf("Out of memory: program size %lu"
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
 */

{
    if (last_expression == CURRENT_PROGRAM_SIZE-1)
    {
        /* We don't have to fear sideeffects and try to prevent
         * the value from being generated.
         */
        switch ( mem_block[A_PROGRAM].block[last_expression])
        {
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
        default: ins_byte(F_POP_VALUE);
        }
        last_expression = -1;
    }
    else
        /* The last expression is too long ago: just pop whatever there
         * is on the stack.
         */
        ins_byte(F_POP_VALUE);
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
 *         expr4[x]
 *             INDEX        -> PUSH_PROTECTED_INDEXED_LVALUE
 *         expr4[<x]
 *             RINDEX       -> PUSH_PROTECTED_RINDEXED_LVALUE
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
 *     has to be replaced by its alternative <code> (the argument byte
 *     is preserved), and the two-byte instruction <newcode> is then
 *     inserted after the replaced instruction and the following code.
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
 *         &(expr4[<x]): F_PROTECTED_INDEX_LVALUE
 *                       -> F_PUSH_PROTECTED_INDEXED_LVALUE;
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
                yyerrorf("Out of memory: program size %lu\n"
                        , CURRENT_PROGRAM_SIZE + length);
                return;
            }

            /* Cycle the indexing code to the end, where it belongs */
            p = PROGRAM_BLOCK;
            memcpy(p + current, p + start, length);
            p += start;
            q = p + length;
            length = current - start;
            do *p++ = *q++; while (--length);

            /* Adjust the code... */
            switch(code)
            {
            case F_PUSH_INDEXED_LVALUE:
                code = F_PUSH_PROTECTED_INDEXED_LVALUE;
                break;
            case F_PUSH_RINDEXED_LVALUE:
                code = F_PUSH_PROTECTED_RINDEXED_LVALUE;
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

            /* ...and store it */
            PUT_CODE(p-1, code >> F_ESCAPE_BITS);
            STORE_CODE(p, code);
            current++;
        code_stored:
            STORE_CODE(p, newcode >> F_ESCAPE_BITS);
            PUT_CODE(p, newcode);
        }
        else
        {
            /* Variant 2: Overwrite the old <code> and insert <newcode> */

            int i;
            p_int length;

            if (!realloc_a_program(2))
            {
                yyerrorf("Out of memory: program size %lu\n"
                        , CURRENT_PROGRAM_SIZE + 2);
                return;
            }

            p = PROGRAM_BLOCK + start;
            i = p[1];
            length = current - start - 2;
            for( ; --length >= 0; p++) PUT_CODE(p, GET_CODE(p+2));
            STORE_CODE(p, code);
            STORE_CODE(p, i);
            STORE_CODE(p, newcode >> F_ESCAPE_BITS);
            PUT_CODE(p, newcode);
        }
    }
    else
    {
        /* Variant 3: Just add a modified <newcode> */

        switch(newcode)
        {
        case F_PROTECTED_INDEX_LVALUE:
            newcode = F_PUSH_PROTECTED_INDEXED_LVALUE;
            break;
        case F_PROTECTED_RINDEX_LVALUE:
            newcode = F_PUSH_PROTECTED_RINDEXED_LVALUE;
            break;
        default:
            yyerror("Need lvalue for range lvalue.");
        }

        if (!realloc_a_program(2))
        {
            yyerrorf("Out of memory: program size %lu\n"
                    , CURRENT_PROGRAM_SIZE + 2);
            return;
        }

        p = PROGRAM_BLOCK + current;
        STORE_CODE(p, newcode >> F_ESCAPE_BITS);
        PUT_CODE(p, newcode);
    }

    /* Correct the program size */
    CURRENT_PROGRAM_SIZE = current + 2;
} /* arrange_protected_lvalue() */

/*-------------------------------------------------------------------------*/
int
proxy_efun (int function, int num_arg UNUSED)

/* If the number of arguments doesn't fit the <function>, maybe there
 * is an alternative.
 * Return the code of the alternative efun, or -1 if there is none.
 */

{
#if defined(__MWERKS__) && !defined(F_EXTRACT)
#    pragma unused(num_arg)
#endif
#ifdef F_EXTRACT
    if (function == F_EXTRACT)
    {
        if (num_arg == 2)
        {
            return F_EXTRACT2;
        }
        if (num_arg == 1)
        {
            return F_EXTRACT1;
        }
    }
#endif

    if (function == F_PREVIOUS_OBJECT)
    {
        /* num_arg == 0 */
        return F_PREVIOUS_OBJECT0;
    }

    return -1;
} /* proxy_efun() */

/*-------------------------------------------------------------------------*/

%ifdef INITIALIZATION_BY___INIT

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

#ifdef ALIGN_FUNCTIONS
        CURRENT_PROGRAM_SIZE = align(CURRENT_PROGRAM_SIZE);
        /* Must happen before PREPARE_INSERT()! */
#endif
        {
            char *name;
            PREPARE_INSERT(sizeof name + 3);

            name = ref_string(STR_VARINIT);
            memcpy(__PREPARE_INSERT__p , (char *)&name, sizeof name);
            __PREPARE_INSERT__p += sizeof(name);
            add_byte(TYPE_ANY);  /* return type */
            add_byte(0);         /* num_arg */
            add_byte(0);         /* num_local */
            first_initializer_start =
              (CURRENT_PROGRAM_SIZE += sizeof name + 3) - 2;
        }
    }
    else if ((p_int)(CURRENT_PROGRAM_SIZE - 3) == last_initializer_end)
    {
        /* The news INIT fragment directly follows the old one, so
         * just overwrite the JUMP instruction of the last.
         */
        mem_block[A_PROGRAM].current_size -= 4;
    }
    else
    {
        /* Change the address of the last jump after the last
         * initializer to this point.
         */
        upd_offset(last_initializer_end, mem_block[A_PROGRAM].current_size);
    }
} /* transfer_init_control() */

/*-------------------------------------------------------------------------*/
static void
add_new_init_jump (void)

/* The compiler just finished an INIT fragment: add a JUMP instruction
 * and let last_initializer_end point to its offset.
 */

{
    ins_byte(F_JUMP);
    last_initializer_end = (p_int)mem_block[A_PROGRAM].current_size;
    ins_byte(0);
    ins_short(0);
} /* add_new_init_jump() */

%endif /* INITIALIZATION_BY___INIT */

/*-------------------------------------------------------------------------*/
static short
lookup_inherited (const char *super_name, const char *real_name
                 , inherit_t **pIP, funflag_t *pFlags)

/* Lookup an inherited function <super_name>::<real_name> and return
 * it's function index, setting *pIP to the inherit_t pointer and
 * *pFlags to the function flags.
 * Return -1 if not found, *pIP set to NULL, and *pFlags set to 0.
 *
 * This function is called by the lexer to resolve #'<inherited_fun> closures,
 * so both strings are not shared.
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

    /* Strip leading '/' */
    while (*super_name == '/')
        super_name++;
    super_length = strlen(super_name);

    num_inherits = INHERIT_COUNT;
    real_name = findstring(real_name);

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
        if (super_length > 0)
        {
            /* ip->prog->name includes .c */
            int l = strlen(ip->prog->name) - 2;

            if (l < super_length)
                continue;
            if (l > super_length && ip->prog->name[l-super_length-1] != '/')
                continue;
            if (strncmp(super_name, ip->prog->name + l - super_length,
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
             &&  prog1->variable_names[ip2->variable_index_offset+numvar2-1].flags
                 & TYPE_MOD_VIRTUAL
             &&  !(prog2->variable_names[numvar2-1].flags & TYPE_MOD_VIRTUAL) )
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
    char *rname;
    funflag_t flags;
    short     ix;

    rname = findstring(real_name);

    ix =  rname ? lookup_inherited(super_name, rname, &ip, &flags) : -1;
    if (ix >= 0) /* Also return the inherit index. */
        *pInherit = ip - (inherit_t *)mem_block[A_INHERITS].block;
    else
        *pInherit = 0;
    return ix;
} /* find_inherited_function() */

/*-------------------------------------------------------------------------*/
static int
insert_inherited (char *super_name, char *real_name
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
 *
 * <real_name> must be a shared string (this function doesn't change
 * the reference count).
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
        add_byte(F_CALL_EXPLICIT_INHERITED);
        add_short(ip - (inherit_t *)mem_block[A_INHERITS].block);
        add_short(found_ix);
        add_byte(num_arg);

        /* Return the program pointer */
        *super_p = ip->prog;

        /* Return a copy of the function structure */
        fun_p->name = real_name;
        fun_p->flags = flags & ~INHERIT_MASK;
        {
            int i2 = found_ix;
            fun_hdr_p funstart;

            /* Find the real function code */
            while ( (flags = ip->prog->functions[i2]) & NAME_INHERITED)
            {
                ip = &ip->prog->inherit[flags & INHERIT_MASK];
                i2 -= ip->function_index_offset;
            }
            funstart = &ip->prog->program[flags & FUNSTART_MASK];
            fun_p->type = FUNCTION_TYPE(funstart);
            fun_p->num_arg = (FUNCTION_NUM_ARGS(funstart) & 0x7f);
            if (FUNCTION_NUM_ARGS(funstart) & ~0x7f)
                fun_p->type |= TYPE_MOD_XVARARGS;
        }
        CURRENT_PROGRAM_SIZE += 6;
        return found_ix;
    } /* if (ip) */

    /* Inherit not found, maybe it's a wildcarded call */
    if (strpbrk(super_name, "*?"))
    {
        int num_inherits;
        Bool *was_called;  /* Flags which inh. fun has been called already */
        inherit_t *ip0;
        int calls = 0;
        int ip_index;
        int first_index;
        short i;

        /* Wildcarded supercalls only work without arguments */
        if (num_arg)
            return INHERITED_WILDCARDED_ARGS;

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
            PREPARE_INSERT(6)

            /* ip->prog->name includes .c */
            int l = strlen(ip0->prog->name + 2);

            ip = ip0; /* ip will be changed in the body */

            if (ip->inherit_type & INHERIT_TYPE_DUPLICATE)
                /* duplicate inherit */
                continue;

            if (ip->inherit_depth > 1)
                /* Only consider direct inherits, otherwise we would even
                 * call functions in sub-inherits which have been redefined.
                 */
                continue;

            if ( !match_string(super_name, ip->prog->name, l) )
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
                 &&  prog1->variable_names[ip2->variable_index_offset+numvar2-1].flags
                     & TYPE_MOD_VIRTUAL
                 &&  !(prog2->variable_names[numvar2-1].flags & TYPE_MOD_VIRTUAL) )
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

            if (!calls) /* First function found */
                first_index = i;

            /* Generate the function call */
            add_byte(F_CALL_EXPLICIT_INHERITED);
            add_short(ip_index);
            add_short(i);
            add_byte(num_arg);

            /* Mark this function as called */
            was_called[ip_index] = MY_TRUE;

            /* Return the program pointer to the caller */
            *super_p = ip->prog;

            /* Return a copy of the function structure to the caller */
            fun_p->name = real_name;
            fun_p->flags = flags & ~INHERIT_MASK;
            {
                inherit_t *ip2 = ip;
                int i2 = i;
                fun_hdr_p funstart;

                /* Find the real function code */
                while ( (flags = ip2->prog->functions[i2]) & NAME_INHERITED)
                {
                    ip2 = &ip2->prog->inherit[flags & INHERIT_MASK];
                    i2 -= ip2->function_index_offset;
                }

                funstart = &ip2->prog->program[flags & FUNSTART_MASK];
                fun_p->type = FUNCTION_TYPE(funstart);
                fun_p->num_arg = FUNCTION_NUM_ARGS(funstart);
            }
            calls++;
            CURRENT_PROGRAM_SIZE += 6;
        } /* for() */

        /* The calls above left their results on the stack.
         * Combine them into a single array (which might be empty).
         */
        {
            PREPARE_INSERT(3)
            add_byte(F_AGGREGATE);
            add_short(calls);
            CURRENT_PROGRAM_SIZE += 3;
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
get_function_id (program_t *progp, int fx)

/* Return a pointer to the function flags of function <fx> in <progp>.
 * This function takes care of resolving cross-definitions and inherits
 * to the real function flag.
 */

{
    funflag_t flags;

    flags = progp->functions[fx];

    /* Handle a cross-define */
    if (flags & NAME_CROSS_DEFINED)
    {
        fx += CROSSDEF_NAME_OFFSET(flags);
        flags = progp->functions[fx];
    }

    /* Walk the inherit chain */
    while(flags & NAME_INHERITED)
    {
        inherit_t *inheritp;

        inheritp = &progp->inherit[flags & INHERIT_MASK];
        progp = inheritp->prog;
        fx -= inheritp->function_index_offset;
        flags = progp->functions[fx];
    }

    /* This is the one */
    return &progp->functions[fx];
} /* get_function_id() */

/*-------------------------------------------------------------------------*/
static
%ifdef INITIALIZATION_BY___INIT
       int
%else
       void
%endif

copy_functions (program_t *from, fulltype_t type)

/* The functions of the program <from> are inherited with visibility <type>.
 * Copy all the function definitions into this program, but as UNDEFINED
 * so that they can be redefined in the current program. The epilog()
 * will later update the non-redefined inherited functions and also copy
 * the types.
 *
 * An explicit call to an inherited function will not be
 * done through this entry (because this entry can be replaced by a new
 * definition). If an function defined by inheritance is called,
 * this is done with F_CALL_EXPLICIT_INHERITED
 *
%ifdef INITIALIZATION_BY___INIT
 * The result is the function index of the inherited __INIT function,
 * or -1 if the inherited program doesn't have an initializer.
%endif
 */

{
%ifdef INITIALIZATION_BY___INIT
    int initializer = -1;
%endif

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
            return
%ifdef INITIALIZATION_BY___INIT
                    0
%endif
                        ;
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
        program_t *defprog;
        inherit_t *ip;
        fun_hdr_p  funstart;
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

        /* Look up the defining program for the inherited function */
        defprog = from;
        while ( (flags = defprog->functions[i2]) & NAME_INHERITED)
        {
            ip = &defprog->inherit[flags & INHERIT_MASK];
            i2 -= ip->function_index_offset;
            defprog = ip->prog;
        }

        /* Copy the function information */
        funstart = &defprog->program[flags & FUNSTART_MASK];
        memcpy(&fun_p->name, FUNCTION_NAMEP(funstart), sizeof fun_p->name);
        fun_p->type = FUNCTION_TYPE(funstart);
        fun_p->num_arg = FUNCTION_NUM_ARGS(funstart) & 0x7f;
        if (FUNCTION_NUM_ARGS(funstart) & ~0x7f)
            fun_p->type |= TYPE_MOD_XVARARGS;
        if (FUNCTION_CODE(funstart)[0] == F_ESCAPE
         && FUNCTION_CODE(funstart)[1] == F_UNDEF  -0x100)
        {
            fun_p->flags |= NAME_UNDEFINED;
        }

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
            /* Test if the function is visible at all.
             * For this test, 'private nomask' degenerates to 'private'
             * if we didn't do that, the driver would crash on a second
             * level inherit (possible on a multiple second-level inherit).
             * TODO: Find out why it crashes.
             */
            {
                fulltype_t fflags = fun.flags;

                if ((fflags & (TYPE_MOD_PRIVATE|TYPE_MOD_NO_MASK))
                 == (TYPE_MOD_PRIVATE|TYPE_MOD_NO_MASK)
                   )
                    fflags &= ~(TYPE_MOD_NO_MASK);

                if ( (fflags & (NAME_HIDDEN|TYPE_MOD_NO_MASK|NAME_UNDEFINED) ) ==

                     (NAME_HIDDEN|TYPE_MOD_NO_MASK) )
                {
                    break;
                }
            }

            /* Visible: create a new identifier for it */
            p = make_global_identifier(fun.name, I_TYPE_GLOBAL);
            if (!p)
                break;

            if (p->type != I_TYPE_UNKNOWN)
            {
                /* We got this ident already somewhere */

                int32 n; /* existing function index */

                if ( (n = p->u.global.function) >= 0)
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
                                "Misplaced prototype for %s in %s\n"
                                , fun.name, current_file
                            );
                            cross_define( &fun, OldFunction
                                        , current_func_index - n );
                            p->u.global.function = current_func_index;
                        }
                        else if ((fun.flags | type) & TYPE_MOD_VIRTUAL
                              && OldFunction->flags & TYPE_MOD_VIRTUAL
                          && !((fun.flags | OldFunction->flags) & NAME_HIDDEN)
                          &&    get_function_id(from, i)
  == get_function_id(INHERIT(OldFunction->offset.inherit).prog
                , n - INHERIT(OldFunction->offset.inherit).function_index_offset
                     )
                                 )
                        {
                            /* Entries denote the same function. We have to use
                             * cross_define nonetheless, to get consistant
                             * redefinition (and we prefer the first one)
                             */
                            OldFunction->flags |= fun.flags &
                                (TYPE_MOD_PUBLIC|TYPE_MOD_NO_MASK);
                            OldFunction->flags &= fun.flags | ~TYPE_MOD_STATIC;
                            cross_define( OldFunction, &fun
                                        , n - current_func_index );
                        }
                        else if ( (fun.flags & OldFunction->flags & TYPE_MOD_NO_MASK)
                             &&  !( (fun.flags|OldFunction->flags) & (TYPE_MOD_PRIVATE|NAME_UNDEFINED) ) )
                        {
                            yyerrorf(
                              "Illegal to inherit 'nomask' function '%s' twice",
                              fun.name);
                        }
                        else if ((   fun.flags & TYPE_MOD_NO_MASK
                                  || OldFunction->flags & (NAME_HIDDEN|NAME_UNDEFINED|TYPE_MOD_PRIVATE))
                              && !(fun.flags & (NAME_HIDDEN|NAME_UNDEFINED))
                                )
                        {
                            /* This function is visible and existing, but the
                             * inherited one is not, or this one is also nomask:
                             * prefer this one.
                             */
                            cross_define( &fun, OldFunction
                                        , current_func_index - n );
                            p->u.global.function = current_func_index;
                        }
                        else if ((   fun.flags & TYPE_MOD_NO_MASK
                                  || OldFunction->flags & (NAME_HIDDEN|NAME_UNDEFINED|TYPE_MOD_PROTECTED))
                              && !(fun.flags & (NAME_HIDDEN|NAME_UNDEFINED))
                                )
                        {
                            /* This function is visible and existing, but the
                             * inherited one is not, or this one is also nomask:
                             * prefer the inherited one.
                             */
                            cross_define( &fun, OldFunction
                                        , current_func_index - n );
                            p->u.global.function = current_func_index;
                        }
                        else if ( (fun.flags & TYPE_MOD_PRIVATE) == 0
                              ||  (OldFunction->flags & TYPE_MOD_PRIVATE) == 0
                              ||  ((OldFunction->flags|fun.flags)
                                   & TYPE_MOD_VIRTUAL) != 0
                                )
                        {
                            /* At least one of the functions is visible
                             * or redefinable: prefer the first one.
                             * TODO: The whole if-condition is more a kludge,
                             * TODO:: developed iteratively from .367
                             * TODO:: through .370. It should be reconsidered,
                             * TODO:: which of course implies a deeper
                             * TODO:: analysis of the going ons here.
                             */
                            cross_define( OldFunction, &fun
                                        , n - current_func_index );
                        }
                    } /* if (n < first_func_index) */
                    else if ( !(fun.flags & NAME_CROSS_DEFINED) )
                    {
                        /* This is the dominant definition in the superclass,
                         * inherit this one.
                         */
#ifdef DEBUG
                        /* The definition we picked before should be
                         * cross-defined to the definition we have now; or
                         * it should be nominally invisible so we can redefine
                         * it.
                         */
                        if ((   !(FUNCTION(n)->flags & NAME_CROSS_DEFINED)
                             ||   FUNCTION(n)->offset.func
                                != MAKE_CROSSDEF_OFFSET(((int32)current_func_index) - n)
                            )
                         && ((FUNCTION(n)->flags & TYPE_MOD_PRIVATE) == 0
                            )
                           )
                        {
                            fatal(
                              "Inconsistent definition of %s() within "
                              "superclass '%s'.\n"
                            , fun.name, from->name
                            );
                        }
#endif
                        p->u.global.function = current_func_index;
                    }
                }
                else /* n < 0: not an lfun */
                {
                    if (n != I_GLOBAL_FUNCTION_EFUN
                     || (fun.flags & (TYPE_MOD_PRIVATE|NAME_HIDDEN)) == 0
                     || (fun.flags & (NAME_UNDEFINED)) != 0
                       )
                    {
                        if (n == I_GLOBAL_FUNCTION_EFUN)
                        {
                            /* This inherited function shadows
                             * an (simul-)efun.
                             */

                            efun_shadow_t *q;

                            q = xalloc(sizeof(efun_shadow_t));
                            if (!q) {
                                yyerrorf("Out of memory: efun shadow (%lu bytes)"
                                        , (unsigned long) sizeof(efun_shadow_t));
                                break;
                            }
                            q->shadow = p;
                            q->next = all_efun_shadows;
                            all_efun_shadows = q;
                        }

                        /* Update the symbol table entry to point
                         * to the newly read function.
                         */
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

                p->type = I_TYPE_GLOBAL;
                p->u.global.variable = I_GLOBAL_VARIABLE_OTHER;
                p->u.global.efun     = I_GLOBAL_EFUN_OTHER;
                p->u.global.sim_efun = I_GLOBAL_SEFUN_OTHER;
                p->u.global.function = current_func_index;
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
             && fun.name[0] == 'h'
             && (strcmp(fun.name, "heart_beat") == 0)
             && !(fun.flags & (NAME_HIDDEN|NAME_UNDEFINED))
               )
            {
                heart_beat = current_func_index;
            }

%ifdef INITIALIZATION_BY___INIT
            /* Recognize the initializer function */
            if (fun.name[0] == '_' && strcmp(fun.name+1, "_INIT") == 0)
            {
                initializer = i;
                fun.flags |= NAME_UNDEFINED;
            }
%endif
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
                    add_to_mem_block(
                      A_ARGUMENT_TYPES,
                      &from->argument_types[from->type_start[i]],
                      (sizeof (unsigned short)) * fun.num_arg
                    );
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

%ifdef INITIALIZATION_BY___INIT
    return initializer;
%endif
} /* copy_functions() */

/*-------------------------------------------------------------------------*/
static void
copy_variables (program_t *from, fulltype_t type

%ifndef INITIALIZATION_BY___INIT
               , svalue_t *initializers
%endif
               )

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

    type &= ~TYPE_MOD_VARARGS; /* aka NAME_INITIALIZED */

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

#ifdef INITIALIZATION_BY___INIT
        if (variables_initialized && from_variable_index_offset < 0)
            yyerror(
              "illegal to inherit virtually after initializing variables\n"
            );
#endif
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
             && from->variable_names[new_bound-1].flags & TYPE_MOD_VIRTUAL
             && !(progp->variable_names[progp->num_variables-1].flags
                  & TYPE_MOD_VIRTUAL)
               )
            {
                inherit_t inherit, *inheritp2;
                int k, inherit_index;
                funflag_t *flagp;
                function_t *funp, *funp2;

#ifdef INITIALIZATION_BY___INIT
                if (variables_initialized)
                    yyerror(
"illegal to inherit virtually after initializing variables\n"
                    );
#endif
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

                inherit_index = (mem_block[A_INHERITS].current_size - j) /
                   sizeof(inherit_t) - 1;
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
                 * to the first (virtual) inherit of the program.
                 */
                flagp = from->functions + inheritp->function_index_offset;
                funp = (function_t *)mem_block[A_FUNCTIONS].block +
                    inherit.function_index_offset;
                funp2 = (function_t *)mem_block[A_FUNCTIONS].block +
                    inheritp2->function_index_offset;
                    /* Usually funp2 == funp, but if the program is inherited
                     * virtually several times with differing visibilities,
                     * the two pointers differ.
                     */
                for (k = inherit.prog->num_functions; --k >= 0; funp++, funp2++)
                {
                    if ( !(funp->flags & NAME_CROSS_DEFINED)
                     &&  !(funp2->flags & NAME_CROSS_DEFINED)
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
            fulltype_t new_type;

            p = make_global_identifier(from->variable_names[j].name
                                      , I_TYPE_GLOBAL);
            if (!p)
                return;

            new_type = type;

            /* 'public' variables should not become private when inherited
             * 'private'.
             */
            if (from->variable_names[j].flags & TYPE_MOD_PUBLIC)
                new_type &= ~TYPE_MOD_PRIVATE;

            /* define_variable checks for previous 'nomask' definition. */
            if (previous_variable_index_offset >= 0)
            {
                if ( !(from->variable_names[j].flags & TYPE_MOD_PRIVATE) )
                    redeclare_variable(p,
                      new_type | from->variable_names[j].flags | NAME_INHERITED,
                      previous_variable_index_offset + j
                    );
            }
            else
            {
                define_variable(p,
                  new_type | from->variable_names[j].flags |
                  (from->variable_names[j].flags & TYPE_MOD_PRIVATE ?
                    (NAME_HIDDEN|NAME_INHERITED)  :  NAME_INHERITED )
%ifndef INITIALIZATION_BY___INIT
                  ,from->variable_names[j].flags & NAME_INITIALIZED ?
                    copy_svalue(&initializers[j]) : &const0
%endif
                );
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
     && current_line - stored_lines >= 2 && current_line - stored_lines <= 9)
    {
        c = offset + 8*(current_line - stored_lines) + 47;
          /* == (lineincr+6) << 3 | (codesize-1) */
        byte_to_mem_block(A_LINENUMBERS, c);
        stored_lines = current_line;
        return;
    }

    /* Use up the excessive amounts of lines */
    stored_lines++;
    while (stored_lines < current_line)
    {
        int lines;

        lines = current_line - stored_lines;
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

    save_current = current_line;
    stored_lines -= 2;
    current_line = stored_lines+1;
    offset = current_line - relocated_from;
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
    current_line = save_current;
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

        inc.filename = make_shared_string(filename);
        if (inc.filename == NULL)
        {
            inc.filename = ref_string(STR_DEFAULT);
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

            inc.name = make_shared_string(tmp);
            if (inc.name == NULL)
            {
                inc.name = ref_string(STR_DEFAULT);
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
        while (stored_lines < current_line)
        {
            int lines;

            lines = current_line - stored_lines;
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
         * information stored by store_include_info().
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
prolog (void)

/* Initialize the compiler environment prior to a compile.
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
    last_expression  = -1;
    compiled_prog    = NULL;  /* NULL means fail to load. */
    heart_beat       = -1;
    comp_stackp      = 0;     /* Local temp stack used by compiler */
    current_continue_address = 0;
    current_break_address    = 0;
    num_parse_error  = 0;
    block_depth      = 0;
    use_local_scopes = MY_TRUE;
    default_varmod = 0;
    default_funmod = 0;

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

    stored_lines = 0;
    stored_bytes = 0;
    last_include_start = -1;
    memset(prog_string_tags, 0, sizeof prog_string_tags);
    num_virtual_variables = 0;
    case_state.free_block = NULL;
    case_state.next_free = NULL;
%ifdef INITIALIZATION_BY___INIT
    last_initializer_end = -4; /* To pass the test in transfer_init_control() */
    variables_initialized = 0;
%endif

    /* Check if call_other() has been replaced by a sefun.
     */
    call_other_sefun = -1;

    id = make_shared_identifier(STR_CALL_OTHER, I_TYPE_UNKNOWN, 0);

    if (!id)
        fatal("Out of memory: identifier '%s'.\n", STR_CALL_OTHER);

    if (id->type == I_TYPE_UNKNOWN)
    {
        /* No such identifier, therefor no such sefun */
        free_shared_identifier(id);
    }
    else
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

} /* prolog() */

/*-------------------------------------------------------------------------*/
static void
epilog (void)

/* The parser finished - now collect the information and generate
 * the program structure, if the parse was successful.
 */

{
    int          size, i;
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
        free_string(last_string_constant);
        last_string_constant = NULL;
    }

    free_case_blocks();

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

%ifndef INITIALIZATION_BY___INIT

    /* Just add the non-virtual values to the virtual block */

    add_to_mem_block(
        A_VIRTUAL_VAR_VALUES,
        mem_block[A_VARIABLE_VALUES].block,
        mem_block[A_VARIABLE_VALUES].current_size
    );
    mem_block[A_VARIABLE_VALUES].current_size = 0;

%else

    /* Define the __INIT function, but only if there was any code
     * to initialize.
     */
    if (last_initializer_end > 0)
    {
        ident_t *ip;

        ip = make_global_identifier("__INIT", I_TYPE_UNKNOWN);
        if (ip)
            define_new_function(MY_FALSE, ip, 0, 0, first_initializer_start, TYPE_MOD_PROTECTED, 0);

        /* ref count for ip->name was incremented by transfer_init_control() */

        /* Change the last jump after the last initializer into a
         * return(1) statement.
         */
        mem_block[A_PROGRAM].block[last_initializer_end-1] =
            F_CONST1;
        mem_block[A_PROGRAM].block[last_initializer_end-0] =
            F_RETURN;
    } /* if (has initializer) */

%endif /* INITIALIZATION_BY___INIT */

    /* Check the string block. We don't have to count the include file names
     * as those won't be accessed from the program code.
     */
    if (mem_block[A_STRINGS].current_size > 0x10000 * sizeof (char *))
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
             * with ESCAPE UNDEF as body.
             * Except __INIT, which is created as CONST1 RETURN.
             */
            if ((f->flags & (NAME_UNDEFINED|NAME_INHERITED)) == NAME_UNDEFINED)
            {
#ifdef ALIGN_FUNCTIONS
                CURRENT_PROGRAM_SIZE = align(CURRENT_PROGRAM_SIZE);
#endif
                if (!realloc_a_program(FUNCTION_HDR_SIZE + 2))
                {
                    yyerrorf("Out of memory: program size %lu\n"
                            , CURRENT_PROGRAM_SIZE + FUNCTION_HDR_SIZE + 2);
                }
                else
                {
                    ref_string(f->name);
                    f->offset.pc = CURRENT_PROGRAM_SIZE + sizeof f->name + 1;
                    p = PROGRAM_BLOCK + CURRENT_PROGRAM_SIZE;
                    memcpy(p, (char *)&f->name, sizeof f->name);
                    p += sizeof f->name;
                    *p++ = f->type;
                    *p++ = f->num_arg;
                    *p++ = f->num_local;
%ifdef INITIALIZATION_BY___INIT
                    /* If __INIT() is undefined (i.e. there was a prototype, but
                     * no explicit function nor the automagic initialization code,
                     * then a dummy function is generated. This prevents crashes
                     * when this program is inherited later.
                     */
                    if (f->name[0] == '_' && !strcmp(f->name, "__INIT")
                     && !f->num_arg)
                    {
                        f->flags &= ~NAME_UNDEFINED;
                        *p++ = F_CONST1;
                        *p   = F_RETURN;
                    } else {
%endif
                        *p++ = F_ESCAPE;
                        *p   = F_UNDEF-0x100;
%ifdef INITIALIZATION_BY___INIT
                    }
%endif
                    CURRENT_PROGRAM_SIZE += sizeof f->name + 5;
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
#ifdef ALIGN_FUNCTIONS
                        if ((funname_start1->name - funname_start2->name) < 0)
#else
                        /* must use memcmp(), because it is used later for the
                         * program.    byteorder is non-portable.
                         */
                        if (memcmp(
                              &funname_start2->name,
                              &funname_start1->name,
                              sizeof(char *)
                            ) < 0)
#endif
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
                    free_string(functions->name);
                }
            }
        }

        /* Done: functions are sorted, resolved, etc etc */
    } /* if (parse successful) */

%ifndef INITIALIZATION_BY___INIT

    /* Return the variable initializers to the caller */
    prog_variable_values =
      (svalue_t *)mem_block[A_VIRTUAL_VAR_VALUES].block;

%endif /* INITIALIZATION_BY___INIT */

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
            s->shadow->u.global.function = I_GLOBAL_FUNCTION_EFUN;
            s->shadow->u.global.variable = I_GLOBAL_VARIABLE_FUN;
            t = s->next;
            xfree(s);
        }
        all_efun_shadows = NULL;
    }

    all_globals = NULL;

    /* Now create the program structure */
    switch (0) { default:

        /* One error, don't create anything */
        if (num_parse_error > 0 || inherit_file)
            break;

        /* Compute the size of the program.
         * Right now, we allocate everything in one block.
         */

        size = align(sizeof (program_t));

        if (!pragma_save_types)
        {
            mem_block[A_ARGUMENT_TYPES].current_size = 0;
            mem_block[A_ARGUMENT_INDEX].current_size = 0;
        }
        for (i=0; i<NUMPAREAS; i++)
        {
            if (i != A_LINENUMBERS)
                size += align(mem_block[i].current_size);
        }

        size += align(num_function_names * sizeof *prog->function_names);
        size += align(num_functions * sizeof *prog->functions);

        /* Get the program structure */
        if ( !(p = xalloc(size)) )
        {
            yyerrorf("Out of memory: program structure (%u bytes)", size);
            break;
        }

        prog = (program_t *)p;
        *prog = NULL_program;

        /* Set up the program structure */
        if ( !(prog->name = string_copy(current_file)) )
        {
            xfree(prog);
            yyerrorf("Out of memory: filename '%s'", current_file);
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
                    | (pragma_no_shadow ? P_NO_SHADOW : 0);
        prog->load_time = current_time;

        total_prog_block_size += prog->total_size + strlen(prog->name)+1;
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
        prog->strings = (char **)p;
        prog->num_strings = num_strings;
        if (mem_block[A_STRINGS].current_size)
            memcpy(p, mem_block[A_STRINGS].block,
                   mem_block[A_STRINGS].current_size);

        p += align(mem_block[A_STRINGS].current_size);

        /* Add the variable descriptions
         */
        prog->variable_names = (variable_t *)p;
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
            prog->argument_types = (unsigned short *)p;
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
                total_prog_block_size -= prog->total_size + strlen(prog->name)+1;
                total_num_prog_blocks -= 1;
                xfree(prog);
                yyerrorf("Out of memory: linenumber structure (%lu bytes)"
                        , (unsigned long)linenumber_size);
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

        for (i = 0; i < NUMAREAS; i++)
        {
%ifndef INITIALIZATION_BY___INIT
            /* Don't free this, the caller is going to
             * need them.
             */
            if (i == A_VIRTUAL_VAR_VALUES)
                continue;
%endif /* INITIALIZATION_BY___INIT */
            xfree(mem_block[i].block);
        }

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

%ifndef INITIALIZATION_BY___INIT
        for (i = 0; i < num_variables; i++)
            free_svalue(&prog_variable_values[i]);
%endif /* INITIALIZATION_BY___INIT */

        /* Free all function names. */
        functions = (function_t *)mem_block[A_FUNCTIONS].block;
        for (i = num_functions; --i >= 0; functions++)
            if ( !(functions->flags & (NAME_INHERITED|NAME_UNDEFINED))
             && functions->name )
            {
                free_string(functions->name);
            }

        do_free_sub_strings( num_strings
                           , (char **)mem_block[A_STRINGS].block
                           , num_variables
                           , (variable_t *)mem_block[A_VIRTUAL_VAR].block
                           , INCLUDE_COUNT
                           , (include_t *)mem_block[A_INCLUDES].block
                           );

        compiled_prog = NULL;

        for (i=0; i<NUMAREAS; i++)
            xfree(mem_block[i].block);

        return;
    }

    /* NOTREACHED */
} /* epilog() */

/*-------------------------------------------------------------------------*/
void
compile_file (int fd)

/* Compile an LPC file. See the head comment for instructions.
 */

{
    prolog();
    start_new_file(fd);
    yyparse();
    /* If the parse failed, either num_parse_error != 0
     * or inherit_file != NULL here.
     */
    epilog();
    end_new_file();
} /* compile_file() */

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
