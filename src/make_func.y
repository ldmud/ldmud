%{
/*---------------------------------------------------------------------------
 * make_func: Generation of the parser description and other tables
 *
 *---------------------------------------------------------------------------
 * make_func is an essential part of build process, as it creates the
 * actual LPC compiler description from a template, as well as some
 * internal tables from a list of code and efun specs.
 *
 * make_func takes one commandline argument which determines what it
 * should create:
 *
 *   make_func instrs
 *     creates efun_defs.c and instrs.h, reads machine.h, config.h and func_spec.
 *
 *   make_func lang
 *     creates lang.y from prolang.y, reads machine.h and config.h.
 *
 *   make_func strings
 *     creates stdstrings.[ch], reads machine.h, config.h and string_spec.
 *
 * The calls are described in detail below.
 *---------------------------------------------------------------------------
 * make_func instrs
 * ----------------
 *
 * make_func reads the specifications of all interpreter bytecodes from
 * the file func_spec and creates the source files needed to compile
 * the driver.
 *
 * The bytecodes both cover interpreter internal instructions as well
 * as efuns. A complication is that every bytecode can occupy only one
 * byte, but there are more than 256 codes and functions. This is solved
 * by the use of multi-byte instructions, where the first byte is
 * one of a set of prefix bytes, followed by the instruction's opcode
 * byte. The prefix bytes divide the instructions into several classes:
 * efuns with no argument, efuns with one argument, etc. The translation
 * from instruction code to prefix/opcode is written down in the
 * instrs[] table created by make_func.
 *
 * The instructions and their prefix/opcode encodings follow the
 * following rules:
 *   - all non-efun instructions do not need a prefix byte and start
 *     at instruction code 0.
 *   - the instruction codes for the tabled efuns are consecutive
 *   - the instruction codes for all tabled varargs efuns are consecutive.
 *
 * When make_func is run, it first reads the files 'machine.h' and
 * 'config.h' to determine which defines are used in the compilation of the
 * driver. For this make_func implements a simple C preprocessor, capable
 * of numeric expressions and simple macro expansions (no function macros),
 * which also allows the use of C-like block comments. With the knowledge
 * of the defines, make_func then reads the file 'func_spec' with the
 * information about the codes and efuns.
 *
 * From this information, make_func generates these files:
 *   efun_defs.c
 *     This file contains the table of all instructions, instrs[].
 *     The table defines for every machine instruction if it is an
 *     internal code or efun, what arguments of what type it takes, etc.
 *     The file also contains the tables for the driver-specific ctype
 *     implementation.
 *     It is included into the lexer lex.c and compiled there.
 *
 *  instrs.h
 *     This file defines the bytecodes representing all the efuns
 *     and non-efun instructions which are not compiler tokens.
 *
 *
 * func_spec is divided into several sections, each introduced with its
 * own %-keyword. The required order and meaning of the sections is this:
 *
 *     %codes
 *         Internal machine codes used by the compiler to generate code.
 *
 *     %efuns
 *         The efuns which are represented by one bytecode.
 *
 *     %tefuns
 *         The tabled efuns. make_func will sort the efuns into
 *         various classes depending on the number of arguments.
 *         Each class is assigned it's own 'efun<n>' prefix code, with
 *         <n> being the number of arguments. Efuns with a variable number
 *         or arguments will be prefixed by 'efunv'.
 *
 * The entries in the sections are separated by whitespace.
 *
 * A token or code entry follows this syntax:
 *
 *    [ "name" ] id [op-type]
 *
 *        <id> is the name used in the compiler source, the generated token
 *        will be called F_<ID>. If <name> is specified, it will be used
 *        in tracedumps instead of the plain <id>.
 *
 *        If the instruction is an operator which can be used as a operator
 *        closure (like +), the optional <op-type> specifies how many operands
 *        the operator takes. <op-type> must be one of the keywords 'unary',
 *        'binary' and 'ternary'.
 *
 *
 * An efun is defined this way:
 *
 *    ret-type name [ alias ] ([argtype-1, ... , argtype-n]) [ "msg" ];
 *
 *        This is the efun <name>, with the compiler source name F_<NAME>,
 *        which returns a value of type <ret-type> and takes n >= 0
 *        arguments of type <argtype-x>. If <alias> is given, it must
 *        be the source name (F_FOO) of another efun this one is an
 *        alias for.
 *
 *        The types are given in the usual LPC syntax, with the additional
 *        type of 'unknown' for return types.
 *
 *        Argument types can be the combination of several types in
 *        the form 'type|type|...', e.g. 'string|object'. If one of the
 *        types is 'void', the argument is optional (but then only optional
 *        arguments may follow).
 *
 *        The last argument can take a default value, which is then specified
 *        as 'default: F_FOO' with F_FOO being the source name of an efun
 *        yielding the desired value. Example:
 *            ..(object default: F_THIS_OBJECT)
 *
 *        Alternatively, the last argument can be given as '...' if the
 *        efun can handle arbitrarily many arguments.
 *
 *        If the efun is deprecated, <msg> is the warning message to
 *        print when the efun is used and pragma warn_deprecated is in
 *        effect. The message will be prefixed by the compiler
 *        with "<name> is deprecated: ".
 *
 *        The following types are recognized:
 *           void, int, string, object, mapping, float, closure, symbol
 *           quoted_array, mixed, null, unknown
 *        'null' is to be used as alternate type when the efun accepts
 *        the number 0 instead of the actual type.
 *---------------------------------------------------------------------------
 * make_func lang
 * --------------
 *
 * make_func implements a preprocessor used to generate the LPC compiler
 * lang.y from the file prolang.y . This step is necessary because
 * no known yacc allows to enable or disable rules conditinally.
 *
 * When make_func is run, it first reads the files 'machine.h' and
 * 'config.h' to determine which defines are used in the compilation of the
 * driver. For this make_func implements a simple C preprocessor, capable
 * of numeric expressions and simple macro expansions (no function macros),
 * which also allows the use of C-like block comments.
 *
 * From this information, make_func generates this file:
 *  lang.y
 *     This is the LPC compiler, which is created by reading and
 *     modifying the template prolang.y . During this process, these
 *     keywords are recognized and replaced:
 *       %line:  generates a #line statement to synchronize the C compiler.
 *       %typemap: generates a lookup table using types as indices
 *                 (see handle_map()).
 *       %hookmap: generates a lookup table using driverhooks as indices.
 *     In addition, the keywords %if, %elif, %else and %endif can be
 *     used in a preprocessor fashion to control which parts of prolang.y
 *     end up in lang.y and which don't.
 *---------------------------------------------------------------------------
 * make_func strings
 * -----------------
 *
 * make_func reads from the file string_spec the definition of all strings
 * which the driver shall predefine as shared strings. It creates the
 * source files stdstrings.[ch] with the implementation.
 *
 * The strings are mostly the names of lfuns called by the interpreter.
 * Predefining them as shared strings and using the resulting pointers
 * speeds up the function calls.
 *
 * string_spec is read line by line, each line defining one string:
 *
 *    id "string"
 *
 *    <id> is the symbolic name used in the compiler source, for the
 *    string <string>. The symbolic name will be made upper case and
 *    is used in two forms:
 *       STR_<id>: the pointer to the shared string
 *       SHX_<id>: the index of the shared string in the table of
 *                 all predefined strings.
 *
 * And that's it.
 *---------------------------------------------------------------------------
 */

#undef lint  /* undef so that precompiled headers can be used */

#include "driver.h"

#include "my-alloca.h"
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <fcntl.h>

#include "exec.h"
#include "hash.h"

#include "../mudlib/sys/driver_hook.h"

#define lint  /* redef again to prevent spurious warnings */

/* driver.h defines its own memory management functions,
 * which we don't need.
 */
#undef malloc
#undef realloc
#undef free

/*-------------------------------------------------------------------------*/

/* Filenames used by make_func */

#define FUNC_SPEC    "func_spec"
  /* The input file with the description of all machine instructions
   * and efuns.
   */

#define STRING_SPEC  "string_spec"
  /* The input file with the definition of all standard
   * shared strings.
   */

#define CONFIG       "config.h"
  /* The configuration file.
   */

#define FILE_MACHINE "machine.h"
  /* The machine configuration file.
   */

#define PRO_LANG     "prolang.y"
  /* The LPC parser template.
   */

#define THE_LANG     "lang.y"
  /* The LPC parser to generate.
   */

#define THE_INSTRS   "instrs.h"
  /* The instruction and ctype table declarations to generate.
   */

#define EFUN_DEFS    "efun_defs.c"
  /* The instruction and ctype table definitions to generate.
   */

#define STDSTRINGS   "stdstrings"
  /* The basename (without extension) of the standard shared
   * string implementation files to generate.
   */

/*-------------------------------------------------------------------------*/

#define MAKE_FUNC_MAXLINE    4096
  /* Maximum length of an input line and other buffers.
   */

#define MAX_FUNC      2048
#define MAX_TOKENS    2048
  /* Maximum number of functions and tokens we care to handle.
   */

#define MAX_ARGTYPES  500
  /* Size of the arg_types[] array.
   */

#define MF_TYPE_MOD_POINTER   0x10000
#define MF_TYPE_MOD_REFERENCE 0x20000
  /* Type modifier for array and reference types.
   * These values are bitflags which are |'ed onto the type values
   * produced by the parser (INT, STRING, etc).
   */

/*-------------------------------------------------------------------------*/

#undef isalunum
#define isalunum(c) (isascii((unsigned char)c) && (isalnum((unsigned char)c) || (c) == '_'))

#define lexwhite(c) (isascii((unsigned char)c) && isspace((unsigned char)c) && (c) != '\n')

#undef lexdigit
#define lexdigit(c) (isascii((unsigned char)c) && isdigit((unsigned char)c))
  /* Our own char classification predicates.
   */

#define NELEMS(arr)    (sizeof arr / sizeof arr[0])
  /* Handy macro to statically determine the number of elements in
   * an array.
   */

/*-------------------------------------------------------------------------*/

/* Parsed functions and instructions are put into different classes.
 * These are the class type definitions and associated predicate macros.
 */

enum ClassCodes {
    C_CODE  = 0  /* Internal machine instructions */
  , C_EFUN       /* Efuns with own instruction codes */
  , C_EFUN0      /* Tabled efuns with 0 arguments */
  , C_EFUN1      /* Tabled efuns with 1 argument */
  , C_EFUN2      /* Tabled efuns with 2 argument */
  , C_EFUN3      /* Tabled efuns with 3 argument */
  , C_EFUN4      /* Tabled efuns with 4 argument */
  , C_EFUNV      /* Tabled efuns with more than 4 or variable arguments */
  , C_ALIAS      /* Aliased efuns. This must come right after
                  * the last C_EFUN*.
                  */
  , C_SEFUN      /* Virtual class used by the lexer when parsing
                  * the classes C_EFUN*.
                  */
  , C_TOTAL      /* Number of different classes */
};

#define C_IS_CODE(x) \
 ((x) == C_CODE)
#define C_IS_EFUN(x) \
 ((x) != C_CODE && (x) != C_ALIAS)

/*-------------------------------------------------------------------------*/

static char * classtag[]
 = { /* C_CODE */ "CODE"
   , /* C_EFUN */ "EFUN"
   , /* C_EFUN0 */ "EFUN0"
   , /* C_EFUN1 */ "EFUN1"
   , /* C_EFUN2 */ "EFUN2"
   , /* C_EFUN3 */ "EFUN3"
   , /* C_EFUN4 */ "EFUN4"
   , /* C_EFUNV */ "EFUNV"
   };
  /* Tag names of the code classes, used to create the '<class>_OFFSET'
   * values in efun_defs.c:instrs[].
   */

static char * classprefix[]
 = { /* C_CODE */ "0"
   , /* C_EFUN */ "0"
   , /* C_EFUN0 */ "F_EFUN0"
   , /* C_EFUN1 */ "F_EFUN1"
   , /* C_EFUN2 */ "F_EFUN2"
   , /* C_EFUN3 */ "F_EFUN3"
   , /* C_EFUN4 */ "F_EFUN4"
   , /* C_EFUNV */ "F_EFUNV"
   };
  /* Instruction names of the prefixes used for the various code classes.
   */

static int num_buff = 0;
  /* Total number of instructions encountered so far.
   */

static int num_instr[C_TOTAL];
  /* Number of instructions encountered, counted separately for
   * every type.
   */

static int instr_offset[C_TOTAL];
  /* Derived from num_instrs, the offset of the instruction classes
   * within the whole table.
   */

struct instrdata_s {
    char *f_name;      /* 'F-'-name of code/efun */
    char *key;         /* internal name of code/efun */
    char *buf;         /* instrs[] entry for code/efun */
    int   code_class;  /* code class of code/efun */
} instr[MAX_FUNC];
  /* Array describing all codes and efuns encountered in FUNC_SPEC.
   * The array is built in the order found in func_spec and later
   * sorted by code_class and key.
   *
   * Also used to hold all string definitions found in STRING_SPEC:
   *   .key is (in upper case) the symbolic name of the string;
   *   .buf is the actual string text.
   */

static int arg_types[MAX_ARGTYPES];
  /* All distinct function argument signatures encountered so far.
   * One signature is simply a list of all argument types, identified
   * by the starting index of the first argument's type.
   * The description of one argument's type is itself a list of the
   * typecodes, terminated by 0.
   * Different signatures may overlap, e.g. a (STRING) signature
   * could be embedded in a (POINTER, STRING) signature.
   *
   * The content of this array is later written as efun_arg_types[]
   * into EFUN_DEFS and indexed by the instrs[].arg_index entries.
   */

static long lpc_types[MAX_ARGTYPES];
  /* The table of distinct function argument signatures again, this
   * time expressed in svalue runtime types.
   * One signature is a list of all arguments' types, identified
   * by the starting index of the first argument's type. The length
   * of the signature is stored in the instr structure for the function.
   * The description of one argument's type is a bitword, where
   * a 1 is set when the associated type is accepted.
   * For example, if an argument can be string or object, the
   * word in the table is set to (1<<T_STRING)|(1<<T_OBJECT).
   *
   * The content of this array is later written as efun_lpc_types[]
   * into EFUN_DEFS and indexed by the instrs[].lpc_arg_index entries.
   *
   * The recognized bitflags are:
   */

#    define LPC_T_ANY           (1 << 0)
#    define LPC_T_LVALUE        (1 << 1)
#    define LPC_T_POINTER       (1 << 2)
#    define LPC_T_NUMBER        (1 << 3)
#    define LPC_T_OBJECT        (1 << 4)
#    define LPC_T_STRING        (1 << 5)
#    define LPC_T_MAPPING       (1 << 6)
#    define LPC_T_FLOAT         (1 << 7)
#    define LPC_T_CLOSURE       (1 << 8)
#    define LPC_T_SYMBOL        (1 << 9)
#    define LPC_T_QUOTED_ARRAY  (1 << 10)
#ifdef USE_STRUCTS
#    define LPC_T_STRUCT        (1 << 11)
#    define LPC_T_NULL          (1 << 12)
#else
#    define LPC_T_NULL          (1 << 11)
#endif


static int last_current_type = 0;
  /* Index of the first unused entry in arg_types[].
   */

static int last_current_lpc_type = 0;
  /* Index of the first unused entry in lpc_types[].
   */

/*-------------------------------------------------------------------------*/

/* Variables used when parsing the lines of FUNC_SPEC */

static int got_error = 0;
  /* Set to TRUE if an error was found.
   */

static int min_arg = -1;
  /* Minimum number of arguments for this function if there
   * are optional arguments, or -1 if all arguments are needed.
   */

static Bool unlimit_max = MY_FALSE;
  /* True when the last argument for a function is the '...'
   */

static int current_code_class;
  /* The current code class (C_CODE, C_EFUN, ...) a parsed identifier
   * shall be assigned to.
   */

static int curr_arg_types[MAX_LOCAL];
  /* The function argument signature of the current function.
   * The signature is a list of all argument types, which themselves
   * are lists of typecodes, each argument's list terminated by 0.
   */

static int curr_lpc_types[MAX_LOCAL];
  /* The function argument signature of the current function expressed
   * in svalue types.
   * The signature is a list of all argument's types, each a word
   * denoting by set bits which bytes are accepted.
   */

static int curr_arg_type_size = 0;
  /* Index of the first unused entry in curr_arg_types[].
   */

static int curr_lpc_type_size = 0;
  /* Index of the first unused entry in curr_lpc_types[].
   */

/*-------------------------------------------------------------------------*/

/* Forward declarations */

static void yyerror(const char *) NORETURN;
static int yylex(void);
int yyparse(void);
int ungetc(int c, FILE *f);
static const char *type_str(int);
static long type2flag (int n);
static const char *etype(long);
static const char *ctype(int);
#ifndef toupper
int toupper(int);
#endif
static void fatal(const char *str) NORETURN;
static int cond_get_exp(int);

/*-------------------------------------------------------------------------*/
static char *
mystrdup (const char *str)

/* Copy <str> into a freshly allocated memory block and return that one.
 *
 * This function is needed on Ultrix 4.2 which doesn't seem to know strdup().
 */

{
    char *copy = malloc(strlen(str)+1);
    if (!copy)
        fatal("strdup failed\n");
    strcpy(copy, str);
    return copy;
}

/*-------------------------------------------------------------------------*/
static void
fatal (const char *str)

/* Print <str> on stderr, flush stdout and exit the program with
 * exitcode 1.
 */

{
    fprintf(stderr, "%s", str);
    fflush(stdout);
    exit(1);
}

/*-------------------------------------------------------------------------*/
static char *
make_f_name (char *str)

/* Take <str> and return a string 'F_<STR>' in its own memory block.
 */

{
    char f_name[500];
    size_t i, len;

    if (strlen(str) + 1 + 2 > sizeof f_name)
        fatal("A local buffer was too small!(1)\n");
    sprintf(f_name, "F_%s", str);
    len = strlen(f_name);
    for (i = 0; i < len; i++)
    {
        if (islower((unsigned char)f_name[i]))
            f_name[i] = (char)toupper(f_name[i]);
    }
    return mystrdup(f_name);
}

/*-------------------------------------------------------------------------*/
static int
check_for_duplicate_instr (const char *f_name, const char *key, int redef_ok)

/* Check if either <f_name> or <key> already appear in instr[], and print
 * appropriate diagnostics. If <redef_ok> is true, a new <key> for an existing
 * <f_name> is allowed.
 * Return true if there is a duplicate for <f_name>, false if not.
 */

{
    size_t i;
    int rc;

    rc = 0;

    for (i = 0; i < (size_t)num_buff; i++)
    {
        if (!strcmp(f_name, instr[i].f_name))
        {
            if (!strcmp(key, instr[i].key))
            {
                rc = 1;
                got_error = 1;
                fprintf(stderr, "Error: Entry '%s':'%s' duplicated.\n"
                              , f_name, key);
            }
            else if (!redef_ok)
            {
                rc = 1;
                got_error = 1;
                fprintf(stderr, "Error: Entry '%s':'%s' redefined to '%s'.\n"
                              , f_name, instr[i].key, key);
            }
        }
        else if (!strcmp(key, instr[i].key))
        {
             fprintf(stderr, "Entry '%s':'%s' duplicated as '%s':... .\n"
                           , instr[i].f_name, instr[i].key, f_name);
        }

    }
    return rc;
}

/*-------------------------------------------------------------------------*/
static int
check_for_duplicate_string (const char *key, const char *buf)

/* Check if either <key> or <buf> already appear in instr[], and print
 * appropriate diagnostics.
 * Return true if there is a duplicate for <key>, false if not.
 */

{
    size_t i;
    int rc;

    rc = 0;

    for (i = 0; i < (size_t)num_buff; i++)
    {
        if (!strcmp(key, instr[i].key))
        {
            rc = 1;
            got_error = 1;
            if (!strcmp(buf, instr[i].buf))
            {
                fprintf(stderr, "Error: Entry '%s':'%s' duplicated.\n"
                              , key, buf);
            }
            else
            {
                fprintf(stderr, "Error: Entry '%s':'%s' redefined to '%s'.\n"
                              , key, instr[i].buf, buf);
            }
        }
        else if (!strcmp(buf, instr[i].buf))
        {
             fprintf(stderr, "Warning: Entry '%s':'%s' duplicated as '%s':... .\n"
                           , instr[i].key, instr[i].buf, key);
        }

    }
    return rc;
}

#if defined(__MWERKS__) && !defined(WARN_ALL)
#    pragma warn_possunwant off
#    pragma warn_implicitconv off
#endif
%}

/*=========================================================================*/

/*                           P A R S E R                                   */

/*-------------------------------------------------------------------------*/

%union {
    int number;
    char *string;
}

%token PARSE_FUNC_SPEC PARSE_STRING_SPEC

%token NAME ID

%token VOID INT STRING OBJECT MAPPING FLOAT CLOSURE SYMBOL QUOTED_ARRAY
%token MIXED UNKNOWN NUL STRUCT

%token DEFAULT

%token CODES EFUNS TEFUNS END

%token UN_OP BIN_OP TRI_OP

%type <number> VOID MIXED UNKNOWN NUL STRUCT
%type <number> INT STRING OBJECT MAPPING FLOAT CLOSURE SYMBOL QUOTED_ARRAY
%type <number> basic arg_type
  /* Value is the basic type value
   */

%type <number> opt_star opt_ref
  /* Value is the appropriate bitflag or 0.
   */

%type <number> type
  /* Value is the complete type, incl. *-  and &-modifier bitflags
   */

%type <number> arg_list typel typel2
  /* Value is the number of arguments (so far)
   */

%type <number>optional_optype
  /* 0: No op-type given
   * 1: UN_OP, 2: BIN_OP, 3: TRI_OP
   * Or in other words: the number of operands :-)
   */

%type <string> ID optional_ID optional_default NAME optional_name
  /* Value is the parsed identifier or NULL (resp. "" for optional_ID).
   */

%%

all: PARSE_FUNC_SPEC   func_spec
   | PARSE_STRING_SPEC string_spec
;

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

func_spec: codes END efuns END tefuns;

codes:    CODES
        | codes code;

efuns:    EFUNS  funcs;

tefuns:   TEFUNS funcs;

/* --- Codes --- */

optional_name: /* empty */ { $$ = NULL; }
               | NAME;

optional_optype: /* empty */ { $$ = 0; }
               | UN_OP       { $$ = 1; }
               | BIN_OP      { $$ = 2; }
               | TRI_OP      { $$ = 3; }
               ;

code:     optional_name ID optional_optype
    {
        char *f_name, buff[500];

        if (num_buff >= MAX_FUNC)
            yyerror("Too many codes and efuns in total!\n");
        if (!$1)
            $1 = mystrdup($2);
        f_name = make_f_name($2);
        check_for_duplicate_instr(f_name, $2, 0);
        instr[num_buff].code_class = current_code_class;
        num_instr[current_code_class]++;
#ifdef USE_STRUCTS
        if ($3 == 0)
            sprintf(buff, "{ %s, %s-%s_OFFSET, 0, 0, -1, { 0, NULL } , -1, -1, \"%s\", NULL },\n"
                        , classprefix[instr[num_buff].code_class]
                        , f_name, classtag[instr[num_buff].code_class]
                        , $1);
        else
            sprintf(buff, "{ %s, %s-%s_OFFSET, %d, %d, 0, { TYPE_ANY, NULL }, -1, -1, \"%s\", NULL },\n"
                        , classprefix[instr[num_buff].code_class]
                        , f_name, classtag[instr[num_buff].code_class]
                        , $3, $3
                        , $1);
#else
        if ($3 == 0)
            sprintf(buff, "{ %s, %s-%s_OFFSET, 0, 0, -1, { 0 } , -1, -1, \"%s\", NULL },\n"
                        , classprefix[instr[num_buff].code_class]
                        , f_name, classtag[instr[num_buff].code_class]
                        , $1);
        else
            sprintf(buff, "{ %s, %s-%s_OFFSET, %d, %d, 0, { TYPE_ANY }, -1, -1, \"%s\", NULL },\n"
                        , classprefix[instr[num_buff].code_class]
                        , f_name, classtag[instr[num_buff].code_class]
                        , $3, $3
                        , $1);
#endif /* USE_STRUCTS */
        if (strlen(buff) > sizeof buff)
            fatal("Local buffer overflow!\n");
        instr[num_buff].f_name = f_name;
        instr[num_buff].key = mystrdup($2);
        instr[num_buff].buf = mystrdup(buff);
        num_buff++;
        free($1);
    }
    ;

/* --- Efuns --- */

funcs:   /* empty */
       | funcs func ;

optional_ID:   ID
             | /* empty */ { $$ = ""; } ;

optional_default:   DEFAULT ':' ID { $$ = $3; }
                  | /* empty */    { $$ = "0"; } ;

func: type ID optional_ID '(' arg_list optional_default ')' optional_name ';'
    {
        char buff[500];
        char *f_name;
        char *f_prefix;
        int  code_class;
        int i;
        int max_arg, arg_index, lpc_index;

        if (num_buff >= MAX_FUNC)
            yyerror("Too many codes and efuns in total!\n");

        if (min_arg == -1)
            min_arg = $5;

        max_arg = $5;

        /* Make the correct f_name and determine the code class
         */
        if ($3[0] == '\0')
        {
            code_class = current_code_class;
            if (current_code_class == C_SEFUN)
            {
                if (!unlimit_max && min_arg == max_arg)
                {
                    if (max_arg < 5)
                        code_class = C_EFUN0 + min_arg;
                    else
                    {
                        code_class = C_EFUNV;
                        fprintf(stderr
                               , "Efun '%s' has %d arguments and will be "
                                 "considered a 'varargs' efun.\n"
                               , $2, min_arg);
                    }
                }

                if (unlimit_max || min_arg != max_arg)
                {
                    code_class = C_EFUNV;
                }

                if (code_class == C_SEFUN)
                {
                    char buf[100];
                    sprintf(buf, "Efun '%s' can't be classified.\n", $2);
                    fatal(buf);
                }
            }

            f_name = make_f_name($2);
            check_for_duplicate_instr(f_name, $2, 0);
            instr[num_buff].code_class = code_class;
            num_instr[code_class]++;
            f_prefix = classprefix[code_class];
        }
        else
        {
            f_name = mystrdup($3);
            check_for_duplicate_instr(f_name, $3, 1);
            instr[num_buff].code_class = code_class = C_ALIAS;
            num_instr[C_ALIAS]++;
            f_prefix = NULL;
        }

        /* Search the function's signature in arg_types[] */

        for (i = 0; i < last_current_type; i++)
        {
            int j;
            for (j = 0; j+i < last_current_type && j < curr_arg_type_size; j++)
            {
                if (curr_arg_types[j] != arg_types[i+j])
                    break;
            }
            if (j == curr_arg_type_size)
                break;
        }

        if (i == last_current_type)
        {
            /* It's a new signature, put it into arg_types[] */
            int j;
            for (j = 0; j < curr_arg_type_size; j++)
            {
                arg_types[last_current_type++] = curr_arg_types[j];
                if (last_current_type == NELEMS(arg_types))
                    yyerror("Array 'arg_types' is too small");
            }
        }

        arg_index = i;

        /* Search the function's signature in lpc_types[].
         * For efuns using a (...) argument the last listed lpc_type is 0,
         * which doesn't need to be compared or stored.
         */

        for (i = 0; i < last_current_lpc_type; i++)
        {
            int j;
            Bool mismatch = MY_FALSE;

            for ( j = 0
                ; j+i < last_current_lpc_type && j < curr_lpc_type_size
                ; j++)
            {
                if ((j+1 < curr_lpc_type_size || curr_lpc_types[j] != 0)
                 && (curr_lpc_types[j] != lpc_types[i+j]))
                {
                    mismatch = MY_TRUE;
                    break;
                }
            }
            if (!mismatch)
                break;
        }

        if (i + curr_lpc_type_size > last_current_lpc_type)
        {
            /* It's a new signature, its first (last_current_lpc_type - i)
             * args matching the last entries in lpc_types.
             * TODO: An even better strategy would be to sort the signatures
             * TODO:: by size and then do the overlapping store with the
             * TODO:: longest one first.
             */

            int j;

            for (j = last_current_lpc_type - i; j < curr_lpc_type_size; j++)
            {
                if (j+1 < curr_lpc_type_size || curr_lpc_types[j] != 0)
                {
                    lpc_types[last_current_lpc_type++] = curr_lpc_types[j];
                }
                if (last_current_lpc_type == NELEMS(lpc_types))
                    yyerror("Array 'lpc_types' is too small");
            }
        }

        lpc_index = i;

        /* Store the data */

        if (code_class != C_ALIAS && code_class != C_SEFUN)
        {
            char * tag;

            tag = (code_class == C_EFUN) ? classtag[C_CODE] : classtag[code_class];
#ifdef USE_STRUCTS
            sprintf(buff, "{ %s, %s-%s_OFFSET, %d, %d, %s, { %s, NULL }, %d, %d, \"%s\""
                        , f_prefix, f_name, tag
                        , unlimit_max ? -1 : max_arg, min_arg
                        , $6, ctype($1), arg_index, lpc_index, $2
                   );
#else
            sprintf(buff, "{ %s, %s-%s_OFFSET, %d, %d, %s, { %s }, %d, %d, \"%s\""
                        , f_prefix, f_name, tag
                        , unlimit_max ? -1 : max_arg, min_arg
                        , $6, ctype($1), arg_index, lpc_index, $2
                   );
#endif /* USE_STRUCTS */
        }
        else
        {
#ifdef USE_STRUCTS
            sprintf(buff, "{ 0, 0, %d, %d, %s, { %s, NULL}, %d, %d, \"%s\""
                        , unlimit_max ? -1 : max_arg, min_arg
                        , $6, ctype($1), arg_index, lpc_index, $2
                   );
#else
            sprintf(buff, "{ 0, 0, %d, %d, %s, { %s }, %d, %d, \"%s\""
                        , unlimit_max ? -1 : max_arg, min_arg
                        , $6, ctype($1), arg_index, lpc_index, $2
                   );
#endif /* USE_STRUCTS */
        }

        if ($8 != NULL)
            sprintf(buff+strlen(buff), ", \"%s\"", $8);
        else
            strcat(buff, ", NULL");

        strcat(buff, " },\n");

        if (strlen(buff) > sizeof buff)
             fatal("Local buffer overwritten !\n");

        instr[num_buff].f_name = f_name;
        instr[num_buff].key = mystrdup($2);
        instr[num_buff].buf = mystrdup(buff);
        num_buff++;

        /* Reset for next function */

        min_arg = -1;
        unlimit_max = MY_FALSE;
        curr_arg_type_size = 0;
        curr_lpc_type_size = 0;
        curr_lpc_types[0] = 0;
    } ;

/* --- Types and Argument lists --- */

type: basic opt_star opt_ref { $$ = $1 | $2 | $3; };

basic: VOID | INT | STRING | MAPPING | FLOAT | MIXED | OBJECT | CLOSURE |
        UNKNOWN | SYMBOL | QUOTED_ARRAY | STRUCT | NUL ;

opt_star : '*' { $$ = MF_TYPE_MOD_POINTER; }
        |      { $$ = 0;                   } ;

opt_ref : '&' { $$ = MF_TYPE_MOD_REFERENCE; }
        |     { $$ = 0;                     } ;

arg_list: /* empty */             { $$ = 0; }
        | typel2                  { $$ = 1; if ($1) min_arg = 0; }
        | arg_list ',' typel2     { $$ = $1 + 1; if ($3) min_arg = $$ - 1; } ;

typel2: typel
    {
        $$ = $1;
        curr_arg_types[curr_arg_type_size++] = 0;
        if (curr_arg_type_size == NELEMS(curr_arg_types))
            yyerror("Too many arguments");
        curr_lpc_type_size++;
        if (curr_lpc_type_size == NELEMS(curr_lpc_types))
            yyerror("Too many arguments");
        curr_lpc_types[curr_lpc_type_size] = 0;
    } ;

arg_type: type
    {
        if ($1 != VOID)
        {
            if ($1 != NUL)
                curr_arg_types[curr_arg_type_size++] = $1;
            if (curr_arg_type_size == NELEMS(curr_arg_types))
                yyerror("Too many arguments");
            curr_lpc_types[curr_lpc_type_size] |= type2flag($1);
        }
        $$ = $1;
    } ;

typel: arg_type              { $$ = ($1 == VOID && min_arg == -1); }
     | typel '|' arg_type    { $$ = (min_arg == -1 && ($1 || $3 == VOID));}
     | '.' '.' '.'           { $$ = min_arg == -1 ; unlimit_max = MY_TRUE; } ;

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

string_spec: stringdefs ;

stringdefs:  /* empty */
       | stringdefs stringdef ;

stringdef: ID NAME
    {
        char *cp;

        if (num_buff >= MAX_FUNC)
            yyerror("Too many string definitions!\n");

        /* Copy the data parsed into the instr[] array */
        /* Make the correct f_name and determine the code class
         */
        check_for_duplicate_string($1, $2);
        instr[num_buff].key = mystrdup($1);
        instr[num_buff].buf = mystrdup($2);

        /* Make sure that the .key is all uppercase */
        for (cp = instr[num_buff].key; *cp != '\0'; cp++)
            if (isalpha((unsigned char)*cp) && islower((unsigned char)*cp))
                *cp = toupper(*cp);

        /* Prepare for next string */

        num_buff++;
    }
; /* stringdef */

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

%%

#ifdef __MWERKS__
#    pragma warn_possunwant reset
#    pragma warn_implicitconv reset
#endif

/*=========================================================================*/

/*                            L E X E R                                    */

/* The Lexer is used to parse CONFIG, FILE_MACHINE, FUNC_SPEC and STRING_SPEC,
 * and to create THE_LANG from PRO_LANG.
 */

/*-------------------------------------------------------------------------*/

/* The recognized type names */

struct type {
    const char *name;  /* name of the type */
    int         num;   /* the type's parser code */
};

static struct type types[]
  = { { "void",         VOID }
    , { "int",          INT }
    , { "string",       STRING }
    , { "object",       OBJECT }
    , { "mapping",      MAPPING }
    , { "float",        FLOAT }
    , { "closure",      CLOSURE }
    , { "symbol",       SYMBOL }
    , { "quoted_array", QUOTED_ARRAY }
    , { "mixed",        MIXED }
    , { "null",         NUL }
    , { "unknown",      UNKNOWN }
#ifdef USE_STRUCTS
    , { "struct",       STRUCT }
#endif
    };

/*-------------------------------------------------------------------------*/

/* Defined macros.
 *
 * The macros are kept in the usual hashtable.
 */
#define MAKE_FUNC_DEFHASHBITS 7
#define MAKE_FUNC_DEFHASH (1 << MAKE_FUNC_DEFHASHBITS)
static INLINE hash16_t defhash(const char *str)
{
    size_t len = strlen(str);
    if (len > 12)
        len = 12;
    return hashmem32(str, len) & hashmask(MAKE_FUNC_DEFHASHBITS);
}

struct defn {
    char *name;         /* Macro name */
    char *exps;         /* Expanded macro text */
    int  num_arg;       /* Number of arguments or -1 for plain macros */
    struct defn *next;  /* Next entry in the hash chain */
};

static struct defn *deftab[MAKE_FUNC_DEFHASH];

/*-------------------------------------------------------------------------*/

/* The stack to handle pending #ifs.
 */

static struct ifstate {
    struct ifstate *next;
    int state;
} *iftop = NULL;

/* Values of ifstate.state: */

#define EXPECT_ELSE   1
#define EXPECT_ENDIF  2

/*-------------------------------------------------------------------------*/

static FILE *fpr, *fpw;
  /* Input and output file.
   */

static int current_line;
  /* Number of the current line.
   */

static const char *current_file;
  /* Name of the current file.
   */

static int last_line;
  /* Last line outside an %if-block.
   */

static char *outp;
  /* Next unprocessed character in the input buffer
   */

static int parsetype;
  /* Either PARSE_FUNC_SPEC or PARSE_STRING_SPEC, sent as first token back
   * to the parser.
   */

static Bool parsetype_sent = MY_FALSE;
  /* Set to TRUE after parsetype was sent to the parser.
   */

/*-------------------------------------------------------------------------*/

/* The definitions and tables for the preprocessor expression evaluator.
 * For a detailed explanation look into lex.c - it uses slightly
 * different tables, but the basic structure is the same.
 */


#define BNOT   1
#define LNOT   2
#define UMINUS 3
#define UPLUS  4

#define MULT   1
#define DIV    2
#define MOD    3
#define BPLUS  4
#define BMINUS 5
#define LSHIFT 6
#define RSHIFT 7
#define LESS   8
#define LEQ    9
#define GREAT 10
#define GEQ   11
#define EQ    12
#define NEQ   13
#define BAND  14
#define XOR   15
#define BOR   16
#define LAND  17
#define LOR   18
#define QMARK 19

static char _optab[]=
{0,4,0,0,0,26,56,0,0,0,18,14,0,10,0,22,0,0,0,0,0,0,0,0,0,0,0,0,30,50,40,74,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,70,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,63,0,1};
static char optab2[]=
{BNOT,0,0,LNOT,'=',NEQ,7,0,0,UMINUS,0,BMINUS,10,UPLUS,0,BPLUS,10,
0,0,MULT,11,0,0,DIV,11,0,0,MOD,11,
0,'<',LSHIFT,9,'=',LEQ,8,0,LESS,8,0,'>',RSHIFT,9,'=',GEQ,8,0,GREAT,8,
0,'=',EQ,7,0,0,0,'&',LAND,3,0,BAND,6,0,'|',LOR,2,0,BOR,4,
0,0,XOR,5,0,0,QMARK,1};
#define optab1(c) (_optab[(c)-' '])

/*-------------------------------------------------------------------------*/
static char
mygetc (void)

/* Return the next character from the input buffer.
 */

{
    return *outp++;
}

/*-------------------------------------------------------------------------*/
static void
myungetc (char c)

/* Unput character <c> so that the next mygetc() returns it.
 */

{
    *--outp = c;
}

/*-------------------------------------------------------------------------*/
static void
add_input (const char *p)

/* Insert text <p> at the current point in the input stream so that
 * the next mygetc()s will read it.
 */

{
    size_t l;

    l = strlen(p);
    outp -= l;
    strncpy(outp, p, l);
}

/*-------------------------------------------------------------------------*/
static void
add_define (const char * name, int num_arg, const char *exps)

/* Add the definition for the macro <name> with <num_arg> arguments
 * and replacement text <exps> to the table of macros.
 */

{
    hash16_t i;
    struct defn *ndef;

    i = defhash(name);
    ndef = malloc(sizeof(struct defn));
    if (!ndef)
    {
        abort();
    }
    ndef->next    = deftab[i];
    ndef->exps    = mystrdup(exps);
    ndef->num_arg = num_arg;
    ndef->name    = mystrdup(name);
    deftab[i]     = ndef;
}

/*-------------------------------------------------------------------------*/
static struct defn *
lookup_define (const char *s)

/* Lookup the macro <s> and return a pointer to its defn structure.
 * Return NULL if the macro is not defined.
 */

{
    struct defn *curr, *prev;
    hash16_t h;

    h = defhash(s);

    curr = deftab[h];
    prev = 0;
    while (curr)
    {
        if (!strcmp(curr->name, s)) /* found it */
        {
            if (prev) /* not at head of list */
            {
                prev->next = curr->next;
                curr->next = deftab[h];
                deftab[h] = curr;
            }
            return curr;
        }
        prev = curr;
        curr = curr->next;
    }

    /* not found */
    return NULL;
}

/*-------------------------------------------------------------------------*/
static int
expand_define (char *s)

/* Try to expand macro <s>.
 * If it is not defined, just return 0.
 * If it is a plain macro, add it to the input stream and return 1.
 * If it is a function macro, just return -1.
 */

{
    struct defn *p;

    p = lookup_define(s);
    if (!p)
        return 0;
    if (p->num_arg < 0)
    {
        add_input(p->exps);
    }
    else
    {
        return -1;
    }
    return 1;
}

/*-------------------------------------------------------------------------*/
static char *
nextword (char *str)

/* Find the next word in the input stream starting with <str> and
 * return a pointer to its first character. The end of the word
 * is marked with a '\0'.
 */

{
    char *cp;

    while (!lexwhite(*str)) str++;
    while ( lexwhite(*str)) str++;
    for (cp = str; isalunum(*cp); ) cp++;
    *cp = '\0';
    return str;
}

/*-------------------------------------------------------------------------*/
static Bool
skip_to (char mark, const char *token, const char *atoken)

/* Skip the file fpr linewise until one of the following control statements
 * is encountered:
 *   <mark>token:  returns true
 *   <mark>atoken: returns false (only if <atoken> is not NULL).
 *
 * An end of file aborts the program.
 * Nested <mark>if...<mark>endif blocks are skipped altogether.
 */

{
    char  b[20]; /* The word after <mark> */
    char *p;
    int   c;     /* Current character */
    int   nest;  /* <mark>if nesting depth */
    FILE *fp = fpr;

    for (nest = 0;;)
    {
        current_line++;
        c = fgetc(fp);

        if (c == mark)
        {
            /* Get the statement word into b[] */

            do {
                c = fgetc(fp);
            } while(lexwhite(c));

            for (p = b; c != '\n' && c != EOF; )
            {
                if (p < b+sizeof b-1)
                    *p++ = (char)c;
                c = fgetc(fp);
            }
            *p++ = '\0';
            for(p = b; *p && !lexwhite(*p); p++) NOOP;
            *p = '\0';

            if (strcmp(b, "if") == 0
             || strcmp(b, "ifdef") == 0
             || strcmp(b, "ifndef") == 0)
            {
                nest++;
            }
            else if (nest > 0)
            {
                if (strcmp(b, "endif") == 0)
                    nest--;
            }
            else
            {
                if (strcmp(b, token) == 0)
                    return MY_TRUE;
                else if (atoken && strcmp(b, atoken) == 0)
                    return MY_FALSE;
            }
        }
        else
        {
            /* Skip the line altogether */

            while (c != '\n' && c != EOF)
            {
                c = fgetc(fp);
            }
            if (c == EOF)
            {
                fprintf(stderr, "Unexpected end of file while skipping");
                abort();
            }
        }
    } /* for() */

    /* NOTREACHED */
} /* skip_to() */

/*-------------------------------------------------------------------------*/
static void
compensate_lines (void)

/* To compensate for skipped blocks, print as many \n to fpw as
 * the difference between last_line and current_line determines.
 * last_line is current_line+1 at the end.
 */
{
    for (; last_line <= current_line; last_line++)
        fputc('\n', fpw);
}

/*-------------------------------------------------------------------------*/
static void
handle_cond (char mark, int c)

/* Evaluate the boolean condition <c> of a preprocessor #if statement.
 * If necessary, skip to the condition branch to read next, and/or
 * push a new state onto the ifstate-stack.
 * If <mark> is '%', the number of skipped lines is compensated.
 */

{
    struct ifstate *p;

    if (c || skip_to(mark, "else", "endif"))
    {
        p = (struct ifstate *)malloc(sizeof(struct ifstate));
        p->next = iftop;
        iftop = p;
        p->state = c ? EXPECT_ELSE : EXPECT_ENDIF;
    }
    if (mark == '%')
        compensate_lines();
}

/*-------------------------------------------------------------------------*/
static void
handle_if (char mark, const char *str)

/* Evaluate the <mark>if condition <str>
 */

{
    int cond;

    add_input(str);
    cond = cond_get_exp(0);
    if (mygetc() != '\n')
    {
        yyerror("Condition too complex in #/%if");
        fflush(stdout);
        if (mark == '%')
            exit(1);
        while(mygetc() != '\n') NOOP;
    }
    else
        handle_cond(mark, cond);
}

/*-------------------------------------------------------------------------*/
static void
handle_else (char mark)

/* Handle an <mark>else statement.
 */

{
    if (iftop && iftop->state == EXPECT_ELSE)
    {
        struct ifstate *p = iftop;

        iftop = p->next;
        free((char *)p);
        skip_to(mark, "endif", (const char *)0);
    }
    else
    {
        fprintf(stderr, "Unexpected #/%%else line %d\n", current_line);
        abort();
    }
}

/*-------------------------------------------------------------------------*/
static void
handle_endif (void)

/* Handle an <mark>else statement.
 */

{
    if (iftop
     && (   iftop->state == EXPECT_ENDIF
         || iftop->state == EXPECT_ELSE))
    {
        struct ifstate *p = iftop;

        iftop = p->next;
        free((char *)p);
    }
    else
    {
        fprintf(stderr, "Unexpected #/%%endif line %d\n", current_line);
        abort();
    }
}

/*-------------------------------------------------------------------------*/
static int
name_to_type (char *name)

/* Return the proper TYPE_ value for the type <name> which must
 * start with 'TYPE_'.
 * Return -1 if the name was not recognized.
 */

{
    while (isspace((unsigned char)*name))
        name++;
    if ( strncmp(name, "TYPE_", (size_t)5) )
        return -1;
    name += 5;
    if ( !strcmp(name, "ANY") )
        return TYPE_ANY;
    if ( !strcmp(name, "NUMBER") )
        return TYPE_NUMBER;
    if ( !strcmp(name, "FLOAT") )
        return TYPE_FLOAT;
    if ( !strcmp(name, "STRING") )
        return TYPE_STRING;
    if ( !strcmp(name, "OBJECT") )
        return TYPE_OBJECT;
    if ( !strcmp(name, "MAPPING") )
        return TYPE_MAPPING;
    if ( !strcmp(name, "CLOSURE") )
        return TYPE_CLOSURE;
#ifdef USE_STRUCTS
    if ( !strcmp(name, "STRUCT") )
        return TYPE_STRUCT;
#endif
    return -1;
}

/*-------------------------------------------------------------------------*/
static int
name_to_hook(char *name)

/* Return the proper H_ value for the driverhook <name> which must
 * start with 'H_'
 * Return -1 if the name was not recognized.
 */

{
    while (isspace((unsigned char)*name))
        name++;
    if ( strncmp(name, "H_", (size_t)2) )
        return -1;
    name += 2;
    if ( !strcmp(name, "MOVE_OBJECT0") )
        return H_MOVE_OBJECT0;
    if ( !strcmp(name, "MOVE_OBJECT1") )
        return H_MOVE_OBJECT1;
    if ( !strcmp(name, "LOAD_UIDS") )
        return H_LOAD_UIDS;
    if ( !strcmp(name, "CLONE_UIDS") )
        return H_CLONE_UIDS;
    if ( !strcmp(name, "CREATE_SUPER") )
        return H_CREATE_SUPER;
    if ( !strcmp(name, "CREATE_OB") )
        return H_CREATE_OB;
    if ( !strcmp(name, "CREATE_CLONE") )
        return H_CREATE_CLONE;
    if ( !strcmp(name, "RESET") )
        return H_RESET;
    if ( !strcmp(name, "CLEAN_UP") )
        return H_CLEAN_UP;
    if ( !strcmp(name, "MODIFY_COMMAND") )
        return H_MODIFY_COMMAND;
    if ( !strcmp(name, "NOTIFY_FAIL") )
        return H_NOTIFY_FAIL;
    if ( !strcmp(name, "NO_IPC_SLOT") )
        return H_NO_IPC_SLOT;
    if ( !strcmp(name, "INCLUDE_DIRS") )
        return H_INCLUDE_DIRS;
    if ( !strcmp(name, "TELNET_NEG") )
        return H_TELNET_NEG;
    if ( !strcmp(name, "NOECHO") )
        return H_NOECHO;
    if ( !strcmp(name, "ERQ_STOP") )
        return H_ERQ_STOP;
    if ( !strcmp(name, "MODIFY_COMMAND_FNAME") )
        return H_MODIFY_COMMAND_FNAME;
    if ( !strcmp(name, "COMMAND") )
        return H_COMMAND;
    if ( !strcmp(name, "SEND_NOTIFY_FAIL") )
        return H_SEND_NOTIFY_FAIL;
    if ( !strcmp(name, "AUTO_INCLUDE") )
        return H_AUTO_INCLUDE;
    if ( !strcmp(name, "DEFAULT_METHOD") )
        return H_DEFAULT_METHOD;
    if ( !strcmp(name, "DEFAULT_PROMPT") )
        return H_DEFAULT_PROMPT;
    if ( !strcmp(name, "PRINT_PROMPT") )
        return H_PRINT_PROMPT;
    if ( !strcmp(name, "REGEXP_PACKAGE") )
        return H_REGEXP_PACKAGE;
    if ( !strcmp(name, "MSG_DISCARDED") )
        return H_MSG_DISCARDED;
    return -1;
}

/*-------------------------------------------------------------------------*/
static void
handle_map (char *str, int size, int (* name_to_index)(char *) )

/* Create a lookup table with <size> elements from the input
 * text <str> (and possibly more lines read from fpr if <str>
 * ends in \<newline>).
 *
 * The input text is supposed to be a comma separated list
 * of <index>:<value> pairs: the text "<index>" is parsed
 * and translated into a number by <name_to_index>().
 * <value> is the assigned to the table element indexed
 * by this number.
 *
 * Unassigned table elements default to the text '0' or
 * to the value set with a 'default:<value>' pair.
 *
 * After the list of pairs is read, the text for the table
 * is written as '{ <value>, ... , <value> }' to fpw.
 */

{
    char **map;        /* The map */
    char deflt[256];   /* Default table entry text */
    char *del = NULL;  /* Position of the current ':' or ',' */
    char *val;         /* Position of the current <value> */
    int i;
    char *output_del = "";  /* How to separate table entries */

    /* Initialize the map */

    map = (char **)alloca(size * sizeof *map);
    strcpy(deflt, "0");
    for (i = 0; i < size; i++) {
        map[i] = deflt;
    }

    /* Process the input list */

    do {

        /* Set str to the next non-blank character,
         * reading a new line if necessary
         */
        while (isspace((unsigned char)*str))
            str++;
        if (*str == '\\')
        {
            str = alloca((size_t)MAKE_FUNC_MAXLINE + 1);
            if (!fgets(str, MAKE_FUNC_MAXLINE, fpr))
                break;
            current_line++;
            if (del)
            {
                output_del = "\n";
            }
        }

        /* Find and mark the elements of the next pair */

        del = strchr(str, ':');
        if (!del)
            break;
        *del = '\0';
        val = del+1;
        del = strchr(val, ',');

        if (!del)
        {
            del = strchr(val, '\n');
            if (del)
            {
                *del = '\0';
                del = NULL;
            }
        }
        else
        {
            *del = '\0';
        }

        /* Evaluate the current pair */
        if ( !strcmp(str, "default") )
        {
            strncpy(deflt, val, sizeof(deflt));
            deflt[sizeof deflt - 1] = '\0';
        }
        else
        {
            i = (*name_to_index)(str);
            if (i < 0)
            {
                fprintf(stderr, "Can't translate '%s' into an index.\n", str);
                exit(-1);
            }
            map[i] = val;
        }
        str = del+1;
    } while (del);

    /* Write the generated map */

    fprintf(fpw, "{");
    fputs(output_del, fpw);
    for (i = 0; i < size; i++)
    {
        fprintf(fpw, "%s,", map[i]);
        fputs(output_del, fpw);
    }
    fprintf(fpw, "};\n");
} /* handle_map() */

/*-------------------------------------------------------------------------*/
static int
exgetc(void)

/* Get the first character of the next element of a condition
 * and return it, leaving the input pointing to the rest of it.
 * Comments are skipped, identifiers not defined as macros are
 * replaced with ' 0 ', the predicate 'defined(<name>)' is
 * replaced with ' 0 ' or ' 1 ' depending on the result.
 */

{
    char c;

    c = mygetc();
    while (isalpha((unsigned char)c) || c == '_' )
    {
        char word[512], *p;
        int space_left;

        p = word;
        space_left = sizeof(word);
        do {
            *p++ = c;
            c = mygetc();
        } while (isalunum(c) && --space_left);
        if (!space_left)
             fatal("Too long word.\n");
        myungetc(c);
        *p = '\0';
        if (strcmp(word, "defined") == 0)
        {
            /* handle the defined "function" in #if */
            do c = mygetc(); while(lexwhite(c));
            if (c == '(')
            {
                // skip whitespaces between '(' and keyword.
                do c = mygetc(); while(lexwhite(c));
            }

            p = word;
            space_left = sizeof(word);
            while ( isalunum(c) && --space_left) {
                *p++ = c;
                c = mygetc();
            }
            *p = '\0';
            // skip all whitespaces following the keyword
            while(lexwhite(c)) c = mygetc();
            // if this is already the end of the line, we have to go back, so
            // that it is read again on next mygetc().
            if (c == '\n')
                myungetc((char)c);
            // if the keywords was enclose in '()', we are now at ')'. If not,
            // we may be at the next expression and have to go back one as
            // well.
            if (c != ')')
                myungetc((char)c);

            if (lookup_define(word))
                add_input(" 1 ");
            else
                add_input(" 0 ");
        }
        else
        {
            int res;

            res = expand_define(word);
            if (res < 0)
            {
                yyerror("Unimplemented macro expansion");
                return 0;
            }
            if (!res) add_input(" 0 ");
        }
        c = mygetc();
    }
    return c;
}

/*-------------------------------------------------------------------------*/
static int
cond_get_exp (int priority)

/* Evaluate the expression in the input buffer at a priority of at least
 * <priority> and return the result.
 *
 * The function assumes to be called at the proper beginning of
 * an expression, i.e. if it encounters an operator even before a value,
 * it must be unary.
 */

{
    int c;
    int value,value2,x;

    do c = exgetc(); while ( lexwhite(c) );

    /* Evaluate the first value */

    if (c == '(')
    {

        /* It's a parenthesized subexpression */

        value = cond_get_exp(0);
        do c = exgetc(); while ( lexwhite(c) );
        if (c != ')' )
        {
            yyerror("bracket not paired in #if");
            if (!c)
                myungetc('\0');
        }
    }
    else if ( ispunct(c) )
    {
        /* It is an unary operator */

        x = optab1(c);
        if (!x)
        {
            yyerror("illegal character in #if");
            return 0;
        }

        /* Get the value for this unary operator */
        value = cond_get_exp(12);

        /* Evaluate the operator */
        switch ( optab2[x-1] )
        {
        case BNOT  : value = ~value; break;
        case LNOT  : value = !value; break;
        case UMINUS: value = -value; break;
        case UPLUS : value =  value; break;
        default :
            yyerror("illegal unary operator in #if");
            return 0;
        }
    }
    else
    {
        /* It must be a number */

        int base;

        if ( !lexdigit(c) )
        {
            if (!c)
            {
                yyerror("missing expression in #if");
                myungetc('\0');
            }
            else
                yyerror("illegal character in #if");
            return 0;
        }
        value = 0;

        /* Determine the base of the number */
        if (c != '0')
            base=10;
        else
        {
            c = mygetc();
            if (c == 'x' || c == 'X' )
            {
                base = 16;
                c = mygetc();
            }
            else
                base = 8;
        }

        /* Now parse the number */
        for(;;)
        {
            if ( isdigit(c) )      x = -'0';
            else if ( isupper(c) ) x = -'A'+10;
            else if ( islower(c) ) x = -'a'+10;
            else break;
            x += c;
            if (x > base)
                break;
            value = value * base + x;
            c = mygetc();
        }
        myungetc((char)c);
    }

    /* Now evaluate the following <binop> <expr> pairs (if any) */

    for (;;)
    {
        do c = exgetc(); while ( lexwhite(c) );

        /* An operator must come next */
        if ( !ispunct(c) )
            break;

        /* Can it be an operator at all? */
        x = optab1(c);
        if (!x)
            break;

        /* See if the optab[] defines an operator for these characters
         */
        value2 = mygetc();
        for (;; x += 3)
        {
            if (!optab2[x])
            {
                myungetc((char)value2);
                if ( !optab2[x+1] )
                {
                    yyerror("illegal operator use in #if");
                    return 0;
                }
                break;
            }
            if (value2 == optab2[x])
                break;
        }

        /* If the priority of the operator is too low, we are done
         * with this (sub)expression.
         */
        if (priority >= optab2[x+2])
        {
            if (optab2[x])
                myungetc((char)value2);
            break;
        }

        /* Get the second operand and perform the operation */
        value2 = cond_get_exp(optab2[x+2]);

        switch ( optab2[x+1] )
        {
        case MULT   : value *= value2;          break;
        case DIV    : value /= value2;          break;
        case MOD    : value %= value2;          break;
        case BPLUS  : value += value2;          break;
        case BMINUS : value -= value2;          break;
        case LSHIFT : value <<= value2;         break;
        case RSHIFT : value >>= value2;         break;
        case LESS   : value = value <  value2;  break;
        case LEQ    : value = value <= value2;  break;
        case GREAT  : value = value >  value2;  break;
        case GEQ    : value = value >= value2;  break;
        case EQ     : value = value == value2;  break;
        case NEQ    : value = value != value2;  break;
        case BAND   : value &= value2;          break;
        case XOR    : value ^= value2;          break;
        case BOR    : value |= value2;          break;
        case LAND   : value = value && value2;  break;
        case LOR    : value = value || value2;  break;
        case QMARK  :
            do c=exgetc(); while( lexwhite(c) );
            if ( c!=':' )
            {
                yyerror("'?' without ':' in #if");
                myungetc((char)c);
                return 0;
            }
            if ( value )
            {
                cond_get_exp(1);
                value = value2;
            }
            else
                value = cond_get_exp(1);
            break;
        } /* switch() */
  } /* for() */

  myungetc((char)c);
  return value;
} /* cond_get_expr() */

/*-------------------------------------------------------------------------*/
static Bool
make_func_isescaped (char c)

/* Return true if <c> is one of the escapable characters (e.g. \r or \"),
 * false if not.
 */

{
    switch(c) {
      case '\007':
      case '\b'  :
      case '\t'  :
      case '\n'  :
      case '\013':
      case '\014':
      case '\r'  :
        return MY_TRUE;
    }
    if (c == '\\' || c == '\"')
        return MY_TRUE;
    return MY_FALSE;
}

/*-------------------------------------------------------------------------*/
static Bool
make_func_issavedel (char c)

/* Return true if <c> is a delimiter in save/restore object (e.g. ',' or ';'),
 * false if not.
 */

{
    switch(c) {
      case ',' : /* in Arrays   */
      case ';' : /* in Mappings */
      case ':' : /* in Mappings */
      case '|' : /* in Closures */
      case '\n':
        return MY_TRUE;
    }
    return MY_FALSE;
}

/*-------------------------------------------------------------------------*/
static int
ident (char c)

/* Parse an identifier (first character is <c>) from fpr and classify it.
 *
 * The typenames in types[] return the associated type code, the
 * string "default" returns DEFAULT (both only when currently parsing
 * for EFUN class identifiers in FUNC_SPEC).
 *
 * When parsing CODEs in FUNC_SPEC, the strings "unary", "binary" and
 * "ternary" return UN_OP, BIN_OP and TRI_OP respectively.
 *
 * Other identifiers are stored in yylval.string and return ID.
 */

{
    char buff[100];
    size_t len, i;

    for (len = 0; isalunum(c); c = (char)getc(fpr))
    {
        if (len == sizeof buff - 1)
        {
            yyerror("Too long indentifier");
            do c = (char)getc(fpr); while (isalunum(c));
            break;
        }
        buff[len++] = c;
    }
    (void)ungetc(c, fpr);

    buff[len] = '\0';

    if ( parsetype == PARSE_FUNC_SPEC && C_IS_EFUN(current_code_class) )
    {
        for (i=0; i < NELEMS(types); i++)
        {
            if (strcmp(buff, types[i].name) == 0)
            {
                yylval.number = types[i].num;
                return types[i].num;
            }
        }
        if (strcmp(buff, "default") == 0)
            return DEFAULT;
    }

    if ( parsetype == PARSE_FUNC_SPEC && C_IS_CODE(current_code_class) )
    {
        if (strcmp(buff, "unary") == 0)
            return UN_OP;
        if (strcmp(buff, "binary") == 0)
            return BIN_OP;
        if (strcmp(buff, "ternary") == 0)
            return TRI_OP;
    }
    yylval.string = mystrdup(buff);
    return ID;
} /* ident() */

/*-------------------------------------------------------------------------*/
static const char *
type_str (int n)

/* Create a string representation of type <n> in a static buffer
 * and return it. Unknown types return 'What?'.
 */

{
    int type = n & 0xffff;
    size_t i;

    for (i = 0; i < NELEMS(types); i++)
    {
        if (types[i].num == type)
        {
            if (n & MF_TYPE_MOD_REFERENCE)
            {
                static char buff[100];
                const char *str;

                str = type_str(n & ~MF_TYPE_MOD_REFERENCE);
                if (strlen(str) + 3 > sizeof buff)
                    fatal("Local buffer too small in type_str()!\n");
                sprintf(buff, "%s &", str);
                return buff;
            }
            if (n & MF_TYPE_MOD_POINTER)
            {
                static char buff[100];

                if (strlen(types[i].name) + 3 > sizeof buff)
                    fatal("Local buffer too small in type_str()!\n");
                sprintf(buff, "%s *", types[i].name);
                return buff;
            }
            return types[i].name;
        }
    }
    return "What?";
} /* type_str() */

/*-------------------------------------------------------------------------*/
static void
skip_comment (void)

/* Skip a block comment on the input stream fpr. It is assumed that
 * the begin marker was already read.
 */

{
    int c;

    for(;;) {
        while((c = getc(fpr)) != '*')
        {
            if (c == EOF)
            {
                yyerror("End of file in a comment");
                return;
            }
            if (c == '\n') {
                current_line++;
            }
        }
        do {
            if ((c = getc(fpr)) == '/')
                return;
            if (c == '\n') {
                current_line++;
            }
        } while(c == '*');
    }
} /* skip_comment() */

/*-------------------------------------------------------------------------*/
static int
yylex1 (void)

/* Parse the next lexical element from the input file and return
 * its token value and -1 on end of file.
 */

{
    register int c;

    for(;;)
    {
        size_t match_tmp;
#define MATCH(str) (isspace((unsigned char)line_buffer[match_tmp=strlen(str)]) &&\
                        strncmp(str, line_buffer, match_tmp) == 0)

        char line_buffer[MAKE_FUNC_MAXLINE+1];
        char defbuf[MAKE_FUNC_MAXLINE + 1];

        outp = defbuf + sizeof(defbuf) - 1;

        line_buffer[MAKE_FUNC_MAXLINE] = '\0';

        switch(c = getc(fpr))
        {
        case ' ':
        case '\t':
        case '\r':
            continue;

        case '#':
        {
            int line;
            char file[MAXPATHLEN+1];

            fgets(line_buffer, MAKE_FUNC_MAXLINE, fpr);
            if ( sscanf(line_buffer, "%d \"%s\"",&line,file ) == 2 )
            {
                current_line = line+1;
                continue;
            }
            current_line++;
            if MATCH("if") {
                handle_if('#', line_buffer+3);
            } else if MATCH("ifdef") {
                handle_cond('#', lookup_define(nextword(line_buffer)) != 0);
            } else if MATCH("ifndef") {
                handle_cond('#', lookup_define(nextword(line_buffer)) == 0);
                continue;
            } else if MATCH("else") {
                handle_else('#');
            } else if MATCH("endif") {
                handle_endif();
            } else {
                yyerror("unrecognised '#' directive\n");
            }
            continue;
        }

        case '%':
        {
            static int send_end = 0;

            if (send_end)
            {
                send_end = 0;
                ungetc('%', fpr);
                return END;
            }
            send_end = 1;
            fgets(line_buffer, MAKE_FUNC_MAXLINE, fpr);
            current_line++;
            if (parsetype == PARSE_FUNC_SPEC)
            {
                if (MATCH("codes"))  { current_code_class=C_CODE;  return CODES;  }
                if (MATCH("efuns"))  { current_code_class=C_EFUN;  return EFUNS;  }
                if (MATCH("tefuns")) { current_code_class=C_SEFUN; return TEFUNS; }
            }
            return '%';

#undef MATCH
        }

        case '\"':
        {
            char buff[100];
            int len;

            for (len=0; c = getc(fpr),c != '\"';)
            {
                if (len == sizeof buff - 1)
                {
                    yyerror("Too long name");
                    do
                        c = getc(fpr);
                    while (c != '\"' && c != '\n' && c != EOF);
                    (void)ungetc(c, fpr);
                    break;
                }
                if (c == '\n' || c == EOF)
                {
                    yyerror("unterminated name");
                    (void)ungetc(c, fpr);
                    break;
                }
                buff[len++] = (char)c;
            }
            buff[len] = '\0';
            yylval.string = mystrdup(buff);
            return NAME;
        }

        case '\n':
          current_line++;
          continue;

        case EOF:
            return -1;

        case '/':
            if ( (c=getc(fpr)) == '*') {
                skip_comment();
                continue;
            } else {
                (void)ungetc(c, fpr);
                return '/';
            }
        default:
            if (isalpha(c))
                return ident((char)c);
            return c;
        }
    } /* for() */
} /* yylex1() */

/*-------------------------------------------------------------------------*/
static int
yylex (void)

/* Parse the next lexical element from the input file and return
 * its token value and -1 on end of file.
 * This just calls yylex1().
 */

{
    int res;

    if (!parsetype_sent)
    {
        parsetype_sent = MY_TRUE;
        return parsetype;
    }

    res = yylex1();
#if 0
    fprintf(stderr,"yylex returns %d '%c'\n",res,res);
#endif
    return res;
}

/*-------------------------------------------------------------------------*/
static void
yyerror (const char *str)

/* Print the error message <str> with information about the current
 * parsing position and exit.
 */

{
    fprintf(stderr, "%s:%d: %s\n", current_file, current_line, str);
    exit(1);
}

/*=========================================================================*/

/*-------------------------------------------------------------------------*/
static const char *
etype (long n)

/* Express type <n> (in bitfield encoding) in the runtime type symbols
 * of interpret.h.
 * Return a pointer to a constant string.
 */

{
    static char str[200];

    if (n & LPC_T_ANY)
        return "TF_ANYTYPE";

    if (!n)
        return "0";

    str[0] = '\0';

#   define CONVERT(type, string) \
    if (n & type) \
    { \
        if (str[0] != '\0') \
            strcat(str, "|"); \
        strcat(str, string); \
        n ^= type; \
    }

    CONVERT(LPC_T_LVALUE, "TF_LVALUE");
    CONVERT(LPC_T_POINTER, "TF_POINTER");
    CONVERT(LPC_T_NUMBER, "TF_NUMBER");
    CONVERT(LPC_T_STRING, "TF_STRING");
    CONVERT(LPC_T_OBJECT, "TF_OBJECT");
    CONVERT(LPC_T_MAPPING, "TF_MAPPING");
    CONVERT(LPC_T_FLOAT, "TF_FLOAT");
    CONVERT(LPC_T_CLOSURE, "TF_CLOSURE");
    CONVERT(LPC_T_SYMBOL, "TF_SYMBOL");
    CONVERT(LPC_T_QUOTED_ARRAY, "TF_QUOTED_ARRAY");
    CONVERT(LPC_T_NULL, "TF_NULL");
#ifdef USE_STRUCTS
    CONVERT(LPC_T_STRUCT, "TF_STRUCT");
#endif /* USE_STRUCTS */

#   undef CONVERT

    if (n != 0)
    {
        printf("Error: Can't convert bit-flags %lx to LPC runtime type.\n"
              , (long)n);
        yyerror("Illegal type for argument");
        return "What ?";
    }

    return str;
} /* etype() */

/*-------------------------------------------------------------------------*/
static long
type2flag (int n)

/* Convert type <n> into the bitfield value needed to create the LPC
 * argument types.
 */

{
    if (n & MF_TYPE_MOD_REFERENCE)
        return LPC_T_LVALUE;

    if (n & MF_TYPE_MOD_POINTER)
        return LPC_T_POINTER;

    n &= ~(MF_TYPE_MOD_REFERENCE|MF_TYPE_MOD_POINTER);

    switch(n) {
      case VOID:    return 0;            break;
      case STRING:  return LPC_T_STRING; break;
      case INT:     return LPC_T_NUMBER; break;
      case OBJECT:  return LPC_T_OBJECT; break;
      case MAPPING: return LPC_T_MAPPING; break;
      case FLOAT:   return LPC_T_FLOAT;   break;
      case CLOSURE: return LPC_T_CLOSURE; break;
      case SYMBOL:  return LPC_T_SYMBOL;  break;
      case MIXED:   return LPC_T_ANY;     break;
      case NUL:     return LPC_T_NULL;     break;
      case UNKNOWN: return LPC_T_ANY;     break;
      case QUOTED_ARRAY:
        return LPC_T_QUOTED_ARRAY; break;
#ifdef USE_STRUCTS
      case STRUCT:  return LPC_T_STRUCT;  break;
#endif
    default: yyerror("(type2flag) Bad type!"); return 0;
    }
} /* type2flag() */

/*-------------------------------------------------------------------------*/
static const char *
ctype (int n)

/* Express type <n> in the compiler type symbols of exec.h.
 * Return a pointer to a constant string.
 */

{
    static char buff[100];        /* 100 is such a comfortable size :-) */
    const char *p = NULL;

    buff[0] = '\0';
    if (n & MF_TYPE_MOD_REFERENCE)
        strcat(buff, "TYPE_MOD_REFERENCE|");
    if (n & MF_TYPE_MOD_POINTER)
        strcat(buff, "TYPE_MOD_POINTER|");
    n &= ~(MF_TYPE_MOD_REFERENCE|MF_TYPE_MOD_POINTER);
    switch(n) {
      case VOID:    p = "TYPE_VOID";    break;
      case STRING:  p = "TYPE_STRING";  break;
      case INT:     p = "TYPE_NUMBER";  break;
      case OBJECT:  p = "TYPE_OBJECT";  break;
      case MAPPING: p = "TYPE_MAPPING"; break;
      case FLOAT:   p = "TYPE_FLOAT";   break;
      case CLOSURE: p = "TYPE_CLOSURE"; break;
      case SYMBOL:  p = "TYPE_SYMBOL";  break;
      case MIXED:   p = "TYPE_ANY";     break;
      case UNKNOWN: p = "TYPE_UNKNOWN"; break;
      case NUL:     p = "0";             break;
      case QUOTED_ARRAY:
        p = "TYPE_QUOTED_ARRAY"; break;
#ifdef USE_STRUCTS
      case STRUCT:  p = "TYPE_STRUCT"; break;
#endif
    default: yyerror("(ctype) Bad type!"); return 0;
    }
    strcat(buff, p);
    if (strlen(buff) + 1 > sizeof buff)
        fatal("Local buffer overwritten in ctype()");
    return buff;
} /* ctype() */

/*-------------------------------------------------------------------------*/
static int
efuncmp (int i, int j)

/* Compare the function entries <i> and <j>.
 * Return 1 if <i> is greater than <j>, 0 if equal, <j> if lesser.
 *
 * This predicate is used to sort the function/code tables read
 * from FUNC_SPEC.
 */

{
    int result;

    result = instr[i].code_class - instr[j].code_class;
    if ( result )
        return result;
    if (C_IS_CODE(instr[i].code_class))
        return 0;
    return strcmp(instr[i].key, instr[j].key);
}

/*-------------------------------------------------------------------------*/
static void
read_config_file (char *fname)

/* Read the config file <fname> to learn about the defines used by the
 * gamedriver.
 */

{
    size_t match_tmp;
    char line_buffer[MAKE_FUNC_MAXLINE + 1];
    char defbuf[MAKE_FUNC_MAXLINE + 1];

    /* --- Read in the file to see what defines are used by the gamedriver ---
     */

    outp = defbuf + sizeof(defbuf) - 1;
    if ((fpr = fopen(fname, "r")) == 0)
    {
       perror(fname);
       fflush(stdout);
       exit(1);
    }
    current_line = 0;
    current_file = fname;

#define MATCH(str) (isspace((unsigned char)line_buffer[1+(match_tmp=strlen(str))]) &&\
                        strncmp(str, line_buffer+1, match_tmp) == 0)

    while (fgets(line_buffer, MAKE_FUNC_MAXLINE, fpr)) {
        char *name;

        current_line++;
        if ( *line_buffer != '#' )
            continue;
        if MATCH("if") {
            handle_if('#', line_buffer+4);
            continue;
        }
        if MATCH("ifdef") {
            handle_cond('#', lookup_define(nextword(line_buffer)) != 0);
            continue;
        }
        if MATCH("ifndef") {
            handle_cond('#', lookup_define(nextword(line_buffer)) == 0);
            continue;
        }
        if MATCH("else") {
            handle_else('#');
            continue;
        }
        if MATCH("endif") {
            handle_endif();
            continue;
        }
        if MATCH("undef") {
            struct defn *old_def;
            old_def = lookup_define(nextword(line_buffer));
            if (old_def)
                old_def->name[0] = '\0';
            continue;
        }
        if ( !MATCH("define") ) {
            continue;
        }
        /* <name> is trusted to be syntactically correct. After all, it was
         * included by the source of this program.
         */
        {
            char *cp, *exps;
            int num_arg;

            cp = line_buffer+8;
            while( isspace((unsigned char)*cp)) cp++;
            name = cp;
            while(isalunum((unsigned char)*cp)) cp++;
            num_arg = *cp == '(' ? 0 : -1;
            if (*cp == '\n') {
                exps = cp;
                *cp = '\0';
            } else {
                *cp++ = '\0';
                while( isspace((unsigned char)*cp)) cp++;
                exps = cp;
                while(*cp != '\n') cp++;
                *cp = '\0';
            }
            add_define(name, num_arg, exps);
        }
    }
    fclose(fpr);

#undef MATCH

} /* read_config_file() */

/*-------------------------------------------------------------------------*/
static void
read_config (void)

/* Read the file CONFIG to learn about the defines used by the gamedriver.
 */

{
    /* --- Read in CONFIG to see what defines are used by the gamedriver ---
     */

    read_config_file(CONFIG);

    /* Sanity check on some of those USE_ defines: undefine
     * those which are not supported on the host system.
     */
    {
        const char * defnames[] = {
#ifndef HAS_IPV6
                             "USE_IPV6",
#endif
#ifndef HAS_MYSQL
                             "USE_MYSQL",
#endif
                             NULL };
        int i;

        for (i = 0; defnames[i] != NULL; i++)
        {
            struct defn *old_def;
            old_def = lookup_define(defnames[i]);
            if (old_def)
            {
                old_def->name[0] = '\0';
            }
        }
    }
} /* read_config() */

/*-------------------------------------------------------------------------*/
static void
read_machine (void)

/* Read the file FILE_MACHINE to learn about the defines used by the machine.
 */

{
    /* Some predefined macros */

#ifdef DEBUG
    add_define("DEBUG", -1, "");
#endif
#ifdef HAVE_GETRUSAGE
    add_define("HAVE_GETRUSAGE",-1,"");
#endif
#ifdef TRACE_CODE
    add_define("TRACE_CODE",-1,"");
#endif

    /* --- Read in FILE_MACHINE to see what defines are used by the gamedriver ---
     */
    read_config_file(FILE_MACHINE);

} /* read_machine() */

/*-------------------------------------------------------------------------*/
static void
read_func_spec (void)

/* Read the file FUNC_SPEC and create the instruction tables.
 */

{
    int i, j;

    for (i = 0; i < C_TOTAL; i++)
        num_instr[i] = 0;

    if ((fpr = fopen(FUNC_SPEC, "r")) == NULL)
    {
        perror(FUNC_SPEC);
        exit(1);
    }
    got_error = 0;
    current_line = 1;
    current_file = FUNC_SPEC;
    parsetype = PARSE_FUNC_SPEC;
    parsetype_sent = MY_FALSE;
    num_buff = 0;
    min_arg = -1;
    unlimit_max = MY_FALSE;
    curr_arg_type_size = 0;
    curr_lpc_type_size = 0;
    curr_lpc_types[0] = 0;
    yyparse();
    fclose(fpr);

    /* Print code usage statistics */

    fprintf(stderr,
"Primary codes:        %3d\n"
"Primary efuns:        %3d\n"
"Tabled efuns:         %3d (%d + %d + %d + %d + %d)\n"
           , num_instr[C_CODE]
           , num_instr[C_EFUN]
           , num_instr[C_EFUN0]+num_instr[C_EFUN1]+num_instr[C_EFUN2]
                               +num_instr[C_EFUN3]+num_instr[C_EFUN4]
           , num_instr[C_EFUN0], num_instr[C_EFUN1], num_instr[C_EFUN2]
           , num_instr[C_EFUN3], num_instr[C_EFUN4]
        );
    fprintf(stderr,
"Tabled vararg efuns:  %3d\n"
           , num_instr[C_EFUNV]
        );
        /* Combining the two print statements into one triggers bugs on
         * AIX 5.2:
         *   Visual Age C 6.0 compiles the for() assignment 'i = C_CODE+1'
         *     such that i is assigned a large number (12298).
         *   gcc 2.9 generates code for the same assignment which causes
         *     the mkfunc program to segfault.
         * The bugs could not be reproduced with a simple test program.
         */

    /* Compute the code offsets.
     * This means that instr_offset[C_ALIAS] is also the
     * the number of real instructions.
     */
    instr_offset[C_CODE] = 0;
    for (i = C_CODE+1; i < C_TOTAL; i++)
    {
        instr_offset[i] = instr_offset[i-1] + num_instr[i-1];
    }

    /* Check if the code ranges has been exhausted
     */
    if ( (num_instr[C_CODE] + num_instr[C_EFUN]) > 255 )
        fatal("Codes exhausted!");

    for (i = C_EFUN+1; i < C_ALIAS; i++)
        if (num_instr[i] > 255)
            fatal("Codes exhausted!\n");

    if (num_instr[C_SEFUN])
    {
        printf("Sefuns: %d\n", num_instr[C_SEFUN]);
        fatal("Unclassified sefuns encountered.\n");
    }

    /* Now sort the table of functions and codes (eeek, bubblesort!)
     * This sort makes sure that functions of the same class
     * are consecutive within the instr[] array, and therefore
     * really are at the places where instr_offset[] says they are.
     * It therefore estables that the index in instr[] is the
     * instruction code.
     */

    for (i = num_buff; --i >= 0; )
    {
        for (j = 0; j < i; j++)
            if (efuncmp(i,j) < 0)
            {
                struct instrdata_s tmp;

                tmp = instr[i]; instr[i] = instr[j]; instr[j] = tmp;
            }
    }

} /* read_func_spec() */

/*-------------------------------------------------------------------------*/
static void
read_string_spec (void)

/* Read the file STRING_SPEC.
 */

{
    if ((fpr = fopen(STRING_SPEC, "r")) == NULL)
    {
        perror(STRING_SPEC);
        exit(1);
    }
    got_error = 0;
    current_line = 1;
    current_file = STRING_SPEC;
    parsetype = PARSE_STRING_SPEC;
    parsetype_sent = MY_FALSE;
    num_buff = 0;
    yyparse();
    fclose(fpr);

} /* read_string_spec() */

/*-------------------------------------------------------------------------*/
static void
create_efun_defs (void)

/* Create the file EFUN_DEFS
 */

{
    int i, j, k;
    unsigned char c;
    char * pattern;

    if ((fpw = fopen(EFUN_DEFS, "w")) == NULL)
    {
       perror(EFUN_DEFS);
       exit(1);
    }

    fprintf(fpw,
"#ifndef EFUN_DEFS_C__\n"
"#define EFUN_DEFS_C__ 1\n"
"\n"
"/* DO NOT EDIT!\n"
" *\n"
" * This file is created automatically by make_func from\n"
" * the specifications in " FUNC_SPEC ".\n"
" * It is meant to be included in lex.c\n"
" */\n"
"\n"
"#include \"exec.h\"  /* struct instr_s == instr_t */\n"
"\n"
           );

    fprintf(fpw,
"/*----------------------------------------------------------------------*/\n"
"\n"
"/* The table of all instructions\n"
" */\n"
"\n"
"instr_t instrs[] = {\n"
           );

    for (k = C_CODE, i = 0; k <= C_ALIAS; k++)
    {
        switch(k)
        {
        case C_CODE:  fprintf(fpw, "\n  /* --- codes --- */\n\n");
                      break;
        case C_EFUN:  fprintf(fpw, "\n  /* --- efuns --- */\n\n");
                      break;
        case C_EFUN0: fprintf(fpw, "\n  /* --- 0-arg efuns --- */\n\n");
                      break;
        case C_EFUN1: fprintf(fpw, "\n  /* --- 1-arg efuns --- */\n\n");
                      break;
        case C_EFUN2: fprintf(fpw, "\n  /* --- 2-arg efuns --- */\n\n");
                      break;
        case C_EFUN3: fprintf(fpw, "\n  /* --- 3-arg efuns --- */\n\n");
                      break;
        case C_EFUN4: fprintf(fpw, "\n  /* --- 4-arg efuns --- */\n\n");
                      break;
        case C_EFUNV: fprintf(fpw, "\n  /* --- vararg efuns --- */\n\n");
                      break;
        case C_ALIAS: fprintf(fpw, "\n  /* --- aliased efuns --- */\n\n");
                      break;
        }

        j = instr_offset[k] + num_instr[k];
        for (; i < j; i++) {
            fprintf(fpw, "  /* %3d */ %s", i, instr[i].buf);
        }
    }
    fprintf(fpw, "};\n\n\n");

    fprintf(fpw, "/* Aliased efuns.\n"
                 " * Index it with <code>-F_LAST_INSTRUCTION-1 to retrieve\n"
                 " * the real instruction code.\n"
                 " */\n\n");
    fprintf(fpw, "short efun_aliases[] = {\n");
    {
        Bool gotone = MY_FALSE;

        for (i = 0; i < num_buff; i++) {
            if (instr[i].code_class == C_ALIAS)
            {
                fprintf(fpw, "    %s,\n", instr[i].f_name);
                gotone = MY_TRUE;
            }
        }
        if (!gotone)
            fprintf(fpw, "    0 /* dummy to keep the compilers happy */\n");
    }
    fprintf(fpw, "};\n\n\n");

    fprintf(fpw,
"/* Table of function argument signatures (compiler).\n"
" * The internal structure is that of arg_types[] in make_func.\n"
" */\n\n"
          );
    fprintf(fpw, "fulltype_t efun_arg_types[] = {\n    /*   0 */ ");
    for (i = 0; i < last_current_type; i++)
    {
        if (arg_types[i] == 0)
        {
#ifdef USE_STRUCTS
            fprintf(fpw, "{ 0, NULL },\n");
#else
            fprintf(fpw, "{ 0 },\n");
#endif /* USE_STRUCTS */
            if (i < last_current_type - 1)
                fprintf(fpw, "    /* %3d */ ", i+1);
        }
        else
#ifdef USE_STRUCTS
            fprintf(fpw, "{ %s, NULL }, ", ctype(arg_types[i]));
#else
            fprintf(fpw, "{ %s }, ", ctype(arg_types[i]));
#endif /* USE_STRUCTS */
    }
    fprintf(fpw, "};\n\n\n");

    fprintf(fpw,
"/* Table of function argument signatures (runtime).\n"
" * The internal structure is that of lpc_types[] in make_func.\n"
" */\n\n"
          );
    fprintf(fpw, "long efun_lpc_types[] = {\n ");
    for (i = 0; i < last_current_lpc_type; i++) {
        fprintf(fpw, "    /* %3d */ %s,\n", i, etype(lpc_types[i]));
    }
    fprintf(fpw, "};\n\n\n");

    pattern = "Variable 'pattern' not initialized.";
    for (k = C_EFUN0; k < C_ALIAS; k++)
    {

        switch(k)
        {
        case C_EFUN0:
            fprintf(fpw, "/* Prototypes of the tabled efuns\n */\n\n");
            pattern = "extern svalue_t *f_%s(svalue_t *);\n";
            break;
        case C_EFUNV:
            fprintf(fpw, "/* Prototypes of the tabled vararg efuns\n */\n\n");
            pattern = "extern svalue_t *v_%s(svalue_t *, int);\n";
            break;
        }

        i = instr_offset[k];
        j = i + num_instr[k];
        for (; i < j; i++)
        {
            fprintf(fpw, pattern, instr[i].key);
        }

        if (num_instr[k])
            fprintf(fpw, "\n\n");
    }

    {
        char * prefix = NULL;
        for (k = C_EFUN0; k < C_ALIAS; k++)
        {

            switch(k)
            {
            case C_EFUN0:
                fprintf(fpw, "/* The table of tabled efuns\n */\n\n");
                fprintf(fpw, "svalue_t *(*efun_table[]) (svalue_t *) = {\n");
                prefix = "f_";
                break;
            case C_EFUNV:
                fprintf(fpw, "/* The table of tabled vararg efuns\n */\n\n");
                fprintf(fpw, "svalue_t *(*vefun_table[]) (svalue_t *, int) = {\n");
                prefix = "v_";
                break;
            }

            i = instr_offset[k];
            j = i + num_instr[k];
            for(; i < j; i++)
            {
                fprintf(fpw, "    /* %3d */ %s%s,\n", i, prefix, instr[i].key);
            }

            if (k == C_EFUNV-1 || k == C_ALIAS-1)
                fprintf(fpw, "};\n\n\n");
        }
    }

    /* TODO: create a my-ctype.[ch] and a utility program to create
     * TODO:: these two files once and for all.
     */
    fprintf(fpw,
"/*----------------------------------------------------------------------*/\n"
"\n"
"/* Our own ctype implementation. This way we can be sure that\n"
" *   (a) we won't choke on non-ASCII characters\n"
" *   (b) we are fast\n"
" *   (c) we get the non-standard classifications we need anyway\n"
" */\n"
"\n"
           );

    fprintf(fpw, "#define lexwhite(c) (_my_ctype[(unsigned char)(c)]&%d)\n",_MCTs);
    fprintf(fpw, "#define leXdigit(c) (_my_ctype[(unsigned char)(c)]&%d)\n",_MCTx);

    fprintf(fpw, "\n" "unsigned char _my_ctype[] = {");
    c = '\0';
    do {
        if (!(c & 0xf))
            fprintf(fpw, "\n    ");
        fprintf(fpw, "%d,"
               ,  ( (isascii(c) && make_func_isescaped(c)) ? _MCTe : 0 )
                | ( (isascii(c) && make_func_issavedel(c)) ? _MCTt : 0 )
                | ( (isascii(c) && isdigit ((unsigned char)c))  ? _MCTd : 0 )
                | ( (isascii(c) && isspace ((unsigned char)c) && c != '\n')
                    ? _MCTs : 0 )
                | ( (isascii(c) && isxdigit((unsigned char)c))  ? _MCTx : 0 )
                | ( ((isascii(c) && (isalnum ((unsigned char)c) || c == '_'))
                   || (((unsigned char)c) >= 0xC0))
                    ? _MCTa : 0 )
               );
        c++;
    } while (c != '\0');
    fprintf(fpw, "\n};\n");


    fprintf(fpw,
"\n"
"/************************************************************************/\n"
"\n"
"#endif /* EFUN_DEFS_C__ */\n"
           );

    fclose(fpw);

} /* create_efun_defs() */

/*-------------------------------------------------------------------------*/
static void
create_instrs (void)

/* Create the file THE_INSTRS
 */

{
    int i, j, k;
    int last_instr;

    if ((fpw = fopen(THE_INSTRS, "w")) == NULL)
    {
       perror(THE_INSTRS);
       exit(1);
    }

    fprintf(fpw,
"#ifndef INSTRS_H__\n"
"#define INSTRS_H__ 1\n"
"\n"
"/* DO NOT EDIT!\n"
" *\n"
" * This file is created automatically by make_func from\n"
" * the specifications in " FUNC_SPEC ".\n"
" *\n"
" * It holds the bytecode values for all machine instructions, plus\n"
" * declarations of the tables holding information about the instructions.\n"
" */\n"
"\n"
"#include \"exec.h\"  /* struct instr_s == instr_t */\n"
"\n"
           );

    last_instr = instr_offset[C_ALIAS] - 1;

    for (k = C_CODE; k < C_ALIAS; k++)
    {
        char * str;

        switch (k)
        {
        case C_CODE:  str = "internal"; break;
        case C_EFUN:  str = "efun"; break;
        case C_EFUN0: str = "efun0"; break;
        case C_EFUN1: str = "efun1"; break;
        case C_EFUN2: str = "efun2"; break;
        case C_EFUN3: str = "efun3"; break;
        case C_EFUN4: str = "efun4"; break;
        case C_EFUNV: str = "efunv"; break;
        default:
          {
            char buf[100];
            sprintf(buf, "create_instrs(): Invalid value %ld for loop index.\n", (long)k);
            fatal(buf);
            str = NULL; /* To keep the compiler happy */
            break;
          }
        }

        fprintf(fpw,
"#define %s_OFFSET  (%d)\n"
"#define %s_COUNT   (%d)\n"
"  /* First offset and number of %s opcodes.\n"
"   */\n\n"
                , classtag[k], instr_offset[k]
                , classtag[k], num_instr[k]
                , str
                );
    }

    fprintf(fpw,
"#define LAST_INSTRUCTION_CODE (%d)\n"
"  /* The highest token value in use.\n"
"   */\n\n"
           , last_instr
    );

    fprintf(fpw,
"#define TEFUN_OFFSET EFUN0_OFFSET\n"
"  /* Offset of the first tabled efun.\n"
"   */\n\n"
    );

    fprintf(fpw, "extern instr_t instrs[];\n"
                 "extern short efun_aliases[];\n"
                 "extern fulltype_t efun_arg_types[];\n"
                 "extern long efun_lpc_types[];\n"
                 "extern svalue_t *(*efun_table[])(svalue_t *);\n"
                 "extern svalue_t *(*vefun_table[])(svalue_t *, int);\n"
    );

    fprintf(fpw,
"  /* All tables are defined in " EFUN_DEFS " and compiled into lex.c\n"
"   * TODO: We might as well create efun_defs.h and compile it separately.\n"
"   */\n"
           );

    for (k = C_CODE; k < C_ALIAS; k++)
    {
        if (!num_instr[k])
            continue;

        switch(k)
        {
        case C_CODE:  fprintf(fpw,"\n/* --- codes --- */\n\n");
                      break;
        case C_EFUN:  fprintf(fpw,"\n/* --- efuns --- */\n\n");
                      break;
        case C_EFUN0: fprintf(fpw,"\n/* --- efun0s --- */\n\n");
                      break;
        case C_EFUN1: fprintf(fpw,"\n/* --- efun1s --- */\n\n");
                      break;
        case C_EFUN2: fprintf(fpw,"\n/* --- efun2s --- */\n\n");
                      break;
        case C_EFUN3: fprintf(fpw,"\n/* --- efun3s --- */\n\n");
                      break;
        case C_EFUN4: fprintf(fpw,"\n/* --- efun4s --- */\n\n");
                      break;
        case C_EFUNV: fprintf(fpw,"\n/* --- vefuns --- */\n\n");
                      break;
        }

        i = instr_offset[k];
        j = i + num_instr[k];
        for ( ; i < j; i++)
        {
           fprintf(fpw, "#define %-30s (%d)\n"
                       , make_f_name(instr[i].key), i);
        }
    }

    fprintf(fpw,
"\n"
"/************************************************************************/\n"
"\n"
"#endif /* INSTRS_H__ */\n"
           );

    fclose(fpw);
} /* create_instrs() */

/*-------------------------------------------------------------------------*/
static void
create_lang (void)

/* Create the file THE_LANG from PRO_LANG,
 */

{
    size_t match_tmp;
    char line_buffer[MAKE_FUNC_MAXLINE + 1];
    Bool bPrintedNotice;

#define MATCH(str) (isspace((unsigned char)line_buffer[1+(match_tmp=strlen(str))]) &&\
                        strncmp(str, line_buffer+1, match_tmp) == 0)

    if ((fpr = fopen(PRO_LANG, "r")) == NULL)
    {
       perror(PRO_LANG);
       exit(1);
    }

    if ((fpw = fopen(THE_LANG, "w")) == NULL)
    {
       perror(THE_LANG);
       exit(1);
    }
    current_line = 0;
    bPrintedNotice = MY_FALSE;
    while (fgets(line_buffer, MAKE_FUNC_MAXLINE, fpr))
    {
        current_line++;
        if (*line_buffer == '%') {
            last_line = current_line;
            if MATCH("if") {
                handle_if('%', line_buffer+4);
                continue;
            }
            if MATCH("ifdef") {
                handle_cond('%', lookup_define(nextword(line_buffer)) != 0);
                continue;
            }
            if MATCH("ifndef") {
                handle_cond('%', lookup_define(nextword(line_buffer)) == 0);
                continue;
            }
            if MATCH("else") {
                handle_else('%');
                compensate_lines();
                continue;
            }
            if MATCH("endif") {
                handle_endif();
                compensate_lines();
                continue;
            }
            if MATCH("line") {
                fprintf(fpw, "#line %d \"%s\"\n", current_line+1, PRO_LANG);
                continue;
            }
            if MATCH("typemap") {
                handle_map(line_buffer+9, TYPEMAP_SIZE, name_to_type);
                continue;
            }
            if MATCH("hookmap") {
                handle_map(line_buffer+9, NUM_DRIVER_HOOKS, name_to_hook);
                continue;
            }
            if MATCH("//") {
                /* c++ - resembling comment */
                fputs("\n", fpw);
                continue;
            }
            if (!bPrintedNotice) {
                if MATCH("{") {
                    bPrintedNotice = MY_TRUE;
                    fputs(
"%{\n"
"\n"
"/* DO NOT EDIT!\n"
" *\n"
" * This file is created automatically by make_func from\n"
" * the template " PRO_LANG " according to the specs in " FUNC_SPEC ".\n"
" */\n"
"\n"
                         , fpw);
                    fprintf(fpw, "#line %d \"%s\"\n", current_line+1, PRO_LANG);
                    continue;
                }
            }
        }
        fputs(line_buffer, fpw);
    }
    fclose(fpr), fclose(fpw);

#undef MATCH

} /* create_lang() */

/*-------------------------------------------------------------------------*/
static void
create_stdstrings (void)

/* Create the files STDSTRINGS.[ch].
 */

{
    int i;

    /* Create stdstrings.h */

    if ((fpw = fopen(STDSTRINGS ".h", "w")) == NULL)
    {
       perror(STDSTRINGS ".h");
       exit(1);
    }

    fputs(
"#ifndef STDSTRINGS_H__\n"
"#define STDSTRINGS_H__ 1\n"
"\n"
"/* DO NOT EDIT!\n"
" *\n"
" * This file is created automatically by make_func from\n"
" * the specifications in " STRING_SPEC ".\n"
" */\n"
"\n"
"#include \"driver.h\"\n"
"#include \"typedefs.h\"\n"
"\n"
"/* --- Common used shared strings. --- */\n"
"/* The most common strings, including all the predefined applies,\n"
" * are kept in shstrings[] for faster usage.\n"
" * The indices are SHX_xxx, the defines STR_xxx expand to shstring[SHX_xxx]\n"
" */\n"
"\n"
"enum StandardStrings {\n"
         , fpw);

    for (i = 0; i < num_buff; i++)
    {
        if (!i)
            fprintf(fpw, "    SHX_%-12s = 0 /* \"%s\" */\n"
                       , instr[i].key, instr[i].buf);
        else
            fprintf(fpw, "  , SHX_%-16s /* \"%s\" */\n"
                       , instr[i].key, instr[i].buf);
    }

    fputs(
"\n"
"  , SHSTR_NOSTRINGS /* The number of strings */\n"
"};\n"
"\n"
"extern string_t *shstring[SHSTR_NOSTRINGS];\n"
"\n"
         , fpw);

     for (i = 0; i < num_buff; i++)
     {
          fprintf(fpw, "#define STR_%-16s  shstring[SHX_%s]\n"
                     , instr[i].key, instr[i].key);
     }

    fputs(
"\n"
"/* --- Prototypes --- */\n"
"extern void init_standard_strings(void);\n"
"\n"
"#endif /* STDSTRINGS_H__ */\n"
         , fpw);

    fclose(fpw);

    /* Create stdstrings.c */

    if ((fpw = fopen(STDSTRINGS ".c", "w")) == NULL)
    {
       perror(STDSTRINGS ".c");
       exit(1);
    }

    fputs(
"/* DO NOT EDIT!\n"
" *\n"
" * This file is created automatically by make_func from\n"
" * the specifications in " STRING_SPEC ".\n"
" *\n"
" * It's purpose is to define and initialize the standard shared string\n"
" * table shstring[].\n"
" */\n"
"\n"
"#include \"driver.h\"\n"
"#include \"typedefs.h\"\n"
"\n"
"#include \"" STDSTRINGS ".h\"\n"
"#include \"mstrings.h\"\n"
"\n"
"/*-------------------------------------------------------------------------*/\n"
"\n"
"string_t *shstring[SHSTR_NOSTRINGS];\n"
"  /* Table of common used and therefore shared strings.\n"
"   */\n"
"\n"
"/*-------------------------------------------------------------------------*/\n"
"void\n"
"init_standard_strings (void)\n"
"\n"
"/* Initialize the common string table.\n"
" */\n"
"\n"
"{\n"
"#   define INIT(x,s) shstring[x] = mstring_new_tabled(s MTRACE_ARG);\n"
"\n"
         , fpw);

    for (i = 0; i < num_buff; i++)
        fprintf(fpw, "    INIT(SHX_%s, \"%s\");\n"
                   , instr[i].key, instr[i].buf);

    fputs(
"\n"
"#   undef INIT\n"
"} /* init_standard_strings() */\n"
"\n"
"/************************************************************************/\n"
         , fpw);

    fclose(fpw);

} /* create_stdstrings() */

/*-------------------------------------------------------------------------*/
int
main (int argc, char ** argv)

/* The main program.
 */

{
    enum { NONE = 0, MakeInstrs, MakeLang, MakeStrings } action = NONE;

    /* --- Check what we have to do --- */
    if (argc == 2)
    {
        if (!strcasecmp(argv[1], "instrs")) action = MakeInstrs;
        else if (!strcasecmp(argv[1], "lang")) action = MakeLang;
        else if (!strcasecmp(argv[1], "strings")) action = MakeStrings;
    }

    if (action == NONE)
    {
        fputs(
"Usage: make_func instrs|lang|strings\n"
"\n"
"  make_func instrs\n"
"    creates " EFUN_DEFS " and " THE_INSTRS ", reads " CONFIG " and " FUNC_SPEC ".\n"
"\n"
"  make_func lang\n"
"    creates " THE_LANG " from " PRO_LANG ", reads " CONFIG ".\n"
"\n"
"  make_func strings\n"
"    creates " STDSTRINGS ".[ch], reads " CONFIG " and " STRING_SPEC ".\n"
             , stderr);
        return 1; /* TODO: There are constants for this */
    }

    /* --- Read the config files --- */
    read_machine();
    read_config();
    if (action == MakeInstrs)
        read_func_spec();

    if (action == MakeStrings)
        read_string_spec();

    if (got_error)
        return 1;

    /* --- Create the output files --- */
    if (action == MakeInstrs)
    {
        create_efun_defs();
        create_instrs();
    }
    if (action == MakeLang)
        create_lang();

    if (action == MakeStrings)
        create_stdstrings();

    /* --- That's it --- */

    return 0;

} /* main() */

#if defined(__MWERKS__) && !defined(WARN_ALL)
#    pragma warn_possunwant off
#    pragma warn_implicitconv off
#endif

/***************************************************************************/

