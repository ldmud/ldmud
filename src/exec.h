#ifndef EXEC_H__
#define EXEC_H__ 1

/*---------------------------------------------------------------------------
 * Types and Macros used to describe and store compiled programs.
 *
 *---------------------------------------------------------------------------
 * Basics
 * ------
 *
 * LPMud distinguished between objects and their programs. Objects hold
 * the live data which is unique to each of them. Programs otoh are shared
 * among objects through inheritance or cloning and hold the program code,
 * variable specification and similar information. Due to the separation
 * programs maintain their own reference count, and it is this separation
 * which allows the separate swapping of programs and variables.
 *
 * A compiled program consists of several data blocks which are allocated
 * together in one memory block. Not only this is advantageous for the
 * memory locality, but also simplifies swapping as all data for a program
 * can be read or written in one go, with a simple relocation computation
 * on the pointers involved.
 *
 * The blocks of a program in correct allocation order are these:
 *
 *   struct program_s: The basic structure of the program, with pointers to
 *       the other data blocks.
 *
 *   bytecode program[]: the actual bytecode for the program.
 *
 *   unsigned short function_names[]: Lookup table of function name index
 *       to the offset of the function within the functions[] table.
 *       function_names[] is allocated together with and right after the
 *       bytecode. If a function redefines an inherited function, this table
 *       points to the redefinition.
 *       Read 'function_name' as 'function_index'.
 *
 *   uint32 functions[]: One word for each function, giving the type
 *       and the offset within the program[] codearray. Additional information
 *       for each function is embedded into the program code.
 *       If a program uses undefined functions, the compiler generates
 *       dummy functions.
 *       Through inheritance and cross-definition these functions can
 *       be resolved on a higher level.
 *
 *   string_t *strings[]: An array of pointers to the string literals (stored
 *       as shared strings) used by the program. This way the program can
 *       use the strings simply by an (easily swappable) index. The compiler
 *       makes sure that each string is unique.
 *
 *       When a program is swapped, the reference counts to these strings are
 *       not removed so that the string literals stay in memory all the time.
 *       This saves time on swap-in, and the string sharing saves more memory
 *       than swapping might gain.
 * TODO: Does it?
 *
 *   struct variable_s variables[]: an array describing all variables
 *       with type and name, inherited or own. When a program is swapped, the
 *       reference counts to these strings are not removed so that the
 *       string literals stay in memory all the time.
 *
 *   struct inherit inherit[]: an array describing all inherited programs.
 *
 *   vartype_t argument_types[]: (only with #pragma save_types)
 *       The types of all function arguments of the program in the
 *       order they were encountered.
 *
 *   unsigned short type_start[]: (only with #pragma save_types)
 *       Lookup table [.num_function_names] function index
 *       -> index in .argument_types[], which gives the index of
 *       the first argument type. If this index is INDEX_START_NONE,
 *       the function has no type information.
 *
 *   include_t includes[]: an array listing all include files used
 *       to compile the program, in the order they were encountered.
 *       When a program is swapped, the reference counts to these strings
 *       are not removed so that the string literals stay in memory all
 *       the time.
 *
 * The linenumber information is allocated separately from the main program
 * block so that it can be swapped separately more easily. The struct
 * linenumbers_s is allocated to size and holds the line number information
 * as an array of bytecodes:
 *
 *   bytecode_t line_numbers[]: the line number information,
 *       encoded in a kind of delta compression. When a program
 *       is swapped in, the line numbers are allocated separately
 *       and not swapped in until needed.
 *
 * TODO: If the program_s is allocated separately from the rest,
 * TODO:: we could swap even if a program is used by clones.
 *
 *
 * Bytecode
 * --------
 *
 * The bytecode is described and defined in bytecode.h .
 *
 *
 * Inheritance
 * -----------
 *
 * Inheritance is handled such that all the variable and function
 * information of the inherited programs are appended to the programs
 * own variables and functions. The inherit entries then give away
 * where in the programs tables the inherited information can be found.
 *
 * Imagine: A and B inherit nothing, C inherits A, D inherits C and B.
 * Then the function and variable blocks are set up like this ('-funs' are
 * real function entries, '-desc' are pointers to inherit descriptors):
 *
 *     A-fblock: A-funs  (B similar)
 *     A-vblock: A-vars  (B similar)
 *
 *     C-fblock: C-funs A-desc
 *     C-vblock: C-vars A-vars
 *
 *     D-fblock: D-funs (C-desc A-desc) B-desc
 *     D-vblock: D-vars  C-vars A-vars  B-vars
 *       and fblock has the twist that (C-desc A-desc) together are
 *       considered being 'the' C-desc block.
 *
 * If a program is inherited virtually several times, each virtual inherit
 * gets its own struct inherit; the variable block however is reserved
 * just once. To mark the virtual inherit, the variable types receive the
 * modifier TYPE_MOD_VIRTUAL. During the inheritance of the compiler, functions
 * from non-virtual inherits temporarily receive the flag
 * NON_VIRTUAL_OFFSET_TAG in their variable indices, too.
 *
 * However, accesses to virtually inherited variables only use the first
 * inherit instance - if the variable index in the child's program code indexes
 * the variable block associated with a later instance, the driver finds
 * the first instance by comparing the .prog entries and uses the 'real'
 * variable associated with this first instance.
 *
 * The compiler places virtual variables always at the begin of the variable
 * block and limits the number to 256.
 *
 * TODO: Find a way to avoid the virtual var searching - either by encoding the
 * TODO:: inherit index in the code, or by adding a ref to the 'first
 * TODO:: instance' to this structure. See interpret.c:find_virtual_value().
 *
 * Old Comments:
 * -------------
 * When an object is compiled with type testing (#pragma strict_types), all
 * types are saved of the arguments for that function during compilation.
 * If the #pragma save_types is specified, then the types are saved even
 * after compilation, to be used when the object is inherited.
 *---------------------------------------------------------------------------
 */

#include "driver.h"
#include "typedefs.h"

#include "bytecode.h"


typedef unsigned short    typeid_t;
typedef uint32            typeflags_t;
typedef struct vartype_s  vartype_t;
typedef struct fulltype_s fulltype_t;


/* --- Type Constants ---
 *
 * These constants and types are used to encode types and visibility
 * of variables and functions. They are used by both the compiler and
 * the interpreter.
 *
 * Variable/value data types consist of three pieces: 
 *  - the primary type constant, optionally modified as
 *      TYPE_MOD_POINTER or TYPE_MOD_REFERENCE;
 *  - the visibility (see below with the function flags for the constants)
 *  - the type object pointer (depending on context either counted or not
 *      counted).
 *
 * The pieces show up in these types:
 *  - vartype:  datatype + type object pointer
 *  - fulltype: vartype + visibility flags
 *
 * TODO: A clean implementation would use just type objects for types.
 */

/* --- struct vartype_s: Basic type information
 *
 * This structure holds the type number, the flags concerning _MOD_POINTER
 * and _MOD_REFERENCE, and the virtual variable flag (for use in variable_t).
 * It also holds a pointer to the type object; depending on the context
 * this reference may be counted or not.
 */

struct vartype_s {
    typeid_t        type;
    struct_type_t * t_struct;
      /* For now, only structs have type objects */
};

/* --- struct fulltype_s: Full type information
 *
 * This structure holds the type number and all flags: type modifiers and
 * visibility.
 * It also holds a pointer to the type object; depending on the context
 * this reference may be counted or not.
 *
 * We do not reuse vartype_t her even though it logically should be, for
 * two reasons:
 *  - the visibility modifier flags expected that type and flags are
 *    in one single long
 *  - some compilers would add 2 bytes of padding
 *
 * TODO: Move the basic visibility  into the lower bits so we don't have
 * TODO:: to use a short for .typeflags.
 */
struct fulltype_s {
    typeflags_t     typeflags;
    struct_type_t * t_struct;
      /* For now, only structs have type objects */
};

/* --- Primary type values --- */
enum primary_types {
    TYPE_UNKNOWN      =  0,   /* This type must be casted */
    TYPE_NUMBER       =  1,
    TYPE_STRING       =  2,
    TYPE_VOID         =  3,
    TYPE_OBJECT       =  4,
    TYPE_MAPPING      =  5,
    TYPE_FLOAT        =  6,
    TYPE_ANY          =  7,  /* Will match any type */
    TYPE_CLOSURE      =  8,
    TYPE_SYMBOL       =  9,
    TYPE_QUOTED_ARRAY = 10,
    TYPE_STRUCT       = 11,   /* Secondary info is the struct id */

    TYPEMAP_SIZE      = 12,   /* Number of types */
};

/* Flags, or'ed on top of the basic type */
enum type_flags {
    TYPE_MOD_POINTER   = 0x0040,       /* Pointer to a basic type */
    TYPE_MOD_REFERENCE = 0x0080,       /* Reference to a type */
    TYPE_MOD_MASK      = 0x000000ff,   /* Mask for basic type and flags. */
  
    /* Mask for the primary type info (sans modifiers) */
    PRIMARY_TYPE_MASK  = 0x0F,

    /* Mask to delete TYPE_MOD_REFERENCE and the visibility mods from
     * a type value. */
    TYPE_MOD_RMASK     = (TYPE_MOD_MASK & ~TYPE_MOD_REFERENCE),

     /* Mask to mask out just the typeid_t from a typeflags_t.
      */
    TYPEID_MASK        = 0x0000ffff,

     /* Flag set in virtual variables, also interpreted as offset
      * in the variable index for virtual variables. */
    VIRTUAL_VAR_TAG    = 0x4000,
};


/* Macros to check a type value for a certain primary type.
 */
static INLINE Bool IS_TYPE_STRUCT(fulltype_t t) __attribute__((pure));
static INLINE Bool IS_TYPE_STRUCT(fulltype_t t) {
    return (t.typeflags & (PRIMARY_TYPE_MASK|TYPE_MOD_POINTER|TYPE_MOD_REFERENCE)) 
             == TYPE_STRUCT;
}
static INLINE Bool IS_TYPE_ANY(fulltype_t t) __attribute__((pure));
static INLINE Bool IS_TYPE_ANY(fulltype_t t) {
    return (t.typeflags & (PRIMARY_TYPE_MASK|TYPE_MOD_POINTER|TYPE_MOD_REFERENCE)) 
             == TYPE_ANY;
}

/* Other type related defines */
enum {
     STRUCT_MAX_MEMBERS = 255,
  /* We allow up to this number of members per struct, so that
   * we can encode the number of actual members, where needed,
   * in a single bytecode.
   */
};


/* void ref_vartype_data(vartype_t *v)
 *   Add another reference to the data associated with vartype <v>.
 * 
 * void ref_fulltype_data(fulltype_t *v)
 *   Add another reference to the data associated with fulltype <v>.
 */

#define ref_vartype_data(v) \
    do{ vartype_t *fvt = v; \
        if (fvt->t_struct) (void)ref_struct_type(fvt->t_struct);\
    } while(0)

#define ref_fulltype_data(v) \
    do{ fulltype_t *fvt = v; \
        if (fvt->t_struct) (void)ref_struct_type(fvt->t_struct);\
    } while(0)


/* void free_vartype_data(vartype_t *v)
 *   Free all data associated with vartype <v>.
 * 
 * void free_fulltype_data(fulltype_t *v)
 *   Free all data associated with fulltype <v>.
 */

#define free_vartype_data(v) \
    do{ vartype_t *fvt = v; \
        if (fvt->t_struct) { /* printf("DEBUG: free_vartype(%s) %s %d\n", get_txt(fvt->t_struct->name), __FILE__, __LINE__); */ free_struct_type(fvt->t_struct); }\
        fvt->t_struct = NULL;\
    } while(0)

#define free_fulltype_data(v) \
    do{ fulltype_t *fvt = v; \
        if (fvt->t_struct) { /* printf("DEBUG: free_fulltype(%s) %s %d\n", get_txt(fvt->t_struct->name), __FILE__, __LINE__); */ free_struct_type(fvt->t_struct); }\
        fvt->t_struct = NULL;\
    } while(0)


#ifdef GC_SUPPORT

/* void clear_vartype_ref(vartype_t *v)
 *   Clear all references associated with vartype <v>.
 * 
 * void clear_fulltype_ref(fulltype_t *v)
 *   Clear all references associated with fulltype <v>.
 */

#define clear_vartype_ref(v) \
    do{ vartype_t *fvt = v; \
        if (fvt->t_struct) clear_struct_type_ref(fvt->t_struct);\
    } while(0)

#define clear_fulltype_ref(v) \
    do{ fulltype_t *fvt = v; \
        if (fvt->t_struct) clear_struct_type_ref(fvt->t_struct);\
    } while(0)


/* void count_vartype_ref(vartype_t *v)
 *   Count all references associated with vartype <v>.
 * 
 * void count_fulltype_ref(fulltype_t *v)
 *   Count all references associated with fulltype <v>.
 */

#define count_vartype_ref(v) \
    do{ vartype_t *fvt = v; \
        if (fvt->t_struct) count_struct_type_ref(fvt->t_struct);\
    } while(0)

#define count_fulltype_ref(v) \
    do{ fulltype_t *fvt = v; \
        if (fvt->t_struct) count_struct_type_ref(fvt->t_struct);\
    } while(0)

#endif /* GC_SUPPORT */


/* --- struct instr_s: description of stackmachine instructions ---
 *
 * Stackmachine instructions are both 'internal' codes with no external
 * representation, as well as efuns.
 *
 * The information about all instructions is collected in the table
 * instrs[] which is indexed by the code of the instructions.
 * The .prefix and .opcode fields give the proper stackmachine opcodes
 * for every instruction.
 *
 * The table is declared in instrs.h and defined in the file efun_defs.c
 * both of which are created by make_func from the func_spec file.
 * The table is compiled into the lexer module and exported from there.
 */

struct instr_s
{
    bytecode_t prefix;  /* 0 or prefix code if required */
    bytecode_t opcode;  /* The instructions (sub-)opcode */
    short max_arg;    /* Maximum number of arguments, -1 for '...' */
    short min_arg;
      /* Minimum number of arguments.
       * The number 0 marks incallable closures: instructions which
       * are used as syntactic markers in lambda closures, but by
       * themselves can't be executed.
       */
    short Default;
      /* An efun to use as default value for last argument.
       * > 0: index into instrs[] to the efun to use.
       *   0: no default value.
       *  -1: this whole entry describes an internal stackmachine code,
       *      not a normal efun (an 'operator' in closure lingo).
       */
    fulltype_t ret_type;  /* The return type used by the compiler. */
    short arg_index;     /* Indexes the efun_arg_types[] arrays. */
    short lpc_arg_index; /* Indexes the efun_lpc_types[] arrays. */
                         /* A '-1' index means that no type information
                          * is available.
                          */
    char *name;          /* The printable name of the instruction. */
    char *deprecated;    /* Usually NULL, for deprecated efuns this is
                          * the warning message to print.
                          */
};


/* --- Function flags ---
 *
 * Programs know about their functions from an array of uint32s which hold
 * the flags for the function and, in one field in the low bits, the
 * address of the code.
 *
 * Some flags (TYPE_MOD_*) are also used for variable types.
 *
 * Using a bitfield is tempting, but generates inefficient code due to the
 * lack of a real 'bool' datatype.
 */

typedef uint32 funflag_t;  /* Function flags */
typedef int32 sfunflag_t;  /* signed version of Function flags */

enum function_flags {
    NAME_INHERITED     = 0x80000000,  /* defined by inheritance */
    TYPE_MOD_STATIC    = 0x40000000,  /* Static function or variable    */
    TYPE_MOD_NO_MASK   = 0x20000000,  /* The nomask => not redefineable */
    TYPE_MOD_PRIVATE   = 0x10000000,  /* Can't be inherited             */
    TYPE_MOD_PUBLIC    = 0x08000000,  /* Force inherit through private  */
    TYPE_MOD_VARARGS   = 0x04000000,  /* Used for type checking         */
    VAR_INITIALIZED    = 0x04000000,  /* Variable is not shared         */
    TYPE_MOD_VIRTUAL   = 0x02000000,  /* can be re- and cross- defined  */
    TYPE_MOD_PROTECTED = 0x01000000,  /* cannot be called externally    */
    TYPE_MOD_XVARARGS  = 0x00800000,  /* accepts optional arguments     */
    TYPE_MOD_NOSAVE    = 0x00400000,  /* vars: can't be saved           */
    NAME_CROSS_DEFINED = 0x00200000,
    TYPE_MOD_DEPRECATED  = 0x00100000, /* lfun is marked deprecated     */
  /* Two functions with the same name inherited from A and B into C.
   * The compiler usually prefers A, and the value 'flags & INHERIT_MASK'
   * (in bias-0x20000 representation) stored as B.offset.func is the
   * difference between to the real functions index (also see below).
   * A special use is A uses a function from B. The function is marked
   * in A as undefined, but the compiler will use cross-defining in C
   * to resolve the function calls in A to call the function in B.
   */

    FUNSTART_MASK      = 0x000fffff,
  /* Function not inherited: unsigned address of the function code relative
   * to the begin of the program block (struct program_s->program).
   */

    INHERIT_MASK        = 0x0003ffff,
  /* Function inherited: unsigned index of the parent program descriptor
   * in the struct program_s->inherit[] array. In the parent program, this
   * function may again be inherited.
   * Function crossdefined: signed difference (in bias-0x20000 notation)
   * from the current function index to the real function index
   * (real = this + offset).
   */

    NAME_HIDDEN        = 0x00020000, /* Not visible for inheritance    */
    NAME_PROTOTYPE     = 0x00010000, /* Defined by a prototype only    */
    NAME_UNDEFINED     = 0x00008000, /* Not defined yet                */
    NAME_TYPES_LOST    = 0x00004000, /* inherited, no save_types       */
};


/* --- Function header ---
 *
 * The bytecode for every function is preceeded by a header with the name
 * and information about arguments and types:
 *
 * struct fun_hdr {
 *     shared string_t * name_of_function;   (usually 4 Bytes)
 *     unsigned short    function_index in defining program (usually 2 Bytes)
 *     vartype_t         return_type;        (usually 6 Byte)
 *         References from the return_type are not counted!
 * --> byte              number_formal_args; (1 Byte)
 *         Bit 7: set if the function has a 'varargs' argument
 * TODO: some code makes use of the fact that this makes the number negative
 *         Bit 6..0: the number of arguments
 *     byte              number_local_vars;  (1 Byte)
 *         This includes the svalues needed for the break stack for
 *         switch() statements.
 *     bytecode_t        opcode[...]
 * }
 *
 * The function address given in the program's function block points to
 * .number_formal_args.
 *
 * Since structs introduce uncontrollable padding, access of all fields
 * is implemented using macros taking the 'function address', typedef'd
 * as fun_hdr_p, as argument and evaluate to the desired value.
 *
 * The whole header structure is aligned properly so that the name_of_function
 * pointer and the return_type short can be used directly.
 *
 * Note: Changes here can affect the struct lambda layout and associated
 *       constants.
 * TODO: the other fields should have proper types, too.
 * TODO: the whole information should be in a table, and not in the
 * TODO:: bytecode. See struct program_s.
 */

typedef bytecode_p fun_hdr_p;
  /* TODO: Make this a void* for now? */

/* TODO: Ugh. I am not convinced that this is a good idea, although I don't
 * TODO::have a better one right now. */
static const bytecode_p SIMUL_EFUN_FUNSTART = (bytecode_p)-1;
  /* Special value used for inter_pc and funstart to mark simul_efuns
   * for dump_trace().
   * TODO: Invent something similar for efun/operator closures?
   */

static const bytecode_p EFUN_FUNSTART = (bytecode_p)-2;
  /* Special value used for funstart to mark efuns for dump_trace.
   */


static INLINE void * FUNCTION_NAMEP(const fun_hdr_p const p) 
                          __attribute__((nonnull(1))) __attribute__((const));
static INLINE void * FUNCTION_NAMEP(const fun_hdr_p const p)
{
    return (char *)p - sizeof(vartype_t) - sizeof(unsigned short) - sizeof(string_t *);
}
static INLINE unsigned short *FUNCTION_INDEXP(const fun_hdr_p const p) 
                          __attribute__((nonnull(1))) __attribute__((const));
static INLINE unsigned short *FUNCTION_INDEXP(const fun_hdr_p const p)
{
    return ((unsigned short*)((char *)p - sizeof(vartype_t) - sizeof(unsigned short)));
}
static INLINE unsigned short FUNCTION_INDEX(const fun_hdr_p const p) 
                          __attribute__((nonnull(1))) __attribute__((const));
static INLINE unsigned short FUNCTION_INDEX(const fun_hdr_p const p)
{
    return *FUNCTION_INDEXP(p);
}
static INLINE void * FUNCTION_TYPEP(const fun_hdr_p const p) 
                          __attribute__((nonnull(1))) __attribute__((const));
static INLINE void * FUNCTION_TYPEP(const fun_hdr_p const p)
{
    return (char *)p - sizeof(vartype_t);
}
static INLINE int FUNCTION_NUM_ARGS(const fun_hdr_p const p) 
                          __attribute__((nonnull(1))) __attribute__((pure));
static INLINE int FUNCTION_NUM_ARGS(const fun_hdr_p const p)
{
    return EXTRACT_SCHAR((char *)p);
}
static INLINE int FUNCTION_NUM_VARS(const fun_hdr_p const p)
                          __attribute__((nonnull(1))) __attribute__((pure));
static INLINE int FUNCTION_NUM_VARS(const fun_hdr_p const p)
{
    return EXTRACT_UCHAR((char *)p + sizeof(char));
}
static INLINE bytecode_p FUNCTION_CODE(const fun_hdr_p const p)
                          __attribute__((nonnull(1))) __attribute__((const));
static INLINE bytecode_p FUNCTION_CODE(const fun_hdr_p const p)
{
    return (unsigned char *)p + 2 * sizeof(char);
}
static INLINE bytecode_p FUNCTION_FROM_CODE(const fun_hdr_p const p)
                          __attribute__((nonnull(1))) __attribute__((const));
static INLINE bytecode_p FUNCTION_FROM_CODE(const fun_hdr_p const p)
{
    return (unsigned char *)p - 2 * sizeof(char);
}

enum function_header_sizes {
    /* Number of function header bytes before the function pointer. */
    FUNCTION_PRE_HDR_SIZE = sizeof(string_t*) + sizeof(unsigned short) + sizeof(vartype_t),
    /* Number of function header bytes after the function pointer. */
    FUNCTION_POST_HDR_SIZE = 2 * sizeof(char),
    /* Total size of the function header. */
    FUNCTION_HDR_SIZE = FUNCTION_PRE_HDR_SIZE + FUNCTION_POST_HDR_SIZE,
};


/* --- struct variable_s: description of one variable
 *
 * This structure describes one variable, inherited or own.
 * The type part of the .flags is used just by the compiler.
 */

struct variable_s
{
    string_t   *name;   /* Name of the variable (shared string) */
    fulltype_t  type;
      /* Type and visibility of the variable (type object counted).
       * If a variable is inherited virtually, the function flag
       * TYPE_MOD_VIRTUAL is or'ed .type.typeflags.
       */
};


/* --- struct inherit: description of one inherited program
 *
 * The information about inherited programs ("objects") for a given
 * program is stored in an array of these structure, and the inherited
 * programs are accessed from the childs' program code by indexing this array.
 */

struct inherit_s
{
    program_t *prog;  /* Pointer to the inherited program */
    unsigned short function_index_offset;
      /* Offset of the inherited program's function block within the
       * inheriting program's function block.
       */
    unsigned short variable_index_offset;
      /* Offset of the inherited program's variables block within the
       * inheriting program's variable block.
       * The NON_VIRTUAL_OFFSET_TAG marks the variables of non-virtual
       * inherits temporarily during compiles.
       */
    unsigned short inherit_type;            /* Type of inherit */

    unsigned short inherit_depth;           /* Depth of inherit */
};
/* Constants for inherit_type in inherit_s */
enum inherit_types {
    INHERIT_TYPE_NORMAL    = 0x0000,  /* Type: Normal inherit */
    INHERIT_TYPE_VIRTUAL   = 0x0001,  /* Type: Virtual inherit */
    INHERIT_TYPE_EXTRA     = 0x0002,  /* Type: Extra inherit added by
                                              * copy_variables()
                                              */
    INHERIT_TYPE_DUPLICATE = 0x0004,  /* Flag: Duplicate virt inherit */
};

/* --- struct struct_def: description of one struct
 *
 * The information about structs visible in the program are stored
 * in an array of these structures; this includes all inherited structs.
 */

struct struct_def_s
{
    funflag_t       flags;  /* Visibility */
    struct_type_t * type;   /* The struct definition itself (counted). */
    int             inh;    /* -1, or the index of the inherit where this
                             * struct came from.
                             */
};


/* --- struct include_s: description of one include file
 *
 * This structure describes one include file used to compile the
 * program.
 */

struct include_s
{
    string_t   *name;
      /* Name as it was found in the program (tabled string). First and last
       * character are the delimiters - either "" or <>.
       */

    string_t   *filename;
      /* Actual filename of the include file, in compat mode
       * without leading slash (tabled string).
       */
    int         depth;
      /* The absolute value is the include depth, counting from 1 upwards.
       * If the include did not generate code, the value is stored negative.
       */
};


/* --- struct program_s: the program head structure
 *
 * This structure is actually just the head of the memory block
 * with all the programs data.
 */

/* TODO: We seem to need a datatype for program offsets (right now: unsigned short).
 * TODO:: It shows up in quite a lot of places.
 * TODO: Replace the on program_s structure with a header/data couple. The
 * TODO:: header keeps vars likes refs, blueprint, swap_num, *data; and the
 * TODO:: data keeps the static bytecodes and similar. This way we can swap
 * TODO:: the program even for clones.
 */

struct program_s
{
    p_int           ref;           /* Reference count */
    p_int           total_size;
      /* Total size of the memory block allocated for this program.
       */
#ifdef DEBUG
    p_int           extra_ref;     /* Used to verify ref count */
#endif
    bytecode_p      program;       /* The binary instructions */
    string_t       *name;
      /* Name of file that defined prog (untabled, no leading '/',
       * but a trailing '.c', no embedded '\0')
       */
    object_t       *blueprint;
      /* Counted pointer to the (possibly destructed) blueprint object,
       * or NULL if the blueprint has been destructed in a previous cycle.
       * Note: Whenever this member is changed, make sure to remove
       * the program from the swap ('remove_prog_swap(prog, MY_TRUE)').
       */
    int32           id_number;
      /* The id-number is unique among all programs and used to store
       * information for this program without actually pointing to
       * the structure.
       */
    mp_int          load_time;     /* When has it been compiled ? */
    linenumbers_t  *line_numbers;
      /* Line number information, NULL when not swapped in.
       * If swapped out, the data is stored in the swap file at
       * .swapnum+.total_size .
       */
    unsigned short *function_names;
      /* Lookup table [.num_function_names] function-index -> offset of
       * the function within the functions[] table. function_names[] is
       * stored right after the the bytecode within the same allocation
       * unit.
       * The table is sorted in descending order of the pointers(!)
       * of the shared function name strings. If the program contains
       * redefinitions of inherited functions, the entry here points
       * to the redefinition, the inherited function can then be
       * found from there.
       */
    funflag_t *functions;
      /* Array [.num_functions] with the flags and addresses of all
       * functions, inherited and own.
       * Nameless functions (those without an entry in function_names[])
       * are collected at the end of the table.
       * TODO: Instead of hiding the function information in the bytecode
       * TODO:: it should be tabled here.
       */
    string_t **strings;
      /* Array [.num_strings] of the shared strings used by the program.
       * Stored in reverse order at the end of the array are the pointers
       * to the names of all included files which generated code - these
       * are used when retrieving line numbers.
       */
    variable_t *variables;
      /* Array [.num_variables] with the flags, types and names of all
       * variables.
       */
    inherit_t *inherit;
      /* Array [.num_inherited] of descriptors for (directly) inherited programs.
       */
    include_t *includes;
      /* Array [.num_includes] of descriptors for included files.
       */
    struct_def_t *struct_defs;
      /* Array [.num_structs] of struct descriptors */

    unsigned short flags;
      /* Flags for the program: */

    short heart_beat;
      /* Index of the heart beat function. -1 means no heart beat
       */

    /* The types of all function arguments are saved in the
     * .argument_types[]. To look up the arguments types for
     * function <n>, retrieve the start index from the .type_start[]
     * as .type_start[n]. If this index is INDEX_START_NONE, the function
     * has no type information.
     *
     * Both arrays will only be allocated if '#pragma save_types' has
     * been specified.
     */
    vartype_t *argument_types;
    unsigned short *type_start;
      /* TODO: Some code relies on this being unsigned short */

    p_int swap_num;
      /* The swap number (swap file offset) for an unswapped program
       * It is set to -1 if it hasn't been swapped yet.
       */

    /*
     * And now some general size information.
     */
    unsigned short num_function_names;
      /* Number of function names listed in the lookup table.
       * This number is <= .num_functions as the redefinition of
       * of inherited functions does not need an additional name
       * entry. Also, private functions have no visible name.
       */
    unsigned short num_functions;
      /* Number of functions (inherited and own) of this program */
    unsigned short num_strings;
      /* Number of shared strings (including filenames) used by the program */
    unsigned short num_includes;
      /* Number of stored include filenames */
    unsigned short num_variables;
      /* Number of variables (inherited and own) of this program */
    unsigned short num_inherited;
      /* Number of (directly) inherited programs */
    unsigned short num_structs;
      /* Number of listed struct definitions */
};

/* Constants for flags in program_s. */
enum program_flags {
    P_REPLACE_ACTIVE  = 0x0001,
      /* This program will be or has been replaced at least once.
       * As this flag is never reset, the caller must check the
       * obj_list_replace if his object is affected or not.
       */
    P_NO_INHERIT      = 0x0002, /* Program must not be inherited */
    P_NO_CLONE        = 0x0004, /* No clones allowed */
    P_NO_SHADOW       = 0x0008, /* No shadows allowed */
    P_SHARE_VARIABLES = 0x0010, /* Clone vars are assigned from 
                                 * the current blueprint vars. */
    P_RTT_CHECKS      = 0x0020, /* enable runtime type checks */
};
/* Special value for type_start in program_s designating that the function has
 * no type info. */
enum {
    INDEX_START_NONE = 65535,   
};

static INLINE bytecode_p PROGRAM_END(program_t program) __attribute__((pure));
static INLINE bytecode_p PROGRAM_END(program_t program) {
    return (bytecode_p)(program.function_names);
}


/* --- struct linenumbers_s: the linenumber head structure
 *
 * This structure is the head of the memory block with the linenumbers
 * data.
 */

struct linenumbers_s
{
    size_t     size;             /* Total allocated size of this structure */
    bytecode_t line_numbers[1];
      /* Array [.size - sizeof(.size)] with the delta-compressed
       * line number information. This is actually one byte too many, but
       * that simplifies the swapping code.
       */
};


/* --- struct function_s: Function description
 *
 * Structures of this type hold various important pieces of
 * information about a function.
 * The compiler uses this structure to collect the function information
 * of newly defined and inherited functions, of which the former will also
 * be compiled into the function header
 * The simul_efun module uses this structure to look up quickly functions.
 */

struct function_s
{
    string_t *name;  /* Name of function (shared string) */
    union {
        uint32 pc;       /* lfuns: Address of function header */
        uint32 inherit;  /* Inherit table index from where inherited. */
         int32 func;
           /* For cross-defined functions, this is the index offset
            * to the original function in bias-0x200000 notation.
            * Semantik: real-index = this-index + offset.
            * The offset is also stored in the function flags in
            * the program_t.functions[] array.
            *
            * simul_efun.c also uses this field as a 'next'
            * index in the simul_efun function table for
            * functions that have been discarded due to a
            * change in the number of arguments.
            */
        function_t *next;        /* used for mergesort */
    } offset;
    funflag_t     flags;      /* Function flags */
    fulltype_t    type;       /* Return type of function (counted). */
    unsigned char num_local;  /* Number of local variables */
    unsigned char num_arg;    /* Number of arguments needed. */
};

enum {
    SIMUL_EFUN_VARARGS = 0xff,  /* Magic num_arg value for varargs */
};

/* --- Bytecodes used to encode line numbers ---
 *
 * The line number information is a block of bytecode associating small
 * blocks of bytecode with the generating line number ranges.
 *
 * To save space, the information uses a delta compression, necessitating
 * to read all information from the beginning and counting up a line number
 * and bytecode offset counter to retrieve a specific bytecode/line number
 * association.
 *
 * The names of included files are stored in the order of appearance
 * at the end of the string table. Multiple included files are stored
 * several times.
 */

/* Bytecodes 0x00..0x3a: The amount of program bytes generated for the
 * current line; this entry is complete with that.
 */
enum line_numbe_bytecodes {
    LI_MAXOFFSET     = 0x3b,
  /* The current line generated 0x3b bytes (more), but the entry
   * is not complete. This code used after the linecodes 0xC0..0xFF.
   */

    LI_BACK          = 0x3c,
  /* Followed by unsigned byte <off>.
   * The current line counter was set back by <off>+1.
   */

    LI_INCLUDE       = 0x3d,
  /* The following program bytes were generated from the next included
   * file. Reset the current line number to 0.
   * LI_INCLUDEs can be nested.
   */

    LI_INCLUDE_END   = 0x3e,
  /* End of the included program part.
   */

    LI_L_RELOCATED   = 0x3f,
  /* Followed by bytes <hi> <lo> LI_L_RELOCATED
   * The lines were relocated from a line by a delta to the current_line-2.
   * The delta, an signed int16, is encoded in <hi> and <lo>.
   */

/* Bytecodes 0x40..0x7f encode actual code/line number relations:
 *   codesize:  1..8
 *   line incr: 2..9
 * -> bytecode = (lineincr+6) << 3 | (codesize-1)
 * Each code is a complete entry.
 */

    LI_RELOCATED     = 0xc0,
  /* Bytecodes 0x80..0xDF: a small line relocation.
   * The lines were relocated from a line by a delta -0x20..0x1F
   * to the current_line-2.
   * The delta is encoded as delta+LI_RELOCATED.
   */

    LI_SMALL_REL     = 0x20,
  /* The allowed magnitude of a relocation to fit into a small
   * relocation code.
   */

    LI_MAXEMPTY      = 0x20,
  /* Bytecodes 0xE0..0xFF: 'use' a number of lines between 1 and 32
   * -> bytecode = (0x100 - lines)
   * The entry is not complete.
   */
};

/***************************************************************************/

/* Static helper functions */
static INLINE intptr_t CROSSDEF_NAME_OFFSET(const funflag_t flags)
                                          __attribute__((const));
static INLINE intptr_t CROSSDEF_NAME_OFFSET(const funflag_t flags)
  /* For a given NAME_CROSS_DEFINED function, extract the difference to
   * the real functions index from the flags.
   */
{
    // flags is unsigned, but the offset _must_ be signed
    return ((sfunflag_t)(flags & INHERIT_MASK)) - 
                          ((INHERIT_MASK + 1) >> 1);
}

static INLINE intptr_t GET_CROSSDEF_OFFSET(const funflag_t value)
                                          __attribute__((const));
static INLINE intptr_t GET_CROSSDEF_OFFSET(const funflag_t value)
  /* The index difference <value> in bias-notation is converted back into
   * the real number. The difference to CROSSDEF_NAME_OFFSET is that <value>
   * is supposed to be the plain number, not a real function flags word.
   */
{
    // value is unsigned, but the offset _must_ be signed
    return ((sfunflag_t)(value)) - ((INHERIT_MASK + 1) >> 1);
}

static INLINE funflag_t MAKE_CROSSDEF_OFFSET(const intptr_t value)
                                          __attribute__((const));
static INLINE funflag_t MAKE_CROSSDEF_OFFSET(const intptr_t value)
  /* Convert the index difference <value> into the bias-notation for storage
   * in the function flags.
   */
{
    // flags is unsigned, but the offset _must_ be signed
    return ((value) + ((INHERIT_MASK + 1) >> 1));
}

static INLINE funflag_t MAKE_CROSSDEF_ROFFSET(const intptr_t value)
                                          __attribute__((const));
static INLINE funflag_t MAKE_CROSSDEF_ROFFSET(const intptr_t value)
  /* Convert the reverse index difference <value> into the bias-notation for
   * storage in the function flags.
   * TODO: What is this exactly?
   */
{
    // flags is unsigned, but the offset _must_ be signed
    return ((value) - ((INHERIT_MASK + 1) >> 1));
}

static INLINE inherit_t * search_function_inherit(const program_t *const progp, 
                                                  const int fx)
                                                __attribute__((pure)) 
                                                __attribute__((nonnull(1)));
static INLINE inherit_t * search_function_inherit(const program_t *const progp, 
                                                  const int fx)
  /* Look for the right inherit index, which contains the function with
   * the index <fx>. It is assumed, that progp has at least one inherit
   * and that <fx> points indeed to a function in an inherit of <progp>.
   */
{
    int inum = progp->num_inherited;
    inherit_t *inheritp;

    for (inheritp = progp->inherit ; 
           inum > 0 && (fx < inheritp->function_index_offset ||
           fx >= inheritp->function_index_offset + inheritp->prog->num_functions);
           inheritp++, inum--) NOOP;
    
    return inheritp;
}

#endif /* EXEC_H__ */
