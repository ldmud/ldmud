#ifndef __EXEC_H__
#define __EXEC_H__ 1
/*
 * A compiled program consists of several data blocks, all allocated
 * contiguos in memory to enhance the working set. At the compilation,
 * the blocks will be allocated separately, as the final size is
 * unknow. When compilation is done, the blocks will be copied into
 * the one big area.
 *
 * There are 8 different blocks of information for each program:
 * 1. The program itself. Consists of machine code instructions for a virtual
 *    stack machine. The size of the program must not be bigger than
 *    65535 bytes, as 16 bit pointers are used. Who would ever need a bigger
 *    program :-)
 * 2. Function names. All local functions that has been defined or called,
 *    with the address of the function in the program. Inherited functions
 *    will be found here too, with information of how far up the inherit
 *    chain that the function was defined.
 * 3. String table. All strings used in the program. They are all pointers
 *    into the shared string area. Thus, they are easily found and deallocated
 *    when the object is destructed.
 * 4. Table of variable names. They all point into the shared string table.
 * 5. Line number information. A table which tells at what address every
 *    line belongs to. The table has the same number of entries as the
 *    programs has source lines. This is used at errors, to find out the
 *    line number of the error.
 * 6. List of inherited objects.
 */

#include "datatypes.h" /* struct svalue */

/* TODO: The 'flags' should maybe be implemented using bitfields, especially
 * TODO:: since some flagvalues actually contain offsets
 */

/*
 * When an new object inherits from another, all function definitions
 * are copied, and all variable definitions.
 * Flags with NAME_ can't explicitly declared. Flags that can be declared,
 * are found with TYPE_ below.
 *
 * When an object is compiled with type testing (#pragma strict_types), all
 * types are saved of the arguments for that function during compilation.
 * If the #pragma save_types is specified, then the types are saved even
 * after compilation, to be used when the object is inherited.
 */

#define NAME_INHERITED                0x80000000 /* Defined by inheritance         */
#define TYPE_MOD_STATIC                0x40000000 /* Static function or variable    */
#define TYPE_MOD_NO_MASK        0x20000000 /* The nomask => not redefineable */
#define TYPE_MOD_PRIVATE        0x10000000 /* Can't be inherited             */
#define TYPE_MOD_PUBLIC                0x08000000 /* Force inherit through private  */
#define TYPE_MOD_VARARGS         0x04000000 /* Used for type checking         */
#define NAME_INITIALIZED        0x04000000 /* only used for variables        */
#define TYPE_MOD_VIRTUAL        0x02000000 /* can be re- and cross- defined  */
#define TYPE_MOD_PROTECTED        0x01000000 /* cannot be called externally    */
#define TYPE_MOD_XVARARGS        0x00800000 /* accepts optional arguments     */

#define FUNSTART_MASK                0x000fffff
#define NAME_CROSS_DEFINED        0x00080000
#define INHERIT_MASK                0x0003ffff


#define NAME_HIDDEN                0x00000800 /* Not visible for inheritance    */
#define NAME_PROTOTYPE                0x00000400 /* Defined by a prototype only    */
#define NAME_UNDEFINED                0x00000200 /* Not defined yet                */
#define NAME_TYPES_LOST                0x00000100 /* inherited, no save_types       */

/* Program code is made up of series of functions.
 * Every function consists of a header followed by the bytecode:
   name : shared string (4 bytes)
   return type          (1)
-->num_arg              (1)
   num_local            (1)
   executable code
 * --> marks the address given by program->functions[] & FUNSTART_MASK
 * TODO: This should be done in a struct, with nice macros for the
 * TODO:: address mangling. But be aware that a struct might introduce padding.
 */

struct function { /* used by compiler and simul_efun only? */
    char *name; /* This is needed very often, therefore it should be first (allocated) */
    union {
        uint32 pc;                /* Address of function                        */
        uint32 inherit;                /* inherit table index when inherited.        */
        /*signed*/ int32 func;        /* offset to first inherited function
                                 * with this name.
                                 * simul_efun also uses this field as a next
                                 * index in the simul_efun function table for
                                 * functions that have been discarded due to a
                                 * change in the number of arguments.
                                 */
        struct function *next;        /* used for mergesort */
    } offset;
    uint32 flags;
    unsigned short type;        /* Return type of function. See below. */
    unsigned char num_local;        /* Number of local variables */
    unsigned char num_arg;        /* Number of arguments needed.
                                 * -1 arguments for a simul_efun means VARARGS.
                                 */
};

struct variable {
    char *name;
    uint32 flags;                /* All flags, also type of variable. */
};

struct inherit {
    struct program *prog;
    unsigned short function_index_offset;
    unsigned short variable_index_offset;
};

struct program {
    p_int ref;                                /* Reference count */
    p_int total_size;                        /* Sum of all data in this struct */
#ifdef DEBUG
    p_int extra_ref;                        /* Used to verify ref count */
#endif
    char *program;                        /* The binary instructions */
/* TODO: program should get its own type. This will sure break
 * TODO:: lots of pointers which assume it's char* resp. uchar*
 * TODO:: (yes, both are used :-().
 */
    char *name;                                /* Name of file that defined prog */
    int32  id_number;                        /* used to associate information with
                                          this prog block without needing to
                                           increase the reference count     */
    int32  load_time;                        /* When has it been compiled ? */
    /*unsigned*/ char *line_numbers;        /* Line number information */
    unsigned short *function_names;
#define PROGRAM_END(program) ((char *)(program).function_names)
    uint32 *functions;
/* Address and flags of all functions, actually a [num_functions] array */
    char **strings;                        /* All strings uses by the program */
    struct variable *variable_names;        /* All variables defined */
    struct inherit *inherit;                /* List of inherited prgms */
    unsigned short flags;
    short heart_beat;                        /* Index of the heart beat function.
                                         * -1 means no heart beat
                                         */
    /*
     * The types of function arguments are saved where 'argument_types'
     * points. It can be a variable number of arguments, so allocation
     * is done dynamically. To know where first argument is found for
     * function 'n' (number of function), use 'type_start[n]'.
     * These two arrays will only be allocated if '#pragma save_types' has
     * been specified. This #pragma should be specified in files that are
     * commonly used for inheritance. There are several lines of code
     * that depends on the type length (16 bits) of 'type_start' (sorry !).
     */
    unsigned short *argument_types;
#define INDEX_START_NONE                65535
    unsigned short *type_start;

    p_int swap_num;                /* Swap file offset. -1 is not swapped yet. */

    /*
     * And now some general size information.
     */
    unsigned short num_function_names;
    unsigned short num_functions;
    unsigned short num_strings;
    unsigned short num_variables;
    unsigned short num_inherited;
};

extern struct program *current_prog;

/*
 * Types available. The number '0' is valid as any type. These types
 * are only used by the compiler, when type checks are enabled. Compare with
 * the run-time types, named T_ interpret.h.
 */

#define TYPE_UNKNOWN        0        /* This type must be casted */
#define TYPE_NUMBER        1
#define TYPE_STRING        2
#define TYPE_VOID        3
#define TYPE_OBJECT        4
#define TYPE_MAPPING        5
#define TYPE_FLOAT        6
#define TYPE_ANY        7        /* Will match any type */
#define TYPE_SPACE        8
#define TYPE_CLOSURE        9
#define TYPE_SYMBOL    10
#define TYPE_QUOTED_ARRAY 11
#define TYPE_TERM      12
#define TYPEMAP_SIZE   13

/*
 * These are or'ed in on top of the basic type.
 */
#define TYPE_MOD_POINTER        0x0040        /* Pointer to a basic type        */
#define TYPE_MOD_REFERENCE        0x0080

#define TYPE_MOD_MASK                0x000000ff

#define TYPE_MOD_RMASK                (TYPE_MOD_MASK & ~TYPE_MOD_REFERENCE)

#define P_REPLACE_ACTIVE        0x0001 /* Program replacement scheduled */

#define H_MOVE_OBJECT0        0
#define H_MOVE_OBJECT1        1
#define H_LOAD_UIDS        2
#define H_CLONE_UIDS        3
#define H_CREATE_SUPER        4
#define H_CREATE_OB        5
#define H_CREATE_CLONE        6
#define H_RESET                7
#define H_CLEAN_UP        8
#define H_MODIFY_COMMAND   9
#define H_NOTIFY_FAIL          10
#define H_NO_IPC_SLOT          11
#define H_INCLUDE_DIRS          12
#define H_TELNET_NEG          13
#define H_NOECHO          14
#define H_ERQ_STOP          15
#define H_MODIFY_COMMAND_FNAME 16
#define NUM_CLOSURE_HOOKS 17

extern struct svalue closure_hook[NUM_CLOSURE_HOOKS];

#define VIRTUAL_VAR_TAG 0x4000

extern struct simul_efun_table_s {
    unsigned char *funstart;
    struct program *program;
    p_int function_index_offset;
    p_int variable_index_offset;
} simul_efun_table[];

#define F_ESCAPE_BITS 7

/* --- struct instr: description of stackmachine instructions ---
 *
 * Stackmachine instructions are both 'internal' codes with no external
 * representation, as well as efuns.
 *
 * The information about all instructions is collected in the table
 * instrs[] which is indexed by the bytecode of the instructions.
 *
 * The table is declared in instrs.h and defined in the file efun_defs.c
 * both of which are created by make_func from the func_spec file.
 * The table is compiled into the lexer module and exported from there.
 */

struct instr
{
    short max_arg;    /* Maximum number of arguments, -1 for '...' */
    short min_arg;    /* Minimum number of arguments. */
    char  type[2];    /* Types of arguments 1 and 2. */
    short Default;
      /* An efun to use as default value for last argument.
       * > 0: index into instrs[] to the efun to use.
       *   0: no default value.
       *  -1: this whole entry describes an internal stackmachine code,
       *      not a normal efun.
       */
    short ret_type;   /* The return type used by the compiler. */
    short arg_index;  /* Indexes the efun_arg_types[] array (see make_func). */
    char *name;       /* The printable name of the instruction. */
};
#endif /* __EXEC_H__ */
