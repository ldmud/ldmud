#ifndef __EXEC_H__
#define __EXEC_H__ 1

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
 * The blocks of a program (hopefully in correct allocation order) are these:
 *
 *   struct program: The basic structure of the program, with pointers to
 *       the other data blocks.
 *
 *   uint32 functions[]: One word for each function, giving the type
 *       and the offset within the program[] codearray. Additional information
 *       for each function is embedded into the program code.
 *     
 *   char *strings[]: An array of pointers to the string literals (stored
 *       as shared strings) used by the program. This way the program can
 *       use the strings simply by (easily swappable) index. 
 *       
 *       The last strings are the names of all files included by the programs
 *       source file, stored in reverse order of their appearance (multiply
 *       included files appear several times).
 *   TODO: Does the compiler try to minimize the array, by storing each string
 *   TODO:: ref only once?
 *
 *       When a program is swapped, the references to these strings are
 *       not removed so that the string literals stay in memory all the time.
 *       This saves time on swap-in, and the string sharing saves more memory
 *       than swapping might gain.
 *
 *   struct variable variable_names[]: an array describing all variables
 *       with type and name, inherited or own.
 *      
 *   struct inherit inherit[]: an array describing all inherited programs.
 *
 *   bytecode program[]: the actual bytecode for the program.
 *
 *   unsigned short function_names[]: TODO: ???
 *
 * The line number information is stored in a separate memory block so
 * it can be swapped out separately and loaded only in case of an error.
 * For a similar reason, the information is encoded using a kind of delta
 * compression.
 *
 *
 * Bytecode
 * --------
 *
 * As the name conveys, the LPC programs are compiled into a bytecode,
 * assuming 8-Bit-Bytes. Since we have more than 256 opcodes, the less
 * often used instructions are encoded in two-byte opcodes: the highbyte
 * is (instruction / 128), the low byte (instruction % 128). The resulting
 * values for the highbyte are 'coincidentally' the values of the prefix
 * opcodes F_ESCAPE, F_TEFUN and F_VEFUN. The divisor 128 is symbolically
 * defined by F_ESCAPE_BITS below.
 *
 * To achieve platform independance, the driver does not operate directly
 * with 'char's and 'char *'s, but instead with 'bytecode_t' and
 * 'bytecode_p's. Combined with some macros this allows the implementation
 * even on platforms with CHAR_BITs != 8.
 * TODO: This support is far from complete/comprehensive, and some values
 * TODO:: in the bytecode are stored in host-sizes and -layouts.
 *
 * The bytecode itself is divided into functions: the code for every
 * function is (except for absolute jumps) selfcontained and prepended
 * by a header holding the name of the function, and the number and types
 * of expected arguments. The advantage is that for a given function
 * the driver does not need to know if it is part of a program or a 
 * lambda closure compiled at runtime: in both cases all necessary
 * information about the function is right where its code is.
 *
 * The maximum size of a program is limited by the biggest offset
 * that can be stored in the 'functions' array, currently 1 MByte.
 * A lot of internal offset counters are shorts even, though so far
 * this never caused a problem.
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
 * gets its own struct inherit, together with reserved space in the
 * objects variable block (TODO: is this true? If yes, what a waste).
 * To mark the virtual inherit, the variable types receive the modifier
 * TYPE_MOD_VIRTUAL.
 *
 * However, accesses to virtually inherited variables only use the first
 * inherit instance - if the variable index in the child's program code indexes
 * the variable block associated with a later instance, the driver finds
 * the first instance by comparing the .prog entries and uses the 'real'
 * variable associated with this first instance.
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
#include "datatypes.h" /* struct svalue */


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
    short min_arg;
      /* Minimum number of arguments.
       * The number 0 marks incallable closures: instructions which
       * are used as syntactic markers in lambda closures, but by
       * themselves can't be executed.
       */
    char  type[2];    /* Types of arguments 1 and 2, using the svalue codes */
    short Default;
      /* An efun to use as default value for last argument.
       * > 0: index into instrs[] to the efun to use.
       *   0: no default value.
       *  -1: this whole entry describes an internal stackmachine code,
       *      not a normal efun (an 'operator' in closure lingo).
       */
    short ret_type;   /* The return type used by the compiler. */
    short arg_index;  /* Indexes the efun_arg_types[] array (see make_func). */
    char *name;       /* The printable name of the instruction. */
};


/* --- Driver Hooks ---
 *
 * The hooks are a bit out of place here, but make_func.y needs them and
 * there don't really fit anywhere.
 * 
 * The hooks must match mudlib/sys/driver_hook.h
 */

#define H_MOVE_OBJECT0           0
#define H_MOVE_OBJECT1           1
#define H_LOAD_UIDS              2
#define H_CLONE_UIDS             3
#define H_CREATE_SUPER           4
#define H_CREATE_OB              5
#define H_CREATE_CLONE           6
#define H_RESET                  7
#define H_CLEAN_UP               8
#define H_MODIFY_COMMAND         9
#define H_NOTIFY_FAIL           10
#define H_NO_IPC_SLOT           11
#define H_INCLUDE_DIRS          12
#define H_TELNET_NEG            13
#define H_NOECHO                14
#define H_ERQ_STOP              15
#define H_MODIFY_COMMAND_FNAME  16

#define NUM_CLOSURE_HOOKS       17  /* Number of hooks */


/* --- Byte code ---
 *
 * The program code is stored as byte code. The following definitions
 * and macros allow its implementation even on platforms with more than
 * 8 bits per character.
 * TODO: This portability is far from complete, and not used everywhere.
 *
 *   bytecode_t: an integral type holding the numbers 0..255.
 *   bytecode_p: an integral type addressing a bytecode. This need not
 *               be a pointer.
 *               
 *   bytecode_t GET_CODE(bytecode_p p)
 *   bytecode_t LOAD_CODE(bytecode_p p)
 *     Return the bytecode from *p, the LOAD_ variant then increments p.
 *     
 *    char GET_INT8(p) , LOAD_INT8(p)
 *   uchar GET_UINT8(p), LOAD_UINT8(p)
 *     Return the 8-Bit (unsigned) int stored at <p>, the LOAD_ variants
 *     then increment <p>.
 *     
 *   void GET_SHORT ([unsigned] short d, bytecode_p p)
 *   void LOAD_SHORT([unsigned] short d, bytecode_p p)
 *     Load the (unsigned) short 'd' stored at <p>, the LOAD_ variant
 *     then increments <p>.
 *
 *   void GET_INT16 ([unsigned] int16 d, bytecode_p p)
 *   void LOAD_INT16([unsigned] int16 d, bytecode_p p)
 *     Load the (unsigned) int16 'd' stored at <p>, the LOAD_ variant
 *     then increments <p>.
 *
 *   void LOAD_INT32([unsigned] int32 d, bytecode_p p)
 *     Load the (unsigned) in32 'd' stored at <p>, then increment <p>.
 */

#define F_ESCAPE_BITS 7
   /* The number of bits the lowbyte of a dual-byte-opcode can hold
    */

#if CHAR_BIT == 8
typedef unsigned char   bytecode_t;
typedef bytecode_t    * bytecode_p;

#define GET_CODE(p)    (*(p))
#define LOAD_CODE(p)   (*(p)++)

#define GET_UINT8(p)   (*((unsigned char *)(p)))
#define GET_INT8(p)    (*((signed char *)(p)))
#define LOAD_UINT8(p)   (*((unsigned char *)(p)++))
#define LOAD_INT8(p)    (*((signed char *)(p)++))

/* TODO: These generic mem-macros should go into a macros.h. See also
 * TODO:: how mudos does it.
 */
#define LOAD_2BYTE(d,p) ( ((char *)&(d))[0] = *(char *)(p)++, \
                          ((char *)&(d))[1] = *(char *)(p)++)

#define GET_2BYTE(d,p)  ( ((char *)&(d))[0] = ((char *)(p))[0], \
                          ((char *)&(d))[1] = ((char *)(p))[1] )

#define LOAD_4BYTE(d,p) ( ((char *)&(d))[0] = *(char *)(p)++, \
                          ((char *)&(d))[1] = *(char *)(p)++, \
                          ((char *)&(d))[2] = *(char *)(p)++, \
                          ((char *)&(d))[3] = *(char *)(p)++ )

#define GET_4BYTE(d,p)  ( ((char *)&(d))[0] = ((char *)(p))[0], \
                          ((char *)&(d))[1] = ((char *)(p))[1], \
                          ((char *)&(d))[2] = ((char *)(p))[2], \
                          ((char *)&(d))[3] = ((char *)(p))[3] )
    
#define GET_SHORT(d,p)  GET_2BYTE(d,p)
#define LOAD_SHORT(d,p) LOAD_2BYTE(d,p)
  /* TODO: This assumes sizeof(short) == 2. */

#define LOAD_INT16(d,p) LOAD_2BYTE(d,p)
#define LOAD_INT32(d,p) LOAD_2BYTE(d,p)
#define GET_INT32(d,p)  GET_4BYTE(d,p)

#endif

#ifndef GET_CODE
#  error "No bytecode type defined."
#endif


/* --- Function flags ---
 *
 * Programs know about their functions from an array of uint32s which hold
 * the flags for the function and, in one field in the low bits, the
 * address of the code.
 *
 * Some flags (TYPE_MOD_VIRTUAL) are also used for variable flags.
 *
 * Using a bitfield is tempting, but generates inefficient code due to the
 * lack of a real 'bool' datatype.
 * 
 * TODO: Function flags should be its own type 'funflag', not just 'uint32'.
 */

#define NAME_INHERITED      0x80000000  /* defined by inheritance */
#define TYPE_MOD_STATIC     0x40000000  /* Static function or variable    */
#define TYPE_MOD_NO_MASK    0x20000000  /* The nomask => not redefineable */
#define TYPE_MOD_PRIVATE    0x10000000  /* Can't be inherited             */
#define TYPE_MOD_PUBLIC     0x08000000  /* Force inherit through private  */
#define TYPE_MOD_VARARGS    0x04000000  /* Used for type checking         */
#define NAME_INITIALIZED    0x04000000  /* only used for variables        */
#define TYPE_MOD_VIRTUAL    0x02000000  /* can be re- and cross- defined  */
#define TYPE_MOD_PROTECTED  0x01000000  /* cannot be called externally    */
#define TYPE_MOD_XVARARGS   0x00800000  /* accepts optional arguments     */

#define FUNSTART_MASK       0x000fffff
  /* Function not inherited: unsigned address of the function code relative
   * to the begin of the program block (struct program->program).
   */

#define NAME_CROSS_DEFINED  0x00080000
  /* Function from A used in B, resolved by C which inherits both A and B.
   * If this is true, the value (flags & INHERIT_MASK) (in bias-0x800000
   * representation) is the difference between the current function index
   * and the real functions index.
   */
 
#define INHERIT_MASK        0x0003ffff
  /* Function inherited: unsigned index of the parent program descriptor
   * in the struct program->inherit[] array. In the parent program, this
   * function may again be inherited.
   */


#define NAME_HIDDEN         0x00000800 /* Not visible for inheritance    */
#define NAME_PROTOTYPE      0x00000400 /* Defined by a prototype only    */
#define NAME_UNDEFINED      0x00000200 /* Not defined yet                */
#define NAME_TYPES_LOST     0x00000100 /* inherited, no save_types       */


/* --- Function header ---
 *
 * The bytecode for every function is preceeded by a header with the name
 * and information about arguments and types:
 *
 * struct fun_hdr {
 *     shared char * name_of_function; (4 Bytes)
 *     byte          return_type;        (1 Byte)
 * --> byte          number_formal_args; (1 Byte)
 *         Bit 7: set if the function has a 'varargs' argument
 * TODO: some code makes use of the fact that this makes the number negative
 *         Bit 6..0: the number of arguments
 *     byte          number_local_vars;  (1 Byte)
 *         This includes the svalues needed for the break stack for
 *         switch() statements.
 *     bytecode_t    opcode[...]
 * }
 *
 * The function address given in the program's function block points to
 * .number_formal_args.
 *
 * Since structs introduce uncontrollable padding, access of all fields
 * is implemented using macros taking the 'function address', typedef'd
 * as fun_hdr_p, as argument and evaluate to the desired value. 
 *
 * Note: Changes here can affect the struct lambda layout and associated
 *       constants.
 * TODO: the other fields should have proper types, too.
 * TODO: the whole information should be in a table, and not in the
 * TODO:: bytecode. See struct program.
 */

typedef bytecode_p fun_hdr_p;
  /* TODO: Make this a void* for now? */

#define SIMUL_EFUN_FUNSTART ((bytecode_p) -1)
  /* Special value used for inter_pc and funstart to mark simul_efuns
   * for dump_trace().
   * TODO: Invent something similar for efun/operator closures?
   */

#define EFUN_FUNSTART ((bytecode_p) -2)
  /* Special value used for funstart to mark simul_efuns for dump_trace.
   */


#define FUNCTION_NAME(p)      (*((char **)((char *)p - sizeof(char) - sizeof(char *))))
#define FUNCTION_TYPE(p)      (*((unsigned char *)((char *)p - sizeof(char))))
#define FUNCTION_NUM_ARGS(p)  EXTRACT_SCHAR((char *)p)
#define FUNCTION_NUM_VARS(p)  (*((unsigned char *)((char *)p + sizeof(char))))
#define FUNCTION_CODE(p)      ((bytecode_p)((unsigned char *)p + 2* sizeof(char)))
#define FUNCTION_FROM_CODE(p) ((fun_hdr_p)((unsigned char *)p - 2* sizeof(char)))


/* --- struct variable: description of one variable
 *
 * This structure describes one variable, inherited or own.
 * The type part of the .flags is used just by the compiler.
 */

struct variable
{
    char   *name;   /* Name of the variable (shared string) */
    uint32  flags;
      /* Flags and type of the variable.
       * If a variable is inherited virtually, the function flag
       * TYPE_MOD_VIRTUAL is or'ed on this.
       */
};


/* --- struct inherit: description of one inherited program
 *
 * The information about inherited programs ("objects") for a given
 * program is stored in an array of these structure, and the inherited
 * programs are accessed from the childs' program code by indexing this array.
 */

struct inherit
{
    struct program *prog;  /* Pointer to the inherited program */
    unsigned short function_index_offset;
      /* Offset of the inherited program's function block within the 
       * inheriting program's function block.
       */
    unsigned short variable_index_offset;
      /* Offset of the inherited program's variables block within the
       * inheriting program's variable block.
       */
    SBool is_extra; /* TRUE for duplicate virtual inherits */
    /* TODO: Why no flag for 'virtual inherited'? */
};


/* --- struct program: the program head structure
 *
 * This structure is actually just the head of the memory block
 * with all the programs data.
 */

struct program
{
    p_int           ref;           /* Reference count */
    p_int           total_size;
      /* Total size of the memory block allocated for this program.
       */
#ifdef DEBUG
    p_int           extra_ref;     /* Used to verify ref count */
#endif
    bytecode_p      program;       /* The binary instructions */
    char           *name;
      /* Name of file that defined prog (allocated, no leading '/',
       * but a trailing '.c')
       */
    int32           id_number;
      /* The id-number is unique among all programs and used to store
       * information for this program without actually pointing to
       * the structure.
       */
    mp_int          load_time;     /* When has it been compiled ? */
    bytecode_p      line_numbers;  /* Line number information */
      /* TODO: Check if the line_numbers are allocated in a separate
       * TODO:: block. If not, the swapper wastes memory on swapping in.
       */
    unsigned short *function_names;
#define PROGRAM_END(program) ((bytecode_p)(program).function_names)
      /* TODO: FUnction_names? Maybe an array put at the end of the
       * TODO:: bytecode block with the offsets of all function names
       * TODO:: within the program?
       */
    /* TODO: funflags */ uint32 *functions;
      /* Array [.num_functions] with the flags and addresses of all
       * functions, inherited and own.
       * TODO: Instead of hiding the function information in the bytecode
       * TODO:: it should be tabled here.
       */
    char **strings;
      /* Array [.num_strings] of the shared strings used by the program.
       * Stored in reverse order at the end of the array the pointers
       * to the names of all included files, used when retrieving line
       * numbers.
       */
    struct variable *variable_names;
      /* Array [.num_variables] with the flags, types and names of all
       * variables.
       */
    struct inherit *inherit;
      /* Array [.num_inherited] of descriptors for inherited programs.
       */
    unsigned short flags;
      /* Flags for the program: */

#   define P_REPLACE_ACTIVE   0x0001 /* Program replacement scheduled */

    short heart_beat;
      /* Index of the heart beat function. -1 means no heart beat
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
    unsigned short *argument_types; /* TODO: ??? */
#define INDEX_START_NONE                65535
    unsigned short *type_start; /* TODO: ??? */

    p_int swap_num;
      /* The swap file offset for an unswapped program
       * It is set to -1 if it hasn't been swapped yet.
       */

    /*
     * And now some general size information.
     */
    unsigned short num_function_names;
    unsigned short num_functions;
      /* Number of functions (inherited and own) of this program */
    unsigned short num_strings;
      /* Number of shared strings used by the program */
    unsigned short num_variables;
      /* Number of variables (inherited and own) of this program */
    unsigned short num_inherited;
      /* Number of inherited programs */
};


/* --- struct function: TODO: ???
 */

struct function /* TODO: used by compiler and simul_efun only? */
{
    char *name;  /* TODO: Name of function (shared?) */
      /* This is needed very often, therefore it should be first (allocated) */
    union {
        uint32 pc;       /* TODO: Address of function */
        uint32 inherit;  /* TODO: inherit table index when inherited. */
        /*signed*/ int32 func;  /* offset to first inherited function
                                 * with this name.
                                 * simul_efun also uses this field as a next
                                 * index in the simul_efun function table for
                                 * functions that have been discarded due to a
                                 * change in the number of arguments.
                                 */
        struct function *next;        /* used for mergesort */
    } offset;
    /* TODO: funflag */ uint32 flags;
    unsigned short type;      /* Return type of function. See below. */
    unsigned char num_local;  /* Number of local variables */
    unsigned char num_arg;    /* Number of arguments needed. */
#   define SIMUL_EFUN_VARARGS  0xff  /* Magic num_arg value for varargs */
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
 * at the end of the string table. Multiply included files are stored
 * several times.
 */

/* Bytecodes 0x00..0x3b: The amount of program bytes generated for the
 * current line; this line is complete with that.
 */

#define LI_MAXOFFSET      0x3c
  /* The current line generated 0x3c bytes (more), but is still not
   * complete.
   */

#define LI_INCLUDE        0x3d
  /* The following program bytes were generated from the next included
   * file. Reset the current line number to 0.
   * LI_INCLUDEs can be nested.
   */

#define LI_INCLUDE_END    0x3e
  /* End of the included program part.
   */

#define LI_L_RELOCATED    0x3f
  /* Followed by bytes <hi> <lo> LI_L_RELOCATED
   * The lines were relocated from a line by a delta to the current_line-2.
   * The delta, an signed int16, is encoded in <hi> and <lo>.
   */

/* Bytecodes 0x40..0x7f encode actual code/line number relations:
 *   codesize:  1..8 
 *   line incr: 2..9
 * -> bytecode = (lineincr+6) << 3 | (codesize-1)
 */

#define LI_RELOCATED      0xc0
  /* Bytecodes 0x80..0xDF: a small line relocation.
   * The lines were relocated from a line by a delta -0x20..0x1F
   * to the current_line-2.
   * The delta is encoded as delta+LI_RELOCATED.
   */

#define LI_SMALL_REL      0x20
  /* The allowed magnitude of a relocation to fit into a small
   * relocation code.
   */

#define LI_MAXEMPTY       0x20


/* --- Type Constants ---
 * 
 * Available types, with the number '0' being valid as any type.
 * The types are used just by the compiler when type checks are
 * enabled.
 */

/* Basic type values */

#define TYPE_UNKNOWN        0   /* This type must be casted */
#define TYPE_NUMBER         1
#define TYPE_STRING         2
#define TYPE_VOID           3
#define TYPE_OBJECT         4
#define TYPE_MAPPING        5
#define TYPE_FLOAT          6
#define TYPE_ANY            7   /* Will match any type */
#define TYPE_SPACE          8   /* TODO: ??? */
#define TYPE_CLOSURE        9
#define TYPE_SYMBOL        10
#define TYPE_QUOTED_ARRAY  11
#define TYPE_TERM          12

#define TYPEMAP_SIZE       13   /* Number of types */

/* Flags, or'ed on top of the basic type */

#define TYPE_MOD_POINTER    0x0040  /* Pointer to a basic type */
#define TYPE_MOD_REFERENCE  0x0080  /* Reference to a type */

#define TYPE_MOD_MASK   0x000000ff
  /* Mask for basic type and flags.
   */

#define TYPE_MOD_RMASK  (TYPE_MOD_MASK & ~TYPE_MOD_REFERENCE)
  /* Mask to delete TYPE_MOD_REFERENCE from a type value
   */


#define VIRTUAL_VAR_TAG 0x4000  /* TODO: ??? */


/***************************************************************************/

#endif /* __EXEC_H__ */
